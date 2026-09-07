/// Linear-algebra dispatch classification.
///
/// CodeGen must never decide for itself what a loop nest "is" in BLAS terms.
/// This module maps an IR node onto a typed `LinAlgCall` descriptor naming a
/// routine, operand roles, dimensions and transpose flags, and owns the
/// POLICY table saying whether a recognised shape is routed through the
/// `blade_linalg.hpp` shim or left to Blade's own loop nest. CodeGen consults;
/// it does not pattern-match BLAS shapes.
///
/// `IRGram`/`IRMatmul` are first-class IR nodes and classify exactly. Other
/// shapes are recognised by matching LOOP-NEST SHAPES against
/// `LoopNestCodeGen` -- the fully-built nest, where "which array feeds which
/// kernel parameter at which level" is already resolved -- via `(|BlasL1|_|)`
/// (Dot) and `(|BlasL2|_|)` (Gemv), each one more `try*` in codegen's
/// shortcircuit chain, falling through on None.
///
/// This module never invents C++ identifiers: a matched descriptor names its
/// operands by echoing a name `LoopNestCodeGen` already holds, or hands back
/// the IR expression for CodeGen to resolve through its own name map.
module Blade.LinAlgPatterns

open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types

// Availability gate

/// How the BLAS/LAPACK toolchain was configured: the four TIERS of
/// docs/plans/plan-toolchain-packaging.md, resolved through `Toolchain.get`
/// (process env first and live, blade.toolchain.json second). First match
/// wins:
///
///   BLADE_BLAS=0|off     -> TierOff        (everything native)
///   BLADE_BLAS_LINK set  -> TierExplicit   (vendor-neutral: verbatim
///                                           include dirs + link inputs;
///                                           the MKL/BLIS door)
///   OPENBLAS_DIR set     -> TierOpenBlasDir (convenience shorthand for an
///                                           OpenBLAS install prefix;
///                                           expanded via Platforms)
///   BLADE_BLAS=1|on      -> TierSystem     (bare -lopenblas on the default
///                                           search paths)
///   otherwise            -> TierOff
type BlasTier =
    | TierOff
    | TierExplicit
    | TierOpenBlasDir
    | TierSystem

/// Which CBLAS/LAPACKE header family the shim headers should include.
/// `FlavorMkl` makes `blasBuildFlags` add `-DBLADE_BLAS_MKL`, under which
/// blade_linalg.hpp / blade_lapack.hpp include mkl_cblas.h / mkl_lapacke.h
/// (MKL implements the standard interfaces but does not ship the netlib
/// header names). `FlavorGeneric` is any other conforming install; both it
/// and the default `FlavorOpenBlas` use <cblas.h> / <lapacke.h>.
type BlasFlavor =
    | FlavorOpenBlas
    | FlavorMkl
    | FlavorGeneric

/// BLADE_BLAS_FLAVOR: "openblas" (default when unset) | "mkl" | anything
/// else -> generic.
let blasFlavor () : BlasFlavor =
    match Toolchain.get "BLADE_BLAS_FLAVOR" with
    | None -> FlavorOpenBlas
    | Some v ->
        match v.Trim().ToLowerInvariant() with
        | "mkl" -> FlavorMkl
        | "openblas" -> FlavorOpenBlas
        | _ -> FlavorGeneric

/// Resolve the tier. Deliberately CHEAP -- a handful of env/file lookups and
/// no filesystem probing -- because the gates below are consulted from
/// codegen's per-node routing decisions. The filesystem half (locating the
/// actual library file) lives in `blasBuildFlags`, which only Build.fs calls
/// and only once per compile.
///
/// A FUNCTION, never a module-level `let`: a module-level binding freezes
/// the environment read at first touch, which would make a mid-process pin
/// (a test's use-guard, a hand-run) silently ineffective. Every consultation
/// re-reads.
let resolveBlasTier () : BlasTier =
    let gate = Toolchain.get "BLADE_BLAS" |> Option.map _.Trim().ToLowerInvariant()
    match gate with
    | Some "0" | Some "off" -> TierOff
    | _ ->
        if (Toolchain.get "BLADE_BLAS_LINK").IsSome then TierExplicit
        elif (Toolchain.get "OPENBLAS_DIR").IsSome then TierOpenBlasDir
        elif gate = Some "1" || gate = Some "on" then TierSystem
        else TierOff

/// The BLAS availability gate -- the single source of truth, consulted by
/// both `shimEntryPoint` below and `Build.fs` (which decides whether the g++
/// line carries `-DBLADE_HAS_BLAS` plus the include/link flags). Two copies of
/// this predicate could disagree, and a disagreement is exactly the
/// configuration where a program emits `blade_linalg::` calls into a header
/// that will not compile.
///
/// Default-off is deliberate: BLAS may differ in the last ULP, and the
/// interpreter/oracle differentials demand byte-identical output, so Blade's
/// own emitted loops remain the verification truth.
/// `where repro` emission scope. While codegen emits the body of a repro
/// function this depth is nonzero and every routing gate below reads OFF: a
/// repro body's arithmetic must be Blade's own emitted loops (the
/// interpreter's operation sequence), never a library's -- BLAS differs in
/// the last ULP, LAPACK's eigenbasis is not even unique. A ref bumped and
/// restored by genFuncDef/genFuncDefAsLambda, not an env gate: it is
/// program-position state, and codegen is single-threaded. Lives HERE (not
/// CodeGenState) because this module compiles first and owns the gates.
let reproScopeDepth = ref 0
let reproScopeActive () = reproScopeDepth.Value > 0

let blasAvailable () : bool =
    not (reproScopeActive ()) && resolveBlasTier () <> TierOff

/// The LAPACK availability gate: a SEPARATE FUNCTION with its own C++ define
/// (`-DBLADE_HAS_LAPACK`) and Build.fs include-sniff arm. On the OpenBLAS
/// tiers it rides the BLAS resolution (OpenBLAS bundles LAPACKE); on
/// TierExplicit it requires its own BLADE_LAPACK_LINK, so a BLAS-only
/// install (e.g. BLIS without LAPACKE) dispatches contractions while `eigh`
/// still falls back to the synthesized Jacobi source.
///
/// NUMERICS WARNING: unlike BLAS routes, which differ from Blade's loops only
/// in the last ULP, an eigensolver's output is NOT UNIQUE (eigenvector signs
/// are arbitrary; a degenerate eigenvalue's subspace has no canonical basis).
/// The shim normalises the two determinate parts (descending order, sign fix)
/// but not the basis choice, so gate-ON results are correct but NOT
/// bit-reproducible against the native Jacobi path -- `interp` /
/// `diff-oracle` must never run with this gate set.
let lapackAvailable () : bool =
    if reproScopeActive () then false else
    match resolveBlasTier () with
    | TierOff -> false
    | TierExplicit -> (Toolchain.get "BLADE_LAPACK_LINK").IsSome
    | TierOpenBlasDir | TierSystem -> true

/// The compile-half / link-half flag strings for a g++ line that must reach
/// BLAS and/or LAPACK -- the EXPANSION of the resolved tier, separated from
/// the cheap gates above because this half touches the filesystem
/// (Platforms.findSharedLib). Build.compileCppWithExtra calls it exactly
/// when one of its include-sniffs fired with its gate on, so `wantsBlas` /
/// `wantsLapack` are sniff-AND-gate conjunctions and at least one is true.
///
/// Compile half: the `-DBLADE_HAS_*` defines (per header, so a BLAS-only
/// program stays distinguishable from a LAPACK-carrying one),
/// `-DBLADE_BLAS_MKL` under FlavorMkl, then `-I` per include dir. Link
/// half: library inputs, BLAS's first (linker order: LAPACKE may reference
/// BLAS symbols).
///
///   TierExplicit    -> BLADE_BLAS_INCLUDE / BLADE_LAPACK_INCLUDE dirs
///                      (PathSeparator-delimited lists), BLADE_BLAS_LINK /
///                      BLADE_LAPACK_LINK verbatim.
///   TierOpenBlasDir -> -I<dir>/include; link the shared library found under
///                      the prefix (Platforms.findSharedLib -- MinGW links a
///                      DLL's export table directly), falling back to
///                      -L<dir>/lib -lopenblas.
///   TierSystem      -> bare -lopenblas on the default search paths.
let blasBuildFlags (wantsBlas: bool) (wantsLapack: bool) : string * string =
    let defines =
        (if wantsBlas then " -DBLADE_HAS_BLAS" else "")
        + (if wantsLapack then " -DBLADE_HAS_LAPACK" else "")
        + (match blasFlavor () with FlavorMkl -> " -DBLADE_BLAS_MKL" | _ -> "")
    let splitPaths (v: string option) =
        match v with
        | None -> []
        | Some s ->
            s.Split(System.IO.Path.PathSeparator)
            |> Array.map _.Trim()
            |> Array.filter (fun p -> p <> "")
            |> Array.toList
    match resolveBlasTier () with
    | TierOff -> ("", "")  // gate/sniff mismatch upstream; emit nothing rather than guess
    | TierExplicit ->
        let includeFlags =
            splitPaths (Toolchain.get "BLADE_BLAS_INCLUDE")
            @ (if wantsLapack then splitPaths (Toolchain.get "BLADE_LAPACK_INCLUDE") else [])
            |> List.distinct
            |> List.map (sprintf " -I\"%s\"")
            |> String.concat ""
        let verbatim key =
            match Toolchain.get key with Some l -> " " + l.Trim() | None -> ""
        let linkFlags =
            verbatim "BLADE_BLAS_LINK"
            + (if wantsLapack then verbatim "BLADE_LAPACK_LINK" else "")
        (defines + includeFlags, linkFlags)
    | TierOpenBlasDir ->
        let dir = Toolchain.get "OPENBLAS_DIR" |> Option.defaultValue ""
        let incFlag = sprintf " -I\"%s\"" (System.IO.Path.Combine(dir, "include"))
        let linkFlags =
            match Platforms.findSharedLib dir "openblas" with
            | Some lib -> $" \"{lib}\""
            | None -> sprintf " -L\"%s\" -lopenblas" (System.IO.Path.Combine(dir, "lib"))
        (defines + incFlag, linkFlags)
    | TierSystem -> (defines, " -lopenblas")

/// The cuBLAS availability gate -- the device sibling of `blasAvailable`, with
/// its own environment variable:
///
///   BLADE_CUBLAS=1|on   -> force on
///   BLADE_CUBLAS=0|off  -> force off
///   unset               -> OFF (does NOT follow GPU presence)
///
/// Unset-means-off deliberately breaks the pattern its siblings follow
/// (falling back to "is OPENBLAS_DIR set"): offloading a contraction changes
/// the PERFORMANCE MODEL (a device allocation, an H2D copy, a D2H copy and a
/// free PER CALL), so a machine with a GPU has not thereby asked for its
/// `gram` to move across PCIe -- opt-in by explicit request only.
///
/// The toolchain conjunct (nvcc availability) is enforced at BUILD, not here:
/// `Build.fs` requires nvcc when it sniffs the `blade_linalg_cuda.hpp`
/// include.
///
/// Not conjoined with `cudaEmitModeEnabled ()`: that gate compiles `where
/// cuda` KERNELS for the device, this one offloads recognised L3
/// CONTRACTIONS. A program can want either without the other.
let cublasAvailable () : bool =
    not (reproScopeActive ())
    && (match System.Environment.GetEnvironmentVariable("BLADE_CUBLAS") with
        | "1" | "on" -> true
        | _ -> false)

// Backend mode

/// The codegen EMISSION MODE a dispatch decision is being made for.
///
/// The classifiers below (`classifyGram`, `(|BlasL1|_|)`, `(|BlasL2|_|)`) are
/// MODE-AGNOSTIC -- none mentions a backend -- and the mode is threaded into
/// the one funnel that turns a classification into a C++ name:
/// `shimEntryPoint`. Adding a target is a new arm there plus its entry-point
/// table, never a second copy of the matching logic.
type LinAlgBackend =
    /// Host CPU, cblas through `blade_linalg.hpp`. Availability is
    /// `blasAvailable ()` (BLADE_BLAS / OPENBLAS_DIR).
    | HostBlas
    /// Device, cuBLAS through `blade_linalg_cuda.hpp`, for the L3 NODE routes
    /// only (`gram` both arms, `matmul`). Availability is `cublasAvailable ()`.
    /// L1/L2 stay native: per-call offload (device alloc + H2D + cuBLAS + D2H
    /// + free) is PCIe-bound for `dot`/`gemv` (O(n)/O(mn) bytes moved for as
    /// many flops), while `gemm`/`syrk` move O(mn + nk) bytes for O(mnk)
    /// flops, so arithmetic intensity grows with the contracted extent and
    /// the transfer amortises.
    ///
    /// cuBLAS is column-major with no row-major mode: a row-major m x n pool
    /// with ld = n is bit-for-bit the column-major n x m matrix A^T, so
    /// reading Blade's pools as their own transposes and SWAPPING the
    /// operands computes the right thing with ZERO copies (swap table in
    /// `src/cpp/blade_linalg_cuda.hpp`). The same-array gram is the one trap:
    /// under the swap, Hermitian rank-k (`herk`) composes an upper<->lower
    /// fill-mode flip with the conjugation, verified at runtime against the
    /// host result. The LAPACK sibling cuSOLVER stays Native (separate
    /// library, handle, workspace-query protocol); the cuBLAS handle is
    /// owned by the shim (function-local static, freed at exit).
    | CudaBlas

// Precision

/// The BLAS precision letter an element type dispatches to: the second axis
/// of the (routine x precision x symmetry) matrix, the routine FAMILY chosen
/// by symmetry (see `LinAlgRoute`), the letter by this.
///
/// Everything not listed -- integers, booleans, structs -- has no BLAS
/// analogue and answers None, a decline at every classifier.
type Precision =
    /// float32 -- `s` routines.
    | PrecS
    /// float64 -- `d` routines.
    | PrecD
    /// complex<float> -- `c` routines.
    | PrecC
    /// complex<double> -- `z` routines.
    | PrecZ

/// The BLAS precision of an element type, or None when the type has no
/// routine family.
///
/// A UNIT annotation is stripped first: `Float64<meter>` stores as a bare
/// `double` -- the wrapper erases entirely at codegen, so the emitted pool is
/// `Array<double, 2>` either way -- and its precision letter is therefore `d`,
/// routing exactly like its unit-free twin. Sound because an array's elements
/// are uniform in unit as well as in width, so one letter still describes the
/// whole pool, and because the OUTPUT's units are computed by the type system
/// from the operand units, never read back off this answer. Two operands with
/// DIFFERENT units still agree here, which is the point: `gram` multiplies the
/// stored doubles, and meter x second is the result TYPE's business.
///
/// Matching bare `IRTScalar` only -- which is what this did before -- declined
/// EVERY route for every unit-annotated array, handing dimensioned physics
/// code scalar loops while the dimensionless twin got `dsyrk`. Silently: the
/// two paths agree to a ULP, so nothing but the emitted text showed it.
let precisionOf (t: IRType) : Precision option =
    match stripUnits t with
    | IRTScalar ETFloat32 -> Some PrecS
    | IRTScalar ETFloat64 -> Some PrecD
    | IRTScalar ETComplex64 -> Some PrecC
    | IRTScalar ETComplex128 -> Some PrecZ
    | _ -> None

/// The one-letter cblas prefix, used to build entry-point names.
let precisionLetter (p: Precision) : string =
    match p with
    | PrecS -> "s"
    | PrecD -> "d"
    | PrecC -> "c"
    | PrecZ -> "z"

/// Is this precision COMPLEX? For a same-array gram it selects `herk` over
/// `syrk` (Blade's complex gram is HERMITIAN -- its scalar loop already
/// conjugates the second factor), and for an inner product it is the reason
/// the route must be `dotu` and never `dotc`.
let isComplexPrecision (p: Precision) : bool =
    match p with
    | PrecC | PrecZ -> true
    | PrecS | PrecD -> false

// Descriptor

/// The BLAS level a routine sits at. Recorded on every policy row because the
/// routing decision is made per LEVEL, not per routine: L3 pays enormously
/// (blocking and microkernels are unreachable from emitted loops), L2 pays
/// modestly, L1-elementwise does not pay at all.
type BlasLevel =
    | L1
    | L2
    | L3

/// The routines this layer can name. `Dot`/`Gemv`/`Axpy`/`Scal` are declared,
/// not yet matched, so the policy table below can state their routing
/// decision explicitly rather than leaving it as an undocumented gap.
type LinAlgRoutine =
    /// C = A * B (general matrix product) -- `blade_gemm`.
    | Gemm
    /// C = A * A^T (real) or A * A^H (complex), one triangle -- `blade_syrk`.
    /// Complex is a HERMITIAN rank-k update (`cherk`/`zherk`), not symmetric:
    /// see `LinAlgRoute.RouteGramSame`.
    | Syrk
    /// y = A * x (matrix-vector).
    | Gemv
    /// `?gemv` with the matrix TRANSPOSED (conjugate-transposed for complex):
    /// the first half of `gram_apply` (t = B^H x). Its own row because its
    /// adapter is its own entry point, and a policy is stated per routine.
    | GemvT
    /// s = x . y (inner product).
    | Dot
    /// s = ||x||_2. Named so the policy table can state its decision; NOT
    /// matched (needs recognising a `sqrt` wrapped around a self-dot).
    | Nrm2
    /// y = alpha * x + y.
    | Axpy
    /// x = alpha * x.
    | Scal
    /// Symmetric / Hermitian eigendecomposition -- LAPACK, not BLAS.
    /// `?spev`/`?hpev` for packed operands, `?syev`/`?heev` for dense.
    | Eigh
    /// General dense linear solve A.x = b by partial-pivoted LU -- LAPACK
    /// `?gesv`. No symmetry axis (unlike Eigh): one routine covers every
    /// square operand, so the classifier's only decisions are shape and
    /// precision.
    | Solve

/// Where a recognised shape is actually EXECUTED.
type Routing =
    /// Through `blade_linalg.hpp`, which resolves to cblas or to the
    /// contract-preserving native fallback depending on the BUILD.
    | ViaShim
    /// Deliberately left to Blade's own emitted loop nest: a RECORDED
    /// DECISION ("matched but routed native"), not a missing feature.
    | Native

/// The role an operand plays in a call, kept separate from the IR expression
/// so the emission site can decide how to obtain the pointer without
/// re-deriving what the operand IS.
type OperandRole =
    /// Left/first factor.
    | RoleA
    /// Right/second factor.
    | RoleB
    /// Result.
    | RoleC

/// The concrete `blade_linalg.hpp` adapter a routed call lands on. A ROUTE is
/// narrower than a routine: `gram(A, A)` and `gram(A, B)` are both rank-k
/// updates in BLAS terms but reach different entry points because Blade's
/// symmetric result is PACKED triangular storage while its general result is
/// a dense pool -- a difference the shim (not codegen) has to absorb.
type LinAlgRoute =
    /// `blade_gram_same_<p>` -- one triangle into Blade's PACKED
    /// upper-triangular storage. REAL: C = A.A^T (`ssyrk`/`dsyrk`). COMPLEX:
    /// C = A.A^H, a HERMITIAN rank-k update (`cherk`/`zherk`) -- not a naming
    /// quibble: Blade's complex-gram scalar loop accumulates
    /// `A[i][k] * conj_scalar(A[j][k])`, exactly A.A^H, so binding it to
    /// `zsyrk` would silently compute a different matrix.
    | RouteGramSame
    /// `blade_gram_distinct_<p>` -- into a dense pool. REAL: C = A.B^T
    /// (`sgemm`/`dgemm`, transB = Trans). COMPLEX: C = A.B^H (`cgemm`/
    /// `zgemm`, transB = ConjTrans), matching the same scalar loop.
    | RouteGramDistinct
    /// `blade_matmul_<p>` -- A.B into a dense pool. f64 only at the SURFACE
    /// (`TypeCheck.inferMatmul`), so only the `d` instance is reachable;
    /// widening the surface is a separate language decision.
    | RouteMatmul
    /// `blade_dot_<p>` -- s = seed + x.y over two rank-1 pools. COMPLEX IS
    /// `dotu`, NEVER `dotc`: the matched shape
    /// `reduce(zip(x, y) under *, (+))` has kernel `a * b` with NO
    /// conjugation, while `dotc` computes sum conj(x_i).y_i, a different
    /// number (most visibly for `dot(x, x)`, real squared norm vs. complex
    /// sum x_i^2). A conjugating inner product needs its own surface form
    /// and is not implemented here.
    | RouteDot
    /// `blade_gemv_<p>` -- y = A.x, row skeleton in, rank-1 pool out. The
    /// matched body is `prodsum(row, x)`, which conjugates nothing, so every
    /// precision uses `CblasNoTrans`, including the complex ones.
    | RouteGemv
    /// `blade_gemv_t_<p>` -- t = B^H x, row skeleton in, rank-1 pool out; real
    /// is CblasTrans, complex CblasConjTrans (the conjugation Blade's scalar
    /// loop applies). The first half of `gram_apply`; the second is RouteGemv.
    | RouteGemvT
    /// `blade_eigh_packed_<p>` -- eigendecomposition of a rank-2 COMPACT
    /// operand, straight off `pool_base`. Real is `?spev`; complex is
    /// `?hpev` (Hermitian). ZERO-CONVERSION ROUTE: Blade's row-major-upper
    /// packed pool IS col-major-lower packed for a real symmetric matrix
    /// (measured n = 1..6), so LAPACK reads it as it stands.
    | RouteEighPacked
    /// `blade_eigh_dense_<p>` -- eigendecomposition of a dense rank-2 operand
    /// asserted symmetric (real, `?syev`) or Hermitian (complex, `?heev`).
    | RouteEighDense
    /// `blade_solve_<p>` -- A.x = b through `?gesv`, dense square A and a
    /// single right-hand side. There is no packed sibling and no same-array
    /// mode: `?spsv` wants a packed SYMMETRIC operand, which the surface
    /// cannot spell today (`MathElaborate.arrayShape` resolves plain axes
    /// only), and A's symmetry is not part of what this route asserts.
    ///
    /// UNLIKE the eigh routes, this one has a NATIVE TWIN: declining it emits
    /// Blade's own LU loop nest, which is the byte-identity truth the
    /// interpreter differential covers. So a `None` here costs the
    /// optimisation and nothing else -- the same relationship `RouteMatmul`
    /// has to its triple loop, and the opposite of `RouteEighDense`, whose
    /// absence means the operation is not emitted at all.
    | RouteSolve

/// An operand as classified: the IR expression, its role, and whether the
/// call consumes it transposed.
type LinAlgOperand = {
    Role: OperandRole
    Expr: IRExpr
    Transposed: bool
}

/// How the emission site obtains a pointer to a NEST-matched operand. A
/// nest's operands reach C++ by three different routes: an input array is
/// already named by `LoopNestCodeGen.InputArrayNames`, the output by
/// `OutputName`, but a value the kernel BODY references (a capture, or an
/// enclosing let-binding) has no name here -- only CodeGen's name map knows
/// it, so the IR expression is handed back untouched.
type NestOperandSource =
    /// A loop-nest input array, under the name the nest already uses.
    | FromNestArray of name: string
    /// A value referenced by the kernel body. CodeGen resolves it through its
    /// own name map; a failure to resolve is a decline, not a guess.
    | FromKernelRef of expr: IRExpr
    /// The nest's freshly-allocated output array.
    | FromNestOutput of name: string

/// How the extents of a call are named: resolved at RUNTIME off the
/// operands' `.extents[]`. The descriptor records WHICH extent of WHICH
/// operand, so the emission site spells the accessor and this module stays
/// free of C++ text.
type DimSource = {
    /// Index into the call's operand list.
    Operand: OperandRole
    /// Which axis of that operand.
    Axis: int
}

/// A classified linear-algebra call.
type LinAlgCall = {
    Routine: LinAlgRoutine
    Route: LinAlgRoute
    Level: BlasLevel
    // No `Routing` field: routing is a function of (routine x BACKEND), and
    // this descriptor is mode-agnostic, so a mode-dependent answer here would
    // be inconsistent. `shimEntryPoint` reads `routingOf backend
    // call.Routine` instead.
    /// NODE-matched routes only (gram/matmul): the operands as IR expressions.
    /// Empty for nest-matched routes, which use `NestOperands` instead.
    Operands: LinAlgOperand list
    /// NEST-matched routes only (dot/gemv): where each operand's pointer
    /// comes from, in the shim call's own argument order. Empty for node
    /// routes.
    NestOperands: (OperandRole * NestOperandSource) list
    /// Rows of the result. `None` for the nest routes, whose extents come
    /// from the built loop nest rather than from an operand axis this module
    /// could name.
    M: DimSource option
    /// Columns of the result (for Syrk this equals M).
    N: DimSource option
    /// The contracted extent.
    K: DimSource option
    /// Element type of the contraction, as classified.
    ElemType: IRType
    /// `ElemType`'s BLAS precision -- the second dispatch axis. Filled by
    /// every classifier and READ by `shimEntryPoint` to pick the routine
    /// letter, so a call can never reach an entry point of the wrong width.
    Precision: Precision
    /// True when the result is written into Blade's packed triangular
    /// (symmetric or Hermitian) storage rather than a dense pool.
    PackedTriangularResult: bool
}

// Policy table

/// The routing policy, stated once and explicitly.
///
/// L1-ELEMENTWISE STAYS NATIVE: `axpy`/`scal` shapes are bandwidth-bound and
/// the flat elementwise loop already vectorises them, so a BLAS call boundary
/// buys nothing. L1 REDUCTION shapes (`dot`, `nrm2`) and all of L2/L3 are the
/// paying routes, with L3 >> L2 because blocking and microkernels are
/// unreachable from generated loop code.
///
/// A row exists for every (routine x BACKEND) pair this module can NAME,
/// including ones it cannot yet MATCH or EMIT, so "matched but routed
/// native", "not yet matched" and "backend not implemented" are all
/// distinguishable by reading one table.
/// The shared rationale for every CudaBlas row that stays Native because it
/// is TRANSFER-BOUND -- a decision, not a "not implemented" note.
let private cudaPcieBound =
    "PCIe-bound under the per-call offload (device alloc + H2D + kernel + D2H + free). The transferred bytes and the flops are the SAME order for an L1/L2 shape, so the copy costs more than the arithmetic it feeds; only L3 amortises, where O(mn + nk) bytes buy O(mnk) flops. Stays on the host, where an OpenBLAS route is available under its own gate"

let policy : (LinAlgRoutine * BlasLevel * LinAlgBackend * Routing * string) list =
    [ Gemm, L3, HostBlas, ViaShim,
      "the one shape emitted loop code cannot approach; blocking/microkernels pay by orders of magnitude"
      Syrk, L3, HostBlas, ViaShim,
      "same as gemm, and it halves the work by computing one triangle -- which is also Blade's storage. COMPLEX instances are HERMITIAN (cherk/zherk), matching what Blade's own complex loop already computes"
      Gemv, L2, HostBlas, ViaShim,
      "pays modestly (bandwidth-bound but cache-blocked); MATCHED (Phase 5b) on the per-row prodsum-fiber nest"
      GemvT, L2, HostBlas, ViaShim,
      "the transposed sibling of gemv, same argument: `gram_apply`'s first half (t = B^H x) is one strided pass over B that a cache-blocked cblas ?gemv(Trans) pays for modestly; its native twin is the j-outer unit-stride accumulation into the t pool"
      Dot,  L1, HostBlas, ViaShim,
      "an L1 REDUCTION, unlike axpy/scal: the serial FP chain is the bottleneck and BLAS breaks it; MATCHED (Phase 5b) on reduce-over-deferred-zip-product. COMPLEX instances are dotu, NEVER dotc (Blade's fold does not conjugate). PRECEDENCE: an `omp`-licensed fold kernel WINS -- an explicit user reorder licence beats a dispatch heuristic, and under no-BLAS this route's fallback is serial, so firing would silently strip licensed parallelism"
      Nrm2, L1, HostBlas, ViaShim,
      "same paying L1-reduction argument as dot, but NOT MATCHED: recognising it means seeing a `sqrt` wrapped around a SELF-dot, and no sqrt-shape case exists in the classifier yet"
      Axpy, L1, HostBlas, Native,
      "bandwidth-bound elementwise; the flat loop already vectorises it, so a call boundary is pure loss"
      Scal, L1, HostBlas, Native,
      "same as axpy -- an elementwise scale is one vectorised pass either way"
      Eigh, L3, HostBlas, ViaShim,
      "LAPACK, not BLAS: an eigensolver is unreachable from emitted loop code at any quality, and Blade's synthesized cyclic Jacobi is O(sweeps*n^3) against LAPACK's blocked tridiagonal reduction. Gated separately (lapackAvailable / -DBLADE_HAS_LAPACK) and PERMANENTLY outside byte-identity: eigenvector sign and degenerate-subspace basis are not unique"
      Solve, L3, HostBlas, ViaShim,
      "LAPACK ?gesv, and the same O(n^3)-against-a-scalar-loop argument as gemm: Blade's emitted LU is an unblocked right-looking factorization, LAPACK's is blocked over ?trsm/?gemm panels. UNLIKE eigh, this row has a native twin -- gate off, Blade emits its own partial-pivoted LU, byte-pinned against Interp/ArrayOps.solveArray -- so the gate chooses an implementation rather than deciding whether the operation exists. The two arms agree to ~1e-14, not bit for bit (different pivot-application and update order), so byte-identity harnesses run gate-OFF like every other route"
      // CudaBlas: the two L3 rows are the only ones that flip, and the flip
      // is deliberately a table edit -- `blade test linalg` pins every row,
      // so landing (or losing) a device route requires changing a test.
      Gemm, L3, CudaBlas, ViaShim,
      "the paying offload: O(mnk) flops against O(mn + nk) transferred bytes, so arithmetic intensity grows with the contracted extent and the PCIe round trip amortises. Reaches `gram(A, B)` (A*B^T / A*B^H) and `matmul(A, B)` through cublas?gemm under the column-major operand SWAP -- a reinterpretation of Blade's own pools, not a transpose, so zero staging"
      Syrk, L3, CudaBlas, ViaShim,
      "same L3 argument as gemm, and it halves the work by computing one triangle. COMPLEX instances are HERMITIAN (cublasCherk/Zherk), matching what Blade's own complex loop computes. THE TRAP ROW: under the operand swap this composes an upper<->lower FILL-MODE FLIP with the conjugation -- verified at runtime against the host result, not argued"
      Gemv, L2, CudaBlas, Native, cudaPcieBound
      GemvT, L2, CudaBlas, Native, cudaPcieBound
      Dot,  L1, CudaBlas, Native, cudaPcieBound
      Nrm2, L1, CudaBlas, Native,
      "not matched on any backend (no sqrt-shape case exists), and " + cudaPcieBound
      Axpy, L1, CudaBlas, Native,
      "Native on EVERY backend for the host reason (bandwidth-bound elementwise, already vectorised by the flat loop) and additionally " + cudaPcieBound
      Scal, L1, CudaBlas, Native,
      "same as axpy: Native on every backend, and additionally " + cudaPcieBound
      // The device eigensolver sibling is cuSOLVER, not cuBLAS: a separate
      // library, handle and workspace-query protocol. Native because it is
      // UNIMPLEMENTED, not because it wouldn't pay -- a recorded follow-on.
      Eigh, L3, CudaBlas, Native,
      "cuSOLVER (cusolverDnDsyevd / cusolverDnZheevd), not cuBLAS: separate library, separate handle, explicit workspace query. NOT IMPLEMENTED -- a recorded follow-on rather than a policy decision, since an O(n^3) eigensolver would amortise its transfer exactly as the L3 rows above do. Gate-off, `math.eigh` keeps the synthesized Jacobi; gate-on with LAPACK it keeps the host route"
      Solve, L3, CudaBlas, Native,
      "cuSOLVER (cusolverDnDgetrf + cusolverDnDgetrs), not cuBLAS -- the same separate-library reason as the Eigh row, and NOT IMPLEMENTED for the same reason. Gate-off it is Blade's own emitted LU; gate-on with LAPACK it keeps the host ?gesv route" ]

/// The routing decision for a routine UNDER A BACKEND, from the table above.
let routingOf (backend: LinAlgBackend) (r: LinAlgRoutine) : Routing =
    policy
    |> List.tryPick (fun (rr, _, bb, routing, _) ->
        if rr = r && bb = backend then Some routing else None)
    |> Option.defaultValue Native

/// The BLAS level of a routine, from the table above. Backend-independent:
/// level is a property of the operation's arithmetic intensity, not of where
/// it runs, so it is read off whichever row names the routine first.
let levelOf (r: LinAlgRoutine) : BlasLevel =
    policy
    |> List.tryPick (fun (rr, lvl, _, _, _) -> if rr = r then Some lvl else None)
    |> Option.defaultValue L1

// Omp-vs-BLAS precedence policy

/// Which side wins when a nest that MATCHES a route also carries an explicit
/// `omp` licence on its kernel.
type OmpPrecedence =
    /// THE USER'S PRAGMA WINS. An `omp` request on the matched kernel declines
    /// the dispatch outright; the nest keeps its `#pragma omp` (or its chunked
    /// parallel fold) exactly as written.
    | OmpWins
    /// THE BLAS ROUTE WINS. The dispatch fires even though the kernel asked
    /// for `omp` -- the licence is honoured by the ROUTINE's own threading
    /// rather than by a pragma on a nest that no longer exists.
    | BlasWins

/// The precedence policy, stated once and per LEVEL -- because the answer is
/// OPPOSITE at L1/L2 and at L3, and the argument for each only reads as an
/// argument when the two sit side by side.
///
/// THIS TABLE EXISTS BECAUSE THE RULE USED TO BE A HARDCODED MATCHER GUARD.
/// `(|BlasL1|_|)` and `(|BlasL2|_|)` each carried their own
/// `ompRequested -> None`, which read as a UNIVERSAL rule; copying it into the
/// L3 nest matcher would have made that pattern fire on nothing, since a real
/// covariance/gram kernel carries `omp`. Stating precedence per level makes
/// "omp wins" and "BLAS wins" both auditable in one place, and makes flipping
/// one a test edit rather than a silent change of behaviour.
let ompPolicy : (BlasLevel * OmpPrecedence * string) list =
    [ L1, OmpWins,
      "an explicit user reorder licence outranks a dispatch heuristic. `blade_dot`'s no-BLAS fallback is a SERIAL accumulator, so firing here would silently convert declared parallelism into serial code in every build without OpenBLAS -- a performance regression with nothing in the emitted text to show for it"
      L2, OmpWins,
      "a parallel row map is exactly what `#pragma omp parallel for` over the outer level already gives, so the dispatch adds cache blocking and takes away the user's pragma. Same no-BLAS-fallback-is-serial argument as dot"
      L3, BlasWins,
      "INVERTED, and deliberately: a threaded scalar triangle is NOT 'what the routine already gives'. OpenBLAS's `?syrk` is itself multithreaded AND register-blocked AND packed, typically an order of magnitude past a `schedule(dynamic)` scalar nest, so honouring the pragma instead of the route would cost the user the very parallelism they asked for. The licence is discharged by the routine, not dropped" ]

/// The precedence for a level, from the table above. Defaults to `OmpWins`:
/// an unstated level keeps the user's pragma, which is the conservative half.
let ompPrecedenceOf (lvl: BlasLevel) : OmpPrecedence =
    ompPolicy
    |> List.tryPick (fun (ll, p, _) -> if ll = lvl then Some p else None)
    |> Option.defaultValue OmpWins

/// Does an `omp` request on the matched kernel DECLINE this level's dispatch?
/// The one predicate every nest matcher consults, so no matcher states the
/// rule for itself.
let ompRequestDeclines (lvl: BlasLevel) : bool = ompPrecedenceOf lvl = OmpWins

/// The C++ entry point a routed call lands on. Kept here (not in CodeGen) so
/// every shim function name this compiler can emit is enumerable from one
/// place.
///
/// THE AVAILABILITY GATE, THE BACKEND MODE and THE PRECISION LETTER are all
/// resolved HERE AND ONLY HERE: every route funnels through this function on
/// its way to emitted text, so one conjunct disables dispatch globally, and
/// the classifiers above stay backend- and precision-agnostic. NOT folded
/// into `routingOf`: that field is POLICY, pinned by tests independent of
/// whether OpenBLAS is installed, while availability is a property of the
/// BUILD -- keeping them separate lets the policy table stay a readable,
/// environment-independent statement.
///
/// A call that classifies but gets no entry point is a DECLINED DISPATCH: all
/// four emission sites already spell that case (gram/matmul fall to their own
/// scalar loops, dot/gemv fall through to the loop-nest emitters), so "gate
/// off", "backend not implemented" and "shape not recognised" all reach the
/// same, already exercised, code.
///
/// The precision letter is appended here so the emitted TEXT names the exact
/// routine family and width (`blade_gram_same_z` is visibly `zherk`), making
/// the routing decision assertable from generated source.
let shimEntryPoint (backend: LinAlgBackend) (call: LinAlgCall) : string option =
    let available =
        match backend with
        // The LAPACK routes ride their own gate and their own header, so
        // availability is per-ROUTINE, not per-backend alone.
        | HostBlas when call.Routine = Eigh || call.Routine = Solve -> lapackAvailable ()
        | HostBlas -> blasAvailable ()
        // The device mode has its own gate, environment variable and default
        // (OFF). See `cublasAvailable`.
        | CudaBlas -> cublasAvailable ()
    if not available then None else
    match routingOf backend call.Routine with
    | Native -> None
    | ViaShim ->
        let p = precisionLetter call.Precision
        match backend with
        | HostBlas ->
            match call.Route with
            | RouteGramSame -> Some $"blade_linalg::blade_gram_same_{p}"
            | RouteGramDistinct -> Some $"blade_linalg::blade_gram_distinct_{p}"
            | RouteMatmul -> Some $"blade_linalg::blade_matmul_{p}"
            | RouteDot -> Some $"blade_linalg::blade_dot_{p}"
            | RouteGemv -> Some $"blade_linalg::blade_gemv_{p}"
            | RouteGemvT -> Some $"blade_linalg::blade_gemv_t_{p}"
            // Different namespace AND different header: `blade_lapack.hpp`
            // carries its own `#ifndef BLADE_HAS_LAPACK #error`, so a program
            // that names these advertises a LAPACK dependency distinct from a
            // BLAS one.
            | RouteEighPacked -> Some $"blade_lapack::blade_eigh_packed_{p}"
            | RouteEighDense -> Some $"blade_lapack::blade_eigh_dense_{p}"
            | RouteSolve -> Some $"blade_lapack::blade_solve_{p}"
        // The device entry-point table is named PER ROUTE, not per cuBLAS
        // routine, with argument lists identical to the host adapters' (same
        // order, same skeleton + pool capacity pairs), so a backend cannot
        // drift into a different calling convention without the emitter
        // noticing. Which cuBLAS routine each lands on, and the column-major
        // swap that gets it there, is documented row by row in
        // `blade_linalg_cuda.hpp`.
        //
        // `extern "C"` at global scope, hence NO namespace qualifier: the
        // definitions are compiled by nvcc into a separate translation unit
        // and reached across an unmangled linkage boundary, exactly like the
        // `__launch_*` wrappers the `where cuda` emitters already use. The
        // `blade_cuda_` prefix is the namespace.
        | CudaBlas ->
            match call.Route with
            | RouteGramSame -> Some $"blade_cuda_gram_same_{p}"
            | RouteGramDistinct -> Some $"blade_cuda_gram_distinct_{p}"
            | RouteMatmul -> Some $"blade_cuda_matmul_{p}"
            // Unreachable: `routingOf CudaBlas` answers Native for Dot / Gemv /
            // Eigh, so the ViaShim arm above already declined. Spelled out so
            // that flipping one of those policy rows produces a compile error
            // here -- a missing entry point rather than a silently wrong one.
            | RouteDot | RouteGemv | RouteGemvT | RouteEighPacked | RouteEighDense | RouteSolve -> None

/// Resolve a NODE-matched call (`gram`, `matmul`) to the backend it runs on
/// and the C++ entry point it lands on -- the one place the emission-mode
/// choice is made.
///
/// Device first when its gate is on, host next, native if neither. Asking
/// `shimEntryPoint CudaBlas` unconditionally would be wrong: for any route
/// whose CudaBlas policy row is `Native`, a caller that stopped there would
/// emit Blade's loops even where the HOST route was available -- turning
/// "BLADE_CUBLAS=1" into a silent regression. Chaining makes the device gate
/// purely additive.
///
/// The backend is RETURNED, not just the name, because the emission site has
/// to name the matching header (`blade_linalg.hpp` vs
/// `blade_linalg_cuda.hpp`); deriving it from the entry-point string would be
/// a second, weaker copy of the same decision.
///
/// Only the NODE routes go through here. `dot` / `gemv` / `eigh` ask for
/// `HostBlas` directly at their own sites: their CudaBlas rows are `Native`
/// by policy, so the chain would resolve to HostBlas anyway.
let resolveNodeRoute (call: LinAlgCall) : (LinAlgBackend * string) option =
    let ask (b: LinAlgBackend) = shimEntryPoint b call |> Option.map (fun e -> (b, e))
    match (if cublasAvailable () then ask CudaBlas else None) with
    | Some r -> Some r
    | None -> ask HostBlas

// Classification entry points

/// The precision two operands AGREE on, or None. Mixed precisions decline
/// rather than promote: BLAS has no mixed-width routine, and silently
/// widening one operand would be a storage change the caller never asked for.
/// Blade's own scalar loops promote per the element-type rules, which is
/// exactly what a declined route falls back to, so declining costs the
/// optimisation and changes no value.
let private agreedPrecision (a: IRType) (b: IRType) : Precision option =
    match precisionOf a, precisionOf b with
    | Some pa, Some pb when pa = pb -> Some pa
    | _ -> None

let private elemOf (e: IRExpr) : IRType option =
    match typeOf e with
    | ArrayElem a -> Some a.ElemType
    | _ -> None

/// Classify `gram(l, r)`.
///
///   sameArray -> Syrk: square m x m written into Blade's PACKED
///                upper-triangular storage. REAL is A.A^T (`ssyrk`/`dsyrk`);
///                COMPLEX is A.A^H, a HERMITIAN rank-k update
///                (`cherk`/`zherk`) -- what Blade's own complex scalar loop
///                already computes, since it conjugates the second factor.
///   distinct  -> Gemm with B transposed: C(m x p) = A(m x n) . B(p x n)^T for
///                real, and A . B^H for complex (`ConjTrans`), matching the
///                same scalar loop. Dense result.
///
/// Returns None (caller keeps its scalar loops) when either operand is not an
/// array, when their element types have no BLAS precision (integers,
/// structs), or when the two precisions disagree.
let classifyGram (l: IRExpr) (r: IRExpr) (sameArray: bool) : LinAlgCall option =
    match elemOf l, elemOf r with
    | Some le, Some re ->
        match agreedPrecision le re with
        | None -> None
        | Some prec ->
        if sameArray then
            Some { Routine = Syrk
                   Route = RouteGramSame
                   Level = L3
                   Operands = [ { Role = RoleA; Expr = l; Transposed = false } ]
                   NestOperands = []
                   // C is m x m from A's leading axis; the contracted extent is
                   // A's trailing axis.
                   M = Some { Operand = RoleA; Axis = 0 }
                   N = Some { Operand = RoleA; Axis = 0 }
                   K = Some { Operand = RoleA; Axis = 1 }
                   ElemType = le
                   Precision = prec
                   PackedTriangularResult = true }
        else
            Some { Routine = Gemm
                   Route = RouteGramDistinct
                   Level = L3
                   // `Transposed` on B is the REAL reading; for a complex
                   // precision the emission site spells it ConjTrans, which is
                   // the same flag with the conjugation the scalar loop applies.
                   Operands = [ { Role = RoleA; Expr = l; Transposed = false }
                                { Role = RoleB; Expr = r; Transposed = true } ]
                   NestOperands = []
                   M = Some { Operand = RoleA; Axis = 0 }
                   N = Some { Operand = RoleB; Axis = 0 }
                   K = Some { Operand = RoleA; Axis = 1 }
                   ElemType = le
                   Precision = prec
                   PackedTriangularResult = false }
    | _ -> None

/// Classify `gram_apply(a, b, x)` = a . (b^H . x) into its TWO L2 halves:
/// `t = b^H x` through the TRANSPOSED gemv adapter (`GemvT`: CblasTrans for
/// real, CblasConjTrans for complex -- the conjugation the scalar loop
/// applies, exactly as the distinct gram's gemm does), then `y = a t` through
/// the plain one. Each half resolves its own backend like a node route (both
/// CudaBlas rows are Native, so they land on the host adapters or the
/// loops). None when any operand is not an array or the three precisions
/// disagree; the emitter then keeps its scalar loops for both halves.
let classifyGramApply (a: IRExpr) (b: IRExpr) (x: IRExpr) : (LinAlgCall * LinAlgCall) option =
    match elemOf a, elemOf b, elemOf x with
    | Some ae, Some be, Some xe ->
        (match agreedPrecision ae be, agreedPrecision be xe with
         | Some prec, Some _ ->
            let half (routine: LinAlgRoutine) (route: LinAlgRoute) (ops: LinAlgOperand list) =
                { Routine = routine
                  Route = route
                  Level = L2
                  Operands = ops
                  NestOperands = []
                  M = Some { Operand = RoleA; Axis = 0 }
                  N = None
                  K = Some { Operand = RoleA; Axis = 1 }
                  ElemType = ae
                  Precision = prec
                  PackedTriangularResult = false }
            let tOps = [ { Role = RoleA; Expr = b; Transposed = true }; { Role = RoleB; Expr = x; Transposed = false } ]
            let yOps = [ { Role = RoleA; Expr = a; Transposed = false } ]
            Some (half GemvT RouteGemvT tOps, half Gemv RouteGemv yOps)
         | _ -> None)
    | _ -> None

/// Classify `matmul(a, b)`: C(m x n) = A(m x k) * B(k x n), dense result, no
/// transposes. The first-class intrinsic's only classification.
///
/// Only the `d` instance is reachable: `TypeCheck.inferMatmul` requires
/// Float64 elements at the SURFACE (BL3999), so a f32/complex/int call never
/// gets here. The precision generalisation below is a backstop that keeps
/// this classifier honest if the surface is ever widened -- but widening it
/// is a LANGUAGE decision (what should `matmul` mean for complex operands:
/// A.B, or A.B^H?) and is deliberately not taken here.
let classifyMatmul (a: IRExpr) (b: IRExpr) : LinAlgCall option =
    match elemOf a, elemOf b with
    | Some ae, Some be ->
        match agreedPrecision ae be with
        | None -> None
        | Some prec ->
            Some { Routine = Gemm
                   Route = RouteMatmul
                   Level = L3
                   Operands = [ { Role = RoleA; Expr = a; Transposed = false }
                                { Role = RoleB; Expr = b; Transposed = false } ]
                   NestOperands = []
                   M = Some { Operand = RoleA; Axis = 0 }
                   N = Some { Operand = RoleB; Axis = 1 }
                   K = Some { Operand = RoleA; Axis = 1 }
                   ElemType = ae
                   Precision = prec
                   PackedTriangularResult = false }
    | _ -> None

/// Is this index slot an ORDINARY dense axis: one index component, no
/// symmetry, no reserved kind, no reserved tag, no dependence on an outer
/// loop index?
///
/// Every one of those five refusals is load-bearing for a BLAS route:
/// symmetry means the pool is a packed triangle rather than a rectangle; a
/// reserved `IxKind` (compound / sparse / ragged / dep / group / orbit) means
/// the axis iterates something other than `[0, extent)`; a `__`-prefixed tag
/// marks a halo window or kind sentinel; and a dependence makes the level
/// triangular. BLAS knows about none of these.
let private isPlainDenseAxis (ix: IRIndexType) =
    ix.Rank = 1
    && ix.Symmetry = SymNone
    && ix.IxKind = IxKPlain
    && ix.Dependencies.IsEmpty
    && (match ix.Tag with Some t -> not (t.StartsWith "__") | None -> true)

/// Classify `eigh(S)` -- the symmetric/Hermitian eigendecomposition -- on the
/// OPERAND'S TYPE alone. Symmetry is the axis that picks the routine FAMILY:
///
///   | operand                          | s      | d      | c       | z       |
///   |-----------------------------------|--------|--------|---------|---------|
///   | rank-2 compact SymSymmetric      | sspev  | dspev  | DECLINE | DECLINE |
///   | rank-2 compact SymHermitian      | sspev  | dspev  | chpev   | zhpev   |
///   | dense rank-2 (symmetry ASSUMED)  | ssyev  | dsyev  | cheev   | zheev   |
///   | anything else (antisym, int...)  | DECLINE                            |
///
/// THE COMPLEX-SYMMETRIC TRAP: a complex array carrying `SymSymmetric` is
/// complex-SYMMETRIC (A = A^T, no conjugation), which is NOT Hermitian and not
/// normal in general. LAPACK has no eigensolver for it -- no `zsyev`, no
/// `zspev`. Its spectrum is COMPLEX and its eigenvectors are not orthogonal,
/// so the right routine is the general `zgeev`, a different result TYPE, not
/// a precision swap -- exactly the row a precision-only widening would get
/// wrong, as `zsyrk`-for-`zherk` would. The route DECLINES to the native path.
///
/// REAL + `SymHermitian` routes to the REAL packed entry point as a theorem,
/// not a convenience: a real Hermitian matrix IS symmetric, and `?spev` is
/// the routine for it.
///
/// DENSE ASSUMES SYMMETRY, inheriting the surface's own domain (`math.eigh`
/// documents "symmetry is ASSUMED, not checked"); a dense COMPLEX operand is
/// assumed HERMITIAN, the only reading under which the operation is defined.
let classifyEigh (operand: IRArrayType) : LinAlgCall option =
    if operand.IsVirtual then None else
    match precisionOf operand.ElemType with
    | None -> None                                   // int, bool, struct: no family
    | Some prec ->
        let complexOperand = isComplexPrecision prec
        match operand.IndexTypes with
        // rank-2 COMPACT group: the packed route
        | [ ix ] when ix.Rank = 2 && ix.IxKind = IxKPlain && ix.Dependencies.IsEmpty ->
            let packedOk =
                match ix.Symmetry with
                | SymSymmetric -> not complexOperand   // the complex-symmetric trap
                | SymHermitian -> true                 // real Hermitian == symmetric
                | _ -> false                           // antisym / wreath: no route
            if not packedOk then None
            else
                Some { Routine = Eigh
                       Route = RouteEighPacked
                       Level = L3
                       Operands = []
                       NestOperands = []
                       M = None; N = None; K = None
                       ElemType = operand.ElemType
                       Precision = prec
                       PackedTriangularResult = false }
        // dense rank-2: symmetry asserted by the caller
        | [ i0; i1 ] when isPlainDenseAxis i0 && isPlainDenseAxis i1 ->
            Some { Routine = Eigh
                   Route = RouteEighDense
                   Level = L3
                   Operands = []
                   NestOperands = []
                   M = None; N = None; K = None
                   ElemType = operand.ElemType
                   Precision = prec
                   PackedTriangularResult = false }
        | _ -> None

/// Classify `solve(A, b)` -- A.x = b by LU -- on the two operands' types.
///
/// There is no symmetry matrix here, and that absence is the whole difference
/// from `classifyEigh`. `?gesv` factorizes ANY square matrix, so symmetry
/// selects nothing: a symmetric operand is simply a matrix whose LU happens to
/// be cheap to have computed differently. What the classifier does require is
/// that both operands be ORDINARY DENSE pools of the agreed precision --
///
///   * A: two plain dense axes (`isPlainDenseAxis`). A rank-2 COMPACT group is
///     declined rather than routed to `?spsv`: that routine wants a packed
///     symmetric operand AND writes a packed factor, while Blade's surface
///     cannot spell a compact `solve` argument at all today
///     (`MathElaborate.arrayShape` resolves plain axes one at a time). Adding
///     the route before the surface exists would be an unreachable branch.
///   * b: one plain dense axis. `readOnlyBlasVector`'s `IxKIrreps` latitude is
///     deliberately NOT taken -- b is COPIED INTO and returned as x's initial
///     contents, an in-place role, not the read-only one that admission was
///     measured for.
///
/// Deliberately does NOT check that the extents agree: `TypeCheck.inferSolve`
/// owns that, and restating it here would be a second place for it to be wrong.
let classifySolve (a: IRArrayType) (b: IRArrayType) : LinAlgCall option =
    if a.IsVirtual || b.IsVirtual then None else
    match a.IndexTypes, b.IndexTypes with
    | [ a0; a1 ], [ b0 ] when isPlainDenseAxis a0 && isPlainDenseAxis a1 && isPlainDenseAxis b0 ->
        match agreedPrecision a.ElemType b.ElemType with
        | None -> None
        | Some prec ->
            Some { Routine = Solve
                   Route = RouteSolve
                   Level = L3
                   Operands = []
                   NestOperands = []
                   // A is n x n and b is n: one extent describes the whole
                   // call, recorded as M (rows of the system). N is the
                   // right-hand-side count, which this surface pins at 1, so
                   // there is no operand axis to name it with.
                   M = Some { Operand = RoleA; Axis = 0 }
                   N = None
                   K = None
                   ElemType = a.ElemType
                   Precision = prec
                   PackedTriangularResult = false }
    | _ -> None

/// The single entry point CodeGen calls: classify whatever node it is holding.
/// Returns None for everything this layer does not (yet) recognise, which is
/// the caller's signal to emit its ordinary loop nest.
let classify (e: IRExpr) : LinAlgCall option =
    match e with
    | IRGram (l, r, sameArray) -> classifyGram l r sameArray
    | IRMatmul (a, b) -> classifyMatmul a b
    | IRSolve (a, b) ->
        (match typeOf a, typeOf b with
         | ArrayElem aa, ArrayElem bb -> classifySolve aa bb
         | _ -> None)
    | _ -> None

// Nest matching -- shared shape predicates

/// A non-virtual, BLAS-precision array of exactly `rank` ordinary dense axes
/// (see `isPlainDenseAxis`); answers its precision so the caller can require
/// agreement across operands. Virtual operands are refused because a
/// `range`/`reverse` view has no pool to point at -- it inlines into index
/// arithmetic at every use.
let private denseBlasArrayOfRank (rank: int) (t: IRArrayType) : Precision option =
    if t.IsVirtual then None
    elif List.length t.IndexTypes <> rank then None
    elif not (t.IndexTypes |> List.forall isPlainDenseAxis) then None
    else precisionOf t.ElemType

/// A rank-1 f64 operand that is only ever READ elementwise -- never iterated
/// as a loop level and never peeled. `IxKIrreps` is admitted HERE and nowhere
/// else: an irreps axis is block-structured but ordinary contiguous dense, so
/// `v.data[t]` is the identical object `v[t]` denotes (the corpus shape:
/// `ml-equiv/018`, `019`: `prodsum(row, fx)` with `fx : Array<Float like
/// IrrepsIdx<...>>`). Deliberately NOT admitted for an array the nest
/// ITERATES (dot's two operands, gemv's matrix, gemv's output): those
/// positions decide loop bounds and peel structure, where the extra tag is a
/// difference this classifier has not established is inert.
let private readOnlyBlasVector (t: IRArrayType) : Precision option =
    if t.IsVirtual then None else
    match t.IndexTypes with
    | [ ix ] when ix.Rank = 1
                  && ix.Symmetry = SymNone
                  && (ix.IxKind = IxKPlain || ix.IxKind = IxKIrreps)
                  && ix.Dependencies.IsEmpty -> precisionOf t.ElemType
    | _ -> None

/// The one loop level of a depth-1 nest, provided it really iterates
/// `[0, extent)`: rectangular (no bound dependencies, no strict offset) and
/// not a fused joint level (whose bound is a PRODUCT of source extents, i.e.
/// a different iteration space than the one the routine's `m`/`n` describe).
let private singleRectangularLevel (cg: LoopNestCodeGen) : LoopIndexBinding option =
    match cg.Bindings with
    | [ b ] when b.BoundDependencies.IsEmpty && b.StrictOffset = 0 && b.FusedRank.IsNone ->
        Some b
    | _ -> None

/// Gates that hold for EVERY nest-matched route: the nest must be an ordinary
/// serial traversal of a real pool, with none of the modes that change what
/// the loop body means or where it runs.
///
/// `HasReynolds`/`IsAntisymmetric` -- the body reads PERMUTED coordinates.
/// `MpiSlab` -- the outer level iterates a rank slab, not the whole extent.
/// A streamed source has no materialised pool at all.
let private nestModeOk (streamedCount: int) (cg: LoopNestCodeGen) =
    streamedCount = 0
    && not cg.MpiSlab
    && not cg.HasReynolds
    && not cg.IsAntisymmetric

// (|BlasL1|_|) -- dot

/// What the caller must tell the L1 pattern about the FOLD, which lives on
/// the reduce node rather than on the nest.
type DotFoldFacts = {
    /// The fold kernel's body is exactly the builtin `+` over its two
    /// parameters (`CodeGen.foldKernelBuiltinOp` = `Some IRAdd`). Anything
    /// else -- a user lambda, `*`, `max` -- is a different accumulation and
    /// `blade_dot` would compute the wrong thing.
    FoldIsBuiltinAdd: bool
    /// The fold kernel carried `where ... omp`. See the PRECEDENCE note below.
    FoldRequestedOmp: bool
}

/// `s = reduce(<unforced zip of x and y under `*`>, (+))`  ->  `blade_dot`.
///
/// SHAPE MATCHED, exactly:
///   * depth-1 rectangular nest accumulating through a fold wrapper
///     (`FoldWrapper.IsSome`, the reduce-over-deferred-computation path);
///   * exactly TWO input arrays, both real f64 of ONE ordinary dense axis,
///     with exactly two element bindings, one per operand, each a real
///     full-depth scalar peel (`ArrayRank = 1`, `RankComponent = 0`);
///   * the kernel body is exactly `p_a * p_b` over those two distinct peel
///     parameters (no capture, index variable or third term);
///   * the fold kernel is the builtin `+`.
///
/// PRECEDENCE is READ OFF `ompPolicy`, never decided here: L1 is `OmpWins`,
/// so an `omp`-licensed fold kernel DECLINES this pattern and the chunked
/// parallel fold keeps the nest -- an explicit user reorder licence outranks
/// a dispatch heuristic, since firing here in a no-BLAS build would silently
/// convert licensed parallelism into serial code.
///
/// The SEED is not part of the match: `reduce`'s seed (the implicit `(+)`
/// identity, or a user `init`) is passed through to the shim, whose native
/// fallback starts its accumulator from it, making the fallback
/// byte-identical to the loop for ANY seed, not only `0.0`.
let (|BlasL1|_|) ((streamedCount, facts, operandTypes, cg): int * DotFoldFacts * IRArrayType list * LoopNestCodeGen)
        : LinAlgCall option =
    if not facts.FoldIsBuiltinAdd then None
    // Precedence from the POLICY TABLE (`ompPolicy`), not from a rule stated
    // here: L1 is `OmpWins`, so the chunked parallel fold keeps the nest.
    elif facts.FoldRequestedOmp && ompRequestDeclines L1 then None
    elif cg.FoldWrapper.IsNone || cg.FoldChunk.IsSome then None
    elif not (nestModeOk streamedCount cg) then None
    else
    match singleRectangularLevel cg with
    | None -> None
    | Some level ->
        let names = cg.InputArrayNames
        if List.length names <> 2 then None
        // Both operands: real f64, ONE ordinary dense axis, non-virtual --
        // rules out sparse/compound/dependent/ragged/orbit axes, none of
        // which `blade_dot`'s pointer pair can describe.
        elif List.length operandTypes <> 2 then None
        else
        match operandTypes |> List.map (denseBlasArrayOfRank 1) with
        | [ Some p0; Some p1 ] when p0 = p1 ->
            let prec = p0
            match level.Elements with
            | [ e0; e1 ] when e0.ArrayPosition <> e1.ArrayPosition ->
                let peelOk (e: ElementBinding) =
                    (match e.Virtual with RealArray -> true | _ -> false)
                    && e.RankComponent = level.Level
                    && e.ArrayRank = 1
                    && e.DimIndex = 0
                    && e.ArrayPosition >= 0 && e.ArrayPosition < 2
                    && e.ArrayName = List.item e.ArrayPosition names
                    && (match e.SlotTag with Some t -> not (t.StartsWith "__") | None -> true)
                    && precisionOf e.ArrayElemType = Some prec
                if not (peelOk e0 && peelOk e1) then None else
                // Body is exactly `<peel> * <peel>` over the two distinct
                // params. NO CONJUGATION appears here, at any precision --
                // why a complex instance of this route must be `dotu` and
                // never `dotc`. See `RouteDot`.
                match cg.KernelExpr with
                | IRBinOp (_, IRMul, IRVar (lId, _), IRVar (rId, _)) when lId <> rId ->
                    let byParam id =
                        [ e0; e1 ] |> List.tryFind (fun e -> e.ParamVarId = id)
                    match byParam lId, byParam rId with
                    | Some le, Some re ->
                        Some { Routine = Dot
                               Route = RouteDot
                               Level = L1
                               Operands = []
                               NestOperands = [ RoleA, FromNestArray le.ArrayName
                                                RoleB, FromNestArray re.ArrayName ]
                               M = None; N = None; K = None
                               ElemType = le.ArrayElemType
                               Precision = prec
                               PackedTriangularResult = false }
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

// (|BlasL2|_|) -- gemv

/// `y = method_for(A) <@> lambda(row) -> prodsum(row, x) |> compute`
///   ->  `blade_gemv`.
///
/// WHY THIS SHAPE: it is how matrix-vector actually appears in Blade
/// programs -- `prodsum` is a first-class fused product-sum over rank-1
/// fibers, and the per-row apply is the only way to reach it with a matrix
/// (corpus: `ml-equiv/018_certificate_derive_linear.blade`,
/// `019_certificate_derive_tp.blade`). No `matvec` keyword, no rank-2/rank-1
/// contraction node to hook instead.
///
/// SHAPE MATCHED, exactly:
///   * a depth-1 rectangular MATERIALISING nest (no fold wrapper);
///   * exactly ONE input array, real f64, of TWO ordinary dense axes, with
///     exactly ONE element binding: a real peel of dim 0 of the rank-2 array
///     (`ArrayRank (2) > depth (1)` makes it a FIBER argument, not a scalar
///     leaf, separating this from the flat-elementwise shape);
///   * the output is a real f64 array of ONE ordinary dense axis;
///   * the kernel body is exactly `prodsum(<the peeled row>, <v>)`, row
///     FIRST, `v` an `IRVar` of a real f64 array with one ordinary dense
///     axis.
///
/// WHY THE ROW MUST COME FIRST: `IRProdSum` takes its loop bound from its
/// FIRST operand (`<arg0>.extents[0]`), which with the row first is A's
/// trailing extent, the `n` this routine is defined over. Reversed
/// (`prodsum(x, row)`) the loop would be bounded by the VECTOR's extent
/// instead -- a different count whenever the two disagree, which Blade's
/// unify does not rule out -- so declining is the honest answer.
///
/// PRECEDENCE is READ OFF `ompPolicy`, same answer as dot: L2 is `OmpWins`,
/// so an `omp` request on the row kernel declines, since a parallel row map is
/// exactly what `#pragma omp parallel for` over the outer level already gives,
/// and the no-BLAS fallback is serial. (L3 answers the opposite -- see
/// `(|BlasL3|_|)`.)
let (|BlasL2|_|) ((streamedCount, ompRequested, operandTypes, cg): int * bool * IRArrayType list * LoopNestCodeGen)
        : LinAlgCall option =
    if cg.FoldWrapper.IsSome || cg.FoldChunk.IsSome then None
    // Precedence from the POLICY TABLE (`ompPolicy`), not from a rule stated
    // here: L2 is `OmpWins`, so the nest keeps the pragma the user asked for.
    elif ompRequested && ompRequestDeclines L2 then None
    elif not (nestModeOk streamedCount cg) then None
    else
    match singleRectangularLevel cg, cg.InputArrayNames, operandTypes with
    | Some level, [ aName ], [ aTy ] when (denseBlasArrayOfRank 2 aTy).IsSome ->
        let prec = (denseBlasArrayOfRank 2 aTy).Value
        // Output: rank-1 dense, SAME precision as the matrix.
        let outOk =
            match cg.OutputType with
            | ArrayElem outTy -> denseBlasArrayOfRank 1 outTy = Some prec
            | _ -> false
        if not outOk then None else
        match level.Elements with
        | [ e ] when (match e.Virtual with RealArray -> true | _ -> false)
                     && e.ArrayPosition = 0
                     && e.ArrayName = aName
                     && e.RankComponent = level.Level
                     && e.ArrayRank = 2
                     && e.DimIndex = 0
                     && precisionOf e.ArrayElemType = Some prec
                     && (match e.SlotTag with Some t -> not (t.StartsWith "__") | None -> true) ->
            (match cg.KernelExpr with
             | IRProdSum [ IRVar (rowId, _); (IRVar (vecId, _) as vecExpr) ] when rowId = e.ParamVarId ->
                // The second operand must be a value from OUTSIDE the kernel:
                // a capture or an enclosing binding, never a kernel parameter.
                // `prodsum(er, er)` (the per-row SELF-dot the math corpus uses
                // for row norms) matches everything above and is emphatically
                // not a matrix-vector product: routing it would pass the
                // peeled row as `x` for every row. This guard separates "one
                // shared vector" from "the row against itself".
                let isKernelParam id =
                    cg.KernelParams |> List.exists (fun (p: IRParam) -> p.VarId = id)
                let vecOk =
                    vecId <> rowId
                    && not (isKernelParam vecId)
                    // Same precision as the matrix and output: a mixed-width
                    // gemv has no BLAS routine, and declining costs only the
                    // optimisation.
                    && (match typeOf vecExpr with
                        | ArrayElem vt -> readOnlyBlasVector vt = Some prec
                        | _ -> false)
                if not vecOk then None
                else
                    // `prodsum(row, x)` conjugates nothing, so every
                    // precision -- complex included -- emits CblasNoTrans.
                    Some { Routine = Gemv
                           Route = RouteGemv
                           Level = L2
                           Operands = []
                           NestOperands = [ RoleA, FromNestArray aName
                                            RoleB, FromKernelRef vecExpr
                                            RoleC, FromNestOutput cg.OutputName ]
                           M = None; N = None; K = None
                           ElemType = e.ArrayElemType
                           Precision = prec
                           PackedTriangularResult = false }
             | _ -> None)
        | _ -> None
    | _ -> None

// (|BlasL3|_|) -- syrk, from a comm-licensed packed-covariance nest

/// A trailing scalar the kernel applies to the contraction -- the `/ N` a
/// centred covariance carries. Kept as the IR EXPRESSION (this module never
/// invents C++ text); CodeGen resolves it through its own name map, exactly
/// as it does a `FromKernelRef` operand.
type NestScale =
    /// `<contraction> / d`
    | ScaleDiv of IRExpr
    /// `<contraction> * s`, either operand order
    | ScaleMul of IRExpr

/// `prodsum(a, b)`, bare or wrapped in EXACTLY ONE scalar `/` or `*`.
///
/// THE ONE DEFINITION OF THE PEEL, exported so that the matcher (which uses it
/// as a shape gate) and the emission site (which needs the scale expression to
/// render) cannot drift into disagreeing about what a scaled contraction is.
///
/// The scale is admitted as a LITERAL or a plain `IRVar` only. Anything richer
/// is declined rather than rendered: a general expression could read a kernel
/// parameter, and a per-CELL factor is not a scale at all.
let (|ProdSumScaled|_|) (e: IRExpr) : (IRExpr list * NestScale option) option =
    let scalarOk (s: IRExpr) =
        (match s with
         | IRLit (IRLitFloat _) | IRLit (IRLitFloat32 _) | IRLit (IRLitInt _) | IRVar _ -> true
         | _ -> false)
        && (match typeOf s with
            | IRTScalar ETFloat64 | IRTScalar ETFloat32 -> true
            | _ -> false)
    match e with
    | IRProdSum args -> Some (args, None)
    | IRBinOp (_, IRDiv, IRProdSum args, d) when scalarOk d -> Some (args, Some (ScaleDiv d))
    | IRBinOp (_, IRMul, IRProdSum args, s) when scalarOk s -> Some (args, Some (ScaleMul s))
    | IRBinOp (_, IRMul, s, IRProdSum args) when scalarOk s -> Some (args, Some (ScaleMul s))
    | _ -> None

/// `C = method_for(A, A) <@> lambda(ri, rj) where comm(ri, rj) -> prodsum(ri, rj)`
///   ->  `blade_gram_same` (`?syrk`).
///
/// WHY THIS SHAPE: it is how a packed covariance / Gram matrix actually
/// appears in Blade programs (corpus: `sgs/005_filter_stress_comoment`,
/// `symmetry/017_fiber_kernel_reduce_block`, `ppl/065_staged_moment_tower`).
/// `comm(ri, rj)` is what made the nest TRIANGULAR, and `?syrk` computes
/// exactly that triangle -- so this is not a rewrite the licence has to
/// justify, it is the licence's own iteration space computed by a better
/// routine.
///
/// SHAPE MATCHED, exactly:
///   * a TWO-level MATERIALISING nest (no fold wrapper, no fold chunk);
///   * level 0 RECTANGULAR, level 1 TRIANGULAR over the SAME extent of the
///     SAME axis (`BoundDependencies = [0]`, `StrictOffset = 0` -- the
///     INCLUSIVE triangle syrk writes, never the strict one an antisymmetric
///     nest iterates), neither level fused;
///   * exactly TWO input array slots naming the SAME array, real f64, of TWO
///     ordinary dense axes;
///   * one element binding per level, each a real peel of dim 0 of that array
///     (`ArrayRank (2) > depth (1)` at its level makes it a FIBER, not a
///     scalar leaf);
///   * the output is a real f64 array of ONE rank-2 `SymSymmetric` group --
///     Blade's PACKED upper triangle, which is byte-for-byte the layout
///     `blade_gram_same_*` already writes (proven in `blade_linalg.hpp`, so
///     the route needs zero staging on C);
///   * the kernel body is exactly `prodsum(<level 0's peel>, <level 1's
///     peel>)`, optionally divided or multiplied by ONE scalar.
///
/// THE SCALE. `prodsum(x, y) / N` is the real covariance shape, and it is
/// admitted: the descriptor carries the scalar and the emission site applies
/// it to the packed triangle AFTER the call. Deliberately NOT folded into
/// syrk's `alpha`: `alpha = 1/N` would round the reciprocal first and then
/// multiply, whereas the nest this replaces divides the finished sum, so
/// post-scaling reproduces the emitted arithmetic one operation for one
/// operation instead of merely agreeing to a ULP.
///
/// PRECEDENCE is READ OFF `ompPolicy`, and L3's answer is the OPPOSITE of
/// L1/L2's: `BlasWins`, so a kernel carrying `omp` still routes. Copying L2's
/// hardcoded decline would have made this pattern fire on nothing, since the
/// covariance kernels it exists for are precisely the ones written with `omp`.
///
/// COMPLEX DECLINES, and not for want of an entry point. The complex instance
/// of this route is a HERMITIAN rank-k update (`?herk`, conjugating the second
/// factor) -- which is right for the gram NODE, whose scalar loop conjugates.
/// The matched kernel body here is `prodsum(ri, rj)`, which conjugates
/// NOTHING, so this nest is A.A^T and `herk` would silently return a different
/// matrix. A conjugating surface form would be a DIFFERENT match, not a
/// precision widening.
///
/// F64 ONLY, deliberately, in this increment. `blade_gram_same_s` exists and
/// is layout-identical, and the NODE route already reaches it -- so widening
/// is the single conjunct below plus its emission test, and is left as that
/// rather than landed unexercised. Every other precision declines to the
/// existing nest, which costs the optimisation and changes no value.
let (|BlasL3|_|) ((streamedCount, ompRequested, operandTypes, cg): int * bool * IRArrayType list * LoopNestCodeGen)
        : LinAlgCall option =
    if cg.FoldWrapper.IsSome || cg.FoldChunk.IsSome then None
    // Precedence from the POLICY TABLE: L3 is `BlasWins`, so this is a no-op
    // today. Spelled anyway, so that flipping the table row flips the matcher
    // and no level silently ignores the policy it is supposed to obey.
    elif ompRequested && ompRequestDeclines L3 then None
    elif not (nestModeOk streamedCount cg) then None
    else
    match cg.Bindings, cg.InputArrayNames, operandTypes with
    | [ l0; l1 ], [ aName; bName ], [ aTy; bTy ]
        when aName = bName
             && l0.BoundDependencies.IsEmpty && l0.StrictOffset = 0 && l0.FusedRank.IsNone
             && l1.BoundDependencies = [ 0 ] && l1.StrictOffset = 0 && l1.FusedRank.IsNone
             // Both levels iterate the SAME axis of the SAME array: `m` is one
             // number, which is what makes the result square.
             && l0.ExtentArrayRef = l1.ExtentArrayRef
             && l0.ExtentDimRef = l1.ExtentDimRef
             && l0.Extent = l1.Extent ->
        match denseBlasArrayOfRank 2 aTy, denseBlasArrayOfRank 2 bTy with
        // `PrecD` and nothing else (see the two notes above): complex is a
        // DIFFERENT operation here, and f32 is an unexercised widening.
        // Relaxing to `not (isComplexPrecision pa)` is what admits `ssyrk`.
        | Some pa, Some pb when pa = pb && pa = PrecD ->
            let prec = pa
            // Output: ONE rank-2 SymSymmetric group -- Blade's packed upper
            // triangle -- of the SAME precision. A dense rank-2 output, an
            // antisymmetric group, or a compound/orbit axis all decline: the
            // shim writes `Crows[i][jr]` with `jr < m - i` and nothing else
            // has that footprint.
            let outOk =
                match cg.OutputType with
                | ArrayElem outTy ->
                    not outTy.IsVirtual
                    && precisionOf outTy.ElemType = Some prec
                    && (match outTy.IndexTypes with
                        | [ ix ] ->
                            ix.Rank = 2
                            && ix.Symmetry = SymSymmetric
                            && ix.IxKind = IxKPlain
                            && ix.Dependencies.IsEmpty
                            // C is m x m over the SAME m the levels iterate.
                            // Deduction gives this today; checking it makes
                            // "square" a fact rather than an assumption, and a
                            // spurious decline costs only the optimisation.
                            && ix.Extent = l0.Extent
                            && (match ix.Tag with Some t -> not (t.StartsWith "__") | None -> true)
                        | _ -> false)
                | _ -> false
            if not outOk then None else
            // NOTE on `RankComponent`: it is NOT checked against the level.
            // For an outer-product nest over two SLOTS of the same array both
            // peels carry component 0 (the component index is per-ARRAY, and
            // each slot is peeled once) -- which array a level peels is
            // `ArrayPosition`, and that is what separates the two here.
            let peelOk (pos: int) (e: ElementBinding) =
                (match e.Virtual with RealArray -> true | _ -> false)
                && e.ArrayPosition = pos
                && e.ArrayName = aName
                && e.ArrayRank = 2
                && e.DimIndex = 0
                && precisionOf e.ArrayElemType = Some prec
                && (match e.SlotTag with Some t -> not (t.StartsWith "__") | None -> true)
            match l0.Elements, l1.Elements with
            | [ e0 ], [ e1 ] when peelOk 0 e0 && peelOk 1 e1 && e0.ParamVarId <> e1.ParamVarId ->
                (match cg.KernelExpr with
                 | ProdSumScaled ([ IRVar (iId, _); IRVar (jId, _) ], scale)
                        when iId = e0.ParamVarId && jId = e1.ParamVarId ->
                    // A scale that reads a KERNEL PARAMETER is not a scale:
                    // it would vary per cell. (No scalar f64 parameter exists
                    // on this kernel today -- both params are fibers -- so
                    // this is a guard against a future widening, not a case
                    // reachable now.)
                    let isKernelParam id =
                        cg.KernelParams |> List.exists (fun (p: IRParam) -> p.VarId = id)
                    let scaleOk =
                        match scale with
                        | Some (ScaleDiv (IRVar (id, _))) | Some (ScaleMul (IRVar (id, _))) ->
                            not (isKernelParam id)
                        | _ -> true
                    if not scaleOk then None
                    else
                        Some { Routine = Syrk
                               Route = RouteGramSame
                               Level = L3
                               Operands = []
                               NestOperands = [ RoleA, FromNestArray aName
                                                RoleC, FromNestOutput cg.OutputName ]
                               M = None; N = None; K = None
                               ElemType = e0.ArrayElemType
                               Precision = prec
                               PackedTriangularResult = true }
                 | _ -> None)
            | _ -> None
        | _ -> None
    | _ -> None

// Still planned -- L3 nest matching, the OTHER shapes
//
// A genuine three-level dense contraction written as loops (rather than the
// `matmul` / `gram` NODES already routed) has no pattern here yet. It arrives
// with its first real case, on the model of `(|BlasL3|_|)` above.
//
// PACKED-SYMMETRIC (dspmv): the shim entry `blade_symv` EXISTS and its layout
// premise is PROVEN (Blade's rank-2 sym-compact DFS pool order is
// byte-for-byte BLAS row-major UPPER packed order, so the route needs zero
// staging; see `blade_linalg.hpp`). There is deliberately no pattern for it,
// because no Blade surface form can currently produce a sym-compact matvec:
// peeling a rank-2 compact group into rank-1 fibers is refused at typecheck
// (BL4004 -- "a rank-k compact group is ONE index slot covering k
// dimensions"), and `reduce`/`prodsum` over compact storage is refused as
// well. `decompact` first, and the operand is dense -- the gemv route above.
// The entry point is the route waiting for a surface.
