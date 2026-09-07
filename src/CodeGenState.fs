// Code-generation state and vocabulary: the AsyncLocal emission cells,
// env-var gates (BLADE_FP_REASSOC / BLADE_OMP_THREADS / BLADE_MEMCHECK --
// deliberately FUNCTIONS, re-read per call so tests can pin them),
// diagnostics collectors, CodeGenContext helpers, the array-shape
// predicates the interpreter shares, and the type-rendering rec-chain.
module Blade.CodeGenState

open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.EmitCpp
open Blade.ReynoldsCore

// Runtime diagnostics emission helpers

/// Escape a string for embedding inside a C++ double-quoted string literal
/// (backslashes and double quotes only -- control chars are not expected in
/// Blade identifiers or spans).
let internal cppStrEscape (s: string) : string =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"")

/// Render a Blade source span as the trailing `(file, line)` argument pair for
/// a `blade_rt::panic(...)` call. Absent/empty file -> `nullptr`; a
/// zero/negative start line -> `0`. panic degrades gracefully on either.
let internal panicSpanArgs (span: Blade.Ast.Span) : string =
    let fileArg =
        match span.File with
        | Some f when f <> "" -> $"\"{(cppStrEscape f)}\""
        | _ -> "nullptr"
    let lineArg = if span.StartLine > 0 then string span.StartLine else "0"
    $"{fileArg}, {lineArg}"

// Code Generation Context

/// Tracks information needed during code generation
type CodeGenContext = {
    /// Map from IR variable IDs to C++ variable names
    VarNames: Map<IRId, string>
    /// The VarIds of the PARAMETERS of the function body being emitted
    /// (empty at module level). A parameter's array type may carry a LITERAL
    /// extent that is not a statement about the runtime array: shape
    /// monomorphization pins a `T^k` parameter from one argument, and a
    /// co-iterating body unifies the parameters' records, so a curried or
    /// let-bound partial application leaves a second parameter typed at the
    /// first one's extent. The co-iteration extent guard (BL8011) therefore
    /// compares runtime extents whenever a parameter is involved, and trusts
    /// equal literals only between non-parameter (allocated) arrays.
    ParamIds: Set<IRId>
    /// Current indentation level
    Indent: int
    /// Generated static declarations (symmetry vectors, extents)
    StaticDecls: string list
    /// C++ var name -> tuple children names (extents provenance), e.g. "_0" -> ["_0_0", "_0_1"].
    TupleChildren: Map<string, string list>
    /// Deferred computation exprs (L <@> f), materialized only when |> compute forces them;
    /// <&!> looks through variables to find the original ApplyInfo for fusion.
    DeferredComputations: Map<IRId, IRExpr>
    /// Let-bound loop-object provenances (`let o = object_for(f)`); kept SEPARATE from
    /// DeferredComputations so array-position consumers never see a bare loop object --
    /// only genComposeApply reads this, to chase a composed leaf back to its IRObjectFor
    /// (the fix for `(o1 >>@ o2) <@> A` over let-bound objects).
    ObjectLoopBindings: Map<IRId, IRExpr>
    /// Deferred provider reads keyed by binding id (from IRModule.ProviderReads);
    /// genBinding emits the registry-dispatched reader (genReadVar / genReadCompoundVar).
    ProviderReads: Map<IRId, ProviderReadSpec>
    /// Deferred provider writes keyed by binding id; genBinding emits a flatten
    /// prologue + the registry-dispatched writer (genWriteVar).
    ProviderWrites: Map<IRId, ProviderWriteSpec>
    /// Streamed provider reads whose prologue is already emitted, keyed by cpp name:
    /// a hit means inline a fiber read at the S/T boundary instead of peeling.
    StreamedArrays: Map<string, ProviderReadSpec>
    /// Groupings emitted by genSegmentsBinding (structural: static offsets,
    /// identity accessor), by cpp name. A group_by over a STREAMED rank-1
    /// variable and one of these reads per run (docs/plans/structural/07 §3.4).
    StructuralGroupings: Set<string>
    /// Deferred random-fill constructors keyed by binding id (from IRModule.RandomInits);
    /// genBinding emits allocate<> + a pool fill from the RandomFillSpec.
    RandomInits: Map<IRId, RandomFillSpec>
    /// Deferred compound(dense, mask) constructors keyed by binding id (from
    /// IRModule.CompoundInits); genBinding emits P0 index materialization + a
    /// dense->compact scatter from (loweredDense, loweredMask).
    CompoundInits: Map<IRId, IRExpr * IRExpr>
    /// Deferred sparse(values, keys) constructors keyed by binding id (from IRModule.SparseInits).
    SparseInits: Map<IRId, IRExpr>
    /// Grouped-array cpp name -> source GroupKeys cpp name; populated by genBinding for
    /// IRGroupBy, consulted by method_for when peeling a ragged outer dim (Tag = "__group_outer").
    GroupedArrays: Map<string, string>
    /// Block-level `let mut` array bindings (from IRModule.MutableArrayLets): genVarAliasBinding
    /// deep-copies storage (fresh alloc + pool copy) instead of aliasing the Array wrapper by
    /// value, so mutation can't corrupt the source array.
    MutableArrayLets: Set<IRId>
    /// Accumulated code generation warnings (unsupported IR nodes, fallbacks, etc.)
    Warnings: string list ref
}

/// Module-level expression warnings collector: exprToCpp is pure (returns a
/// string) so it can't use CodeGenContext directly; warnings sync back into
/// CodeGenContext after. AsyncLocal (not a shared `ref []`) so each parallel
/// test task gets its own ref cell -- otherwise appends from one task would
/// interleave with resets from another.
let internal exprWarningsStorage =
    System.Threading.AsyncLocal<string list ref>()

let exprWarningsCell () : string list ref =
    let v = exprWarningsStorage.Value
    // Box to obj before isNull: F# enforces non-nullability on its own
    // `Ref<T>` record type, even though the CLR-level default is null
    // when AsyncLocal hasn't been assigned. The Dictionary-based caches
    // don't need this dance because Dictionary is a BCL class that F#
    // treats as nullable.
    if isNull (box v) then
        let fresh = ref []
        exprWarningsStorage.Value <- fresh
        fresh
    else v

/// Ids of module-level DEFERRED bindings codegen actually FORCED (materialized
/// under their own name at main's top level) by forceDeferredArrayInput's
/// IRVar arm. genPrintStatements consults this: a forced binding auto-prints
/// like any eager one; a still-deferred binding prints nothing. Nested-scope
/// forcing (Indent > 0) is NOT recorded -- that C++ variable is block-scoped
/// and a main-end print would not compile. Reset per program assembly.
let internal forcedDeferredIdsStorage =
    System.Threading.AsyncLocal<Set<int> ref>()

let forcedDeferredIdsCell () : Set<int> ref =
    let v = forcedDeferredIdsStorage.Value
    if isNull (box v) then
        let fresh = ref Set.empty
        forcedDeferredIdsStorage.Value <- fresh
        fresh
    else v

/// Append-only, insertion-ordered line collector for the emission-side cells
/// below. Replaces the `string list ref` they used to be: every contribution
/// was `cell.Value <- cell.Value @ lines` (an O(n) list copy per append) and the
/// de-duplicating ones added an O(n) `List.contains` scan on top, so collecting
/// n declarations cost O(n^2). Here appends are amortized O(1) and membership is
/// a hash lookup, while `Value` still reads the lines back as a list in exactly
/// the insertion order the emitted C++ depends on.
///
/// `Value` is settable so the existing per-program reset sites (`.Value <- []`)
/// keep working unchanged; assignment replaces the contents wholesale.
type DeclCollector() =
    let items = System.Collections.Generic.List<string>()
    let seen = System.Collections.Generic.HashSet<string>()
    /// Append lines unconditionally, in order.
    member _.Append (lines: string list) = items.AddRange lines
    /// Append `line` only if an identical line was never appended (by any
    /// route) -- the idempotent-declaration case. Insertion order is kept.
    member _.AppendDistinct (line: string) = if seen.Add line then items.Add line
    /// Does any collected line satisfy `p`? Non-allocating.
    member _.Exists (p: string -> bool) = Seq.exists p items
    member _.Value
        with get () : string list = List.ofSeq items
        and set (v: string list) =
            items.Clear()
            seen.Clear()
            items.AddRange v
            for l in v do seen.Add l |> ignore

/// Collector for CUDA kernel definitions destined for the .cu file. genCudaKernel
/// appends each __global__ kernel + extern "C" wrapper here; the assembler reads
/// it to produce the .cu (only when non-empty). AsyncLocal per-flow cell, like
/// exprWarningsCell, so a deep emission site can contribute without a threaded return.
let internal cudaKernelDefsStorage =
    System.Threading.AsyncLocal<DeclCollector>()

let cudaKernelDefsCell () : DeclCollector =
    let v = cudaKernelDefsStorage.Value
    if isNull (box v) then
        let fresh = DeclCollector()
        cudaKernelDefsStorage.Value <- fresh
        fresh
    else v

/// Collector for symmetry-vector array declarations, which must live at NAMESPACE
/// scope: MSVC (C2131) refuses to treat the ADDRESS of a function-local `static
/// constexpr` array as a constant expression, but allocate<>'s SYMM template
/// argument for a symmetric output needs one (rectangular outputs dodge this by
/// passing nullptr). Mirrors cudaKernelDefsCell; reset at program assembly.
let internal symmDeclsStorage =
    System.Threading.AsyncLocal<DeclCollector>()

let symmDeclsCell () : DeclCollector =
    let v = symmDeclsStorage.Value
    if isNull (box v) then
        let fresh = DeclCollector()
        symmDeclsStorage.Value <- fresh
        fresh
    else v

/// Collector for module-level bindings PROMOTED to namespace scope (S0 of the
/// kernel-body-materialization plan, docs/plan-kernel-body-materialization.md
/// section 6). A module-level `let` is normally a `main()` local, while lifted
/// kernels and user functions are namespace-scope -- so a namespace-scope
/// function that must NAME a module-level binding (to forward it as a capture
/// argument, or because the kernel body was inlined into its loop) emits an
/// undeclared identifier. Promotion moves only the DECLARATION out to namespace
/// scope; the initialization stays at its original point inside `main()` as a
/// plain assignment, so evaluation order, timing phases and allocation scopes
/// are untouched. Mirrors symmDeclsCell; reset at program assembly.
let internal moduleGlobalDeclsStorage =
    System.Threading.AsyncLocal<DeclCollector>()

let moduleGlobalDeclsCell () : DeclCollector =
    let v = moduleGlobalDeclsStorage.Value
    if isNull (box v) then
        let fresh = DeclCollector()
        moduleGlobalDeclsStorage.Value <- fresh
        fresh
    else v

/// Per-binding-name cache for tryHoistModuleBindingDecl's pattern. The pattern
/// embeds the binding name, so it cannot be compiled once globally -- but it
/// CAN be compiled once per name instead of once per call, which is what this
/// gives. Concurrent-safe (the parallel test runner shares this process, and a
/// `Regex` is immutable and thread-safe for matching).
let internal hoistDeclRegexCache =
    System.Collections.Concurrent.ConcurrentDictionary<string, System.Text.RegularExpressions.Regex>()

/// Declaration keywords that disqualify a line from the split (hoisted out of
/// tryHoistModuleBindingDecl so the set is built once, not per call).
let internal hoistDeclReserved =
    set ["auto"; "const"; "constexpr"; "static"; "register"; "volatile"; "return"; "else"]

/// Split a module-level binding's emitted code into (namespace-scope
/// declaration, rewritten body) when its definition is a single ordinary
/// `TYPE NAME = RHS;` line -- the shape every scalar and every
/// `Array<T,N>`/`Ragged<T>` wrapper binding emits. Returns None (caller keeps
/// the status quo) for anything else: an `auto`/`const`/`constexpr`/reference
/// declaration, a binding with no definition line (deferred computations, unit
/// values) or more than one, since none of those can be split into a
/// default-construct-then-assign pair.
///
/// The declaration's TYPE is read back out of the emitted text rather than
/// recomputed, so this helper can never disagree with the emitter about it.
/// Namespace-scope objects are zero-initialized before `main` runs and the
/// assignment happens at exactly the original program point, so the promoted
/// binding holds the same value at every point the un-promoted one did.
let tryHoistModuleBindingDecl (name: string) (lines: string list) : (string * string list) option =
    let re =
        hoistDeclRegexCache.GetOrAdd(name, fun n ->
            let pattern =
                $"^(?<ind>\\s*)(?<ty>[A-Za-z_][A-Za-z0-9_:<>,\\* ]*?)\\s+{(System.Text.RegularExpressions.Regex.Escape n)}\\s*=\\s*(?<rhs>.*;)\\s*$"
            System.Text.RegularExpressions.Regex(pattern))
    let reserved = hoistDeclReserved
    let matches =
        lines
        |> List.mapi (fun i l -> (i, re.Match l))
        |> List.filter (fun (_, m) ->
            m.Success &&
            (let ty = m.Groups.["ty"].Value.Trim()
             ty <> "" && not (ty.Contains "&") &&
             (ty.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
              |> Array.forall (fun w -> not (reserved.Contains w)))))
    match matches with
    | [ (idx, m) ] ->
        let ty = m.Groups.["ty"].Value.Trim()
        let decl = $"{ty} {name};"
        let rewritten =
            lines |> List.mapi (fun i l ->
                if i = idx then $"""{(m.Groups.["ind"].Value)}{name} = {(m.Groups.["rhs"].Value)}"""
                else l)
        Some (decl, rewritten)
    | _ -> None

/// Append a namespace-scope symm-array decl to the hoist collector (idempotent
/// per distinct name). Returns the name for the allocate<> call site's template
/// argument, now a valid constant expression under MSVC since it's file-scope.
let hoistSymmDecl (name: string) (symmVec: int list) : string =
    let cell = symmDeclsCell ()
    let values = symmVec |> List.map string |> String.concat ", "
    let decl = $"static constexpr const size_t {name}[{symmVec.Length}] = {{{values}}};"
    cell.AppendDistinct decl
    name

/// Emit the right-hand side of an output array allocation from a backend-neutral
/// AllocSpec (allocRoutineFor / classifyOutputStorage), not the kernel's
/// Reynolds flag.
///   Ok rhs    -- the `{ allocate...(extents), extents }` brace-init expression
///               the Array<T,N> wrapper is initialized from, or
///   Error msg -- a diagnostic for a shape with no representable allocator; the
///               call site emits a `#error` line so the TU fails loudly.
///
///   AllocDense/AllocSymmetric -> allocate<promote<T,R>::type, SYMM>(extents)
///     (symmArg is "nullptr" for dense, a hoisted vec name for symmetric)
///   AllocAntisymmetric -> allocate<..., {1,..}, false>(extents) -- an all-grouped
///     mask + DIAGONALS=false (strict simplex), unified with the symmetric path:
///     antisym is a strict symmetric grouping. allocate_antisym in the runtime
///     header exists for C++-only testing.
///
/// AllocUnsupported has no representable allocator (allocate_antisym applies the
/// strict shrink at every depth, so it cannot express antisym-plus-free-dimension).
/// No current type annotation can trigger it (each yields one index group); the
/// Error path guards a future front-end change that might.
let internal emitAllocRhs
        (spec: AllocSpec)
        (elemType: string) (rank: int) (symmArg: string) (extentsName: string)
        : Result<string, string> =
    match spec with
    | AllocAntisymmetric ->
        // Matches allocate_antisym byte-for-byte (verified at ranks 2-4).
        let allOnes = List.replicate rank 1
        let maskName = hoistSymmDecl ($"{extentsName}_anti") allOnes
        Ok ($"{{ allocate<typename promote<{elemType}, {rank}>::type, {maskName}, false>({extentsName}), {extentsName} }}")
    | AllocDense | AllocSymmetric ->
        Ok ($"{{ allocate<typename promote<{elemType}, {rank}>::type, {symmArg}>({extentsName}), {extentsName} }}")
    | AllocPerGroupStrict strictVec ->
        // symmArg MUST be the compact-grouped SYMM mask (antisym grouped like
        // symmetric), built by the caller's buildSymmVecWithStrict so SYMM and
        // STRICT align position-for-position. Sign is lazy-on-read (canon_*),
        // never baked into storage.
        let strictName = hoistSymmDecl ($"{extentsName}_strict") strictVec
        Ok ($"{{ allocate_strict<typename promote<{elemType}, {rank}>::type, {symmArg}, {strictName}>({extentsName}), {extentsName} }}")
    | AllocWreath (levels, _, _) ->
        // Reaching here means some OTHER site (a copy, negate, materializer) tried
        // to put a wreath class into an Array wrapper; genWreathApply is the real
        // wreath emitter and never calls this function. Refuse loudly.
        Error (sprintf "Blade codegen: an OrbIdx%s (iterated-wreath) pool cannot be allocated as an \
Array<T,N>: allocate<> builds ONE shrinking simplex and a wreath's rows shrink per level. Only a \
deduced wreath OUTPUT of a comm-tied apply has an emitter (a flat orb_cell_count pool)."
                      (ppOrbitLevels levels))
    | AllocUnsupported reason ->
        Error ($"Blade codegen: unsupported antisymmetric output storage -- {reason}")

/// The `orb_level<...>` template-argument list for an OrbIdx class, in the
/// header's public (doc) order: OUTERMOST LAST, exactly as `IROrbitClass`
/// carries it. ONE spelling, shared by every wreath emitter -- the traversal
/// nest, the canonical argument read, the arbitrary-tuple `orb_read`, the
/// decompaction nest and the printer. A level list rendered in the wrong order
/// is a DIFFERENT CLASS that still compiles, so this must not be re-spelled per
/// site. (Hoisted this far up the file only so `renderIndexExpr` can reach it;
/// it depends on nothing.)
let orbLevelArgs (levels: (int * bool) list) : string =
    levels
    |> List.map (fun (r, isPlus) ->
        $"""orbit_wreath_utilities::orb_level<{r},{(if isPlus then "true" else "false")}>""")
    |> String.concat ", "

// Materializer allocation descriptors (deterministic deallocation, site 7)
//
/// What a `materialize*Form` builder allocated, described BY the code that chose
/// the template arguments. The builders pick (spec, SYMM, STRICT) from
/// form-specific masks that their statement-level consumers never see;
/// re-deriving that triple at the consumer would risk `deallocate` walking a
/// different skeleton than `allocate` built -- silent heap corruption, not a
/// compile error. So each builder returns its own descriptors alongside the
/// emitted lines, and the consumer only decides WHETHER to register them:
/// statement-position consumers do (their scope's closing brace is a real exit
/// point), expression / IIFE consumers drop them (unchanged, still leaked).
///
/// Declared here, beside `emitAllocRhs` / `EmitCpp.arrayAlloc`, because the
/// builders live in the `exprToCppCore` rec group far above the allocation
/// registry; `registerMaterializedAllocs` (below the registry) converts these
/// into `TrackedAlloc`s.
/// EVERY array-shaped case carries `OwnedExtents`, the name of a heap extents
/// table this form allocated (`emitExtentsTable`'s second result), or `None`
/// for the static-constexpr table that owns nothing. It rides ON the array's
/// own descriptor rather than arriving as a separate raw-buffer entry because
/// the two must be spared TOGETHER: `genFuncBodyScoped`'s return suppression is
/// a whole-token match against the returned text, which sees `return g;` and
/// therefore `g` -- never `g_extents`. A separately tracked table would be
/// deleted out from under exactly the wrapper that just escaped, which is the
/// dangling-shape bug in a new spelling.
type MaterializedAlloc =
    /// `Array<Elem, Rank> Name = { allocate[_strict]<promote<Elem,Rank>::type,
    /// Symm[, Strict]>(ext), ext }` -- freed by the mirrored deallocate routine,
    /// then the owned extents table (in that order: the deallocate READS
    /// `Name.extents` to walk the skeleton).
    | MatPool of
        Name: string * Elem: string * Rank: int * Symm: string *
        Strict: string option * OwnedExtents: string option
    /// A form that built its allocation through `emitAllocRhs` from a data-dependent
    /// AllocSpec (negate / conjugate / array-copy: same storage class as the SOURCE).
    /// Freed via `deallocArgsFor` with the identical spec/elem/rank/SYMM/extents-name.
    | MatPoolSpec of
        Name: string * Spec: AllocSpec * Elem: string * Rank: int *
        Symm: string * ExtentsName: string * OwnedExtents: string option
    /// `Array<Elem, 1> Name = { new Elem[n], ext }` -- the mask / sort / unique /
    /// union / intersect family's raw backing. Freed with `delete[] Name.data`.
    | MatRawData of Name: string * OwnedExtents: string option
    /// A bare `T* Name = new T[n]` scratch buffer (sort's permutation table).
    /// Freed with `delete[] Name`.
    | MatRawBuf of Name: string

/// Module-level OpenMP test-mode flag: when set, parallel loop nests emit
/// thread-coverage instrumentation (each region records which OpenMP threads
/// actually ran, printed after the loop) so the test harness can verify the
/// emitted pragmas produce GENUINE parallel regions, not just syntactic ones.
/// OFF by default (toggled on only by the harness, else it pollutes output).
/// AsyncLocal so parallel test tasks don't race on the flag.
let internal ompTestModeStorage =
    System.Threading.AsyncLocal<bool ref>()

let ompTestModeCell () : bool ref =
    let v = ompTestModeStorage.Value
    if isNull (box v) then
        let fresh = ref false
        ompTestModeStorage.Value <- fresh
        fresh
    else v

/// Set/clear OpenMP test-mode for the current async flow (called by the harness).
let setOmpTestMode (on: bool) : unit =
    (ompTestModeCell ()).Value <- on

/// Split-timing mode: when on, main() emits TWO timing checkpoints -- input-data
/// setup and computation -- instead of one whole-body clock, so the differential
/// harness can report compute time excluding (sometimes large) input allocation.
/// Default OFF so every other test's single "completed in" line is unchanged.
/// AsyncLocal for the same reason as ompTestMode.
let internal splitTimingModeStorage =
    System.Threading.AsyncLocal<bool ref>()

let splitTimingModeCell () : bool ref =
    let v = splitTimingModeStorage.Value
    if isNull (box v) then
        let fresh = ref false
        splitTimingModeStorage.Value <- fresh
        fresh
    else v

let setSplitTimingMode (on: bool) : unit =
    (splitTimingModeCell ()).Value <- on

let splitTimingModeEnabled () : bool =
    (splitTimingModeCell ()).Value

// ---- Floating-point reassociation opt-in (BLADE_FP_REASSOC) -----------------

/// May the emitter reassociate a SERIAL floating-point accumulation chain?
///
///   BLADE_FP_REASSOC=1|on   -> yes
///   unset / 0 / anything else -> NO (the default)
///
/// CONTRACT, stated where the knob lives so it cannot be read without it:
///
///  * Reassociation CHANGES BITWISE RESULTS. `(a+b)+c` and `a+(b+c)` are
///    different doubles. Off is therefore the default, and OFF MEANS NO
///    REASSOCIATION ANYWHERE: every site guarded by this predicate emits,
///    character for character, the serial chain it emitted before the knob
///    existed. (The separate `literalOrRuntimeExtent` rule below changes some
///    of those chains' loop BOUNDS from a runtime read to the literal the type
///    already carries. That is a textual change on both settings and a
///    numerically inert one -- same trip count, same summands, same order.)
///  * The INTERPRETER DIFFERENTIALS MUST RUN WITH IT OFF. `src/Interp` is a
///    byte-identity twin of exactly these chains (`ArrayOps.prodSum` walks `t`
///    ascending through one accumulator, and so does the fold), so
///    `blade test interp ...` and `diff-oracle` compare two evaluators that
///    only agree while the emitter is not reassociating. Nothing has to be
///    done to keep them honest -- they inherit the default -- but a harness
///    that pins this ON is asserting a false equality.
///  * DETERMINISM, and this is the part that CHANGED. What the knob buys is
///    instruction-level and SIMD parallelism, never threads -- no site guarded
///    by this predicate creates a team, so a surrounding parallel region and
///    OMP_NUM_THREADS remain irrelevant to the answer. What a knob-on result is
///    therefore guaranteed to be:
///
///        deterministic for a FIXED BINARY -- identical bytes across runs, and
///        across OMP_NUM_THREADS values
///
///    and what it is NOT guaranteed to be:
///
///        reproducible ACROSS compilers, compiler versions, or optimization
///        flags -- the same Blade program built twice differently may print
///        different low bits
///
///    on the sites that emit `omp simd reduction` (`fpReassocSimdStmts`), whose
///    summation order is the vectorizer's: its vector width, its unroll factor,
///    its choice to vectorize at all. That weakening is DELIBERATE, licensed by
///    the project policy that optimized (-O3) Release builds are not expected
///    to be bit-reproducible across builds, and it is what bought the measured
///    wins recorded at those sites (1.64x on the dot nest and 2.60x on the gemv
///    fiber, both of which the lane form left at PARITY with the serial chain;
///    3.36x on the 3-stream moment former against the lanes' 1.80x).
///
///    Which sites those are is a per-site MEASUREMENT, not a blanket switch,
///    and the split is real:
///
///        simd   IRProdSum's fiber IIFE; the reduce-over-computation nest
///        lanes  reduce over a MATERIALIZED array (both its statement and its
///               expression form) -- where the lanes measured 1.12x-1.90x
///               FASTER than simd, so nothing was traded away
///        lanes  any `comm`-declared kernel, at every site: its combine is a
///               call, which no reduction clause can name
///
///    So the older, stronger property -- a fixed function of the data and K
///    alone, reproducible across any toolchain -- is not gone; it still holds
///    wherever `fpReassocLaneStmts` is what got emitted. Off is, as always,
///    stronger than either: byte-identical to the serial chain everywhere.
///  * The knob licenses reassociation of arithmetic the COMPILER owns (a
///    recognised builtin fold body) or that the USER has declared reorderable
///    (`comm`) -- never an arbitrary user kernel. See `foldReorderLicensed`.
///    Which FORM that licence is spent in is a second, narrower question, and
///    `fpReassocSimdOp` answers it.
///
/// A FUNCTION, never a module-level `let`: a module-level binding freezes the
/// environment read at first touch, which would make a mid-process pin (a
/// test's scoped ON, a hand-run between two compiles in one process) silently
/// ineffective. Same reason `LinAlgPatterns.blasAvailable` and `Build.fs`'s
/// `fpContractFlag` are functions. Every consultation re-reads.
let fpReassocEnabled () : bool =
    match System.Environment.GetEnvironmentVariable("BLADE_FP_REASSOC") with
    | "1" | "on" -> true
    | _ -> false

/// How many independent LANE accumulators a reassociated accumulation chain
/// runs. A single accumulator makes the sweep a serial dependence chain through
/// the fold kernel; K lanes give the C++ compiler K independent chains over the
/// same contiguous range.
///
/// COMPILE-TIME CONSTANT on purpose: the lane count is part of the fold's
/// evaluation order, so a runtime knob would make the result depend on
/// something outside the program (OMP_NUM_THREADS); as a constant, the answer
/// is a fixed function of the data and K.
///
/// LICENCE: reordering elements within a chunk is the same reorder the fold's
/// comm/builtin-body gate already licensed. This constant is read only on
/// already-licensed arms -- Path B's chunked fold (which needs no knob), and
/// the `fpReassocEnabled ()` sites, which need one.
///
/// Why 8 (measured over 1e5/1e6/1e7 f64 at OMP_NUM_THREADS=1, K in
/// {1,4,8,16}): 8 is never worse than 4 and is 1.3-1.5x better while the data
/// is cache-resident; 16 REGRESSES (register pressure spills the lanes back to
/// the stack). At 1e7 the sweep is memory-bandwidth-bound and 4/8/16 converge.
///
/// DEFINED HERE, far above its first fold-path reader, because the `prodsum`
/// IIFE emitter in `exprToCppCore` needs it too and F# is order-dependent. One
/// definition is the point: two lane counts would be two evaluation orders.
let internal foldLaneCount = 8

/// The lane count for an accumulation whose every lane iteration keeps `s`
/// concurrent ARRAY OPERAND STREAMS live (prodsum's argument count; the number
/// of distinct array leaves a reduce-over-computation body reads; 1 for a plain
/// `reduce`/sumred sweep).
///
/// THE PRINCIPLE, not a tuning table: the lane count divides a FIXED
/// register/ILP budget among the concurrent value streams a lane iteration
/// keeps live. One lane of an s-stream body holds s loaded values plus its
/// accumulator, so K lanes hold roughly K*s live values; past the machine's
/// architectural register file the lanes spill to the stack and every update
/// becomes the load-modify-store the lanes existed to remove. The ANCHOR is the
/// repo's own measurement at one and two streams -- `foldLaneCount` = 8 (Round
/// C's K sweep over 1e5/1e6/1e7 f64, where 16 already regressed) and the
/// 2-operand prodsum's +15% at the same K -- and the 1/s scaling simply holds
/// the product K*s constant at that anchor:
///
///     K(1) = K(2) = 8   (the measured optimum; 1 and 2 streams share it)
///     K(s) = (2 * 8) / s  for s >= 3,  floored at 2 lanes
///
/// so K(3) = 5, K(4) = 4, K(8) = 2. Two lanes is the floor because one lane is
/// not a lane form at all -- it is the serial chain, which the callers' short
/// fallback already emits.
///
/// NOT machine-tuned per s: only the s <= 2 anchor is measured, and the rest is
/// the budget identity extrapolated from it. That is deliberate -- a per-s
/// table would be a fit to one microarchitecture, and the lane count is part of
/// the fold's EVALUATION ORDER, so it must be a stated rule the emitted answer
/// can be reproduced from, not a number someone searched for.
let internal laneCountForStreams (s: int) : int =
    if s <= 2 then foldLaneCount
    else max 2 ((2 * foldLaneCount) / s)

// ---- Thread-level OpenMP emission knob (BLADE_OMP_THREADS) ------------------

/// May this BUILD emit THREAD-level OpenMP constructs at all?
///
///   unset / "2" / "8" / anything unparseable -> YES (the default: status quo,
///                                              emit thread pragmas wherever
///                                              the source licensed them)
///   "1" / "0" / "off"                        -> NO  (serial emission)
///
/// THE DESIGN PRINCIPLE, stated here because this is the only place it is
/// enforced: the `omp` / `omp(a: n)` LICENCE IN SOURCE and this BUILD KNOB
/// answer two different questions and must not be conflated.
///
///   * The LICENCE is a statement about the KERNEL -- which dimensions are
///     safe to carry threads, which folds are safe to reassociate. It is a
///     property of the mathematics the program expresses, it is checked
///     (BL4016 refuses an unlicensed parallel fold), and it STAYS IN SOURCE
///     unchanged whatever this knob says.
///   * The KNOB decides whether a licensed parallelism is SPENT in this
///     particular build. That is a property of the machine the binary will run
///     on, not of the program. ONE SOURCE, PER-DEPLOYMENT BUILDS.
///
/// WHY THE KNOB EXISTS AT ALL, and why `OMP_NUM_THREADS=1` at RUNTIME is not
/// the same thing. GCC's `parallel for` outlines the loop body into a separate
/// function called through the OpenMP runtime; that outlining is a COMPILE-TIME
/// decision and its cost is paid even when the team turns out to have one
/// thread. Measured on the fiberdot row-map shape:
///
///     pragma emitted, OMP_NUM_THREADS=1   488 us
///     no pragma emitted (serial)          263 us   <- 1.86x FASTER
///     pragma emitted, multi-threaded      187 us   (parity with hand C++)
///
/// So on a single-core (or thread-pinned) deployment the licensed pragma is
/// pure loss and no runtime setting recovers it -- only not emitting it does.
///
/// `omp simd` IS NOT SUPPRESSED, in either mode, and that is the whole reason
/// this predicate is named "thread emission" rather than "omp emission". A
/// `simd` construct creates no team, calls no runtime, and is not outlined --
/// it is a vectorization hint the compiler consumes in place. It therefore
/// costs nothing at one thread and is kept unconditionally, which is why the
/// suppressed forms below are `omp simd` / `BLADE_OMP_SIMD_REDUCTION` rather
/// than "no pragma at all" wherever vectorization was already licensed.
///
/// A NUMERIC VALUE >= 2 DOES NOT BAKE `num_threads(n)`, deliberately, in v1.
/// Emission is unchanged from the default and the DEGREE of parallelism stays
/// the runtime's `OMP_NUM_THREADS`. The knob's v1 job is the binary
/// emit/don't-emit decision that runtime cannot make; baking a team size would
/// additionally freeze into the binary a number the deployment usually wants to
/// set per run, and would change Path B's chunk count -- which its determinism
/// contract is stated in terms of. `>= 2` is accepted (rather than rejected) so
/// a deployment can write the true thread count in one place and have both this
/// and `OMP_NUM_THREADS` read it.
///
/// `-fopenmp` STAYS IN THE BUILD FLAGS IN BOTH MODES (Build.compileCppWithExtra):
/// `omp simd` needs it, and it costs nothing in a program with no parallel
/// construct. A future refinement could pass `-fopenmp-simd` instead when this
/// returns false, which would additionally drop the libgomp link; that is a
/// Build.fs change and is not attempted here.
///
/// SET IT GLOBALLY. This is process-environment state, read per emission, so
/// `BLADE_OMP_THREADS=1` in the environment governs every compile that process
/// performs -- which is the intended usage (one setting per deployment box).
/// The omp test blocks pin it UNSET around their own scoped compiles so a
/// globally-serial box does not make them vacuous.
///
/// A FUNCTION, never a module-level `let`, for exactly the reason stated at
/// `fpReassocEnabled`: a module-level binding freezes the environment read at
/// first touch and would make a mid-process pin (a test's scoped guard)
/// silently ineffective. Every consultation re-reads.
let ompThreadEmissionEnabled () : bool =
    match System.Environment.GetEnvironmentVariable("BLADE_OMP_THREADS") with
    | "1" | "0" | "off" -> false
    | _ -> true

/// The reason string every site hands to the emitted-C++ marker when this knob
/// is what suppressed a thread construct. ONE spelling, so a census over
/// generated code (`grep "[omp]"`) finds every declined site with one pattern
/// and can tell a knob decline from a licence decline.
let ompThreadsSuppressedReason () : string =
    sprintf "BLADE_OMP_THREADS=%s -- this build emits no thread-level OpenMP (the omp licence in source is unchanged; omp simd is unaffected)"
        (System.Environment.GetEnvironmentVariable("BLADE_OMP_THREADS"))

/// The same marker as a BLOCK comment, for the INTRINSIC emitters (gram /
/// matmul / the materialize*Form family), whose lines can be SPACE-JOINED into
/// a single-line IIFE at expression positions -- where a `//` comment would
/// swallow the rest of the statement. Same argument as the `dispatchMarkerTag`
/// block comments at those sites.
let ompThreadsSuppressedBlockMarker () : string =
    $"/* [omp] thread pragma suppressed: {(ompThreadsSuppressedReason ())} */"

/// The census phrase for "this kernel asked for `omp` and got serial code",
/// shared by BOTH comment forms so they cannot drift. `ompSuppressedMarker`
/// (the `//` line form, used by the loop-nest emitters) and
/// `ompSuppressedBlockMarker` (the `/* */` form below) are the only two
/// spellings, which is what lets one `grep "[omp] requested but emitted
/// serial"` enumerate every declined site.
///
/// Defined HERE, far above `ompSuppressedMarker`, only because F# is
/// order-dependent and the earliest consumer -- `renderReduceExpr`, an
/// expression-position emitter -- sits above that function.
let ompSuppressedPhrase (reason: string) : string =
    $"[omp] requested but emitted serial: {reason}"

/// The census marker as a BLOCK comment, for emitters whose output is a
/// SINGLE-LINE IIFE (`[&]() { ... }()`) at an expression position, where a `//`
/// comment would swallow the rest of the statement. Same argument as
/// `ompThreadsSuppressedBlockMarker` above; the trailing space is included so
/// callers can splice it directly after `[&]() { `.
///
/// Returns "" when the kernel never asked for `omp`, so a caller can prepend it
/// unconditionally.
let ompSuppressedBlockMarker (requested: bool) (reason: string) : string =
    if requested then $"/* {(ompSuppressedPhrase reason)} */ " else ""

/// Collector: did THIS program assembly emit a `blade_linalg::` dispatch call?
/// Set by the gram / matmul emitters during genModule; the program assemblers
/// append the `#include "blade_linalg.hpp"` line after body generation (the
/// cudaKernelDefsCell collect-then-assemble pattern). AsyncLocal for the
/// parallel test runner.
///
/// Codegen does not decide BLAS-vs-native: a recognised route always emits the
/// same `blade_linalg::` call text, and the shim header's `#ifdef
/// BLADE_HAS_BLAS` -- driven by Build.fs's OPENBLAS_DIR/BLADE_BLAS resolution --
/// picks cblas or the native fallback at C++ compile time. The include line
/// below is what keeps codegen and build in lockstep; Build.fs keys its
/// -D/-I/link flags off it.
let internal linalgUsedStorage =
    System.Threading.AsyncLocal<bool ref>()

let linalgUsedCell () : bool ref =
    let v = linalgUsedStorage.Value
    if isNull (box v) then
        let fresh = ref false
        linalgUsedStorage.Value <- fresh
        fresh
    else v

/// Collector: did THIS program assembly emit a `blade_cuda_*` (cuBLAS) dispatch
/// call? Set when `LinAlgPatterns.resolveNodeRoute` picks `CudaBlas`; assemblers
/// append `#include "blade_linalg_cuda.hpp"`, which Build.fs sniffs to build the
/// companion `.cu` with nvcc and link it in.
///
/// A THIRD SEPARATE CELL because the three headers carry independent build
/// consequences (`-DBLADE_HAS_BLAS`+`-lopenblas`, `-DBLADE_HAS_LAPACK`, nvcc+
/// `-lcublas`): a host-BLAS program must not advertise CUDA, a device program
/// must not advertise OpenBLAS, yet one program can legitimately need BOTH (a
/// device `matmul` beside a host `dot`).
let internal cudaLinalgUsedStorage =
    System.Threading.AsyncLocal<bool ref>()

let cudaLinalgUsedCell () : bool ref =
    let v = cudaLinalgUsedStorage.Value
    if isNull (box v) then
        let fresh = ref false
        cudaLinalgUsedStorage.Value <- fresh
        fresh
    else v

/// Collector: did THIS program assembly emit a `blade_lapack::` dispatch call?
/// Set by the eigh emitter; assemblers append `#include "blade_lapack.hpp"`,
/// which Build.fs sniffs to add `-DBLADE_HAS_LAPACK`. Separate from
/// `linalgUsedCell` (independent headers/defines) so a gram/matmul program
/// never advertises a LAPACK dependency it doesn't have, or vice versa.
let internal lapackUsedStorage =
    System.Threading.AsyncLocal<bool ref>()

let lapackUsedCell () : bool ref =
    let v = lapackUsedStorage.Value
    if isNull (box v) then
        let fresh = ref false
        lapackUsedStorage.Value <- fresh
        fresh
    else v

/// Collector: did THIS program assembly emit code calling the OpenMP RUNTIME
/// API (`omp_get_max_threads`/`omp_get_thread_num`), vs only `#pragma omp`
/// (which needs no header)? Set by the comm-licensed parallel-fold's manual
/// chunked path (which computes its own team size); assemblers append
/// `#include <omp.h>`. Same shape as linalgUsedCell.
let internal ompApiUsedStorage =
    System.Threading.AsyncLocal<bool ref>()

let ompApiUsedCell () : bool ref =
    let v = ompApiUsedStorage.Value
    if isNull (box v) then
        let fresh = ref false
        ompApiUsedStorage.Value <- fresh
        fresh
    else v

/// Optional refinement of split-timing: Some name starts the compute clock
/// immediately before that binding (everything earlier is "input allocation"),
/// isolating one final kernel's runtime from its input prep. None falls back to
/// "first compute binding starts the clock". Dependency-safe: bindings emit in
/// strict ID order, so everything the named binding reads is already emitted.
let internal splitTimingOnlyBindingStorage =
    System.Threading.AsyncLocal<string option ref>()

let internal splitTimingOnlyBindingCell () : string option ref =
    let v = splitTimingOnlyBindingStorage.Value
    if isNull (box v) then
        let fresh = ref None
        splitTimingOnlyBindingStorage.Value <- fresh
        fresh
    else v

let setSplitTimingOnlyBinding (name: string option) : unit =
    (splitTimingOnlyBindingCell ()).Value <- name

let splitTimingOnlyBinding () : string option =
    (splitTimingOnlyBindingCell ()).Value


/// Query whether OpenMP test-mode instrumentation should be emitted.
let ompTestModeEnabled () : bool =
    (ompTestModeCell ()).Value

/// Memcheck emission gate (BLADE_MEMCHECK=1): the generated program includes
/// blade_memcheck.hpp, whose static Report object brackets main() with
/// ASan-hook allocation accounting and prints one BLADE-MEMCHECK line on
/// stderr at exit; Build.fs switches to the Debug+ASan compile profile off
/// the same read. An environment read, not an AsyncLocal: the flag is a
/// per-process harness pin (like BLADE_MARCH/BLADE_FP_CONTRACT in Build.fs),
/// set by `blade run --memcheck` before codegen starts. Default
/// (unset/empty/"0") emits NOTHING -- default output stays byte-identical,
/// and the interpreter/REPL lanes never pay for it.
let memcheckEnabled () : bool =
    match System.Environment.GetEnvironmentVariable "BLADE_MEMCHECK" with
    | null | "" | "0" -> false
    | _ -> true

/// CUDA emission gate. genCudaKernel[Simplicial] emits an `extern "C"` launch
/// call into the host .cpp plus a `__global__` kernel into a separate .cu, which
/// only links when the .cu is built alongside it -- true only in the dedicated
/// CUDA test phase. During ordinary (host-only) compilation the .cu is never
/// built, so a launch call would be an undefined-symbol link error; this flag
/// keeps the `cuda` clause inert (host fallback) outside that phase. Default OFF,
/// AsyncLocal like ompTestMode.
let internal cudaEmitModeStorage =
    System.Threading.AsyncLocal<bool ref>()

let cudaEmitModeCell () : bool ref =
    let v = cudaEmitModeStorage.Value
    if isNull (box v) then
        let fresh = ref false
        cudaEmitModeStorage.Value <- fresh
        fresh
    else v

/// Enable/disable actual CUDA kernel emission (called by the CUDA test phase).
let setCudaEmitMode (on: bool) : unit =
    (cudaEmitModeCell ()).Value <- on

/// Query whether CUDA kernels should actually be emitted (vs host fallback).
let cudaEmitModeEnabled () : bool =
    (cudaEmitModeCell ()).Value

/// MPI emission gate. When ON, `where mpi` kernels decompose across ranks (SPMD:
/// slab/flat-range local loop + MPI_Allgatherv), and the program gains
/// MPI_Init/Finalize + `#include <mpi.h>` (needs -lmsmpi, runs meaningfully only
/// under mpiexec). OFF during ordinary compilation, so the default suite never
/// needs an MPI toolchain. Same AsyncLocal pattern as cudaEmitMode.
let internal mpiEmitModeStorage =
    System.Threading.AsyncLocal<bool ref>()

let mpiEmitModeCell () : bool ref =
    let v = mpiEmitModeStorage.Value
    if isNull (box v) then
        let fresh = ref false
        mpiEmitModeStorage.Value <- fresh
        fresh
    else v

/// Enable/disable MPI decomposition emission (`blade run --mpi N`, MPI tests).
let setMpiEmitMode (on: bool) : unit =
    (mpiEmitModeCell ()).Value <- on

/// Query whether MPI decomposition should actually be emitted (vs serial).
let mpiEmitModeEnabled () : bool =
    (mpiEmitModeCell ()).Value

/// The MPI datatype constant for an element type, or None when it has no direct
/// MPI datatype (bool, structs -- not MPI-eligible; Allgatherv needs a native
/// one). Complex uses the MPI-2.2 C complex datatypes: std::complex<T> is
/// layout-compatible with `T _Complex` (T[2]); verified byte-correct at -n
/// 1/2/4. No reduction-op support is assumed: every use site is pure data
/// movement (reductions are MPI-ineligible upfront).
let mpiDatatypeOf (et: ElemType) : string option =
    match et with
    | ETFloat64 -> Some "MPI_DOUBLE"
    | ETFloat32 -> Some "MPI_FLOAT"
    | ETInt64 -> Some "MPI_LONG_LONG"
    | ETInt32 -> Some "MPI_INT"
    | ETComplex128 -> Some "MPI_C_DOUBLE_COMPLEX"
    | ETComplex64 -> Some "MPI_C_FLOAT_COMPLEX"
    | _ -> None

/// Whether any callable in the module requested MPI decomposition. A PURE
/// module predicate (not an emission-time cell) because program assembly
/// computes includes and printCode BEFORE genModule runs. Lifted lambdas land
/// in module.Functions, so both kernel forms (lambda / top-level fn) are seen.
let moduleUsesMpi (modul: IRModule) : bool =
    modul.Functions |> List.exists (_.IsMpiParallel)

/// Whether any kernel is the MPI-outer/OpenMP-inner hybrid (`where mpi,
/// omp(...)`). Drives the thread-aware MPI init: hybrid ranks host an OMP
/// team, so main() must request MPI_THREAD_FUNNELED (only the main thread
/// makes MPI calls -- every Allgatherv is outside the omp region). Pure-mpi
/// modules keep plain MPI_Init byte-identically.
let moduleHybridMpiOmp (modul: IRModule) : bool =
    modul.Functions |> List.exists (fun f -> f.IsMpiParallel && f.IsOmpParallel)

/// Whether the program CURRENTLY being assembled has MPI scaffolding (emit gate
/// on AND module uses mpi) -- MPI_Init has run and __blade_mpi_rank/size exist.
/// Set alongside the generators' mpiOn computation; consumed by provider-I/O
/// intercepts (distributed packed reads, rank-0 write guards).
let internal mpiProgramOnStorage =
    System.Threading.AsyncLocal<bool ref>()

let internal mpiProgramOnCell () : bool ref =
    let v = mpiProgramOnStorage.Value
    if isNull (box v) then
        let fresh = ref false
        mpiProgramOnStorage.Value <- fresh
        fresh
    else v

let setMpiProgramOn (on: bool) : unit =
    (mpiProgramOnCell ()).Value <- on

let mpiProgramOn () : bool =
    (mpiProgramOnCell ()).Value


/// Expression-position codegen refusals, verbatim, for the `#error` directives
/// the assembler appends to the generated translation unit. Separate from
/// `exprWarningsCell` because that cell also carries `codegenError`'s messages
/// (which already emit their own `#error` at the refusal site) and the
/// unresolved-type notes, none of which should be re-emitted.
///
/// WHY this exists. `exprError` alone emits a bare `BLADE_CODEGEN_ERROR_...`
/// identifier. That does fail the C++ compile -- but as an
/// "identifier not declared" from g++, not as a Blade refusal. The corpus
/// runner's `// REJECT-AT: codegen` verdict is decided by
/// `cppCode.Contains "#error"` (tests/Runner.fs), so without this the entire
/// expression-position sentinel class could never form a PASSING reject probe.
/// AsyncLocal per-flow ref for the same per-parallel-test-task isolation as
/// exprWarningsCell.
let internal exprSentinelsStorage =
    System.Threading.AsyncLocal<string list ref>()

let exprSentinelsCell () : string list ref =
    let v = exprSentinelsStorage.Value
    if isNull (box v) then
        let fresh = ref []
        exprSentinelsStorage.Value <- fresh
        fresh
    else v

// UNHANDLED IR NODES, as opposed to deliberate refusals.
//
// Every other message that reaches `exprError` / `codegenError` is a REFUSAL:
// codegen understood the construct and declined to render it, with a sentence
// saying what to write instead. Those are features, and their `#error`
// delivery is the contract `// REJECT-AT: codegen` probes are written against.
//
// A node arriving at a catch-all arm is a different animal: it means this
// position grew no arm for that shape, which is a hole in the back end, not a
// statement about the user's program. Delivering THAT as a C++ compile error
// naming `BLADE_CODEGEN_ERROR_UNSUPPORTED_IR_NODE_<X>` hands the user a g++
// diagnostic about an undeclared C++ identifier for a Blade-side gap -- no
// code, no file, no line, and nothing to search for.
//
// So the catch-alls ALSO record here, and the compile driver turns a non-empty
// channel into a coded, spanned Blade diagnostic (BL7001) and refuses BEFORE
// the C++ compiler is ever invoked. The `#error` half is kept as-is: the test
// harness drives codegen directly rather than through the driver, and the
// guard is what its codegen-stage verdict reads.
let internal unhandledNodesStorage =
    System.Threading.AsyncLocal<(string * string) list ref>()

let unhandledNodesCell () : (string * string) list ref =
    let v = unhandledNodesStorage.Value
    if isNull (box v) then
        let fresh = ref []
        unhandledNodesStorage.Value <- fresh
        fresh
    else v

/// The declaration currently being emitted, for attributing an unhandled node
/// to a source position. `genModule`/`genModuleSplit` set it around each item.
let internal currentDeclStorage = System.Threading.AsyncLocal<string ref>()

let currentDeclCell () : string ref =
    let v = currentDeclStorage.Value
    if isNull (box v) then
        let fresh = ref ""
        currentDeclStorage.Value <- fresh
        fresh
    else v

let setCurrentCodegenDecl (name: string) : unit = (currentDeclCell ()).Value <- name

/// Record that `nodeName` reached codegen with no arm in `position`, tagging it
/// with the declaration being emitted. Deduplicated: one loop nest can render
/// the same hole once per iteration of the emitter, and the user needs to be
/// told once.
let recordUnhandledIRNode (position: string) (nodeName: string) : unit =
    let cell = unhandledNodesCell ()
    let entry = ($"{nodeName} in {position}", (currentDeclCell ()).Value)
    if not (List.contains entry cell.Value) then
        cell.Value <- cell.Value @ [entry]

/// Drain the unhandled-node channel as ready-made diagnostics: BL7001
/// (backend limit), spanned at the enclosing declaration when Lowering
/// recorded one. Called by the compile driver after codegen.
let takeUnhandledIRNodeDiagnostics () : Blade.Diagnostics.Diagnostic list =
    let cell = unhandledNodesCell ()
    let entries = cell.Value
    cell.Value <- []
    entries |> List.map (fun (what, declName) ->
        let where = if declName = "" then "" else $" (while emitting '{declName}')"
        Blade.Diagnostics.Codes.backendLimit (IR.declSpanOf declName)
            ($"code generation has no rule for {what}{where}")
        |> Blade.Diagnostics.withNote
            "this is a gap in the C++ back end, not an error in your program -- \
             the construct typechecked and lowered. Please report it; meanwhile, \
             materializing the sub-expression with `|> compute` and binding it to \
             a name often routes around the hole.")

// DELIBERATE CODEGEN REFUSALS, surfaced as coded diagnostics.
//
// Every `#error` / `BLADE_CODEGEN_ERROR_` splice that reaches the generated
// C++ is a refusal codegen chose on purpose -- but delivering it THROUGH g++
// hands the user a preprocessor error (or an undeclared-identifier error)
// with no Blade file/line and no code to search for. So refusal sites ALSO
// record here, tagged with the declaration being emitted, and the compile
// driver turns a non-empty channel into BL7004 diagnostics and refuses
// before the C++ compiler runs -- the exact mechanism BL7001 uses for
// unhandled nodes, applied to the refusal class.
//
// The `#error` halves are kept as-is: the test harness drives codegen
// directly (never through the driver), and its `// REJECT-AT: codegen`
// verdict reads `cppCode.Contains "#error"`.
//
// GATING (see takeCodegenRefusalDiagnostics): a rendered-then-DISCARDED
// refusal line records here but never reaches the translation unit (the
// lazy-rendering trap documented at the row-write emitter). The driver
// therefore refuses only when the generated source actually carries a
// marker; otherwise the channel is drained and dropped, preserving the
// discarded-render behavior byte for byte.
let internal codegenRefusalsStorage =
    System.Threading.AsyncLocal<(string * string) list ref>()

let codegenRefusalsCell () : (string * string) list ref =
    let v = codegenRefusalsStorage.Value
    if isNull (box v) then
        let fresh = ref []
        codegenRefusalsStorage.Value <- fresh
        fresh
    else v

let recordCodegenRefusal (msg: string) : unit =
    let cell = codegenRefusalsCell ()
    let entry = (msg, (currentDeclCell ()).Value)
    if not (List.contains entry cell.Value) then
        cell.Value <- cell.Value @ [entry]

/// Drain the refusal channel as BL7004 diagnostics, spanned at the enclosing
/// declaration. `cppCode` gates delivery: entries are returned only when the
/// generated source actually carries a refusal marker (a discarded render
/// records a message but splices nothing -- see the channel's note). The
/// channel is drained either way so state never leaks across compilations.
let takeCodegenRefusalDiagnostics (cppCode: string) : Blade.Diagnostics.Diagnostic list =
    let cell = codegenRefusalsCell ()
    let entries = cell.Value
    cell.Value <- []
    if entries.IsEmpty
       || not (cppCode.Contains "#error" || cppCode.Contains "BLADE_CODEGEN_ERROR_") then []
    else
        entries |> List.map (fun (msg, declName) ->
            let where = if declName = "" then "" else $" (while emitting '{declName}')"
            Blade.Diagnostics.Codes.backendRefusal (IR.declSpanOf declName)
                ($"{msg}{where}"))

/// Record an expression-level warning and return a C++ expression that causes a compile error.
/// The identifier is the in-place marker; the companion `#error` directive is
/// appended to the translation unit by `genSelfContainedProgramFromIR` (an
/// expression position cannot host a preprocessor directive, which is why the
/// two halves are split).
let exprError (msg: string) : string =
    let cell = exprWarningsCell ()
    cell.Value <- cell.Value @ [msg]
    let sentinels = exprSentinelsCell ()
    if not (List.contains msg sentinels.Value) then
        sentinels.Value <- sentinels.Value @ [msg]
    // Refusal channel (BL7004): every sentinel becomes an `#error` in the
    // translation unit, so record it -- EXCEPT the unhandled-node catch-alls,
    // which have already recorded themselves on the BL7001 channel and would
    // otherwise be reported twice under two codes.
    if not (msg.StartsWith "unsupported IR node") then
        recordCodegenRefusal msg
    $"""BLADE_CODEGEN_ERROR_{(msg.Replace(" ", "_").Replace("'", "").Replace("(", "").Replace(")", "").Replace(",", "").Replace(":", "").Replace("\"", "").ToUpper())}"""

// Substitution map for contains-aware mask rendering.
//
// When a mask renderer hoists a contains-set build into its preamble, it
// registers each hoisted IRContains node here (keyed by object reference)
// alongside the C++ name of the precomputed set. As exprToCpp walks the
// predicate body to produce the C++ string for the count/fill loops'
// `if (...)` clauses, the IRContains arm consults this map first: a hit
// emits `<set>.count(<value>)`; a miss falls through to the original
// linear-scan IIFE.
//
// Reference equality (not structural) is essential: two structurally-equal
// IRContains nodes can appear at distinct positions (e.g. once where the
// build is hoistable, once where it isn't), and the map must distinguish
// them. Produced and consumed within a single rendering pass over the same
// IR tree, so references are stable. Linear search is fine: per-mask probe
// counts are small (typically 1-3).
type SubstMap = (IRExpr * string) list

let internal emptySubst : SubstMap = []

let internal trySubst (subst: SubstMap) (node: IRExpr) : string option =
    subst
    |> List.tryFind (fun (n, _) -> System.Object.ReferenceEquals(n, node))
    |> Option.map snd

/// Check if an expression is unit/void-typed (should not generate a value)
let isUnitExpr (expr: IRExpr) : bool =
    match expr with
    | IRLit IRLitUnit -> true
    | IRAssign _ -> true
    | IRForRange _ -> true
    | _ -> false

let emptyContext () = {
    VarNames = Map.empty
    ParamIds = Set.empty
    Indent = 0
    StaticDecls = []
    TupleChildren = Map.empty
    DeferredComputations = Map.empty
    ObjectLoopBindings = Map.empty
    ProviderReads = Map.empty
    ProviderWrites = Map.empty
    StreamedArrays = Map.empty
    StructuralGroupings = Set.empty
    RandomInits = Map.empty
    CompoundInits = Map.empty
    SparseInits = Map.empty
    GroupedArrays = Map.empty
    MutableArrayLets = Set.empty
    Warnings = ref []
}

let indent ctx = { ctx with Indent = ctx.Indent + 1 }
let indentStr ctx = String.replicate ctx.Indent "    "

/// Record a codegen warning and return a C++ #error directive.
/// This ensures the generated C++ will not compile silently. Also records on
/// the refusal channel so the compile driver reports it as BL7004 instead of
/// letting g++ deliver the raw `#error`.
let codegenError (ctx: CodeGenContext) (ind: string) (msg: string) : string list =
    ctx.Warnings.Value <- ctx.Warnings.Value @ [msg]
    recordCodegenRefusal msg
    [sprintf "%s#error \"Blade codegen: %s\"" ind (msg.Replace("\"", "'"))]

/// Direct-splice refusal for the sprintf-`#error` sites that have no ctx at
/// hand: record on the BL7004 channel and return the `#error` line, quoted
/// exactly as the site has always emitted it (msg goes inside the quotes
/// verbatim -- callers that historically carried a "Blade codegen: " prefix
/// pass it in msg).
let refusalErrorLine (ind: string) (msg: string) : string =
    recordCodegenRefusal msg
    sprintf "%s#error \"%s\"" ind (msg.Replace("\"", "'"))

/// C++ reserved words and built-in type names that cannot be used as identifiers
let cppReservedWords = Set.ofList [
    // Types that conflict with Blade names
    "double"; "float"; "int"; "long"; "short"; "char"; "bool"; "void"; "auto"
    "signed"; "unsigned"; "const"; "volatile"; "static"; "extern"; "register"
    // Keywords
    "class"; "struct"; "enum"; "union"; "namespace"; "template"; "typename"
    "virtual"; "override"; "final"; "public"; "private"; "protected"
    "new"; "delete"; "this"; "return"; "if"; "else"; "for"; "while"; "do"
    "switch"; "case"; "break"; "continue"; "goto"; "default"; "try"; "catch"; "throw"
    "sizeof"; "alignof"; "decltype"; "typedef"; "using"; "operator"
    "true"; "false"; "nullptr"; "inline"; "constexpr"; "mutable"
]

/// Sanitize a name to avoid C++ reserved word conflicts
let sanitizeCppName (name: string) : string =
    if Set.contains name cppReservedWords then name + "_"
    else name

/// Identifiers the generated TU's own includes declare at GLOBAL scope. None
/// of them is a C++ keyword, so `cppReservedWords` does not catch them, but a
/// `using <name> = ...;` alias redeclares them just as fatally:
///
///     using time = int64_t;   // error: redeclared as different kind of entity
///
/// which is what a Zarr/NetCDF store with a dimension named `time` -- the
/// geoscience default -- used to emit. The list is the C standard library
/// (<cmath>/<cstdlib>/<cstdio>/<cstring>/<ctime>/<cctype>, all included or
/// pulled in transitively), the two `using std::` declarations the preamble
/// hoists into global scope, and the POSIX names MinGW and glibc leak from the
/// same headers. Blade's OWN runtime headers need no entries: everything they
/// declare lives in a namespace.
let cppLibraryGlobals = Set.ofList [
    // <cmath>, plus the non-standard Bessel/gamma names both MinGW and glibc declare
    "abs"; "acos"; "acosh"; "asin"; "asinh"; "atan"; "atan2"; "atanh"; "cbrt"
    "ceil"; "copysign"; "cos"; "cosh"; "drem"; "erf"; "erfc"; "exp"; "exp2"
    "expm1"; "fabs"; "fdim"; "floor"; "fma"; "fmax"; "fmin"; "fmod"; "frexp"
    "gamma"; "hypot"; "ilogb"; "j0"; "j1"; "jn"; "ldexp"; "lgamma"; "llrint"
    "llround"; "log"; "log10"; "log1p"; "log2"; "logb"; "lrint"; "lround"
    "modf"; "nan"; "nearbyint"; "nextafter"; "nexttoward"; "pow"; "remainder"
    "remquo"; "rint"; "round"; "scalbln"; "scalbn"; "significand"; "sin"
    "sinh"; "sqrt"; "tan"; "tanh"; "tgamma"; "trunc"; "y0"; "y1"; "yn"
    // <cstdlib>
    "abort"; "atexit"; "atof"; "atoi"; "atol"; "atoll"; "bsearch"; "calloc"
    "div"; "free"; "getenv"; "labs"; "ldiv"; "llabs"; "lldiv"; "malloc"
    "mblen"; "mbstowcs"; "mbtowc"; "qsort"; "rand"; "realloc"; "srand"
    "strtod"; "strtof"; "strtol"; "strtold"; "strtoll"; "strtoul"; "strtoull"
    "system"; "wcstombs"; "wctomb"
    // <cstdio>
    "clearerr"; "fclose"; "feof"; "ferror"; "fflush"; "fgetc"; "fgetpos"
    "fgets"; "fopen"; "fprintf"; "fputc"; "fputs"; "fread"; "freopen"
    "fscanf"; "fseek"; "fsetpos"; "ftell"; "fwrite"; "getc"; "getchar"
    "gets"; "perror"; "printf"; "putc"; "putchar"; "puts"; "remove"; "rename"
    "rewind"; "scanf"; "setbuf"; "setvbuf"; "snprintf"; "sprintf"; "sscanf"
    "tmpfile"; "tmpnam"; "ungetc"; "vfprintf"; "vprintf"; "vsprintf"
    // <cstring>
    "memchr"; "memcmp"; "memcpy"; "memmove"; "memset"; "strcat"; "strchr"
    "strcmp"; "strcoll"; "strcpy"; "strcspn"; "strerror"; "strlen"; "strncat"
    "strncmp"; "strncpy"; "strpbrk"; "strrchr"; "strspn"; "strstr"; "strtok"
    "strxfrm"
    // <ctime> -- reached transitively through <chrono> and pthread.h
    "asctime"; "clock"; "ctime"; "difftime"; "gmtime"; "localtime"; "mktime"
    "strftime"; "time"
    // <cctype>, <csignal>, <csetjmp>, <clocale>
    "isalnum"; "isalpha"; "isblank"; "iscntrl"; "isdigit"; "isgraph"
    "islower"; "isprint"; "ispunct"; "isspace"; "isupper"; "isxdigit"
    "tolower"; "toupper"; "longjmp"; "raise"; "setjmp"; "signal"
    "localeconv"; "setlocale"
    // Hoisted into global scope by the preamble's `using std::cout/endl;`
    "cout"; "endl"
    // POSIX names MinGW/glibc declare alongside the above
    "access"; "close"; "daylight"; "dup"; "environ"; "index"; "kill"; "link"
    "open"; "optarg"; "optind"; "pclose"; "pipe"; "popen"; "random"; "read"
    "recv"; "rindex"; "select"; "send"; "sleep"; "socket"; "srandom"; "stat"
    "timezone"; "times"; "tzname"; "unlink"; "wait"; "write"
]

/// Emitted spelling for an INDEX TYPE's C++ alias. `using <name> = int64_t;`
/// declares a name in the global namespace, so it must dodge both C++ keywords
/// and the library globals above -- unlike an ordinary binding, which lands in
/// a function body and may legally shadow `time`.
///
/// Every emission of an index type name goes through here: the `using` itself
/// (CodeGen.genTypeDefs) and every reference to it (irTypeToCpp's IRefNamed
/// arm). A name that collides gains a `_` suffix, the same convention
/// `sanitizeCppName` uses; a name that does not is emitted verbatim, so the
/// generated C++ for the overwhelming majority of index types is byte-identical
/// to what it was before this guard existed.
let indexTypeCppName (name: string) : string =
    if Set.contains name cppReservedWords || Set.contains name cppLibraryGlobals
    then name + "_"
    else name

let addVarName id name ctx = 
    { ctx with VarNames = Map.add id (sanitizeCppName name) ctx.VarNames }


// Type Inference Helper (for code generation)

// Capture computation and match-case usage checks call IR.collectVarRefsIR,
// the canonical ExprShape-based collector.

/// Struct-fields registry for `IRFieldAccess` type resolution. Forwards to
/// IR.fs's AsyncLocal cache (the same registry the lift pass populates), so
/// both population points (liftInlineFormsModule, genModule) fill one cache
/// from the same module's Types.
let setCodegenStructFieldsCache (types: IRTypeDef list) = IR.setStructFieldsCache types

// CompoundIndexForm, classifyCompoundIndexTuple, synthSlotId* live in IR.fs
// beside the canonical typeOf; they resolve here via `open Blade.IR`.

/// Thin alias of the canonical derivation in IR.typeOf (shared with the lift
/// pass, so codegen and lift can't diverge on an expression's type). Kept
/// under this name to avoid churning ~90 call sites.
let inferExprType (expr: IRExpr) : IRType = IR.typeOf expr

// C++ Type Mapping

/// Convert a primitive ElemType enum value to C++ type string.
/// Use this only when you have a raw `ElemType` value (e.g., from
/// promoteElemType). For array element types post-B2, use `elemTypeToCpp`
/// which takes the full IRType.
let primTypeToCpp = function
    | ETInt32 -> "int32_t"
    | ETInt64 -> "int64_t"
    | ETFloat32 -> "float"
    | ETFloat64 -> "double"
    | ETComplex64 -> "std::complex<float>"
    | ETComplex128 -> "std::complex<double>"
    | ETBool -> "bool"
    | ETUnit -> "void"
    | ETString -> "std::string"

/// Get rank (total dimensions) from array type
let arrayRank (arr: IRArrayType) =
    arr.IndexTypes |> List.sumBy (_.Rank)

// ---------------------------------------------------------------------------
// Array-SHAPE predicates: which C++ wrapper an array type is spelled as.
//
// Defined HERE, above the irTypeToCpp/elemTypeToCpp recursion group, because
// `cppArrayTypeStr` joins that group (it must call elemTypeToCpp) and every
// signature-rendering site inside it -- notably arrowSlotTypeForFuncSig, which
// renders std::function<> slots -- has to reach the SAME wrapper decision the
// declaration sites use. They were previously declared below the group, which
// forced arrowSlotTypeForFuncSig to hardcode `Array<T, N>` and silently
// disagree with the ragged/compound/sparse declaration wrappers.
// ---------------------------------------------------------------------------

/// Detect whether an IRArrayType is RAGGED -- any index in the ragged family
///   - __raggedidx           : a ragged literal's inner dimension
///   - __group_member        : a group_by result's inner dimension
///   - __raggedidx_opaque    : opaque RaggedIdx<_> (kernel param / sub-array)
/// All share the property that the array shape carries a per-row lengths
/// companion at codegen time.
let isRaggedArrayType (arrTy: IRArrayType) : bool =
    arrTy.IndexTypes |> List.exists (fun idx -> isRaggedFamilyKind idx.IxKind)

/// A rank-1 value whose single axis is a RAGGED-FAMILY inner dimension: a
/// peeled/indexed row of a ragged literal (__raggedidx*), a DepIdx-allocated
/// array (__depidx_inner), or a group_by result (__group_member). All three
/// share the same runtime row shape -- a pointer plus a per-row length --
/// and are represented as `RaggedRow<T>` when bound (`.len`, operator[]).
/// This is the ONE predicate for "does this rank-1 operand carry its length
/// inline as .len rather than via .extents", used consistently by the
/// sub-view binding emission, reduce (both forms), IRExtent, and print, so
/// the accessor never disagrees with the declared type.
let isRaggedRowType (arrTy: IRArrayType) : bool =
    arrTy.IndexTypes.Length = 1 && isRaggedRowKind arrTy.IndexTypes.[0].IxKind


/// Detect whether an IRArrayType represents a DepIdx array -- outer Idx plus an
/// inner record whose Extent is a function of the outer iteration index
/// (the `__depidx_inner` tag on a non-first index). Once allocated it has the
/// same `_lens`/`_offsets`/row-pointer runtime layout as ragged, so iteration
/// predicates treat both as "has row-lengths companion" via `isRaggedArrayType
/// OR isDepIdxArrayType`; genArrayLiteral keeps a separate branch since lens
/// come from the formula, not literal structure.
let isDepIdxArrayType (arrTy: IRArrayType) : bool =
    arrTy.IndexTypes |> List.exists (fun idx ->
        idx.IxKind = IxKDepInner)

/// Detect whether an IRArrayType is a CompoundIdx<mask> array -- a masked
/// product space (formalism 4.5), tabulated at runtime, rendered as
/// `Compound<T, RANK>`, accessed by whole-tuple gather. Matches the
/// `__compoundidx` tag by EXACT equality, NOT a prefix test:
/// `__compoundidx_dynamic` is the unrelated group_by compound-key index and
/// must not be rendered as Compound<T,RANK>.
let isCompoundArrayType (arrTy: IRArrayType) : bool =
    arrTy.IndexTypes |> List.exists (fun idx ->
        idx.IxKind = IxKCompound)

/// Detect whether an IRArrayType is a SparseIdx<keys> array -- an explicit
/// valid-tuple enumeration (formalism 3.5) rendered as `Sparse<T, RANK>`,
/// accessed by whole-tuple hash lookup. Twin of isCompoundArrayType over the
/// sparse kind.
let isSparseArrayType (arrTy: IRArrayType) : bool =
    arrTy.IndexTypes |> List.exists (fun idx ->
        idx.IxKind = IxKSparse)

/// Does this array type DECLARE as `RaggedRow<T>` in C++? THE single predicate
/// for that spelling: `cppArrayTypeStr` (every declaration position, including
/// std::function<> slots via arrowSlotTypeForFuncSig) and the grouped/ragged
/// peel's row bindings both call it, so a peeled row's declared type can never
/// drift from the signature it is passed to. The peel sites used to inline
/// this test, which is exactly how they drifted.
///
/// KNOWN GAP -- deliberately NARROWER than `isRaggedRowType`: a rank-1
/// `IxKGroupMember` row (a peeled `group_by` row) is a ragged ROW at every
/// ACCESSOR site (`.len`, via isRaggedRowType in reduce/prodsum/IRExtent/print)
/// but still DECLARES as `Array<T,1>` here. Widening this to `isRaggedRowType`
/// is the correct end state, and it makes `method_for(grouped) <@> lambda(g)
/// -> f(g)` (unannotated param, ragged-annotated callee) compile -- but it
/// then breaks ABSTRACT-ARITY callees: a `function f(t: T^1)` is zonk-closed
/// to a deduced-rank `Array<T,1>` (IxKPlain), and HM monomorphization learns
/// only ELEMENT bindings (`unifyParamWithArg`'s ArrayElem arm, keyed by
/// infer-var id), so it can never adopt the argument's ragged SHAPE. Widening
/// therefore needs monomorphization to specialize on array shape -- a new
/// binding channel plus an IxKind component in `canonTypeKey`. The pins that
/// fail if it is widened alone: tests/corpus/sql-group-by/029 and
/// tests/corpus/functions/068.
let declaresAsRaggedRow (arrTy: IRArrayType) : bool =
    (isRaggedArrayType arrTy || isDepIdxArrayType arrTy)
    && arrTy.IndexTypes.Length = 1

/// Convert an IRType in array-element position to a C++ type string.
/// Dispatches via active patterns:
///   - PrimElem / AnyPrimElem: render the primitive (with units erased).
///   - NamedElem: render the struct/sum's name. Codegen for nominal-typed
///     element arrays is still future work (the `promote<T, k>` template
///     handles them at the C++ level, but Blade-side support for
///     constructing/operating on them is incomplete).
///   - InferElem: codegen-time error. Should never reach here if typecheck
///     and zonking did their job.
///   - InvalidElem: hard error -- these types have no value-level meaning.
///   - PolyElem: not implemented; specialization should have replaced it.
///   - Other (FuncElem, ArrayElem, TupleElem): delegate to irTypeToCpp,
///     which already renders these correctly. Codegen for arrays-of-these
///     is future work but the type system stops blocking them.
let rec elemTypeToCpp (ty: IRType) : string =
    match ty with
    // Tagged element types must route through irTypeToCpp BEFORE the
    // AnyPrimElem catch, because AnyPrimElem extracts the inner primitive
    // (e.g., int64) which loses the typedef alias name. irTypeToCpp's
    // IRTIdxTagged arm renders IRefNamed as the alias unconditionally.
    | IRTIdxTagged _ -> irTypeToCpp ty
    | AnyPrimElem et -> primTypeToCpp et
    | NamedElem "String" -> "std::string"
    | NamedElem name -> name
    | InferElem id ->
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @
            [$"elemTypeToCpp: unresolved type variable T?{id} in element position"]
        $"BLADE_UNRESOLVED_ELEM_TYPE_{id}"
    | PolyElem (_, var) ->
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @
            [$"elemTypeToCpp: PolyElem<{var}> in element position is not yet implemented"]
        "BLADE_NOT_IMPLEMENTED_POLY_ELEM"
    | InvalidElem ->
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @
            [sprintf "elemTypeToCpp: invalid type in element position: %A" ty]
        "BLADE_INVALID_ELEM_TYPE"
    | _ ->
        // FuncElem / ArrayElem / TupleElem / other: delegate to irTypeToCpp.
        irTypeToCpp ty

/// Convert IR type to C++ type string. Mutually recursive with
/// `elemTypeToCpp` because the array element type is itself an IRType.
and irTypeToCpp = function
    | IRTScalar et -> primTypeToCpp et
    | IRTTuple ts ->
        // Array-shaped elements render as the WRAPPER form (Array<T, N>),
        // not the raw promote<>::type pointer: a std::tuple is a value
        // boundary exactly like a function signature (which already uses
        // the wrapper via arrowSlotTypeForFuncSig), and the wrapper's
        // implicit conversion to the raw pointer means a raw-element tuple
        // silently DROPS extents when a wrapper flows in -- anything
        // downstream needing `.extents` (auto-print, loop bounds) then
        // breaks. arrowSlotTypeForFuncSig delegates non-array elements
        // back to irTypeToCpp, so scalar/nested-tuple elements render as
        // before.
        $"""std::tuple<{(ts |> List.map arrowSlotTypeForFuncSig |> String.concat ", ")}>"""
    | IRTUnit -> "void"
    | IRTLoop lt ->
        match lt.Kind with
        | LKMethod -> "BLADE_ERROR_METHOD_LOOP_TYPE"
        | LKObject -> "BLADE_ERROR_OBJECT_LOOP_TYPE"
    | IRTComputation t -> irTypeToCpp t  // Computation<T> erases to T at runtime
    | IRTPoly (base', _) -> 
        // After monomorphization, IRTPoly should not reach codegen.
        // If it does, fall back to the base type.
        irTypeToCpp base'
    | IRTNat _ -> "size_t"
    | IRTIdxTagged (inner, idxRef) ->
        // Parallel to IRTUnitAnnotated: tag is a typecheck-time invariant,
        // erased at codegen. For IRefNamed, render the typedef alias
        // unconditionally -- a `using <name> = ...;` is emitted alongside
        // the type declaration, so the alias is in scope. For IRefAnon
        // there's no alias to use; render the inner type directly.
        match idxRef with
        // Compound-inner halo window: the param is a POINTER into the
        // materialized compound index's contiguous rank_to_tuple table at the
        // center cell, so w(o) neighbor reads are param-local pointer
        // arithmetic -- valid inside lifted standalone kernel functions where
        // no nest-scope alias could reach. Currently rank-1 masks only (array size 1).
        | IRefNamed name when name.StartsWith("__halowin|c:") -> "const std::array<size_t, 1>*"
        // Internal ("__"-prefixed) tags -- e.g. a dense halo window -- are
        // compiler-synthesized and have no `using` alias, so they must
        // erase to the raw inner type rather than leaking the tag as a C++
        // type name. User aliases (no "__") render their emitted typedef.
        | IRefNamed name when name.StartsWith("__") -> irTypeToCpp inner
        | IRefNamed name -> indexTypeCppName name
        | IRefAnon _ -> irTypeToCpp inner
        // The `Base<_>` wildcard names no index type, so there is no `using`
        // alias to render -- erase to the inner type, same as IRefAnon.
        | IRefAny -> irTypeToCpp inner
    | IRTDist _ ->
        // Dist<r, tau> is erased at lowering (a Dist value lowers to the tuple
        // of its packed cumulant component arrays); reaching codegen means
        // the erasure was skipped.
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @ ["irTypeToCpp: IRTDist reached codegen -- Dist erasure was skipped at lowering"]
        "BLADE_ERROR_DIST_TYPE"
    | IRTNamed "String" -> "std::string"  // Blade String -> C++ std::string
    | IRTNamed name -> name  // Named types (structs, etc.) use their name directly
    | IRTInfer n ->
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @ [$"unresolved type variable _{n} reached codegen"]
        $"BLADE_UNRESOLVED_TYPE_{n}"
    | IRTUnitAnnotated (inner, _) -> irTypeToCpp inner  // Units erase at codegen
    | IRTGroupKeys _ -> "void*"  // GroupKeys is an opaque runtime structure
    | IRTArrow (slots, result, identity) ->
        // Three shapes possible:
        //   - all-SVal (incl. empty = nullary): std::function<RetType(Arg1, Arg2, ...)>,
        //     with array-typed slots rendered as the WRAPPER form (Array<T,N>) via
        //     arrowSlotTypeForFuncSig, matching genFuncDef's ArrayElem branch.
        //   - all-SIdx/SIdxVirt, non-empty: array-shaped arrow, renders as the raw
        //     `promote<elem, rank>::type` pointer -- what indexing an Array<T,N>/
        //     Ragged<T> via operator[] returns (`let row = arr(i)` needs this, not
        //     the wrapper, which is reserved for allocation sites and signatures).
        //   - mixed slot kinds: not yet expressible by language surface; sentinel.
        let isAllSVal = slots |> List.forall (_.IsSVal)
        let isAllStored = slots |> List.forall (_.IsSIdx)
        let isAllVirtual = slots |> List.forall (_.IsSIdxVirt)
        if isAllSVal then
            let paramTypes =
                slots |> List.map (function
                    | SVal t -> arrowSlotTypeForFuncSig t
                    | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.iceCodegen "unreachable -- guarded by isAllSVal")))
            let paramList = String.concat ", " paramTypes
            $"std::function<{(arrowSlotTypeForFuncSig result)}({paramList})>"
        elif (isAllStored || isAllVirtual) && not slots.IsEmpty then
            // Reconstruct an IRArrayType view for rendering
            let indexTypes =
                slots |> List.map (function
                    | SIdx i | SIdxVirt i -> i
                    | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.iceCodegen "unreachable")))
            let arr = {
                ElemType = result
                IndexTypes = indexTypes
                IsVirtual = isAllVirtual
                Identity = identity
            }
            $"promote<{(elemTypeToCpp arr.ElemType)}, {arrayRank arr}>::type"
        else
            let cell = exprWarningsCell ()
            cell.Value <- cell.Value @ ["IRTArrow with mixed slot kinds reached codegen (no language construct produces these yet)"]
            "BLADE_UNSUPPORTED_ARROW_TYPE"

/// Render an array type as its C++ type string. Five cases:
///   * CompoundIdx<mask> -> `Compound<T, RANK>` (RANK = mask dimensionality).
///   * SparseIdx<keys>   -> `Sparse<T, RANK>` (RANK = key tuple arity).
///   * Rank-1 ragged/dep-idx -> `RaggedRow<T>` (a peeled-row slice; kernel-side
///     peeling of a 2D ragged's inner dim, never a source-level annotation --
///     rank-1 ragged at the source level is malformed, `__error_ragged_no_prior`).
///   * Rank >= 2 ragged/dep-idx -> `Ragged<T>` (full multi-row container).
///   * Otherwise -> `Array<T, N>`.
/// The rank-1 case matters because `Ragged<T>::operator[]` returns `RaggedRow<T>`,
/// not `T`: a lambda whose param IS a peeled row must declare the row type
/// directly so `g[0]` resolves to the element.
///
/// THE single wrapper chooser. Every site that spells an array type in a
/// DECLARATION position -- function params and returns, captures, and
/// std::function<> slots via arrowSlotTypeForFuncSig -- goes through here, so
/// a signature can never disagree with the peel/binding that feeds it. Lives
/// in this recursion group (rather than below it, where it used to) precisely
/// so arrowSlotTypeForFuncSig can reach it.
and cppArrayTypeStr (arr: IRArrayType) : string =
    if isCompoundArrayType arr then
        // Compound<T, RANK>: a masked product space. RANK is the mask's
        // dimensionality, carried on the compound index type's Rank (a generic
        // "dimensions spanned" -- a rank here, not a symmetric arity). Read off
        // the compound index type directly rather than via arrayRank so that a
        // future surrounding-dims form would not fold extra axes into RANK.
        let rank =
            arr.IndexTypes
            |> List.tryFind (fun idx -> idx.IxKind = IxKCompound)
            |> Option.map (_.Rank)
            |> Option.defaultValue (arrayRank arr)
        $"Compound<{(elemTypeToCpp arr.ElemType)}, {rank}>"
    elif isSparseArrayType arr then
        // Sparse<T, RANK>: an explicit key enumeration. RANK is the key tuple
        // arity, carried on the sparse index type's Rank -- same read-off
        // discipline as the compound arm above.
        let rank =
            arr.IndexTypes
            |> List.tryFind (fun idx -> idx.IxKind = IxKSparse)
            |> Option.map (_.Rank)
            |> Option.defaultValue (arrayRank arr)
        $"Sparse<{(elemTypeToCpp arr.ElemType)}, {rank}>"
    elif declaresAsRaggedRow arr then
        // Rank-1 peeled row: carries its length inline as `.len`. Shared with
        // the peel's row bindings via `declaresAsRaggedRow` (see its doc for
        // the IxKGroupMember gap).
        $"RaggedRow<{(elemTypeToCpp arr.ElemType)}>"
    elif isRaggedArrayType arr || isDepIdxArrayType arr then
        // Rank >= 2 ragged/dep-idx container. A grouped array
        // ([__group_outer; __group_member]) deliberately does NOT land here:
        // group_by materializes a row-pointer skeleton (`Array<T*, 1>`), not
        // a Ragged<T>, and that representation is unchanged.
        $"Ragged<{(elemTypeToCpp arr.ElemType)}>"
    else
        $"Array<{(elemTypeToCpp arr.ElemType)}, {arrayRank arr}>"

/// Render a type for use inside a std::function<...> signature. Array types
/// render as the WRAPPER form to match function declarations at the call
/// boundary; everything else delegates to `irTypeToCpp`. Without this,
/// std::function<> templates would use the raw-pointer form
/// (`promote<T, N>::type`), mismatching genFuncDef's wrapper-form return type
/// and blocking `funcs[i] = arrayReturningFunc;` assignments.
///
/// Delegates to `cppArrayTypeStr` rather than hardcoding `Array<T, N>`: a
/// std::function<> slot is a DECLARATION position, and a
/// ragged/DepIdx/compound/sparse param must be spelled with the same wrapper
/// its function declaration uses or the assignment `std::function<...> f =
/// namedFn;` has no viable conversion. This previously hardcoded the dense
/// form, so passing a `function f(row: Array<T like RaggedIdx<_>>)` as a
/// kernel emitted `std::function<R(Array<T,1>)>` against a declared
/// `R f(RaggedRow<T>)` -- a g++ type mismatch at the forwarding call.
and arrowSlotTypeForFuncSig (ty: IRType) : string =
    match ty with
    | ArrayElem arr -> cppArrayTypeStr arr
    | _ -> irTypeToCpp ty


