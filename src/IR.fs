// Blade-DSL Intermediate Representation
// Lowered from AST, ready for optimization and code generation
// Implements the Structural Trinity: Loop Reification, Arity Polymorphism, Dimensional Currying

module Blade.IR

open Blade.Types

open System

// Core Type Definitions

/// Unique identifier for IR values
/// Index type in IR - represents a single dimension's structure
type IRLit =
    | IRLitInt of int64
    | IRLitFloat of float
    /// A float literal RECONCILED to a Float32 position (lowerLiteralValued):
    /// width is part of the literal so `typeOf` answers ETFloat32 and codegen
    /// emits an `f`-suffixed C++ literal -- a width-less `1.0` beside float
    /// operands promotes the op to double and then narrows at the store,
    /// which -Werror=float-conversion rejects.
    | IRLitFloat32 of float32
    | IRLitBool of bool
    | IRLitString of string
    | IRLitUnit

/// Binary operations
type IRBinOp =
    | IRAdd | IRSub | IRMul | IRDiv | IRMod | IRCaret  // ^ for power
    | IREq | IRNeq | IRLt | IRLe | IRGt | IRGe
    | IRAnd | IROr
    /// Binary math intrinsic (atan2 / log_base), lowered from Ast.OpMath2.
    /// Renders as a CALL, not an infix operator -- the same shape IRCaret
    /// already needs (`pow(l, r)`). Always real-valued Float64.
    | IRMath2 of string

/// Mode for binary array operations
type IRBinOpMode =
    | IRElementwise   // a + b (zip iteration)
    | IROuter         // a [+] b (cross iteration)

/// Unary operations
type IRUnaryOp =
    | IRNeg | IRNot | IRConj
    | IRReal | IRImag | IRArg  // complex component/phase accessors
                              // (real/imag: identity/0 on real operands)
    | IRMath of string  // scalar math intrinsic (exp/log/sqrt/...);
                        // renders as std::<name>(arg), result Float64
                        // (complex operand preserves the complex type,
                        //  except abs which always yields the real magnitude)
    | IRCast of ElemType  // explicit numeric cast (Float32(x)/Int64(floor(x))):
                          // result type is always the target element type.
                          // Renders as static_cast<T>(arg) for real/int
                          // targets and the std::complex<T>(arg) constructor
                          // for complex targets. Legality (complex source
                          // never casts real; float->int only through a
                          // visible floor/ceil) is TypeCheck's job -- by the
                          // time this node exists the conversion is licensed.

/// IR Expressions - SSA-like representation
type IRExpr =
    | IRLit of IRLit
    | IRVar of id: IRId * ty: IRTypeG<IRExpr>
    | IRParam of name: string * idx: int * ty: IRTypeG<IRExpr>
    | IRBinOp of IRBinOpMode * IRBinOp * IRExpr * IRExpr
    | IRUnaryOp of IRUnaryOp * IRExpr
    | IRArrayLit of IRExpr list * IRArrayTypeG<IRExpr>
    | IRIndex of array: IRExpr * index: IRExpr list * identity: ArrayIdentity option
    | IRSlice of array: IRExpr * dim: int * start: IRExpr * stop: IRExpr
    | IRCurry of array: IRExpr * index: IRExpr * resultRank: int
    | IRApp of func: IRExpr * args: IRExpr list * retType: IRTypeG<IRExpr>
    | IRTuple of IRExpr list
    // std::complex<double>(re, im) at codegen. Distinct from IRTuple: Complex
    // is a scalar throughout the IR. Components are arbitrary float-typed
    // IRExpr, not just literals -- supports `complex(x, y)` for x, y: Float64.
    | IRComplex of re: IRExpr * im: IRExpr
    /// Fused multiply-add a*b + c, ONE rounding (std::fma / Math.FusedMultiplyAdd
    /// / llvm.fma.f64). Lowered from TExprFma; a scalar Float64 node. The
    /// fusion is the meaning, which is why it is a node and not an IRBinOp
    /// pair: it is bit-identical on every lane REGARDLESS of BLADE_FP_CONTRACT,
    /// where a*b + c is not (see the header of Build.fs).
    | IRFma of a: IRExpr * b: IRExpr * c: IRExpr
    | IRTupleProj of IRExpr * int * bool  // expr, index, isFlat (true=flat leaf index, false=structural type index)
    | IRTupleCons of head: IRExpr * tail: IRExpr
    | IRTupleDecons of tuple: IRExpr
    | IRFieldAccess of obj: IRExpr * field: string  // Struct field access: obj.field
    | IRStructLit of typeName: string * fields: (string * IRExpr) list  // Struct literal: T { f1 = e1, ... }
    | IRIf of cond: IRExpr * thenBr: IRExpr * elseBr: IRExpr
    | IRMatch of scrutinee: IRExpr * cases: IRMatchCase list
    | IRLet of IRId * value: IRExpr * body: IRExpr
    | IRMethodFor of MethodForInfo
    | IRObjectFor of ObjectForInfo
    | IRApplyCombinator of ApplyInfo
    /// Slot-inverted apply: `(object_for(f) >>@ object_for(g)) <@> arrays`.
    /// A canonical apply's kernel slot holds a callable; the compose case
    /// threads arrays through a composed-object chain instead. Kept as a
    /// separate variant so no consumer needs to inspect Loop's shape to tell
    /// the two apart. See `ComposeApplyInfo` below.
    | IRComposeApply of ComposeApplyInfo
    | IRBind of comp: IRExpr * cont: IRExpr
    | IRParallel of IRExpr * IRExpr * fusionDepth: int option
    | IRFusion of IRExpr * IRExpr
    | IRChoice of IRExpr * IRExpr
    // <|:> allocated-fallback: left where storage holds the cell, else right.
    // Distinct from IRChoice (value-level zero test) -- see TExprFallback.
    | IRFallback of IRExpr * IRExpr
    | IRArrayProduct of IRExpr * IRExpr
    | IRComposeObj of IRExpr * IRExpr
    | IRComposeMeth of IRExpr * IRExpr
    | IRCompose of IRExpr * IRExpr
    | IRFunctorMap of func: IRExpr * comp: IRExpr
    | IRPure of IRExpr
    | IRCompute of IRExpr
    | IRReynolds of kernel: IRExpr * isAntisymmetric: bool  // Reynolds combinator
    | IRGuard of cond: IRExpr * body: IRExpr
    | IRSequence of IRExpr list
    | IRReplicate of count: IRExpr * body: IRExpr
    | IRMask of array: IRExpr * pred: IRExpr  // mask(array, pred) - filter array by predicate
    | IRIntersect of IRExpr * IRExpr          // intersect(A, B) - set intersection (deduplicated, order from A)
    | IRUnion of IRExpr * IRExpr              // union(A, B) - set union (deduplicated, A's elements first)
    | IRUnique of array: IRExpr               // unique(A) - dedup, first-occurrence order
    | IRContains of array: IRExpr * value: IRExpr  // contains(A, x) - membership test, returns bool
    /// display.emit(mime, data[, meta]) -- one display-frame line on stdout,
    /// evaluating to `true`. The only EFFECTFUL expression node in the IR:
    /// `head`/`quoted`/`metaTail` are elaboration-time constants (see
    /// TypedAst.TExprDisplayEmit) and `data` is the runtime String payload.
    /// Both back ends share Blade.Display.Frame's byte format -- the
    /// interpreter buffers, the compiled binary writes std::cout, and the
    /// differential gate pins the two together.
    ///
    /// `id` is None for `display.emit` (the frame's `meta.id` is the run's
    /// `<SessionTag><ordinal>`) and Some for `display.emit_id`, whose id is
    /// that runtime String. One node, one operand of difference: every walker,
    /// both back ends and the interpreter would otherwise carry a second arm
    /// that is a copy of the first.
    | IRDisplayEmit of head: string * quoted: bool * data: IRExpr * metaTail: string * id: IRExpr option
    /// display.json_array(A): rank-1/rank-2 numeric array -> JSON text
    /// (String). Pure (unlike IRDisplayEmit). `rank` pinned at typecheck;
    /// formatting is the shared 15-significant-digit byte-parity rule.
    | IRDisplayJson of rank: int * data: IRExpr
    /// display.json_num(x): numeric scalar -> JSON text (String). Pure.
    | IRDisplayNum of data: IRExpr
    /// display.json_string(s): String -> a QUOTED, escaped JSON string. Pure.
    | IRDisplayStr of data: IRExpr
    | IRGroupBy of values: IRExpr * grouping: IRExpr  // group_by(vals, gk) - apply grouping
    | IRGroupKeys of keys: IRExpr list               // group_keys(keys1, keys2, ...) - CSR grouping; multi-key => compound dispatch
    | IRGroupBucket of grouping: IRExpr              // group_bucket(gk) - row -> bucket over the source index space, -1 for dropped rows
    | IRSegments of offsets: int64 list * labels: string list option  // segments(A): structural grouping, run boundaries [0; ..; N], identity permutation
    | IRSegmentsGrid of bounds: int64 list list                        // segments(C0, C1): the product grouping of two slots, one group per tile, per-slot boundaries
    | IRUngroupGrid of grouped: IRExpr * sources: IRIndexTypeG<IRExpr> list * bounds: int64 list list  // ungroup of a grid grouping: tiles back over the two axes
    | IRUngroupRows of rows: IRExpr list * offsets: int64 list * source: IRIndexTypeG<IRExpr>  // ungroup([r1..rF], A): per-file arrays assembled over the tiled axis
    | IRUngroup of grouped: IRExpr * source: IRIndexTypeG<IRExpr>  // ungroup(G): rows of a segment-grouped array reassembled over the source axis
    | IRGroupSizes of grouping: IRExpr               // extents(gk) - per-group sizes over the group axis; no gather
    | IRSort of array: IRExpr * key: IRExpr          // sort(arr, key) - stable ascending sort by key
    | IRReduce of array: IRExpr * kernel: IRExpr * init: IRExpr option  // reduce(arr, op[, init]) - fold innermost dim; init seeds the fold and defines the empty result
    // reduce(deferred, op[, init]): the FUSED reduction terminal -- folds a
    // deferred computation (IRApplyCombinator, or an IRFusion tree of them)
    // without materializing the array(s). ONE loop nest; one scalar
    // accumulator per fusion leaf (tuple of scalars for trees). init is
    // ALWAYS filled by the checker (identity for (+)/(*) sections, user's
    // init otherwise -- arbitrary kernels REQUIRE an explicit init).
    //
    // THE JOIN ENCODING (docs/plan-reduction-joins.md). A REDUCTION JOIN --
    // `object_for(<&!>) <@> (r1, .., rk)` / `reduce([r1, .., rk], (<&!>))` --
    // is the same node with PER-LEG folds: `kernel` and `init` are each an
    // `IRTuple` of k entries in leaf order, one per fusion leaf. Legs need
    // their own seed even when they share an operator (`prodsum` seeds at 0,
    // `reduce(x, (+), 10.0)` at 10.0), and their own kernel when they do not.
    // A single (non-tuple) kernel/init is the shared-fold form `<&!>` maps
    // have always used; every generic walker treats both slots as opaque
    // children, so only typeOf, the fold emitter, and the interpreter's fold
    // read the distinction.
    | IRReduceCompute of computation: IRExpr * kernel: IRExpr * init: IRExpr
    | IRProdSum of args: IRExpr list  // prodsum(x1..xk): fused sum_t prod_l x_l(t) over rank-1 arrays of equal extent; empty extent => 0
    | IRZip of IRExpr list
    | IRAlign of arrays: IRExpr list * spec: AlignSpec
    | IRStack of IRExpr list
    | IRTranspose of array: IRExpr * dim1: int * dim2: int
    | IRDecompact of array: IRExpr * dim: int
    | IRGram of left: IRExpr * right: IRExpr * isSameArray: bool  // A * B^H contraction; symmetric/Hermitian when isSameArray
    | IRGramApply of left: IRExpr * right: IRExpr * vec: IRExpr  // A * (B^H * x): the action of gram(A, B) on x; rank-1 result, no m x p matrix
    | IRMatmul of left: IRExpr * right: IRExpr  // A(m x k) * B(k x n) -> dense m x n; the math package's matmul, emitted through blade_linalg
    /// eigh(S): eigendecomposition of a rank-2 square operand -> the TUPLE
    /// (Q, LAM), emitted through `blade_lapack`. TUPLE-typed with TWO fresh
    /// pools; for complex input Q is complex but LAM is real (eigenvalues of
    /// a Hermitian matrix are real). Exists only when `lapackAvailable ()`
    /// held at elaboration; gate off, `math.eigh` expands to synthesized
    /// Jacobi source instead and this node is never built.
    | IREigh of operand: IRExpr
    /// solve(A, b): A.x = b by partial-pivoted LU -> a fresh dense rank-1 x.
    /// UNCONDITIONAL, unlike IREigh: the native arm is the emitted LU loop
    /// nest (byte-pinned against `Interp/ArrayOps.solveArray`), and the LAPACK
    /// `dgesv` route only replaces those loops when the gate is on.
    | IRSolve of matrix: IRExpr * rhs: IRExpr
    /// `m.lu(A)` -> (LU, piv): the factorization value (tuple of two pools),
    /// the partial-pivoted LU `m.solve` computes, kept. Same pivot rule, same
    /// arithmetic order, so `lu_solve(lu(A), b)` is bitwise `solve(A, b)`.
    | IRLu of matrix: IRExpr
    /// `lu_solve(LU, piv, b[, transposed])`: apply the stored factors.
    | IRLuSolve of lu: IRExpr * piv: IRExpr * rhs: IRExpr * transposed: bool
    | IRArrayNegate of array: IRExpr     // whole-array elementwise negation (eager); type-preserving
    | IRArrayConjugate of array: IRExpr  // whole-array elementwise conjugation (eager); type-preserving
    | IRReverse of array: IRExpr * dim: int
    | IRShift of array: IRExpr * dim: int * offset: IRExpr * boundary: BoundaryMode
    | IRDiag of array: IRExpr
    | IRJoin of arrays: IRExpr list * dim: int
    | IRSubset of array: IRExpr * dim: int * start: IRExpr * length: IRExpr
    | IRRange of IRIndexTypeG<IRExpr> list * offset: IRExpr option
    | IRVirtualReverse of IRIndexTypeG<IRExpr>
    // halo<CompoundIdx<m>> window read w(o): the COORDINATE of the present
    // cell at ordinal (window + offset). Renders via the peel-emitted local
    // alias `<w>_hcidx` of the materialized compound index (dense halo reads
    // stay plain `(w + o)` arithmetic and never build this node). This node
    // is also the future carousel seam: buffered reads replace its rendering.
    | IRHaloUnhash of window: IRExpr * offset: int64
    | IRArity of resolved: int option * paramName: string  // None = unresolved (use paramName), Some n = bound
    | IRNth
    | IRZero
    | IRRank of array: IRExpr
    | IRPolyIndex of pack: IRExpr * index: IRExpr  // Dynamic poly-pack indexing: args[k]
    // Pack tail from cons-destructuring `let head :: tail = A`: a "shifted
    // pack view" behaving as a Poly pack of arity one less than `pack`,
    // dropping the first `drop` elements. Pre-monomorphization only --
    // specializeFunction rewrites it into concrete params, so it never
    // survives to codegen/interp.
    | IRPolyTail of pack: IRExpr * drop: int
    | IRExtent of array: IRExpr * dim: int
    // Ragged-extent marker: where this appears as an Extent, codegen emits a
    // lookup into the lengths array at the current iteration's flat outer
    // position. The lengths array is shaped to match the prior index
    // dimensions (e.g. Array<Nat like Idx<M>, Idx<N>> for `Idx<M>, Idx<N>,
    // RaggedIdx<lengths>`). Distinct from IRExtent (queries an array's
    // extent metadata): this reads a value from the lengths array itself.
    | IRRaggedLookup of lengths: IRExpr
    // Compound-index marker (formalism 4.5): where this appears as an Extent,
    // the index type is a CompoundIdx -- a masked product space whose valid
    // coordinates are selected by `mask`, a RUNTIME array (a CompoundIdx is
    // identified by a whole-mask hash). Rank = mask rank; per-dim extents
    // come from the mask's array type at codegen. Cardinality is NOT
    // closed-form -- compound_index_t builds its rank<->tuple table at
    // construction and reports cardinality at runtime. Distinct from
    // IRRaggedLookup: a compound mask couples all dimensions at once.
    | IRCompoundMask of mask: IRExpr
    // Residual-compound marker (formalism 4.5, partial indexing): where this
    // appears as an Extent, the index type is a CompoundIdx from PARTIALLY
    // indexing a parent compound -- pinning the first `prefixLen` (= j) of
    // the parent's k coordinates, k-j left free. Rank = k-j. Representation:
    // SHARED DATA, MATERIALIZED INDEX -- data is a non-copied window into the
    // parent's lex-sorted pool (prefix_range [lo,hi)); the index is a freshly
    // materialized compound_index_t<k-j> over that window. Cost is O(window),
    // not O(n). `parent` is the parent compound array; `prefixLen` is j --
    // pinned coordinate VALUES live at the indexing site (IRIndex), not here.
    // Codegen for the residual construction is not yet implemented; the
    // partial-index path emits an explicit error rather than miscompiling.
    | IRCompoundProject of parent: IRExpr * prefixLen: int
    // Sparse extent marker (formalism 3.5): where this appears as an Extent,
    // the index type is a SparseIdx -- an explicit valid-tuple enumeration
    // with hash lookup, keys in GIVEN order (never sorted). Twin of
    // IRCompoundMask, mask replaced by SkStatic (compile-time-folded key
    // list, baked as literals) or SkRuntime (runtime rank-1 tuple-element
    // array; sparse_index_t built from it at runtime). Rank = key tuple
    // arity; Cardinality = key count. Drives PlaceTabulated, like IRCompoundMask.
    | IRSparseKeys of source: SparseKeysSource
    // Orbit (iterated-wreath) class marker: where this appears as an Extent,
    // the index type is an `OrbIdx` of DEPTH >= 2 (docs/plan-orbit-index-types.md
    // section 2). Twin of IRSparseKeys: carries the flat level list
    // [(r1,s1), ..., (rd,sd)], outermost-last, `true` = '+' -- has nowhere
    // else to live without a field every other index kind carries as None.
    // `extent` is the BASE extent n (the fold's M0), an ordinary extent
    // expression reached like a plain Idx extent by folding/varref/subst.
    // Rank = PRODUCT of the level ranks. Cardinality is closed-form (section
    // 4's iterated binomial, `Blade.OrbRank.cellCountChecked`), so this
    // drives PlaceCombinatorial, NOT PlaceTabulated. DEPTH <= 1 NEVER
    // PRODUCES THIS: `OrbIdx<[(r,+)],n>` lowers to the exact SymIdx record,
    // `OrbIdx<[],n>` to the exact Idx record.
    | IROrbitClass of levels: (int * bool) list * extent: IRExpr
    // Opaque-extent marker: an Extent determined by surrounding context
    // rather than declared up front (e.g. kernel-parameter type
    // `RaggedIdx<_>`, where the peeled param is bound to a sub-array whose
    // `_extents[0]` carries the actual length). Unlike IRRaggedLookup it
    // carries no data -- the loop binding's ExtentArrayRef tells codegen
    // where to read the concrete extent from. Should not normally reach
    // codegen directly; the loop builder reads it via ExtentArrayRef.
    | IROpaqueExtent
    | IRAssign of target: IRExpr * value: IRExpr
    | IRForRange of varId: IRId * lo: IRExpr * hi: IRExpr * body: IRExpr
    /// Runtime constraint guard, statement-positioned:
    /// `if (!(cond)) { blade_rt::panic(code, message, file, line); }`.
    /// Synthesized (mutual-group joint checks, struct constraint checks,
    /// the rec-array while-guard budget abort); value type is unit. `code`
    /// is the BLxxxx the panic renders (BL8001 for value-constraint guards,
    /// BL8010 for the budget abort). The span carries source provenance for
    /// runtime traces; noSpan degrades the panic to a nullptr file / 0 line.
    /// Safe to add here: IRConstraintCheck is statement-positioned and never
    /// hits the structural `=` fast paths other IRExpr cases flow through.
    | IRConstraintCheck of cond: IRExpr * code: string * message: string * span: Blade.Ast.Span
    /// Early exit from the ENCLOSING IRForRange when cond is true -- the
    /// `while` guard on a recursive array's inductive arm (a C++ `break`).
    /// Statement-positioned and unit-valued like IRConstraintCheck, and only
    /// synthesized into rec-array recursion loops by inferRecArray; the
    /// emitters refuse it anywhere a `break` could not stand. The C++ break
    /// path must run the per-iteration frees accumulated so far FIRST
    /// (genForRangeBinding's alloc scope) or each guarded loop leaks its
    /// breaking iteration's slice materializations.
    | IRBreakIf of cond: IRExpr

/// Abstract callable in IR: the merged form of source-level functions and
/// lambdas. Lives in the IRExpr mutual-recursion group because
/// `IRCallable.Body : IRExpr` cycles with IRExpr's variants.
///
/// Field roles by source kind: Id/Name always populated (lambdas get
/// synthesized "__lambda_<id>"); Captures empty for functions, populated for
/// lambdas by lambda-lifting; IsCommutative/CommGroups default false/[] when
/// unannotated; Parallelism fields populate identically for both kinds
/// (omp/cuda/mpi clauses from a where-clause or a strategy list); IsStatic/
/// IsArityPoly/ArityParam are function-only (false/None for lambdas);
/// RetType comes from declaration for functions, inference for lambdas.
///
/// Codegen renders all callables as top-level C++ functions
/// (originalParams..., captures...); call sites referencing a callable with
/// non-empty Captures get a thin C++ lambda wrapper forwarding captures by
/// reference, preserving OCaml-style capture semantics.
and IRCallable = {
    Id: IRId
    Name: string
    Params: IRParam list
    RetType: IRTypeG<IRExpr>
    Body: IRExpr
    IsStatic: bool
    IsCommutative: bool
    CommGroups: int list list
    // AntisymGroups: positions a `where anticomm(...)` clause declared
    // anti-invariant. Also appear in CommGroups (same grouping/iteration
    // license as comm); this list is the extra bit saying the simplex is
    // STRICT (no diagonal, sign flip on swap). Empty unless declared, so the
    // reynolds antisymmetrization path (its own flag on IRReynolds) is
    // untouched.
    AntisymGroups: int list list
    // Per-(param-index, dim-count) detail from an `omp(...)` clause.
    // IsOmpParallel is the derived "requested OpenMP" opt-in flag driving
    // loop parallelization (buildLoopNestCodeGen).
    Parallelism: (int * int) list
    IsOmpParallel: bool
    // Opt-in flag for the `cuda` strategy: true emits a flat-launch
    // __global__ kernel + host launch instead of a host loop nest
    // (genCudaKernel). CudaBlockSize is the launch block size (default 256).
    // omp/cuda/mpi are mutually exclusive; all false means serial host loop.
    IsCudaKernel: bool
    CudaBlockSize: int
    // Opt-in flag for `mpi`: iteration domain decomposed across MPI ranks
    // (SPMD; output restored via Allgatherv). Gated like cuda -- inert
    // unless mpiEmitMode is on (genApplyCombinator).
    IsMpiParallel: bool
    IsArityPoly: bool
    ArityParam: string option
    // `where repro` (functions only): the emitted body must keep the
    // interpreter's operation sequence. Codegen discharges it as the
    // BLADE_REPRO_FN attribute (noinline + fp-contract off) on the emitted
    // definition, a veto inside `foldReorderLicensed` (which also covers the
    // BLADE_FP_REASSOC lanes and the LLVM lane's fast-math flags), and
    // BLAS/LAPACK routing declined while this body emits. Grafted like
    // AntisymGroups by the ONE construction site that has the clause
    // (Lowering.lowerTypedFuncDecl); every other site leaves it false.
    IsRepro: bool
    Captures: CaptureInfo list
    // Per-parameter SIGN parity of the body (KspOdd/KspEven/KspUnknown, in
    // declaration order), consumed by `deduceWreathTie`'s soundness gate.
    // Populated by the TypeCheck apply seam (including eta wrappers, via the
    // callee's FuncSignParities) and carried through Lowering so codegen and
    // the interpreter see the same values. Empty for non-kernel callables --
    // missing entries read as KspUnknown.
    SignParities: KernelSignParity list
    // Conservative effect summary of the body (Blade.Effects), computed
    // from the TYPED body at checkFunctionDecl with callees resolved and
    // grafted here by Lowering.lowerTypedFuncDecl -- the one legality fact
    // every cost-only rewrite consults (fusion's splice, freeze
    // recognition's callee test). `unknown` for every callable built
    // anywhere else (lambdas, synthesized kernels, specialized clones of
    // unmarked callables), which each consumer reads as "run your own
    // analysis or decline" -- never as pure.
    Effects: Blade.Effects.EffectSummary
}

/// Semantic-marker alias for IRCallable naming "top-level function in
/// module.Functions" -- distinct in intent from codegen-internal synthetic
/// callables and let-bound aliases; same underlying record.
and IRFuncDef = IRCallable

and CaptureInfo = {
    Id: IRId
    Name: string
    /// Lifted-lambda codegen extends the C++ signature with one parameter
    /// per capture, typed as a reference (`T&`) so mutation propagates and
    /// the value stays alive via the wrapper's `[&]` capture. Irrelevant for
    /// source-level functions (no captures).
    Type: IRTypeG<IRExpr>
    IsMutable: bool
}

/// Information about a method_for construction
and MethodForInfo = {
    Arrays: IRExpr list
    Identities: ArrayIdentity list
    ArrayTypes: IRArrayTypeG<IRExpr> list
    SDimsPerArray: int list
    TotalSDims: int
    SharedIndexTypes: IRIndexTypeG<IRExpr> list  // For co-iteration: shared iteration records (empty = not co-iteration; multi = product space)
}

/// Information about an object_for construction
and ObjectForInfo = {
    Kernel: IRExpr
    CommGroups: int list list
    InputRanks: int list  // irank(f, i) for each parameter
    OutputRank: int       // orank(f) - T-dimensions added by kernel
}

/// Information about combinator application (<@>)
and ApplyInfo = {
    Loop: IRExpr                            // Provenance: IRMethodFor or IRObjectFor
    Kernel: IRExpr
    Arrays: IRExpr list                     // The actual array expressions
    Identities: ArrayIdentity list          // Array identity tracking (for symmetry)
    ArrayTypes: IRArrayTypeG<IRExpr> list            // Array type info
    SharedIndexTypes: IRIndexTypeG<IRExpr> list      // For co-iteration (zip): shared records (empty = not co-iteration)
    SymcomStates: SymcomState list
    TriangularLevels: bool list
    SDimsPerArray: int list
    KernelInputRanks: int list
    KernelOutputRank: int
    KernelTDims: IRIndexTypeG<IRExpr> list           // T-dimension index types from kernel return type
    SpeedupFactor: int64
    ReynoldsSpeedup: int64        // Reynolds permutation count (n!); actual terms may be fewer after dedup
    HasReynolds: bool              // Whether kernel has Reynolds annotation
    OutputType: IRTypeG<IRExpr>             // Deduced output array type
    IsCoIteration: bool            // True for 'for ... in' co-iteration
}

/// Information for slot-inverted compose application:
///   (object_for(f) >>@ object_for(g)) <@> A
/// In a canonical `IRApplyCombinator` the `Kernel` slot is a callable
/// reference; in the compose case the kernel slot of `<@>` instead
/// holds the input arrays threaded through the composed-object chain.
/// `Composition` is the chain itself (`IRComposeObj`, or an `IRVar`
/// let-bound to one -- codegen resolves the latter via
/// `ctx.DeferredComputations`). `InputArrays` are the arrays the
/// chain is applied to. Unlike `ApplyInfo`, no symmetry/triangulation
/// metadata is carried: the composition's leaves carry their own.
and ComposeApplyInfo = {
    Composition: IRExpr     // Provenance: IRComposeObj (or IRVar bound to one)
    InputArrays: IRExpr list
    OutputType: IRTypeG<IRExpr>
}

and IRParam = {
    Name: string
    Type: IRTypeG<IRExpr>
    Index: int
    VarId: IRId   // The variable ID used in the lambda body
}

and IRMatchCase = {
    Pattern: IRPattern
    Guard: IRExpr option
    Body: IRExpr
}

and IRPattern =
    | IRPatWild
    | IRPatVar of IRId
    | IRPatLit of IRLit
    | IRPatTuple of IRPattern list
    | IRPatCons of IRPattern * IRPattern
    | IRPatVariant of name: string * tag: int * IRPattern option * isEnum: bool
    /// Struct destructuring with its field NAMES kept. Lowering used to
    /// collapse this to `IRPatTuple` in pattern order, which lost two
    /// different things: the C++ lane then read a real struct with
    /// `std::get<i>` (which does not compile), and a pattern listing fields
    /// out of DECLARATION order -- `Point { y, x }` -- bound every field to
    /// the wrong slot, silently, in both evaluators. The name is the only
    /// thing that makes either decodable, so it rides the node.
    | IRPatStruct of typeName: string * fields: (string * IRPattern) list

and BoundaryMode =
    | BndShrink
    | BndPad of IRExpr
    | BndPeriodic
    | BndReflect

and AlignSpec = {
    Offsets: (int * int) list
    Boundary: BoundaryMode
}

/// The key source of an IRSparseKeys extent (see that case's doc). SkStatic
/// carries the compile-time-folded entry list (outer = keys in given order,
/// inner = one tuple's coordinates); SkRuntime references the runtime keys
/// array expression.
and SparseKeysSource =
    | SkStatic of entries: int64 list list
    | SkRuntime of keys: IRExpr
    /// Enumerable constrained domain (docs/plans/structural/06): the keys are
    /// the solutions of a static struct's linear constraints, enumerated in
    /// closed form at RUN time (the C++ key builder) and at compile time
    /// (the interpreter, the certificate) from one plan -- never a baked
    /// table, so the domain is not box-capped.
    | SkDomain of plan: DomainPlan


// Concrete instantiations of the Types.fs generic family at IRExpr --
// the prototype's extent representation. Everything downstream uses
// these names; swapping the extent representation (the rewrite's
// dedicated Extent DU) is a type-argument change here, nothing more.
type IRType = IRTypeG<IRExpr>
type IRIndexType = IRIndexTypeG<IRExpr>
type IRArrayType = IRArrayTypeG<IRExpr>
type IdxRef = IdxRefG<IRExpr>
type IRArrowSlot = IRArrowSlotG<IRExpr>
type LoopType = LoopTypeG<IRExpr>

// OrbIdx (iterated-wreath) accessors. The level list rides the Extent slot as
// an IROrbitClass marker (see that case), so every consumer reads it through
// these three functions rather than pattern-matching the extent inline -- one
// place to change if the carrier ever moves to a record field.

/// The (rank, isPlus) level list of a wreath class, OUTERMOST-LAST. `[]` for
/// every non-wreath record, so `not (List.isEmpty (orbitLevelsOf ix))` is
/// exactly "this is a depth >= 2 OrbIdx".
let orbitLevelsOf (ix: IRIndexType) : (int * bool) list =
    match ix.Extent with
    | IROrbitClass (levels, _) -> levels
    | _ -> []

/// The BASE extent (the section 4 fold's M0) of a wreath class; the record's own
/// Extent for anything else, so extent-reading code that does not care about
/// the class can go through here uniformly.
let orbitBaseExtent (ix: IRIndexType) : IRExpr =
    match ix.Extent with
    | IROrbitClass (_, n) -> n
    | other -> other

/// Render a level list in the surface spelling: `[(2,-), (2,+)]`.
let ppOrbitLevels (levels: (int * bool) list) : string =
    "[" + (levels |> List.map (fun (r, plus) -> $"""({r},{(if plus then "+" else "-")})""")
                  |> String.concat ", ") + "]"

/// The ONE text for the hard refusal at the storage boundary: every refusal
/// site (typecheck gates, allocator, loop builder, compact read/print,
/// providers) calls this instead of spelling its own string. `where_` names
/// the seam ("let binding 'R'", "reduce()", ...) and reads as a prefix.
let orbitStorageUnsupported (where_: string) (levels: (int * bool) list) : string =
    sprintf "%s: OrbIdx<%s, n> is a declarable index class of depth %d, and a DEDUCED one can now be \
allocated, written, printed, READ at an arbitrary tuple (the per-level canon fold, the accumulated \
character, the zero set), FULLY DECOMPACTED to its dense tensor, and round-tripped through a Zarr \
store (the spec_version 2 'orbit' head -- providers/ZarrTriangularSpec.md). What is still missing is every \
path that would put a wreath pool anywhere but under its own traversal nest: an OrbIdx ANNOTATION \
(a store is now a producer, but the annotation also admits classes nothing produces), reduce/prodsum \
over the pool, transpose, PARTIAL (per-level) decompaction, a WINDOWED or distributed store read, and \
provider I/O outside Zarr (CSV and NetCDF have no pool axis to carry the class on). So the \
compiler refuses here rather than compute an address it cannot compute. \
The depth-1 spellings work through the existing compact machinery instead: OrbIdx<[(r,+)], n> is \
exactly SymIdx<r, n>, OrbIdx<[(r,-)], n> is exactly AntisymIdx<r, n>, and OrbIdx<[], n> is exactly \
Idx<n>."
            where_ (ppOrbitLevels levels) (List.length levels)

/// Bridge to `Blade.OrbRank`'s own level spelling: OrbRank is a
/// dependency-free module (no Blade namespace, so proofs/scripts can `#load`
/// it standalone) and owns `OrbSign`; the IR side keeps a bare bool. The ONE
/// conversion point.
let orbRankLevels (levels: (int * bool) list) : Blade.OrbRank.Level list =
    levels |> List.map (fun (r, plus) ->
        (r, (if plus then Blade.OrbRank.OPlus else Blade.OrbRank.OMinus)))

/// section 7.2's normalization: a level with r = 1 is the trivial group S_1,
/// a no-op at EITHER sign, so it's dropped. Mirrors `OrbRank.normalizeLevels`
/// over the IR's level representation (the ONE conversion point above keeps
/// the two spellings from growing separate rules).
let normalizeOrbitLevels (levels: (int * bool) list) : (int * bool) list =
    levels |> List.filter (fun (r, _) -> r <> 1)

/// Which of the three record shapes a normalized level list takes. THE ONE
/// normalization rule: both producers of an OrbIdx class -- the SURFACE type
/// (`TypeCheck.orbitIndexRecord`) and DEDUCTION (`deduceWreathTie`) -- route
/// through here, so they cannot disagree about the same class.
type OrbitNormalForm =
    /// `[]` -- the trivial class. The plain `Idx<n>` record.
    | OrbNfTrivial
    /// `[(r,s)]` -- exactly the `SymIdx<r,n>` / `AntisymIdx<r,n>` record.
    | OrbNfDepth1 of rank: int * isPlus: bool
    /// depth >= 2 -- the `SymWreath` record (see `mkWreathIndexRecord`).
    | OrbNfWreath of levels: (int * bool) list

let orbitNormalForm (levels: (int * bool) list) : OrbitNormalForm =
    match normalizeOrbitLevels levels with
    | [] -> OrbNfTrivial
    | [ (r, s) ] -> OrbNfDepth1 (r, s)
    | ls -> OrbNfWreath ls

/// Build the depth >= 2 `SymWreath` index record: Rank = RAW AXIS COUNT
/// (product of level ranks), level list in the Extent slot as an
/// `IROrbitClass` marker, `IxKOrbit` + "__orbidx" sentinel Tag (validator
/// enforces agreement). Shared by surface-type lowering and deduction so the
/// two cannot build differently-shaped records for the same class; `levels`
/// must already be normalized (a rank-1 level would miscount the Rank).
///
/// The axis fold SATURATES at a 65536 cap rather than running to completion
/// (Rank is a loop count/subscript arity all over the compiler; a
/// wrapped-negative Rank would be a nonsense type, not an error).
let mkWreathIndexRecord (id: IRId) (levels: (int * bool) list) (baseExtent: IRExpr) : IRIndexType =
    let axes64 = levels |> List.fold (fun a (r, _) -> if a > 65536L then a else a * int64 r) 1L
    if axes64 > 65536L then
        failwithf "OrbIdx%s: the class acts on more than 65536 raw axes (the product of its level ranks), which is \
past what this compiler will build an index record for. An iterated-wreath class's cell count leaves int64 well before \
its rank does (docs/plan-orbit-index-types.md section 7.2), so a class this wide is not storable at any extent."
                  (ppOrbitLevels levels)
    { Id = id; Rank = int axes64; Extent = IROrbitClass (levels, baseExtent)
      Symmetry = SymWreath; Tag = Some "__orbidx"; IxKind = IxKOrbit
      Kind = SDimension; Dependencies = [] }

/// Level-1 placement classification of a full index type. Today this derives
/// purely from the symmetry class (placementClassOf); it is the seam where
/// tabulated detection (CompoundIdx / SparseIdx, from the index type's typedef)
/// will hook in. Derived, not stored (mirrors behaviorOf): there is no
/// PlacementClass field on IRIndexType to fall out of sync.
let placementOf (ix: IRIndexType) : PlacementClass =
    // Tabulated placement is detected from the Extent carrier (IRCompoundMask),
    // not the symmetry class -- a CompoundIdx has Symmetry = SymNone but is NOT
    // dense. Everything else derives from symmetry via placementClassOf.
    match ix.Extent with
    | IRCompoundMask _ -> PlaceTabulated
    | IRSparseKeys _ -> PlaceTabulated
    // A residual (partially-indexed) compound: Rank 1 means one free
    // coordinate at the pinned prefix -- a contiguous window [lo,hi),
    // iterated as an ordinary dense 1-D loop. Rank >= 2 is still a masked
    // product space needing the tabulated (materialized child index) path.
    | IRCompoundProject _ -> if ix.Rank <= 1 then placementClassOf ix.Symmetry else PlaceTabulated
    | _ -> placementClassOf ix.Symmetry

/// Compute the promoted element type for two numeric types per section 3.4.2.
/// Returns None if the types are incompatible for promotion. Index nominal
/// tags are not represented at the ElemType level; their strict unification
/// happens at the IRType level via IRTIdxTagged in `unify`.
let promoteElemType (a: ElemType) (b: ElemType) : ElemType option =
    if a = b then Some a
    else
        match a, b with
        | ETInt32, ETInt64   | ETInt64, ETInt32   -> Some ETInt64
        | ETInt32, ETFloat32 | ETFloat32, ETInt32 -> Some ETFloat32
        | ETInt32, ETFloat64 | ETFloat64, ETInt32 -> Some ETFloat64
        | ETInt64, ETFloat32 | ETFloat32, ETInt64 -> Some ETFloat64
        | ETInt64, ETFloat64 | ETFloat64, ETInt64 -> Some ETFloat64
        | ETFloat32, ETFloat64 | ETFloat64, ETFloat32 -> Some ETFloat64
        // Complex mixed with a real (int/float) or a narrower complex widens to
        // the appropriate complex width. Complex64 mixed with Float64 or with
        // Complex128 widens to Complex128 (component precision follows the wider
        // operand). CodeGen inserts the explicit casts std::complex's same-type
        // operators require (see coerceComplexOperand).
        | ETComplex128, (ETFloat64 | ETFloat32 | ETInt64 | ETInt32 | ETComplex64 | ETComplex128)
        | (ETFloat64 | ETFloat32 | ETInt64 | ETInt32 | ETComplex64), ETComplex128 -> Some ETComplex128
        | ETComplex64, (ETFloat32 | ETInt64 | ETInt32 | ETComplex64)
        | (ETFloat32 | ETInt64 | ETInt32), ETComplex64 -> Some ETComplex64
        | ETComplex64, ETFloat64 | ETFloat64, ETComplex64 -> Some ETComplex128
        | _ -> None

/// Active pattern for assignment target (lvalue) classification
let (|LVVar|LVIndex|LVField|LVOther|) = function
    | IRVar (id, _)              -> LVVar id
    | IRIndex (arr, indices, _)  -> LVIndex (arr, indices)
    | IRFieldAccess (obj, field) -> LVField (obj, field)
    | e                          -> LVOther e

// Element-type active patterns.
//
// These patterns project an IRType into the role of "array element type."
// Each pattern represents one concern (primitives, inference variables,
// named types, function values); a site that only handles primitives uses
// `PrimElem` or `AnyPrimElem` and fails to match otherwise, so silent
// fallthroughs are caught at compile time rather than producing wrong-typed
// values.

/// Strict primitive element. Doesn't match through unit annotation.
let (|PrimElem|_|) (ty: IRType) =
    match ty with
    | IRTScalar et -> Some et
    | _ -> None

/// Primitive element, optionally unit-annotated or index-tagged. Workhorse
/// for read sites that just want the primitive and don't care whether
/// wrappers are attached. Matches through both IRTUnitAnnotated (physical
/// units) and IRTIdxTagged (nominal index tags) -- both preserve their
/// inner type and erase at codegen.
let (|AnyPrimElem|_|) (ty: IRType) =
    match ty with
    | IRTScalar et -> Some et
    | IRTUnitAnnotated (IRTScalar et, _) -> Some et
    | IRTIdxTagged (IRTScalar et, _) -> Some et
    | _ -> None

/// Unit-annotated primitive: returns both the elem type and the unit
/// signature. For unit-aware sites that need to track or propagate units.
let (|UnitPrimElem|_|) (ty: IRType) =
    match ty with
    | IRTUnitAnnotated (IRTScalar et, units) -> Some (et, units)
    | _ -> None

/// Inference variable in element position: the natural representation for
/// "kernel param's elem type, deferred until <@> unifies it with the
/// source array's per-row type."
let (|InferElem|_|) (ty: IRType) =
    match ty with
    | IRTInfer id -> Some id
    | _ -> None

/// Named (struct or sum) elem type: the dispatch site for eventual codegen
/// support of arrays of user-defined types.
let (|NamedElem|_|) (ty: IRType) =
    match ty with
    | IRTNamed name -> Some name
    | _ -> None

/// Function-valued elem type: reflects the array-function duality
/// (Array<T like Idx<n>> is conceptually `Idx<n> -> T`). Matches `IRTArrow`
/// when every slot is `SVal` (pure function, no storage-backed slots),
/// returning the `(args, ret)` view. Matches the empty-slot case too
/// (`IRTArrow ([], ret, None)`, a nullary function) -- the symmetric
/// counterpart to ArrayElem's empty-slot rejection. `identity` is ignored:
/// functions don't carry array identity.
let (|FuncElem|_|) (ty: IRType) =
    match ty with
    | IRTArrow (slots, ret, _) when slots |> List.forall (function SVal _ -> true | _ -> false) ->
        let args = slots |> List.map (function SVal t -> t | _ -> failwith "unreachable")
        Some (args, ret)
    | _ -> None

/// Smart constructor: build an arrow-shaped type from a parameter type
/// list and return type. Produces an `IRTArrow` with all-`SVal` slots
/// and no identity -- the unified-IR function form.
let mkFuncArrow (args: IRType list) (ret: IRType) : IRType =
    IRTArrow (args |> List.map SVal, ret, None)

/// Validate the shape constraints on an IRTArrow's slot list and result:
///   1. If any slot is SIdxVirt, all slots from that point on must be too
///      (no SIdx/SVal after the first SIdxVirt).
///   2. If any slot is SIdxVirt, the result must NOT be IRTArrow (virtual
///      arrays' elements must be simple values).
/// Returns [] if well-formed, else human-readable error strings.
let rec validateArrowShape (slots: IRArrowSlot list) (result: IRType) : string list =
    let errs = ResizeArray<string>()
    let firstVirt =
        slots |> List.tryFindIndex (function SIdxVirt _ -> true | _ -> false)
    match firstVirt with
    | None -> ()  // No virtual slots -- no constraint
    | Some k ->
        // Constraint 1: all slots at or after k must be SIdxVirt
        slots
        |> List.iteri (fun i slot ->
            if i > k then
                match slot with
                | SIdxVirt _ -> ()
                | SIdx _ ->
                    errs.Add $"Slot {i} is SIdx but appears after first SIdxVirt at {k} (stored cannot follow virtual)"
                | SVal _ ->
                    errs.Add $"Slot {i} is SVal but appears after first SIdxVirt at {k} (virtual arrays cannot contain functions)")
        // Constraint 2: result must not be an arrow
        match result with
        | IRTArrow _ ->
            errs.Add("Virtual arrow has IRTArrow result (virtual arrays cannot contain arrays/functions)")
        | _ -> ()
    errs |> List.ofSeq

/// Array-shaped type: matches `IRTArrow` when slots are uniformly all
/// `SIdx` (stored) or all `SIdxVirt` (virtual); mixed slots or any `SVal`
/// do NOT match. Returns an `IRArrayType` view (`.ElemType`, `.IndexTypes`,
/// `.IsVirtual`, `.Identity`) reconstructed fresh on each match --
/// `IRArrayType` is view-only, no DU constructor wraps it. Empty-slot
/// arrows do NOT match (nullary functions, per `mkFuncArrow []`); rank-0
/// arrays don't exist as a type form (`mkArrayLike` collapses them to
/// their element type at the producer side).
let (|ArrayElem|_|) (ty: IRType) =
    match ty with
    | IRTArrow ([], _, _) -> None  // nullary function, not an array -- see docstring
    | IRTArrow (slots, result, identity) ->
        let allStored = slots |> List.forall (function SIdx _ -> true | _ -> false)
        let allVirtual = slots |> List.forall (function SIdxVirt _ -> true | _ -> false)
        if allStored || allVirtual then
            let indexTypes =
                slots |> List.map (function
                    | SIdx i -> i
                    | SIdxVirt i -> i
                    | _ -> failwith "unreachable -- checked by guards above")
            Some {
                ElemType = result
                IndexTypes = indexTypes
                IsVirtual = allVirtual
                Identity = identity
            }
        else
            None
    | _ -> None

/// Stored-array variant of ArrayElem. Matches IRTArrow with all-SIdx slots
/// (non-empty). Useful for codegen paths that need to allocate / read
/// storage and want to reject virtual sources at the type-match level.
let (|StoredArrayElem|_|) (ty: IRType) =
    match ty with
    | IRTArrow ([], _, _) -> None  // nullary function -- see ArrayElem
    | IRTArrow (slots, result, identity)
        when slots |> List.forall (function SIdx _ -> true | _ -> false) ->
        let indexTypes = slots |> List.map (function SIdx i -> i | _ -> failwith "unreachable")
        Some {
            ElemType = result
            IndexTypes = indexTypes
            IsVirtual = false
            Identity = identity
        }
    | _ -> None

/// Virtual-array variant of ArrayElem. Matches IRTArrow with all-SIdxVirt
/// slots (non-empty). Useful for iteration codegen and range/reverse/blocked
/// dispatch.
let (|VirtualArrayElem|_|) (ty: IRType) =
    match ty with
    | IRTArrow ([], _, _) -> None  // nullary function -- see ArrayElem
    | IRTArrow (slots, result, _identity)
        when slots |> List.forall (function SIdxVirt _ -> true | _ -> false) ->
        let indexTypes = slots |> List.map (function SIdxVirt i -> i | _ -> failwith "unreachable")
        Some {
            ElemType = result
            IndexTypes = indexTypes
            IsVirtual = true
            Identity = None  // Virtual arrays don't carry identity
        }
    | _ -> None

/// Smart constructor for a stored array arrow. The slot list is the
/// array's index types each wrapped as `SIdx`, the result is the
/// element type, and identity is the caller-supplied handle.
let mkArrayArrow (indexTypes: IRIndexType list) (elemType: IRType) (identity: ArrayIdentity option) : IRType =
    IRTArrow (indexTypes |> List.map SIdx, elemType, identity)

/// Smart constructor for a virtual array arrow. Identity is forced to `None`
/// (virtual arrays don't materialize, so there's no handle to track).
///
/// Gate: invokes `validateArrowShape` and raises on any violation -- most
/// commonly an `IRTArrow` passed as `elemType` (virtual arrays must hold
/// simple values). This is a compiler-invariant check, not a user-facing
/// diagnostic; user-facing rejection (e.g. `reverse` of an array-of-arrays)
/// belongs earlier, in TypeCheck. A raise here means an invalid shape got
/// through upstream -- treat as a bug report on that path.
///
/// `mkArrayArrow`/`mkFuncArrow` don't gate: without any `SIdxVirt` neither
/// constraint can fire. If those ever admit mixed slots, move the gate here too.
let mkVirtualArrayArrow (indexTypes: IRIndexType list) (elemType: IRType) : IRType =
    let slots = indexTypes |> List.map SIdxVirt
    match validateArrowShape slots elemType with
    | [] -> IRTArrow (slots, elemType, None)
    | errs ->
        failwithf "mkVirtualArrayArrow: invalid virtual-array shape (compiler invariant violation):\n  %s\n  indexTypes count: %d, elemType: %A"
                  (System.String.Join("\n  ", errs))
                  indexTypes.Length
                  elemType

/// Smart constructor that takes an `IRArrayType` view (as returned by
/// `ArrayElem`) and produces the appropriate `IRTArrow` form. Dispatches
/// on `IsVirtual` to choose between `mkArrayArrow` (stored, all-SIdx)
/// and `mkVirtualArrayArrow` (virtual, all-SIdxVirt).
///
/// Used at form-update producer sites: `ArrayElem arr -> mkArrayLike { arr with ... }`.
/// The virtual/stored character is preserved through the rebuild.
let mkArrayLike (arr: IRArrayType) : IRType =
    // Rank-0 collapse: a zero-rank array equals its element. Prevents the
    // empty-slot `IRTArrow ([], _, _)` form, which is reserved for nullary
    // functions per mkFuncArrow. Without this guard, `mkArrayLike` with
    // empty IndexTypes would produce a shape ambiguous with a nullary
    // function and consumers via ArrayElem would reject it.
    if arr.IndexTypes.IsEmpty then
        arr.ElemType
    elif arr.IsVirtual then
        mkVirtualArrayArrow arr.IndexTypes arr.ElemType
    else
        mkArrayArrow arr.IndexTypes arr.ElemType arr.Identity

/// The kappa_k component array type of a Dist<order, elem, axes> (typed dist
/// tower, ppl/NOTES.md). kappa_1 is the mean tensor over the variable axes
/// as-declared; kappa_k for k >= 2 is the order-k joint cumulant, symmetric-
/// packed over the FUSED variable-axis space: one SymIdx record of Rank k
/// whose Extent is the product of the axes' extents. Used by the checker to
/// type cumulant(d, k) projections and by Zonk to ERASE IRTDist into the
/// component tuple.
let distComponentType (k: int) (elem: IRType) (axes: IRIndexType list) : IRType =
    if k = 1 then
        mkArrayArrow axes elem None
    else
        let fusedExtent =
            match axes with
            | [] -> IRLit (IRLitInt 0L)
            | [one] -> one.Extent
            | first :: rest ->
                rest |> List.fold (fun acc a ->
                    match acc, a.Extent with
                    | IRLit (IRLitInt m), IRLit (IRLitInt n) -> IRLit (IRLitInt (m * n))
                    | l, r -> IRBinOp (IRElementwise, IRMul, l, r)) first.Extent
        let symIdx = {
            Id = (axes |> List.tryHead |> Option.map _.Id |> Option.defaultValue 0)
            Rank = k
            Extent = fusedExtent
            Symmetry = SymSymmetric
            Tag = None; IxKind = IxKPlain
            Kind = SDimension
            Dependencies = []
        }
        mkArrayArrow [symIdx] elem None

/// All component types kappa_1 .. kappa_order of a Dist -- the tuple a Dist
/// value erases to after type checking.
let distComponentTypes (order: int) (elem: IRType) (axes: IRIndexType list) : IRType list =
    [ for k in 1 .. order -> distComponentType k elem axes ]

/// The view transform behind `load_compound(var, mask)`: replace a
/// variable's dimensions with a single CompoundIdx whose presence mask is
/// `maskIR`. Pure type transform -- no data read; materialization happens
/// at `|> read`. The mask is an INPUT array, so no is_nan synthesis (Blade
/// core stays NaN-less).
///
/// The mask covers a LEADING PREFIX of the variable's dims, matched by
/// index-type Id; the masked prefix collapses into one CompoundIdx
/// (Rank = mask rank), the remainder stays regular trailing slots
/// (`Array<T like CompoundIdx<mask>, Idx<...>>`, formalism 4.5; all-dims
/// coverage gives scalar `Compound<T, RANK>`). The mask is ANY integer
/// array (NetCDF has no bool type; flag vars are NC_BYTE/NC_INT, nonzero =
/// present) -- `load_compound` is itself the signal that triggers the int ->
/// std::vector<bool> conversion at materialization. Reordered/non-prefix/
/// non-integer masks and ragged trailing dims are rejected or deferred.
let compoundViewType (freshId: IRId) (varArr: IRArrayType) (maskArr: IRArrayType) (maskIR: IRExpr) : Result<IRType, string> =
    let isMaskElem =
        match maskArr.ElemType with
        | IRTScalar ETInt64 | IRTScalar ETInt32 | IRTScalar ETBool -> true
        | _ -> false
    if not isMaskElem then
        Error (sprintf "load_compound: the mask must be an integer presence array (NetCDF stores flag/mask variables as NC_BYTE/NC_INT, read as Int; nonzero = present); got element type %A" maskArr.ElemType)
    else
        let varIdxs = varArr.IndexTypes
        let maskIdxs = maskArr.IndexTypes
        let maskRank = List.length maskIdxs
        // The mask must cover a LEADING prefix, in order; remaining dims are
        // regular trailing slots (maskRank = total -> empty trailing, scalar
        // Compound). Two dims "correspond" via INDEX-SPACE IDENTITY, not
        // equal extents (mirrors IR.indexSpacesMatch, replicated inline since
        // this precedes it): same Id (provider shares one index-type
        // instance per file), OR same non-anonymous Tag (a NAMED index type
        // matches across fresh Ids), OR both anonymous sharing a named-
        // reference extent (IRVar id / IRParam name). Bare literal-extent
        // equality does NOT match (formalism 14.6) -- shared identity must
        // be established by NAMING the index types.
        let dimsMatch (d: IRIndexType) (m: IRIndexType) : bool =
            if d.Id = m.Id then true
            else
                match d.Tag, m.Tag with
                | Some tagD, Some tagM -> tagD = tagM
                | None, None ->
                    (match d.Extent, m.Extent with
                     | IRVar (idD, _), IRVar (idM, _) -> idD = idM
                     | IRParam (nD, _, _), IRParam (nM, _, _) -> nD = nM
                     | _ -> false)
                | _ -> false
        let isLeadingPrefix =
            maskRank <= List.length varIdxs
            && List.forall2 dimsMatch (varIdxs |> List.truncate maskRank) maskIdxs
        if not isLeadingPrefix then
            let varIds = varIdxs |> List.map _.Id
            let maskIds = maskIdxs |> List.map _.Id
            Error (sprintf "compound/load_compound: the mask must cover a leading prefix of the array's dimensions, sharing index-space identity (same named index type, or same provider dimension). Mask and dense leading dimensions do not correspond (mask dim Ids %A vs array dim Ids %A). Name the shared index types (e.g. `type LatIdx = Idx<n>`) so the mask and dense array refer to the same index space; reordered or non-prefix masks are not yet supported" maskIds varIds)
        else
            let compoundIdx =
                { Id = freshId
                  Rank = maskRank
                  Extent = IRCompoundMask maskIR
                  Symmetry = SymNone
                  Tag = Some "__compoundidx"; IxKind = IxKCompound
                  Kind = SDimension
                  Dependencies = [] }
            // Trailing regular dims: the variable's dimensions after the masked
            // prefix. Empty in the all-dims case (scalar Compound<T, RANK>).
            let trailing = varArr.IndexTypes |> List.skip maskRank
            Ok (mkArrayArrow (compoundIdx :: trailing) varArr.ElemType varArr.Identity)

// Type-pattern matching (concrete + abstract) -- shared by the type-structure
// test harness and the language server's "type of expression" queries.
//
// `matchesTypePattern pattern actual` decides whether `actual` is an
// INSTANCE of `pattern` (CONCRETE = strict structural equality on identity
// dimensions; ABSTRACT = holes matching any filling). Deliberately NOT
// `unify` (symmetric, treats SymNone as compatible with any symmetry --
// wrong for an assertion) and NOT raw `=` (too strict: compares extents/
// inference ids/synthetic tags, none of which are type identity).
//
// Holes in the PATTERN: `IRTInfer _` (whole-type hole), `IRTNat None`
// (abstract type-level nat), an abstract index Extent (always ignored --
// extents are runtime values, never type identity). Everything else in the
// pattern matches concretely.
let rec matchesTypePattern (pattern: IRType) (actual: IRType) : bool =
    match pattern, actual with
    // Whole-type hole in the pattern matches anything.
    | IRTInfer _, _ -> true
    | IRTScalar e1, IRTScalar e2 -> e1 = e2
    | IRTNamed n1, IRTNamed n2 -> n1 = n2
    | IRTUnit, IRTUnit -> true
    | IRTNat _, IRTNat _ -> true       // nat-vs-nat: value is not type identity
    | ArrayElem p, ArrayElem a ->
        // Rank and virtual character are identity.
        p.IndexTypes.Length = a.IndexTypes.Length
        && p.IsVirtual = a.IsVirtual
        && List.forall2 matchesIndexPattern p.IndexTypes a.IndexTypes
        && matchesTypePattern p.ElemType a.ElemType
    | IRTTuple ps, IRTTuple as_ ->
        ps.Length = as_.Length && List.forall2 matchesTypePattern ps as_
    | FuncElem (pa, pr), FuncElem (aa, ar) ->
        pa.Length = aa.Length
        && List.forall2 matchesTypePattern pa aa
        && matchesTypePattern pr ar
    | IRTComputation p, IRTComputation a -> matchesTypePattern p a
    | _ -> pattern = actual   // fallback: exact structural equality

/// Per-index pattern match. Rank and Symmetry are type identity and must match
/// exactly UNLESS the pattern leaves them abstract:
///   - Rank = 0 in the pattern is the "any rank" hole.
///   - A user-meaningful Tag in the pattern must match; a None tag or a
///     synthetic `__` tag in the pattern is treated as "don't care".
/// Extent and Dependencies are NEVER compared (runtime / iteration detail).
/// Kind (S/T dimension) IS compared (it's part of how the dimension behaves).
and matchesIndexPattern (p: IRIndexType) (a: IRIndexType) : bool =
    let rankOk = p.Rank = 0 || p.Rank = a.Rank
    let symOk = p.Symmetry = a.Symmetry
    let kindOk = p.Kind = a.Kind
    let tagOk =
        match p.Tag with
        | None -> true
        | Some t when t.StartsWith("__") -> true
        | Some t -> (a.Tag = Some t)
    rankOk && symOk && kindOk && tagOk


//
// The IR admits two definitionally-equivalent encodings of the same type
// (formalism section 5.2): Nested (IRTArrow ([SIdx I; SIdx J], IRTArrow
// ([SVal P], R, None), Some id)) vs Flat (IRTArrow ([SIdx I; SIdx J; SVal P],
// R, Some id)). Producers emit nested forms exclusively, so this normalizer
// is currently a no-op on producer output; its value is a canonical form for
// decidable type equivalence, future-proofing for any mixed-slot producer,
// and making the section 5.2 identity an algorithm rather than an external
// proof obligation.
//
// `NormalizeMode` is a parameter so the canonical direction is a single
// choice-point. `ToFlat` is the eventual B-flat direction; currently stubbed.

/// Direction of canonical-form normalization.
///   - `ToNested`: split mixed-slot arrows at slot-kind boundaries into
///     nested uniform-kind arrows. Currently the committed canonical form.
///   - `ToFlat`: merge nested uniform-kind arrows into a single mixed-slot
///     arrow. Not yet implemented -- reserved for future B-flat migration.
type NormalizeMode =
    | ToNested
    | ToFlat

/// Kind discriminator for arrow slots, used for grouping consecutive
/// slots of the same kind during normalization. The integer values are
/// arbitrary; only equality matters.
let private slotKind (s: IRArrowSlot) : int =
    match s with
    | SIdx _ -> 0
    | SIdxVirt _ -> 1
    | SVal _ -> 2

/// True if all slots have the same kind. Vacuously true for an empty
/// list (which represents a nullary function per mkFuncArrow []).
let private isUniformKind (slots: IRArrowSlot list) : bool =
    match slots with
    | [] -> true
    | first :: rest ->
        let k = slotKind first
        rest |> List.forall (fun s -> slotKind s = k)

/// Group consecutive slots of the same kind into sub-lists. Order is
/// preserved. For an empty input, returns empty. For uniform input,
/// returns a single-group list.
let private groupConsecutiveByKind (slots: IRArrowSlot list) : IRArrowSlot list list =
    let rec loop (current: IRArrowSlot list) (acc: IRArrowSlot list list) (remaining: IRArrowSlot list) =
        match remaining with
        | [] ->
            match current with
            | [] -> List.rev acc
            | _ -> List.rev (List.rev current :: acc)
        | x :: xs ->
            match current with
            | [] -> loop [x] acc xs
            | y :: _ when slotKind x = slotKind y -> loop (x :: current) acc xs
            | _ -> loop [x] (List.rev current :: acc) xs
    loop [] [] slots

/// Normalize an IRType to the canonical form selected by `mode`; walks every
/// IRType subterm, splitting mixed-slot `IRTArrow`s at slot-kind boundaries
/// (ToNested) into a chain of nested uniform-kind arrows.
///
/// Identity propagation (ToNested split): the outermost split sub-arrow
/// inherits the original identity, inner sub-arrows get `None` -- identity
/// tracks a stored-array handle from program start, and inner sub-arrows are
/// either function residuals (no identity) or function-returned arrays
/// (identity unknown at type level).
///
/// `ToFlat` is not yet implemented and raises.
let rec normalize (mode: NormalizeMode) (ty: IRType) : IRType =
    match mode with
    | ToNested -> normalizeToNested ty
    | ToFlat -> failwith "normalize ToFlat: not yet implemented (reserved for B-flat migration)"

and normalizeToNested (ty: IRType) : IRType =
    match ty with
    | IRTArrow (slots, result, idOpt) ->
        // Recurse first: normalize result, and any IRType inside SVal slots.
        // Index types (SIdx, SIdxVirt) carry IRIndexType, which doesn't
        // contain IRType members -- opaque under this walker.
        let normResult = normalizeToNested result
        let normSlots =
            slots |> List.map (fun s ->
                match s with
                | SVal t -> SVal (normalizeToNested t)
                | SIdx _ | SIdxVirt _ -> s)
        // Now decide whether to split this arrow.
        if isUniformKind normSlots then
            // Already uniform; rebuild with normalized sub-parts.
            IRTArrow (normSlots, normResult, idOpt)
        else
            // Mixed slots -- split at kind boundaries into nested arrows.
            let groups = groupConsecutiveByKind normSlots
            match groups with
            | [] ->
                // Unreachable: isUniformKind is true for empty lists, so
                // we never enter this branch with [] slots.
                IRTArrow (normSlots, normResult, idOpt)
            | firstGroup :: restGroups ->
                // Build inner arrows right-to-left with None identity.
                let inner =
                    List.foldBack
                        (fun grp acc -> IRTArrow (grp, acc, None))
                        restGroups
                        normResult
                // Outermost arrow inherits the original identity.
                IRTArrow (firstGroup, inner, idOpt)

    // Compound types: recurse into substructure.
    | IRTTuple ts ->
        IRTTuple (ts |> List.map normalizeToNested)
    | IRTLoop lt ->
        IRTLoop { lt with
                    ArrayTypes = lt.ArrayTypes |> List.map normalizeToNested
                    KernelType = lt.KernelType |> Option.map normalizeToNested }
    | IRTComputation inner ->
        IRTComputation (normalizeToNested inner)
    | IRTPoly (baseT, var) ->
        IRTPoly (normalizeToNested baseT, var)
    | IRTIdxTagged (inner, tag) ->
        IRTIdxTagged (normalizeToNested inner, tag)
    | IRTUnitAnnotated (inner, units) ->
        IRTUnitAnnotated (normalizeToNested inner, units)
    | IRTDist (order, elem, axes) ->
        // Axes are IRIndexTypes (no IRType members) -- opaque under this walker.
        IRTDist (order, normalizeToNested elem, axes)

    // Leaf types -- no IRType subterms.
    | IRTScalar _
    | IRTUnit
    | IRTNat _
    | IRTNamed _
    | IRTInfer _
    | IRTGroupKeys _ -> ty

/// Structural equivalence on IRTypes, modulo the canonical (B-nested)
/// normalization: `true` iff `t1` and `t2` normalize to the same form under
/// `normalize ToNested`.
///
/// Bridges section 5.3 mixed-slot identity (flat mixed-slot arrows and their
/// split nested forms, e.g. `IRTArrow ([SIdx I, SVal A], R, _)` ==
/// `IRTArrow ([SIdx I], IRTArrow ([SVal A], R, _), _)`), but NOT section 5.2
/// array identity: `Array<T like I, J>` and `Array<Array<T like J> like I>`
/// are NOT equivalent here (ToNested only splits at slot-kind boundaries;
/// uniform-kind multi-slot is already canonical). That collapse becomes
/// available once `ToFlat` is implemented (B-flat migration).
///
/// Does NOT alpha-rename IRTInfer ids -- those are globally unique
/// unification handles, not bound variables.
let irTypeEquiv (t1: IRType) (t2: IRType) : bool =
    normalize ToNested t1 = normalize ToNested t2

/// Tuple elem type. Arrays of tuples. Useful for structured records that
/// don't have a named type definition.
let (|TupleElem|_|) (ty: IRType) =
    match ty with
    | IRTTuple ts -> Some ts
    | _ -> None

/// Pre-specialization parameter-pack type (`Poly<T^r>`): a base type + arity
/// variable that `specializeFunction` expands into `r` individual params at
/// compile time. Semantically a tuple of base types, not a container -- no
/// value-level representation outside the function-parameter position.
///
/// `Poly` in element position (`Array<Poly<T^k> like ...>`) has unclear
/// semantics (packs resolve at specialization time, array elements at
/// runtime); sites matching `PolyElem` should emit an explicit "not
/// implemented" error rather than silently doing the wrong thing.
let (|PolyElem|_|) (ty: IRType) =
    match ty with
    | IRTPoly (inner, var) -> Some (inner, var)
    | _ -> None

/// Elem types that aren't valid as array elements. These represent
/// runtime structures or compile-time-only constructs that have no
/// value-level meaning:
///   - IRTLoop: a loop object, not a value
///   - IRTComputation: deferred-computation wrapper
///   - IRTGroupKeys: opaque runtime CSR structure
let (|InvalidElem|_|) (ty: IRType) =
    match ty with
    | IRTLoop _
    | IRTComputation _
    | IRTGroupKeys _ -> true
    | _ -> false


/// Strip unit annotation from a type, returning the bare type
let rec stripUnits (ty: IRType) : IRType =
    match ty with
    | IRTUnitAnnotated (inner, _) -> stripUnits inner
    | _ -> ty

/// Extract unit signature from a type, if present
let getUnits (ty: IRType) : UnitSig option =
    match ty with
    | IRTUnitAnnotated (_, units) -> Some units
    | _ -> None

/// Flatten nested tuple types: ((a, b), c) -> (a, b, c)
/// Makes left-folded tuples syntactically equivalent to flat tuples.
let rec flattenTupleType (ty: IRType) : IRType =
    match ty with
    | IRTTuple ts ->
        let flattened =
            ts |> List.collect (fun t ->
                match flattenTupleType t with
                | IRTTuple inner -> inner
                | other -> [other])
        IRTTuple flattened
    | _ -> ty

/// Extract the flat leaf types from a potentially nested tuple.
/// ((a, b), c) -> [a; b; c]
let rec flattenTupleLeaves (ty: IRType) : IRType list =
    match ty with
    | IRTTuple ts -> ts |> List.collect flattenTupleLeaves
    | _ -> [ty]

// Loop Structure (For Code Generation)

// Index Space Matching (for Partial Product Symmetry)

/// Information about an index space (for partial symmetry detection)
type IndexSpaceInfo = {
    Tag: string option
    Extent: IRExpr
    Symmetry: SymmetryClass
    Kind: DimensionKind
    SourceRank: int
}


/// Check if two index spaces are "shared" (same logical index space).
/// DIAGNOSTIC-ONLY: nominal index-space identity is NOT a symmetry license --
/// distinct arrays over the same named index type get NO triangular grouping
/// (proofs.md shared_units_insufficient; grouping requires array identity,
/// see rawAxisGroups). Kept for alignment diagnostics and future
/// nominal-typing checks.
let indexSpacesMatch (a: IndexSpaceInfo) (b: IndexSpaceInfo) : bool =
    if a.Kind <> SDimension || b.Kind <> SDimension then false
    else
        match a.Tag, b.Tag with
        | Some tagA, Some tagB -> tagA = tagB
        | None, None ->
            // Only match on named references (variables or parameters),
            // not on anonymous literal extents. Two arrays that happen to
            // have the same length don't share an index space.
            // See section 14.6: "commutativity is the license, shared index spaces
            // are the payoff" -- shared means same named type, not same extent.
            match a.Extent, b.Extent with
            | IRVar (idA, _), IRVar (idB, _) -> idA = idB
            | IRParam (nA, _, _), IRParam (nB, _, _) -> nA = nB
            | _ -> false
        | _ -> false

// Loop Level Structure

/// Represents a single loop level in the nested loop structure
/// The KIND of an index type -- the classification iteration/addressing and
/// other kind-specific logic dispatch on. Derived from Symmetry + Tag
/// (mirrors behaviorOf). Symmetry-like classes are ONE grouped arm (shared
/// triangular/simplex storage); Compound, Dep, Ragged are siblings with
/// their own storage/iteration. SymNone is NOT a kind -- "no class
/// assigned", resolving to plain dense only when no tag claims it. New
/// kinds (Enum, CG, ...) add an arm here.
let (|IxSymmetryLike|IxCompound|IxDep|IxRagged|IxDense|) (ix: IRIndexType) =
    match ix.Symmetry with
    // A wreath class groups with symmetry-like: compact storage over a
    // permutation group with a +-1 character needs canonicalization/compact
    // iteration. It is NOT a single simplex, so consumers reached through
    // this arm refuse it explicitly (orbitStorageUnsupported) instead of
    // falling into depth-1 triangular machinery -- IxDense would silently
    // miscompile (a dense walk over prod(ri) axes into a pool sized for the fold).
    | SymSymmetric | SymAntisymmetric | SymHermitian | SymWreath -> IxSymmetryLike
    | SymNone ->
        // Kind dispatch reads IxKind, never Tag strings (audit section 3.3) --
        // exhaustive, so a new IxKind case must decide its family here.
        (match ix.IxKind with
         // IxKSparse groups with compound: same tabulated storage/iteration
         // shape (materialized index, cardinality-bounded loop).
         | IxKCompound | IxKCompoundDynamic | IxKSparse -> IxCompound
         // IxKOrbit with Symmetry = SymNone is a MALFORMED record (the two
         // are stamped together by the one constructor; validateIR checks
         // Tag/IxKind agreement). Grouping it dense would give that record a
         // plausible dense reading, so it's refused via symmetry-like instead.
         | IxKOrbit -> IxSymmetryLike
         | IxKDep | IxKDepInner | IxKDepOuter -> IxDep
         | IxKRagged | IxKRaggedInline | IxKRaggedOpaque -> IxRagged
         | IxKPlain | IxKGroupOuter | IxKGroupMember | IxKSeq
         // IxKIrreps is dense BY DESIGN: every cell of the irreps space is
         // stored (extent = total_dim(spec), no compression); the block
         // structure is type identity, not a storage/iteration class.
         // IxKPgIrreps (the point-group member) is dense for exactly the same
         // reason -- extent = pg_total_dim(spec), every cell stored.
         | IxKIrreps | IxKPgIrreps
         | IxKErrorRaggedNoPrior | IxKErrorIrrepsBadSpec
         | IxKErrorPgIrrepsBadSpec -> IxDense)


// IR Declarations

type IRTypeDef =
    | IRTDAlias of name: string * ty: IRType
    // Struct where-constraints don't live on the type def: the checker
    // synthesizes IRConstraintCheck guards at every assignment site.
    | IRTDStruct of name: string * fields: (string * IRType) list
    | IRTDVariant of name: string * variants: (string * IRType option) list
    | IRTDIndexType of name: string * idx: IRIndexType
    | IRTDEnumIdx of name: string * idx: IRIndexType * values: EnumValue list
      // Named index type declaration, e.g. "type lat = Idx<180>"
      // Provides nominal identity: two arrays sharing module.lat
      // have the same index space.  Future: schemas can supply these
      // so that multiple files share the same nominal types.

/// Helpers for EnumValue lists. allInt/allString classify a values list;
/// underlyingElemType produces the corresponding ElemType for codegen
/// (used for both the `using <name> = ...;` alias and the reverse-lookup
/// comparison op).
module EnumValue =
    let allInt (vs: EnumValue list) =
        vs |> List.forall (function EVInt _ -> true | _ -> false)
    let allString (vs: EnumValue list) =
        vs |> List.forall (function EVString _ -> true | _ -> false)
    let underlyingElemType (vs: EnumValue list) : ElemType =
        if allString vs && not vs.IsEmpty then ETString
        else ETInt64  // empty or all-int both default to int64

/// Top-level binding
type IRBinding = {
    Id: IRId
    Name: string
    Type: IRType
    Value: IRExpr
    IsConst: bool
    IsMutable: bool
}

/// IR Module
/// Specification for a deferred provider data read, recovered at the read site
/// (`view |> alias.read`) and consumed at codegen to emit the provider's
/// reader. Keyed in IRModule.ProviderReads by the receiving binding's IRId. A
/// plain dense read leaves MaskName/MaskType = None; a load_compound read
/// carries both. Provider is the registry module name ("netcdf", "zarr") --
/// codegen dispatches the emitters through it.
type ProviderReadSpec = {
    Provider: string
    FilePath: string
    VarName: string
    VarType: IRArrayType
    MaskName: string option
    MaskType: IRArrayType option
    /// Sub-simplex window [lo, hi) for `alias.read_window(var, lo, hi)`
    /// over a packed variable; None for whole-variable reads. When set,
    /// VarType is the WINDOW type (leading packed extent = hi-lo).
    Window: (int64 * int64) option
    /// `alias.stream(var)`: not materialized at the binding -- consuming loop
    /// nests inline per-fiber reads at the S/T boundary. Only fiber-kernel
    /// method_for consumers are stream-eligible; other consumption is a
    /// loud codegen error steering to `.read` -- BL7004, raised wherever the
    /// binding would render as a value (CodeGenState, "STREAMED VALUES NEVER
    /// REACH C++"), never an undeclared name handed to g++.
    Streamed: bool
}

/// Specification for a deferred provider data write (`alias.write("path", A)`),
/// keyed in IRModule.ProviderWrites by the write binding's IRId. The source
/// must be a named top-level array binding; codegen emits a flatten prologue
/// (nested -> `<cpp>_flat`, row-major) and then the provider's writer.
type ProviderWriteSpec = {
    Provider: string
    FilePath: string
    /// Variable name inside the store (the source binding's surface name).
    VarName: string
    /// The source array binding being written.
    SourceId: IRId
    SourceType: IRArrayType
    /// Dimension names for the store's metadata (named index types when
    /// known; synthesized dim<i> otherwise).
    DimNames: string list
}

/// Deferred array-fill constructor spec, keyed in RandomInits by the receiving
/// binding's IRId. `fill_random(mod)` records a FillModulus (rand() % mod, C
/// rand(), nondeterministic); `rand.<fam>(key, params..)` records a RandGen
/// (deterministic mt19937_64-based runtime, keyed by `key`). Both allocate the
/// binding's array type and fill its pool at codegen.
///
/// RandGen `pars` are the family's runtime Float64 scalar parameters in surface
/// order -- ordinary IRExprs evaluated ONCE at the binding's position (not per
/// draw), emitted as trailing `(double)` arguments after the key. Empty for
/// uniform/normal; one for exponential/poisson/bernoulli; two for gamma/beta.
///
/// `weights` is the array-valued parameter channel, `Some` only for
/// `categorical` (which has no scalar pars). It pairs the lowered rank-1
/// Float64 array expression with the STATIC extent the checker pinned: codegen
/// emits `pool_base(<expr>.data), (size_t)<len>` in the parameter position, so
/// the length travels with the pointer rather than being re-derived from the
/// expression's type at each consumer.
type RandomFillSpec =
    | FillModulus of IRExpr              // fill_random(mod)
    // rand.<kind>(key, pars..[, weights]); kind = uniform | normal | exponential
    // | gamma | poisson | bernoulli | beta | categorical
    // `address` is the `_at` families' (stream, offset) channel, `Some` only
    // for kinds ending in `_at`: two Int64 expressions evaluated once at the
    // binding, emitted right after the key.
    | RandGen of kind: string * key: IRExpr * pars: IRExpr list * weights: (IRExpr * int) option * address: (IRExpr * IRExpr) option

type IRModule = {
    Name: string
    Types: IRTypeDef list
    Functions: IRFuncDef list
    Bindings: IRBinding list
    /// Diagnostics: static function usage tracking (function name -> usage kind)
    /// "compile-time" | "runtime" | "both" | "unused"
    StaticFunctionUsage: Map<string, string>
    /// Deferred provider reads, keyed by the receiving binding's IRId.
    /// Populated during lowering, consumed at codegen for the provider reader.
    ProviderReads: Map<IRId, ProviderReadSpec>
    /// Deferred provider writes (`alias.write("path", A)`), keyed by the
    /// write binding's IRId; consumed at codegen for a flatten prologue + writer.
    ProviderWrites: Map<IRId, ProviderWriteSpec>
    /// Deferred random-fill array constructors, keyed by the receiving
    /// binding's IRId (`fill_random(mod)` or `rand.uniform/normal(key)`);
    /// consumed at codegen to emit allocate<> + a pool fill.
    RandomInits: Map<IRId, RandomFillSpec>
    /// Deferred compound-construction constructors (compound(dense, mask)),
    /// keyed by the receiving binding's IRId; consumed at codegen for P0
    /// index materialization + a dense->compact scatter. Value is (loweredDense, loweredMask).
    CompoundInits: Map<IRId, IRExpr * IRExpr>
    /// Deferred sparse-construction constructors (sparse(values, keys)),
    /// keyed by the receiving binding's IRId; consumed at codegen for index
    /// materialization + a straight pool copy (key order, no scatter). The
    /// keys source rides the binding TYPE's IRSparseKeys extent.
    SparseInits: Map<IRId, IRExpr>
    /// Block-level `let mut` bindings of ARRAY type, by binding IRId. IRLet
    /// erases the surface mutability flag, but a mut binding initialized from
    /// an existing array (`let mut a = Z`) must DEEP-COPY the storage --
    /// binding by value shares the data pointer, so mutation through `a`
    /// would silently corrupt `Z`. CodeGen/interpreter deep-copy at the
    /// binding site when the id is in this set.
    MutableArrayLets: Set<IRId>
    /// EMISSION-ORDER PROXY for functions a compiler pass SYNTHESIZED from an
    /// existing one: `derived id -> origin id`. Codegen emits bindings and
    /// functions interleaved in IRId order (a lower id means "written earlier
    /// in the source"), which is load-bearing for every `computeMainLocalFuncIds`
    /// main-local function: those are `std::function` LOCALS inside `main()`
    /// with no forward declaration, so a use before definition is a hard C++
    /// error. A pass minting a copy with `builder.FreshId()` gets the LARGEST
    /// id and sorts after every call site it just rewrote -- exactly the
    /// failure mode that broke `ml-equiv`/`sgs`.
    ///
    /// Contract: the derived function is placed IMMEDIATELY AFTER its origin
    /// in `Functions`, and codegen keys order on the ORIGIN's id, so a copy
    /// is emitted at exactly its origin's program point -- no per-pass
    /// main-locality reasoning required. Empty for modules with no
    /// synthesized copies; populated by `shapeMonomorphizeModules`, which
    /// records a copy in the module that DEFINES its origin even when the call
    /// site that earned it lives in another module.
    DerivedFuncOrigins: Map<IRId, IRId>
}

/// IR Program
type IRProgram = {
    Modules: IRModule list
}

/// Query: the fully-deduced IR type of a top-level binding by name, searched
/// across all modules of a lowered program. Shallow accessor intended for reuse
/// by the language server (hover / inline type) and the type-structure test
/// harness. Returns None if no binding with that name exists.
let bindingTypeByName (program: IRProgram) (name: string) : IRType option =
    program.Modules
    |> List.tryPick (fun m ->
        m.Bindings |> List.tryFind (fun b -> b.Name = name) |> Option.map _.Type)


// IR Construction Helpers

type IRBuilder() =
    let mutable nextId = 0
    let mutable nextInferId = 0
    
    member _.FreshId() =
        let id = nextId
        nextId <- nextId + 1
        id

    member _.CurrentId() = nextId
    /// Raise the id floor so ids minted here can never collide with ids
    /// minted by an earlier builder (codegen builds a fresh IRBuilder and
    /// must not reuse typecheck/lowering-era ids -- a synthetic binding
    /// registered under a reused id hijacks the original variable's
    /// rendered name).
    member _.EnsureAtLeast(n: int) = if nextId < n then nextId <- n
    member _.Reset() = 
        nextId <- 0
        nextInferId <- 0
    member this.MkVar(ty) = IRVar (this.FreshId(), ty)
    
    member _.FreshInferType() = 
        let id = nextInferId
        nextInferId <- nextInferId + 1
        IRTInfer id
    
    member this.MkLet(value, bodyFn) =
        let id = this.FreshId()
        IRLet(id, value, bodyFn id)

/// Caller-supplied fields that distinguish a named source-level function
/// from an anonymous lambda when building an IRCallable. Everything else
/// (params, body, captures, commutativity, parallelism) is the same for
/// both and is passed positionally to `mkCallable`.
///
///   - NameOverride: Some name for a source function or a named let-bound
///     lambda; None synthesizes "__lambda_<id>" for a truly anonymous one.
///   - IdOverride: Some id when the callable's IRId was allocated up front
///     (a source function's FuncId, or a named lambda that must be bound in
///     scope before its body is lowered for self-reference); None allocates
///     a fresh id here.
///   - IsStatic / IsArityPoly / ArityParam: function-only metadata; the
///     anonymous-lambda default (`defaultLambdaOptions`) leaves them off.
type CallableOptions =
    { NameOverride : string option
      IdOverride   : IRId option
      IsStatic     : bool
      IsArityPoly  : bool
      ArityParam   : string option }

/// Options for an anonymous lambda: no name, fresh id, no function-only
/// metadata. The single construction point for every lambda-shaped callable
/// in Lowering.fs (real lambdas, operator sections, partial applications,
/// synthesized binop kernels).
let defaultLambdaOptions : CallableOptions =
    { NameOverride = None; IdOverride = None; IsStatic = false; IsArityPoly = false; ArityParam = None }

/// The single builder for an IRCallable -- the merged construction point for
/// source-level functions and lambdas. `opts` carries the fields that differ
/// between the two (name, id, static, arity-poly); captures, return type,
/// commutativity, and parallelism are caller-supplied per callable (omp/
/// cuda/mpi clauses from either a where-clause or a strategy list flow
/// through identically for both kinds).
let mkCallable
    (builder: IRBuilder)
    (opts: CallableOptions)
    (parms: IRParam list)
    (body: IRExpr)
    (retType: IRType)
    (captures: CaptureInfo list)
    (isCommutative: bool)
    (commGroups: int list list)
    (parallelism: (int * int) list)
    (isOmpParallel: bool)
    (isCudaKernel: bool)
    (cudaBlockSize: int)
    (isMpiParallel: bool)
    : IRCallable =
    let id = match opts.IdOverride with Some i -> i | None -> builder.FreshId()
    {
        Id = id
        Name = match opts.NameOverride with Some n -> n | None -> $"__lambda_{id}"
        Params = parms
        RetType = retType
        Body = body
        IsStatic = opts.IsStatic
        IsCommutative = isCommutative
        CommGroups = commGroups
        // Declared antisymmetry is grafted on by the ONE construction site
        // that has it (Lowering.lowerTypedLambda, from the lambda's
        // where-clause); every other callable-building site is comm-only.
        AntisymGroups = []
        Parallelism = parallelism
        IsOmpParallel = isOmpParallel
        IsCudaKernel = isCudaKernel
        CudaBlockSize = cudaBlockSize
        IsMpiParallel = isMpiParallel
        IsArityPoly = opts.IsArityPoly
        ArityParam = opts.ArityParam
        // Grafted by Lowering.lowerTypedFuncDecl (the one site with the
        // clause), like AntisymGroups below.
        IsRepro = false
        Captures = captures
        // Like AntisymGroups: grafted on by the one construction site that has
        // it (Lowering.lowerTypedLambda, from the typechecked kernel's
        // summary); every other callable-building site carries none.
        SignParities = []
        // Grafted by Lowering.lowerTypedFuncDecl from the typed declaration;
        // unknown everywhere else (see the field's comment).
        Effects = Blade.Effects.unknown
    }

/// Build a fresh IRCallable for an anonymous inline lambda: synthesized
/// "__lambda_<id>" name, fresh id, not static/arity-polymorphic. Thin
/// wrapper over `mkCallable` with `defaultLambdaOptions`.
let mkLambdaCallable
    (builder: IRBuilder)
    (parms: IRParam list)
    (body: IRExpr)
    (retType: IRType)
    (captures: CaptureInfo list)
    (isCommutative: bool)
    (commGroups: int list list)
    (parallelism: (int * int) list)
    (isOmpParallel: bool)
    (isCudaKernel: bool)
    (cudaBlockSize: int)
    (isMpiParallel: bool)
    : IRCallable =
    mkCallable builder defaultLambdaOptions parms body retType captures
               isCommutative commGroups parallelism isOmpParallel
               isCudaKernel cudaBlockSize isMpiParallel

// The deduced WREATH TIE (docs/plan-orbit-index-types.md section 7 / section 9 step 4)
//
// section 1's motivating gap: `func(A, A)` with `A : SymIdx<2,n>` under
// `where comm` licenses `S_2 wr S_2` on the output's FOUR raw axes, with no
// prior way to say so. Juxtaposing `SymIdx<2,n>` x2 drops the tie (pool 36
// cells vs orbit count 21); widening to `SymIdx<4,n>` over-claims full S_4.
//
// The rule is APPEND A LEVEL: input class `L`, `k` repeated occurrences tied
// by a declared clause -> `L ++ [(k, s)]`, `s = '+'` for comm, `'-'` for
// anticomm. Depth-1 `SymIdx<r,n>` contributes `[(r,+)]`, `AntisymIdx<r,n>`
// contributes `[(r,-)]`, an already-wreath input contributes its own list --
// so depth 3 (`let P = f(A,A) in g(P,P)`, section 7.1) falls out of the same
// rule.


// Cross-procedural analysis context. All callable references in IR are
// IRVar(callable.Id, funcType); resolveCallable threads them back to
// the underlying IRCallable via the CallablesTable installed in the
// AsyncLocal context. Consumers (buildLoopNestCodeGen, validator's
// ApplyInfo check, mask-rewrite, exprAttrs IRApp arm) all share this
// resolution path.

type CallablesTable = Map<IRId, IRCallable>

type AnalysisContext = {
    Callables: CallablesTable
    Visited:   Set<IRId>
    /// Per-codegen-pass registry of transient synthetic callables, for
    /// transformations that need "callable with modified body" (e.g.
    /// applyFunctorWrappers' inline-wrap, IRIf guard wrap) without storing
    /// them in module.Functions. `resolveCallable` queries this registry
    /// alongside the module's CallablesTable. Mutable Dictionary for cheap
    /// in-place accumulation; a per-flow AsyncLocal field, so concurrent
    /// module compilations don't interfere.
    SyntheticCallables: System.Collections.Generic.Dictionary<IRId, IRCallable>
}

let private analysisCtxStorage =
    System.Threading.AsyncLocal<AnalysisContext>()

let private emptyAnalysisCtx : AnalysisContext =
    { Callables = Map.empty
      Visited = Set.empty
      SyntheticCallables = System.Collections.Generic.Dictionary<IRId, IRCallable>() }

let internal currentAnalysisCtx () : AnalysisContext =
    let v = analysisCtxStorage.Value
    if isNull (box v) then emptyAnalysisCtx else v

/// Install the callables table. Returns the previous context for
/// stack-style save/restore by the caller. The synthetic registry
/// is reset to a fresh empty Dictionary -- each module compilation
/// starts with no synthetic callables. Synthetic callables produced
/// during one module's codegen don't leak into another's.
let setCallablesContext (callables: CallablesTable) : AnalysisContext =
    let prev = currentAnalysisCtx ()
    analysisCtxStorage.Value <-
        { prev with
            Callables = callables
            SyntheticCallables = System.Collections.Generic.Dictionary<IRId, IRCallable>() }
    prev

/// Restore a previously-captured context.
let restoreAnalysisContext (ctx: AnalysisContext) : unit =
    analysisCtxStorage.Value <- ctx

/// Run `action` with fId added to Visited, restoring on completion.
/// Used by the IRApp arm of exprAttrs to mark a function as being
/// walked, so mutual recursion short-circuits when the cycle closes.
let internal withVisited (fId: IRId) (action: unit -> 'T) : 'T =
    let prev = currentAnalysisCtx ()
    analysisCtxStorage.Value <- { prev with Visited = Set.add fId prev.Visited }
    try action()
    finally analysisCtxStorage.Value <- prev

/// Register a synthetic callable in the current AnalysisContext's
/// registry and return an IRVar reference to it. The caller must
/// supply a fresh IRId for the callable (typically via
/// IRBuilder.FreshId()) so it doesn't collide with module.Functions
/// ids or other synthetic ids. The returned IRVar can be consumed
/// like any other callable reference -- resolveCallable will find
/// the registered version via the SyntheticCallables registry.
let registerSyntheticCallable (callable: IRCallable) : IRExpr =
    let ctx = currentAnalysisCtx ()
    ctx.SyntheticCallables.[callable.Id] <- callable
    let paramTypes = callable.Params |> List.map _.Type
    let funcType = mkFuncArrow paramTypes callable.RetType
    IRVar (callable.Id, funcType)

/// Resolve an expression at a "callable position" to the underlying
/// IRCallable: `IRVar(id, _)` resolving in the CallablesTable (module.
/// Functions + let-binding aliases) or the SyntheticCallables registry
/// (codegen-internal synthetics); anything else (or an unresolvable IRVar)
/// returns None.
let resolveCallable (expr: IRExpr) : IRCallable option =
    match expr with
    | IRVar (id, _) ->
        let ctx = currentAnalysisCtx ()
        match Map.tryFind id ctx.Callables with
        | Some c -> Some c
        | None ->
            // Fall through to synthetic registry. Dictionary lookup is
            // O(1); the registry is typically empty or single-digit
            // entries per module compilation.
            match ctx.SyntheticCallables.TryGetValue(id) with
            | true, c -> Some c
            | false, _ -> None
    | _ -> None

// Reynolds peel/resolve helpers
//
// The kernel slot of an ApplyInfo (and of functor-wrapper composition
// sites) may be either a bare callable reference (`IRVar(id, _)`) or
// that same reference wrapped in `IRReynolds(_, isAntisymmetric)`.
// Several passes need to look through the optional Reynolds wrapper to
// reach the underlying callable; these three helpers express the common
// pattern in one place rather than each site open-coding its own peel +
// resolveCallable dance.

/// Captures the flags carried by an `IRReynolds` wrapper. For
/// non-Reynolds kernels both flags are `false`. The invariant
/// `not HasReynolds => not IsAntisymmetric` is preserved by construction
/// in `peelReynolds` (the only constructor in normal use).
type ReynoldsDescriptor = {
    HasReynolds: bool
    IsAntisymmetric: bool
}

/// Peel an `IRReynolds` wrapper if present, returning the inner
/// expression and a descriptor of the wrapper's flags. For non-Reynolds
/// expressions the input is returned unchanged with a descriptor whose
/// flags are both `false`.
let peelReynolds (expr: IRExpr) : IRExpr * ReynoldsDescriptor =
    match expr with
    | IRReynolds (inner, isAnti) ->
        (inner, { HasReynolds = true; IsAntisymmetric = isAnti })
    | other ->
        (other, { HasReynolds = false; IsAntisymmetric = false })

/// Result of resolving a (possibly Reynolds-wrapped) kernel expression
/// to a callable through the CallablesTable + synthetic registry.
type ResolvedKernel = {
    Callable: IRCallable
    Reynolds: ReynoldsDescriptor
}

/// Peel any `IRReynolds` wrapper and resolve the inner expression to a
/// callable via `resolveCallable` (CallablesTable + synthetic registry).
/// Returns `None` if the inner doesn't resolve, regardless of whether a
/// Reynolds wrapper was present.
let resolveKernel (expr: IRExpr) : ResolvedKernel option =
    let (inner, desc) = peelReynolds expr
    resolveCallable inner
    |> Option.map (fun c -> { Callable = c; Reynolds = desc })

/// Apply a transformation to the inner callable of a (possibly
/// Reynolds-wrapped) kernel expression. Preserves the Reynolds wrapper
/// (with its `isAntisymmetric` flag) if present. If the inner doesn't
/// resolve to a callable, returns the original expression unchanged.
let mapKernelInner (transform: IRCallable -> IRExpr) (expr: IRExpr) : IRExpr =
    let (inner, desc) = peelReynolds expr
    match resolveCallable inner with
    | Some c ->
        let transformed = transform c
        if desc.HasReynolds then IRReynolds (transformed, desc.IsAntisymmetric)
        else transformed
    | None -> expr



// Canonical expression traversal -- ExprShape (audit section 3.2)
//
// THE one place that knows every IRExpr variant's immediate expression
// children. Every generic walker (mapIRExpr, collectVarRefsIR,
// collectTypesInExpr, exprAttrs) folds over this shape, so a new variant is
// added in exactly one place. Wildcard-free: a new variant fails to compile
// until declared here, an exhaustiveness guarantee a per-walker `| _ ->`
// fallback would silently destroy.
//
// Scope decisions (uniform across all walkers by construction): IRIndexType
// is OPAQUE to traversal (extent-marker expressions inside it are reached by
// dedicated extent paths, never generic traversal); boundary pads and range
// offsets ARE children (the BndPad expression in IRShift/IRAlign and
// IRRange's offset are real sub-expressions -- hand-maintained per-walker
// versions can disagree about this without the canonical shape).
//
// `rebuild` requires exactly the children it handed out (same count, same
// order); anything else is a hard failure, never a silent drop.

/// Child-list mismatch in a rebuild -- always a walker bug, never recoverable.
let private badChildren (ctor: string) : 'a =
    failwith $"ExprShape.rebuild: child list does not match {ctor}'s shape"

/// Total active pattern: an expression's immediate children, plus a function
/// rebuilding the same variant around replacement children.
let (|ExprShape|) (expr: IRExpr) : IRExpr list * (IRExpr list -> IRExpr) =
    match expr with
    // -- Leaves: no expression children ------------------------------------
    | IRLit _ | IRVar _ | IRParam _ | IRNth | IRZero | IROpaqueExtent
    | IRVirtualReverse _ | IRArity _ ->
        [], (function [] -> expr | _ -> badChildren "leaf")
    | IRRange (idxTys, offset) ->
        Option.toList offset,
        (function
         | [] when Option.isNone offset -> expr
         | [off'] when Option.isSome offset -> IRRange (idxTys, Some off')
         | _ -> badChildren "IRRange")

    // -- One child ----------------------------------------------------------
    | IRUnaryOp (op, e) -> [e], (function [e'] -> IRUnaryOp (op, e') | _ -> badChildren "IRUnaryOp")
    | IRTupleProj (e, i, flat) -> [e], (function [e'] -> IRTupleProj (e', i, flat) | _ -> badChildren "IRTupleProj")
    | IRTupleDecons e -> [e], (function [e'] -> IRTupleDecons e' | _ -> badChildren "IRTupleDecons")
    | IRFieldAccess (e, fld) -> [e], (function [e'] -> IRFieldAccess (e', fld) | _ -> badChildren "IRFieldAccess")
    | IRPure e -> [e], (function [e'] -> IRPure e' | _ -> badChildren "IRPure")
    | IRCompute e -> [e], (function [e'] -> IRCompute e' | _ -> badChildren "IRCompute")
    | IRReynolds (e, anti) -> [e], (function [e'] -> IRReynolds (e', anti) | _ -> badChildren "IRReynolds")
    | IRTranspose (e, d1, d2) -> [e], (function [e'] -> IRTranspose (e', d1, d2) | _ -> badChildren "IRTranspose")
    | IRDecompact (e, d) -> [e], (function [e'] -> IRDecompact (e', d) | _ -> badChildren "IRDecompact")
    | IREigh e -> [e], (function [e'] -> IREigh e' | _ -> badChildren "IREigh")
    | IRSolve (a, b) -> [a; b], (function [a'; b'] -> IRSolve (a', b') | _ -> badChildren "IRSolve")
    | IRLu a -> [a], (function [a'] -> IRLu a' | _ -> badChildren "IRLu")
    | IRLuSolve (l, p, b, t) -> [l; p; b], (function [l'; p'; b'] -> IRLuSolve (l', p', b', t) | _ -> badChildren "IRLuSolve")
    | IRHaloUnhash (w, o) -> [w], (function [w'] -> IRHaloUnhash (w', o) | _ -> badChildren "IRHaloUnhash")
    | IRArrayNegate e -> [e], (function [e'] -> IRArrayNegate e' | _ -> badChildren "IRArrayNegate")
    | IRArrayConjugate e -> [e], (function [e'] -> IRArrayConjugate e' | _ -> badChildren "IRArrayConjugate")
    | IRReverse (e, d) -> [e], (function [e'] -> IRReverse (e', d) | _ -> badChildren "IRReverse")
    | IRDiag e -> [e], (function [e'] -> IRDiag e' | _ -> badChildren "IRDiag")
    | IRRank e -> [e], (function [e'] -> IRRank e' | _ -> badChildren "IRRank")
    | IRExtent (e, d) -> [e], (function [e'] -> IRExtent (e', d) | _ -> badChildren "IRExtent")
    | IRRaggedLookup e -> [e], (function [e'] -> IRRaggedLookup e' | _ -> badChildren "IRRaggedLookup")
    | IRCompoundMask e -> [e], (function [e'] -> IRCompoundMask e' | _ -> badChildren "IRCompoundMask")
    | IRCompoundProject (e, plen) -> [e], (function [e'] -> IRCompoundProject (e', plen) | _ -> badChildren "IRCompoundProject")
    | IRSparseKeys (SkRuntime e) -> [e], (function [e'] -> IRSparseKeys (SkRuntime e') | _ -> badChildren "IRSparseKeys")
    | IRSparseKeys (SkStatic _) -> [], (function [] -> expr | _ -> badChildren "IRSparseKeys")   // baked entries: no child exprs
    | IRSparseKeys (SkDomain _) -> [], (function [] -> expr | _ -> badChildren "IRSparseKeys")   // a plan: data, no child exprs
    // The level list is compile-time data, never an expression; the BASE extent
    // is an ordinary extent expression and is exposed as the one child, so
    // substitution / varref collection / folding reach it exactly as they reach
    // a plain Idx extent.
    | IROrbitClass (levels, n) ->
        [n], (function [n'] -> IROrbitClass (levels, n') | _ -> badChildren "IROrbitClass")
    | IRUnique e -> [e], (function [e'] -> IRUnique e' | _ -> badChildren "IRUnique")

    // -- Two children ---------------------------------------------------------
    | IRBinOp (mode, op, l, r) -> [l; r], (function [l'; r'] -> IRBinOp (mode, op, l', r') | _ -> badChildren "IRBinOp")
    | IRComplex (re, im) -> [re; im], (function [re'; im'] -> IRComplex (re', im') | _ -> badChildren "IRComplex")
    | IRTupleCons (h, t) -> [h; t], (function [h'; t'] -> IRTupleCons (h', t') | _ -> badChildren "IRTupleCons")
    | IRBind (c, k) -> [c; k], (function [c'; k'] -> IRBind (c', k') | _ -> badChildren "IRBind")
    | IRParallel (a, b, d) -> [a; b], (function [a'; b'] -> IRParallel (a', b', d) | _ -> badChildren "IRParallel")
    | IRFusion (a, b) -> [a; b], (function [a'; b'] -> IRFusion (a', b') | _ -> badChildren "IRFusion")
    | IRChoice (a, b) -> [a; b], (function [a'; b'] -> IRChoice (a', b') | _ -> badChildren "IRChoice")
    | IRFallback (a, b) -> [a; b], (function [a'; b'] -> IRFallback (a', b') | _ -> badChildren "IRFallback")
    | IRArrayProduct (a, b) -> [a; b], (function [a'; b'] -> IRArrayProduct (a', b') | _ -> badChildren "IRArrayProduct")
    | IRComposeObj (a, b) -> [a; b], (function [a'; b'] -> IRComposeObj (a', b') | _ -> badChildren "IRComposeObj")
    | IRComposeMeth (a, b) -> [a; b], (function [a'; b'] -> IRComposeMeth (a', b') | _ -> badChildren "IRComposeMeth")
    | IRCompose (a, b) -> [a; b], (function [a'; b'] -> IRCompose (a', b') | _ -> badChildren "IRCompose")
    | IRFunctorMap (fn, c) -> [fn; c], (function [fn'; c'] -> IRFunctorMap (fn', c') | _ -> badChildren "IRFunctorMap")
    | IRGuard (c, b) -> [c; b], (function [c'; b'] -> IRGuard (c', b') | _ -> badChildren "IRGuard")
    | IRReplicate (count, body) -> [count; body], (function [c'; b'] -> IRReplicate (c', b') | _ -> badChildren "IRReplicate")
    | IRMask (a, p) -> [a; p], (function [a'; p'] -> IRMask (a', p') | _ -> badChildren "IRMask")
    | IRIntersect (a, b) -> [a; b], (function [a'; b'] -> IRIntersect (a', b') | _ -> badChildren "IRIntersect")
    | IRUnion (a, b) -> [a; b], (function [a'; b'] -> IRUnion (a', b') | _ -> badChildren "IRUnion")
    | IRContains (a, v) -> [a; v], (function [a'; v'] -> IRContains (a', v') | _ -> badChildren "IRContains")
    // The id operand, when present, is a child like the payload: a walker that
    // skipped it would rewrite the payload and leave a stale id expression.
    | IRDisplayEmit (h, q, d, m, None) ->
        [d], (function [d'] -> IRDisplayEmit (h, q, d', m, None) | _ -> badChildren "IRDisplayEmit")
    | IRDisplayEmit (h, q, d, m, Some i) ->
        [d; i], (function [d'; i'] -> IRDisplayEmit (h, q, d', m, Some i') | _ -> badChildren "IRDisplayEmit")
    | IRDisplayJson (r, d) -> [d], (function [d'] -> IRDisplayJson (r, d') | _ -> badChildren "IRDisplayJson")
    | IRDisplayNum d -> [d], (function [d'] -> IRDisplayNum d' | _ -> badChildren "IRDisplayNum")
    | IRDisplayStr d -> [d], (function [d'] -> IRDisplayStr d' | _ -> badChildren "IRDisplayStr")
    | IRGroupBy (v, k) -> [v; k], (function [v'; k'] -> IRGroupBy (v', k') | _ -> badChildren "IRGroupBy")
    | IRGroupBucket gk -> [gk], (function [gk'] -> IRGroupBucket gk' | _ -> badChildren "IRGroupBucket")
    | IRSegments _ -> [], (fun _ -> expr)
    | IRSegmentsGrid _ -> [], (fun _ -> expr)
    | IRUngroupGrid (g, srcs, b) -> [g], (function [g'] -> IRUngroupGrid (g', srcs, b) | _ -> badChildren "IRUngroupGrid")
    | IRUngroup (g, src) -> [g], (function [g'] -> IRUngroup (g', src) | _ -> badChildren "IRUngroup")
    | IRUngroupRows (rows, offs, src) ->
        rows, (fun rows' -> if rows'.Length = rows.Length then IRUngroupRows (rows', offs, src) else badChildren "IRUngroupRows")
    | IRGroupSizes gk -> [gk], (function [gk'] -> IRGroupSizes gk' | _ -> badChildren "IRGroupSizes")
    | IRSort (a, k) -> [a; k], (function [a'; k'] -> IRSort (a', k') | _ -> badChildren "IRSort")
    | IRReduce (a, k, None) -> [a; k], (function [a'; k'] -> IRReduce (a', k', None) | _ -> badChildren "IRReduce")
    | IRReduce (a, k, Some i) -> [a; k; i], (function [a'; k'; i'] -> IRReduce (a', k', Some i') | _ -> badChildren "IRReduce")
    | IRReduceCompute (c, k, i) -> [c; k; i], (function [c'; k'; i'] -> IRReduceCompute (c', k', i') | _ -> badChildren "IRReduceCompute")
    | IRProdSum args -> args, (fun args' -> IRProdSum args')
    | IRPolyIndex (p, i) -> [p; i], (function [p'; i'] -> IRPolyIndex (p', i') | _ -> badChildren "IRPolyIndex")
    | IRPolyTail (p, drop) -> [p], (function [p'] -> IRPolyTail (p', drop) | _ -> badChildren "IRPolyTail")
    | IRAssign (t, v) -> [t; v], (function [t'; v'] -> IRAssign (t', v') | _ -> badChildren "IRAssign")
    | IRConstraintCheck (c, code, msg, sp) -> [c], (function [c'] -> IRConstraintCheck (c', code, msg, sp) | _ -> badChildren "IRConstraintCheck")
    | IRBreakIf c -> [c], (function [c'] -> IRBreakIf c' | _ -> badChildren "IRBreakIf")
    | IRCurry (arr, idx, r) -> [arr; idx], (function [arr'; idx'] -> IRCurry (arr', idx', r) | _ -> badChildren "IRCurry")
    | IRGram (l, r, same) -> [l; r], (function [l'; r'] -> IRGram (l', r', same) | _ -> badChildren "IRGram")
    | IRGramApply (l, r, x) -> [l; r; x], (function [l'; r'; x'] -> IRGramApply (l', r', x') | _ -> badChildren "IRGramApply")
    | IRMatmul (l, r) -> [l; r], (function [l'; r'] -> IRMatmul (l', r') | _ -> badChildren "IRMatmul")
    | IRLet (id, v, b) -> [v; b], (function [v'; b'] -> IRLet (id, v', b') | _ -> badChildren "IRLet")

    // -- Three children -------------------------------------------------------
    | IRIf (c, t, e) -> [c; t; e], (function [c'; t'; e'] -> IRIf (c', t', e') | _ -> badChildren "IRIf")
    | IRFma (a, b, c) -> [a; b; c], (function [a'; b'; c'] -> IRFma (a', b', c') | _ -> badChildren "IRFma")
    | IRSlice (arr, d, s, e) -> [arr; s; e], (function [arr'; s'; e'] -> IRSlice (arr', d, s', e') | _ -> badChildren "IRSlice")
    | IRSubset (arr, d, s, len) -> [arr; s; len], (function [arr'; s'; len'] -> IRSubset (arr', d, s', len') | _ -> badChildren "IRSubset")
    | IRForRange (vid, lo, hi, body) -> [lo; hi; body], (function [lo'; hi'; b'] -> IRForRange (vid, lo', hi', b') | _ -> badChildren "IRForRange")
    | IRShift (arr, d, off, bnd) ->
        (match bnd with
         | BndPad p ->
             [arr; off; p],
             (function [arr'; off'; p'] -> IRShift (arr', d, off', BndPad p') | _ -> badChildren "IRShift")
         | BndShrink | BndPeriodic | BndReflect ->
             [arr; off],
             (function [arr'; off'] -> IRShift (arr', d, off', bnd) | _ -> badChildren "IRShift"))

    // -- Head + list ----------------------------------------------------------
    | IRIndex (arr, idxs, ident) ->
        arr :: idxs,
        (function
         | arr' :: idxs' when idxs'.Length = idxs.Length -> IRIndex (arr', idxs', ident)
         | _ -> badChildren "IRIndex")
    | IRApp (f, args, rt) ->
        f :: args,
        (function
         | f' :: args' when args'.Length = args.Length -> IRApp (f', args', rt)
         | _ -> badChildren "IRApp")

    // -- Lists ----------------------------------------------------------------
    | IRArrayLit (es, ty) -> es, (fun es' -> if es'.Length = es.Length then IRArrayLit (es', ty) else badChildren "IRArrayLit")
    | IRTuple es -> es, (fun es' -> if es'.Length = es.Length then IRTuple es' else badChildren "IRTuple")
    | IRSequence es -> es, (fun es' -> if es'.Length = es.Length then IRSequence es' else badChildren "IRSequence")
    | IRZip es -> es, (fun es' -> if es'.Length = es.Length then IRZip es' else badChildren "IRZip")
    | IRStack es -> es, (fun es' -> if es'.Length = es.Length then IRStack es' else badChildren "IRStack")
    | IRJoin (es, d) -> es, (fun es' -> if es'.Length = es.Length then IRJoin (es', d) else badChildren "IRJoin")
    | IRGroupKeys ks -> ks, (fun ks' -> if ks'.Length = ks.Length then IRGroupKeys ks' else badChildren "IRGroupKeys")
    | IRAlign (es, spec) ->
        (match spec.Boundary with
         | BndPad p ->
             es @ [p],
             (fun cs ->
                 if cs.Length <> es.Length + 1 then badChildren "IRAlign"
                 else
                     let es', rest = List.splitAt es.Length cs
                     IRAlign (es', { spec with Boundary = BndPad (List.exactlyOne rest) }))
         | BndShrink | BndPeriodic | BndReflect ->
             es, (fun es' -> if es'.Length = es.Length then IRAlign (es', spec) else badChildren "IRAlign"))
    | IRStructLit (tn, fields) ->
        List.map snd fields,
        (fun es' ->
            if es'.Length <> fields.Length then badChildren "IRStructLit"
            else IRStructLit (tn, List.map2 (fun (n, _) e' -> (n, e')) fields es'))

    // -- Match: flat child list is scrutinee, then per-case guard?/body ------
    | IRMatch (scrut, cases) ->
        let caseChildren = cases |> List.collect (fun c -> Option.toList c.Guard @ [c.Body])
        scrut :: caseChildren,
        (function
         | scrut' :: rest ->
             // Re-thread the flat list back through the (guard?, body) case
             // structure; leftovers or shortfalls are shape violations.
             // Accumulated reversed (cons, not `acc @ [x]`): this fold runs on
             // every IRMatch rebuild in every mapIRExpr pass.
             let casesRev, leftover =
                 cases |> List.fold (fun (acc, remaining) c ->
                     match c.Guard, remaining with
                     | Some _, g' :: b' :: tl -> ({ c with Guard = Some g'; Body = b' } :: acc, tl)
                     | None, b' :: tl -> ({ c with Body = b' } :: acc, tl)
                     | _ -> badChildren "IRMatch") ([], rest)
             if not (List.isEmpty leftover) then badChildren "IRMatch"
             else IRMatch (scrut', List.rev casesRev)
         | [] -> badChildren "IRMatch")

    // -- Info-record combinators ----------------------------------------------
    | IRMethodFor info ->
        info.Arrays,
        (fun arrs' ->
            if arrs'.Length = info.Arrays.Length then IRMethodFor { info with Arrays = arrs' }
            else badChildren "IRMethodFor")
    | IRObjectFor info ->
        [info.Kernel], (function [k'] -> IRObjectFor { info with Kernel = k' } | _ -> badChildren "IRObjectFor")
    | IRApplyCombinator info ->
        info.Loop :: info.Kernel :: info.Arrays,
        (function
         | l' :: k' :: arrs' when arrs'.Length = info.Arrays.Length ->
             IRApplyCombinator { info with Loop = l'; Kernel = k'; Arrays = arrs' }
         | _ -> badChildren "IRApplyCombinator")
    | IRComposeApply info ->
        info.Composition :: info.InputArrays,
        (function
         | c' :: arrs' when arrs'.Length = info.InputArrays.Length ->
             IRComposeApply { info with Composition = c'; InputArrays = arrs' }
         | _ -> badChildren "IRComposeApply")

/// Immediate expression children of a node, in canonical order.
let childrenOf (ExprShape (children, _)) : IRExpr list = children

/// Rebuild a node around replacement children (same count/order as
/// childrenOf handed out).
let rebuildWith (expr: IRExpr) (children: IRExpr list) : IRExpr =
    let (ExprShape (_, rebuild)) = expr
    rebuild children

/// Expression-node count of a lowered program: every binding value plus every
/// function body, counted through `childrenOf`. Measurement support only
/// (docs/plan-compile-speed.md Stage 5 -- the "typeOf calls vs IR node count"
/// ratio), so it is called from the driver ONLY when PerfCounters are on and
/// costs nothing otherwise.
let countProgramNodes (program: IRProgram) : int64 =
    let rec count (e: IRExpr) : int64 =
        childrenOf e |> List.fold (fun acc c -> acc + count c) 1L
    program.Modules
    |> List.sumBy (fun m ->
        (m.Bindings |> List.sumBy (fun b -> count b.Value))
        + (m.Functions |> List.sumBy (fun f -> count f.Body)))

/// Pattern bindings: IRPatVar introduces an IRId visible in the case body
/// and (if present) the guard. Nested patterns (tuple, cons, variant
/// payload) accumulate all their child bindings.
let rec patternBoundIds (pat: IRPattern) : Set<IRId> =
    match pat with
    | IRPatWild | IRPatLit _ -> Set.empty
    | IRPatVar id -> Set.singleton id
    | IRPatTuple pats -> pats |> List.map patternBoundIds |> Set.unionMany
    | IRPatCons (h, t) -> Set.union (patternBoundIds h) (patternBoundIds t)
    | IRPatVariant (_, _, Some p, _) -> patternBoundIds p
    | IRPatVariant (_, _, None, _) -> Set.empty
    | IRPatStruct (_, flds) ->
        flds |> List.fold (fun acc (_, p) -> Set.union acc (patternBoundIds p)) Set.empty

/// The variants that introduce variable scopes, factored out for
/// binder-aware dispatchers (exprAttrs today; any future capture or escape
/// analysis). Returns the children NOT under a binder, plus one
/// (boundIds, scopedChildren) group per scope. Non-binding variants return
/// None and fall through to the generic ExprShape arm of whichever
/// dispatcher is asking -- so a new binding variant needs exactly one case
/// here to get correct scoping everywhere.
let (|BinderShape|_|) (expr: IRExpr) : (IRExpr list * (Set<IRId> * IRExpr list) list) option =
    match expr with
    | IRLet (id, value, body) ->
        // `value` is deliberately OUTSIDE the scope: a reference to `id`
        // inside its own value is ill-formed IR, and leaving it unscoped
        // keeps such a bug visible as a free var at the outer level.
        Some ([value], [Set.singleton id, [body]])
    | IRForRange (vid, lo, hi, body) ->
        Some ([lo; hi], [Set.singleton vid, [body]])
    | IRMatch (scrut, cases) ->
        // Pattern bindings are visible in both the guard and the body.
        Some ([scrut],
              cases |> List.map (fun c ->
                  (patternBoundIds c.Pattern, Option.toList c.Guard @ [c.Body])))
    | _ -> None

// Expression Mapping (bottom-up rewriter)

/// Apply f to every sub-expression bottom-up, then to the root.
/// f should return the expression unchanged for cases it doesn't handle.
/// Generic recursion is a fold over ExprShape -- variant-specific structure
/// lives entirely in the shape enumeration above.
let rec mapIRExpr (f: IRExpr -> IRExpr) (expr: IRExpr) : IRExpr =
    let mapped =
        match expr with
        | ExprShape ([], _) -> expr
        | ExprShape (children, rebuild) -> rebuild (children |> List.map (mapIRExpr f))
    f mapped

/// Visit every sub-expression, in EXACTLY the order `mapIRExpr` applies its
/// callback -- children left-to-right, then the node itself (post-order) --
/// without rebuilding a single node. The visitor twin of `mapIRExpr`: any
/// site that only wanted the traversal (and threw the rebuilt tree away with
/// `|> ignore`) uses this instead, so a whole IR copy is not allocated per
/// analysis pass. The post-order contract is load-bearing: accumulators that
/// prepend, or dictionaries where a later write wins, depend on it.
let rec iterIRExpr (f: IRExpr -> unit) (expr: IRExpr) : unit =
    (match expr with
     | ExprShape ([], _) -> ()
     | ExprShape (children, _) -> children |> List.iter (iterIRExpr f))
    f expr

/// Collect every variable id referenced (IRVar) anywhere in an expression.
/// The one var-ref collector (audit section 3.2): capture computation and
/// match-case usage checks in CodeGen call this rather than keeping their
/// own duplicate. Recursion is the ExprShape fold, so no variant's subtree
/// can be silently skipped by a stray `| _ -> Set.empty` catchall.
///
/// Scoping contract: only IRForRange subtracts its binder -- its loop var is
/// synthesized and callers never mean it. IRLet ids and match-pattern ids
/// stay IN the result because the call sites subtract or query specific ids
/// themselves. For real free/bound analysis use exprAttrs, which scopes all
/// binders via BinderShape.
let rec collectVarRefsIR (expr: IRExpr) : Set<IRId> =
    match expr with
    | IRVar (id, _) -> Set.singleton id
    | IRForRange (vid, lo, hi, body) ->
        Set.unionMany [collectVarRefsIR lo; collectVarRefsIR hi; Set.remove vid (collectVarRefsIR body)]
    | ExprShape (children, _) ->
        children |> List.map collectVarRefsIR |> Set.unionMany

// HM Type Substitution
//
// Substitutes IRTInfer occurrences with concrete types throughout types and
// expressions. Pure structural substitution -- no rewrites, no expansion.
// This is the substrate shared between HM monomorphization and (eventually,
// in the unified architecture) Poly's type-substitution step.

/// Substitute IRTInfer occurrences in a type, recursing into compound types.
/// Bindings is a map from type-var ID to concrete type. Type vars not in
/// the map are left as IRTInfer.
let rec substTypeInIRType (bindings: Map<int, IRType>) (ty: IRType) : IRType =
    match ty with
    | IRTInfer n when bindings.ContainsKey n -> bindings.[n]
    | IRTTuple ts -> IRTTuple (ts |> List.map (substTypeInIRType bindings))
    | IRTComputation t -> IRTComputation (substTypeInIRType bindings t)
    | IRTPoly (base', var) -> IRTPoly (substTypeInIRType bindings base', var)
    | IRTUnitAnnotated (inner, units) -> IRTUnitAnnotated (substTypeInIRType bindings inner, units)
    | IRTIdxTagged (inner, idxRef) -> IRTIdxTagged (substTypeInIRType bindings inner, idxRef)
    | IRTDist (order, elem, axes) -> IRTDist (order, substTypeInIRType bindings elem, axes)
    | IRTArrow (slots, result, identity) ->
        let substSlot = function
            | SIdx idx -> SIdx idx
            | SIdxVirt idx -> SIdxVirt idx   // IRIndexType has no IRType members; opaque
            | SVal ty -> SVal (substTypeInIRType bindings ty)
        IRTArrow (slots |> List.map substSlot, substTypeInIRType bindings result, identity)
    | _ -> ty

/// Substitute IRVar references throughout an expression tree: each
/// IRVar(id, _) with an entry in `mapping` is replaced by the mapped
/// expression. Used when importing a called function's probes into a
/// caller's analysis, resolving the probe's formal-parameter references to
/// the actual argument expressions at the call site. Walks bottom-up via
/// mapIRExpr; only IRVar nodes in `mapping` are affected.
let substituteIRVars (mapping: Map<IRId, IRExpr>) (expr: IRExpr) : IRExpr =
    mapIRExpr (fun e ->
        match e with
        | IRVar (id, _) when Map.containsKey id mapping -> Map.find id mapping
        | _ -> e) expr

/// Substitute types in an IRExpr at all type-bearing positions. Uses
/// mapIRExpr for structural traversal; the per-node callback only updates
/// fields that carry types directly (IRVar.ty, IRApp.retType, etc.).
let substTypeInIRExpr (bindings: Map<int, IRType>) (expr: IRExpr) : IRExpr =
    let st = substTypeInIRType bindings
    let substInNode e =
        match e with
        | IRVar (id, ty) -> IRVar (id, st ty)
        | IRParam (n, i, ty) -> IRParam (n, i, st ty)
        | IRApp (fn, args, retType) -> IRApp (fn, args, st retType)
        | IRArrayLit (elems, aty) ->
            IRArrayLit (elems, { aty with ElemType = st aty.ElemType })
        | IRMethodFor info ->
            IRMethodFor { info with
                            ArrayTypes = info.ArrayTypes
                                         |> List.map (fun aty -> { aty with ElemType = st aty.ElemType }) }
        | IRApplyCombinator info ->
            // ArrayTypes (ElemType may be a type variable referencing the
            // surrounding function's T) and OutputType (the deduced
            // result-array type, often sharing that T) must be substituted
            // in lockstep with the rest of the tree; skipping them leaves
            // stale IRTInfer in spec bodies, which the IR validator flags.
            IRApplyCombinator { info with
                                  ArrayTypes = info.ArrayTypes
                                               |> List.map (fun aty ->
                                                    { aty with ElemType = st aty.ElemType })
                                  OutputType = st info.OutputType }
        | IRComposeApply info ->
            // ComposeApplyInfo carries only OutputType as a type-bearing
            // field. The composition's leaves (IRObjectFor entries with
            // their own kernels) carry their own type metadata that the
            // generic walk reaches via the Composition / InputArrays
            // descent.
            IRComposeApply { info with OutputType = st info.OutputType }
        | _ -> e
    mapIRExpr substInNode expr

/// Types carried directly on a node -- no reconstruction, no environment.
/// The shared first tier of the canonical typing (audit section 2.2): the first
/// arm of typeOf, and the base case every arm of exprTypeIfKnown bottoms out in.
let (|CarriedType|_|) (expr: IRExpr) : IRType option =
    match expr with
    | IRVar (_, ty) -> Some ty
    | IRParam (_, _, ty) -> Some ty
    | IRApp (_, _, retType) -> Some retType
    | IRArrayLit (_, aty) -> Some (mkArrayLike aty)
    | IRStructLit (typeName, _) -> Some (IRTNamed typeName)
    | IRApplyCombinator info -> Some info.OutputType
    | IRComposeApply info -> Some info.OutputType
    | IRLit (IRLitInt _) -> Some (IRTScalar ETInt64)
    | IRLit (IRLitFloat _) -> Some (IRTScalar ETFloat64)
    | IRLit (IRLitFloat32 _) -> Some (IRTScalar ETFloat32)
    | IRLit (IRLitBool _) -> Some (IRTScalar ETBool)
    | IRLit (IRLitString _) -> Some (IRTScalar ETString)
    | IRLit IRLitUnit -> Some IRTUnit
    | _ -> None

/// Map from struct name to its fields, used by typeOf for IRFieldAccess
/// resolution. Built at liftInlineFormsModule entry, used throughout the
/// same lift-pass invocation.
///
/// Thread-safety: the test runner compiles tests in parallel
/// (`Array.Parallel.mapi`); a plain module-level mutable Dictionary would
/// let one test's `setStructFieldsCache` wipe another's cache state,
/// causing intermittent `IRTUnit` results (see the `Struct Array With
/// Array Field` regression). `AsyncLocal<T>` with a fresh Dictionary per
/// set call gives each task its own instance.
let private structFieldsCacheStorage =
    System.Threading.AsyncLocal<System.Collections.Generic.Dictionary<string, (string * IRType) list>>()

let private getStructFieldsCache () : System.Collections.Generic.Dictionary<string, (string * IRType) list> =
    let v = structFieldsCacheStorage.Value
    if isNull v then
        let fresh = System.Collections.Generic.Dictionary<string, (string * IRType) list>()
        structFieldsCacheStorage.Value <- fresh
        fresh
    else v

// typeOf memoization (docs/plan-compile-speed.md Stage 5)
//
// WHY: `typeOf` reconstructs a node's type from its children, so one call on
// the root of a deep un-let-bound expression costs O(depth) -- and every pass
// that asks each node in turn therefore pays O(n^2). Measured on the Stage 5
// chain probes (one expression, n `x * k` terms): typeOf calls come out at
// EXACTLY 2n^2 (n=200 -> 80,398 calls over 800 IR nodes = 100x the node
// count; n=2000 -> 8,003,998 over 8,000 nodes = 1000x). Real programs sit at
// 1.3-1.9x (lsdft, 01_weather_stations), so this is purely a cliff-remover.
//
// SOUNDNESS: every `typeOf` arm is a pure function of the node's structure
// with ONE exception -- `IRFieldAccess` consults `tryLookupFieldType`, i.e.
// the struct-fields cache, which is (re)populated per module at
// liftInlineFormsModule entry and again at codegen entry. A result computed
// before a population would otherwise be served after it. So the memo is
// dropped whenever that cache is set (below): entries can only outlive a
// generation in which nothing changed. Everything else typeOf touches
// (promoteElemType, mkArrayLike, flattenTupleLeaves, classifyCompound...,
// orbitBaseExtent) is pure, and IRExpr values are immutable -- a rebuilt node
// is a NEW reference and therefore a new key.
//
// Keyed by REFERENCE (ConditionalWeakTable), so there is no structural
// hashing cost and no lifetime extension: entries die with their nodes.
let mutable private typeOfMemo =
    System.Runtime.CompilerServices.ConditionalWeakTable<IRExpr, IRType>()

/// Drop every memoized `typeOf` result. See the note above: this is what keeps
/// the memo honest across struct-fields-cache generations.
let private invalidateTypeOfMemo () =
    typeOfMemo <- System.Runtime.CompilerServices.ConditionalWeakTable<IRExpr, IRType>()

let setStructFieldsCache (types: IRTypeDef list) =
    // Create a fresh Dictionary for this async context -- do not reuse and
    // .Clear() a shared instance, since other tasks may hold the same
    // reference from earlier in the parallel test run.
    let cache = System.Collections.Generic.Dictionary<string, (string * IRType) list>()
    for td in types do
        match td with
        | IRTDStruct (name, fields) -> cache.[name] <- fields
        | _ -> ()
    structFieldsCacheStorage.Value <- cache
    // A new struct-fields generation invalidates every reconstruction that
    // could have consulted the old one (typeOf's IRFieldAccess arm).
    invalidateTypeOfMemo ()

// Declaration spans, by top-level name.
//
// The IR carries NO source positions -- deliberately: it is the phase where
// surface syntax has been discharged. That is fine until a back-end phase has
// to REFUSE something, at which point "which line" is the only question the
// user has. Lowering does see the surface program, so it records the span of
// every top-level declaration here on the way past, and codegen refusals
// resolve their enclosing declaration's name through this table.
//
// Coarse ON PURPOSE: one span per declaration, not per node. It points the
// caret at the `let`/`function` the refused construct lives in, which is the
// granularity codegen can honestly support. AsyncLocal for the parallel test
// runner's per-task isolation, like every other cache in this file.
let private declSpansStorage = System.Threading.AsyncLocal<Map<string, Blade.Ast.Span> ref>()

let declSpansCell () : Map<string, Blade.Ast.Span> ref =
    let v = declSpansStorage.Value
    if isNull (box v) then
        let fresh = ref Map.empty
        declSpansStorage.Value <- fresh
        fresh
    else v

/// Record one declaration's span. Later declarations with the same name (a
/// shadowing `let`) win, matching what the emitted code refers to last.
let recordDeclSpan (name: string) (span: Blade.Ast.Span) : unit =
    if span.StartLine > 0 then
        let cell = declSpansCell ()
        cell.Value <- Map.add name span cell.Value

/// The recorded span for `name`, or `noSpan` when nothing was recorded
/// (a synthesized declaration, or a caller that never populated the table).
let declSpanOf (name: string) : Blade.Ast.Span =
    match Map.tryFind name (declSpansCell ()).Value with
    | Some s -> s
    | None -> Blade.Ast.noSpan

/// A named struct's fields in DECLARATION order, out of the same per-module
/// cache `tryLookupFieldType` reads. The order is what lets a name-erased
/// POSITIONAL struct pattern be decoded at all; a named one (`IRPatStruct`)
/// does not need to ask.
let tryLookupStructFieldsByName (structName: string) : (string * IRType) list option =
    let cache = getStructFieldsCache ()
    match cache.TryGetValue(structName) with
    | true, fields -> Some fields
    | false, _ -> None

/// The same lookup keyed by a scrutinee's type rather than a bare name.
let tryLookupStructFields (objType: IRType) : (string * IRType) list option =
    match objType with
    | IRTNamed structName -> tryLookupStructFieldsByName structName
    | _ -> None

let tryLookupFieldType (objType: IRType) (fieldName: string) : IRType option =
    match objType with
    | IRTNamed structName ->
        let cache = getStructFieldsCache ()
        match cache.TryGetValue(structName) with
        | true, fields ->
            fields |> List.tryFind (fun (n, _) -> n = fieldName) |> Option.map snd
        | false, _ -> None
    | _ -> None

// Canonical expression typing -- typeOf (audit section 2.2)
//
// THE one type-reconstruction over IR expressions. Until every IR node
// carries its type (the rewrite's design), passes that need a type must
// re-derive it -- multiple hand-maintained derivations invite silent
// divergence between them (a wrong-codegen bug class). The three roles:
//   - typeOf                -- the full reconstruction (this section)
//   - exprTypeIfKnown       -- CarriedType plus the view/wrapper forms that
//                             DELEGATE their rule here (defined after this
//                             section, which is why); HM call sites must not
//                             unify against a freely reconstructed type. See
//                             its doc comment.
//   - CodeGen.inferExprType -- thin alias of typeOf
//
// Dispatch is organized as active-pattern families feeding a top-level
// match, not one flat 74-arm wall:
//   CarriedType -- types carried directly on the node (defined earlier,
//                 shared with exprTypeIfKnown)
//   TypeVia     -- variants whose type IS one distinguished child's type
//   IntValued   -- index-arithmetic markers, always Int64
// Structural rules (indexing, group_by, gram, ...) follow, and the
// deliberately-untyped variants (loop objects, emission-internal markers)
// are enumerated WITHOUT a wildcard, so a new IRExpr variant demands an
// explicit typing decision here instead of silently becoming IRTUnit.

/// Variants whose type equals one distinguished child's type. The returned
/// expression is the child whose type to take -- not necessarily the first
/// child (e.g. `IRComposeMeth (_, right)` types as `right`).
let (|TypeVia|_|) (expr: IRExpr) : IRExpr option =
    match expr with
    // Shape- and element-preserving array transforms.
    | IRSort (a, _) | IRArrayNegate a | IRArrayConjugate a
    // Element-preserving, extent-changing set ops (extent is runtime data,
    // not part of the arrow shape consumers read).
    | IRIntersect (a, _) | IRUnion (a, _) | IRUnique a
    // Computation wrappers erase at this level.
    | IRCompute a | IRPure a
    // Control flow: the type of the canonical branch/body.
    | IRLet (_, _, a) | IRIf (_, a, _) | IRGuard (_, a) | IRChoice (a, _)
    // <|:> result is the dense expansion; the RIGHT side already carries the
    // dense type (compound-left widens to it, dense-left unified with it).
    | IRFallback (_, a)
    // @>> composition: the right side's type.
    | IRComposeMeth (_, a) ->
        Some a
    | IRMatch (_, c :: _) -> Some c.Body
    | _ -> None

/// Index-space arithmetic markers: always Int64 scalars.
let (|IntValued|_|) (expr: IRExpr) : bool =
    match expr with
    | IRArity _ | IRNth | IRRank _ | IRExtent _ | IRRaggedLookup _
    | IRCompoundMask _ | IRCompoundProject _ | IRSparseKeys _ | IROrbitClass _
    | IROpaqueExtent | IRRange _ ->
        true
    | _ -> false

// Synthetic sentinel index IDs
//
// Some typeOf branches construct an IRIndexType in flight (e.g. recovering
// the rank-2 shape of a not-yet-let-bound IRGroupBy result) without access
// to an IRBuilder to allocate fresh IDs. Convention: synthetic sentinel IDs
// are NEGATIVE (IRBuilder.FreshId counts up from 0, so the negative range
// never collides); each synthesizing call site picks a distinct one below.
// IDs are not load-bearing for codegen -- consumers pattern-match on
// structure (ArrayElem, IRTScalar) and `Tag`, not `Id`.
let synthSlotIdOuter : IRId = -1
let synthSlotIdMember : IRId = -2
let synthSlotIdCompoundResidual : IRId = -3

// Compound partial-index classification (formalism 4.5)
//
// Shared by typeOf's IRIndex arm, CodeGen's genScalarBinding wrapper
// decision, and exprToCppCore's compoundRead emission, so the three never
// disagree about WHICH indexing form a compound read is.
//
// A wildcard coordinate arrives as an `IRLit IRLitUnit` sentinel: TypeCheck's
// dispatchAppOrIndex rewrites each TExprWildcard hole to a unit literal
// (never a valid coordinate, so unambiguous). A short tuple (arity j < k, no
// sentinels) pins the LEADING j coordinates -- B((a,b)) and B((a,b,_)) on a
// rank-3 compound are the same read.

/// Classification of the FIRST index against a rank-k compound head slot.
type CompoundIndexForm =
    /// All k coordinates pinned: the compound axis is fully consumed.
    | CompoundFull
    /// Partial: `pinned` = (axis position, coordinate expr) for each pinned
    /// axis in increasing position order; `freePos` = the free axis positions.
    | CompoundPartial of pinned: (int * IRExpr) list * freePos: int list

let classifyCompoundIndexTuple (k: int) (coords: IRExpr list) : CompoundIndexForm =
    let isFreeSentinel = function IRLit IRLitUnit -> true | _ -> false
    if coords |> List.exists isFreeSentinel then
        // Full-arity wildcard tuple (TypeCheck enforces arity = k and >= 1 pin).
        let indexed = coords |> List.mapi (fun i c -> (i, c))
        let pinned = indexed |> List.filter (fun (_, c) -> not (isFreeSentinel c))
        let free = indexed |> List.filter (fun (_, c) -> isFreeSentinel c) |> List.map fst
        CompoundPartial (pinned, free)
    elif coords.Length < k then
        // Short tuple: leading-prefix pin, trailing axes free.
        CompoundPartial (coords |> List.mapi (fun i c -> (i, c)), [coords.Length .. k - 1])
    else
        CompoundFull

/// Coverage-arm backstop: a family active pattern above no longer covers a
/// constructor its coverage arm claims. Impossible unless a family pattern
/// was edited out of sync with typeOf's coverage tail -- fail loudly rather
/// than mistype silently.
let private unreachableTyping (family: string) (expr: IRExpr) : 'a =
    failwith $"typeOf: family pattern {family} no longer covers {(expr.GetType().Name)} -- coverage arm and family out of sync"

/// The canonical expression type reconstruction. See the section comment
/// above for how this relates to exprTypeIfKnown and CodeGen.inferExprType.
let rec typeOf (expr: IRExpr) : IRType =
    // Stage 5 instrumentation (docs/plan-compile-speed.md): total invocations,
    // and how many of them the CarriedType fast path answers outright. The
    // difference is the reconstructing traffic the memo below pays for.
    if PerfCounters.enabled then PerfCounters.typeOfCall ()
    match expr with
    // Node-carried types answer without touching a child, so they stay AHEAD
    // of the memo: a table probe would cost more than the answer.
    | CarriedType ty ->
        if PerfCounters.enabled then PerfCounters.typeOfCarried ()
        ty
    | _ ->
        // Reconstructing arms only (see the memo note above setStructFieldsCache).
        match typeOfMemo.TryGetValue expr with
        | true, ty ->
            if PerfCounters.enabled then PerfCounters.typeOfMemoHit ()
            ty
        | _ ->
            let ty = typeOfReconstruct expr
            // AddOrUpdate, not Add: two threads reconstructing the same node
            // compute the same (deterministic) answer, so a race is benign --
            // but Add would throw on the duplicate key.
            typeOfMemo.AddOrUpdate(expr, ty)
            ty

/// `typeOf`'s reconstruction body. Call `typeOf`, never this: the memo and the
/// carried-type fast path both live in the wrapper above, and the recursive
/// calls inside here deliberately go back through it so every intermediate
/// result is memoized once.
and private typeOfReconstruct (expr: IRExpr) : IRType =
    match expr with
    // -- Node-carried types (shared with exprTypeIfKnown) --
    // Unreachable through `typeOf` (the wrapper answers these), but kept so
    // this match stays exhaustive the same way it always was -- the coverage
    // tail below attributes IRVar/IRParam/... to exactly this arm.
    | CarriedType ty -> ty

    // -- Pass-throughs: the type of one distinguished child --
    | TypeVia child -> typeOf child

    // -- Index-arithmetic markers --
    | IntValued -> IRTScalar ETInt64

    | IRBinOp (_, op, left, right) ->
        (match op with
         | IREq | IRNeq | IRLt | IRLe | IRGt | IRGe | IRAnd | IROr -> IRTScalar ETBool
         // atan2 / log_base are real-valued regardless of operand widths (the
         // C++ overload set promotes integer operands to double), so they do
         // NOT follow the promote-the-operands rule below -- `atan2(1, 1)` over
         // two Int64s is a double, not an int.
         | IRMath2 _ -> IRTScalar ETFloat64
         | _ ->
             match typeOf left, typeOf right with
             | IRTScalar e1, IRTScalar e2 ->
                 IRTScalar (promoteElemType e1 e2 |> Option.defaultValue e1)
             | lt, _ -> lt)
    | IRUnaryOp (op, operand) ->
        (match op with
         | IRNot -> IRTScalar ETBool
         | IRNeg -> typeOf operand
         | IRConj -> typeOf operand
         // real/imag project a complex to its component width (identity on a
         // real operand); arg is a real angle. Complex128 -> Float64,
         // Complex64 -> Float32.
         | IRReal | IRImag ->
             (match typeOf operand with
              | IRTScalar ETComplex64 -> IRTScalar ETFloat32
              | IRTScalar ETComplex128 -> IRTScalar ETFloat64
              | other -> other)
         | IRArg -> IRTScalar ETFloat64
         // abs always yields the real magnitude (Float64); other intrinsics on
         // a complex operand preserve the complex type (std::exp(complex)->
         // complex), and stay Float64 on a real operand.
         | IRMath "abs" -> IRTScalar ETFloat64
         | IRMath _ ->
             (match typeOf operand with
              | IRTScalar (ETComplex64 | ETComplex128) as ct -> ct
              | _ -> IRTScalar ETFloat64)
         // A cast's type is its target, whatever the operand resolved to.
         | IRCast et -> IRTScalar et)
    | IRTuple exprs -> IRTTuple (exprs |> List.map typeOf)
    | IRFma _ -> IRTScalar ETFloat64
    | IRComplex (re, _) ->
        // Complex type derived from component width: Float32 -> Complex64,
        // Float64 -> Complex128. Reports as a scalar (NOT a tuple) -- that's
        // the whole point of having a separate IRComplex node.
        (match typeOf re with
         | IRTScalar ETFloat32 -> IRTScalar ETComplex64
         | _ -> IRTScalar ETComplex128)
    | IRTupleProj (e, i, isFlat) ->
        let parentTy = typeOf e
        if isFlat then
            let leaves = flattenTupleLeaves parentTy
            if i < leaves.Length then leaves.[i] else IRTUnit
        else
            (match parentTy with
             | IRTTuple ts when i < ts.Length -> ts.[i]
             | _ -> IRTUnit)
    | IRMatch (_, []) -> IRTUnit
    | IRIndex (arr, indices, _) ->
        // Indexing peels dimensions; full indexing yields the element type.
        //
        // Compound-head partial indexing is the exception to positional
        // peeling: a rank-k compound is ONE slot filled by ONE tuple, and a
        // PARTIAL tuple (short prefix, or full-arity with wildcard sentinels)
        // REPLACES that slot with a residual fragment rather than consuming
        // it (mirroring TypeCheck's compoundResidualType). Without this
        // branch a partial read reports the element type, breaking chained/
        // inline consumers at codegen.
        (match typeOf arr with
         | ArrayElem arrTy ->
             let headTabulated =
                 match arrTy.IndexTypes with
                 | h :: _ when (h.IxKind = IxKCompound || h.IxKind = IxKSparse) -> Some h
                 | _ -> None
             (match headTabulated, indices with
              | Some h, (IRTuple coords) :: trailingIdxs ->
                  (match classifyCompoundIndexTuple h.Rank coords with
                   | CompoundPartial (pinned, freePos) ->
                       let rr = freePos.Length
                       // Residual keeps the PARENT's kind (a partially indexed
                       // sparse is a sparse), mirroring tabulatedResidualType.
                       let residualTag, residualKind =
                           match h.IxKind with
                           | IxKSparse -> Some "__sparseidx", IxKSparse
                           | _ -> Some "__compoundidx", IxKCompound
                       let residual =
                           if rr = 1 then
                               { Id = synthSlotIdCompoundResidual; Rank = 1
                                 Extent = IRCompoundProject (arr, pinned.Length)
                                 Symmetry = SymNone; Tag = None; IxKind = IxKPlain
                                 Kind = SDimension; Dependencies = [] }
                           else
                               { Id = synthSlotIdCompoundResidual; Rank = rr
                                 Extent = IRCompoundProject (arr, pinned.Length)
                                 Symmetry = SymNone; Tag = residualTag; IxKind = residualKind
                                 Kind = SDimension; Dependencies = [] }
                       let trailingSlots = List.tail arrTy.IndexTypes
                       let trailingRemaining =
                           if trailingIdxs.Length <= trailingSlots.Length
                           then trailingSlots |> List.skip trailingIdxs.Length
                           else []
                       mkArrayLike { arrTy with IndexTypes = residual :: trailingRemaining }
                   | CompoundFull ->
                       // Full tuple consumes the one compound slot; any further
                       // indices consume trailing slots positionally (the
                       // pre-existing rule below already counts them right).
                       if indices.Length >= arrTy.IndexTypes.Length then arrTy.ElemType
                       else mkArrayLike { arrTy with IndexTypes = arrTy.IndexTypes |> List.skip indices.Length })
              | _ ->
                  if indices.Length >= arrTy.IndexTypes.Length then arrTy.ElemType
                  else mkArrayLike { arrTy with IndexTypes = arrTy.IndexTypes |> List.skip indices.Length })
         | t -> t)
    | IRSequence exprs ->
        (match exprs with
         | [] -> IRTUnit
         | _ ->
             // Sequence produces array with Idx<N> over element type
             let elemType = typeOf (List.head exprs)
             (match elemType with
              | ArrayElem arr ->
                  // Array elements: prepend sequence dimension
                  let seqIdx = { Id = 0; Rank = 1; Extent = IRLit (IRLitInt (int64 exprs.Length)); Symmetry = SymNone; Tag = Some "__seq"; IxKind = IxKSeq; Kind = SDimension; Dependencies = [] }
                  mkArrayLike { arr with IndexTypes = seqIdx :: arr.IndexTypes }
              | IRTScalar et ->
                  // Scalar elements: simple array
                  let seqIdx = { Id = 0; Rank = 1; Extent = IRLit (IRLitInt (int64 exprs.Length)); Symmetry = SymNone; Tag = Some "__seq"; IxKind = IxKSeq; Kind = SDimension; Dependencies = [] }
                  mkArrayArrow [seqIdx] (IRTScalar et) None
              | _ -> elemType))
    | IRAssign _ -> IRTUnit
    | IRForRange _ -> IRTUnit
    | IRConstraintCheck _ -> IRTUnit
    | IRBreakIf _ -> IRTUnit
    | IRFieldAccess (obj, field) ->
        // Resolved via the ONE struct-fields cache (structFieldsCacheStorage
        // above), populated both at liftInlineFormsModule entry and at
        // codegen module entry from the same module's Types -- collapsing the
        // duplicate codegen-side cache that audit section 2.4 flagged as a
        // valid-but-wrong-lookup hazard.
        (match tryLookupFieldType (typeOf obj) field with
         | Some ty -> ty
         | None -> IRTUnit)
    | IRFunctorMap (f, c) ->
        // f <$> c: return type is f's return type
        (match typeOf f with
         | FuncElem (_, retTy) -> retTy
         | _ -> typeOf c)  // fallback: preserve computation type
    | IRBind (_, cont) ->
        // >>= : result type is continuation's return type
        (match typeOf cont with
         | FuncElem (_, retTy) -> retTy
         | t -> t)
    | IRParallel (l, r, _) -> IRTTuple [typeOf l; typeOf r]
    | IRFusion (l, r) -> IRTTuple [typeOf l; typeOf r]
    | IRMask (arr, _) ->
        // Bool presence array over the source's own index space (verbatim
        // records -- index-space identity feeds compound()).
        (match typeOf arr with
         | ArrayElem a -> mkArrayLike { a with ElemType = IRTScalar ETBool }
         | t -> t)
    | IRContains _ -> IRTScalar ETBool  // Membership returns bool
    | IRDisplayEmit _ -> IRTScalar ETBool  // display.emit always answers true
    | IRDisplayJson _ | IRDisplayNum _ | IRDisplayStr _ -> IRTScalar ETString  // JSON text
    | IRGroupBy (v, gk) ->
        // TypeCheck's `ExprGroupBy` rule constructs a rank-2 array type with
        // `__group_outer` + `__group_member` tagged index slots. For
        // let-bound group_by results (the only currently-allowed usage;
        // inline group_by in method_for() is rejected at codegen entry) the
        // binding's Type field carries this form, and `IRVar` lookups return
        // it correctly. This branch fires when an IRGroupBy node is
        // consulted directly (lifted bindings, future inline support):
        // reconstruct the same rank-2 form so shape-matching consumers see
        // the correct structure (see `synthSlotId*` above).
        let valsTy = typeOf v
        let gkTy = typeOf gk
        (match gkTy, valsTy with
         | IRTGroupKeys (outerIdx, _, _), ArrayElem valsArr ->
             let outer = { outerIdx with Id = synthSlotIdOuter; Tag = Some "__group_outer"; IxKind = IxKGroupOuter }
             let memberIdx = {
                 Id = synthSlotIdMember
                 Rank = 1
                 Extent = IRParam ("__groupsz", 0, IRTNat None)
                 Symmetry = SymNone
                 Tag = Some "__group_member"; IxKind = IxKGroupMember
                 Kind = SDimension
                 Dependencies = []
             }
             mkArrayArrow [outer; memberIdx] valsArr.ElemType None
         | _ ->
             // Fallback: gk isn't IRTGroupKeys-typed yet or v isn't an
             // array. Returning vals's type preserves the prior placeholder
             // behavior -- same shape, same element type -- so any caller
             // that was previously satisfied stays satisfied.
             valsTy)
    | IRGroupKeys _ -> IRTUnit  // GroupKeys is an opaque structure, not a runtime value with a simple type
    | IRSegments _ -> IRTUnit   // the structural grouping: same opaque sentinel as group_keys
    | IRSegmentsGrid _ -> IRTUnit
    | IRUngroupGrid (g, srcs, _) ->
        (match typeOf g with
         | IRTArrow (slots, res, x) when slots.Length >= 2 -> IRTArrow ((srcs |> List.map SIdx) @ List.skip 2 slots, res, x)
         | other -> other)
    | IRUngroupRows (rows, _, src) ->
        // Each row is [file axis; rest...] over elem; the result is
        // [source; rest...] over elem.
        (match rows with
         | r :: _ ->
             (match typeOf r with
              | IRTArrow (slots, res, x) when not slots.IsEmpty -> IRTArrow (SIdx src :: List.tail slots, res, x)
              | other -> other)
         | [] -> IRTUnit)
    | IRUngroup (g, src) ->
        // The grouped operand is [outer; member; rest...] over elem; the
        // result is [source; rest...] over elem.
        (match typeOf g with
         | IRTArrow (slots, res, x) when slots.Length >= 2 -> IRTArrow (SIdx src :: List.skip 2 slots, res, x)
         | other -> other)
    | IRGroupBucket gk ->
        // Rank-1 Int64 over the grouping's SOURCE index space -- the same slot
        // the key array was indexed by, so `bucket` co-iterates with the values
        // it partitions. Reconstructed here for the same reason IRGroupBy is:
        // a lifted node consulted without its binding's Type field.
        (match typeOf gk with
         | IRTGroupKeys (_, sourceIdx, _) -> mkArrayArrow [sourceIdx] (IRTScalar ETInt64) None
         | _ -> IRTScalar ETInt64)
    | IRGroupSizes gk ->
        // Rank-1 Int64 over the GROUP axis -- the same slot group_by's outer
        // dimension carries, so sizes line up with any per-group aggregate.
        (match typeOf gk with
         | IRTGroupKeys (outerIdx, _, _) ->
             let outer = { outerIdx with Tag = Some "__group_outer"; IxKind = IxKGroupOuter }
             mkArrayArrow [outer] (IRTScalar ETInt64) None
         | _ -> IRTScalar ETInt64)
    | IRTranspose (arr, d1, d2) ->
        // Swap the two index slots. (TypeCheck has already verified both axes
        // are arity-1 SymNone, so dim index == slot index here.)
        (match typeOf arr with
         | ArrayElem a when d1 < a.IndexTypes.Length && d2 < a.IndexTypes.Length ->
            let swapped =
                a.IndexTypes
                |> List.mapi (fun i ix ->
                    if i = d1 then a.IndexTypes.[d2]
                    elif i = d2 then a.IndexTypes.[d1]
                    else ix)
            mkArrayLike { a with IndexTypes = swapped }
         | t -> t)
    | IRDecompact (arr, d) ->
        // Split the compact slot containing dim d: left-remainder / extracted
        // Idx / right-remainder. Shape only (codegen reads arity/symmetry off
        // this); Ids reused -- authoritative nominal type is set by TypeCheck.
        (match typeOf arr with
         // FULL decompaction of a wreath class, ahead of the dimension walk:
         // `d` is the number of LEVELS TO KEEP (docs/plan-orbidx-decompaction.md
         // section 4.3), and TypeCheck's inferDecompact has already refused
         // every d except 0. The shape is the dense rank-prod(ri) tensor: one
         // plain Idx<n> axis per raw axis, extent the class's BASE extent
         // (reading `ix.Extent` directly would put the IROrbitClass marker
         // itself on a dense axis). Built from scratch, not `{ ix with ... }`:
         // IxKOrbit, the "__orbidx" Tag, and the IROrbitClass extent are all
         // wreath-specific and validator-enforced together.
         | ArrayElem a when (match a.IndexTypes with
                             | [ ix ] -> ix.Symmetry = SymWreath
                             | _ -> false) ->
            let ix = List.head a.IndexTypes
            let axes = max 1 ix.Rank
            let baseExtent = orbitBaseExtent ix
            let denseAxis =
                { Id = ix.Id; Rank = 1; Extent = baseExtent
                  Symmetry = SymNone; Tag = None; IxKind = IxKPlain
                  Kind = SDimension; Dependencies = [] }
            mkArrayLike { a with IndexTypes = List.replicate axes denseAxis }
         | ArrayElem a ->
            let rec walk slotIdx acc remaining =
                match remaining with
                | [] -> None
                | (ix: IRIndexType) :: rest ->
                    let ar = max 1 ix.Rank
                    if d < acc + ar then Some (slotIdx, ar, d - acc, ix)
                    else walk (slotIdx + 1) (acc + ar) rest
            (match walk 0 0 a.IndexTypes with
             // A wreath COMBINED with other slots is excluded rather than
             // refused: `typeOf` is a pure shape reconstruction with no error
             // channel, and TypeCheck's inferDecompact already refuses that
             // arrangement with the real diagnostic. Excluding it means that
             // if one somehow arrived here, this returns the array's shape
             // UNCHANGED instead of a `{ ix with Rank = ar }` whose Rank no
             // longer matches its level list (internally inconsistent but
             // looking well formed).
             | Some (slot, r, posInSlot, ix) when r >= 2 && ix.Symmetry <> SymNone
                                                  && ix.Symmetry <> SymWreath ->
                let mkRemainder (ar: int) : IRIndexType list =
                    if ar <= 0 then []
                    elif ar = 1 then [ { ix with Rank = 1; Symmetry = SymNone } ]
                    else [ { ix with Rank = ar } ]
                let extracted = { ix with Rank = 1; Symmetry = SymNone }
                let replacement = mkRemainder posInSlot @ [extracted] @ mkRemainder (r - 1 - posInSlot)
                let newIdx =
                    a.IndexTypes
                    |> List.mapi (fun i s -> (i, s))
                    |> List.collect (fun (i, s) -> if i = slot then replacement else [s])
                mkArrayLike { a with IndexTypes = newIdx }
             | _ -> mkArrayLike a)
         | t -> t)
    | IRGram (l, r, sameArray) ->
        // gram(A, B) = A * B^H. A : m x n, B : p x n -> m x p. Element type is
        // complex iff either operand is complex. Same-array -> square m x m,
        // compact group of arity 2 (Hermitian if complex, else symmetric);
        // distinct -> dense m x p (two plain axes).
        (match typeOf l, typeOf r with
         | ArrayElem la, ArrayElem ra when la.IndexTypes.Length >= 1 && ra.IndexTypes.Length >= 1 ->
            let isComplexElem (t: IRType) =
                match stripUnits t with IRTScalar (ETComplex64 | ETComplex128) -> true | _ -> false
            let outBare =
                if isComplexElem la.ElemType then stripUnits la.ElemType
                elif isComplexElem ra.ElemType then stripUnits ra.ElemType
                else stripUnits la.ElemType
            // Twin of inferGram's join: the contraction is multiplicative, so
            // units multiply (nominal dropped one-sided) and complex is
            // detected under the unit wrapper.
            let outElem =
                match getUnits la.ElemType, getUnits ra.ElemType with
                | Some lu, Some ru -> IRTUnitAnnotated (outBare, unitMul lu ru)
                | Some u, None | None, Some u -> IRTUnitAnnotated (outBare, { u with Nominal = None })
                | None, None -> outBare
            let mOuter = la.IndexTypes.[0]
            let pOuter = ra.IndexTypes.[0]
            if sameArray then
                let sym = if isComplexElem outBare then SymHermitian else SymSymmetric
                let grp = { mOuter with Rank = 2; Symmetry = sym }
                mkArrayLike { la with ElemType = outElem; IndexTypes = [grp] }
            else
                let s0 = { mOuter with Rank = 1; Symmetry = SymNone }
                let s1 = { pOuter with Rank = 1; Symmetry = SymNone }
                mkArrayLike { la with ElemType = outElem; IndexTypes = [s0; s1] }
         | t, _ -> t)
    | IRGramApply (l, r, x) ->
        // gram_apply(A, B, x) = A * (B^H * x). A : m x n, B : p x n, x : p ->
        // y : m, one plain axis with A's leading extent. Element type complex
        // iff any operand is; units multiply through both contractions (twin
        // of inferGramApply's join).
        (match typeOf l, typeOf r, typeOf x with
         | ArrayElem la, ArrayElem ra, ArrayElem xa when la.IndexTypes.Length >= 1 ->
            let isComplexElem (t: IRType) =
                match stripUnits t with IRTScalar (ETComplex64 | ETComplex128) -> true | _ -> false
            let outBare =
                [ la.ElemType; ra.ElemType; xa.ElemType ]
                |> List.tryFind isComplexElem
                |> Option.map stripUnits
                |> Option.defaultValue (stripUnits la.ElemType)
            let mulU a b =
                match a, b with
                | Some lu, Some ru -> Some (unitMul lu ru)
                | Some u, None | None, Some u -> Some { u with Nominal = None }
                | None, None -> None
            let outElem =
                match mulU (mulU (getUnits la.ElemType) (getUnits ra.ElemType)) (getUnits xa.ElemType) with
                | Some u -> IRTUnitAnnotated (outBare, u)
                | None -> outBare
            let s0 = { la.IndexTypes.[0] with Rank = 1; Symmetry = SymNone }
            mkArrayLike { la with ElemType = outElem; IndexTypes = [s0] }
         | t, _, _ -> t)
    | IRMatmul (l, r) ->
        // matmul(A, B). A : m x k, B : k x n -> DENSE m x n (two plain axes,
        // SymNone). No conjugation and no symmetry claim: unlike gram, matmul
        // over the same array is not symmetric, so there is no same-array mode.
        // Element type is the left operand's (the checker requires both real
        // Float64 -- the shim's current domain).
        (match typeOf l, typeOf r with
         | ArrayElem la, ArrayElem ra when la.IndexTypes.Length >= 1 && ra.IndexTypes.Length >= 1 ->
            let mOuter = la.IndexTypes.[0]
            let nOuter = List.last ra.IndexTypes
            let s0 = { mOuter with Rank = 1; Symmetry = SymNone }
            let s1 = { nOuter with Rank = 1; Symmetry = SymNone }
            mkArrayLike { la with IndexTypes = [s0; s1] }
         | t, _ -> t)
    | IREigh operand ->
        // eigh(S) -> (Q : n x n dense, LAM : n dense).
        //
        // THE MIXED-ELEMENT TUPLE: Q inherits the operand's element type, but
        // LAM does NOT (eigenvalues of a symmetric/Hermitian matrix are REAL,
        // so Complex128 yields `(Array<Complex128,2>, Array<Float64,1>)`) --
        // matching `blade_lapack`'s signatures (`std::complex<double>** V`
        // beside `double* lam`); getting it wrong would be a silent
        // storage-width error rather than a type error.
        //
        // The operand is rank-2 square in EITHER admissible spelling (ONE
        // compact slot of arity 2, or TWO plain dense axes); both give n from
        // the first slot's extent. Ids here are cosmetic -- the authoritative
        // result type is the one `inferEigh` built and lowering attached.
        (match typeOf operand with
         | ArrayElem sa when not sa.IndexTypes.IsEmpty ->
            let ix0 = sa.IndexTypes.Head
            let axis = { ix0 with Rank = 1; Symmetry = SymNone; IxKind = IxKPlain; Dependencies = [] }
            let lamElem =
                match sa.ElemType with
                | IRTScalar ETComplex128 -> IRTScalar ETFloat64
                | IRTScalar ETComplex64 -> IRTScalar ETFloat32
                | t -> t
            let qTy = mkArrayLike { sa with IndexTypes = [axis; axis] }
            let lamTy = mkArrayLike { sa with ElemType = lamElem; IndexTypes = [axis] }
            IRTTuple [qTy; lamTy]
         | t -> t)
    | IRSolve (matrix, _) ->
        // solve(A, b) -> x : DENSE rank-1 of A's leading extent, element type
        // A's. Taken from A rather than from b so the carried type names the
        // same `n` the emitted loops and the shim both iterate; `inferSolve`
        // has already required the two to agree wherever that is statically
        // decidable. The id is cosmetic -- the authoritative result type is the
        // one `inferSolve` built and lowering attached.
        (match typeOf matrix with
         | ArrayElem aa when not aa.IndexTypes.IsEmpty ->
            let axis = { aa.IndexTypes.Head with Rank = 1; Symmetry = SymNone; IxKind = IxKPlain; Dependencies = [] }
            mkArrayLike { aa with IndexTypes = [axis] }
         | t -> t)
    | IRLu matrix ->
        // lu(A) -> (LU : n x n dense Float64, piv : n dense Int64). Both
        // extents are A's leading one; ids cosmetic (`inferLu` built the
        // authoritative type).
        (match typeOf matrix with
         | ArrayElem aa when not aa.IndexTypes.IsEmpty ->
            let axis = { aa.IndexTypes.Head with Rank = 1; Symmetry = SymNone; IxKind = IxKPlain; Dependencies = [] }
            IRTTuple [ mkArrayLike { aa with IndexTypes = [axis; axis] }
                       mkArrayLike { aa with ElemType = IRTScalar ETInt64; IndexTypes = [axis] } ]
         | t -> t)
    | IRLuSolve (lu, _, _, _) ->
        // lu_solve(LU, piv, b) -> x : dense rank-1 of LU's leading extent,
        // Float64 (the solve twin's rule, from the factor).
        (match typeOf lu with
         | ArrayElem aa when not aa.IndexTypes.IsEmpty ->
            let axis = { aa.IndexTypes.Head with Rank = 1; Symmetry = SymNone; IxKind = IxKPlain; Dependencies = [] }
            mkArrayLike { aa with IndexTypes = [axis] }
         | t -> t)
    | IRHaloUnhash _ ->
        // A window neighbor read yields the inner index's coordinate: int64.
        IRTScalar ETInt64
    | IRReduce (arr, _, _) ->
        // Reduces innermost dim by 1. For rank-1 input, result is a scalar.
        (match typeOf arr with
         | ArrayElem a when a.IndexTypes.Length = 1 -> a.ElemType  // IRType already
         | ArrayElem a ->
             // Multi-rank reduction: drop innermost index. (Not yet supported by
             // codegen; TypeCheck rejects rank>1 today, but keep this consistent.)
             mkArrayLike { a with IndexTypes = a.IndexTypes |> List.take (a.IndexTypes.Length - 1) }
         | t -> t)
    | IRReduceCompute (comp, _, seed) ->
        // Fused reduction terminal: one scalar per fusion leaf. The seed
        // carries the accumulator type (checker-unified with every leaf's
        // element type); the result mirrors the tree's nested-pair shape.
        //
        // JOIN ENCODING: a reduction join seeds each leg separately, so `seed`
        // is an `IRTuple` of k seeds in leaf order and each leaf takes its OWN
        // one. A shared-fold terminal keeps one seed for every leaf.
        match seed with
        | IRTuple seeds when seeds.Length >= 2 ->
            // A JOIN answers a FLAT Tuple<k>, not the chain's nested pairs.
            // `<&!>` between two maps is a binary operator, so its result
            // nests; a join is k-ary by construction and its VALUE is one flat
            // `make_tuple` of k accumulators. Nesting the TYPE over a flat
            // value is what makes a nested `std::get` chain project the wrong
            // slot (and, in a kernel body, not compile at all) -- so the join
            // types flat, and every projection is `get<i>`.
            IRTTuple (seeds |> List.map typeOf)
        | _ ->
            let rec shape e =
                match e with
                | IRFusion (l, r) -> IRTTuple [shape l; shape r]
                | _ -> typeOf seed
            shape comp
    | IRProdSum args ->
        // Scalar: the fused fold of rank-1 operands (TypeCheck enforces rank 1).
        (match args with
         | first :: _ ->
             (match typeOf first with
              | ArrayElem a -> a.ElemType
              | t -> t)
         | [] -> IRTScalar ETFloat64)

    // -- Rank-changing assembly combinators (formalism 2.6) ---------------
    // `stack(A1..An)` adds a fresh LEADING axis of extent n over the operands'
    // (identical -- TypeCheck enforces it) shape; `join(A1..An, d)` keeps the
    // shape and SUMS the extents on axis d. Both used to sit in the untyped
    // list below, with each consumer that needed an element type reaching PAST
    // the node to its first operand instead (CodeGen's `inferElemTypeStrict`
    // has a two-line arm doing exactly that, and says so).
    //
    // That was survivable while every consumer reached past. It stopped being
    // survivable once `isStatementShapedArraysArg` -- the ONE predicate behind
    // the function-body, RETURN and loop-operand hoisting routes -- gated on
    // the node being ARRAY-TYPED. `stack(x, y) * 2.0` failed that test, so the
    // stack was left inline in the broadcast's operand slot and the emitted
    // nest read an `arr0` nothing had declared (module scope and function body
    // alike). Reaching past the node cannot fix that: the hoist needs a type
    // for the NAME it mints, which is the operand type plus a rank change.
    //
    // This mirrors `TypeCheck.inferStack` / `inferJoin` arithmetic-for-
    // arithmetic. It is not a second check: by the time IR exists those two
    // have already REFUSED non-array operands, rank disagreement and off-axis
    // extent clashes, so every shape reaching here is validated. The
    // synthesized axis carries Id 0 -- an IR-level axis has no source-level
    // identity to preserve, and its consumers read Extent / IxKind / Symmetry,
    // never the id. An operand `typeOf` cannot reconstruct leaves the node
    // untyped, exactly as before.
    | IRStack (first :: _ as es) ->
        (match typeOf first with
         | ArrayElem a ->
             let leadIdx =
                 { Id = 0; Rank = 1; Extent = IRLit (IRLitInt (int64 es.Length))
                   Symmetry = SymNone; Tag = None; IxKind = IxKPlain
                   Kind = SDimension; Dependencies = [] }
             mkArrayArrow (leadIdx :: a.IndexTypes) a.ElemType None
         | _ -> IRTUnit)
    | IRJoin ((first :: _ as es), dim) ->
        (match typeOf first with
         | ArrayElem a when dim >= 0 && dim < a.IndexTypes.Length ->
             let dimExtents =
                 es |> List.map (fun e ->
                     match typeOf e with
                     | ArrayElem at when dim < at.IndexTypes.Length -> Some at.IndexTypes.[dim].Extent
                     | _ -> None)
             // The literal sum when every operand's axis extent is static (the
             // common case, and what pins depend on); the runtime addition
             // chain otherwise. Same two-branch rule as `inferJoin`.
             let joinedExtent =
                 match dimExtents |> List.map (function Some (IRLit (IRLitInt n)) -> Some n | _ -> None) with
                 | statics when statics |> List.forall Option.isSome ->
                     IRLit (IRLitInt (statics |> List.sumBy Option.get))
                 | _ ->
                     match dimExtents |> List.choose id with
                     | [] -> a.IndexTypes.[dim].Extent
                     | xs -> xs |> List.reduce (fun l r -> IRBinOp (IRElementwise, IRAdd, l, r))
             let joined = { a.IndexTypes.[dim] with Extent = joinedExtent; Tag = None }
             mkArrayArrow
                 (a.IndexTypes |> List.mapi (fun d ix -> if d = dim then joined else ix))
                 a.ElemType None
         | _ -> IRTUnit)

    // -- Deliberately untyped (loop objects, combinator/emission-internal
    //    markers -- not runtime values with a simple type). Enumerated with
    //    no wildcard so a NEW variant demands a typing decision here.
    | IRMethodFor _ | IRObjectFor _ | IRReynolds _ | IRArrayProduct _
    | IRComposeObj _ | IRCompose _
    | IRSlice _ | IRCurry _ | IRSubset _ | IRShift _ | IRReverse _ | IRDiag _
    | IRZip _ | IRAlign _ | IRStack [] | IRJoin ([], _)
    | IRTupleCons _ | IRTupleDecons _ | IRPolyIndex _ | IRPolyTail _ | IRReplicate _
    | IRVirtualReverse _ | IRZero ->
        IRTUnit

    // -- Coverage tail ---------------------------------------------------
    // Every constructor below is already handled by a family pattern above
    // (partial active patterns are invisible to the exhaustiveness checker).
    // Listing them keeps this match provably exhaustive WITHOUT a wildcard --
    // a brand-new IRExpr variant still fails to compile until it gets a
    // typing rule -- and if one of these arms ever fires, a family pattern
    // was edited out of sync: fail loudly, never mistype.
    | IRVar _ | IRParam _ | IRApp _ | IRArrayLit _ | IRStructLit _
    | IRApplyCombinator _ | IRComposeApply _ | IRLit _ ->
        unreachableTyping "CarriedType" expr
    | IRSort _ | IRArrayNegate _ | IRArrayConjugate _ | IRIntersect _
    | IRUnion _ | IRUnique _ | IRCompute _ | IRPure _ | IRLet _ | IRIf _
    | IRGuard _ | IRChoice _ | IRFallback _ | IRComposeMeth _ | IRMatch _ ->
        unreachableTyping "TypeVia" expr
    | IRArity _ | IRNth | IRRank _ | IRExtent _ | IRRaggedLookup _
    | IRCompoundMask _ | IRCompoundProject _ | IRSparseKeys _ | IROrbitClass _
    | IROpaqueExtent | IRRange _ ->
        unreachableTyping "IntValued" expr

/// Get the type of an IRExpr where the node itself determines it: the whole of
/// the `CarriedType` tier, plus the few VIEW and WRAPPER forms whose type is a
/// structural function of a child that is itself known this way. Used at HM
/// call sites to extract argument types for unification against parameter
/// types.
///
/// Deliberately NOT `typeOf`. A full reconstruction climbs through nodes whose
/// type can still be pre-substitution or environment-dependent, and a
/// confidently WRONG type here mints a wrong specialization -- worse than no
/// specialization. What every arm below has in common is that its LEAVES still
/// come from `CarriedType`: an arm answers None the moment a child does,
/// rather than inventing a partial type. Anything else returns None, and the
/// call site falls back to the callee's declared type-var positions.
///
/// Sits after `typeOf` purely so the view arms can DELEGATE their structural
/// rule to it instead of restating it -- a second hand-maintained peel rule is
/// exactly the silent-divergence hazard the canonical-typing section exists to
/// prevent.
let rec exprTypeIfKnown (expr: IRExpr) : IRType option =
    match expr with
    | CarriedType ty -> Some ty
    // A TUPLE LITERAL carries no type of its own -- `IRTuple` is a bare
    // component list -- so an HM call site whose argument is written
    // `f((a, b))` learned NOTHING about the parameter's component type vars.
    // With `f(st: Tuple<T^1, U^1>)` that leaves T and U unbound, `paramVarsCovered`
    // false, no specialization generated, and the (dropped) HM original's call
    // site left dangling: `blade check` passes and IR validation then reports
    // BL6001 "unresolved type variable" plus a dangling VarId. Measured on
    // `functions/059`'s fully-abstract spelling, `sql-group-by/029`'s
    // `rowdotT`, and `examples/lswosa.blade`'s `hanning`/`wosa_lsdft`.
    //
    // Recursing COMPONENTWISE keeps the node-carried discipline this function
    // exists to enforce: every leaf type still comes from `CarriedType`, and a
    // tuple with even one unknown component still answers None rather than
    // inventing a partial type.
    | IRTuple es ->
        let comps = es |> List.map exprTypeIfKnown
        if comps |> List.forall Option.isSome then
            Some (IRTTuple (comps |> List.map Option.get))
        else None
    // COMPUTATION WRAPPERS erase at the type level (they are `TypeVia`
    // pass-throughs): `xs |> compute` materializes a deferred computation
    // without changing WHAT it computes. Written inline in an argument slot --
    // `plot.heatmap(x, y, (method_for(range<Y, X>) <@> k) |> compute)` -- the
    // wrapper hid an `IRApplyCombinator`, which carries its `OutputType`
    // perfectly well; the callee's element var went unlearned and every
    // generic helper BEHIND the call then failed to specialize, so the program
    // died in a BL6001 spray naming stdlib helpers it never mentions. Binding
    // the pipeline to a `let` first was the only spelling that worked, for the
    // uninteresting reason that a binding reference is an `IRVar`.
    | IRCompute inner | IRPure inner -> exprTypeIfKnown inner
    // A PARTIAL INDEX -- the row view `R(36)` on an `Array<T like Scan, Y, X>`
    // -- carries no type either; peeling the indexed dimensions off the head's
    // type is what gives it one, and that peel is `typeOf`'s rule, not a fresh
    // one (compound and sparse heads take a residual-slot rule rather than
    // positional peeling). Requiring the head to be a KNOWN ARRAY keeps the
    // discipline and keeps `typeOf` on its reconstructing path: it re-derives
    // the very head type this guard just accepted.
    //
    // The sibling view forms -- IRSlice, IRCurry, IRSubset -- must NOT be
    // routed here: `typeOf` leaves them deliberately untyped (IRTUnit), which
    // is precisely the wrong-but-concrete answer this function exists to
    // refuse.
    | IRIndex (arr, _, _) ->
        (match exprTypeIfKnown arr with
         | Some (ArrayElem _) -> Some (typeOf expr)
         | _ -> None)
    | _ -> None
