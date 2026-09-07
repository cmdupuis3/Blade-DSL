// The typed AST resulting from type checking. Every expression node carries
// its inferred type, enabling IDE hover info, incremental type checking, and
// a clean separation of type inference from IR lowering. Mirrors Ast's
// structure but with type annotations and pre-computed symmetry information
// for combinator applications.

module Blade.TypedAst

open Blade.Ast
open Blade.IR
open Blade.Types

// Span and noSpan are inherited from Blade.Ast via 'open'.

type TypedVarInfo = {
    Name: string
    Type: IRType
    Identity: ArrayIdentity option
    IsMutable: bool
    VarId: IRId
}

type TypedParam = {
    Name: string
    Type: IRType
    Index: int
    VarId: IRId
    /// Typed default value expression, checked at the lambda declaration with
    /// the params in scope (it references required params by their VarIds).
    /// Consumed by the kernel-apply seam, which binds an omitted trailing
    /// param as a body-entry TExprLet over this expr; direct calls re-type
    /// the SURFACE default at the call site instead. None on required params
    /// and every synthesized param.
    Default: TypedExpr option
    /// Source span of the parameter's NAME TOKEN, carried through from
    /// Ast.ParamDecl/LambdaParam so `references[]` can point go-to-definition
    /// at it. `noSpan` on synthesized params (the kernels elaborators invent).
    NameSpan: Span
}

// (`and`, not `type`: TypedParam.Default references TypedExpr, so it joins
// the TypedLambdaInfo/TypedExpr recursive group.)
and TypedLambdaInfo = {
    Params: TypedParam list
    Body: TypedExpr
    ReturnType: IRType
    CommGroups: int list list
    /// `where anticomm(...)` groups, by parameter index: the signed twin of
    /// CommGroups. Licensed output storage is the strict simplex (AntisymIdx<r,
    /// n>: no diagonal, negate on swapped reads), not the inclusive triangle.
    /// Kept separate from CommGroups so validators can distinguish a declared
    /// comm on a PNeg body from a declared anticomm on a PInv body.
    AntisymGroups: int list list
    /// Per-parameter sign parity of the body (`Types.KernelSignParity`, decl
    /// order), feeding `IRLoopStructure.deduceWreathTie`'s soundness gate. Populated only
    /// when this lambda reaches the seam as a kernel; other construction sites
    /// leave it empty (read as all-unknown). Lowering copies it onto the
    /// lifted `IRCallable` so codegen and the interpreter agree on the tie.
    SignParities: KernelSignParity list
    Captures: TypedVarInfo list
    IsCommutative: bool
    // Parallelization strategy assignments (see WhereClause.Parallel),
    // propagated from the lambda's where-clause. Today 0 or 1 element.
    Parallel: ParallelStrategy list
    // Self-binding for a named, recursive lambda: Some (name, id) when this is
    // `let const name = lambda(...)` (including the nested-`function` desugar)
    // and its body refers to itself. Threaded to Lowering so the lifted
    // callable keeps the real name/id and can call itself in emitted C++.
    SelfBinding: (string * IRId) option
}

and TypedMethodForInfo = {
    Arrays: TypedExpr list
    Identities: ArrayIdentity list
    ArrayTypes: IRArrayType list
    SDimsPerArray: int list
    TotalSDims: int
    SharedIndexTypes: IRIndexType list  // For co-iteration: shared iteration records (empty = not co-iteration; multi = product space)
}

and TypedObjectForInfo = {
    Kernel: TypedExpr
    CommGroups: int list list
    InputRanks: int list
    OutputRank: int
}

// Application info for the <@> combinator.
and TypedApplyInfo = {
    Loop: TypedExpr                         // Provenance: TExprMethodFor, TExprObjectFor, or TExprCompose(OpComposeObj,...)
    Kernel: TypedExpr
    Arrays: TypedExpr list                  // The actual array expressions
    Identities: ArrayIdentity list          // Array identity tracking (for symmetry)
    ArrayTypes: IRArrayType list            // Array type info
    SharedIndexTypes: IRIndexType list      // For co-iteration (zip): shared records (empty = not co-iteration)
    SymcomStates: SymcomState list
    TriangularLevels: bool list
    SDimsPerArray: int list
    KernelInputRanks: int list
    KernelOutputRank: int
    KernelTDims: IRIndexType list           // T-dimension index types from kernel return type
    SpeedupFactor: int64
    ReynoldsSpeedup: int64
    HasReynolds: bool
    OutputType: IRType
    IsCoIteration: bool
    /// True when Loop = TExprCompose(OpComposeObj, _, _) (or a TExprVar
    /// resolving to one), which puts input arrays in the Kernel slot rather
    /// than a callable; Lowering routes to IRComposeApply instead of
    /// IRApplyCombinator. Defaults to false for ordinary applies.
    IsComposeApply: bool
}

and TypedStmt =
    | TStmtLet of TypedBinding
    | TStmtAssign of lhs: TypedExpr * rhs: TypedExpr
    | TStmtExpr of TypedExpr
    | TStmtForIn of varName: string * varId: IRId * lo: TypedExpr * hi: TypedExpr * body: TypedStmt list

and TypedExpr = {
    Kind: TypedExprKind
    Type: IRType
    Span: Span
}

and TypedExprKind =
    // Literals
    | TExprLit of Literal
    // Wildcard hole `_` (typed sibling of TPatWild). Carries a hole type so it
    // flows through tuple inference; reaching lowering/codegen unconsumed is an error.
    | TExprWildcard
    
    // Variables and names
    | TExprVar of name: string * varId: IRId * identity: ArrayIdentity option
    | TExprQualified of names: string list
    
    // Binary and unary operations
    | TExprBinOp of BinOpMode * BinOp * TypedExpr * TypedExpr
    | TExprUnaryOp of UnaryOp * TypedExpr
    
    // Function application (also used for array indexing)
    | TExprApp of func: TypedExpr * args: TypedExpr list
    
    // Poly-tuple indexing: args[k]
    | TExprTupleIndex of tuple: TypedExpr * index: TypedExpr

    // Pack tail from cons-destructuring `let head :: tail = pack`; `drop` is
    // the number of leading elements peeled. Lowers to IRPolyTail, expanded
    // into trailing pack params by arity-monomorphization. Only for Poly pack
    // scrutinees (tuples keep the TExprTuple-of-projections desugaring).
    | TExprPolyTail of pack: TypedExpr * drop: int
    
    // Field access
    | TExprField of TypedExpr * field: string * fieldIndex: int
    
    // Lambda
    | TExprLambda of TypedLambdaInfo
    
    // Let binding
    | TExprLet of name: string * varId: IRId * value: TypedExpr * body: TypedExpr
    
    // Match expression
    | TExprMatch of scrutinee: TypedExpr * cases: TypedMatchCase list
    
    // If-then-else
    | TExprIf of cond: TypedExpr * thenBr: TypedExpr * elseBr: TypedExpr
    
    // Tuple construction
    | TExprTuple of TypedExpr list
    
    // Complex literal `complex(re, im)`. Distinct from TExprTuple to preserve
    // scalar nature: runtime layout is two floats (std::complex<double>),
    // typed as scalar IRTScalar ETComplex64/128. Lowering routes to IRLitComplex.
    | TExprComplexLit of re: TypedExpr * im: TypedExpr

    // Fused multiply-add `fma(a, b, c)` = a*b + c with ONE rounding. A
    // dedicated node rather than a binop chain because the fusion is the
    // semantics: an IR pass that saw `a*b` and `+ c` as separate nodes could
    // legally split them, and a split fma is not an fma (TwoProd's error
    // term would come out exactly 0). All three operands Float64; lowers to
    // IRFma, rendered std::fma / Math.FusedMultiplyAdd / llvm.fma.f64.
    | TExprFma of a: TypedExpr * b: TypedExpr * c: TypedExpr
    
    // Array literal
    | TExprArrayLit of elems: TypedExpr list * arrayType: IRArrayType
    
    // Loop constructs
    | TExprMethodFor of TypedMethodForInfo
    | TExprObjectFor of TypedObjectForInfo
    
    // Combinator application (with pre-computed symmetry info)
    | TExprApply of TypedApplyInfo
    
    // Other combinators (simpler, no symmetry analysis needed)
    | TExprBind of TypedExpr * TypedExpr
    | TExprParallel of TypedExpr * TypedExpr
    | TExprFusion of TypedExpr * TypedExpr
    | TExprFunctorMap of func: TypedExpr * comp: TypedExpr
    | TExprChoice of TypedExpr * TypedExpr
    // <|:> allocated-fallback: read left where its storage has the cell,
    // else right. Distinct from TExprChoice (zero-vs-nonzero on VALUES):
    // fallback keys on STORAGE (compound mask bit / dense pointer chain).
    | TExprFallback of TypedExpr * TypedExpr
    | TExprCompose of BinOp * TypedExpr * TypedExpr
    
    // Virtual arrays
    | TExprRange of indexTypes: IRIndexType list
    | TExprDotDot of lo: TypedExpr * hi: TypedExpr
    | TExprReverse of indexType: IRIndexType
    // halo<Inner, [offsets]> has no typed node -- it typechecks to a
    // TExprRange over a "__halowin|"-tagged slot (TypeCheck.haloSlotOf); the
    // per-slot center offset is re-derived from the tag at loop building.
    
    // Zip and stack
    | TExprZip of TypedExpr list
    | TExprStack of TypedExpr list
    | TExprJoin of arrays: TypedExpr list * dim: int
    
    // Special forms
    | TExprPure of TypedExpr
    | TExprCompute of TypedExpr
    | TExprRead of TypedExpr
    // fill_random(mod): internal builtin, random-filled array constructor. Type
    // comes from the binding annotation, so only appears as an annotated
    // let-binding value. Lowering records it in RandomInits; `modulus` is the
    // argument to rand() % modulus.
    | TExprFillRandom of modulus: TypedExpr
    // rand.<fam>(key, params..., shape): internal builtin, deterministic
    // random-array constructor, self-typed from the shape argument. Lowering
    // records (kind, key, pars, weights) in RandomInits. `kind` is the family
    // name ("uniform" | "normal" | "exponential" | "gamma" | "poisson" |
    // "bernoulli" | "beta" | "categorical"); `pars` are the family's runtime
    // Float64 SCALAR parameters in surface order (empty for uniform/normal, one
    // for exponential/poisson/bernoulli, two for gamma/beta) -- ordinary typed
    // expressions, evaluated once per fill, NOT per draw; `dims` are the
    // static extents.
    //
    // `weights` is the SECOND parameter channel, and is `Some` only for
    // `categorical` (whose `pars` is correspondingly empty). It is an
    // ARRAY-valued parameter -- a rank-1 Float64 array -- carried separately
    // from `pars` because it is shaped differently at every stage downstream:
    // codegen passes a pool pointer plus a length where a scalar par passes one
    // `(double)` cast, and the interpreter unwraps a VArray where a scalar par
    // evaluates to a number. Folding it into `pars` would make every consumer
    // re-discover which entries are arrays.
    //
    // The paired int is the weights array's STATIC extent, pinned by the
    // checker off the argument's own type. It rides along rather than being
    // re-read from the TypedExpr's type downstream so that the "is this extent
    // static?" question is asked (and refused) in exactly one place.
    | TExprRandGen of kind: string * key: TypedExpr * pars: TypedExpr list * weights: (TypedExpr * int) option * dims: int list
    | TExprGuard of cond: TypedExpr * body: TypedExpr
    | TExprZero
    | TExprReynolds of kernel: TypedExpr * isAntisymmetric: bool
    
    // Arity special forms
    | TExprArity of paramName: string
    | TExprRank of TypedExpr
    
    // Filtered array
    | TExprMask of array: TypedExpr * pred: TypedExpr
    | TExprCompound of dense: TypedExpr * mask: TypedExpr
    | TExprSparse of values: TypedExpr * keys: TypedExpr
    | TExprIntersect of TypedExpr * TypedExpr
    | TExprUnion of TypedExpr * TypedExpr
    | TExprUnique of array: TypedExpr
    | TExprContains of array: TypedExpr * value: TypedExpr
    /// `display.emit(mime, data[, meta])`: write one display frame to stdout,
    /// evaluate to `true`. Everything except `data` is fixed at elaboration
    /// time (Blade.Display.Elaborate) -- `head` is the frame JSON up to and
    /// including `"data":`, `quoted` says whether the payload goes out as a
    /// quoted JSON string or an inline JSON value, `metaTail` is the user
    /// `meta` object minus its braces. Byte format: Blade.Display.Frame.
    ///
    /// `id` is `None` for `display.emit` -- the frame's `meta.id` is then the
    /// run's `<SessionTag><ordinal>` -- and `Some e` for `display.emit_id`,
    /// whose id is that runtime String instead. One node rather than two
    /// because the two spellings differ in exactly this operand: everything
    /// downstream (typing, lowering, both back ends) would otherwise be
    /// duplicated verbatim.
    | TExprDisplayEmit of head: string * quoted: bool * data: TypedExpr * metaTail: string * id: TypedExpr option
    /// `display.json_array(A)`: render a rank-1 or rank-2 numeric array as
    /// JSON text (String). `rank` is pinned at typecheck so both back ends
    /// pick the 1-D/2-D serializer without re-resolving the type. Number
    /// formatting is the byte-parity 15-significant-digit print rule.
    | TExprDisplayJson of rank: int * data: TypedExpr
    /// `display.json_num(x)`: render a numeric scalar as JSON text (String),
    /// same formatting rule as TExprDisplayJson.
    | TExprDisplayNum of data: TypedExpr
    /// `display.json_string(s)`: render a String as a QUOTED, escaped JSON
    /// string. The escape table is Blade.Display.Frame.escape's, shared with
    /// the quoted-payload path.
    | TExprDisplayStr of data: TypedExpr
    | TExprGroupBy of values: TypedExpr * grouping: TypedExpr
    | TExprGroupKeys of keys: TypedExpr list
    /// group_bucket(gk): row -> bucket over the grouping's source index space.
    | TExprGroupBucket of grouping: TypedExpr
    /// segments(A): the STRUCTURAL grouping of a `Chunked` axis
    /// (docs/plans/structural/07 §2.2) -- a GroupKeys whose partition is
    /// the segment table: `offsets` are the run boundaries `[0; ..; N]`,
    /// `labels` the file labels of a file-segmented axis. No key array,
    /// no CSR: the permutation is the identity.
    | TExprSegments of alias: string * offsets: int64 list * labels: string list option
    /// ungroup(G): the inverse of `group_by(_, segments(A))` -- G's rows
    /// reassembled over the SOURCE axis `source` (§2.3, §3.3).
    | TExprUngroup of grouped: TypedExpr * source: IRIndexType
    /// ungroup([r1, .., rF], A): the per-file arrays of a file-segmented axis
    /// assembled over the axis (§2.7: the inverse of `files(A)` applied to
    /// explicit rows -- how a variable that lives in several stores is
    /// named over the tiled axis). Rows are bare names; `offsets` are the
    /// file boundaries.
    | TExprUngroupRows of rows: TypedExpr list * offsets: int64 list * source: IRIndexType
    | TExprSort of array: TypedExpr * key: TypedExpr
    | TExprReduce of array: TypedExpr * kernel: TypedExpr * init: TypedExpr option
    | TExprProdSum of args: TypedExpr list  // prodsum(x1..xk): fused sum_t prod_l x_l(t) over rank-1 arrays
    | TExprTranspose of array: TypedExpr * dim1: int * dim2: int
    | TExprDecompact of array: TypedExpr * dim: int
    | TExprGram of left: TypedExpr * right: TypedExpr * isSameArray: bool
    /// matmul(A, B): A(m x k) * B(k x n) -> dense m x n, routed through
    /// blade_linalg rather than synthesized as a Blade triple loop.
    | TExprMatmul of left: TypedExpr * right: TypedExpr
    /// eigh(S): symmetric/Hermitian eigendecomposition of a rank-2 square
    /// operand, yielding (Q, LAM): Q's columns the eigenvectors, LAM
    /// descending. Routed through blade_lapack when available at elaboration
    /// time; otherwise `math.eigh` expands to Blade Jacobi source instead.
    | TExprEigh of operand: TypedExpr
    /// solve(A, b): the general dense linear solve A.x = b by partial-pivoted
    /// LU, A rank-2 square and b rank-1 of the matching extent, yielding x.
    /// ALWAYS a first-class node (unlike eigh): the native arm is emitted LU
    /// loops whose operation order the interpreter's `A.solveArray` mirrors, so
    /// the byte-identity differential covers the code an ordinary build runs.
    /// The LAPACK `dgesv` route is an availability-gated replacement for those
    /// loops, not a precondition for the node existing.
    | TExprSolve of matrix: TypedExpr * rhs: TypedExpr
    | TExprArrayNegate of array: TypedExpr
    | TExprArrayConjugate of array: TypedExpr
    | TExprExtents of array: TypedExpr
    
    // Struct construction
    | TExprStruct of typeName: string * fields: (string * TypedExpr) list
    
    // Index expression (array indexing result)
    | TExprIndex of array: TypedExpr * indices: TypedExpr list * identity: ArrayIdentity option
    
    // Block expression (preserves statement structure for IDE support)
    | TExprBlock of stmts: TypedStmt list * finalExpr: TypedExpr option
    
    // Assignment expression
    | TExprAssign of lhs: TypedExpr * rhs: TypedExpr
    
    // Sequence (evaluated in order, result is last)
    | TExprSequence of TypedExpr list
    
    // Replicate
    | TExprReplicate of count: TypedExpr * body: TypedExpr
    
    // Alignment
    | TExprAlign of exprs: TypedExpr list * spec: Ast.AlignSpec option
    
    // Sectioned operator (e.g., (+) becomes a lambda)
    | TExprSection of BinOp

    // Partial application of operator (e.g., (+ 3) or (3 +))
    | TExprPartialApp of op: BinOp * arg: TypedExpr * isLeft: bool

    // Runtime constraint guard: emits `if (!(cond)) { panic(code, message); }`.
    // Synthesized by the checker for mutual-group joint bindings, struct
    // constraint checks, and the rec-array budget abort; not expressible in
    // surface syntax. `code` is the BLxxxx the panic renders (BL8001 for
    // value-constraint guards, BL8010 for the while-guard budget abort).
    | TExprConstraintCheck of cond: TypedExpr * code: string * message: string

    // Early exit from the ENCLOSING for-in loop when cond is true. Synthesized
    // ONLY by inferRecArray for the `while` guard on a recursive array's
    // inductive arm -- never parser-reachable, and only legal in for-in
    // statement position (the emitters refuse it anywhere a C++ `break`
    // could not stand).
    | TExprBreakIf of cond: TypedExpr

and TypedMatchCase = {
    Pattern: TypedPattern
    Guard: TypedExpr option
    Body: TypedExpr
}

and TypedPattern = {
    Kind: TypedPatternKind
    Type: IRType
    Bindings: (string * IRId * IRType) list  // Variables bound by this pattern
}

and TypedPatternKind =
    | TPatWild
    | TPatVar of name: string * varId: IRId
    | TPatLit of Literal
    | TPatTuple of TypedPattern list
    | TPatCons of TypedPattern * TypedPattern
    | TPatVariant of tag: string * payload: TypedPattern option * isEnum: bool
    | TPatStruct of typeName: string * fields: (string * TypedPattern) list
    | TPatGuarded of TypedPattern * TypedExpr

and TypedBinding = {
    Name: string
    VarId: IRId
    Type: IRType
    Identity: ArrayIdentity option
    IsMutable: bool
    Value: TypedExpr
    /// Destructured sub-bindings: (name, varId, type) for PatTuple/PatCons/PatStruct
    SubBindings: (string * IRId * IRType) list
    /// How Lowering derives each SubBindings entry -- without it, `head ::
    /// tail` is indistinguishable from a tuple pattern and miscompiles into
    /// two positional projections. See DestructureShape.
    Destructure: DestructureShape
    /// Constraint guards run right after this binding (mutual-group joint
    /// checks). IRIds are allocated directly after the SubBinding ids, since
    /// module emission is IRId-ordered and later fresh ids would run too late.
    PostChecks: (IRId * TypedExpr) list
}

/// How a TypedBinding's SubBindings relate to its value. Lives on the binding
/// rather than each sub-binding entry because SubBindings' element shape is
/// consumed positionally by Cli.fs/Ide.fs/Zonk.fs for information only
/// Lowering needs.
and DestructureShape =
    /// Sub-binding i is element i of a tuple (or the field with its own name,
    /// for a struct scrutinee). Also the shape when there is no destructuring.
    | DSPositional
    /// Tuple pattern with its slot assignment stated explicitly: sub-binding k
    /// reads slot `components[k]`, and the whole pattern consumes `slots`
    /// slots. Both numbers are needed because a pattern position that binds
    /// NOTHING (a wildcard, a literal) still consumes its slot: deriving the
    /// slot from the sub-binding's list position instead -- which is what
    /// DSPositional means -- compacts every later binder onto the leading
    /// components, so `let (_, g) = f(x)` silently bound g to element 0.
    /// `slots` (not SubBindings.Length) is also what Lowering's flat-vs-
    /// structural test must count, since that test compares the pattern's
    /// width against the scrutinee's flat and structural widths.
    | DSTupleAt of components: int list * slots: int
    /// Cons split over a tuple scrutinee. `::` is right-associative, so a chain
    /// `a :: b :: rest` flattens into leading leaves [a; b] plus one rest leaf.
    /// Every leaf but the last is positional; the last takes the whole
    /// remainder (re-tupled), or the bare element if the remainder is one
    /// element (Blade has no 1-tuple).
    | DSConsRest

and TypedFunctionDecl = {
    Name: string
    FuncId: IRId
    TypeParams: string list
    Params: TypedParam list
    ReturnType: IRType
    WhereClause: WhereClause option
    Body: TypedExpr
    CommGroups: int list list
    IsStatic: bool
    /// Source span of the function's NAME TOKEN (see Ast.FunctionDecl.NameSpan).
    NameSpan: Span
    /// Conservative effect summary of `Body` (Blade.Effects), computed by
    /// checkFunctionDecl with callees resolved through TypeEnv.FuncEffects;
    /// Lowering grafts it onto the IR callable.
    Effects: Blade.Effects.EffectSummary
}

// Typed type definitions, resolved from raw TypeDecl.
and TypedTypeDef =
    | TTDAlias of name: string * typeParams: string list * resolved: IRType
    | TTDStruct of name: string * typeParams: string list * fields: (string * IRType) list
    | TTDVariant of name: string * typeParams: string list * variants: (string * IRType option) list
    /// Index-type alias (`type RegionIdx = Idx<3>`), distinguished from
    /// TTDAlias so codegen emits `using RegionIdx = int64_t;` rather than a
    /// generic IRType alias.
    | TTDIndexType of name: string * idx: IRIndexType
    /// EnumIdx alias (`type LandType = EnumIdx<[101, 205, 307]>`), carrying
    /// the concrete value list for downstream reverse-lookup codegen.
    | TTDEnumIdx of name: string * idx: IRIndexType * values: EnumValue list
    /// Mutually constrained alias group (`type P1 = T1 and P2 = T2 where ...`);
    /// the joint constraint itself lives in TypeEnv.MutualGroups, not here.
    | TTDMutualGroup of members: (string * IRType) list

and TypedImplDecl = {
    ForType: TypeExpr
    TypeName: string
    Methods: TypedFunctionDecl list
}

and TypedDecl =
    | TDeclLet of TypedBinding
    | TDeclFunction of TypedFunctionDecl
    | TDeclType of TypedTypeDef
    | TDeclInterface of InterfaceDecl
    | TDeclImpl of TypedImplDecl
    | TDeclStatic of TypedBinding
    | TDeclUnit of UnitDecl
    | TDeclImport of QualifiedName * ImportStyle

type TypedModule = {
    Name: string list option     // Qualified module name
    Decls: TypedDecl list
}

type TypedProgram = {
    Modules: TypedModule list
}

/// Create a typed expression with a given kind and type
let mkTyped kind ty : TypedExpr = 
    { Kind = kind; Type = ty; Span = noSpan }

/// Create a typed expression with span
let mkTypedSpan kind ty span : TypedExpr = 
    { Kind = kind; Type = ty; Span = span }

/// Create a typed literal
let mkLit lit ty = mkTyped (TExprLit lit) ty

/// Create a typed variable reference
let mkVar name varId identity ty = 
    mkTyped (TExprVar (name, varId, identity)) ty

/// Create a typed binary operation
let mkBinOp mode op left right ty =
    mkTyped (TExprBinOp (mode, op, left, right)) ty

/// Create a typed application
let mkApp func args ty =
    mkTyped (TExprApp (func, args)) ty

/// Create a typed let binding expression
let mkLet name varId value body ty =
    mkTyped (TExprLet (name, varId, value, body)) ty

/// Create a typed if-then-else
let mkIf cond thenBr elseBr ty =
    mkTyped (TExprIf (cond, thenBr, elseBr)) ty

/// Create a typed tuple
let mkTuple elems ty =
    mkTyped (TExprTuple elems) ty
