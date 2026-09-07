// Unification core: the error types, the mutable substitution, occursIn,
// unify, and type schemes' free-variable machinery.
module Blade.Unify

open Blade.Ast
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types

// Error Types (public -- consumed by Lowering.fs and Main.fs)

type TypeError =
    | UnboundVariable of string
    /// BL2009. A second top-level `function` declaration reusing a name its
    /// module scope already declared. Without this refusal the later
    /// declaration silently SHADOWED the earlier one, and a call matching the
    /// first signature died with a rank/type mismatch blaming the CALLER.
    /// Same-scope only: a nested `function` desugars to a block-scoped
    /// `let const name = lambda` and never reaches this check, so inner
    /// shadowing of an outer function name stays legal. `firstSite` is
    /// preformatted ("line L, column C"). When clause dispatch lands
    /// (plan-match-statements.md §5 R1), same-name declarations at
    /// compatible-but-distinct signatures become a clause set; everything
    /// else stays refused here.
    | DuplicateFunctionDecl of name: string * firstSite: string
    | TypeMismatch of expected: IRType * actual: IRType
    | ArityMismatch of expected: int * actual: int
    /// BL3002, kernel-apply seam. The WIDTH SCHEMA did not cover the pack
    /// (docs/plan-tuples-vs-arg-packs.md 6c, Design C): a parameter list is a
    /// schema over the pack's flat leaf sequence -- unannotated params consume
    /// one leaf, `Tuple<k>` consumes k -- and the totals must agree. Its own
    /// variant rather than `ArityMismatch` because "expected N args, got M" is
    /// the wrong sentence here: the number that has to move may be the number
    /// of PARAMS, the number of OPERANDS, or an annotation. Replaces the
    /// `else Ok ()` that promised the check was "handled elsewhere" and made
    /// under-arity a silent operand drop (3.4, M2). Payload = whole message.
    | KernelPackArity of message: string
    /// Rank disagreement between a declared parameter and the argument
    /// supplied at a DIRECT application. Raised by dispatchAppOrIndex's
    /// FuncElem arm, which does not unify args against params (see the
    /// comment there); `pos` is 1-based.
    | ArgRankMismatch of pos: int * expected: int * actual: int * expectedTy: string * actualTy: string
    /// BL3001. The ELEMENT-CLASS twin of ArgRankMismatch, from the same
    /// (non-unifying) seam: at a DIRECT application the argument's type and
    /// the parameter's declared type are in different, mutually unreachable
    /// classes (text vs number, bool vs number, struct vs scalar, ...). Only
    /// raised when BOTH sides are already concrete -- an open inference var
    /// on either side declines to be classified, so HM instantiation of a
    /// `T^k` parameter is untouched. `func` names the callee for the message;
    /// `pos` is 1-based.
    | ArgTypeMismatch of pos: int * func: string * expected: string * actual: string
    | InvalidArrayCapture of varName: string
    | InvalidApplication of funcType: IRType
    | PatternTypeMismatch of pattern: string * expected: IRType
    /// BL2007. A provider's NATIVE library (libnetcdf) failed to load while
    /// reading store metadata at a `let store = alias.load(path)` site.
    /// Distinct from a missing/unreadable STORE (those keep checkDecl's
    /// silent opaque fallback -- Lowering.tryInvokeProvider owns their
    /// diagnostics): with no library, no store this provider names can ever
    /// resolve in this process, and the fallback would surface as a baffling
    /// element-type mismatch (the defaulted Float64 vs the store's real
    /// element type) at some downstream ascription instead of the cause.
    | ProviderNativeLoadFailure of provider: string * path: string * detail: string
    /// BL2008. A provider REFUSED to resolve the store named at a
    /// `let store = alias.load(path)` site (which is also every
    /// `repo.checkout(...)`, after ProviderDesugar rewrites it into one): the
    /// repo is missing, the ref is typo'd/ambiguous/a deleted tag, the header
    /// is not spec 2, the status is Offline, or the snapshot carries
    /// something this reader refuses by name.
    ///
    /// Sibling of ProviderNativeLoadFailure, one condition over: there the
    /// LIBRARY is unusable, here the STORE is. Both park at the load site
    /// because with no metadata every dim/variable the store binds is
    /// untypeable and the opaque fallback dies far downstream as a baffling
    /// element-type mismatch -- but here the fallback was worse: the refusal
    /// set IS the feature, and `blade check` reported none of it.
    ///
    /// Providers signal this by raising `Types.ProviderResolutionError`;
    /// zarr/netcdf/csv do not, so their missing-store diagnostics stay in
    /// Lowering.tryInvokeProvider, untouched.
    | ProviderStoreUnresolvable of provider: string * path: string * detail: string
    // FIELDS = sprintf args; formatTypeError (TypeEnv.fs) renders each verbatim.
    // Index-type violations (BL4003)
    | IndexTagMismatchNamed of expected: string * actual: string
    | IndexTagMismatchAnon of expected: string
    | CrossNominalIndexArith of left: string * right: string
    | CrossAnonIndexArith of left: int * right: int
    | IndexTypeArithForbidden of name: string
    | IrrepsIdxArgMismatch of pos: int * expected: string * actual: string
    /// The family-level twin of the above: a direct-application mismatch
    /// where at least one side is the POINT-GROUP block-spec member. Covers
    /// pg-vs-pg and cross-member (irreps is never pg-irreps, any extent).
    | BlockSpecArgMismatch of pos: int * expected: string * actual: string
    // CompoundIdx flat-subscript errors: a compound axis takes k FLAT
    // positional subscripts like SymIdx (B(i,j,t)), not tuple form
    // B((i,j)) -- wildcard/partial forms live in SparseIdx.
    /// Tuple/wildcard form reached a compound axis; not accepted here.
    | CompoundTupleForm of rank: int
    /// Fewer than k subscripts supplied for the rank-k compound axis.
    | CompoundUnderSupplied of rank: int * got: int
    /// More subscripts than the compound axis + trailing dims can consume.
    | CompoundOverSupplied of rank: int * got: int
    // The SparseIdx application-form errors: the one-tuple-per-tabulated-axis
    // rules (validateTabulatedIndex) with wildcard currying (formalism 3.5).
    | SparseBareWildcard of rank: int
    | SparseWildcardArity of rank: int * tupleLen: int
    | SparseAllFree of rank: int
    | SparseOverSupplied of rank: int * got: int
    | SparseNeedsTuple of rank: int
    /// HARD REFUSAL at the OrbIdx storage boundary (docs/plan-orbit-index-types.md
    /// sec. 9, BL4003): a depth >= 2 iterated-wreath class type-checks, but no
    /// allocator/traversal/compact-read/printer/provider path emits it -- legal
    /// type, unavailable storage. `levels` = rendered level list; `where_` = the seam.
    | OrbitStorageUnsupported of levels: string * where_: string
    /// `range<R>` / `Array<T like R>` over a `static struct` whose solution
    /// set cannot be enumerated: the constraints are not closed-form AND the
    /// box is over the table cap (docs/plans/structural/06). BL4020.
    | ConstrainedDomainRefused of name: string * why: string
    /// A wreath subscript at the wrong arity. A depth >= 2 OrbIdx record is ONE
    /// index slot spanning prod(ri) RAW AXES, so `W(i,j,k,l)` presents 4 args
    /// against 1 slot; without this case `dispatchAppOrIndex`'s catch-all would
    /// mint a fresh var and return Ok instead of erroring. `got < axes` is also
    /// PARTIAL read, refused outright: a wreath pool's rows shrink per level,
    /// so no residual class describes a fibre of it.
    | OrbitSubscriptArity of levels: string * axes: int * got: int
    /// `decompact(W, d)` at a `d` other than 0 on a wreath class. Only FULL
    /// decompaction is implemented; the partial/peel lattice
    /// (docs/plan-orbidx-decompaction.md sec. 3) is not built.
    | OrbitDecompactPartial of levels: string * dim: int
    /// reduce()/prodsum() over a wreath pool. Separate from the generic storage
    /// refusal because the REMEDY differs: full decompaction now works, so this
    /// message names it rather than saying the class cannot be touched.
    | OrbitFoldUnsupported of levels: string * op: string
    | RaggedIdxNeedsPrior of func: string
    | IrrepsIdxSpec of detail: string
    | IrrepsIdxSpecFn of func: string * detail: string
    /// The pg-irreps twin of the two above: separate cases since the
    /// trailing "what a spec looks like" sentence differs per block-spec
    /// member, and is the actionable half of the diagnostic.
    | PgIrrepsIdxSpec of detail: string
    | PgIrrepsIdxSpecFn of func: string * detail: string
    /// `Base<_>` outside parameter position. `where` names the site.
    | TagWildcardNotParam of where_: string
    // Symmetry / compact-group violations (BL4004)
    /// Two index slots agree positionally but span a different index
    /// COMPONENT count: a rank-k compact group (SymIdx<k>/AntisymIdx<k>) is
    /// one slot but k dims of Array<T, k>; cell counts can coincide, so no
    /// extent check implies this. Not labelled expected/actual -- `unify`
    /// is symmetric, only `where_` situates the failure.
    | IndexRankMismatch of where_: string * left: string * leftRank: int * right: string * rightRank: int
    | DecompactDimRange of dim: int * totalDims: int
    | DecompactPlainAxis of dim: int
    | DecompactLastSlotOnly of slots: int * slot: int
    | TransposeAxisRange of axis: int * totalDims: int
    | TransposeAxesEqual of axisA: int * axisB: int
    | TransposeWithinGroup of rank: int
    // stack / join shape violations (BL4004)
    | StackNeedsArrays of pos: int * got: string
    | StackShapeMismatch of pos: int * detail: string
    | JoinNeedsArrays of pos: int * got: string
    | JoinDimRange of dim: int * totalDims: int
    | JoinShapeMismatch of pos: int * detail: string
    | StackJoinCompactSlot of op: string * slot: int
    // Unit mismatch (BL3006)
    | UnitMismatch of context: string * left: string * right: string
    // Quantity (nominal unit) violations
    /// BL3010: a function parameter declared with a QUANTITY (nominal unit)
    /// received an argument that does not carry that quantity — bare and
    /// structurally-dimensioned arguments are both rejected; the caller must
    /// ascribe (`x : speed`). `got` describes the argument's signature.
    | QuantityArgMismatch of pos: int * quantity: string * got: string
    /// BL3016: a parameter's index slot and the argument's BOTH carry a
    /// compile-time-literal extent, and they differ. Codegen bakes a literal
    /// parameter extent into loop bounds and result allocations (a symbolic
    /// `Idx<n>` reads `.extents[d]` at runtime instead), so this is an
    /// out-of-bounds read, not a naming disagreement. `dim` is 1-based over
    /// the array's index slots. Raised at BOTH param-vs-arg seams: direct
    /// application (dispatchAppOrIndex) and kernel application
    /// (buildApplyInfo), which is why `pos` says "argument"/"parameter"
    /// rather than naming a call form.
    | ExtentArgMismatch of pos: int * dim: int * expected: int64 * actual: int64
    /// BL3016 (same family as ExtentArgMismatch, the halo twin): a kernel body
    /// reads an array through a halo window (`A(w(o))`), the halo's declared
    /// inner extent and the array's extent on that slot are BOTH compile-time
    /// literals, and they differ. The window walk is bounded by the DECLARED
    /// extent, so an oversized halo reads past the array's allocation and an
    /// undersized one silently emits fewer windows -- a wrong answer with no
    /// symptom. `dim` is 1-based over the indexed array's slots.
    | HaloExtentMismatch of declared: int64 * dim: int * targetName: string * actual: int64
    /// BL4019: a window read `A(w(o))` whose LITERAL offset `o` lies outside
    /// the halo's declared offset set (`halo<I, [-1, 0, 1]>` read at `w(2)`).
    /// The interior is shrunk for the DECLARED reach only, so such a read
    /// lands past the array's pool at the boundary -- silently in the
    /// compiled lane, as a BL8003 panic in the interpreter (a latent
    /// differential red; docs/plans/structural/02 section 1.5). A computed
    /// offset is not judged here (fail-open, as loops/080 relies on).
    | HaloOffsetOutsideSet of offset: int * declared: int list * targetName: string
    /// BL3016 (same family, the co-iteration twin): two operands of one
    /// elementwise zip -- `A + B`, `zip(A, B) <@> k`, `method_for(zip ..)` --
    /// carry DIFFERENT compile-time-literal extents on the shared axis. The
    /// shared iteration record comes from the FIRST operand, so the walk runs
    /// the first operand's extent over every operand's storage: the shorter
    /// one is read past its allocation, silently, with no broadcast anywhere
    /// in the language to justify it. `pos` is the offending operand, 1-based.
    | ZipExtentMismatch of pos: int * expected: int64 * actual: int64
    /// BL3016 (same family, the ABSTRACT-PARAMETER twin of ZipExtentMismatch):
    /// a callee's body co-iterates two of its parameters, and this call passes
    /// them arrays whose shared-axis extents are different compile-time
    /// literals.
    ///
    /// ZipExtentMismatch catches the disagreement AT the zip, but only when
    /// both extents are literals there. Through abstract `T^1` parameters they
    /// are not: the body sees two rank-1 arrays with no extents to compare, so
    /// nothing was checked, and the extents only become concrete here, at the
    /// call. That is the whole hole -- `addup(q6, p3)` on a body that zips its
    /// two parameters typechecked clean and walked q's 6 cells over p's 3-cell
    /// allocation, in both lanes. The obligation rides
    /// `TypeEnv.FuncCoIterObligations`, derived from the callee's body.
    /// `posA`/`posB` are 1-based ARGUMENT positions in this call.
    | CoIterArgExtentMismatch of callee: string * posA: int * posB: int * extA: int64 * extB: int64
    /// BL3016 (the one-sided twin of CoIterArgExtentMismatch): a callee's body
    /// co-iterates one of its abstract parameters with an array whose extent is
    /// already CONCRETE there -- `wsum(a: T^1) = reduce(zip(a, weights3) ..)`.
    /// The zip has one literal and one abstract side, so `zipHeadClash`'s
    /// literal-vs-literal rule does not fire, and the walk (bounded by operand
    /// 1) ran the argument's extent over the concrete array's storage. The
    /// body's literal rides `FuncCoIterObligations` alongside the parameter
    /// positions; `pos` is the 1-based ARGUMENT position in this call.
    | CoIterBodyExtentMismatch of callee: string * pos: int * argExt: int64 * bodyExt: int64
    /// BL3016 (same family, the provider-ascription twin): a `let` annotation
    /// or `:` ascription on a PROVIDER READ names a compile-time-literal
    /// extent on some index slot and the read's own type names a different
    /// one. Wherever two extents can come from DIFFERENT places, this compiler
    /// already has a check saying so -- param vs argument, operand vs operand,
    /// halo vs target, literal vs annotation. The provider read is the one
    /// such seam that had none. The store fixes the ALLOCATION (codegen bakes
    /// the file's real shape into the buffer and the reader loop) while the
    /// annotation fixes the TYPE every later index expression compiles
    /// against -- and the read arm passes the operand's type through
    /// unchanged, so nothing reconciled them. `dim` is 1-based over the
    /// array's index slots; `provider` is the registry name for the message.
    | ProviderReadExtentMismatch of provider: string * dim: int * annotated: int64 * actual: int64
    /// BL3011: a quantity name used inside unit algebra (`Unit x = speed * m`)
    /// or as the RHS of another quantity (`Unit q: speed`). Quantities are
    /// TERMINAL: the nominal layer is exactly one level deep.
    | QuantityTerminal of quantity: string * declName: string
    /// BL3015: a name on the RHS of a `Unit` declaration that resolves to
    /// neither a declared unit nor a built-in scale constant. `candidates`
    /// holds near-miss spellings already in scope, for the message.
    | UnknownUnitName of name: string * declName: string * candidates: string list
    // Parameter defaults (BL3012)
    /// A required (default-less) parameter follows a defaulted one: defaults
    /// are TRAILING (classic rule), or omitted-argument calls would be
    /// ambiguous. `func` is "<lambda>" for anonymous lambdas.
    | DefaultParamOrder of func: string * requiredParam: string * defaultedParam: string
    /// A parameter's default expression references another DEFAULTED
    /// parameter. Defaults may reference the required parameters only —
    /// they evaluate left-to-right at call entry with just the required
    /// arguments bound.
    | DefaultParamScope of func: string * param: string * referenced: string
    /// BL3012, call site. A default is spliced into the call as surface
    /// syntax, and a free name it reads resolves THERE -- so when the caller
    /// has a parameter or local of the same spelling, the default silently
    /// read that instead of the declaration-site binding (`function f(x =
    /// k)` from `function g(k) = f()` returned g's argument). The splice
    /// compares binding identities (TypeEnv.FuncDefaultCaptures) and refuses.
    | DefaultParamShadowed of func: string * param: string * name: string
    // Factory quantity slots
    /// BL3013 (declaration): two DEFAULTED params of one function carry the
    /// SAME quantity nominal. By-nominal argument routing needs each quantity
    /// to name exactly one slot, so this rejects at the declaration even if
    /// the function is never called.
    | FactoryDupQuantityDecl of func: string * quantity: string * param1: string * param2: string
    /// BL3014 (call site): one quantity slot received two arguments -- two
    /// tagged args with the same nominal, or a tagged arg targeting a slot
    /// already claimed by the positional prefix.
    | FactoryDupFill of callee: string * quantity: string * slot: string
    /// BL3014 (call site): a quantity-tagged trailing argument matches none
    /// of the callee's quantity slots. `candidates` names the slots it has.
    | FactoryUnknownTag of callee: string * quantity: string * candidates: string list
    /// BL3014 (call site): an untagged (positional) trailing argument appears
    /// AFTER a quantity-tagged one -- its slot would be a guess.
    | FactoryAmbiguousMix of callee: string * pos: int
    // Invalid builtin/intrinsic argument (BL3007)
    | IntrinsicBindArrayFailed of op: string
    | IntrinsicNeedsArray of op: string
    | IntrinsicNotComplex of name: string
    | IntrinsicNeedsNumeric of name: string
    | AbsNeedsNumericScalar of got: string
    | IntrinsicComplexScalarOnly of name: string
    | IntrinsicNeedsComplex of name: string * got: string
    | ComplexArity of got: int
    /// BL3019. An explicit numeric cast (`Float32(x)`, `Int64(floor(x))`)
    /// refused: complex source into a real/int target, a float->int cast
    /// without the rounding visible at the cast site, a non-numeric operand,
    /// an operand whose type is not yet known, or a wrong arity. Payload is
    /// the complete message.
    | InvalidCast of message: string
    /// BL3021. A `match` whose scrutinee is a SPECIALIZATION INDEX --
    /// `arity(p)` over a pack param, or `rank(p)` over an abstract/caret
    /// param -- selects its arm when the specialization is cloned, not at
    /// runtime, so every arm must be statically decidable: unguarded integer
    /// literals plus at most one catch-all. `scrutinee` is the intrinsic as
    /// written; `offender` is a clause naming the arm that cannot be decided.
    | SpecIndexMatchNotStatic of scrutinee: string * offender: string
    | ReduceEmptyArray of extent: int64
    | ProdsumExtentMismatch of a: int64 * b: int64
    | GramNeedsRank2 of leftRank: int * rightRank: int
    /// A gram operand whose two dimensions arrive as ONE compact rank-2
    /// group (SymIdx / AntisymIdx / HermitianIdx storage, e.g. a gram
    /// result): the dims SUM is 2 but there is no second slot to read the
    /// contracted extent from. `side` names the offender as a subject-verb
    /// clause ("the left operand carries", "both operands carry").
    | GramCompactOperand of side: string
    | ArrayLitLength of got: int * expected: int * axisTag: string option
    /// An array literal checked against a COMPACT index group (SymIdx /
    /// AntisymIdx / HermitianIdx, rank >= 2): ONE axis whose stored cells
    /// are a left-justified simplex (rows of length n, then n - i0 -
    /// strict, ...; `canon_left_justify`, the allocator's DFS order). `idx`
    /// = group, `shape` = expected skeleton, `where_` = disagreeing
    /// position, `detail` = what was found.
    | CompactLitShape of idx: string * shape: string * where_: string * detail: string
    /// A HermitianIdx literal whose DIAGONAL cell carries a non-zero
    /// imaginary part: A(i,i) = conj(A(i,i)) forces the diagonal real, and
    /// the stored cell is read unconjugated at (i,i). `where_` locates the cell.
    | HermitianLitDiagComplex of where_: string
    /// A ragged array LITERAL whose row lengths disagree with the lens its
    /// closed `RaggedIdx<lens>` annotation names. Nothing derives the shape
    /// from the lens -- construction bakes `_lens`/`_offsets` from the
    /// literal's own nesting -- so the annotation and the array it annotates
    /// would describe different shapes.
    | RaggedLensMismatch of lensName: string * declared: string * actual: string
    /// The same seam, one step earlier: the lens a closed `RaggedIdx<lens>`
    /// names is not a compile-time value, so there is nothing to check the
    /// literal against and nothing construction could honour.
    | RaggedLensNotStatic of lensName: string
    | ObjectForKernel of got: string
    | ChainOpNeedsMethodFor of leftDesc: string
    | ChainOpBadKernel of rightDesc: string
    | ChainOpUndecidable of leftDesc: string * rightDesc: string
    | CommContradictsBody of param1: string * param2: string
    | AntisymmContradictsBody of param1: string * param2: string
    // The Hermitian third of the pair-swap contradiction family: the body
    // provably CONJUGATES under the swap (f(y,x) = conj(f(x,y)), deduced
    // PConj) over complex elements, so both the identity mirror comm
    // licenses and the sign mirror anticomm licenses answer mirrored
    // reads wrong. Only minted when the pair's element type is provably
    // complex -- over reals conj is the identity and the same body is
    // genuinely symmetric.
    | CommContradictsConjBody of param1: string * param2: string
    | AntisymmContradictsConjBody of param1: string * param2: string
    | AntisymMapNotOdd of param: string * proved: string
    | HermitianMapNotReal of param: string
    // The wreath-tie analog of AntisymMapNotOdd (IRLoopStructure.deduceWreathTie
    // condition 6): a comm/anticomm tie over an input class with a '-'
    // INNER level needs the kernel provably sign-odd in every tied argument;
    // the deduction says `param` is `proved` instead. `levels` renders the class.
    | WreathTieKernelNotOdd of param: string * proved: string * levels: string
    // `where ... omp` on a FOLD kernel (reduce's 2nd arg) needs a reorder
    // licence: chunking hands different associations to different threads,
    // so the kernel needs commutative AND associative. `comm(a, b)` declares
    // the first (cross-checked by CommContradictsBody); a recognised builtin
    // body (`a+b`/`a*b`/`a&&b`/`a||b`) supplies both. Neither present =
    // refuse rather than silently emit a serial loop. `kernelDesc` names it.
    | FoldOmpNeedsLicense of kernelDesc: string
    | PlaceholderNeedsAllBound of got: int * total: int
    | GroupKeysRank1
    /// BL3017: a `group_keys` result reached a position other than its own
    /// `let` RHS or a `group_by` grouping slot. The value is NAME-KEYED, not
    /// a value: `genGroupKeysBinding` puts the whole CSR structure in C++
    /// locals suffixed off the BINDING name (`<n>__ngroups`, `<n>__offsets`,
    /// `<n>__perm`) and hands the binding itself a `void*` sentinel, so
    /// `genGroupByBinding` can only find the state under the exact name the
    /// keys were bound to. Any indirection -- an alias, a tuple, a parameter
    /// -- used to emit C++ referencing suffixed symbols that were never
    /// declared. `what` names the offending expression, `pos` the position.
    /// TypeCheck's same-gk co-iteration check already ASSUMES this invariant
    /// (it compares gk operands by binding NAME); this enforces it.
    | GroupKeysEscapes of what: string * pos: string
    // The grouping ACCESSORS (`group_bucket`, `extents`) take a gk by NAME, for
    // the same reason BL3017 exists -- the state is reachable only through the
    // binding. These two are the accessor-side argument checks, raised during
    // inference (the accessor cannot even be TYPED without a resolved grouping);
    // BL3017's post-zonk sweep is what catches a gk escaping anywhere else.
    | GroupingNeedsName of intrinsic: string * got: string
    | GroupBucketNotGrouping of got: string
    | CumulantOrderPositive of order: int
    | CumulantOrderExceeds of order: int * carried: int
    | CumulantNeedsDist of got: string
    | DistOrderDisagree of op: string * leftOrder: int * rightOrder: int
    | DistNotIndependent of op: string * source1: string * source2: string * steering: string
    | DistOpUndefined of left: string * right: string
    | EnumIdxMixedKinds of name: string
    | EnumIdxUnknownLabel of enumName: string * label: string * available: string list
    | ImplMissingMethods of iface: string * typeName: string * methods: string
    // <|:> allocated-fallback operand violations (BL3007)
    | FallbackNeedsArrays of leftDesc: string * rightDesc: string
    | FallbackSymmetricLeft
    | FallbackRightNotDense of what: string
    | FallbackRankMismatch of leftRank: int * rightRank: int
    // Struct construction (BL3008)
    | StructFieldDuplicate of structName: string * field: string
    | StructNoField of structName: string * field: string
    /// Field ACCESS (`v.w`, `v.w(i)`) naming a field the struct does not
    /// declare -- BL3018, not BL3008's constructor-side twin above. The two
    /// are separate codes because they fail at opposite ends: a constructor
    /// has the full field list in hand and always refused, while an ACCESS
    /// used to degrade to a fresh type variable, so the program typechecked
    /// and the mistake surfaced (if at all) as an unrelated codegen refusal
    /// against an array of unknown extent. `available` is the declared field
    /// list in declaration order; `steering` is optional extra guidance
    /// (empty when there is none) -- e.g. a provider `<mod>__vars` access
    /// whose name is really filed under the sibling `<mod>__dims`.
    | StructFieldUnknown of structName: string * field: string * available: string list * steering: string
    | StructSpreadBase of structName: string
    | StructSpreadNotStruct of structName: string * got: string
    | StructSpreadRedundant of structName: string
    | StructMissingField of structName: string * field: string
    | StructFieldType of structName: string * field: string * expected: string * actual: string
    | UnknownStructType of name: string
    | StructBoundScope of structName: string * field: string * bad: string
    /// `static struct S` declares that every field is statically evaluable;
    /// this field's type is not. `why` names the offending shape.
    | StaticStructField of structName: string * field: string * why: string
    /// A bounded primitive whose bounds cross: `min=` above `max=`, decided
    /// statically. `where_` locates it ("struct R, field 'm'", "let x", ...).
    | BoundsInverted of where_: string * lo: string * hi: string
    /// A `min=`/`max=` bound applied to an AGGREGATE. `TyBounded` is the
    /// bounded PRIMITIVE node; `boundedConjuncts`'s guards compare against
    /// the annotated value, which arrays/tuples/structs/arrows have no
    /// answer for. `where_` locates it, `noun`/`subject` describe what.
    | BoundsOnAggregate of where_: string * noun: string * subject: string
    // Rank deduction violation (BL3009)
    /// A value flowed into a position demanding rank >= k (stage-2 rank
    /// deduction LOWER BOUND, max-joined across the body's uses) but
    /// resolved shallower. Dedicated case (not `Other`/BL3999) so `needed`
    /// stays machine-readable; `got` is the rendered actual. Distinct from
    /// `IndexRankMismatch`/BL4004 (component-rank WITHIN a slot).
    | RankBoundViolation of needed: int * got: string
    // Constraint / where-clause violations (BL4001)
    | StructWhereNotBool of structName: string * got: string
    | StructWhereError of structName: string * inner: string
    | WherePredicateUnannotated of owner: string * func: string
    | PplConstraintNeedsImport of func: string * bare: string
    | UnknownWhereConstraint of func: string * name: string * vocab: string
    // Static-evaluation requirement (BL4002)
    | DistOrderCompileTime of func: string
    // Mutability violations (BL4005)
    | ImmutableStaticAssign of name: string
    | MutParamNotArray of func: string * param: string
    /// An assignment whose TARGET the callee may not write through. Carries its
    /// own reason because the two shapes it covers fail for different causes:
    /// rebinding a whole array (which names a new array instead of writing the
    /// old one's storage), and writing through an index/field into a binding
    /// that never granted write permission.
    | MutAssignRefused of target: string * reason: string
    /// A call-site argument handed to a `mut` parameter without the write
    /// permission that parameter implies (formalism 2.7: only `let mut`
    /// is mut-passable). `got` renders what was actually passed.
    | MutArgNotPassable of func: string * argIndex: int * got: string
    // Mutual-group binding / constraint violations (BL4006)
    | MutualBindJointly of typeName: string * describe: string * lowerNames: string
    | MutualDirectElementsOnly of describe: string
    | MutualMixedGroups
    | MutualDuplicateMember of describe: string
    | MutualIncompleteAnnotation of describe: string
    | MutualJointAnnotationOnly of describe: string
    | MutualParamMemberType of func: string * param: string * memberName: string
    | MutualBindTuple of names: string
    | MutualReturnTupleElements of describe: string
    | StructFieldMutualType of structName: string * field: string * memberName: string
    | MutualMemberDupGroup of memberName: string
    | MutualMemberNotStruct of memberName: string * name: string
    | MutualMemberBadAlias of memberName: string * got: string
    | MutualUnknownField of memberName: string * field: string * structName: string
    | MutualScalarBare of memberName: string * field: string
    | MutualStructNeedsField of memberName: string
    | MutualUnknownIdent of name: string
    | MutualUnsupportedExpr
    | MutualConstraintNotBool of groupId: string * got: string
    | MutualConstraintError of groupId: string * inner: string
    // Provider (import/read/write/stream) argument errors (BL3007 / BL2003)
    | ProviderStreamNeedsVar of alias: string
    | ProviderReadWindowBounds of alias: string * lo: int64 * hi: int64 * n: int64
    | ProviderReadWindowLiteralExtent of alias: string
    | ProviderReadWindowPacked of alias: string
    | ProviderReadWindowNeedsVar of alias: string
    | ProviderReadWindowArgs of alias: string
    | ProviderWriteNeedsArray of alias: string
    | ProviderWriteNamedBinding of alias: string
    | ProviderWriteArgs of alias: string
    /// `alias.write(...)` written anywhere but a module-level `let` binding.
    | ProviderWriteModuleScope of alias: string
    | ProviderImportByModule of suggestion: string * providers: string
    | ProviderNoSelectiveImport of pname: string
    | Other of string

/// A compile error with source location and context stack
type CompileError = {
    Error: TypeError
    Span: Span
    Context: string list  // e.g., ["in function 'foo'"; "in let binding 'x'"]
    /// BLxxxx diagnostic code when the raiser knows it (elaborators, index
    /// validation). None = derive from the TypeError variant at render time.
    Code: string option
}

type TypeResult<'T> = Result<'T, TypeError>

// 1. Unification Infrastructure

/// Mutable substitution mapping inference variable IDs to resolved types.
type Subst() =
    let mutable map : Map<int, IRType> = Map.empty
    let mutable nextId = 10000  // High start avoids collision with IRBuilder IDs
    let mutable typeVarScope : Map<string, IRType * int> = Map.empty
    let mutable arityConstraints : Map<int, int> = Map.empty
    /// Inference vars minted for value-position literals (`1`, `2.0`,
    /// `true`). ElemType is the literal's *default* type when unpinned (so
    /// `let x = 1` stays Int64) and its *kind* for the no-narrowing bind
    /// guard in `unify`. Parallels `arityConstraints`.
    let mutable literalDefaults : Map<int, ElemType> = Map.empty
    let mutable knownTypeVarNames : Set<string> = Set.empty
    /// IDs of type variables HM-polymorphic at a function boundary:
    /// preserved by zonking (not defaulted to Float64) so IR-phase HM
    /// monomorphization can substitute at call sites. Populated lazily as
    /// LookupOrCreateTypeVar mints IDs for prescanTypeVarNames names.
    let mutable polymorphicIds : Set<int> = Set.empty
    /// Minimum-rank lower bounds on inference vars (stage-2 deduction): the
    /// var must resolve to rank >= k. Populated at direct-application seams
    /// from callee parameter ranks (max-joined), propagated on var->var
    /// binds, validated when the var meets a concrete type (unify).
    /// Parallels arityConstraints, the EXACT-rank pin `T^k` uses.
    let mutable rankLowerBounds : Map<int, int> = Map.empty

    member _.Fresh() =
        let id = nextId
        nextId <- nextId + 1
        IRTInfer id

    member _.Bind(id, ty) =
        // POLYMORPHIC MARK PROPAGATION (var-to-var only). The mark says "zonk
        // must keep this var open; IR-phase HM monomorphization substitutes
        // it per call site". When a marked var defers to ANOTHER var the mark
        // has to travel with it, or the survivor gets defaulted to Float64 by
        // zonk and the function collapses to one element type. Exactly the
        // `CopyLiteralDefault` situation, one bind arm up. Concrete binds are
        // untouched -- a mark on a resolved var means nothing.
        (match ty with
         | IRTInfer id2 when Set.contains id polymorphicIds ->
             polymorphicIds <- Set.add id2 polymorphicIds
         | _ -> ())
        map <- Map.add id ty map

    member _.TryFind(id) =
        Map.tryFind id map

    member this.LookupOrCreateTypeVar(name: string, arity: int, builder: IRBuilder) : IRType =
        let key = name + "^" + string arity
        match Map.tryFind key typeVarScope with
        | Some (tv, _) -> tv
        | None ->
            let tv = this.Fresh()
            if arity > 0 then
                match tv with
                | IRTInfer id -> arityConstraints <- Map.add id arity arityConstraints
                | _ -> ()
            typeVarScope <- Map.add key (tv, arity) typeVarScope
            // Track HM-polymorphic names here too (see polymorphicIds doc).
            if Set.contains name knownTypeVarNames then
                match tv with
                | IRTInfer id -> polymorphicIds <- Set.add id polymorphicIds
                | _ -> ()
            tv

    member this.LookupOrCreateTypeVar(name: string) : IRType =
        let key = name + "^0"
        match Map.tryFind key typeVarScope with
        | Some (tv, _) -> tv
        | None ->
            let tv = this.Fresh()
            typeVarScope <- Map.add key (tv, 0) typeVarScope
            if Set.contains name knownTypeVarNames then
                match tv with
                | IRTInfer id -> polymorphicIds <- Set.add id polymorphicIds
                | _ -> ()
            tv

    member _.IsPolymorphicId(id: int) : bool =
        Set.contains id polymorphicIds

    /// Mint the mark on a var that was NOT created from a signature type-var
    /// NAME. The only client is the array-shape synthesis in
    /// `requireArrayArgMinRank`: giving a `T^k` signature var its array shape
    /// replaces one polymorphic var with `Array<E, ..>`, and unless `E`
    /// inherits the mark the function stops being polymorphic in its element
    /// type at exactly the moment its shape becomes known.
    member _.MarkPolymorphic(id: int) =
        polymorphicIds <- Set.add id polymorphicIds

    member _.CopyPolymorphic(fromId: int, toId: int) =
        if Set.contains fromId polymorphicIds then
            polymorphicIds <- Set.add toId polymorphicIds


    member _.AddRankLowerBound(id: int, k: int) =
        if k > 0 then
            let cur = Map.tryFind id rankLowerBounds |> Option.defaultValue 0
            if k > cur then rankLowerBounds <- Map.add id k rankLowerBounds

    member _.GetRankLowerBound(id: int) : int option =
        Map.tryFind id rankLowerBounds

    member _.GetArityConstraint(id: int) : int option =
        Map.tryFind id arityConstraints

    member _.CopyArityConstraint(fromId: int, toId: int) =
        match Map.tryFind fromId arityConstraints with
        | Some k -> arityConstraints <- Map.add toId k arityConstraints
        | None -> ()

    /// The `rankLowerBounds` twin of CopyArityConstraint, for `instantiate`:
    /// a generalized value's fresh use-site var must inherit its scheme
    /// var's bound, else the deduction is lost each instantiation. Routed
    /// through AddRankLowerBound so an existing bound is MAX-JOINED, not
    /// overwritten (CopyArityConstraint overwrites -- an arity pin is exact).
    member this.CopyRankLowerBound(fromId: int, toId: int) =
        match Map.tryFind fromId rankLowerBounds with
        | Some k -> this.AddRankLowerBound(toId, k)
        | None -> ()

    /// Mint a fresh inference var seeded with a literal's value class (its
    /// default-when-unpinned type and its numeric/bool kind). See
    /// `literalDefaults`.
    member this.FreshLiteral(et: ElemType) : IRType =
        let tv = this.Fresh()
        match tv with
        | IRTInfer id -> literalDefaults <- Map.add id et literalDefaults
        | _ -> ()
        tv

    member _.GetLiteralDefault(id: int) : ElemType option =
        Map.tryFind id literalDefaults

    member _.SetLiteralDefault(id: int, et: ElemType) =
        literalDefaults <- Map.add id et literalDefaults

    member _.CopyLiteralDefault(fromId: int, toId: int) =
        match Map.tryFind fromId literalDefaults with
        | Some et ->
            // Only carry the seed forward if the target isn't already a
            // literal var (don't clobber an existing kind).
            if not (Map.containsKey toId literalDefaults) then
                literalDefaults <- Map.add toId et literalDefaults
        | None -> ()

    member _.RegisterTypeVarName(name: string) =
        knownTypeVarNames <- Set.add name knownTypeVarNames

    member _.IsTypeVar(name: string) : bool =
        Set.contains name knownTypeVarNames

    member _.PushTypeVarScope() : Map<string, IRType * int> * Set<string> =
        let savedScope = typeVarScope
        let savedNames = knownTypeVarNames
        typeVarScope <- Map.empty
        knownTypeVarNames <- Set.empty
        (savedScope, savedNames)

    member _.PopTypeVarScope(saved: Map<string, IRType * int> * Set<string>) =
        typeVarScope <- fst saved
        knownTypeVarNames <- snd saved

    /// Instrumented twin of the `IRTInfer` hop below, used ONLY when
    /// `PerfCounters.enabled` (docs/plan-compile-speed.md Stage 5): the same
    /// walk -- follow each bound inference variable, stop at the first unbound
    /// id or non-infer type and resolve that structurally -- with the number of
    /// indirections recorded. A member rather than a local `let rec` so the
    /// measurement allocates no closure, and tail-recursive so it costs no more
    /// stack than the hop it mirrors.
    member private this.WalkInferChain(t: IRType, hops: int) : IRType =
        match t with
        | IRTInfer id ->
            match this.TryFind id with
            | Some next -> this.WalkInferChain(next, hops + 1)
            | None ->
                PerfCounters.resolveChain hops
                t
        | other ->
            PerfCounters.resolveChain hops
            this.Resolve other

    /// Recursively resolve a type through the substitution.
    /// Applies rank-0 collapse: Array<T, (no indices)> -> Scalar T.
    member this.Resolve(ty: IRType) : IRType =
        if PerfCounters.enabled then PerfCounters.resolveCall ()
        match ty with
        | IRTInfer id ->
            if PerfCounters.enabled then this.WalkInferChain(ty, 0) else
            match this.TryFind id with
            | Some ty' -> this.Resolve ty'
            | None -> ty
        | IRTTuple ts -> IRTTuple (ts |> List.map this.Resolve)
        | IRTComputation t -> IRTComputation (this.Resolve t)
        | IRTLoop lt ->
            IRTLoop { lt with
                        ArrayTypes = lt.ArrayTypes |> List.map this.Resolve
                        KernelType = lt.KernelType |> Option.map this.Resolve }
        | IRTPoly (base', var) -> IRTPoly (this.Resolve base', var)
        | IRTUnitAnnotated (inner, units) -> IRTUnitAnnotated (this.Resolve inner, units)
        | IRTIdxTagged (inner, idxRef) -> IRTIdxTagged (this.Resolve inner, idxRef)
        | IRTDist (order, elem, axes) -> IRTDist (order, this.Resolve elem, axes)
        | IRTArrow (slots, result, identity) ->
            let resolveSlot = function
                | SIdx idx -> SIdx idx
                | SIdxVirt idx -> SIdxVirt idx
                | SVal ty -> SVal (this.Resolve ty)
            IRTArrow (slots |> List.map resolveSlot, this.Resolve result, identity)
        | _ -> ty

    member this.ResolveIdx (idx: IRIndexType) = idx  // Index extents are IRExpr, not IRType

/// Occurs check: does inference variable `id` appear in `ty`?
let rec occursIn (id: int) (ty: IRType) : bool =
    match ty with
    | IRTInfer id2 -> id = id2
    | IRTTuple ts -> ts |> List.exists (occursIn id)
    | IRTComputation t -> occursIn id t
    | IRTPoly (base', _) -> occursIn id base'
    | IRTLoop lt ->
        (lt.ArrayTypes |> List.exists (occursIn id)) ||
        (lt.KernelType |> Option.map (occursIn id) |> Option.defaultValue false)
    | IRTUnitAnnotated (inner, _) -> occursIn id inner
    | IRTIdxTagged (inner, _) -> occursIn id inner
    | IRTDist (_, elem, _) -> occursIn id elem
    | IRTArrow (slots, ret, _) ->
        let slotHit =
            slots |> List.exists (function
                | SVal ty -> occursIn id ty
                | SIdx _ | SIdxVirt _ -> false)
        slotHit || occursIn id ret
    | _ -> false

/// Walk an inference variable chain to find the leaf variable bound to a
/// concrete scalar (or unbound).  Returns its id so we can rebind it to
/// the promoted type.  Returns None if the type is not an inference chain.
let rec findLeafInferScalar (subst: Subst) (ty: IRType) : int option =
    match ty with
    | IRTInfer id ->
        match subst.TryFind id with
        | Some (IRTInfer _ as next) -> findLeafInferScalar subst next
        | Some (IRTScalar _) -> Some id   // bound to concrete scalar -- rebindable
        | None -> Some id                  // unbound -- bindable
        | _ -> None                        // bound to non-scalar -- leave alone
    | _ -> None

/// Strip one layer of tag/unit annotation. Used by the wildcard arm to reach
/// the value type underneath whatever the concrete side is carrying.
let stripTagAnnotation (ty: IRType) : IRType =
    match ty with
    | IRTIdxTagged (inner, _) -> inner
    | IRTUnitAnnotated (inner, _) -> inner
    | other -> other

/// Unify two types under the current substitution; mutates `subst` to bind
/// inference variables. Both sides pass through `normalize ToNested` first
/// (sec. 5.3 mixed-slot identity): concrete-equal pairs short-circuit via
/// `=`, flat-mixed-slot types unify against their split-nested equivalent,
/// genuine mismatches (slot kind, index identity, rank) still reject. NOT
/// bridged: sec. 5.2 uniform-kind array identity needs a `ToFlat`
/// normalization mode, not yet built.

/// A BLOCK-SPEC tag, generalized over the family's two members: the O(3)
/// irreps tag `__irreps:<name>:<l,p,m|...>` and the point-group tag
/// `__pgirreps:<group>:<name>:<LABEL,mult|...>`. Yields (identity, alias
/// option); SAME SPACE iff identity matches -- within a member it's the
/// serialized spec payload (differs even at equal total_dim); across
/// members it can never collide, since the frozen member prefix is part of
/// it. Aliases are nominative (two named aliases differ; anon-vs-named is
/// compatible). Identity is built by re-serializing with the alias erased
/// via the canonical tag writer -- injective, no %A truncation hazard.
let (|BlockSpecTag|_|) (tag: string) : (string * string option) option =
    match tag with
    | IrrepsTag (nameOpt, triples) -> Some (mkIrrepsTag None triples, nameOpt)
    | PgIrrepsTag (group, nameOpt, entries) -> Some (mkPgIrrepsTag group None entries, nameOpt)
    | _ -> None

/// Rank disagreement between two positionally-matched index slots (index
/// COMPONENT count: 1 for Idx, k for SymIdx<k>/AntisymIdx<k>), deciding
/// `Array<T, N>`'s N -- one slot is NOT one dimension. Checked first since
/// no other rule implies it: Symmetry doesn't (SymIdx<2,4>/SymIdx<3,4>
/// both SymSymmetric, SymNone a wildcard); irreps doesn't (keys on spec
/// identity, returns early); ArrayElem slot-count doesn't (rank-k = ONE
/// entry, same as rank-1); extents can't (coincide where it matters --
/// SymIdx<2,3> = 6 cells = Idx<6>). Unchecked, reaches codegen as a g++
/// error (`could not convert Array<double,1> to Array<double,2>`). Rank 0
/// normalizes to 1, same `max 1` convention CodeGen uses for storage.
let indexRankDiffers (i1: IRIndexType) (i2: IRIndexType) : bool =
    max 1 i1.Rank <> max 1 i2.Rank

/// Per-index compatibility for POSITIONAL index-list matching (ArrayElem
/// index types, Dist axes). True = INCOMPATIBLE. One shared predicate (not
/// separate copies in ArrayElem/IRTDist) so the rules cannot drift:
///   - Component RANK must match first, unconditionally (other arms can
///     each mask a rank difference).
///   - Block-spec tags (checked before the synthetic-tag exemption):
///     identity is MEMBER + SPEC + optional alias name -- differs even at
///     equal total_dim; aliases nominative; anon-vs-named compatible;
///     irreps never pg-irreps. Tag = None on one side is COMPATIBLE.
///   - User-named tags are nominative (lat != lon even if both Idx<180>);
///     synthetic ("__") tags are structural, never gate -- EXCEPT the
///     provider provenance family (`isProviderAxisTag`), which is a name the
///     STORE wrote and gates like any other (see `gatesNominally`).
///   - Extents are NOT compared; Symmetry must be compatible (SymNone
///     wildcard); WREATH LEVEL LISTS ARE compared (see the arm below).
let indexPairIncompatible (i1: IRIndexType) (i2: IRIndexType) : bool =
    let isSyntheticTag (t: string) = t.StartsWith("__")
    /// Does this tag GATE -- i.e. is it an identity two records must share,
    /// rather than a structural sentinel every record of a kind carries?
    ///
    /// User-written names always gate. `__` tags normally do not, and that
    /// exemption LAUNDERED provider provenance: `__icaxis|`/`__icpool|` are
    /// `__`-prefixed only so the four seams that read a Tag as a user-facing
    /// name (`checkArrayIndexTags`, `elemTypeForIterationIndex`,
    /// `Ide.indexNamesOf`, and this predicate's own exemption) leave them
    /// alone -- but they are IDENTITIES, and this arm is the one place that
    /// distinction decides the answer. Before this, ascribing two DIVERGED
    /// checkouts' arrays to one `type L = ck1.index.lat` alias (or to any
    /// user-written `type L = Idx<5>`) passed here, so the arithmetic that
    /// followed saw a single tag and co-iterated happily -- the refusal the
    /// axis tag exists to produce, walked around by an annotation. It also
    /// closes the function BOUNDARY: a param typed through a checkout's own
    /// axis alias now refuses the other checkout's array.
    ///
    /// Provider-vs-untagged stays COMPATIBLE (this arm needs `Some` on both
    /// sides): an unnamed index is "some axis of this extent" and co-iterates
    /// with either -- the escape hatch BL3999's own message advertises, and
    /// the route by which a user can still assert two diverged axes are one.
    /// Provider-vs-other-`__` stays compatible too: those are kind sentinels,
    /// and gating them would refuse sound code.
    let gatesNominally (t: string) = not (isSyntheticTag t) || isProviderAxisTag t
    // The wreath arm, ahead of everything but the rank check. Rank +
    // Symmetry alone are NOT sufficient: OrbIdx<[(2,+),(2,+)], n> and
    // OrbIdx<[(2,-),(2,-)], n> are both Rank 4, SymWreath, share the
    // "__orbidx" tag (exempted below) -- every other test says
    // "compatible" while cell counts differ (55 vs 15 at n=4). It rides
    // the Extent slot for lack of a dedicated IRIndexTypeG field
    // (IRSparseKeys precedent); "extents never compared" means runtime
    // EXTENTS -- the BASE extent inside the marker is still not compared.
    let wreathLevels (ix: IRIndexType) =
        match ix.Extent with
        | IROrbitClass (levels, _) -> Some levels
        | _ -> None
    let wreathMismatch =
        match wreathLevels i1, wreathLevels i2 with
        | Some l1, Some l2 -> l1 <> l2
        // A wreath record against a non-wreath one: the Symmetry test below
        // already separates them unless the other side is SymNone (the
        // wildcard) -- a plain Idx flowing into a wreath slot is adoption,
        // so leave it to that arm rather than hard-refusing here.
        | _ -> false
    indexRankDiffers i1 i2 ||
    wreathMismatch ||
    match i1.Tag, i2.Tag with
    | Some (BlockSpecTag (s1, n1)), Some (BlockSpecTag (s2, n2)) ->
        s1 <> s2 || (match n1, n2 with
                     | Some a, Some b -> a <> b
                     | _ -> false)
    | Some t1, Some t2 when t1 <> t2
                            && gatesNominally t1
                            && gatesNominally t2 -> true
    | _ ->
        i1.Symmetry <> i2.Symmetry && i1.Symmetry <> SymNone && i2.Symmetry <> SymNone

let rec unify (subst: Subst) (t1: IRType) (t2: IRType) : TypeResult<unit> =
    let orig1 = t1
    let orig2 = t2
    let t1 = subst.Resolve t1 |> normalize ToNested
    let t2 = subst.Resolve t2 |> normalize ToNested
    // sec. 5.3 fast path: post-normalization, structural equality holds
    // iff the surface forms are equivalent under the mixed-slot identity --
    // catches concrete-on-both-sides cases without entering recursion.
    if t1 = t2 then Ok ()
    else
    match t1, t2 with
    | IRTInfer id1, IRTInfer id2 when id1 = id2 -> Ok ()
    | IRTInfer id, ty | ty, IRTInfer id ->
        if occursIn id ty then Error (Other "Infinite type detected")
        else
            // A var-to-var bind must carry the HM-polymorphic flag to the
            // SURVIVING var. `polymorphicIds` is populated only at
            // LookupOrCreateTypeVar; every arm below ends in `Bind(id, ty)`,
            // so when `ty` is itself a var, `id`'s flag would otherwise die
            // with the bind and Zonk would default the survivor to Float64.
            // Seam that fires this: an UNANNOTATED return type -- unify of the
            // body's boundary `T` against the decl's fresh retType var demoted
            // `T` to a monomorphic Float64 function silently.
            (match ty with
             | IRTInfer id2 -> subst.CopyPolymorphic(id, id2)
             | _ -> ())
            // Rank lower bound (stage-2 deduction): validate/propagate before
            // any bind. A too-low-rank array or a scalar violates the bound;
            // another inference var inherits it (max-join).
            let rankBoundViolation : TypeError option =
                match subst.GetRankLowerBound(id) with
                | Some k when k > 0 ->
                    (match ty with
                     | ArrayElem arr ->
                         if arr.IndexTypes.Length < k then
                             Some (RankBoundViolation (k, $"a rank-{arr.IndexTypes.Length} array"))
                         else None
                     | IRTInfer id2 ->
                         subst.AddRankLowerBound(id2, k)
                         None
                     | IRTScalar _ ->
                         Some (RankBoundViolation (k, "a scalar"))
                     | _ -> None)
                | _ -> None
            match rankBoundViolation with
            | Some e -> Error e
            | None ->
            // Check arity invariant: T^k must unify with rank-k array
            match subst.GetArityConstraint(id) with
            | Some k when k > 0 ->
                match ty with
                | ArrayElem arr when arr.IndexTypes.Length = k ->
                    subst.Bind(id, ty); Ok ()
                | IRTInfer id2 ->
                    // Binding two inference vars: the invariant travels WITH
                    // the bind. `id` disappears behind `id2`, so unless `id2`
                    // inherits the pin every later `GetArityConstraint` reads
                    // None and the `T^k` annotation goes VACUOUS -- the same
                    // survivor problem `CopyPolymorphic` (above), the rank
                    // lower bound (one arm up) and `CopyLiteralDefault`
                    // (below) each already solve. There is nothing to defer
                    // to: no seam ever revisits a var-to-var bind.
                    //
                    // Seam that fires this is the one named in the polymorphic
                    // note above -- an UNANNOTATED return type, whose fresh
                    // retType var swallows the signature's `T^k` var. Measured:
                    // `function addrow(a: T^1, b: T^1) = a + b` used as a fold
                    // kernel (`reduce(g, addrow)`) had its params specialize to
                    // SCALARS and silently computed row sums, while the same
                    // function spelled `-> T^1` (retType = the same var, so no
                    // bind, so no loss) refused the scalar correctly.
                    (match subst.GetArityConstraint(id2) with
                     | Some k2 when k2 <> k ->
                        Error (Other $"a `^{k}` type variable cannot unify with a `^{k2}` one: the caret pins an EXACT rank, so the two annotations describe different shapes")
                     | _ ->
                        subst.CopyArityConstraint(id, id2)
                        subst.Bind(id, ty); Ok ())
                | _ ->
                    // `ppIRType`, not `%A`: this arm's raw-union rendering
                    // ("got IRTScalar ETInt64") leaked F# constructor names
                    // into user-facing output, and the fold-kernel seam above
                    // now routes ordinary programs here.
                    Error (Other ($"a `^{k}` type variable is a rank-{k} array, but this position supplies {(ppIRType ty)} -- the caret is a rank CLAIM, not a shorthand, so drop it (`T`) where the value is an element rather than an array. In fold-kernel position (`reduce(A, f)`) the parameters ARE the element type: a `T^1` kernel fits an array of rank-1 elements, not a rank-1 array of scalars."))
            | _ ->
                match subst.GetLiteralDefault(id) with
                | Some litE ->
                    // `id` is a literal var (numeric/bool kind): may WIDEN to a
                    // compatible scalar or DEFER to another var, but never binds
                    // to an array (routed through checkExpr's fill coercion) or a
                    // non-numeric type, and never narrows (TypeCheck.fs:6016).
                    match ty with
                    | IRTInfer id2 ->
                        match subst.GetLiteralDefault(id2) with
                        | Some litE2 ->
                            // Two literal vars: the survivor's kind is the wider.
                            match promoteElemType litE litE2 with
                            | Some p -> subst.Bind(id, ty); subst.SetLiteralDefault(id2, p); Ok ()
                            | None -> Error (TypeMismatch (t1, t2))
                        | None ->
                            // Defer to a plain var; carry the seed to the survivor.
                            subst.Bind(id, ty); subst.CopyLiteralDefault(id, id2); Ok ()
                    | IRTScalar targetE ->
                        match promoteElemType litE targetE with
                        | Some p when p = targetE -> subst.Bind(id, ty); Ok ()  // widen-only
                        | _ -> Error (TypeMismatch (t1, t2))                    // narrow / incompatible
                    | _ ->
                        // arrays (-> fill coercion), tuples, funcs, idx/nat, strings...
                        Error (TypeMismatch (t1, t2))
                | None ->
                    subst.Bind(id, ty); Ok ()
    | IRTScalar e1, IRTScalar e2 when e1 = e2 -> Ok ()
    // Scalar unification: two concrete-and-different primitives are a real
    // type error, not promotable -- accepting via `promoteElemType` would
    // let an Int64-annotated param silently unify with Float64, truncating
    // values in C++. The rebind path stays alive only when at least one
    // side is an inference variable; with both concrete it would rewrite
    // types without binding anything -- silent acceptance.
    | IRTScalar e1, IRTScalar e2 ->
        match promoteElemType e1 e2 with
        | Some promoted ->
            let leaf1 = findLeafInferScalar subst orig1
            let leaf2 = findLeafInferScalar subst orig2
            match leaf1, leaf2 with
            | None, None ->
                // Both concrete: promoteElemType's "compatible wider type"
                // is a value-promotion fact (used by binop result-type
                // inference), not a type-equality fact. Refuse.
                Error (TypeMismatch (t1, t2))
            | _ ->
                let promotedTy = IRTScalar promoted
                leaf1 |> Option.iter (fun id -> subst.Bind(id, promotedTy))
                leaf2 |> Option.iter (fun id -> subst.Bind(id, promotedTy))
                Ok ()
        | None ->
            Error (TypeMismatch (t1, t2))
    | ArrayElem a1, ArrayElem a2 ->
        // ArrayElem matches IRTArrow with all-SIdx or all-SIdxVirt slots.
        // Rank must match, virtual/stored character must match.
        if a1.IndexTypes.Length <> a2.IndexTypes.Length || a1.IsVirtual <> a2.IsVirtual then
            Error (TypeMismatch (t1, t2))
        else
            // Per-index compatibility: shared indexPairIncompatible
            // predicate (see doc above unify) -- rank, tags, symmetry,
            // extents never compared.
            let indexMismatch =
                List.zip a1.IndexTypes a2.IndexTypes
                |> List.indexed
                |> List.tryFind (fun (_, (i1, i2)) -> indexPairIncompatible i1 i2)
            match indexMismatch with
            // A rank disagreement gets its own diagnostic: "expected X, got
            // Y" reads as a puzzle when the two slots hold the same cell
            // count and differ only in index-component span.
            | Some (slot, (i1, i2)) when indexRankDiffers i1 i2 ->
                Error (IndexRankMismatch ($"index slot {slot}",
                                          ppIndexType i1, max 1 i1.Rank,
                                          ppIndexType i2, max 1 i2.Rank))
            | Some _ -> Error (TypeMismatch (t1, t2))
            | None ->
                // ElemType is IRType, so this falls through to unify:
                // inference vars bind, primitives promote where compatible,
                // genuine mismatches error (same silent-narrowing hazard
                // scalar unification guards against).
                unify subst a1.ElemType a2.ElemType
    | IRTTuple ts1, IRTTuple ts2 when ts1.Length = ts2.Length ->
        List.zip ts1 ts2 |> List.fold (fun acc (a, b) ->
            acc |> Result.bind (fun () -> unify subst a b)) (Ok ())
    | FuncElem (a1, r1), FuncElem (a2, r2) when a1.Length = a2.Length ->
        // FuncElem matches IRTArrow with all-SVal slots (the unified function form).
        // Slot-by-slot arg unification followed by return-type unification.
        List.zip a1 a2 |> List.fold (fun acc (a, b) ->
            acc |> Result.bind (fun () -> unify subst a b)) (Ok ())
        |> Result.bind (fun () -> unify subst r1 r2)
    | IRTUnit, IRTUnit -> Ok ()
    | IRTNamed n1, IRTNamed n2 when n1 = n2 -> Ok ()
    | IRTLoop l1, IRTLoop l2 when l1.Kind = l2.Kind -> Ok ()
    | IRTComputation t1, IRTComputation t2 -> unify subst t1 t2
    // Two parameter packs unify by their base (element) type; the arity
    // variable NAME is diagnostics-only (each Poly occurrence mints a
    // fresh `r%d`) -- lets a recursive call on `tail` (`Poly<base, fresh>`)
    // flow into a parameter declared `Poly<base, r_orig>`.
    | IRTPoly (b1, _), IRTPoly (b2, _) -> unify subst b1 b2
    | IRTNat _, IRTNat _ -> Ok ()
    // The tag WILDCARD `Base<_>` (IRefAny), ahead of the strict
    // IRTIdxTagged and IRTUnitAnnotated arms so top-down match order makes
    // it permissive. Matches any tagged value, any unit-annotated value,
    // or a bare untagged scalar -- only the VALUE types unify; the
    // tag/unit is what the parameter declined to constrain. It does NOT
    // absorb the concrete tag it met: erases to its inner type at codegen,
    // carrying no more tag guarantee than an untagged int
    // (checkArrayIndexTags treats both the same). Propagating the tag
    // needs a rebindable inference variable behind the parameter.
    | IRTIdxTagged (wInner, IRefAny), other
    | other, IRTIdxTagged (wInner, IRefAny) ->
        unify subst wInner (stripTagAnnotation other)
    // IRTIdxTagged unification (parallel to IRTUnitAnnotated below): inner
    // types must unify (an int64 tag won't match float-tagged, even with
    // the same IdxRef); IdxRefs must be structurally equal (named needs
    // name match, anon needs nominalId match, extent ignored; mixed
    // named/anon never compatible). No asymmetric arms: strict, unlike
    // IRTUnitAnnotated -- a plain int can't flow to Nat<I> without a cast,
    // enforcing sec. 4.18.3's "untyped literal" rule.
    | IRTIdxTagged (inner1, r1), IRTIdxTagged (inner2, r2) ->
        let refMatch =
            match r1, r2 with
            | IRefNamed n1, IRefNamed n2 when n1 = n2 -> true
            | IRefAnon (id1, _), IRefAnon (id2, _) when id1 = id2 -> true
            | _ -> false
        if refMatch then unify subst inner1 inner2
        else Error (TypeMismatch (t1, t2))
    // IRTDist unification: strict, like IRTIdxTagged -- no asymmetric
    // arms, so a bare tuple of arrays never flows into a Dist (only the
    // dist intrinsic and dist-typed operators produce Dist values).
    // Carried orders must be EQUAL (different stochastic order is a type
    // error, not runtime); axes agree positionally under the same rule as
    // ArrayElem index types; element types unify recursively.
    | IRTDist (o1, e1, ax1), IRTDist (o2, e2, ax2) ->
        if o1 <> o2 || ax1.Length <> ax2.Length then
            Error (TypeMismatch (t1, t2))
        else
            let axisMismatch =
                List.zip ax1 ax2
                |> List.indexed
                |> List.tryFind (fun (_, (i1, i2)) -> indexPairIncompatible i1 i2)
            match axisMismatch with
            | Some (axis, (i1, i2)) when indexRankDiffers i1 i2 ->
                Error (IndexRankMismatch ($"Dist axis {axis}",
                                          ppIndexType i1, max 1 i1.Rank,
                                          ppIndexType i2, max 1 i2.Rank))
            | Some _ -> Error (TypeMismatch (t1, t2))
            | None -> unify subst e1 e2
    // IRTArrow: slot-by-slot unification. Slot kinds (SIdx/SIdxVirt/SVal)
    // must agree; SIdx/SIdxVirt need matching index identity (id, tag);
    // SVal recurses. Result types also unify; Identity field is metadata.
    | IRTArrow (s1, r1, _), IRTArrow (s2, r2, _) when s1.Length = s2.Length ->
        let unifySlot acc (sa, sb) =
            acc |> Result.bind (fun () ->
                match sa, sb with
                | SVal ta, SVal tb -> unify subst ta tb
                | SIdx ia, SIdx ib | SIdxVirt ia, SIdxVirt ib ->
                    if ia.Id = ib.Id && ia.Tag = ib.Tag then Ok ()
                    else Error (TypeMismatch (t1, t2))
                | _ -> Error (TypeMismatch (t1, t2)))
        List.zip s1 s2 |> List.fold unifySlot (Ok ())
        |> Result.bind (fun () -> unify subst r1 r2)
    // Unit-annotated types: when BOTH sides carry a unit signature they
    // must agree -- this is what makes bindings and function boundaries
    // unit-checked. The asymmetric arms stay permissive: bare values flow
    // freely into/out of annotated positions (how units are introduced).
    | IRTUnitAnnotated (inner1, u1), IRTUnitAnnotated (inner2, u2) ->
        // Inner types first, then the signature check. The order matters only
        // for diagnostics: a bare-quantity ascription (`t : speed`) carries a
        // FRESH inner var, and binding it before a signature rejection lets
        // the error render `Float64<speed>` rather than `T?n<speed>`.
        // (unitCompatible also demands NOMINAL agreement: same quantity, or
        // at least one side structural.)
        unify subst inner1 inner2 |> Result.bind (fun () ->
            if not (unitCompatible u1 u2) then
                Error (UnitMismatch ("assignment", ppUnitSig u1, ppUnitSig u2))
            // Same dims, different MAGNITUDE (`day` into a `second` slot).
            // Convertible, but unify is a pure type-level relation with no
            // expression in hand to multiply, so it cannot bridge the factor
            // here -- reject and name it rather than let the raw number
            // through. The scalar +/-/comparison seam is where a conversion
            // is actually inserted.
            elif not (unitSameScale u1 u2) then
                Error (Other (sprintf
                        "assignment relates %s and %s: same dimensions, but magnitudes differing by the factor %s. Scale the value explicitly, or annotate it as %s"
                        (ppUnitSig u1) (ppUnitSig u2)
                        (ppUnitScale (unitConversionFactor u2 u1)) (ppUnitSig u1)))
            else Ok ())
    | IRTUnitAnnotated (inner, _), other | other, IRTUnitAnnotated (inner, _) -> unify subst inner other
    | _ -> Error (TypeMismatch (t1, t2))

// 1b. Let-Generalization (Hindley-Milner Polymorphism)

/// A type scheme: a type with universally quantified inference variables.
/// E.g., `let id = lambda(x: T) -> x` gets scheme `forall {#10001}. #10001 -> #10001`.
type TypeScheme = {
    QuantifiedVars: int list   // Inference variable IDs that are universally quantified
    Body: IRType               // The type (resolved), with quantified vars still as IRTInfer
}


/// Collect free (unresolved) inference variable IDs in a type.
let rec freeInferVars (subst: Subst) (ty: IRType) : Set<int> =
    match subst.Resolve ty with
    | IRTInfer id -> Set.singleton id
    | IRTScalar _ | IRTUnit | IRTNamed _ | IRTNat _ -> Set.empty
    | IRTTuple ts -> ts |> List.map (freeInferVars subst) |> Set.unionMany
    | IRTComputation t -> freeInferVars subst t
    | IRTPoly (t, _) -> freeInferVars subst t
    | IRTLoop lt ->
        Set.union
            (lt.ArrayTypes |> List.map (freeInferVars subst) |> Set.unionMany)
            (lt.KernelType |> Option.map (freeInferVars subst) |> Option.defaultValue Set.empty)
    | IRTUnitAnnotated (inner, _) -> freeInferVars subst inner
    | IRTIdxTagged (inner, _) -> freeInferVars subst inner
    | IRTDist (_, elem, _) -> freeInferVars subst elem
    | IRTArrow (slots, ret, _) ->
        let slotVars =
            slots |> List.map (function
                | SVal ty -> freeInferVars subst ty
                | SIdx _ | SIdxVirt _ -> Set.empty)
            |> Set.unionMany
        Set.union slotVars (freeInferVars subst ret)
    | IRTGroupKeys _ -> Set.empty

