// The inline-forms lifting pass: classification predicates (isInlineForm
// and friends), the liftChild family, liftExpr, and the module driver.
module Blade.IRLift

open Blade.Types
open Blade.IR

/// Predicate: is this an inline form that needs lifting when in a non-blessed
/// position? Excludes IRReduce (its own codegen handles inline forms via
/// IIFE; the array argument is what gets lifted, not reduce itself).
/// Includes IRReduceCompute (statement-shaped, no expression/IIFE rendering,
/// only emittable at a let-binding); IRCompute(IRApplyCombinator ...) (only
/// correct as a statement-form loop nest at a let-RHS -- the bare unwrapped
/// IRApplyCombinator is NOT lifted, being genuinely deferred with no
/// materialized value); IRMatmul (an intrinsic node from the math package's
/// in-place elaborator, so it reaches ordinary expression positions and must
/// be hoisted to materialize its pool -- IRGram is NOT included, entering
/// only via the `gram` keyword's let-RHS); IREigh (same elaborator, two
/// pools plus the naming tuple); and IRSolve (same elaborator, one fresh
/// rank-1 pool plus its LU scratch).
let isInlineForm (e: IRExpr) : bool =
    match e with
    | IRMask _ | IRSort _ | IRIntersect _ | IRUnion _ | IRUnique _
    | IRGroupBy _ | IRGroupKeys _ | IRGroupBucket _ | IRGroupSizes _ | IRSegments _ | IRUngroup _ | IRTranspose _ | IRDecompact _ | IRArrayNegate _ | IRArrayConjugate _
    | IRReduceCompute _ | IRMatmul _ | IREigh _ | IRSolve _ -> true
    | IRCompute (IRApplyCombinator _) -> true
    | _ -> false

/// Nodes whose ONLY emitter is `genBinding` -- they declare an extents table,
/// call `allocate<>`, and run a fill nest, which is a STATEMENT SEQUENCE.
/// `exprToCppCore` has no rendering for any of them: it answers either the
/// unhandled-node sentinel (BL7001) or a hand-written refusal (BL7004 for
/// `<|:>`). So wherever one of these lands in an expression position -- a
/// function-body `let`, a function RETURN, or a loop form's `Arrays` slot --
/// the only correct move is to bind it to a name and let `genBinding` emit it.
///
/// This is the single predicate behind all three of those routings. It lives
/// in IR.fs rather than beside CodeGen's `isMaterializedFreshArray` (where the
/// emitter-side neighbours are) for one hard reason: `liftExpr` below consumes
/// it, and IR.fs precedes CodeGen.fs in Blade.fsproj's compile order. One
/// definition read by both sides beats two that can drift apart.
///
/// Deliberate EXCLUSIONS, each of which would be a behavior change rather than
/// a gap closure:
///   * IRReduce / IRReduceCompute -- statement-shaped, but both already own
///     dedicated arms at every site this predicate feeds, and those arms do
///     more than bind-and-emit (the array-valued/scalar split for IRReduce,
///     `nestedTupleReturn` for the fused join). A catch-all that ran first
///     would silently drop that work.
///   * IRApplyCombinator / IRComposeApply -- DEFERRED forms with no name behind
///     them until a forcing site runs; they have their own arms for exactly
///     that reason.
///   * every view/projection form -- they render inline correctly today.
let isStatementShaped (e: IRExpr) : bool =
    match e with
    // Data-dependent cardinality forms.
    | IRMask _ | IRSort _ | IRUnique _ | IRIntersect _ | IRUnion _ -> true
    // Shape-changing / contraction forms.
    | IRTranspose _ | IRDecompact _ | IRStack _ | IRJoin _
    | IRGram _ | IRMatmul _ | IREigh _ | IRSolve _ -> true
    // Whole-array eager unary forms.
    | IRArrayNegate _ | IRArrayConjugate _ -> true
    // Grouping: the `group_keys` CSR tables and the two accessors that read
    // them back out. All four hang a name-suffix ABI off the binding's name.
    | IRGroupKeys _ | IRGroupBy _ | IRGroupBucket _ | IRGroupSizes _ | IRSegments _ | IRUngroup _ -> true
    // Array literals: extents table + allocate + per-element init.
    | IRArrayLit _ -> true
    // The DEFERRING family. `genBinding` answers these with a comment and a
    // DeferredComputations entry rather than code, so binding one of them
    // BARE registers a name with no declaration behind it. They must be
    // routed in their FORCED spelling -- see `forceDeferringForm`.
    | IRChoice _ | IRFallback _ | IRGuard _ | IRSequence _ -> true
    | _ -> false

/// The four forms whose emission `genBinding` DEFERS: it records the value in
/// `DeferredComputations` and emits only a `// <deferred ...>` comment, leaving
/// materialization to a later `|> compute` that reaches `genComputeBinding`.
///
/// A function body and a loop form's `Arrays` slot both LACK such a forcing
/// site -- the callee is the last scope that can force (a caller receives a
/// VALUE, never a lazy combinator), and a loop nest subscripts its operand by
/// name in the very statement it is built into. Binding one of these bare in
/// either position therefore reproduces exactly the `'__v27' was not declared`
/// failure that `genFuncBodyScoped`'s IRApplyCombinator arm now raises a loud
/// invariant about.
///
/// So we hoist the FORCED shape. This is also what keeps the hoist free of the
/// extents-ALIASING hazard: `isFreshPoolForm` documents that these four BORROW
/// an operand's `.extents` pointer, and a bare hoist would put a second
/// borrowing wrapper into a deterministic-dealloc frame that already plans to
/// free the lender. `IRCompute` routes to the materializing emitter instead,
/// which builds a real pool -- and, since this change, its own extents table.
///
/// `isDeferringForm` is the MEMBERSHIP test and `forceDeferringForm` the
/// transform. They are split because not every consumer wants the transform:
/// genBinding's `|> compute` peel has to SUBTRACT this family from
/// `isStatementShaped` (forcing them there would route them back to the emitter
/// that defers, making the compute a no-op), and subtracting a set is only
/// honest if it names the same set.
let isDeferringForm (e: IRExpr) : bool =
    match e with
    | IRChoice _ | IRFallback _ | IRGuard _ | IRSequence _ -> true
    | _ -> false

let forceDeferringForm (e: IRExpr) : IRExpr =
    if isDeferringForm e then IRCompute e else e

/// The CSR-TABLE grouping members of `isStatementShaped`: `group_keys` and the
/// `group_by` that reads it. Named for the same reason as `isDeferringForm` --
/// the one consumer that must subtract them (genBinding's `|> compute` peel,
/// where a grouping is not a spelling the surface produces and genComputeBinding
/// has no arm to fall back on) should say which set it is subtracting rather
/// than re-listing constructors.
///
/// The two ACCESSORS -- IRGroupBucket / IRGroupSizes -- are deliberately NOT
/// here. They materialize an ordinary array and peel like any other eager form.
let isGroupTableForm (e: IRExpr) : bool =
    match e with
    | IRGroupKeys _ | IRGroupBy _ | IRSegments _ -> true
    | _ -> false

/// `isStatementShaped` through an explicit `|> compute`. The user's own force
/// is the SAME routing problem, not a different one: `sequence(a, b) |> compute`
/// as a body let arrives as `IRCompute(IRSequence ...)`, which matches neither
/// the bare-node arms nor `IRCompute(IRApplyCombinator)`, so it fell to the
/// default arm's inline rendering and the IRSequence sentinel -- the identical
/// BL7001 the unwrapped spelling raised. The wrapper is passed THROUGH to
/// `genBinding` rather than peeled here: `genComputeBinding` is where the
/// deferring family's materializing emitters live, and genBinding's own
/// eager-peel arm handles the rest.
let isStatementShapedValue (e: IRExpr) : bool =
    match e with
    | IRCompute inner -> isStatementShaped inner
    | _ -> isStatementShaped e

/// The inline forms a loop form's `Arrays` slot AUTO-MATERIALIZES on the
/// codegen side. They are the blessed positions `liftExpr`'s header comment
/// refers to, and hoisting one would route it away from a path that already
/// works -- so they are subtracted from the `isStatementShaped` lift below.
/// The list is exactly what CodeGen's auto-materialize arm knows; anything
/// else in that slot falls through to an undeclared `arr<i>`.
let isArraysSlotAutoMaterialized (e: IRExpr) : bool =
    match e with
    | IRMask _ | IRIntersect _ | IRUnion _ | IRUnique _ -> true
    | _ -> false

/// The `Arrays`-slot half of `isStatementShaped`: statement-shaped, NOT already
/// auto-materialized there, and genuinely ARRAY-TYPED.
///
/// The type test is the same guard `isNestedLoopComputeArg` applies, and it is
/// load-bearing in one direction only -- it can never suppress a real hoist,
/// because a loop form's `Arrays` slot holds arrays by construction. What it
/// prevents is a hoist that would be actively harmful if the slot ever did hold
/// something else: minting a let for an `IRGroupKeys` would put a `gk` outside
/// the whitelist of blessed positions BL3017 enforces, turning a program that
/// compiled into one that is refused. IREigh is the same story from the other
/// side (tuple-typed, so no surface spelling reaches this slot). Keeping the
/// test here means neither has to be special-cased out of `isStatementShaped`,
/// where both belong for the function-body and RETURN routings.
let isStatementShapedArraysArg (e: IRExpr) : bool =
    isStatementShaped e
    && not (isArraysSlotAutoMaterialized e)
    && (match typeOf e with ArrayElem _ -> true | _ -> false)

/// A loop-form array operand (in a method_for / apply-combinator / compose-apply
/// `Arrays` list) that is itself a forced or inline elementwise computation --
/// e.g. the left input `A * B` of a chained positional op `A * B * C`, which
/// lowers to `IRCompute(IRApplyCombinator ...)`. Unlike the blessed inline forms
/// (mask/intersect/union/unique), these have NO codegen-side auto-materialize
/// path, so the loop-nest builder names them `arr0` and reads an array it never
/// declared (`error: 'arr0' was not declared in this scope`). They must be
/// hoisted to their own let-RHS so codegen materializes each into a real temp
/// before the outer loop consumes it -- exactly as writing the intermediate
/// `let` by hand would. Deliberately narrow: it does NOT list the blessed inline
/// forms, so their existing auto-materialize path stays untouched.
///
/// Array-typed APPLICATION and partial-INDEX operands are included for the same
/// reason -- `f(x) + g(x)` (both operands calls) and `m(0) + m(1)` (both operands
/// row views) equally leave the nest with no named array to read. A call operand
/// must also be evaluated exactly once rather than re-invoked per element. This
/// mirrors the `materialize` helper in `lowerArrayBinOpsModule`, which covers the
/// raw-`IRBinOp` half of the same problem. Fully-indexed reads are scalar, so
/// they fail the array-type test and stay inline.
let internal isNestedLoopComputeArg (e: IRExpr) : bool =
    let isArrayTyped () =
        match typeOf e with
        | ArrayElem _ -> true
        | _ -> false
    match e with
    | IRCompute _ -> true
    | IRApp (IRObjectFor _, _, _) -> true
    | IRApp _ | IRIndex _ -> isArrayTyped ()
    // `m.matmul(A, B) * 2.0` puts a matmul directly in a loop form's Arrays
    // list. As an intrinsic node -- not a synthesized function-call IRApp,
    // which the line above already hoists -- it needs its own entry or the
    // nest reads an `arr<i>` it never declared.
    | IRMatmul _ -> true
    // `m.solve(A, b) * 2.0` is the same shape as the matmul line above, and
    // ARRAY-typed (unlike eigh), so it genuinely can occupy an `Arrays` slot.
    | IRSolve _ -> true
    // `gram(A, B) * 2.0` -- the same shape again, and its omission was a plain
    // oversight rather than a decision. `isInlineForm`'s header says IRGram
    // "enters only via the `gram` keyword's let-RHS", but that premise is
    // false for a CONSUMED gram: the operand slot of an elementwise op is an
    // ordinary expression position, and the nest read an `arr0` it never
    // declared. IRGram allocates one fresh pool with its own extents table, so
    // it hoists exactly like IRMatmul beside it.
    | IRGram _ -> true
    // IREigh is deliberately ABSENT, and its absence is a decision rather than
    // an omission: an eigh node is TUPLE-typed, and a loop form's `Arrays` slot
    // holds arrays. There is no surface spelling that puts a tuple where the
    // nest expects an array -- the destructured `Q` / `LAM` are what reach a
    // loop, and those are ordinary IRVars by then. Adding an arm here would be
    // a dead branch that reads as if it guarded something.
    | _ -> false

/// An INLINE array literal sitting directly in a loop form's `Arrays` list --
/// e.g. the right operand of `yr - [2.0, -14.0]`. Same gap as
/// `isNestedLoopComputeArg`: the blessed-position exemption assumes codegen's
/// auto-materialize covers the slot, but that arm only knows the inline
/// mask/intersect/union/unique forms. An IRArrayLit falls through to the
/// `arr<i>` placeholder and the nest peels an identifier that was never
/// declared. Hoisting it to its own let-RHS routes it through the ordinary
/// array-literal emission, exactly as let-binding it by hand would.
let internal isInlineArrayLitArg (e: IRExpr) : bool = e.IsIRArrayLit

/// An ARRAY-VALUED SELECT sitting directly in a loop form's `Arrays` list --
/// the third instance of the same gap as `isInlineArrayLitArg`. A recursive
/// array's out-of-prefix lag read desugars to exactly this shape: TypeCheck's
/// `rewritePrefixReads` hoists the clamped row read into a `__lag<k>_` binding
/// and leaves the bounds SELECT inline (`if n - 3 >= 0 then __lag0_m else
/// __zs0_m`), so an elementwise use like `0.5 * prefix(n - 3)` puts the whole
/// `if` in a loop form's array slot. Codegen's auto-materialize knows only the
/// blessed mask/intersect/union/unique forms, so the select fell through to the
/// `arr<i>` placeholder and the nest peeled an identifier it never declared.
///
/// Hoisting the select to its own let-RHS is what routing the same expression
/// through a helper function (`method_for(zip(...)) <@> ... |> compute`) already
/// does. It is only half the fix: the binding this mints must also be DECLARED
/// as an `Array<T, N>` rather than a raw `promote<>::type` pointer, which is
/// CodeGen's `producesWrapperOf` IRIf arm. Changing one without the other trades
/// the undeclared `arr<i>` for a pointer with no `.extents`.
///
/// Scalar selects are untouched -- they render inline as an ordinary ternary
/// and never occupy an array slot.
let internal isArrayValuedSelect (e: IRExpr) : bool =
    match e with
    | IRIf _ ->
        match typeOf e with
        | ArrayElem _ -> true
        | _ -> false
    | _ -> false

/// A LOOP FORM sitting directly in another loop form's `Arrays` list -- the
/// fourth instance of the same gap as `isArrayValuedSelect`. A CHAINED MAP
/// (`(A <@> f) <@> g`, the pipeline shape) lowers to exactly this: the inner
/// `<@>` is an apply-combinator occupying the outer one's array slot. Codegen's
/// auto-materialize knows only the blessed mask/intersect/union/unique forms, so
/// the inner map fell through to the `arr<i>` placeholder -- the outer nest then
/// read `arr0` for an intermediate that was never emitted, for ANY inner kernel
/// (lambda or operator section alike).
///
/// Hoisting it to its own let-RHS materializes the intermediate, which is
/// precisely what splitting the chain into two `let`s by hand already does.
///
/// Only INLINE forms are caught. A deferred map reaches a consuming slot as an
/// `IRVar` pointing at its let-binding, so the fusion paths that depend on
/// deferred operands staying unmaterialized -- `<&!>`'s shared-traversal
/// repointing above all -- never see this predicate.
let internal isNestedLoopFormArg (e: IRExpr) : bool =
    match e with
    | IRApplyCombinator _ | IRComposeApply _ -> true
    | _ -> false

/// Peel any IRLet chain that descendant lifts produced.
/// When a sub-expression's lift wraps it in `IRLet(id, v, IRLet(...,inner))`,
/// the chain shouldn't be visible to the parent context (e.g., an outer
/// IRArrayLit's element list, or a struct field value). Peeling pulls the
/// chain out as a list of bindings; the caller's wrapLets re-wraps them at
/// the appropriate enclosing level.
///
/// Without peeling, lifts produced by descendant calls would appear as
/// siblings of other elements in multi-child contexts, breaking codegen
/// (e.g., the genArrayLiteral walker treats IRLet as a leaf and emits an
/// IIFE that doesn't know how to render an IRArrayLit inline).
let peelLetChain (e: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let rec loop acc e =
        match e with
        | IRLet (id, v, body) -> loop (acc @ [(id, typeOf v, v)]) body
        | _ -> (acc, e)
    loop [] e

/// Predicate: is this an IRFieldAccess whose result type is an array? Such
/// accesses need to be hoisted to a let-RHS so codegen can synthesize the
/// companion `_extents` (and `_lens` for ragged) array -- without a let-RHS
/// drain point, the field access expression `t.samples` produces a pointer
/// but no shape information, breaking any consumer that expects an extents
/// sibling (kernel args, reduce, method_for, etc.).
let internal isArrayFieldAccess (e: IRExpr) : bool =
    match e with
    | IRFieldAccess _ ->
        match typeOf e with
        | ArrayElem _ -> true
        | _ -> false
    | _ -> false

/// Lift a single child if it's an inline form. Returns either ([], child)
/// for the no-rewrite case, or ([(id, ty, child)], IRVar(id, ty)) for the
/// lifted case.
///
/// Also peels any IRLet chain the descendant produced, so the chain
/// bindings hoist alongside any new lift binding to the caller's wrap
/// point.
let liftChild (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    if isInlineForm inner then
        let id = builder.FreshId()
        let ty = typeOf inner
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    elif isArrayFieldAccess inner then
        // Hoist `t.samples` (when samples is array-typed) into a
        // let-RHS so codegen can synthesize `<bound_name>_extents`.
        let id = builder.FreshId()
        let ty = typeOf inner
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    else
        (peeled, inner)

/// Like `liftChild`, but additionally lifts IRArrayLit. Used at sites
/// where an inline IRArrayLit can't render (struct field values, function
/// args). NOT used at IRArrayLit element positions -- there, the inner
/// IRArrayLit must remain so the genArrayLiteral walker sees full nesting
/// depth (otherwise dims and per-leaf indexing break).
let liftChildIncludingArrayLit (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    match inner with
    | IRArrayLit (_, arrTy) ->
        let id = builder.FreshId()
        let ty = mkArrayLike arrTy
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    | e when isInlineForm e ->
        let id = builder.FreshId()
        let ty = typeOf e
        (peeled @ [(id, ty, e)], IRVar (id, ty))
    | e when isArrayFieldAccess e ->
        // Same hoisting as liftChild, so struct field values and
        // function args carrying `t.samples` get the same treatment.
        let id = builder.FreshId()
        let ty = typeOf e
        (peeled @ [(id, ty, e)], IRVar (id, ty))
    | e -> (peeled, e)

/// Like `liftChild`, but ALSO hoists array-typed applications, partial index
/// reads and forced computations -- i.e. everything `isNestedLoopComputeArg`
/// covers. Used for the LINALG intrinsic operands (`gram`, `matmul`): their
/// emission spells each operand's C++ text more than once (`X.extents[0]`,
/// `X.extents[1]`, `X.data`), so an unhoisted call operand would be
/// re-invoked per occurrence, allocating a fresh array each time and handing
/// the contraction two different (if equal-valued) pools.
let liftChildEvaluatedOnce (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    if isInlineForm inner || isNestedLoopComputeArg inner || isArrayFieldAccess inner then
        let id = builder.FreshId()
        let ty = typeOf inner
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    else
        (peeled, inner)

/// Like `liftChild`, but ALSO hoists a SYNTHESIZED ELEMENTWISE LOOP
/// APPLICATION -- `IRApp(IRObjectFor ..., [A])`, what an array/scalar
/// broadcast `x - s` lowers to when TypeCheck's `method_for(A) <@>
/// lambda(__bx) -> ...` re-synthesis does not fire (it is skipped when the
/// scalar operand's type is still an unresolved inference variable, e.g.
/// `reduce(x, (+)) / n` with an Int64 `n`). That form materializes only from a
/// let-RHS -- genBinding's IRApp(IRObjectFor) arm and genFuncBody's
/// hoistLoopApps are the two expansion sites -- so left inline in a consuming
/// operand slot it reaches exprToCpp and renders as the
/// LOOP_OBJECT_USED_AS_VALUE sentinel.
///
/// Deliberately NARROWER than `liftChildEvaluatedOnce`: it adds only this one
/// shape, not the whole `isNestedLoopComputeArg` family. In particular a bare
/// `IRCompute` operand is left alone -- a forced FUNCTOR MAP (`exp <$> (L <@>
/// k)`) still has to reach the consumer whole, since hoisting it splits the
/// wrapper off the loop it wraps and the consumer then reads a binding that
/// was never emitted under that id.
let liftChildIncludingLoopApp (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    match inner with
    | IRApp (IRObjectFor _, _, _) ->
        let id = builder.FreshId()
        let ty = typeOf inner
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    // A BARE range in a by-name operand slot (`reduce(0..n, (+))`, prodsum,
    // whole-array negate): hoist it to its own let-RHS so the consumer
    // subscripts a materialized binding (genRangeBinding) -- exactly what
    // writing the intermediate `let` by hand does. Ranges are deliberately
    // NOT `isInlineForm`: a method_for/nest operand slot must keep them
    // virtual so the nest peels them as induction values, and those slots
    // never route through this helper. The type is spelled here rather than
    // taken from `typeOf` -- IRRange sits in typeOf's IntValued tier (its
    // ELEMENT under peeling), which would type the hoisted binding as a
    // scalar and starve the fold of its extents.
    | IRRange (ixs, _) ->
        let id = builder.FreshId()
        let ty = mkArrayLike { ElemType = IRTScalar ETInt64; IndexTypes = ixs; IsVirtual = false; Identity = None }
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    | _ ->
        let (b, e) = liftChild builder inner
        (peeled @ b, e)

/// `liftChildIncludingLoopApp`, plus the still-DEFERRED combinator forms.
///
/// Used only for `prodsum`'s operand slots, and for the reason that makes them
/// different from every other consuming position: the fused IIFE subscripts each
/// operand BY NAME (`__ps += a[__pt] * e[__pt]`), so an operand has to be a
/// binding, never an expression. A bare `sin <@> a` sitting there is genuinely
/// deferred -- it has no materialized value and no name -- and `exprToCpp`
/// answers with the UNEVALUATED_COMPUTATION_USED_AS_VALUE sentinel, which is a
/// diagnostic STRING spliced straight into the subscript position. That is the
/// one thing the by-name rule may never do: force it or fuse it, never splice.
///
/// Hoisting is the force. Minting `let __vN = <combinator>` puts the operand on
/// exactly the road a hand-written intermediate `let` takes, and the existing
/// deferred-forcing machinery finishes the job from there -- module level via
/// `ctx.DeferredComputations` + `collectDeferredPositionalReads` (whose IRProdSum
/// arm already notes every operand), function-body level via `forcedDeferredIds`,
/// which is seeded from the same collector. The RHS is carried over UNCHANGED
/// rather than wrapped in `IRCompute`: a wrap would only be recognised for the
/// IRApplyCombinator half (`isInlineForm`), leaving `IRComposeApply` -- a
/// `>>@`-composed pipeline applied with `<@>` -- to splice the sentinel exactly
/// as before, which it did even when the user wrote `|> compute` themselves.
let liftDeferredOperand (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    match inner with
    | IRApplyCombinator _ | IRComposeApply _
    | IRCompute (IRApplyCombinator _) | IRCompute (IRComposeApply _) ->
        let id = builder.FreshId()
        let ty = typeOf inner
        (peeled @ [(id, ty, inner)], IRVar (id, ty))
    | _ ->
        let (b, e) = liftChildIncludingLoopApp builder inner
        (peeled @ b, e)
/// Lift an ARGUMENT of a synthesized elementwise loop application
/// (`IRApp(IRObjectFor ..., args)`, what an array/scalar broadcast `x - s`
/// lowers to). Its emitter -- `genObjectForApplication` -- spells each operand
/// by looking the arg up as an `IRVar` in `ctx.VarNames` and falling back to
/// the placeholder `arr<i>`, exactly like a loop form's `Arrays` list, and it
/// has no auto-materialize arm at all. So an arg that is not already a named
/// binding leaves the nest reading an identifier that was never declared
/// (`error: 'arr0' was not declared in this scope`).
///
/// The failing shape is an INLINE ROW VIEW: `a(i) - mean(a(i))` inside a kernel
/// body, where the broadcast's array operand is the partial index `a(i)` rather
/// than a rank-1 parameter. Naming it in a `let` is what writing the
/// intermediate binding by hand already does.
///
/// The predicate set is deliberately the SAME one the `IRMethodFor` /
/// `IRApplyCombinator` / `IRComposeApply` arms apply to their `Arrays` slots
/// (`isArrayFieldAccess`, `isNestedLoopComputeArg`, `isInlineArrayLitArg`,
/// `isArrayValuedSelect`, `isNestedLoopFormArg`, `isStatementShapedArraysArg`)
/// -- the operand-naming rule is shared, so the hoisting rule has to be -- PLUS
/// `isInlineForm`, which the loop forms exempt only because codegen
/// auto-materializes mask/intersect/union/unique in an `Arrays` slot and this
/// emitter does not.
///
/// `isStatementShapedArraysArg` is the fourth consumer of the one predicate the
/// three sibling arms already share, and it is here because "same predicate
/// set" was a claim this lane did not actually satisfy when it was added: a
/// hand-copied list omitted it, so `stack(x, y) * 2.0` in a function body still
/// reached `genObjectForApplication` as a bare IRStack and the nest read an
/// `arr0` nothing declared. Everything statement-shaped that is not already
/// covered above enters through that one name -- IRStack and IRJoin, and the
/// DEFERRING family (`<|:>` / sequence / guard / choice), which hoists in its
/// FORCED spelling for the reason `forceDeferringForm` gives: bound bare, its
/// emitter records a deferral and declares no name at all.
let liftLoopAppOperand (builder: IRBuilder) (child: IRExpr) : (IRId * IRType * IRExpr) list * IRExpr =
    let (peeled, inner) = peelLetChain child
    let needsName =
        match inner with
        | IRArrayLit _ -> true
        | e ->
            isInlineForm e || isArrayFieldAccess e || isNestedLoopComputeArg e
            || isInlineArrayLitArg e || isArrayValuedSelect e || isNestedLoopFormArg e
            || isStatementShapedArraysArg e
    if needsName then
        let id = builder.FreshId()
        // IRArrayLit carries its array type in the node rather than through
        // `typeOf` (mirroring liftChildIncludingArrayLit).
        let ty = match inner with IRArrayLit (_, arrTy) -> mkArrayLike arrTy | _ -> typeOf inner
        // Identity for everything but the deferring family, exactly as in the
        // three sibling arms.
        (peeled @ [(id, ty, forceDeferringForm inner)], IRVar (id, ty))
    else
        (peeled, inner)

/// Lift a list of children, accumulating bindings.
let liftChildren (builder: IRBuilder) (children: IRExpr list) : (IRId * IRType * IRExpr) list * IRExpr list =
    children |> List.fold (fun (binds, acc) child ->
        let (b, c) = liftChild builder child
        (binds @ b, acc @ [c])) ([], [])

/// Wrap an expression with a sequence of let-bindings (innermost first).
let wrapLets (bindings: (IRId * IRType * IRExpr) list) (body: IRExpr) : IRExpr =
    List.foldBack (fun (id, _, v) acc -> IRLet (id, v, acc)) bindings body

/// Walk an expression bottom-up, hoisting any inline form found in a
/// non-blessed child position into a fresh IRLet wrapping the parent.
///
/// Note: when an inline form is itself the IRLet-RHS, we leave it alone
/// (that's the canonical position). When it's nested inside IRMethodFor's
/// or IRApplyCombinator's Arrays list, we also leave it -- codegen's
/// auto-materialize handles those positions.
let rec liftExpr (builder: IRBuilder) (expr: IRExpr) : IRExpr =
    match expr with
    // Leaves: nothing to do
    | IRLit _ | IRVar _ | IRParam _ | IRNth | IRZero
    | IRRange _ | IRVirtualReverse _ | IRArity _
    | IROpaqueExtent -> expr

    // Blessed positions: don't lift the value's top-level inline form; do
    // descend into both sides for nested cases.
    | IRLet (id, value, body) ->
        IRLet (id, liftExpr builder value, liftExpr builder body)

    // The inline forms themselves: descend into their sub-expressions
    // (which may contain further nested inline forms), but DO NOT lift
    // them at this point -- the parent's child slot will lift them if
    // needed.
    | IRMask (a, p) ->
        // Lift inline-form array arg so codegen sees a let-bound name in
        // the array slot (rather than another inline form it can't render
        // inside its own template). The predicate is a lambda -- not an
        // inline form -- so it just recurses normally.
        let a' = liftExpr builder a
        let p' = liftExpr builder p
        let (binds, aFinal) = liftChildIncludingArrayLit builder a'
        wrapLets binds (IRMask (aFinal, p'))
    | IRSort (a, k) ->
        let a' = liftExpr builder a
        let k' = liftExpr builder k
        let (binds, aFinal) = liftChildIncludingArrayLit builder a'
        wrapLets binds (IRSort (aFinal, k'))
    | IRIntersect (a, b) ->
        let a' = liftExpr builder a
        let b' = liftExpr builder b
        let (bindsA, aFinal) = liftChildIncludingArrayLit builder a'
        let (bindsB, bFinal) = liftChildIncludingArrayLit builder b'
        wrapLets (bindsA @ bindsB) (IRIntersect (aFinal, bFinal))
    | IRUnion (a, b) ->
        let a' = liftExpr builder a
        let b' = liftExpr builder b
        let (bindsA, aFinal) = liftChildIncludingArrayLit builder a'
        let (bindsB, bFinal) = liftChildIncludingArrayLit builder b'
        wrapLets (bindsA @ bindsB) (IRUnion (aFinal, bFinal))
    | IRUnique a ->
        let a' = liftExpr builder a
        let (binds, aFinal) = liftChildIncludingArrayLit builder a'
        wrapLets binds (IRUnique aFinal)
    | IRGroupBy (v, k) -> IRGroupBy (liftExpr builder v, liftExpr builder k)
    | IRGroupKeys ks -> IRGroupKeys (List.map (liftExpr builder) ks)
    | IRSegments _ -> expr
    | IRUngroup (g, src) -> IRUngroup (liftExpr builder g, src)
    // The gk operand is a bare name by construction (inferGroupBucket refuses
    // anything else), so there is nothing to lift out of it.
    | IRGroupBucket gk -> IRGroupBucket (liftExpr builder gk)
    | IRGroupSizes gk -> IRGroupSizes (liftExpr builder gk)

    // Contains returns a scalar Bool -- its array argument may be an inline
    // form that needs lifting (so codegen can read .extents off a named binding).
    | IRContains (arr, v) ->
        let arr' = liftExpr builder arr
        let v' = liftExpr builder v
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRContains (arrFinal, v'))

    // display.emit's payload is a plain String scalar -- nothing to lift, but
    // the child still recurses so an inline form INSIDE the payload
    // expression is handled like anywhere else.
    | IRDisplayEmit (h, q, data, m, idOpt) ->
        IRDisplayEmit (h, q, liftExpr builder data, m, Option.map (liftExpr builder) idOpt)

    // display.json_array consumes an ARRAY: recurse, then hoist an inline
    // form in the data slot into a let, exactly like IRReduce's array slot.
    | IRDisplayJson (r, data) ->
        let data' = liftExpr builder data
        let (binds, dataFinal) = liftChild builder data'
        wrapLets binds (IRDisplayJson (r, dataFinal))
    | IRDisplayNum data -> IRDisplayNum (liftExpr builder data)
    | IRDisplayStr data -> IRDisplayStr (liftExpr builder data)

    // Single-child consumers where the array slot can hold an inline form.
    //
    // Both use liftChildIncludingLoopApp, not liftChild: neither emitter has a
    // rendering for a synthesized elementwise loop application, so an operand
    // holding one (`x - s`, when TypeCheck's method_for re-synthesis is skipped
    // -- see that helper's note) reached exprToCpp and rendered as codegen's
    // LOOP_OBJECT_USED_AS_VALUE sentinel. Hoisting it to its own let-RHS is
    // what writing the intermediate `let` by hand already does.
    | IRReduce (arr, kernel, init) ->
        let arr' = liftExpr builder arr
        let kernel' = liftExpr builder kernel
        let init' = init |> Option.map (liftExpr builder)
        let (binds, arrFinal) = liftChildIncludingLoopApp builder arr'
        wrapLets binds (IRReduce (arrFinal, kernel', init'))
    | IRReduceCompute (comp, kernel, seed) ->
        // The computation child is a deferred combinator (apply/fusion
        // tree) -- never lift it into a binding (it has no materialized
        // value); recurse for nested inline forms in kernel arrays/seed.
        IRReduceCompute (liftExpr builder comp, liftExpr builder kernel, liftExpr builder seed)
    | IRProdSum args ->
        // Every operand slot can hold an inline form; lift each so codegen
        // reads .extents off named bindings. `liftDeferredOperand`, not
        // `liftChildIncludingLoopApp`: the fused IIFE reads EVERY operand by
        // name, so a still-deferred combinator has to be hoisted too (see its
        // doc -- the alternative is the sentinel spliced into the subscript).
        let (allBinds, finals) =
            args |> List.fold (fun (bs, fs) a ->
                let a' = liftExpr builder a
                let (b, aFinal) = liftDeferredOperand builder a'
                (bs @ b, fs @ [aFinal])) ([], [])
        wrapLets allBinds (IRProdSum finals)
    | IRExtent (arr, dim) ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRExtent (arrFinal, dim))
    | IRIndex (arr, idxs, identity) ->
        let arr' = liftExpr builder arr
        let idxs' = idxs |> List.map (liftExpr builder)
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRIndex (arrFinal, idxs', identity))
    | IRSlice (arr, dim, s, e) ->
        let arr' = liftExpr builder arr
        let s' = liftExpr builder s
        let e' = liftExpr builder e
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRSlice (arrFinal, dim, s', e'))
    | IRSubset (arr, dim, s, l) ->
        let arr' = liftExpr builder arr
        let s' = liftExpr builder s
        let l' = liftExpr builder l
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRSubset (arrFinal, dim, s', l'))
    | IRCurry (arr, idx, r) ->
        let arr' = liftExpr builder arr
        let idx' = liftExpr builder idx
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRCurry (arrFinal, idx', r))
    | IRTranspose (arr, d1, d2) ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRTranspose (arrFinal, d1, d2))
    | IRDecompact (arr, d) ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRDecompact (arrFinal, d))
    | IRHaloUnhash (w, o) ->
        // Scalar coordinate read; the window is a param var -- nothing to lift.
        IRHaloUnhash (liftExpr builder w, o)
    // Both linalg intrinsics use the evaluate-once lift: their emitters spell
    // each operand several times (extents + data), so a call/index operand has
    // to arrive as a named binding.
    | IRGram (l, r, s) ->
        let l' = liftExpr builder l
        let r' = liftExpr builder r
        let (bindsL, lFinal) = liftChildEvaluatedOnce builder l'
        let (bindsR, rFinal) = liftChildEvaluatedOnce builder r'
        wrapLets (bindsL @ bindsR) (IRGram (lFinal, rFinal, s))
    | IRMatmul (l, r) ->
        let l' = liftExpr builder l
        let r' = liftExpr builder r
        let (bindsL, lFinal) = liftChildEvaluatedOnce builder l'
        let (bindsR, rFinal) = liftChildEvaluatedOnce builder r'
        wrapLets (bindsL @ bindsR) (IRMatmul (lFinal, rFinal))
    | IREigh operand ->
        // Same evaluate-once lift, same reason: `materializeEighForm` spells
        // the operand THREE times (`.extents[0]`, and `.data` twice, bare and
        // via `pool_base`), so `eigh(f(A))` would otherwise re-invoke `f`
        // per occurrence, each call allocating a fresh pool.
        let operand' = liftExpr builder operand
        let (binds, opFinal) = liftChildEvaluatedOnce builder operand'
        wrapLets binds (IREigh opFinal)
    | IRSolve (a, b) ->
        // Same evaluate-once lift as matmul/eigh, and needed harder here:
        // `materializeSolveForm` spells A FIVE times (`.extents[0]` twice plus
        // `.data` in the copy-in loop and in either dispatch arm), so
        // `solve(f(A), b)` would otherwise re-invoke `f` per occurrence, each
        // call allocating a fresh pool -- and, worse, factorize a different
        // matrix than the one whose extent bounded the loops.
        let a' = liftExpr builder a
        let b' = liftExpr builder b
        let (bindsA, aFinal) = liftChildEvaluatedOnce builder a'
        let (bindsB, bFinal) = liftChildEvaluatedOnce builder b'
        wrapLets (bindsA @ bindsB) (IRSolve (aFinal, bFinal))
    // liftChildIncludingLoopApp, not liftChild, for the same reason as
    // IRReduce/IRProdSum above: the whole-array negate/conjugate emitters have
    // no rendering for a synthesized elementwise loop application, so
    // `-(A [-] B)` -- a bracketed op, which lowers to IRApp(IRObjectFor ..) --
    // reached exprToCpp and rendered as codegen's LOOP_OBJECT_USED_AS_VALUE
    // sentinel. Bare `A [-] B` already worked because a let-RHS is a blessed
    // position; only the wrapped form fell through.
    | IRArrayNegate arr ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChildIncludingLoopApp builder arr'
        wrapLets binds (IRArrayNegate arrFinal)
    | IRArrayConjugate arr ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChildIncludingLoopApp builder arr'
        wrapLets binds (IRArrayConjugate arrFinal)
    | IRReverse (arr, d) ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRReverse (arrFinal, d))
    | IRDiag arr ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRDiag arrFinal)
    | IRRank arr ->
        let arr' = liftExpr builder arr
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRRank arrFinal)
    | IRShift (arr, d, off, bm) ->
        let arr' = liftExpr builder arr
        let off' = liftExpr builder off
        let (binds, arrFinal) = liftChild builder arr'
        wrapLets binds (IRShift (arrFinal, d, off', bm))

    // Multi-child consumers (any arg can be an inline form)
    | IRApp (fn, args, retTy) ->
        // Function args may contain inline IRArrayLit (e.g.,
        // `f([1.0, 2.0, 3.0])`) which can't render inline. Use the
        // extended helper that lifts both inline forms and IRArrayLit.
        let fn' = liftExpr builder fn
        let args' = args |> List.map (liftExpr builder)
        // A SYNTHESIZED LOOP APPLICATION (`IRApp(IRObjectFor ...)`, the shape
        // an array/scalar broadcast lowers to) does not consume its args as
        // ordinary call arguments: they are the loop's ARRAY OPERANDS, named
        // positionally by genObjectForApplication. They need the loop-form
        // Arrays hoisting rule, not the call-argument one -- see
        // `liftLoopAppOperand`.
        let liftArg =
            match fn' with
            | IRObjectFor _ -> liftLoopAppOperand builder
            | _ -> liftChildIncludingArrayLit builder
        let (binds, argsFinal) =
            args' |> List.fold (fun (accB, accA) a ->
                let (b, a') = liftArg a
                (accB @ b, accA @ [a'])) ([], [])
        wrapLets binds (IRApp (fn', argsFinal, retTy))
    | IRJoin (arrs, dim) ->
        let arrs' = arrs |> List.map (liftExpr builder)
        let (binds, arrsFinal) = liftChildren builder arrs'
        wrapLets binds (IRJoin (arrsFinal, dim))
    | IRStack arrs ->
        let arrs' = arrs |> List.map (liftExpr builder)
        let (binds, arrsFinal) = liftChildren builder arrs'
        wrapLets binds (IRStack arrsFinal)
    | IRZip arrs ->
        let arrs' = arrs |> List.map (liftExpr builder)
        let (binds, arrsFinal) = liftChildren builder arrs'
        wrapLets binds (IRZip arrsFinal)
    | IRAlign (arrs, sp) ->
        let arrs' = arrs |> List.map (liftExpr builder)
        let (binds, arrsFinal) = liftChildren builder arrs'
        wrapLets binds (IRAlign (arrsFinal, sp))
    | IRTuple es ->
        let es' = es |> List.map (liftExpr builder)
        let (binds, esFinal) = liftChildren builder es'
        wrapLets binds (IRTuple esFinal)
    | IRComplex (re, im) ->
        let re' = liftExpr builder re
        let im' = liftExpr builder im
        let (binds, esFinal) = liftChildren builder [re'; im']
        match esFinal with
        | [reF; imF] -> wrapLets binds (IRComplex (reF, imF))
        | _ -> wrapLets binds (IRComplex (re', im'))  // unreachable; defensive
    | IRFma (a, b, c) ->
        let a' = liftExpr builder a
        let b' = liftExpr builder b
        let c' = liftExpr builder c
        let (binds, esFinal) = liftChildren builder [a'; b'; c']
        match esFinal with
        | [aF; bF; cF] -> wrapLets binds (IRFma (aF, bF, cF))
        | _ -> wrapLets binds (IRFma (a', b', c'))  // unreachable; defensive
    | IRArrayLit (es, ty) ->
        // Peel any IRLet chains from element results
        // (descendant lifts) and re-wrap them at THIS level. Don't lift an
        // ARRAY-TYPED peeled element further -- those are the structure the
        // genArrayLiteral walker measures (nested IRArrayLit for multi-dim,
        // row values for the rank-raising row map). Replacing one with an
        // IRVar would shorten computeArrayDims to just this level and break
        // extents/print/walker.
        //
        // A SCALAR LEAF that is an inline form is the opposite case, and must
        // be lifted. `[reduce(A * B, (+)), reduce(C * D, (+))]` puts an
        // IRReduceCompute -- accumulators plus a fused loop nest, with no
        // expression rendering anywhere -- in a leaf slot, where exprToCppCore
        // answers it with the BL7004 "must be bound to a let" refusal while the
        // interpreter evaluates it happily: a silent lane divergence that an
        // interp-first notebook only meets when it finally compiles. Hoisting
        // it is exactly what writing the intermediate `let` by hand does, and
        // exactly what CodeGen's own IRReduceCompute arms already do for the
        // body-let and RETURN positions.
        let es' = es |> List.map (liftExpr builder)
        let (binds, esPeeled) = es' |> List.fold (fun (accB, accE) e ->
            let (b, e') = peelLetChain e
            if isInlineForm e' && (match typeOf e' with ArrayElem _ -> false | _ -> true) then
                let id = builder.FreshId()
                let ty = typeOf e'
                (accB @ b @ [(id, ty, e')], accE @ [IRVar (id, ty)])
            else (accB @ b, accE @ [e'])) ([], [])
        wrapLets binds (IRArrayLit (esPeeled, ty))

    // BinOps: array-typed binops can have inline forms on either side.
    | IRBinOp (mode, op, l, r) ->
        let l' = liftExpr builder l
        let r' = liftExpr builder r
        let (lBinds, lFinal) = liftChild builder l'
        let (rBinds, rFinal) = liftChild builder r'
        wrapLets (lBinds @ rBinds) (IRBinOp (mode, op, lFinal, rFinal))
    | IRUnaryOp (op, e) ->
        let e' = liftExpr builder e
        let (binds, eFinal) = liftChild builder e'
        wrapLets binds (IRUnaryOp (op, eFinal))

    // Pass-through traversals (no lift at this level; descend into sub-expressions)
    | IRTupleProj (e, i, fl) -> IRTupleProj (liftExpr builder e, i, fl)
    | IRTupleCons (h, t) -> IRTupleCons (liftExpr builder h, liftExpr builder t)
    | IRTupleDecons e -> IRTupleDecons (liftExpr builder e)
    | IRFieldAccess (e, f) -> IRFieldAccess (liftExpr builder e, f)
    | IRStructLit (n, flds) ->
        // Nested element types: descend into each field expression,
        // then lift IRArrayLit and inline-form values into auto-let bindings.
        // Array literals are statement-level constructs (allocation; rendered
        // by genArrayLiteral, not exprToCpp), so they cannot appear inline as
        // struct field values. The auto-let pattern moves the literal to a
        // let-RHS where genArrayLiteral handles it; the field value becomes
        // an IRVar reference. liftChildIncludingArrayLit also peels any
        // IRLet chains the descent produced (so they hoist past this struct
        // lit to the next drain point).
        let flds' = flds |> List.map (fun (fn, fe) -> (fn, liftExpr builder fe))
        let (binds, fldsLifted) =
            flds' |> List.fold (fun (accBinds, accFlds) (fn, fe) ->
                let (b, fe') = liftChildIncludingArrayLit builder fe
                (accBinds @ b, accFlds @ [(fn, fe')])) ([], [])
        wrapLets binds (IRStructLit (n, fldsLifted))
    | IRIf (c, t, e) ->
        // The CONDITION's lifts hoist ABOVE the select; the BRANCHES' stay put.
        // A condition is evaluated exactly once whatever the select does, so a
        // statement-shaped operand inside it (`if reduce(A * B, (+)) > 0.0 ...`,
        // whose IRBinOp arm mints the let that then had nowhere to live but the
        // condition slot, where codegen renders it inline and hits the BL7004
        // reduce refusal) belongs at the enclosing drain point. A BRANCH is
        // not: hoisting out of an untaken arm would compute it unconditionally
        // -- a cost change at best, and a panic the interpreter never raises at
        // worst. Statement-shaped values in conditional arms stay refused (the
        // whole family: an IRArrayLit there is the same BL7001 today).
        let (binds, cFinal) = peelLetChain (liftExpr builder c)
        wrapLets binds (IRIf (cFinal, liftExpr builder t, liftExpr builder e))
    | IRMatch (scr, cases) ->
        IRMatch (liftExpr builder scr, cases |> List.map (fun c ->
            { c with Guard = c.Guard |> Option.map (liftExpr builder)
                     Body = liftExpr builder c.Body }))
    | IRSequence es -> IRSequence (es |> List.map (liftExpr builder))
    | IRGuard (c, b) -> IRGuard (liftExpr builder c, liftExpr builder b)
    | IRReplicate (c, b) -> IRReplicate (liftExpr builder c, liftExpr builder b)
    | IRPure e -> IRPure (liftExpr builder e)
    | IRCompute e ->
        // Drain any let-chain that lifting the inner expression produced out
        // of the IRCompute wrapper: genComputeBinding has no IRLet arm, so a
        // let left inside IRCompute falls to the scalar exprToCpp path and
        // errors (BLADE_CODEGEN_ERROR_UNEVALUATED_COMPUTATION). Peeling it out
        // yields `let __t = A * B in (... |> compute)`, materialized by
        // genLetChainBinding as ordered statement bindings.
        let e' = liftExpr builder e
        let (peeled, inner) = peelLetChain e'
        wrapLets peeled (IRCompute inner)
    | IRReynolds (e, a) -> IRReynolds (liftExpr builder e, a)
    | IRBind (c, k) -> IRBind (liftExpr builder c, liftExpr builder k)
    | IRParallel (a, b, d) -> IRParallel (liftExpr builder a, liftExpr builder b, d)
    | IRFusion (a, b) -> IRFusion (liftExpr builder a, liftExpr builder b)
    | IRChoice (a, b) -> IRChoice (liftExpr builder a, liftExpr builder b)
    | IRFallback (a, b) -> IRFallback (liftExpr builder a, liftExpr builder b)
    | IRArrayProduct (a, b) -> IRArrayProduct (liftExpr builder a, liftExpr builder b)
    | IRComposeObj (a, b) -> IRComposeObj (liftExpr builder a, liftExpr builder b)
    | IRComposeMeth (a, b) -> IRComposeMeth (liftExpr builder a, liftExpr builder b)
    | IRCompose (a, b) -> IRCompose (liftExpr builder a, liftExpr builder b)
    | IRFunctorMap (fn, c) -> IRFunctorMap (liftExpr builder fn, liftExpr builder c)
    | IRPolyIndex (p, i) -> IRPolyIndex (liftExpr builder p, liftExpr builder i)
    | IRPolyTail (p, drop) -> IRPolyTail (liftExpr builder p, drop)
    | IRRaggedLookup l -> IRRaggedLookup (liftExpr builder l)
    | IRCompoundMask mk -> IRCompoundMask (liftExpr builder mk)
    | IRCompoundProject (parent, plen) -> IRCompoundProject (liftExpr builder parent, plen)
    | IRSparseKeys (SkRuntime keys) -> IRSparseKeys (SkRuntime (liftExpr builder keys))
    | IRSparseKeys (SkStatic _) -> expr
    // Only the base extent can hold a liftable inline form; the level list is data.
    | IROrbitClass (levels, n) -> IROrbitClass (levels, liftExpr builder n)
    | IRAssign (t, v) -> IRAssign (t, liftExpr builder v)
    | IRConstraintCheck (c, code, msg, sp) -> IRConstraintCheck (liftExpr builder c, code, msg, sp)
    | IRBreakIf c -> IRBreakIf (liftExpr builder c)
    | IRForRange (vid, lo, hi, body) ->
        IRForRange (vid, liftExpr builder lo, liftExpr builder hi, liftExpr builder body)
    | IRBlocked (it, bs) -> IRBlocked (it, liftExpr builder bs)

    // Loop forms: their auto-materialize handles top-level Arrays for
    // inline forms. We still descend into the kernels and any nested
    // expressions, AND lift any array-typed IRFieldAccess in Arrays so
    // codegen can find the companion `_extents` (auto-materialize doesn't
    // synthesize extents from struct field types).
    | IRMethodFor info ->
        let arrays' = info.Arrays |> List.map (liftExpr builder)
        let (binds, arraysFinal) =
            arrays' |> List.fold (fun (accB, accA) a ->
                let (peeled, inner) = peelLetChain a
                if isArrayFieldAccess inner || isNestedLoopComputeArg inner || isInlineArrayLitArg inner
                   || isArrayValuedSelect inner || isNestedLoopFormArg inner
                   // Statement-shaped forms (gram, decompact, transpose, the
                   // <|:> / sequence family, ...) have no inline rendering, so
                   // left in this slot the nest peels an `arr<i>` that was
                   // never declared. Hoist to a let-RHS, minus the four the
                   // codegen-side auto-materialize already covers. The
                   // deferring members hoist in their FORCED spelling -- a
                   // bare one would bind a name genBinding never declares.
                   || isStatementShapedArraysArg inner then
                    let id = builder.FreshId()
                    let ty = typeOf inner
                    (accB @ peeled @ [(id, ty, forceDeferringForm inner)], accA @ [IRVar (id, ty)])
                else
                    (accB @ peeled, accA @ [inner])) ([], [])
        wrapLets binds (IRMethodFor { info with Arrays = arraysFinal })
    | IRObjectFor info ->
        IRObjectFor { info with Kernel = liftExpr builder info.Kernel }
    | IRApplyCombinator info ->
        let loop' = liftExpr builder info.Loop
        let kernel' = liftExpr builder info.Kernel
        let arrays' = info.Arrays |> List.map (liftExpr builder)
        let (binds, arraysFinal) =
            arrays' |> List.fold (fun (accB, accA) a ->
                let (peeled, inner) = peelLetChain a
                if isArrayFieldAccess inner || isNestedLoopComputeArg inner || isInlineArrayLitArg inner
                   || isArrayValuedSelect inner || isNestedLoopFormArg inner
                   // Statement-shaped forms (gram, decompact, transpose, the
                   // <|:> / sequence family, ...) have no inline rendering, so
                   // left in this slot the nest peels an `arr<i>` that was
                   // never declared. Hoist to a let-RHS, minus the four the
                   // codegen-side auto-materialize already covers. The
                   // deferring members hoist in their FORCED spelling -- a
                   // bare one would bind a name genBinding never declares.
                   || isStatementShapedArraysArg inner then
                    let id = builder.FreshId()
                    let ty = typeOf inner
                    (accB @ peeled @ [(id, ty, forceDeferringForm inner)], accA @ [IRVar (id, ty)])
                else
                    (accB @ peeled, accA @ [inner])) ([], [])
        wrapLets binds (IRApplyCombinator { info with Loop = loop'; Kernel = kernel'; Arrays = arraysFinal })
    | IRComposeApply info ->
        // Same array-let lifting as IRApplyCombinator, applied to
        // InputArrays. No Kernel slot to lift (slot inversion: the
        // arrays *are* what would have gone in the kernel position).
        let composition' = liftExpr builder info.Composition
        let arrays' = info.InputArrays |> List.map (liftExpr builder)
        let (binds, arraysFinal) =
            arrays' |> List.fold (fun (accB, accA) a ->
                let (peeled, inner) = peelLetChain a
                if isArrayFieldAccess inner || isNestedLoopComputeArg inner || isInlineArrayLitArg inner
                   || isArrayValuedSelect inner || isNestedLoopFormArg inner
                   // Statement-shaped forms (gram, decompact, transpose, the
                   // <|:> / sequence family, ...) have no inline rendering, so
                   // left in this slot the nest peels an `arr<i>` that was
                   // never declared. Hoist to a let-RHS, minus the four the
                   // codegen-side auto-materialize already covers. The
                   // deferring members hoist in their FORCED spelling -- a
                   // bare one would bind a name genBinding never declares.
                   || isStatementShapedArraysArg inner then
                    let id = builder.FreshId()
                    let ty = typeOf inner
                    (accB @ peeled @ [(id, ty, forceDeferringForm inner)], accA @ [IRVar (id, ty)])
                else
                    (accB @ peeled, accA @ [inner])) ([], [])
        wrapLets binds (IRComposeApply { info with Composition = composition'; InputArrays = arraysFinal })

/// Lift inline forms across an entire IR module's bindings and functions.
let liftInlineFormsModule (modul: IRModule) (builder: IRBuilder) : IRModule =
    // Populate the struct fields cache so typeOf can resolve IRFieldAccess
    // result types. Required for hoisting array-typed field accesses to
    // let-RHS so codegen can synthesize their _extents companions.
    setStructFieldsCache modul.Types
    let liftedBindings =
        modul.Bindings |> List.map (fun b -> { b with Value = liftExpr builder b.Value })
    let liftedFunctions =
        modul.Functions |> List.map (fun f -> { f with Body = liftExpr builder f.Body })
    { modul with Bindings = liftedBindings; Functions = liftedFunctions }

