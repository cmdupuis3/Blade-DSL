// Blade-DSL C++ Code Generation
// Transforms IR structures into C++ source code
// Generates complete, compilable C++ programs

module Blade.CodeGen

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
open Blade.CodeGenState
open Blade.CodeGenExprSupport
open Blade.CodeGenExpr
open Blade.CodeGenLoopNest
open Blade.CodeGenCuda
open Blade.CodeGenFusion
open Blade.CodeGenBinding

// Re-exports: the split moved these to CodeGenState/CodeGenLoopNest, but
// Build.fs, the interpreter, and the test suites address them as
// CodeGen.* -- keep that surface stable.
let isRaggedArrayType = CodeGenState.isRaggedArrayType
let isRaggedRowType = CodeGenState.isRaggedRowType
let isDepIdxArrayType = CodeGenState.isDepIdxArrayType
let isCompoundArrayType = CodeGenState.isCompoundArrayType
let isSparseArrayType = CodeGenState.isSparseArrayType
let arrayRank = CodeGenState.arrayRank
let inferExprType = CodeGenState.inferExprType
let memcheckEnabled = CodeGenState.memcheckEnabled
let foldLaneCount = CodeGenState.foldLaneCount
let ompThreadEmissionEnabled = CodeGenState.ompThreadEmissionEnabled
let setCudaEmitMode = CodeGenState.setCudaEmitMode
let setMpiEmitMode = CodeGenState.setMpiEmitMode
let setSplitTimingMode = CodeGenState.setSplitTimingMode
let setSplitTimingOnlyBinding = CodeGenState.setSplitTimingOnlyBinding
let setOmpTestMode = CodeGenState.setOmpTestMode
let takeUnhandledIRNodeDiagnostics = CodeGenState.takeUnhandledIRNodeDiagnostics
let takeCodegenRefusalDiagnostics = CodeGenState.takeCodegenRefusalDiagnostics
let canonicalKey = CodeGenLoopNest.canonicalKey
let exprToCppWithSubst = CodeGenLoopNest.exprToCppWithSubst
let runtimeHeaderNames = CodeGenLoopNest.runtimeHeaderNames
let runtimeHeaderText = CodeGenLoopNest.runtimeHeaderText
let deployRuntimeHeaders = CodeGenLoopNest.deployRuntimeHeaders
let compoundArrayNamesOf = CodeGenLoopNest.compoundArrayNamesOf
let genForLoopHeader = CodeGenLoopNest.genForLoopHeader
let genElementBindingNew = CodeGenLoopNest.genElementBindingNew
let compoundOutputSubscript = CodeGenLoopNest.compoundOutputSubscript
let genNestPragma = CodeGenLoopNest.genNestPragma
let peelRowPragma = CodeGenLoopNest.peelRowPragma

/// Re-spell a FLAT tuple local as the NESTED shape the function's declared C++
/// return type has, or hand back `flatName` unchanged when there is nothing to
/// reconcile.
///
/// Two conventions meet at a `return`, and only here do they have to agree on
/// one C++ TYPE rather than merely on which leaf is which:
///   - every fusion emitter builds ONE FLAT tuple (`std::make_tuple(t_0, t_1,
///     t_2)`) and registers its leaf names in ctx.TupleChildren;
///   - the function's C++ signature is `irTypeToCpp` of the IR type, which is
///     the LEFT-NESTED pair tree `<&!>`/`<&>` parse -- `std::tuple<std::tuple<
///     double, double>, double>` for three legs.
/// Inside a body that mismatch is invisible (a destructure resolves through
/// TupleChildren and never names the tuple); across the call boundary it is a
/// hard type error in the CALLEE -- returning a flat 3-tuple as a nested one.
/// Two legs are exempt because nested and flat coincide at width 2, which is
/// why this only ever showed up at three.
///
/// Rebuilding at the return keeps the FLAT convention everywhere the emitters
/// own (accumulators, TupleChildren, destructures) and pays the one nesting
/// exactly where the ABI demands it. The caller needs no change: its
/// destructure already navigates the nested declared type (TypeCheck's
/// flatTupleLeafPaths and CodeGen's flat IRTupleProj arm agree on that path),
/// which is precisely why it was the RETURN that was wrong, not the read.
///
/// Applicability is deliberately narrow -- a registered flat-children entry
/// whose width equals the type's leaf count. A genuine nested `IRTuple` value
/// registers no children and renders nested already, so it falls through.
let private nestedTupleReturn (retTy: IRType) (flatName: string) (children: Map<string, string list>) : string =
    let isNested =
        match retTy with
        | IRTTuple ts -> ts |> List.exists (_.IsIRTTuple)
        | _ -> false
    match (if isNested then Map.tryFind flatName children else None) with
    | Some leaves when leaves.Length = (IR.flattenTupleLeaves retTy).Length ->
        // Consume leaf names left-to-right along the type's structure -- the
        // same order flattenTupleLeaves produces them in, so leaf i of the
        // emission lands at leaf i of the type by construction.
        let rec build (ty: IRType) (remaining: string list) : string * string list =
            match ty with
            | IRTTuple ts ->
                let (parts, rest) =
                    ts |> List.fold (fun (acc, rem) t ->
                        let (s, rem') = build t rem
                        (acc @ [s], rem')) ([], remaining)
                ($"""std::make_tuple({(parts |> String.concat ", ")})""", rest)
            | _ ->
                match remaining with
                | l :: rest -> (l, rest)
                | [] -> (flatName, [])   // width-checked above; unreachable
        fst (build retTy leaves)
    | _ -> flatName

/// The IRTGroupKeys type of a function-body `let gk = group_keys(...)`, which
/// picks the bucketing regime (positional / EnumIdx / dynamic).
///
/// A module binding carries that type in its record; a body let is a bare
/// (id, value) pair and the IRGroupKeys node itself carries nothing, so the
/// regime has to come from somewhere else. It comes from the USES: every legal
/// mention of a grouping is the bare `gk` name (BL3017 refuses every
/// indirection), and Lowering stamps those IRVar references with the type the
/// typechecker inferred from the key array's element type -- the same
/// inference, in the same module env, that a module-scope spelling gets.
///
/// Reading it back matters for correctness, not speed: dynamic discovery
/// answers a DIFFERENT question than positional bucketing (buckets in
/// first-occurrence order over the observed keys, versus bucket = key value
/// over the declared Idx<N>, empty groups included), so guessing dynamic would
/// make `group_bucket`/`extents` inside a function disagree with the identical
/// source text outside it. `None` (no typed use, or an unresolved extent) keeps
/// the dynamic form, which is what every un-annotated key array gets anyway.
let private groupKeysTypeInScope (id: IRId) (scope: IRExpr list) : IRType option =
    let rec scan (e: IRExpr) : IRType option =
        match e with
        | IRVar (vid, (IRTGroupKeys _ as ty)) when vid = id -> Some ty
        | ExprShape (children, _) -> children |> List.tryPick scan
    scope |> List.tryPick scan

/// Function-body emission proper: the per-let statement fold plus the return-arm
/// dispatch, over an already-unrolled let chain. Split out of genFuncBody so the
/// deterministic-deallocation frame can be pushed and popped around it inside a
/// `try ... finally` WITHOUT adding a closure layer over the mutable `currentNames`
/// the fold threads (see setAllocOwner's note on that hazard).
let private genFuncBodyScoped
        (ctx: CodeGenContext) (builder: IRBuilder) (names: Map<IRId, string>) (indent: string)
        (lets: (IRId * IRExpr) list) (retExpr: IRExpr) : string list =
    let mutable currentNames = names
    // Grouped-array registrations made by THIS body's lets (group_by ->
    // group_keys name map). genGroupByBinding records them in its returned
    // ctx; at module level the binding fold threads that ctx forward, and the
    // grouped-zip peel reads it to recognize co-iterable operands. The
    // function-body fold below constructs a fresh bodyCtx per let, so the
    // registrations must be accumulated here and spliced into every bodyCtx --
    // dropping them makes `method_for(zip(gt, gs))` inside a function body
    // fall past the peel into the multi-array refusal.
    let mutable currentGrouped = ctx.GroupedArrays
    // Deferred computations bound by THIS body's lets. Only a REDUCTION-JOIN
    // operand gets to stay deferred inside a body (S2 forces everything else,
    // and the arm below is a hard error for anything that arrives unforced
    // without being one); the join's emitter resolves the operand through this
    // map to inline it as a shared per-iteration local.
    let mutable currentDeferred = ctx.DeferredComputations
    // Ids S2 deliberately left unforced: bound to a bare combinator and read
    // ONLY as a join operand. Computed over the whole body at once, because
    // the accounting that makes the exemption safe is global.
    let joinDeferredIds = Blade.Lowering.joinDeferrableIdsMany ((lets |> List.map snd) @ [retExpr])
    // Flat-tuple child names registered by THIS body's lets, threaded for the
    // same reason as currentGrouped above. EVERY tuple this file emits is FLAT
    // (`std::make_tuple(x_0, x_1, x_2)` -- tryGenMergedCompute, genParallelTree
    // and genReduceComputeBinding's fused arm all share that convention), while
    // the tuple's IR TYPE is the left-nested pair tree the `<&!>`/`<&>` parse
    // built. TupleChildren is what reconciles the two: a destructure resolves
    // through it straight to the accumulator names. Dropping it sent the
    // projection to genScalarBinding's std::get fallback, which navigates the
    // NESTED type -- `std::get<0>(std::get<0>(t))` against a flat 3-tuple, an
    // uncompilable C++ that only two leaves (where nested == flat) survived.
    //
    // A REDUCTION JOIN (`object_for(<&!>) <@> (...)`) answers a flat Tuple<k>
    // and so never needs this reconciliation; the binary `<&!>` chain, whose
    // typing IS nested, still does.
    let mutable currentTupleChildren = ctx.TupleChildren
    // Indentation for genApplyCombinator emissions: the function body lives one
    // level deeper than the function declaration's ctx.Indent.
    let bodyIndent = ctx.Indent + 1
    // A function-body `let` bound to a STILL-DEFERRED combinator emits no
    // statement (the IRApplyCombinator arm below), yet any later statement that
    // reads it POSITIONALLY -- `prodsum(s, e)`, `e[i]`, `extents(e)`, `f(e)` --
    // renders it BY NAME and names an identifier that was never declared.
    // Module level solves this through ctx.DeferredComputations + the forcing
    // helpers; a function body never populates that map, so decide it here with
    // the SAME by-name rule: seed a probe ctx with the deferred lets and ask
    // collectDeferredPositionalReads which of them a consumer names. Only those
    // materialize -- a binding that is merely absorbed (fused into a reduce,
    // forced later by `|> compute`, or unused) stays deferred exactly as before.
    let deferredLets =
        lets |> List.choose (fun (id, v) ->
            match v with
            | IRApplyCombinator _ | IRComposeApply _ -> Some (id, v)
            | _ -> None)
    let forcedDeferredIds =
        if List.isEmpty deferredLets then Set.empty
        else
            let probeCtx = { ctx with DeferredComputations = Map.ofList deferredLets }
            let read =
                ((lets |> List.map snd) @ [retExpr])
                |> List.collect (collectDeferredPositionalReads probeCtx)
                |> Set.ofList
            // `return e` naming a deferred local is a by-name read too. The
            // collector deliberately does NOT note a bare IRVar (at module level
            // that shape is a deliberately-deferred ALIAS binding), but a
            // function RETURN has to hand the caller a materialized array.
            match retExpr with
            | IRVar (rid, _) when deferredLets |> List.exists (fun (i, _) -> i = rid) ->
                Set.add rid read
            | _ -> read
    let stmts = lets |> List.collect (fun (id, value) ->
        // Every allocation emitted while THIS let renders is owned by it (see
        // setAllocOwner); the fold overwrites the stamp each iteration.
        setAllocOwner (Some id)
        let varName = $"__v{id}"
        // Collapse stacked IRCompute wrappers before dispatching. `compute` is
        // idempotent at inference, but IR construction can stack the user's
        // `|> compute` on a node that already carries its own wrap (measured:
        // `reduce(g <@> k, (+)) |> compute` in a function body arrives as
        // IRCompute(IRCompute(..))), and every arm below matches exactly ONE
        // wrapper -- the nested form fell to the default arm's inline
        // rendering and its sentinel.
        let value =
            let rec collapse e =
                match e with
                | IRCompute (IRCompute _ as inner) -> collapse inner
                | e -> e
            collapse value
        match value with
        | IRForRange (vid, lo, hi, forBody) ->
            // Route through genForRangeBinding -- the recursive binding-level
            // renderer -- so nested for-in loops (and any statement form its
            // genBinding dispatch supports) work inside FUNCTION bodies
            // exactly as they do at module level. The old inline renderer
            // here was flat: a nested IRForRange fell through exprToCpp and
            // emitted an unsupported-expression marker.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = IRTUnit
                Value = value; IsConst = true; IsMutable = false
            }
            let (code, _) = genForRangeBinding bodyCtx tempBinding builder vid lo hi forBody
            currentNames <- Map.add id varName currentNames
            code
        | IRAssign (target, v) ->
            let targetStr =
                match target with
                | LVVar tid -> Map.tryFind tid currentNames |> Option.defaultValue ($"__v{tid}")
                | _ -> exprToCpp currentNames target
            currentNames <- Map.add id varName currentNames
            // Copy-in-place (see exprToCppCore's IRAssign arm): sole-owner mut keeps
            // ONE pool; the RHS temp stays iteration-owned and its scope frees it.
            match copyInPlaceAssign target v with
            | Some (_, rid, n) ->
                let rhsStr = Map.tryFind rid currentNames |> Option.defaultValue ($"__v{rid}")
                [$"{indent}std::copy_n(pool_base({rhsStr}.data), {n}, pool_base({targetStr}.data));"]
            | None ->
                [$"{indent}{targetStr} = {(exprToCpp currentNames v)};"]
        | IRConstraintCheck (cond, blCode, message, span) ->
            currentNames <- Map.add id varName currentNames
            [ $$"""{{indent}}if (!({{(exprToCpp currentNames cond)}})) {"""
              $"{indent}    blade_rt::panic(\"{blCode}\", \"{message}\", {(panicSpanArgs span)});"
              $"{indent}}}" ]
        | IRLit IRLitUnit ->
            // Skip unit literals (side effects already emitted)
            currentNames <- Map.add id varName currentNames
            []
        | IRMethodFor _ | IRObjectFor _ ->
            // Loop objects are compile-time only -- they're resolved when <@> is processed
            currentNames <- Map.add id varName currentNames
            []
        | IRApplyCombinator info when Set.contains id forcedDeferredIds ->
            // Still-deferred combinator that a LATER statement reads BY NAME.
            // Materialize it here, through the same statement-form path the
            // `|> compute` arm below uses, or the read names an identifier
            // this arm never declared.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; TupleChildren = currentTupleChildren }
            let code = genApplyCombinator bodyCtx varName info builder
            currentNames <- Map.add id varName currentNames
            code
        | IRApplyCombinator _ when Set.contains id joinDeferredIds ->
            // REDUCTION-JOIN OPERAND: the one body-level let that legitimately
            // stays deferred. A later join in this same body inlines it as a
            // per-iteration `const` in the joint nest, so materializing it here
            // would build (and read back) a whole array per outer cell -- the
            // cost the join exists to remove. Nothing else may read it: S2's
            // exemption only fires when every reference is a join operand.
            currentNames <- Map.add id varName currentNames
            currentDeferred <- Map.add id value currentDeferred
            [$"{indent}// {varName} = <deferred computation (reduction-join operand)>"]
        | IRApplyCombinator _ | IRComposeApply _ ->
            // WAS: "unevaluated computations -- deferred until |> compute forces
            // them", registering the name and emitting NOTHING. That premise is
            // true at MODULE level (genComputeBinding peels IRCompute and the
            // forcing site consumes the deferred node) and FALSE here: a
            // function/lambda body has no forcing site, so the name entered
            // currentNames and every downstream read spelled a C++ identifier
            // that was never declared -- the `'__v27' was not declared` bug
            // (docs/plan-kernel-body-materialization.md section 2), with the
            // dropped node's kernel emitted as a free function and never called.
            //
            // Stage S2's Lowering half (Lowering.forceBareCombinatorLets) now
            // forces every such let at the source of the IR, so this arm is
            // unreachable. Keep it as a LOUD invariant rather than a silent
            // drop: a bare combinator arriving here again means a body-lowering
            // path bypassed the force, and the symptom of the silent form is an
            // undeclared identifier hundreds of lines away.
            failwithf "internal: a function-body let (id %d) bound a bare, unforced \
IRApplyCombinator/IRComposeApply. Lowering.forceBareCombinatorLets must wrap every \
body-level let RHS of that shape in IRCompute; emitting nothing here would register \
'%s' as a name with no declaration behind it." id varName
        | IRCompute (IRApplyCombinator info) ->
            // Function-body let-binding of `method_for(...) <@> kernel |> compute`.
            // Use the statement-form genApplyCombinator, which emits the full
            // sequence (extents declaration, allocation, loop nest) with
            // `varName_extents` etc. as proper C++ identifiers -- so any
            // downstream operation that uses the companion-array convention
            // (reduce's inline IIFE, extents() runtime read, etc.) finds them.
            //
            // The inline genApplyCombinatorExpr can't preserve that convention
            // through an IIFE boundary (companion arrays would be lambda-local,
            // not visible in the enclosing scope) and only handles the 2-array
            // accumulation form anyway. Routing through genApplyCombinator here
            // mirrors what genBinding does at the module level.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let code = genApplyCombinator bodyCtx varName info builder
            currentNames <- Map.add id varName currentNames
            code
        | IRCompute (IRComposeApply _) ->
            // The IRComposeApply half of the arm above. `(o1 >>@ o2) <@> A` as a
            // body-level let now arrives forced (Lowering.forceBareCombinatorLets
            // wraps both combinator shapes), and genBinding's genComputeBinding
            // peels the IRCompute and routes to genComposeApply -- the same
            // statement form module level uses. exprToCpp has no expression
            // rendering for it.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRReduceCompute _ ->
            // Fused reduce over a deferred computation (`reduce(A <@> k, (+))`)
            // as a body-level let. Statement-shaped -- it declares scalar
            // accumulators and a loop nest -- so it routes through genBinding's
            // genReduceComputeBinding exactly as at module level. exprToCpp has
            // no expression form for it and would emit its sentinel.
            //
            // `currentDeferred` (not ctx's map) so a REDUCTION JOIN can resolve
            // an operand this body left deferred and share it per iteration.
            // `currentTupleChildren` so a binary-chain join's destructure
            // resolves to the per-leaf accumulator names rather than std::get
            // on its nested type.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent
                                     GroupedArrays = currentGrouped
                                     DeferredComputations = currentDeferred
                                     TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRApp (IRObjectFor _, _, _) ->
            // A hoisted (or directly let-bound) synthesized loop
            // application -- route through genBinding, whose
            // IRApp(IRObjectFor) arm expands the full loop nest, exactly as
            // at module level. Capture forwarding for the kernel resolves
            // through currentNames (the hoisted scalar lets precede this
            // entry in dependency order).
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | _ when (let rec chainTail e =
                      match e with
                      | IRCompute inner | IRLet (_, _, inner) -> chainTail inner
                      | e -> e
                  match chainTail value with
                  | IRApp (IRObjectFor _, _, _) -> true
                  | _ -> false) ->
            // An IRLet-WRAPPED loop application (possibly under the user's
            // `|> compute`): the direct-lowered scalar<*>array broadcast form,
            // `IRLet(s, scalar, IRApp(IRObjectFor kernel, [A]))`. It arises in
            // a body when the scalar side is an unresolved kernel param at
            // typing time (the `scalarish` gate skips the compute re-synthesis
            // -- see Lowering's broadcast arm), so none of the IRCompute(...)
            // arms above match. The default arm's inline rendering walks into
            // the chain and hits exprToCpp's loop-object sentinel; module
            // level routes the identical shape through genLetChainBinding.
            // Do the same here, stripping a leading IRCompute first (compute
            // over an already-eager chain is the identity -- computeWrap's
            // rule, which does not reach this form because the wrap sits
            // OUTSIDE the let chain).
            let bare = (match value with IRCompute inner -> inner | v -> v)
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType bare
                Value = bare; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRArrayLit _ ->
            // Array literal as a function-body let (e.g. a locally built
            // buffer that loops then fill): route through genBinding, whose
            // IRArrayLit arm emits the statement form (extents + allocate +
            // per-element init). The default arm's exprToCpp has no inline
            // rendering for array literals.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRRange _ | IRCompute (IRRange _) ->
            // A BARE range as a function-body let -- written (`let xs = 0..n`,
            // with or without `|> compute`) or minted by the lift pass's
            // reduce-operand hoist. Statement-shaped (extents table + allocate
            // + fill), so route through genBinding's IRRange arm exactly as at
            // module level; the default arm's exprToCpp has no inline
            // rendering for a standalone range.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRVar _ when Set.contains id ctx.MutableArrayLets ->
            // Function-body `let mut a = Z` over an array: route through
            // genBinding so genVarAliasBinding's mut-copy path runs (fresh
            // alloc + pool copy). The default arm's `auto` alias would share
            // Z's storage and let mutations through `a` corrupt it.
            //
            // UNLESS Z is a scope-local staging let that solely owns a fresh
            // pool (canAliasStagingLet): nothing else names that storage, so
            // there is no Z to corrupt and the copy is the same double
            // materialization genLetChainBinding elides at module level. A
            // `let rec` written INSIDE a function body arrives here rather than
            // at genLetChainBinding because genFuncBody's deepUnroll flattens
            // the block's chain into this let list, leaving the block's value
            // as an ordinary `let traj = <staging>` sibling. Suppressing the
            // marks routes genVarAliasBinding to its plain-alias arm; it checks
            // MutableArrayLets independently of IsMutable, so both must go.
            let aliasStaging =
                match value with
                | IRVar (srcId, _) -> canAliasStagingLet lets (Some id) srcId
                | _ -> false
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let bodyCtx =
                if aliasStaging
                then { bodyCtx with MutableArrayLets = Set.remove id bodyCtx.MutableArrayLets }
                else bodyCtx
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = not aliasStaging
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | (IRReduce _ | IRCompute (IRReduce _)) when
            (let inner = (match value with IRCompute e -> e | e -> e)
             match inferExprType inner with ArrayElem _ -> true | _ -> false) ->
            // An ARRAY-VALUED reduce (axis fold: rank-2 -> rank-1, lswosa's
            // per-frequency segment fold `reduce(ls_e <@> mag2, (+)) |> compute`)
            // as a function-body let, with or without the user's IRCompute
            // wrapper. Statement-shaped -- it allocates the folded output and
            // runs a nest -- so route through genBinding (genComputeBinding
            // peels the wrapper, genReduceBinding emits) as at module level.
            // The default arm's inline exprToCpp rendering only serves the
            // SCALAR fold (a self-contained IIFE), which is why scalar reduces
            // deliberately stay below.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName
                Type = inferExprType (match value with IRCompute e -> e | e -> e)
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRGroupKeys _ | IRGroupBy _ ->
            // group_keys/group_by as FUNCTION-BODY lets (lswosa's
            // `family_spectra` shape: the whole segmentation pipeline lives
            // inside a named function). Statement-shaped with a name-suffix
            // ABI (`<name>__ngroups`/`__offsets`/`__perm`, read by name
            // downstream), so route through genBinding's
            // genGroupKeysBinding/genGroupByBinding exactly as at module
            // level; the suffixes derive from the mapped `__v<id>` name, so
            // downstream consumers (the grouped peel, IRGroupBy itself)
            // resolve them through currentNames unchanged.
            //
            // Two module-level facts have no function-body counterpart and are
            // reconstructed here:
            //  - genGroupKeysBinding dispatches its three cases on the
            //    BINDING's IRTGroupKeys type, which only a typechecked module
            //    binding record carries (a body let is a bare (id, value)
            //    pair, and inferExprType says IRTUnit for the opaque node).
            //    groupKeysTypeInScope reads it back off this body's typed
            //    references to the grouping, so an annotated key array picks
            //    the SAME regime it would at module scope; only a grouping
            //    with no typed use left falls back to dynamic discovery.
            //  - genGroupByBinding registers name -> gk in GroupedArrays via
            //    its returned ctx; capture it into currentGrouped so the
            //    grouped-zip peel sees it.
            let tempType =
                match value with
                | IRGroupKeys _ ->
                    match groupKeysTypeInScope id ((lets |> List.map snd) @ [retExpr]) with
                    | Some ty -> ty
                    | None ->
                        let dynIdx = {
                            Id = 0; Rank = 1
                            Extent = IRParam ("__fnbody_group_n", 0, IRTNat None)
                            Symmetry = SymNone; Tag = None; IxKind = IxKPlain
                            Kind = SDimension; Dependencies = []
                        }
                        IRTGroupKeys (dynIdx, dynIdx, None)
                | _ -> inferExprType value
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = tempType
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentGrouped <- ctxAfter.GroupedArrays
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRGroupBucket _ | IRGroupSizes _ ->
            // The two GROUPING ACCESSORS as function-body lets, the companions of
            // the arm above: a body that builds its own `group_keys` should be
            // able to read the grouping back out in the same scope. Both are
            // statement-shaped (extents table + allocate + one pass over the CSR
            // tables), so exprToCpp has only its unhandled-node sentinel for
            // them -- the BL7001 an in-function `group_bucket(gk)` used to raise.
            //
            // Unlike group_keys, neither emitter consults the BINDING's type
            // (they answer a plain rank-1 Int64 array whose length is a `<gk>__`
            // local), so `inferExprType` suffices and no type has to be
            // reconstructed. What they DO need is the gk name, and that resolves
            // through currentNames exactly as at module level: the grouping's own
            // let precedes this one and registered `__v<id>`, off which the
            // `__ngroups`/`__offsets`/`__perm`/`__nsrc` suffixes hang.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRTupleProj _ ->
            // Destructure of a tuple bound EARLIER IN THIS BODY -- the
            // `let (sc, ss, cc) = reduce(k1 <&!> k2 <&!> k3, (+))` shape, whose
            // producer is the fused-reduce arm above. Route through genBinding
            // (genTupleProjBinding) so the projection resolves through the
            // currentTupleChildren threaded from that producer, straight to the
            // per-leaf accumulator names -- exactly what module level does.
            //
            // The default arm below renders IRTupleProj inline instead, and
            // inline rendering has only the tuple's IR TYPE to navigate: it
            // walks the nested pair tree the `<&!>` parse built and emits
            // `std::get<0>(std::get<0>(t))` against a FLAT `std::make_tuple`.
            // Two leaves survived that (nested and flat agree at width 2);
            // three or more did not compile. genTupleProjBinding keeps the
            // std::get form as its own fallback for tuples with no registered
            // children (a callee's returned tuple), so nothing that worked
            // before is rerouted.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType value
                Value = value; IsConst = true; IsMutable = false
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        | IRMask _ | IRIntersect _ | IRUnion _ | IRSort _ | IRUnique _ | IRTranspose _ | IRDecompact _ | IRArrayNegate _ | IRArrayConjugate _ | IRGram _ | IRGramApply _ | IRMatmul _ | IREigh _ | IRSolve _ | IRLu _ | IRLuSolve _
        | IRStack _ | IRJoin _ ->
            // The lift pass can place an inline form as a let value at
            // function-body level. The same materialization helper used by
            // exprToCpp's IRLet (for kernel-body IIFEs) produces format-
            // neutral statement lines; here we emit them with the function
            // body's indent rather than space-joined inline.
            let elemStr = lazy (inferInlineElemTypeStr "lambda-body inline form" value)
            match materializeInlineForm emptySubst currentNames varName elemStr value with
            | Some (matStmts, allocs) ->
                // Statement position inside a live function frame: register.
                registerMaterializedAllocs allocs
                currentNames <- Map.add id varName currentNames
                matStmts |> List.map (fun s -> indent + s)
            | None ->
                // Defensive: shouldn't fire for the patterns we matched.
                let valStr = exprToCpp currentNames value
                currentNames <- Map.add id varName currentNames
                [$"{indent}auto {varName} = {valStr};"]
        | v when isStatementShapedValue v ->
            // UNIFIED statement-shaped routing. Every arm above this point
            // names ONE form and does the same three things: build a temp
            // IRBinding, call genBinding, thread the returned ctx. This arm is
            // the general case, and it exists because the list of forms that
            // needed it kept growing one BL7001 at a time.
            //
            // It is deliberately placed LAST among the shaped arms rather than
            // replacing them: first-match-wins means every existing arm keeps
            // its exact behavior (several do more than bind-and-emit -- the
            // group_keys type reconstruction, the mut-alias suppression, the
            // materializeInlineForm route just above), and this arm picks up
            // only what fell through. What actually reaches it today is the
            // DEFERRING family: `<|:>` (BL7004 in a body let) and sequence /
            // choice / guard (BL7001).
            //
            // `forceDeferringForm` is what makes those four legal here. Bound
            // BARE they would reach genFallbackBinding / genSequenceBinding /
            // genChoiceBinding / genGuardBinding, each of which emits a
            // `// <deferred ...>` comment and registers the id in
            // DeferredComputations -- correct at MODULE level, where a later
            // `|> compute` reaches genComputeBinding, and wrong here, because
            // a function body has no forcing site and the name would be spelled
            // by a downstream read that nothing ever declared. Wrapping in
            // IRCompute routes straight to the materializing emitter, which is
            // also the semantically right answer: the callee is the last scope
            // that can force, since its caller receives a VALUE.
            let bodyCtx = { ctx with VarNames = currentNames; Indent = bodyIndent
                                     GroupedArrays = currentGrouped
                                     DeferredComputations = currentDeferred
                                     TupleChildren = currentTupleChildren }
            let forced = forceDeferringForm v
            let tempBinding = {
                Id = id; Name = varName; Type = inferExprType v
                Value = forced; IsConst = false; IsMutable = true
            }
            let (code, ctxAfter) = genBinding bodyCtx tempBinding builder
            currentGrouped <- ctxAfter.GroupedArrays
            currentTupleChildren <- ctxAfter.TupleChildren
            currentNames <- Map.add id varName currentNames
            code
        // Deterministic deallocation, site 3b: the FUNCTION-BODY twin of
        // site 3 (`let r = f(a)` where the CALLEE allocated the pool).
        // Same guard set, for the same reasons: FreshPool callees only (a
        // NotFresh return may hand back its own parameter, so registering
        // the binding would double-free), dense + nullptr only (the one
        // storage combination whose free cannot disagree with the callee's
        // allocate). Emission is the fall-through arm's `auto` line
        // unchanged -- only the scope registration is new. Without this arm
        // such lets fell through unregistered and every intermediate of a
        // chained-step helper leaked per call (measured on 08_burgers_les:
        // dns10's nine burgers_step intermediates, 512 B each, every call).
        | IRApp (fn, _, _) when
            (match inferExprType value with
             | ArrayElem at ->
                 freshReturnOf fn = FreshPool
                 && isFreeableDenseArrayType at
                 && classifyOutputStorage (inferExprType value) = AllocDense
                 && not (hasRealSymmetry (buildSymmVec (inferExprType value)))
             | _ -> false) ->
            let valStr = exprToCpp currentNames value
            (match inferExprType value with
             | ArrayElem at ->
                 registerPoolAlloc AllocDense (elemTypeToCpp at.ElemType) (arrayRank at)
                     "nullptr" (varName + "_extents") varName None
             | _ -> ())
            currentNames <- Map.add id varName currentNames
            [$"{indent}auto {varName} = {valStr};"]
        | _ ->
            let valStr = exprToCpp currentNames value
            currentNames <- Map.add id varName currentNames
            [$"{indent}auto {varName} = {valStr};"])
    // Scratch reuse: a let the plan gave a donor takes the donor's pool -- its
    // `allocate<>` declaration is rewritten into an alias of the donor's data
    // (its own extents table stays), and its scope free is spared; the donor's
    // registration frees the storage once, at scope exit (or never, when the
    // reuser escapes: computeScopeEscapes carries an escaping reuser over to
    // its donor). Only a rewrite that MATCHED spares the free.
    let stmts =
        lets |> List.fold (fun (acc: string list) (id, _) ->
            match Blade.Types.PoolReuseTable.tryDonor id with
            | Some d ->
                (match Map.tryFind d currentNames with
                 | Some dn ->
                     let name = $"__v{id}"
                     let (acc', matched) = rewritePoolAlias acc name dn
                     if matched then suppressAllocName name
                     acc'
                 | None -> acc)
            | None -> acc) stmts
    // Return-arm emissions carry NO owner: a __retN temporary must not be matched
    // against some let's escape status by accident. It is exempted by NAME below.
    setAllocOwner None
    if isUnitExpr retExpr then
        // Void function: no return statement, so the frees close the body.
        stmts @ popAllocScopeFrees indent
    else
        // If the return expression is `compute(applyCombinator)`, synthesize
        // an internal let binding so the statement-form genApplyCombinator
        // emits the full sequence (extents/alloc + loop nest for array
        // output, scalar accumulator + loop nest for scalar output) and we
        // return the bound name. This unifies both shapes through the same
        // LoopNestCodeGen machinery; without it, exprToCpp would route
        // through the inline expression-form genApplyCombinatorExpr -- which
        // is still a hardcoded 2-array IIFE special case kept for inline
        // expression contexts that lack a surrounding statement scope (a
        // separate cleanup will fold that into a wrapper around this path).
        // The BARE combinator return takes the same arm: a function's caller
        // receives a VALUE (its return type is an array or scalar, never a
        // loop object), so laziness cannot cross the boundary -- the callee
        // is the last scope that can force. Without this, a body ending in
        // `omegas <@> lambda(w) -> ...` with no `|> compute` fell through to
        // the inline-expression sentinel (UNEVALUATED_COMPUTATION_USED_AS_
        // VALUE) even though the statement-form emitter handles it exactly
        // as it handles the computed spelling.
        match retExpr with
        | IRCompute (IRApplyCombinator info) | IRApplyCombinator info ->
            let retVarName = $"__ret{builder.FreshId()}"
            let bodyCtx = { ctx with VarNames = currentNames; Indent = ctx.Indent + 1; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let combCode = genApplyCombinator bodyCtx retVarName info builder
            // Scratch reuse at the RETURN position: the returned value takes
            // its donor's pool, which leaves with it, so the donor's scope
            // free is spared exactly like the `__retN` name below -- but only
            // when the declaration actually rewrote.
            let combCode =
                match Blade.Types.PoolReuseTable.currentReturnDonor () |> Option.bind (fun d -> Map.tryFind d currentNames) with
                | Some dn ->
                    let (rewritten, matched) = rewritePoolAlias combCode retVarName dn
                    if matched then suppressAllocName dn
                    rewritten
                | None -> combCode
            // The returned pool leaves with the value; free everything else.
            suppressAllocName retVarName
            stmts @ combCode @ popAllocScopeFrees indent @ [$"{indent}return {retVarName};"]
        | (IRIf _ | IRMatch _) when branchingReturnMaterializes retExpr ->
            // A BRANCHING return whose arms each materialize an array
            // (`if flag then xs <@> k1 else xs <@> k2`). S4's return force
            // reaches these leaves, but the emitter cannot: every materializing
            // arm above binds a `__retN` with its OWN allocation and extents,
            // and a C++ ternary has nowhere to put two of them -- they need one
            // destination declared before the branch, with the branch lowered to
            // a STATEMENT if/else and each arm's pool spared from the scope's
            // frees. That is a separate piece of work (S3's output-sizing
            // machinery is the natural home for the shared destination).
            //
            // This program never compiled: before S4 the arms were bare
            // combinators and `exprToCppCore` refused them with
            // UNEVALUATED_COMPUTATION_USED_AS_VALUE. Refuse it HERE instead, so
            // the message names the actual limitation rather than an unrelated
            // inline-combinator ceiling reached three layers down.
            let errLines =
                codegenError ctx indent
                    "a branching return (if/match) whose arms each materialize an array is not supported yet: \
each arm needs its own allocation and there is no shared destination to write them into. \
Bind the branches to a let first (`let r = if c then ... else ...` over already-computed arrays), \
or return a scalar and materialize at the call site"
            stmts @ errLines
        | IRCompute (IRComposeApply _) ->
            // The IRComposeApply half of the arm above. S4 forces a bare
            // `(o1 >>@ o2) <@> A` in RETURN position exactly as it forces a bare
            // apply, so this shape now arrives here; genBinding's
            // genComputeBinding peels the IRCompute and routes to
            // genComposeApply -- the same statement form module level uses.
            // exprToCpp has no expression rendering for it.
            let retVarName = $"__ret{builder.FreshId()}"
            let bodyCtx = { ctx with VarNames = currentNames; Indent = ctx.Indent + 1; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = builder.FreshId(); Name = retVarName; Type = inferExprType retExpr
                Value = retExpr; IsConst = false; IsMutable = true
            }
            let (compCode, _) = genBinding bodyCtx tempBinding builder
            suppressAllocName retVarName
            stmts @ compCode @ popAllocScopeFrees indent @ [$"{indent}return {retVarName};"]
        | IRReduceCompute _ ->
            // Same reason as the IRCompute(IRApplyCombinator) arm above, for the
            // fused-reduce terminal: `reduce(A <@> k, (+))` in RETURN position of
            // a kernel/function body. Statement-shaped, so bind it to a __retN
            // through genBinding and return the name; exprToCpp's IRReduceCompute
            // arm is a sentinel. Reached by every kernel body whose tail is a
            // reduce over a body-local computation (plan section 1, M-A).
            let retVarName = $"__ret{builder.FreshId()}"
            let bodyCtx = { ctx with VarNames = currentNames; Indent = ctx.Indent + 1; GroupedArrays = currentGrouped; TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = builder.FreshId(); Name = retVarName; Type = inferExprType retExpr
                Value = retExpr; IsConst = false; IsMutable = true
            }
            let (redCode, ctxAfter) = genBinding bodyCtx tempBinding builder
            // A JOINED reduce (`reduce(k1 <&!> k2 <&!> k3, (+))`) yields a flat
            // tuple of accumulators, which the signature declares nested -- see
            // nestedTupleReturn. Unjoined folds and two-leg joins pass through
            // it unchanged.
            let retName = nestedTupleReturn (inferExprType retExpr) retVarName ctxAfter.TupleChildren
            // A reduce yields a SCALAR: nothing to spare from the frees, and the
            // value is already in a local, so the frees may close before return.
            stmts @ redCode @ popAllocScopeFrees indent @ [$"{indent}return {retName};"]
        | r when isStatementShapedValue r ->
            // UNIFIED statement-shaped RETURN. This one arm replaces the four
            // that used to sit here -- IRTranspose, IRGroupBucket/IRGroupSizes,
            // and IRArrayLit -- each of which had been added the same way, one
            // BL7001 report at a time, and each of which did character-for-
            // character the same thing: mint a `__retN`, emit the form through
            // its statement generator, spare the pool from the scope frees, and
            // return the name. (The IRArrayLit arm called `genArrayLiteral`
            // directly; `genBinding`'s own IRArrayLit arm is that same call, so
            // folding it in changes nothing but the number of places to edit.)
            //
            // What it newly ACCEPTS is the rest of the family, all of which
            // were hard refusals in return position until now: gram, decompact,
            // matmul, solve, eigh, stack, join, sort, mask, the set ops, negate
            // and conjugate (BL7001), and `<|:>` (BL7004). Note that Phase 0's
            // regression tests reach those forms only through a body LET that
            // is then returned as a plain IRVar; the DIRECT return -- `function
            // g(x) = { gram(x, x) }`, no let -- landed here and died.
            //
            // `forceDeferringForm` for the same reason as in the let fold: the
            // <|:> / sequence / choice / guard emitters DEFER when bound bare,
            // and a return is the one position where deferring is provably
            // wrong, since the caller's type is an array or a scalar and
            // laziness cannot cross the boundary.
            //
            // Extents outlive the frame by construction. Every one of these
            // emitters now builds its table through `emitExtentsTable` (static
            // constexpr where all dims are literal, `new size_t[R]` otherwise),
            // which is what commit 7905b36 established for the materialize*Form
            // builders and what this change extends to genFallbackMaterialize
            // and the IRSequence arm -- the last two that were still handing
            // back a shape their frame owned.
            let retVarName = $"__ret{builder.FreshId()}"
            let bodyCtx = { ctx with VarNames = currentNames; Indent = ctx.Indent + 1
                                     GroupedArrays = currentGrouped
                                     DeferredComputations = currentDeferred
                                     TupleChildren = currentTupleChildren }
            let tempBinding = {
                Id = builder.FreshId(); Name = retVarName; Type = inferExprType r
                Value = forceDeferringForm r; IsConst = false; IsMutable = true
            }
            // EVERY allocation this binding registers leaves with the value;
            // free everything the frame held before it. Sparing the delta
            // rather than the NAME `__retN` is what covers the multi-pool
            // members of the family, and covers them without a per-form list:
            //
            //   * IREigh declares `__retN__q` / `__retN__lam` and binds
            //     `__retN` to a make_tuple of those two wrappers. Freeing them
            //     here returned a tuple of dangling pointers (live only on the
            //     BLAS route -- the Jacobi fallback never reaches this arm).
            //   * IRSequence ALIASES its inputs rather than copying them: the
            //     emitter assembles `<name>[i] = <name>_i`, so the returned
            //     array's cells ARE the per-child pools (`isFreshPoolForm`
            //     documents exactly this, which is why it refuses to call a
            //     sequence a fresh-pool barrier).
            //
            // Both used to need their own suffix-reconstructing arm here; the
            // mark/spare pair in `allocRegistrationMark` states the rule once
            // and the next multi-pool form inherits it.
            let allocMark = allocRegistrationMark ()
            let (retCode, _) = genBinding bodyCtx tempBinding builder
            suppressAllocsSince allocMark
            stmts @ retCode @ popAllocScopeFrees indent @ [$"{indent}return {retVarName};"]
        | _ ->
            // Return-extent ABI (supersedes the stage-2b guard): a
            // loop-materialized array CAN now be returned. The former guard
            // existed because `Array<T,R>` holds only a POINTER to its
            // extents table, and the hoisted materialization declared that
            // table as a frame-local `size_t[R]` -- returning the wrapper
            // handed the caller a dangling shape pointer, so `c.extents[d]`
            // read garbage even though the data pool (heap) was intact.
            // genObjectForApplication now heap-allocates the table (matching
            // genApplyCombinator, whose array returns already worked for
            // exactly this reason), which makes the wrapper self-describing
            // across the call boundary. No caller-side change is needed: the
            // caller already binds the returned wrapper and reads shape off
            // it (`c.extents[0]`), never off a companion `c_extents`.
            let retStr = exprToCpp currentNames retExpr
            // `return t` where a body let bound `t` to a JOINED reduce: the
            // local is the emitters' flat tuple, the signature says nested.
            // Same reconciliation as the IRReduceCompute arm above, one step
            // later -- the join is a LET here, so it is the plain identifier
            // that reaches the return rather than a __retN. Everything else
            // (a genuine IRTuple, an array, a scalar) passes through untouched.
            //
            // Re-spelling before the suppress-by-token loop below is deliberate
            // and strictly better: for an ARRAY-leaf join it is the per-leaf
            // names that are the REGISTERED allocations, so the rewritten text
            // spares exactly the pools that leave with the value, where the
            // flat tuple's own name matched nothing.
            let retStr = nestedTupleReturn (inferExprType retExpr) retStr currentTupleChildren
            // The returned EXPRESSION may name a registered allocation directly
            // (`return r;`, `return std::make_tuple(r0, r1);`). Whole-token match,
            // so `r_extents` / `rows` do not spuriously spare `r`. Anything the
            // returned value merely READS from is handled by the escape seeds.
            //
            // SCALAR returns take the value-first shape instead: evaluate the
            // return expression into a local BEFORE the frees, so a fold that
            // reads a registered temp in the return statement
            // (`return reduce(t)/N;`) does not spare the temp -- the read
            // happens before the delete, and a scalar cannot smuggle an array
            // out. Without this, every fold-returning helper leaked its
            // whole working set (the memfree-stress gate's failure mode).
            // Array/tuple returns keep the suppress-by-token belt: their
            // frees still sit before `return`, so anything the return names
            // must survive.
            match inferExprType retExpr with
            // A FreshPool-call ARRAY return takes the value-first shape too:
            // the callee's wrapper is a fresh pool (own extents) that can
            // alias none of this scope's bindings, so the call is evaluated
            // into a local BEFORE the frees and the bindings it consumed are
            // genuinely freed. The suppress-by-token belt below would instead
            // spare every binding the return text names -- a per-call leak
            // (measured on 09_qg_atmosphere: `return tendency(.., uq, vq, ph)`
            // sparing three 64x64 fields per H_single call, every timestep).
            // Works with the matching retSeeds narrowing in
            // computeScopeEscapes; NotFresh callees keep the old shape.
            | ArrayElem _ when (match retExpr with
                                | IRApp (f, _, _) -> freshReturnOf f = FreshPool
                                | _ -> false) ->
                let frees = popAllocScopeFrees indent
                if List.isEmpty frees then
                    stmts @ [$"{indent}return {retStr};"]
                else
                    let rv = $"__retv{builder.FreshId()}"
                    stmts
                    @ [$"{indent}auto {rv} = {retStr};"]
                    @ frees
                    @ [$"{indent}return {rv};"]
            // An INTERIOR VIEW of a scope-local array, materialized. `return
            // traj(9999)` hands back a wrapper into the trajectory's pool, and
            // nothing can free that pool afterwards -- not this scope (the view
            // still reads it) and not the caller (it never sees the base). The
            // suppress-by-token belt below would only make that official.
            // Copy the slice into its OWN pool first; the base then falls to
            // the ordinary frees, and the value the caller gets is
            // self-contained. computeScopeEscapes drops the matching return
            // seed and computeFreshReturnFacts promotes the callee to
            // FreshPool, both off this same predicate.
            | ArrayElem sat when (returnedInteriorView lets retExpr).IsSome ->
                let retVarName = $"__ret{builder.FreshId()}"
                let viewName = retVarName + "_vw"
                let extentsName = retVarName + "_extents"
                let rank = arrayRank sat
                let elemStr = elemTypeToCpp sat.ElemType
                // Return-extent ABI (see the IRTranspose arm): a frame-local
                // `size_t[R]` table dangles the moment the wrapper crosses the
                // call boundary. emitExtentsTable gives a static-constexpr
                // table where every extent is literal and a heap one otherwise
                // -- both outlive the frame. The heap table is deliberately NOT
                // registered: it leaves with the return value.
                let dims =
                    [ for d in 0 .. rank - 1 ->
                        match literalExtentOfArray sat d with
                        | Some n -> (string n, true)
                        | None -> ($"{viewName}.extents[{d}]", false) ]
                let (extentsDecl, _leavesWithValue) = emitExtentsTable indent extentsName rank dims
                let allocRhs =
                    match emitAllocRhs AllocDense elemStr rank "nullptr" extentsName with
                    | Ok rhs -> rhs
                    | Error msg -> recordCodegenRefusal msg; $"{{ nullptr, {extentsName} }};\n#error \"{msg}\""
                // The slice is a CONTIGUOUS sub-block of a dense pool
                // (returnedInteriorView proved the leading-prefix, all-scalar,
                // unsymmetric shape), so `pool_base` on the sub-skeleton lands
                // on its first cell and one flat copy_n moves the whole slice.
                let matCode =
                    [ $"{indent}Array<{elemStr}, {rank}> {viewName} = {retStr};" ]
                    @ extentsDecl
                    @ [ $"{indent}Array<{elemStr}, {rank}> {retVarName} = {allocRhs};"
                        $"{indent}size_t {retVarName}_n = count_leaves<typename promote<{elemStr}, {rank}>::type, nullptr>({extentsName});"
                        $"{indent}std::copy_n(pool_base({viewName}.data), {retVarName}_n, pool_base({retVarName}.data));" ]
                stmts @ matCode @ popAllocScopeFrees indent
                @ [$"{indent}return {retVarName};"]
            | ArrayElem _ | IRTTuple _ ->
                for n in registeredAllocNames () do
                    if containsIdentToken retStr n then suppressAllocName n
                stmts @ popAllocScopeFrees indent @ [$"{indent}return {retStr};"]
            | _ ->
                let frees = popAllocScopeFrees indent
                if List.isEmpty frees then
                    stmts @ [$"{indent}return {retStr};"]
                else
                    let rv = $"__retv{builder.FreshId()}"
                    stmts
                    @ [$"{indent}auto {rv} = {retStr};"]
                    @ frees
                    @ [$"{indent}return {rv};"]

/// Emit a function body's statements. Wraps genFuncBodyScoped in the
/// deterministic-deallocation FUNCTION scope: every allocation the body emits
/// registers into that frame and is freed immediately before the single exit,
/// unless the escape analysis (or a return-name suppression) spares it.
let genFuncBody (ctx: CodeGenContext) (builder: IRBuilder) (names: Map<IRId, string>) (indent: string) (body: IRExpr) : string list =
    // Deep unroll: flatten all nested IRLet chains into a flat list
    let rec deepUnroll (expr: IRExpr) : (IRId * IRExpr) list * IRExpr =
        match expr with
        | IRLet (id, value, body) ->
            // Check if value itself contains nested IRLets
            let (innerLets, innerFinal) = deepUnroll value
            let (restLets, restFinal) = deepUnroll body
            // If value had nested lets, emit those first, then bind the final value
            match innerLets with
            | [] -> ((id, value) :: restLets, restFinal)
            | _ -> (innerLets @ [(id, innerFinal)] @ restLets, restFinal)
        | _ -> ([], expr)
    let (lets0, retExpr0) = deepUnroll body
    // Expression-position loop materialization. Synthesized
    // elementwise/broadcast loops (IRApp over IRObjectFor) are statement-
    // shaped: at module level genBinding materializes them, but inside a
    // function body they would otherwise fall through exprToCpp's "loop
    // object used as value" sentinel whenever they sit in return or argument
    // position (`center(a) = a - mymean(a)`, `mymean((a-..)*(b-..))`).
    // Hoist every such application -- and any IRLet chain wrapping one, e.g.
    // the broadcast's hoisted-scalar let -- bottom-up into the flat let list,
    // dependencies first, leaving an IRVar in its place. The let-dispatch
    // below then routes each through genBinding exactly like the
    // IRArrayLit/IRForRange arms. Recursion stays within eager value
    // positions (apps, binops, tuples, unary, index); other shapes keep
    // their existing behavior.
    let rec hoistLoopApps (e: IRExpr) : (IRId * IRExpr) list * IRExpr =
        match e with
        | IRLet (id, v, b) ->
            let (lv, v') = hoistLoopApps v
            let (lb, b') = hoistLoopApps b
            (lv @ [(id, v')] @ lb, b')
        | IRApp (IRObjectFor info, args, ty) ->
            let hs = args |> List.map hoistLoopApps
            let tmp = builder.FreshId()
            ((hs |> List.collect fst) @ [(tmp, IRApp (IRObjectFor info, hs |> List.map snd, ty))],
             IRVar (tmp, ty))
        | IRApp (f, args, ty) ->
            let hs = args |> List.map hoistLoopApps
            (hs |> List.collect fst, IRApp (f, hs |> List.map snd, ty))
        | IRBinOp (m, op, l, r) ->
            let (ll, l') = hoistLoopApps l
            let (lr, r') = hoistLoopApps r
            (ll @ lr, IRBinOp (m, op, l', r'))
        | IRUnaryOp (op, i) ->
            let (li, i') = hoistLoopApps i
            (li, IRUnaryOp (op, i'))
        | IRTuple es ->
            let hs = es |> List.map hoistLoopApps
            (hs |> List.collect fst, IRTuple (hs |> List.map snd))
        | IRIndex (a, idxs, ident) ->
            let (la, a') = hoistLoopApps a
            let hs = idxs |> List.map hoistLoopApps
            (la @ (hs |> List.collect fst), IRIndex (a', hs |> List.map snd, ident))
        | _ -> ([], e)
    let lets =
        (lets0 |> List.collect (fun (id, v) ->
            let (lv, v') = hoistLoopApps v
            lv @ [(id, v')]))
    let (retLets, retExpr) = hoistLoopApps retExpr0
    let lets = lets @ retLets
    // Second hoist: FreshPool calls buried in ARGUMENT position (g(f(x))),
    // whose temporaries the scope tracker could otherwise never register.
    // Runs before computeScopeEscapes so the minted lets are in its domain.
    let lets =
        lets |> List.collect (fun (id, v) ->
            let (hv, v') = hoistFreshPoolCallArgs builder v
            hv @ [(id, v')])
    let (retHoist, retExpr) = hoistFreshPoolCallArgs builder retExpr
    let lets = lets @ retHoist
    let escapes =
        computeScopeEscapes ctx (FuncScope (if isUnitExpr retExpr then None else Some retExpr)) lets
    let allocDepth = allocScopeDepth ()
    pushAllocScope SFunc escapes
    try genFuncBodyScoped ctx builder names indent lets retExpr
    finally truncateAllocScopeStack allocDepth

// Shadow-stack frames: emission markers and panic-reachability resolution
//
// Every emitted function body opens with a `BLADE_FRAME` so a runtime panic
// can print a Blade call stack. The shadow stack is WRITTEN only by
// `blade_rt::Scope`, READ only by `blade_rt::panic`, so a frame is observable
// exactly when a panic is REACHABLE while it's live -- a body reaching no
// panic never appears in a trace, and dropping its frame is invisible.
//
// Worth doing: the frame sits in the hot loop of every inlined arithmetic
// kernel. Measured (ucrt64 g++ 15.2): 1.81 ns/element framed vs 1.24 unframed
// -- the gap is the per-element store to `stack[]`, which the compiler can't
// prove doesn't alias the user pool, blocking vectorization. The 8-lane
// Path B fold calls its kernel wrapper K times per iteration, so a surviving
// frame is paid K times over.
//
// Reachability is a whole-module property, so emission can't decide it
// locally: each body opens with a MARKER carrying its C++ name, and
// `resolveShadowFrames` fixpoints over every emitted bucket, rewriting each
// marker into a `BLADE_FRAME` statement or nothing. genModule/genModuleSplit
// are the only producers and both resolve before returning.
//
// The analysis reads EMITTED TEXT, not the IR, on purpose: this file has 20+
// panic-emitting sites reached through as many paths, and a new one keeps its
// frames automatically instead of needing a mirror-image rule to rot.

let private frameMarkBegin = "//__BLADE_FRAME__"
let private frameMarkEnd = "//__BLADE_FRAME_END__"

/// C++ keywords that are followed by `(` but are not calls.
let private cppNonCallKeywords =
    Set.ofList [ "if"; "for"; "while"; "switch"; "do"; "else"; "return"; "case";
                 "catch"; "throw"; "sizeof"; "alignof"; "decltype"; "noexcept";
                 "and"; "or"; "not"; "new"; "delete" ]

/// Namespaces whose functions this file does not generate, and which therefore
/// cannot reach `blade_rt::panic`. That is a real source-tree invariant, not an
/// assumption: the panic-containment tripwire in tests/Test_Diagnostics.fs
/// fails if any header under src/cpp other than blade_runtime.hpp gains a
/// panic call. `blade_rt::` is deliberately absent -- panic itself lives there.
let private panicFreeNamespaces =
    [ "std::"; "thrust::"; "nested_array_utilities::"; "orbit_wreath_utilities::"
      "blade_linalg::"; "blade_lapack::"; "blade_rand::"; "linearized_storage::"
      "symmetric::"; "antisymmetric::" ]

/// Call-shaped tokens in emitted C++: an optionally-qualified `ns::name(`,
/// plus the three spellings that name no identifier before the paren --
/// `f<T>(x)` (template call), `(*fp)(x)` / `g()(x)` (functor call through a
/// value). A LAMBDA HEADER `[&](` is deliberately NOT call-shaped: it
/// defines, it does not call. Its body text is inline in this same scanned
/// body, so a panic inside it is caught by the direct substring check and
/// its calls are collected as this body's callees by this same scan; the
/// escaping-lambda case is covered on the RECEIVING side, where a
/// function-typed parameter forces the frame (shadowFrameOpen). Before the
/// lookbehind, every let-block's IIFE marked its function Unknown, keeping
/// a shadow frame on every block-bodied function -- and the frame's RAII
/// destructor takes a recursive call out of tail position: measured 3.2x on
/// a self-recursive escape-time kernel g++ otherwise turns into a register
/// loop. `table[i](x)` (call through an indexed slot) stays call-shaped.
let private callShapedRe =
    System.Text.RegularExpressions.Regex(
        @"(?<qual>(?:[A-Za-z_][A-Za-z0-9_]*::)+)?(?<id>[A-Za-z_][A-Za-z0-9_]*)\s*\(|(?<ind>(?:[>)]|(?<!\[&)\])\s*\()",
        System.Text.RegularExpressions.RegexOptions.Compiled)

/// What one emitted body does that bears on whether its frame is observable.
type private FrameFacts = {
    /// The body itself panics, or the signature admits an indirect call.
    Forced: bool
    /// The body calls something this analysis cannot follow, so it must be
    /// assumed to reach a panic.
    Unknown: bool
    /// Calls to other emitted functions, by C++ name.
    Callees: Set<string>
}

/// Classify a body's calls. A call is FOLLOWED when it names another emitted
/// function, IGNORED when it lands in a panic-free namespace or is a keyword,
/// and UNKNOWN otherwise -- an unqualified name this analysis cannot resolve is
/// assumed to reach a panic, because a call through a function-typed parameter
/// (`kern(x, y)`, where `kern` is a `std::function` argument) looks exactly
/// like one and CAN reach a panic in whatever it was bound to.
let private scanBodyCalls (peers: Set<string>) (body: string) : bool * Set<string> =
    let ms = callShapedRe.Matches body
    let mutable unknown = false
    let mutable callees = Set.empty
    for i in 0 .. ms.Count - 1 do
        let m = ms.[i]
        if m.Groups.["ind"].Success then unknown <- true
        else
            let qual = m.Groups.["qual"].Value
            let id = m.Groups.["id"].Value
            if qual <> "" then
                if not (panicFreeNamespaces |> List.exists (fun ns -> qual.StartsWith ns)) then
                    unknown <- true
            elif Set.contains id cppNonCallKeywords then ()
            elif Set.contains id peers then callees <- Set.add id callees
            else unknown <- true
    (unknown, callees)

/// Open an emitted function body with a resolvable shadow-frame marker.
/// `forced` covers what the body text alone cannot see: a function-typed
/// parameter or capture can be CALLED, reaching a panic in a callee this body
/// never names.
let private shadowFrameOpen (funcDef: IRFuncDef) (bodyInd: string) : string list =
    let forced =
        (funcDef.Params |> List.exists (fun p -> match p.Type with FuncElem _ -> true | _ -> false))
        || (funcDef.Captures |> List.exists (fun c -> match c.Type with FuncElem _ -> true | _ -> false))
    [ sprintf "%s%s %s %d BLADE_FRAME(\"%s\", nullptr, 0);"
        bodyInd frameMarkBegin (sanitizeCppName funcDef.Name)
        (if forced then 1 else 0) (cppStrEscape funcDef.Name) ]

/// Close a marked body. Delimiting the body explicitly keeps the scan exact:
/// without it a body would run to the next marker and pick up the following
/// function's signature line as a phantom call.
let private shadowFrameClose (bodyInd: string) : string list = [bodyInd + frameMarkEnd]

/// Resolve every frame marker in `buckets`: compute panic reachability over the
/// emitted call graph, then replace each marker with its `BLADE_FRAME`
/// statement (reachable) or drop it (not), dropping every end marker too.
/// Buckets are analyzed together and rewritten separately, because a file-scope
/// function and a main-local lambda land in different ones but call each other.
let private resolveShadowFrames (buckets: string list list) : string list list =
    // Pass 1: carve out each marked body and record its name, indent, and the
    // statement to emit if it turns out to be observable.
    let facts = System.Collections.Generic.Dictionary<string, FrameFacts>()
    let bodies = ResizeArray<string * string>()   // name, body text
    for bucket in buckets do
        let mutable current : (string * bool * ResizeArray<string>) option = None
        for line in bucket do
            let t = line.TrimStart()
            if t.StartsWith frameMarkBegin then
                let parts = t.Split([|' '|], 4)
                // parts = [| marker; cppName; forcedFlag; statement |]
                if parts.Length = 4 then current <- Some (parts.[1], parts.[2] = "1", ResizeArray())
            elif t.StartsWith frameMarkEnd then
                match current with
                | Some (name, forced, acc) ->
                    let body = String.concat "\n" acc
                    bodies.Add((name, body))
                    let direct = forced || body.Contains "blade_rt::panic("
                    // MERGE, never overwrite: two emitted functions can share a
                    // C++ name (a file-scope one and a main-local one, or two
                    // modules merged). A call site naming it might reach either,
                    // so the node must carry the union of what both do -- taking
                    // just the last would let a panic-free twin drop the frame
                    // off a body that panics.
                    let prev =
                        match facts.TryGetValue name with
                        | true, p -> p
                        | _ -> { Forced = false; Unknown = false; Callees = Set.empty }
                    facts.[name] <- { prev with Forced = prev.Forced || direct }
                    current <- None
                | None -> ()
            else
                match current with
                | Some (_, _, acc) -> acc.Add line
                | None -> ()
    // Pass 2: classify calls now that every emitted name is known.
    let peers = facts.Keys |> Set.ofSeq
    for (name, body) in bodies do
        let (unknown, callees) = scanBodyCalls peers body
        let f = facts.[name]
        // Union here too, for the shared-name case (see pass 1).
        facts.[name] <- { f with Unknown = f.Unknown || unknown
                                 Callees = Set.union f.Callees callees }
    // Pass 3: least fixpoint of `needs(f) = forced(f) || unknown(f) ||
    // exists g in callees(f). needs(g)`. Starting from false means a function
    // that only recurses into itself does not force its own frame.
    let needs = System.Collections.Generic.HashSet<string>()
    for KeyValue(name, f) in facts do
        if f.Forced || f.Unknown then needs.Add name |> ignore
    let mutable changed = true
    while changed do
        changed <- false
        for KeyValue(name, f) in facts do
            if not (needs.Contains name) && f.Callees |> Set.exists needs.Contains then
                needs.Add name |> ignore
                changed <- true
    // Pass 4: rewrite.
    buckets
    |> List.map (fun bucket ->
        bucket |> List.choose (fun line ->
            let t = line.TrimStart()
            if t.StartsWith frameMarkBegin then
                let parts = t.Split([|' '|], 4)
                if parts.Length = 4 && needs.Contains parts.[1] then
                    Some (line.Substring(0, line.Length - t.Length) + parts.[3])
                else None
            elif t.StartsWith frameMarkEnd then None
            else Some line))

let genFuncDef (ctx: CodeGenContext) (builder: IRBuilder) (funcDef: IRFuncDef) : string list * CodeGenContext =
    let ind = indentStr ctx
    let bodyInd = ind + "    "

    // Array parameters are wrappers. Body reads shape via
    // `<name>.extents[d]`, `<name>.lens[d]`, `<name>.offsets[d]`. No need
    // for separate body-level aliases -- the wrapper IS the binding.
    let paramStr (name: string) (ty: IRType) : string =
        match ty with
        | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {name}"
        | _ -> $"{(irTypeToCpp ty)} {name}"
    let captureParamStr (cap: CaptureInfo) : string =
        // Captures are appended after the regular params, pass-by-reference so
        // mutation propagates and lifetimes tie to the wrapper's `[&]` capture
        // at the use site: `T&` for plain types, `Array<T, N>&` for arrays.
        //
        // A GROUPED capture (a `group_by` result -- see the grouped-capture
        // forwarding block) is typed as the row-pointer table it actually is,
        // `Array<T*, 1>&`; the IR rank-2 rendering would not bind to the call
        // site's value.
        //
        // Function-typed captures use `const std::function<...>&` instead:
        // Blade's top-level `function name(args) = body` emits an ordinary C++
        // function, whose name denotes a function reference, not a
        // std::function value -- a non-const reference param can't bind to
        // that rvalue (C++ would need a temporary std::function, which can't
        // bind non-const), but a const reference can. Trade-off: no
        // mutation-through-capture for function values, which is fine since
        // they're immutable bindings in Blade.
        match cap.Type with
        | ArrayElem arr when (groupedCaptureGkOf cap).IsSome ->
            $"Array<{(elemTypeToCpp arr.ElemType)}*, 1>& {cap.Name}"
        | ArrayElem arr -> $"{(cppArrayTypeStr arr)}& {cap.Name}"
        | FuncElem _ -> $"const {(irTypeToCpp cap.Type)}& {cap.Name}"
        | _ -> $"{(irTypeToCpp cap.Type)}& {cap.Name}"
    let regularParams = funcDef.Params |> List.map (fun p -> paramStr p.Name p.Type)
    let captureParams = (funcDef.Captures |> List.map captureParamStr) @ gkSidecarParams funcDef.Captures
    let paramList = (regularParams @ captureParams) |> String.concat ", "

    // Use declared return type, or infer from body as fallback
    let retType = 
        match funcDef.RetType with
        | IRTInfer _ -> irTypeToCpp (inferExprType funcDef.Body)  // Should not happen with typed IR
        | ArrayElem arr -> cppArrayTypeStr arr
        | t -> irTypeToCpp t

    // Build name map: regular params + captures both contribute, so the
    // body's IRVar references to captured variables resolve to the same
    // C++ name the signature declared.
    let bodyNames =
        funcDef.Params
        |> List.fold (fun m p -> Map.add p.VarId p.Name m) ctx.VarNames
    let bodyNames =
        funcDef.Captures
        |> List.fold (fun m c -> Map.add c.Id c.Name m) bodyNames

    // Generate proper C++ function
    let safeName = sanitizeCppName funcDef.Name
    // Grouped captures: seed the body context so the peels resolve per-row
    // lengths through the forwarded `__gk<id>__*` params exactly as they
    // resolve them in the frame that built the group (grouped-capture
    // forwarding, requirement 3).
    //
    // The stem is ALWAYS `__gk<id>` here, because that is what
    // `gkSidecarParams` names the params this signature declares. It used to
    // be `gkSidecarStem ctx.VarNames gkId`, which only lands on `__gk<id>`
    // when the gk is absent from the name map -- true for a gk local to an
    // enclosing function, false for a MODULE-level one, which the map spells
    // `seg`. Then the body read `seg__ngroups`, a main() local this free
    // function cannot see, while the forwarded `__gk<id>__ngroups` sat unused.
    let bodyCtx =
        funcDef.Captures
        |> List.fold (fun c cap ->
            match groupedCaptureGkOf cap with
            | Some gkId ->
                { c with GroupedArrays = Map.add cap.Name $"__gk{gkId}" c.GroupedArrays }
            | None -> c)
            // The body's parameters, for the co-iteration extent guard
            // (CodeGenContext.ParamIds).
            { ctx with ParamIds = funcDef.Params |> List.map (fun p -> p.VarId) |> Set.ofList }
    // A captured GROUPING (`group_keys` / `segments` / `tiles` ...) is refused
    // rather than emitted into g++'s "was not declared in this scope". The
    // binding is an opaque `void*` sentinel whose state lives in locals named
    // after it (`<gk>__ngroups`, `__offsets`, `__perm`, `__at`, grid bounds)
    // in the scope that bound it, and a body can only use a grouping through
    // that state (`group_by(x, gk)`, the gk accessors) -- so a free function
    // capturing one has never compiled, called or not, and refusing it takes
    // nothing that worked. A captured group_by RESULT is different and fine:
    // its row table is a real value and its gk's ngroups/offsets are
    // forwarded (grouped-capture forwarding, seeded above).
    //
    // A named function using a grouping works, because it is emitted as a
    // `[&]` closure in main() (genFuncDefAsLambda) that resolves the state by
    // name. A lifted kernel cannot simply take that route: its call sites
    // forward the captures as arguments and its closure would be emitted after
    // the binding that applies it. Both steers in the message compile and run.
    //
    // NOT refused here: a captured STREAMED variable. A lifted function that
    // merely receives it as `Array<T, N>& A` compiles, and is dead code
    // whenever the consumer inlines the kernel (the streamed halo stencil) --
    // what breaks is a CALLER that names the never-materialized `A`, which is
    // a different site.
    let unforwardableCaptureRefusal =
        funcDef.Captures |> List.tryPick (fun cap ->
            match cap.Type with
            | IRTGroupKeys _ ->
                let outer = captureForwardName ctx.VarNames cap
                Some ($"a kernel body captures the grouping '{outer}', but the body is compiled as a separate "
                      + "C++ function and a grouping's state lives in locals named after its binding "
                      + $"(`{outer}__ngroups`, `{outer}__offsets`, ...) that the function cannot see. Group "
                      + $"outside the kernel (`let g = group_by(x, {outer})`) and use `g` inside it -- a "
                      + "group_by result IS forwarded, and grouping once beats regrouping on every "
                      + "iteration -- or move the body into a named function")
            | _ -> None)
    // A captured `.stream` variable IS an array in this body -- the signature
    // receives it as a parameter -- so it reads by name here, and no
    // stream-eligible consumer in the body may take the store-reading route
    // (whose state is main() locals). What cannot work is FORWARDING it from
    // a scope that never materialized it; captureForwardArgs refuses that at
    // the call site. A lifted kernel whose consumer inlines it instead (the
    // streamed halo stencil) is dead code and costs nothing.
    let streamedCaptureNames = funcDef.Captures |> List.choose (fun c -> streamedBindingName c.Id)
    use _captureMask = maskStreamedNames streamedCaptureNames
    let bodyCtx =
        { bodyCtx with
            StreamedArrays = streamedCaptureNames |> List.fold (fun m n -> Map.remove n m) bodyCtx.StreamedArrays }
    // `where repro`: the body emits inside the routing veto scope (no
    // BLAS/LAPACK/cuBLAS classification while depth > 0), and the definition
    // carries BLADE_REPRO_FN (noinline + fp-contract off on GCC). try/finally
    // so a codegen exception cannot leave the scope stuck on.
    // Scratch reuse: the plan's donor for THIS body's return position, read
    // by genFuncBody's return arm; cleared after so no other body sees it.
    Blade.Types.PoolReuseTable.setCurrentReturnDonor (Blade.Types.PoolReuseTable.tryReturnDonor funcDef.Id)
    let bodyStmts =
        try
            match unforwardableCaptureRefusal with
            | Some msg -> codegenError ctx bodyInd msg
            | None ->
            if funcDef.IsRepro then
                Blade.LinAlgPatterns.reproScopeDepth.Value <-
                    Blade.LinAlgPatterns.reproScopeDepth.Value + 1
                try genFuncBody bodyCtx builder bodyNames bodyInd funcDef.Body
                finally
                    Blade.LinAlgPatterns.reproScopeDepth.Value <-
                        Blade.LinAlgPatterns.reproScopeDepth.Value - 1
            else genFuncBody bodyCtx builder bodyNames bodyInd funcDef.Body
        finally Blade.Types.PoolReuseTable.setCurrentReturnDonor None
    // Shadow-stack frame: named as the Blade function so a runtime
    // panic prints a Blade call stack. file/line are nullptr/0 because
    // IRCallable carries no span (adding one touches TypeCheck.fs's IRCallable
    // constructions) -- the function name is the main win. Host-only via the
    // BLADE_FRAME macro's CUDA guard.
    // Emitted as a MARKER, not the statement: whether this body can reach a
    // panic depends on what its callees do. resolveShadowFrames settles it.
    let reproAttr = if funcDef.IsRepro then "BLADE_REPRO_FN " else ""
    let reproNote =
        if funcDef.IsRepro
        then [$"{bodyInd}// [repro] contraction off, noinline; library routing and reorder licences vetoed in this body"]
        else []
    let code =
        [$$"""{{ind}}{{reproAttr}}{{retType}} {{safeName}}({{paramList}}) {"""]
        @ reproNote
        @ shadowFrameOpen funcDef bodyInd
        @ bodyStmts
        @ shadowFrameClose bodyInd
        @ [$"{ind}}}"]

    let ctx' = addVarName funcDef.Id funcDef.Name ctx
    (code, ctx')

/// Generate a function as a C++ lambda (for functions that capture module-level bindings)
let genFuncDefAsLambda (ctx: CodeGenContext) (builder: IRBuilder) (funcDef: IRFuncDef) : string list * CodeGenContext =
    let ind = indentStr ctx

    // Array params are wrappers; same approach as genFuncDef.
    let paramList =
        funcDef.Params
        |> List.map (fun p ->
            match p.Type with
            | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {p.Name}"
            | _ -> $"{(irTypeToCpp p.Type)} {p.Name}")
        |> String.concat ", "

    let retType =
        match funcDef.RetType with
        | IRTInfer _ -> irTypeToCpp (inferExprType funcDef.Body)
        | ArrayElem arr -> cppArrayTypeStr arr
        | t -> irTypeToCpp t

    let bodyNames = funcDef.Params |> List.fold (fun m p -> Map.add p.VarId p.Name m) ctx.VarNames
    // Parity with genFuncDef (which folds captures in alongside params): a
    // source-level `function` always has Captures = [], but the main-locality
    // fixpoint can route a lifted callable with non-empty Captures here, and
    // its body's IRVar references must resolve to the same names.
    let bodyNames = funcDef.Captures |> List.fold (fun m c -> Map.add c.Id c.Name m) bodyNames
    let safeName = sanitizeCppName funcDef.Name
    // std::function type with one param type per Blade param (no companion args).
    let paramTypeList =
        funcDef.Params
        |> List.map (fun p ->
            match p.Type with
            | ArrayElem arr -> cppArrayTypeStr arr
            | _ -> irTypeToCpp p.Type)
        |> String.concat ", "
    let funcType = $"std::function<{retType}({paramTypeList})>"
    // Statement-form body via genFuncBody -- the same renderer proper C++
    // functions use -- so for-in loops, local array literals, and element
    // assignment work in captured functions too. A bare `return <exprToCpp
    // body>` form would silently DROP loop and assignment statements: a
    // captured function containing a for-in would compile to just its final
    // expression.
    let bodyInd = ind + "    "
    let bodyCtx = { ctx with VarNames = bodyNames; Indent = ctx.Indent + 1 }
    // Grouped captures resolve by NAME here (the [&] closure sees main's
    // locals), so only the GroupedArrays seed is needed -- the stem is the
    // gk's emitted name in the enclosing scope, no hidden params.
    let bodyCtx =
        funcDef.Captures
        |> List.fold (fun c cap ->
            match groupedCaptureGkOf cap with
            | Some gkId ->
                { c with GroupedArrays = Map.add cap.Name (gkSidecarStem ctx.VarNames gkId) c.GroupedArrays }
            | None -> c) bodyCtx
    // `where repro` on a MAIN-LOCAL function: the routing veto scope still
    // applies (no library dispatch in this body), but a std::function lambda
    // cannot carry the GCC `optimize` attribute, so the contraction half of
    // the demand is NOT dischargeable here. Say so in the emitted text --
    // never silently -- rather than refuse the whole program.
    // Scratch reuse: the plan's donor for THIS body's return position, read
    // by genFuncBody's return arm; cleared after so no other body sees it.
    Blade.Types.PoolReuseTable.setCurrentReturnDonor (Blade.Types.PoolReuseTable.tryReturnDonor funcDef.Id)
    let bodyStmts =
        try
            if funcDef.IsRepro then
                Blade.LinAlgPatterns.reproScopeDepth.Value <-
                    Blade.LinAlgPatterns.reproScopeDepth.Value + 1
                try genFuncBody bodyCtx builder bodyNames bodyInd funcDef.Body
                finally
                    Blade.LinAlgPatterns.reproScopeDepth.Value <-
                        Blade.LinAlgPatterns.reproScopeDepth.Value - 1
            else genFuncBody bodyCtx builder bodyNames bodyInd funcDef.Body
        finally Blade.Types.PoolReuseTable.setCurrentReturnDonor None
    let reproNote =
        if funcDef.IsRepro
        then [$"{bodyInd}// [repro] main-local emission: routing vetoed, but the contraction attribute cannot attach to a lambda -- bind this function's inputs at module level to restore the full guarantee"]
        else []
    // Shadow-stack frame; see genFuncDef. Name-only (nullptr/0),
    // and marker-form so resolveShadowFrames can drop it if no panic is
    // reachable from this body.
    let code =
        [$$"""{{ind}}{{funcType}} {{safeName}} = [&]({{paramList}}) -> {{retType}} {"""]
        @ reproNote
        @ shadowFrameOpen funcDef bodyInd
        @ bodyStmts
        @ shadowFrameClose bodyInd
        @ [$"{ind}}};"]
    let ctx' = addVarName funcDef.Id funcDef.Name ctx
    (code, ctx')

/// Generate C++ code for an entire IR module.
/// Returns (functionDefs, bindingCode) -- functions go outside main(), bindings inside.
/// Forward declarations for file-scope functions. Factored out so genModule
/// and genModuleSplit share one source of truth. Signature uses Array<T,N> /
/// Ragged<T> wrappers; captures appear after regular params as additional
/// reference-typed args, matching genFuncDef's emission.
let private genForwardDecls (fileScopeFuncs: IRFuncDef list) : string list =
    let decls =
        fileScopeFuncs |> List.map (fun funcDef ->
            let paramList =
                funcDef.Params
                |> List.map (fun p ->
                    match p.Type with
                    | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {p.Name}"
                    | _ -> $"{(irTypeToCpp p.Type)} {p.Name}")
            let captureList =
                funcDef.Captures
                |> List.map (fun cap ->
                    match cap.Type with
                    // Grouped capture: the row-pointer table form, matching
                    // genFuncDef's captureParamStr token for token.
                    | ArrayElem arr when (groupedCaptureGkOf cap).IsSome ->
                        $"Array<{(elemTypeToCpp arr.ElemType)}*, 1>& {cap.Name}"
                    | ArrayElem arr -> $"{(cppArrayTypeStr arr)}& {cap.Name}"
                    | FuncElem _ -> $"const {(irTypeToCpp cap.Type)}& {cap.Name}"
                    | _ -> $"{(irTypeToCpp cap.Type)}& {cap.Name}")
            let allParams = (paramList @ captureList @ gkSidecarParams funcDef.Captures) |> String.concat ", "
            let retType =
                match funcDef.RetType with
                | IRTInfer _ -> irTypeToCpp (inferExprType funcDef.Body)
                | ArrayElem arr -> cppArrayTypeStr arr
                | t -> irTypeToCpp t
            let safeName = sanitizeCppName funcDef.Name
            // Attribute on the declaration too: GCC wants function
            // attributes visible at the first declaration.
            let reproAttr = if funcDef.IsRepro then "BLADE_REPRO_FN " else ""
            $"{reproAttr}{retType} {safeName}({allParams});")
    if decls.IsEmpty then [] else decls @ [""]

/// Classify a binding as a "computation" (forced combinator / compute) vs
/// "data setup" (array literals, scalar lets, plain values). Used only by the
/// split-timing path to decide which timing phase a binding's emitted code
/// belongs to. Walks past IRLet/IRCompute wrappers to the operative form.
let rec private isComputeBindingExpr (e: IRExpr) : bool =
    match e with
    | IRCompute _ -> true
    | IRApplyCombinator _ | IRComposeApply _ | IRReynolds _
    | IRMethodFor _ | IRObjectFor _ | IRBind _ | IRParallel _
    | IRFusion _ | IRChoice _ | IRFallback _ | IRArrayProduct _ | IRComposeObj _
    | IRComposeMeth _ | IRCompose _ | IRFunctorMap _ | IRPure _
    | IRReplicate _ | IRSequence _ -> true
    | IRLet (_, _, body) -> isComputeBindingExpr body
    | _ -> false

let private isComputeBinding (b: IRBinding) : bool =
    isComputeBindingExpr b.Value

/// Compute the set of functions that must be emitted INSIDE main() as
/// std::function lambda bindings (genFuncDefAsLambda) rather than as free
/// C++ functions (genFuncDef). A function is main-local if its body has a
/// free variable naming a module-level binding (that binding only exists as
/// a local inside main), or -- transitively -- if it references another
/// main-local function: a free C++ function calling a main()-local
/// std::function fails compilation with "'<name>' was not declared in this
/// scope". References a function already receives as explicit capture
/// parameters (lifted lambdas with function-typed captures) do NOT
/// propagate -- the call-site wrapper closes over those inside main, so the
/// callee's main-locality never leaks into the lifted function's body.
///
/// A body's free variables are not just the ones it SPELLS. By the time this
/// runs, a kernel lambda has been lifted into its own IRCallable and the body
/// retains only `IRVar(lambdaId)` -- so a module binding the kernel reads
/// survives only in that callable's `Captures`, and `collectVarRefsIR` (a
/// syntactic id walk) cannot see it. Whoever NAMES the callable is the one
/// that has to supply those captures, whether the callee is inlined into this
/// body's loop nest or called through a genCallableWrapper that forwards them
/// as arguments. So a body also inherits the capture obligations of every
/// callable it names, split by kind: module-binding captures make it
/// main-local outright, function-typed captures become extra edges for the
/// fixpoint below. That is the opposite direction from the paragraph above --
/// there, a callee's main-locality must not leak outward into a lifted body;
/// here, a caller inherits its callee's UNMET obligations.
let private computeMainLocalFuncIds (modul: IRModule) (ctx0: CodeGenContext) : Set<IRId> =
    let funcIds = modul.Functions |> List.map (_.Id) |> Set.ofList
    let capturesById =
        modul.Functions
        |> List.map (fun f -> (f.Id, f.Captures |> List.map (_.Id) |> Set.ofList))
        |> Map.ofList
    // The callables a body NAMES, as opposed to receives: intersecting with
    // funcIds drops params (a param's VarId is never a function id) and
    // subtracting the body's own captures drops function-typed captures.
    let uncapturedFuncRefs =
        modul.Functions
        |> List.map (fun f ->
            let captureIds = f.Captures |> List.map (_.Id) |> Set.ofList
            (f.Id, Set.difference (Set.intersect (collectVarRefsIR f.Body) funcIds) captureIds))
        |> Map.ofList
    let inheritedCaptures (funcDef: IRFuncDef) =
        uncapturedFuncRefs.[funcDef.Id]
        |> Set.fold (fun acc g ->
            Set.union acc (Map.tryFind g capturesById |> Option.defaultValue Set.empty)) Set.empty
    let capturesModuleBinding (funcDef: IRFuncDef) =
        let paramIds = funcDef.Params |> List.map (_.VarId) |> Set.ofList
        let captureIds = funcDef.Captures |> List.map (_.Id) |> Set.ofList
        let bound = Set.unionMany [paramIds; captureIds; funcIds]
        let spelled = Set.difference (collectVarRefsIR funcDef.Body) bound
        // Subtract `bound` AFTER the fold, never inside it: a self-recursive
        // lifted callable names itself, and a callee whose captures are all
        // params of THIS function is already satisfied here. Both would
        // false-positive otherwise. Subtracting funcIds is load-bearing too --
        // function ids are in ctx0.VarNames, so a function-typed capture would
        // otherwise flag every caller; those are handled as edges instead.
        let inherited = Set.difference (inheritedCaptures funcDef) bound
        Set.union spelled inherited |> Set.exists (fun id -> Map.containsKey id ctx0.VarNames)
    // Function-typed captures of a named callee, as fixpoint edges. Note the
    // deliberate asymmetry with `capturesModuleBinding`: that one subtracts the
    // full `bound` (funcIds included), this one keeps funcIds -- they are the
    // whole point -- and subtracts only what is genuinely bound locally.
    let inheritedFuncRefs =
        modul.Functions
        |> List.map (fun f ->
            let localBound =
                Set.union
                    (f.Params |> List.map (_.VarId) |> Set.ofList)
                    (f.Captures |> List.map (_.Id) |> Set.ofList)
            (f.Id, Set.difference (Set.intersect (inheritedCaptures f) funcIds) localBound))
        |> Map.ofList
    let refEdges (id: IRId) = Set.union uncapturedFuncRefs.[id] inheritedFuncRefs.[id]
    // A callable that captures another FUNCTION'S PARAMETER can never be a
    // main-local sibling, whatever its module captures say.
    //
    // Main-locality works by lexical `[&]` capture: the callable is emitted as
    // a `std::function` local in main(), so every name it closes over must be
    // in scope THERE. Module bindings are (they are main's locals). A function
    // parameter is not -- it exists only inside that function's body -- so the
    // emitted closure names an undeclared identifier
    // ("'shift' was not declared in this scope").
    //
    // Such a callable is a lifted kernel of an enclosing function: it belongs
    // at namespace scope, receiving those captures as ordinary capture params
    // forwarded at the call site inside its parent. Its MODULE captures are
    // then served by the S0 declaration promotion
    // (computeModuleCaptureHoistIds), which exists for exactly this shape --
    // the two mechanisms are alternative answers to "a module binding is a
    // main() local", and only promotion can serve a namespace-scope referrer.
    //
    // Measured on examples/lswosa.blade: `family_spectra`'s grid kernel
    // captures the enclosing function's `shift`/`seg_span`/`freqs`/`t_zero`
    // AND transitively inherits `two_pi` from `wosa_lsdft`, so the inherited
    // module binding alone would have made it main-local.
    let allParamIds =
        modul.Functions
        |> List.collect (fun f -> f.Params |> List.map (_.VarId))
        |> Set.ofList
    // Tested on captures AND spelled body references, because main-local
    // emission drops the capture params entirely (that is the point of `[&]`),
    // so a clone can reach an enclosing param either way. Own params are
    // subtracted -- they are bound here.
    let usesEnclosingParam (funcDef: IRFuncDef) =
        let ownParams = funcDef.Params |> List.map (_.VarId) |> Set.ofList
        let referenced =
            Set.union
                (funcDef.Captures |> List.map (_.Id) |> Set.ofList)
                (collectVarRefsIR funcDef.Body)
        Set.difference (Set.intersect referenced allParamIds) ownParams |> Set.isEmpty |> not
    // Excluded from main-locality ENTIRELY -- not just from `direct`. The
    // fixpoint below promotes any caller of a main-local callee, and this
    // kernel calls one (`wosa_lsdft`, which captures `two_pi` directly), so
    // filtering only the seed left it re-added one round later.
    let cannotBeMainLocalSeed =
        modul.Functions |> List.filter usesEnclosingParam |> List.map (_.Id) |> Set.ofList
    // Propagated DOWNWARD along the reference edges: a namespace-scope C++
    // function cannot call a `std::function` local of main(), so everything
    // reachable from something that cannot be main-local must also stay at
    // namespace scope. (The main-local fixpoint below propagates the opposite
    // way, caller-wards, which is why both directions are needed and why they
    // are computed as separate closures.) Whatever module bindings these
    // callees capture are served by the S0 declaration promotion instead.
    //
    // Measured on examples/lswosa.blade: with only the seed excluded, the grid
    // kernel became a namespace-scope function calling `hanning`, which had
    // stayed a main-local closure because it captures `two_pi`.
    let cannotBeMainLocal =
        let rec grow (acc: Set<IRId>) =
            let acc' =
                acc |> Set.fold (fun s id ->
                    match Map.tryFind id uncapturedFuncRefs with
                    | Some _ -> Set.union s (refEdges id)
                    | None -> s) acc
            if acc' = acc then acc else grow acc'
        grow cannotBeMainLocalSeed
    let direct =
        modul.Functions
        |> List.filter capturesModuleBinding
        |> List.map (_.Id)
        |> Set.ofList
        |> fun s -> Set.difference s cannotBeMainLocal
    let rec close (acc: Set<IRId>) =
        let acc' =
            modul.Functions
            |> List.fold (fun s f ->
                if Set.contains f.Id s || Set.contains f.Id cannotBeMainLocal then s
                elif not (Set.isEmpty (Set.intersect (refEdges f.Id) s)) then Set.add f.Id s
                else s) acc
        if acc' = acc then acc else close acc'
    close direct

/// The module-level bindings that MUST be nameable at namespace scope -- S0 of
/// docs/plan-kernel-body-materialization.md section 6.
///
/// A callable's captures are forwarded (or, for an inlined kernel body, read
/// directly) in the scope of whoever references it. For a MAIN-LOCAL referrer
/// that scope is inside `main()`, where every module-level binding is already
/// a local, so nothing is needed. For a FILE-SCOPE referrer -- a top-level
/// `function`, or a lifted kernel emitted as a free C++ function -- the module
/// binding has no name at all, and the emitted call/loop names an undeclared
/// identifier.
///
/// So: for every file-scope callable `f`, every callable `c` that `f`'s body
/// references contributes its module-level captures, minus the ones `f` can
/// already name (its own params and its own capture params -- a lifted kernel
/// forwards its inner kernel's captures through its own signature, which is
/// why nesting terminates here rather than cascading).
///
/// The set is deliberately DEMAND-driven: a module binding captured only by
/// kernels whose call sites all sit in `main()` is left exactly where it was.
let private computeModuleCaptureHoistIds
        (modul: IRModule) (mainLocalFuncIds: Set<IRId>) : Set<IRId> =
    let moduleBindingIds = modul.Bindings |> List.map (_.Id) |> Set.ofList
    if Set.isEmpty moduleBindingIds then Set.empty
    else
    let byId = modul.Functions |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    modul.Functions
    |> List.filter (fun f -> not (Set.contains f.Id mainLocalFuncIds))
    |> List.collect (fun f ->
        let nameable =
            Set.union
                (f.Params |> List.map (_.VarId) |> Set.ofList)
                (f.Captures |> List.map (_.Id) |> Set.ofList)
        collectVarRefsIR f.Body
        |> Set.toList
        |> List.collect (fun rid ->
            if rid = f.Id then []
            else
                match Map.tryFind rid byId with
                | Some c ->
                    c.Captures
                    |> List.map (_.Id)
                    |> List.filter (fun cid ->
                        Set.contains cid moduleBindingIds && not (Set.contains cid nameable))
                | None -> []))
    |> Set.ofList

/// Emit one module-level binding, promoting its DECLARATION to namespace scope
/// when `hoistIds` demands it and the emitted shape allows the split (see
/// `tryHoistModuleBindingDecl`). Falls back to the unmodified emission -- the
/// pre-S0 status quo -- for any binding whose definition is not a single
/// ordinary `TYPE NAME = RHS;` line.
let private genModuleBinding
        (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder)
        (hoistIds: Set<IRId>) : string list * CodeGenContext =
    let (code, ctx') = genBinding ctx binding builder
    if not (Set.contains binding.Id hoistIds) then (code, ctx')
    else
        let name = bindingCppName binding
        match tryHoistModuleBindingDecl name code with
        | Some (decl, rewritten) ->
            let cell = moduleGlobalDeclsCell ()
            cell.AppendDistinct decl
            (rewritten, ctx')
        | None ->
            // Status quo, plus a breadcrumb: this binding stays a main() local
            // even though a namespace-scope body wants to name it. Nothing is
            // broken TODAY only because the call that would name it is not
            // generated yet; when it is, the C++ error lands here.
            (code @ [ sprintf "%s// (module binding '%s' is demanded at namespace scope but its emitted \
shape has no single `TYPE %s = ...;` definition line to split -- see tryHoistModuleBindingDecl)"
                              (indentStr ctx) name name ], ctx')

/// The bindings-and-functions emission order, as ONE definition shared by
/// `genModule` and `genModuleSplit` so the two can never drift.
///
/// Order is by IRId -- a lower id was created earlier in the source -- with one
/// correction: a function a pass SYNTHESIZED from another (an entry in
/// `IRModule.DerivedFuncOrigins`, e.g. a shape specialization) is keyed
/// on its ORIGIN's id, not its own. Its own id is freshly minted and therefore
/// the largest in the module, which would sort it after every call site that
/// references it. That is fatal for anything `computeMainLocalFuncIds` puts
/// inside `main()` as a `std::function` local: those get no forward
/// declaration, so a use before the definition is "'<name>' was not declared in
/// this scope" rather than a link-time detail.
///
/// Producers place the copy immediately after its origin in `Functions`; this
/// key plus a STABLE sort then reproduces that adjacency in the merged stream,
/// so the copy is emitted at exactly the origin's program point and is in scope
/// for precisely the call sites the origin was in scope for.
let private emissionOrderedItems (modul: IRModule) : (IRId * Choice<IRBinding, IRFuncDef>) list =
    // TRANSITIVELY, because derivation composes: shape monomorphization
    // specializes the copies HM monomorphization produced, so a
    // `f_HM_..._shape_...` names an HM spec as its origin, and that spec's own
    // id is itself freshly minted and sorts late. Following one link left the
    // shape copy after its call sites again -- the same
    // "'lswosa_HM_..._shape_...' was not declared in this scope" the one-level
    // rule was written to prevent. Bounded by the number of entries and
    // guarded against a cycle (which would be a producer bug, not a shape a
    // program can request).
    let orderKey (id: IRId) =
        let rec follow (cur: IRId) (fuel: int) =
            if fuel <= 0 then cur
            else
                match Map.tryFind cur modul.DerivedFuncOrigins with
                | Some originId when originId <> cur -> follow originId (fuel - 1)
                | _ -> cur
        follow id (Map.count modul.DerivedFuncOrigins + 1)
    let bindingItems = modul.Bindings |> List.map (fun b -> (b.Id, Choice1Of2 b))
    let funcItems = modul.Functions |> List.map (fun f -> (f.Id, Choice2Of2 f))
    bindingItems @ funcItems |> List.sortBy (fst >> orderKey)

let genModule (modul: IRModule) (builder: IRBuilder) : string list * string list =
    // Companion-array gap: populate the codegen-side struct fields
    // cache so inferExprType can resolve IRFieldAccess result types.
    setCodegenStructFieldsCache modul.Types

    // Install the callables table so exprAttrs walks (e.g., from the
    // mask renderer) can do cross-procedural probe analysis. Lambdas
    // and named functions are now handled uniformly: the IR-tree walk
    // descends into lambda bodies directly, and the CallablesTable
    // resolves named function IDs to their (params, body) so the IRApp
    // arm can walk them with parameter substitution.
    let callables = IRPrint.buildCallablesTableForModule modul
    IR.setCallablesContext callables |> ignore

    // Deterministic deallocation: the fresh-return fixpoint resolves callees
    // through the table installed above, so it must run AFTER it. The stack reset
    // is a belt (the four assembly sites reset too): a previous module that raised
    // mid-emission must not leave a frame active for this one's bindings.
    (freshReturnFactsCell ()).Value <- computeFreshReturnFacts modul
    (copyInPlaceMutsCell ()).Value <- computeCopyInPlaceMuts modul
    (groupedCaptureFactsCell ()).Value <- computeGroupedCaptureFacts modul
    // Also after the callables table: kernel bodies are resolved through it.
    (extentsOnlyGroupBysCell ()).Value <- computeExtentsOnlyGroupBys modul
    resetAllocScopeStack ()

    let ctx0 = emptyContext ()
    let ctx0 = { ctx0 with ProviderReads = modul.ProviderReads; ProviderWrites = modul.ProviderWrites; RandomInits = modul.RandomInits; CompoundInits = modul.CompoundInits; SparseInits = modul.SparseInits; MutableArrayLets = modul.MutableArrayLets }
    // Revision reuse (docs/plans/structural/04): empty unless BLADE_TILE_CACHE is set.
    let (tilePlans, tileReads) = Blade.CodeGenTiles.planTiles modul
    let ctx0 = { ctx0 with TilePlans = tilePlans; TileReads = tileReads }

    // First pass: register ALL names (both bindings and functions) in context
    let ctx0 =
        modul.Bindings |> List.fold (fun c b -> addVarName b.Id b.Name c) ctx0
    let ctx0 =
        modul.Functions |> List.fold (fun c f -> addVarName f.Id f.Name c) ctx0

    // Build a combined list of items in emission order (see
    // `emissionOrderedItems`: IRId order, with synthesized copies keyed on
    // their origin). Lifted lambdas live alongside source-level functions in
    // module.Functions and emit as ordinary top-level C++ functions:
    // genFuncDef appends captures as reference parameters, and lambda bodies
    // whose top form is IRApplyCombinator are wrapped in IRCompute at lift
    // time so genFuncBody's return-position handler renders them
    // correctly. Use sites reference the lifted callable through
    // IRVar(callable.Id) and call it through a thin wrapper closure
    // (genCallableWrapper) that hides the capture parameters from
    // consumers expecting the callable's surface arity.
    let allItems = emissionOrderedItems modul

    // Generate in ID order (approximates source order).
    // First, collect file-scope functions to generate forward declarations.
    //
    // Explicit-pass capture: lifted callables receive their captures
    // as additional reference-typed parameters appended after the
    // regular params (see genFuncDef). For the
    // file-scope eligibility check, capture VarIds therefore count as
    // "param-like" -- they're in the function's actual C++ signature,
    // not its enclosing scope. Without including them in `paramIds`
    // here, `collectVarRefsIR funcDef.Body` reports them as free vars
    // and the function gets excluded from forward declarations. That's a
    // problem because `let f = lambda(...)` emits a wrapper closure
    // `auto f = [&](...) { return __lambda_X(..., captures); };`
    // at the binding's emission site, which may precede the lifted
    // function's definition in the file. Without a forward decl,
    // `__lambda_X` is unknown at the wrapper's site and the C++
    // compile fails with "not declared in this scope".
    //
    // Main-locality is TRANSITIVE (computeMainLocalFuncIds): a function
    // whose body references a main-local function is itself main-local,
    // since its free-function form couldn't name the main()-scoped
    // std::function it calls.
    let mainLocalFuncIds = computeMainLocalFuncIds modul ctx0

    let fileScopeFuncs =
        allItems |> List.choose (fun (_, item) ->
            match item with
            | Choice2Of2 funcDef when not (Set.contains funcDef.Id mainLocalFuncIds) -> Some funcDef
            | _ -> None)

    // Generate forward declarations for all file-scope functions (shared
    // helper, also used by genModuleSplit).
    let forwardDecls = genForwardDecls fileScopeFuncs

    // S0 (plan section 6): module-level bindings a file-scope callable must be
    // able to NAME get their declaration promoted to namespace scope.
    let hoistIds = computeModuleCaptureHoistIds modul mainLocalFuncIds

    // Line accumulation is a ResizeArray per bucket (the fold still threads the
    // context, which is what makes emission order-dependent); the old
    // `bc @ code @ [""]` copied the whole bucket once per item, quadratic in
    // module size. Append order is byte-for-byte the same.
    let funcLines = ResizeArray<string>(forwardDecls)
    let bindLines = ResizeArray<string>()
    let finalCtx =
        allItems |> List.fold (fun c (_, item) ->
            match item with
            | Choice1Of2 binding ->
                setCurrentCodegenDecl binding.Name
                let (code, c') = genModuleBinding c binding builder hoistIds
                bindLines.AddRange code
                bindLines.Add ""
                c'
            | Choice2Of2 funcDef ->
                setCurrentCodegenDecl funcDef.Name
                if Set.contains funcDef.Id mainLocalFuncIds then
                    let (code, c') = genFuncDefAsLambda c builder funcDef
                    bindLines.AddRange code
                    bindLines.Add ""
                    c'
                else
                    let (code, c') = genFuncDef c builder funcDef
                    funcLines.AddRange code
                    funcLines.Add ""
                    c'
        ) ctx0
    let funcCode = List.ofSeq funcLines
    let bindCode = List.ofSeq bindLines

    // Merge context warnings into module-level collector
    let cell = exprWarningsCell ()
    cell.Value <- cell.Value @ finalCtx.Warnings.Value

    // Settle every shadow-frame marker before the code leaves this function:
    // both buckets are analyzed together (a file-scope function and a
    // main-local lambda call each other across them) and rewritten apart.
    match resolveShadowFrames [funcCode; bindCode] with
    | [fc; bc] -> (fc, bc)
    | _ -> (funcCode, bindCode)

/// Split-timing variant of genModule: identical context-threaded emission, but
/// binding output is routed into TWO buckets -- `setupCode` (data-setup
/// bindings) and `computeCode` (forced computations) -- so the caller can place
/// a timing checkpoint between them. Functions stay in `funcCode`. Context is
/// still threaded through ALL items in ID order (later bindings reference
/// earlier ones), so only the OUTPUT is partitioned, never the evaluation
/// order. Returns (funcCode, setupCode, computeCode).
let genModuleSplit (modul: IRModule) (builder: IRBuilder) : string list * string list * string list =
    setCodegenStructFieldsCache modul.Types
    let callables = IRPrint.buildCallablesTableForModule modul
    IR.setCallablesContext callables |> ignore
    // Same install as genModule -- split-timing mode goes through this entry point
    // instead, and a missing install here would silently demote every callee to
    // NotFresh (leaks only, but a divergence between the two modes).
    (freshReturnFactsCell ()).Value <- computeFreshReturnFacts modul
    (copyInPlaceMutsCell ()).Value <- computeCopyInPlaceMuts modul
    (groupedCaptureFactsCell ()).Value <- computeGroupedCaptureFacts modul
    // Also after the callables table: kernel bodies are resolved through it.
    (extentsOnlyGroupBysCell ()).Value <- computeExtentsOnlyGroupBys modul
    resetAllocScopeStack ()
    let ctx0 = emptyContext ()
    let ctx0 = { ctx0 with ProviderReads = modul.ProviderReads; ProviderWrites = modul.ProviderWrites; RandomInits = modul.RandomInits; CompoundInits = modul.CompoundInits; SparseInits = modul.SparseInits; MutableArrayLets = modul.MutableArrayLets }
    // Revision reuse (docs/plans/structural/04): empty unless BLADE_TILE_CACHE is set.
    let (tilePlans, tileReads) = Blade.CodeGenTiles.planTiles modul
    let ctx0 = { ctx0 with TilePlans = tilePlans; TileReads = tileReads }
    let ctx0 =
        modul.Bindings |> List.fold (fun c b -> addVarName b.Id b.Name c) ctx0
    let ctx0 =
        modul.Functions |> List.fold (fun c f -> addVarName f.Id f.Name c) ctx0
    // Same emission order as genModule, from the same definition.
    let allItems = emissionOrderedItems modul
    // Transitive main-locality -- same rule as genModule; see
    // computeMainLocalFuncIds.
    let mainLocalFuncIds = computeMainLocalFuncIds modul ctx0
    let fileScopeFuncs =
        allItems |> List.choose (fun (_, item) ->
            match item with
            | Choice2Of2 funcDef when not (Set.contains funcDef.Id mainLocalFuncIds) -> Some funcDef
            | _ -> None)
    let forwardDecls = genForwardDecls fileScopeFuncs
    // S0 (plan section 6): same namespace-scope promotion as genModule.
    let hoistIds = computeModuleCaptureHoistIds modul mainLocalFuncIds
    // Single split point: emit in strict ID order (NO reordering), and once
    // the first compute binding is seen, every subsequent item stays in the
    // compute phase. This preserves all cross-binding dependencies -- a consumer
    // of a compute result (e.g. `decompact(sym,0)` reading `sym`) is emitted
    // AFTER its producer because it comes later in ID order and the phase flag
    // is already in compute. The setup phase is exactly the leading run of
    // data-declaration bindings before the first computation, matching the
    // archaic prototype's "input allocation" vs "calculation" split.
    //
    // (Per-binding classification was wrong: it floated a non-compute consumer
    // like decompact UP into setup, above the compute binding it depends on,
    // producing an out-of-order "'sym' was not declared" C++ error.)
    // Same ResizeArray accumulation as genModule -- see the comment there. The
    // phase flag and the context are still threaded through the fold.
    let funcLines = ResizeArray<string>(forwardDecls)
    let setupLines = ResizeArray<string>()
    let computeLines = ResizeArray<string>()
    let (_seenCompute, finalCtx) =
        let onlyBinding = splitTimingOnlyBinding ()
        allItems |> List.fold (fun (seen, c) (_, item) ->
            match item with
            | Choice1Of2 binding ->
                // When a specific binding name is designated as the timed
                // kernel, the clock starts exactly at that binding (everything
                // prior -- producers, decompact chains -- is setup). Otherwise
                // fall back to "first compute binding starts the clock".
                let nowCompute =
                    match onlyBinding with
                    | Some target -> seen || binding.Name = target
                    | None -> seen || isComputeBinding binding
                setCurrentCodegenDecl binding.Name
                let (code, c') = genModuleBinding c binding builder hoistIds
                if nowCompute then
                    computeLines.AddRange code
                    computeLines.Add ""
                    (true, c')
                else
                    setupLines.AddRange code
                    setupLines.Add ""
                    (false, c')
            | Choice2Of2 funcDef ->
                setCurrentCodegenDecl funcDef.Name
                if Set.contains funcDef.Id mainLocalFuncIds then
                    // Lambda-as-binding (closure definition): follows the
                    // current phase -- setup if before the first compute, else
                    // compute -- so it never floats across a dependency.
                    let (code, c') = genFuncDefAsLambda c builder funcDef
                    if seen then
                        computeLines.AddRange code
                        computeLines.Add ""
                        (true, c')
                    else
                        setupLines.AddRange code
                        setupLines.Add ""
                        (false, c')
                else
                    // True top-level function: always the function-def bucket,
                    // emitted in the preamble (no effect on the phase flag).
                    let (code, c') = genFuncDef c builder funcDef
                    funcLines.AddRange code
                    funcLines.Add ""
                    (seen, c')
        ) (false, ctx0)
    let funcCode = List.ofSeq funcLines
    let setupCode = List.ofSeq setupLines
    let computeCode = List.ofSeq computeLines
    let cell = exprWarningsCell ()
    cell.Value <- cell.Value @ finalCtx.Warnings.Value
    // See genModule: markers are resolved across all three buckets at once.
    match resolveShadowFrames [funcCode; setupCode; computeCode] with
    | [fc; sc; cc] -> (fc, sc, cc)
    | _ -> (funcCode, setupCode, computeCode)

/// Generate a complete C++ program with main() from an IR module
let genStructDef (name: string) (fields: (string * IRType) list) : string list =
    let fieldLines = fields |> List.map (fun (fname, fty) ->
        // Array-typed fields render as Array<T,N> / Ragged<T> wrappers so
        // the field carries its shape with it. Other types use the
        // standard irTypeToCpp rendering.
        let cppTy =
            match fty with
            | ArrayElem arr -> cppArrayTypeStr arr
            | _ -> irTypeToCpp fty
        $"    {cppTy} {fname};")
    [$$"""struct {{name}} {"""]
    @ fieldLines
    @ ["};"
       ""]

/// Generate type definitions for a module
let genTypeDefs (modul: IRModule) : string list =
    modul.Types |> List.collect (function
        | IRTDStruct (name, fields) -> genStructDef name fields
        | IRTDVariant (name, variants) ->
            let hasData = variants |> List.exists (fun (_, d) -> d.IsSome)
            if hasData then
                // Tagged union using std::variant. Wrapper structs use a _T
                // suffix to avoid a name clash with the constructor functions
                // below (both would otherwise want the variant's bare name).
                let variantStructs = variants |> List.collect (fun (vname, data) ->
                    match data with
                    | Some ty -> 
                        [$$"""struct {{vname}}_T { {{(irTypeToCpp ty)}} value; };"""]
                    | None -> 
                        [$$"""struct {{vname}}_T {};"""]
                )
                let variantTypes = variants |> List.map (fun (v, _) -> v + "_T") |> String.concat ", "
                let variantAlias = $"using {name} = std::variant<{variantTypes}>;"
                let ctorFuncs = variants |> List.collect (fun (vname, data) ->
                    match data with
                    | Some ty ->
                        [$$"""inline {{name}} {{vname}}({{(irTypeToCpp ty)}} v) { return {{vname}}_T{v}; }"""]
                    | None ->
                        [$$"""const {{name}} {{vname}} = {{vname}}_T{};"""]
                )
                variantStructs @ [variantAlias] @ ctorFuncs @ [""]
            else
                // Simple enum - use plain enum for unscoped names
                [$"""enum {name} {{ {(variants |> List.map fst |> String.concat ", ")} }};"""
                 ""]
        | IRTDAlias (name, ty) ->
            [$"using {name} = {(irTypeToCpp ty)};"; ""]
        | IRTDIndexType (name, _) ->
            // Emit a typedef for the index type so foreign-key element types
            // (IRTIdxTagged (_, IRefNamed name)) can render as the alias
            // rather than bare int64_t. The alias is transparent --
            // int64_t-compatible -- but makes generated C++ self-documenting
            // and leaves a hook for future strong typing.
            //
            // The alias lands in the GLOBAL namespace, so the name goes
            // through indexTypeCppName: a provider derives an index type per
            // store dimension, and `time` (the geoscience default) would
            // otherwise redeclare <ctime>'s `time`. irTypeToCpp's IRefNamed
            // arm applies the same function, so references still resolve.
            [$"using {(indexTypeCppName name)} = int64_t;"; ""]
        | IRTDEnumIdx (name, _, values) ->
            // EnumIdx alias: render as the underlying runtime type. All-int
            // values -> int64_t; all-string values -> std::string. The chosen
            // C++ type must match what the Case 2 reverse-lookup dispatch
            // and any keys array stored under this type expect.
            let underlying = EnumValue.underlyingElemType values
            [$"using {(indexTypeCppName name)} = {(primTypeToCpp underlying)};"; ""]
    )

// Every printer below takes the binding's SOURCE name and splits it in two:
// the `name = ...` LABEL keeps the source spelling (that is what the corpus
// EXPECT pins and the interpreter's twin printers read), while the C++
// expressions and temporaries go through `sanitizeCppName`, which is the
// spelling `bindingCppName` declared the binding under. The two differ only
// for a binding whose name is a C++ reserved word (`let final = ...`).

/// Generate code to print a scalar value
let genPrintScalar (name: string) : string list =
    [$"    cout << \"{name} = \" << {(sanitizeCppName name)} << endl;"]

/// Rank-2 print in the NESTED form -- `name = [[a, b], [c, d]]` -- which is the
/// shape a rank-2 literal is written in, so the printed line round-trips as
/// source. `outerBound` / `innerBound` are C++ expressions; the inner one may
/// reference the outer loop var `i` (a compact group's row shrinks with it),
/// which is why this takes bound TEXT rather than deriving `extents[d]` itself.
///
/// Rank 1 is already one level of brackets and ranks >= 3 stay flat: the pins
/// that read these lines check VALUES, and a three-deep nest buys a reader
/// nothing the extents don't already say. The interpreter's twin
/// (Interp/ArrayOps.emitNested2) must stay byte-identical to this.
let private genPrintNested2 (name: string) (outerBound: string) (innerBound: string) : string list =
    let v = sanitizeCppName name
    let firstVar = $"{v}__first"
    [ $"    cout << \"{name} = [\";"
      $$"""    for (size_t i = 0; i < {{outerBound}}; i++) {"""
      "        if (i) cout << \", \";"
      "        cout << \"[\";"
      $"        bool {firstVar} = true;"
      $$"""        for (size_t j = 0; j < {{innerBound}}; j++) {"""
      $"            if (!{firstVar}) cout << \", \";"
      $"            {firstVar} = false;"
      $"            cout << {v}[i][j];"
      "        }"
      "        cout << \"]\";"
      "    }"
      "    cout << \"]\" << endl;" ]

/// Generate code to print a dense array value. ONE regime for every rank:
/// rank 2 nests (genPrintNested2, the shape its literal is written in), every
/// other rank >= 1 prints FLAT -- a single row-major comma-separated run.
///
/// Flat past rank 2 is a decision, not a shortcut. The EXPECT harness treats a
/// flat run and a depth-2 nest as the same value (Expect.fs's flatten duality)
/// but parses nothing deeper, so a rank-r bracket nest would break every flat
/// pin while adding shape text the extents already carry; and no bracket
/// nesting makes a rank-3+ dump readable anyway. This regime replaced two
/// historical formats that made higher ranks UNPINNABLE: a rank-4 multi-line
/// `name[i][j] = [...]` grid (whose lines never parse as a `name = value`
/// pin) and a rank-5+ `<rank-N array>` placeholder that printed no values at
/// all -- while rank 3 printed flat, the inconsistency this retired.
let genPrintArrayFlat (name: string) (rank: int) : string list =
    let v = sanitizeCppName name
    let firstVar = $"{v}__first"
    if rank < 1 then
        [$"    cout << \"{name} = <rank-0>\" << endl;"]
    elif rank = 2 then
        genPrintNested2 name ($"{v}.extents[0]") ($"{v}.extents[1]")
    else
        // Loop-var names as genPrintArraySymAware spells them, with the same
        // numbered overflow past eight (nothing collides: the print block is
        // its own statement scope).
        let loopVarNames = [| "i"; "j"; "k"; "l"; "m"; "n_"; "p"; "q" |]
        let loopVar d = if d < loopVarNames.Length then loopVarNames.[d] else $"d{d}"
        let opens = [
            $"    cout << \"{name} = [\";"
            $"    bool {firstVar} = true;" ]
        let loops =
            [ for d in 0 .. rank - 1 ->
                $"""    {(String.replicate d "    ")}for (size_t {(loopVar d)} = 0; {(loopVar d)} < {v}.extents[{d}]; {(loopVar d)}++) {{""" ]
        let inner =
            let ind = "    " + String.replicate rank "    "
            let idx = [ for d in 0 .. rank - 1 -> $"[{(loopVar d)}]" ] |> String.concat ""
            [ $"{ind}if (!{firstVar}) cout << \", \";"
              $"{ind}{firstVar} = false;"
              $"{ind}cout << {v}{idx};" ]
        let closes =
            [ for d in rank - 1 .. -1 .. 0 ->
                $"""    {(String.replicate d "    ")}}}""" ]
        let finish = [ "    cout << \"]\" << endl;" ]
        opens @ loops @ inner @ closes @ finish

/// Generate print loop for arrays with per-dimension symmetry awareness.
/// Expands IRIndexType list into per-dimension loop structure:
///   - SymIdx<k,n>: k dims, first is free range, rest subtract prior vars in group
///   - Idx<n>: 1 dim, free range
/// This correctly handles mixed symmetry (e.g. SymIdx<2> + Idx).
let genPrintArraySymAware (name: string) (indexTypes: IRIndexType list) : string list =
    let v = sanitizeCppName name
    // Expand index types into per-dimension info: (loopVar, dimIdx, offsetVars)
    // offsetVars = list of loop vars to subtract from extent (empty for free dims)
    let loopVarNames = [| "i"; "j"; "k"; "l"; "m"; "n_"; "p"; "q" |]
    // Same refusal as the interpreter's emitSymAware, for the same reason: the
    // two printers must byte-match, and neither has a wreath walk. A triangular
    // walk here would emit C++ that prints a cell set nothing else agrees with.
    indexTypes |> List.iter (fun idx ->
        if idx.Symmetry = SymWreath then
            failwith (orbitStorageUnsupported "compact print (genPrintArraySymAware)" (orbitLevelsOf idx)))
    let dims =
        indexTypes |> List.fold (fun (acc, dimIdx) idx ->
            let idxRank = max 1 idx.Rank
            let isSym = idx.Symmetry = SymSymmetric || idx.Symmetry = SymAntisymmetric || idx.Symmetry = SymHermitian
            // Antisymmetric storage is STRICT (i < j < ...): each successive
            // level in the group loses one more slot than the symmetric
            // (i <= j) case. The writer applies this as StrictOffset=1 per
            // triangular antisym level (genLoopHeader / IR strictOffset). The
            // reader must mirror it exactly, or it walks one element past the
            // end of each strict-packed row into adjacent/garbage memory --
            // precisely the antisym-Reynolds value mismatch. Symmetric stays
            // strictConst = 0 (left-justified bound n - i).
            let strictConst = if idx.Symmetry = SymAntisymmetric then 1 else 0
            let groupDims =
                [0 .. idxRank - 1] |> List.map (fun a ->
                    let loopVar = if dimIdx + a < loopVarNames.Length then loopVarNames.[dimIdx + a] else $"d{dimIdx + a}"
                    let offsets =
                        if isSym && a > 0 then
                            [0 .. a - 1] |> List.map (fun prev -> loopVarNames.[dimIdx + prev])
                        else []
                    // Strict offset applies on every group level beyond the
                    // first (a > 0): level a subtracts a * strictConst.
                    let strict = if a > 0 then a * strictConst else 0
                    (loopVar, dimIdx + a, offsets, strict))
            (acc @ groupDims, dimIdx + idxRank)
        ) ([], 0) |> fst
    let rank = dims.Length
    // The bound at one dimension: extent minus the prior group vars minus the
    // strict constant (see the fold above). Shared by the nested rank-2 form
    // and the flat loop nest below so the two cannot drift.
    let boundAt (loopVar: string, dimIdx: int, offsets: string list, strict: int) =
        ignore loopVar
        match offsets @ (if strict > 0 then [string strict] else []) with
        | [] -> $"{v}.extents[{dimIdx}]"
        | subParts -> $"""{v}.extents[{dimIdx}] - {(String.concat " - " subParts)}"""
    if rank < 1 || rank > 8 then
        [$"    cout << \"{name} = <rank-{rank} array>\" << endl;"]
    elif rank = 2 then
        // A rank-2 compact group nests exactly as its literal does: the inner
        // bound carries the row shrink (`extents[1] - i`, or `- i - 1` when the
        // group is strict), and genPrintNested2's loop vars are the `i`/`j` the
        // offsets above name.
        genPrintNested2 name (boundAt (List.item 0 dims)) (boundAt (List.item 1 dims))
    else
        let firstVar = $"{v}__first"
        let opens = [
            $"    cout << \"{name} = [\";"
            $"    bool {firstVar} = true;" ]
        let loops =
            dims |> List.map (fun ((loopVar, _, _, _) as dim) ->
                let indent = "    " + String.replicate (dims |> List.findIndex (fun (v,_,_,_) -> v = loopVar)) "    "
                // Bound = extents[d] - (prior loop vars) - (strict constant):
                // a free dim is the bare extent, a symmetric group level is
                // extent - priorVars, an antisymmetric one also - a (strict).
                forLoop indent loopVar (boundAt dim))
        let innerIndent = "    " + String.replicate rank "    "
        let idx = dims |> List.map (fun (v,_,_,_) -> $"[{v}]") |> String.concat ""
        let inner = [
            $"{innerIndent}if (!{firstVar}) cout << \", \";"
            $"{innerIndent}{firstVar} = false;"
            $"{innerIndent}cout << {v}{idx};" ]
        let closes =
            [for d in rank - 1 .. -1 .. 0 ->
                $"""    {(String.replicate d "    ")}}}"""]
        let finish = ["    cout << \"]\" << endl;"]
        opens @ loops @ inner @ closes @ finish

/// Print an OrbIdx (iterated-wreath) pool: its cells in STORAGE order (=
/// `orb_visit` order = `OrbRank.visitStream` order = ascending-lex canonical),
/// the direct analogue of how a `SymIdx` array prints its triangle, with the
/// same `name = [c0, c1, ...]` framing every other array printer uses.
///
/// The bound is re-derived from the type (`orb_cell_count` over the record's
/// own level list and extent) rather than read from the emit site's
/// `<name>__orbcells`: the printer runs in a different function and must work
/// for any wreath binding, so re-deriving from the SAME header entry point
/// keeps the walked count equal to the allocated count.
///
/// Nothing here folds or transforms: a stored cell is canonical by
/// construction (character +1), and the mirrored read that would need one
/// stays refused.
let genPrintArrayWreath (name: string) (levels: (int * bool) list) (extent: int64) : string list =
    let levelArgs = orbLevelArgs levels
    let v = sanitizeCppName name
    let firstVar = $"{v}__first"
    [ $"    cout << \"{name} = [\";"
      $"    bool {firstVar} = true;"
      $"    for (int64_t __ok = 0; __ok < orbit_wreath_utilities::orb_cell_count<{levelArgs}>({extent}); __ok++) {{"
      $"        if (!{firstVar}) cout << \", \";"
      $"        {firstVar} = false;"
      $"        cout << {v}[__ok];"
      "    }"
      "    cout << \"]\" << endl;" ]

/// Compute which binding IDs are deferred (no C++ code generated).
/// A binding is deferred if it's a computation that hasn't been materialized via |> compute.
let computeDeferredIds (bindings: IRBinding list) : Set<int> =
    let isDeferred (ids: Set<int>) (e: IRExpr) =
        match e with
        | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _ | IRBind _ | IRZip _ | IRSequence _ -> true
        | IRVar (id, _) -> Set.contains id ids
        | _ -> false
    bindings |> List.fold (fun ids b ->
        // A combinator whose RESULT TYPE is an array is a deferred computation
        // regardless of whether its operands are already deferred -- it materializes
        // only at |> compute, exactly like `<@>` (which always defers). Without this,
        // `<$>` / sequence / `<|>` / guard / `>>=` / `>>@` applied to CONCRETE arrays
        // fell through to `| _ -> false` and were eagerly materialized (and printed)
        // with no |> compute. Scalar `<|>` / guard stay eager ternaries (result type
        // is IRTScalar, so resultIsArray is false).
        let resultIsArray = match IR.stripUnits b.Type with ArrayElem _ -> true | _ -> false
        let shouldDefer =
            match b.Value with
            | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ -> true
            | IRZip _ -> true
            | IRComposeObj _ -> true
            | IRBind (comp, _) -> resultIsArray || isDeferred ids comp
            | IRComposeMeth (left, right) -> resultIsArray || isDeferred ids left || isDeferred ids right
            | IRFunctorMap (_, inner) -> resultIsArray || isDeferred ids inner
            | IRChoice (left, right) -> resultIsArray || isDeferred ids left || isDeferred ids right
            // <|:> operands are arrays, so the binding ALWAYS defers
            // (genFallbackBinding) -- materialization happens at |> compute.
            | IRFallback _ -> true
            | IRGuard (_, body) ->
                let rec leafIsDeferred e =
                    match e with
                    | IRGuard (_, inner) -> leafIsDeferred inner
                    | _ -> isDeferred ids e
                resultIsArray || leafIsDeferred body
            | IRSequence elems -> resultIsArray || (elems |> List.exists (isDeferred ids))
            | IRTuple elems -> elems |> List.forall (isDeferred ids)
            | IRTupleProj (IRVar (pid, _), _, _) -> Set.contains pid ids
            | IRVar (srcId, _) -> Set.contains srcId ids
            | _ -> false
        if shouldDefer then Set.add b.Id ids else ids
    ) Set.empty

let genPrintStatements (modul: IRModule) : string list =
    let deferredIds = computeDeferredIds modul.Bindings
    // `--print <names>` (BLADE_PRINT, CodeGenState.printSelection): print only
    // the named bindings. A name that is not a top-level binding AT ALL is a
    // typo, and a typo that silently printed nothing would look exactly like a
    // program that computed nothing -- so it refuses, by name, listing what is
    // there. (A name that IS a binding but never prints -- a deferred loop
    // value, a streamed read -- prints nothing, as it does unselected.)
    let selection = printSelection ()
    // The refusal is SPLICED (refusalErrorLine), not merely recorded: the
    // channel delivers BL7004 only for a translation unit that carries a
    // marker, and a recorded-but-unspliced message would leave the typo
    // printing nothing and exiting 0 -- the very failure this guards.
    let selectionRefusal =
        match selection with
        | Some names ->
            let sep = ", "
            let declared = modul.Bindings |> List.map (fun b -> b.Name) |> Set.ofList
            let unknown = Set.difference names declared |> Set.toList
            if unknown.IsEmpty then []
            else
                let known = declared |> Set.toList |> List.filter (fun n -> not (n.StartsWith "__")) |> String.concat sep
                let missing = String.concat sep unknown
                let verb = if unknown.Length = 1 then "is not a top-level binding of this program" else "are not top-level bindings of this program"
                [ refusalErrorLine "    " $"--print: {missing} {verb} -- it has: {known}" ]
        | None -> []
    // A deferred binding that a consumer FORCED (forceDeferredArrayInput
    // materialized it under its own name at main's top level) is a real
    // array by program end and prints like any eager binding; one that
    // stayed deferred through the whole program prints nothing. The cell is
    // populated during genModule, so callers must assemble print code AFTER
    // body generation.
    let forcedIds = (forcedDeferredIdsCell ()).Value
    selectionRefusal @
    (modul.Bindings |> List.collect (fun b ->
        // |> compute of a DEFERRED combinator is a forced materialization and
        // always prints; |> compute of anything ELSE prints exactly when the
        // wrapped value itself would (an eager reduce/scalar is unchanged by
        // compute -- `let s = reduce(xs, (+)) |> compute` must echo like the
        // computeless form). Unmaterialized loop values never print.
        let rec printableValue (v: IRExpr) =
            match v with
            | IRCompute (IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRVar _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeMeth _ | IRBind _ | IRGuard _ | IRSequence _) -> true
            | IRCompute inner -> printableValue inner
            | IRMethodFor _ | IRObjectFor _ -> false
            | _ -> true
        let isPrintable =
            if Set.contains b.Id deferredIds && not (Set.contains b.Id forcedIds) then false
            // A STREAMED provider read has no materialized array (fiber
            // reads happen inside consuming nests) -- nothing to print.
            elif (match Map.tryFind b.Id modul.ProviderReads with
                  | Some spec -> spec.Streamed
                  | None -> false) then false
            else printableValue b.Value
        
        let hasSymmetry =
            match IR.stripUnits b.Type with
            | ArrayElem arr ->
                arr.IndexTypes |> List.exists (fun idx ->
                    idx.Symmetry = SymSymmetric || idx.Symmetry = SymAntisymmetric || idx.Symmetry = SymHermitian)
            | _ -> false

        // The `--print` selection, applied exactly where the interpreter's
        // twin applies it (Interp/Print.printBindingsOnly's `wanted`).
        let wanted =
            match selection with
            | Some names -> Set.contains b.Name names
            | None -> true

        if isPrintable && wanted then
            match IR.stripUnits b.Type with
            | IRTScalar (ETFloat64 | ETFloat32 | ETInt64 | ETInt32 | ETBool | ETComplex64 | ETComplex128 | ETString) ->
                genPrintScalar b.Name
            | IRTIdxTagged _ ->
                // Tagged int prints same as int
                genPrintScalar b.Name
            | IRTNat _ ->
                // Type-level natural in value position (e.g. a Nat tuple
                // component from destructuring) renders as size_t -- streams
                // like any integer scalar.
                genPrintScalar b.Name
            // An OrbIdx (iterated-wreath) binding is a FLAT pool, not an
            // Array<T,N>: print its cells in storage order. Ahead of every
            // other array arm because those all reach for `.extents` or a
            // triangular walk, neither of which a wreath pool has.
            | ArrayElem arrType when arrType.IndexTypes |> List.exists (fun ix -> ix.Symmetry = SymWreath) ->
                (match arrType.IndexTypes with
                 | [ ix ] ->
                     (match orbitBaseExtent ix with
                      | IRLit (IRLitInt n) -> genPrintArrayWreath b.Name (orbitLevelsOf ix) n
                      | _ ->
                          [$"    // (OrbIdx array '{b.Name}' not auto-printed: a wreath pool needs a compile-time extent to size its cell count)"])
                 | _ ->
                     [$"    // (OrbIdx array '{b.Name}' not auto-printed: a wreath group combined with other index groups has no pool layout)"])
            | ArrayElem arrType when isCompoundArrayType arrType ->
                // Compound (load_compound) values wrap a compact buffer plus a
                // compound_index_t pointer; there is no operator<< for
                // Compound<T,RANK>. Skip auto-print with a diagnostic so the
                // generated program still compiles -- scalar value-checks via
                // element access (e.g. data(lead, t)) remain available.
                [$"    // (compound array '{b.Name}' not auto-printed; Compound<T,RANK> has no operator<<)"]
            | ArrayElem arrType when isSparseArrayType arrType ->
                // Same rationale as the compound arm: Sparse<T,RANK> has no
                // operator<<; value-checks read cells via full-key access.
                [$"    // (sparse array '{b.Name}' not auto-printed; Sparse<T,RANK> has no operator<<)"]
            | ArrayElem arrType ->
                // Arrays of named (struct) types -- cout's operator<<
                // isn't defined for user types. For rank-1 arrays of structs,
                // emit a per-field print loop driven by the IRTDStruct's
                // declared field list (looked up from modul.Types). Format:
                //   name = [{f1: V, f2: V}, {f1: V, f2: V}, ...]
                // Higher ranks fall back to a skip comment for now;
                // value-checks via scalar field reads still work in either
                // case.
                match arrType.ElemType with
                | FuncElem _ ->
                    // Arrays of functions have no general `operator<<`.
                    // std::function isn't streamable, and a generic print
                    // would need either signature-specific formatting or
                    // address-printing -- neither is meaningful for testing.
                    // Skip with a diagnostic comment so the surrounding
                    // value-check on scalar results derived from calls
                    // (e.g. `let r = funcs(1)(5.0)`) still runs.
                    [$"    // (array '{b.Name}' of function values not auto-printed; std::function isn't streamable)"]
                | IRTNamed structName ->
                    let rank = arrayRank arrType
                    let structFields =
                        modul.Types |> List.tryPick (fun td ->
                            match td with
                            | IRTDStruct (n, fs) when n = structName -> Some fs
                            | _ -> None)
                    match structFields, rank with
                    | Some fields, 1 when not (List.isEmpty fields) ->
                        let bv = sanitizeCppName b.Name
                        let firstVar = $"{bv}__first"
                        let fieldPrints =
                            fields |> List.mapi (fun i (fname, _) ->
                                let prefix = if i = 0 then "" else ", "
                                $"        cout << \"{prefix}{fname}: \" << {bv}[i].{fname};")
                        [
                            $"    cout << \"{b.Name} = [\";"
                            $"    bool {firstVar} = true;"
                            $$"""    for (size_t i = 0; i < {{bv}}.extents[0]; i++) {"""
                            $"        if (!{firstVar}) cout << \", \";"
                            $"        {firstVar} = false;"
                            "        cout << \"{\";"
                        ]
                        @ fieldPrints
                        @ [
                            "        cout << \"}\";"
                            "    }"
                            "    cout << \"]\" << endl;"
                        ]
                    | _ ->
                        // Struct not found in module Types, or rank > 1, or
                        // no fields -- emit diagnostic comment and skip.
                        [$"    // (array '{b.Name}' of struct '{structName}' not auto-printed; access individual fields via {b.Name}[i].field)"]
                | IRTTuple _ ->
                    // std::tuple has no operator<<; value-checks read
                    // components via destructuring instead.
                    [$"    // (array '{b.Name}' of tuple values not auto-printed; std::tuple has no operator<<)"]
                | _ ->
                let rank = arrayRank arrType
                // Distinguish three cases for ragged-tagged bindings, based on
                // what C++ metadata exists for each:
                //   (a) Ragged literal (Value is IRArrayLit): genArrayLiteral
                //       emitted both _lens and _extents -- ragged print loop.
                //   (b) Ragged-peel output (Value is IRApplyCombinator): only
                //       _extents was emitted, runtime shape is rank-1
                //       rectangular (one scalar per outer iteration) -- flat print.
                //   (c) Sub-view binding (Value is IRIndex, e.g. r(1)): neither
                //       was emitted; printing would reference undefined names -- skip.
                let isRaggedLiteralBinding =
                    (isRaggedArrayType arrType || isDepIdxArrayType arrType) &&
                    (match b.Value with
                     | IRArrayLit _ -> true
                     | _ -> false)
                // A FULL compound read that leaves trailing dims (B((i,j)) or
                // B((i,j), _) on Array<T like CompoundIdx<m>, Idx<...>>) binds
                // the raw trailing-row T* (.row()), which carries no .extents
                // member -- the flat print loop would not compile. Skip with a
                // diagnostic; scalar derivations (r(t)) still print. (PARTIAL
                // reads are unaffected: they produce real wrappers.)
                let isCompoundRowSubview =
                    match b.Value with
                    | IRIndex (a, (IRTuple coords) :: _, _) ->
                        (match inferExprType a with
                         | ArrayElem at when isCompoundArrayType at || isSparseArrayType at ->
                             let k =
                                 at.IndexTypes
                                 |> List.tryFind (fun ix -> ix.IxKind = IxKCompound || ix.IxKind = IxKSparse)
                                 |> Option.map (_.Rank)
                                 |> Option.defaultValue coords.Length
                             (match classifyCompoundIndexTuple k coords with
                              | CompoundFull -> true  // dense-typed result of a full read = trailing row
                              | CompoundPartial _ -> false)
                         | _ -> false)
                    | _ -> false
                // Look through IRCompute wrappers (from |> compute) to find
                // the underlying combinator. Also check |> bind continuations
                // and other materialization wrappers as needed.
                let rec unwrapMaterialization (e: IRExpr) : IRExpr =
                    match e with
                    | IRCompute inner -> unwrapMaterialization inner
                    | _ -> e
                let isRaggedPeelOutput =
                    isRaggedArrayType arrType &&
                    (match unwrapMaterialization b.Value with
                     | IRApplyCombinator _ -> true
                     | _ -> false)
                // A rank-1 ragged-family SUB-VIEW binding (`let row = r(i)`,
                // `let g0 = grouped(i)`) is a RaggedRow<T> with an inline
                // `.len`, so it is printable directly.
                let isRaggedRowBinding =
                    isRaggedRowType arrType &&
                    (b.Value.IsIRIndex)
                if isCompoundRowSubview then
                    [$"    // (trailing-row view '{b.Name}' not auto-printed; the raw T* row carries no extents -- derive scalars via {b.Name}(t))"]
                elif isRaggedRowBinding then
                    let bv = sanitizeCppName b.Name
                    let firstVar = $"{bv}__first"
                    [
                        $"    cout << \"{b.Name} = [\";"
                        $"    bool {firstVar} = true;"
                        $$"""    for (size_t __rk = 0; __rk < {{bv}}.len; __rk++) {"""
                        $"        if (!{firstVar}) cout << \", \";"
                        $"        {firstVar} = false;"
                        $"        cout << {bv}[__rk];"
                        "    }"
                        "    cout << \"]\" << endl;"
                    ]
                elif isRaggedLiteralBinding || (isRaggedPeelOutput && rank >= 2) then
                    // Ragged wrapper with lens/offsets companions: a ragged
                    // LITERAL, or an ELEMENTWISE map output (shape-preserving
                    // Ragged<T> sharing the parent's metadata). Iterate rows
                    // via .lens; print as the flat value sequence the
                    // validation framework expects.
                    let bv = sanitizeCppName b.Name
                    let firstVar = $"{bv}__first"
                    // Nested, like every other rank-2 print (genPrintNested2):
                    // a ragged array's rows are the one thing its flat pool
                    // cannot show, and `lens[i]` is exactly the row boundary.
                    [
                        $"    cout << \"{b.Name} = [\";"
                        $$"""    for (size_t __ri = 0; __ri < {{bv}}.extents[0]; __ri++) {"""
                        "        if (__ri) cout << \", \";"
                        "        cout << \"[\";"
                        $"        bool {firstVar} = true;"
                        $$"""        for (size_t __rj = 0; __rj < {{bv}}.lens[__ri]; __rj++) {"""
                        $"            if (!{firstVar}) cout << \", \";"
                        $"            {firstVar} = false;"
                        $"            cout << {bv}[__ri][__rj];"
                        "        }"
                        "        cout << \"]\";"
                        "    }"
                        "    cout << \"]\" << endl;"
                    ]
                elif isRaggedPeelOutput then
                    // Peel output is rank-1 rectangular at runtime; flat print
                    // works regardless of the type-level rank/tag.
                    genPrintArrayFlat b.Name 1
                elif isRaggedArrayType arrType then
                    // Sub-view binding: no metadata to drive a print loop.
                    // Skip rather than emit broken code. Scalar derivations
                    // from the sub-view still print normally.
                    [$"    // (sub-view of ragged array '{b.Name}' not printed; metadata not propagated)"]
                // Every symmetric rank routes to the sym-aware printer: its
                // internal guard emits the `<rank-N array>` placeholder past
                // rank 8, whereas genPrintArrayFlat now dense-walks EVERY rank
                // and would misread compact storage.
                elif hasSymmetry && rank >= 2 then
                    genPrintArraySymAware b.Name arrType.IndexTypes
                else
                    genPrintArrayFlat b.Name rank
            | IRTTuple _ -> []
            | IRTNamed _ -> []
            | IRTUnit -> []
            | _ -> []
        else [])
    )

/// Assemble the main() function wrapper around binding code and print statements.
/// `mpi` = true (MPI emit mode + module uses mpi): main takes argc/argv,
/// brackets the body with MPI_Init/Finalize, and guards the timing + result
/// prints behind rank 0 -- every rank computes (SPMD), exactly one rank
/// reports, so output is deterministic and byte-comparable to a serial run.
/// `mpi` = false emits the historical wrapper byte-identically.
/// Does this module read or write through the netcdf provider?
///
/// Decided from the IR rather than by sniffing the emitted text, so it is
/// settled before a line is written. `providerIncludes` answers a related
/// question further down the file, but the main wrapper is emitted above it.
let moduleUsesNetcdf (modul: IRModule) : bool =
    let providerOf (specs: Map<_, _>) f = specs |> Map.toList |> List.map (snd >> f)
    (providerOf modul.ProviderReads (_.Provider)
     @ providerOf modul.ProviderWrites (_.Provider))
    |> List.exists (fun p -> p = "netcdf")

/// The teardown a netcdf program needs before it may return.
///
/// WHY A PROGRAM THAT HAS FINISHED CANNOT SIMPLY RETURN. Some libnetcdf builds
/// link a large closure -- MSYS2's pulls in libcurl and the AWS C++ SDK, whose
/// CRT runs an event-loop thread pool. `ExitProcess` (which is where returning
/// from main, `exit()` and even `_exit()` all end up) terminates every other
/// thread and THEN runs DLL_PROCESS_DETACH across that closure; a worker killed
/// while holding a lock a detach handler then wants is a deadlock, and the
/// process hangs forever having already printed every correct answer. Measured
/// on a GitHub Windows runner: returning hung, `_exit(0)` hung, `nc_finalize()`
/// then returning exited cleanly.
///
/// So this is not defensive tidying -- it is the difference between a program
/// that terminates and one that does not, on builds that happen to be linked
/// that way. Builds with a small closure (an MSVC netCDF pulls only hdf5 and
/// the CRT) never had the problem and are unaffected: the call is cheap and
/// ordinary.
///
/// Version-gated because nc_finalize arrived in netcdf-c 4.9. Absent the
/// macros -- an older netcdf, or one shipping no netcdf_meta.h -- nothing is
/// emitted and that build compiles exactly as it did before.
let private netcdfFinalizeLines : string list =
    [ ""
      "    __blade_nc_finalize();" ]

/// Registered FIRST, before any netcdf call can fail.
///
/// The end-of-main call alone was not enough: a failed nc_open and
/// blade_rt::panic both leave through std::exit(1), which never reaches it --
/// and on a build whose netcdf closure deadlocks at teardown, that path hangs
/// exactly as the success path did. std::exit runs atexit handlers before
/// ExitProcess, so registering covers every way out that is not an outright
/// crash. The helper is idempotent, so this and the explicit call coexist.
let private netcdfRegisterLines : string list =
    [ "    std::atexit(__blade_nc_finalize);" ]

/// The run record's file-scope lines (Blade.RunRecord.cppLines): the input
/// manifest of this module -- its provider reads plus the folds this
/// compilation logged -- and the static writer. `emitted` is the program
/// text assembled so far, sniffed for the `rand` runtime so the record can
/// name the generator. Every program carries the record (a getenv at exit
/// when BLADE_RUN_RECORD is unset), so byte-identity across builds of the
/// same program is unaffected and no environment reaches the emitted text.
let private runRecordLines (modul: IRModule) (testName: string) (mpiOn: bool) (emitted: string list) : string list =
    let folds = Blade.ProviderStatics.drainFoldLog ()
    let entries = Blade.RunRecord.manifestOf [ modul ] folds
    let usesRng = emitted |> List.exists (fun (l: string) -> l.Contains "blade_rand::")
    let rankExpr = if mpiOn then "&__blade_mpi_rank" else "nullptr"
    Blade.RunRecord.cppLines testName Blade.RunRecord.bladeVersion usesRng rankExpr entries @ [ "" ]

let genMainWrapper (mpi: bool, mpiThreaded: bool, netcdf: bool) (testName: string) (bodyIndented: string list) (printCode: string list) : string list =
    let header =
        if mpi then
            [ "int main(int argc, char** argv) {"
              "    cout << std::setprecision(15);"
              "    cout << std::boolalpha;"
              (if mpiThreaded then
                  "    { int __blade_mpi_prov; MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &__blade_mpi_prov); if (__blade_mpi_prov < MPI_THREAD_FUNNELED) { std::cerr << \"error[BL8004]: MPI thread support below MPI_THREAD_FUNNELED\" << std::endl; MPI_Abort(MPI_COMM_WORLD, 14); } }"
               else
                  "    MPI_Init(&argc, &argv);")
              "    MPI_Comm_rank(MPI_COMM_WORLD, &__blade_mpi_rank);"
              "    MPI_Comm_size(MPI_COMM_WORLD, &__blade_mpi_size);"
              "    auto start = TIME;"
              "" ]
        else
            [ "int main() {"
              "    cout << std::setprecision(15);"
              "    cout << std::boolalpha;"
              "    auto start = TIME;"
              "" ]
    let timing =
        if mpi then
            [ ""
              "    auto end = TIME;"
              "    double elapsed = 1e-9 * TIME_DIFF;"
              $"    if (__blade_mpi_rank == 0) {{ cout << \"{testName} completed in \" << elapsed << \"s\" << endl; }}"
              ""
              "    // Print results for verification (rank 0 only)"
              "    if (__blade_mpi_rank == 0) {" ]
        else
            [ ""
              "    auto end = TIME;"
              "    double elapsed = 1e-9 * TIME_DIFF;"
              $"    cout << \"{testName} completed in \" << elapsed << \"s\" << endl;"
              ""
              "    // Print results for verification" ]
    let finalizeLines = if netcdf then netcdfFinalizeLines else []
    let footer =
        if mpi then
            // After the rank guard closes, so every rank shuts its own netcdf
            // down, and before MPI_Finalize, because a parallel-enabled netcdf
            // is layered on MPI rather than beside it.
            [ "    }" ]
            @ finalizeLines
            @ [ ""
                "    MPI_Finalize();"
                "    return 0;"
                "}" ]
        else
            finalizeLines
            @ [ ""
                "    return 0;"
                "}" ]
    // Wrap the whole body in try/catch so C++ exceptions (bad_alloc, etc.)
    // route to blade_rt::panic (BL8005) instead of std::terminate. MPI
    // init/finalize straddle the try; a panic exits without MPI_Finalize,
    // acceptable on a failure path. Success-path output is byte-identical.
    let tryLine = [ "    try {" ]
    let catchClose =
        [ "    } catch (const std::exception& e) { blade_rt::panic(\"BL8005\", e.what(), nullptr, 0); }"
          "      catch (...) { blade_rt::panic(\"BL8005\", \"unknown exception\", nullptr, 0); }"
          "}" ]
    let footerBody = footer |> List.rev |> List.tail |> List.rev  // drop footer's closing "}"
    header @ (if netcdf then netcdfRegisterLines else []) @ tryLine @ bodyIndented @ timing @ printCode @ footerBody @ catchClose

/// Split-timing variant of genMainWrapper. `setupIndented` is input-data setup
/// (array literals, etc.); `computeIndented` is the computation. Emits two
/// checkpoints: "Input Allocation took <t>s" around setup, and the canonical
/// "<name> completed in <t>s" around ONLY the compute region -- so the harness's
/// existing "completed in" parser reads the compute time, not the whole body.
/// The clock variable is reused (start/end reset between phases) exactly as the
/// archaic Blade prototype did.
let genMainWrapperSplit (mpi: bool, mpiThreaded: bool, netcdf: bool) (testName: string) (setupIndented: string list) (computeIndented: string list) (printCode: string list) : string list =
    let header =
        if mpi then
            [ "int main(int argc, char** argv) {"
              "    cout << std::setprecision(15);"
              "    cout << std::boolalpha;"
              (if mpiThreaded then
                  "    { int __blade_mpi_prov; MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &__blade_mpi_prov); if (__blade_mpi_prov < MPI_THREAD_FUNNELED) { std::cerr << \"error[BL8004]: MPI thread support below MPI_THREAD_FUNNELED\" << std::endl; MPI_Abort(MPI_COMM_WORLD, 14); } }"
               else
                  "    MPI_Init(&argc, &argv);")
              "    MPI_Comm_rank(MPI_COMM_WORLD, &__blade_mpi_rank);"
              "    MPI_Comm_size(MPI_COMM_WORLD, &__blade_mpi_size);"
              "    auto start = TIME;"
              "" ]
        else
            [ "int main() {"
              "    cout << std::setprecision(15);"
              "    cout << std::boolalpha;"
              "    auto start = TIME;"
              "" ]
    let setupTiming =
        let line = $"cout << \"{testName} input allocation took \" << setup_elapsed << \"s\" << endl;"
        [ ""
          "    auto end = TIME;"
          "    double setup_elapsed = 1e-9 * TIME_DIFF;"
          (if mpi then $$"""    if (__blade_mpi_rank == 0) { {{line}} }""" else "    " + line)
          ""
          "    start = TIME;" ]
    let computeTiming =
        let line = $"cout << \"{testName} completed in \" << elapsed << \"s\" << endl;"
        if mpi then
            [ ""
              "    end = TIME;"
              "    double elapsed = 1e-9 * TIME_DIFF;"
              $$"""    if (__blade_mpi_rank == 0) { {{line}} }"""
              ""
              "    // Print results for verification (rank 0 only)"
              "    if (__blade_mpi_rank == 0) {" ]
        else
            [ ""
              "    end = TIME;"
              "    double elapsed = 1e-9 * TIME_DIFF;"
              "    " + line
              ""
              "    // Print results for verification" ]
    let finalizeLines = if netcdf then netcdfFinalizeLines else []
    let footer =
        if mpi then
            // After the rank guard closes, so every rank shuts its own netcdf
            // down, and before MPI_Finalize, because a parallel-enabled netcdf
            // is layered on MPI rather than beside it.
            [ "    }" ]
            @ finalizeLines
            @ [ ""
                "    MPI_Finalize();"
                "    return 0;"
                "}" ]
        else
            finalizeLines
            @ [ ""
                "    return 0;"
                "}" ]
    // See genMainWrapper: wrap the body in try/catch -> blade_rt::panic (BL8005).
    let tryLine = [ "    try {" ]
    let catchClose =
        [ "    } catch (const std::exception& e) { blade_rt::panic(\"BL8005\", e.what(), nullptr, 0); }"
          "      catch (...) { blade_rt::panic(\"BL8005\", \"unknown exception\", nullptr, 0); }"
          "}" ]
    let footerBody = footer |> List.rev |> List.tail |> List.rev  // drop footer's closing "}"
    header @ (if netcdf then netcdfRegisterLines else []) @ tryLine @ setupIndented @ setupTiming @ computeIndented @ computeTiming @ printCode @ footerBody @ catchClose

/// Generate a C++ program (uses external runtime header)
/// Generate print statements for all bindings in a module.
/// Shared by genSelfContainedProgram and genProgramWithExternalRuntime.
let genMainProgram (modul: IRModule) (testName: string) : string =
    (exprWarningsCell ()).Value <- []
    // Reset the CUDA kernel collector; genCudaKernel appends during genModule.
    (cudaKernelDefsCell ()).Value <- []
    (symmDeclsCell ()).Value <- []
    (moduleGlobalDeclsCell ()).Value <- []
    (streamBufDeclsCell ()).Value <- Set.empty
    (forcedDeferredIdsCell ()).Value <- Set.empty
    (linalgUsedCell ()).Value <- false
    (tilesUsedCell ()).Value <- false
    (cudaLinalgUsedCell ()).Value <- false
    (lapackUsedCell ()).Value <- false
    (ompApiUsedCell ()).Value <- false
    // Deterministic deallocation: clear both cells for this program. genModule
    // reinstalls the facts immediately (it needs the callables table first).
    (freshReturnFactsCell ()).Value <- Map.empty
    (copyInPlaceMutsCell ()).Value <- Map.empty
    resetAllocScopeStack ()
    let builder = IRBuilder()
    // Codegen-synthesized ids (sequence children, __s1 stages, __ret temps)
    // must not collide with typecheck/lowering ids arriving in the module --
    // a reused id re-registers the original variable's name in VarNames.
    // 2^30 is far above any real program's id count.
    builder.EnsureAtLeast(0x40000000)

    let includes = genIncludes ()
    // MPI scaffolding (see genSelfContainedProgram).
    let mpiOn = mpiEmitModeEnabled () && moduleUsesMpi modul
    setMpiProgramOn mpiOn
    let includes = if mpiOn then includes @ ["#include <mpi.h>"; "#include \"linearized_storage.hpp\""] else includes
    let mpiDecls =
        if mpiOn then
            [ "static int __blade_mpi_rank = 0;"
              "static int __blade_mpi_size = 1;" ]
        else []
    let (funcDefs, bindCode) = genModule modul builder
    // blade_linalg.hpp include only when a linalg route (gram / matmul) was
    // actually emitted this assembly (collector fills during genModule;
    // Build.fs keys -DBLADE_HAS_BLAS + the -I/link flags off this include
    // line). Appended post-body like the CUDA prototypes below. A program
    // using neither gram nor matmul never names the header at all.
    let includes = if (linalgUsedCell ()).Value then includes @ ["#include \"blade_linalg.hpp\""] else includes
    // blade_tilecache.hpp only when a tiled binding was emitted (revision
    // reuse, docs/plans/structural/04); Build.fs keys -DBLADE_TOOLCHAIN_ID
    // off this include line.
    let includes = if (tilesUsedCell ()).Value then includes @ ["#include \"blade_tilecache.hpp\""] else includes
    // blade_linalg_cuda.hpp: the DEVICE half of the same collect-then-append
    // shape, its OWN cell and its own build consequence -- Build.fs
    // sniffs THIS line to write the companion `.cu`, build it with nvcc and
    // link it in. A third cell rather than a shared one because under
    // `resolveNodeRoute`'s fallback chain one program can legitimately reach
    // both backends (a device matmul beside a host dot), and each dependency
    // surface must be advertised on its own.
    let includes = if (cudaLinalgUsedCell ()).Value then includes @ ["#include \"blade_linalg_cuda.hpp\""] else includes
    // blade_lapack.hpp: the same collect-then-append shape, its OWN cell and
    // its own define (-DBLADE_HAS_LAPACK). Separate from the line above so a
    // gram/matmul program never advertises a LAPACK dependency and an eigh
    // program never advertises a BLAS one.
    let includes = if (lapackUsedCell ()).Value then includes @ ["#include \"blade_lapack.hpp\""] else includes
    // <omp.h> only when a comm-licensed parallel fold emitted omp_* runtime
    // calls this assembly. Same collect-then-append shape as linalg; `#pragma
    // omp` alone needs no header, so every other program keeps its includes.
    let includes =
        if (ompApiUsedCell ()).Value
           && not (includes |> List.exists (fun (s: string) -> s.StartsWith "#include <omp.h>"))
        then includes @ ["#include <omp.h>  // comm-licensed parallel fold (omp_get_max_threads)"] else includes

    // extern "C" launch-wrapper prototypes for any CUDA kernels emitted during
    // genModule. Bodies live in the .cu (nvcc); the .cpp needs only the proto to
    // call across the linkage boundary. Extract each wrapper's signature line
    // (starts `extern "C" void __launch_`) and `;`-terminate it.
    let cudaProtos =
        (cudaKernelDefsCell ()).Value
        |> List.filter (fun line -> line.StartsWith("extern \"C\"") && line.Contains("void __launch_"))
        |> List.map (fun sigLine ->
            // The hybrid (mpi+cuda) wrappers are dllexport'd in the .cu;
            // the host proto imports plainly (MinGW links the DLL exports).
            let trimmed = sigLine.Replace("__declspec(dllexport) ", "").TrimEnd()
            (if trimmed.EndsWith("{") then trimmed.Substring(0, trimmed.Length - 1).TrimEnd() else trimmed) + ";")
    let symmDecls = (symmDeclsCell ()).Value
    // S0: module-level bindings promoted to namespace scope (declaration only;
    // main() still initializes them at their original program point).
    let moduleGlobalDecls = (moduleGlobalDeclsCell ()).Value

    let bodyIndented = bindCode |> List.map (fun s -> "    " + s)
    let mainFunc = genMainWrapper (mpiOn, mpiOn && moduleHybridMpiOmp modul, moduleUsesNetcdf modul) testName bodyIndented []
    let rrLines = runRecordLines modul testName mpiOn (funcDefs @ mainFunc)

    (includes @ [""] @ mpiDecls @ rrLines @ symmDecls @ moduleGlobalDecls @ [""] @ cudaProtos @ [""] @ funcDefs @ mainFunc) |> String.concat "\n"

/// The .cu file content for the most recently assembled program, or None if no
/// CUDA kernel was emitted. Call AFTER genMainProgram/genProgramFromIR (the
/// collector is populated during assembly).
let getCudaFileContent () : string option =
    match (cudaKernelDefsCell ()).Value with
    | [] -> None
    | defs ->
        // Complex kernels: <complex> for the extern "C" wrapper signatures
        // (std::complex, the host side of the boundary) and thrust/complex.h
        // for the device dialect in kernel bodies/buffers. Conditional so
        // non-complex programs keep the previous byte-identical .cu.
        let usesComplex =
            defs |> List.exists (fun (l: string) ->
                l.Contains "std::complex" || l.Contains "thrust::complex")
        let header =
            [ "// Generated CUDA kernels (.cu) -- compiled by nvcc, linked with the .cpp."
              "#include <cstddef>"
              "#include <cstdint>" ]
            @ (if usesComplex then ["#include <complex>"; "#include <thrust/complex.h>"] else [])
            @ [ "" ]
        Some ((header @ defs) |> String.concat "\n")

/// MODULE IDENTITY SURVIVES THE MERGE. Two file modules may each declare an
/// `f`; the checker keeps them apart (a qualified import binds `a.f` and
/// `b.f` to distinct VarIds) but the C++ TU is one namespace, and both used
/// to emit as `int64_t f(int64_t)` -- a g++ redefinition. A function whose
/// name another module also declares is emitted as `<module>__<name>`
/// unless it belongs to the LAST module (the program's main module keeps
/// its spelling). Call sites resolve through the Id-keyed VarNames map, so
/// the rename is total by construction; nothing emits a function by its
/// string name. Shared by both multi-module assembly sites.
let private disambiguateModuleFunctions (modules: IRModule list) : IRModule list =
    let nameCounts =
        modules
        |> List.collect (fun m -> m.Functions |> List.map (fun f -> f.Name))
        |> List.countBy id
        |> Map.ofList
    let lastIdx = modules.Length - 1
    modules |> List.mapi (fun i m ->
        if i = lastIdx then m
        else
            let prefix = (if m.Name = "" then $"m{i}" else m.Name).Replace('.', '_')
            { m with
                Functions =
                    m.Functions |> List.map (fun f ->
                        match Map.tryFind f.Name nameCounts with
                        | Some n when n > 1 -> { f with Name = $"{prefix}__{f.Name}" }
                        | _ -> f) })

/// Generate a complete C++ program from an IR program (all modules)
let genProgramFromIR (program: IRProgram) (testName: string) : string =
    match program.Modules with
    | [] -> "// Empty program\nint main() { return 0; }\n"
    | [modul] -> genMainProgram modul testName
    | modules ->
        let modules = disambiguateModuleFunctions modules
        let merged = {
            Name = "merged"
            Types = modules |> List.collect (_.Types)
            Functions = modules |> List.collect (_.Functions)
            Bindings = modules |> List.collect (_.Bindings)
            StaticFunctionUsage = Map.empty
            ProviderReads = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderReads) Map.empty
            ProviderWrites = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderWrites) Map.empty
            RandomInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.RandomInits) Map.empty
            CompoundInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.CompoundInits) Map.empty
            SparseInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.SparseInits) Map.empty
            MutableArrayLets = modules |> List.fold (fun acc m -> Set.union acc m.MutableArrayLets) Set.empty
            // Ids are module-global (one builder), so the union carries every
            // copy's origin key across the merge intact.
            DerivedFuncOrigins = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.DerivedFuncOrigins) Map.empty
        }
        genMainProgram merged testName

/// Provider-driven #include lines for a module: the union of each involved
/// provider's declared includes (registry-dispatched over the module's
/// reads and writes), plus linearized_storage.hpp when any provider-read or
/// -written array is packed (SymIdx/AntisymIdx) -- the unlinearize copy in
/// packed readers is index-type-driven, not provider-specific. Deduplicated,
/// provider order sorted for deterministic emission.
let providerIncludes (modul: IRModule) : string list =
    let readSpecs = modul.ProviderReads |> Map.toList |> List.map snd
    let writeSpecs = modul.ProviderWrites |> Map.toList |> List.map snd
    let providers =
        (readSpecs |> List.map (_.Provider))
        @ (writeSpecs |> List.map (_.Provider))
        |> List.distinct |> List.sort
    let fromProviders =
        providers |> List.collect (fun p ->
            match Blade.ProviderRegistry.tryFind p with
            | Some spec -> spec.Includes ()
            | None -> [])
    let isPackedArr (at: IRArrayType) =
        at.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone && ix.Rank >= 2)
    let anyPacked =
        (readSpecs |> List.exists (fun s -> isPackedArr s.VarType))
        || (writeSpecs |> List.exists (fun s -> isPackedArr s.SourceType))
    (fromProviders @ (if anyPacked then ["#include \"linearized_storage.hpp\""] else []))
    |> List.distinct

/// Generate C++ struct definition from IRTDStruct
let genSelfContainedProgram (modul: IRModule) (testName: string) : string =
    let builder = IRBuilder()
    builder.EnsureAtLeast(0x40000000)  // see genMainProgram: keep codegen ids disjoint
    // Reset the CUDA kernel collector for this program; genCudaKernel appends
    // during genModule. Read afterward via getCudaFileContent for the .cu file.
    (cudaKernelDefsCell ()).Value <- []
    // Reset the symm-decl hoist collector; symmetric outputs append namespace-
    // scope symm arrays during genModule, emitted in the preamble below.
    (symmDeclsCell ()).Value <- []
    // Reset the S0 module-global promotion collector (see moduleGlobalDeclsCell).
    (moduleGlobalDeclsCell ()).Value <- []
    (streamBufDeclsCell ()).Value <- Set.empty
    // Reset the forced-deferred collector; forceDeferredArrayInput populates it
    // during genModule and genPrintStatements (called AFTER body generation)
    // reads it to auto-print deferred bindings that ended up materialized.
    (forcedDeferredIdsCell ()).Value <- Set.empty
    (linalgUsedCell ()).Value <- false
    (tilesUsedCell ()).Value <- false
    (cudaLinalgUsedCell ()).Value <- false
    (lapackUsedCell ()).Value <- false
    (ompApiUsedCell ()).Value <- false
    // Deterministic deallocation: see genMainProgram. genModule / genModuleSplit
    // reinstall the facts (both entry points below install).
    (freshReturnFactsCell ()).Value <- Map.empty
    (copyInPlaceMutsCell ()).Value <- Map.empty
    resetAllocScopeStack ()

    let includes =
        // Provider reads/writes emit provider-specific runtime calls (nc_*,
        // fstream chunk I/O, ...) needing their own headers. Added only when
        // the module actually has provider I/O, so non-provider programs gain
        // no extra dependency (registry-dispatched per provider).
        genIncludesExternal () @ providerIncludes modul
    // MPI scaffolding: only when the emit gate is on AND the module has an
    // mpi kernel (a PURE predicate -- includes are computed before genModule
    // runs, so an emission-time cell would not work here). Adds
    // <mpi.h> and the namespace-scope rank/size globals (loop nests are also
    // emitted inside top-level function bodies, so main() locals can't work).
    // Defaults 0/1 keep any pre-Init execution serially correct.
    let mpiOn = mpiEmitModeEnabled () && moduleUsesMpi modul
    setMpiProgramOn mpiOn
    let includes = if mpiOn then includes @ ["#include <mpi.h>"; "#include \"linearized_storage.hpp\""] else includes
    let mpiDecls =
        if mpiOn then
            [ "static int __blade_mpi_rank = 0;"
              "static int __blade_mpi_size = 1;" ]
        else []
    let typeDefs = genTypeDefs modul

    // Split-timing mode emits two clock checkpoints (input allocation vs
    // compute) via genModuleSplit + genMainWrapperSplit; default mode emits the
    // single whole-body clock. Both share the same CUDA-proto / symm-decl
    // preamble, computed after generation (genModule* populates the cells).
    // Print code is assembled AFTER body generation: genPrintStatements reads
    // the forced-deferred collector genModule* populates.
    let mainFunc =
        if splitTimingModeEnabled () then
            let (funcDefs, setupCode, computeCode) = genModuleSplit modul builder
            let printCode = genPrintStatements modul
            let setupIndented = setupCode |> List.map (fun s -> "    " + s)
            let computeIndented = computeCode |> List.map (fun s -> "    " + s)
            (funcDefs, genMainWrapperSplit (mpiOn, mpiOn && moduleHybridMpiOmp modul, moduleUsesNetcdf modul) testName setupIndented computeIndented printCode)
        else
            let (funcDefs, bindCode) = genModule modul builder
            let printCode = genPrintStatements modul
            let bodyIndented = bindCode |> List.map (fun s -> "    " + s)
            (funcDefs, genMainWrapper (mpiOn, mpiOn && moduleHybridMpiOmp modul, moduleUsesNetcdf modul) testName bodyIndented printCode)
    let (funcDefs, mainBody) = mainFunc
    // blade_linalg.hpp include only when a linalg route (gram / matmul) was
    // actually emitted this assembly (collector fills during genModule*;
    // Build.fs keys -DBLADE_HAS_BLAS + the -I/link flags off this include
    // line). Appended post-body like the CUDA prototypes below.
    let includes = if (linalgUsedCell ()).Value then includes @ ["#include \"blade_linalg.hpp\""] else includes
    // blade_tilecache.hpp only when a tiled binding was emitted (revision
    // reuse, docs/plans/structural/04); Build.fs keys -DBLADE_TOOLCHAIN_ID
    // off this include line.
    let includes = if (tilesUsedCell ()).Value then includes @ ["#include \"blade_tilecache.hpp\""] else includes
    // blade_linalg_cuda.hpp: the DEVICE half of the same collect-then-append
    // shape, its OWN cell and its own build consequence -- Build.fs
    // sniffs THIS line to write the companion `.cu`, build it with nvcc and
    // link it in. A third cell rather than a shared one because under
    // `resolveNodeRoute`'s fallback chain one program can legitimately reach
    // both backends (a device matmul beside a host dot), and each dependency
    // surface must be advertised on its own.
    let includes = if (cudaLinalgUsedCell ()).Value then includes @ ["#include \"blade_linalg_cuda.hpp\""] else includes
    // blade_lapack.hpp: the same collect-then-append shape, its OWN cell and
    // its own define (-DBLADE_HAS_LAPACK). Separate from the line above so a
    // gram/matmul program never advertises a LAPACK dependency and an eigh
    // program never advertises a BLAS one.
    let includes = if (lapackUsedCell ()).Value then includes @ ["#include \"blade_lapack.hpp\""] else includes
    // <omp.h>: see genMainProgram -- appended only for a comm-licensed fold.
    let includes =
        if (ompApiUsedCell ()).Value
           && not (includes |> List.exists (fun (s: string) -> s.StartsWith "#include <omp.h>"))
        then includes @ ["#include <omp.h>  // comm-licensed parallel fold (omp_get_max_threads)"] else includes

    // extern "C" launch-wrapper prototypes for any CUDA kernels emitted: the
    // .cpp calls them across the linkage boundary (bodies live in the .cu).
    let cudaProtos =
        (cudaKernelDefsCell ()).Value
        |> List.filter (fun line -> line.StartsWith("extern \"C\"") && line.Contains("void __launch_"))
        |> List.map (fun sigLine ->
            // The hybrid (mpi+cuda) wrappers are dllexport'd in the .cu;
            // the host proto imports plainly (MinGW links the DLL exports).
            let trimmed = sigLine.Replace("__declspec(dllexport) ", "").TrimEnd()
            (if trimmed.EndsWith("{") then trimmed.Substring(0, trimmed.Length - 1).TrimEnd() else trimmed) + ";")

    // Namespace-scope symm arrays hoisted out of main() (MSVC constant-address
    // requirement -- see hoistSymmDecl).
    let symmDecls = (symmDeclsCell ()).Value
    // S0: module-level bindings promoted to namespace scope (declaration only).
    let moduleGlobalDecls = (moduleGlobalDeclsCell ()).Value

    let rrLines = runRecordLines modul testName mpiOn (funcDefs @ mainBody)
    (includes @ typeDefs @ [""] @ mpiDecls @ rrLines @ symmDecls @ moduleGlobalDecls @ [""] @ cudaProtos @ [""] @ funcDefs @ mainBody) |> String.concat "\n"

/// Generate a C++ program with external runtime header
/// Returns (mainFileContent, headerFileContent)
let genProgramWithExternalRuntime (modul: IRModule) (testName: string) : string * string =
    let builder = IRBuilder()
    builder.EnsureAtLeast(0x40000000)  // see genMainProgram: keep codegen ids disjoint
    
    let includes =
        // See genSelfContainedProgram: provider headers only for provider I/O.
        genIncludesExternal () @ providerIncludes modul
    // MPI scaffolding (see genSelfContainedProgram).
    let mpiOn = mpiEmitModeEnabled () && moduleUsesMpi modul
    setMpiProgramOn mpiOn
    let includes = if mpiOn then includes @ ["#include <mpi.h>"; "#include \"linearized_storage.hpp\""] else includes
    let mpiDecls =
        if mpiOn then
            [ "static int __blade_mpi_rank = 0;"
              "static int __blade_mpi_size = 1;" ]
        else []
    let typeDefs = genTypeDefs modul
    // Reset the forced-deferred collector before body generation; the
    // genPrintStatements call below (correctly AFTER genModule) reads it.
    (forcedDeferredIdsCell ()).Value <- Set.empty
    // Reset the S0 module-global promotion collector (see moduleGlobalDeclsCell).
    (moduleGlobalDeclsCell ()).Value <- []
    (linalgUsedCell ()).Value <- false
    (tilesUsedCell ()).Value <- false
    (cudaLinalgUsedCell ()).Value <- false
    (lapackUsedCell ()).Value <- false
    (ompApiUsedCell ()).Value <- false
    // Deterministic deallocation: see genMainProgram.
    (freshReturnFactsCell ()).Value <- Map.empty
    (copyInPlaceMutsCell ()).Value <- Map.empty
    resetAllocScopeStack ()
    let (funcDefs, bindCode) = genModule modul builder
    // blade_linalg.hpp include only when a linalg route (gram / matmul) was
    // actually emitted this assembly (collector fills during genModule;
    // Build.fs keys -DBLADE_HAS_BLAS + the -I/link flags off this include
    // line).
    let includes = if (linalgUsedCell ()).Value then includes @ ["#include \"blade_linalg.hpp\""] else includes
    // blade_tilecache.hpp only when a tiled binding was emitted (revision
    // reuse, docs/plans/structural/04); Build.fs keys -DBLADE_TOOLCHAIN_ID
    // off this include line.
    let includes = if (tilesUsedCell ()).Value then includes @ ["#include \"blade_tilecache.hpp\""] else includes
    // blade_linalg_cuda.hpp: the DEVICE half of the same collect-then-append
    // shape, its OWN cell and its own build consequence -- Build.fs
    // sniffs THIS line to write the companion `.cu`, build it with nvcc and
    // link it in. A third cell rather than a shared one because under
    // `resolveNodeRoute`'s fallback chain one program can legitimately reach
    // both backends (a device matmul beside a host dot), and each dependency
    // surface must be advertised on its own.
    let includes = if (cudaLinalgUsedCell ()).Value then includes @ ["#include \"blade_linalg_cuda.hpp\""] else includes
    // blade_lapack.hpp: the same collect-then-append shape, its OWN cell and
    // its own define (-DBLADE_HAS_LAPACK). Separate from the line above so a
    // gram/matmul program never advertises a LAPACK dependency and an eigh
    // program never advertises a BLAS one.
    let includes = if (lapackUsedCell ()).Value then includes @ ["#include \"blade_lapack.hpp\""] else includes
    // <omp.h>: see genMainProgram -- appended only for a comm-licensed fold.
    let includes =
        if (ompApiUsedCell ()).Value
           && not (includes |> List.exists (fun (s: string) -> s.StartsWith "#include <omp.h>"))
        then includes @ ["#include <omp.h>  // comm-licensed parallel fold (omp_get_max_threads)"] else includes

    let bodyIndented = bindCode |> List.map (fun s -> "    " + s)
    let printCode = genPrintStatements modul
    let mainFunc = genMainWrapper (mpiOn, mpiOn && moduleHybridMpiOmp modul, moduleUsesNetcdf modul) testName bodyIndented printCode

    // S0: module-level bindings promoted to namespace scope (declaration only).
    let moduleGlobalDecls = (moduleGlobalDeclsCell ()).Value
    let rrLines = runRecordLines modul testName mpiOn (funcDefs @ mainFunc)
    let mainFile = (includes @ typeDefs @ [""] @ mpiDecls @ rrLines @ moduleGlobalDecls @ funcDefs @ mainFunc) |> String.concat "\n"
    let headerFile = genRuntimeHeader ()
    (mainFile, headerFile)

/// Generate a self-contained C++ program from an IR program
let genSelfContainedProgramFromIR (program: IRProgram) (testName: string) : string * string list =
    // Reset module-level expression warnings (per-task via AsyncLocal cell)
    let cell = exprWarningsCell ()
    cell.Value <- []
    (exprSentinelsCell ()).Value <- []
    // Back-end holes from a previous assembly must not be attributed to this
    // one (the driver drains the channel, but a caller that never drains --
    // the test harness -- would otherwise accumulate across tests).
    (unhandledNodesCell ()).Value <- []
    (codegenRefusalsCell ()).Value <- []
    (currentDeclCell ()).Value <- ""
    resetStreamedValueState ()
    // Deterministic deallocation: see genMainProgram.
    (freshReturnFactsCell ()).Value <- Map.empty
    (copyInPlaceMutsCell ()).Value <- Map.empty
    resetAllocScopeStack ()
    let code =
        match program.Modules with
        | [] -> "// Empty program\nint main() { return 0; }\n"
        | [modul] -> genSelfContainedProgram modul testName
        | modules ->
            // Multi-module: merge all modules into one for code generation
            // Functions and bindings from earlier modules come first
            let modules = disambiguateModuleFunctions modules
            let merged = {
                Name = "merged"
                Types = modules |> List.collect (_.Types)
                Functions = modules |> List.collect (_.Functions)
                Bindings = modules |> List.collect (_.Bindings)
                StaticFunctionUsage = modules |> List.fold (fun acc m -> 
                    Map.fold (fun a k v -> Map.add k v a) acc m.StaticFunctionUsage) Map.empty
                ProviderReads = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderReads) Map.empty
                ProviderWrites = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderWrites) Map.empty
                RandomInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.RandomInits) Map.empty
                CompoundInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.CompoundInits) Map.empty
                SparseInits = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.SparseInits) Map.empty
                MutableArrayLets = modules |> List.fold (fun acc m -> Set.union acc m.MutableArrayLets) Set.empty
                // See the genProgramFromIR twin: ids are module-global, so the
                // union preserves every copy's origin key.
                DerivedFuncOrigins = modules |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.DerivedFuncOrigins) Map.empty
            }
            genSelfContainedProgram merged testName
    // Expression-position refusals become real `#error` directives. They are
    // appended to the END of the translation unit rather than spliced at the
    // refusal site because the refusal site is an EXPRESSION and a preprocessor
    // directive cannot live inside one. Position does not matter for the
    // diagnostic: `#error` fires during preprocessing, before the bare
    // BLADE_CODEGEN_ERROR_ identifier is ever looked up, so this message -- not
    // g++'s "not declared in this scope" -- is what the user and the corpus
    // runner's REJECT-AT: codegen verdict see. A program with no expression
    // refusal appends nothing, so no currently-compiling program is affected.
    // A `.stream` binding rendered as a value: refused only if its sentinel
    // reached the unit (CodeGenState, "STREAMED VALUES NEVER REACH C++").
    let code = settleStreamedValueLeaks code
    let sentinels = (exprSentinelsCell ()).Value
    let code =
        if List.isEmpty sentinels then code
        else
            let directives =
                sentinels
                |> List.map (fun m -> sprintf "#error \"Blade codegen: %s\"" (m.Replace("\"", "'")))
            code + "\n" + (directives |> String.concat "\n") + "\n"
    (code, cell.Value)
