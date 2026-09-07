// Per-binding-kind statement emission: one rec-chain from genBinding
// through the ~40 genXBinding emitters (compute, provider read/write,
// group-by family, sort/transpose/stack/join, reduce family, guard/
// sequence/let-chain). Atomic by mutual recursion -- do not split further.
module Blade.CodeGenBinding

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

let rec genBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding

    match binding.Value with
    | _ when Map.containsKey binding.Id ctx.ProviderReads ->
        genProviderReadBinding ctx binding builder
    | _ when Map.containsKey binding.Id ctx.ProviderWrites ->
        genProviderWriteBinding ctx binding builder
    | _ when Map.containsKey binding.Id ctx.RandomInits ->
        match ctx.RandomInits.[binding.Id] with
        | RandGen _ -> genRandGenBinding ctx binding builder
        | FillModulus _ -> genRandomInitBinding ctx binding builder
    | _ when Map.containsKey binding.Id ctx.CompoundInits ->
        genCompoundInitBinding ctx binding builder
    | _ when Map.containsKey binding.Id ctx.SparseInits ->
        genSparseInitBinding ctx binding
    | IRMask (arrExpr, predExpr) ->
        genMaskBinding ctx binding builder arrExpr predExpr
    | IRIntersect (aExpr, bExpr) ->
        genIntersectBinding ctx binding builder aExpr bExpr
    | IRUnion (aExpr, bExpr) ->
        genUnionBinding ctx binding builder aExpr bExpr
    | IRUnique arrExpr ->
        genUniqueBinding ctx binding builder arrExpr
    | IRRange _ ->
        genRangeBinding ctx binding
    | IRGroupKeys keys ->
        genGroupKeysBinding ctx binding builder keys
    | IRSegments (offsets, _) ->
        genSegmentsBinding ctx binding offsets
    | IRSegmentsGrid bounds ->
        genSegmentsGridBinding ctx binding bounds
    | IRUngroupGrid (g, srcs, bounds) ->
        genUngroupGridBinding ctx binding g srcs bounds
    | IRUngroup (g, src) ->
        genUngroupBinding ctx binding g src
    | IRUngroupRows (rows, offsets, src) ->
        genUngroupRowsBinding ctx binding rows offsets src
    | IRGroupBy (vals, gk) ->
        genGroupByBinding ctx binding builder vals gk
    | IRGroupBucket gk ->
        genGroupBucketBinding ctx binding builder gk
    | IRGroupSizes gk ->
        genGroupSizesBinding ctx binding builder gk
    | IRSort (arrExpr, keyExpr) ->
        genSortBinding ctx binding builder arrExpr keyExpr
    | IRTranspose (arrExpr, d1, d2) ->
        genTransposeBinding ctx binding builder arrExpr d1 d2
    | IRStack arrs ->
        genStackJoinBinding ctx binding builder arrs None
    | IRJoin (arrs, dim) ->
        genStackJoinBinding ctx binding builder arrs (Some dim)
    | IRDecompact (arrExpr, d) ->
        genDecompactBinding ctx binding builder arrExpr d
    | IRArrayNegate arrExpr | IRArrayConjugate arrExpr ->
        genArrayNegateConjugateBinding ctx binding builder arrExpr
    | IRGram (_, _, _) ->
        genGramBinding ctx binding builder
    | IRGramApply (_, _, _) ->
        genGramApplyBinding ctx binding builder
    | IRMatmul (_, _) ->
        genMatmulBinding ctx binding builder
    | IREigh _ ->
        genEighBinding ctx binding builder
    | IRSolve (_, _) ->
        genSolveBinding ctx binding builder
    | IRReduce (arrExpr, kernelExpr, initExpr) ->
        genReduceBinding ctx binding builder arrExpr kernelExpr initExpr
    | IRReduceCompute (compExpr, kernelExpr, seedExpr) ->
        genReduceComputeBinding ctx binding builder compExpr kernelExpr seedExpr
    | IRProdSum _ ->
        // Scalar result; the expression renderer emits the fused-loop IIFE.
        // That IIFE subscripts every operand BY NAME, so a still-deferred
        // producer must be materialized first (this arm bypasses
        // genScalarExprBinding, which is where the pre-pass normally runs).
        let (forceCode, ctx) = forceDeferredPositionalReads ctx builder ($"{name}__def") binding.Value
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (forceCode @ code, ctx')
    | IRArrayLit (elements, arrType) ->
        let code = genArrayLiteral ctx name elements arrType
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')
    
    | IRApplyCombinator info ->
        // Defer: computation not materialized until |> compute or combinator forces it
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation>"], ctx')

    | IRComposeApply _ ->
        // Defer: compose-apply is also a lazy computation; materialized
        // when |> compute reaches it (or a combinator forces it).
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred compose-apply>"], ctx')
    
    | IRParallel _ | IRFusion _ ->
        // Defer: computation combinator not materialized until |> compute
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation combinator>"], ctx')
    
    | IRFunctorMap _ ->
        // Defer: functor map not materialized until |> compute
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred functor map>"], ctx')
    
    | IRZip _ ->
        // Defer: zip is a lazy array combinator, absorbed by method_for or materialized by |> compute
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred zip>"], ctx')
    
    | IRChoice (left, right) ->
        genChoiceBinding ctx binding builder left right
    | IRFallback (left, right) ->
        genFallbackBinding ctx binding builder left right
    | IRGuard (_, body) ->
        genGuardBinding ctx binding builder body
    | IRSequence elems ->
        genSequenceBinding ctx binding builder elems
    // `|> compute` on an ALREADY-EAGER whole-array form is the identity: each
    // of these materializes a fresh pool by construction, so there is nothing
    // deferred left to force. genComputeBinding's dispatch has no arm for any
    // of them and its fall-through treats the value as a SCALAR, so they died
    // on the unsupported-node sentinel -- `let n = -A` worked while
    // `let n = -A |> compute` did not, and `decompact(S, 0) |> compute` was
    // BL7001 at module scope even though the unwrapped spelling was fine.
    // Re-dispatch on the unwrapped form, which the arms above already handle.
    //
    // The set is `isStatementShaped` MINUS the deferring family (whose whole
    // point is that `|> compute` IS the forcing site -- peeling their wrapper
    // would route them back to the emitter that defers, and the compute would
    // become a no-op) and MINUS IRGroupKeys/IRGroupBy (a `|> compute` on a
    // grouping is not a spelling the surface produces, and genComputeBinding
    // has no arm to fall back on if this guess were wrong).
    //
    // WRITTEN AS THAT SUBTRACTION, not as the 18 constructors it works out to.
    // The hand-enumerated version said the same thing in the comment and then
    // re-derived it by hand below, so the two could drift the moment a new
    // statement-shaped node was added: `isStatementShaped` would gain it and
    // this list would not, and the miss is silent (the node falls to
    // genComputeBinding's scalar fall-through and dies on the unsupported-node
    // sentinel -- exactly the BL7001 this arm was added to stop). Both
    // subtrahends are predicates in IR.fs beside `isStatementShaped` itself.
    | IRCompute (IRRange _ as inner) ->
        // `0..n |> compute`: compute IS the forcing request, and the range's
        // own binding arm is the materializer -- re-dispatch on the unwrapped
        // range. (Not statement-shaped, so the subtraction arm below never
        // catches it, and genComputeBinding's fall-through would treat it as
        // a scalar and die on the unsupported-node sentinel.)
        genBinding ctx { binding with Value = inner } builder
    | IRCompute eager when
        isStatementShaped eager && not (isDeferringForm eager) && not (isGroupTableForm eager) ->
        genBinding ctx { binding with Value = eager } builder
    | IRCompute inner ->
        genComputeBinding ctx binding builder inner
    | IRMethodFor _ ->
        // method_for creates a loop object - no runtime code needed
        // Just track the variable name for later use
        let ctx' = addVarName binding.Id name ctx
        ([$"{ind}// {name} = method_for(...) [loop object]"], ctx')
    
    | IRObjectFor _ ->
        // object_for creates a loop object - no runtime code needed.
        // Register the provenance in ObjectLoopBindings (NOT DeferredComputations)
        // so genComposeApply can chase a composed leaf `IRVar(o)` back to this
        // IRObjectFor; without it, `(o1 >>@ o2) <@> A` over let-bound objects
        // emits the object binding name as an undeclared C++ callable.
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with ObjectLoopBindings = Map.add binding.Id binding.Value ctx'.ObjectLoopBindings }
        ([$"{ind}// {name} = object_for(...) [loop object]"], ctx')
    
    | IRApp (IRObjectFor objInfo, args, _) ->
        // Inline application of object_for - need to expand to loop nest
        // This handles cases like: let added = A [+] B
        // Convert to an ApplyCombinator-like structure and generate
        let arrays =
            match args with
            | [IRTuple elems] -> elems
            | _ -> args
        let code = genObjectForApplication ctx name objInfo arrays builder
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')
    
    | IRStructLit _ ->
        // Struct construction -- where-constraint guards are separate
        // IRConstraintCheck bindings synthesized by the checker.
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')
    
    | IRTupleProj (parentExpr, projIdx, isFlat) ->
        genTupleProjBinding ctx binding builder parentExpr projIdx isFlat
    | IRVar (srcId, _) ->
        genVarAliasBinding ctx binding builder srcId
    | IRBind (comp, cont) ->
        genBindChainBinding ctx binding builder comp cont
    | IRTuple _ | IRComplex _ | IRFma _ | IRFieldAccess _ | IRLit _ | IRBinOp _ | IRUnaryOp _ | IRIf _ | IRApp _ | IRParam _ | IRMatch _
    // display.emit is a Bool-valued scalar like the rest of this group -- the
    // frame write is a side effect of evaluating it, and it lands in main()'s
    // BODY, ahead of the timing line and the print block. That position is what
    // the interpreter mirrors (Interp/Run.fs flushes its frame buffer before
    // printBindings), which is what keeps the differential gate happy.
    | IRPure _ | IRIndex _ | IRExtent _ | IRContains _ | IRDisplayEmit _
    // json_array / json_num / json_string answer a String scalar; the same
    // scalar-binding path serves them (their C++ is a single
    // blade_display::json* call).
    | IRDisplayJson _ | IRDisplayNum _ | IRDisplayStr _ ->
        genScalarExprBinding ctx binding builder
    
    | IRCompose _ ->
        // Function composition: uses generic lambdas (auto... args)
        let valueStr = exprToCppCtx ctx binding.Value
        let code = [$"{ind}auto {name} = {valueStr};"]
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

    | IRComposeObj _ ->
        // Defer: ObjectLoop composition, materialized when applied via <@>
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred compose_obj>"], ctx')

    | IRComposeMeth _ ->
        // Defer: computation composition, materialized when |> compute
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred compose_meth>"], ctx')
    
    | IRLet _ ->
        genLetChainBinding ctx binding builder 
    | IRAssign _ ->
        // Assignment expression: generate as statement
        let code = [$"{ind}{(exprToCppCtx ctx binding.Value)};"]
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

    | IRConstraintCheck (cond, blCode, message, span) ->
        // Runtime constraint guard -- the loud-failure idiom (cerr + abort).
        let code =
            [ $$"""{{ind}}if (!({{(exprToCppCtx ctx cond)}})) {"""
              $"{ind}    blade_rt::panic(\"{blCode}\", \"{message}\", {(panicSpanArgs span)});"
              $"{ind}}}" ]
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

    | IRBreakIf cond ->
        // Early exit from the enclosing for-range (the rec-array `while`
        // guard). The per-iteration frees accumulated SO FAR must run before
        // the break -- a naive `break` skips the bottom-of-body frees and
        // leaks whatever this iteration already materialized. Peek, don't
        // pop: the frame stays live for the non-breaking path.
        let frees = peekAllocScopeFrees (ind + "    ")
        let code =
            [ $$"""{{ind}}if ({{(exprToCppCtx ctx cond)}}) {""" ]
            @ frees
            @ [ $"{ind}    break;"
                $"{ind}}}" ]
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

    | IRForRange (vid, lo, hi, body) ->
        genForRangeBinding ctx binding builder vid lo hi body
    | other ->
        let ctx' = addVarName binding.Id name ctx
        let nodeType = other.GetType().Name
        // Same classification as exprToCpp's catch-all: no arm for this shape
        // in binding position is a back-end gap, reported as a Blade error.
        recordUnhandledIRNode ($"binding position (binding '{name}')") nodeType
        (codegenError ctx ind ($"unsupported expression for binding '{name}' (IR node: {nodeType})"), ctx')

// Module Generation

/// Generate a function body as a list of C++ statements.
/// Unrolls IRLet chains into sequential variable declarations with a final return.

and genScalarExprBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Force deferred producers read positionally inside a compound scalar
    // expression (`let x = A(0) + A(1)`, `if A(0) > 0.0 then ...`). The direct
    // single-read case (`let x = A(i)`) is handled by the dedicated IRIndex arm
    // below; this pre-pass covers reads nested in binops / ifs / calls / match.
    // After forcing, those bases are no longer in DeferredComputations, so the
    // IRIndex arm's guard falls through to the plain by-name render.
    let (forceCode, ctx) = forceDeferredPositionalReads ctx builder ($"{name}__def") binding.Value
    let prepend (code, ctx') = (forceCode @ code, ctx')
    // Check if it's a tuple of deferred computations
    prepend <|
    match binding.Value with
    | IRTuple elems when elems |> List.forall (fun e ->
        match e with
        | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ -> true
        | IRVar (id, _) -> Map.containsKey id ctx.DeferredComputations
        | _ -> false) ->
        // All elements are computations -- defer the whole tuple
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation tuple>"], ctx')
    | IRFieldAccess _ when (match binding.Type with ArrayElem _ -> true | _ -> false) ->
        // Struct field of array type: the field itself is already an
        // Array<T,N> / Ragged<T> wrapper (per genStructDef field
        // rendering). Assigning it to a wrapper-typed binding copies
        // the wrapper, which carries its shape via .extents. No
        // companion alias needed.
        let dataCode = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (dataCode, ctx')
    | IRIndex (arrExpr, indices, identity) when (match arrExpr with
                                                 | IRVar (id, _) -> Map.containsKey id ctx.DeferredComputations
                                                 | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _
                                                 | IRFunctorMap _ | IRComposeMeth _ | IRBind _ | IRCompute _ -> true
                                                 | _ -> false) ->
        // Positional read whose base array is a still-unforced computation
        // (e.g. the PPL formers' row slices `let __ppl_row_A_i = A(i)` over a
        // COMPUTED source array): the emitted C++ indexes the array by NAME
        // (`A.data[i]`), so the producer must be materialized in scope first --
        // the same contract the rearrangement combinators enforce via the
        // shared forceDeferredArrayInput helper.
        let (forceCode, ctx, arrExpr') = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
        let code = genScalarBinding ctx name (IRIndex (arrExpr', indices, identity)) binding.Type
        let ctx' = addVarName binding.Id name ctx
        (forceCode @ code, ctx')
    | _ ->
        // Scalar expressions including tuples, field access, match, bind, pure
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

and genGroupKeysBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (keys: IRExpr list) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // group_keys: build CSR offsets + permutation from a key array. Three
    // cases, dispatched on (ngroupsOpt, enumValuesOpt) from IRTGroupKeys:
    //   Case 1 -- positional buckets (Idx<N> keys): ngroups known at compile
    //     time, keys are bucket indices in [0, N); stack-allocated counts/
    //     offsets/fill.
    //   Case 2 -- EnumIdx reverse lookup: ngroups known, plus an explicit list
    //     of admissible key values; a __bucket(__v) lambda maps each key to
    //     its position in the values list.
    //   Case 3 -- dynamic discovery: ngroups unknown; builds a key ->
    //     bucket-index unordered_map in first-occurrence order, reused for
    //     counts and permutation. All sizing arrays heap-allocated.
    //
    // C++ ABI, all three cases (consumed by IRGroupBy codegen and method_for
    // ragged-peel paths below): `<name>__ngroups` (size_t, group count),
    // `<name>__offsets` (CSR, length ngroups+1), `<name>__perm` (permutation,
    // length input), `<name>__nsrc` (source row count = the allocated length of
    // __perm), plus case-specific transients not consumed elsewhere.
    // `<name>` itself is a void* sentinel -- state lives in these suffixed
    // symbols, read by name downstream.
    //
    // __perm is allocated at __nsrc but only FILLED to offsets[ngroups]: a row
    // whose key was negative is dropped (negativeKeyDrop) and never named. So
    // __nsrc is not recoverable from the CSR pair, and genGroupBucketBinding --
    // which must answer for every source row, dropped ones included -- needs it
    // carried explicitly.
    //
    // Compound (multi-key) mode (`keys` length >1): dispatch becomes an
    // unordered_map<std::tuple<...>, size_t> keyed by component tuple, each
    // unique tuple its own bucket. Same ABI as the single-key dynamic case, so
    // downstream consumers don't need to know single- vs multi-key. Requires
    // the tuple_hasher helper from nested_array_utilities.hpp.
    match keys with
    | [] ->
        let ctx' = addVarName binding.Id name ctx
        (codegenError ctx ind "group_keys with empty key list (should have been caught by typechecker)", ctx')
    | [singleKey] ->
        // Existing single-key path: three sub-cases (positional /
        // EnumIdx / dynamic), dispatched on the binding's
        // IRTGroupKeys (ngroupsOpt, enumValuesOpt).
        let keysName = exprToCppCtx ctx singleKey
        let (elemType, keysElemErrCode) = inferElemTypeStrict ctx ind singleKey "group_keys"
        let elemStr = elemTypeToCpp elemType
        // The key array may be a rank-1 COMPOUND compact view (the
        // mask -> compound -> map chain), which has no .extents / operator[].
        let (keysBound, keysAt) = compactOrDenseSource singleKey keysName
        // Negative keys drop their row from the grouping (see negativeKeyDrop).
        let dropNeg = negativeKeyDrop elemType "__k" ind
        match binding.Type with
        | IRTGroupKeys (outerIdx, _, enumValuesOpt) ->
            let ngroupsOpt =
                match outerIdx.Extent with
                | IRLit (IRLitInt n) -> Some (int n)
                | _ -> None
            match ngroupsOpt, enumValuesOpt with
            | None, _ ->
                // Case 3 -- dynamic ngroups via hash discovery. Builds a key ->
                // bucket-index map in a single discovery pass, then reuses the
                // map for counts/offsets/permutation. Bucket indices are
                // assigned in first-occurrence order: the bucket for a key is
                // its position in the sequence of distinct keys as encountered
                // walking the input left-to-right. This handles sparse keys
                // (e.g. [101, 205, 307]) correctly, unlike a max-key-scan
                // (which would allocate one bucket per integer in [0..max]).
                // Users who want numeric bucket ordering instead of
                // first-occurrence order should annotate with `Idx<N>` to opt
                // into Case 1.
                // Each key pass opens the same way: read the cell into __k,
                // then (numeric keys) skip the row when the key is negative.
                let openPass =
                    [ $$"""{{ind}}for (size_t __ki = 0; __ki < {{keysBound}}; __ki++) {"""
                      $"""{ind}    {elemStr} __k = {(keysAt "__ki")};""" ] @ dropNeg
                let code = List.concat [
                    keysElemErrCode
                    [ $"{ind}// group_keys: dynamic ngroups (hash discovery, {elemStr} keys)"
                      $"{ind}std::unordered_map<{elemStr}, size_t> {name}__lookup;"
                      $"{ind}size_t {name}__ngroups = 0;" ]
                    openPass
                    [ $"{ind}    if ({name}__lookup.find(__k) == {name}__lookup.end()) {name}__lookup[__k] = {name}__ngroups++;"
                      $"{ind}}}"
                      $"{ind}size_t* {name}__counts = new size_t[{name}__ngroups]();" ]
                    openPass
                    [ $"{ind}    {name}__counts[{name}__lookup[__k]]++;"
                      $"{ind}}}"
                      $"{ind}size_t* {name}__offsets = new size_t[{name}__ngroups + 1];"
                      $"{ind}{name}__offsets[0] = 0;"
                      $"{ind}for (size_t __gi = 0; __gi < {name}__ngroups; __gi++) {name}__offsets[__gi + 1] = {name}__offsets[__gi] + {name}__counts[__gi];"
                      $"{ind}size_t* {name}__fill = new size_t[{name}__ngroups]();"
                      $"{ind}size_t* {name}__perm = new size_t[{keysBound}];" ]
                    openPass
                    [ $"{ind}    size_t __g = {name}__lookup[__k];"
                      $"{ind}    {name}__perm[{name}__offsets[__g] + {name}__fill[__g]++] = __ki;"
                      $"{ind}}}"
                      $"{ind}size_t {name}__nsrc = {keysBound}; // source rows (>= offsets[ngroups]; negative keys drop)"
                      $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
                      $"{ind}auto {name}__at = [&](size_t __p) {{ return {name}__perm[__p]; }};"
                      $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets, {name}__perm" ]
                ]
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
            | Some ngroups, None ->
                // Case 1: positional bucketing. keys[i] in [0, ngroups).
                let openPass =
                    [ $$"""{{ind}}for (size_t __ki = 0; __ki < {{keysBound}}; __ki++) {"""
                      $"""{ind}    {elemStr} __k = {(keysAt "__ki")};""" ] @ dropNeg
                let code = List.concat [
                    keysElemErrCode
                    [ $"{ind}// group_keys: {ngroups} groups, positional buckets (Idx<N> keys)"
                      $"{ind}size_t {name}__ngroups = {ngroups};"
                      $$"""{{ind}}size_t {{name}}__counts[{{ngroups}}] = {0};""" ]
                    openPass
                    [ $"{ind}    {name}__counts[__k]++;"
                      $"{ind}}}"
                      $"{ind}size_t {name}__offsets[{ngroups + 1}];"
                      $"{ind}{name}__offsets[0] = 0;"
                      $"{ind}for (size_t __gi = 0; __gi < {ngroups}; __gi++) {name}__offsets[__gi + 1] = {name}__offsets[__gi] + {name}__counts[__gi];"
                      $$"""{{ind}}size_t {{name}}__fill[{{ngroups}}] = {0};"""
                      $"{ind}size_t* {name}__perm = new size_t[{keysBound}];" ]
                    openPass
                    [ $"{ind}    size_t __g = (size_t)__k;"
                      $"{ind}    {name}__perm[{name}__offsets[__g] + {name}__fill[__g]++] = __ki;"
                      $"{ind}}}"
                      $"{ind}size_t {name}__nsrc = {keysBound}; // source rows (>= offsets[ngroups]; negative keys drop)"
                      $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
                      $"{ind}auto {name}__at = [&](size_t __p) {{ return {name}__perm[__p]; }};"
                      $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets, {name}__perm" ]
                ]
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
            | Some ngroups, Some values ->
                // Case 2: EnumIdx -- keys are arbitrary integers OR strings,
                // mapped to bucket indices [0, ngroups) via an
                // `unordered_map<K, size_t>` lookup. Each value's bucket
                // index is its position in the EnumIdx values list. The
                // map is `static const` at the enclosing function scope:
                // initialized once on first encounter (thread-safe magic
                // static), reused across every group_keys evaluation in
                // the same call site. Lookup falls through to bucket 0
                // for unknown keys, preserving the prior silent-default
                // behavior (EnumIdx is type-checked, so unknown keys
                // indicate a typechecker bug rather than a user error).
                //
                // Why a map and not a switch / if-chain: dispatch cost
                // scales O(1) instead of O(values) per element. Especially
                // visible for string EnumIdx, where prior if-chains
                // compared each key against every value-literal in turn.
                let bucketEntries =
                    let renderVal v =
                        match v with
                        | EVInt n -> $"{n}LL"
                        | EVString s -> escapeStringLit s
                    values
                    |> List.mapi (fun i v ->
                        $"{{{(renderVal v)}, (size_t){i}}}")
                    |> String.concat ", "
                let bucketMapDecl =
                    $"static const std::unordered_map<{elemStr}, size_t> {name}__bucket_map = {{{bucketEntries}}};"
                let bucketLambdaDecl =
                    $$"""auto {{name}}__bucket = [](const {{elemStr}}& __v) -> size_t { auto it = {{name}}__bucket_map.find(__v); return it != {{name}}__bucket_map.end() ? it->second : (size_t)0; };"""
                let code = keysElemErrCode @ [
                    $"{ind}// group_keys: {ngroups} groups, EnumIdx reverse lookup (unordered_map dispatch)"
                    $"{ind}size_t {name}__ngroups = {ngroups};"
                    $"{ind}{bucketMapDecl}"
                    $"{ind}{bucketLambdaDecl}"
                    $$"""{{ind}}size_t {{name}}__counts[{{ngroups}}] = {0};"""
                    $$"""{{ind}}for (size_t __ki = 0; __ki < {{keysBound}}; __ki++) {"""
                    $"""{ind}    {name}__counts[{name}__bucket({(keysAt "__ki")})]++;"""
                    $"{ind}}}"
                    $"{ind}size_t {name}__offsets[{ngroups + 1}];"
                    $"{ind}{name}__offsets[0] = 0;"
                    $"{ind}for (size_t __gi = 0; __gi < {ngroups}; __gi++) {name}__offsets[__gi + 1] = {name}__offsets[__gi] + {name}__counts[__gi];"
                    $$"""{{ind}}size_t {{name}}__fill[{{ngroups}}] = {0};"""
                    $"{ind}size_t* {name}__perm = new size_t[{keysBound}];"
                    $$"""{{ind}}for (size_t __ki = 0; __ki < {{keysBound}}; __ki++) {"""
                    $"""{ind}    size_t __g = {name}__bucket({(keysAt "__ki")});"""
                    $"{ind}    {name}__perm[{name}__offsets[__g] + {name}__fill[__g]++] = __ki;"
                    $"{ind}}}"
                    $"{ind}size_t {name}__nsrc = {keysBound}; // source rows (EnumIdx keys never drop, so == offsets[ngroups])"
                    $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
                    $"{ind}auto {name}__at = [&](size_t __p) {{ return {name}__perm[__p]; }};"
                    $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets, {name}__perm"
                ]
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
        | _ ->
            let ctx' = addVarName binding.Id name ctx
            (codegenError ctx ind ($"group_keys binding '{name}' has wrong inferred type (expected IRTGroupKeys)"), ctx')
    | multipleKeys ->
        // Compound (multi-key) dispatch: tuple-keyed unordered_map. Each
        // (k1, k2, ...) tuple discovered becomes its own bucket in
        // first-occurrence order -- the dynamic Case 3, generalized to
        // multi-component keys. Tuple type std::tuple<T1, T2, ...>: each Ti
        // must be a hashable scalar (enforced by IRTGroupKeys construction in
        // typecheck); `tuple_hasher` (nested_array_utilities.hpp) applies the
        // canonical hash-combine recipe. C++ ABI is identical to single-key
        // Case 3 (`<name>__ngroups`/`__offsets`/`__perm`, plus compound-
        // specific `__lookup`/`__counts`/`__fill`), so downstream IRGroupBy
        // and method_for ragged-peel paths see a normal CSR structure without
        // knowing whether grouping was compound or scalar.
        
        // Per-key data: C++ name + element type + any err lines.
        let keyData =
            multipleKeys |> List.map (fun k ->
                let kName = exprToCppCtx ctx k
                let (kElem, kErr) = inferElemTypeStrict ctx ind k "group_keys (compound key)"
                (kName, elemTypeToCpp kElem, kErr))
        let keyErrCode = keyData |> List.collect (fun (_, _, e) -> e)
        let keyNames = keyData |> List.map (fun (n, _, _) -> n)
        let tupleTypeStr =
            keyData
            |> List.map (fun (_, t, _) -> t)
            |> String.concat ", "
            |> sprintf "std::tuple<%s>"
        // Use the FIRST key array's length for outer iteration. Typecheck has
        // verified all key arrays share the outer extent. Each component reads
        // through the compound-or-dense accessor, so a compact view works here
        // too.
        let keyAccess = List.map2 (fun k n -> compactOrDenseSource k n) multipleKeys keyNames
        let outerExtent = fst (List.head keyAccess)
        // make_tuple(k1[__ki], k2[__ki], ...) expression.
        let makeTupleAt indexVar =
            keyAccess
            |> List.map (fun (_, at) -> at indexVar)
            |> String.concat ", "
            |> sprintf "std::make_tuple(%s)"
        // A tuple row is dropped when ANY numeric component is negative --
        // the componentwise reading of the negative-key sentinel (a row that
        // no family claims on any axis belongs to no group).
        let dropNegTuple =
            let tests =
                List.map2 (fun k (_, at) ->
                    let (kElem, _) = inferElemTypeStrict ctx ind k "group_keys (compound key)"
                    if List.isEmpty (negativeKeyDrop kElem "__x" ind) then None
                    else Some ($"""{(at "__ki")} < 0""")) multipleKeys keyAccess
                |> List.choose id
            if List.isEmpty tests then []
            else [$"""{ind}    if ({(String.concat " || " tests)}) continue; // negative key component: row belongs to no group"""]
        let openPass =
            [ $$"""{{ind}}for (size_t __ki = 0; __ki < {{outerExtent}}; __ki++) {""" ] @ dropNegTuple
        let code = List.concat [
            keyErrCode
            [ $"{ind}// group_keys: compound dispatch ({multipleKeys.Length}-key tuple), dynamic ngroups via hash discovery"
              $"{ind}std::unordered_map<{tupleTypeStr}, size_t, tuple_hasher> {name}__lookup;"
              $"{ind}size_t {name}__ngroups = 0;" ]
            openPass
            [ $"""{ind}    auto __k = {(makeTupleAt "__ki")};"""
              $"{ind}    if ({name}__lookup.find(__k) == {name}__lookup.end()) {name}__lookup[__k] = {name}__ngroups++;"
              $"{ind}}}"
              $"{ind}size_t* {name}__counts = new size_t[{name}__ngroups]();" ]
            openPass
            [ $"""{ind}    {name}__counts[{name}__lookup[{(makeTupleAt "__ki")}]]++;"""
              $"{ind}}}"
              $"{ind}size_t* {name}__offsets = new size_t[{name}__ngroups + 1];"
              $"{ind}{name}__offsets[0] = 0;"
              $"{ind}for (size_t __gi = 0; __gi < {name}__ngroups; __gi++) {name}__offsets[__gi + 1] = {name}__offsets[__gi] + {name}__counts[__gi];"
              $"{ind}size_t* {name}__fill = new size_t[{name}__ngroups]();"
              $"{ind}size_t* {name}__perm = new size_t[{outerExtent}];" ]
            openPass
            [ $"""{ind}    size_t __g = {name}__lookup[{(makeTupleAt "__ki")}];"""
              $"{ind}    {name}__perm[{name}__offsets[__g] + {name}__fill[__g]++] = __ki;"
              $"{ind}}}"
              $"{ind}size_t {name}__nsrc = {outerExtent}; // source rows (>= offsets[ngroups]; negative components drop)"
              $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
              $"{ind}auto {name}__at = [&](size_t __p) {{ return {name}__perm[__p]; }};"
              $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets, {name}__perm (compound)" ]
        ]
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



and genComputeBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (inner: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Compute unwraps - handle the inner expression
    // Recursive resolver: peels IRFunctorMap wrappers, resolves IRVar through deferred,
    // and handles IRComposeMeth by extracting right's kernel as a functor wrapper.
    // Returns (innerExpr, wrapperFunctions) where wrappers are innermost-first
    let rec resolveComputation (expr: IRExpr) (wrappers: IRExpr list) : IRExpr * IRExpr list =
        match expr with
        | IRVar (id, _) ->
            match Map.tryFind id ctx.DeferredComputations with
            | Some deferred -> resolveComputation deferred wrappers
            | None -> (expr, wrappers)
        | IRCompute inner ->
            // An IRCompute wrap is added at lambda lift time around
            // bodies whose top form is IRApplyCombinator. The wrap is
            // semantically the identity at compute time (force evaluation,
            // which a computation already is). Peel it through here so the
            // downstream dispatch sees the unwrapped form. Without this,
            // a double-wrapped IRCompute(IRCompute(IRApplyCombinator)) --
            // produced when the bind expansion explicitly wraps an
            // already-wrapped continuation body -- would fall through to
            // the generic case below and fail to materialize.
            resolveComputation inner wrappers
        | IRFunctorMap (f, inner) ->
            resolveComputation inner (f :: wrappers)
        | IRGuard (cond, body) ->
            // Resolve through guard: push wrappers into the body
            let (innerResolved, innerWrappers) = resolveComputation body wrappers
            (IRGuard (cond, innerResolved), innerWrappers)
        | IRComposeMeth (left, right) ->
            // @>> : c1 @>> c2 means "at each index, apply c2's kernel to c1's result"
            // The kernel resolves to a callable via the
            // CallablesTable; the returned kernel expression flows
            // into the wrappers list and gets substituted by
            // betaReduce via the same resolution.
            let rec extractInlinableKernel e =
                match e with
                | IRVar (id, _) ->
                    match Map.tryFind id ctx.DeferredComputations with
                    | Some d -> extractInlinableKernel d
                    | None -> None
                | IRApplyCombinator info ->
                    match resolveCallable info.Kernel with
                    | Some _ -> Some info.Kernel
                    | None -> None
                | IRFunctorMap (f, inner) ->
                    match extractInlinableKernel inner with
                    | Some k -> Some (IRCompose (k, f))
                    | None -> None
                | _ -> None
            match extractInlinableKernel right with
            | Some kernel -> resolveComputation left (kernel :: wrappers)
            | None -> (expr, wrappers)  // fallback: leave for IRComposeMeth handler
        | _ -> (expr, wrappers)
    
    let (resolved, functorWrappers) = resolveComputation inner []
    
    // Compose functor wrappers into ApplyInfo kernel if present
    // f <$> (L <@> g) -> L <@> (f . g)
    // Wraps kernel body: lambdaparams -> f(g(params))
    let applyFunctorWrappers (info: ApplyInfo) (wrappers: IRExpr list) : ApplyInfo =
        if wrappers.IsEmpty then info
        else
            // Beta-reduce: substitute wrapper's parameter with inner body
            // f <$> (L <@> g) where f = lambdax -> h(x)
            // becomes L <@> lambdaparams -> h(g(params))
            let betaReduce (wrapper: IRExpr) (body: IRExpr) : IRExpr =
                // Resolve through the CallablesTable. When the
                // wrapper resolves to a single-param callable,
                // substitute the param VarId with `body`. When it
                // doesn't resolve (or arity mismatch), fall back
                // to IRApp form (still correct, just not inlined).
                match resolveCallable wrapper with
                | Some c when c.Params.Length = 1 ->
                    // Substitute param VarId with body expression
                    let paramId = c.Params.[0].VarId
                    let rec subst (expr: IRExpr) =
                        match expr with
                        | IRVar (id, _) when id = paramId -> body
                        | IRVar _ | IRLit _ | IRParam _ -> expr
                        | IRBinOp (m, op, l, r) -> IRBinOp (m, op, subst l, subst r)
                        | IRUnaryOp (op, e) -> IRUnaryOp (op, subst e)
                        | IRIf (c, t, e) -> IRIf (subst c, subst t, subst e)
                        | IRApp (f, args, rt) -> IRApp (subst f, args |> List.map subst, rt)
                        | IRIndex (a, idxs, ty) -> IRIndex (subst a, idxs |> List.map subst, ty)
                        | IRTuple es -> IRTuple (es |> List.map subst)
                        | IRComplex (re, im) -> IRComplex (subst re, subst im)
                        | IRFma (a, b, c) -> IRFma (subst a, subst b, subst c)
                        | IRTupleProj (e, i, flat) -> IRTupleProj (subst e, i, flat)
                        | IRFieldAccess (e, f) -> IRFieldAccess (subst e, f)
                        | IRLet (id, v, b) -> IRLet (id, subst v, subst b)
                        | _ -> expr  // For complex nodes, leave as-is
                    subst c.Body
                | _ ->
                    // Can't beta-reduce (couldn't resolve, or arity
                    // mismatch), fall back to IRApp (IIFE in C++).
                    let retTy =
                        match resolveCallable wrapper with
                        | Some c -> c.RetType
                        | None -> IRTScalar ETFloat64
                    IRApp (wrapper, [body], retTy)
            
            let wrappedKernel =
                // Synthetic-registry construction. Each
                // per-use-site wrap produces a fresh IRCallable with
                // a new builder-allocated id, registers it in the
                // codegen-pass synthetic registry, and returns an
                // IRVar reference. resolveCallable queries both
                // the module's CallablesTable and the synthetic
                // registry, so downstream consumers (buildLoopNest-
                // CodeGen for inline emission, betaReduce for further
                // kernel-fold) see the wrapped body uniformly.
                // `mapKernelInner` peels any `IRReynolds` wrapper,
                // applies the transform to the inner callable, and
                // re-wraps with Reynolds (preserving isAntisymmetric)
                // if it was present.
                let wrapBody (body: IRExpr) =
                    wrappers |> List.fold (fun b w -> betaReduce w b) body
                let buildInline (c: IRCallable) : IRExpr =
                    let synthetic =
                        { c with Id = builder.FreshId()
                                 Body = wrapBody c.Body }
                    registerSyntheticCallable synthetic
                mapKernelInner buildInline info.Kernel
            // Update output type from outermost wrapper's return
            // type, resolving the wrapper through resolveCallable.
            // info.OutputType might otherwise be stale relative to
            // the wrapped kernel, mis-typing downstream sites (the
            // element-type adjustment below, output allocation).
            let newOutputType =
                match wrappers |> List.tryHead with
                | Some w ->
                    match resolveCallable w with
                    | Some c -> c.RetType
                    | None -> info.OutputType
                | None -> info.OutputType
            // If output is an array, update the element type
            let adjustedOutputType =
                match info.OutputType, newOutputType with
                | ArrayElem arr, IRTScalar et -> mkArrayLike { arr with ElemType = IRTScalar et }
                | _ -> newOutputType
            { info with Kernel = wrappedKernel; OutputType = adjustedOutputType }
    
    match resolved with
    | IRApplyCombinator info ->
        // info.Loop is never a composed-object chain here -- those route
        // through the IRComposeApply arm below.
        let info' = applyFunctorWrappers info functorWrappers
        // Force any DEFERRED method_for inputs first: `let llsq = ll^2` over
        // a deliberately-deferred `ll` (a bare kernel binding with no
        // |> compute) reads ll by NAME inside the emitted nest, so the
        // producer must be materialized (under its own name, dropped from
        // the deferred map) before this nest renders -- exactly what the
        // rearrangement combinators do for their inputs. Recursion through
        // genBinding handles chains: forcing k2i forces llsq/kksq first.
        let (forceCode, ctx) =
            info'.Arrays
            |> List.fold (fun (accCode, accCtx: CodeGenContext) arr ->
                match arr with
                | IRVar (id, _) when Map.containsKey id accCtx.DeferredComputations ->
                    let (fcode, fctx, _) = forceDeferredArrayInput accCtx builder ($"{name}__in{id}") arr
                    (accCode @ fcode, fctx)
                | _ -> (accCode, accCtx)) ([], ctx)
        // Same rule one slot over: a deferred binding the KERNEL closes over
        // is forwarded BY NAME as a capture argument at every call site, so it
        // must be materialized before the nest renders too (see
        // collectDeferredKernelCaptures). Runs after the input forcing so a
        // binding that is both an input and a capture is materialized once.
        let (capForceCode, ctx) =
            collectDeferredKernelCaptures ctx info'.Kernel
            |> forceDeferredBindingIds ctx builder ($"{name}__cap")
        let code = genApplyCombinator ctx name info' builder
        let ctx' = addVarName binding.Id name ctx
        (forceCode @ capForceCode @ code, ctx')

    | IRComposeApply info ->
        // Slot-inverted apply: (object_for(f) >>@ object_for(g)) <@> A.
        //
        // If functorWrappers is non-empty (the @>> case, where
        // extractInlinableKernel of the right operand surfaced its
        // kernel as a wrapper to apply to the left's result),
        // materialize the compose-apply into a temporary and then
        // emit a separate element-wise stage that applies the
        // wrapped kernel chain. For canonical IRApplyCombinator
        // this is handled by applyFunctorWrappers folding the
        // wrappers into the kernel body; for compose-apply that
        // doesn't apply (no single kernel slot), so we use the
        // stage-on-stage form instead.
        if functorWrappers.IsEmpty then
            let (code, ctx') = genComposeApply ctx name info info.OutputType builder
            let ctx'' = addVarName binding.Id name ctx'
            (code, ctx'')
        else
            let s1Name = $"{name}__wrap_s1"
            let s1Id = builder.FreshId()
            let s1Type = info.OutputType
            let (code1, ctx1) = genComposeApply ctx s1Name info s1Type builder
            let ctx1' = addVarName s1Id s1Name ctx1
            // Build an ApplyInfo whose kernel is the innermost
            // wrapper, then fold the rest on top via the existing
            // wrapper-composition machinery.
            let firstWrapper = List.head functorWrappers
            let restWrappers = List.tail functorWrappers
            let baseInfo = buildSimpleApplyInfo [IRVar(s1Id, s1Type)] firstWrapper binding.Type
            let finalInfo = applyFunctorWrappers baseInfo restWrappers
            let code2 = genApplyCombinator ctx1' name finalInfo builder
            let ctx2 = addVarName binding.Id name ctx1'
            (code1 @ [""] @ code2, ctx2)
    
    | IRComposeMeth (left, right) ->
        // @>> : sequential composition -- compute left, feed result to right's kernel
        // Stage 1: materialize left computation
        let s1Name = $"{name}__s1"
        let s1Id = builder.FreshId()
        
        // Resolve left through deferred
        let rec resolveDeferred e =
            match e with
            | IRVar (id, _) -> 
                match Map.tryFind id ctx.DeferredComputations with
                | Some d -> resolveDeferred d
                | None -> e
            | _ -> e
        let resolvedLeft = resolveDeferred left
        let resolvedRight = resolveDeferred right

        // Extract right's kernel
        let rightKernel = 
            match resolvedRight with 
            | IRApplyCombinator info -> info.Kernel 
            | _ -> resolvedRight
        let rightKernelName = 
            match rightKernel with 
            | IRVar (id, _) -> Map.tryFind id ctx.VarNames 
            | _ -> None
        
        // Materialize left as stage 1
        let s1Type = inferExprType resolvedLeft
        let s1Binding = { Id = s1Id; Name = s1Name; Type = s1Type; Value = IRCompute resolvedLeft; IsConst = true; IsMutable = false }
        let (code1, ctx1) = genBinding ctx s1Binding builder
        
        match rightKernelName with
        | Some kName ->
            // Right kernel is a named function -- generate element-wise function-call loop
            let arrRank = match s1Type with ArrayElem arr -> arrayRank arr | _ -> 1
            // s1Type comes from the upstream binding; it MUST be an
            // array at this point or the composition is malformed.
            // No `double` fallback: a non-array s1Type indicates a
            // real upstream bug worth diagnosing, not papering over.
            let (elemType, elemTypeErrCode) =
                match s1Type with
                | ArrayElem arr -> (elemTypeToCpp arr.ElemType, [])
                | t ->
                    (elemTypeToCpp (IRTScalar ETFloat64),
                     codegenError ctx ind (sprintf "method composition: left side has non-array type %A (typechecker or IR bug)" t))
            // Deterministic deallocation, site 5a: `@>>` stage 2. Dense/nullptr,
            // with extents borrowed from stage 1, so nothing is owned here. Stage 1
            // was emitted (and registered on its own path) above, so this lands
            // SECOND and is therefore freed FIRST -- which is exactly what keeps the
            // borrowed `.extents` pointer live across this free.
            (match binding.Type with
             | ArrayElem at when isFreeableDenseArrayType at ->
                 registerPoolAlloc AllocDense elemType arrRank "nullptr" (name + "_extents") name None
             | _ -> ())
            // Stage 2 sweeps stage 1's shape; stage 1's own type is what says
            // whether that shape is statically pinned.
            let s2Bound =
                match s1Type with
                | ArrayElem at -> literalOrRuntimeExtentOfArray at s1Name 0
                | _ -> $"{s1Name}.extents[0]"
            let s2Code = [
                $"{ind}const size_t* {name}_extents = {s1Name}.extents;"
                $$"""{{ind}}Array<{{elemType}}, {{arrRank}}> {{name}} = { allocate<typename promote<{{elemType}}, {{arrRank}}>::type, nullptr>({{name}}_extents), {{name}}_extents };"""
                $$"""{{ind}}for (size_t __i0 = 0; __i0 < {{s2Bound}}; __i0++) {"""
                $"{ind}    {name}[__i0] = {kName}({s1Name}[__i0]);"
                $"{ind}}}"
            ]
            let ctx2 = addVarName binding.Id name ctx1
            (code1 @ [""] @ elemTypeErrCode @ s2Code, ctx2)
        | None ->
            // Right kernel is inline lambda -- use buildSimpleApplyInfo path
            let s2Info = buildSimpleApplyInfo [IRVar(s1Id, s1Type)] rightKernel binding.Type
            let code2 = genApplyCombinator ctx1 name s2Info builder
            let ctx2 = addVarName binding.Id name ctx1
            (code1 @ [""] @ code2, ctx2)
    
    | IRBind (comp, cont) ->
        // Monadic bind: c >>= k
        // Stage 1: materialize comp
        let s1Name = $"{name}__s1"
        let s1Id = builder.FreshId()
        
        // Resolve comp through deferred
        let rec resolveDeferred e =
            match e with
            | IRVar (id, _) -> 
                match Map.tryFind id ctx.DeferredComputations with
                | Some d -> resolveDeferred d
                | None -> e
            | _ -> e
        let resolvedComp = resolveDeferred comp
        
        let s1Type = inferExprType resolvedComp
        let s1Binding = { Id = s1Id; Name = s1Name; Type = s1Type; Value = IRCompute resolvedComp; IsConst = true; IsMutable = false }
        let (code1, ctx1) = genBinding ctx s1Binding builder
        
        // Resolve continuation to its underlying callable. Post-3c.4
        // the continuation arrives as IRVar(callableId, _) for any
        // let-bound or inline continuation; resolveCallable handles
        // both the CallablesTable and the synthetic registry.
        let resolvedCont = resolveDeferred cont
        match resolveCallable resolvedCont with
        | Some lInfo when lInfo.Params.Length >= 1 ->
            // Bind lambda parameter to stage 1 result
            let param = lInfo.Params.[0]
            let ctx2 = addVarName param.VarId s1Name ctx1
            
            // Generate code for lambda body as a computation
            let bodyBinding = { Id = binding.Id; Name = name; Type = binding.Type; Value = IRCompute lInfo.Body; IsConst = true; IsMutable = false }
            let (code2, ctx3) = genBinding ctx2 bodyBinding builder
            (code1 @ [""] @ code2, ctx3)
        | _ ->
            // Fallback: continuation not resolvable to callable --
            // generate a function call against whatever cont
            // reference we have.
            let contName = match cont with IRVar (id, _) -> Map.tryFind id ctx.VarNames | _ -> None
            match contName with
            | Some kName ->
                let code = [$"{ind}auto {name} = {kName}({s1Name});"]
                let ctx' = addVarName binding.Id name ctx1
                (code1 @ [""] @ code, ctx')
            | None ->
                let code = genScalarBinding ctx1 name (IRApp(cont, [IRVar(s1Id, s1Type)], binding.Type)) binding.Type
                let ctx' = addVarName binding.Id name ctx1
                (code1 @ [""] @ code, ctx')

    | IRParallel _ ->
        // Parallel composition: recursively generate independent loops, combine as nested pairs
        let (code, _, childrenMap) = genParallelTree ctx name resolved builder
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with TupleChildren = Map.fold (fun acc k v -> Map.add k v acc) ctx'.TupleChildren childrenMap }
        (code, ctx')
    
    | IRFusion _ ->
        // Mandatory fusion: single fused loop nest with all kernels
        let (code, _, childrenMap) = genFusionTree ctx name resolved builder
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with TupleChildren = Map.fold (fun acc k v -> Map.add k v acc) ctx'.TupleChildren childrenMap }
        (code, ctx')
    
    | IRChoice (left, right) ->
        // Computation-level choice: materialize both sides, element-wise combine
        // result[i] = (lhs[i] != 0) ? lhs[i] : rhs[i]
        // If functor wrappers present: f <$> (c1 <|> c2) == (f <$> c1) <|> (f <$> c2)
        let wrapSide side =
            if functorWrappers.IsEmpty then side
            else functorWrappers |> List.fold (fun acc w -> IRFunctorMap(w, acc)) side
        let left' = wrapSide left
        let right' = wrapSide right
        let nameL = $"{name}__lhs"
        let nameR = $"{name}__rhs"
        let idL = builder.FreshId()
        let idR = builder.FreshId()
        let bindingL = { Id = idL; Name = nameL; Type = binding.Type; Value = IRCompute left'; IsConst = true; IsMutable = false }
        let bindingR = { Id = idR; Name = nameR; Type = binding.Type; Value = IRCompute right'; IsConst = true; IsMutable = false }
        let (codeL, ctxL) = genBinding ctx bindingL builder
        let (codeR, ctxR) = genBinding ctxL bindingR builder
        
        let ind = indentStr ctx
        let rank = match binding.Type with ArrayElem arr -> arrayRank arr | _ -> 0
        // Choice `<|>` legitimately handles both array and scalar
        // bindings (rank=0 => scalar). Both cases get their elem type
        // from the binding's resolved type. A type that's neither
        // ArrayElem nor IRTScalar at this point is an upstream
        // typechecker bug.
        let (elemType, elemTypeErrCode) =
            match binding.Type with
            | ArrayElem arr -> (elemTypeToCpp arr.ElemType, [])
            | IRTScalar et -> (primTypeToCpp et, [])
            | t ->
                (elemTypeToCpp (IRTScalar ETFloat64),
                 codegenError ctx ind (sprintf "<|>: binding type is neither array nor scalar (got %A) -- likely a typechecker or IR bug" t))
        
        if rank = 0 then
            // Scalar choice
            let code = [$"{ind}{elemType} {name} = ({nameL} != 0) ? {nameL} : {nameR};"]
            let ctx' = addVarName binding.Id name ctxR
            (codeL @ [""] @ codeR @ [""] @ elemTypeErrCode @ code, ctx')
        else
            // Array choice: allocate result, element-wise combine.
            // Read the source's shape via the wrapper's .extents
            // member; populate name_extents alias for the allocate<>
            // template (which still takes a const size_t*).
            // Array choice: allocate result, element-wise combine. The
            // choice of two same-shaped arrays is rectangular at this layer;
            // pass nullptr for SYMM (a symmetric <|> would need the operand's
            // hoisted symm name, which isn't threaded here -- out of scope and
            // not currently produced). This avoids referencing a nonexistent
            // function-local `nameL_symm` after the symm-hoist refactor.
            let extentsAlias = $"{ind}const size_t* {name}_extents = {nameL}.extents;"
            let allocDecl = $"{ind}Array<{elemType}, {rank}> {name} = {{ allocate<typename promote<{elemType}, {rank}>::type, nullptr>({name}_extents), {name}_extents }};"
            // Deterministic deallocation, site 5b: `<|>` array result. rank > 0
            // here (the scalar arm returned above). The extents alias borrows
            // `<name>__lhs.extents`, so nothing is owned; the operands registered
            // first (genBinding above), so reverse order frees this result before
            // the lhs whose pointer it holds.
            (match binding.Type with
             | ArrayElem at when isFreeableDenseArrayType at ->
                 registerPoolAlloc AllocDense elemType rank "nullptr" (name + "_extents") name None
             | _ -> ())

            // Generate nested loops for element-wise choice
            let mutable loopLines = []
            let mutable depth = ctx.Indent
            let indD d = String.replicate d "    "
            for i in 0 .. rank - 1 do
                // The result's own declared type carries the pinned extents;
                // `name` is the just-allocated output, so its records are the
                // binding's.
                let bound =
                    match binding.Type with
                    | ArrayElem at -> literalOrRuntimeExtentOfArray at name i
                    | _ -> $"{name}.extents[{i}]"
                loopLines <- loopLines @ [$$"""{{(indD depth)}}for (size_t __i{{i}} = 0; __i{{i}} < {{bound}}; __i{{i}}++) {"""]
                depth <- depth + 1
            
            let idxStr = [for i in 0 .. rank - 1 -> $"[__i{i}]"] |> String.concat ""
            let lhsElem = $"{nameL}{idxStr}"
            let rhsElem = $"{nameR}{idxStr}"
            loopLines <- loopLines @ [$"{(indD depth)}{name}{idxStr} = ({lhsElem} != 0) ? {lhsElem} : {rhsElem};"]
            
            for _ in 0 .. rank - 1 do
                depth <- depth - 1
                loopLines <- loopLines @ [$"{(indD depth)}}}"]
            
            let ctx' = addVarName binding.Id name ctxR
            (codeL @ [""] @ codeR @ [""] @ elemTypeErrCode @ [extentsAlias; allocDecl; ""] @ loopLines, ctx')

    | IRFallback (left, right) ->
        // <|:> allocated-fallback materialization (storage-keyed, unlike the
        // value-keyed IRChoice arm above) -- see genFallbackMaterialize.
        // Functor wrappers are not distributed over storage fallback (f <$>
        // (A <|:> B) would need f mapped over both operands' storage): reject.
        if not functorWrappers.IsEmpty then
            let code = codegenError ctx (indentStr ctx) "<$> over a <|:> fallback is not supported; materialize the fallback with |> compute first, then map"
            (code, addVarName binding.Id (bindingCppName binding) ctx)
        else
            genFallbackMaterialize ctx binding builder left right

    | IRGuard (cond, body) ->
        // guard(p, c) |> compute: conditionally execute computation
        // Strategy: wrap the kernel body with the guard condition
        // guard(cond, L <@> f) -> L <@> (lambdaargs -> cond ? f(args) : 0)
        // This allocates the array always but fills with zeros when false
        let isComputation =
            match body with
            | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRChoice _ | IRFallback _ -> true
            | IRVar (id, _) -> Map.containsKey id ctx.DeferredComputations
            | _ -> false
        if isComputation then
            // Resolve the inner computation
            let resolvedBody =
                match body with
                | IRVar (id, _) -> Map.tryFind id ctx.DeferredComputations |> Option.defaultValue body
                | _ -> body
            match resolvedBody with
            | IRApplyCombinator info ->
                // Wrap kernel: lambdaparams -> cond ? kernel_body : 0
                //
                // Resolves the kernel through resolveCallable and
                // routes through the synthetic registry: a fresh
                // callable with a new builder-allocated id holds
                // the conditional-wrapped body, gets registered,
                // and is referenced via IRVar. The original
                // callable in module.Functions is unchanged -- the
                // guard wrap is per-use-site.
                let zeroForReturnType (retTy: IRType) =
                    match retTy with
                    | IRTScalar ETBool -> IRLit (IRLitBool false)
                    | IRTScalar ETInt64 | IRTScalar ETInt32 -> IRLit (IRLitInt 0L)
                    | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> IRLit (IRLitInt 0L)
                    | IRTScalar ETFloat32 -> IRLit (IRLitFloat32 0.0f)
                    | _ -> IRLit (IRLitFloat 0.0)
                let buildGuarded (c: IRCallable) : IRExpr =
                    let zeroVal = zeroForReturnType c.RetType
                    let synthetic =
                        { c with Id = builder.FreshId()
                                 Body = IRIf (cond, c.Body, zeroVal) }
                    registerSyntheticCallable synthetic
                // `mapKernelInner` peels any `IRReynolds` wrapper,
                // applies `buildGuarded` to the inner callable, and
                // re-wraps with Reynolds (preserving isAntisymmetric)
                // if it was present. Before this consolidation the
                // peel was open-coded as `resolveCallable info.Kernel`
                // which returns None on Reynolds-wrapped kernels,
                // silently dropping the guard predicate.
                let wrappedKernel = mapKernelInner buildGuarded info.Kernel
                let guardedInfo = { info with Kernel = wrappedKernel }
                // Apply any functor wrappers
                let finalInfo = applyFunctorWrappers guardedInfo functorWrappers
                let code = genApplyCombinator ctx name finalInfo builder
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
            | _ ->
                // Non-apply computation (parallel, fusion, etc.) -- fall back to scalar guard
                let guardExpr = IRGuard (cond, body)
                let code = genScalarBinding ctx name guardExpr binding.Type
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
        else
            // guard over a NON-computation body. An ARRAY-typed body (a CONCRETE
            // array) gets an element-wise array guard `name[idx] = cond ? body[idx]
            // : 0` -- the predicate is a scalar here (no kernel params), so it holds
            // for every cell; this mirrors the interpreter's guard-over-concrete
            // (cond ? A : zeros). A scalar body keeps the exprToCpp ternary (the
            // historical `(cond ? A : 0.0)` emission was a type error over arrays).
            match binding.Type with
            | ArrayElem arr ->
                let bodyName = $"{name}__gbody"
                let bodyId = builder.FreshId()
                let bodyBinding = { Id = bodyId; Name = bodyName; Type = binding.Type; Value = IRCompute body; IsConst = true; IsMutable = false }
                let (codeB, ctxB) = genBinding ctx bodyBinding builder
                // Materialize the scalar predicate once into its own temp.
                let condName = $"{name}__gcond"
                let condId = builder.FreshId()
                let condBinding = { Id = condId; Name = condName; Type = inferExprType cond; Value = cond; IsConst = true; IsMutable = false }
                let (codeC, ctxC) = genBinding ctxB condBinding builder
                let elemType = elemTypeToCpp arr.ElemType
                let rank = arrayRank arr
                let zeroStr =
                    match arr.ElemType with
                    | IRTScalar ETBool -> "false"
                    | IRTScalar (ETInt64 | ETInt32) -> "0L"
                    | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> "0L"
                    | _ -> "0.0"
                let extentsAlias = $"{ind}const size_t* {name}_extents = {bodyName}.extents;"
                let allocDecl = $$"""{{ind}}Array<{{elemType}}, {{rank}}> {{name}} = { allocate<typename promote<{{elemType}}, {{rank}}>::type, nullptr>({{name}}_extents), {{name}}_extents };"""
                // Deterministic deallocation, site 5c: `guard` over a CONCRETE
                // array. Extents borrowed from `<name>__gbody`, which registered
                // above via genBinding, so reverse order frees this result first.
                if isFreeableDenseArrayType arr then
                    registerPoolAlloc AllocDense elemType rank "nullptr" (name + "_extents") name None
                let mutable loopLines = []
                let mutable depth = ctx.Indent
                let indD d = String.replicate d "    "
                for i in 0 .. rank - 1 do
                    // `arr` is the guarded array's type; the result borrows its
                    // shape wholesale (aliased extents, same rank).
                    let bound = literalOrRuntimeExtentOfArray arr name i
                    loopLines <- loopLines @ [$$"""{{(indD depth)}}for (size_t __i{{i}} = 0; __i{{i}} < {{bound}}; __i{{i}}++) {"""]
                    depth <- depth + 1
                let idxStr = [for i in 0 .. rank - 1 -> $"[__i{i}]"] |> String.concat ""
                loopLines <- loopLines @ [$"{(indD depth)}{name}{idxStr} = ({condName}) ? {bodyName}{idxStr} : {zeroStr};"]
                for _ in 0 .. rank - 1 do
                    depth <- depth - 1
                    loopLines <- loopLines @ [$"{(indD depth)}}}"]
                let ctx' = addVarName binding.Id name ctxC
                (codeB @ codeC @ [""] @ [extentsAlias; allocDecl; ""] @ loopLines, ctx')
            | _ ->
                let guardExpr = IRGuard (cond, body)
                let code = genScalarBinding ctx name guardExpr binding.Type
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')

    | IRSequence elems ->
        // Homogeneous n-ary parallel: each child produces same type
        // Result is array indexed by Idx<N> containing the child results
        // IMPORTANT: each child generates against the original ctx, not accumulated,
        // to prevent one child's output from contaminating another's array resolution.
        let n = elems.Length
        let childNames = elems |> List.mapi (fun i _ -> $"{name}_{i}")
        // The type each child BINDING is emitted with. Shared with the copy nest
        // below, which needs the type of the array that actually exists under
        // `<name>_i` to bake its loop bounds -- deriving it a second time from
        // `elems` would let the nest and the emission disagree.
        let childTypeOf (elem: IRExpr) =
            let wrappedElem =
                if functorWrappers.IsEmpty then elem
                else functorWrappers |> List.fold (fun acc w -> IRFunctorMap(w, acc)) elem
            let ty =
                match wrappedElem with
                | IRApplyCombinator info -> info.OutputType
                | IRComposeApply info -> info.OutputType
                | _ -> inferExprType wrappedElem
            (wrappedElem, ty)
        let childTypes = elems |> List.map (childTypeOf >> snd)
        let (allCode, mergedVarNames) =
            (elems, childNames) ||> List.map2 (fun elem childName ->
                let (wrappedElem, childType) = childTypeOf elem
                let childBinding = { Id = builder.FreshId(); Name = childName; Type = childType; Value = IRCompute wrappedElem; IsConst = true; IsMutable = false }
                genBinding ctx childBinding builder)
            |> List.fold (fun (accCode, accNames) (code, newCtx) ->
                (accCode @ code @ [""], Map.fold (fun a k v -> Map.add k v a) accNames newCtx.VarNames)
            ) ([], ctx.VarNames)
        // Determine child element type and rank
        let childType = inferExprType (List.head elems)
        let (childElemType, childRank, childTypeErrCode) =
            match childType with
            | ArrayElem arr -> (elemTypeToCpp arr.ElemType, arrayRank arr, [])
            | IRTScalar et -> (primTypeToCpp et, 0, [])
            | t ->
                (elemTypeToCpp (IRTScalar ETFloat64), 0,
                 codegenError ctx ind (sprintf "IRSequence: child has non-array, non-scalar type %A (likely a typechecker or IR bug)" t))
        let outerRank = childRank + 1
        // Build extents array: [N, child_extents...].
        //
        // Return-extent ABI (commit 7905b36). This was a frame-local
        // `size_t <name>_extents[R]`, which is fine for a binding consumed in
        // the frame that built it -- the only way an IRSequence could be
        // reached until the unified statement-shaped return arm let one be
        // RETURNED. `Array<T,R>` stores only a POINTER to its extents, so the
        // returned wrapper's shape dangled the instant the frame died, and the
        // auto-printer's first `.extents[0]` read segfaulted. `emitExtentsTable`
        // is the same helper the materialize*Form builders were moved onto:
        // `static constexpr` when every dim is literal (the SCALAR-child case,
        // where the sole extent is the child count) and `new size_t[R]`
        // otherwise -- both outlive the frame.
        //
        // Neither the heap table nor the pool below is registered for freeing.
        // The original reason was that the cells ALIASED the children's storage,
        // so a free here would race the children's own frees -- and, for a
        // sequence RETURNED from a frame, the escape analysis had to spare the
        // children's pools too or it would delete the rows out from under the
        // value that just escaped (corpus sequence-combinators/006). The copy
        // nest below retires that argument: the pool is exclusively this
        // binding's and the children are dead the moment they are copied.
        //
        // Left unregistered anyway, because that is a LEAK and this commit is a
        // correctness fix -- registering is what frees too early, and 006 is the
        // test that would crash rather than merely grow. The follow-up is to
        // report `MatPool (name, childElemType, outerRank, "nullptr", None,
        // ownedExtents)` (what materializeStackForm reports) AND to stop sparing
        // the children, which must land together: sparing them while freeing the
        // outer pool leaks strictly more than today.
        let extentsName = name + "_extents"
        let extentsDims =
            (string n, true)
            :: [for d in 0 .. childRank - 1 -> ($"{(List.head childNames)}.extents[{d}]", false)]
        let (extentsDeclLines, _ownedExtents) = emitExtentsTable ind extentsName outerRank extentsDims
        // ARRAY children: one dense pool of this binding's OWN plus a per-child
        // copy nest -- the same fresh-pool discipline materializeStackForm uses,
        // and for the same two reasons it was moved onto it.
        //
        // The pointer-aliasing assembly this replaces (`out[k] = <name>_k`, one
        // `new T*[n]` skeleton whose cells point into the CHILDREN's pools) is
        // not one contiguous pool, so it violated the invariant `pool_base`
        // documents and every consumer that reads an array through it walked off
        // the end of child 0's pool into uninitialized memory. The reachable
        // case was the assignable-let deep copy (`let b = <a sequence>`, which
        // includes the `__exprN` desugar of a bare top-level expression) and the
        // negate/conjugate transform; BLAS, CUDA and MPI streaming read the same
        // way. Copying also makes the combinator a VALUE: `let` bindings are
        // assignable in Blade, so an aliased sequence would see later writes to
        // any child.
        //
        // It was also the reason `sequence`/`replicate` were rank-1-only -- the
        // skeleton was emitted with exactly one `*` whatever the child rank, so
        // a rank-2 child failed to compile as C++ (`new double*[2]` for an
        // `Array<double,3>`). The nest below carries any child rank.
        //
        // SCALAR children keep their value array: `new T[n]` filled by
        // assignment is already one contiguous pool, which is the invariant.
        let allocDecl =
            if childRank > 0 then
                arrayAlloc { Ind = ind; Elem = childElemType; Rank = outerRank; Name = name
                             Symm = "nullptr"; Strict = None; Extents = extentsName }
            else
                $$"""{{ind}}Array<{{childElemType}}, 1> {{name}} = { new {{childElemType}}[{{n}}], {{name}}_extents };"""
        let assignLines =
            if childRank = 0 then
                childNames |> List.mapi (fun i cn ->
                    $"{ind}{name}[{i}] = {cn};")
            else
                // One nest per child. Loop variables are declared in each `for`
                // init, so sibling nests at the same level reuse the names.
                // Each nest reads its OWN child's extents (TypeCheck has proven
                // the children share a shape at runtime, but not that shape
                // monomorphization pinned them all to literals), so a child that
                // knows its own trip count bakes it whatever its siblings got.
                let loopVar d = $"__sq{name}_{d}"
                (childNames, childTypes)
                ||> List.mapi2 (fun k cn cty ->
                    let boundAt d =
                        match cty with
                        | ArrayElem st -> literalOrRuntimeExtentOfArray st cn d
                        | _ -> $"{cn}.extents[{d}]"
                    let opens =
                        [ for d in 0 .. childRank - 1 ->
                            $"""{ind}{(String.replicate d "    ")}for (size_t {(loopVar d)} = 0; {(loopVar d)} < {(boundAt d)}; {(loopVar d)}++) {{""" ]
                    let sub =
                        [ for d in 0 .. childRank - 1 -> $"[{(loopVar d)}]" ] |> String.concat ""
                    let body =
                        [ $"""{ind}{(String.replicate childRank "    ")}{name}[{k}]{sub} = {cn}{sub};""" ]
                    let closes =
                        [ for d in childRank - 1 .. -1 .. 0 -> $"""{ind}{(String.replicate d "    ")}}}""" ]
                    opens @ body @ closes)
                |> List.concat
        let ctx' = { ctx with VarNames = Map.add binding.Id name mergedVarNames }
        (allCode @ childTypeErrCode @ extentsDeclLines @ [allocDecl] @ assignLines, ctx')
    
    | _ when not functorWrappers.IsEmpty && (match inferExprType resolved with ArrayElem _ -> true | _ -> false) ->
        // `f <$> A` where A is a CONCRETE array (not a computation): materialize as
        // `method_for(A) <@> f` so the functor wrappers apply ELEMENTWISE instead of
        // being dropped (the historical `Array<T,N> name = A;` bug -- the wrappers
        // never reached the kernel). Same treatment `<@>` already gets, and the
        // value-space twin of the interpreter's applyWrappersToValue.
        let firstWrapper = List.head functorWrappers
        let restWrappers = List.tail functorWrappers
        let baseInfo = buildSimpleApplyInfo [resolved] firstWrapper binding.Type
        let finalInfo = applyFunctorWrappers baseInfo restWrappers
        let code = genApplyCombinator ctx name finalInfo builder
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

    | _ ->
        // Other compute expressions - treat as scalar
        let code = genScalarBinding ctx name resolved binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



and genProviderReadBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Deferred provider read materialized at the `|> alias.read` force point
    // (approach (b)): emit the provider's reader producing `name`, dispatched
    // through the registry on the spec's provider name. A compound
    // (load_compound) read carries a mask; a plain dense read does not.
    let spec = ctx.ProviderReads.[binding.Id]
    let pspec =
        match Blade.ProviderRegistry.tryFind spec.Provider with
        | Some p -> p
        | None -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' is not registered -- was ProviderStatics.install () run?")))
    if spec.Streamed then
        // Streamed read: emit only the provider's hoisted stream prologue
        // (open handles, fiber extents vector). Consuming nests inline the
        // per-fiber reads via ctx.StreamedArrays; nothing named `name`
        // exists as an array, so any non-nest consumer fails to compile --
        // and the eligible-shape checks in the nest fail loudly first.
        // A RANK-1 stream has no fiber to read by site: it streams per
        // SEGMENT under a structural grouping (docs/plans/structural/07
        // §3.4), through the provider's rows prologue.
        let rank1 = spec.VarType.IndexTypes.Length = 1
        let opener =
            if rank1 then
                match pspec.GenStreamRowsOpen with
                | Some g -> Some g
                | None -> None
            else pspec.GenStreamOpen
        match opener with
        | None ->
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan (
                if rank1 then $"provider '{spec.Provider}' does not support per-segment streamed reads of a rank-1 variable ('{spec.VarName}' -- bind with .read)"
                else $"provider '{spec.Provider}' does not support streamed reads (variable '{spec.VarName}' -- bind with .read)")))
        | Some gen ->
            let code = gen spec.FilePath spec.VarName name spec.VarType
            let ctx' = addVarName binding.Id name ctx
            let ctx' = { ctx' with StreamedArrays = Map.add name spec ctx'.StreamedArrays }
            (code |> List.map (fun s -> ind + s), ctx')
    else
    // A wreath group passes the `Symmetry <> SymNone && Rank >= 2` packed test
    // (correctly -- it IS packed) but must NOT take the Array<T,N> + pool-copy
    // materialization below: a wreath array's in-memory storage is a bare flat
    // `T*` of orb_cell_count cells (IRStorage.classifyOutputStorage's AllocWreath, the
    // same shape genWreathApply allocates), not a skeleton-backed Array. It gets
    // its own arm, and a provider that does not store wreath pools
    // (ReadWreathPool = None -- csv, netcdf) still refuses here.
    match spec.VarType.IndexTypes |> List.tryFind (fun ix -> ix.Symmetry = SymWreath) with
    | Some ix when pspec.ReadWreathPool.IsNone ->
        failwith (orbitStorageUnsupported
                      ($"provider read of '{spec.VarName}' (provider '{spec.Provider}' stores no OrbIdx pools)")
                      (orbitLevelsOf ix))
    | Some ix ->
        // The store's pool IS the storage, so the whole read is: assemble the
        // chunks into `<name>_flat` (the provider's ordinary flat walk, which
        // knows nothing about wreaths -- the layout attribute already pinned
        // the class at metadata parse) and adopt that buffer as the array.
        // No copy, no unlinearize, no reorder: spec_version 2 defines exactly
        // one on-disk order and it is the one the pool is in.
        if spec.Window.IsSome then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"z.read_window over an OrbIdx (iterated-wreath) pool ('{spec.VarName}') is not supported: a wreath class has no translated sub-class to window into")))
        match classifyOutputStorage binding.Type, pspec.GenReadPacked with
        | AllocWreath (levels, n, cells), Some gen ->
            let elemCpp =
                match binding.Type with
                | ArrayElem at -> elemTypeToCpp at.ElemType
                | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.iceCodegen ($"wreath provider read '{spec.VarName}': binding is not array-typed")))
            let opts : Blade.ProviderRegistry.PackedReadOpts = { Distribute = false; Window = None }
            let assemble = gen spec.FilePath spec.VarName name spec.VarType opts
            // Same runtime pin genWreathApply emits: the C++ section 4 fold and the
            // F# one (OrbRank.cellCountChecked, which is also what validated
            // the store's pool length at metadata parse) are independent
            // implementations, and a disagreement is a wrong-sized pool that
            // nothing on the value side could notice.
            let pin =
                [ $"if (orbit_wreath_utilities::orb_cell_count<{(orbLevelArgs levels)}>({n}) != {cells}LL) {{ blade_rt::panic(\"BL8004\", \"OrbIdx pool size disagreement: orb_cell_count vs the store's declared cardinality\", nullptr, 0); }}" ]
            registerAlloc (RawAlloc (name, None))
            let adopt =
                [ $"// OrbIdx{(ppOrbitLevels levels)} over extent {n}: {cells} pool cells adopted from the store (ascending-lex canonical order)"
                  $"{elemCpp}* {name} = {name}_flat;" ]
            ((assemble @ pin @ adopt) |> List.map (fun s -> ind + s), addVarName binding.Id name ctx)
        | AllocWreath _, None ->
            failwith (orbitStorageUnsupported
                          ($"provider read of '{spec.VarName}' (provider '{spec.Provider}' emits no packed reader)")
                          (orbitLevelsOf ix))
        | spec_, _ ->
            failwith (orbitStorageUnsupported
                          (sprintf "provider read of '%s' (%A)" spec.VarName spec_) (orbitLevelsOf ix))
    | None ->
    let isPackedVar =
        spec.VarType.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone && ix.Rank >= 2)
    if isPackedVar then
        // Packed (SymIdx/AntisymIdx) read: the provider assembles the store's
        // canonical flat pool into `<name>_flat`; the materialization lives
        // HERE because the SYMM template argument must be hoisted to
        // namespace scope (hoistSymmDecl), which a provider string generator
        // cannot reach. The pool copy is linear: the allocator's flat pool
        // holds exactly the canonical cells in ascending-lex order (the same
        // pinned order the store uses), so no per-cell unlinearize is needed.
        if spec.MaskName.IsSome then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}': load_compound over a packed variable ('{spec.VarName}') is not supported")))
        match pspec.GenReadPacked with
        | None ->
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' does not support packed (symmetric/antisymmetric) variables (variable '{spec.VarName}')")))
        | Some gen ->
            (match binding.Type with
             | ArrayElem arrTy ->
                 // Distribute only whole-variable reads, and only when the
                 // program has MPI scaffolding (windows are small and local).
                 let opts : Blade.ProviderRegistry.PackedReadOpts =
                     { Distribute = mpiProgramOn () && spec.Window.IsNone
                       Window = spec.Window }
                 let assemble = gen spec.FilePath spec.VarName name spec.VarType opts
                 let elemCpp = elemTypeToCpp arrTy.ElemType
                 let componentExtents =
                     arrTy.IndexTypes |> List.collect (fun idx -> List.replicate idx.Rank idx.Extent)
                 let rank = componentExtents.Length
                 let extentTerms =
                     componentExtents |> List.map (fun e ->
                         match e with
                         | IRLit (IRLitInt n) -> string n
                         | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"packed provider read of '{spec.VarName}' requires literal extents"))))
                 let extentsName = $"{name}_extents"
                 let extentsArr = $$"""size_t {{extentsName}}[] = { {{(String.concat ", " extentTerms)}} };"""
                 let symmVec = buildSymmVec binding.Type
                 let symmArg =
                     if hasRealSymmetry symmVec then hoistSymmDecl ($"{name}_symm") symmVec
                     else "nullptr"
                 let allocLine =
                     match emitAllocRhs (classifyOutputStorage binding.Type) elemCpp rank symmArg extentsName with
                     | Ok rhs -> $"Array<{elemCpp}, {rank}> {name} = {rhs};"
                     | Error msg -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"packed provider read '{spec.VarName}': {msg}")))
                 let copy = genPackedPoolCopy arrTy name name spec.VarName false
                 ((assemble @ [extentsArr; allocLine] @ copy @ [$"delete[] {name}_flat;"])
                  |> List.map (fun s -> ind + s),
                  addVarName binding.Id name ctx)
             | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.iceCodegen ($"packed provider read '{spec.VarName}': binding is not array-typed"))))
    else
    let readCode =
        (match spec.MaskName, spec.MaskType with
         | Some maskName, Some maskType ->
             (match pspec.GenReadCompoundVar with
              | Some gen -> gen spec.FilePath spec.VarName maskName name spec.VarType maskType
              | None -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' does not support load_compound (variable '{spec.VarName}')"))))
         | _ ->
             pspec.GenReadVar spec.FilePath spec.VarName name spec.VarType)
    (readCode |> List.map (fun s -> ind + s), addVarName binding.Id name ctx)

and genProviderWriteBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    // Deferred provider write (`alias.write("path", A)`): flatten the source
    // array into `<base>_flat` (row-major Horner, the inverse of genReadVar's
    // materialization copy), hand the provider's writer that buffer, then
    // release it. `base` is the write binding's own cpp name suffixed over
    // the source's, so two writes of one array cannot collide.
    let spec = ctx.ProviderWrites.[binding.Id]
    let pspec =
        match Blade.ProviderRegistry.tryFind spec.Provider with
        | Some p -> p
        | None -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' is not registered -- was ProviderStatics.install () run?")))
    let srcCpp =
        match Map.tryFind spec.SourceId ctx.VarNames with
        | Some n -> n
        | None -> sanitizeCppName spec.VarName
    let baseName = $"{srcCpp}_wr{int binding.Id}"
    let arrTy = spec.SourceType
    let elemCpp = elemTypeToCpp arrTy.ElemType
    // The wreath arm runs BEFORE the component-extent fold below: a wreath
    // record's Extent is the IROrbitClass marker, not an IRLit, so the literal
    // check would fire first and report "requires literal extents" for a class
    // whose extent is perfectly literal one level down.
    match arrTy.IndexTypes |> List.tryFind (fun ix -> ix.Symmetry = SymWreath) with
    | Some ix when pspec.ReadWreathPool.IsNone ->
        failwith (orbitStorageUnsupported
                      ($"provider write of '{spec.VarName}' (provider '{spec.Provider}' stores no OrbIdx pools)")
                      (orbitLevelsOf ix))
    | Some ix ->
        // The source array IS the flat pool in ascending-lex canonical order,
        // so the "flatten" is a straight element copy into a buffer the writer
        // owns -- the depth-1 packed path's linear pool_base copy, minus the
        // skeleton indirection (a wreath array has no skeleton). The copy is not
        // elided: the writer's cleanup deletes the buffer it was handed, and
        // that must not be the array.
        (match classifyOutputStorage (mkArrayLike arrTy) with
         | AllocWreath (levels, n, cells) ->
             let pin =
                 $"if (orbit_wreath_utilities::orb_cell_count<{(orbLevelArgs levels)}>({n}) != {cells}LL) {{ blade_rt::panic(\"BL8004\", \"OrbIdx pool size disagreement: orb_cell_count vs the compiler's iterated-binomial fold\", nullptr, 0); }}"
             let flatten =
                 [ $"// Write {spec.VarName} (OrbIdx{(ppOrbitLevels levels)} pool, {cells} cells) to {spec.FilePath}"
                   pin
                   $"{elemCpp}* {baseName}_flat = new {elemCpp}[{cells}];"
                   $$"""for (size_t __ow_i = 0; __ow_i < {{cells}}; __ow_i++) { {{baseName}}_flat[__ow_i] = {{srcCpp}}[__ow_i]; }""" ]
             let writeCode = pspec.GenWriteVar spec.FilePath spec.VarName baseName arrTy spec.DimNames
             let cleanup = [ $"delete[] {baseName}_flat;" ]
             (guardProviderWrite ind (flatten @ writeCode @ cleanup), ctx)
         | other ->
             failwith (orbitStorageUnsupported
                           (sprintf "provider write of '%s' (%A)" spec.VarName other) (orbitLevelsOf ix)))
    | None ->
    let componentExtents =
        arrTy.IndexTypes |> List.collect (fun idx -> List.replicate idx.Rank idx.Extent)
    let rank = componentExtents.Length
    let extentTerms =
        componentExtents |> List.map (fun e ->
            match e with
            | IRLit (IRLitInt n) -> string n
            | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider write of '{spec.VarName}' requires literal extents"))))
    let isPacked =
        arrTy.IndexTypes |> List.exists (fun idx -> idx.Symmetry <> SymNone && idx.Rank >= 2)
    if isPacked then
        // Packed (SymIdx/AntisymIdx) source: the flatten is a LINEAR pool
        // copy -- the allocator's flat pool holds exactly the canonical
        // cells in ascending-lex order, which is the store's pool order.
        // GenReadPacked presence is the provider's packed-layout capability
        // flag (read and write go together: both are pool-order I/O).
        if pspec.GenReadPacked.IsNone then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' does not support packed (symmetric/antisymmetric) writes (variable '{spec.VarName}')")))
        let poolCount =
            deviceBufferCardinality (deviceBufferTypeOfArray arrTy) |> exprToCpp ctx.VarNames
        let flatten =
            [ $"// Write {spec.VarName} (packed pool) to {spec.FilePath}"
              $"{elemCpp}* {baseName}_flat = new {elemCpp}[{poolCount}];" ]
            @ genPackedPoolCopy arrTy srcCpp baseName spec.VarName true
        let writeCode = pspec.GenWriteVar spec.FilePath spec.VarName baseName arrTy spec.DimNames
        let cleanup = [ $"delete[] {baseName}_flat;" ]
        (guardProviderWrite ind (flatten @ writeCode @ cleanup), ctx)
    else
    // DESTINATION PASSING, gate 1 (plan-fortran-killer-2.md section 4): a
    // plain dense source already IS the flat buffer the writer wants.
    // allocate<> places every scalar of a nested Array in ONE contiguous pool
    // in DFS order -- row-major, which is exactly the Horner index the copy
    // loop below used to compute -- so `<base>_flat` aliases
    // pool_base(src.data): no allocation, no copy, no delete, one full-array
    // temporary fewer at the pipeline's terminal. The copy stays for every
    // storage pool_base is not defined on (compound / sparse tabulated
    // records, ragged rows, group records) and for the packed and wreath
    // arms above, whose pool order is not the store's dense order.
    let plainDense =
        rank > 0
        && arrTy.IndexTypes |> List.forall (fun ix -> ix.IxKind = IxKPlain && ix.Symmetry = SymNone)
    if plainDense then
        let alias =
            [ $"// Write {spec.VarName} to {spec.FilePath} (destination passing: the pool is the row-major buffer)"
              $"{elemCpp}* {baseName}_flat = pool_base({srcCpp}.data);" ]
        let writeCode = pspec.GenWriteVar spec.FilePath spec.VarName baseName arrTy spec.DimNames
        (guardProviderWrite ind (alias @ writeCode), ctx)
    else
    let extentNames = extentTerms |> List.mapi (fun i _ -> $"{baseName}_ext{i}")
    let extentDecls =
        List.zip extentNames extentTerms
        |> List.map (fun (n, t) -> $"size_t {n} = {t};")
    let idxVars = [ for i in 0 .. rank - 1 -> $"{baseName}_i{i}" ]
    let openLoops =
        idxVars |> List.mapi (fun d iv ->
            let indl = String.replicate d "    "
            $$"""{{indl}}for (size_t {{iv}} = 0; {{iv}} < {{extentNames.[d]}}; {{iv}}++) {""")
    let nestedSub = idxVars |> List.map (sprintf "[%s]") |> String.concat ""
    let flatIdx =
        match idxVars with
        | [] -> "0"
        | first :: _ ->
            let mutable acc = first
            for i in 1 .. rank - 1 do
                acc <- $"({acc}) * {extentNames.[i]} + {idxVars.[i]}"
            acc
    let bodyInd = String.replicate rank "    "
    let flatten =
        [ $"// Write {spec.VarName} to {spec.FilePath}" ]
        @ extentDecls
        @ [ $"""{elemCpp}* {baseName}_flat = new {elemCpp}[{(String.concat " * " extentNames)}];""" ]
        @ openLoops
        @ [ $"{bodyInd}{baseName}_flat[{flatIdx}] = {srcCpp}{nestedSub};" ]
        @ [ for d in rank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
    let writeCode = pspec.GenWriteVar spec.FilePath spec.VarName baseName arrTy spec.DimNames
    let cleanup = [ $"delete[] {baseName}_flat;" ]
    (guardProviderWrite ind (flatten @ writeCode @ cleanup), ctx)


/// rand.<fam>(key, params.., shape): allocate the dense array (self-typed from
/// the shape) and fill its flat contiguous pool with `card` deterministic draws
/// keyed by `key`, via the blade_rand runtime. All rand arrays are dense
/// SymNone, so pool_base gives the full pool and the draw count is the product
/// of extents. Mirrors the fill_random dense path but uses a flat pool fill.
///
/// The pool's C++ type is whatever `elemTypeToCpp` makes of the binding's
/// ElemType, which is how `categorical` gets an `int64_t` pool (and every other
/// family a `double` one) without a second allocation arm here: the checker
/// picked the element type, this code was already generic in it.
///
/// The family's runtime Float64 parameters follow the key as trailing
/// `(double)`-cast arguments, in surface order; a zero-parameter family
/// (uniform/normal) emits the original three-argument call unchanged, so this
/// extension is byte-compatible with the pre-existing emission for those two.
/// The array parameter channel (categorical's weights) emits in the same
/// position as a POINTER-plus-LENGTH pair, `pool_base(W.data), (size_t)k`.
and genRandGenBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let kind, keyExpr, parExprs, weightsExpr =
        match ctx.RandomInits.[binding.Id] with
        | RandGen (k, key, pars, weights) -> k, key, pars, weights
        | FillModulus _ -> "uniform", IRLit (IRLitInt 0L), [], None  // unreachable: dispatch guards this
    match binding.Type with
    | ArrayElem arrTy ->
        let elemCpp = elemTypeToCpp arrTy.ElemType
        let extents = arrTy.IndexTypes |> List.collect (fun idx -> List.replicate idx.Rank idx.Extent)
        let rank = extents.Length
        let nonLiteral = extents |> List.exists (fun e -> match e with IRLit (IRLitInt _) -> false | _ -> true)
        if nonLiteral then
            ([refusalErrorLine ind ($"rand binding '{name}' requires literal extents")], addVarName binding.Id name ctx)
        else
            let extentTerms = extents |> List.map (fun e -> match e with IRLit (IRLitInt n) -> string n | _ -> "0")
            let extentsName = $"{name}_extents"
            let extentsArr = $$"""{{ind}}size_t {{extentsName}}[] = { {{(String.concat ", " extentTerms)}} };"""
            let card = extents |> List.fold (fun acc e -> match e with IRLit (IRLitInt n) -> acc * n | _ -> acc) 1L
            let allocLine =
                $"{ind}Array<{elemCpp}, {rank}> {name} = {{ allocate<typename promote<{elemCpp}, {rank}>::type, nullptr>({extentsName}), {extentsName} }};"
            // Array parameter first (surface order puts weights before any
            // scalar par), then the scalar pars.
            let weightsArgs =
                match weightsExpr with
                | None -> ""
                | Some (wExpr, k) ->
                    $", nested_array_utilities::pool_base({(exprToCpp ctx.VarNames wExpr)}.data), (size_t){k}LL"
            let parArgs =
                parExprs
                |> List.map (fun p -> $", (double)({(exprToCpp ctx.VarNames p)})")
                |> String.concat ""
            let fillLine =
                $"{ind}blade_rand::{kind}(nested_array_utilities::pool_base({name}.data), (size_t){card}LL, (int64_t)({(exprToCpp ctx.VarNames keyExpr)}){weightsArgs}{parArgs});"
            ([extentsArr; allocLine; fillLine], addVarName binding.Id name ctx)
    | _ ->
        ([refusalErrorLine ind ($"rand binding '{name}' is not an array type")], addVarName binding.Id name ctx)

and genRandomInitBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Random-fill constructor (`let A: Array<..> = fill_random(mod)`):
    // allocate the nested Array (same form as a literal binding) and fill it
    // with rand() % mod via the runtime fill_random. Shape/elem come from the
    // binding's array type; the modulus from RandomInits. Rectangular, so
    // SYMM defaults to nullptr. fill_random deduces its type from the first
    // arg, so pass the raw nested pointer (.data), not the Array wrapper --
    // the wrapper would deduce as a scalar leaf and never recurse.
    let modExpr =
        match ctx.RandomInits.[binding.Id] with
        | FillModulus m -> m
        | RandGen _ -> IRLit (IRLitInt 1L)  // unreachable: dispatch routes RandGen to genRandGenBinding
    (match binding.Type with
     | ArrayElem arrTy ->
         let elemCpp = elemTypeToCpp arrTy.ElemType
         let allDenseRank1 =
             arrTy.IndexTypes |> List.forall (fun idx -> idx.Rank = 1 && idx.Symmetry = SymNone)
         let hasHermitian =
             arrTy.IndexTypes |> List.exists (fun idx -> idx.Symmetry = SymHermitian)
         if allDenseRank1 then
             // Dense-rectangular path: unchanged (byte-compatible with the
             // pre-arc-3 emission -- the runtime fill_random walks the shape).
             let rank = arrayRank arrTy
             let extentNames = arrTy.IndexTypes |> List.mapi (fun i _ -> $"{name}_extent_{i}")
             let extentDecls =
                 arrTy.IndexTypes |> List.mapi (fun i idx ->
                     match idx.Extent with
                     | IRLit (IRLitInt n) -> $"{ind}size_t {name}_extent_{i} = {n};"
                     | _ -> refusalErrorLine ind ($"fill_random binding '{name}' has a non-literal extent at dim {i}"))
             let extentsArr = $$"""{{ind}}size_t {{name}}_extents[] = { {{(String.concat ", " extentNames)}} };"""
             let allocLine =
                 $"{ind}Array<{elemCpp}, {rank}> {name} = {{ allocate<typename promote<{elemCpp}, {rank}>::type, nullptr>({name}_extents), {name}_extents }};"
             let fillLine =
                 $"{ind}fill_random({name}.data, {name}_extents, (int)({(exprToCpp ctx.VarNames modExpr)}));"
             (extentDecls @ [extentsArr; allocLine; fillLine], addVarName binding.Id name ctx)
         elif hasHermitian then
             // Hermitian stores the full n^2 cells but they are CONSTRAINT-
             // COUPLED (A(i,j) = conj(A(j,i))): independent pool draws would
             // violate the invariant, so hermitian fill needs a canonical-
             // half fill + mirrored conjugation -- not yet emitted.
             ([refusalErrorLine ind ($"fill_random binding '{name}': HermitianIdx is not supported (stored cells are constraint-coupled)")],
              addVarName binding.Id name ctx)
         else
             // GENERALIZED fill (arc 3, formalism 3.5): one draw per STORED
             // cell. Compact storage classes (SymIdx/AntisymIdx, mixed with
             // dense axes) allocate with their SYMM vector and fill the flat
             // pool linearly -- the pool holds exactly the canonical cells, so
             // symmetry holds by construction, antisym diagonals stay
             // implicit zeros, and the draw count is the storage cardinality
             // (deviceBufferCardinality -- same closed forms as allocation).
             let componentExtents =
                 arrTy.IndexTypes |> List.collect (fun idx -> List.replicate idx.Rank idx.Extent)
             let rank = componentExtents.Length
             let nonLiteral =
                 componentExtents |> List.exists (fun e -> match e with IRLit (IRLitInt _) -> false | _ -> true)
             if nonLiteral then
                 ([refusalErrorLine ind ($"fill_random binding '{name}' requires literal extents")],
                  addVarName binding.Id name ctx)
             else
                 let extentTerms =
                     componentExtents |> List.map (fun e ->
                         match e with IRLit (IRLitInt n) -> string n | _ -> "0")
                 let extentsName = $"{name}_extents"
                 let extentsArr = $$"""{{ind}}size_t {{extentsName}}[] = { {{(String.concat ", " extentTerms)}} };"""
                 let symmVec = buildSymmVec binding.Type
                 let symmArg =
                     if hasRealSymmetry symmVec then hoistSymmDecl ($"{name}_symm") symmVec
                     else "nullptr"
                 let allocLines =
                     match emitAllocRhs (classifyOutputStorage binding.Type) elemCpp rank symmArg extentsName with
                     | Ok rhs -> [$"{ind}Array<{elemCpp}, {rank}> {name} = {rhs};"]
                     | Error msg -> [refusalErrorLine ind ($"fill_random '{name}': {msg}")]
                 let poolCount =
                     deviceBufferCardinality (deviceBufferTypeOfArray arrTy)
                     |> exprToCpp ctx.VarNames
                 let fillLines =
                     [ $"{ind}{{ auto* __fr_pool = nested_array_utilities::pool_base({name}.data);"
                       sprintf "%s  for (size_t __fr_i = 0; __fr_i < %s; __fr_i++) { __fr_pool[__fr_i] = static_cast<%s>(rand() %% (int)(%s)); } }"
                           ind poolCount elemCpp (exprToCpp ctx.VarNames modExpr) ]
                 (extentsArr :: allocLines @ fillLines, addVarName binding.Id name ctx)
     | _ ->
         ([refusalErrorLine ind ($"fill_random binding '{name}' is not an array type")], addVarName binding.Id name ctx))


and genCompoundInitBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Compound-construction constructor (`let B = compound(dense, mask)`):
    // materialize the compound index from the bool mask (P0,
    // genCompoundIndexFromMask), then scatter the dense array's present
    // leading cells into a compact buffer and bundle a Compound<T, RANK>.
    // The mask covers a LEADING PREFIX of dense's dims (validated by
    // compoundViewType in typecheck); remaining dims fold into trailing_stride.
    //
    // dense and mask are lowered IRVar references (recorded in CompoundInits);
    // exprToCpp yields their in-scope C++ variable names. Both are Array<...>
    // wrappers, so .data is the nested pointer and pool_base flattens to the
    // contiguous row-major pool the scatter walks.
    let (denseExpr, maskExpr) = ctx.CompoundInits.[binding.Id]
    let denseName = exprToCpp ctx.VarNames denseExpr
    // The mask operand may be written INLINE inside compound(...) --
    // `compound(A, mask(A, p))` -- in which case it lowers to a bare
    // IRMask node. exprToCpp cannot render a mask inline (it needs a
    // multi-statement materialization), so it would emit the
    // BLADE_CODEGEN_ERROR sentinel as the "name". Materialize such an
    // inline mask into a Bool presence-array temp first (the same helper
    // the method_for auto-materialize path uses), then feed that temp's
    // name to the index builder. A let-bound mask
    // (`let m = mask(...); compound(A, m)`) arrives as an IRVar and skips
    // this. maskPre is prepended to the emitted lines below.
    let (maskPre, maskName) =
        match maskExpr with
        | IRMask _ ->
            let tmpName = $"{name}__masksrc"
            (match materializeInlineForm emptySubst ctx.VarNames tmpName (lazy "bool") maskExpr with
             // Deliberately NOT registered: this temp feeds the COMPOUND index
             // construction below, and compound storage / ownership is owned by
             // a separate workstream (the same reason isFreeableDenseArrayType
             // excludes compound types).
             | Some (stmts, _) -> (stmts |> List.map (fun s -> ind + s), tmpName)
             | None -> ([], exprToCpp ctx.VarNames maskExpr))
        | _ -> ([], exprToCpp ctx.VarNames maskExpr)
    (match binding.Type with
     | ArrayElem arrTy when isCompoundArrayType arrTy ->
         let leadRank =
             arrTy.IndexTypes
             |> List.tryFind (fun ix -> ix.IxKind = IxKCompound)
             |> Option.map (_.Rank)
             |> Option.defaultValue 1
         let elemCpp = elemTypeToCpp arrTy.ElemType
         // The compound array type carries leadRank (compound) + trailing
         // slots; the number of trailing dims = arrTy.IndexTypes.Length - 1.
         let trailingDimCount = arrTy.IndexTypes.Length - 1
         let idxName = $"{name}_idx"
         let (idxLines, _) = genCompoundIndexFromMask maskName leadRank idxName
         // trail = product of dense.extents[leadRank .. leadRank+trailingDimCount-1]
         let trailTerms =
             [ for d in 0 .. trailingDimCount - 1 -> $"{denseName}.extents[{leadRank + d}]" ]
         let trailExpr = match trailTerms with | [] -> "1" | xs -> String.concat " * " xs
         let lines =
             maskPre
             @ (idxLines |> List.map (fun l -> ind + l))
             @ [ $"{ind}size_t {name}_trail = {trailExpr};"
                 $"{ind}{elemCpp}* {name}_densepool = nested_array_utilities::pool_base({denseName}.data);"
                 // The `+ 1` is REQUIRED BY the branchless scatter and only by
                 // it: that form stores unconditionally and lets the cursor sit
                 // at `cardinality` after the last selected cell, so a trailing
                 // unselected cell writes one past the logical end. One element
                 // of slack, never read, is the price of deleting a branch that
                 // mispredicts once per grid cell.
                 (if trailTerms.IsEmpty then
                     $"{ind}{elemCpp}* {name}_compact = new {elemCpp}[{idxName}->cardinality * {name}_trail + 1];"
                  else
                     $"{ind}{elemCpp}* {name}_compact = new {elemCpp}[{idxName}->cardinality * {name}_trail];")
                 // scatter present leading cells (row-major prefix-popcount)
                 compactScatter { Ind = ind; Name = name; IdxName = idxName; ScalarTrail = trailTerms.IsEmpty }
                 $$"""{{ind}}nested_array_utilities::Compound<{{elemCpp}}, {{leadRank}}> {{name}} { {{name}}_compact, {{idxName}}, {{name}}_trail };""" ]
         // Owns BOTH the compact buffer and the freshly built index. (BL6002
         // restricts compound() to top-level lets today, where the empty
         // scope stack makes this a no-op -- registered anyway so a future
         // lifting of BL6002 inherits correct frees. The idx's grid/pool
         // locals alias the mask; only data + idx are owned.)
         registerShapedAlloc name "deallocate_compound" name
         (lines, addVarName binding.Id name ctx)
     | _ ->
         ([refusalErrorLine ind ($"compound() binding '{name}' is not a CompoundIdx array type")], addVarName binding.Id name ctx))

and genSparseInitBinding (ctx: CodeGenContext) (binding: IRBinding) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Sparse-construction constructor (`let S = sparse(values, keys)`):
    // materialize the sparse index from the keys source (genSparseIndexFromKeys
    // -- static literal table or runtime tuple-array loop), then copy the values
    // buffer straight into a compact pool. NO scatter: the values arrive
    // already in key order -- that is the builder's contract, and the reason
    // this is a flat pool copy where genCompoundInitBinding needs compactScatter.
    //
    // Trailing dims (values rank > 1, mirroring the compound builder's
    // leading-prefix rule): values.extents[0] is the key axis and the REMAINING
    // extents fold into trailing_stride, so each key owns a contiguous block of
    // `trail` elements. The copy walks the flattened pool (pool_base), which is
    // exactly the key-major layout the Sparse wrapper indexes.
    // A |values| != |keys| mismatch panics at runtime (BL8001).
    let valuesExpr = ctx.SparseInits.[binding.Id]
    let valuesName = exprToCpp ctx.VarNames valuesExpr
    (match binding.Type with
     | ArrayElem arrTy when isSparseArrayType arrTy ->
         let sparseIx = arrTy.IndexTypes |> List.find (fun ix -> ix.IxKind = IxKSparse)
         let leadRank = sparseIx.Rank
         let elemCpp = elemTypeToCpp arrTy.ElemType
         // Trailing dim count = slots after the sparse head (the key axis
         // consumed values dim 0, so these are values dims 1..).
         let trailingDimCount = arrTy.IndexTypes.Length - 1
         let idxName = $"{name}_idx"
         let idxLines =
             match sparseIx.Extent with
             | IRSparseKeys (SkStatic _ as src) -> genSparseIndexFromKeys src None leadRank idxName
             | IRSparseKeys (SkRuntime (IRVar (kid, _)) as src) ->
                 genSparseIndexFromKeys src (Map.tryFind kid ctx.VarNames) leadRank idxName
             | _ -> [ refusalErrorLine "" ($"sparse() binding '{name}': keys source is not a SparseIdx extent") ]
         let trailTerms =
             [ for d in 1 .. trailingDimCount -> $"{valuesName}.extents[{d}]" ]
         let trailExpr = match trailTerms with | [] -> "1" | xs -> String.concat " * " xs
         let lines =
             (idxLines |> List.map (fun l -> ind + l))
             @ [ $"{ind}if ({valuesName}.extents[0] != {idxName}->cardinality) {{ blade_rt::panic(\"BL8001\", \"sparse(values, keys): values length does not match key count\", nullptr, 0); }}"
                 $"{ind}size_t {name}_trail = {trailExpr};"
                 $"{ind}{elemCpp}* {name}_valpool = nested_array_utilities::pool_base({valuesName}.data);"
                 $"{ind}{elemCpp}* {name}_compact = new {elemCpp}[{idxName}->cardinality * {name}_trail];"
                 $"{ind}for (size_t __r = 0; __r < {idxName}->cardinality * {name}_trail; __r++) {name}_compact[__r] = {name}_valpool[__r];"
                 $$"""{{ind}}nested_array_utilities::Sparse<{{elemCpp}}, {{leadRank}}> {{name}} { {{name}}_compact, {{idxName}}, {{name}}_trail };""" ]
         // Owns BOTH the compact buffer and the freshly built index.
         registerShapedAlloc name "deallocate_sparse" name
         (lines, addVarName binding.Id name ctx)
     | _ ->
         ([refusalErrorLine ind ($"sparse() binding '{name}' is not a SparseIdx array type")], addVarName binding.Id name ctx))


/// Rearrangement combinators (group_by, sort, mask, transpose, decompact,
/// intersect, union, unique, array negate/conjugate) index their array
/// inputs by NAME in the emitted C++, so they need a MATERIALIZED input.
/// An input that is still an unforced computation -- a deferred binding
/// (only a "<deferred computation>" comment in the C++), or an inline
/// computation node -- is forced here first, exactly as |> compute would
/// force it. Returns (forceCode, ctx', expr') where expr' names the
/// materialized array; callers rebuild their form node from expr' before
/// rendering. `tmpBase` names the synthetic temporary when the input is an
/// inline computation (and is the fallback name for a deferred binding
/// missing from VarNames); pass a distinct base per input slot.
and forceDeferredArrayInput (ctx: CodeGenContext) (builder: IRBuilder) (tmpBase: string) (expr: IRExpr) : string list * CodeGenContext * IRExpr =
    match expr with
    | IRVar (srcId, ty) when Map.containsKey srcId ctx.DeferredComputations ->
        // Materialize under the deferred binding's own name, then drop it
        // from the deferred map: later consumers (including a second
        // rearrangement over the same binding) must see the materialized
        // array, not re-force it into a C++ redefinition.
        let srcName = Map.tryFind srcId ctx.VarNames |> Option.defaultValue tmpBase
        let matBinding = { Id = srcId; Name = srcName; Type = ty; Value = IRCompute (IRVar (srcId, ty)); IsConst = true; IsMutable = false }
        let (code, ctx1) = genBinding ctx matBinding builder
        let ctx1 = { ctx1 with DeferredComputations = Map.remove srcId ctx1.DeferredComputations }
        // A module-level binding materialized here at main's top level joins
        // the auto-print list (genPrintStatements consults the cell); a
        // nested-scope force (Indent > 0) stays block-scoped and unprintable.
        if ctx.Indent = 0 then
            let cell = forcedDeferredIdsCell ()
            cell.Value <- Set.add srcId cell.Value
        (code @ [""], ctx1, expr)
    | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRComposeMeth _ | IRBind _ | IRCompute _ ->
        // Inline computation as the array argument: materialize into a
        // synthetic temporary and rearrange over that.
        let tmpId = builder.FreshId()
        let ty = inferExprType expr
        let matBinding = { Id = tmpId; Name = tmpBase; Type = ty; Value = IRCompute expr; IsConst = true; IsMutable = false }
        let (code, ctx1) = genBinding ctx matBinding builder
        (code @ [""], ctx1, IRVar (tmpId, ty))
    | _ -> ([], ctx, expr)


/// Force every deferred-computation binding that `value` reads positionally
/// (see collectDeferredPositionalReads) so it is materialized in scope before
/// the by-name reads are emitted. Returns the prelude force-code plus a ctx
/// with those bindings dropped from DeferredComputations. Idempotent: a binding
/// already forced (removed from the map) is skipped by the collector, so this
/// composes cleanly with the per-input forcing the rearrangement combinators do.
and forceDeferredPositionalReads (ctx: CodeGenContext) (builder: IRBuilder) (tmpBase: string) (value: IRExpr) : string list * CodeGenContext =
    forceDeferredBindingIds ctx builder tmpBase (collectDeferredPositionalReads ctx value)

/// Materialize each deferred binding id in `ids`, threading the ctx so a
/// binding forced by an earlier id (or already forced by the caller) is
/// skipped rather than emitted twice. Shared by the positional-read pre-pass
/// and the kernel-capture forcing in genComputeBinding.
and forceDeferredBindingIds (ctx: CodeGenContext) (builder: IRBuilder) (tmpBase: string) (ids: IRId list) : string list * CodeGenContext =
    ids |> List.fold (fun (accCode, accCtx: CodeGenContext) id ->
        if not (Map.containsKey id accCtx.DeferredComputations) then (accCode, accCtx) else
        // Recover the array type from the deferred computation so the
        // materialized binding is typed correctly (forceDeferredArrayInput's
        // IRVar arm carries the IRVar's type onto the synthesized binding).
        let ty =
            match Map.tryFind id accCtx.DeferredComputations with
            | Some defExpr -> inferExprType defExpr
            | None -> IRTUnit
        let (code, ctx', _) = forceDeferredArrayInput accCtx builder ($"{tmpBase}_{id}") (IRVar (id, ty))
        (accCode @ code, ctx')
    ) ([], ctx)


and genMaskBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) (predExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // mask(array, pred): a Bool PRESENCE array in the SOURCE index space --
    // m[i] = pred(A[i]), same extent as A, NO compaction (compaction happens
    // downstream in compound(A, m)). See materializeMaskForm.
    // Strict elem-type inference (emits #error if unresolvable) and
    // predicate-callable validation happen here at the call site; the
    // shared `materializeInlineForm` helper just emits the C++ template.
    //
    // Validation accepts any predicate that resolves to a
    // single-parameter callable through resolveCallable.
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "mask"
    let elemStr = elemTypeToCpp elemET
    let predErrCode =
        match resolveCallable predExpr with
        | Some callable when callable.Params.Length = 1 -> []
        | _ -> codegenError ctx ind "mask: predicate must resolve to a single-parameter callable; got something else (typechecker or IR bug)"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRMask (arrExpr, predExpr)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []  // Unreachable: helper supports IRMask
    let code = forceCode @ elemErrCode @ predErrCode @ [$"{ind}// mask: count + compact"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genIntersectBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (aExpr: IRExpr) (bExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // intersect(A, B): elements present in both arrays.
    let (forceCodeA, ctx, aExpr) = forceDeferredArrayInput ctx builder ($"{name}__a") aExpr
    let (forceCodeB, ctx, bExpr) = forceDeferredArrayInput ctx builder ($"{name}__b") bExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind aExpr "intersect"
    let elemStr = elemTypeToCpp elemET
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRIntersect (aExpr, bExpr)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCodeA @ forceCodeB @ elemErrCode @ [$"{ind}// intersect: build set from B, scan A"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genUnionBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (aExpr: IRExpr) (bExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // union(A, B): all elements from A, plus elements from B not in A.
    let (forceCodeA, ctx, aExpr) = forceDeferredArrayInput ctx builder ($"{name}__a") aExpr
    let (forceCodeB, ctx, bExpr) = forceDeferredArrayInput ctx builder ($"{name}__b") bExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind aExpr "union"
    let elemStr = elemTypeToCpp elemET
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRUnion (aExpr, bExpr)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCodeA @ forceCodeB @ elemErrCode @ [$"{ind}// union: all of A, plus elements from B not in A"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genUniqueBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // unique(A): dedup, preserving first-occurrence order. Two-pass:
    // first counts unique elements via std::unordered_set, then fills
    // the output array on a second pass (clearing the set in between
    // so first-occurrence membership testing repeats identically).
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "unique"
    let elemStr = elemTypeToCpp elemET
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRUnique arrExpr) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCode @ elemErrCode @ [$"{ind}// unique: dedup via unordered_set, first-occurrence order"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genRangeBinding (ctx: CodeGenContext) (binding: IRBinding) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // A BARE range in binding position (`let xs = 0..n`, `let r = range<I>`,
    // or either behind `|> compute`) materializes the iota it denotes. Inside
    // a combinator a range never reaches this arm -- the nest peels it as
    // induction values, which is the whole point of a virtual array. Only the
    // single-slot plain dense form has a standalone value meaning; compound/
    // sparse/halo slots enumerate coordinate sets and a multi-slot range only
    // means anything to a loop nest, so those keep a refusal.
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy "int64_t") binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None ->
            codegenError ctx ind "a compound/sparse/halo or multi-slot range has no standalone value form; iterate it via method_for(range<...>)"
    let code = [$"{ind}// range: materialized iota"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genGroupByBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (vals: IRExpr) (gk: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // group_by: per-group nested pointer allocation. Each grouped[g] is a
    // separately-allocated buffer of size offsets[g+1] - offsets[g], holding
    // the values for group g in the order discovered by the keys scan.
    // Layout matches normal rank-2 nested arrays so dimensional currying
    // (kernel taking a sub-array) works without touching the loop builder.
    // Outer extent = gk__ngroups; inner is ragged. Track grouped -> gk so
    // future ragged-aware iteration can recover offsets.
    //
    // The outer pointer-array is wrapped in Array<T*, 1>. The wrapper's
    // .extents points at the 2-element local size_t array {ngroups, 0};
    // .extents[0] = ngroups, .extents[1] reads 0 (placeholder for the
    // ragged inner). Element type T* keeps `grouped[g]` as a bare row
    // pointer for downstream peeling. Print's inner-loop bound of 0
    // means no values printed, matching prior behavior.
    //
    // PER-SEGMENT STREAM (docs/plans/structural/07 §3.4): the values are a
    // rank-1 variable bound with `.stream` (no array named after it exists)
    // and the grouping is structural (static run boundaries), so each row
    // is read from the store directly into its slot of the pool -- the
    // whole variable is never materialized. Any other consumer of a
    // streamed variable, and a key grouping over one, keep the standing
    // refusals.
    let streamedVals =
        match vals with
        | IRVar (vid, _) ->
            (match Map.tryFind vid ctx.VarNames with
             | Some vn -> Map.tryFind vn ctx.StreamedArrays |> Option.map (fun s -> (vn, s))
             | None -> None)
        | _ -> None
    match streamedVals with
    | Some (valsName, spec) ->
        let gkName = exprToCppCtx ctx gk
        if not (Set.contains gkName ctx.StructuralGroupings) then
            let ctx' = addVarName binding.Id name ctx
            (codegenError ctx ind $"group_by over the streamed variable '{spec.VarName}' needs a structural grouping (`segments(..)` / `files(..)`), which reads one run at a time; a key grouping needs the whole variable -- bind it with .read", ctx')
        else
        let pspec = (Blade.ProviderRegistry.tryFind spec.Provider).Value
        match Map.tryFind gkName ctx.GridGroupings, pspec.GenStreamWindow with
        | Some bounds, Some genWindow when spec.VarType.IndexTypes.Length = 2 ->
            // TILE GATHER over a streamed rank-2 source (§4.1b + §3.4): each
            // tile is one rectangular window read straight into its slot of
            // the pool, row-major -- the member layout the grid grouping
            // defines. With inherited edges a tile is exactly one chunk file.
            let elemStr = elemTypeToCpp spec.VarType.ElemType
            let g1 = bounds.[1].Length - 1
            let extentsDecl =
                fst (emitExtentsTable ind (name + "_extents") 2
                         [($"{gkName}__ngroups", false); ("0 /* inner extent is ragged */", false)])
            let tileRead =
                genWindow spec.FilePath spec.VarName valsName ($"{name}__pool + {gkName}__offsets[__t]")
                          [ ($"{gkName}__b0[__t0]", $"{gkName}__b0[__t0 + 1]"); ($"{gkName}__b1[__t1]", $"{gkName}__b1[__t1 + 1]") ]
                          spec.VarType
                |> List.map (fun s -> ind + "    " + s)
            let code =
                [ $"{ind}// group_by: per-tile STREAMED windows of '{spec.VarName}' -- each tile read from the store into its slot; no whole-array buffer" ]
                @ extentsDecl
                @ [ $$"""{{ind}}Array<{{elemStr}}*, 1> {{name}} = { new {{elemStr}}*[{{gkName}}__ngroups], {{name}}_extents };"""
                    $"{ind}{elemStr}* {name}__pool = new {elemStr}[{gkName}__offsets[{gkName}__ngroups]];"
                    $$"""{{ind}}for (size_t __t = 0; __t < {{gkName}}__ngroups; __t++) {"""
                    $"{ind}    size_t __t0 = __t / {g1}, __t1 = __t %% {g1};"
                    $"{ind}    {name}[__t] = {name}__pool + {gkName}__offsets[__t];" ]
                @ tileRead
                @ [ $"{ind}}}" ]
            registerShapedAlloc name "deallocate_ragged_storage" ($"{name}.data, {name}__pool")
            let ctx' = addVarName binding.Id name ctx
            let ctx' = { ctx' with GroupedArrays = Map.add name gkName ctx'.GroupedArrays }
            (code, ctx')
        | _ when spec.VarType.IndexTypes.Length <> 1 ->
            let ctx' = addVarName binding.Id name ctx
            (codegenError ctx ind $"group_by over the streamed rank-{spec.VarType.IndexTypes.Length} variable '{spec.VarName}' needs its tile grouping (`segments(C0, C1)`); bind it with .read for anything else", ctx')
        | _, _ ->
        match pspec.GenStreamRows with
        | None ->
            let ctx' = addVarName binding.Id name ctx
            (codegenError ctx ind $"provider '{spec.Provider}' does not support per-segment streamed reads ('{spec.VarName}' -- bind with .read)", ctx')
        | Some genRows ->
            let elemStr = elemTypeToCpp spec.VarType.ElemType
            let extentsDecl =
                fst (emitExtentsTable ind (name + "_extents") 2
                         [($"{gkName}__ngroups", false); ("0 /* inner extent is ragged */", false)])
            let rowRead =
                genRows spec.FilePath spec.VarName valsName ($"{name}__pool + __off") $"{gkName}__offsets[__g]" $"{gkName}__offsets[__g + 1]" spec.VarType
                |> List.map (fun s -> ind + "    " + s)
            let code =
                [ $"{ind}// group_by: per-segment STREAMED rows of '{spec.VarName}' -- each run read from the store into its slot; no whole-array buffer" ]
                @ extentsDecl
                @ [ $$"""{{ind}}Array<{{elemStr}}*, 1> {{name}} = { new {{elemStr}}*[{{gkName}}__ngroups], {{name}}_extents };"""
                    $"{ind}{elemStr}* {name}__pool = new {elemStr}[{gkName}__offsets[{gkName}__ngroups]];"
                    $$"""{{ind}}for (size_t __g = 0; __g < {{gkName}}__ngroups; __g++) {"""
                    $"{ind}    size_t __off = {gkName}__offsets[__g];"
                    $"{ind}    {name}[__g] = {name}__pool + __off;" ]
                @ rowRead
                @ [ $"{ind}}}" ]
            registerShapedAlloc name "deallocate_ragged_storage"
            let ctx' = addVarName binding.Id name ctx
            let ctx' = { ctx' with GroupedArrays = Map.add name gkName ctx'.GroupedArrays }
            (code, ctx')
    | None ->
    // group_by's copy loop indexes vals by name, so it needs a MATERIALIZED
    // input; the shared helper forces a still-deferred or inline vals first.
    let (forceCode, ctx, vals) = forceDeferredArrayInput ctx builder ($"{name}__vals") vals
    let gridGk =
        let gkName = exprToCppCtx ctx gk
        Map.tryFind gkName ctx.GridGroupings |> Option.map (fun b -> (gkName, b))
    match gridGk with
    | Some (gkName, bounds) ->
        // TILE GATHER (docs/plans/structural/07 §4.1b): the values are rank 2,
        // addressed by coordinates; tile t = (t / G1, t % G1) copies its
        // window row-major into its slot of the pool.
        let valsName = exprToCppCtx ctx vals
        let (elemType, elemErrCode) = inferElemTypeStrict ctx ind vals "group_by"
        let elemStr = elemTypeToCpp elemType
        let g1 = bounds.[1].Length - 1
        let extentsDecl =
            fst (emitExtentsTable ind (name + "_extents") 2
                     [($"{gkName}__ngroups", false); ("0 /* inner extent is ragged */", false)])
        let code =
            elemErrCode
            @ [ $"{ind}// group_by: tile gather over a rank-2 value (grid grouping {gkName})" ]
            @ extentsDecl
            @ [ $$"""{{ind}}Array<{{elemStr}}*, 1> {{name}} = { new {{elemStr}}*[{{gkName}}__ngroups], {{name}}_extents };"""
                $"{ind}{elemStr}* {name}__pool = new {elemStr}[{gkName}__offsets[{gkName}__ngroups]];"
                $$"""{{ind}}for (size_t __t = 0; __t < {{gkName}}__ngroups; __t++) {"""
                $"{ind}    size_t __t0 = __t / {g1}, __t1 = __t %% {g1};"
                $"{ind}    size_t __lo0 = {gkName}__b0[__t0], __hi0 = {gkName}__b0[__t0 + 1];"
                $"{ind}    size_t __lo1 = {gkName}__b1[__t1], __hi1 = {gkName}__b1[__t1 + 1];"
                $"{ind}    size_t __w = __hi1 - __lo1;"
                $"{ind}    {name}[__t] = {name}__pool + {gkName}__offsets[__t];"
                $$"""{{ind}}    for (size_t __i = __lo0; __i < __hi0; __i++) for (size_t __j = __lo1; __j < __hi1; __j++) {{name}}[__t][(__i - __lo0) * __w + (__j - __lo1)] = {{valsName}}[__i][__j];"""
                $"{ind}}}" ]
        registerShapedAlloc name "deallocate_ragged_storage" ($"{name}.data, {name}__pool")
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with GroupedArrays = Map.add name gkName ctx'.GroupedArrays }
        (forceCode @ code, ctx')
    | None ->
    let valsName = exprToCppCtx ctx vals
    let gkName = exprToCppCtx ctx gk
    let (elemType, elemErrCode) = inferElemTypeStrict ctx ind vals "group_by"
    let elemStr = elemTypeToCpp elemType
    // The value array may be a rank-1 COMPOUND compact view (`group_by` over
    // the surviving cells of a mask). gk__perm indexes the same COMPACT order
    // the key scan walked, so the gather just needs the matching accessor.
    let (_, valsAt) = compactOrDenseSource vals valsName
    // Heap extents, not a frame-local `size_t[2]`: `Array<T*,1>` stores only a
    // POINTER to the table, so a grouped array that is RETURNED (or otherwise
    // outlives this frame) would hand its consumer a dangling shape. Same
    // return-extent ABI the peel emitters follow; see emitExtentsTable.
    let extentsDecl =
        fst (emitExtentsTable ind (name + "_extents") 2
                 [($"{gkName}__ngroups", false); ("0 /* inner extent is ragged */", false)])
    // GATHER ELISION: every consumer of this binding reads only row LENGTHS
    // (computeExtentsOnlyGroupBys), which come from the gk offsets. The row
    // TABLE is still emitted -- the peel indexes it, and print reads its extents
    // -- but the per-group buffers and the O(n) copy are dead, so the rows stay
    // null. Legal because the peel reads the pointer without dereferencing it,
    // and deallocate_ragged_rows_owned's `delete[] rows[i]` no-ops on null.
    let elideGather = Set.contains binding.Id (extentsOnlyGroupBysCell ()).Value
    let code =
        elemErrCode
        @ [ if elideGather
            then $"{ind}// group_by: rows NOT gathered -- every consumer reads only extents(row)"
            else $"{ind}// group_by: per-group nested allocation, group-contiguous via gk__perm" ]
        @ extentsDecl
        @ [ $$"""{{ind}}Array<{{elemStr}}*, 1> {{name}} = { new {{elemStr}}*[{{gkName}}__ngroups], {{name}}_extents };""" ]
        @ (if elideGather then
             [ $"{ind}for (size_t __g = 0; __g < {gkName}__ngroups; __g++) {name}[__g] = nullptr;" ]
           else
             // ONE CSR POOL, not one `new[]` per group. The total is already in
             // hand -- `gk__offsets[gk__ngroups]` is the CSR convention -- so the
             // per-group allocation was never buying anything, and it cost three
             // ways at once: G malloc/free pairs, G allocator headers roughly
             // doubling footprint, and the destruction of spatial locality that
             // made every later pass walk a header-interleaved heap instead of
             // one contiguous run. Measured on the segmented-reduction prototype
             // (src/microkernels/segmented_fold.c): 2.4x / 11.3x / 3.6x on
             // uniform-large / uniform-small / skewed group distributions, and
             // 62-94% of that kernel's whole recoverable win. The small-group
             // case is where it hurts most -- G mallocs for a 4-element group is
             // ~78 ns of allocator per group against a handful of ns of copy.
             //
             // Rows now SLICE the pool, so ownership changes with them: the
             // table's deallocator becomes `deallocate_ragged_storage(rows, pool)`
             // (registered below). `deallocate_ragged_rows_owned` would be
             // undefined behaviour here -- it `delete[]`s each row, and these are
             // interior pointers.
             [ $"{ind}{elemStr}* {name}__pool = new {elemStr}[{gkName}__offsets[{gkName}__ngroups]];"
               $$"""{{ind}}for (size_t __g = 0; __g < {{gkName}}__ngroups; __g++) {"""
               $"{ind}    size_t __off = {gkName}__offsets[__g];"
               $"{ind}    size_t __sz = {gkName}__offsets[__g + 1] - __off;"
               $"{ind}    {name}[__g] = {name}__pool + __off;"
               $$"""{{ind}}    for (size_t __k = 0; __k < __sz; __k++) {"""
               $"""{ind}        {name}[__g][__k] = {(valsAt (sprintf "%s__at(__off + __k)" gkName))};"""
               $$"""{{ind}}    }"""
               $"{ind}}}" ])
    // Owns the row table AND every per-group row (each a separate new[]).
    // The wrapper is Array<T*,1> whose .extents[1] is the ragged placeholder
    // 0, so the row count comes from the gk__ngroups local -- a plain size_t
    // in the same scope, guaranteed live at the scope-exit free point. The
    // gk__offsets/__perm side tables are NOT owned here (see group_keys).
    // Ownership follows the layout chosen above. The GATHERED form slices one
    // CSR pool, so the table and the pool are freed together and no row is
    // individually owned -- `deallocate_ragged_storage` is exactly that shape,
    // and its own comment already named the per-row-owned group_by layout as
    // the opposite hazard. The ELIDED form allocates no pool and leaves every
    // row null, so it keeps the per-row deallocator, which no-ops on null.
    if elideGather then
        registerShapedAlloc name "deallocate_ragged_rows_owned"
            ($"{name}.data, {gkName}__ngroups")
    else
        registerShapedAlloc name "deallocate_ragged_storage"
            ($"{name}.data, {name}__pool")
    let ctx' = addVarName binding.Id name ctx
    let ctx' = { ctx' with GroupedArrays = Map.add name gkName ctx'.GroupedArrays }
    (forceCode @ code, ctx')



/// segments(A): the STRUCTURAL grouping (docs/plans/structural/07 §2.2, §3.2).
/// Same name-suffix ABI as group_keys -- `__ngroups`, `__offsets`, `__nsrc`,
/// `__at` -- but NO permutation is built: the offsets are the static run
/// boundaries and `__at` is the identity, so a grouped read is a direct
/// read. `__perm` is not declared at all; every consumer goes through
/// `__at`, which is what makes the two regimes interchangeable downstream.
and genSegmentsBinding (ctx: CodeGenContext) (binding: IRBinding) (offsets: int64 list) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let ngroups = offsets.Length - 1
    let n = List.last offsets
    let table = offsets |> List.map (fun b -> $"{b}UL") |> String.concat ", "
    let code =
        [ $"{ind}// segments: {ngroups} runs over {n} cells, structural (static boundaries, identity permutation)"
          $"{ind}size_t {name}__ngroups = {ngroups};"
          $"{ind}const size_t {name}__offsets[{ngroups + 1}] = {{ {table} }};"
          $"{ind}size_t {name}__nsrc = {n};"
          $"{ind}auto {name}__at = [](size_t __p) {{ return __p; }};"
          $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
          $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets (structural: no __perm)" ]
    let ctx' = addVarName binding.Id name ctx
    let ctx' = { ctx' with StructuralGroupings = Set.add name ctx'.StructuralGroupings }
    (code, ctx')

/// segments(C0, C1): the tile grouping of two segmented slots (docs/plans/
/// structural/07 §4.1b). Same name-suffix ABI; the offsets are the
/// cumulative tile sizes, tile t = (t / G1, t % G1); the per-slot boundary
/// tables are emitted for the gather and the scatter. `__at` is the
/// identity over the tile-major member order (used by nothing today; the
/// grid gather addresses the rank-2 value by coordinates).
and genSegmentsGridBinding (ctx: CodeGenContext) (binding: IRBinding) (bounds: int64 list list) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let b0, b1 = bounds.[0], bounds.[1]
    let g0, g1 = b0.Length - 1, b1.Length - 1
    let sizes =
        [ for i in 0 .. g0 - 1 do
            for j in 0 .. g1 - 1 ->
                (b0.[i + 1] - b0.[i]) * (b1.[j + 1] - b1.[j]) ]
    let offsets = sizes |> List.scan (+) 0L
    let table (xs: int64 list) = xs |> List.map (fun b -> $"{b}UL") |> String.concat ", "
    let code =
        [ $"{ind}// segments grid: {g0} x {g1} tiles over {List.last b0} x {List.last b1} cells, structural"
          $"{ind}size_t {name}__ngroups = {g0 * g1};"
          $"{ind}const size_t {name}__offsets[{g0 * g1 + 1}] = {{ {table offsets} }};"
          $"{ind}const size_t {name}__b0[{g0 + 1}] = {{ {table b0} }};"
          $"{ind}const size_t {name}__b1[{g1 + 1}] = {{ {table b1} }};"
          $"{ind}size_t {name}__nsrc = {List.last offsets};"
          $"{ind}auto {name}__at = [](size_t __p) {{ return __p; }};"
          $"{ind}size_t {name}_extents[1] = {{{name}__ngroups}};"
          $"{ind}void* {name} = nullptr; // gk: state in {name}__ngroups, {name}__offsets, {name}__b0/__b1 (grid)" ]
    let ctx' = addVarName binding.Id name ctx
    let ctx' = { ctx' with StructuralGroupings = Set.add name ctx'.StructuralGroupings
                           GridGroupings = Map.add name bounds ctx'.GridGroupings }
    (code, ctx')

/// ungroup(G) of a tile grouping: the tiles written back over the two
/// source axes into one dense rank-2 array.
and genUngroupGridBinding (ctx: CodeGenContext) (binding: IRBinding) (g: IRExpr) (srcs: IRIndexType list) (bounds: int64 list list) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let gName = exprToCppCtx ctx g
    let elemStr =
        match binding.Type with
        | ArrayElem at -> elemTypeToCpp at.ElemType
        | _ -> "double"
    match Map.tryFind gName ctx.GroupedArrays with
    | None ->
        let ctx' = addVarName binding.Id name ctx
        (codegenError ctx ind $"ungroup: '{gName}' is not a group_by result this emitter can trace to its grouping (bind `let G = group_by(A, segments(X, Y))` and ungroup that name)", ctx')
    | Some gkName ->
        let b0, b1 = bounds.[0], bounds.[1]
        let n0, n1 = List.last b0, List.last b1
        let g1 = b1.Length - 1
        let (extentsDecl, ownedExtents) =
            emitExtentsTable ind (name + "_extents") 2 [($"{n0}UL", false); ($"{n1}UL", false)]
        let code =
            [ $"{ind}// ungroup: tiles of {gName} written back over the two axes ({n0} x {n1})" ]
            @ extentsDecl
            @ [ $$"""{{ind}}Array<{{elemStr}}, 2> {{name}} = { allocate<promote<{{elemStr}}, 2>::type>({{name}}_extents), {{name}}_extents };"""
                $$"""{{ind}}for (size_t __t = 0; __t < {{gkName}}__ngroups; __t++) {"""
                $"{ind}    size_t __t0 = __t / {g1}, __t1 = __t %% {g1};"
                $"{ind}    size_t __lo0 = {gkName}__b0[__t0], __hi0 = {gkName}__b0[__t0 + 1];"
                $"{ind}    size_t __lo1 = {gkName}__b1[__t1], __hi1 = {gkName}__b1[__t1 + 1];"
                $"{ind}    size_t __w = __hi1 - __lo1;"
                $$"""{{ind}}    for (size_t __i = __lo0; __i < __hi0; __i++) for (size_t __j = __lo1; __j < __hi1; __j++) {{name}}[__i][__j] = {{gName}}[__t][(__i - __lo0) * __w + (__j - __lo1)];"""
                $"{ind}}}" ]
        registerPoolAlloc AllocDense elemStr 2 "nullptr" (name + "_extents") name ownedExtents
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

/// ungroup(G): the rows of a segment-grouped array written back over the
/// source axis (§3.3). G's grouping is recovered from GroupedArrays (the
/// same registration the grouped peel uses), so the offsets are the
/// grouping's own; the destination is one dense buffer of the source
/// extent, and each row lands at its run.
and genUngroupBinding (ctx: CodeGenContext) (binding: IRBinding) (g: IRExpr) (src: IRIndexType) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let gName = exprToCppCtx ctx g
    let elemStr =
        match binding.Type with
        | ArrayElem at -> elemTypeToCpp at.ElemType
        | _ -> "double"
    match Map.tryFind gName ctx.GroupedArrays with
    | None ->
        let ctx' = addVarName binding.Id name ctx
        (codegenError ctx ind $"ungroup: '{gName}' is not a group_by result this emitter can trace to its grouping (bind `let G = group_by(A, segments(X))` and ungroup that name)", ctx')
    | Some gkName ->
        let total =
            match src.Extent with
            | IRLit (IRLitInt n) -> $"{n}UL"
            | _ -> $"{gkName}__offsets[{gkName}__ngroups]"
        let (extentsDecl, ownedExtents) =
            emitExtentsTable ind (name + "_extents") 1 [(total, false)]
        let code =
            [ $"{ind}// ungroup: rows of {gName} written back over the source axis ({total} cells)" ]
            @ extentsDecl
            @ [ $$"""{{ind}}Array<{{elemStr}}, 1> {{name}} = { allocate<promote<{{elemStr}}, 1>::type>({{name}}_extents), {{name}}_extents };"""
                $$"""{{ind}}for (size_t __g = 0; __g < {{gkName}}__ngroups; __g++) {"""
                $"{ind}    size_t __off = {gkName}__offsets[__g];"
                $"{ind}    size_t __sz = {gkName}__offsets[__g + 1] - __off;"
                $$"""{{ind}}    for (size_t __k = 0; __k < __sz; __k++) {{name}}[{{gkName}}__at(__off + __k)] = {{gName}}[__g][__k];"""
                $"{ind}}}" ]
        registerPoolAlloc AllocDense elemStr 1 "nullptr" (name + "_extents") name ownedExtents
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')

/// ungroup([r1, .., rF], A): per-file arrays copied at their file offsets
/// into one buffer over the tiled axis (§2.7). Rows are bare names by
/// construction (inferUngroupRows) and the offsets are static.
and genUngroupRowsBinding (ctx: CodeGenContext) (binding: IRBinding) (rows: IRExpr list) (offsets: int64 list) (src: IRIndexType) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let elemStr =
        match binding.Type with
        | ArrayElem at -> elemTypeToCpp at.ElemType
        | _ -> "double"
    let total = List.last offsets
    let (extentsDecl, ownedExtents) =
        emitExtentsTable ind (name + "_extents") 1 [($"{total}UL", false)]
    let copies =
        List.zip rows (List.pairwise offsets)
        |> List.collect (fun (r, (lo, hi)) ->
            let rName = exprToCppCtx ctx r
            [ $$"""{{ind}}for (size_t __k = 0; __k < {{hi - lo}}UL; __k++) {{name}}[{{lo}}UL + __k] = {{rName}}[__k];""" ])
    let code =
        [ $"{ind}// ungroup: {rows.Length} per-file array(s) assembled over the tiled axis ({total} cells)" ]
        @ extentsDecl
        @ [ $$"""{{ind}}Array<{{elemStr}}, 1> {{name}} = { allocate<promote<{{elemStr}}, 1>::type>({{name}}_extents), {{name}}_extents };""" ]
        @ copies
    registerPoolAlloc AllocDense elemStr 1 "nullptr" (name + "_extents") name ownedExtents
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')

and genGroupBucketBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (gk: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let gkName = exprToCppCtx ctx gk
    // group_bucket(gk): invert the CSR (perm, offsets) pair into a dense
    // row -> bucket map over the SOURCE index space. One pass over the members:
    //
    //   for g in [0, ngroups): for p in [offsets[g], offsets[g+1]): b[perm[p]] = g
    //
    // Rows the permutation never names are exactly the rows a negative key
    // dropped (docs/features/sql.md), so the -1 prefill IS the drop marker --
    // no separate key rescan, and no dependence on which of the three bucketing
    // regimes built the table.
    //
    // Total work is offsets[ngroups] + nsrc, and the writes are a permutation
    // (each row written at most once), so no scatter conflict to serialize.
    //
    // gk is a bare name by construction (inferGroupBucket refuses anything
    // else), which is what makes the `<gk>__` suffixed reads below resolvable.
    let elemStr = "int64_t"
    let (extentsDecl, ownedExtents) =
        emitExtentsTable ind (name + "_extents") 1 [($"{gkName}__nsrc", false)]
    let code =
        [ $"{ind}// group_bucket: invert gk's perm/offsets into row -> bucket (-1 = dropped row)" ]
        @ extentsDecl
        @ [ $$"""{{ind}}Array<{{elemStr}}, 1> {{name}} = { allocate<promote<{{elemStr}}, 1>::type>({{name}}_extents), {{name}}_extents };"""
            $"{ind}for (size_t __i = 0; __i < {gkName}__nsrc; __i++) {name}[__i] = -1;"
            $$"""{{ind}}for (size_t __g = 0; __g < {{gkName}}__ngroups; __g++) {"""
            $$"""{{ind}}    for (size_t __p = {{gkName}}__offsets[__g]; __p < {{gkName}}__offsets[__g + 1]; __p++) {"""
            $"{ind}        {name}[{gkName}__at(__p)] = ({elemStr})__g;"
            $$"""{{ind}}    }"""
            $"{ind}}}" ]
    registerPoolAlloc AllocDense elemStr 1 "nullptr" (name + "_extents") name ownedExtents
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genGroupSizesBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (gk: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let gkName = exprToCppCtx ctx gk
    // extents(gk): per-group sizes straight off the CSR row pointers,
    // sizes[g] = offsets[g+1] - offsets[g]. O(ngroups), and -- the point of the
    // spelling -- NO group_by: the values are never gathered, so a count-only
    // query stops paying for a copy of the data it ignores.
    //
    // This is the same arithmetic the ragged peel builds each row's `.len` from
    // (genGroupByBinding's RaggedRow), which is what makes `extents(gk)` and
    // `method_for(group_by(v, gk)) <@> lambda(r) -> extents(r)` the same answer.
    // Rows dropped by a negative key are absent from every group's span, so they
    // are counted nowhere -- consistent with group_bucket reading -1 for them.
    let elemStr = "int64_t"
    let (extentsDecl, ownedExtents) =
        emitExtentsTable ind (name + "_extents") 1 [($"{gkName}__ngroups", false)]
    let code =
        [ $"{ind}// extents(gk): per-group sizes from the CSR offsets (no gather)" ]
        @ extentsDecl
        @ [ $$"""{{ind}}Array<{{elemStr}}, 1> {{name}} = { allocate<promote<{{elemStr}}, 1>::type>({{name}}_extents), {{name}}_extents };"""
            $$"""{{ind}}for (size_t __g = 0; __g < {{gkName}}__ngroups; __g++) {"""
            $"{ind}    {name}[__g] = ({elemStr})({gkName}__offsets[__g + 1] - {gkName}__offsets[__g]);"
            $"{ind}}}" ]
    registerPoolAlloc AllocDense elemStr 1 "nullptr" (name + "_extents") name ownedExtents
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genSortBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) (keyExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // sort(array, key): stable ascending sort by key.
    //
    // Current approach: eager materialization. Construct a permutation via
    // std::stable_sort with a comparator that calls the user's key function,
    // then write the permuted elements into a fresh contiguous buffer.
    //
    // A lazy chain handle -- recording (key_fn, permutation, source_pointer)
    // and deferring materialization to first access -- would let long chains
    // of sorts and other rearrangements be analyzed before any layout commits,
    // enabling sort-skip, merge-style joins, and other optimizations; not yet
    // implemented. Materialization caching would sit downstream of that
    // analysis, not as a substitute for it.
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "sort"
    let elemStr = elemTypeToCpp elemET
    // Validate single-param key callable. Helper falls back to a 0L key
    // (preserving input order); the #error here surfaces the IR bug
    // before the silently-wrong sort runs.
    let keyErrCode =
        match resolveCallable keyExpr with
        | Some callable when callable.Params.Length = 1 -> []
        | _ -> codegenError ctx ind "sort: key must resolve to a single-parameter callable; got something else (typechecker or IR bug)"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRSort (arrExpr, keyExpr)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCode @ elemErrCode @ keyErrCode @ [$"{ind}// sort: stable_sort on permutation, eager materialization"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genTransposeBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) (d1: int) (d2: int) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // transpose(array, [d1, d2]): hard transpose -- allocate a fresh pool at
    // the swapped extents and copy with axes d1/d2 exchanged. Eager
    // materialization (same phase-1 strategy as sort); the result is an
    // independent array with no aliasing back to the source. TypeCheck has
    // already verified both axes are arity-1 SymNone and in range.
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "transpose"
    let elemStr = elemTypeToCpp elemET
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRTranspose (arrExpr, d1, d2)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCode @ elemErrCode @ [$"{ind}// transpose: hard (swapped-extent alloc + axis-swapped copy)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genStackJoinBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrs: IRExpr list) (joinDim: int option) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let opName = match joinDim with Some _ -> "join" | None -> "stack"
    // Eager materialization (same phase-1 strategy as transpose/decompact): a
    // fresh dense pool plus per-source copy nests. Every source is forced first
    // so a deferred producer materializes under its own name before the copy.
    let (forceCode, ctx, arrs) =
        arrs |> List.mapi (fun i a -> (i, a))
        |> List.fold (fun (accCode, accCtx, accArrs) (i, a) ->
            let (code, ctx', a') = forceDeferredArrayInput accCtx builder ($"{name}__src{i}") a
            (accCode @ code, ctx', accArrs @ [a'])) ([], ctx, [])
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind (List.head arrs) opName
    let elemStr = elemTypeToCpp elemET
    let form = match joinDim with Some d -> IRJoin (arrs, d) | None -> IRStack arrs
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) form with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let note =
        match joinDim with
        | Some d -> $"{ind}// join: dense alloc (summed extent on dim {d}) + per-source offset copy"
        | None -> $"{ind}// stack: fresh leading axis (dense alloc + per-source copy)"
    let code = forceCode @ elemErrCode @ [note] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')


and genDecompactBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) (d: int) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // decompact(array, d): pull the compact component at dim d out as a
    // free Idx. Hard materialization -- allocate a fresh dense pool and
    // scatter the canonical (triangular-packed) source elements into all
    // of the decompacted component's image positions, applying the per-
    // class transform (Sym copy / Antisym sign + zero diagonal / Hermitian
    // conj). TypeCheck has verified dim d targets a compact slot and that
    // the Antisym middle-peel case is excluded.
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "decompact"
    let elemStr = elemTypeToCpp elemET
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) (IRDecompact (arrExpr, d)) with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCode @ elemErrCode @ [$"{ind}// decompact: hard (dense alloc + symmetry-expanding scatter)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genArrayNegateConjugateBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Whole-array eager negate/conjugate (the cheap intra-group transposes).
    // Type-preserving: same-shape alloc + flat contiguous-pool transform.
    let isConj = (binding.Value.IsIRArrayConjugate)
    let label = if isConj then "conjugate" else "negate"
    let (forceCode, ctx, arrExpr) = forceDeferredArrayInput ctx builder ($"{name}__arr") arrExpr
    let (elemET, elemErrCode) = inferElemTypeStrict ctx ind arrExpr ($"array_{label}")
    let elemStr = elemTypeToCpp elemET
    let form = if isConj then IRArrayConjugate arrExpr else IRArrayNegate arrExpr
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) form with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = forceCode @ elemErrCode @ [$"{ind}// array_{label}: whole-array eager transform (same-shape alloc + pool loop)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genGramBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // gram(A, B) = A * B^H. Materialized as a triangular (same-array,
    // symmetric/Hermitian) or full (distinct, dense) scatter with an inner
    // contracted-axis reduction. The shared helper emits the statement form.
    let elemStr =
        match binding.Type with
        | ArrayElem at -> irTypeToCpp at.ElemType
        | _ -> "double"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = [$"{ind}// gram: A * B^H (Gram product)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')


and genGramApplyBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    // gram_apply(A, B, x) = A * (B^H * x): the action of gram(A, B) on x,
    // materialized as two rank-1 pools (B^H x, then A times it) and never the
    // m x p matrix. The shared helper emits the statement form.
    let ind = indentStr ctx
    let name = bindingCppName binding
    let elemStr =
        match binding.Type with
        | ArrayElem at -> irTypeToCpp at.ElemType
        | _ -> "double"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = [$"{ind}// gram_apply: A * (B^H * x) (Gram action, no m x p pool)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')


and genMatmulBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    // matmul(A, B) = A * B. Dense m x n result. Same shape as genGramBinding:
    // the shared materialize helper emits the statement form (allocation plus
    // one `blade_linalg::blade_matmul` dispatch call).
    let ind = indentStr ctx
    let name = bindingCppName binding
    let elemStr =
        match binding.Type with
        | ArrayElem at -> irTypeToCpp at.ElemType
        | _ -> "double"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code = [$"{ind}// matmul: A * B (dense matrix product)"] @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')


and genEighBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    // eigh(S) -> (Q, LAM). The one linalg binding whose value is a TUPLE, so
    // unlike gram/matmul there is no single element type to pass down: the
    // shared helper derives both from the operand (see materializeEighForm's
    // design note). `binding.Type` here is the `IRTTuple` inferEigh built; the
    // emitted `std::tuple<...>` spelling is derived from the operand instead, so
    // the two cannot drift apart through a stale binding type.
    let ind = indentStr ctx
    let name = bindingCppName binding
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy "") binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code =
        [$"{ind}// eigh: symmetric/Hermitian eigendecomposition -> (Q, LAM)"]
        @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')


and genSolveBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    // solve(A, b) -> x. Same shape as genMatmulBinding: one array result, one
    // element type, and the shared materialize helper emits the statement form
    // (allocation plus either the LU loop nest or one `blade_lapack::blade_solve`
    // dispatch call, per the availability gate).
    let ind = indentStr ctx
    let name = bindingCppName binding
    let elemStr =
        match binding.Type with
        | ArrayElem at -> irTypeToCpp at.ElemType
        | _ -> "double"
    let matStmts =
        match materializeInlineForm emptySubst ctx.VarNames name (lazy elemStr) binding.Value with
        | Some (s, allocs) -> registerMaterializedAllocs allocs; s
        | None -> []
    let code =
        [$"{ind}// solve: A.x = b by partial-pivoted LU -> x"]
        @ (matStmts |> List.map (fun s -> ind + s))
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genReduceBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (arrExpr: IRExpr) (kernelExpr: IRExpr) (initExpr: IRExpr option) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // reduce(array, op): T/S reduction. Consumes the innermost dim by a
    // binary kernel, producing a scalar (rank-1 input only for now).
    //
    // Empty-array handling (post-extents integration):
    //   - Static extent > 0: standard loop, no runtime check
    //     (typecheck already proved non-emptiness)
    //   - Dynamic extent: emit a runtime guard that aborts cleanly on
    //     empty rather than reading uninitialized memory from arr[0]
    //   - Static extent = 0: typecheck rejects before reaching here
    //
    // Section kernels like (+) lower to callables during Lowering
    // with properly-resolved scalar types. Resolution flows
    // through `resolveCallable`.
    let arrName = exprToCppCtx ctx arrExpr
    let (elemType, elemErrCode) = inferElemTypeStrict ctx ind arrExpr "reduce"
    let elemStr = elemTypeToCpp elemType

    // Length accessor: a rank-1 ragged-family operand (a let-bound row of
    // a ragged/DepIdx array or of a group_by result) is a RaggedRow<T>,
    // which carries its length inline as `.len` -- not `.extents[0]`.
    // Same predicate the expression-form reduce, IRExtent, and the
    // sub-view binding use, so the accessor always matches the declared
    // type. Element access `%s[%s]` works for both via operator[].
    let isRaggedRowOperand =
        match inferExprType arrExpr with
        | ArrayElem at -> isRaggedRowType at
        | _ -> false
    // A compound operand reduces over its compact buffer: bound =
    // cardinality * trailing_stride (all present values), elements via
    // .data[i]. This is what makes `reduce(compound(A, mask(A, p)), (+))`
    // -- SQL's SUM(x) WHERE p -- a one-liner. Mirrors the expression-form
    // reduce's compound handling.
    let isCompoundOperand =
        match inferExprType arrExpr with
        | ArrayElem at -> isCompoundArrayType at || isSparseArrayType at
        | _ -> false
    let boundExpr =
        if isRaggedRowOperand then $"{arrName}.len"
        elif isCompoundOperand then $"({arrName}.idx->cardinality * {arrName}.trailing_stride)"
        // Dense operand: the shared literal-or-runtime rule, exactly as the
        // expression-form reduce already does. The two forms emit the same
        // fold over the same array and must agree on its trip count.
        else
            match inferExprType arrExpr with
            | ArrayElem at -> literalOrRuntimeExtentOfArray at arrName 0
            | _ -> $"{arrName}.extents[0]"
    let elemAt (i: string) =
        if isCompoundOperand then $"{arrName}.data[{i}]"
        else $"{arrName}[{i}]"

    // Decide whether to emit a runtime extent check based on whether
    // the array's innermost-dim extent is statically known.
    let isStaticallyNonEmpty =
        match inferExprType arrExpr with
        | ArrayElem at when at.IndexTypes.Length >= 1 ->
            match tryEvalIntIR at.IndexTypes.[at.IndexTypes.Length - 1].Extent with
            | Some n -> n > 0L
            | None -> false
        | _ -> false

    let guardLines =
        // The 3-arg (init) form defines the empty fold as init, so it never
        // needs an emptiness guard, static or dynamic.
        if isStaticallyNonEmpty || initExpr.IsSome then []
        else [
            $"{ind}// reduce: dynamic extent -- runtime non-emptiness guard"
            $"{ind}if ({boundExpr} == 0) {{ blade_rt::panic(\"BL8003\", \"reduce: empty array, no reduction possible\", nullptr, 0); }}"
        ]

    // Comm-licensed parallel fold. `where ... omp` on the fold kernel is the
    // opt-in; the LICENCE (declared comm, or a builtin body) is what makes the
    // reorder sound (typecheck refuses the unlicensed combination, BL4016).
    // Two emitted shapes:
    //   Path A -- builtin `+`/`*` over a real arithmetic element type: one
    //     `#pragma omp parallel for simd reduction(<op>:acc)` over the flat
    //     sweep -- the serial fold up to reassociation, nothing else changes.
    //   Path B -- any other licensed kernel: manual contiguous chunks with a
    //     per-thread partial, each seeded from its OWN first element (chunk 0
    //     inherits the caller's seed via fixed-order combine), so no identity
    //     element is needed. Deterministic for a fixed OMP_NUM_THREADS.
    // Both are flat 1-D sweeps (`reduce` takes rank-1 arrays only); compound/
    // sparse operands walk their contiguous `.data` buffer of present cells.
    let parallelFold =
        match resolveCallable kernelExpr with
        | Some c when c.Params.Length = 2 && c.IsOmpParallel && foldReorderLicensed c -> Some c
        | _ -> None
    // Path A additionally needs an element type OpenMP's built-in reduction
    // identities are defined for. Complex (and anything non-arithmetic) takes
    // Path B, which needs no identity.
    let pathAOp =
        match parallelFold with
        | Some c ->
            (match elemType with
             | IRTScalar (ETFloat64 | ETFloat32 | ETInt64 | ETInt32) ->
                foldKernelBuiltinOp c |> Option.bind (fun op ->
                    ompReductionOperator op |> Option.map (fun r -> (op, r)))
             | _ -> None)
        | None -> None
    // Hoisted sweep bound. OpenMP's canonical loop form wants a loop-invariant
    // upper bound, and the compound operand's is a two-field product through a
    // pointer -- computed once here for both parallel paths. (The serial path
    // keeps using `boundExpr` inline, byte-identical to before.)
    let rnName = $"__rn_{name}"
    // BUILD KNOB, applied ONE LEVEL ABOVE the two paths because they need
    // DIFFERENT suppressions and the difference is not arbitrary:
    //
    //   Path A is `parallel for simd reduction(op:acc)` -- TWO constructs. Only
    //     `parallel for` opens a team; `simd reduction` is vectorization the
    //     `omp` licence already paid for and the knob is not about. So Path A
    //     stays Path A, minus the team: the `BLADE_OMP_SIMD_REDUCTION` form
    //     `fpReassocSimdStmts` already emits at the BLADE_FP_REASSOC sites.
    //     REUSED rather than re-spelled, so the suppressed Path A and the knob's
    //     own simd sites cannot drift into two different summation shapes.
    //   Path B is a hand-written team (`#pragma omp parallel num_threads`,
    //     `omp_get_max_threads`, per-thread partials). There is no vector half
    //     to keep -- the whole construction IS the threading -- so it is
    //     suppressed wholesale by making `parallelFold` invisible below, which
    //     lands the fold on the SAME serial/lane arms an unlicensed kernel
    //     takes. No `omp_get_*` call survives anywhere in the output.
    //
    // See `ompThreadEmissionEnabled` for why this is a build decision.
    let ompThreadsOn = ompThreadEmissionEnabled ()
    // Census line for a Path B fold this build serialized. The arms it lands on
    // (lane / plain serial) are the ones an UNLICENSED kernel takes and emit
    // nothing about omp on their own, so without this the dropped clause would
    // be invisible -- the same silence `ompSuppressedMarker` exists to prevent
    // in the loop nests. Empty for Path A (which emits its own marker beside the
    // simd form it keeps) and for a kernel that never asked for omp.
    let pathBSuppressedNote =
        if parallelFold.IsSome && not ompThreadsOn
        then [ $"{ind}// [omp] requested but emitted serial: {(ompThreadsSuppressedReason ())}" ]
        else
        // The `where repro` veto: the kernel (or the named function its eta
        // wrapper calls) demands the serial operation sequence, so
        // foldReorderLicensed said no and the fold lands on the serial arms
        // below -- which say nothing about omp on their own. Same
        // silence-prevention as the knob note above; without it a
        // comm-licensed `omp, repro` fold would drop its omp request with no
        // trace in the emitted text.
        match resolveCallable kernelExpr with
        | Some c when c.Params.Length = 2 && c.IsOmpParallel && foldKernelReproVetoed c ->
            [ $"{ind}// [omp] requested but emitted serial: fold kernel is `where repro` (the operation order is the contract; reorder licence vetoed)" ]
        | _ -> []
    // STREAMED FOLD (docs/plans/structural/07, decision D6 as read: a fold over
    // a segmented axis is the plain flat fold at the API). The operand is a
    // rank-1 variable bound with `.stream`: no array exists, so the fold walks
    // the store one block at a time -- the block is the store's own chunk
    // edge -- and folds the cells in STORAGE ORDER. That is the same operation
    // sequence as the flat fold over a materialized copy, so the two agree
    // bitwise; nothing is reassociated and no reorder licence is asked
    // (`omp` on the kernel is noted and ignored: the store is read serially).
    let streamedFold =
        match Map.tryFind arrName ctx.StreamedArrays with
        | Some spec when spec.VarType.IndexTypes.Length = 1 ->
            let pspec = (Blade.ProviderRegistry.tryFind spec.Provider).Value
            match pspec.GenStreamRows, pspec.StreamRowsBlock with
            | Some genRows, Some blockOf ->
                let n =
                    match spec.VarType.IndexTypes.[0].Extent with
                    | IRLit (IRLitInt n) -> n
                    | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed fold over '{spec.VarName}' needs a static extent")))
                let block = max 1L (blockOf spec.FilePath spec.VarName)
                let blk = $"{name}__blk"
                let stepOf (acc: string) (cell: string) : string * string list =
                    match resolveCallable kernelExpr with
                    | Some callable when callable.Params.Length = 2 ->
                        (match pathAOp with
                         | Some (op, _) -> ($"{acc} {(binOpToCpp op)} {cell}", [])
                         | None ->
                             let (wrapperCode, wname) = genCallableWrapper ctx.VarNames name callable
                             ($"{wname}({acc}, {cell})", wrapperCode |> List.map (fun s -> ind + s)))
                    | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed fold over '{spec.VarName}': the kernel must be a two-argument fold kernel")))
                let (stepExpr, wrapperLines) = stepOf name $"{blk}[__q]"
                let seedLines, firstGuard =
                    match initExpr with
                    | Some initE -> ([ $"{ind}{elemStr} {name} = {(exprToCppCtx ctx initE)};" ], "")
                    | None ->
                        if n <= 0L then
                            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed fold over '{spec.VarName}': the variable is empty and the fold has no seed")))
                        ([ $"{ind}{elemStr} {name} = ({elemStr})0;"; $"{ind}bool {name}__first = true;" ],
                         $"if ({name}__first) {{ {name} = {blk}[__q]; {name}__first = false; }} else ")
                let ompNote =
                    match resolveCallable kernelExpr with
                    | Some c when c.IsOmpParallel -> [ $"{ind}// [omp] requested but emitted serial: the operand is streamed from the store in storage order" ]
                    | _ -> []
                let rowRead =
                    genRows spec.FilePath spec.VarName arrName blk "__lo" "__hi" spec.VarType
                    |> List.map (fun s -> ind + "    " + s)
                Some (
                    elemErrCode @ ompNote @ wrapperLines
                    @ [ $"{ind}// reduce: STREAMED fold over '{spec.VarName}' in storage order, {block} cells per block (bitwise the flat fold)"
                        $"{ind}{elemStr}* {blk} = new {elemStr}[{block}];" ]
                    @ seedLines
                    @ [ $$"""{{ind}}for (size_t __lo = 0; __lo < {{n}}UL; __lo += {{block}}UL) {"""
                        $"{ind}    size_t __hi = __lo + {block}UL; if (__hi > {n}UL) __hi = {n}UL;" ]
                    @ rowRead
                    @ [ $"{ind}    for (size_t __q = 0; __q < __hi - __lo; __q++) {firstGuard}{name} = {stepExpr};"
                        $"{ind}}}"
                        $"{ind}delete[] {blk};" ])
            | _ ->
                let ctx' = addVarName binding.Id name ctx
                Some (codegenError ctx ind $"provider '{spec.Provider}' does not support per-block streamed folds ('{spec.VarName}' -- bind with .read)")
        | _ -> None
    match streamedFold with
    | Some code -> (code, addVarName binding.Id name ctx)
    | None ->
    let code =
        match resolveCallable kernelExpr with
        | Some callable when callable.Params.Length = 2 ->
            // Wrapper-based emission. The fold's
            // accumulator and the wrapper agree on type -- both come
            // from the array's elem type via the inferReduce
            // unification -- so the call `__r = __wrap(__r, arr[i])`
            // type-checks without narrowing/conversion warnings.
            let (wrapperCode, wname) = genCallableWrapper ctx.VarNames name callable
            let wrapperLines = wrapperCode |> List.map (fun s -> ind + s)
            // Seed and loop start: without init, seed = arr[0], fold from 1;
            // with init, seed = init, fold over ALL elements from 0.
            let (seedStr, loopStart) =
                match initExpr with
                | Some initE -> (exprToCppCtx ctx initE, "0")
                | None -> (elemAt "0", "1")
            match pathAOp, (if ompThreadsOn then parallelFold else None) with
            | Some (op, redOp), _ when not ompThreadsOn ->
                // Path A, THREADS SUPPRESSED. The team is gone; the vectorized
                // reduction stays, in the shared `BLADE_OMP_SIMD_REDUCTION`
                // spelling (portable to pre-4.0 OpenMP, where it expands to
                // nothing and the loop below is the plain serial chain).
                //
                // `ompApiUsedCell` is deliberately NOT set: this form calls no
                // omp_* runtime function, so the program must not acquire an
                // <omp.h> dependency it does not have -- which is also what lets
                // "serial emission contains no `omp_get_`" be checkable by
                // grepping the output.
                elemErrCode @ guardLines @ [
                    $"{ind}// reduce: comm-licensed reduction (builtin '{(binOpToCpp op)}'), flat sweep"
                    $"{ind}// [omp] requested but emitted serial: {(ompThreadsSuppressedReason ())}"
                    $"{ind}const size_t {rnName} = {boundExpr};"
                    $"{ind}{elemStr} {name} = {seedStr};"
                ]
                @ (fpReassocSimdStmts redOp name "__ri" loopStart rnName [] elemAt
                   |> List.map (fun s -> ind + s))
            | Some (op, redOp), _ ->
                // Path A. No wrapper: the reduction clause requires the update
                // statement to have the `x = x op expr` shape, so the builtin op
                // is emitted directly (and the wrapper would be dead code).
                (ompApiUsedCell ()).Value <- true
                elemErrCode @ guardLines @ [
                    $"{ind}// reduce: comm-licensed OpenMP reduction (builtin '{(binOpToCpp op)}'), flat sweep"
                    $"{ind}const size_t {rnName} = {boundExpr};"
                    $"{ind}{elemStr} {name} = {seedStr};"
                    $"{ind}#pragma omp parallel for simd reduction({redOp}:{name})"
                    $$"""{{ind}}for (size_t __ri = {{loopStart}}; __ri < {{rnName}}; __ri++) {"""
                    $"""{ind}    {name} = {name} {(binOpToCpp op)} {(elemAt "__ri")};"""
                    $"{ind}}}"
                ]
            | None, Some _ ->
                // Path B. Chunk bounds and combine order are fixed functions of
                // omp_get_max_threads(), so a fixed OMP_NUM_THREADS reproduces
                // bit-for-bit. The `__rcnt > 0` guard is load-bearing: a chunk
                // seeds from its first element, and with nothing to fold the
                // result is the seed alone (which is exactly the serial answer
                // for an empty range, init form included).
                (ompApiUsedCell ()).Value <- true
                let partName = $"__rpart_{name}"
                // K independent LANE accumulators inside each chunk. Lane l owns
                // __rlo+l, __rlo+l+K, __rlo+l+2K, ... and seeds from its own
                // first element (no identity needed); lanes fold into lane 0 in
                // fixed ascending order, so the emitted order is a fixed
                // function of (team size, K) -- determinism unchanged.
                //
                // Lanes are SEPARATE NAMED LOCALS, not an array: an array kept
                // live by the seed/tail loops can defeat scalar replacement,
                // reintroducing the load-modify-store latency the lanes exist
                // to remove. Below K elements the chunk falls back to a plain
                // serial fold, byte-identical to the lane form for len < K.
                let kLanes = foldLaneCount
                let laneName (l: int) = $"__rlane{l}"
                let laneBody =
                    [ $$"""{{ind}}            if (__rhi - __rlo < (size_t){{kLanes}}) {"""
                      $"""{ind}                {elemStr} __racc = {(elemAt "__rlo")};"""
                      $$"""{{ind}}                for (size_t __ri = __rlo + 1; __ri < __rhi; __ri++) {"""
                      $"""{ind}                    __racc = {wname}(__racc, {(elemAt "__ri")});"""
                      $$"""{{ind}}                }"""
                      $"{ind}                {partName}[__rt] = __racc;"
                      $$"""{{ind}}            } else {""" ]
                    @ [ for l in 0 .. kLanes - 1 ->
                          $"""{ind}                {elemStr} {(laneName l)} = {(elemAt (sprintf "__rlo + %d" l))};""" ]
                    @ [ $"{ind}                size_t __ri = __rlo + {kLanes};"
                        $$"""{{ind}}                for (; __ri + {{kLanes}} <= __rhi; __ri += {{kLanes}}) {""" ]
                    @ [ for l in 0 .. kLanes - 1 ->
                          $"""{ind}                    {(laneName l)} = {wname}({(laneName l)}, {(elemAt (sprintf "__ri + %d" l))});""" ]
                    @ [ $$"""{{ind}}                }""" ]
                    // Tail: at most K-1 elements remain, and they belong to
                    // lanes 0..K-2 in order (lane K-1 can never receive one).
                    @ [ for l in 0 .. kLanes - 2 ->
                          $$"""{{ind}}                if (__ri < __rhi) { {{(laneName l)}} = {{wname}}({{(laneName l)}}, {{(elemAt "__ri")}}); __ri++; }""" ]
                    @ [ for l in 1 .. kLanes - 1 ->
                          $"{ind}                {(laneName 0)} = {wname}({(laneName 0)}, {(laneName l)});" ]
                    @ [ $"{ind}                {partName}[__rt] = {(laneName 0)};"
                        $$"""{{ind}}            }""" ]
                elemErrCode @ guardLines @ wrapperLines @ [
                    $"{ind}// reduce: comm-licensed parallel fold, contiguous chunked partials ({kLanes}-lane)"
                    $"{ind}const size_t {rnName} = {boundExpr};"
                    $"{ind}{elemStr} {name} = {seedStr};"
                    $"{ind}{{"
                    $"{ind}    const size_t __rlo0 = {loopStart};"
                    $"{ind}    const size_t __rcnt = ({rnName} > __rlo0) ? ({rnName} - __rlo0) : (size_t)0;"
                    $$"""{{ind}}    if (__rcnt > 0) {"""
                    $"{ind}        int __rT = omp_get_max_threads();"
                    $"{ind}        if ((size_t)__rT > __rcnt) __rT = (int)__rcnt;"
                    $"{ind}        if (__rT < 1) __rT = 1;"
                    // num_threads(n) is a REQUEST, not a guarantee: dynamic
                    // adjustment, or landing inside an enclosing parallel region
                    // with nesting off, can hand back a SMALLER team. Splitting
                    // by the requested count would then leave the tail chunks
                    // uncomputed and silently drop elements, so the split reads
                    // the team size the region actually got, and the combine
                    // reads it back through a slot only thread 0 writes (the
                    // region's implicit barrier publishes it).
                    $"{ind}        int __rTact = __rT;"
                    $"{ind}        {elemStr}* {partName} = new {elemStr}[__rT];"
                    $"{ind}        #pragma omp parallel num_threads(__rT)"
                    $$"""{{ind}}        {"""
                    $"{ind}            const int __rnt = omp_get_num_threads();"
                    $"{ind}            const int __rt = omp_get_thread_num();"
                    $"{ind}            if (__rt == 0) __rTact = __rnt;"
                    $"{ind}            const size_t __rlo = __rlo0 + (__rcnt * (size_t)__rt) / (size_t)__rnt;"
                    $"{ind}            const size_t __rhi = __rlo0 + (__rcnt * ((size_t)__rt + 1)) / (size_t)__rnt;"
                ] @ laneBody @ [
                    $$"""{{ind}}        }"""
                    $"{ind}        for (int __rt = 0; __rt < __rTact; __rt++) {name} = {wname}({name}, {partName}[__rt]);"
                    $"{ind}        delete[] {partName};"
                    $$"""{{ind}}    }"""
                    $"{ind}}}"
                ]
            | None, None when fpReassocEnabled () && foldReorderLicensed callable ->
                // BLADE_FP_REASSOC. The user did not write `omp`, so there are
                // no threads here and none are added: this is Path B's flat
                // shape with the thread chunking removed -- ONE chunk covering
                // [loopStart, bound), swept by K lanes and combined in fixed
                // ascending order. The knob supplies the reproducibility opt-in
                // that `omp` supplies on the parallel paths; the LICENCE is the
                // ordinary `foldReorderLicensed` (a recognised builtin body, or
                // declared comm), so an unlicensed user kernel still falls to
                // the serial arm below with the knob on.
                //
                // LANES, NOT `omp simd reduction`, AND THAT IS A MEASUREMENT.
                // The simd form (`fpReassocSimdStmts`) is available here -- the
                // licence class that reaches this arm is usually a builtin `+`
                // -- and it is what sites `IRProdSum` and `genReduceComputeBinding`
                // now emit. It LOSES here, at both ends of the memory hierarchy
                // (sumred = `reduce(x, (+))`, f64, OMP_NUM_THREADS=1, medians
                // over 3 processes x 5 reps, `bench\blade`):
                //
                //     n = 10000019 (bandwidth-bound)  serial 8.02 ms
                //                                     8 lanes 2.78 ms  (2.88x)
                //                                     simd    3.10 ms  (2.59x)
                //     n = 300007 (cache-resident)     serial  223 us
                //                                     8 lanes 29.9 us  (7.45x)
                //                                     simd    56.7 us  (3.93x)
                //
                // and the cache-resident gap -- 1.9x, entirely outside the
                // spread -- is the one that says why. A ONE-STREAM add chain is
                // register-cheap, so all 8 named lanes stay in registers and GCC
                // additionally packs and unrolls them (the emitted lane form
                // itself vectorizes: 2 `vaddpd` per iteration, 8 doubles). The
                // simd form hands the compiler ONE 4-wide accumulator and it
                // stops there, so it runs 4 chains where the lanes run 8+.
                // Where the simd form wins is the shapes with more per-element
                // work, where the lanes spill instead (see the two sites above).
                //
                // DETERMINISM, on this arm, is therefore still the STRONG
                // property the knob originally promised: no omp_get_max_threads,
                // no team, no pragma at all, so the answer is a fixed function
                // of the data and K alone and reproduces across toolchains.
                // One operand stream (a materialized rank-1 array), so the
                // measured anchor applies directly -- see laneCountForStreams.
                let kLanes = foldLaneCount
                let (laneStmts, resultLane) =
                    fpReassocLaneStmts kLanes elemStr "__rlane" "__ri" "__rlo" "__rhi" elemAt
                        (fun acc rhs -> $"{acc} = {wname}({acc}, {rhs});")
                elemErrCode @ guardLines @ wrapperLines @ pathBSuppressedNote @ [
                    $"{ind}// reduce: accumulator loop, eager ({kLanes}-lane, BLADE_FP_REASSOC)"
                    $"{ind}{elemStr} {name} = {seedStr};"
                    $"{ind}{{"
                    $"{ind}    const size_t __rlo = {loopStart};"
                    $"{ind}    const size_t __rhi = {boundExpr};"
                    $"{ind}    const size_t __rcnt = (__rhi > __rlo) ? (__rhi - __rlo) : (size_t)0;"
                    $$"""{{ind}}    if (__rcnt < (size_t){{kLanes}}) {"""
                    // Below K elements there is nothing to interleave, so this
                    // is the serial chain verbatim -- same order, same bits.
                    $$"""{{ind}}        for (size_t __ri = __rlo; __ri < __rhi; __ri++) {"""
                    $"""{ind}            {name} = {wname}({name}, {(elemAt "__ri")});"""
                    $$"""{{ind}}        }"""
                    $$"""{{ind}}    } else {"""
                ] @ (laneStmts |> List.map (fun s -> ind + "        " + s)) @ [
                    $"{ind}        {name} = {wname}({name}, {resultLane});"
                    $$"""{{ind}}    }"""
                    $"{ind}}}"
                ]
            | None, None ->
            elemErrCode @ guardLines @ wrapperLines @ pathBSuppressedNote @ [
                $"{ind}// reduce: accumulator loop, eager"
                $"{ind}{elemStr} {name} = {seedStr};"
                $$"""{{ind}}for (size_t __ri = {{loopStart}}; __ri < {{boundExpr}}; __ri++) {"""
                $"""{ind}    {name} = {wname}({name}, {(elemAt "__ri")});"""
                $"{ind}}}"
            ]
        | _ ->
            let errLines = codegenError ctx ind "reduce: kernel must resolve to a binary callable (typechecker or IR bug if not)"
            elemErrCode @ errLines
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')

and genReduceComputeBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (compExpr: IRExpr) (kernelExpr: IRExpr) (seedExpr: IRExpr) : string list * CodeGenContext =
    // HOISTED-OPERAND PRELUDE. The lift pass pulls a synthesized loop
    // application out of a combinator's `Arrays` slot into its own let
    // (`liftChildIncludingLoopApp`), so a deferred computation whose operand is
    // a broadcast -- `reduce(exp <@> (i * w * ts), (+))`, i.e. units/065's shape
    // once the enclosing kernel body is emitted through the lifted form -- arrives
    // here as `IRLet(v, IRApp(IRObjectFor ...), IRApplyCombinator ...)`, not as a
    // bare combinator. The leaf check below then saw an IRLet, not an apply, and
    // refused a perfectly well-formed fold.
    //
    // Emit those bindings as a statement prelude (genBinding is the same
    // statement-form dispatch a body-level let uses) and fold over the
    // combinator underneath, with the hoisted names in scope. Only ever turns a
    // refusal into code: an IRLet-wrapped computation had no other outcome.
    let rec peelCompLets (accCtx: CodeGenContext) (accLines: string list) (e: IRExpr) =
        match e with
        | IRLet (id, value, body) ->
            let tempBinding = {
                Id = id; Name = $"__v{id}"; Type = inferExprType value
                Value = value; IsConst = false; IsMutable = true
            }
            let (code, nextCtx) = genBinding accCtx tempBinding builder
            peelCompLets nextCtx (accLines @ code) body
        | _ -> (accCtx, accLines, e)
    let (ctx, prelude, compExpr) = peelCompLets ctx [] compExpr
    let (code, outCtx) = genReduceComputeBindingCore ctx binding builder compExpr kernelExpr seedExpr
    (prelude @ code, outCtx)

and genReduceComputeBindingCore (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (compExpr: IRExpr) (kernelExpr: IRExpr) (seedExpr: IRExpr) : string list * CodeGenContext =
    // THE JOIN ENCODING (IR.fs, IRReduceCompute): tuple kernel + tuple seed =
    // one fold per leg. Routed to its own emitter, because everything below
    // this line assumes ONE fold callable shared by every leaf.
    match kernelExpr, seedExpr with
    | IRTuple kernels, IRTuple seeds when kernels.Length = seeds.Length && kernels.Length >= 2 ->
        genReduceJoinCore ctx binding builder compExpr kernels seeds
    | _ ->
    let ind = indentStr ctx
    let name = bindingCppName binding
    // The fused reduction terminal: reduce(deferred, op[, init]). Fold every
    // cell of a deferred computation -- a single unforced apply, or an <&!>
    // fusion tree of them -- into scalar accumulator(s) through the fold
    // kernel's wrapper, WITHOUT materializing any output array. One loop
    // nest total; a fusion tree gets one accumulator per leaf and packs a
    // flat tuple (mirroring genFusionTree's make_tuple convention, so tuple
    // destructuring projects with flat get<i> indices).
    let rec resolveDeferred e =
        match e with
        | IRVar (id, _) ->
            (match Map.tryFind id ctx.DeferredComputations with
             | Some d -> resolveDeferred d
             | None -> e)
        | _ -> e
    let rec collectLeaves e =
        match resolveDeferred e with
        | IRFusion (l, r) -> collectLeaves l @ collectLeaves r
        | other -> [other]
    let leaves = collectLeaves compExpr
    let infos = leaves |> List.choose (function IRApplyCombinator i -> Some i | _ -> None)
    let ctx' = addVarName binding.Id name ctx
    if infos.IsEmpty || infos.Length <> leaves.Length then
        (codegenError ctx ind "reduce over a deferred computation requires unforced method_for/object_for applications at every leaf (typechecker or IR bug if not)", ctx')
    else
        // The fold bypasses genApplyCombinator's special input paths
        // (ragged peel, grouped, compound, CUDA) -- reject what they handle.
        let unsupportedInput =
            infos |> List.exists (fun info ->
                info.ArrayTypes |> List.exists (fun at ->
                    at.IndexTypes |> List.exists (fun ix ->
                        isRaggedFamilyKind ix.IxKind || ix.IxKind = IxKDepInner
                        || ix.IxKind = IxKGroupOuter || ix.IxKind = IxKCompound)))
        if unsupportedInput then
            (codegenError ctx ind "reduce over a deferred computation is not supported for ragged/grouped/compound inputs yet -- force with |> compute and reduce the array", ctx')
        else
            match resolveCallable kernelExpr with
            | Some callable when callable.Params.Length = 2 ->
                let (wrapperCode, wname) = genCallableWrapper ctx.VarNames name callable
                let wrapperLines = wrapperCode |> List.map (fun s -> ind + s)
                // Accumulator C++ type: the fold callable's return type (the
                // checker unified it with every leaf's element type).
                let elemStr =
                    match callable.RetType with
                    | IRTScalar et -> primTypeToCpp et
                    | t -> irTypeToCpp t
                let seedStr = exprToCppCtx ctx seedExpr
                let arrayNamesOf (info: ApplyInfo) =
                    info.Arrays |> List.mapi (fun i arr ->
                        match arr with
                        | IRVar (id, _) -> Map.tryFind id ctx.VarNames |> Option.defaultValue ($"arr{i}")
                        | IRRange _ -> $"__range{i}"
                        | IRVirtualReverse _ -> $"__rev{i}"
                        | _ -> $"arr{i}")
                let foldCg (info: ApplyInfo) (accName: string) =
                    // S2 routing, same rule as the single-kernel site.
                    let cg = routeKernelBodyThroughCall info (buildLoopNestCodeGen info (arrayNamesOf info) accName builder)
                    { cg with OutputType = callable.RetType; FoldWrapper = Some wname }
                match infos with
                | [single] ->
                    let cg0 = foldCg single name
                    // Path B: a comm-licensed fold kernel that asked for omp gets
                    // the outermost level chunked across an explicit team. This
                    // covers exactly the single-leaf case; the fused tree below
                    // (one accumulator per leaf) stays serial.
                    //
                    // The outermost binding must be RECTANGULAR -- no bound
                    // dependencies, no strict offset -- so [0, extent) really is
                    // the chunked range. Inner levels may be anything, including
                    // triangular: each thread runs the full inner nest for the
                    // outer indices it owns, exactly as the serial nest would.
                    //
                    // `ompThreadEmissionEnabled` is DELIBERATELY NOT consulted
                    // here. The knob suppresses this plan at its CONSUMER
                    // (`genLoopNestStreamed`, which is where the team would be
                    // opened), not at this producer, because `FoldChunk.IsSome`
                    // is also read as a FACT by the dispatch patterns
                    // (`LinAlgPatterns` L1/L2/L3 decline on it) and by the flat
                    // elementwise fast path. Clearing it here would make a
                    // serial-emission build take DIFFERENT ROUTES, not just emit
                    // different text -- e.g. handing an omp-licensed fold to a
                    // BLAS dispatch it declines today. The knob's invariant is
                    // that route selection is identical in both modes.
                    let chunkable =
                        callable.IsOmpParallel
                        && foldReorderLicensed callable
                        && (match cg0.Bindings with
                            | head :: _ -> head.BoundDependencies.IsEmpty && head.StrictOffset = 0
                            | [] -> false)
                    let cg =
                        if chunkable then
                            { cg0 with FoldChunk = Some { ElemCpp = elemStr; Tag = sanitizeCppName name } }
                        else cg0
                    // L1 LinAlg dispatch. `reduce(<unforced zip under *>, (+))`
                    // over two rank-1 f64 pools IS a dot product; the pattern owns
                    // the recognition, this site only spells the call. With the
                    // BLAS gate off (the default) `shimEntryPoint` yields None and
                    // the fold nest below is emitted unchanged.
                    //
                    // PRECEDENCE, enforced by the fact passed in: an `omp`-licensed
                    // fold kernel keeps the Path B chunked parallel fold. The
                    // pattern declines on `FoldRequestedOmp`, so a licensed fold
                    // can never be silently serialized by this route.
                    //
                    // Stands down in omp test mode for the same reason the flat
                    // path does -- the coverage instrumentation reports on the nest
                    // that ran, and a dispatched call has no nest.
                    let dotCall =
                        if ompTestModeEnabled () then None
                        else
                            let facts : Blade.LinAlgPatterns.DotFoldFacts =
                                { FoldIsBuiltinAdd = (foldKernelBuiltinOp callable = Some IRAdd)
                                  FoldRequestedOmp = callable.IsOmpParallel }
                            match (0, facts, single.ArrayTypes, cg) with
                            | Blade.LinAlgPatterns.BlasL1 call ->
                                Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas call
                                |> Option.map (fun entry -> (call, entry))
                            | _ -> None
                    // BLADE_FP_REASSOC for the reduce-over-DEFERRED-COMPUTATION
                    // nest -- the shape `reduce(<unforced zip>, (+))` (the dot
                    // benchmark) lowers through, and the last serial arm the knob
                    // did not reach. Same K-lane shape as every other
                    // `fpReassocEnabled ()` site (fpReassocLaneStmts owns it), so
                    // the numeric answer cannot drift between them.
                    //
                    // What is different here is the ELEMENT: the flat forms fold
                    // `A[__ri + l]`, a subscript; this one folds the nest's whole
                    // per-iteration BODY at `__ri + l`. That is only meaningful
                    // when the body is a PURE EXPRESSION of the level's index --
                    // which is exactly what the gate below establishes, and what
                    // makes K independent evaluations of it legitimate. The body
                    // is rendered ONCE into a local `[&](size_t __i0) -> T`
                    // lambda and the lanes call it at their own indices; the K
                    // copies are the inliner's, not the emitter's. Rendering once
                    // is not only smaller text: the kernel emitters are not all
                    // pure (some register collected definitions), so emitting the
                    // same body twice is a hazard the single render removes.
                    //
                    // DECLINED, deliberately, in v1 (each falls through to the
                    // unchanged serial nest below):
                    //   * MULTI-LEVEL nests. A lane would have to stride the
                    //     outermost level while re-running the whole inner nest,
                    //     which is a different (and much larger) unit of work
                    //     than a body expression -- and the inner nest may be
                    //     triangular, so lane l and lane l+1 do not even do equal
                    //     work. The comm-licensed multi-level case already has
                    //     its own answer (FoldChunk, Path B).
                    //   * Non-rectangular / fused / tabulated (compound, sparse)
                    //     head levels: `__ri + l` is not an element of the
                    //     iteration space.
                    //   * Reynolds bodies, MPI slabs, and halo-window slots (the
                    //     carousel body is stateful across iterations by
                    //     construction, so it is not a function of the index).
                    //   * `FoldChunk` (an `omp`-licensed fold): threads and lanes
                    //     are separate opt-ins and Path B already owns that arm.
                    //   * The FUSED TREE (several leaves, several accumulators) --
                    //     handled in the branch below, which stays serial.
                    //
                    // Licence is the ordinary one: `foldReorderLicensed callable`.
                    // On this path `(+)` resolves to a two-parameter callable whose
                    // body is exactly `p0 + p1`, so `foldKernelBuiltinOp` grants it
                    // outright (the same fact `dotCall` reads as
                    // `FoldIsBuiltinAdd`); an unlicensed user fold kernel stays
                    // serial with the knob on.
                    // A FUNCTION, not a value: it renders the kernel body, and the
                    // BLAS-dispatch arm below emits no body at all -- computing it
                    // eagerly would render (and discard) one there.
                    let laneLines () : string list option =
                        let laneable (lvl: LoopIndexBinding) =
                            lvl.BoundDependencies.IsEmpty && lvl.StrictOffset = 0
                            && lvl.FusedRank.IsNone
                            && (match lvl.Extent with
                                | IRCompoundMask _ | IRSparseKeys _ -> false
                                | _ -> true)
                            && not cg.HasReynolds && not cg.MpiSlab
                            && (lvl.Elements
                                |> List.forall (fun e ->
                                    match e.SlotTag with
                                    | Some t -> not (t.StartsWith "__halowin")
                                    | None -> true))
                        match cg.Bindings with
                        | [lvl] when fpReassocEnabled () && foldReorderLicensed callable
                                     && cg.FoldChunk.IsNone && laneable lvl ->
                            // The nest's own element peels and kernel expression,
                            // built exactly as genLoopNestStreamed builds them for
                            // one level. `restrictPeelSites` is provably EMPTY for
                            // a single-level nest (its chain rule needs a deeper
                            // level to peel into), so `false` here is not a
                            // simplification -- it is the value that site computes.
                            // RESTRICT SOURCE ALIASES. The peel below would read
                            // each rank-1 operand as `<name>[i]` through the
                            // Array struct, and behind that subscript the
                            // vectorizer cannot prove the sweep is
                            // dependence-free (the pool pointers could alias the
                            // accumulator's storage as far as it knows). Reading
                            // through raw restrict pointers to the pool bases is
                            // what lets it fire. Licence is Phase 1's: these
                            // operands are READ-ONLY here (the fold writes only
                            // its scalar accumulator), and read-only sharing --
                            // including `zip(x, x)` binding one array to two
                            // slots -- is permitted under C's restrict
                            // semantics; only written-through pointers must be
                            // exclusive.
                            //
                            // HISTORY, because it is the measurement that chose
                            // the form below. These aliases were added for the
                            // K-LANE form, whose independent partials live in
                            // named scalars and need GCC's SLP pass to pack them
                            // into vectors. With the aliases SLP did fire -- and
                            // spent the entire gain marshalling the named lanes
                            // through a vunpck/vpermpd permute storm, landing at
                            // PARITY with the serial chain (dot shape, n = 1e7,
                            // 1 thread: 8.63 ms serial vs 8.66 ms in 8 lanes).
                            // The simd form asks for the partials in vector
                            // registers directly, so nothing is ever marshalled;
                            // the same aliases then buy the whole win.
                            let mutable srcAliases : Map<string, string> = Map.empty
                            let mutable aliasDecls : string list = []
                            for elem in lvl.Elements do
                                match elem.Virtual with
                                | RealArray when elem.ArrayRank = 1
                                                 && not (Map.containsKey elem.ArrayName srcAliases) ->
                                    let alias = $"__rsrc{Map.count srcAliases}"
                                    srcAliases <- Map.add elem.ArrayName alias srcAliases
                                    aliasDecls <- aliasDecls
                                        @ [ $"const {(irTypeToCpp elem.ArrayElemType)}* BLADE_RESTRICT {alias} = {elem.ArrayName}.data;" ]
                                | _ -> ()
                            let mutable currentNames : Map<int, string> = Map.empty
                            let mutable paramFinalNames : Map<IRId, string> = Map.empty
                            let mutable declaredNames : Map<string, string> = Map.empty
                            let mutable peels : string list = []
                            for elem in lvl.Elements do
                                let currentName =
                                    Map.tryFind elem.ArrayPosition currentNames
                                    |> Option.defaultValue
                                           (Map.tryFind elem.ArrayName srcAliases
                                            |> Option.defaultValue elem.ArrayName)
                                let (peelCode, newName) = genElementBindingPeel false lvl elem currentName
                                // Same dedup rule as the nest: zipping an array
                                // WITH ITSELF puts two slots on one declaration.
                                if Map.tryFind newName declaredNames <> Some peelCode then
                                    peels <- peels @ [peelCode]
                                declaredNames <- Map.add newName peelCode declaredNames
                                currentNames <- Map.add elem.ArrayPosition newName currentNames
                                match elem.Virtual with
                                | VirtualRange _ | VirtualReverse ->
                                    paramFinalNames <- Map.add elem.ParamVarId elem.ParamName paramFinalNames
                                | RealArray ->
                                    paramFinalNames <- Map.add elem.ParamVarId newName paramFinalNames
                            let nameMap =
                                paramFinalNames
                                |> Map.fold (fun acc k v -> Map.add k v acc) ctx.VarNames
                            let nameMap =
                                cg.Captures
                                |> List.fold (fun acc c ->
                                       if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc)
                                   nameMap
                            let bodyExpr =
                                (genKernelExprWithReynolds cg.KernelExpr cg.KernelParams
                                     false false nameMap paramFinalNames).CppExpr
                            // OPERAND STREAMS the body reads: the distinct real
                            // arrays peeled at this level (a dot reads two; a
                            // self-dot `zip(x, x)` reads one pointer and counts as
                            // one). Virtual range/reverse slots are index
                            // arithmetic, not memory streams, and are not counted.
                            let streams =
                                lvl.Elements
                                |> List.filter (_.Virtual.IsRealArray)
                                |> List.map (_.ArrayName)
                                |> List.distinct
                                |> List.length
                            let boundStr = genLoopBoundExpr (compoundArrayNamesOf cg.Bindings) lvl
                            match fpReassocSimdOp callable elemStr with
                            | Some opStr ->
                                // SIMD FORM, and the site the two forms differ
                                // most sharply at. The peels go INSIDE the
                                // vectorized loop (the `prelude`), so the body
                                // expression is written straight into it -- no
                                // `__rbody` lambda, because there is only ONE
                                // index to evaluate it at and nothing to
                                // re-render.
                                //
                                // MEASURED (dot shape, `reduce(<unforced zip>,
                                // (+))`, f64, OMP_NUM_THREADS=1, medians over 3
                                // processes x 5 reps, `bench\blade`):
                                //
                                //   n = 10000019 (bandwidth-bound)
                                //       serial 8.82 ms  8 lanes 9.32 ms (0.95x)
                                //                       simd    5.38 ms (1.64x)
                                //   n = 300007 (cache-resident)
                                //       serial  224 us  8 lanes  229 us (0.97x)
                                //                       simd    58.6 us (3.81x)
                                //
                                // The lanes are worth NOTHING at this shape in
                                // either regime -- they are the permute storm
                                // described above -- and 5.4 ms is what a
                                // hand-written packed-FMA dot reaches on this
                                // machine. The whole gain was sitting behind the
                                // marshalling.
                                //
                                // NO short fallback and no `streams` count: the
                                // vectorizer emits its own scalar remainder, and
                                // the register-budget rule the lane count exists
                                // to apply (`laneCountForStreams`) is about
                                // NAMED scalar accumulators -- it has nothing to
                                // say about vector-register partials, whose
                                // width the compiler picks.
                                Some (
                                    [ $"""{ind}// reduce over computation: accumulator loop (omp simd reduction, BLADE_FP_REASSOC, {streams} operand stream{(if streams = 1 then "" else "s")})"""
                                      $"{ind}{{"
                                      $"{ind}    const size_t __rhi = {boundStr};" ]
                                    @ (aliasDecls |> List.map (fun s -> ind + "    " + s))
                                    @ (fpReassocSimdStmts opStr name lvl.IndexName "0" "__rhi"
                                           peels (fun _ -> bodyExpr)
                                       |> List.map (fun s -> ind + "    " + s))
                                    @ [ $"{ind}}}" ])
                            | None ->
                            // K-LANE FORM: the fallback for a licence the simd
                            // arm cannot spell (a `comm`-declared kernel, whose
                            // combine is a call, or a non-scalar element type).
                            // Here the body IS evaluated at K different indices,
                            // so it is rendered ONCE into a local
                            // `[&](size_t) -> T` lambda and the lanes call it at
                            // their own indices; the K copies are the inliner's,
                            // not the emitter's. Rendering once is not only
                            // smaller text: the kernel emitters are not all pure
                            // (some register collected definitions), so emitting
                            // the same body twice is a hazard the single render
                            // removes.
                            let kLanes = laneCountForStreams streams
                            let bodyAt (i: string) = $"__rbody({i})"
                            let (laneStmts, resultLane) =
                                fpReassocLaneStmts kLanes elemStr "__rlane" "__ri" "0" "__rhi" bodyAt
                                    (fun acc rhs -> $"{acc} = {wname}({acc}, {rhs});")
                            Some (
                                [ $"""{ind}// reduce over computation: accumulator loop ({kLanes}-lane, BLADE_FP_REASSOC, {streams} operand stream{(if streams = 1 then "" else "s")})"""
                                  $"{ind}{{"
                                  $"{ind}    const size_t __rhi = {boundStr};" ]
                                @ (aliasDecls |> List.map (fun s -> ind + "    " + s))
                                @ [ $"""{ind}    auto __rbody = [&](size_t {lvl.IndexName}) -> {elemStr} {{ {(peels |> String.concat " ")} return {bodyExpr}; }};"""
                                    $$"""{{ind}}    if (__rhi < (size_t){{kLanes}}) {"""
                                    // Below K elements there is nothing to
                                    // interleave: the serial chain verbatim -- the
                                    // same bodies, folded in the same ascending
                                    // order into the same seeded accumulator, hence
                                    // the same double the nest below produces.
                                    $$"""{{ind}}        for (size_t __ri = 0; __ri < __rhi; __ri++) {"""
                                    $"{ind}            {name} = {wname}({name}, __rbody(__ri));"
                                    $$"""{{ind}}        }"""
                                    $$"""{{ind}}    } else {""" ]
                                @ (laneStmts |> List.map (fun s -> ind + "        " + s))
                                @ [ $"{ind}        {name} = {wname}({name}, {resultLane});"
                                    $$"""{{ind}}    }"""
                                    $"{ind}}}" ])
                        | _ -> None
                    // The fold nest as emitted today, and the lane form when the
                    // knob and the gate above both admit it. Shared by both
                    // non-dispatch arms so they cannot drift.
                    let foldNestLines () =
                        wrapperLines
                        @ [$"{ind}{elemStr} {name} = {seedStr};"]
                        @ (match laneLines () with
                           | Some ls -> ls
                           | None -> genLoopNest cg ctx.VarNames ctx.Indent)
                    let code =
                        match dotCall with
                        | Some (call, entry) ->
                            let nameOf role =
                                call.NestOperands
                                |> List.tryPick (fun (r, src) ->
                                    if r <> role then None
                                    else match src with
                                         | Blade.LinAlgPatterns.FromNestArray n -> Some n
                                         | _ -> None)
                            match nameOf Blade.LinAlgPatterns.RoleA,
                                  nameOf Blade.LinAlgPatterns.RoleB with
                            | Some xName, Some yName ->
                                (linalgUsedCell ()).Value <- true
                                let nExtent =
                                    genLoopBoundExpr (compoundArrayNamesOf cg.Bindings)
                                                     (List.head cg.Bindings)
                                // BLOCK comment -- see materializeGramForm's note.
                                [ $"{ind}/* linalg dispatch: dot({xName}, {yName}) = reduce({xName} * {yName}, (+)) */ {elemStr} {name} = {entry}({nExtent}, {xName}.data, {yName}.data, {seedStr});" ]
                            | _ -> foldNestLines ()
                        | None -> foldNestLines ()
                    (code, ctx')
                | _ :: _ ->
                    // Fused tree: ONE merged nest, one scalar accumulator per
                    // leaf. Each leaf accumulates at its OWN depth from its
                    // OWN arrays (genFusedLoopNest staggers mixed-arity
                    // trees), so incompatible loop structures are a loud
                    // diagnostic, never silently-shared loops.
                    let leafNames = infos |> List.mapi (fun i _ -> $"{name}_{i}")
                    let leafCgs = infos |> List.mapi (fun i info -> foldCg info leafNames.[i])
                    // A fused fold writes shared scalar accumulators, which race
                    // under any parallel/device backend (omp reduction clauses
                    // and device reductions are the future upgrade). Reject a
                    // device leaf loudly; host leaves fold serially.
                    let deviceLeaf = infos |> List.tryPick (fun info ->
                        match classifyLeafBackend info with
                        | BkCuda _ -> Some "cuda" | BkMpi -> Some "mpi" | _ -> None)
                    match checkMergeCompatible leafCgs, deviceLeaf with
                    | _, Some bk ->
                        (codegenError ctx ind ($"reduce over a fused computation with a {bk} leaf: device/parallel reductions over a fused tree are not supported yet -- force the leaf with |> compute and reduce the array"), ctx')
                    | Error reason, _ ->
                        (codegenError ctx ind ($"reduce over a fused computation: cannot fuse the leaves into one loop nest: {reason}"), ctx')
                    | Ok _, None ->
                        let declCode =
                            leafNames |> List.map (fun ln -> $"{ind}{elemStr} {ln} = {seedStr};")
                        let (sm, sp, sNew) = streamedNestSetup ctx.StreamedArrays ind leafCgs
                        registerStreamBufDecls sNew
                        let loopCode = sp @ genFusedLoopNestStreamed sm leafCgs ctx.VarNames ctx.Indent false None
                        let tupleLine = $"""{ind}auto {name} = std::make_tuple({(leafNames |> String.concat ", ")});"""
                        // Destructure sub-bindings resolve through TupleChildren
                        // straight to the accumulator names (the fusion-tree
                        // convention) -- never through std::get on the nested type.
                        let ctxOut = { ctx' with TupleChildren = Map.add name leafNames ctx'.TupleChildren }
                        (wrapperLines @ declCode @ loopCode @ [tupleLine], ctxOut)
                | [] ->
                    (codegenError ctx ind "reduce over a deferred computation: no leaves (unreachable)", ctx')
            | _ ->
                (codegenError ctx ind "reduce over a deferred computation: fold kernel must resolve to a binary callable (typechecker or IR bug if not)", ctx')



/// REDUCTION JOIN (docs/plan-reduction-joins.md): k reductions, one traversal,
/// a flat tuple of k accumulators. The shape is the fused tree's -- one merged
/// nest, one scalar accumulator per leaf, `make_tuple` at the end -- with two
/// things the shared-fold tree does not have:
///
///   PER-LEG FOLDS. Each leg brings its own kernel wrapper, its own seed, and
///   its own accumulator type, because a join's legs are independent
///   reductions (`prodsum` folds `(+)` from 0; `reduce(x, max, -inf)` folds a
///   lambda from a user seed) that only agree on WHERE they iterate.
///
///   SHARING BY NAMING. A leg operand that is a named DEFERRED map -- `let ct
///   = cos <@> ph`, never forced -- becomes a per-iteration `const` computed
///   ONCE in the joint nest, and every leg reading that name reads the local.
///   The declaration is the NAME: nothing is deduced about what the legs
///   compute, only that they spell the same binding. A `|> compute`d operand
///   is an array and keeps today's behavior (read from memory).
///
/// The rewrite that makes sharing work is small and local: a consumer leg's
/// deferred operand slot is repointed at the deferred map's OWN source array
/// (so the level's extent and peel name exist in C++, and dedup with the share
/// leaf's peel), and the kernel param that slot bound is substituted for a
/// reference to the deferred binding -- whose emitted name is exactly the
/// shared local's. Nothing else in the nest emitter needs to know.
and genReduceJoinCore (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder)
                      (compExpr: IRExpr) (kernelExprs: IRExpr list) (seedExprs: IRExpr list)
                      : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let rec resolveDeferred e =
        match e with
        | IRVar (id, _) ->
            (match Map.tryFind id ctx.DeferredComputations with
             | Some d -> resolveDeferred d
             | None -> e)
        | _ -> e
    // A leaf that is a NAMED deferred map keeps its id beside the resolved
    // apply: `reduce(e, (+))` next to `prodsum(e, v)` is the sharing
    // declaration read from the leg's side (docs/plans/structural/03, D2),
    // and the id is what ties this leg to the share leaf the slot declares.
    let rec collectLeaves e =
        match e with
        | IRVar (id, _) when Map.containsKey id ctx.DeferredComputations ->
            (match resolveDeferred e with
             | IRFusion _ as f -> collectLeaves f
             | IRApplyCombinator _ as a -> [(a, Some id)]
             | other -> [(other, None)])
        | IRFusion (l, r) -> collectLeaves l @ collectLeaves r
        | other -> [(other, None)]
    let leafPairs = collectLeaves compExpr
    let leaves = leafPairs |> List.map fst
    let leafVarIds = leafPairs |> List.map snd
    let infos = leaves |> List.choose (function IRApplyCombinator i -> Some i | _ -> None)
    let ctx' = addVarName binding.Id name ctx
    if infos.Length <> leaves.Length || infos.Length <> kernelExprs.Length then
        (codegenError ctx ind "reduction join: every leg must resolve to an unforced kernel application (typechecker or IR bug if not)", ctx')
    else
    let unsupportedInput =
        infos |> List.exists (fun info ->
            info.ArrayTypes |> List.exists (fun at ->
                at.IndexTypes |> List.exists (fun ix ->
                    isRaggedFamilyKind ix.IxKind || ix.IxKind = IxKDepInner
                    || ix.IxKind = IxKGroupOuter || ix.IxKind = IxKCompound)))
    if unsupportedInput then
        (codegenError ctx ind "reduction join over ragged/grouped/compound inputs is not supported yet -- force the legs with |> compute and reduce the arrays", ctx')
    else
    let deviceLeaf =
        infos |> List.tryPick (fun info ->
            match classifyLeafBackend info with
            | BkCuda _ -> Some "cuda" | BkMpi -> Some "mpi" | _ -> None)
    match deviceLeaf with
    | Some bk ->
        (codegenError ctx ind ($"reduction join with a {bk} leg: device/parallel reductions over a joined traversal are not supported yet -- force the leg with |> compute and reduce the array"), ctx')
    | None ->
    let callables = kernelExprs |> List.map resolveCallable
    if callables |> List.exists (fun c -> match c with Some cb -> cb.Params.Length <> 2 | None -> true) then
        (codegenError ctx ind "reduction join: every leg's fold kernel must resolve to a binary callable (typechecker or IR bug if not)", ctx')
    else
    let callables = callables |> List.map Option.get
    // ---- SHARED DEFERRED OPERANDS -----------------------------------------
    // A leg operand that resolves (through the binding) to another unforced
    // apply. Distinct ids, in first-use order: that order is the order the
    // share leaves are emitted in, and a shared value must be declared before
    // the leg that reads it.
    let deferredOperand (e: IRExpr) : (IRId * ApplyInfo) option =
        match e with
        | IRVar (id, _) ->
            (match Map.tryFind id ctx.DeferredComputations with
             | Some (IRApplyCombinator dinfo) -> Some (id, dinfo)
             | _ -> None)
        | _ -> None
    // Per leg, in traversal order: the leg's own leaf when it is a named map
    // (collectLeaves), then its operand slots. A slot operand is always a
    // share (that is how a deferred slot gets a C++ definition at all); a
    // leaf name is a share only when the join spells it at least twice -- as
    // a leaf again or as a slot -- so a leg that merely folds a named map on
    // its own keeps today's inlined nest.
    let leafDeferred (v: IRId option) : (IRId * ApplyInfo) option =
        v |> Option.bind (fun id ->
            match Map.tryFind id ctx.DeferredComputations with
            | Some (IRApplyCombinator di) -> Some (id, di)
            | _ -> None)
    let leafShares = leafVarIds |> List.choose leafDeferred
    let slotShares = infos |> List.collect (fun info -> info.Arrays |> List.choose deferredOperand)
    let spelled (id: IRId) =
        (leafShares |> List.filter (fun (i, _) -> i = id) |> List.length)
        + (slotShares |> List.filter (fun (i, _) -> i = id) |> List.length)
    let sharedIds =
        List.zip leafVarIds infos
        |> List.collect (fun (lv, info) ->
            (match leafDeferred lv with
             | Some (id, di) when spelled id >= 2 -> [(id, di)]
             | _ -> [])
            @ (info.Arrays |> List.choose deferredOperand))
        |> List.fold (fun acc (id, di) -> if acc |> List.exists (fun (i, _) -> i = id) then acc else acc @ [(id, di)]) []
    let badShare =
        sharedIds |> List.tryPick (fun (id, di) ->
            let who = Map.tryFind id ctx.VarNames |> Option.defaultValue "<anon>"
            if di.Arrays.IsEmpty then
                Some ($"shared deferred operand '{who}' has no array source to take its traversal from")
            // A deferred map OVER a deferred map: the share leaf would name an
            // operand that has no C++ definition either. One level only.
            elif di.Arrays |> List.exists (fun a -> (deferredOperand a).IsSome) then
                Some ($"shared deferred operand '{who}' reads another deferred computation; only one level of sharing is supported")
            else None)
    match badShare with
    | Some reason ->
        (codegenError ctx ind ($"reduction join: {reason} -- force it with |> compute and join over the array"), ctx')
    | None ->
    let sharedName (id: IRId) = Map.tryFind id ctx.VarNames |> Option.defaultValue ($"__jshr{id}")
    let arrayNamesOf (info: ApplyInfo) =
        info.Arrays |> List.mapi (fun i arr ->
            match arr with
            | IRVar (id, _) ->
                (match sharedIds |> List.tryFind (fun (sid, _) -> sid = id) with
                 // A deferred operand keeps the SOURCE array's name: the slot
                 // has already been repointed at it below.
                 | Some (_, di) ->
                    (match di.Arrays.Head with
                     | IRVar (srcId, _) -> Map.tryFind srcId ctx.VarNames |> Option.defaultValue ($"arr{i}")
                     | _ -> $"arr{i}")
                 | None -> Map.tryFind id ctx.VarNames |> Option.defaultValue ($"arr{i}"))
            | IRRange _ -> $"__range{i}"
            | IRVirtualReverse _ -> $"__rev{i}"
            | _ -> $"arr{i}")
    /// Repoint every deferred operand slot at the deferred map's own leading
    /// source array, so the level's bound and peel name exist in C++ (and
    /// dedup with the share leaf's identical peel). Which slots were repointed
    /// is returned, so the kernel params they bound can be substituted.
    let repoint (info: ApplyInfo) : ApplyInfo * (int * IRId) list =
        let mutable moved = []
        let arrays =
            info.Arrays |> List.mapi (fun pos a ->
                match deferredOperand a with
                | Some (id, di) ->
                    moved <- moved @ [(pos, id)]
                    di.Arrays.Head
                | None -> a)
        let arrayTypes =
            info.ArrayTypes |> List.mapi (fun pos t ->
                match List.tryItem pos info.Arrays |> Option.bind deferredOperand with
                | Some (_, di) -> (match List.tryHead di.ArrayTypes with Some dt -> dt | None -> t)
                | None -> t)
        ({ info with Arrays = arrays; ArrayTypes = arrayTypes }, moved)
    /// Replace every reference to `fromId` with `toExpr`, everywhere.
    let rec substVar (fromId: IRId) (toExpr: IRExpr) (e: IRExpr) : IRExpr =
        match e with
        | IRVar (id, _) when id = fromId -> toExpr
        | ExprShape (children, rebuild) ->
            if children.IsEmpty then e else rebuild (children |> List.map (substVar fromId toExpr))
    // ---- Share leaves ------------------------------------------------------
    let shareCgs =
        sharedIds |> List.map (fun (id, di) ->
            let shName = sanitizeCppName (sharedName id)
            let elemCpp =
                match inferExprType (IRApplyCombinator di) with
                | ArrayElem at -> elemTypeToCpp at.ElemType
                | IRTScalar et -> primTypeToCpp et
                | t -> irTypeToCpp t
            let cg = routeKernelBodyThroughCall di (buildLoopNestCodeGen di (arrayNamesOf di) shName builder)
            { cg with ShareDecl = Some elemCpp })
    // ---- Leg leaves --------------------------------------------------------
    // One wrapper per leg, never deduplicated: two legs folding the same
    // operator get two identical lambdas, which costs a line of text and keeps
    // the wrapper name a pure function of the leg index.
    let wrappers =
        callables |> List.mapi (fun i cb -> genCallableWrapper ctx.VarNames ($"{name}_j{i}") cb)
    let wnames = wrappers |> List.map snd
    let sharedElemTy (id: IRId) =
        match sharedIds |> List.tryFind (fun (sid, _) -> sid = id) with
        | Some (_, di) ->
            (match inferExprType (IRApplyCombinator di) with
             | ArrayElem at -> at.ElemType
             | t -> t)
        | None -> IRTScalar ETFloat64
    let leafNames = infos |> List.mapi (fun i _ -> $"{name}_{i}")
    let leafCgs =
        List.mapi (fun i (info: ApplyInfo) ->
            match leafVarIds.[i] |> Option.bind (fun sid -> sharedIds |> List.tryFind (fun (s, _) -> s = sid)) with
            | Some (sid, di) ->
                // This leg's traversal IS the shared map: the same nest as the
                // share leaf (its peels dedup with the share's), whose per-cell
                // value is the share local itself -- `acc = wrap(acc, <share>)`,
                // never a second spelling of the producer.
                let cg0 = routeKernelBodyThroughCall di (buildLoopNestCodeGen di (arrayNamesOf di) leafNames.[i] builder)
                { cg0 with KernelExpr = IRVar (sid, sharedElemTy sid)
                           OutputType = callables.[i].RetType
                           FoldWrapper = Some wnames.[i] }
            | None ->
            let (info', moved) = repoint info
            let cg0 = routeKernelBodyThroughCall info' (buildLoopNestCodeGen info' (arrayNamesOf info') leafNames.[i] builder)
            // Every param bound by a repointed slot now reads the SHARED local
            // instead of the source cell. The deferred binding's own id renders
            // through ctx.VarNames as exactly the share leaf's output name.
            let kernelExpr =
                moved |> List.fold (fun body (pos, sid) ->
                    let paramIds =
                        cg0.Bindings
                        |> List.collect (_.Elements)
                        |> List.filter (fun el -> el.ArrayPosition = pos)
                        |> List.map (_.ParamVarId)
                    paramIds |> List.fold (fun acc pid ->
                        substVar pid (IRVar (sid, sharedElemTy sid)) acc) body) cg0.KernelExpr
            { cg0 with KernelExpr = kernelExpr
                       OutputType = callables.[i].RetType
                       FoldWrapper = Some wnames.[i] })
            infos
    match checkJoinCompatible (shareCgs @ leafCgs) with
    | Error reason ->
        (codegenError ctx ind ($"reduction join: cannot fold the legs in one traversal: {reason}"), ctx')
    | Ok _ ->
        let wrapperLines = wrappers |> List.collect fst |> List.map (fun s -> ind + s)
        let elemStrs =
            callables |> List.map (fun cb ->
                match cb.RetType with
                | IRTScalar et -> primTypeToCpp et
                | t -> irTypeToCpp t)
        let declCode =
            List.mapi (fun i ln ->
                $"{ind}{elemStrs.[i]} {ln} = {(exprToCppCtx ctx seedExprs.[i])};") leafNames
        let allCgs = shareCgs @ leafCgs
        let (sm, sp, sNew) = streamedNestSetup ctx.StreamedArrays ind allCgs
        registerStreamBufDecls sNew
        let loopCode = sp @ genFusedLoopNestStreamed sm allCgs ctx.VarNames ctx.Indent false None
        let tupleLine = $"""{ind}auto {name} = std::make_tuple({(leafNames |> String.concat ", ")});"""
        let shareNote =
            if sharedIds.IsEmpty then []
            else [ sprintf "%s// reduction join: %d leg(s), sharing %s per iteration" ind leafNames.Length
                       (sharedIds |> List.map (fst >> sharedName) |> String.concat ", ") ]
        let ctxOut = { ctx' with TupleChildren = Map.add name leafNames ctx'.TupleChildren }
        (wrapperLines @ shareNote @ declCode @ loopCode @ [tupleLine], ctxOut)

and genTupleProjBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (parentExpr: IRExpr) (projIdx: int) (isFlat: bool) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Check if parent is a deferred computation tuple -- if so, project and defer
    let parentDeferred =
        match parentExpr with
        | IRVar (pid, _) -> Map.tryFind pid ctx.DeferredComputations
        | _ -> None
    match parentDeferred with
    | Some (IRTuple elems) when projIdx < elems.Length ->
        // Parent is a deferred tuple -- project out the element and defer it
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id elems.[projIdx] ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation (tuple proj)>"], ctx')
    | Some (IRParallel _ | IRFusion _) ->
        // Parent is a deferred combinator -- defer the projection too
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation (proj of combinator)>"], ctx')
    | _ ->
        // Tuple projection -- resolve through TupleChildren map
        let parentName =
            match parentExpr with
            | IRVar (pid, _) -> Map.tryFind pid ctx.VarNames |> Option.defaultValue "_"
            | _ -> "_"
        let parentType = inferExprType parentExpr
        let flatChildren =
            match Map.tryFind parentName ctx.TupleChildren with
            | Some children -> children
            | None -> []

        if isFlat then
            // Flat projection: projIdx is a flat leaf index
            if projIdx < flatChildren.Length then
                let sourceName = flatChildren.[projIdx]
                let code = [$"{ind}auto& {name} = {sourceName};"]
                let extentsAlias =
                    match IR.stripUnits binding.Type with
                    | ArrayElem _ ->
                        [$"{ind}const size_t* {name}_extents = {sourceName}.extents;"]
                    | _ -> []
                let ctx' = addVarName binding.Id name ctx
                let ctx' =
                    match Map.tryFind sourceName ctx'.TupleChildren with
                    | Some children -> { ctx' with TupleChildren = Map.add name children ctx'.TupleChildren }
                    | None -> ctx'
                (code @ extentsAlias, ctx')
            else
                let code = genScalarBinding ctx name binding.Value binding.Type
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')
        else
            // Structural projection: projIdx is a type-level index
            let ranges = tupleLeafRanges parentType
            let (flatStart, leafCount) =
                if projIdx < ranges.Length then ranges.[projIdx]
                else (projIdx, 1)

            if leafCount > 1 && flatChildren.Length > 0 && flatStart + leafCount <= flatChildren.Length then
                // Sub-tuple: synthesize from flat children range
                let subChildren = flatChildren.[flatStart .. flatStart + leafCount - 1]
                let tupleLine = $"""{ind}auto {name} = std::make_tuple({(subChildren |> String.concat ", ")});"""
                let ctx' = addVarName binding.Id name ctx
                let ctx' = { ctx' with TupleChildren = Map.add name subChildren ctx'.TupleChildren }
                ([tupleLine], ctx')

            elif flatStart < flatChildren.Length then
                // Single leaf at computed position
                let sourceName = flatChildren.[flatStart]
                let code = [$"{ind}auto& {name} = {sourceName};"]
                let extentsAlias =
                    match IR.stripUnits binding.Type with
                    | ArrayElem _ ->
                        [$"{ind}const size_t* {name}_extents = {sourceName}.extents;"]
                    | _ -> []
                let ctx' = addVarName binding.Id name ctx
                let ctx' =
                    match Map.tryFind sourceName ctx'.TupleChildren with
                    | Some children -> { ctx' with TupleChildren = Map.add name children ctx'.TupleChildren }
                    | None -> ctx'
                (code @ extentsAlias, ctx')

            else
                // No TupleChildren -- fall back to std::get
                let code = genScalarBinding ctx name binding.Value binding.Type
                let ctx' = addVarName binding.Id name ctx
                (code, ctx')



and genVarAliasBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (srcId: IRId) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Check if source is deferred -- propagate deferral (but a MUTABLE
    // binding deep-copies its initializer's storage, so a deferred source
    // must be FORCED first: propagating would leave both unmaterialized
    // while later assignments/reads use them by name).
    match Map.tryFind srcId ctx.DeferredComputations with
    | Some _ when binding.IsMutable || Set.contains binding.Id ctx.MutableArrayLets ->
        let (fcode, fctx, _) = forceDeferredArrayInput ctx builder ($"{name}__src") (IRVar (srcId, binding.Type))
        let (code, ctx') = genVarAliasBinding fctx binding builder srcId
        (fcode @ code, ctx')
    | Some deferred ->
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id deferred ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred computation (alias)>"], ctx')
    | None ->
        // Let-binding whose value is a reference to a lifted callable.
        // `let f = lambda(...)` lowers to `binding.Value = IRVar(callable.Id,
        // funcType)`; emit a wrapper closure bound to `f` so direct calls
        // `f(args)` work, with the wrapper's signature matching the
        // callable's regular params (captures pulled in by `[&]`, hidden from
        // the call site). Without this, genScalarBinding's fallback would emit
        // `std::function<...> f = __lambda_X;`, which fails to compile since
        // the lifted function has captures as extra positional params.
        //
        // Closure body: `return __lambda_X(regulars..., captures...);`. The
        // wrapper's declared return type defers to `auto` -- the body is a
        // trivial forwarding call, so the compiler infers it precisely from
        // the callable's signature and the user never sees the wrapper's type.
        match resolveCallable binding.Value with
        | Some callable ->
            // The wrapper forwards every capture BY NAME, so a still-DEFERRED
            // one has to be materialized first -- the same capture-boundary
            // forcing genComputeBinding does for a combinator's kernel (see
            // collectDeferredKernelCaptures). Runs before VarNames is read
            // below, since forcing can rename nothing but must be in scope.
            let (capForceCode, ctx) =
                collectDeferredKernelCaptures ctx binding.Value
                |> forceDeferredBindingIds ctx builder ($"{name}__cap")
            let safeName = sanitizeCppName callable.Name
            let paramSig =
                callable.Params
                |> List.map (fun p ->
                    match p.Type with
                    | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {p.Name}"
                    | _ -> $"{(irTypeToCpp p.Type)} {p.Name}")
                |> String.concat ", "
            let regularArgs = callable.Params |> List.map (_.Name)
            let captureArgs = captureForwardArgs ctx.VarNames callable.Captures
            let allArgs = (regularArgs @ captureArgs) |> String.concat ", "
            // Wrapper type: `std::function<Ret(P1, P2, ...)>`. Explicit
            // type per the codegen convention (auto reserved for thin
            // forwarding wrappers prefixed `__wrap_*`). std::function
            // is required when the wrapper itself flows into another
            // function's capture slot -- the receiving function takes
            // captures as `std::function<...>&`, which can't bind to
            // an rvalue temporary if we emit raw closures via auto.
            // std::function gives the binding a stable lvalue type
            // that matches the capture-slot signature.
            let paramTypes =
                callable.Params
                |> List.map (fun p ->
                    match p.Type with
                    | ArrayElem arr -> cppArrayTypeStr arr
                    | _ -> irTypeToCpp p.Type)
            let retTypeStr =
                match callable.RetType with
                | ArrayElem arr -> cppArrayTypeStr arr
                | t -> irTypeToCpp t
            let funcTypeStr =
                $"""std::function<{retTypeStr}({(String.concat ", " paramTypes)})>"""
            let code = [$$"""{{ind}}{{funcTypeStr}} {{name}} = [&]({{paramSig}}) { return {{safeName}}({{allArgs}}); };"""]
            let ctx' = addVarName binding.Id name ctx
            (capForceCode @ code, ctx')
        | None ->
            // Plain variable alias -- may be aliasing a tuple, propagate children
            let srcName = Map.tryFind srcId ctx.VarNames |> Option.defaultValue ""
            let hasTupleChildren = Map.containsKey srcName ctx.TupleChildren
            // An ASSIGNABLE binding of an existing DENSE array (`let a = Z` /
            // `let mut a = Z`; block-level via ctx.MutableArrayLets,
            // top-level via IsMutable -- TypeCheck marks every non-static let
            // assignable, so both spellings admit `a(i) = ...`) deep-copies
            // the storage: the wrapper-by-value alias below shares Z's data
            // pointer, so mutations through `a` would silently corrupt `Z`.
            // Compound/ragged/dep-idx initializers keep the historical alias
            // (no dense .extents/pool contract; no assignment path exercises
            // them today).
            let mutArrayCopy =
                if binding.IsMutable || Set.contains binding.Id ctx.MutableArrayLets then
                    match binding.Type with
                    | ArrayElem at when not (isCompoundArrayType at)
                                       && not (isRaggedArrayType at)
                                       && not (isDepIdxArrayType at) ->
                        materializeArrayCopyForm emptySubst ctx.VarNames name (elemTypeToCpp at.ElemType) binding.Value
                    | _ -> None
                else None
            match mutArrayCopy with
            | Some (copyStmts, copyAllocs) ->
                // Deterministic deallocation, site 6: the `let (mut) a = Z` deep
                // copy. ONE registration covers BOTH mut paths -- genFuncBody's mut
                // arm routes through genBinding to here. Site 7 folded the former
                // hand-rebuilt registration into the builder's OWN descriptor:
                // materializeArrayCopyForm picks (spec, SYMM) from the SOURCE type
                // and its rank with `max 1 ix.Rank`, and now reports exactly that,
                // so the earlier dense-and-unsymmetric restriction (which existed
                // only because the consumer had to guess) is gone. Extents are a
                // STACK array here, so nothing is owned.
                registerMaterializedAllocs copyAllocs
                let ctx' = addVarName binding.Id name ctx
                (copyStmts |> List.map (fun s -> ind + s), ctx')
            | None ->
                // Use auto& when source has flat TupleChildren to avoid type mismatch
                let code =
                    if hasTupleChildren then
                        [$"{ind}auto& {name} = {srcName};"]
                    else
                        genScalarBinding ctx name binding.Value binding.Type
                let ctx' = addVarName binding.Id name ctx
                let ctx' =
                    match Map.tryFind srcName ctx'.TupleChildren with
                    | Some children -> { ctx' with TupleChildren = Map.add name children ctx'.TupleChildren }
                    | None -> ctx'
                (code, ctx')



and genChoiceBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (left: IRExpr) (right: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Only defer when children are computation-level (not scalar)
    let isCompExpr e = match e with IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _ | IRBind _ | IRGuard _ | IRSequence _ -> true | IRVar _ -> true | _ -> false
    if isCompExpr left || isCompExpr right then
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred choice>"], ctx')
    else
        // Scalar choice: generate directly
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



/// `let C = A <|:> B` binding site. The operands are arrays (typecheck
/// guarantees it -- scalars steer to <|>), and the combinator is lazy like the
/// rest of its family: defer, and materialize at |> compute
/// (genFallbackMaterialize via genComputeBinding's IRFallback arm).
and genFallbackBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (left: IRExpr) (right: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    let ctx' = addVarName binding.Id name ctx
    let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
    ([$"{ind}// {name} = <deferred fallback>"], ctx')

/// Materialize `A <|:> B` (allocated-fallback, formalism 2.6): read A where
/// A's STORAGE holds the cell, else B. Two storage regimes, one judgment:
///   * compound-left: the CompoundIdx mask IS the allocation record. Iterate
///     the dense underlying space (B's extents = result extents); present
///     lead-tuples read A's compact buffer (linearize * trailing_stride),
///     absent ones read B. An allocated zero survives -- the distinguisher
///     from <|>'s value-keyed zero test.
///   * dense-left: allocation = the nested-pointer chain, checked per curry
///     level by the fallback_copy<> runtime helper (nullptr-robust; compiler-
///     built arrays are fully allocated, partially-allocated ones arrive via
///     the C++-level partial-depth allocation API).
/// The result is always a fully-allocated dense array.
and genFallbackMaterialize (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (left: IRExpr) (right: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Operand prep: named materialized vars pass through; anything else (a
    // deferred-computation var, an inline combinator, or a nested IRFallback
    // from the object_for(<|:>) fold) materializes into a synthetic
    // sub-binding first (the genChoiceBinding pattern).
    let prepOperand (ctxIn: CodeGenContext) (e: IRExpr) (tag: string) : string list * CodeGenContext * string * IRType =
        match e with
        | IRVar (id, ty) when Map.containsKey id ctxIn.VarNames && not (Map.containsKey id ctxIn.DeferredComputations) ->
            ([], ctxIn, Map.find id ctxIn.VarNames, ty)
        | _ ->
            let subTy = match e with IRVar (_, ty) -> ty | _ -> binding.Type
            let subName = $"{name}__{tag}"
            let subBinding = { Id = builder.FreshId(); Name = subName; Type = subTy
                               Value = IRCompute e; IsConst = true; IsMutable = false }
            let (code, ctx') = genBinding ctxIn subBinding builder
            (code, ctx', subName, subTy)
    let (codeL, ctxL, nameL, tyL) = prepOperand ctx left "lhs"
    let (codeR, ctxR, nameR, _tyR) = prepOperand ctxL right "rhs"
    match binding.Type with
    | ArrayElem resArr ->
        let rank = arrayRank resArr
        let elemType = elemTypeToCpp resArr.ElemType
        let leftCompound =
            match tyL with
            | ArrayElem aL when isCompoundArrayType aL -> Some aL
            | _ -> None
        // Result extents come from the operand that spans the dense space:
        // the left array for dense-left (operands are type-unified), the
        // RIGHT array for compound-left (the left is compact storage).
        let extentsSrc = match leftCompound with Some _ -> nameR | None -> nameL
        // Return-extent ABI (commit 7905b36, extended). This used to be a bare
        // ALIAS -- `const size_t* name_extents = <operand>.extents;` -- which
        // made the result's SHAPE a pointer into an operand's table. Fine while
        // the result was consumed in the frame that built it, and dangling the
        // moment it crossed a return: the frees run in reverse order, so the
        // lender is deleted while the escaping wrapper still points at its
        // table. That was invisible until the unified statement-shaped return
        // arm made `function f(x, y) = { x <|:> y }` compile at all.
        //
        // `emitExtentsTable` is the same helper the 14 materialize*Form
        // builders use. The dims are runtime `.extents[d]` reads, never
        // literals, so it takes the `new size_t[R]` branch and hands back an
        // owned name -- which rides on the array's own descriptor via
        // registerPoolAlloc's `ownedExtents`, exactly as 7905b36 established.
        // A separately tracked table would be freed out from under the very
        // wrapper that just escaped, since the return suppression is a
        // whole-token match on the rendered return text and can only ever see
        // `<name>`, never `<name>_extents`.
        let extentsName = name + "_extents"
        let dims = [ for d in 0 .. rank - 1 -> ($"{extentsSrc}.extents[{d}]", false) ]
        let (extentsDecl, ownedExtents) = emitExtentsTable ind extentsName rank dims
        let allocDecl = $"{ind}Array<{elemType}, {rank}> {name} = {{ allocate<typename promote<{elemType}, {rank}>::type, nullptr>({name}_extents), {name}_extents }};"
        // Deterministic deallocation, site 5d: `<|:>` result. Always a
        // fully-allocated dense array, and now the owner of its own extents
        // table rather than a borrower of an operand's.
        if isFreeableDenseArrayType resArr then
            registerPoolAlloc AllocDense elemType rank "nullptr" extentsName name ownedExtents
        let indD d = String.replicate d "    "
        let idxVar i = $"__fb{i}"
        let subscript n = [for i in 0 .. n - 1 -> $"[{(idxVar i)}]"] |> String.concat ""
        let bodyLines =
            match leftCompound with
            | None ->
                // Dense-left: one nullptr-robust recursive copy.
                [$"{ind}nested_array_utilities::fallback_copy<{elemType}, {rank}>({name}.data, {nameL}.data, {nameR}.data, {name}_extents);"]
            | Some aL ->
                let leadRank =
                    aL.IndexTypes
                    |> List.tryFind (fun ix -> ix.IxKind = IxKCompound)
                    |> Option.map (_.Rank)
                    |> Option.defaultValue 1
                let trailingCount = rank - leadRank
                // Runtime shape guard: the mask's underlying extents must
                // agree with the dense right operand (statically only ranks
                // and element types are checkable -- the mask is runtime data).
                let guards =
                    [ for d in 0 .. leadRank - 1 ->
                        $"{ind}if ({nameL}.idx->extents[{d}] != {name}_extents[{d}]) {{ blade_rt::panic(\"BL8001\", \"<|:>: compound left operand's underlying extents disagree with the dense right operand's shape\", nullptr, 0); }}" ]
                let mutable lines = guards
                let mutable depth = ctx.Indent
                for i in 0 .. leadRank - 1 do
                    lines <- lines @ [$$"""{{(indD depth)}}for (size_t {{(idxVar i)}} = 0; {{(idxVar i)}} < {{name}}_extents[{{i}}]; {{(idxVar i)}}++) {"""]
                    depth <- depth + 1
                let leadTuple =
                    [for i in 0 .. leadRank - 1 -> idxVar i] |> String.concat ", "
                lines <- lines @ [$"{(indD depth)}std::array<size_t, {leadRank}> __fb_t{{{{ {leadTuple} }}}};"]
                // Row-major flatten of the trailing coordinate inside a
                // present cell's contiguous block.
                let trailOffsetExpr =
                    if trailingCount = 0 then ""
                    else
                        [leadRank .. rank - 1]
                        |> List.fold (fun acc j ->
                            if acc = "" then idxVar j
                            else $"({acc} * {name}_extents[{j}] + {(idxVar j)})") ""
                let readCompact =
                    if trailingCount = 0 then $"{nameL}.data[{nameL}.idx->linearize(__fb_t)]"
                    else $"{nameL}.data[{nameL}.idx->linearize(__fb_t) * {nameL}.trailing_stride + {trailOffsetExpr}]"
                let emitTrailingAssign (baseDepth: int) (rhs: string) : string list =
                    let mutable ls = []
                    let mutable d = baseDepth
                    for j in leadRank .. rank - 1 do
                        ls <- ls @ [$$"""{{(indD d)}}for (size_t {{(idxVar j)}} = 0; {{(idxVar j)}} < {{name}}_extents[{{j}}]; {{(idxVar j)}}++) {"""]
                        d <- d + 1
                    ls <- ls @ [$"{(indD d)}{name}{(subscript rank)} = {rhs};"]
                    for _ in leadRank .. rank - 1 do
                        d <- d - 1
                        ls <- ls @ [$"{(indD d)}}}"]
                    ls
                lines <- lines @ [$$"""{{(indD depth)}}if ({{nameL}}.idx->present(__fb_t)) {"""]
                lines <- lines @ emitTrailingAssign (depth + 1) readCompact
                lines <- lines @ [$"{(indD depth)}}} else {{"]
                lines <- lines @ emitTrailingAssign (depth + 1) ($"{nameR}{(subscript rank)}")
                lines <- lines @ [$"{(indD depth)}}}"]
                for _ in 0 .. leadRank - 1 do
                    depth <- depth - 1
                    lines <- lines @ [$"{(indD depth)}}}"]
                lines
        let ctx' = addVarName binding.Id name ctxR
        (codeL @ codeR @ [""; $"{ind}// <|:> allocated-fallback: {nameL} where allocated, else {nameR}"] @ extentsDecl @ [allocDecl] @ bodyLines, ctx')
    | t ->
        let code = codegenError ctx ind (sprintf "<|:>: binding type is not an array (got %A) -- likely a typechecker or IR bug" t)
        (codeL @ codeR @ code, addVarName binding.Id name ctxR)

and genGuardBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (body: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Guard wrapping a computation: defer for later materialization via |> compute
    // Recurse through nested guards to check if the leaf body is a computation
    let rec leafIsComputation e =
        match e with
        | IRGuard (_, inner) -> leafIsComputation inner
        | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _ | IRBind _ | IRSequence _ -> true
        | IRVar (id, _) -> Map.containsKey id ctx.DeferredComputations
        | _ -> false
    if leafIsComputation body then
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred guard>"], ctx')
    else
        // Scalar guard: generate directly
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



and genSequenceBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (elems: IRExpr list) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Defer: sequence is a flat n-ary parallel, materialized by |> compute
    let isCompExpr e = match e with IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _ | IRBind _ | IRGuard _ | IRSequence _ -> true | IRVar _ -> true | _ -> false
    if elems |> List.exists isCompExpr then
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred sequence>"], ctx')
    else
        // All scalars: generate as tuple
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



and genForRangeBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (vid: IRId) (lo: IRExpr) (hi: IRExpr) (body: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Force any deferred producer this loop reads positionally BEFORE the loop
    // header, so the materialization is not re-run each iteration. When this
    // for-range is nested inside a block, genLetChainBinding already forced it
    // (dropped from DeferredComputations) and the collector finds nothing.
    let (forceCode, ctx) = forceDeferredPositionalReads ctx builder ($"{name}__def") body
    // Imperative for-range loop
    let loStr = exprToCppCtx ctx lo
    let hiStr = exprToCppCtx ctx hi
    let varName = $"__k{vid}"
    let innerCtx = addVarName vid varName ctx
    // Unroll the body IRLet chain into statements
    let (bodyLets, _bodyFinal) = unrollLetChain body
    // FreshPool calls in ARGUMENT position get their own lets here exactly as
    // in genFuncBody, so a per-iteration `g(f(x))` temporary is registered
    // and recycled instead of leaking once per trip.
    let bodyLets =
        bodyLets |> List.collect (fun (id, v) ->
            let (hv, v') = hoistFreshPoolCallArgs builder v
            hv @ [(id, v')])
    // Deterministic deallocation: ONE loop-body frame, pushed AFTER
    // forceDeferredPositionalReads above (whose materializations belong to the
    // OUTER scope and must not be freed per iteration). Registrations always land
    // on the top frame, so the frees emitted at the bottom of the body can only
    // name storage THIS iteration allocated -- structurally, with no id analysis.
    let escapes = computeScopeEscapes ctx LoopScope bodyLets
    let allocDepth = allocScopeDepth ()
    pushAllocScope SLoop escapes
    let (bodyCode, frees) =
        try
            let (code, _) =
                bodyLets |> List.fold (fun (accCode, accCtx) (id, value) ->
                    // See setAllocOwner: this let owns every allocation emitted
                    // while it renders, sub-temporaries included.
                    setAllocOwner (Some id)
                    let tempName = $"__v{id}"
                    let tempBinding = {
                        Id = id; Name = tempName; Type = inferExprType value
                        Value = value; IsConst = true; IsMutable = false
                    }
                    let (code, ctx') = genBinding { accCtx with Indent = ctx.Indent + 1 } tempBinding builder
                    (accCode @ code, { ctx' with Indent = ctx.Indent })
                ) ([], innerCtx)
            setAllocOwner None
            (code, popAllocScopeFrees (ind + "    "))
        finally truncateAllocScopeStack allocDepth
    let code =
        forceCode
        @ [forLoopFrom ind varName loStr hiStr]
        @ bodyCode
        @ frees
        @ [$"{ind}}}"]
    let ctx' = addVarName binding.Id name ctx
    (code, ctx')



and genBindChainBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) (comp: IRExpr) (cont: IRExpr) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Monadic bind: defer if comp is a deferred computation
    let isCompDeferred =
        match comp with
        | IRVar (id, _) -> Map.containsKey id ctx.DeferredComputations
        | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _ -> true
        | _ -> false
    if isCompDeferred then
        let ctx' = addVarName binding.Id name ctx
        let ctx' = { ctx' with DeferredComputations = Map.add binding.Id binding.Value ctx'.DeferredComputations }
        ([$"{ind}// {name} = <deferred bind>"], ctx')
    else
        // Scalar bind: cont(comp)
        let code = genScalarBinding ctx name binding.Value binding.Type
        let ctx' = addVarName binding.Id name ctx
        (code, ctx')



and genLetChainBinding (ctx: CodeGenContext) (binding: IRBinding) (builder: IRBuilder) : string list * CodeGenContext =
    let ind = indentStr ctx
    let name = bindingCppName binding
    // Force any deferred producer this block reads positionally (`A(i)` over a
    // still-deferred computed array) at the block's OUTER indent, before any
    // block statement is emitted -- materializing inside the block/loop body
    // would re-run the producer per iteration or dangle its loop var.
    let (forceCode, ctx) = forceDeferredPositionalReads ctx builder ($"{name}__def") binding.Value
    // Block expression: unroll the IRLet chain into sequential bindings
    let (lets, finalExpr) = unrollLetChain binding.Value
    let (allCode, foldCtx) =
        lets |> List.fold (fun (accCode, accCtx) (id, value) ->
            let tempName = $"__v{id}"
            let tempBinding = {
                Id = id; Name = tempName; Type = inferExprType value
                Value = value; IsConst = true; IsMutable = false
            }
            let (code, ctx') = genBinding accCtx tempBinding builder
            (accCode @ code, ctx')
        ) ([], ctx)
    // Generate the final named binding.
    //
    // Double-materialize elision: a chain whose VALUE is a bare reference to
    // one of its own inner lets (`{ let buf = zeros(...); ...; buf }` -- the
    // recursive-array elaboration's exact shape) would otherwise go through
    // genVarAliasBinding's assignability deep copy, leaving BOTH the staging
    // buffer and the copy live for the rest of the program, since module
    // bindings are never scope-freed: 2x the footprint of every `let rec`
    // trajectory (~1.5 GB of 09_qg_atmosphere's residency, measured). The
    // staging let is block-scoped -- no name outside this chain can reach
    // it -- so the final binding may ALIAS its wrapper instead, under
    // canAliasStagingLet's three conditions (sole-owned fresh pool, no assign
    // leaking it out of the chain, no rival alias).
    //
    // The original form also required an EMPTY alloc-scope stack, on the
    // reasoning that a function/loop frame's registration plus escape analysis
    // already recycled the block. That gate is gone: it is the frame case that
    // leaks hardest (a `let rec` built inside a function is re-materialized on
    // EVERY call), and the alias changes nothing the frame reasons about --
    // both the staging alloc and the elided copy carry the same owner stamp
    // (setAllocOwner is per OUTER let), so the frame simply tracks one
    // allocation instead of two.
    let aliasableStagingRef =
        match finalExpr with
        | IRVar (srcId, _) -> canAliasStagingLet lets None srcId
        | _ -> false
    let finalBinding = {
        Id = binding.Id; Name = name; Type = binding.Type
        Value = finalExpr; IsConst = binding.IsConst
        // Dropping the mutability marks routes genVarAliasBinding to its
        // plain-alias arm; assignment through the binding then writes the
        // sole-owner staging pool, which is observationally identical.
        IsMutable = binding.IsMutable && not aliasableStagingRef
    }
    // A block-local `let rec` arrives here with its id in MutableArrayLets
    // (every block array let is assignable), which genVarAliasBinding checks
    // INDEPENDENTLY of IsMutable -- lift the membership for the final bind
    // only, restoring the caller-visible set afterwards.
    let foldCtxForFinal =
        if aliasableStagingRef
        then { foldCtx with MutableArrayLets = Set.remove binding.Id foldCtx.MutableArrayLets }
        else foldCtx
    let (finalCode, finalCtx) = genBinding foldCtxForFinal finalBinding builder
    let finalCtx =
        if aliasableStagingRef
        then { finalCtx with MutableArrayLets = foldCtx.MutableArrayLets }
        else finalCtx
    (forceCode @ allCode @ finalCode, finalCtx)


