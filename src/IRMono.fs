// Monomorphization: HM-style type-variable specialization, arity-
// polymorphism specialization, array-binop lowering, the per-module
// drivers, and shape (symbolic-extent) monomorphization.
module Blade.IRMono

open Blade.Types
open Blade.IR

/// Unify a parameter type against an argument type, accumulating
/// (typeVarId, concreteType) bindings. Walks pairs structurally:
/// ArrayElem pairs ElemType, IRTTuple pairs elementwise, FuncElem
/// pairs args and ret. An IRTInfer on the param side absorbs whatever's
/// on the arg side. This is one-sided (not full unification) because at
/// HM call sites the arg type is fully concrete.
let rec unifyParamWithArg (paramTy: IRType) (argTy: IRType) (acc: Map<int, IRType>) : Map<int, IRType> =
    match paramTy, argTy with
    | IRTInfer n, t when not (acc.ContainsKey n) -> Map.add n t acc
    | IRTInfer n, t when acc.[n] = t -> acc  // Consistent reuse -- fine
    | IRTInfer _, _ -> acc  // Inconsistent -- leave as-is; the IR validator will catch it
    | ArrayElem pa, ArrayElem aa ->
        unifyParamWithArg pa.ElemType aa.ElemType acc
    | IRTTuple pts, IRTTuple ats when pts.Length = ats.Length ->
        List.zip pts ats |> List.fold (fun m (p, a) -> unifyParamWithArg p a m) acc
    | FuncElem (pas, pr), FuncElem (aas, ar) when pas.Length = aas.Length ->
        // FuncElem matches IRTArrow with all-SVal slots (function form).
        let acc' = List.zip pas aas |> List.fold (fun m (p, a) -> unifyParamWithArg p a m) acc
        unifyParamWithArg pr ar acc'
    | IRTComputation pt, IRTComputation at -> unifyParamWithArg pt at acc
    | IRTPoly (pb, _), IRTPoly (ab, _) -> unifyParamWithArg pb ab acc
    | IRTUnitAnnotated (pi, _), IRTUnitAnnotated (ai, _) -> unifyParamWithArg pi ai acc
    | IRTIdxTagged (pi, _), IRTIdxTagged (ai, _) -> unifyParamWithArg pi ai acc
    | IRTDist (po, pe, _), IRTDist (ao, ae, _) when po = ao -> unifyParamWithArg pe ae acc
    | IRTArrow (pSlots, pRet, _), IRTArrow (aSlots, aRet, _) when pSlots.Length = aSlots.Length ->
        // Generic IRTArrow-vs-IRTArrow: handles arrows with SIdx and/or
        // SIdxVirt slots (FuncElem above only matched all-SVal arrows).
        // Identity is ignored for unification -- it's metadata, not type.
        let unifySlot acc' (p, a) =
            match p, a with
            | SVal pt, SVal at -> unifyParamWithArg pt at acc'
            | SIdx _, SIdx _ -> acc'
            | SIdxVirt _, SIdxVirt _ -> acc'
            | _ -> acc'
        let acc' = List.zip pSlots aSlots |> List.fold unifySlot acc
        unifyParamWithArg pRet aRet acc'
    // A unit annotation on ONE side only. Last, so the both-annotated arm
    // above still wins; these mirror `Unify.unify`'s permissive asymmetric
    // unit arms, which is what lets a BARE value flow into an annotated
    // position in the first place -- `f(x: Float<day>)` accepts a bare
    // literal or a bare array. Learning bindings has to see through the
    // wrapper for the same reason: with a unit-carrying ABSTRACT parameter
    // (`T<day>^1`, whose element is `IRTUnitAnnotated (IRTInfer n, day)`) a
    // bare argument left `n` unlearned, so specialization never substituted
    // it and the var reached the IR validator as BL6001 "unresolved type
    // variable" -- a program that passed `blade check` and then died. No
    // unit CHECK is skipped here: compatibility is settled in TypeCheck, at
    // the call site, long before monomorphization runs.
    | IRTUnitAnnotated (pi, _), _ -> unifyParamWithArg pi argTy acc
    | _, IRTUnitAnnotated (ai, _) -> unifyParamWithArg paramTy ai acc
    | _ -> acc  // Concrete types or unhandled compound -- no bindings learned

/// Walk a type collecting all IRTInfer IDs found inside (recursively).
let rec collectInferIds (ty: IRType) : Set<int> =
    match ty with
    | IRTInfer n -> Set.singleton n
    | IRTTuple ts -> ts |> List.fold (fun s t -> Set.union s (collectInferIds t)) Set.empty
    | IRTComputation t -> collectInferIds t
    | IRTPoly (b, _) -> collectInferIds b
    | IRTUnitAnnotated (i, _) -> collectInferIds i
    | IRTIdxTagged (i, _) -> collectInferIds i
    | IRTDist (_, e, _) -> collectInferIds e
    | IRTArrow (slots, ret, _) ->
        let slotIds =
            slots |> List.fold (fun s slot ->
                match slot with
                | SVal ty -> Set.union s (collectInferIds ty)
                | SIdx _ | SIdxVirt _ -> s) Set.empty
        Set.union slotIds (collectInferIds ret)
    | _ -> Set.empty

/// Does this function carry any unresolved type variables in its declared
/// signature (params or return type)? Boundary criterion for HM
/// monomorphization -- only such functions need specialization.
let hasTypeVarsInSignature (func: IRFuncDef) : bool =
    let paramIds =
        func.Params |> List.fold (fun s p -> Set.union s (collectInferIds p.Type)) Set.empty
    let retIds = collectInferIds func.RetType
    not (Set.isEmpty paramIds && Set.isEmpty retIds)

/// Does this function have type vars in its PARAMETERS specifically? Only such
/// functions need per-call-site specialization. A function whose type vars sit
/// ONLY in the return type (params fully concrete, e.g. `lambda(x) ->
/// rowsum(x)` echoing an HM helper's abstract return) must be KEPT -- body
/// call-site-rewritten, return type substituted from global bindings -- not
/// dropped-and-specialized; dropping it while referenced as a first-class
/// kernel value leaves a dangling VarId.
let hasTypeVarsInParams (func: IRFuncDef) : bool =
    func.Params
    |> List.exists (fun p -> not (Set.isEmpty (collectInferIds p.Type)))

// HM Monomorphization
//
// Generates specialized copies of functions with free type variables in
// their signatures, one per unique call-site type pattern. Sibling pass to
// Arity (Poly) monomorphization; runs before Poly so type-substitution
// happens at the abstract signature level before Poly expands param packs.
// Architecture mirrors monomorphizeModule's 5-phase shape (identify, collect
// call sites with bindings, specialize, build rewrite map, rewrite all
// expressions); the key difference is SpecRequest carrying (typeVarId ->
// concreteType) bindings rather than a single arity int.

/// Collect call sites of HM-polymorphic functions.
/// Returns list of (funcId, sortedBindings) pairs. Bindings are sorted
/// by ID to give a canonical key for deduplication across call sites.
let collectHMCallSites (hmFuncMap: Map<IRId, IRFuncDef>) (expr: IRExpr) : (IRId * (int * IRType) list) list =
    let results = System.Collections.Generic.List<_>()
    let walk e =
        match e with
        | IRApp (IRVar (funcId, _), args, _) when hmFuncMap.ContainsKey funcId ->
            let func = hmFuncMap.[funcId]
            // Pair each param with its corresponding arg, unifying types
            let bindings =
                if args.Length <> func.Params.Length then Map.empty
                else
                    List.zip func.Params args
                    |> List.fold (fun acc (p, arg) ->
                        match exprTypeIfKnown arg with
                        | Some argTy -> unifyParamWithArg p.Type argTy acc
                        | None -> acc) Map.empty
            // Convert to sorted list for canonical comparison
            let sortedBindings =
                bindings |> Map.toList |> List.sortBy fst
            results.Add((funcId, sortedBindings))
        | _ -> ()
    iterIRExpr walk expr
    results |> Seq.toList

/// What a call site teaches about the CALLER's OWN type variables.
///
/// `collectHMCallSites` learns the CALLEE's vars from the argument types, which
/// is what a specialization is keyed and named on. It cannot learn the caller's,
/// and across a MODULE BOUNDARY those are a second, disjoint namespace:
/// TypeCheck instantiates an imported signature with fresh vars at the import
/// site, while the callee's `IRFuncDef` keeps the ones it was declared with. So
/// an imported `mean(row: T^1) -> T^0` specialized correctly to
/// `mean_HM_10001_double` while every expression AROUND the call still carried
/// the caller's `T?10007` -- which nothing then substituted, giving BL6001
/// "unresolved type variable" on a program whose types were all perfectly well
/// determined. Same-module calls share one namespace, which is why this was
/// invisible until a Blade-source stdlib started exporting generics.
///
/// The missing equation is the RETURN type: the callee's return, substituted
/// with what the arguments taught, IS the call's recorded result type, so
/// matching the two binds the caller-side vars. That is the exact mirror of the
/// param/arg direction, and it adds no new information about the callee.
///
/// Deliberately NOT folded into `collectHMCallSites`: these bindings must stay
/// out of the specialization KEY, or two call sites differing only in
/// caller-side var numbering would mint two identical specs under two names.
let collectHMCallSiteReturnBindings
    (hmFuncMap: Map<IRId, IRFuncDef>) (expr: IRExpr) : (int * IRType) list =
    let results = System.Collections.Generic.List<_>()
    let walk e =
        match e with
        | IRApp (IRVar (funcId, _), args, retTy) when hmFuncMap.ContainsKey funcId ->
            let func = hmFuncMap.[funcId]
            if args.Length = func.Params.Length then
                let calleeBindings =
                    List.zip func.Params args
                    |> List.fold (fun acc (p, arg) ->
                        match exprTypeIfKnown arg with
                        | Some argTy -> unifyParamWithArg p.Type argTy acc
                        | None -> acc) Map.empty
                let concreteRet = substTypeInIRType calleeBindings func.RetType
                unifyParamWithArg retTy concreteRet Map.empty
                |> Map.iter (fun k v -> results.Add((k, v)))
        | _ -> ()
    iterIRExpr walk expr
    results |> Seq.toList

/// Occurrence-id-INDEPENDENT structural key for a type: same element, rank,
/// extent, and symmetry => same key, regardless of the per-occurrence index-type
/// `Id`s. Used BOTH to dedup HM specializations and to NAME them, so the two
/// stay consistent. Without this, one recursive poly kernel (`comoment_prod`)
/// specialized at the same arity from two call chains carried two different
/// index ids for the same `Array<double, Idx<3>>` -- distinct dedup keys but an
/// identical mangled name (a naive `mangleType` collapses every array to "T"),
/// which g++ rejects as a redefinition. Output is C++-identifier-safe.
let rec canonTypeKey (ty: IRType) : string =
    match ty with
    | IRTScalar ETFloat64 -> "double"
    | IRTScalar ETFloat32 -> "float"
    | IRTScalar ETInt64 -> "int64"
    | IRTScalar ETInt32 -> "int32"
    | IRTScalar ETBool -> "bool"
    | IRTScalar ETString -> "string"
    | IRTScalar ETComplex64 -> "c64"
    | IRTScalar ETComplex128 -> "c128"
    | IRTNamed n -> n
    | IRTUnit -> "unit"
    | IRTIdxTagged (inner, _) -> canonTypeKey inner
    | IRTUnitAnnotated (inner, _) -> canonTypeKey inner
    | IRTPoly (b, _) -> "poly_" + canonTypeKey b
    | IRTTuple ts -> "tup_" + (ts |> List.map canonTypeKey |> String.concat "_")
    | ArrayElem arr ->
        let symTag =
            function
            | SymNone -> "0" | SymSymmetric -> "s"
            | SymAntisymmetric -> "a" | SymHermitian -> "h"
            // The LEVEL LIST is part of a wreath class's identity, so this
            // monomorphization key has to carry it: [(2,+),(2,+)] and
            // [(2,-),(2,-)] are both Rank 4 and would otherwise share a
            // specialization. Rendered inline (w2p2p) rather than as a bare "w".
            | SymWreath -> "w"
        let idxKey (idx: IRIndexType) =
            let ext, levelTag =
                match idx.Extent with
                | IRLit (IRLitInt n) -> string n, ""
                | IROrbitClass (levels, n) ->
                    (match n with IRLit (IRLitInt v) -> string v | _ -> "d"),
                    (levels |> List.map (fun (r, p) -> $"""{r}{(if p then "p" else "m")}""")
                            |> String.concat "")
                | _ -> "d", ""
            $"r{idx.Rank}s{symTag idx.Symmetry}{levelTag}e{ext}"
        $"""arr_{(canonTypeKey arr.ElemType)}__{(arr.IndexTypes |> List.map idxKey |> String.concat "_")}"""
    | IRTInfer id -> $"v{id}"
    | _ -> "T"

/// Generate a specialized copy of a function for a given set of type-var
/// bindings. Substitutes types throughout params, return, and body, and
/// mangles the name to encode the binding pattern.
let specializeHMFunction (func: IRFuncDef) (bindings: Map<int, IRType>) (builder: IRBuilder) (callables: Map<IRId, IRCallable>) : IRFuncDef * IRCallable list =
    let newParams =
        func.Params |> List.map (fun p ->
            { p with Type = substTypeInIRType bindings p.Type
                     VarId = builder.FreshId() })
    let newRetType = substTypeInIRType bindings func.RetType
    // Rewrite body: substitute types AND remap old param VarIds to new ones
    let varIdRemap =
        List.zip func.Params newParams
        |> List.map (fun (oldP, newP) -> (oldP.VarId, newP.VarId))
        |> Map.ofList
    let bodyWithTypes = substTypeInIRExpr bindings func.Body

    // Lifted lambdas capturing HM-polymorphic params must be cloned-and-
    // specialized alongside their enclosing function: the lambda lives in
    // `module.Functions`, so the `mapIRExpr` walk over `bodyWithTypes` only
    // reaches an `IRVar(lambdaId, _)` reference -- its own body and
    // Captures.Type still hold the unsubstituted T, and Captures.Id still
    // points at the pre-spec function's param VarIds, failing validation
    // (dangling VarId, unresolved IRTInfer). Fix: for each lifted callable
    // the body references whose captures intersect this function's params,
    // clone it -- fresh ids for its own params, captures' Ids/types remapped
    // via `varIdRemap`/`bindings`, body's IRVar refs via the combined map.
    // The original lambda stays in module.Functions unchanged.
    let origParamIds = func.Params |> List.map _.VarId |> Set.ofList
    let lambdaClones = System.Collections.Generic.Dictionary<IRId, IRCallable>()
    // Ids applied directly in a body (heads of IRApp). These go through the
    // module-level call-site rewrite + memoized spec path, so we must NOT
    // clone them into this parent -- doing so would bypass spec dedup and
    // leave any inner HM calls in the clone unrewritten. Computed per body,
    // since the discovery walk below visits clone bodies too.
    let appliedIdsOf (body: IRExpr) =
        let acc = System.Collections.Generic.HashSet<IRId>()
        iterIRExpr (fun e ->
            match e with
            | IRApp (IRVar (id, _), _, _) -> acc.Add id |> ignore
            | _ -> ()) body
        acc
    let needsClone (appliedIds: System.Collections.Generic.HashSet<IRId>) (c: IRCallable) : bool =
        // (a) closures capturing one of this function's params, or (b)
        // HM-polymorphic callables referenced as first-class values (e.g. an
        // operator-section lambda passed as a `reduce` kernel): the
        // module-level pass drops every un-applied HM function, so without a
        // clone the spec body would reference a deleted id. Applied HM
        // callees are excluded -- they specialize via the normal spec path.
        (c.Captures |> List.exists (fun cap -> Set.contains cap.Id origParamIds))
        || (hasTypeVarsInSignature c && not (appliedIds.Contains c.Id))
    // Walk bodyWithTypes to identify referenced lambdas needing clones --
    // TRANSITIVELY. A lifted kernel can reference a SECOND callable as a value
    // (`__lambda_49`'s broadcast body holding `__lambda_48`), and that second
    // one is visible only from inside the first one's body. Scanning the
    // parent's body alone left it uncloned while the module-level filter
    // dropped it for having type vars in its params, so the clone referenced a
    // deleted id -- BL6001 "dangling VarId reference: v48" on `functions/055`.
    // Worklist over clone bodies, terminating because `lambdaClones` is keyed
    // by ORIGINAL id and each id is cloned at most once.
    let pendingBodies = System.Collections.Generic.Queue<IRExpr>()
    pendingBodies.Enqueue bodyWithTypes
    while pendingBodies.Count > 0 do
        let scanBody = pendingBodies.Dequeue()
        let appliedIds = appliedIdsOf scanBody
        mapIRExpr (fun e ->
            (match e with
             | IRVar (id, _) when callables.ContainsKey id && not (lambdaClones.ContainsKey id) ->
                 let lam = callables.[id]
                 if needsClone appliedIds lam then
                     let cloneId = builder.FreshId()
                     let newCaps =
                         lam.Captures |> List.map (fun cap ->
                             let newId =
                                 match Map.tryFind cap.Id varIdRemap with
                                 | Some n -> n
                                 | None -> cap.Id
                             { cap with Id = newId; Type = substTypeInIRType bindings cap.Type })
                     // Clone lambda's own params with fresh VarIds (independent
                     // of the parent's param remap). The combined remap
                     // covers both parent's captures-as-our-captures and
                     // our local params.
                     let paramRemap =
                         lam.Params |> List.map (fun p -> (p.VarId, builder.FreshId())) |> Map.ofList
                     let newParams' =
                         lam.Params |> List.map (fun p ->
                             { p with VarId = paramRemap.[p.VarId]
                                      Type = substTypeInIRType bindings p.Type })
                     let combinedRemap =
                         varIdRemap
                         |> Map.fold (fun acc k v -> Map.add k v acc) paramRemap
                     let newBody =
                         lam.Body
                         |> substTypeInIRExpr bindings
                         |> mapIRExpr (fun e2 ->
                             match e2 with
                             | IRVar (id2, ty) when combinedRemap.ContainsKey id2 ->
                                 IRVar (combinedRemap.[id2], ty)
                             | _ -> e2)
                     let newRet = substTypeInIRType bindings lam.RetType
                     let clone =
                         { lam with
                             Id = cloneId
                             Name = $"{lam.Name}_HM_{cloneId}"
                             Params = newParams'
                             Captures = newCaps
                             Body = newBody
                             RetType = newRet }
                     lambdaClones.[id] <- clone
                     pendingBodies.Enqueue newBody
             | _ -> ())
            e) scanBody |> ignore

    // Point every reference at the CLONE. The parent's body needs its param
    // remap as well; clone bodies already had theirs applied at construction,
    // and only need the callable-id redirect -- but they DO need it, or a
    // clone keeps calling the about-to-be-deleted original (the other half of
    // the `functions/055` dangle).
    let redirectToClones (e: IRExpr) =
        match e with
        | IRVar (id, _) when lambdaClones.ContainsKey id ->
            let clone = lambdaClones.[id]
            let funcTy = mkFuncArrow (clone.Params |> List.map _.Type) clone.RetType
            IRVar (clone.Id, funcTy)
        | _ -> e
    let bodyRewritten =
        mapIRExpr (fun e ->
            match e with
            | IRVar (id, ty) when varIdRemap.ContainsKey id -> IRVar (varIdRemap.[id], ty)
            | _ -> redirectToClones e) bodyWithTypes
    for kv in lambdaClones |> Seq.toList do
        lambdaClones.[kv.Key] <- { kv.Value with Body = mapIRExpr redirectToClones kv.Value.Body }
    // Name-mangle by binding signature, using the occurrence-id-independent
    // canonTypeKey so the emitted name matches the HM dedup key exactly (arrays
    // don't collapse to a colliding "T"; see canonTypeKey).
    let suffix =
        bindings
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (id, ty) -> $"_{id}_{canonTypeKey ty}")
        |> String.concat ""
    let spec =
        { func with
            Id = builder.FreshId()
            Name = $"{func.Name}_HM{suffix}"
            Params = newParams
            RetType = newRetType
            Body = bodyRewritten }
    let clonesList = lambdaClones.Values |> List.ofSeq
    (spec, clonesList)

/// Driver: monomorphize all HM-polymorphic functions in a module.
///
/// An *iterative* fixpoint, not single-pass: specialization can expose new
/// concrete types (specializing `twiceId(x: T) -> T = id(id(x))` with
/// `T -> Int64` makes the inner `id(x)` call's arg concrete, licensing
/// `id`'s own specialization). A single pass would see `id(x)`'s arg as
/// still-abstract `IRTInfer 10001`. The loop runs until no new
/// (funcId, bindings) keys appear.
///
/// Also (a) substitutes call-site-learned bindings into a binding's
/// *declared type* (TypeCheck leaves it `IRTInfer N` when the call site's
/// return was polymorphic), and (b) substitutes types throughout each spec
/// body so it's free of `IRTInfer`.
let monomorphizeHMFunctions (modul: IRModule) (builder: IRBuilder) : IRModule =
    // 1. Identify functions with type vars in signature
    // Only functions with type vars in their PARAMETERS get dropped-and-
    // specialized per call site. Return-only-type-var functions (e.g. a former
    // kernel `lambda(x) -> hmHelper(x)` whose params are concrete but whose
    // return echoes the helper's abstract return) stay in newFunctions, where
    // their body is call-site-rewritten and their return type substituted.
    let hmFuncs = modul.Functions |> List.filter hasTypeVarsInParams
    if hmFuncs.IsEmpty then modul
    else
    let hmFuncMap = hmFuncs |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    let hmFuncIdSet = hmFuncs |> List.map _.Id |> Set.ofList

    // 2. Iterate to fixpoint: each round inspects the original module's
    //    expressions AND earlier-round spec bodies (an HM call's arg types
    //    may only become concrete after substitution). Keyed by (funcId,
    //    canonical (varId, type-key) list) -- occurrence-id-independent
    //    (canonTypeKey), so the same specialization from two differently-
    //    numbered call chains dedups to one spec.
    let mutable specMap : Map<IRId * (int * string) list, IRFuncDef> = Map.empty
    // Cloned lambdas accumulated across spec generation.
    // See specializeHMFunction's clone logic.
    let lambdaClones = System.Collections.Generic.List<IRCallable>()
    // EMISSION ORDER for the functions this pass synthesizes, keyed on the
    // ORIGIN they were derived from (IRModule.DerivedFuncOrigins; the rule and
    // its rationale live at CodeGen.emissionOrderedItems). Shape
    // monomorphization has always registered its copies; HM specs and their
    // lambda clones did not, and their freshly-minted ids sort after every
    // call site that names them. That is invisible while the caller is a
    // namespace-scope C++ function -- a forward declaration covers it -- and
    // FATAL once the caller is one of the `std::function` locals
    // `computeMainLocalFuncIds` puts inside main(), which get no forward
    // declaration: "'family_spectra_HM_...' was not declared in this scope".
    // Measured on examples/lswosa.blade, whose whole call chain is main-local
    // (it captures module bindings) and HM-specialized at every level.
    let mutable derivedOrigins : Map<IRId, IRId> = Map.empty
    let mutable changed = true
    let mutable iterationGuard = 0
    let MAX_ITERATIONS = 16  // pathological safety net; real programs converge in 2-3
    while changed && iterationGuard < MAX_ITERATIONS do
        changed <- false
        iterationGuard <- iterationGuard + 1

        // Sources of call sites: original module + already-generated specs.
        // Spec bodies need scanning because an outer spec's body, after type
        // substitution, may contain HM calls with newly-concrete arg types.
        let sitesFromFuncs =
            modul.Functions |> List.collect (fun f -> collectHMCallSites hmFuncMap f.Body)
        let sitesFromBindings =
            modul.Bindings |> List.collect (fun b -> collectHMCallSites hmFuncMap b.Value)
        let sitesFromSpecs =
            specMap |> Map.toList
                    |> List.collect (fun (_, spec) -> collectHMCallSites hmFuncMap spec.Body)
        // AND CLONE BODIES, for the same reason spec bodies are scanned. A
        // lifted kernel that calls an HM helper (`lambda(trow, srow) ->
        // hanning((trow, srow), ..)`) is cloned-and-substituted alongside its
        // enclosing spec, and only in the CLONE do that inner call's argument
        // types become concrete -- in the original the helper's own signature
        // vars are still bound to the enclosing function's vars. Without this
        // source the helper's concrete specialization was never requested, the
        // clone kept calling the (about-to-be-dropped) abstract original, and
        // its still-open var reached the validator as BL6001. Measured on
        // `examples/lswosa.blade`: `family_spectra`'s grid kernel, calling
        // `hanning` and `wosa_lsdft`.
        let sitesFromClones =
            lambdaClones |> Seq.collect (fun c -> collectHMCallSites hmFuncMap c.Body) |> List.ofSeq
        // Dedup on the SPEC KEY (the canonTypeKey string form), not on raw
        // `IRType` trees: `List.distinct` had to structurally hash and compare
        // whole type trees -- extent expressions included -- once per call site
        // per fixpoint round. The key is what the loop body already computes and
        // what `specMap` is keyed on, so deduping on it is behaviour-preserving:
        // two sites sharing a key would have had the second one skipped by the
        // `Map.containsKey key specMap` test anyway, and `distinctBy` keeps the
        // same (first) occurrence `List.distinct` did. `allConcrete` and
        // `paramVarsCovered` are both functions of the key alone -- an `IRTInfer
        // n` renders as `vn` and nothing else does -- so key-equal sites never
        // disagree about whether they are specializable.
        let uniqueSites =
            (sitesFromFuncs @ sitesFromBindings @ sitesFromSpecs @ sitesFromClones)
            |> List.map (fun (funcId, sortedBindings) ->
                ((funcId, sortedBindings |> List.map (fun (id, ty) -> (id, canonTypeKey ty))),
                 (funcId, sortedBindings)))
            |> List.distinctBy fst

        for (key, (funcId, sortedBindings)) in uniqueSites do
            // Only generate specs whose bindings are entirely concrete. A
            // self-binding like (10001, IRTInfer 10002) means the call site
            // was inside a still-abstract context; the fixpoint revisits it
            // after the surrounding spec is generated and its types become
            // concrete.
            let allConcrete =
                sortedBindings |> List.forall (fun (_, v) ->
                    match v with IRTInfer _ -> false | _ -> true)
            // Also require the call site to have resolved every type var in
            // the callee's PARAMETERS -- an empty/partial binding means the
            // call sits inside a still-abstract enclosing function (e.g. a
            // pack element's not-yet-concrete type); the fixpoint revisits
            // once the enclosing function is specialized.
            let origFunc = hmFuncMap.[funcId]
            let paramVarIds =
                origFunc.Params
                |> List.fold (fun s p -> Set.union s (collectInferIds p.Type)) Set.empty
            let boundIds = sortedBindings |> List.map fst |> Set.ofList
            let paramVarsCovered =
                paramVarIds |> Set.forall (fun id -> Set.contains id boundIds)
            if allConcrete && paramVarsCovered && not (Map.containsKey key specMap) then
                let bindingMap = sortedBindings |> Map.ofList
                let availableCallables =
                    modul.Functions @ (lambdaClones |> List.ofSeq)
                    |> List.map (fun f -> (f.Id, f))
                    |> Map.ofList
                let (spec, clones) = specializeHMFunction origFunc bindingMap builder availableCallables
                specMap <- Map.add key spec specMap
                lambdaClones.AddRange(clones)
                // Both the spec and every lambda it cloned belong at the
                // ORIGIN's program point: the spec replaces calls the origin
                // served, and the clones are referenced only from the spec.
                derivedOrigins <- Map.add spec.Id origFunc.Id derivedOrigins
                for c in clones do
                    derivedOrigins <- Map.add c.Id origFunc.Id derivedOrigins
                changed <- true

    // 3. Build the call-site rewriter using the now-frozen specMap.
    //    Same logic as before, but operating against a complete spec map.
    let rewriteCallSite e =
        match e with
        | IRApp (IRVar (funcId, _), args, _) when hmFuncMap.ContainsKey funcId ->
            let func = hmFuncMap.[funcId]
            let bindings =
                if args.Length <> func.Params.Length then Map.empty
                else
                    List.zip func.Params args
                    |> List.fold (fun acc (p, arg) ->
                        match exprTypeIfKnown arg with
                        | Some argTy -> unifyParamWithArg p.Type argTy acc
                        | None -> acc) Map.empty
            let sortedBindings = bindings |> Map.toList |> List.sortBy fst
            let key = (funcId, sortedBindings |> List.map (fun (id, ty) -> (id, canonTypeKey ty)))
            match Map.tryFind key specMap with
            | Some spec ->
                IRApp (IRVar (spec.Id, mkFuncArrow (spec.Params |> List.map _.Type) spec.RetType),
                       args, spec.RetType)
            | None -> e
        | _ -> e

    // 4. Build a *global*, conflict-free type-var binding map for the whole
    //    module: a downstream binding like `let r = result(0)` references
    //    the same IRTInfer N as the upstream `let result = arr_id(xs)`
    //    (TypeCheck propagates a polymorphic return through dependents
    //    without generalizing top-level functions), so per-binding-local
    //    substitution alone wouldn't fix r.Type. If the same ID binds to
    //    different concrete types at different call sites, drop it from the
    //    global map -- the per-call-site rewrite still produces the right
    //    specs, and the per-binding fallback covers r.Type.
    let collectAllBindingsFromExpr (expr: IRExpr) : (int * IRType) list =
        let calleeSide =
            collectHMCallSites hmFuncMap expr
            |> List.collect (fun (_, sortedBindings) -> sortedBindings)
        // The caller's own vars, which a cross-module call site can only teach
        // through its return type (see collectHMCallSiteReturnBindings).
        // Substitution-only: these never reach a specialization key.
        let callerSide = collectHMCallSiteReturnBindings hmFuncMap expr
        calleeSide @ callerSide
        |> List.choose (fun (k, v) ->
            match v with
            | IRTInfer _ -> None  // self-binding; ignore
            | _ -> Some (k, v))
    let allObservedBindings : (int * IRType) list =
        let fromFns =
            modul.Functions |> List.collect (fun f -> collectAllBindingsFromExpr f.Body)
        let fromBindings =
            modul.Bindings |> List.collect (fun b -> collectAllBindingsFromExpr b.Value)
        let fromSpecs =
            specMap |> Map.toList
                    |> List.collect (fun (_, s) -> collectAllBindingsFromExpr s.Body)
        let fromClones =
            lambdaClones |> Seq.collect (fun c -> collectAllBindingsFromExpr c.Body) |> List.ofSeq
        fromFns @ fromBindings @ fromSpecs @ fromClones
    // Group by ID; keep only IDs whose observations all agree.
    let globalBindings : Map<int, IRType> =
        allObservedBindings
        |> List.groupBy fst
        |> List.choose (fun (id, pairs) ->
            let distinctTypes = pairs |> List.map snd |> List.distinct
            match distinctTypes with
            | [singleTy] -> Some (id, singleTy)
            | _ -> None)  // conflict -- leave alone, per-call-site rewrite handles each
        |> Map.ofList

    // Per-binding-local fallback: for a call sitting directly in a binding's
    // value, when global bindings don't already cover the IDs in its
    // declared type.
    let unionBindingsFromExpr (expr: IRExpr) : Map<int, IRType> =
        collectAllBindingsFromExpr expr
        |> List.fold (fun acc (k, v) ->
            match Map.tryFind k acc with
            | Some _ -> acc
            | None -> Map.add k v acc) Map.empty

    // 5. Rewrite all expressions; substitute binding declared types using
    //    the union of (global, per-binding-local) bindings, and rewrite call
    //    sites inside spec bodies. Local bindings win over global on
    //    conflict (each call site is locally consistent).
    let mergeBindings (local: Map<int, IRType>) : Map<int, IRType> =
        local |> Map.fold (fun acc k v -> Map.add k v acc) globalBindings
    // The ordinary rewrite every surviving function gets: inner HM calls
    // repointed at their specs, residual IRTInfer substituted out of the body,
    // signature and captures.
    let rewriteFunc (f: IRFuncDef) =
        let bindings = mergeBindings (unionBindingsFromExpr f.Body)
        let bodyWithRewrittenCalls = mapIRExpr rewriteCallSite f.Body
        let bodyWithSubstitutedTypes = substTypeInIRExpr bindings bodyWithRewrittenCalls
        { f with Body = bodyWithSubstitutedTypes
                 RetType = substTypeInIRType bindings f.RetType
                 Params = f.Params |> List.map (fun p ->
                            { p with Type = substTypeInIRType bindings p.Type })
                 Captures = f.Captures |> List.map (fun c ->
                            { c with Type = substTypeInIRType bindings c.Type }) }
    // Captures carry types too (a former kernel captures the HM helper it
    // calls); leaving them abstract emits an unresolved-type sentinel in the
    // lifted lambda's signature -- see rewriteFunc.
    let newFunctions =
        modul.Functions
        |> List.filter (fun f -> not (Set.contains f.Id hmFuncIdSet))
        |> List.map rewriteFunc
    let newBindings =
        modul.Bindings
        |> List.map (fun b ->
            let bindings = mergeBindings (unionBindingsFromExpr b.Value)
            let newType = substTypeInIRType bindings b.Type
            let valueWithRewrittenCalls = mapIRExpr rewriteCallSite b.Value
            let valueWithSubstitutedTypes = substTypeInIRExpr bindings valueWithRewrittenCalls
            { b with Type = newType; Value = valueWithSubstitutedTypes })
    // Spec function bodies need the same treatment as ordinary function
    // bodies: their inner HM calls (e.g. `id(id(x))` inside `twiceId`'s spec)
    // must be rewritten to point at the inner specs, and any residual
    // IRTInfer in their expression-tree types substituted out.
    let specFuncs =
        specMap
        |> Map.toList
        |> List.map (fun (_, spec) ->
            let bindings = mergeBindings (unionBindingsFromExpr spec.Body)
            let bodyWithRewrittenCalls = mapIRExpr rewriteCallSite spec.Body
            let bodyWithSubstitutedTypes = substTypeInIRExpr bindings bodyWithRewrittenCalls
            { spec with Body = bodyWithSubstitutedTypes
                        RetType = substTypeInIRType bindings spec.RetType
                        Params = spec.Params |> List.map (fun p ->
                                   { p with Type = substTypeInIRType bindings p.Type })
                        Captures = spec.Captures |> List.map (fun c ->
                                   { c with Type = substTypeInIRType bindings c.Type }) })

    // Clone bodies get the SAME treatment as ordinary and spec function
    // bodies, and for the same two reasons: their inner HM calls must be
    // rewritten to the specs the fixpoint generated for them (the abstract
    // originals are dropped from `newFunctions` just below), and any residual
    // IRTInfer -- in the body, the return type, the params, or a CAPTURE's
    // type -- substituted out. Appending them raw left an lswosa kernel clone
    // still calling the deleted `hanning` and carrying `T?10014` in both its
    // body and its captured function-value type.
    let cloneFuncs =
        lambdaClones
        |> List.ofSeq
        |> List.map (fun c ->
            let bindings = mergeBindings (unionBindingsFromExpr c.Body)
            let bodyWithRewrittenCalls = mapIRExpr rewriteCallSite c.Body
            let bodyWithSubstitutedTypes = substTypeInIRExpr bindings bodyWithRewrittenCalls
            { c with Body = bodyWithSubstitutedTypes
                     RetType = substTypeInIRType bindings c.RetType
                     Params = c.Params |> List.map (fun p ->
                                { p with Type = substTypeInIRType bindings p.Type })
                     Captures = c.Captures |> List.map (fun cap ->
                                { cap with Type = substTypeInIRType bindings cap.Type }) })
    // ORPHAN RESCUE: an hmFunc that nothing replaced must not be deleted.
    //
    // Dropping the abstract originals is correct for a function whose call
    // sites were rewritten to specs -- but the collector only recognises a
    // call site as `IRApp (IRVar (funcId, _), args, _)`. A lifted callable
    // referenced ONLY as a first-class KERNEL VALUE (an IRApplyCombinator /
    // IRComposeApply kernel slot) is never such a call site, so no spec is
    // ever requested for it -- and it was dropped anyway, leaving the kernel
    // slot pointing at nothing:
    //
    //     error[BL6001]: dangling VarId reference: v23
    //     error[BL6001]: ApplyInfo: kernel slot is IRVar(v23) [id resolves in
    //                    neither CallablesTable nor synthetic registry]
    //
    // Reached by `reduce(cells, (+))` over a rank-2 returned from a function
    // with abstract params: the partial fold's synthesized row kernel takes
    // `T^1`, so `hasTypeVarsInParams` classifies it, and it is only ever named
    // from the fold's kernel slot.
    //
    // The rule is "keep what nothing replaced", decided on the REWRITTEN
    // module: every genuine call site has by now been repointed at its spec,
    // so a surviving `IRVar(f.Id)` means a reference the specialization could
    // not express. Such a function is kept and rewritten like any other -- its
    // residual type vars are the enclosing program's, which `globalBindings`
    // resolves. A truly dead abstract original is referenced by nothing and
    // still goes away, so this adds no unresolved-type signatures.
    //
    // Iterated to a fixpoint because a rescued function's own body can be the
    // only place another one is named.
    let rescuedHmFuncs =
        if Set.isEmpty hmFuncIdSet then []
        else
            let hmById = hmFuncs |> List.map (fun f -> (f.Id, f)) |> Map.ofList
            let referencedIn (e: IRExpr) =
                let acc = System.Collections.Generic.HashSet<IRId>()
                iterIRExpr (fun n ->
                    match n with
                    | IRVar (id, _) when hmById.ContainsKey id -> acc.Add id |> ignore
                    | _ -> ()) e
                acc
            let seedRefs =
                let acc = System.Collections.Generic.HashSet<IRId>()
                for f in newFunctions do acc.UnionWith(referencedIn f.Body)
                for b in newBindings do acc.UnionWith(referencedIn b.Value)
                for f in specFuncs do acc.UnionWith(referencedIn f.Body)
                for c in cloneFuncs do acc.UnionWith(referencedIn c.Body)
                acc
            let mutable keep : Map<IRId, IRFuncDef> = Map.empty
            let mutable frontier = seedRefs |> List.ofSeq
            while not frontier.IsEmpty do
                let next = System.Collections.Generic.HashSet<IRId>()
                for id in frontier do
                    if not (Map.containsKey id keep) then
                        match Map.tryFind id hmById with
                        | Some f ->
                            let rewritten = rewriteFunc f
                            keep <- Map.add id rewritten keep
                            next.UnionWith(referencedIn rewritten.Body)
                        | None -> ()
                frontier <- next |> Seq.filter (fun i -> not (Map.containsKey i keep)) |> List.ofSeq
            // Original module order, so emission order is unchanged for them.
            hmFuncs |> List.choose (fun f -> Map.tryFind f.Id keep)

    { modul with
        Functions = newFunctions @ rescuedHmFuncs @ specFuncs @ cloneFuncs
        Bindings = newBindings
        DerivedFuncOrigins =
            derivedOrigins
            |> Map.fold (fun acc k v -> Map.add k v acc) modul.DerivedFuncOrigins }

/// Whole-program driver for `monomorphizeHMFunctions`.
///
/// HM specialization is CALL-SITE driven, so a generic function DEFINED in one
/// module and CALLED from another had nowhere to learn its bindings. Run per
/// module, the defining module sees no call site at all (its generic is dropped
/// as an uninstantiated polymorph) and the calling module does not own the
/// callee, so the call survived carrying `IRTInfer` -- a guaranteed BL6001
/// "unresolved type variable" plus a dangling VarId. The practical consequence
/// was that a stdlib written in Blade source could not export anything generic;
/// `stdlib/plot.blade` says so in its own header and shares only String-typed
/// helpers because of it.
///
/// The fix is the one `shapeMonomorphizeModules` already applies for extents:
/// run the pass ONCE over a merged view of every module's functions and
/// bindings, then split the result back. Sound for the same two reasons given
/// there -- a module's EXPORTS are fixed by `lowerTypedModule` before any of
/// these passes run, so widening what this pass can see cannot change what a
/// later module resolves; and both back ends already merge modules before
/// consuming them (`CodeGen.genProgramFromIR`, `Interp.Run.printableModule`),
/// so which module a synthesized spec lands in is not observable.
///
/// SPLIT-BACK. Bindings are rewritten 1:1 and in order, so per-module counts
/// restore them exactly. A surviving function keeps its Id and returns to its
/// own module. A SYNTHESIZED function (spec or lambda clone) follows its origin
/// through `DerivedFuncOrigins`, mirroring shapeMonomorphizeModules' "record
/// the copy in the module that DEFINES its origin"; an unknown origin lands in
/// the entry module.
///
/// A single-module program takes the fast path and is bit-identical to before,
/// down to the ids the builder mints.
let monomorphizeHMFunctionsModules (modules: IRModule list) (builder: IRBuilder) : IRModule list =
    match modules with
    | [] -> []
    | [ single ] -> [ monomorphizeHMFunctions single builder ]
    | _ ->

    let merged =
        { modules.Head with
            Functions = modules |> List.collect _.Functions
            Bindings = modules |> List.collect _.Bindings
            DerivedFuncOrigins =
                modules |> List.fold (fun acc m ->
                    m.DerivedFuncOrigins |> Map.fold (fun a k v -> Map.add k v a) acc) Map.empty }
    let out = monomorphizeHMFunctions merged builder

    // Where each PRE-EXISTING function id came from.
    let homeOf =
        modules
        |> List.mapi (fun i m -> m.Functions |> List.map (fun f -> (f.Id, i)))
        |> List.concat
        |> Map.ofList
    let lastIdx = modules.Length - 1
    let placementOf (f: IRFuncDef) =
        match Map.tryFind f.Id homeOf with
        | Some i -> i
        | None ->
            match Map.tryFind f.Id out.DerivedFuncOrigins with
            | Some origin -> Map.tryFind origin homeOf |> Option.defaultValue lastIdx
            | None -> lastIdx
    let funcsByModule = out.Functions |> List.groupBy placementOf |> Map.ofList

    let mutable remainingBindings = out.Bindings
    modules
    |> List.mapi (fun i m ->
        let n = m.Bindings.Length
        let mine = remainingBindings |> List.truncate n
        remainingBindings <- remainingBindings |> List.skip (min n remainingBindings.Length)
        { m with
            Functions = Map.tryFind i funcsByModule |> Option.defaultValue []
            Bindings = mine
            // The whole derived-origin map rides on the entry module: codegen
            // and the interpreter both UNION these across modules, so one
            // carrier is enough and duplicating it would only invite drift.
            DerivedFuncOrigins =
                if i = lastIdx then
                    out.DerivedFuncOrigins
                    |> Map.fold (fun acc k v -> Map.add k v acc) m.DerivedFuncOrigins
                else m.DerivedFuncOrigins })

/// Post-monomorphization rewrite: a raw *elementwise* `IRBinOp` whose
/// operands are BOTH arrays becomes the `method_for(zip ..) <@> kernel |>
/// compute` co-iteration combinator -- the same shape TypeCheck.inferBinOp
/// synthesizes for a top-level `x + y`.
///
/// Pack-element operands can't be recognized at lowering/type-check time:
/// in `firstsum(A: Poly<Float64^1>) = A[0] + A[1]` the element type is
/// unresolved until Poly + HM specialization substitutes the concrete
/// `Array<..>` in. Running here (after BOTH monomorphizers) the operand
/// types are concrete. Without this the binop stays a raw `Array op Array`
/// with no C++ operator overload, and the interpreter rejects it (BL8010).
///
/// The synthesized `lambda(a, b) -> a op b` kernel closes over nothing.
/// Only Elementwise mode is rewritten (pack-element `A[i] + A[j]`); outer
/// products and scalar/broadcast binops are left untouched.
let lowerArrayBinOpsModule (modul: IRModule) (builder: IRBuilder) : IRModule =
    let newLambdas = System.Collections.Generic.List<IRCallable>()
    let isCmpOrLogical op =
        match op with
        | IREq | IRNeq | IRLt | IRLe | IRGt | IRGe | IRAnd | IROr -> true
        | _ -> false
    // Distinct identity per distinct operand var so codegen's symmetry
    // deduction treats `A_0 + A_1` as two different arrays (and `A_0 + A_0` as
    // the same array -- correct commutative collapse).
    let identityOf e =
        match e with
        | IRVar (id, _) -> AIDVariable $"__coi{id}"
        | _ -> AIDVariable "__coi"
    // Operand type, seeing through a `|> compute` wrapper so a *nested* array
    // binop -- whose inner rewrite already produced `IRCompute(IRApplyCombinator)`
    // (an array-typed, but not CarriedType-tagged, node) -- is still recognized
    // as an array operand by the enclosing binop (`A[0] + A[1] + A[2]`).
    let rec operandType e =
        match e with
        | IRCompute inner -> operandType inner
        // See through a let (e.g. the scalar-materialization wrapper a nested
        // array-scalar broadcast produces) to the value it ultimately yields.
        | IRLet (_, _, body) -> operandType body
        | CarriedType ty -> Some ty
        | _ -> None
    // TRIGGER PRE-SCAN. `rewrite` below fires on exactly three operand-type
    // pairs -- (array, array), (array, scalar), (scalar, array) -- so a module
    // with no array-typed elementwise binop anywhere cannot be changed by this
    // pass, and rebuilding every function body and binding value to discover
    // that is pure waste (post-TypeCheck, almost every `x + y` over arrays is
    // ALREADY a combinator; this pass exists only for pack elements whose array
    // type resolves after monomorphization).
    //
    // Deliberately an OVER-approximation: `Some _, Some (ArrayElem _)` also
    // admits pairs `rewrite` falls through on, which costs a no-op pass, never
    // a missed rewrite. Nesting is covered because the pass is bottom-up: an
    // outer binop only becomes array-typed once an INNER one was rewritten, and
    // that inner one is itself a trigger in the un-rewritten tree.
    let isArrayBinOpTrigger (e: IRExpr) : bool =
        match e with
        | IRBinOp (IRElementwise, _, l, r) ->
            (match operandType l, operandType r with
             | Some (ArrayElem _), Some _ | Some _, Some (ArrayElem _) -> true
             | _ -> false)
        | _ -> false
    let hasArrayBinOp =
        let mutable hit = false
        let scan (b: IRExpr) =
            if not hit then
                iterIRExpr (fun e -> if not hit && isArrayBinOpTrigger e then hit <- true) b
        modul.Functions |> List.iter (fun f -> scan f.Body)
        modul.Bindings |> List.iter (fun b -> scan b.Value)
        hit
    if not hasArrayBinOp then modul
    else
    // Broadcast a scalar against an array (`arr op scalar` / `scalar op
    // arr`): value-space twin of TypeCheck.inferBinOp's array-scalar path,
    // for pack elements whose array type only resolves post-monomorphization
    // (e.g. `head - mean(head)` in a Poly<T^1> kernel). The scalar is
    // materialized into a captured local; a single-array method_for maps it.
    let broadcastScalar op (arr: IRExpr) (arrTy: IRType) (la: IRArrayType)
                        (scalarE: IRExpr) (sElem: ElemType) (scalarOnLeft: bool) : IRExpr =
        let arrElem = match la.ElemType with PrimElem et -> et | _ -> ETFloat64
        let kernelRet = if isCmpOrLogical op then IRTScalar ETBool else IRTScalar arrElem
        let sId = builder.FreshId()
        let sTy = IRTScalar sElem
        let xId = builder.FreshId()
        let xVar = IRVar (xId, IRTScalar arrElem)
        let sVar = IRVar (sId, sTy)
        // Kernel `lambda(__bx) -> __bx op s` (or `s op __bx`); `s` is captured.
        let kbody =
            if scalarOnLeft then IRBinOp (IRElementwise, op, sVar, xVar)
            else IRBinOp (IRElementwise, op, xVar, sVar)
        let parms : IRParam list =
            [ { Name = "__bx"; Type = IRTScalar arrElem; Index = 0; VarId = xId } ]
        let cap : CaptureInfo = { Id = sId; Name = $"__v{sId}"; Type = sTy; IsMutable = false }
        let lam = mkLambdaCallable builder parms kbody kernelRet [cap] false [] [] false false 256 false
        newLambdas.Add lam
        let kernelFuncType = IRTArrow ([SVal (IRTScalar arrElem)], kernelRet, None)
        let ident = identityOf arr
        let sdims = la.IndexTypes.Length
        let outputType =
            match arrTy with
            | IRTArrow (slots, _, id2) -> IRTArrow (slots, kernelRet, id2)
            | _ -> arrTy
        let mfInfo : MethodForInfo =
            { Arrays = [arr]; Identities = [ident]; ArrayTypes = [la]
              SDimsPerArray = [sdims]; TotalSDims = sdims; SharedIndexTypes = [] }
        let applyInfo : ApplyInfo =
            { Loop = IRMethodFor mfInfo
              Kernel = IRVar (lam.Id, kernelFuncType)
              Arrays = [arr]; Identities = [ident]; ArrayTypes = [la]
              SharedIndexTypes = []; SymcomStates = [SCNeither]; TriangularLevels = [false]
              SDimsPerArray = [sdims]; KernelInputRanks = [0]; KernelOutputRank = 0
              KernelTDims = []; SpeedupFactor = 1L; ReynoldsSpeedup = 1L; HasReynolds = false
              OutputType = outputType; IsCoIteration = false }
        // Materialize the scalar once, outside the loop, as the captured local.
        IRLet (sId, scalarE, IRCompute (IRApplyCombinator applyInfo))
    let rewrite (e: IRExpr) : IRExpr =
        match e with
        | IRBinOp (IRElementwise, op, l, r) ->
            match operandType l, operandType r with
            | Some ((ArrayElem la) as lt), Some ((ArrayElem ra) as rt) ->
                let elemTypeL = match la.ElemType with PrimElem et -> et | _ -> ETFloat64
                let elemTypeR = match ra.ElemType with PrimElem et -> et | _ -> ETFloat64
                let kernelRet =
                    if isCmpOrLogical op then IRTScalar ETBool else IRTScalar elemTypeL
                // Co-iteration kernel: lambda(__zl, __zr) -> __zl op __zr.
                let aId = builder.FreshId()
                let bId = builder.FreshId()
                let kbody =
                    IRBinOp (IRElementwise, op,
                             IRVar (aId, IRTScalar elemTypeL),
                             IRVar (bId, IRTScalar elemTypeR))
                let parms : IRParam list =
                    [ { Name = "__zl"; Type = IRTScalar elemTypeL; Index = 0; VarId = aId }
                      { Name = "__zr"; Type = IRTScalar elemTypeR; Index = 1; VarId = bId } ]
                let lam =
                    mkLambdaCallable builder parms kbody kernelRet [] false [] [] false false 256 false
                newLambdas.Add lam
                let kernelFuncType =
                    IRTArrow ([SVal (IRTScalar elemTypeL); SVal (IRTScalar elemTypeR)], kernelRet, None)
                // Materialize non-variable operands into let-bindings so the loop
                // reads NAMED arrays. A function-call operand (`head + packsum1(tail)`
                // in a recursive pack kernel) must be evaluated once and bound --
                // codegen's loop reads `arr[i]` by the operand's name, and an
                // unnamed call expression there is an undeclared identifier.
                let mutable prelude : (IRId * IRExpr) list = []
                let materialize (operand: IRExpr) (ty: IRType) : IRExpr =
                    match operand with
                    | IRVar _ -> operand
                    | _ ->
                        let id = builder.FreshId()
                        prelude <- prelude @ [(id, operand)]
                        IRVar (id, ty)
                let lArr = materialize l lt
                let rArr = materialize r rt
                let identities = [identityOf lArr; identityOf rArr]
                let arrayTypes = [la; ra]
                // Shared co-iteration record: the left array's index axes (both
                // operands share the same iteration space -- the elementwise
                // conformance the type-checker guaranteed).
                let sharedIdx = la.IndexTypes
                let sdimsPerArray = [la.IndexTypes.Length; ra.IndexTypes.Length]
                let totalSDims = List.sum sdimsPerArray
                let mfInfo : MethodForInfo =
                    { Arrays = [lArr; rArr]
                      Identities = identities
                      ArrayTypes = arrayTypes
                      SDimsPerArray = sdimsPerArray
                      TotalSDims = totalSDims
                      SharedIndexTypes = sharedIdx }
                // Output array type: left operand's index axes, element type from
                // the kernel (Bool for comparison/logical, else arithmetic elem).
                let outputType =
                    match lt with
                    | IRTArrow (slots, _, ident) -> IRTArrow (slots, kernelRet, ident)
                    | _ -> lt
                let applyInfo : ApplyInfo =
                    { Loop = IRMethodFor mfInfo
                      Kernel = IRVar (lam.Id, kernelFuncType)
                      Arrays = [lArr; rArr]
                      Identities = identities
                      ArrayTypes = arrayTypes
                      SharedIndexTypes = sharedIdx
                      SymcomStates = [SCNeither; SCNeither]
                      TriangularLevels = [false; false]
                      SDimsPerArray = sdimsPerArray
                      KernelInputRanks = [0; 0]
                      KernelOutputRank = 0
                      KernelTDims = []
                      SpeedupFactor = 1L
                      ReynoldsSpeedup = 1L
                      HasReynolds = false
                      OutputType = outputType
                      IsCoIteration = true }
                let combined = IRCompute (IRApplyCombinator applyInfo)
                // Wrap in the hoisted operand bindings (outermost = first operand).
                List.foldBack (fun (id, v) acc -> IRLet (id, v, acc)) prelude combined
            | Some ((ArrayElem la) as lt), Some (IRTScalar sElem) ->
                broadcastScalar op l lt la r sElem false
            | Some (IRTScalar sElem), Some ((ArrayElem ra) as rt) ->
                broadcastScalar op r rt ra l sElem true
            | _ -> e
        | _ -> e
    let rewriteExpr expr = mapIRExpr rewrite expr
    let newFunctions =
        modul.Functions |> List.map (fun f -> { f with Body = rewriteExpr f.Body })
    let newBindings =
        modul.Bindings |> List.map (fun b -> { b with Value = rewriteExpr b.Value })
    { modul with
        Functions = newFunctions @ (newLambdas |> List.ofSeq)
        Bindings = newBindings }

// Elementwise-chain fusion (docs/plans/plan-fortran-killer.md, arc 1).
//
// TypeCheck desugars every elementwise array binop into its own
// `method_for(zip(l, r)) <@> lambda |> compute` pipeline -- one deferred
// combinator per operator -- so `a + b * c - d` reaches codegen as three
// nested computations and materializes two intermediate pools. The identical
// hand-written `method_for(zip(a, b, c, d)) <@> lambda(w, x, y, z) ->
// w + x*y - z` emits one flat loop and zero temporaries. Same computation,
// two spellings, ~3x the memory traffic apart: a SAME-EMIT violation.
//
// This pass flattens the nest. An IRApplyCombinator whose operand slot holds
// a directly nested `IRCompute (IRApplyCombinator ..)` splices the inner
// combinator's operand arrays into its own list and substitutes the inner
// kernel body for the corresponding outer kernel parameter -- producing
// exactly the flat co-iteration shape the emitters already handle best.
// Cell-independent maps compose without reordering any cell's arithmetic, so
// the rewrite is bitwise and carries no licence; `BLADE_FUSION=0|off` is the
// A/B escape hatch (read per call, like every other environment gate).
//
// It fires only on the plainest shapes and declines everything else
// unchanged: dense IxKPlain/SymNone records only, kernels with no
// comm/anticomm groups and no omp/cuda/mpi strategy, no Reynolds, rank-0
// scalar kernels, pure bodies, co-iterations only (an outer product would
// change meaning if its operand list grew -- refused by construction via
// IsCoIteration). Nested computes reached through a LET binding (the
// materialized-operand shape `lowerArrayBinOpsModule` produces for pack
// elements) are deliberately out of scope for v1: fusing those needs a
// use-count census; the direct nest is where the user-facing chains live.
let private fusionEnabled () =
    match System.Environment.GetEnvironmentVariable "BLADE_FUSION" with
    | null -> true
    | v ->
        match v.Trim().ToLowerInvariant() with
        | "0" | "off" | "false" -> false
        | _ -> true

let fuseElementwiseChainsModule (modul: IRModule) (builder: IRBuilder) : IRModule =
    if not (fusionEnabled ()) then modul else
    let callables = System.Collections.Generic.Dictionary<IRId, IRCallable>()
    for f in modul.Functions do callables.[f.Id] <- f
    let newLambdas = System.Collections.Generic.List<IRCallable>()

    // Fan-in cap: register pressure grows with fused operand count; beyond
    // this the remaining operands simply stay unfused (still correct).
    let maxFusedOperands = 8
    // Each substitution copies the inner body once per occurrence of the
    // replaced parameter. Binop-minted kernels reference each parameter
    // exactly once; 2 bounds body growth while covering everything this
    // pass targets.
    let maxParamOccurrences = 2

    let substVar (vid: IRId) (replacement: IRExpr) (body: IRExpr) : IRExpr =
        mapIRExpr (fun e ->
            match e with
            | IRVar (id, _) when id = vid -> replacement
            | _ -> e) body

    let occurrences (vid: IRId) (body: IRExpr) : int =
        let mutable n = 0
        iterIRExpr (fun e ->
            match e with
            | IRVar (id, _) when id = vid -> n <- n + 1
            | _ -> ()) body
        n

    // Purity with module-local resolution. The AsyncLocal CallablesTable is
    // not installed yet at this point in the pipeline (it is built at
    // liftInlineFormsModule entry), so IRPrint.exprAttrs' cross-procedural
    // IRApp arm cannot be used here. Anything unresolvable declines.
    let rec pureBody (visited: Set<IRId>) (e: IRExpr) : bool =
        let mutable ok = true
        iterIRExpr (fun n ->
            if ok then
                match n with
                | IRDisplayEmit _ -> ok <- false
                | IRAssign _ -> ok <- false
                | IRApp (IRVar (fid, _), _, _) ->
                    if not (Set.contains fid visited) then
                        match callables.TryGetValue fid with
                        | true, callee ->
                            if callee.IsStatic then ok <- false
                            // The typed summary (Blade.Effects, computed at
                            // checkFunctionDecl with callees resolved) is the
                            // shared legality fact: REPEATABLE means no
                            // assignment, no display, no external read, no
                            // unknown call anywhere below -- everything this
                            // walk would check, plus callees the module-local
                            // table cannot see. `unknown` (lambdas, clones of
                            // unmarked callables) falls through to the walk.
                            elif Blade.Effects.isRepeatable callee.Effects then ()
                            elif not (pureBody (Set.add fid visited) callee.Body) then
                                ok <- false
                        | _ -> ok <- false
                | IRApp _ -> ok <- false
                | _ -> ()) e
        ok

    let plainIx (ix: IRIndexType) =
        ix.IxKind = IxKPlain && ix.Symmetry = SymNone
    let plainArrayTypes (ats: IRArrayType list) =
        ats |> List.forall (fun at -> at.IndexTypes |> List.forall plainIx)

    // A combinator this pass may touch (as host or inner): a plain dense
    // elementwise map/co-iteration with a scalar kernel and none of the
    // structure-exploiting metadata populated.
    let plainInfo (info: ApplyInfo) =
        not info.HasReynolds
        && info.SpeedupFactor = 1L && info.ReynoldsSpeedup = 1L
        && info.KernelTDims.IsEmpty && info.KernelOutputRank = 0
        && info.KernelInputRanks |> List.forall ((=) 0)
        && info.SymcomStates |> List.forall (fun s -> s = SCNeither)
        && info.TriangularLevels |> List.forall not
        && plainArrayTypes info.ArrayTypes
        && (match info.Loop with IRMethodFor _ | IRObjectFor _ -> true | _ -> false)
        // Arity >= 2 must be a genuine co-iteration: splicing operands into
        // an OUTER PRODUCT would change its meaning, not its cost.
        && (info.Arrays.Length = 1
            || (info.IsCoIteration && not info.SharedIndexTypes.IsEmpty))
        // Defensive: every per-array list rides in lockstep with Arrays.
        && info.Identities.Length = info.Arrays.Length
        && info.ArrayTypes.Length = info.Arrays.Length
        && info.SDimsPerArray.Length = info.Arrays.Length
        && info.SymcomStates.Length = info.Arrays.Length
        && info.TriangularLevels.Length = info.Arrays.Length
        && info.KernelInputRanks.Length = info.Arrays.Length

    let plainKernel (k: IRCallable) (arity: int) =
        not k.IsCommutative && k.CommGroups.IsEmpty && k.AntisymGroups.IsEmpty
        && k.Parallelism.IsEmpty && not k.IsOmpParallel && not k.IsCudaKernel
        && not k.IsMpiParallel && not k.IsArityPoly && not k.IsStatic
        && k.Params.Length = arity

    let kernelOf (info: ApplyInfo) : IRCallable option =
        match info.Kernel with
        | IRVar (kid, _) ->
            match callables.TryGetValue kid with
            | true, k -> Some k
            | _ -> None
        | _ -> None

    // Clone an inner kernel's parameters with fresh ids (the same callable
    // can be spliced at more than one site; ids must stay unique), renaming
    // for hygiene, and rewrite its body onto the fresh ids.
    let freshen (ik: IRCallable) : IRParam list * IRExpr =
        let mapping = ik.Params |> List.map (fun p -> p.VarId, builder.FreshId())
        let mapped = Map.ofList mapping
        let params' =
            ik.Params |> List.map (fun p ->
                let nid = mapped.[p.VarId]
                { p with VarId = nid; Name = $"__fz{nid}" })
        let body' =
            mapIRExpr (fun e ->
                match e with
                | IRVar (id, t) ->
                    (match Map.tryFind id mapped with
                     | Some nid -> IRVar (nid, t)
                     | None -> e)
                | _ -> e) ik.Body
        params', body'

    // `Error why` is the DECLINE reason, recorded by `rewrite` when the host
    // had a fusable-looking operand (a `compute`d inner map) -- the shape a
    // user would expect to fuse. A host with no such operand is not a
    // decision, just a plain map, and stays silent.
    let tryFuse (host: ApplyInfo) : Result<ApplyInfo, string> =
        if not (plainInfo host) then Error "the host is not a plain dense map or co-iteration (structured storage, symmetry, or a non-scalar kernel)" else
        match kernelOf host with
        | None -> Error "the host kernel is not a resolvable callable"
        | Some hk when not (plainKernel hk host.Arrays.Length) ->
            Error "the host kernel is not a plain scalar kernel (declared symmetry, parallel strategy, arity polymorphism, or a parameter-count mismatch)"
        | Some hk when not (pureBody Set.empty hk.Body) ->
            Error "the host kernel body is not repeatable (it assigns, emits output, or calls something whose effects are unknown)"
        | Some hk ->
        let mutable declineWhy = "no operand is a `compute`d inner map"
        let innerEligible (inner: ApplyInfo) : IRCallable option =
            if not (plainInfo inner) then
                declineWhy <- "an inner map is not a plain dense map or co-iteration"
                None
            else
            match kernelOf inner with
            | Some ik when plainKernel ik inner.Arrays.Length
                           && pureBody Set.empty ik.Body -> Some ik
            | Some _ ->
                declineWhy <- "an inner kernel is not a plain repeatable scalar kernel"
                None
            | None ->
                declineWhy <- "an inner kernel is not a resolvable callable"
                None
        let mutable total = host.Arrays.Length
        let mutable fusedAny = false
        let mutable body = hk.Body
        let mutable sharedFromInner : IRIndexType list = []
        let capts = System.Collections.Generic.List<CaptureInfo>()
        // Per-slot splice, left to right. Each per-array metadata list is
        // rebuilt in lockstep with Arrays.
        let spliced =
            List.mapi (fun i (a: IRExpr) ->
                let keep () =
                    ([a], [host.Identities.[i]], [host.ArrayTypes.[i]],
                     [host.SDimsPerArray.[i]], [host.SymcomStates.[i]],
                     [host.TriangularLevels.[i]], [host.KernelInputRanks.[i]],
                     [hk.Params.[i]])
                match a with
                | IRCompute (IRApplyCombinator inner) when
                        not (total + inner.Arrays.Length - 1 <= maxFusedOperands
                             && occurrences hk.Params.[i].VarId hk.Body <= maxParamOccurrences) ->
                    declineWhy <- $"the fan-in cap ({maxFusedOperands} operands) or the parameter-occurrence cap ({maxParamOccurrences}) would be exceeded"
                    keep ()
                | IRCompute (IRApplyCombinator inner) ->
                    (match innerEligible inner with
                     | Some ik ->
                         let ps', innerBody = freshen ik
                         body <- substVar hk.Params.[i].VarId innerBody body
                         total <- total + inner.Arrays.Length - 1
                         fusedAny <- true
                         if sharedFromInner.IsEmpty then
                             sharedFromInner <- inner.SharedIndexTypes
                         capts.AddRange ik.Captures
                         (inner.Arrays, inner.Identities, inner.ArrayTypes,
                          inner.SDimsPerArray, inner.SymcomStates,
                          inner.TriangularLevels, inner.KernelInputRanks, ps')
                     | None -> keep ())
                | _ -> keep ()) host.Arrays
        if not fusedAny then Error declineWhy else
        let newArrays     = spliced |> List.collect (fun (x, _, _, _, _, _, _, _) -> x)
        let newIdentities = spliced |> List.collect (fun (_, x, _, _, _, _, _, _) -> x)
        let newArrayTypes = spliced |> List.collect (fun (_, _, x, _, _, _, _, _) -> x)
        let newSDims      = spliced |> List.collect (fun (_, _, _, x, _, _, _, _) -> x)
        let newSymcom     = spliced |> List.collect (fun (_, _, _, _, x, _, _, _) -> x)
        let newTri        = spliced |> List.collect (fun (_, _, _, _, _, x, _, _) -> x)
        let newKIR        = spliced |> List.collect (fun (_, _, _, _, _, _, x, _) -> x)
        let newParams =
            spliced |> List.collect (fun (_, _, _, _, _, _, _, x) -> x)
            |> List.mapi (fun idx p -> { p with Index = idx })
        // Shared iteration records for the fused node: the host's when it was
        // already a co-iteration; otherwise inherited from the first fused
        // inner (a single-array map fusing a zip becomes that zip's
        // co-iteration). An arity > 1 result with no shared records would
        // read as an outer product downstream -- decline rather than emit it.
        let sharedIdx =
            if not host.SharedIndexTypes.IsEmpty then host.SharedIndexTypes
            elif newArrays.Length > 1 then sharedFromInner
            else []
        if newArrays.Length > 1 && sharedIdx.IsEmpty then
            Error "the fused node would read as an outer product (no shared index records to inherit)" else
        let captures =
            (hk.Captures @ List.ofSeq capts)
            |> List.fold (fun (seen, acc) (c: CaptureInfo) ->
                if Set.contains c.Id seen then (seen, acc)
                else (Set.add c.Id seen, c :: acc)) (Set.empty, [])
            |> snd |> List.rev
        let lam =
            mkLambdaCallable builder newParams body hk.RetType captures
                             false [] [] false false 256 false
        newLambdas.Add lam
        callables.[lam.Id] <- lam
        let funcTy =
            IRTArrow (newParams |> List.map (fun p -> SVal p.Type), hk.RetType, None)
        let kernelVar = IRVar (lam.Id, funcTy)
        let totalS = List.sum newSDims
        let newLoop =
            match host.Loop with
            | IRMethodFor mf ->
                IRMethodFor
                    { mf with
                        Arrays = newArrays; Identities = newIdentities
                        ArrayTypes = newArrayTypes; SDimsPerArray = newSDims
                        TotalSDims = totalS; SharedIndexTypes = sharedIdx }
            | IRObjectFor _ ->
                IRObjectFor
                    { Kernel = kernelVar; CommGroups = []
                      InputRanks = newKIR; OutputRank = 0 }
            | other -> other  // unreachable: plainInfo admits only the two above
        Ok { host with
               Loop = newLoop; Kernel = kernelVar
               Arrays = newArrays; Identities = newIdentities
               ArrayTypes = newArrayTypes; SharedIndexTypes = sharedIdx
               SymcomStates = newSymcom; TriangularLevels = newTri
               SDimsPerArray = newSDims; KernelInputRanks = newKIR
               IsCoIteration = newArrays.Length > 1 }

    // Decision record (Blade.Effects.Decisions; a no-op unless `blade plan`
    // or a test installed a collector). One record per host KERNEL: the
    // operand tree is duplicated between info.Arrays and the Loop
    // provenance, so the bottom-up rewrite meets some hosts twice.
    let decided = System.Collections.Generic.HashSet<IRId>()
    let kernelName (info: ApplyInfo) =
        match info.Kernel with
        | IRVar (kid, _) ->
            (match callables.TryGetValue kid with
             | true, k -> k.Name
             | _ -> $"kernel#{kid}")
        | _ -> "<inline kernel>"
    let kernelId (info: ApplyInfo) =
        match info.Kernel with IRVar (kid, _) -> Some kid | _ -> None
    let hasInnerCompute (info: ApplyInfo) =
        info.Arrays |> List.exists (function IRCompute (IRApplyCombinator _) -> true | _ -> false)
    let record (info: ApplyInfo) (outcome: Blade.Effects.DecisionOutcome) (evidence: string list) =
        let fresh =
            match kernelId info with
            | Some kid -> decided.Add kid
            | None -> true
        if fresh then
            Blade.Effects.Decisions.record
                { Blade.Effects.Rule = "elementwise-fusion"; Version = 1
                  Span = Blade.Ast.noSpan; Subject = kernelName info
                  Outcome = outcome; Evidence = evidence }

    let rewrite (e: IRExpr) : IRExpr =
        match e with
        | IRApplyCombinator info ->
            (match tryFuse info with
             | Ok fused ->
                 let inner =
                     info.Arrays |> List.filter (function IRCompute (IRApplyCombinator _) -> true | _ -> false)
                                 |> List.length
                 record info Blade.Effects.Applied
                     [ $"{inner} inner map(s) spliced into the host kernel; {info.Arrays.Length} -> {fused.Arrays.Length} operand(s), one nest"
                       "host and inner kernels are plain repeatable scalar kernels"
                       (if fused.IsCoIteration then "shared index records inherited" else "single-operand map") ]
                 IRApplyCombinator fused
             | Error why ->
                 if hasInnerCompute info then record info (Blade.Effects.Declined why) []
                 e)
        | _ -> e
    let rewriteExpr expr = mapIRExpr rewrite expr
    let newFunctions =
        modul.Functions |> List.map (fun f -> { f with Body = rewriteExpr f.Body })
    let newBindings =
        modul.Bindings |> List.map (fun b -> { b with Value = rewriteExpr b.Value })
    // Sweep this pass's OWN dead mints. The operand tree is duplicated
    // between info.Arrays and the Loop-provenance MethodForInfo.Arrays, so
    // the bottom-up traversal fuses both copies; the Loop copy's composed
    // lambda is then overwritten by the host rewrite and never referenced.
    // Intermediate compositions of a longer chain go dead the same way.
    // Only lambdas minted HERE are candidates -- pre-existing kernels that
    // fusion orphaned stay (exports and DerivedFuncOrigins may name them).
    let survivingLambdas =
        if newLambdas.Count = 0 then []
        else
            let referenced = System.Collections.Generic.HashSet<IRId>()
            let scan (b: IRExpr) =
                iterIRExpr (fun e ->
                    match e with
                    | IRVar (id, _) -> referenced.Add id |> ignore
                    | _ -> ()) b
            newFunctions |> List.iter (fun f -> scan f.Body)
            newBindings |> List.iter (fun b -> scan b.Value)
            newLambdas |> Seq.filter (fun l -> referenced.Contains l.Id) |> List.ofSeq
    { modul with
        Functions = newFunctions @ survivingLambdas
        Bindings = newBindings }

// Constant-scrutinee match folding (docs/plans/plan-match-statements.md).
//
// `match <int literal> with ...` selects its arm at compile time. Two
// producers reach this shape: `arity(A)` inside a specializing Poly kernel
// (rewritten to a literal by specializeFunction, which folds DURING
// specialization so the recursion cascade terminates at the base arm), and
// `rank(x)` in a concrete context (lowered to IRLit straight from the typed
// expression's type). Before this pass the fold existed only inside the
// arity specializer, so `match rank(x) with | 0 -> .. | _ -> ..` on a
// concrete array survived to codegen as `(0L == 0L ? .. : ..)` -- a runtime
// ternary over a decided question, and (worse) a ternary whose dead arm
// could carry a type the live arm doesn't, handing g++ an ill-typed
// expression the fold would have removed.
//
// Arm selection is ORDER-SOUND, which the specializer's original local copy
// was not: walk arms in source order; a literal arm that cannot match the
// scrutinee is skipped; the first arm whose pattern matches is chosen ONLY
// if it is guard-free -- a GUARDED arm whose pattern matches bails the
// whole fold (the guard is a runtime question, and folding past it would
// answer it statically as `false`). Non-int patterns against an int
// scrutinee also bail rather than guess.
let foldConstIntMatch (e: IRExpr) : IRExpr =
    match e with
    // A SYMBOLIC rank surviving from a generic body (Lowering defers
    // `rank(x)` when the operand's type is still an inference var there)
    // resolves here, after the monomorphizers have made the clone's types
    // concrete. Bottom-up traversal then hands the enclosing
    // `IRMatch(IRLit ..)` its folded scrutinee in the same pass -- this arm
    // is what makes `match rank(x)` dispatch per HM specialization instead
    // of sharing one baked answer. Rank stays symbolic (the emitters'
    // existing IRRank arms) only when the type is genuinely unknown.
    | IRRank operand ->
        let rec rankOfType (t: IRType) : int option =
            match t with
            | ArrayElem at -> Some (at.IndexTypes |> List.sumBy _.Rank)
            | IRTInfer _ -> None
            | IRTUnitAnnotated (inner, _) -> rankOfType inner
            | _ -> Some 0
        (match exprTypeIfKnown operand |> Option.bind rankOfType with
         | Some r -> IRLit (IRLitInt (int64 r))
         | None -> e)
    | IRMatch (IRLit (IRLitInt n), cases) ->
        let rec pick (cs: IRMatchCase list) =
            match cs with
            | [] -> None
            | c :: rest ->
                let patMatches =
                    match c.Pattern with
                    | IRPatLit (IRLitInt m) -> Some (m = n)
                    | IRPatWild | IRPatVar _ -> Some true
                    | _ -> None
                match patMatches with
                | None -> None                    // foreign pattern shape: bail
                | Some false -> pick rest
                | Some true -> if c.Guard.IsSome then None else Some c
        match pick cases with
        | Some c ->
            (match c.Pattern with
             | IRPatVar vid -> IRLet (vid, IRLit (IRLitInt n), c.Body)
             | _ -> c.Body)
        | None -> e
    | _ -> e

/// Bottom-up over one expression: inner constant matches fold first, so a
/// chosen arm that itself contains one arrives here already reduced.
let foldConstMatchesExpr (expr: IRExpr) : IRExpr =
    mapIRExpr foldConstIntMatch expr

/// The pipeline pass: every function body and binding value. Runs after the
/// monomorphizers (so specialized bodies' literals are in place) and before
/// codegen/interp split -- one fold serves both back ends.
let foldConstMatchesModule (modul: IRModule) : IRModule =
    { modul with
        Functions = modul.Functions |> List.map (fun f -> { f with Body = foldConstMatchesExpr f.Body })
        Bindings = modul.Bindings |> List.map (fun b -> { b with Value = foldConstMatchesExpr b.Value }) }

// Arity Monomorphization

/// Locate every Poly param's index in a function, in declaration order.
/// Each Poly pack is independent -- different slots may have different
/// concrete arities at any given call site. Returns [] for non-Poly
/// functions.
let findPolyParamIndices (func: IRFuncDef) : int list =
    func.Params
    |> List.mapi (fun i p ->
        match p.Type with
        | IRTPoly _ -> Some i
        | _ -> None)
    |> List.choose id

/// Compute the concrete pack arity for a call to an arity-polymorphic
/// function, returning one arity per Poly slot (in declaration order). Each
/// pack stands independently -- `pairSum((1.0, 2.0), (3.0, 4.0, 5.0))` is a
/// valid call with arities [2; 3]. Three call shapes are recognized:
/// (a) Variadic -- single Poly param, no free params: every positional arg
///     is a pack element, returns [args.Length].
/// (b) Single-Poly tuple-as-pack -- single Poly param plus free params: the
///     pack is a tuple at the Poly slot, returns [tuple.Length].
/// (c) Multi-Poly tuple-as-pack -- every Poly slot gets its own tuple,
///     returns [size_slot_0; size_slot_1; ...].
/// Returns None for unsupported shapes (mismatched arg count, non-tuple at
/// a required Poly slot).
let computePolyArity (func: IRFuncDef) (args: IRExpr list) : int list option =
    let polyIndices = findPolyParamIndices func
    // A call passing a symbolic pack tail (`f(tail)` inside an un-specialized
    // recursive body, `tail = IRPolyTail(A, k)`) is NOT a concrete call site:
    // its true arity is only known once the ENCLOSING function is
    // specialized, at which point specializeFunction spreads the tail into
    // `f(A_k, .., A_{n-1})`, the real call site the worklist then picks up.
    // Treating the symbolic call as concrete (arity = arg-count = 1) mints a
    // bogus specialization -- for a `| 2` base it cascades into an invalid
    // arity-0 spec. Reject it here.
    if args |> List.exists (function IRPolyTail _ -> true | _ -> false) then None
    else
    match polyIndices with
    | [] -> None
    | [pidx] when func.Params.Length = 1 ->
        // Variadic -- args are the pack elements directly
        Some [args.Length]
    | _ ->
        // Tuple-as-pack at every Poly slot. args.Length must equal the
        // formal arity (no variadic spreading when free params or multiple
        // packs are involved).
        if args.Length <> func.Params.Length then None
        else
            let perSlot =
                polyIndices |> List.map (fun pidx ->
                    match List.item pidx args with
                    | IRTuple elems -> Some elems.Length
                    | _ -> None)
            if perSlot |> List.forall Option.isSome then
                Some (perSlot |> List.map Option.get)
            else None

/// Rewrite a call's arg list to match the specialized function's expanded
/// param list. Variadic single-Poly leaves args flat; tuple-as-pack (single
/// or multi) expands each tuple at its Poly slot. Each pack expands
/// independently -- different slots can yield different numbers of elements.
let flattenAtPolyPosition (func: IRFuncDef) (args: IRExpr list) : IRExpr list =
    let polyIndices = findPolyParamIndices func
    match polyIndices with
    | [] -> args
    | [_] when func.Params.Length = 1 -> args  // variadic -- already flat
    | _ ->
        if args.Length <> func.Params.Length then args
        else
            let polySet = Set.ofList polyIndices
            args |> List.mapi (fun i a ->
                if Set.contains i polySet then
                    match a with
                    | IRTuple elems -> elems
                    | _ -> [a]  // shouldn't happen post-computePolyArity
                else
                    [a])
            |> List.concat

/// Collect all call sites to arity-polymorphic functions.
/// Returns list of (funcId, arities) pairs where arities is the per-slot
/// arity list. Unsupported call shapes are silently skipped -- they surface
/// as type errors downstream rather than producing wrong-arity specs.
let collectPolyCallSites (polyFuncMap: Map<IRId, IRFuncDef>) (expr: IRExpr) : (IRId * int list) list =
    let results = System.Collections.Generic.List<_>()
    let walk e =
        match e with
        | IRApp (IRVar (funcId, _), args, _) when Map.containsKey funcId polyFuncMap ->
            let func = polyFuncMap.[funcId]
            match computePolyArity func args with
            | Some arities -> results.Add((funcId, arities))
            | None -> ()
        | _ -> ()
    iterIRExpr walk expr
    results |> Seq.toList

/// Create a monomorphized copy of a poly function for a list of slot arities.
/// `arities` carries one arity per Poly slot, in declaration order -- packs
/// are independent, so different slots may have different sizes.
let specializeFunction (func: IRFuncDef) (arities: int list) (funcMap: Map<IRId, IRFuncDef>) (builder: IRBuilder) : IRFuncDef =
    let polyIndices = findPolyParamIndices func
    if List.isEmpty polyIndices then func  // Not actually poly -- shouldn't happen
    elif List.length arities <> List.length polyIndices then func  // arity-count mismatch
    else
        // Per-slot info: original param, base type, the N_slot new params
        // (where N_slot is that slot's arity).
        let slotInfo =
            List.zip polyIndices arities
            |> List.map (fun (pidx, slotArity) ->
                let polyParam = func.Params.[pidx]
                let baseType =
                    match polyParam.Type with
                    | IRTPoly (bt, _) -> bt
                    | _ -> IRTScalar ETFloat64
                let newParams =
                    List.init slotArity (fun i ->
                        { Name = $"{polyParam.Name}_{i}"
                          Type = baseType
                          Index = 0  // recomputed below
                          VarId = builder.FreshId() } : IRParam)
                (pidx, polyParam, baseType, newParams))

        // Expand param list: walk original, replace each Poly slot with its
        // (slot-specific number of) new params. Reindex flat.
        let polySet = polyIndices |> Set.ofList
        let slotByIdx = slotInfo |> List.map (fun (i, _, _, np) -> (i, np)) |> Map.ofList
        let expandedParams =
            func.Params
            |> List.mapi (fun i p ->
                if Set.contains i polySet then slotByIdx.[i]
                else [p])
            |> List.concat
            |> List.mapi (fun newIdx p -> { p with Index = newIdx })

        // Per-slot data indexed by slot, used during body rewrite.
        let newParamsBySlot =
            slotInfo |> List.map (fun (_, _, _, np) -> np) |> List.toArray
        let baseTypeBySlot =
            slotInfo |> List.map (fun (_, _, bt, _) -> bt) |> List.toArray
        let aritiesArr = arities |> List.toArray

        // Alias map: VarId -> (slotIdx, offset), for each pack-slot param plus
        // every let-alias and cons-destructuring tail view built from it. The
        // offset is leading elements dropped: 0 for the pack itself; `off +
        // drop` for `let t = tail-of(view)` (from `let head :: tail = view`).
        // A read `view[k]` resolves to expanded param `off + k`; a call
        // passing `view` spreads params `off ..`. Top-down walk so a later
        // alias sees an earlier one (lets are ordered).
        let aliasInfo : Map<IRId, int * int> =
            let mutable info =
                slotInfo
                |> List.mapi (fun slotIdx (_, polyParam, _, _) -> (polyParam.VarId, (slotIdx, 0)))
                |> Map.ofList
            let rec walk expr =
                match expr with
                | IRLet (id, value, body) ->
                    (match value with
                     | IRVar (srcId, _) when Map.containsKey srcId info ->
                         info <- Map.add id info.[srcId] info
                     | IRPolyTail (IRVar (srcId, _), drop) when Map.containsKey srcId info ->
                         let (slot, off) = info.[srcId]
                         info <- Map.add id (slot, off + drop) info
                     | _ -> ())
                    walk value; walk body
                | ExprShape (children, _) -> children |> List.iter walk
            walk func.Body
            info
        // Slot-only view, for the pack-former unroller's membership test.
        let aliasToSlot = aliasInfo |> Map.map (fun _ (slot, _) -> slot)

        // Map param name -> slot index, for the IRArity intrinsic. `arity(xs)`
        // resolves to slot 0's arity; `arity(ys)` to slot 1's, etc.
        let paramNameToSlot =
            slotInfo
            |> List.mapi (fun slotIdx (_, p, _, _) -> (p.Name, slotIdx))
            |> Map.ofList

        // Draw expanded param `idx` of `slot` as an IRVar (used for reads and for
        // spreading a pack argument at a call site).
        let slotParamVar slot idx =
            IRVar (newParamsBySlot.[slot].[idx].VarId, baseTypeBySlot.[slot])

        // Rewrite body: resolve IRPolyIndex reads to the expanded param,
        // IRArity to a literal, and spread any pack-view argument at a call
        // site into its trailing params (`f(tail)` becomes
        // `f(A_off, .., A_{n-1})`, a normal call the driver re-collects --
        // how recursion over a shrinking pack terminates at the base case).
        let rewrite e =
            match e with
            | IRPolyIndex (IRVar (id, _), IRLit (IRLitInt k)) when Map.containsKey id aliasInfo ->
                let (slotIdx, off) = aliasInfo.[id]
                let idx = off + int k
                let slotArity = aritiesArr.[slotIdx]
                if idx >= 0 && idx < slotArity then slotParamVar slotIdx idx
                else e
            | IRPolyIndex (IRVar (id, _), _) when Map.containsKey id aliasInfo ->
                e  // Dynamic index -- can't monomorphize, leave as-is
            | IRArity (_, name) when Map.containsKey name paramNameToSlot ->
                let slotIdx = paramNameToSlot.[name]
                IRLit (IRLitInt (int64 aritiesArr.[slotIdx]))
            | IRApp (callee, args, rty)
                when args |> List.exists (fun a ->
                        match a with IRVar (id, _) -> Map.containsKey id aliasInfo | _ -> false) ->
                // How a pack view spreads depends on the CALLEE's call shape
                // (mirroring computePolyArity): a variadic callee (single Poly
                // param, no free params) takes the elements flat; a
                // tuple-as-pack callee (multi-Poly, or Poly plus free params)
                // takes ONE TUPLE per Poly slot, which computePolyArity
                // re-reads as a concrete per-slot arity. Spreading flat into a
                // tuple-as-pack callee makes the call unrecognizable -- no
                // specialization is minted and the call keeps referencing the
                // original poly function after it is removed (dangling VarId).
                let calleeIsVariadic =
                    match callee with
                    | IRVar (fid, _) ->
                        (match Map.tryFind fid funcMap with
                         | Some f when f.IsArityPoly ->
                             (match findPolyParamIndices f with
                              | [_] when f.Params.Length = 1 -> true
                              | _ -> false)
                         | _ -> true)  // non-poly callee: keep the flat spread
                    | _ -> true
                let expandedArgs =
                    args |> List.collect (fun a ->
                        match a with
                        | IRVar (id, _) when Map.containsKey id aliasInfo ->
                            let (slot, off) = aliasInfo.[id]
                            let elems =
                                [ for j in off .. aritiesArr.[slot] - 1 -> slotParamVar slot j ]
                            if calleeIsVariadic then elems else [IRTuple elems]
                        | _ -> [a])
                IRApp (callee, expandedArgs, rty)
            | _ -> e
        let newBody = mapIRExpr rewrite func.Body

        // Static match reduction: after `rewrite` turns `arity(A)` into a
        // literal, `match arity(A) with | k -> .. | _ -> ..` picks its one
        // live arm at compile time. Essential for recursion termination: the
        // base arm must be selected (and the recursive arm, which
        // destructures the pack and calls f(tail), discarded) at the base
        // arity, or specialization would shrink past 0 and destructure an
        // empty pack. Shares `foldConstIntMatch` with the pipeline-level
        // pass, so the arm-selection semantics cannot drift between the two.
        let newBody = foldConstMatchesExpr newBody

        // Drop the now-dead pack-alias let bindings (`let _ = A`, `let tail = A[1..]`).
        // Every use of them was rewritten to expanded params above; the bindings
        // themselves reference the pre-expansion pack (a dangling VarId) or an
        // IRPolyTail marker (no codegen), so they must not survive.
        let rec dropAliasLets expr =
            match expr with
            | IRLet (id, _, rest) when Map.containsKey id aliasInfo -> dropAliasLets rest
            | ExprShape (children, rebuild) -> rebuild (children |> List.map dropAliasLets)
        let newBody = dropAliasLets newBody

        // Second pass: unroll IRForRange with literal bounds. This handles
        // `for k in 0..arity(args)` after arity is resolved. A body carrying
        // IRBreakIf (a `while`-guarded rec array inside an arity-poly
        // function) must NOT unroll: the unrolled copies have no loop to
        // break out of, so the early exit would silently vanish and every
        // iteration would run.
        let rec containsBreakIf e =
            match e with
            | IRBreakIf _ -> true
            | ExprShape (children, _) -> children |> List.exists containsBreakIf
        let rec unrollForRanges expr =
            match expr with
            | IRLet (id, IRForRange (vid, IRLit (IRLitInt lo), IRLit (IRLitInt hi), body), rest)
                    when not (containsBreakIf body) ->
                let restUnrolled = unrollForRanges rest
                let indices = [ int lo .. int hi - 1 ] |> List.rev
                indices |> List.fold (fun acc k ->
                    let substBody =
                        mapIRExpr (fun e ->
                            match e with
                            | IRVar (varId, _) when varId = vid -> IRLit (IRLitInt (int64 k))
                            | _ -> e) body
                    let substBody2 = mapIRExpr rewrite substBody
                    let dummyId = builder.FreshId()
                    IRLet (dummyId, unrollForRanges substBody2, acc)
                ) restUnrolled
            | IRLet (id, v, b) -> IRLet (id, unrollForRanges v, unrollForRanges b)
            | _ -> expr
        let newBody = unrollForRanges newBody

        // Pack-former unrolling pass (poly-specialization only).
        //
        // Recognizes a let-bound former whose iteration source is a single
        // virtual range and whose kernel (a lifted lambda) reads THIS
        // function's pack via a dynamic `IRPolyIndex` (`args[k]`). Two
        // things block ordinary codegen once arity is known: (1) the kernel
        // keeps the dynamic subscript `args[k]`, invalid once the pack is
        // split into scalar params; (2) the range extent is an opaque
        // `IRParam` placeholder (not `IRArity`), so codegen emits an
        // undeclared `__range0.extents[0]`. Since the pack size is exactly
        // the slot arity, unroll the former into an n-element ARRAY LITERAL:
        // element k = the kernel body with its ordinal param substituted to
        // `Lit k`, re-run through `rewrite` so `IRPolyIndex(pack, Lit k)`
        // becomes the k-th monomorphized param.
        //
        // Tightly scoped: fires ONLY when the kernel lambda body references a
        // pack slot of this function (`aliasToSlot` membership test); every
        // other former is left byte-for-byte untouched.
        let unrollPackFormers expr =
            let tryUnroll (info: ApplyInfo) : IRExpr option =
                // The virtual source must be a single range with no real data
                // arrays threaded in (a pure ordinal iteration).
                let pureRange =
                    match info.Arrays with
                    | [IRRange _] -> true
                    | _ -> false
                match info.Kernel with
                | IRVar (lamId, _) when pureRange && Map.containsKey lamId funcMap ->
                    let lam = funcMap.[lamId]
                    match lam.Params with
                    | [ordinalParam] ->
                        // Which pack slot (if any) does the lambda body read?
                        let mutable packSlot = None
                        iterIRExpr (fun e ->
                            match e with
                            | IRPolyIndex (IRVar (pid, _), _) when Map.containsKey pid aliasToSlot ->
                                packSlot <- Some aliasToSlot.[pid]
                            | _ -> ()) lam.Body
                        match packSlot with
                        | Some slotIdx ->
                            let n = aritiesArr.[slotIdx]
                            // Element type + index-type shape from the former's
                            // deduced OutputType arrow; the extent is replaced
                            // with the (now literal) pack size.
                            let (idxRec, elemTy) =
                                match info.OutputType with
                                | IRTArrow ([SIdx ix], et, _) -> (ix, et)
                                | IRTArrow ([SIdxVirt ix], et, _) -> (ix, et)
                                | _ ->
                                    ({ Id = builder.FreshId(); Rank = 1
                                       Extent = IRLit (IRLitInt (int64 n))
                                       Symmetry = SymNone; Tag = None
                                       Kind = SDimension; IxKind = IxKPlain
                                       Dependencies = [] }, baseTypeBySlot.[slotIdx])
                            let arrTy : IRArrayTypeG<IRExpr> =
                                { ElemType = elemTy
                                  IndexTypes = [ { idxRec with Extent = IRLit (IRLitInt (int64 n)) } ]
                                  IsVirtual = false
                                  Identity = None }
                            let elems =
                                [ for k in 0 .. n - 1 ->
                                    let substituted =
                                        mapIRExpr (fun e ->
                                            match e with
                                            | IRVar (vid, _) when vid = ordinalParam.VarId ->
                                                IRLit (IRLitInt (int64 k))
                                            | _ -> e) lam.Body
                                    mapIRExpr rewrite substituted ]
                            Some (IRArrayLit (elems, arrTy))
                        | None -> None
                    | _ -> None
                | _ -> None
            mapIRExpr (fun e ->
                match e with
                | IRLet (id, IRCompute (IRApplyCombinator info), rest) ->
                    (match tryUnroll info with
                     | Some arrLit -> IRLet (id, arrLit, rest)
                     | None -> e)
                | IRLet (id, IRApplyCombinator info, rest) ->
                    (match tryUnroll info with
                     | Some arrLit -> IRLet (id, arrLit, rest)
                     | None -> e)
                | _ -> e) expr
        let newBody = unrollPackFormers newBody

        let declaredRetType =
            match func.RetType with
            | IRTPoly (bt, _) -> bt
            | other -> other
        // Arity-dependent return rank: when arity reduction collapses this
        // specialization's body to a SCALAR (an empty-pack base arm, e.g.
        // `| 0 -> zero`), the specialization genuinely returns T^0, not the
        // declared array shape, and the enclosing op broadcasts it. We only
        // ever narrow a declared array to the body's scalar, never widen.
        let rec finalScalarType e =
            match e with
            | IRLet (_, _, body) -> finalScalarType body
            | CarriedType (IRTScalar _ as ty) -> Some ty
            | _ -> None
        let newRetType =
            match finalScalarType newBody with
            | Some sTy -> sTy               // arity-collapsed scalar base (see above)
            | None -> declaredRetType

        // Commutativity group expansion: each original index maps to a
        // (newStart, span) in the expanded list. For a Poly slot, span =
        // that slot's arity; for a free param, span = 1.
        let origToNew =
            let mutable acc = []
            let mutable cur = 0
            for i in 0 .. func.Params.Length - 1 do
                let span =
                    if Set.contains i polySet then
                        // Look up this slot's arity. polyIndices is in order,
                        // so its position in the list is the slot index.
                        let slotIdx = polyIndices |> List.findIndex (fun x -> x = i)
                        aritiesArr.[slotIdx]
                    else 1
                acc <- (i, (cur, span)) :: acc
                cur <- cur + span
            acc |> List.rev |> Map.ofList

        // Specializing arity groups means rewriting group indices to
        // account for expanded parameters: a pack slot of span k becomes the
        // k consecutive expanded positions, everything else maps 1:1.
        let expandGroups (groups: int list list) =
            groups |> List.map (fun group ->
                group |> List.collect (fun idx ->
                    match Map.tryFind idx origToNew with
                    | Some (start, span) ->
                        if span = 1 then [start]
                        else List.init span (fun i -> start + i)
                    | None -> [idx]))
        // Expand whatever groups the source carried, independent of
        // IsCommutative: the declared-anticomm spelling means the flag and a
        // non-empty group list are not always set together -- an antisym
        // kernel has groups but is NOT commutative -- so the expansion keys
        // off the lists themselves and the flag is carried through untouched.
        let newIsComm = func.IsCommutative
        let newCommGroups = expandGroups func.CommGroups
        let newAntisymGroups = expandGroups func.AntisymGroups

        // Mangled name encodes every slot's arity, so different shapes get
        // distinct specializations. `pairSum_arity_2_3` for arities [2; 3].
        let arityTag = arities |> List.map string |> String.concat "_"

        { Id = builder.FreshId()
          Name = $"{func.Name}_arity_{arityTag}"
          Params = expandedParams
          RetType = newRetType
          Body = newBody
          IsStatic = func.IsStatic
          IsCommutative = newIsComm
          CommGroups = newCommGroups
          AntisymGroups = newAntisymGroups
          Parallelism = func.Parallelism
          IsOmpParallel = func.IsOmpParallel
          IsCudaKernel = func.IsCudaKernel
          CudaBlockSize = func.CudaBlockSize
          IsMpiParallel = func.IsMpiParallel
          IsArityPoly = false
          ArityParam = None
          // The reproducibility demand survives specialization: the clone is
          // the same declared function at a concrete arity.
          IsRepro = func.IsRepro
          // So does the effect summary: specialization changes arity, not
          // what the body does.
          Effects = func.Effects
          // Specialized clones inherit the original's captures verbatim;
          // arity specialization doesn't introduce new free vars.
          Captures = func.Captures
          // Sign parities are per-PARAMETER: a pack slot expanding to k
          // positions replicates its origin's parity across them. Vacuous
          // today (only apply-seam kernel lambdas carry a summary, and
          // those are fixed-arity) but kept honest for a future producer.
          SignParities =
            (if List.isEmpty func.SignParities then []
             else
                func.SignParities
                |> List.mapi (fun idx p ->
                    match Map.tryFind idx origToNew with
                    | Some (_, span) -> List.replicate span p
                    | None -> [p])
                |> List.concat) }

// Inline-Form Lifting Pass
//
// Some IR forms (IRMask, IRSort, IRIntersect, IRUnion, IRGroupBy, IRGroupKeys)
// require a *named binding*: codegen for the form emits multi-statement
// setup (size computation, allocation, fill loop), so inline use as a
// sub-expression (`reduce(mask(g, pred), op)`) would need every consumer
// (IRExtent, IRIndex, IRApp, ...) to inline-emit that setup itself.
//
// Rather than bespoke inline-materialization per consumer, this pass
// normalizes the IR: any inline-form occurrence in a non-let-RHS position
// is rewritten to a fresh `IRLet(tmp, form, parent(IRVar(tmp, ty)))`, so
// codegen only ever sees the canonical `let tmp = mask(...)` pattern.
//
// Blessed positions (no rewrite): the value side of an IRLet, and the
// Arrays list of IRMethodFor/IRApplyCombinator (auto-materialized at
// codegen). Everywhere else the rewrite fires.


/// Monomorphize all arity-polymorphic functions in an IR module.
/// Collects call sites, generates specialized versions, rewrites calls.
let monomorphizeModule (modul: IRModule) (builder: IRBuilder) : IRModule =
    // 1. Identify poly functions
    let polyFuncs =
        modul.Functions |> List.filter _.IsArityPoly
    if polyFuncs.IsEmpty then modul  // Nothing to do
    else
    let polyFuncIds = polyFuncs |> List.map _.Id |> Set.ofList
    let polyFuncMap = polyFuncs |> List.map (fun f -> (f.Id, f)) |> Map.ofList

    // 2. Collect call sites from the original module. Seed ONLY from concrete
    //    entry points -- NON-poly functions and top-level bindings. A poly
    //    function's own body reaches other poly functions (and itself) with the
    //    still-symbolic pack/tail as the argument (`f(rest)`, `comoment_prod(a)`),
    //    which computePolyArity would mis-read as an arity-1 call and mint a
    //    bogus spec. Those calls become CONCRETE -- the tail/pack spread into
    //    real per-element args -- only once the enclosing function is specialized,
    //    at which point the spec-body scan below (step 3) picks up the real
    //    arity. (A `| 2`-base recursion seeded from a poly call site would
    //    otherwise cascade into an invalid arity-0 spec.)
    let callSitesFromFuncs =
        modul.Functions
        |> List.filter (fun f -> not f.IsArityPoly)
        |> List.collect (fun f -> collectPolyCallSites polyFuncMap f.Body)
    let callSitesFromBindings =
        modul.Bindings |> List.collect (fun b -> collectPolyCallSites polyFuncMap b.Value)

    // 3. Generate specialized functions to a fixpoint. Specializing a
    //    function can introduce NEW call sites: a recursion over a shrinking
    //    pack (`f(tail)`) is rewritten into an arity-(n-1) call, so
    //    specializing f_arity_n demands f_arity_(n-1), down to the base arm
    //    `match arity` statically selects. The worklist scans each fresh
    //    spec's body and enqueues what it finds until nothing new appears.
    let funcMap = modul.Functions |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    let mutable specMap : Map<IRId * int list, IRFuncDef> = Map.empty
    let queue = System.Collections.Generic.Queue<IRId * int list>()
    for site in (callSitesFromFuncs @ callSitesFromBindings) |> List.distinct do
        queue.Enqueue site
    let mutable guard = 0
    let MAX_SPECS = 100000  // runaway backstop; real recursion depth = max pack arity
    while queue.Count > 0 && guard < MAX_SPECS do
        guard <- guard + 1
        let (funcId, arity) = queue.Dequeue()
        if not (Map.containsKey (funcId, arity) specMap) then
            let origFunc = polyFuncMap.[funcId]
            let spec = specializeFunction origFunc arity funcMap builder
            specMap <- Map.add (funcId, arity) spec specMap
            for site in collectPolyCallSites polyFuncMap spec.Body do
                if not (Map.containsKey site specMap) then queue.Enqueue site
    let specializations = specMap |> Map.toList

    // 4. Build rewrite function for call sites. The arity comes from
    //    computePolyArity (shape-aware: variadic vs tuple-as-pack), and the
    //    args are flattened at the Poly slot so they line up with the
    //    specialization's expanded param list.
    let rewriteCallSite e =
        match e with
        | IRApp (IRVar (funcId, fty), args, _) when polyFuncIds.Contains funcId ->
            let func = polyFuncMap.[funcId]
            match computePolyArity func args with
            | Some arity ->
                match Map.tryFind (funcId, arity) specMap with
                | Some spec ->
                    let flatArgs = flattenAtPolyPosition func args
                    IRApp (IRVar (spec.Id, fty), flatArgs, spec.RetType)
                | None -> e
            | None -> e
        | _ -> e

    // 5. Rewrite all expressions in module
    let newFunctions =
        modul.Functions
        |> List.filter (fun f -> not f.IsArityPoly)  // Remove original poly funcs
        |> List.map (fun f -> { f with Body = mapIRExpr rewriteCallSite f.Body })
    let newBindings =
        modul.Bindings
        |> List.map (fun b -> { b with Value = mapIRExpr rewriteCallSite b.Value })
    // Spec bodies carry the recursive/other poly calls (as arity-(n-1) variadic
    // applications of the ORIGINAL poly id); rewrite those to the concrete specs
    // too, or the recursion would reference a poly function that no longer exists.
    let specFuncs =
        specializations |> List.map (fun (_, spec) ->
            { spec with Body = mapIRExpr rewriteCallSite spec.Body })

    // Prune lifted kernel lambdas that captured a poly PACK and are now dead.
    // The pack-former unroller inlines such a lambda's body into the
    // specialized caller, leaving the original lambda unreferenced and
    // un-codegen-able (its body still subscripts a Poly-typed pack that no
    // longer exists as a scalar). Drop it once unreferenced. The gate is
    // narrow (synthesized "__lambda_" name, Poly-typed capture, unreferenced)
    // so no live function is ever removed.
    let allFuncs = newFunctions @ specFuncs
    let referencedIds =
        (allFuncs |> List.map _.Body)
        @ (newBindings |> List.map _.Value)
        |> List.map collectVarRefsIR
        |> Set.unionMany
    let capturesPolyPack (f: IRFuncDef) =
        f.Captures |> List.exists (fun c ->
            match c.Type with IRTPoly _ -> true | _ -> false)
    let prunedFuncs =
        allFuncs |> List.filter (fun f ->
            not (f.Name.StartsWith("__lambda_")
                 && capturesPolyPack f
                 && not (Set.contains f.Id referencedIds)))

    { modul with
        Functions = prunedFuncs
        Bindings = newBindings }

// Shape monomorphization (docs/plan-cpp-perf-exploitation.md)
//
// A function declared over a SYMBOLIC extent (`f(A: Array<Float64 like Idx<n>>)`)
// carries `IRParam ("n", 0, IRTNat None)` as its index records' `Extent` -- a
// cosmetic placeholder that nothing downstream turns into a number (unify
// never compares extents; HM substitution can't carry them). So
// `genLoopBoundExpr` falls to `<arr>.extents[d]` in every such function, and
// the flat elementwise mode (which needs a literal extent for its compile-
// time cell count) can never fire there.
//
// This pass closes that gap the way `monomorphizeModule` closes the arity
// gap: collect call sites, key a spec map by (funcId, extent signature), and
// emit one specialized copy per unique signature with the placeholders
// rewritten to `IRLit`. The generic copy stays for calls that don't pin a
// literal.
//
// It does NOT change the runtime ABI: `Array<T,R>` still carries
// `const size_t* extents`, and every `.extents[d]` expression left alone
// still yields the right number (the literal came from the ARGUMENT's own
// type), so the rewrite is confined to `IRIndexTypeG.Extent` fields and
// never touches body expressions.
//
// Explosion control: dedupe by signature, cap at SHAPE_SPEC_CAP copies per
// function, and decline recursive/mutually recursive functions outright --
// bounded by real call-site diversity and capped in the compiler.

/// Most specialized copies any one function may earn, counted across the whole
/// PROGRAM (the pass spans modules). Past this, further call sites keep the
/// generic copy -- declines are counted and surfaced under
/// BLADE_DEBUG_SHAPE_SPEC, never as a user diagnostic (a missed optimization
/// is not a program defect).
///
/// `BLADE_SHAPE_SPEC_CAP` overrides it. Unset (or unparseable) is 4, the
/// measured-safe default the corpus never reaches. There is deliberately NO
/// "unlimited" setting: the cap is the standing termination backstop for the
/// worklist -- a self-recursive or mutually recursive candidate can otherwise
/// keep minting signatures -- so `0`, a negative value and anything above 64
/// all clamp to 64 rather than opening the door to unbounded code growth.
let internal shapeSpecCap () =
    match System.Environment.GetEnvironmentVariable("BLADE_SHAPE_SPEC_CAP") with
    | null | "" -> 4
    | s ->
        match System.Int32.TryParse s with
        | true, v when v > 0 -> min v 64
        | true, _ -> 64
        | _ -> 4

/// `BLADE_DEBUG_SHAPE_SPEC=1` prints the per-module specialize/cap/decline
/// census. Orchestration/diagnostic aid only; silent by default.
let internal shapeSpecDebug () =
    match System.Environment.GetEnvironmentVariable("BLADE_DEBUG_SHAPE_SPEC") with
    | null | "" | "0" -> false
    | _ -> true

/// The bakeable symbolic extent: the cosmetic placeholder `lowerExtentExpr`
/// emits for `Idx<n>`. `"?"` is that function's give-up marker (an extent
/// expression it could not lower) and names nothing, so it never bakes.
let internal (|ShapeSymbolicExtent|_|) (e: IRExpr) =
    match e with
    | IRParam (name, _, _) when name <> "?" && name <> "" -> Some name
    | _ -> None

let internal (|ShapeLiteralExtent|_|) (e: IRExpr) =
    match e with
    | IRLit (IRLitInt n) when n > 0L -> Some n
    | _ -> None

/// Rewrite an EXTENT expression under a name->literal substitution.
/// Deliberately narrow: the placeholder itself, arithmetic built over it
/// (dependent extents like `n - i` stay correct), and an orbit class's base
/// extent. Everything else -- compound masks, sparse key sources, ragged
/// lookups, `extents(A)` reads, opaque extents -- is a RUNTIME value that
/// happens to sit in an Extent slot and must be left alone.
let rec internal shapeRewriteExtent (subst: Map<string, int64>) (e: IRExpr) : IRExpr =
    match e with
    | IRParam (name, _, _) when subst.ContainsKey name -> IRLit (IRLitInt subst.[name])
    | IRBinOp (mode, op, l, r) ->
        IRBinOp (mode, op, shapeRewriteExtent subst l, shapeRewriteExtent subst r)
    | IRUnaryOp (op, x) -> IRUnaryOp (op, shapeRewriteExtent subst x)
    | IROrbitClass (levels, n) -> IROrbitClass (levels, shapeRewriteExtent subst n)
    | _ -> e

let internal shapeRewriteIx (subst: Map<string, int64>) (ix: IRIndexType) : IRIndexType =
    { ix with Extent = shapeRewriteExtent subst ix.Extent }

/// Rewrite every index record reachable from a type. Structural mirror of
/// `substTypeInIRType`, but over the Extent axis instead of the IRTInfer axis
/// -- and it must descend into the arrow SLOTS, which the HM substituter
/// deliberately skips (`SIdx idx -> SIdx idx`) precisely because extents were
/// out of its scope.
let rec internal shapeRewriteType (subst: Map<string, int64>) (ty: IRType) : IRType =
    match ty with
    | IRTArrow (slots, result, identity) ->
        let slots' =
            slots |> List.map (function
                | SIdx ix -> SIdx (shapeRewriteIx subst ix)
                | SIdxVirt ix -> SIdxVirt (shapeRewriteIx subst ix)
                | SVal t -> SVal (shapeRewriteType subst t))
        IRTArrow (slots', shapeRewriteType subst result, identity)
    | IRTTuple ts -> IRTTuple (ts |> List.map (shapeRewriteType subst))
    | IRTComputation t -> IRTComputation (shapeRewriteType subst t)
    | IRTPoly (b, v) -> IRTPoly (shapeRewriteType subst b, v)
    | IRTUnitAnnotated (t, u) -> IRTUnitAnnotated (shapeRewriteType subst t, u)
    | IRTIdxTagged (t, tag) ->
        // IRefAnon's extent is diagnostics-only (never part of tag identity),
        // but keeping it in step avoids printing `Idx<n>` beside a baked bound.
        let tag' =
            match tag with
            | IRefAnon (nid, ext) -> IRefAnon (nid, shapeRewriteExtent subst ext)
            | other -> other
        IRTIdxTagged (shapeRewriteType subst t, tag')
    | IRTDist (order, elem, axes) ->
        IRTDist (order, shapeRewriteType subst elem, axes |> List.map (shapeRewriteIx subst))
    | IRTGroupKeys (outerIdx, sourceIdx, ev) ->
        IRTGroupKeys (shapeRewriteIx subst outerIdx, shapeRewriteIx subst sourceIdx, ev)
    | IRTLoop lt ->
        IRTLoop { lt with
                    ArrayTypes = lt.ArrayTypes |> List.map (shapeRewriteType subst)
                    KernelType = lt.KernelType |> Option.map (shapeRewriteType subst) }
    | _ -> ty

let internal shapeRewriteArrayType (subst: Map<string, int64>) (aty: IRArrayType) : IRArrayType =
    { aty with
        ElemType = shapeRewriteType subst aty.ElemType
        IndexTypes = aty.IndexTypes |> List.map (shapeRewriteIx subst) }

/// Rewrite every type-bearing field of every node in an expression tree.
/// `mapIRExpr` supplies the traversal; this callback enumerates the positions
/// that actually carry index records -- IRIndexType is OPAQUE to ExprShape,
/// so the records on IRRange/IRVirtualReverse/IRBlocked and the combinator
/// info records must be named here or they are never reached.
let internal shapeRewriteExpr (subst: Map<string, int64>) (expr: IRExpr) : IRExpr =
    if Map.isEmpty subst then expr else
    let rt = shapeRewriteType subst
    let rix = shapeRewriteIx subst
    let rat = shapeRewriteArrayType subst
    mapIRExpr (fun e ->
        match e with
        | IRVar (id, ty) -> IRVar (id, rt ty)
        // NOTE: only the node's TYPE. An IRParam in expression position is a
        // parameter reference; the extent PLACEHOLDER of the same shape only
        // ever lives inside an index record, which this walk reaches through
        // the type positions instead.
        | IRParam (n, i, ty) -> IRParam (n, i, rt ty)
        | IRApp (f, args, retTy) -> IRApp (f, args, rt retTy)
        | IRArrayLit (es, aty) -> IRArrayLit (es, rat aty)
        | IRRange (ixs, off) -> IRRange (ixs |> List.map rix, off)
        | IRVirtualReverse ix -> IRVirtualReverse (rix ix)
        | IRMethodFor info ->
            IRMethodFor { info with
                            ArrayTypes = info.ArrayTypes |> List.map rat
                            SharedIndexTypes = info.SharedIndexTypes |> List.map rix }
        | IRApplyCombinator info ->
            IRApplyCombinator { info with
                                  ArrayTypes = info.ArrayTypes |> List.map rat
                                  SharedIndexTypes = info.SharedIndexTypes |> List.map rix
                                  KernelTDims = info.KernelTDims |> List.map rix
                                  OutputType = rt info.OutputType }
        | IRComposeApply info -> IRComposeApply { info with OutputType = rt info.OutputType }
        | _ -> e) expr

/// Every symbolic extent NAME a parameter list mentions, with its occurrence
/// count. A name bakes only when EVERY one of its occurrences was pinned to
/// the same literal by the call site -- see `shapeSignatureAt`.
let internal shapeSymbolicOccurrences (paramTys: IRType list) : Map<string, int> =
    let acc = System.Collections.Generic.Dictionary<string, int>()
    let bump name =
        acc.[name] <- (match acc.TryGetValue name with | true, v -> v | _ -> 0) + 1
    let rec goIx (ix: IRIndexType) =
        match ix.Extent with
        | ShapeSymbolicExtent name -> bump name
        | _ -> ()
    and goTy (ty: IRType) =
        match ty with
        | IRTArrow (slots, result, _) ->
            slots |> List.iter (function
                | SIdx ix | SIdxVirt ix -> goIx ix
                | SVal t -> goTy t)
            goTy result
        | IRTTuple ts -> ts |> List.iter goTy
        | IRTComputation t | IRTPoly (t, _) | IRTUnitAnnotated (t, _) | IRTIdxTagged (t, _) -> goTy t
        | IRTDist (_, elem, axes) -> goTy elem; axes |> List.iter goIx
        | IRTGroupKeys (a, b, _) -> goIx a; goIx b
        | IRTLoop lt -> lt.ArrayTypes |> List.iter goTy; lt.KernelType |> Option.iter goTy
        | _ -> ()
    paramTys |> List.iter goTy
    acc |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> Map.ofSeq

/// Just the NAME SET a type mentions. Occurrence counts are load-bearing only
/// for the parameter list (full-coverage rule); the name-provenance gate below
/// asks a membership question and nothing more.
let internal shapeSymbolicNames (ty: IRType) : Set<string> =
    shapeSymbolicOccurrences [ty] |> Map.toSeq |> Seq.map fst |> Set.ofSeq

/// Walk a (parameter type, argument type) pair in lockstep, recording for each
/// symbolic parameter extent the literal the argument pins it to. Positions
/// that fail to line up structurally simply record nothing, which -- because a
/// name bakes only at FULL occurrence coverage -- makes them a decline rather
/// than a guess.
let internal shapeObservations (paramTy: IRType) (argTy: IRType) : (string * int64) list =
    let acc = System.Collections.Generic.List<string * int64>()
    let obsIx (p: IRIndexType) (a: IRIndexType) =
        match p.Extent with
        | ShapeSymbolicExtent name ->
            // The two records must describe the same KIND of axis before the
            // argument's number can be believed as this axis' bound.
            if p.Rank = a.Rank && p.Symmetry = a.Symmetry && p.IxKind = a.IxKind then
                match a.Extent with
                | ShapeLiteralExtent n -> acc.Add((name, n))
                | _ -> ()
        | _ -> ()
    let rec go (p: IRType) (a: IRType) =
        match p, a with
        | IRTArrow (ps, pr, _), IRTArrow (as_, ar, _) when ps.Length = as_.Length ->
            List.iter2 (fun pslot aslot ->
                match pslot, aslot with
                | SIdx pi, SIdx ai | SIdxVirt pi, SIdxVirt ai
                | SIdx pi, SIdxVirt ai | SIdxVirt pi, SIdx ai -> obsIx pi ai
                | SVal pt, SVal at -> go pt at
                | _ -> ()) ps as_
            go pr ar
        | IRTTuple pts, IRTTuple ats when pts.Length = ats.Length -> List.iter2 go pts ats
        | IRTComputation pt, IRTComputation at
        | IRTPoly (pt, _), IRTPoly (at, _)
        | IRTUnitAnnotated (pt, _), IRTUnitAnnotated (at, _)
        | IRTIdxTagged (pt, _), IRTIdxTagged (at, _) -> go pt at
        // A unit/tag wrapper on one side only: unwrap and keep pairing.
        | IRTUnitAnnotated (pt, _), _ -> go pt a
        | _, IRTUnitAnnotated (at, _) -> go p at
        | IRTIdxTagged (pt, _), _ -> go pt a
        | _, IRTIdxTagged (at, _) -> go p at
        | _ -> ()
    go paramTy argTy
    acc |> List.ofSeq

/// The extent signature a call site pins on a callee: the sorted
/// (name, literal) list that keys the spec map. A name is admitted only when
/// the arguments pinned EVERY occurrence of it in the parameter list, all to
/// the SAME literal. Both halves matter: full coverage rules out
/// `f(a: Idx<n>, b: Idx<n>)` called with a literal `a` and runtime `b` (baking
/// `n` from `a` would install a wrong bound on `b`'s loop); agreement rules
/// out the same call with a 3-array and a 5-array (unify never compares
/// extents, so this typechecks today).
let internal shapeSignatureAt (func: IRFuncDef) (args: IRExpr list) : (string * int64) list =
    if args.Length <> func.Params.Length then [] else
    let paramTys = func.Params |> List.map _.Type
    let occ = shapeSymbolicOccurrences paramTys
    if Map.isEmpty occ then [] else
    let obs =
        List.zip paramTys args
        |> List.collect (fun (pty, arg) ->
            match exprTypeIfKnown arg with
            | Some aty -> shapeObservations pty aty
            | None -> [])
    obs
    |> List.groupBy fst
    |> List.choose (fun (name, pairs) ->
        let lits = pairs |> List.map snd
        match Map.tryFind name occ with
        | Some k when lits.Length = k && (lits |> List.distinct |> List.length) = 1 ->
            Some (name, List.head lits)
        | _ -> None)
    |> List.sortBy fst

/// Would a specialized copy actually pay? Only if the body iterates: the
/// baked literal reaches the emitted C++ exclusively through loop bounds and
/// through the flat mode's compile-time cell count. A function that merely
/// forwards or reads `extents(A)` gets nothing from a copy, so it does not
/// get one.
let internal shapeSpecWorthwhile (func: IRFuncDef) : bool =
    let mutable found = false
    iterIRExpr (fun e ->
        match e with
        | IRApplyCombinator _ | IRComposeApply _ | IRMethodFor _
        | IRReduce _ | IRReduceCompute _ | IRProdSum _ | IRForRange _
        | IRGram _ | IRGramApply _ | IRMatmul _ | IRSolve _ | IRLu _ | IRLuSolve _ | IRArrayProduct _ | IRArrayNegate _ | IRArrayConjugate _
        | IRReynolds _ | IRDecompact _ | IRTranspose _ -> found <- true
        | _ -> ()) func.Body
    found

/// PROVENANCE GATE. A spec bakes its literals by NAME, over every index record
/// in the body -- but a symbolic extent name sitting in a body type is not
/// necessarily the function's OWN. Two functions that both write `Idx<n>` mint
/// byte-identical `IRParam ("n", …)` placeholders, and a call's result type
/// carries the CALLEE's names into the caller's body, where the local it is
/// bound to keeps them. Baking the enclosing signature's literal into one of
/// those installs a bound the array does not have.
///
/// That is not hypothetical: `f(B: Idx<n>)` holding `let z = scale(w)` with
/// `scale(A: Idx<n>) -> Idx<n>` and a 3-element `w`, specialized at `n = 5`,
/// emitted `for (__ri = 1; __ri < 5; …)` over a 3-cell `z` -- a silent
/// out-of-bounds read (right answer only because the slack pool cells were 0).
///
/// The fix cannot be per-node attribution: at this layer the two `n`s are
/// indistinguishable. So the spec is REFUSED whenever the body introduces a
/// name the signature is about to bake from anywhere other than the function's
/// own parameters. A body acquires foreign names through exactly two doors --
/// a call's result type, and a reference to a module-level binding -- and both
/// are checked here. A call is waved through when its OWN signature pins that
/// name to the very literal being baked, which is the ordinary case (`f` and
/// its callee sharing an extent because the caller forwarded its array), so the
/// gate costs the corpus nothing while closing the collision.
let internal shapeSpecNamesAreOwn
        (funcById: Map<IRId, IRFuncDef>)
        (bindingIds: Set<IRId>)
        (subst: Map<string, int64>)
        (body: IRExpr) : bool =
    let dom = subst |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    let mutable ok = true
    mapIRExpr (fun e ->
        (match e with
         | IRApp (IRVar (fid, _), args, retTy) ->
             let intro = Set.intersect dom (shapeSymbolicNames retTy)
             if not (Set.isEmpty intro) then
                 // The call site is read with the ENCLOSING substitution
                 // applied to its arguments -- that is the form the spec body
                 // will hold, and the arguments are the caller's own values, so
                 // rewriting them is exactly right.
                 let pinned =
                     match Map.tryFind fid funcById with
                     | Some callee ->
                         shapeSignatureAt callee (args |> List.map (shapeRewriteExpr subst)) |> Map.ofList
                     | None -> Map.empty
                 for nm in intro do
                     match Map.tryFind nm pinned with
                     | Some v when v = subst.[nm] -> ()
                     | _ -> ok <- false
         | IRVar (vid, ty) when Set.contains vid bindingIds ->
             if not (Set.isEmpty (Set.intersect dom (shapeSymbolicNames ty))) then ok <- false
         | _ -> ())
        e) body |> ignore
    ok

/// Transitive static call-graph reachability, `caller id -> every function id
/// it can reach`. Spans the whole program: a cycle may run through two modules
/// just as easily as one.
let internal shapeCallReach (funcs: IRFuncDef list) : Map<IRId, Set<IRId>> =
    let ids = funcs |> List.map _.Id |> Set.ofList
    let direct =
        funcs
        |> List.map (fun f -> (f.Id, Set.intersect ids (collectVarRefsIR f.Body)))
        |> Map.ofList
    let mutable reach = direct
    let mutable changed = true
    let mutable guard = 0
    while changed && guard < 64 do
        changed <- false
        guard <- guard + 1
        let next =
            reach |> Map.map (fun _ vs ->
                vs |> Set.fold (fun acc v ->
                    match Map.tryFind v reach with
                    | Some vs2 -> Set.union acc vs2
                    | None -> acc) vs)
        if next <> reach then
            changed <- true
            reach <- next
    reach

/// Function ids that can reach themselves (direct self-recursion or a mutual
/// cycle).
let internal shapeRecursiveIdsOf (reach: Map<IRId, Set<IRId>>) : Set<IRId> =
    reach |> Map.toList |> List.choose (fun (k, vs) -> if Set.contains k vs then Some k else None) |> Set.ofList

/// Every call site a body makes to a named function, as (callee id, args).
let internal shapeCallSitesIn (body: IRExpr) : (IRId * IRExpr list) list =
    let mutable acc = []
    iterIRExpr (fun e ->
        match e with
        | IRApp (IRVar (fid, _), args, _) -> acc <- (fid, args) :: acc
        | _ -> ()) body
    acc

/// Does this call hand the callee the CALLER's own extents, unchanged and in
/// place? True when every extent-carrying parameter position of the callee
/// receives an argument whose index record carries a bare symbolic extent name
/// drawn from the caller's OWN parameter list -- no literal, no arithmetic, no
/// name from anywhere else.
///
/// This is the identity extent substitution that makes a signature CLOSED under
/// a recursive call: specializing the caller rewrites those argument records to
/// the very literals the signature names, so the call re-pins the same
/// signature and rewrites to the spec itself. A call that CHANGES an extent
/// (the `n - 1` shape) is exactly the non-uniform case, and it is refused here:
/// the argument extent stops being a literal, so the recursive call would keep
/// the generic copy while the enclosing spec's own types claim the baked bound.
let internal shapeCallForwardsExtents (callerNames: Set<string>) (calleeParamTys: IRType list) (args: IRExpr list) : bool =
    if args.Length <> calleeParamTys.Length then false else
    let mutable ok = true
    let obsIx (p: IRIndexType) (a: IRIndexType) =
        match p.Extent with
        | ShapeSymbolicExtent _ ->
            let sameAxis = p.Rank = a.Rank && p.Symmetry = a.Symmetry && p.IxKind = a.IxKind
            match a.Extent with
            | ShapeSymbolicExtent k when sameAxis && Set.contains k callerNames -> ()
            | _ -> ok <- false
        | _ -> ()
    // Structural mirror of `shapeObservations`, but a position that fails to
    // line up is a REFUSAL here rather than a silent no-op: the question is
    // "can I prove this forwards", and an unreadable position proves nothing.
    let rec go (p: IRType) (a: IRType) =
        match p, a with
        | IRTArrow (ps, pr, _), IRTArrow (as_, ar, _) when ps.Length = as_.Length ->
            List.iter2 (fun pslot aslot ->
                match pslot, aslot with
                | SIdx pi, SIdx ai | SIdxVirt pi, SIdxVirt ai
                | SIdx pi, SIdxVirt ai | SIdxVirt pi, SIdx ai -> obsIx pi ai
                | SVal pt, SVal at -> go pt at
                | _ -> ok <- false) ps as_
            go pr ar
        | IRTTuple pts, IRTTuple ats when pts.Length = ats.Length -> List.iter2 go pts ats
        | IRTComputation pt, IRTComputation at
        | IRTPoly (pt, _), IRTPoly (at, _)
        | IRTUnitAnnotated (pt, _), IRTUnitAnnotated (at, _)
        | IRTIdxTagged (pt, _), IRTIdxTagged (at, _) -> go pt at
        | IRTUnitAnnotated (pt, _), _ -> go pt a
        | _, IRTUnitAnnotated (at, _) -> go p at
        | IRTIdxTagged (pt, _), _ -> go pt a
        | _, IRTIdxTagged (at, _) -> go p at
        | _ -> if not (Set.isEmpty (shapeSymbolicNames p)) then ok <- false
    List.iter2 (fun (pty: IRType) arg ->
        if not (Set.isEmpty (shapeSymbolicNames pty)) then
            match exprTypeIfKnown arg with
            | Some aty -> go pty aty
            | None -> ok <- false) calleeParamTys args
    ok

/// One planned specialization: the callee it copies, the name->literal map the
/// copy bakes, and the fresh id/name the copy will carry.
type internal ShapeSpec = {
    Orig: IRFuncDef
    Subst: Map<string, int64>
    SpecId: IRId
    SpecName: string
}

/// One planned CO-specialization: a lifted kernel lambda copied alongside the
/// spec whose body applies it, baking that spec's own substitution.
///
/// A spec's body reaches its kernels as VALUES -- `ApplyInfo.Kernel` /
/// `IRObjectFor.Kernel` hold a bare `IRVar (lambdaId, funcTy)`, and codegen
/// resolves and inlines the callable behind it. `shapeRewriteExpr` therefore
/// rewrites the reference's type and stops: the lambda's own parameter,
/// capture and body index records live in a separate `IRFuncDef` in
/// `module.Functions` and keep their symbolic extents, so the inlined kernel's
/// loop bound stays `<row>.extents[0]` inside a spec whose every other bound
/// is a literal. Cloning the lambda per (lambda, signature) and pointing only
/// the spec's own reference at the clone closes that.
type internal ShapeLambdaClone = {
    LOrig: IRCallable
    LSubst: Map<string, int64>
    LCloneId: IRId
    LCloneName: string
}

/// Give every symbolic-extent function a literal-extent copy per distinct
/// call-site shape. Runs after arity and HM monomorphization (both can
/// create the call sites this reads) and before codegen.
///
/// PROGRAM-level, not per-module. Blade's module system is thin: one source
/// file is one module, `import` binds the DEFINING module's function ids
/// straight into the importer's environment, ids come from a single builder so
/// they are program-global, and codegen concatenates every module into one
/// merged unit before emitting. A call from module A to module B is therefore
/// an ordinary `IRApp` over B's id, and the only thing that ever blocked
/// specializing it was that the pass ran once per module. It now sees them all:
/// candidates come from every module, call sites are harvested from every
/// module, and a spec is placed in the module that DEFINES its origin --
/// immediately after it, so the `DerivedFuncOrigins` emission-order rule keeps
/// holding across the merge (the merged map is the union, and B's ids precede
/// A's because a module can only import what was lowered before it).
let shapeMonomorphizeModules (modules: IRModule list) (builder: IRBuilder) : IRModule list =
    let debug = shapeSpecDebug ()
    let cap = shapeSpecCap ()
    let allFuncs = modules |> List.collect _.Functions
    let funcById = allFuncs |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    let bindingIds =
        modules |> List.collect (fun m -> m.Bindings |> List.map _.Id) |> Set.ofList
    let ownNamesOf (f: IRFuncDef) =
        shapeSymbolicOccurrences (f.Params |> List.map _.Type)
        |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    // RECURSION. The blanket decline is replaced by the narrow sound case: a
    // cycle may specialize when every call INSIDE the cycle forwards the
    // caller's own parameter extents through unchanged (see
    // `shapeCallForwardsExtents`). Then each member's signature is closed under
    // the cycle -- specializing one rewrites its intra-cycle calls to the specs
    // of the same shape -- and the fixpoint below reaches that closure on its
    // own, because the forwarded argument records carry the baked literals.
    //
    // The whole cycle must qualify, not just the entry: admitting f while some
    // g on the cycle calls back with a CHANGED extent would leave f's spec
    // reachable from a copy whose types no longer describe the arrays it holds.
    // A cycle through a lifted lambda never qualifies (the lambda's parameter
    // list does not carry the enclosing function's names), which is the
    // conservative answer.
    let reach = shapeCallReach allFuncs
    let recursiveIds = shapeRecursiveIdsOf reach
    let cycleOf (id: IRId) =
        match Map.tryFind id reach with
        | Some outs -> outs |> Set.filter (fun g -> g = id || (match Map.tryFind g reach with
                                                               | Some back -> Set.contains id back
                                                               | None -> false))
        | None -> Set.singleton id
    // Memoized: every member of one cycle asks the same question, and each
    // answer costs a walk of every member's body.
    let admittedCycles = System.Collections.Generic.Dictionary<IRId, bool>()
    let cycleAdmits (id: IRId) : bool =
        match admittedCycles.TryGetValue id with
        | true, v -> v
        | _ ->
            let cycle = cycleOf id
            let ok =
                cycle |> Set.forall (fun mid ->
                    match Map.tryFind mid funcById with
                    | None -> false
                    | Some caller ->
                        let callerNames = ownNamesOf caller
                        shapeCallSitesIn caller.Body
                        |> List.forall (fun (calleeId, args) ->
                            if not (Set.contains calleeId cycle) then true
                            else
                                match Map.tryFind calleeId funcById with
                                | Some callee ->
                                    shapeCallForwardsExtents callerNames
                                        (callee.Params |> List.map _.Type) args
                                | None -> false))
            admittedCycles.[id] <- ok
            ok

    let candidates =
        allFuncs
        |> List.filter (fun f ->
            not f.IsArityPoly
            && (not (Set.contains f.Id recursiveIds) || cycleAdmits f.Id)
            && not (Map.isEmpty (shapeSymbolicOccurrences (f.Params |> List.map _.Type)))
            && shapeSpecWorthwhile f)
    let recursiveDeclines =
        recursiveIds |> Set.filter (fun id -> not (cycleAdmits id))
    if candidates.IsEmpty then modules else
    let candMap = candidates |> List.map (fun f -> (f.Id, f)) |> Map.ofList

    // CO-SPECIALIZING THE LIFTED KERNEL LAMBDAS
    //
    // A spec bakes literals into its own types, but the kernels its body
    // applies are separate `IRFuncDef`s in `module.Functions`, reached as bare
    // `IRVar (lambdaId, funcTy)` values in a combinator record's Kernel slot.
    // The reference's TYPE is rewritten; the definition behind it is not, so
    // the inlined kernel keeps a `.extents[d]` bound inside a spec whose
    // surrounding nest is fully baked. Cloning the lambda per (lambda,
    // signature) and repointing only the spec's own reference closes it.
    //
    // The linkage question is the whole difficulty. Nothing in the IR records
    // "this lambda was lifted out of that function" -- lowering appends every
    // lifted callable to a flat `module.Functions` -- so it is established from
    // the reference structure plus the one thing the name does tell us, and it
    // is exactly what makes inheriting the parent's PROVENANCE (ff3ad88's gate)
    // sound. See `liftedKernelIds` and `lambdaOwnsNames` below.
    let allFuncIds = allFuncs |> List.map _.Id |> Set.ofList
    /// Callable ids appearing as the HEAD of an application anywhere in the
    /// program. An applied callee is specialized through the per-signature spec
    /// path above -- its own call sites pin its own names, which is both more
    /// precise and already deduped -- so co-specialization deliberately covers
    /// only callables reached as VALUES. Excluding them program-wide is also
    /// what lets the reference rewrite be a plain bottom-up id swap: no `IRVar`
    /// it can reach is ever an application head.
    let programAppliedIds =
        let acc = System.Collections.Generic.HashSet<IRId>()
        let scan (b: IRExpr) =
            iterIRExpr (fun e ->
                match e with
                | IRApp (IRVar (id, _), _, _) -> acc.Add id |> ignore
                | _ -> ()) b
        allFuncs |> List.iter (fun f -> scan f.Body)
        modules |> List.iter (fun m -> m.Bindings |> List.iter (fun b -> scan b.Value))
        Set.ofSeq acc
    /// How many DEFINITIONS (function bodies, module-level binding values)
    /// mention each callable id at all -- one count per definition, however
    /// many times that definition names it.
    let refCensus =
        let fromDef (b: IRExpr) = Set.intersect allFuncIds (collectVarRefsIR b) |> Set.toList
        ((allFuncs |> List.collect (fun f -> fromDef f.Body))
         @ (modules |> List.collect (fun m -> m.Bindings |> List.collect (fun b -> fromDef b.Value))))
        |> List.countBy id |> Map.ofList
    /// The callables a spec may co-specialize AT ALL, before any per-signature
    /// question is asked. Every clause is load-bearing for provenance:
    ///
    /// - **synthesized `__lambda_N` name**: this is the linkage. Lowering mints
    ///   such a callable at the point the source lambda expression occurs, so
    ///   its index records were written INSIDE some function's lexical scope
    ///   and an `Idx<n>` in them denotes that function's `n`. A source-level
    ///   function used as a kernel (`method_for(A) <@> krn`) has a real name and
    ///   declares its OWN `n` -- identically spelled, unrelated axis -- and
    ///   baking a caller's literal into it is precisely the out-of-bounds bug
    ///   ff3ad88's gate exists to refuse. (The same prefix test already marks
    ///   synthesized lambdas for `liftInlineFormsModule`'s dead-copy pruning.)
    /// - **referenced by exactly ONE definition**: that definition is then the
    ///   only lexical site the lambda can have come from, which is what turns
    ///   "written inside SOME function's scope" into "written inside THIS
    ///   spec's origin's scope". It also rules out a kernel shared by two
    ///   parents, whose names could not be attributed to either, and (because a
    ///   self-reference counts as a second definition) any recursive lambda --
    ///   the conservative answer the recursion rule already gives.
    /// - **never applied**: see `programAppliedIds`.
    /// - **benefit gate**: same rule as a function spec. A copy pays only
    ///   through loop bounds, so a forwarding lambda (an eta wrapper around a
    ///   named kernel, say) gets none.
    let liftedKernelIds =
        allFuncs
        |> List.filter (fun f ->
            f.Name.StartsWith "__lambda_"
            && not f.IsArityPoly
            && not (Set.contains f.Id programAppliedIds)
            && (match Map.tryFind f.Id refCensus with Some 1 -> true | _ -> false)
            && shapeSpecWorthwhile f)
        |> List.map _.Id
        |> Set.ofList
    /// The per-signature half of the provenance question, and the inherited
    /// form of `shapeSpecNamesAreOwn`.
    ///
    /// `liftedKernelIds` establishes that the lambda's own parameter records
    /// were written in the parent's scope. Two further doors can still carry a
    /// FOREIGN name of the same spelling into the copy, and both are shut here:
    ///
    /// - a CAPTURE, whose type comes from whatever the enclosing scope bound.
    ///   Admitted only when the captured id is one of the owning chain's own
    ///   parameters (`ownedIds`) or its type names nothing being baked. This is
    ///   what refuses the eta wrapper a named kernel produces: it captures the
    ///   named function itself, and that arrow type carries the CALLEE's `n`.
    ///   A capture of an intermediate local declines rather than being chased,
    ///   which costs an optimization and never correctness.
    /// - the lambda's BODY, through exactly the two doors `shapeSpecNamesAreOwn`
    ///   already enumerates (a call's result type, a module-level binding
    ///   reference). The parent passed that gate over ITS body; the lambda's
    ///   body is not part of it, so it is gated in its own right.
    let lambdaOwnsNames (ownedIds: Set<IRId>) (subst: Map<string, int64>) (lam: IRCallable) : bool =
        let dom = subst |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        (lam.Captures
         |> List.forall (fun c ->
                Set.isEmpty (Set.intersect dom (shapeSymbolicNames c.Type))
                || Set.contains c.Id ownedIds))
        && shapeSpecNamesAreOwn funcById bindingIds subst lam.Body

    // Planned specs, keyed exactly as monomorphizeModule's specMap is:
    // (callee id, the signature that distinguishes this copy).
    let mutable specMap : Map<IRId * (string * int64) list, ShapeSpec> = Map.empty
    // Distinct signatures turned away by the cap (a set, not a tally: the
    // fixpoint re-visits the same declined site every round).
    let mutable capDeclines : Set<IRId * (string * int64) list> = Set.empty
    // Distinct signatures turned away by the name-provenance gate.
    let mutable nameDeclines : Set<IRId * (string * int64) list> = Set.empty
    // Planned lambda co-specializations, keyed the same way: (lifted lambda id,
    // the signature of the spec that asked for it). A reference type, not a
    // `let mutable`, because the recursive planner below closes over it.
    let lamMap = System.Collections.Generic.Dictionary<IRId * (string * int64) list, ShapeLambdaClone>()

    /// Rewrite one call site against the CURRENT spec map. Pure, so the
    /// scan below can apply it to a throwaway copy of every body and read
    /// the cascade (an inner call's baked return type is an outer call's
    /// literal argument extent) without committing to anything.
    let rewriteCallSites (expr: IRExpr) : IRExpr =
        mapIRExpr (fun e ->
            match e with
            | IRApp (IRVar (fid, fty), args, retTy) when candMap.ContainsKey fid ->
                let sign = shapeSignatureAt candMap.[fid] args
                if List.isEmpty sign then e
                else
                    match Map.tryFind (fid, sign) specMap with
                    | Some spec ->
                        let subst = spec.Subst
                        IRApp (IRVar (spec.SpecId, shapeRewriteType subst fty),
                               args,
                               shapeRewriteType subst retTy)
                    | None -> e
            | _ -> e) expr

    let specBody (s: ShapeSpec) : IRExpr = shapeRewriteExpr s.Subst s.Orig.Body

    /// Point a body's kernel-slot references at the co-specialized copies
    /// planned for ITS OWN signature. Only bodies belonging to that signature
    /// (the spec's, and its clones') are ever rewritten -- the originals keep
    /// referencing the original lambdas, which is what makes a clone private to
    /// the one copy that wanted it.
    ///
    /// A bottom-up id swap is safe precisely because `liftedKernelIds` excludes
    /// every id that is ever an application head: no `IRVar` this can reach is
    /// the callee position of a call.
    let rewriteLambdaRefs (sign: (string * int64) list) (expr: IRExpr) : IRExpr =
        if lamMap.Count = 0 then expr else
        mapIRExpr (fun e ->
            match e with
            | IRVar (id, ty) when Set.contains id liftedKernelIds ->
                match lamMap.TryGetValue ((id, sign)) with
                | true, c -> IRVar (c.LCloneId, ty)
                | _ -> e
            | _ -> e) expr

    /// The clone's own body: the same rewrite the parent spec's body gets.
    /// VarIds are deliberately NOT freshened, for exactly the reason the spec
    /// itself does not freshen them (see `materialize`): the copy is
    /// type-identical to the original at every VALUE position, so sharing ids
    /// keeps `Captures.Id` pointing at the parameters of the spec that will
    /// hold it -- which are the ORIGINAL function's parameter ids, unchanged.
    let lamBody (c: ShapeLambdaClone) : IRExpr = shapeRewriteExpr c.LSubst c.LOrig.Body

    /// Plan the lambda clones one body needs, transitively: a kernel may itself
    /// apply a further lifted kernel, and that one inherits the same signature
    /// and the same owned-id chain (extended by this lambda's own parameters,
    /// which the nested lambda captures).
    ///
    /// Termination: the recursion follows `liftedKernelIds`, whose members are
    /// referenced by exactly one definition each, so the reference graph over
    /// them is a forest -- no cycle to chase -- and `cap` bounds the breadth per
    /// origin regardless.
    let rec planLambdas (ownedIds: Set<IRId>) (subst: Map<string, int64>)
                        (sign: (string * int64) list) (body: IRExpr) : unit =
        for lid in Set.intersect liftedKernelIds (collectVarRefsIR body) do
            if not (lamMap.ContainsKey ((lid, sign))) then
                match Map.tryFind lid funcById with
                | None -> ()
                | Some lam ->
                    if lambdaOwnsNames ownedIds subst lam then
                        let newParams =
                            lam.Params |> List.map (fun p -> { p with Type = shapeRewriteType subst p.Type })
                        let newRet = shapeRewriteType subst lam.RetType
                        let newCaps =
                            lam.Captures |> List.map (fun c -> { c with Type = shapeRewriteType subst c.Type })
                        let newBody = shapeRewriteExpr subst lam.Body
                        // Vacuity: this signature names nothing the lambda
                        // mentions, so the copy would be the original.
                        let vacuous =
                            newParams = lam.Params && newRet = lam.RetType
                            && newCaps = lam.Captures && newBody = lam.Body
                        // Same cap as a function spec, counted per ORIGIN
                        // lambda: the standing termination backstop applies
                        // here too, and a lambda cannot outgrow the specs that
                        // ask for it by more than the nesting depth.
                        let existing =
                            lamMap.Keys |> Seq.filter (fun (k, _) -> k = lid) |> Seq.length
                        if not vacuous && existing < cap then
                            lamMap.[(lid, sign)] <-
                                { LOrig = lam
                                  LSubst = subst
                                  LCloneId = builder.FreshId()
                                  LCloneName =
                                    sprintf "%s_shape%s" lam.Name
                                            (sign |> List.map (fun (n, v) -> $"_{n}{v}")
                                                  |> String.concat "") }
                            planLambdas
                                (Set.union ownedIds (lam.Params |> List.map _.VarId |> Set.ofList))
                                subst sign newBody

    // Fixpoint: each round rewrites every body (originals, bindings, and the
    // specs planned so far) with the current map, then harvests the call sites
    // exposed. A new spec can expose more (its body's own calls now carry
    // literal argument extents), so the round repeats until nothing changes.
    // `rounds < 8` is a runaway backstop; real convergence is 2. Anything past
    // the backstop simply keeps the generic copy.
    let mutable changed = true
    let mutable rounds = 0
    while changed && rounds < 8 do
        changed <- false
        rounds <- rounds + 1
        let bodies =
            (allFuncs |> List.map _.Body)
            @ (modules |> List.collect (fun m -> m.Bindings |> List.map _.Value))
            @ (specMap |> Map.toList |> List.map (snd >> specBody))
            // A clone's body is a call-site source in its own right: its calls
            // now carry literal argument extents, which can specialize a
            // function nothing else pinned.
            @ (lamMap.Values |> Seq.map lamBody |> List.ofSeq)
        let sites =
            bodies
            |> List.collect (fun b ->
                let mutable found = []
                iterIRExpr (fun e ->
                    match e with
                    | IRApp (IRVar (fid, _), args, _) when candMap.ContainsKey fid ->
                        let sign = shapeSignatureAt candMap.[fid] args
                        if not (List.isEmpty sign) then found <- (fid, sign) :: found
                    | _ -> ()) (rewriteCallSites b)
                found)
            |> List.distinct
        for (fid, sign) in sites do
            if not (Map.containsKey (fid, sign) specMap) then
                let orig = candMap.[fid]
                // Provenance gate: refuse a signature whose literals would be
                // baked into a name this body did not own (see
                // `shapeSpecNamesAreOwn`). Checked per (function, signature)
                // rather than per function -- the same body is safe at one
                // shape and unsafe at another.
                if not (shapeSpecNamesAreOwn funcById bindingIds (Map.ofList sign) orig.Body) then
                    nameDeclines <- Set.add (fid, sign) nameDeclines
                else
                let existing = specMap |> Map.filter (fun (k, _) _ -> k = fid) |> Map.count
                if existing >= cap then
                    capDeclines <- Set.add (fid, sign) capDeclines
                else
                    let specId = builder.FreshId()
                    let suffix = sign |> List.map (fun (n, v) -> $"_{n}{v}") |> String.concat ""
                    specMap <- Map.add (fid, sign)
                                       { Orig = orig
                                         Subst = Map.ofList sign
                                         SpecId = specId
                                         SpecName = $"{orig.Name}_shape{suffix}" }
                                       specMap
                    changed <- true
        // Co-specialize the kernels every planned spec applies. Done inside the
        // fixpoint rather than after it so a clone's own calls join the next
        // round's harvest, and so a clone minted by a spec planned this round
        // is seen before the loop settles.
        let lamBefore = lamMap.Count
        specMap
        |> Map.toList
        |> List.iter (fun ((_, sign), s) ->
            planLambdas (s.Orig.Params |> List.map _.VarId |> Set.ofList)
                        s.Subst sign (specBody s))
        if lamMap.Count > lamBefore then changed <- true

    // Census, per DEFINING module: candidates counted where they are defined,
    // specs where they are placed, declines charged to the module owning the
    // callee. One module in, one line out -- the format the orchestration
    // scripts diff -- and identical numbers to the per-module pass for a
    // single-module program.
    let ownerOfFunc =
        modules
        |> List.mapi (fun i m -> m.Functions |> List.map (fun f -> (f.Id, i)))
        |> List.concat |> Map.ofList
    let reportCensus () =
        if debug then
            modules |> List.iteri (fun i m ->
                let inThis (id: IRId) = (match Map.tryFind id ownerOfFunc with Some j -> j = i | None -> false)
                let cands = candidates |> List.filter (fun f -> inThis f.Id)
                if not cands.IsEmpty then
                    let mySpecs = specMap |> Map.toList |> List.filter (fun ((fid, _), _) -> inThis fid)
                    let countDecl (s: Set<IRId * (string * int64) list>) =
                        s |> Set.filter (fun (fid, _) -> inThis fid) |> Set.count
                    if mySpecs.IsEmpty then
                        eprintfn "[shape-spec] %s: %d candidate(s), 0 specialized" m.Name cands.Length
                    else
                        let perFunc =
                            mySpecs |> List.map (fun ((fid, _), s) -> (fid, s.Orig.Name))
                            |> List.groupBy id |> List.map (fun ((_, n), g) -> $"{n} x{g.Length}")
                        // Lambda clones are charged to the module owning the
                        // LAMBDA, which is where they are placed -- the same
                        // rule the spec counts follow.
                        let myLams =
                            lamMap.Keys |> Seq.filter (fun (lid, _) -> inThis lid) |> Seq.length
                        eprintfn "[shape-spec] %s: %d candidate(s), %d spec(s) [%s], %d lambda-clone(s), %d cap-decline(s), %d name-decline(s), %d recursive decline(s), %d round(s)"
                                 m.Name cands.Length mySpecs.Length (String.concat "; " perFunc) myLams
                                 (countDecl capDeclines) (countDecl nameDeclines)
                                 (recursiveDeclines |> Set.filter inThis |> Set.count) rounds)

    if Map.isEmpty specMap then
        reportCensus ()
        modules
    else

    // Materialize the copies. Param VarIds are deliberately NOT freshened
    // (unlike the HM specializer, which freshens and then must clone every
    // lifted lambda that captured a param to repair Captures.Id): the copy is
    // type-identical to the original at every VALUE position -- only Extent
    // fields differ -- so sharing VarIds keeps every lifted lambda the body
    // references, ORIGINAL or CO-SPECIALIZED, bound to exactly the parameters
    // it always was. That is what lets a clone carry its origin's
    // `Captures.Id` list over untouched.
    let materialize (s: ShapeSpec) : IRFuncDef =
        { s.Orig with
            Id = s.SpecId
            Name = s.SpecName
            Params = s.Orig.Params |> List.map (fun p -> { p with Type = shapeRewriteType s.Subst p.Type })
            RetType = shapeRewriteType s.Subst s.Orig.RetType
            Captures = s.Orig.Captures |> List.map (fun c -> { c with Type = shapeRewriteType s.Subst c.Type })
            Body = rewriteLambdaRefs (Map.toList s.Subst) (rewriteCallSites (specBody s)) }

    /// A co-specialized kernel. Same shape as `materialize`, and same VarId
    /// rule for the same reason; the reference rewrite is keyed on the SAME
    /// signature, so a nested kernel resolves to the clone made for this copy.
    let materializeLambda (c: ShapeLambdaClone) : IRCallable =
        { c.LOrig with
            Id = c.LCloneId
            Name = c.LCloneName
            Params = c.LOrig.Params |> List.map (fun p -> { p with Type = shapeRewriteType c.LSubst p.Type })
            RetType = shapeRewriteType c.LSubst c.LOrig.RetType
            Captures = c.LOrig.Captures |> List.map (fun cp -> { cp with Type = shapeRewriteType c.LSubst cp.Type })
            Body = rewriteLambdaRefs (Map.toList c.LSubst) (rewriteCallSites (lamBody c)) }

    // PLACEMENT IS PART OF CORRECTNESS, not cosmetics. Codegen interleaves
    // bindings and functions in IRId order; every function
    // `computeMainLocalFuncIds` classifies as main-local is a `std::function`
    // LOCAL inside main() with no forward declaration, so a copy carrying a
    // fresh (largest) id sorts AFTER the call sites this pass just rewrote to
    // it -- a scope error. The copy is placed immediately after its origin
    // here, keyed by the ORIGIN's id (IRModule.DerivedFuncOrigins), so it
    // lands at exactly its origin's program point and is visible to precisely
    // the call sites the origin was.
    // The copy lands in the module that DEFINES the origin, which is what makes
    // the rule survive going cross-module: the merged emission stream keys a
    // derived id on its origin's, so B's copy sits at B's program point even
    // though A's call site is what asked for it.
    // A lambda clone obeys the identical rule against ITS origin, the lambda --
    // and that is enough for the spec that references it, without a second
    // argument. The spec is emitted at its own origin `f`'s program point; `f`
    // already referenced the lambda, so wherever `f` is in scope the lambda is
    // too, and the clone sits immediately after the lambda.
    let specsByOrigin =
        specMap |> Map.toList |> List.map snd |> List.groupBy _.Orig.Id |> Map.ofList
    let lamsByOrigin =
        lamMap.Values |> List.ofSeq |> List.groupBy _.LOrig.Id |> Map.ofList
    let rewritten =
        modules |> List.map (fun modul ->
            let newFunctions =
                modul.Functions
                |> List.collect (fun f ->
                    let f' = { f with Body = rewriteCallSites f.Body }
                    let specCopies =
                        match Map.tryFind f.Id specsByOrigin with
                        | Some specs -> specs |> List.map materialize
                        | None -> []
                    let lamCopies =
                        match Map.tryFind f.Id lamsByOrigin with
                        | Some clones -> clones |> List.map materializeLambda
                        | None -> []
                    f' :: (specCopies @ lamCopies))
            let newBindings = modul.Bindings |> List.map (fun b -> { b with Value = rewriteCallSites b.Value })
            let derivedOrigins =
                let withSpecs =
                    specMap
                    |> Map.toList
                    |> List.fold (fun acc (_, s) ->
                        if newFunctions |> List.exists (fun f -> f.Id = s.SpecId)
                        then Map.add s.SpecId s.Orig.Id acc else acc) modul.DerivedFuncOrigins
                lamMap.Values
                |> Seq.fold (fun acc c ->
                    if newFunctions |> List.exists (fun f -> f.Id = c.LCloneId)
                    then Map.add c.LCloneId c.LOrig.Id acc else acc) withSpecs
            { modul with
                Functions = newFunctions
                Bindings = newBindings
                DerivedFuncOrigins = derivedOrigins })

    reportCensus ()
    rewritten

/// Single-module entry point, for callers that hold one `IRModule` rather than
/// a whole program. The lowering pipeline uses the plural form -- a program's
/// modules must be handed over TOGETHER or a cross-module call site cannot see
/// the definition it wants specialized.
let shapeMonomorphizeModule (modul: IRModule) (builder: IRBuilder) : IRModule =
    match shapeMonomorphizeModules [modul] builder with
    | [m] -> m
    | _ -> modul

