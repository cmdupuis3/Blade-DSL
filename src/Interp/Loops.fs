// Blade tree-walking interpreter: loop-nest interpreter (Milestone M2).
//
// The heart of M2: turns the deferred/force combinator algebra and the dense
// loop-nest / reduction machinery into runtime Values that byte-match the
// compiled C++ (the differential gate). Anti-drift invariant: this module
// consumes the same `LoopNestCodeGen` structure CodeGen renders, built by
// calling `IRStorage.buildLoopNestCodeGen` directly (never re-derived), so
// iteration order, bound formulas (Extent - SumDeps - Strict), per-level
// element peeling, and reduction seed/order/empty semantics cannot diverge
// from the compiled binary.
//
// Ground truth (line refs to the scout-time tree): genApplyCombinator
// (CodeGen.fs:5427), genLoopNestStreamed (3599), genElementBindingNew (2944),
// genForLoopHeader/genLoopBoundExpr (3381/3344), genComputeBinding +
// resolveComputation + applyFunctorWrappers (7473/7480/7534),
// genReduceBinding (8619), genReduceComputeBinding (8714), genParallelTree
// (6749), buildLoopNestCodeGen (IR.fs:2824).
//
// AsyncLocal DOES flow into Runtime.runOnLargeStack's worker thread
// (ExecutionContext capture on Thread.Start), and a set on the worker is
// visible within that worker synchronously. `withCallablesContext` installs
// the module's callables table via IR.setCallablesContext; Run.fs wraps the
// whole run in it on the worker thread, so buildLoopNestCodeGen's
// resolveKernel always resolves.
//
// Compiled after Interp/Core.fs (Core.evalExpr, InterpState, InterpHooks)
// and Interp/ArrayOps.fs (dense storage primitives), before Interp/Print.fs.
// Run.fs installs { EvalArrayNode = evalArrayNode; Force = force } into
// InterpState.Hooks.
module Blade.Interp.Loops

open System.Collections.Generic
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Interp.Value
open Blade.Interp.Core

module N = Blade.Interp.Numerics
module A = Blade.Interp.ArrayOps

// AnalysisContext install (buildLoopNestCodeGen's resolveKernel dependency).

/// Install the module's callables table into the AsyncLocal AnalysisContext for
/// the duration of `f`, restoring the prior context afterward. MUST wrap every
/// entry point that reaches buildLoopNestCodeGen (resolveKernel reads the table).
/// Run.fs calls this once around the whole run on the large-stack worker thread.
let withCallablesContext (modul: IRModule) (f: unit -> 'a) : 'a =
    let table = buildCallablesTableForModule modul
    let prev = setCallablesContext table
    try f ()
    finally restoreAnalysisContext prev

// Small value coercions (local mirrors of Core's private ones).

let private toI64 (v: Value) : int64 =
    match v with
    | VInt n -> n | VInt32 n -> int64 n
    | VFloat f -> int64 f | VFloat32 f -> int64 (float f)
    | VBool b -> (if b then 1L else 0L)
    | VChar c -> int64 (int c)
    | _ -> 0L

/// C++ truthiness of a kernel value for `<|>` (choice) and guard: `x != 0`.
let private isNonZero (v: Value) : bool =
    match v with
    | VBool b -> b
    | VInt n -> n <> 0L
    | VInt32 n -> n <> 0
    | VFloat f -> f <> 0.0
    | VFloat32 f -> f <> 0.0f
    | VComplex (r, i) -> r <> 0.0 || i <> 0.0
    | _ -> false

/// The zero seed of a scalar element type -- mirrors `T name = 0;` (the scalar
/// accumulator declaration in genApplyCombinator's IRTScalar branch).
let private zeroOfElem (et: ElemType) : Value =
    match et with
    | ETInt64 | ETInt32 -> VInt 0L
    | ETBool -> VBool false
    | ETComplex128 | ETComplex64 -> VComplex (0.0, 0.0)
    | _ -> VFloat 0.0

let private nodeCase (e: IRExpr) : string =
    let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(e, typeof<IRExpr>)
    case.Name

/// Count the fully-flattened tuple leaves of a Value (mirrors Core.countLeaves /
/// IR.flattenTupleLeaves: only VTuple recurses; every other value is one leaf).
let rec private countLeaves (v: Value) : int =
    match v with
    | VTuple els -> els |> Array.sumBy countLeaves
    | _ -> 1

/// Project element `i` from a (forced) tuple value -- structural (get<i>) or
/// flat (leaf of the fully-flattened tuple). Mirrors Core.projectStruct / projectFlat.
let rec private projectValue (v: Value) (i: int) (isFlat: bool) : Value =
    if not isFlat then
        match v with
        | VTuple els when i >= 0 && i < els.Length -> els.[i]
        | _ -> raise (InterpUnsupported "tuple projection of a non-tuple (deferred tuple did not force to a VTuple)")
    else
        match v with
        | VTuple els ->
            let mutable offset = 0
            let mutable result = None
            let mutable k = 0
            while result.IsNone && k < els.Length do
                let c = countLeaves els.[k]
                if i < offset + c then
                    match els.[k] with
                    | VTuple _ -> result <- Some (projectValue els.[k] (i - offset) true)
                    | leaf -> result <- Some leaf
                offset <- offset + c
                k <- k + 1
            match result with
            | Some r -> r
            | None -> raise (InterpUnsupported "flat tuple projection index out of range")
        | leaf -> leaf

// Nest input sources + output sinks.

/// A resolved input to a nest, keyed by ArrayPosition. Virtual sources carry no
/// store: their element values are computed from the loop index at peel time.
type private ArraySource =
    | SReal of BladeArray
    | SVirtual

/// Output sink for interpretNest: dense cell writes (array output) OR a scalar
/// fold accumulator (scalar-output apply, or a reduce-over-computation leaf).
type private OutTarget =
    | OutArray of BladeArray
    | OutFold of acc: ValueRef * wrapper: (Value -> Value -> Value)
    /// ARRAY-VALUED KERNEL RETURN (stage S3, manifestation M-C): the kernel
    /// produces a whole ROW per iterated cell, so the output's rank exceeds the
    /// nest depth and each store writes `rowRank` trailing coordinates at once.
    /// The compiled twin copies the returned pool into the output row; here the
    /// row's own cells are written through the same `writeCell` path, so the two
    /// lanes agree cell for cell rather than by construction.
    | OutArrayRows of BladeArray

// applyFunctorWrappers: ported from CodeGen.genComputeBinding (7534). Folds
// functor-map wrappers into the ApplyInfo kernel body (beta-reduce +
// synthetic-callable registration in the AnalysisContext, which the
// interpreter's own resolveKernel then reads). Uses st.Builder for FreshId.

let private applyFunctorWrappers (st: InterpState) (info: ApplyInfo) (wrappers: IRExpr list) : ApplyInfo =
    if List.isEmpty wrappers then info
    else
        let betaReduce (wrapper: IRExpr) (body: IRExpr) : IRExpr =
            match resolveCallable wrapper with
            | Some c when c.Params.Length = 1 ->
                let paramId = c.Params.[0].VarId
                let rec subst (expr: IRExpr) =
                    match expr with
                    | IRVar (id, _) when id = paramId -> body
                    | IRVar _ | IRLit _ | IRParam _ -> expr
                    | IRBinOp (m, op, l, r) -> IRBinOp (m, op, subst l, subst r)
                    | IRUnaryOp (op, e) -> IRUnaryOp (op, subst e)
                    | IRIf (c2, t, e) -> IRIf (subst c2, subst t, subst e)
                    | IRApp (f, args, rt) -> IRApp (subst f, args |> List.map subst, rt)
                    | IRIndex (a2, idxs, ty) -> IRIndex (subst a2, idxs |> List.map subst, ty)
                    | IRTuple es -> IRTuple (es |> List.map subst)
                    | IRComplex (re, im) -> IRComplex (subst re, subst im)
                    | IRFma (a, b, c) -> IRFma (subst a, subst b, subst c)
                    | IRTupleProj (e, i, flat) -> IRTupleProj (subst e, i, flat)
                    | IRFieldAccess (e, f) -> IRFieldAccess (subst e, f)
                    | IRLet (id, v, b) -> IRLet (id, subst v, subst b)
                    | _ -> expr
                subst c.Body
            | _ ->
                let retTy =
                    match resolveCallable wrapper with
                    | Some c -> c.RetType
                    | None -> IRTScalar ETFloat64
                IRApp (wrapper, [body], retTy)
        let wrappedKernel =
            let wrapBody (body: IRExpr) = wrappers |> List.fold (fun b w -> betaReduce w b) body
            let buildInline (c: IRCallable) : IRExpr =
                let synthetic = { c with Id = st.Builder.FreshId(); Body = wrapBody c.Body }
                registerSyntheticCallable synthetic
            mapKernelInner buildInline info.Kernel
        let newOutputType =
            match wrappers |> List.tryHead with
            | Some w -> (match resolveCallable w with Some c -> c.RetType | None -> info.OutputType)
            | None -> info.OutputType
        let adjustedOutputType =
            match info.OutputType, newOutputType with
            | ArrayElem arr, IRTScalar et -> mkArrayLike { arr with ElemType = IRTScalar et }
            | _ -> newOutputType
        { info with Kernel = wrappedKernel; OutputType = adjustedOutputType }

// Input gating: mirrors CodeGen's ragged/grouped/compound/mpi refusals so
// the differential gate SKIP-UNSUPPORTEDs the exact categories CodeGen also gates.

let private raggedFamilyOrCompound (ix: IRIndexType) : bool =
    match ix.IxKind with
    | IxKRagged | IxKRaggedInline | IxKRaggedOpaque
    | IxKDep | IxKDepInner | IxKDepOuter
    | IxKGroupOuter | IxKGroupMember
    | IxKCompound | IxKCompoundDynamic | IxKSparse -> true
    | _ -> false

let private gateInputs (info: ApplyInfo) : unit =
    if info.ArrayTypes |> List.exists (fun at -> at.IndexTypes |> List.exists raggedFamilyOrCompound) then
        raise (InterpUnsupported "apply over ragged/grouped/compound input (M2.7)")

// Binary fold resolution (reduce kernels / choice sections lower to callables).

/// The fold kernel of a `reduce` / `<&!>` leg, as a binary closure.
///
/// The SITE ENV is threaded for the same reason `resolveUnaryKernel` threads it:
/// `callCallable` passes NO captures and `evalCall` chains a callable's frame to
/// the module-global scope only, so a capture bound in an enclosing FUNCTION
/// frame is invisible. `function wsum(xs) = { let w = 2.0; reduce(xs, lambda(p,
/// q) -> p + w * q) }` captures `w` that way -- the compiled lane forwards it
/// through the kernel's capture list, and without this the interpreter died on
/// the unbound id (BL8004) while the compiled lane printed the answer. Kernels
/// that capture nothing bind an empty map and behave exactly as before.
let private resolveBinaryFold (st: InterpState) (env: Env) (kernel: IRExpr) : (Value -> Value -> Value) =
    match resolveKernel kernel with
    | Some rk when rk.Callable.Params.Length = 2 ->
        let callable = rk.Callable
        let caps =
            callable.Captures
            |> List.choose (fun c -> envTryFind env c.Id |> Option.map (fun cell -> (c.Id, cell)))
            |> Map.ofList
        (fun a b -> Core.evalCall st callable caps [a; b])
    | _ -> raise (InterpUnsupported "reduce/fold kernel does not resolve to a binary callable")

// Array literal -> dense BladeArray (via ArrayOps.arrayLitFromValues). The
// top-level element exprs evaluate to Values: scalar leaves for a rank-1
// literal; VArray rows for a rank>=2 literal (a nested IRArrayLit element
// goes back through Core.evalExpr -> this hook -> a VArray), packed flat
// (rank-1) / SNested (rank>=2). Ragged/DepIdx literals gated.

let private evalArrayLit (st: InterpState) (env: Env) (elems: IRExpr list) (arrType: IRArrayType) : Value =
    if arrType.IndexTypes |> List.exists raggedFamilyOrCompound then
        raise (InterpUnsupported "ragged/depidx array literal (M2.7)")
    let vals = elems |> List.map (Core.evalExpr st env)
    VArray (A.arrayLitFromValues arrType vals)

// Forward declarations resolved via the recursive block below.

/// Public Force hook: drive a possibly-deferred Value to a concrete one.
let rec force (st: InterpState) (env: Env) (v: Value) : Value =
    match v with
    | VDeferred (expr, denv) ->
        // Forced-on-read auto-print parity: a payload that is a module-level
        // binding's Value node (hit in DeferredBindingIndex) means this force
        // materializes that binding "under its own name" -- the value-space
        // twin of CodeGen's forceDeferredArrayInput IRVar arm. Record the id
        // (Print adds it to the render list) and memoize into the root cell
        // so later consumers, and Print, see the materialized value, as the
        // C++ names the materialized array once. Sub-expression VDeferreds
        // miss the index and force as before; resolveComp/forceTreeShaped
        // peel through root VDeferreds without calling force, mirroring
        // resolveComputation's inline resolution (also non-materializing).
        (match st.DeferredBindingIndex.TryGetValue expr with
         | true, id ->
             let fv = forceExpr st denv expr
             st.ForcedDeferred.Add id |> ignore
             (match st.Global with
              | Some g ->
                  (match envTryFind g id with
                   | Some cell -> cell.V <- fv
                   | None -> ())
              | None -> ())
             fv
         | _ -> forceExpr st denv expr)
    | other -> other

// ----------------------------------------------------------------------------
// resolveComputation (CodeGen.fs:7480) in value space: peel IRCompute /
// IRFunctorMap (collecting wrappers innermost-first) / IRVar-through-deferred /
// IRGuard (re-wrap) / IRComposeMeth (extract right's kernel as a wrapper).
// Threads the env so a var bound to a VDeferred with its OWN captured env
// resolves that env's captures correctly.
// ----------------------------------------------------------------------------
and private resolveComp (st: InterpState) (env: Env) (expr: IRExpr) (wrappers: IRExpr list) : IRExpr * IRExpr list * Env =
    match expr with
    | IRVar (id, _) ->
        match envTryFind env id with
        | Some cell ->
            match cell.V with
            | VDeferred (e2, env2) -> resolveComp st env2 e2 wrappers
            | _ -> (expr, wrappers, env)
        | None -> (expr, wrappers, env)
    | IRCompute inner -> resolveComp st env inner wrappers
    | IRFunctorMap (f, inner) -> resolveComp st env inner (f :: wrappers)
    | IRGuard (cond, body) ->
        let (r, w, e) = resolveComp st env body wrappers
        (IRGuard (cond, r), w, e)
    | IRComposeMeth (left, right) ->
        match extractInlinableKernel st env right with
        | Some k -> resolveComp st env left (k :: wrappers)
        | None -> (expr, wrappers, env)
    | _ -> (expr, wrappers, env)

/// Mirror resolveComputation's IRComposeMeth kernel extraction (7509): reach the
/// right operand's inline kernel through deferred vars / functor-map composition.
and private extractInlinableKernel (st: InterpState) (env: Env) (e: IRExpr) : IRExpr option =
    match e with
    | IRVar (id, _) ->
        match envTryFind env id with
        | Some cell -> (match cell.V with VDeferred (e2, env2) -> extractInlinableKernel st env2 e2 | _ -> None)
        | None -> None
    | IRApplyCombinator info ->
        match resolveCallable info.Kernel with Some _ -> Some info.Kernel | None -> None
    | IRFunctorMap (f, inner) ->
        match extractInlinableKernel st env inner with
        | Some k -> Some (IRCompose (k, f))
        | None -> None
    | _ -> None

/// Apply resolveComp-collected functor / compose wrappers (innermost-first) to
/// a concrete value -- the value-space twin of applyFunctorWrappers for a base
/// that bottomed out at a CONCRETE array (or scalar), e.g. `f <$> A` over a
/// plain array. Mirrors materializeComposeApply's wrapAll fold (IRCompose(k,f)
/// = f.k) so the result matches `method_for(A) <@> f |> compute` byte-for-byte.
and private applyWrappersToValue (st: InterpState) (env: Env) (wrappers: IRExpr list) (v: Value) : Value =
    if List.isEmpty wrappers then v else
    let rec wrapperFn (w: IRExpr) : (Value -> Value) =
        match w with
        | IRCompose (k, f) -> let kf = wrapperFn k in let ff = wrapperFn f in (fun x -> ff (kf x))
        | _ -> resolveUnaryKernel st env w
    let wrapAll = wrappers |> List.fold (fun acc w -> let wf = wrapperFn w in (fun x -> wf (acc x))) id
    match v with
    | VArray a ->
        let out = A.allocDense a.ElemType a.IndexTypes a.Extents
        let rank = a.Extents.Length
        let rec walk (level: int) (acc: int64 list) =
            if level = rank then
                let coords = List.rev acc
                A.writeCell out coords (wrapAll (A.readCell a coords))
            else
                for i in 0L .. a.Extents.[level] - 1L do walk (level + 1) (i :: acc)
        walk 0 []
        VArray out
    | scalar -> wrapAll scalar

/// Force an IRExpr (the deferred payload) to a concrete Value. Mirrors
/// genComputeBinding's resolveComputation + dispatch.
and private forceExpr (st: InterpState) (env: Env) (expr: IRExpr) : Value =
    let (resolved, wrappers, renv) = resolveComp st env expr []
    match resolved with
    | IRApplyCombinator info -> materializeApply st renv info wrappers
    | IRParallel _ | IRFusion _ -> forceParallelTree st renv resolved wrappers
    | IRChoice (left, right) -> forceChoice st renv left right wrappers
    | IRFallback (a, b) -> forceFallback st renv a b wrappers
    | IRGuard (cond, body) -> forceGuard st renv cond body wrappers
    | IRSequence elems -> forceSequence st renv elems wrappers
    | IRBind (comp, cont) -> forceBind st renv comp cont
    | IRComposeMeth (left, right) -> forceComposeMeth st renv left right
    | IRComposeApply cinfo -> materializeComposeApply st renv cinfo wrappers
    | IRComposeObj _ -> raise (InterpUnsupported "IRComposeObj force")
    // A projection of a deferred tuple (`(c1,c2) = <combinator producing a tuple>`,
    // tuple-of-deferred 4): force the inner to a VTuple, project element i, then
    // force the projected element (itself possibly a deferred computation).
    | IRTupleProj (inner, i, isFlat) ->
        let tv = forceExpr st renv inner
        applyWrappersToValue st renv wrappers (force st renv (projectValue tv i isFlat))
    | IRVar (id, _) ->
        // A base that bottomed out at a CONCRETE array/scalar (resolveComp already
        // followed any VDeferred alias); apply the trailing functor wrappers here:
        // `f <$> A` over a plain array A.
        match envTryFind renv id with
        | Some cell -> applyWrappersToValue st renv wrappers (force st renv cell.V)
        | None -> raise (InterpUnsupported "force of unbound var")
    // A BARE range forced as a value (`let xs = 0..n` via evalBinding's eager
    // force, `reduce(0..n, (+))`, `0..n |> compute`): materialize the iota,
    // x[i] = offset + i -- the differential twin of genRangeBinding /
    // materializeRangeForm. Nest inputs never come here (resolveArraySource
    // keeps ranges SVirtual). Without this arm the fallback below re-defers
    // (Core.evalExpr's IRRange arm answers VDeferred) and the force is a
    // no-op. Same single-slot plain dense scope as the compiled side;
    // everything else stays a gate SKIP.
    | IRRange (ixs, offset) ->
        (match ixs with
         | [ ix ] when ix.IxKind = IxKPlain && ix.Rank = 1
                       && (match ix.Tag with
                           | Some t -> not (t.StartsWith haloWinTagPrefix)
                           | None -> true) ->
             let evalInt (e: IRExpr) (what: string) =
                 match Core.evalExpr st renv e with
                 | VInt n -> n
                 | VInt32 n -> int64 n
                 | _ -> raise (InterpUnsupported $"range {what} did not evaluate to an integer")
             let n = evalInt ix.Extent "extent"
             let off = match offset with Some o -> evalInt o "offset" | None -> 0L
             let arrType : IRArrayType =
                 { ElemType = IRTScalar ETInt64; IndexTypes = [ix]; IsVirtual = false; Identity = None }
             let vals = [ for i in 0L .. n - 1L -> VInt (off + i) ]
             applyWrappersToValue st renv wrappers (VArray (A.arrayLitFromValues arrType vals))
         | _ -> raise (InterpUnsupported "force of a compound/sparse/halo or multi-slot range"))
    | other -> applyWrappersToValue st renv wrappers (Core.evalExpr st renv other)

// ----------------------------------------------------------------------------
// Parallel / fusion: collect leaves (flatten <&>/<&!>, resolve deferred vars),
// materialize each independently (pillar (c): each leaf its own nest / order),
// and assemble a FLAT tuple in left-to-right leaf order (matching genParallelTree
// and genFusionTree's make_tuple convention consumed by tuple destructuring).
// ----------------------------------------------------------------------------
and private forceParallelTree (st: InterpState) (env: Env) (expr: IRExpr) (wrappers: IRExpr list) : Value =
    if not (List.isEmpty wrappers) then
        // CodeGen's IRParallel/IRFusion arms drop functor wrappers; rather than
        // silently reproduce a possibly-latent drop, gate (gate SKIPs).
        raise (InterpUnsupported "functor-map wrapper over a parallel/fusion tree")
    forceTreeShaped st env expr

/// Force a parallel/fusion tree to a structured tuple mirroring the tree
/// shape -- VTuple [| left; right |] per IRParallel/IRFusion node: `a <&> b
/// <&> c` is ((a,b),c). CodeGen emits a flat make_tuple plus a TupleChildren
/// map genTupleProjBinding consults for structural projections; the
/// interpreter's projections are shape-driven (Core.projectStruct/
/// projectFlat), so the value shape itself must carry the structure. A flat
/// projection flattens through nested VTuples (countLeaves), so both
/// destructure styles resolve identically to the compiled binary (pinned by
/// tuple-views structural 3-way: a flat 3-tuple made structural proj 0
/// returned leaf a instead of (a,b) -- BL8003 downstream).
and private forceTreeShaped (st: InterpState) (env: Env) (e: IRExpr) : Value =
    match e with
    | IRParallel (l, r, _) | IRFusion (l, r) ->
        VTuple [| forceTreeShaped st env l; forceTreeShaped st env r |]
    | IRVar (id, _) ->
        (match envTryFind env id with
         | Some cell ->
             (match cell.V with
              | VDeferred (e2, env2) -> forceTreeShaped st env2 e2
              | v -> v)                      // already-forced (memoized) leaf
         | None -> forceLeaf st env e)
    | leaf -> forceLeaf st env leaf

and private forceLeaf (st: InterpState) (env: Env) (leaf: IRExpr) : Value =
    match leaf with
    | IRApplyCombinator info -> materializeApply st env info []
    | _ -> forceExpr st env leaf

// ----------------------------------------------------------------------------
// Choice `<|>` (genChoiceBinding 8997 + choice force 7788): materialize both
// sides, elementwise `(lhs != 0) ? lhs : rhs`. Scalar sides use the same rule.
// ----------------------------------------------------------------------------
and private forceChoice (st: InterpState) (env: Env) (left: IRExpr) (right: IRExpr) (wrappers: IRExpr list) : Value =
    let wrapSide s = wrappers |> List.fold (fun acc w -> IRFunctorMap (w, acc)) s
    let lv = forceExpr st env (wrapSide left)
    let rv = forceExpr st env (wrapSide right)
    match lv, rv with
    | VArray la, VArray ra -> VArray (choiceArray la ra)
    | _ -> if isNonZero lv then lv else rv

and private choiceArray (la: BladeArray) (ra: BladeArray) : BladeArray =
    let out = A.allocDense la.ElemType la.IndexTypes la.Extents
    let rank = la.Extents.Length
    let rec walk (level: int) (acc: int64 list) =
        if level = rank then
            let coords = List.rev acc
            let lval = A.readCell la coords
            A.writeCell out coords (if isNonZero lval then lval else A.readCell ra coords)
        else
            for i in 0L .. la.Extents.[level] - 1L do walk (level + 1) (i :: acc)
    walk 0 []
    out

// ----------------------------------------------------------------------------
// Fallback `<|:>` (genFallbackBinding always-defers + genFallbackMaterialize
// 9368). ALLOCATION-keyed, unlike value-keyed `<|>`. Two regimes on the LEFT
// operand type: dense-left = fallback_copy (A fully allocated, so
// `A <|:> B = A`); compound-left = SQL sparse overlay (M2.7 compound-index
// representation, gated like compound-halo / apply-over-compound). RIGHT is
// unused (and NOT forced) in the dense-left regime -- arrays are pure, so
// forcing it could spuriously raise for an otherwise-fine program.
// ----------------------------------------------------------------------------
and private forceFallback (st: InterpState) (env: Env) (a: IRExpr) (b: IRExpr) (wrappers: IRExpr list) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over a fallback (<$> over <|:> is steered to error in CodeGen)")
    match forceExpr st env a with
    | VArray la ->
        VArray (A.fallbackDense la)
    | VCompound cvS ->
        // Compound-left `S <|:> D`: the SQL sparse overlay. Here the RIGHT
        // operand IS needed (D fills the absent leading cells), so force it --
        // it resolves to a plain dense array (a nested `S2 <|:> D` inner already
        // forced to dense).
        (match forceExpr st env b with
         | VArray d -> VArray (A.fallbackCompoundLeft cvS d)
         | _ -> raise (InterpUnsupported "compound-left <|:>: right operand did not force to a dense array"))
    | _ ->
        raise (InterpUnsupported "apply over ragged/grouped/compound input (M2.7)")

// ----------------------------------------------------------------------------
// Guard `guard(cond, comp)` (genGuardBinding 8862): fold the predicate into the
// kernel body (lambda args -> cond ? body : 0) via a synthetic callable, then
// materialize -- an allocated array filled with zeros where the guard is false.
// ----------------------------------------------------------------------------
and private forceGuard (st: InterpState) (env: Env) (cond: IRExpr) (body: IRExpr) (wrappers: IRExpr list) : Value =
    let (resolved, innerWrappers, renv) = resolveComp st env body []
    let allWrappers = wrappers @ innerWrappers
    match resolved with
    | IRApplyCombinator info ->
        let zeroForReturnType (retTy: IRType) =
            match retTy with
            | IRTScalar ETBool -> IRLit (IRLitBool false)
            | IRTScalar ETInt64 | IRTScalar ETInt32 -> IRLit (IRLitInt 0L)
            | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> IRLit (IRLitInt 0L)
            | IRTScalar ETFloat32 -> IRLit (IRLitFloat32 0.0f)
            | _ -> IRLit (IRLitFloat 0.0)
        let buildGuarded (c: IRCallable) : IRExpr =
            let synthetic = { c with Id = st.Builder.FreshId(); Body = IRIf (cond, c.Body, zeroForReturnType c.RetType) }
            registerSyntheticCallable synthetic
        let wrappedKernel = mapKernelInner buildGuarded info.Kernel
        materializeApply st renv { info with Kernel = wrappedKernel } allWrappers
    | IRParallel _ | IRFusion _ ->
        raise (InterpUnsupported "guard over a parallel/fusion computation")
    | _ ->
        // guard over a CONCRETE array / scalar (or choice/sequence) body: the
        // predicate is a scalar here (it cannot reference per-cell values without a
        // kernel), so evaluate it once: true -> the (wrapper-applied) materialized
        // body, false -> a zero array/scalar of the same shape. Mirrors CodeGen's
        // non-apply guard materialization.
        let bodyVal = applyWrappersToValue st renv allWrappers (forceExpr st renv resolved)
        if isNonZero (Core.evalExpr st env cond) then bodyVal
        else
            match bodyVal with
            | VArray a -> VArray (A.allocDense a.ElemType a.IndexTypes a.Extents)
            | VFloat _ -> VFloat 0.0
            | VFloat32 _ -> VFloat32 0.0f
            | VInt _ -> VInt 0L
            | VInt32 _ -> VInt32 0
            | VComplex _ -> VComplex (0.0, 0.0)
            | VBool _ -> VBool false
            | other -> other

// ----------------------------------------------------------------------------
// Sequence (genSequenceBinding 7928): n children of same shape stacked into a
// rank-added array [N, child_extents...]; `out[i] = child_i`.
// ----------------------------------------------------------------------------
and private forceSequence (st: InterpState) (env: Env) (elems: IRExpr list) (wrappers: IRExpr list) : Value =
    let wrap s = wrappers |> List.fold (fun acc w -> IRFunctorMap (w, acc)) s
    let childVals = elems |> List.map (fun e -> forceExpr st env (wrap e))
    match childVals with
    | (VArray first) :: _ when childVals |> List.forall _.IsVArray ->
        // Stack child rows into a rank-added array [N, child_extents...]. Built
        // as an SNested record directly (mkDenseArray reshapes a FLAT store; the
        // rows are already nested stores). Printing keys off the binding type, so
        // the extra outer Idx<N> need not be reflected in IndexTypes here.
        let rows = childVals |> List.map (function VArray a -> a.Data | _ -> failwith "unreachable") |> Array.ofList
        let outExtents = Array.append [| int64 elems.Length |] first.Extents
        VArray { ElemType = first.ElemType; IndexTypes = first.IndexTypes; Extents = outExtents; Data = SNested rows }
    | _ ->
        // Scalar children: rank-1 array of the child values (via storeOfValues).
        let et = match childVals with (VFloat _) :: _ -> ETFloat64 | (VInt _) :: _ -> ETInt64 | _ -> ETFloat64
        VArray (A.mkDenseArray (IRTScalar et) [] [| int64 elems.Length |] (A.storeOfValues (IRTScalar et) (Array.ofList childVals)))

// ----------------------------------------------------------------------------
// Bind (genBindChainBinding / IRBind force 7724): materialize comp as s1, bind
// the continuation's parameter to s1's value, force the continuation body.
// ----------------------------------------------------------------------------
and private forceBind (st: InterpState) (env: Env) (comp: IRExpr) (cont: IRExpr) : Value =
    let s1 = forceExpr st env comp
    match resolveCallable (resolveContRef st env cont) with
    | Some lInfo when lInfo.Params.Length >= 1 ->
        let benv = envChild env
        envBind benv lInfo.Params.[0].VarId s1 |> ignore
        for c in lInfo.Captures do
            match envTryFind env c.Id with Some cell -> envBindRef benv c.Id cell | None -> ()
        // The continuation body is itself a computation (IRCompute-wrapped).
        forceExpr st benv lInfo.Body
    | _ -> raise (InterpUnsupported "bind continuation does not resolve to a callable")

and private resolveContRef (st: InterpState) (env: Env) (e: IRExpr) : IRExpr =
    match e with
    | IRVar (id, _) ->
        match envTryFind env id with
        | Some cell -> (match cell.V with VDeferred (e2, env2) -> resolveContRef st env2 e2 | _ -> e)
        | None -> e
    | _ -> e

// ----------------------------------------------------------------------------
// Method composition @>> (IRComposeMeth force 7662): materialize left as s1,
// then apply right's kernel elementwise over s1.
// ----------------------------------------------------------------------------
and private forceComposeMeth (st: InterpState) (env: Env) (left: IRExpr) (right: IRExpr) : Value =
    let s1 = forceExpr st env left
    match s1 with
    | VArray a ->
        match extractInlinableKernel st env right with
        | Some kernelRef ->
            match resolveKernel kernelRef with
            | Some rk when rk.Callable.Params.Length = 1 ->
                let callable = rk.Callable
                // Captures from the SITE env, exactly as resolveUnaryKernel
                // binds them: `callCallable` passes none and `evalCall` chains
                // only to module-global, so an `@>>` right kernel that reads a
                // binding of the enclosing FUNCTION frame would otherwise throw
                // on the unbound id while the compiled lane forwards it.
                let caps =
                    callable.Captures
                    |> List.choose (fun c -> envTryFind env c.Id |> Option.map (fun cell -> (c.Id, cell)))
                    |> Map.ofList
                let out = A.allocDense a.ElemType a.IndexTypes a.Extents
                let rank = a.Extents.Length
                let rec walk (level: int) (acc: int64 list) =
                    if level = rank then
                        let coords = List.rev acc
                        A.writeCell out coords (Core.evalCall st callable caps [ A.readCell a coords ])
                    else
                        for i in 0L .. a.Extents.[level] - 1L do walk (level + 1) (i :: acc)
                walk 0 []
                VArray out
            | _ -> raise (InterpUnsupported "compose-meth right kernel arity != 1")
        | None -> raise (InterpUnsupported "compose-meth right kernel not resolvable")
    | _ -> raise (InterpUnsupported "compose-meth left side is not an array")

// ----------------------------------------------------------------------------
// Compose-object apply `(object_for(k1) >>@ object_for(k2)) <@> A`
// (IRComposeApply; genComposeApply, CodeGen.fs:6653): the SLOT-INVERTED apply.
// Two SEPARATE elementwise stages over the (single, rank-1 in corpus) input:
//   s1[i] = k1(A[i]);   out[i] = k2(s1[i])   so   out[i] = k2(k1(A[i])).
// CodeGen allocates BOTH s1 and the output with the INPUT array's element type
// (not the kernels' return types), so writeCell's coercion reproduces the
// compiled store exactly. Composition resolves through deferred vars to
// IRComposeObj; each object's kernel is IRObjectFor.Kernel (or a bare kernel
// expr). Returns a plain VArray so forceTreeShaped wraps a parallel/fusion
// leaf correctly (046/047).
//
// `wrappers` are trailing functor-map / @>>-extracted kernels resolveComp
// collected around this node (innermost-first): `p @>> q` reaches here with
// q's kernel as ONE wrapper (loops/048), applied as a final elementwise stage
// `out[i] = wrapAll(k2(k1(A[i])))`, folding left-to-right like
// applyFunctorWrappers.
// ----------------------------------------------------------------------------
and private materializeComposeApply (st: InterpState) (env: Env) (cinfo: ComposeApplyInfo) (wrappers: IRExpr list) : Value =
    let rec resolveDef (e: IRExpr) (en: Env) : IRExpr * Env =
        match e with
        | IRVar (id, _) ->
            match envTryFind en id with
            | Some cell ->
                match cell.V with
                | VDeferred (e2, en2) -> resolveDef e2 en2
                // A let-bound object (`let o = object_for(f)`) is a VLoopObj, not
                // a VDeferred; unwrap to its IRObjectFor provenance so kernelOf can
                // reach `.Kernel`. Mirrors the codegen ObjectLoopBindings chase.
                | VLoopObj lo -> resolveDef lo.Provenance lo.Captured
                | _ -> (e, en)
            | None -> (e, en)
        | _ -> (e, en)
    let (comp, cenv) = resolveDef cinfo.Composition env
    match comp with
    | IRComposeObj (o1, o2) ->
        let kernelOf (o: IRExpr) : IRExpr =
            let (ro, _) = resolveDef o cenv
            match ro with IRObjectFor lo -> lo.Kernel | _ -> ro
        let call1 = resolveUnaryKernel st cenv (kernelOf o1)
        let call2 = resolveUnaryKernel st cenv (kernelOf o2)
        // A wrapper is a unary transform; an extracted IRCompose(k,f) means
        // f.k (applyValue's compose convention). Fold all wrappers innermost-
        // first onto stage 2's result.
        let rec wrapperFn (w: IRExpr) : (Value -> Value) =
            match w with
            | IRCompose (k, f) -> let kf = wrapperFn k in let ff = wrapperFn f in (fun v -> ff (kf v))
            | _ -> resolveUnaryKernel st cenv w
        let wrapAll = wrappers |> List.fold (fun acc w -> let wf = wrapperFn w in (fun v -> wf (acc v))) id
        let call2Wrapped v = wrapAll (call2 v)
        match cinfo.InputArrays with
        | [ arrExpr ] ->
            let a = forceInputArray st env arrExpr
            let rank = a.Extents.Length
            let rec walk (src: BladeArray) (dst: BladeArray) (call: Value -> Value) (level: int) (acc: int64 list) =
                if level = rank then
                    let coords = List.rev acc
                    A.writeCell dst coords (call (A.readCell src coords))
                else
                    for i in 0L .. src.Extents.[level] - 1L do walk src dst call (level + 1) (i :: acc)
            // Stage 1 then stage 2, each its own pass (matching CodeGen's two
            // loops). Each store carries ITS STAGE'S result type -- the twin
            // of genComposeApply's `stageElemOf`: an Int64 input through
            // `half(Int64) -> Float64` used to allocate the intermediate as
            // Int64 and truncate every cell (`[0, 2, 2]` for `[1, 2, 3]`).
            // A stage that does not resolve, or returns an array, keeps the
            // element type flowing into it, as the compiled lane does.
            let stageElemOf (k: IRExpr) (fallback: IRType) : IRType =
                match resolveKernel k with
                | Some rk ->
                    let ret =
                        match rk.Callable.RetType with
                        | IRTInfer _ -> typeOf rk.Callable.Body
                        | t -> t
                    (match ret with
                     | ArrayElem _ | IRTUnit | IRTInfer _ -> fallback
                     | t -> t)
                | None -> fallback
            let s1Elem = stageElemOf (kernelOf o1) a.ElemType
            let s2Elem = stageElemOf (kernelOf o2) s1Elem
            // The trailing wrappers run after stage 2 into the same store.
            let outElem =
                wrappers |> List.fold (fun acc w ->
                    match w with
                    | IRCompose _ -> acc
                    | _ -> stageElemOf w acc) s2Elem
            let s1 = A.allocDense s1Elem a.IndexTypes a.Extents
            walk a s1 call1 0 []
            let out = A.allocDense outElem a.IndexTypes a.Extents
            walk s1 out call2Wrapped 0 []
            VArray out
        | _ -> raise (InterpUnsupported "compose-apply with multiple input arrays (M2.3)")
    | _ -> raise (InterpUnsupported "IRComposeApply: composition did not resolve to IRComposeObj")

/// Force an eager-op / compose-apply INPUT expr to a concrete BladeArray,
/// memoizing a deferred IRVar cell (resolveArraySource's double-consumer rule,
/// 0.3) so a second consumer of the same binding sees the already-materialized
/// array -- the value-space twin of forceDeferredArrayInput. A bare virtual
/// source fed directly to an eager op is not in the corpus (CodeGen's eager ops
/// index a materialized `.extents[0]`), so it gates.
and private forceInputArray (st: InterpState) (env: Env) (arrExpr: IRExpr) : BladeArray =
    match resolveArraySource st env arrExpr with
    | SReal a -> a
    | SVirtual -> raise (InterpUnsupported "eager op over a bare virtual source (materialize first)")

/// Resolve a sort key / mask predicate / compose-apply stage expr to a unary
/// Value->Value closure via resolveKernel (peels Reynolds, resolves through the
/// callables + synthetic table). Captures are bound from the site env, the same
/// way `resolveBinaryFold` and `forceComposeMeth` bind theirs; module-level
/// kernels additionally reach theirs via st.Global.
and private resolveUnaryKernel (st: InterpState) (env: Env) (kernel: IRExpr) : (Value -> Value) =
    match resolveKernel kernel with
    | Some rk when rk.Callable.Params.Length = 1 ->
        // Bind the kernel's declared captures from the SITE env, for exactly
        // the reason materializeObjectForApp does: `callCallable` passes no
        // captures and `evalCall` chains to the module-global scope only, so
        // a capture bound in an enclosing FUNCTION frame is invisible. A sort
        // key that reads the array being keyed on -- `sort(idxs, lambda(i: I)
        // -> a(i))`, the AD permutation route -- captures `a` that way, and
        // without this the key throws mid-comparison and the failure surfaces
        // as List.sortWith's opaque "failed to compare two elements".
        let caps =
            rk.Callable.Captures
            |> List.choose (fun c -> envTryFind env c.Id |> Option.map (fun cell -> (c.Id, cell)))
            |> Map.ofList
        (fun v -> Core.evalCall st rk.Callable caps [ v ])
    | _ -> raise (InterpUnsupported "sort/mask kernel does not resolve to a unary callable")

/// Inline object_for application: `A [op] B` (bracketed OUTER product) and its
/// elementwise / single-array-broadcast siblings. Mirrors genObjectForApplication
/// (CodeGen.fs:6537): force each input array, resolve the (1- or 2-param) kernel
/// callable, allocate a fresh DENSE output of the carried type, and fill by
/// invoking the kernel per output cell. `objInfo.InputRanks` selects the shape
/// ([1;1] = outer m x p, [0;0] = elementwise m, [0] = single-array map). (The
/// two-array ELEMENTWISE binop `A + B` is re-synthesized by the checker as
/// `compute(zip <@> lambda)` and never reaches here.) The kernel's return type
/// IS the output element type (a comparison `[<]` produces bool), carried by
/// the output array type, so writeCell coerces the VBool cell.
and private materializeObjectForApp
        (st: InterpState) (env: Env) (outType: IRType) (objInfo: ObjectForInfo) (arrays: IRExpr list) : Value =
    let arrs = arrays |> List.map (forceInputArray st env)
    let kernel =
        match resolveKernel objInfo.Kernel with
        | Some rk -> rk.Callable
        | None -> raise (InterpUnsupported "object_for application: kernel does not resolve to a callable")
    // Bind the kernel's declared captures from the SITE env (makeClosure's
    // snapshot, done here because the kernel arrives as a raw callable, not a
    // closure value). evalCall chains frames to the module-global scope only,
    // so a capture bound in an enclosing FUNCTION frame -- e.g. the hoisted
    // scalar of a broadcast (`a - mymean(a)`) inside a function body -- is
    // invisible without this; the compiled backend forwards the same capture
    // via captureForwardName.
    let kernelCaptures =
        kernel.Captures
        |> List.choose (fun c ->
            match envTryFind env c.Id with
            | Some cell -> Some (c.Id, cell)
            | None -> None)
        |> Map.ofList
    let call (vs: Value list) : Value = Core.evalCall st kernel kernelCaptures vs
    // Output element type + dense index slots. genObjectForApplication derives
    // the element type from the KERNEL's return type (a comparison/logical op
    // produces bool) and IGNORES the IRApp's carried result type, which for
    // bool-returning OUTER forms the checker collapses to a bare scalar (the
    // compiled binary then prints such a binding's raw Array data POINTER,
    // masked to 0xPTR; the interp still materializes the true bool array so a
    // pointer-aware Print emitter can render the matching token). Prefer the
    // carried array type when present; else rebuild dense slots from the inputs.
    let outElem, srcIdxTys =
        match outType with
        | ArrayElem outArr -> outArr.ElemType, outArr.IndexTypes
        | _ ->
            let firstIdx (arr: BladeArray) = match arr.IndexTypes with ix :: _ -> [ix] | [] -> []
            kernel.RetType, (arrs |> List.collect firstIdx)
    match objInfo.InputRanks, arrs with
    | [1; 1], [ a; b ] ->
        // Outer product: out[i][j] = kernel(A[i], B[j]); dense m x p.
        let m = a.Extents.[0]
        let p = b.Extents.[0]
        let out = A.allocDense outElem srcIdxTys [| m; p |]
        for i in 0L .. m - 1L do
            for j in 0L .. p - 1L do
                A.writeCell out [ i; j ] (call [ A.peelDim a i; A.peelDim b j ])
        VArray out
    | [0; 0], [ a; b ] ->
        // Elementwise: out[i] = kernel(A[i], B[i]); dense m.
        let m = a.Extents.[0]
        let out = A.allocDense outElem srcIdxTys [| m |]
        for i in 0L .. m - 1L do
            A.writeCell out [ i ] (call [ A.peelDim a i; A.peelDim b i ])
        VArray out
    | [0], [ a ] ->
        // Single-array broadcast map: out[i] = kernel(A[i]); dense m.
        let m = a.Extents.[0]
        let out = A.allocDense outElem srcIdxTys [| m |]
        for i in 0L .. m - 1L do
            A.writeCell out [ i ] (call [ A.peelDim a i ])
        VArray out
    | _ -> raise (InterpUnsupported "object_for application: unsupported input-rank configuration")

// materializeApply: the standard dense/co-iteration path (genApplyCombinator).

and private materializeApply (st: InterpState) (env: Env) (info0: ApplyInfo) (wrappers: IRExpr list) : Value =
    match tryCompoundHaloMap info0 with
    | Some (maskExpr, leadRank, tag) -> materializeCompoundHaloMap st env info0 wrappers maskExpr leadRank tag
    | None ->
    match tryCompoundRangeMap info0 with
    | Some (maskExpr, leadRank) -> materializeCompoundRangeMap st env info0 wrappers maskExpr leadRank
    | None ->
    match trySparseRangeMap info0 with
    | Some (src, leadRank) -> materializeSparseRangeMap st env info0 wrappers src leadRank
    | None ->
    if tryRowPeelMap info0 then materializeRowPeelMap st env info0 wrappers
    else
    match tryWreathApply info0 with
    | Some tie -> materializeWreathApply st env info0 wrappers tie
    | None ->
    gateInputs info0
    let info = applyFunctorWrappers st info0 wrappers
    let arrayNames = info.Arrays |> List.mapi (fun i _ -> $"a{i}")
    let cg = buildLoopNestCodeGen info arrayNames "out" st.Builder
    // HALO-EXTENT RUNTIME GUARD (BL8009) -- the interpreter twin of
    // genApplyCombinator's haloExtentGuards, checked ONCE before the nest.
    // A dense halo slot's declared inner extent is a literal; any array read
    // through the window (`IRBinOp(IRAdd, w, off)` with `w` typed by the
    // "__halowin|d:" tag) must have exactly that extent on the indexed slot.
    // Unresolvable targets are skipped (fail-open), matching the compiled
    // side's not-in-VarNames rule.
    (let haloDecl =
        info.Arrays
        |> List.collect (fun a -> match a with IRRange (ixs, _) -> ixs | _ -> [])
        |> List.choose (fun ix ->
            match ix.Tag with
            | Some tag when tag.StartsWith (haloWinTagPrefix + "d:") ->
                (match ix.Extent, haloAccessOfTag tag with
                 | IRLit (IRLitInt shrunk), Some h -> Some (tag, snd (haloDemand h (0L, shrunk)))
                 | _ -> None)
            | _ -> None)
        // Same-tag ambiguity rule as the compiled guard: equal-offset anonymous
        // halos with different extents collide on one tag -- drop it.
        |> List.groupBy fst
        |> List.choose (fun (tag, entries) ->
            match entries |> List.map snd |> List.distinct with
            | [ n ] -> Some (tag, n)
            | _ -> None)
        |> Map.ofList
     if not (Map.isEmpty haloDecl) then
        // The same shared scan the compiled guard consumes
        // (IRAccess.windowReadsOf): one matcher, two callers.
        for r in Blade.IRAccess.windowReadsOf (fun wv -> (Blade.IRAccess.denseHaloTagOf wv).IsSome) cg.KernelExpr do
            match Blade.IRAccess.denseHaloTagOf r.Window |> Option.bind (fun t -> Map.tryFind t haloDecl) with
            | Some declared ->
                (match envTryFind env r.ArrayId with
                 | Some cell ->
                     (match force st env cell.V with
                      | VArray a when r.Dim < a.Extents.Length && a.Extents.[r.Dim] <> declared ->
                          raise (InterpPanic ("BL8009", "halo extent mismatch", None, 0))
                      | _ -> ())
                 | None -> ())
            | None -> ())
    // Symmetric/antisymmetric/Hermitian output storage (compact) and Reynolds
    // kernels (permutation sum) are interpreted -- see the ArrayElem arm's
    // compact allocation and interpretNest's Reynolds path. Fused-joint output
    // levels (one loop level spanning d plain-dense source dims -- joint
    // symmetry over the compound axis) are materialized by interpretNest's
    // fused-peel arm (per-dim coordinate decode, mirrors CodeGen.fs:3087).
    // Resolve input array values by position.
    let inputs = Dictionary<int, ArraySource>()
    info.Arrays |> List.iteri (fun i arr -> inputs.[i] <- resolveArraySource st env arr)
    let realAt (pos: int) =
        match inputs.TryGetValue pos with
        | true, SReal a -> a
        | _ -> raise (InterpUnsupported $"expected a materialized array at position {pos}")
    // Loop-level extent (mirror genLoopBoundExpr's EXTENT, pre-subtraction). A
    // fused-joint level's extent is the PRODUCT of the array's first d plain-dense
    // extents (the compound-axis cardinality), not a single dim.
    let levelExtent (b: LoopIndexBinding) : int64 =
        match b.FusedRank with
        | Some d ->
            let pos = match b.Elements with e :: _ -> e.ArrayPosition | [] -> 0
            let a = realAt pos
            [ 0 .. d - 1 ] |> List.fold (fun acc j -> acc * a.Extents.[j]) 1L
        | None ->
        match b.Extent with
        | IRLit (IRLitInt n) -> n
        | IRCompoundMask _ -> raise (InterpUnsupported "compound-index loop level (M2.7)")
        | _ ->
            let pos = match b.Elements with e :: _ -> e.ArrayPosition | [] -> 0
            match inputs.TryGetValue pos with
            | true, SReal a -> a.Extents.[b.ExtentDimRef]
            | _ -> toI64 (Core.evalExpr st env b.Extent)
    // CO-ITERATION EXTENT GUARD (BL8011) -- the interpreter twin of
    // genApplyCombinator's guard, checked ONCE before the nest: every REAL
    // operand peeled at a co-iteration level must have the level's extent
    // (the first operand's) on the axis it is peeled along, or the walk
    // reads the shorter one past its end.
    for b in cg.Bindings do
        match b.FusedRank, b.Elements with
        | None, e0 :: rest when not rest.IsEmpty && (match e0.Virtual with RealArray -> true | _ -> false) ->
            let bound = levelExtent b
            for e in rest do
                match e.Virtual, inputs.TryGetValue e.ArrayPosition with
                | RealArray, (true, SReal a) when a.Extents.[e.DimIndex] <> bound ->
                    raise (InterpPanic ("BL8011", "co-iteration extent mismatch", None, 0))
                | _ -> ()
        | _ -> ()
    match cg.OutputType with
    | IRTScalar et ->
        let acc = { V = zeroOfElem et }
        interpretNest st env cg inputs realAt levelExtent (OutFold (acc, (fun a b -> N.evalBinOp IRAdd a b)))
        acc.V
    | ArrayElem arr ->
        // Outer extents come from the loop bindings; when the kernel returns an
        // ARRAY the output type carries its T-dimensions after them (stage S3,
        // manifestation M-C), so the allocation needs those trailing extents too
        // -- the interpreter's version of the short extents table that made the
        // compiled lane print `[[], []]`.
        // COUNTED BY RANK, NOT BY ENTRY -- the twin of the same correction in
        // CodeGen's extents table. A comm-licensed application over one identity
        // group gives the output a COMPOUND leading index (`SymIdx<2, I>` is ONE
        // entry of Rank 2), so comparing `IndexTypes.Length` against the loop-
        // level count made a rank-3 compact output look like it had no trailing
        // T-dimension at all: the allocation came out one axis short and the row
        // write ran off the end of the pool. Flat rank on both sides, and the
        // trailing ENTRIES peeled off the end until their ranks account for the
        // missing dims. Identical to the old `List.skip` on an all-rank-1
        // output, so the dense path is untouched.
        let outerExtents = cg.Bindings |> List.map levelExtent
        let trailingExtents =
            let flatRank = arr.IndexTypes |> List.sumBy _.Rank
            let missing = flatRank - outerExtents.Length
            if missing <= 0 then []
            else
                let rec peel (acc: IRIndexType list) (taken: int)
                             (remaining: IRIndexType list) : IRIndexType list option =
                    if taken = missing then Some acc
                    elif taken > missing then None
                    else
                        match remaining with
                        | [] -> None
                        | ix :: rest -> peel (ix :: acc) (taken + ix.Rank) rest
                match peel [] 0 (List.rev arr.IndexTypes) with
                | Some tDimEntries ->
                    tDimEntries |> List.map (fun (ix: IRIndexType) ->
                        toI64 (Core.evalExpr st env ix.Extent))
                // Boundary falls INSIDE a compound index: not a shape this
                // describes, so leave the allocation as it was.
                | None -> []
        let rowShaped = not (List.isEmpty trailingExtents)
        let extents = (outerExtents @ trailingExtents) |> Array.ofList
        st.Cells <- st.Cells + (extents |> Array.fold (*) 1L)
        // Compact storage iff the OUTPUT index type carries a real symmetry
        // group (adjacent-equal storage group). buildSymmVecWithStrict groups
        // sym/herm/antisym together, so hasRealSymmetry on ITS vec detects ALL
        // three compact classes; the strict vec drives the antisym diagonal drop.
        let (osym, ostrict) = buildSymmVecWithStrict cg.OutputType
        let outArr =
            if hasRealSymmetry osym then
                A.allocCompact arr.ElemType arr.IndexTypes extents (Array.ofList osym) (Array.ofList ostrict)
            else
                A.allocDense arr.ElemType arr.IndexTypes extents
        interpretNest st env cg inputs realAt levelExtent
            (if rowShaped then OutArrayRows outArr else OutArray outArr)
        VArray outArr
    | other -> raise (InterpUnsupported $"apply output type {nodeTypeName other}")

and private nodeTypeName (ty: IRType) : string =
    let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(ty, typeof<IRType>)
    case.Name

// Deduced OrbIdx (iterated-wreath) output: the interpreter's traversal nest
// (docs/plan-orbit-index-types.md 9 step 4; plan-orbidx-bijections.md 2).
//
// The interpreter's twin of CodeGen.genWreathApply, and must produce the same
// cells in the same order -- the corpus harness diffs the two. Both are
// driven by the verified reference emitter rather than a hand-rolled peeled
// nest: C++ instantiates `orb_visit<Levels...>`, this walks
// `OrbRank.visitStream`, and `blade test orbwreath` diffs the two
// cell-for-cell on every run -- two independent hand-written nests is the
// one thing this feature must not have. Like the compiled path, it bypasses
// `buildLoopNestCodeGen` entirely: that builder refuses a wreath input and
// has no concept of segment peeling for the depth-2 shape.

/// Is this application a deduced wreath tie? Same predicate, same arguments as
/// `deduceOutputType`'s and codegen's, so all three agree by construction.
and private tryWreathApply (info: ApplyInfo) : WreathTie option =
    match resolveKernel info.Kernel with
    | Some rk ->
        match deduceWreathTie info.ArrayTypes info.Identities
                              (rk.Callable.CommGroups @ rk.Callable.AntisymGroups)
                              (if info.HasReynolds then [] else rk.Callable.AntisymGroups)
                              info.KernelTDims
                              (info.KernelInputRanks |> List.exists (fun r -> r > 0))
                              info.HasReynolds
                              rk.Callable.SignParities with
        | WreathTied t -> Some t
        | WreathNoTie -> None
        | WreathKernelNotOdd (argPos, _, _) ->
            // Unreachable: the typecheck seam runs the same call with the same
            // arguments (SignParities is the summary it recorded) and refuses
            // the program before interpretation starts. Loud, not a fallback.
            failwith (sprintf "internal: interpreter reached a wreath tie whose kernel is not \
provably sign-odd in tied argument %d; typecheck should have refused this application" argPos)
    | None -> None

/// Read one tied argument at a CANONICAL sub-key of the traversal stream.
/// Depth-1 inner classes go through `readCompact` (the same canonical reader
/// every SymIdx/AntisymIdx read uses, so the value comes from exactly the
/// cell the compiled `arr[c0][c1-c0]` subscript names); a wreath inner class
/// (depth >= 3) goes through `orbRank` on the pool. Neither is the mirrored
/// read: the sub-key is canonical by construction, so nothing folds.
and private wreathArgValue (arr: BladeArray) (innerLevels: (int * bool) list)
                           (n: int64) (subKey: int list) : Value =
    if List.length innerLevels >= 2 then
        A.wreathReadCanonical arr innerLevels n subKey
    else
        A.readCompact arr (subKey |> List.map int64)

and private materializeWreathApply
        (st: InterpState) (env: Env) (info0: ApplyInfo) (wrappers: IRExpr list)
        (tie: WreathTie) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over an OrbIdx (iterated-wreath) application")
    gateInputs info0
    match resolveKernel info0.Kernel with
    | None -> raise (InterpUnsupported "OrbIdx application: the kernel does not resolve to a callable")
    | Some rk ->
    let (elemTy, outIdxTys) =
        match info0.OutputType with
        | ArrayElem arr -> (arr.ElemType, arr.IndexTypes)
        | other -> raise (InterpUnsupported $"OrbIdx application output type {nodeTypeName other}")
    let n =
        match tie.BaseExtent with
        | IRLit (IRLitInt v) -> v
        | _ -> raise (InterpUnsupported "OrbIdx application: a wreath class needs a compile-time extent")
    // Every tied position holds the SAME object (that is what earned the tie),
    // so one resolve serves them all; resolving per position would force a
    // deferred source k times.
    let src =
        match resolveArraySource st env (List.head info0.Arrays) with
        | SReal a -> a
        | _ -> raise (InterpUnsupported "OrbIdx application: the tied argument is not a materialized array")
    let out = A.allocWreath elemTy outIdxTys tie.OutputLevels n
    // Kernel env: captures by reference + one reusable cell per param, exactly
    // as interpretNest builds them.
    let kenv = envChild env
    for c in rk.Callable.Captures do
        match envTryFind env c.Id with Some cell -> envBindRef kenv c.Id cell | None -> ()
    let paramCells = Dictionary<IRId, ValueRef>()
    for p in rk.Callable.Params do
        let cell = { V = VUnit }
        paramCells.[p.VarId] <- cell
        envBindRef kenv p.VarId cell
    let innerAxes = tie.InnerLevels |> List.fold (fun a (r, _) -> a * r) 1
    let k = List.length tie.Positions
    let stream =
        match Blade.OrbRank.visitStreamChecked (orbRankLevels tie.OutputLevels) (int n) with
        | Ok s -> s
        | Error detail ->
            raise (InterpUnsupported ($"OrbIdx{(ppOrbitLevels tie.OutputLevels)} at extent {n}: {detail}"))
    let mutable pos = 0L
    for tuple in stream do
        let t = List.toArray tuple
        for j in 0 .. k - 1 do
            let subKey = [ for a in 0 .. innerAxes - 1 -> t.[j * innerAxes + a] ]
            let v = wreathArgValue src tie.InnerLevels n subKey
            let p = tie.Positions.[j]
            if p < rk.Callable.Params.Length then
                match paramCells.TryGetValue rk.Callable.Params.[p].VarId with
                | true, cell -> cell.V <- v
                | _ -> ()
        A.wreathWriteAt out pos (force st kenv (Core.evalExpr st kenv rk.Callable.Body))
        pos <- pos + 1L
    // The stream length and the (4) fold are INDEPENDENT computations of the
    // same number; pin them against each other, as the emitted C++ pins
    // `orb_cell_count`: a short stream leaves the pool tail zero-initialized
    // (plausible-looking data), and a long one would already have thrown.
    if pos <> int64 (A.wreathCellCount out) then
        failwith $"OrbIdx{(ppOrbitLevels tie.OutputLevels)} at extent {n}: the traversal visited {pos} cells but the class's fold says {(A.wreathCellCount out)}"
    st.Cells <- st.Cells + pos
    VArray out

/// Detect a `method_for(range<CompoundIdx<m>>) <@> kernel` map: the sole input
/// is a range whose sole index type is a compound mask. Returns (maskExpr,
/// leadRank). This is the ONE supported compound-range form (a compound slot
/// cannot mix with other index types -- CodeGen.fs:5971).
and private tryCompoundRangeMap (info: ApplyInfo) : (IRExpr * int) option =
    match info.Arrays with
    | [ IRRange (idxTys, _) ] ->
        (match idxTys with
         // A halo<CompoundIdx<m>, [..]> slot ALSO has IxKCompound + IRCompoundMask
         // but carries a "__halowin|" tag and reads via IRHaloUnhash (window
         // pointer into the table) -- a different peel. Exclude it (it stays a
         // clean skip rather than being mis-driven as a plain coordinate map).
         | [ ix ] when ix.IxKind = IxKCompound
                       && (match ix.Tag with Some t when t.StartsWith "__halowin|" -> false | _ -> true) ->
             (match ix.Extent with IRCompoundMask m -> Some (m, ix.Rank) | _ -> None)
         | _ -> None)
    | _ -> None

/// Materialize a range<CompoundIdx<m>> map to a Compound VALUE: iterate the
/// present cells (lex rank order), binding each kernel param to its tuple
/// COORDINATE via the index's O(1) unhash, storing the kernel result into the
/// compact buffer at that rank (CodeGen.fs:3054-3064 / 3696-3704). The output
/// shares the range's mask/index (same present tuples), trailing_stride 1.
and private materializeCompoundRangeMap
        (st: InterpState) (env: Env) (info0: ApplyInfo) (wrappers: IRExpr list)
        (maskExpr: IRExpr) (leadRank: int) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over a range<CompoundIdx> map")
    let maskArr =
        match force st env (Core.evalExpr st env maskExpr) with
        | VArray a -> a
        | _ -> raise (InterpUnsupported "range<CompoundIdx>: mask is not an array")
    let leadExtents = maskArr.Extents
    let maskBits = A.maskToBits maskArr
    let (table, rankOf, card) = A.buildCompoundIndex leadExtents maskBits
    // Kernel + per-coordinate param plan from the SAME loop-nest builder CodeGen
    // uses (so the kernel body + param identities cannot drift).
    let arrayNames = info0.Arrays |> List.mapi (fun i _ -> $"a{i}")
    let cg = buildLoopNestCodeGen info0 arrayNames "out" st.Builder
    let (elemTy, outIdxTys) =
        match cg.OutputType with
        | ArrayElem arr -> (arr.ElemType, arr.IndexTypes)
        | other -> raise (InterpUnsupported $"range<CompoundIdx> map output type {nodeTypeName other}")
    // Kernel env: captures + reusable param cells (as interpretNest builds them).
    let kenv = envChild env
    bindKernelCaptures st env kenv cg.Captures cg.KernelExpr
    let paramCells = Dictionary<IRId, ValueRef>()
    for p in cg.KernelParams do
        let cell = { V = VUnit }
        paramCells.[p.VarId] <- cell
        envBindRef kenv p.VarId cell
    // (paramVarId, coordinate index rc): the compound level's element bindings,
    // each `int64 <param> = <cidx>->unhash(r)[rc]`.
    let paramCoord =
        cg.Bindings |> List.collect (fun b -> b.Elements |> List.map (fun e -> (e.ParamVarId, e.RankComponent)))
    st.Cells <- st.Cells + card
    let results = Array.create (int card) VUnit
    for r in 0 .. int card - 1 do
        let tuple = table.[r]
        for (pv, rc) in paramCoord do
            match paramCells.TryGetValue pv with
            | true, cell -> cell.V <- VInt tuple.[rc]
            | _ -> ()
        results.[r] <- force st kenv (Core.evalExpr st kenv cg.KernelExpr)
    VCompound
        { ElemType = elemTy
          IndexTypes = outIdxTys
          LeadRank = leadRank
          LeadExtents = leadExtents
          Mask = maskBits
          Table = table
          RankOf = rankOf
          Cardinality = card
          TrailingStride = 1L
          Data = A.storeOfValues elemTy results }

/// Detect a `method_for(range<SparseIdx<keys>>) <@> kernel` map: the sole
/// input is a range whose sole index type is a sparse key set. Returns
/// (keys source, leadRank) -- the sparse twin of tryCompoundRangeMap.
and private trySparseRangeMap (info: ApplyInfo) : (SparseKeysSource * int) option =
    match info.Arrays with
    | [ IRRange (idxTys, _) ] ->
        (match idxTys with
         | [ ix ] when ix.IxKind = IxKSparse ->
             (match ix.Extent with IRSparseKeys src -> Some (src, ix.Rank) | _ -> None)
         | _ -> None)
    | _ -> None

/// Resolve a SparseKeysSource to its concrete key list (given order):
/// SkStatic entries are baked; SkRuntime forces the keys array and unpacks
/// its tuple elements (or bare integers for a rank-1 sparse).
and private resolveSparseKeys (st: InterpState) (env: Env) (src: SparseKeysSource) (leadRank: int) : int64[][] =
    match src with
    | SkStatic entries ->
        entries |> List.map Array.ofList |> Array.ofList
    | SkRuntime keysExpr ->
        let keysArr =
            match force st env (Core.evalExpr st env keysExpr) with
            | VArray a -> a
            | _ -> raise (InterpUnsupported "SparseIdx: keys operand is not an array")
        if keysArr.Extents.Length <> 1 then
            raise (InterpUnsupported "SparseIdx: keys array is not rank 1")
        Array.init (int keysArr.Extents.[0]) (fun i ->
            match A.readCell keysArr [ int64 i ] with
            | VTuple comps -> comps |> Array.map toI64
            | scalar when leadRank = 1 -> [| toI64 scalar |]
            | _ -> raise (InterpUnsupported "SparseIdx: keys array elements are not tuples"))

/// Materialize a range<SparseIdx<keys>> map to a Sparse VALUE: iterate the
/// keys IN GIVEN ORDER, binding each kernel param to its tuple COORDINATE, and
/// store the kernel result into the compact buffer at that rank -- the sparse
/// twin of materializeCompoundRangeMap (same param plan; the only differences
/// are the key source and the absence of a mask/grid).
and private materializeSparseRangeMap
        (st: InterpState) (env: Env) (info0: ApplyInfo) (wrappers: IRExpr list)
        (src: SparseKeysSource) (leadRank: int) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over a range<SparseIdx> map")
    let keys = resolveSparseKeys st env src leadRank
    let (rankOf, card) = A.buildSparseIndex keys
    let arrayNames = info0.Arrays |> List.mapi (fun i _ -> $"a{i}")
    let cg = buildLoopNestCodeGen info0 arrayNames "out" st.Builder
    let (elemTy, outIdxTys) =
        match cg.OutputType with
        | ArrayElem arr -> (arr.ElemType, arr.IndexTypes)
        | other -> raise (InterpUnsupported $"range<SparseIdx> map output type {nodeTypeName other}")
    let kenv = envChild env
    bindKernelCaptures st env kenv cg.Captures cg.KernelExpr
    let paramCells = Dictionary<IRId, ValueRef>()
    for p in cg.KernelParams do
        let cell = { V = VUnit }
        paramCells.[p.VarId] <- cell
        envBindRef kenv p.VarId cell
    let paramCoord =
        cg.Bindings |> List.collect (fun b -> b.Elements |> List.map (fun e -> (e.ParamVarId, e.RankComponent)))
    st.Cells <- st.Cells + card
    let results = Array.create (int card) VUnit
    for r in 0 .. int card - 1 do
        let key = keys.[r]
        for (pv, rc) in paramCoord do
            match paramCells.TryGetValue pv with
            | true, cell -> cell.V <- VInt key.[rc]
            | _ -> ()
        results.[r] <- force st kenv (Core.evalExpr st kenv cg.KernelExpr)
    VSparse
        { ElemType = elemTy
          IndexTypes = outIdxTys
          LeadRank = leadRank
          Keys = keys
          RankOf = rankOf
          Cardinality = card
          TrailingStride = 1L
          Data = A.storeOfValues elemTy results }

/// Detect a `method_for(halo<CompoundIdx<m>, [..]>) <@> lambda(w) -> ...` map:
/// the sole input is a range whose sole index is a compound mask carrying a
/// "__halowin|" tag. Returns (maskExpr, leadRank, haloTag).
and private tryCompoundHaloMap (info: ApplyInfo) : (IRExpr * int * string) option =
    match info.Arrays with
    | [ IRRange (idxTys, _) ] ->
        (match idxTys with
         | [ ix ] when ix.IxKind = IxKCompound ->
             (match ix.Tag, ix.Extent with
              | Some tag, IRCompoundMask m when tag.StartsWith "__halowin|" -> Some (m, ix.Rank, tag)
              | _ -> None)
         | _ -> None)
    | _ -> None

/// Materialize a compound-halo map: the ordinals walk the PRESENT cells, so
/// the window `w` at ordinal c exposes `w(o)` = the COORDINATE of the (c+o)-th
/// present cell (IRHaloUnhash over rank_to_tuple). The loop shrinks by the
/// halo's interior loss (runtime-cardinality bound minus window span,
/// IR.fs:3109-3120); each `w` binds to (coordinate column, center ordinal) so
/// IRHaloUnhash reads col[center+o]. Output: dense rank-1 over the shrunk axis.
and private materializeCompoundHaloMap
        (st: InterpState) (env: Env) (info: ApplyInfo) (wrappers: IRExpr list)
        (maskExpr: IRExpr) (leadRank: int) (tag: string) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over a compound-halo map")
    if leadRank <> 1 then
        raise (InterpUnsupported "compound-halo over a rank>1 mask (rank-1 only)")
    let maskArr =
        match force st env (Core.evalExpr st env maskExpr) with
        | VArray a -> a
        | _ -> raise (InterpUnsupported "compound-halo: mask is not an array")
    let (table, _, card) = A.buildCompoundIndex maskArr.Extents (A.maskToBits maskArr)
    let start = Blade.Types.haloStartOffsetOfTag tag |> Option.defaultValue 0L
    let shrink = Blade.Types.haloShrinkOfTag tag |> Option.defaultValue 0L
    let outLen = card - shrink
    // Coordinate column: coordinate 0 of each present tuple, in rank order.
    let coordCol =
        VArray
            { ElemType = IRTScalar ETInt64
              IndexTypes = []
              Extents = [| card |]
              Data = SInt (Array.init (int card) (fun r -> table.[r].[0])) }
    let arrayNames = info.Arrays |> List.mapi (fun i _ -> $"a{i}")
    let cg = buildLoopNestCodeGen info arrayNames "out" st.Builder
    let elemTy =
        match cg.OutputType with
        | ArrayElem arr -> arr.ElemType
        | IRTScalar et -> IRTScalar et
        | other -> raise (InterpUnsupported $"compound-halo map output type {nodeTypeName other}")
    let param =
        match cg.KernelParams with
        | [ p ] -> p
        | _ -> raise (InterpUnsupported "compound-halo map: kernel is not single-parameter")
    let kenv = envChild env
    bindKernelCaptures st env kenv cg.Captures cg.KernelExpr
    let cell = { V = VUnit }
    envBindRef kenv param.VarId cell
    st.Cells <- st.Cells + (if outLen > 0L then outLen else 0L)
    let n = if outLen > 0L then int outLen else 0
    let results =
        Array.init n (fun c ->
            cell.V <- VTuple [| coordCol; VInt (int64 c + start) |]
            force st kenv (Core.evalExpr st kenv cg.KernelExpr))
    VArray (A.mkDenseArray elemTy [] [| int64 n |] (A.storeOfValues elemTy results))

/// Detect a `method_for(x) <@> lambda(g) -> ...` ROW-PEEL map: the sole input
/// is a group_by result (first index IxKGroupOuter) or a ragged/DepIdx array
/// (some non-first index is ragged-family or IxKDepInner). Deliberately the
/// SAME predicate CodeGen's `tryRaggedPeel` gates on, so the two lanes agree
/// about which applies take the peel; the kernel receives each row as `g`.
and private tryRowPeelMap (info: ApplyInfo) : bool =
    match info.ArrayTypes with
    | [ at ] ->
        let groupedOuter =
            match at.IndexTypes with h :: _ -> h.IxKind = IxKGroupOuter | [] -> false
        let raggedInner =
            at.IndexTypes.Length >= 2 &&
            at.IndexTypes |> List.skip 1 |> List.exists (fun ix ->
                isRaggedFamilyKind ix.IxKind || ix.IxKind = IxKDepInner)
        groupedOuter || raggedInner
    | _ -> false

/// Materialize a ROW-PEEL map (CodeGen's `tryRaggedPeel` in value space):
/// iterate the outer axis, bind the kernel's single param, and build the
/// output in whichever of the three shapes the OUTPUT TYPE calls for --
/// keyed on the output exactly as the emitter is, so the two lanes cannot
/// disagree about which shape an apply has:
///
///   * ELEMENTWISE (output keeps a ragged-family inner slot): the param binds
///     each ELEMENT and the result is shape-preserving ragged.
///   * ARRAY-VALUED ROWS (output rank >= 2, dense trailing): the param binds
///     the ROW and the kernel hands back a whole dense row.
///   * CONSUMING (output rank 1): the param binds the ROW and the kernel
///     collapses it to a scalar.
///
/// The input is a group_by result or a ragged/DepIdx array; `A.peelDim`
/// serves both from one SRagged store (per-row `lens`, not the parent's
/// placeholder inner extent), so `g(0)`, `reduce(g, +)` and `extents(g)` in
/// the body all see an ordinary rank-1 row.
and private materializeRowPeelMap (st: InterpState) (env: Env) (info: ApplyInfo) (wrappers: IRExpr list) : Value =
    if not (List.isEmpty wrappers) then
        raise (InterpUnsupported "functor-map wrapper over a row-peel map")
    let grouped =
        match resolveArraySource st env info.Arrays.[0] with
        | SReal a -> a
        | _ -> raise (InterpUnsupported "row-peel map: input is not a materialized array")
    let arrayNames = info.Arrays |> List.mapi (fun i _ -> $"a{i}")
    let cg = buildLoopNestCodeGen info arrayNames "out" st.Builder
    let elemTy =
        match cg.OutputType with
        | ArrayElem arr -> arr.ElemType
        | IRTScalar et -> IRTScalar et
        | other -> raise (InterpUnsupported $"row-peel map output type {nodeTypeName other}")
    let param =
        match cg.KernelParams with
        | [ p ] -> p
        | _ -> raise (InterpUnsupported "row-peel map: kernel is not single-parameter")
    let kenv = envChild env
    bindKernelCaptures st env kenv cg.Captures cg.KernelExpr
    let cell = { V = VUnit }
    envBindRef kenv param.VarId cell
    let ngroups = int grouped.Extents.[0]
    let outIdxTys =
        match cg.OutputType with
        | ArrayElem arr -> arr.IndexTypes
        | _ -> []
    let outputIsRaggedShaped =
        outIdxTys.Length >= 2 &&
        (outIdxTys |> List.skip 1 |> List.exists (fun ix ->
            isRaggedFamilyKind ix.IxKind || ix.IxKind = IxKDepInner))
    let denseTrailingRows =
        not outputIsRaggedShaped &&
        outIdxTys.Length >= 2 &&
        (outIdxTys |> List.skip 1 |> List.forall (fun ix -> ix.IxKind = IxKPlain))
    if outputIsRaggedShaped then
        // ELEMENTWISE over a ragged array: shape-preserving. The param binds
        // each ELEMENT (not the row), and the result keeps the input's row
        // lengths -- the value-space twin of the emitter's fresh pool over
        // the parent's shared extents/lens/offsets.
        //
        // Grouped inputs never reach here: codegen refuses an elementwise map
        // over a group_by result outright (its output would need a grouped
        // type nothing downstream resolves lengths for), so such a program is
        // rejected before either lane runs it.
        let (rowStores, lens, offsets) =
            match grouped.Data with
            | SRagged (rows, lens, offs) -> (rows, lens, offs)
            | _ -> raise (InterpUnsupported "elementwise map over a ragged array: input is not a CSR ragged store")
        st.Cells <- st.Cells + (lens |> Array.fold (+) 0L)
        let outRows =
            rowStores
            |> Array.mapi (fun g rowStore ->
                // `lens` is the authority on a row's length (the parent's
                // inner extent is the max-row placeholder), same rule
                // A.peelDim follows.
                let rlen = if g < lens.Length then lens.[g] else 0L
                let rowArr =
                    { ElemType = grouped.ElemType
                      IndexTypes = (match grouped.IndexTypes with _ :: t -> t | [] -> [])
                      Extents = [| rlen |]
                      Data = rowStore }
                let vals =
                    Array.init (int rlen) (fun k ->
                        cell.V <- A.readCell rowArr [ int64 k ]
                        force st kenv (Core.evalExpr st kenv cg.KernelExpr))
                A.storeOfValues elemTy vals)
        VArray { ElemType = elemTy
                 IndexTypes = outIdxTys
                 Extents = Array.copy grouped.Extents
                 Data = SRagged (outRows, Array.copy lens, Array.copy offsets) }
    elif denseTrailingRows then
        let rows =
            Array.init ngroups (fun g ->
                cell.V <- A.peelDim grouped (int64 g)
                match force st kenv (Core.evalExpr st kenv cg.KernelExpr) with
                | VArray row -> row
                | _ -> raise (InterpUnsupported "row-peel map: array-valued kernel return produced a non-array row"))
        // Trailing extents come off the FIRST row (self-describing, like the
        // compiled size-on-first-row form); an empty grouping falls back to
        // the output type's own trailing extents.
        let trailing =
            match rows |> Array.tryHead with
            | Some row -> row.Extents
            | None ->
                outIdxTys
                |> List.skip 1
                |> List.map (fun ix -> toI64 (Core.evalExpr st env ix.Extent))
                |> Array.ofList
        let extents = Array.append [| int64 ngroups |] trailing
        st.Cells <- st.Cells + (extents |> Array.fold (*) 1L)
        let out = A.allocDense elemTy outIdxTys extents
        rows |> Array.iteri (fun g row ->
            let rec walk (dim: int) (acc: int64 list) =
                if dim = row.Extents.Length then
                    A.writeCell out (int64 g :: List.rev acc) (A.readCell row (List.rev acc))
                else
                    for j in 0L .. row.Extents.[dim] - 1L do walk (dim + 1) (j :: acc)
            walk 0 [])
        VArray out
    else
    st.Cells <- st.Cells + int64 ngroups
    let results =
        Array.init ngroups (fun g ->
            cell.V <- A.peelDim grouped (int64 g)
            force st kenv (Core.evalExpr st kenv cg.KernelExpr))
    VArray (A.mkDenseArray elemTy [] [| int64 ngroups |] (A.storeOfValues elemTy results))

/// Build a Compound VALUE for a `let B = compound(dense, mask)` binding (recorded
/// in IRModule.CompoundInits). Run.fs intercepts the binding at its position in
/// the sequence -- like RandomInits -- and calls this with the lowered dense/mask
/// exprs; both are forced to concrete arrays here, then bundled via the pure
/// ArrayOps builder. Mirrors CodeGen.genCompoundInitBinding (CodeGen.fs:8581).
and materializeCompoundBinding
        (st: InterpState) (env: Env) (binding: IRBinding) (denseExpr: IRExpr) (maskExpr: IRExpr) : Value =
    let forceArr (e: IRExpr) (what: string) : BladeArray =
        match force st env (Core.evalExpr st env e) with
        | VArray a -> a
        | _ -> raise (InterpUnsupported $"compound() {what} operand is not an array")
    match binding.Type with
    | ArrayElem arrTy ->
        let dense = forceArr denseExpr "dense"
        let mask = forceArr maskExpr "mask"
        let cv = A.buildCompound arrTy dense mask
        st.Cells <- st.Cells + cv.Cardinality * cv.TrailingStride
        VCompound cv
    | _ -> raise (InterpUnsupported "compound() binding is not an array type")

/// Build a Sparse VALUE for a `let S = sparse(values, keys)` binding (recorded
/// in IRModule.SparseInits). The keys source rides the binding TYPE's
/// IRSparseKeys extent; the values' LEADING dim is the key axis and any
/// remaining dims fold into the trailing stride (buildSparse), copied straight
/// into the compact buffer in key order -- no scatter. A |values| != |keys|
/// mismatch panics (BL8001), mirroring genSparseInitBinding's runtime guard.
and materializeSparseBinding
        (st: InterpState) (env: Env) (binding: IRBinding) (valuesExpr: IRExpr) : Value =
    match binding.Type with
    | ArrayElem arrTy ->
        let sparseIx =
            arrTy.IndexTypes
            |> List.tryFind (fun ix -> ix.IxKind = IxKSparse)
            |> Option.defaultWith (fun () -> raise (InterpUnsupported "sparse() binding is not a SparseIdx array type"))
        let src =
            match sparseIx.Extent with
            | IRSparseKeys src -> src
            | _ -> raise (InterpUnsupported "sparse() binding: keys source is not a SparseIdx extent")
        let keys = resolveSparseKeys st env src sparseIx.Rank
        let (rankOf, card) = A.buildSparseIndex keys
        let values =
            match force st env (Core.evalExpr st env valuesExpr) with
            | VArray a -> a
            | _ -> raise (InterpUnsupported "sparse() values operand is not an array")
        let sv = A.buildSparse arrTy values keys rankOf card
        st.Cells <- st.Cells + sv.Cardinality * sv.TrailingStride
        VSparse sv
    | _ -> raise (InterpUnsupported "sparse() binding is not an array type")

/// Resolve one `info.Arrays.[pos]` to an ArraySource. Virtual sources
/// (range/reverse/blocked) carry no store. A deferred `IRVar` input is forced
/// AND its cell overwritten with the materialized array (forceDeferredArrayInput's
/// double-consumer memoization, 0.3).
and private resolveArraySource (st: InterpState) (env: Env) (arr: IRExpr) : ArraySource =
    match arr with
    | IRRange _ | IRVirtualReverse _ -> SVirtual
    | IRVar (id, _) ->
        match envTryFind env id with
        | Some cell ->
            match cell.V with
            | VArray a -> SReal a
            // A compound/sparse operand of an eager op (sort/reduce/set-op)
            // walks its compact present-cell buffer as a plain dense rank-1
            // array (4.1 compound-operand path; sparse in key order).
            | VCompound cv -> SReal (A.compoundToDense cv)
            | VSparse sv -> SReal (A.sparseToDense sv)
            | VDeferred _ as d ->
                let fv = force st env d
                cell.V <- fv   // memoize once (a second consumer sees the array)
                (match fv with
                 | VArray a -> SReal a
                 | VCompound cv -> SReal (A.compoundToDense cv)
                 | VSparse sv -> SReal (A.sparseToDense sv)
                 | _ -> raise (InterpUnsupported "deferred array input did not force to an array"))
            | other ->
                (match other with
                 | VArray a -> SReal a
                 | VCompound cv -> SReal (A.compoundToDense cv)
                 | VSparse sv -> SReal (A.sparseToDense sv)
                 | _ -> raise (InterpUnsupported "array input var is not an array"))
        | None ->
            match Core.evalExpr st env arr with
            | VArray a -> SReal a
            | VCompound cv -> SReal (A.compoundToDense cv)
            | VSparse sv -> SReal (A.sparseToDense sv)
            | _ -> raise (InterpUnsupported "array input var unbound")
    | _ ->
        match force st env (Core.evalExpr st env arr) with
        | VArray a -> SReal a
        | VCompound cv -> SReal (A.compoundToDense cv)
        | VSparse sv -> SReal (A.sparseToDense sv)
        | _ -> raise (InterpUnsupported "array input expression is not an array")

// Bind a kernel's capture cells into `kenv`. A capture id absent from the
// environment AND from the callables table cannot be resolved -- if the
// kernel expression actually READS it, evaluation is guaranteed to panic
// BL8004 (unbound variable) mid-kernel. That is an interpreter LIMIT, not a
// program error: the compiled side resolves such captures by NAME
// (captureForwardName's source-name fallback), e.g. an HM-cloned nested
// kernel slot whose capture ids still point at the original function's
// params. Classify it as unsupported so the harness reports
// SKIP-UNSUPPORTED instead of a crash-red.
//
// The reference check matters: capture analysis deliberately OVER-reports
// free vars (safe on the emit side, where buildCaptures filters), so a dead
// capture that the kernel never reads must keep the silent-skip behavior --
// refusing on it would demote passing diffs to skips. A miss that IS in the
// callables table also stays silent: evalExpr's IRVar arm reifies it as a
// closure.
and private bindKernelCaptures (st: InterpState) (env: Env) (kenv: Env) (caps: CaptureInfo list) (kernelExpr: IRExpr) : unit =
    let mutable referenced = Unchecked.defaultof<Set<IRId>>
    let mutable referencedComputed = false
    for c in caps do
        match envTryFind env c.Id with
        | Some cell -> envBindRef kenv c.Id cell
        | None ->
            if not (st.Callables.ContainsKey c.Id) then
                if not referencedComputed then
                    referenced <- collectVarRefsIR kernelExpr
                    referencedComputed <- true
                if Set.contains c.Id referenced then
                    raise (InterpUnsupported
                               $"kernel capture '{c.Name}' not resolvable by id (nested HM-specialized kernel)")

// interpretNest: the nest interpreter (analog of genLoopNest). Outermost-first
// recursion; bound = Extent - SumBoundDependencies - StrictOffset; per-level
// element peeling arm-for-arm; innermost kernel via Core.evalExpr.

and private interpretNest
        (st: InterpState) (env: Env) (cg: LoopNestCodeGen)
        (inputs: Dictionary<int, ArraySource>) (realAt: int -> BladeArray)
        (levelExtent: LoopIndexBinding -> int64) (out: OutTarget) : unit =
    let bindings = cg.Bindings |> Array.ofList
    let n = bindings.Length
    let idxVals : int64[] = Array.zeroCreate n
    // Kernel env: child of the deferred env (so module bindings + enclosing
    // locals remain reachable), with capture cells + reusable param cells.
    let kenv = envChild env
    bindKernelCaptures st env kenv cg.Captures cg.KernelExpr
    let paramCells = Dictionary<IRId, ValueRef>()
    for p in cg.KernelParams do
        let cell = { V = VUnit }
        paramCells.[p.VarId] <- cell
        envBindRef kenv p.VarId cell

    // Reynolds term plan (precomputed once per nest; iteration-invariant). The
    // kernel is summed over the surviving parameter permutations exactly as
    // CodeGen.genKernelExprWithReynolds renders it (CodeGen.fs:3531). buildKey
    // REUSES CodeGen.canonicalKey with a synthetic per-index name map -- a
    // consistent bijective renaming of CodeGen's peeled C++ names -- so the
    // dedup / coefficient / first-occurrence ordering are IDENTICAL to the
    // compiled binary by construction.
    let reynoldsPlan : (ValueRef[] * Blade.ReynoldsCore.ReynoldsTermPlan) option =
        if cg.HasReynolds && cg.KernelParams.Length >= 2 then
            let n = cg.KernelParams.Length
            let paramNames = Array.init n (fun i -> $"__rp{i}")
            let permNameMap (perm: int list) : Map<int, string> =
                cg.KernelParams
                |> List.mapi (fun i p -> (p.VarId, paramNames.[perm.[i]]))
                |> List.fold (fun acc (vid, nm) -> Map.add vid nm acc) Map.empty
            let plan =
                Blade.ReynoldsCore.reynoldsTermPlan n cg.IsAntisymmetric
                    (fun perm -> Blade.CodeGen.canonicalKey (permNameMap perm) cg.KernelExpr)
            let pcells = cg.KernelParams |> List.map (fun p -> paramCells.[p.VarId]) |> Array.ofList
            Some (pcells, plan)
        else None

    // Evaluate the Reynolds permutation sum for the CURRENT peeled param values,
    // mirroring genKernelExprWithReynolds's formatTerm/sumExpr semantics:
    //   symmetric : sum_i (coeff_i==1 ? v_i : coeff_i * v_i)   [coeffs > 0]
    //   antisym   : first term signed; later negative terms SUBTRACTED
    //               (acc - |c|*v), positive ADDED (acc + c*v); |c|==1 drops
    //               the multiply. Empty plan -> 0.0.
    let scaleCoeff (c: int) (v: Value) : Value = N.evalBinOp IRMul (VFloat (float c)) v
    let evalReynolds (pcells: ValueRef[]) (plan: Blade.ReynoldsCore.ReynoldsTermPlan) : Value =
        let origVals = pcells |> Array.map _.V
        let evalPerm (perm: int list) : Value =
            perm |> List.iteri (fun i src -> pcells.[i].V <- origVals.[src])
            force st kenv (Core.evalExpr st kenv cg.KernelExpr)
        let result =
            match plan.Terms with
            | [] -> VFloat 0.0
            | (coeff0, perm0) :: rest ->
                let v0 = evalPerm perm0
                let mutable acc =
                    if cg.IsAntisymmetric then
                        if abs coeff0 = 1 then (if coeff0 > 0 then v0 else N.evalUnaryOp IRNeg v0)
                        else scaleCoeff coeff0 v0
                    else
                        if coeff0 = 1 then v0 else scaleCoeff coeff0 v0
                for (coeff, perm) in rest do
                    let v = evalPerm perm
                    if cg.IsAntisymmetric && coeff < 0 then
                        let part = if abs coeff = 1 then v else scaleCoeff (abs coeff) v
                        acc <- N.evalBinOp IRSub acc part
                    else
                        let part = if coeff = 1 then v else scaleCoeff coeff v
                        acc <- N.evalBinOp IRAdd acc part
                acc
        // Restore the peeled values so the next output cell starts clean.
        Array.iteri (fun i (c: ValueRef) -> c.V <- origVals.[i]) pcells
        result

    // Peel one element at `level` given the current per-position peeled arrays
    // (immutable Map threaded down the recursion -- sibling iterations don't see
    // each other's peels) and the "sliced?" set (positions peeled at an ancestor
    // level). Returns the updated (curArrays, slicedSet). Mirrors
    // genElementBindingNew arm-for-arm in value space.
    let peelElement (b: LoopIndexBinding) (elem: ElementBinding) (extent: int64)
                    (curArrays: Map<int, BladeArray>) (sliced: Set<int>) : Map<int, BladeArray> * Set<int> =
        let i = idxVals.[b.Level]
        match elem.Virtual with
        // range<I>: the param is the raw loop counter (plus a literal offset).
        // As in CodeGen's VirtualRange arm, b.BoundDependencies / b.StrictOffset
        // are NOT folded in, so a multi-rank slot (range<SymIdx<r,N>>) delivers
        // packed storage coordinates -- prefix offsets, canonical[m] = p0+...+pm
        // (+ m for antisym). Mirroring that arm is what keeps the differential
        // gates quiet; see docs/formalism.md 7.3 for the assessment and
        // tests/corpus/loops/170-173 for the pins. Any change to the convention
        // must land in BOTH arms.
        | VirtualRange (Some off) ->
            let offV = toI64 (Core.evalExpr st kenv off)
            paramCells.[elem.ParamVarId].V <- VInt (i + offV)
            (curArrays, sliced)
        | VirtualRange None ->
            paramCells.[elem.ParamVarId].V <- VInt i
            (curArrays, sliced)
        | VirtualReverse ->
            paramCells.[elem.ParamVarId].V <- VInt (extent - 1L - i)
            (curArrays, sliced)
        | RealArray when b.FusedRank.IsSome ->
            // Arc-1 fused JOINT level (IRLoopStructure.fuseJointSLevels; genElementBinding
            // CodeGen.fs:3087): spans the array's whole d-dim plain-dense S-block.
            // The loop var is a left-justified compound coordinate; component 0
            // shifts it to the ABSOLUTE compound coord p (bound deps + strict
            // offset), then component rc decodes its per-dim coordinate
            //   coord_rc = (p / prod_{j>rc} n_j) % n_rc
            // (row-major lex) and peels ONE dim of the progressively-sliced array.
            // The d components chain through curArrays[pos]; the final (rc = d-1)
            // peel binds the kernel param.
            let d = b.FusedRank.Value
            let rc = elem.RankComponent
            let pos = elem.ArrayPosition
            let baseArr = realAt pos
            let pAbs =
                i + (b.BoundDependencies |> List.sumBy (fun dd -> idxVals.[dd])) + int64 b.StrictOffset
            let extAt j = baseArr.Extents.[j]
            let strideAfter k =
                if k >= d - 1 then 1L
                else [ k + 1 .. d - 1 ] |> List.fold (fun acc j -> acc * extAt j) 1L
            let coordRc =
                if d = 1 then pAbs
                elif rc = 0 then pAbs / (strideAfter 0)
                elif rc = d - 1 then pAbs % (extAt rc)
                else (pAbs / (strideAfter rc)) % (extAt rc)
            let currentArr = Map.tryFind pos curArrays |> Option.defaultValue baseArr
            let peeled = A.peelDim currentArr coordRc
            paramCells.[elem.ParamVarId].V <- peeled
            match peeled with
            | VArray sub -> (Map.add pos sub curArrays, Set.add pos sliced)
            | _ -> (curArrays, sliced)
        | RealArray ->
            let pos = elem.ArrayPosition
            let baseArr = realAt pos
            let currentArr = Map.tryFind pos curArrays |> Option.defaultValue baseArr
            let isSliced = Set.contains pos sliced
            // Absolute flat coordinate: local loop var + bound-deps + strict if
            // reading the ORIGINAL array. Once peeled at an outer level, the
            // slice IS the storage row (allocCompact already shortened and
            // seeded it past the diagonal), so the 0-based loop var is the slot
            // and BOTH shifts drop (mirrors genElementBindingNew RealArray arm).
            let index =
                if isSliced then i
                else (b.BoundDependencies |> List.sumBy (fun d -> idxVals.[d])) + int64 b.StrictOffset + i
            let peeled = A.peelDim currentArr index
            paramCells.[elem.ParamVarId].V <- peeled
            match peeled with
            | VArray sub -> (Map.add pos sub curArrays, Set.add pos sliced)
            | _ -> (curArrays, sliced)

    let evalKernelAndStore () =
        // Force the kernel result: Core.fs defers some sub-expression forms
        // (e.g. a kernel-embedded `<|>`/guard -> VDeferred) rather than routing
        // them to this backend, so a raw Core.evalExpr can hand back a
        // VDeferred that must be resolved before it is stored. A Reynolds
        // kernel instead sums over its surviving parameter permutations.
        let v =
            match reynoldsPlan with
            | Some (pcells, plan) -> evalReynolds pcells plan
            | None -> force st kenv (Core.evalExpr st kenv cg.KernelExpr)
        match out with
        | OutArray a ->
            let coords = [ for lvl in 0 .. n - 1 -> idxVals.[lvl] ]
            A.writeCell a coords v
        | OutArrayRows a ->
            // The kernel value IS the row. Walk the row's own coordinate space
            // and write each cell at (outer coords ++ inner coords); the row's
            // extents come from the row, so a shorter/longer row than the
            // output's trailing axes would raise the coordinate/shape mismatch
            // rather than silently truncate.
            let outer = [ for lvl in 0 .. n - 1 -> idxVals.[lvl] ]
            (match v with
             | VArray row ->
                 let rec walk (dim: int) (acc: int64 list) =
                     if dim = row.Extents.Length then
                         A.writeCell a (outer @ List.rev acc) (A.readCell row (List.rev acc))
                     else
                         for j in 0L .. row.Extents.[dim] - 1L do walk (dim + 1) (j :: acc)
                 walk 0 []
             | _ -> raise (InterpUnsupported "array-valued kernel return produced a non-array row"))
        | OutFold (acc, wrapper) ->
            acc.V <- wrapper acc.V v

    let rec loop (lvl: int) (curArrays: Map<int, BladeArray>) (sliced: Set<int>) =
        if lvl = n then evalKernelAndStore ()
        else
            let b = bindings.[lvl]
            let extent = levelExtent b
            let sub = (b.BoundDependencies |> List.sumBy (fun d -> idxVals.[d])) + int64 b.StrictOffset
            let bound = extent - sub
            let mutable i = 0L
            while i < bound do
                idxVals.[lvl] <- i
                let mutable ca = curArrays
                let mutable sl = sliced
                for elem in b.Elements do
                    let (ca', sl') = peelElement b elem extent ca sl
                    ca <- ca'
                    sl <- sl'
                loop (lvl + 1) ca sl
                i <- i + 1L

    // Base current-arrays: each real position starts at its base array.
    let baseMap =
        inputs
        |> Seq.choose (fun kv -> match kv.Value with SReal a -> Some (kv.Key, a) | _ -> None)
        |> Map.ofSeq
    loop 0 baseMap Set.empty

/// `resolveArraySource` for a FUSED operand -- the legs of a reduction join.
///
/// A join does not MATERIALIZE its operands. Codegen fuses each leg's map into
/// the joint nest and binds it to a per-iteration `const`; that is the whole
/// point loops/141 pins ("exactly one `std::cos` ... not five"). So a deferred
/// binding read only by a join is never built as an array, never joins the
/// auto-print list, and prints nothing -- loops/129's `d` states the same rule
/// from the other side.
///
/// The ordinary resolver forces such a binding through `force`, which is what
/// registers it in `ForcedDeferred` "under its own name". That is exactly right
/// for an EAGER consumer -- sort/set-op/reduce over a real array, whose codegen
/// twin is `forceDeferredArrayInput`, which likewise materializes it and adds it
/// to `forcedDeferredIdsCell` -- and exactly wrong here. It is why the two lanes
/// disagreed on loops/141: the interpreter printed `ct` as a materialized array
/// where the compiled binary printed nothing for it and went straight to `j_pc`.
///
/// Deliberately does NOT memoize into the cell either. The compiled lane
/// recomputes the map inside the nest and materializes SEPARATELY for any eager
/// consumer, so leaving the cell deferred keeps a later eager read on the
/// registering path. Memoizing would make that read find a plain `VArray`,
/// skip registration, and swallow a print the compiled lane does emit --
/// trading this drift for its mirror image.
let private resolveFusedArraySource (st: InterpState) (env: Env) (arr: IRExpr) : ArraySource =
    match arr with
    | IRVar (id, _) ->
        (match envTryFind env id with
         | Some cell ->
             (match cell.V with
              | VDeferred (dexpr, denv) ->
                  (match forceExpr st denv dexpr with
                   | VArray a -> SReal a
                   | VCompound cv -> SReal (A.compoundToDense cv)
                   | VSparse sv -> SReal (A.sparseToDense sv)
                   | _ -> raise (InterpUnsupported "deferred array input did not force to an array"))
              | _ -> resolveArraySource st env arr)
         | None -> resolveArraySource st env arr)
    | _ -> resolveArraySource st env arr

/// reduce over a deferred computation (genReduceComputeBinding 8714): fold
/// every cell of each leaf apply into a per-leaf scalar accumulator (all
/// seeded with `seed`), through the fold kernel wrapper -- one nest per leaf
/// (value-identical to CodeGen's staggered merged nest, since leaves don't
/// interact). Single leaf = scalar; fusion tree = a structured tuple
/// mirroring the tree shape (same rationale as forceTreeShaped).
let rec private forceReduceCompute (st: InterpState) (env: Env) (comp: IRExpr) (kernel: IRExpr) (seed: Value) : Value =
    // HOISTED-OPERAND PRELUDE -- the twin of CodeGen.genReduceComputeBinding's.
    // The lift pass pulls a synthesized loop application out of the
    // combinator's `Arrays` slot into its own let, so a deferred computation
    // whose operand is a broadcast (`reduce(exp <@> (i * w * ts), (+))`, i.e.
    // units/065's kernel body) arrives here as
    // `IRLet(v, IRApp(IRObjectFor ...), IRApplyCombinator ...)`. The leaf check
    // below then saw an IRLet, not an apply, and raised InterpUnsupported for a
    // well-formed fold the compiled lane now evaluates.
    //
    // Bind the hoisted values into the (flat, SSA-keyed) env exactly as Core's
    // IRLet arm does -- the combinator underneath then resolves its operand by
    // id -- and fold over that. Only ever turns an InterpUnsupported into a
    // value: an IRLet-wrapped computation had no other outcome here.
    let rec peelCompLets e =
        match e with
        | IRLet (id, value, body) ->
            let v = force st env (Core.evalExpr st env value)
            envBind env id v |> ignore
            peelCompLets body
        | _ -> e
    let comp = peelCompLets comp
    let rec resolveDeferred e =
        match e with
        | IRVar (id, _) -> (match envTryFind env id with Some cell -> (match cell.V with VDeferred (e2, _) -> resolveDeferred e2 | _ -> e) | None -> e)
        | _ -> e
    let rec collect e =
        match resolveDeferred e with
        | IRFusion (l, r) -> collect l @ collect r
        | other -> [ other ]
    let leaves = collect comp
    let infos = leaves |> List.choose (function IRApplyCombinator i -> Some i | _ -> None)
    if infos.IsEmpty || infos.Length <> leaves.Length then
        raise (InterpUnsupported "reduce over a deferred computation with non-apply leaves")
    // JOIN ENCODING (IR.fs, IRReduceCompute): per-leg kernels and seeds. The
    // interpreter needs no shared-value machinery -- sharing a named deferred
    // map is a per-iteration CSE of a pure map, so the values are identical
    // either way, and each leg gets its own nest here as it always has.
    let legFolds =
        match kernel with
        | IRTuple ks when ks.Length = infos.Length -> ks |> List.map (resolveBinaryFold st env)
        | _ -> infos |> List.map (fun _ -> resolveBinaryFold st env kernel)
    let legSeeds =
        match seed with
        | VTuple ss when ss.Length = infos.Length -> List.ofArray ss
        | _ -> infos |> List.map (fun _ -> seed)
    let accVals =
        infos |> List.mapi (fun li info ->
            gateInputs info
            let names = info.Arrays |> List.mapi (fun i _ -> $"a{i}")
            let cg = buildLoopNestCodeGen info names "acc" st.Builder
            if hasRealSymmetry cg.OutputSymmVec || cg.HasReynolds || (cg.Bindings |> List.exists _.FusedRank.IsSome) then
                raise (InterpUnsupported "reduce over symmetric/Reynolds/fused computation (M2.5)")
            let inputs = Dictionary<int, ArraySource>()
            // FUSED, not materialized: see resolveFusedArraySource. A deferred
            // binding read only by these legs must not join the print list.
            info.Arrays |> List.iteri (fun i arr -> inputs.[i] <- resolveFusedArraySource st env arr)
            let realAt (pos: int) = match inputs.TryGetValue pos with | true, SReal a -> a | _ -> raise (InterpUnsupported "reduce-compute: virtual position needs no realAt")
            let levelExtent (b: LoopIndexBinding) : int64 =
                match b.Extent with
                | IRLit (IRLitInt n) -> n
                | IRCompoundMask _ -> raise (InterpUnsupported "compound-index loop level (M2.7)")
                | _ ->
                    let pos = match b.Elements with e :: _ -> e.ArrayPosition | [] -> 0
                    match inputs.TryGetValue pos with | true, SReal a -> a.Extents.[b.ExtentDimRef] | _ -> toI64 (Core.evalExpr st env b.Extent)
            let acc = { V = legSeeds.[li] }
            interpretNest st env cg inputs realAt levelExtent (OutFold (acc, legFolds.[li]))
            acc.V)
    match accVals with
    | [ single ] -> single
    // JOIN: a FLAT Tuple<k>, matching typeOf's shape for the join encoding and
    // codegen's flat `make_tuple` (IR.fs, IRReduceCompute). The chain below
    // nests because `<&!>` between two maps is a binary operator; a join is
    // k-ary, and assembling it into pairs made every projection past index 1
    // fail with BL8003 "tuple projection index out of range".
    | _ when (match kernel with
              | IRTuple ks -> ks.Length = accVals.Length
              | _ -> false) ->
        VTuple (Array.ofList accVals)
    | _ ->
        // Reassemble the accumulators into the TREE shape (accVals is the
        // in-order leaf list, so consuming it left-to-right while walking the
        // fusion tree reproduces the type's nesting exactly).
        let rec assemble (e: IRExpr) (accs: Value list) : Value * Value list =
            match resolveDeferred e with
            | IRFusion (l, r) ->
                let (lv, rest) = assemble l accs
                let (rv, rest') = assemble r rest
                (VTuple [| lv; rv |], rest')
            | _ ->
                match accs with
                | a :: rest -> (a, rest)
                | [] -> raise (InterpUnsupported "reduce-compute: accumulator/tree shape mismatch")
        fst (assemble comp accVals)

// Standalone virtual-array materialization (rare: a bare printed range/reverse).

let private materializeVirtual (st: InterpState) (env: Env) (idxTys: IRIndexType list) (kind: VirtualKind) : Value =
    match idxTys with
    | [ ix ] ->
        let n = match ix.Extent with IRLit (IRLitInt n) -> n | e -> toI64 (Core.evalExpr st env e)
        let data =
            match kind with
            | VirtualReverse -> SInt (Array.init (int n) (fun i -> n - 1L - int64 i))
            | VirtualRange (Some (IRLit (IRLitInt off))) -> SInt (Array.init (int n) (fun i -> int64 i + off))
            | VirtualRange (Some off) -> let o = toI64 (Core.evalExpr st env off) in SInt (Array.init (int n) (fun i -> int64 i + o))
            | VirtualRange None -> SInt (Array.init (int n) (fun i -> int64 i))
            | RealArray -> raise (InterpUnsupported "materializeVirtual: RealArray")
        VArray (A.mkDenseArray (IRTScalar ETInt64) idxTys [| n |] data)
    | _ -> raise (InterpUnsupported "standalone multi-index virtual array")

// evalArrayNode: Core's array/loop/combinator fallthrough hook.

let rec evalArrayNode (st: InterpState) (env: Env) (expr: IRExpr) : Value =
    match expr with
    // -- Loop objects: pure provenance (the <@> apply reads the baked ApplyInfo).
    | IRMethodFor _ | IRObjectFor _ -> VLoopObj { Provenance = expr; Captured = env }

    // -- Deferred combinator forms: hold the unevaluated expr + its env.
    | IRApplyCombinator _ | IRComposeApply _
    | IRParallel _ | IRFusion _ | IRFunctorMap _
    | IRGuard _ | IRSequence _ | IRBind _
    | IRComposeObj _ | IRComposeMeth _
    | IRFallback _ -> VDeferred (expr, env)

    // -- Choice `<|>`: DEFER only when an operand is itself a computation
    //    (mirrors computeDeferredIds' isDeferred -- the printability oracle);
    //    otherwise it materializes NOW. A SCALAR choice (both operands scalar,
    //    incl. one embedded in a kernel body, 039) is `(l != 0) ? l : r` with
    //    `l` evaluated once -- the exprToCpp(IRChoice) ternary.
    | IRChoice (left, right) ->
        let isChoiceComp (e: IRExpr) =
            match e with
            | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _
            | IRFunctorMap _ | IRChoice _ | IRComposeObj _ | IRComposeMeth _
            | IRBind _ | IRGuard _ | IRSequence _ | IRZip _ -> true
            | IRVar (id, _) ->
                (match envTryFind env id with
                 | Some c -> c.V.IsVDeferred
                 | None -> false)
            | _ -> false
        if isChoiceComp left || isChoiceComp right then VDeferred (expr, env)
        else
            let lv = Core.evalExpr st env left
            if isNonZero lv then lv else Core.evalExpr st env right

    // -- Zip in kernel position: a value tuple. (A bare top-level zip binding is
    //    tuple-typed and prints nothing; consumed forms are absorbed into
    //    co-iteration ApplyInfo, never reaching here.)
    | IRZip elems -> VTuple (elems |> List.map (Core.evalExpr st env) |> Array.ofList)

    // -- The force point.
    | IRCompute inner -> forceExpr st env inner

    // -- Materialized array producers.
    | IRArrayLit (elems, arrType) -> evalArrayLit st env elems arrType
    | IRReduce (arrExpr, kernel, init) ->
        let av = force st env (Core.evalExpr st env arrExpr)
        let initV = init |> Option.map (Core.evalExpr st env)
        match av with
        | VArray a ->
            if a.Extents.Length <> 1 then raise (InterpUnsupported "reduce over rank>1 array (M2.7)")
            A.reduceArray a (resolveBinaryFold st env kernel) initV
        | VCompound cv ->
            // reduce over a compound walks its compact present-cell buffer
            // (genReduceBinding compound arm, CodeGen.fs:1934-1938).
            A.compoundReduce cv (resolveBinaryFold st env kernel) initV
        | VSparse sv ->
            // reduce over a sparse walks its compact buffer in key order.
            A.sparseReduce sv (resolveBinaryFold st env kernel) initV
        | _ -> raise (InterpUnsupported "reduce over a non-array value")
    | IRReduceCompute (comp, kernel, seedExpr) ->
        let seed = Core.evalExpr st env seedExpr
        forceReduceCompute st env comp kernel seed
    | IRProdSum args ->
        let arrs = args |> List.map (fun e -> match force st env (Core.evalExpr st env e) with VArray a -> a | _ -> raise (InterpUnsupported "prodsum over non-array"))
        A.prodSum arrs

    // -- group_by(vals, gk): gather each CSR group's values into a ragged array
    //    (genGroupByBinding). `gk` is a VGroupKeys (built at group_keys binding
    //    time); `vals` is forced (double-consumer memoized) so a deferred/computed
    //    values array is materialized once before the gather (013/022).
    | IRGroupBy (valsExpr, gkExpr) ->
        let gk =
            match Core.evalExpr st env gkExpr with
            | VGroupKeys g -> g
            | _ -> raise (InterpUnsupported "group_by: grouping operand is not a group_keys value")
        let vals = forceInputArray st env valsExpr
        let idxTys = match typeOf expr with ArrayElem at -> at.IndexTypes | _ -> []
        VArray (A.buildGroupBy idxTys gk vals)

    // -- ungroup(G): the rows of a segment-grouped array written back over the
    //    source axis (genUngroupBinding's twin).
    | IRUngroup (gExpr, src) ->
        let g = forceInputArray st env gExpr
        (match g.Data with
         | SRagged (rows, lens, _) ->
             let cells =
                 [| for r in 0 .. rows.Length - 1 do
                        for k in 0L .. lens.[r] - 1L do
                            yield A.readCell g [ int64 r; k ] |]
             VArray { ElemType = g.ElemType
                      IndexTypes = [ src ]
                      Extents = [| int64 cells.Length |]
                      Data = A.storeOfValues g.ElemType cells }
         | _ -> raise (InterpUnsupported "ungroup: operand is not a ragged (grouped) array"))

    // -- ungroup(G) of a tile grouping: tiles back over the two axes
    //    (genUngroupGridBinding's twin).
    | IRUngroupGrid (gExpr, srcs, bounds) ->
        let g = forceInputArray st env gExpr
        let b0, b1 = Array.ofList bounds.[0], Array.ofList bounds.[1]
        let n0, n1 = int b0.[b0.Length - 1], int b1.[b1.Length - 1]
        let g1 = b1.Length - 1
        let cells : Value[] = Array.zeroCreate (n0 * n1)
        (match g.Data with
         | SRagged (rows, _, _) ->
             for t in 0 .. rows.Length - 1 do
                 let t0, t1 = t / g1, t % g1
                 let lo0, hi0 = int b0.[t0], int b0.[t0 + 1]
                 let lo1, hi1 = int b1.[t1], int b1.[t1 + 1]
                 let w = hi1 - lo1
                 for i in lo0 .. hi0 - 1 do
                     for j in lo1 .. hi1 - 1 do
                         cells.[i * n1 + j] <- A.readCell g [ int64 t; int64 ((i - lo0) * w + (j - lo1)) ]
         | _ -> raise (InterpUnsupported "ungroup: operand is not a ragged (grouped) array"))
        // a rank-2 dense value is NESTED rows in the interpreter (peelDim's
        // view contract), not a flat pool
        let rows = Array.init n0 (fun i -> A.storeOfValues g.ElemType cells.[i * n1 .. i * n1 + n1 - 1])
        VArray { ElemType = g.ElemType
                 IndexTypes = srcs
                 Extents = [| int64 n0; int64 n1 |]
                 Data = SNested rows }

    // -- ungroup([r1..rF], A): per-file arrays assembled over the tiled axis
    //    (genUngroupRowsBinding's twin).
    | IRUngroupRows (rowExprs, _, src) ->
        let rows = rowExprs |> List.map (forceInputArray st env)
        (match rows with
         | [] -> raise (InterpUnsupported "ungroup: no rows")
         | first :: _ ->
             let cells =
                 rows |> List.toArray |> Array.collect (fun r ->
                     Array.init (int r.Extents.[0]) (fun k -> A.readCell r [ int64 k ]))
             VArray { ElemType = first.ElemType
                      IndexTypes = [ src ]
                      Extents = [| int64 cells.Length |]
                      Data = A.storeOfValues first.ElemType cells })

    // -- group_bucket(gk): the CSR pair inverted into a dense row -> bucket map
    //    (genGroupBucketBinding). Same VGroupKeys operand as group_by; typecheck
    //    has already refused anything but a bare gk name.
    | IRGroupBucket gkExpr ->
        let gk =
            match Core.evalExpr st env gkExpr with
            | VGroupKeys g -> g
            | _ -> raise (InterpUnsupported "group_bucket: operand is not a group_keys value")
        let idxTys = match typeOf expr with ArrayElem at -> at.IndexTypes | _ -> []
        VArray (A.buildGroupBucket idxTys gk)

    // -- extents(gk): per-group sizes off the CSR offsets, no gather
    //    (genGroupSizesBinding).
    | IRGroupSizes gkExpr ->
        let gk =
            match Core.evalExpr st env gkExpr with
            | VGroupKeys g -> g
            | _ -> raise (InterpUnsupported "extents(gk): operand is not a group_keys value")
        let idxTys = match typeOf expr with ArrayElem at -> at.IndexTypes | _ -> []
        VArray (A.buildGroupSizes idxTys gk)

    // -- Virtual arrays (standalone materialization; usually consumed as inputs).
    | IRRange (idxTys, offset) -> materializeVirtual st env idxTys (VirtualRange offset)
    | IRVirtualReverse ix -> materializeVirtual st env [ ix ] VirtualReverse

    // -- Array expression ops.
    | IRIndex (arrExpr, idxExprs, _) ->
        (match force st env (Core.evalExpr st env arrExpr) with
         | VArray a -> A.indexArray a (idxExprs |> List.map (Core.evalExpr st env))
         | VCompound cv -> compoundIndexRead st env cv idxExprs
         | VSparse sv -> sparseIndexRead st env sv idxExprs
         | _ -> raise (InterpUnsupported "IRIndex on non-array value"))
    | IRCurry (arrExpr, idxExpr, _) ->
        (match force st env (Core.evalExpr st env arrExpr) with
         | VArray a -> A.curryArray a (toI64 (Core.evalExpr st env idxExpr))
         | _ -> raise (InterpUnsupported "curry of non-array"))
    | IRExtent (arrExpr, dim) ->
        (match force st env (Core.evalExpr st env arrExpr) with
         | VArray a -> VInt (A.extent a dim)
         | _ -> raise (InterpUnsupported "extent of non-array"))
    | IRPolyIndex (packExpr, idxExpr) ->
        A.polyIndex (force st env (Core.evalExpr st env packExpr)) (toI64 (Core.evalExpr st env idxExpr))
    | IRContains (arrExpr, valExpr) ->
        // A compound operand walks its compact present-cell buffer as a plain
        // dense array (contains over an empty compound -> false, 006).
        let arrOpt =
            match force st env (Core.evalExpr st env arrExpr) with
            | VArray a -> Some a
            | VCompound cv -> Some (A.compoundToDense cv)
            | VSparse sv -> Some (A.sparseToDense sv)
            | _ -> None
        (match arrOpt with
         | Some a ->
            let target = Core.evalExpr st env valExpr
            let rank = a.Extents.Length
            let mutable found = false
            let rec walk lvl acc =
                if found then ()
                elif lvl = rank then
                    let cell = A.readCell a (List.rev acc)
                    if valuesEqual cell target then found <- true
                else for i in 0L .. a.Extents.[lvl] - 1L do (if not found then walk (lvl + 1) (i :: acc))
            walk 0 []
            VBool found
         | None -> raise (InterpUnsupported "contains over non-array"))

    | IRReplicate (countExpr, bodyExpr) ->
        let count = toI64 (Core.evalExpr st env countExpr)
        A.replicateArray count (force st env (Core.evalExpr st env bodyExpr))

    // -- M4 wave-1 eager set/reshape ops. Each FORCES its input(s) first (the
    //    double-consumer memoization via forceInputArray), then materializes a
    //    fresh dense rank-1 array (transpose: any rank). Semantics pinned to the
    //    CodeGen materialize*Form emitters (byte-verified):
    //      mask  = Bool PRESENCE array `m[i]=pred(A[i])`, source index space,
    //              NO compaction (maskPresence, NOT the deprecated maskArray);
    //      sort  = stable ascending by key; unique/intersect/union =
    //              first-occurrence order; transpose = hard swap of two
    //              arity-1 axes into a fresh pool.
    | IRMask (arrExpr, predExpr) ->
        let a = forceInputArray st env arrExpr
        VArray (A.maskPresence a (resolveUnaryKernel st env predExpr))
    | IRSort (arrExpr, keyExpr) ->
        let a = forceInputArray st env arrExpr
        VArray (A.sortArray a (resolveUnaryKernel st env keyExpr))
    | IRUnique arrExpr ->
        VArray (A.uniqueArray (forceInputArray st env arrExpr))
    | IRIntersect (aExpr, bExpr) ->
        let a = forceInputArray st env aExpr
        let b = forceInputArray st env bExpr
        VArray (A.intersectArray a b)
    | IRUnion (aExpr, bExpr) ->
        let a = forceInputArray st env aExpr
        let b = forceInputArray st env bExpr
        VArray (A.unionArray a b)
    | IRTranspose (arrExpr, d1, d2) ->
        VArray (A.transposeArray (forceInputArray st env arrExpr) d1 d2)

    // -- Rank-changing assembly (formalism 2.6). Each operand is FORCED first
    //    (a deferred producer materializes once), then copied into a fresh
    //    dense pool: stack adds a leading selector axis, join concatenates
    //    along dim d. Pinned to CodeGen's materialize{Stack,Join}Form.
    | IRStack arrs ->
        VArray (A.stackArrays (arrs |> List.map (forceInputArray st env)))
    | IRJoin (arrs, dim) ->
        VArray (A.joinArrays (arrs |> List.map (forceInputArray st env)) dim)

    // -- Symmetry producers. Each FORCES its input(s) first (so a deferred
    //    reynolds/gram source materializes once -- 109/110/066), then materializes
    //    a fresh output. `typeOf expr` carries the fission/gram/preserved output
    //    type (shape + symmetry), driving storage class and print routing:
    //    decompact = value-equivalent group fission (readCompact per output
    //    cell); gram = A.B^H into Sym/Hermitian-compact (same) or dense
    //    (distinct); negate/conjugate = shape-preserving pool transform.
    | IRDecompact (arrExpr, _) ->
        VArray (A.decompactArray (forceInputArray st env arrExpr) (typeOf expr))
    | IRGram (lExpr, rExpr, _) ->
        let l = forceInputArray st env lExpr
        let r = forceInputArray st env rExpr
        VArray (A.gramArray l r (typeOf expr))
    | IRGramApply (lExpr, rExpr, xExpr) ->
        // gram_apply = the action A (B^H x). Same FORCE-then-materialize
        // shape as gram; two ascending folds, twin of materializeGramApplyForm.
        let l = forceInputArray st env lExpr
        let r = forceInputArray st env rExpr
        let x = forceInputArray st env xExpr
        VArray (A.gramApplyArray l r x (typeOf expr))
    | IRMatmul (lExpr, rExpr) ->
        // matmul = the dense A.B product. Same FORCE-then-materialize
        // shape as gram; the naive i/j/t-ascending fold mirrors the shim's
        // native fallback so the compiled/interpreted differential is exact.
        let l = forceInputArray st env lExpr
        let r = forceInputArray st env rExpr
        VArray (A.matmulArray l r (typeOf expr))
    | IREigh operandExpr ->
        // eigh = the one linalg node whose value is a TUPLE, so it cannot go
        // through `VArray`. Same FORCE-then-materialize shape as gram/matmul
        // otherwise. See `A.eighArrays` for why this is a deliberate copy of the
        // BladeMath oracle rather than a call into it, and why it only ever runs
        // with the LAPACK gate ON.
        let s = forceInputArray st env operandExpr
        let (q, lam) = A.eighArrays s (typeOf expr)
        VTuple [| VArray q; VArray lam |]
    | IRSolve (mExpr, rExpr) ->
        // solve = the dense LU linear solve. Same FORCE-then-materialize shape
        // as gram/matmul, and like matmul (and unlike eigh) it is the twin of
        // code an ORDINARY build runs -- `A.solveArray` mirrors
        // `materializeSolveForm`'s emitted loop nest operation for operation,
        // so the compiled/interpreted differential is exact.
        let m = forceInputArray st env mExpr
        let r = forceInputArray st env rExpr
        VArray (A.solveArray m r (typeOf expr))
    | IRArrayNegate arrExpr ->
        VArray (A.negateConjugateArray false (forceInputArray st env arrExpr))
    | IRArrayConjugate arrExpr ->
        VArray (A.negateConjugateArray true (forceInputArray st env arrExpr))

    // -- Inline object_for application: `A [op] B` outer product (and its
    //    elementwise / single-array-broadcast siblings). CodeGen.fs:7388 peels
    //    the array list out of the single tuple argument the same way.
    | IRApp (IRObjectFor objInfo, args, _) ->
        let arrays = match args with [ IRTuple elems ] -> elems | _ -> args
        materializeObjectForApp st env (typeOf expr) objInfo arrays

    | other -> raise (InterpUnsupported (nodeCase other))

/// Read a compound value at an index tuple (full / trailing / partial),
/// mirroring exprToCppCore's compoundRead dispatch (CodeGen.fs:1589-1712).
/// The coordinate exprs are evaluated in `env`; wildcard positions arrive as
/// `IRLit IRLitUnit` sentinels (classifyCompoundIndexTuple).
and private compoundIndexRead (st: InterpState) (env: Env) (cv: CompoundValue) (idxExprs: IRExpr list) : Value =
    // Rank-1 compound scalar sugar: `C(i)` == the 1-tuple read `C((i))`
    // (there is no surface 1-tuple literal). CodeGen.fs:1598-1606.
    let idxExprs =
        match cv.LeadRank, idxExprs with
        | 1, first :: rest when (match first with IRTuple _ -> false | _ -> true) -> IRTuple [ first ] :: rest
        | _ -> idxExprs
    match idxExprs with
    | (IRTuple coords) :: trailingIdxs ->
        match classifyCompoundIndexTuple cv.LeadRank coords with
        | CompoundFull ->
            let coordVals = coords |> List.map (fun c -> toI64 (Core.evalExpr st env c)) |> Array.ofList
            // A trailing wildcard (unit sentinel) or an omitted trailing index
            // leaves that dim free -> row sub-view; a concrete trailing index
            // supplies the offset -> scalar (CodeGen.fs:1688-1710).
            let concreteTrail =
                trailingIdxs |> List.filter (function IRLit IRLitUnit -> false | _ -> true)
            if cv.TrailingStride = 1L then
                A.compoundFullScalar cv coordVals 0L
            elif not (List.isEmpty concreteTrail) then
                A.compoundFullScalar cv coordVals (toI64 (Core.evalExpr st env (List.head concreteTrail)))
            else
                A.compoundRow cv coordVals
        | CompoundPartial _ ->
            // Unreachable since the flat-subscript conversion: typecheck packs
            // full k-tuples for compound heads and rejects wildcard/partial
            // forms (they moved to SparseIdx). Mirror codegen's backstop.
            raise (InterpUnsupported "internal: a partial compound index tuple reached the interpreter -- partial/wildcard reads on CompoundIdx were removed (use SparseIdx)")
    | _ -> raise (InterpUnsupported "compound index without a tuple head")

/// Sparse index read -- the sparse twin of compoundIndexRead, mirroring the
/// same C++ read emission (full scalar / trailing row / gather partial). The
/// classification is shared (classifyCompoundIndexTuple is shape-only).
and private sparseIndexRead (st: InterpState) (env: Env) (sv: SparseValue) (idxExprs: IRExpr list) : Value =
    let idxExprs =
        match sv.LeadRank, idxExprs with
        | 1, first :: rest when (match first with IRTuple _ -> false | _ -> true) -> IRTuple [ first ] :: rest
        | _ -> idxExprs
    match idxExprs with
    | (IRTuple coords) :: trailingIdxs ->
        match classifyCompoundIndexTuple sv.LeadRank coords with
        | CompoundFull ->
            let coordVals = coords |> List.map (fun c -> toI64 (Core.evalExpr st env c)) |> Array.ofList
            let concreteTrail =
                trailingIdxs |> List.filter (function IRLit IRLitUnit -> false | _ -> true)
            if sv.TrailingStride = 1L then
                A.sparseFullScalar sv coordVals 0L
            elif not (List.isEmpty concreteTrail) then
                A.sparseFullScalar sv coordVals (toI64 (Core.evalExpr st env (List.head concreteTrail)))
            else
                A.sparseRow sv coordVals
        | CompoundPartial (pinned, freePos) ->
            let pinnedVals = pinned |> List.map (fun (pos, c) -> (pos, toI64 (Core.evalExpr st env c)))
            A.sparsePartial sv pinnedVals freePos
    | _ -> raise (InterpUnsupported "sparse index without a tuple head")

/// Structural value equality for `contains` (numeric-aware).
and private valuesEqual (a: Value) (b: Value) : bool =
    match a, b with
    | VInt x, VInt y -> x = y
    | VFloat x, VFloat y -> x = y
    | VInt x, VFloat y | VFloat y, VInt x -> float x = y
    | VBool x, VBool y -> x = y
    | VString x, VString y -> System.String.Equals(x, y, System.StringComparison.Ordinal)
    | VComplex (r1, i1), VComplex (r2, i2) -> r1 = r2 && i1 = i2
    | _ -> false
