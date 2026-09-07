// Device backends: CUDA device-body planning and kernel emission, the
// simplicial CUDA/MPI nests, wreath application, and genApplyCombinator
// (the whole-apply backend dispatch across serial/OMP/CUDA/MPI).
module Blade.CodeGenCuda

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

// ---------------------------------------------------------------------------
// What a __global__ can NAME (device-body expressibility) -- section 1 of 2
// ---------------------------------------------------------------------------
//
// The .cu is a SEPARATE translation unit: it sees the kernel's own parameters
// and nothing else. `exprToCpp`, though, renders a capture by its HOST
// identifier and a materializing kernel body as a CALL to that body's lifted
// HOST function (routeKernelBodyThroughCall) -- both undeclared inside the .cu,
// which nvcc reports as `identifier "s" is undefined` after the host half has
// already been told a device kernel exists. So the capability gate and the
// emitter have to be derived from ONE predicate: the emitters below ask what a
// device body may name, and refuse (host fallback) whatever it may not.

/// Free variable ids of an expression: every `IRVar` NOT bound by a binder
/// inside it. Folds over the canonical `BinderShape`/`ExprShape` pair, so a new
/// IRExpr variant is scoped correctly here the moment it is declared there
/// (unlike `collectVarRefsIR`, which deliberately keeps let-bound ids).
let internal deviceFreeVarIds (expr: IRExpr) : Set<IRId> =
    let rec go (bound: Set<IRId>) (e: IRExpr) : Set<IRId> =
        let anyOf b kids = kids |> List.fold (fun acc k -> Set.union acc (go b k)) Set.empty
        match e with
        | IRVar (id, _) -> if Set.contains id bound then Set.empty else Set.singleton id
        | BinderShape (plain, scopes) ->
            scopes
            |> List.fold (fun acc (ids, kids) -> Set.union acc (anyOf (Set.union bound ids) kids))
                         (anyOf bound plain)
        | ExprShape (kids, _) -> anyOf bound kids
    go Set.empty expr

/// The C++ name of the first user/lifted callable this expression CALLS, if any.
/// Such a call renders as `<fnName>(...)`; the .cu declares no host functions,
/// so its presence is a refusal (unless the caller inlines the callee first).
let internal deviceCalleeName (expr: IRExpr) : string option =
    let mutable hit : string option = None
    let rec go (e: IRExpr) =
        if hit.IsSome then () else
        match e with
        | IRApp (f, args, _) ->
            (match resolveCallable f with
             | Some c -> hit <- Some (sanitizeCppName c.Name)
             | None -> ())
            if hit.IsNone then f :: args |> List.iter go
        | ExprShape (kids, _) -> kids |> List.iter go
    go expr
    hit

/// The first `IRParam` name mentioned. `exprToCppCore` renders one as the bare
/// SOURCE parameter name, which no device kernel declares (its operands are the
/// peeled `<array>__<index>` reads).
let internal deviceParamRef (expr: IRExpr) : string option =
    let mutable hit : string option = None
    let rec go (e: IRExpr) =
        if hit.IsSome then () else
        match e with
        | IRParam (n, _, _) -> hit <- Some n
        | ExprShape (kids, _) -> kids |> List.iter go
    go expr
    hit

/// Why this kernel body cannot be rendered into the .cu as one device
/// expression, given `declared` -- the ids the `__global__` itself binds (the
/// peeled operand reads, plus any forwarded captures). `None` means it can.
///
/// Every arm names a construct the emitter genuinely cannot express, so the
/// gate and the emission agree by construction: a refusal is a HOST FALLBACK
/// (correct values, no device kernel), never a half-emitted one.
let cudaDeviceBodyRefusal (declared: Set<IRId>) (names: Map<IRId, string>) (body: IRExpr) : string option =
    let spell id = Map.tryFind id names |> Option.defaultValue ($"__v{id}")
    if not (kernelBodyIsExpressionShaped body) then
        Some "a kernel-local array materialization"
    else
        match deviceFreeVarIds body |> Set.toList |> List.filter (fun id -> not (Set.contains id declared)) with
        | id :: _ -> Some ($"the captured value '{(spell id)}'")
        | [] ->
            match deviceCalleeName body with
            | Some fn -> Some ($"the call to host function '{fn}'")
            | None -> deviceParamRef body |> Option.map (sprintf "the parameter reference '%s'")

/// How many cells a device buffer has.
///
/// STATIC is the only shape this emitter originally had, and it stays
/// byte-identical: the count is a literal in the .cu. RUNTIME is what a kernel
/// inside a GENERIC FUNCTION needs -- its operands' extents are not known until
/// the call, so the count is evaluated at the launch site (`HostExpr`, host C++
/// naming the host `Array<T,N>` wrapper) and travels into the .cu as an extra
/// `size_t` parameter (`ParamName`) that the wrapper and the kernel both read.
type CudaCells =
    | CellsStatic of int64
    | CellsRuntime of hostExpr: string * paramName: string

/// The .cu-side text for a cell count: a literal, or the parameter carrying it.
let cudaCellsText (c: CudaCells) : string =
    match c with
    | CellsStatic n -> $"{n}UL"
    | CellsRuntime (_, p) -> p

/// The extra `size_t` parameter a runtime count contributes to the kernel and
/// wrapper signatures (none for a static one).
let cudaCellsParam (c: CudaCells) : string option =
    match c with
    | CellsStatic _ -> None
    | CellsRuntime (_, p) -> Some ($"size_t {p}")

/// The argument a runtime count contributes at the host call site.
let cudaCellsHostArg (c: CudaCells) : string option =
    match c with
    | CellsStatic _ -> None
    | CellsRuntime (h, _) -> Some h

/// Two device buffers may be folded together iff their cell counts AGREE.
/// Two STATIC counts are compared exactly. A count that is only known at
/// runtime is TRUSTED -- the same rule the host applies to a join's legs
/// (docs/plan-reduction-joins.md section 1.5): only a PROVABLE disagreement is
/// refused, and nothing is provable about an extent neither side knows here.
/// Comparing the two host EXPRESSIONS textually instead would refuse
/// `prodsum(s, ct)` beside `prodsum(ct, ct)` -- different arrays, same axis --
/// which is the shape this whole path exists for.
let cudaCellsCompatible (a: CudaCells) (b: CudaCells) : bool =
    match a, b with
    | CellsStatic x, CellsStatic y -> x = y
    | _ -> true

/// One captured value forwarded into a device kernel as an extra parameter.
type CudaFwdCapture = {
    /// Identifier naming it in the HOST scope that emits the launch. Resolved
    /// through `captureForwardName`, i.e. the EMITTED spelling -- a block-local
    /// `let` is renamed to `__v<id>`, and forwarding the source name there is
    /// the trap this repo has hit before (block-local-capture-forwarding.md).
    HostName: string
    /// Parameter name inside the .cu (and the device buffer's stem).
    DevName: string
    /// Pool cells for an array capture; `None` for a scalar (passed by value).
    Cells: CudaCells option
    /// Host (`std::`) spelling for the `extern "C"` wrapper signature, and the
    /// device (`thrust::`) spelling for the kernel signature / device buffer.
    HostCpp: string
    DevCpp: string
}

/// A device-side rendering of one kernel body: the captures it needs forwarded,
/// the statements to place inside the `__global__` ahead of the output write,
/// and the expression that write assigns.
type CudaDeviceBody = {
    Captures: CudaFwdCapture list
    Stmts: string list
    Expr: string
}

/// Scalar element types that cross the host/device boundary and render in the
/// device dialect. Shares `isCudaBoundarySafeElem`'s judgement so a capture can
/// never be forwarded in a shape an operand would be refused in.
let internal cudaFwdScalarCpp (ty: IRType) : (string * string) option =
    if isCudaBoundarySafeElem ty then Some (elemTypeToCpp ty, cudaDevElemTypeToCpp ty) else None

/// A DENSE RANK-1 array type's (cells, host elem, device elem). Rank 1 only:
/// the device sees a flat pool, and only a plain single-axis array has
/// `pool[k] == A(k)` -- a packed (sym/antisym), compound or ragged axis
/// addresses through machinery the device side does not carry.
///
/// A NON-LITERAL extent is no longer a refusal: the host wrapper is an
/// `Array<T,1>`, so its own extents table names the count at the launch site,
/// and it rides into the .cu as the `<devName>_n` parameter. The shape gate is
/// unchanged -- plain, dense, rank <= 1, non-virtual -- only the *knowability*
/// of the count relaxed. Rank 0 (`ix.Rank = 0`, a scalar-shaped slot) keeps
/// `extents[0]` as its count for the same reason the static arm used the type's
/// single extent: the pool is still one contiguous axis.
let internal cudaFwdArrayShape (hostName: string) (devName: string) (ty: IRType)
        : (CudaCells * string * string) option =
    match ty with
    | ArrayElem arr when isCudaBoundarySafeElem arr.ElemType && not arr.IsVirtual ->
        match arr.IndexTypes with
        | [ix] when ix.Rank <= 1 && ix.Symmetry = SymNone && ix.Kind = SDimension && ix.IxKind = IxKPlain ->
            let cells =
                match ix.Extent with
                | IRLit (IRLitInt n) -> CellsStatic n
                | _ -> CellsRuntime ($"{hostName}.extents[0]", $"{devName}_n")
            Some (cells, elemTypeToCpp arr.ElemType, cudaDevElemTypeToCpp arr.ElemType)
        | _ -> None
    | _ -> None

// ---------------------------------------------------------------------------
// Device-body planning -- section 2 of 2
// ---------------------------------------------------------------------------
//
// A `where cuda` kernel whose body MATERIALIZES an array (`let c = cos <@> (w *
// t); prodsum(s, c)`) has no inline expression form, so the S2 router turns it
// into a call to its lifted host function. That call cannot cross into the .cu,
// but the body itself can: every array it names is either a forwarded capture
// (a device pointer) or a POINTWISE producer over the same index space, and
// the space is consumed by folds. So the device form is one fused loop per
// fold -- which is also what a hand-written kernel would do, and needs no
// per-thread temporary buffer for the intermediate rows at all.
//
// The gate is deliberately narrow (rank-1 static extents, pointwise producers,
// `prodsum` folds, expression-shaped helper lambdas). Anything outside it
// returns `Error <reason>`, and the caller falls back to the host loop.

/// A device-side array value: how many cells it has, its element type, and how
/// to render its element at an index variable. A forwarded capture renders as
/// `ptr[k]`; a pointwise producer renders as its kernel applied to its sources'
/// elements at the same `k` -- so a chain of maps composes without ever being
/// materialized.
type internal DevArray = {
    Cells: CudaCells
    Elem: IRType
    At: string -> string
}

/// Beta-reduce every call to a resolvable lifted/user callable, so the device
/// renderer never has to name a host function. A callee's CAPTURES stay free
/// after substitution (their ids are the enclosing bindings') and resolve
/// through the caller's environment, which is exactly the discipline the host
/// side uses when it forwards them as extra arguments.
let rec internal cudaInlineCalls (fuel: int) (expr: IRExpr) : IRExpr =
    if fuel <= 0 then expr
    else
        mapIRExpr (fun e ->
            match e with
            | IRApp ((IRVar (fid, _) as callee), args, _) ->
                // Resolve the CALLEE, not the application: `resolveCallable`
                // also sees through let-aliases, and the `c.Id = fid` guard is
                // what keeps this to a direct lifted-callable reference (the
                // same pairing exprToCppCore's IRApp arm uses).
                match resolveCallable callee with
                | Some c when c.Id = fid && c.Params.Length = args.Length
                              && kernelBodyIsExpressionShaped c.Body ->
                    let m =
                        List.zip (c.Params |> List.map (_.VarId)) args |> Map.ofList
                    cudaInlineCalls (fuel - 1) (substituteIRVars m c.Body)
                | _ -> e
            | _ -> e) expr

/// Nodes a device SCALAR expression may contain once calls are inlined and
/// folds hoisted. A whitelist, not a blacklist: an unrecognized node means the
/// emitter has no evidence it renders as valid device code, so it refuses.
let rec internal cudaScalarNodeOk (e: IRExpr) : bool =
    match e with
    | IRLit _ | IRVar _ -> true
    | IRBinOp (IRElementwise, _, l, r) -> cudaScalarNodeOk l && cudaScalarNodeOk r
    | IRUnaryOp (_, x) -> cudaScalarNodeOk x
    | IRComplex (re, im) -> cudaScalarNodeOk re && cudaScalarNodeOk im
    | IRFma (a, b, c) -> cudaScalarNodeOk a && cudaScalarNodeOk b && cudaScalarNodeOk c  // device fma()
    | IRIf (c, t, f) -> cudaScalarNodeOk c && cudaScalarNodeOk t && cudaScalarNodeOk f
    | _ -> false

/// Plan the device rendering of a kernel body, forwarding whatever captures it
/// needs. `paramFinalNames` maps each kernel parameter id to the device local
/// holding its peeled operand read; `outerNames` is the host scope's name map
/// (for `captureForwardName`); `stem` disambiguates generated locals per kernel.
///
/// The simple case -- a body that already renders as one device expression over
/// its operands -- returns the same text as before, with no captures, so every
/// kernel that compiled before this planner existed still emits byte-identically.
let planCudaDeviceBody
        (codeGen: LoopNestCodeGen)
        (paramFinalNames: Map<IRId, string>)
        (outerNames: Map<IRId, string>)
        (stem: string)
        : Result<CudaDeviceBody, string> =
    let declared = paramFinalNames |> Map.toList |> List.map fst |> Set.ofList
    match cudaDeviceBodyRefusal declared paramFinalNames codeGen.KernelExpr with
    | None ->
        let r =
            withCudaDeviceDialect (fun () ->
                genKernelExprWithReynolds codeGen.KernelExpr codeGen.KernelParams
                                          false false paramFinalNames paramFinalNames)
        Ok { Captures = []; Stmts = []; Expr = r.CppExpr }
    | Some plainReason ->

    // The materializing form: the S2 router replaced the body with a call to
    // its lifted callable, so the callable IS the body -- reopen it and emit it
    // into the device kernel directly.
    let routed =
        match codeGen.KernelExpr with
        | IRApp (IRVar (fid, _) as f, args, _) ->
            match resolveCallable f with
            | Some c when c.Id = fid && c.Params.Length = args.Length -> Some (c, args)
            | _ -> None
        | _ -> None
    match routed with
    | None -> Error plainReason
    | Some (callable, args) ->

    // Kernel parameters: the routed call passes `IRVar(param.VarId)` for each
    // param, and that id is what `paramFinalNames` is keyed on.
    let paramBinds =
        List.zip callable.Params args
        |> List.map (fun (p, a) ->
            match a with
            | IRVar (aid, _) -> Map.tryFind aid paramFinalNames |> Option.map (fun nm -> (p.VarId, nm))
            | _ -> None)
    if paramBinds |> List.exists Option.isNone then
        Error "a kernel parameter that is not a peeled operand read"
    elif not (isCudaBoundarySafeElem callable.RetType) then
        // `isCudaBoundarySafeElem` matches only boundary-crossing SCALARS
        // (through AnyPrimElem, so a unit-annotated one still counts), which is
        // exactly the condition here: an array- or tuple-valued kernel result
        // has no single cell to write.
        Error "a kernel result that is not a boundary-crossing scalar"
    else

    let mutable scalars : Map<IRId, string> = paramBinds |> List.choose id |> Map.ofList
    let mutable arrays : Map<IRId, DevArray> = Map.empty
    /// Kernel-local deferred maps, in BINDING order. A join's share locals are
    /// emitted in this order, so a producer reading an earlier producer is
    /// always declared after it. Forwarded captures are deliberately absent:
    /// they are memory reads, not recomputation.
    let mutable producerOrder : IRId list = []
    /// A join binding's per-leg accumulators, as synthetic ids already present
    /// in `scalars`. `get<i>` of that binding resolves through this map.
    let mutable joinLegs : Map<IRId, (IRId * IRType) list> = Map.empty
    let mutable fwd : CudaFwdCapture list = []
    let stmts = System.Collections.Generic.List<string>()
    let mutable err : string option = None
    let fail (r: string) = if err.IsNone then err <- Some r
    let mutable serial = 0
    let fresh (p: string) = serial <- serial + 1; $"__blade_{stem}_{p}{serial}"
    // Synthetic ids for hoisted fold accumulators. Far above any builder id, and
    // only ever placed in the local `scalars` map, so they cannot shadow real IR.
    let mutable synth = 1500000000
    let freshId () = synth <- synth + 1; synth

    // --- captures -> device parameters -------------------------------------
    // Device parameter names are derived from the SOURCE name (so the .cu reads
    // like the program), but two captures can share one -- a shadowed binding
    // closed over at both depths. The id disambiguates the second onwards, and
    // only then, so the common case keeps the readable spelling.
    let mutable devNamesUsed : Set<string> = Set.empty
    for cap in callable.Captures do
        let hostName = captureForwardName outerNames cap
        let devName =
            let stem' = $"__blade_cap_{(sanitizeCppName cap.Name)}"
            if Set.contains stem' devNamesUsed then $"{stem'}_{cap.Id}" else stem'
        devNamesUsed <- Set.add devName devNamesUsed
        match cudaFwdArrayShape hostName devName cap.Type with
        | Some (cells, hostCpp, devCpp) ->
            fwd <- fwd @ [ { HostName = hostName; DevName = devName; Cells = Some cells
                             HostCpp = hostCpp; DevCpp = devCpp } ]
            let elem = match cap.Type with ArrayElem arr -> arr.ElemType | t -> t
            arrays <- Map.add cap.Id { Cells = cells; Elem = elem
                                       At = fun k -> $"{devName}[{k}]" } arrays
        | None ->
            match cudaFwdScalarCpp cap.Type with
            | Some (hostCpp, devCpp) ->
                fwd <- fwd @ [ { HostName = hostName; DevName = devName; Cells = None
                                 HostCpp = hostCpp; DevCpp = devCpp } ]
                scalars <- Map.add cap.Id devName scalars
            | None ->
                fail (sprintf "the captured value '%s' (only dense rank-1 arrays and scalars \
with a compile-time shape are forwarded to the device)" hostName)

    // --- device rendering of a scalar expression ---------------------------
    // Calls are inlined first (no host function may be named), then join
    // projections are resolved to the accumulators they name, then folds are
    // hoisted into accumulator loops, then what is left must be plain device
    // arithmetic over names this kernel declares.
    let rec renderScalar (extra: Map<IRId, string>) (e: IRExpr) : string =
        let hoisted = hoistFolds (resolveJoinProjs (cudaInlineCalls 8 e))
        let env = extra |> Map.fold (fun acc k v -> Map.add k v acc) scalars
        if not (cudaScalarNodeOk hoisted) then
            fail "a scalar operation with no device form"
            ""
        else
            match deviceFreeVarIds hoisted |> Set.toList |> List.filter (fun i -> not (Map.containsKey i env)) with
            | id :: _ ->
                fail ($"""the value '{(Map.tryFind id outerNames |> Option.defaultValue ($"__v{id}"))}' (no device binding)""")
                ""
            | [] -> withCudaDeviceDialect (fun () -> exprToCpp env hoisted)

    /// Rewrite `get<i>` of a JOIN's tuple into a reference to that leg's own
    /// accumulator. A reduction join answers a FLAT `Tuple<k>`
    /// (docs/plan-reduction-joins.md section 1.3), so the projection index IS
    /// the leg index -- no nested-get chain to unwind, in either the `isFlat`
    /// or the structural spelling.
    and resolveJoinProjs (e: IRExpr) : IRExpr =
        mapIRExpr (fun x ->
            match x with
            | IRTupleProj (IRVar (tid, _), i, _) when err.IsNone ->
                match Map.tryFind tid joinLegs with
                | Some legIds when i >= 0 && i < List.length legIds ->
                    let (vid, ty) = List.item i legIds
                    IRVar (vid, ty)
                | Some legIds ->
                    fail ($"a join projection past its {List.length legIds} leg(s)"); x
                | None -> x
            | _ -> x) e

    /// Replace every `prodsum` over device arrays with a hoisted accumulator,
    /// emitting its fused loop into `stmts` first. `mapIRExpr` is bottom-up, so
    /// a fold's operands are already resolved when its node is reached.
    and hoistFolds (e: IRExpr) : IRExpr =
        mapIRExpr (fun x ->
            match x with
            | IRProdSum operands when operands.Length >= 2 && err.IsNone ->
                let resolved = operands |> List.map resolveArray
                if resolved |> List.exists Option.isNone then
                    fail "a prodsum operand that is not a device array"; x
                else
                    let ds = resolved |> List.choose id
                    let cells = (List.head ds).Cells
                    if ds |> List.exists (fun d -> not (cudaCellsCompatible d.Cells cells)) then
                        fail "a prodsum over arrays of differing extents"; x
                    else
                        // Accumulator type from the FIRST operand's element
                        // type, which is what the host IIFE uses (`%s __ps = 0`
                        // off `inferExprType (List.head args)`). Taking the
                        // prodsum node's own type instead would silently widen a
                        // real-headed mixed product the host truncates, and the
                        // two lanes have to agree cell for cell.
                        let ty = (List.head ds).Elem
                        let cpp = cudaDevElemTypeToCpp ty
                        let acc = fresh "acc"
                        let k = fresh "k"
                        stmts.Add ($"    {cpp} {acc} = {cpp}();")
                        stmts.Add ($$"""    for (size_t {{k}} = 0; {{k}} < {{(cudaCellsText cells)}}; {{k}}++) {""")
                        stmts.Add ($"""        {acc} += {(ds |> List.map (fun d -> d.At k) |> String.concat " * ")};""")
                        stmts.Add "    }"
                        let id = freshId ()
                        scalars <- Map.add id acc scalars
                        IRVar (id, ty)
            | _ -> x) e

    /// The REDUCTION JOIN on the device: k legs sharing one traversal, folded
    /// into k per-thread accumulators (registers), with each distinct named
    /// deferred operand evaluated ONCE per iteration into a per-thread `const`
    /// local. That is the same loop the host emitter builds
    /// (docs/plan-reduction-joins.md section 1.4) -- the surface DECLARED the
    /// sharing, so this emitter only has to express it, never rediscover it.
    ///
    /// Returns one (accumulator name, element type) per leg, in leg order.
    /// The single-leg (non-tuple) encoding routes here too: it is the same loop
    /// with k = 1, and the caller binds the scalar directly.
    and emitJoin (comp: IRExpr) (kernels: IRExpr list) (seeds: IRExpr list) : (string * IRType) list option =
        // Peel any binders the traversal carries (`peelCompLets`'s device twin)
        // so a let-wrapped fusion tree reaches the leaf walk.
        let rec peel (e: IRExpr) =
            match e with
            | IRLet (id, rhs, body) -> bindLet id rhs; (if err.IsSome then e else peel body)
            | IRCompute inner -> peel inner
            | other -> other
        let rec leavesOf (e: IRExpr) =
            match peel e with
            | IRFusion (l, r) -> leavesOf l @ leavesOf r
            | other -> [other]
        let leaves = leavesOf comp
        if err.IsSome then None
        elif leaves.Length <> kernels.Length || leaves.Length <> seeds.Length then
            fail "a join whose leg count disagrees with its fold kernels"; None
        else
        let folds = kernels |> List.map resolveCallable
        if folds |> List.exists (fun c -> match c with Some cb -> cb.Params.Length <> 2 | None -> true) then
            fail "a join fold kernel that is not a binary device callable"; None
        else
        let folds = folds |> List.map Option.get
        if folds |> List.exists (fun cb -> not (isCudaBoundarySafeElem cb.RetType)) then
            fail "a join accumulator whose type is not a device scalar"; None
        else
        // SHARE LOCALS. A named deferred map (`let ct = ts <@> ...`, no
        // `compute`) that this traversal names is evaluated ONCE per iteration
        // and read by every leg. Ordered by BINDING order, so a share that
        // reads an earlier share is declared after it; `producerOrder` is that
        // order by construction.
        //
        // Collected from the legs' OPERAND SLOTS specifically, not from every
        // var reference under `comp`. A share local is indexed by this join's
        // own loop variable, so it is only sound for a value that iterates this
        // join's axis -- which an operand slot is by construction and an
        // arbitrary reference is not. One level deep, matching the host rule
        // (a deferred map reading another deferred map shares the OUTER one,
        // whose text inlines the inner one once anyway).
        let rec operandIds (e: IRExpr) : IRId list =
            match e with
            | IRLet (_, _, body) -> operandIds body
            | IRCompute inner -> operandIds inner
            | IRApplyCombinator info -> info.Arrays |> List.choose (function IRVar (i, _) -> Some i | _ -> None)
            | IRApp (IRObjectFor _, [src], _) -> (match src with IRVar (i, _) -> [i] | _ -> [])
            | IRVar (i, _) -> [i]
            | _ -> []
        let named = leaves |> List.collect operandIds |> Set.ofList
        let shares = producerOrder |> List.filter (fun i -> Set.contains i named)
        let k = fresh "k"
        let accs = folds |> List.map (fun cb -> (fresh "jacc", cb.RetType))
        // Render every text BEFORE touching `stmts`: `renderScalar` hoists a
        // nested fold into `stmts` itself, and such a hoist has no correct
        // placement relative to a loop it would have to run inside. Snapshot
        // the count and refuse if one happened, rather than emit a kernel whose
        // accumulator loop reads a variable declared after it.
        let before = stmts.Count
        let seedTexts = seeds |> List.map (renderScalar Map.empty)
        // ORDER IS LOAD-BEARING: the shares are rebound BEFORE the legs are
        // resolved. `pointwise` closes over the DevArrays it was handed, so a
        // leg resolved first would have captured the producer's kernel and gone
        // on rendering `cos(w*ts[k])` inline no matter what the share local
        // said -- the loop would declare a shared value nothing reads, which is
        // exactly the "compiles, right answer, wrong loop" failure the CPU side
        // had to be caught doing (plan section 3 of the differential findings).
        let saved = shares |> List.map (fun sid -> (sid, arrays.[sid]))
        let shareLines =
            shares |> List.map (fun sid ->
                let d = arrays.[sid]
                let nm = fresh "shr"
                let text = d.At k
                // Rebind AFTER rendering this one: a producer must not read its
                // own local, but it may read every share declared before it.
                arrays <- Map.add sid { d with At = fun _ -> nm } arrays
                $"        const {(cudaDevElemTypeToCpp d.Elem)} {nm} = {text};")
        let legs = leaves |> List.map resolveArray
        let legCells =
            if legs |> List.exists Option.isNone then None
            else
                let ls = legs |> List.choose id
                if ls |> List.exists (fun d -> not (cudaCellsCompatible d.Cells (List.head ls).Cells))
                then None else Some ((List.head ls).Cells, ls)
        let legLines =
            match legCells with
            | None -> []
            | Some (_, ls) ->
                List.map3 (fun (acc, _) (cb: IRCallable) (leg: DevArray) ->
                    let extra =
                        Map.ofList [ ((List.item 0 cb.Params).VarId, acc)
                                     ((List.item 1 cb.Params).VarId, $"({(leg.At k)})") ]
                    $"        {acc} = {(renderScalar extra cb.Body)};") accs folds ls
        // RESTORE the producers. The share locals are scoped to the loop this
        // emitter is about to write, so leaving the rebinding in place would
        // let a LATER use of the same deferred map -- another join, a bare
        // `prodsum(ct, ts)` after this one -- render a name that is out of
        // scope by then, which nvcc reports as an undefined identifier long
        // after the host half has been told the kernel exists.
        for (sid, d) in saved do arrays <- Map.add sid d arrays
        match legCells with
        | None ->
            (if legs |> List.exists Option.isNone
             then fail "a join leg that is not a pointwise map over device arrays"
             else fail "a join over legs of differing extents")
            None
        | Some (cells, _) ->
        let accLines = legLines
        if err.IsSome then None
        elif stmts.Count <> before then
            fail "a join leg containing a nested fold"; None
        else
        for ((acc, ty), seed) in List.zip accs seedTexts do
            stmts.Add ($"    {(cudaDevElemTypeToCpp ty)} {acc} = {seed};")
        stmts.Add ($$"""    for (size_t {{k}} = 0; {{k}} < {{(cudaCellsText cells)}}; {{k}}++) {""")
        for l in shareLines do stmts.Add l
        for l in accLines do stmts.Add l
        stmts.Add "    }"
        Some accs

    /// Bind one kernel-local `let` into the device environment: an array-valued
    /// one is recorded as a producer and never materialized, a scalar one gets a
    /// device local. Shared by the body walk and by `resolveArray`, because a
    /// materializing body nests its whole chain inside the OUTER let's rhs
    /// (`let c = (let w' = w in let u = ... in cos <@> u)`), so the same binder
    /// has to be understood in both positions.
    and bindLet (id: IRId) (rhs: IRExpr) : unit =
        match rhs with
        // A REDUCTION JOIN's tuple is k device scalars, not one -- so it is
        // bound before the device-scalar test below, which a `Tuple<k>` could
        // never pass. `joinLegs` is what makes the downstream `get<i>`
        // projections resolve to the individual accumulators.
        | IRReduceCompute (comp, IRTuple ks, IRTuple ss) when ks.Length = ss.Length && ks.Length >= 2 ->
            match emitJoin comp ks ss with
            | Some accs ->
                let legIds =
                    accs |> List.map (fun (nm, ty) ->
                        let vid = freshId ()
                        scalars <- Map.add vid nm scalars
                        (vid, ty))
                joinLegs <- Map.add id legIds joinLegs
            | None -> ()
        // The one-leg encoding: `reduce(<deferred>, op, init)` in a kernel body
        // is the SAME loop at k = 1, and its value is a plain scalar.
        | IRReduceCompute (comp, kernel, seed) ->
            match emitJoin comp [kernel] [seed] with
            | Some [ (nm, _) ] -> scalars <- Map.add id nm scalars
            | _ -> ()
        | _ ->
        match inferExprType rhs with
        | ArrayElem _ ->
            match resolveArray rhs with
            | Some d ->
                arrays <- Map.add id d arrays
                // A kernel-local map is a PRODUCER: unlike a forwarded capture
                // (a memory read), re-reading it re-evaluates its kernel, so it
                // is exactly what a join's share locals exist to evaluate once.
                if not (List.contains id producerOrder) then producerOrder <- producerOrder @ [id]
            | None -> fail "a kernel-local array that is not a pointwise map"
        | ty when not (isCudaBoundarySafeElem ty) ->
            // Tuples, strings, function values: no device local to declare.
            fail "a kernel-local binding whose type is not a device scalar"
        | ty ->
            let s = renderScalar Map.empty rhs
            if err.IsNone then
                let nm = fresh "v"
                stmts.Add ($"    {(cudaDevElemTypeToCpp ty)} {nm} = {s};")
                scalars <- Map.add id nm scalars

    /// Resolve an array-valued expression to its device form: a forwarded
    /// capture, or a pointwise producer over one (never materialized).
    and resolveArray (e: IRExpr) : DevArray option =
        match e with
        | IRVar (id, _) -> Map.tryFind id arrays
        | IRLet (id, rhs, body) -> bindLet id rhs; (if err.IsSome then None else resolveArray body)
        | IRCompute inner -> resolveArray inner
        // Exactly ONE source axis. Several operands with no co-iteration record
        // are an OUTER product, whose result has a higher rank than a fused
        // rank-1 read of it would assume -- rendering that as a zip would be
        // silently wrong, so it is refused, not approximated.
        | IRApp (IRObjectFor oi, [src], _)
              when oi.OutputRank = 0 && oi.InputRanks |> List.forall ((=) 0) ->
            pointwise oi.Kernel [src]
        | IRApplyCombinator info
              when info.KernelOutputRank = 0
                   && info.KernelInputRanks |> List.forall ((=) 0)
                   && not info.HasReynolds
                   && (info.IsCoIteration || info.Arrays.Length = 1) ->
            pointwise info.Kernel info.Arrays
        | _ -> None

    /// A map of `kernel` over co-iterated `srcs`, as a renderer of the result's
    /// element at an index -- the producer form that is never materialized.
    and pointwise (kernel: IRExpr) (srcs: IRExpr list) : DevArray option =
        let ds = srcs |> List.map resolveArray
        if ds |> List.exists Option.isNone || List.isEmpty srcs then None
        else
            let ds = ds |> List.choose id
            let cells = (List.head ds).Cells
            if ds |> List.exists (fun d -> not (cudaCellsCompatible d.Cells cells)) then None
            else
                match resolveCallable kernel with
                | Some c when c.Params.Length = ds.Length
                              && isCudaBoundarySafeElem c.RetType ->
                    let elem = c.RetType
                    Some { Cells = cells
                           Elem = elem
                           At = fun k ->
                                    // The map kernel is rendered per element by
                                    // binding its params to the sources' element
                                    // TEXT: pure, so a repeated use just repeats
                                    // the (already-peeled) read.
                                    let extra =
                                        List.zip c.Params ds
                                        |> List.map (fun (p, d) -> (p.VarId, $"({(d.At k)})"))
                                        |> Map.ofList
                                    renderScalar extra c.Body }
                | _ -> None

    // --- walk the body's let-chain ------------------------------------------
    let rec walk (e: IRExpr) : string =
        if err.IsSome then "" else
        match e with
        | IRLet (id, rhs, body) -> bindLet id rhs; walk body
        | tail -> renderScalar Map.empty tail

    let expr = walk callable.Body
    match err with
    | Some r -> Error r
    | None -> Ok { Captures = fwd; Stmts = List.ofSeq stmts; Expr = expr }

/// Emit a CUDA kernel for any single S-dimension symmetry group of arity >= 2,
/// symmetric (inclusive simplex i0<=i1<=...) or antisymmetric (strict simplex
/// i0<i1<...) -- the GENERAL simplicial-unrank kernel, one path for every rank
/// via arity R and symmetry.
///
/// Per formalism Section 9.2/10.8, S-dims ARE the iteration structure: stored
/// compactly and flattened to a contiguous pool to cross the extern "C" device
/// boundary, the flat thread id unranks to the canonical S-tuple. (T-dims are
/// per-thread slices, not part of this addressing; requires scalar-leaf,
/// all-S, single-group output: T-rank 0.)
///
/// Device unrank is the combinatorial number system (proven against
/// lexicographic order, exact emitted form, R=2..5 both variants, in sandbox):
///   strict = (Symmetry = SymAntisymmetric)
///   neff   = strict ? N : N + R - 1          (inclusive maps to strict over N+R-1)
///   card   = C(neff, R)
///   unrank(t): x=0; rem=t
///     for pos in 0..R-1:
///       after = R - pos - 1
///       v = x; while (rem >= C(neff-1-v, after)) { rem -= C(...); v++ }
///       idx[pos] = strict ? v : (v - pos)     (de-shift for inclusive)
///       x = v + 1
/// One __device__ binomial helper __blade_binom is emitted per .cu.
///
/// Fold: symmetric = raw comm kernel; antisymmetric IS Reynolds
/// antisymmetrization. Storage: symmetric -> allocate<T, SYMM={1..1}>;
/// antisymmetric -> allocate_strict<T, SYMM={1..1}, STRICT={1..1}>.
///
/// `mpiRange` (the `where mpi, cuda(...)` hybrid): RANK-SCOPED launch -- the
/// kernel gains [lo, hi) flat cell-range params, the wrapper picks the rank's
/// device (rank % deviceCount), launches ceil((hi-lo)/block) blocks and copies
/// back only [lo, hi); host restores the full pool via the standard MPI
/// cell-range Allgatherv. The wrapper is dllexport'd: nvcc -shared builds a
/// self-contained MSVC DLL that g++/-lmsmpi links directly (the netcdf.dll
/// trick), avoiding cross-ABI object link. mpiRange=false is unaffected --
/// identical to the plain (non-hybrid) launch.
let genCudaKernelSimplicial (mpiRange: bool) (softSplit: bool) (codeGen: LoopNestCodeGen) (name: string) (blockSize: int) : string list option =
    // Detect a single S-dim symmetry group of arity >= 2 (sym or antisym).
    let grpOpt =
        match codeGen.OutputType with
        | ArrayElem arr ->
            match arr.IndexTypes with
            | [ix] when (max 1 ix.Rank) >= 2
                        && (ix.Symmetry = SymSymmetric || ix.Symmetry = SymAntisymmetric)
                        && ix.Kind = SDimension
                        && isCudaBoundarySafeElem arr.ElemType ->
                Some (arr.ElemType, ix.Extent, (max 1 ix.Rank), ix.Symmetry)
            | _ -> None
        | _ -> None
    if grpOpt.IsNone then None
    else
    let (outElemTy, extentExpr, grpRank, sym) = grpOpt.Value
    let strict = (sym = SymAntisymmetric)
    // Antisym requires the Reynolds antisymmetrization; symmetric is a raw comm.
    if strict && not (codeGen.HasReynolds && codeGen.IsAntisymmetric) then None
    elif (not strict) && codeGen.IsAntisymmetric then None
    else
    let nOpt = match extentExpr with IRLit (IRLitInt n) -> Some n | _ -> None
    if nOpt.IsNone then None
    else
    let n = nOpt.Value
    if codeGen.InputArrayNames.IsEmpty then None
    else
    let bindings = codeGen.Bindings
    if List.length bindings < grpRank then None
    else
    let srcName = match codeGen.InputArrayNames with n0 :: _ -> n0 | [] -> ""
    if srcName = "" then None
    else
    let elemCpp = elemTypeToCpp outElemTy
    // Device spellings (.cu internals): complex renders as thrust::complex.
    // Wrapper SIGNATURES keep the host (std::) spelling -- they are text-copied
    // into the host .cpp as prototypes. The source array's element type is
    // taken from the peeled elements (it can differ from the output's, e.g. a
    // complex source with a real-valued kernel).
    let devElemCpp = cudaDevElemTypeToCpp outElemTy
    let srcElemTy =
        bindings |> List.collect (_.Elements)
        |> List.tryHead |> Option.map (_.ArrayElemType)
        |> Option.defaultValue outElemTy
    let srcHostCpp = elemTypeToCpp srcElemTy
    let srcDevCpp = cudaDevElemTypeToCpp srcElemTy
    let r = int grpRank
    // card = C(neff, R), neff = strict ? N : N+R-1
    let neff = if strict then n else n + int64 r - 1L
    let binom (m: int64) (k: int) : int64 =
        if k < 0 || m < int64 k then 0L
        else
            let mutable num = 1L
            let mutable den = 1L
            for i in 0 .. k - 1 do
                num <- num * (m - int64 i)
                den <- den * int64 (i + 1)
            num / den
    let card = binom neff r
    let kernelName = $"__cuda_{(sanitizeCppName name)}"
    let launchName = $"__launch_{(sanitizeCppName name)}"
    // Per-level index variables idx[0..r-1] -> device names.
    let idxVarOf pos = $"__blade_idx_{pos}"
    // Operand reads keyed by elem.ParamVarId (the var-id the kernel body uses),
    // each binding level reading at its unranked index.
    let mutable paramFinalNames : Map<IRId, string> = Map.empty
    let readBinds =
        [ for b in bindings do
            let idxVar = idxVarOf b.Level
            // A FUSED joint level (arc 1) carries one element per source dim,
            // all sharing (Level, ArrayPosition) and the same ParamVarId. On
            // the DEVICE the operand is the flat pool, where the compound
            // index IS the row-major position -- a single flat read serves the
            // whole fused block (no per-dim decode needed, unlike the host
            // peel chain). Dedup so the read variable is declared once
            // (duplicate declarations were an nvcc redefinition error).
            for elem in b.Elements |> List.distinctBy (_.ArrayPosition) do
                let readName = $"__blade_op_{b.Level}_{elem.ArrayPosition}"
                let etStr = cudaDevElemTypeToCpp elem.ArrayElemType
                paramFinalNames <- Map.add elem.ParamVarId readName paramFinalNames
                yield $"    {etStr} {readName} = {srcName}[{idxVar}];" ]
    let nameMap =
        codeGen.Captures |> List.fold (fun acc c -> Map.add c.Id c.Name acc) paramFinalNames
    // CAPABILITY GATE. The simplicial kernel declares exactly the peeled operand
    // reads, so a body naming anything else (a capture, a lifted host function)
    // would emit an undefined identifier into the .cu. Refuse to the host loop
    // instead. Capture forwarding is implemented for the rectangular path only;
    // a simplicial nest reaching here with captures falls back, which is correct
    // (slower) rather than uncompilable.
    if (cudaDeviceBodyRefusal (paramFinalNames |> Map.toList |> List.map fst |> Set.ofList)
                              nameMap codeGen.KernelExpr).IsSome then None
    else
    // Antisym: Reynolds fold (true,true) emits the signed antisymmetrization.
    // Symmetric: raw comm kernel (false,false). Rendered in the CUDA device
    // dialect (thrust complex vocabulary) -- this is __global__ body text.
    let reynolds =
        withCudaDeviceDialect (fun () ->
            genKernelExprWithReynolds codeGen.KernelExpr codeGen.KernelParams strict strict nameMap paramFinalNames)
    // Device combinadic unrank loop. Emits idx_0..idx_{r-1} as absolute indices.
    //
    // Per level, the count of cells whose value is < v has the closed form
    //   cum(v) = C(neff-x, after+1) - C(neff-v, after+1)
    // (hockey-stick identity over C(neff-1-q, after) for q in [x, v); verified in
    // the sandbox against lexicographic order for r=2..5, both variants). Because
    // cum is monotonically increasing in v, each level brackets its value by
    // BINARY SEARCH in O(log n) rather than the O(n) linear scan, for a total
    // per-thread cost of O(r log n) at arbitrary rank.
    //
    // FUTURE O(1) OPTION (deferred until timing tests exist): no constant-time
    // closed form exists (inverting the combinatorial number system is
    // fundamentally a search), but precomputing the card x r canonical-tuple
    // table ONCE and having each thread load idx[pos] = table[t*r + pos]
    // trades O(r log n) arithmetic for a memory gather. Whether that wins
    // depends on r, n, reuse count and the target GPU's memory-vs-compute
    // balance, so it should be chosen by BENCHMARK, not assumed.
    let unrank =
        [ "    size_t __blade_t = __blade_i;"
          $"    long __blade_neff = {neff}L;"
          "    long __blade_x = 0;"
          "    long long __blade_rem = (long long)__blade_t;" ]
        @ [ for pos in 0 .. r - 1 do
              let after = r - pos - 1
              // binary search largest v in [x, neff] with cum(v) <= rem
              yield $"    long __blade_lo_{pos} = __blade_x; long __blade_hi_{pos} = __blade_neff; long __blade_vf_{pos} = __blade_x;"
              yield $"    long long __blade_base_{pos} = __blade_binom(__blade_neff - __blade_x, {after + 1});"
              yield  "    while (true) {"
              yield $"        if (__blade_lo_{pos} > __blade_hi_{pos}) break;"
              yield $"        long __blade_mid_{pos} = (__blade_lo_{pos} + __blade_hi_{pos}) / 2;"
              yield $"        long long __blade_cum_{pos} = __blade_base_{pos} - __blade_binom(__blade_neff - __blade_mid_{pos}, {after + 1});"
              yield $$"""        if (__blade_cum_{{pos}} <= __blade_rem) { __blade_vf_{{pos}} = __blade_mid_{{pos}}; __blade_lo_{{pos}} = __blade_mid_{{pos}} + 1; }"""
              yield $$"""        else { __blade_hi_{{pos}} = __blade_mid_{{pos}} - 1; }"""
              yield  "    }"
              yield $"    long long __blade_cumf_{pos} = __blade_base_{pos} - __blade_binom(__blade_neff - __blade_vf_{pos}, {after + 1});"
              yield $"    __blade_rem -= __blade_cumf_{pos};"
              // strict -> absolute v ; inclusive -> v - pos (de-shift)
              if strict then
                  yield $"    size_t {(idxVarOf pos)} = (size_t)__blade_vf_{pos};"
              else
                  yield $"    size_t {(idxVarOf pos)} = (size_t)(__blade_vf_{pos} - {pos});"
              yield $"    __blade_x = __blade_vf_{pos} + 1;" ]
    let kernelParams = $"const {srcDevCpp}* {srcName}"
    let kernelDef =
        if mpiRange then
            // Rank-scoped: thread t computes ABSOLUTE cell lo + t for
            // t in [0, hi - lo); the unrank below consumes __blade_i.
            [ $$"""__global__ void {{kernelName}}({{kernelParams}}, {{devElemCpp}}* __blade_out, size_t __blade_rlo, size_t __blade_rhi) {"""
              "    size_t __blade_tid = (size_t)blockIdx.x * blockDim.x + threadIdx.x;"
              "    if (__blade_tid >= __blade_rhi - __blade_rlo) return;"
              "    size_t __blade_i = __blade_rlo + __blade_tid;" ]
            @ unrank @ readBinds
            @ [ $"    __blade_out[__blade_i] = {reynolds.CppExpr};"; "}" ]
        else
            [ $$"""__global__ void {{kernelName}}({{kernelParams}}, {{devElemCpp}}* __blade_out, size_t __blade_card) {"""
              "    size_t __blade_i = (size_t)blockIdx.x * blockDim.x + threadIdx.x;"
              "    if (__blade_i >= __blade_card) return;" ]
            @ unrank @ readBinds
            @ [ $"    __blade_out[__blade_i] = {reynolds.CppExpr};"; "}" ]
    let wrapper =
        if mpiRange then
            // dllexport'd: the hybrid build ships the .cu as a self-contained
            // MSVC DLL the g++ host links directly.
            [ $"extern \"C\" __declspec(dllexport) void {launchName}(const {srcHostCpp}* {srcName}, {elemCpp}* __blade_host_out, size_t __blade_rlo, size_t __blade_rhi, int __blade_rank) {{"
              "    int __blade_dc = 1; cudaGetDeviceCount(&__blade_dc); if (__blade_dc < 1) __blade_dc = 1;"
              "    cudaSetDevice(__blade_rank % __blade_dc);"
              "    if (__blade_rhi <= __blade_rlo) return;"
              $"    size_t __blade_card = {card}UL;"
              $"    {srcDevCpp}* __blade_d_{srcName}; cudaMalloc(&__blade_d_{srcName}, {n}UL * sizeof({srcDevCpp}));"
              $"    cudaMemcpy(__blade_d_{srcName}, {srcName}, {n}UL * sizeof({srcDevCpp}), cudaMemcpyHostToDevice);"
              $"    {devElemCpp}* __blade_d_out; cudaMalloc(&__blade_d_out, __blade_card * sizeof({devElemCpp}));"
              $"    size_t __blade_blocks = ((__blade_rhi - __blade_rlo) + {blockSize}UL - 1UL) / {blockSize}UL;"
              $"    {kernelName}<<<(unsigned)__blade_blocks, {blockSize}>>>(__blade_d_{srcName}, __blade_d_out, __blade_rlo, __blade_rhi);"
              "    cudaDeviceSynchronize();"
              $"    cudaMemcpy(__blade_host_out + __blade_rlo, __blade_d_out + __blade_rlo, (__blade_rhi - __blade_rlo) * sizeof({devElemCpp}), cudaMemcpyDeviceToHost);"
              $"    cudaFree(__blade_d_{srcName});"
              "    cudaFree(__blade_d_out);"; "}" ]
        elif softSplit then
            // <&> soft-join split wrappers (see genCudaKernel's softSplit arm):
            // begin = H2D + ASYNC launch on a round-robin device, end = sync +
            // D2H + free. Device selection lives HERE (the g++ host half never
            // touches the CUDA API); one device => default-stream serialization.
            let sdPrefix = $"__blade_sd_{(sanitizeCppName name)}"
            [ $"static {srcDevCpp}* {sdPrefix}_d_src = nullptr;"
              $"static {devElemCpp}* {sdPrefix}_d_out = nullptr;"
              $"static int {sdPrefix}_dev = 0;"
              $"extern \"C\" void {launchName}_begin(const {srcHostCpp}* {srcName}, int __blade_leaf) {{"
              "    int __blade_dc = 1; cudaGetDeviceCount(&__blade_dc); if (__blade_dc < 1) __blade_dc = 1;"
              sprintf "    %s_dev = __blade_leaf %% __blade_dc;" sdPrefix
              $"    cudaSetDevice({sdPrefix}_dev);"
              $"    size_t __blade_card = {card}UL;"
              $"    cudaMalloc(&{sdPrefix}_d_src, {n}UL * sizeof({srcDevCpp}));"
              $"    cudaMemcpy({sdPrefix}_d_src, {srcName}, {n}UL * sizeof({srcDevCpp}), cudaMemcpyHostToDevice);"
              $"    cudaMalloc(&{sdPrefix}_d_out, __blade_card * sizeof({devElemCpp}));"
              $"    size_t __blade_blocks = (__blade_card + {blockSize}UL - 1UL) / {blockSize}UL;"
              $"    {kernelName}<<<(unsigned)__blade_blocks, {blockSize}>>>({sdPrefix}_d_src, {sdPrefix}_d_out, __blade_card);"
              "}"
              $"extern \"C\" void {launchName}_end({elemCpp}* __blade_host_out) {{"
              $"    cudaSetDevice({sdPrefix}_dev);"
              "    cudaDeviceSynchronize();"
              $"    cudaMemcpy(__blade_host_out, {sdPrefix}_d_out, {card}UL * sizeof({devElemCpp}), cudaMemcpyDeviceToHost);"
              $"    cudaFree({sdPrefix}_d_src);"
              $"    cudaFree({sdPrefix}_d_out);"
              "    cudaSetDevice(0);"; "}" ]
        else
            [ $"extern \"C\" void {launchName}(const {srcHostCpp}* {srcName}, {elemCpp}* __blade_host_out) {{"
              $"    size_t __blade_card = {card}UL;"
              $"    {srcDevCpp}* __blade_d_{srcName}; cudaMalloc(&__blade_d_{srcName}, {n}UL * sizeof({srcDevCpp}));"
              $"    cudaMemcpy(__blade_d_{srcName}, {srcName}, {n}UL * sizeof({srcDevCpp}), cudaMemcpyHostToDevice);"
              $"    {devElemCpp}* __blade_d_out; cudaMalloc(&__blade_d_out, __blade_card * sizeof({devElemCpp}));"
              $"    size_t __blade_blocks = (__blade_card + {blockSize}UL - 1UL) / {blockSize}UL;"
              $"    {kernelName}<<<(unsigned)__blade_blocks, {blockSize}>>>(__blade_d_{srcName}, __blade_d_out, __blade_card);"
              "    cudaDeviceSynchronize();"
              $"    cudaMemcpy(__blade_host_out, __blade_d_out, __blade_card * sizeof({devElemCpp}), cudaMemcpyDeviceToHost);"
              $"    cudaFree(__blade_d_{srcName});"
              "    cudaFree(__blade_d_out);"; "}" ]
    // Emit the __device__ binomial helper once per .cu. Idempotency is keyed on
    // the cell's own contents (race-safe: cudaKernelDefsCell is AsyncLocal, so each
    // program-assembly flow has its own cell, reset per program) rather than a
    // module-level mutable, which would not reset between programs and would race
    // under the parallel test runner.
    let cell = cudaKernelDefsCell ()
    let helperMarker = "__device__ static long long __blade_binom"
    let binomHelper =
        if cell.Exists (fun l -> l.StartsWith helperMarker) then []
        else
            [ "__device__ static long long __blade_binom(long m, long k) {"
              "    if (k < 0 || m < (long)k) return 0;"
              "    if (k == 0) return 1;"
              "    long long num = 1; long long den = 1;"
              "    for (long i = 0; i < k; i++) { num *= (m - i); den *= (i + 1); }"
              "    return num / den;"
              "}"
              "" ]
    cell.Append (binomHelper @ kernelDef @ [""] @ wrapper @ [""])
    // Host-side inline allocation matching the host storage:
    //   symmetric  -> allocate<T, SYMM={1..1}>
    //   antisym    -> allocate_strict<T, SYMM={1..1}, STRICT={1..1}>
    let extentsName = $"{name}_extents"
    let ones = List.replicate r 1
    let symmArg = hoistSymmDecl ($"{name}_symm") ones
    // The group extent came from an `IRLit (IRLitInt n)` gate above (a
    // non-literal extent is out of simplicial scope entirely), so every entry
    // of this table is a literal and it takes the static form unconditionally.
    // These are host `.cpp` lines -- the device text goes to cudaKernelDefsCell
    // -- so a function-local static is in scope-kind terms an ordinary one.
    let (extentDecls, ownedExtents) =
        emitExtentsTable "    " extentsName r [ for _ in 1 .. r -> ($"{n}UL", true) ]
    let strictArgOpt =
        if strict then Some (hoistSymmDecl ($"{name}_strict") ones) else None
    let allocLine =
        arrayAlloc { Ind = "    "; Elem = elemCpp; Rank = r; Name = name
                     Symm = symmArg; Strict = strictArgOpt; Extents = extentsName }
    // dealloc(D): the HOST packed output (its extents table is static, so
    // `ownedExtents` is None and no `delete[]` is registered for it). The
    // device buffers (cudaMalloc/cudaFree) are untouched -- they are already
    // paired inside the .cu wrapper. Deferred to the two Some-returning arms
    // below so a `None` (no MPI datatype) registers nothing, and skipped
    // entirely under `softSplit`, whose caller (tryGenCudaSoftJoin) abandons
    // every piece when ANY leaf turns out ineligible -- registering there would
    // emit frees for C++ names that never got declared.
    let registerHostOutput () =
        if not softSplit then
            registerArrayAlloc name elemCpp r symmArg strictArgOpt ownedExtents
    if mpiRange then
        // Rank-scoped launch: balanced flat cell-range split (the same
        // q/rem the MPI host path uses), per-rank device launch over
        // [lo, hi), then cell-range Allgatherv restores the full pool.
        let mpiDtype =
            match outElemTy with
            | AnyPrimElem et -> mpiDatatypeOf et
            | _ -> None
        match mpiDtype with
        | None -> None  // no MPI datatype => not hybrid-eligible
        | Some dtype ->
            let split =
                [ $"    size_t __blade_mpi_n_{name} = {card}UL;"
                  $"    size_t __blade_mpi_q_{name} = __blade_mpi_n_{name} / (size_t)__blade_mpi_size;"
                  sprintf "    size_t __blade_mpi_r_%s = __blade_mpi_n_%s %% (size_t)__blade_mpi_size;" name name
                  $"    size_t __blade_mpi_lo_{name} = (size_t)__blade_mpi_rank * __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? (size_t)__blade_mpi_rank : __blade_mpi_r_{name});"
                  $"    size_t __blade_mpi_hi_{name} = __blade_mpi_lo_{name} + __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? 1 : 0);" ]
            let launch =
                // Ranks launch concurrently: the sections are independent
                // (each writes its own [lo, hi) slice; the CUDA driver
                // time-slices a shared device) and the Allgatherv below is
                // the only cross-rank dependency. (Bring-up used a token-
                // ring serialization here; removed once the differential
                // passed, and re-verified without it.)
                [ $"    {launchName}(pool_base({srcName}.data), pool_base({name}.data), __blade_mpi_lo_{name}, __blade_mpi_hi_{name}, __blade_mpi_rank);" ]
            let gather =
                [ $$"""    { // MPI: restore full {{name}} on all ranks (device ranges)"""
                  $"        if (__blade_mpi_n_{name} > 2147483647ULL) {{ std::cerr << \"error[BL8004]: element count exceeds int32 range (rank \" << __blade_mpi_rank << \")\" << std::endl; MPI_Abort(MPI_COMM_WORLD, 13); }}"
                  "        int* __blade_mpi_counts = new int[__blade_mpi_size];"
                  "        int* __blade_mpi_displs = new int[__blade_mpi_size];"
                  "        for (int __r = 0; __r < __blade_mpi_size; __r++) {"
                  $"            size_t __lo = (size_t)__r * __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? (size_t)__r : __blade_mpi_r_{name});"
                  $"            size_t __hi = __lo + __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? 1 : 0);"
                  "            __blade_mpi_counts[__r] = (int)(__hi - __lo);"
                  "            __blade_mpi_displs[__r] = (int)__lo;"
                  "        }"
                  $"        MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, pool_base({name}.data), __blade_mpi_counts, __blade_mpi_displs, {dtype}, MPI_COMM_WORLD);"
                  "        delete[] __blade_mpi_counts; delete[] __blade_mpi_displs;"
                  "    }" ]
            registerHostOutput ()
            Some (extentDecls @ [allocLine] @ split @ launch @ gather)
    else
    let inlineLines =
        extentDecls
        @ [ allocLine ]
        // Soft-join caller sequences the begin/end calls itself; return the
        // host output allocation only.
        @ (if softSplit then []
           else [ $"    {launchName}(pool_base({srcName}.data), pool_base({name}.data));" ])
    registerHostOutput ()
    Some inlineLines

/// MPI flat-cell-range decomposition of a single-S-group symmetric or
/// antisymmetric nest (the triangular sibling of the dense MpiSlab path).
/// The packed pool of C(n+r-1, r) (sym) / C(n, r) (antisym) cells is split
/// into balanced contiguous ranges [lo, hi) per rank; each rank unranks its
/// cells back to canonical coordinates via linearized_storage's host
/// unlinearize (O(r log n) bisection -- the same combinadics the CUDA
/// simplicial kernel does on device; those passing differentials pin that
/// linearized_storage's canonical order == allocate<>'s pool DFS order), and
/// the Allgatherv over cell ranges restores the full pool on all ranks.
/// Same shape gates as genCudaKernelSimplicial (with the elem gate swapped
/// for the MPI-datatype requirement); None = out of scope, the caller
/// decides whether that is an error. `innerOmp` (the `where mpi, omp(...)`
/// hybrid) threads each rank's flat cell-range: a bare `parallel for` on
/// the cell loop -- each cell unranks and writes independently, so the
/// pragma is race-free by construction (no collapse/schedule decision to
/// make on a single flat loop).
let genMpiNestSimplicial (innerOmp: bool) (codeGen: LoopNestCodeGen) (name: string) : string list option =
    // Detect a single S-dim symmetry group of arity >= 2 (sym or antisym).
    let grpOpt =
        match codeGen.OutputType with
        | ArrayElem arr ->
            match arr.IndexTypes with
            | [ix] when (max 1 ix.Rank) >= 2
                        && (ix.Symmetry = SymSymmetric || ix.Symmetry = SymAntisymmetric)
                        && ix.Kind = SDimension ->
                match arr.ElemType with
                | AnyPrimElem et when (mpiDatatypeOf et).IsSome ->
                    Some (arr.ElemType, (mpiDatatypeOf et).Value, ix.Extent, (max 1 ix.Rank), ix.Symmetry)
                | _ -> None
            | _ -> None
        | _ -> None
    match grpOpt with
    | None -> None
    | Some (outElemTy, mpiDtype, extentExpr, grpRank, sym) ->
    let strict = (sym = SymAntisymmetric)
    // Antisym requires the Reynolds antisymmetrization; symmetric is a raw comm.
    if strict && not (codeGen.HasReynolds && codeGen.IsAntisymmetric) then None
    elif (not strict) && codeGen.IsAntisymmetric then None
    else
    // Every peel must be a SIMPLE one: one leading-dim level per array
    // position (RankComponent 0, DimIndex 0). A packed-symmetric INPUT
    // (co-iteration) binds multiple rank components of one position to a
    // single kernel param -- its cell read needs linearize(idx...), which
    // this emitter does not do; without this guard the per-level reads
    // would silently overwrite each other. Rejecting -> caller's #error.
    let peelsAreSimple =
        codeGen.Bindings |> List.forall (fun b ->
            b.Elements |> List.forall (fun e -> e.RankComponent = 0 && e.DimIndex = 0))
    if not peelsAreSimple then None
    else
    match extentExpr with
    | IRLit (IRLitInt n) when not codeGen.InputArrayNames.IsEmpty
                              && List.length codeGen.Bindings >= int grpRank ->
        let elemCpp = elemTypeToCpp outElemTy
        let r = int grpRank
        let neff = if strict then n else n + int64 r - 1L
        let binom (m: int64) (k: int) : int64 =
            if k < 0 || m < int64 k then 0L
            else
                let mutable num = 1L
                let mutable den = 1L
                for i in 0 .. k - 1 do
                    num <- num * (m - int64 i)
                    den <- den * int64 (i + 1)
                num / den
        let card = binom neff r
        let nsName = if strict then "antisymmetric" else "symmetric"
        let idxVar = $"__blade_mpi_idx_{name}"
        // Operand reads keyed by elem.ParamVarId, each level reading its
        // array at the ABSOLUTE unranked coordinate (host arrays are in
        // scope by name -- unlike the device path, which streams one flat
        // pool and reads it positionally).
        let mutable paramFinalNames : Map<IRId, string> = Map.empty
        let readBinds =
            [ for b in codeGen.Bindings do
                for elem in b.Elements |> List.distinctBy (_.ArrayPosition) do
                    let readName = $"__blade_op_{b.Level}_{elem.ArrayPosition}"
                    paramFinalNames <- Map.add elem.ParamVarId readName paramFinalNames
                    // Scalar peel (rank-1 input) binds the element; FIBER
                    // peel (rank-2 input, e.g. comoment kernels over
                    // per-variable observation rows) binds a sub-array
                    // WRAPPER sharing the parent's extents -- same pattern
                    // as genLoopNest's host peel -- so kernel intrinsics
                    // (prodsum's `.extents[0]` bound) keep working.
                    let resultRank = elem.ArrayRank - 1
                    if resultRank <= 0 then
                        yield $"        auto {readName} = {elem.ArrayName}[{idxVar}[{b.Level}]];"
                    else
                        yield sprintf "        Array<%s, %d> %s = { %s.data[%s[%d]], %s.extents + 1 };"
                                  (elemTypeToCpp elem.ArrayElemType) resultRank readName
                                  elem.ArrayName idxVar b.Level elem.ArrayName ]
        let nameMap =
            codeGen.Captures |> List.fold (fun acc c -> Map.add c.Id c.Name acc) paramFinalNames
        // Antisym: Reynolds fold (true,true) emits the signed
        // antisymmetrization; symmetric: raw comm kernel (false,false).
        let reynolds =
            genKernelExprWithReynolds codeGen.KernelExpr codeGen.KernelParams strict strict nameMap paramFinalNames
        // Host packed allocation -- identical to the CUDA simplicial path:
        // symmetric -> allocate<T, SYMM={1..1}>, antisym -> allocate_strict.
        let extentsName = $"{name}_extents"
        let ones = List.replicate r 1
        let symmArg = hoistSymmDecl ($"{name}_symm") ones
        // Same literal-only story as the CUDA simplicial peel: this arm is
        // reached only through the `IRLit (IRLitInt n)` match on the group
        // extent, so the table is all-literal and takes the static form.
        let (extentDecls, ownedExtents) =
            emitExtentsTable "    " extentsName r [ for _ in 1 .. r -> ($"{n}UL", true) ]
        let strictArgOpt =
            if strict then Some (hoistSymmDecl ($"{name}_strict") ones) else None
        let allocLine =
            arrayAlloc { Ind = "    "; Elem = elemCpp; Rank = r; Name = name
                         Symm = symmArg; Strict = strictArgOpt; Extents = extentsName }
        // Balanced cell-range split over the packed pool. Cell counts are
        // near-equal by construction (contiguous flat ranges), unlike an
        // outer-row slab of a triangle. Same q/rem formula as the dense slab.
        let split =
            [ $"    size_t __blade_mpi_n_{name} = {card}UL;"
              $"    size_t __blade_mpi_q_{name} = __blade_mpi_n_{name} / (size_t)__blade_mpi_size;"
              sprintf "    size_t __blade_mpi_r_%s = __blade_mpi_n_%s %% (size_t)__blade_mpi_size;" name name
              $"    size_t __blade_mpi_lo_{name} = (size_t)__blade_mpi_rank * __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? (size_t)__blade_mpi_rank : __blade_mpi_r_{name});"
              $"    size_t __blade_mpi_hi_{name} = __blade_mpi_lo_{name} + __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? 1 : 0);" ]
        let loop =
            [ $"    {elemCpp}* __blade_mpi_out_{name} = nested_array_utilities::pool_base({name}.data);" ]
            // BUILD KNOB gates the OMP HALF OF THE HYBRID ONLY. The rank
            // decomposition above (`__blade_mpi_lo/hi`, the Allgatherv below) is
            // MPI and is out of scope: `BLADE_OMP_THREADS` says nothing about
            // how many RANKS this build uses, only whether a rank's local work
            // is threaded. So `where mpi, omp(...)` under serial emission is
            // exactly `where mpi`. See `ompThreadEmissionEnabled`.
            @ (if innerOmp && ompThreadEmissionEnabled () then [ "    #pragma omp parallel for" ]
               elif innerOmp then [ $"    // [omp] requested but emitted serial: {(ompThreadsSuppressedReason ())}" ]
               else [])
            @ [ $$"""    for (size_t __blade_c = __blade_mpi_lo_{{name}}; __blade_c < __blade_mpi_hi_{{name}}; __blade_c++) {"""
                // Per-cell unrank (O(r log n)). Odometer advance -- unrank once
                // at lo, then increment lexicographically -- is the amortized-
                // O(1) upgrade once timing tests exist.
                $"        auto {idxVar} = linearized_storage::{nsName}::unlinearize<{r}>(__blade_c, {n}UL);" ]
            @ readBinds
            @ [ $"        __blade_mpi_out_{name}[__blade_c] = {reynolds.CppExpr};"
                "    }" ]
        let gather =
            [ $$"""    { // MPI: restore full {{name}} on all ranks"""
              $"        if (__blade_mpi_n_{name} > 2147483647ULL) {{ std::cerr << \"error[BL8004]: element count exceeds int32 range (rank \" << __blade_mpi_rank << \")\" << std::endl; MPI_Abort(MPI_COMM_WORLD, 13); }}"
              "        int* __blade_mpi_counts = new int[__blade_mpi_size];"
              "        int* __blade_mpi_displs = new int[__blade_mpi_size];"
              "        for (int __r = 0; __r < __blade_mpi_size; __r++) {"
              $"            size_t __lo = (size_t)__r * __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? (size_t)__r : __blade_mpi_r_{name});"
              $"            size_t __hi = __lo + __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? 1 : 0);"
              "            __blade_mpi_counts[__r] = (int)(__hi - __lo);"
              "            __blade_mpi_displs[__r] = (int)__lo;"
              "        }"
              $"        MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, __blade_mpi_out_{name}, __blade_mpi_counts, __blade_mpi_displs, {mpiDtype}, MPI_COMM_WORLD);"
              "        delete[] __blade_mpi_counts; delete[] __blade_mpi_displs;"
              "    }" ]
        // dealloc(D): the MPI simplicial HOST packed output (extents static,
        // so nothing extra to free). Every rank keeps the full pool after the
        // Allgatherv, so this is an ordinary scope-owned array; the
        // counts/displs tables above already delete themselves and are left
        // alone.
        registerArrayAlloc name elemCpp r symmArg strictArgOpt ownedExtents
        Some (extentDecls @ [allocLine] @ split @ loop @ gather)
    | _ -> None

/// Emit a CUDA kernel for the first-kernel scope (rectangular pointwise,
/// boundary-safe scalar elements, single-chunk synchronous). Returns
///   Some inlineLaunchLines  when emitted: the __global__ kernel + its
///     extern "C" launch wrapper are appended to cudaKernelDefsCell (destined
///     for the .cu file); the returned lines are the inline .cpp host code.
///   None  when out of scope (caller falls back to the host loop).
/// Gates: every binding rectangular const-extent RealArray scalar-leaf; array
/// output with boundary-safe elem type; no Reynolds. Only flat T*/size_t cross
/// the extern "C" boundary (pool_base supplies flat host pointers).
let genCudaKernel (softSplit: bool) (outerNames: Map<IRId, string>) (codeGen: LoopNestCodeGen) (name: string) (blockSize: int) : string list option =
    let bindings = codeGen.Bindings
    let nDims = List.length bindings
    /// A level's extent as HOST C++, when the level is a plain dense axis whose
    /// bound is a pure function of its own array's shape. `None` means the
    /// launch grid cannot be computed at all -- which is a genuine refusal, and
    /// a different thing from "not known until run time".
    ///
    /// A LITERAL keeps its literal spelling everywhere downstream, so a kernel
    /// whose extents were static before this existed emits byte-identically.
    let hostExtentOf (b: LoopIndexBinding) : CudaCells option =
        match b.Extent with
        | IRLit (IRLitInt n) -> Some (CellsStatic n)
        | _ when System.String.IsNullOrEmpty b.ExtentArrayRef -> None
        | _ ->
            // The same expression `emitExtentsTable` renders for this level (a
            // fused level's bound is the product of the dims it spans), so the
            // grid and the output's own extents table cannot disagree.
            let e =
                match b.FusedRank with
                | Some d -> [0 .. d - 1] |> List.map (sprintf "%s.extents[%d]" b.ExtentArrayRef) |> String.concat " * "
                | None -> $"{b.ExtentArrayRef}.extents[{b.ExtentDimRef}]"
            Some (CellsRuntime (e, $"__blade_ext{b.Level}"))
    let dimCells = bindings |> List.map hostExtentOf
    // RECTANGULARITY is about the SHAPE of the iteration space, not about when
    // its size is known: a dependent bound (`for j < i`), a strict offset, or a
    // virtual (range/reverse) operand has no flat thread grid at any time. A
    // plain dense axis always does -- `(n + block - 1) / block` is computable at
    // the launch, so a RUNTIME extent (every kernel inside a generic function)
    // is emitted, not refused.
    let rectOk =
        (dimCells |> List.forall Option.isSome)
        && (bindings |> List.forall (fun b ->
                b.BoundDependencies.IsEmpty && b.StrictOffset = 0
                && (b.Elements |> List.forall (_.Virtual.IsRealArray))))
    /// Every input must be indexed by at least one level, because that is where
    /// its POOL SIZE comes from (`inputCellsText`). An input reachable by no
    /// level has no size this emitter can derive, and guessing one would size a
    /// `cudaMemcpy` off the host pool by something other than its length.
    let inputsSized =
        codeGen.InputArrayNames |> List.mapi (fun pos _ -> pos)
        |> List.forall (fun pos ->
            bindings |> List.exists (fun b -> b.Elements |> List.exists (fun e -> e.ArrayPosition = pos)))
    let allStatic = dimCells |> List.forall (function Some (CellsStatic _) -> true | _ -> false)
    let outElemTyOpt =
        match codeGen.OutputType with
        | ArrayElem arr when isCudaBoundarySafeElem arr.ElemType -> Some arr.ElemType
        | _ -> None
    // A `where cuda` kernel that ends up on the host must SAY SO -- a device
    // request is not something to drop silently (same rule the `omp` census
    // marker enforces). This is the last emitter the CUDA dispatch tries, so
    // its refusal is the whole dispatch's refusal and the only one worth a
    // message; the simplicial path declining just means "not that shape".
    let refuseToHost (reason: string) : string list option =
        (exprWarningsCell ()).Value <-
            (exprWarningsCell ()).Value @
            [ sprintf "[cuda] a `where cuda` kernel ran on the HOST instead: the device \
kernel cannot express %s (nest '%s')" reason name ]
        None
    let shapeRefusal =
        if codeGen.HasReynolds then Some "a Reynolds (symmetrized) kernel over a rectangular nest"
        elif not rectOk then
            Some "an iteration space that is not rectangular \
(a dependent, offset or virtual axis has no flat thread grid)"
        elif outElemTyOpt.IsNone then
            Some "an output element type that cannot cross the extern \"C\" device boundary"
        elif not inputsSized then
            Some "an operand no loop level indexes (its device buffer has no derivable size)"
        elif softSplit && not allStatic then
            // The soft-join begin/end wrappers stage through FILE-STATIC device
            // buffers sized at their declaration; a runtime extent has nothing
            // to size them with there. The plain (non-split) launch below has no
            // such constraint -- it allocates inside the wrapper.
            Some "a runtime-sized axis under a <&> soft-join leaf (its begin/end \
wrappers bake the staging sizes)"
        else None
    match shapeRefusal with
    | Some r -> refuseToHost r
    | None ->
    let outElemTy = outElemTyOpt.Value
    let elemCpp = elemTypeToCpp outElemTy
    // Device spellings (.cu internals): complex renders as thrust::complex;
    // wrapper SIGNATURES keep the host (std::) spelling (they are text-copied
    // into the host .cpp as prototypes). Inputs are typed PER POSITION from
    // the peeled elements -- a kernel can mix e.g. a complex and a real input,
    // and the output element type is not the input's in general.
    let devElemCpp = cudaDevElemTypeToCpp outElemTy
    let inputElemTys =
        codeGen.InputArrayNames |> List.mapi (fun pos _ ->
            bindings
            |> List.collect (_.Elements)
            |> List.tryFind (fun e -> e.ArrayPosition = pos)
            |> Option.map (_.ArrayElemType)
            |> Option.defaultValue outElemTy)
    let inputHostCpp = inputElemTys |> List.map elemTypeToCpp
    let inputDevCpp = inputElemTys |> List.map cudaDevElemTypeToCpp
    // Per-level cell counts, literal or runtime, in nesting order. `rectOk`
    // established every entry is Some.
    let dims = dimCells |> List.map Option.get
    let dimText (i: int) = cudaCellsText (List.item i dims)
    let extentLits =
        bindings |> List.map (fun b -> match b.Extent with IRLit (IRLitInt n) -> n | _ -> 0L)
    /// Grid cardinality: the literal product when every extent is static (the
    /// text this emitter has always produced), else the runtime product of the
    /// extent parameters.
    let cardinalityText =
        if allStatic then $"{extentLits |> List.fold (fun a n -> a * n) 1L}UL"
        else [0 .. nDims - 1] |> List.map dimText |> String.concat " * "
    /// An input's POOL SIZE, for staging it onto the device: the product of the
    /// extents of the levels that actually index it.
    ///
    /// Derived rather than read off `extentLits.[pos]` (this emitter's original
    /// spelling), which silently assumed input `i` is indexed by level `i` and
    /// nothing else. It agrees with that assumption wherever the assumption
    /// held -- one input per level, in order -- and is right where it did not:
    /// a co-iterated `zip(A, B)` has two inputs on ONE level (and indexed past
    /// the end of the list), a rank-2 input spans two.
    let inputCellsText (pos: int) : string option =
        let ds = bindings |> List.mapi (fun i b -> (i, b))
                 |> List.filter (fun (_, b) -> b.Elements |> List.exists (fun e -> e.ArrayPosition = pos))
        match ds with
        | [] -> None
        | ds -> Some (ds |> List.map (fst >> dimText) |> String.concat " * ")
    let kernelName = $"__cuda_{(sanitizeCppName name)}"
    let launchName = $"__launch_{(sanitizeCppName name)}"
    let mutable paramFinalNames : Map<IRId, string> = Map.empty
    let mutable currentNames : Map<int, string> =
        codeGen.InputArrayNames |> List.mapi (fun i n -> (i, n)) |> Map.ofList
    let bodyBinds =
        [ for b in bindings do
            let mutable declared : Set<string> = Set.empty
            for elem in b.Elements do
                let cur = Map.tryFind elem.ArrayPosition currentNames |> Option.defaultValue elem.ArrayName
                let newName = $"{cur}__{b.IndexName}"
                let etStr = cudaDevElemTypeToCpp elem.ArrayElemType
                currentNames <- Map.add elem.ArrayPosition newName currentNames
                paramFinalNames <- Map.add elem.ParamVarId newName paramFinalNames
                // Self-zip: two operand slots on the same (array, index) peel
                // to the identical declaration -- bind once (nvcc redefinition).
                if not (Set.contains newName declared) then
                    declared <- Set.add newName declared
                    yield $"    {etStr} {newName} = {cur}[{b.IndexName}];" ]
    // Kernel-body text lives in the __global__ -- rendered in the device dialect
    // by the planner, which also decides which captures have to travel with it.
    // `None` (and a captures-under-soft-split refusal, whose begin/end wrappers
    // do not carry extra buffers) means host fallback -- see planCudaDeviceBody.
    match planCudaDeviceBody codeGen paramFinalNames outerNames (sanitizeCppName name) with
    | Error reason -> refuseToHost reason
    | Ok plan when softSplit && not plan.Captures.IsEmpty ->
        refuseToHost "captures forwarded from a <&> soft-join leaf (its begin/end \
wrappers stage only the leaf's own operands)"
    | Ok plan ->
    let caps = plan.Captures
    // Extra kernel/wrapper parameters, in one order used by the kernel
    // signature, the wrapper signature, the launch, and the host call site.
    // A RUNTIME-sized array capture contributes its cell count right after its
    // pointer, so the three lists stay positionally aligned by construction.
    let capKernelParams =
        caps |> List.collect (fun c ->
            match c.Cells with
            | Some cells -> $"const {c.DevCpp}* {c.DevName}" :: Option.toList (cudaCellsParam cells)
            | None -> [ $"{c.DevCpp} {c.DevName}" ])
    let capWrapParams =
        caps |> List.collect (fun c ->
            match c.Cells with
            | Some cells -> $"const {c.HostCpp}* {c.DevName}" :: Option.toList (cudaCellsParam cells)
            | None -> [ $"{c.HostCpp} {c.DevName}" ])
    /// The argument each capture contributes at the launch, given a prefix for
    /// the device buffer of an array one (scalars pass straight through).
    let capLaunchArgs (bufPrefix: string) =
        caps |> List.collect (fun c ->
            match c.Cells with
            | Some cells ->
                $"{bufPrefix}{c.DevName}"
                // At the HOST call site a runtime count is its host expression;
                // INSIDE the wrapper it is already the parameter, so the same
                // list serves both (the wrapper forwards the name it was given).
                :: (match cells with
                    | CellsStatic _ -> []
                    | CellsRuntime (_, p) -> [ p ])
            | None -> [ c.DevName ])
    /// What the HOST passes for a capture: array pointer (staged by the
    /// wrapper from the host pool) plus, when runtime-sized, its cell count.
    let capHostArgs =
        caps |> List.collect (fun c ->
            match c.Cells with
            | Some cells ->
                $"pool_base({c.HostName}.data)" :: Option.toList (cudaCellsHostArg cells)
            | None -> [ c.HostName ])
    // Extent parameters for the runtime levels, in nesting order: the kernel
    // needs them to unrank its flat thread id, the wrapper to size the grid and
    // the staging copies. A fully static nest contributes none, so its
    // signature is unchanged.
    let extKernelParams = dims |> List.choose cudaCellsParam
    let extHostArgs = dims |> List.choose cudaCellsHostArg
    let recover =
        [ yield "    size_t __blade_g = __blade_i;"
          for i in (nDims - 1) .. -1 .. 0 do
            let e = dimText i
            let b = bindings.[i]
            yield sprintf "    size_t %s = __blade_g %% %s;" b.IndexName e
            if i > 0 then yield $"    __blade_g /= {e};" ]
    let kernelParams =
        ((codeGen.InputArrayNames, inputDevCpp) ||> List.map2 (fun n et -> $"const {et}* {n}"))
        @ capKernelParams @ extKernelParams |> String.concat ", "
    let kernelDef =
        // NOTE on naming: generated CUDA-internal identifiers use a `__blade_`
        // prefix. A plain `__out` collides with MSVC's SAL
        // annotation macro `__out` (sal.h, pulled in transitively on Windows),
        // which expands to nothing -- turning `__out[__i] = ...` into a stray
        // `[__i] = ...` that nvcc/cl rejected as a bad attribute. `__in/__inout/
        // __out` are MSVC macros; `__blade_*` cannot collide with SAL or other
        // implementation-reserved names.
        [ $$"""__global__ void {{kernelName}}({{kernelParams}}, {{devElemCpp}}* __blade_out, size_t __blade_card) {"""
          "    size_t __blade_i = (size_t)blockIdx.x * blockDim.x + threadIdx.x;"
          "    if (__blade_i >= __blade_card) return;" ]
        @ recover @ bodyBinds @ plan.Stmts
        @ [ $"    __blade_out[__blade_i] = {plan.Expr};"; "}" ]
    let wrapInParams =
        ((codeGen.InputArrayNames, inputHostCpp) ||> List.map2 (fun n et -> $"const {et}* {n}"))
        @ capWrapParams @ extKernelParams |> String.concat ", "
    let sdPrefix = $"__blade_sd_{(sanitizeCppName name)}"
    let wrapper =
        if softSplit then
            // <&> soft-join split wrappers: begin = H2D + ASYNC launch on a
            // round-robin device (leaf % deviceCount, queried HERE so the host
            // half never touches the CUDA API); end = per-device sync + D2H +
            // free. One device => the default stream serializes the leaves.
            [ for (n, et) in List.zip codeGen.InputArrayNames inputDevCpp -> $"static {et}* {sdPrefix}_d_{n} = nullptr;" ]
            @ [ $"static {devElemCpp}* {sdPrefix}_d_out = nullptr;"
                $"static int {sdPrefix}_dev = 0;"
                $"extern \"C\" void {launchName}_begin({wrapInParams}, int __blade_leaf) {{"
                "    int __blade_dc = 1; cudaGetDeviceCount(&__blade_dc); if (__blade_dc < 1) __blade_dc = 1;"
                sprintf "    %s_dev = __blade_leaf %% __blade_dc;" sdPrefix
                $"    cudaSetDevice({sdPrefix}_dev);"
                $"    size_t __blade_card = {cardinalityText};" ]
            @ [ for (i, n) in List.mapi (fun i n -> (i, n)) codeGen.InputArrayNames do
                  let sz = inputCellsText i |> Option.defaultValue cardinalityText
                  let et = inputDevCpp.[i]
                  yield $"    cudaMalloc(&{sdPrefix}_d_{n}, {sz} * sizeof({et}));"
                  yield $"    cudaMemcpy({sdPrefix}_d_{n}, {n}, {sz} * sizeof({et}), cudaMemcpyHostToDevice);" ]
            @ [ $"    cudaMalloc(&{sdPrefix}_d_out, __blade_card * sizeof({devElemCpp}));"
                $"    size_t __blade_blocks = (__blade_card + {blockSize}UL - 1UL) / {blockSize}UL;"
                sprintf "    %s<<<(unsigned)__blade_blocks, %d>>>(%s, %s_d_out, __blade_card);" kernelName blockSize
                  (codeGen.InputArrayNames |> List.map (fun n -> $"{sdPrefix}_d_{n}") |> String.concat ", ") sdPrefix
                "}"
                $"extern \"C\" void {launchName}_end({elemCpp}* __blade_host_out) {{"
                $"    cudaSetDevice({sdPrefix}_dev);"
                "    cudaDeviceSynchronize();"
                $"    cudaMemcpy(__blade_host_out, {sdPrefix}_d_out, {cardinalityText} * sizeof({devElemCpp}), cudaMemcpyDeviceToHost);" ]
            @ [ for n in codeGen.InputArrayNames -> $"    cudaFree({sdPrefix}_d_{n});" ]
            @ [ $"    cudaFree({sdPrefix}_d_out);"
                "    cudaSetDevice(0);"; "}" ]
        else
        [ $"extern \"C\" void {launchName}({wrapInParams}, {elemCpp}* __blade_host_out) {{"
          $"    size_t __blade_card = {cardinalityText};" ]
        @ [ for (i, n) in List.mapi (fun i n -> (i, n)) codeGen.InputArrayNames do
              let sz = inputCellsText i |> Option.defaultValue cardinalityText
              let et = inputDevCpp.[i]
              yield $"    {et}* __blade_d_{n}; cudaMalloc(&__blade_d_{n}, {sz} * sizeof({et}));"
              yield $"    cudaMemcpy(__blade_d_{n}, {n}, {sz} * sizeof({et}), cudaMemcpyHostToDevice);" ]
        // Forwarded captures travel exactly as the operands do: an array capture
        // gets its own device buffer staged here (the wrapper takes the host
        // pool pointer), a scalar one rides the signature by value.
        @ [ for c in caps do
              match c.Cells with
              | Some cells ->
                  let sz = cudaCellsText cells
                  yield $"    {c.DevCpp}* __blade_d_{c.DevName}; cudaMalloc(&__blade_d_{c.DevName}, {sz} * sizeof({c.DevCpp}));"
                  yield $"    cudaMemcpy(__blade_d_{c.DevName}, {c.DevName}, {sz} * sizeof({c.DevCpp}), cudaMemcpyHostToDevice);"
              | None -> () ]
        @ [ $"    {devElemCpp}* __blade_d_out; cudaMalloc(&__blade_d_out, __blade_card * sizeof({devElemCpp}));"
            $"    size_t __blade_blocks = (__blade_card + {blockSize}UL - 1UL) / {blockSize}UL;"
            sprintf "    %s<<<(unsigned)__blade_blocks, %d>>>(%s, __blade_d_out, __blade_card);" kernelName blockSize
              ((codeGen.InputArrayNames |> List.map (sprintf "__blade_d_%s"))
               @ capLaunchArgs "__blade_d_"
               // The runtime extents the wrapper was handed, forwarded verbatim:
               // `cudaCellsParam` named them, so the parameter IS the argument.
               @ (dims |> List.choose (function CellsRuntime (_, p) -> Some p | CellsStatic _ -> None))
               |> String.concat ", ")
            "    cudaDeviceSynchronize();"
            $"    cudaMemcpy(__blade_host_out, __blade_d_out, __blade_card * sizeof({devElemCpp}), cudaMemcpyDeviceToHost);" ]
        @ [ for n in codeGen.InputArrayNames -> $"    cudaFree(__blade_d_{n});" ]
        @ [ for c in caps do
              match c.Cells with
              | Some _ -> yield $"    cudaFree(__blade_d_{c.DevName});"
              | None -> () ]
        @ [ "    cudaFree(__blade_d_out);"; "}" ]
    let cell = cudaKernelDefsCell ()
    cell.Append (kernelDef @ [""] @ wrapper @ [""])
    let outputRank = nDims
    let extentsName = $"{name}_extents"
    // First-kernel scope is rectangular (no symmetry), so pass `nullptr` directly
    // as the symm template arg -- not via a function-local static (MSVC C2131).
    // Extent entries: the non-literal arms are LIVE now that a runtime-sized
    // axis is emitted rather than refused, and they render the same expressions
    // `hostExtentOf` hands the launch grid -- one source for both, so the
    // allocation and the grid cannot disagree about how many cells exist.
    let extentDims =
        bindings |> List.map (fun b ->
            match b.Extent with
            | IRLit (IRLitInt n) -> ($"{n}UL", true)
            | _ ->
                match b.FusedRank with
                | Some d ->
                    let prod = [0 .. d - 1] |> List.map (sprintf "%s.extents[%d]" b.ExtentArrayRef) |> String.concat " * "
                    (prod, false)
                | None -> ($"{b.ExtentArrayRef}.extents[{b.ExtentDimRef}]", false))
    let (extentDecls, ownedExtents) = emitExtentsTable "    " extentsName outputRank extentDims
    let inlineLines =
        extentDecls
        @ [ $"    Array<{elemCpp}, {outputRank}> {name} = {{ allocate<typename promote<{elemCpp}, {outputRank}>::type, nullptr>({extentsName}), {extentsName} }};"
            sprintf "    %s(%s, pool_base(%s.data));"
                launchName
                // Inputs are Array<T,N> wrappers (array-literal bindings render as
                // `Array<T,1> A = { ... }`), same as the output -- so the flat pool
                // is reached via `.data`. Do not gate `.data` on a host-shape test:
                // inputs are wrappers here, never bare pointers.
                //
                // A forwarded capture is named by its EMITTED host spelling
                // (captureForwardName, resolved in the planner) -- a block-local
                // `let` is `__v<id>` here, and passing the SOURCE name is the
                // trap every new forwarding site in this file has to avoid.
                (((codeGen.InputArrayNames |> List.map (fun n -> $"pool_base({n}.data)"))
                  @ capHostArgs
                  // Runtime extents are read off the HOST wrappers at the call
                  // site -- the same expressions the output's extents table
                  // above is built from, so grid and allocation cannot diverge.
                  @ extHostArgs)
                 |> String.concat ", ")
                name ]
    // dealloc(D): the rectangular HOST output (dense, so the SYMM argument is
    // the `nullptr` literal emitted just above; extents are static here, so
    // ownedExtents is None and nothing extra is freed). Skipped under
    // `softSplit` for the same abandon-the-pieces reason as the simplicial peel.
    if not softSplit then
        registerArrayAlloc name elemCpp outputRank "nullptr" None ownedExtents
    if softSplit then
        // Soft-join caller sequences the begin/end calls itself; return the
        // host output allocation only (everything but the final call line).
        Some (inlineLines |> List.filter (fun l -> not (l.Contains (launchName + "("))))
    else
    Some inlineLines

/// CUDA CO-FUSION: one `__global__` computing EVERY leaf's output on a single
/// shared grid. This is the device analog of the host merged nest, for the
/// all-cuda case. Scope (returns None -> caller rejects with steering when any
/// fails): all leaves SAME arity and rectangular (a flat thread grid can't
/// stagger arities -- that needs guarded shallow writes, gated separately),
/// SAME input arrays in the same order (so inputs are loaded once and shared),
/// no Reynolds, boundary-safe output elems, matching block size. Each leaf
/// still keeps its own kernel expression and output buffer.
let genCudaCoFusion (leafCgs: LoopNestCodeGen list) (leafNames: string list) (name: string) (blockSize: int) : string list option =
    let primary = List.head leafCgs
    let bindings = primary.Bindings
    let nDims = List.length bindings
    let rectOk (cg: LoopNestCodeGen) =
        cg.Bindings |> List.forall (fun b ->
            b.BoundDependencies.IsEmpty && b.StrictOffset = 0
            && (match b.Extent with IRLit (IRLitInt _) -> true | _ -> false)
            && (b.Elements |> List.forall (_.Virtual.IsRealArray)))
    // Every leaf: same arity, rectangular, no Reynolds, boundary-safe output,
    // and identical input arrays (name + order) so the grid + input loads are
    // genuinely shared.
    let sameArity = leafCgs |> List.forall (fun cg -> cg.Bindings.Length = nDims)
    let sameInputs = leafCgs |> List.forall (fun cg -> cg.InputArrayNames = primary.InputArrayNames)
    let outElemOpt (cg: LoopNestCodeGen) =
        match cg.OutputType with
        | ArrayElem arr when isCudaBoundarySafeElem arr.ElemType -> Some arr.ElemType
        | _ -> None
    if not sameArity || not sameInputs || nDims = 0
       || leafCgs |> List.exists (fun cg -> not (rectOk cg) || cg.HasReynolds || (outElemOpt cg).IsNone)
    then None
    else
    let extentLits = bindings |> List.map (fun b -> match b.Extent with IRLit (IRLitInt n) -> n | _ -> 0L)
    let cardinality = extentLits |> List.fold (fun a n -> a * n) 1L
    // Per-input elem type (from the primary's element bindings by array
    // position), so mixed-type inputs are declared correctly. Host (std::)
    // spelling for the extern "C" wrapper signature (text-copied into the
    // host .cpp as a prototype); device (thrust::) spelling for the kernel
    // signature and the device buffers.
    let inputElemTys =
        primary.InputArrayNames |> List.mapi (fun pos _ ->
            primary.Bindings
            |> List.collect (_.Elements)
            |> List.tryFind (fun e -> e.ArrayPosition = pos)
            |> Option.map (_.ArrayElemType))
    let inputElemCpp =
        inputElemTys |> List.map (Option.map elemTypeToCpp >> Option.defaultValue "double")
    let inputDevCpp =
        inputElemTys |> List.map (Option.map cudaDevElemTypeToCpp >> Option.defaultValue "double")
    let kernelName = $"__cuda_{(sanitizeCppName name)}"
    let launchName = $"__launch_{(sanitizeCppName name)}"
    // Shared coordinate recovery (row-major unflatten of the flat thread id).
    let recover =
        [ yield "    size_t __blade_g = __blade_i;"
          for i in (nDims - 1) .. -1 .. 0 do
            let e = extentLits.[i]
            let b = bindings.[i]
            yield sprintf "    size_t %s = __blade_g %% %dUL;" b.IndexName e
            if i > 0 then yield $"    __blade_g /= {e}UL;" ]
    // Shared input peels (identical across leaves -- bound once). Also records
    // per-array-position peeled names to bridge each leaf's params.
    let mutable currentNames : Map<int, string> = primary.InputArrayNames |> List.mapi (fun i n -> (i, n)) |> Map.ofList
    let mutable sharedFinal : Map<int, string> = Map.empty  // arrayPosition -> peeled name
    let bodyBinds =
        [ for b in bindings do
            let mutable declared : Set<string> = Set.empty
            for elem in b.Elements do
                let cur = Map.tryFind elem.ArrayPosition currentNames |> Option.defaultValue elem.ArrayName
                let newName = $"{cur}__{b.IndexName}"
                let etStr = cudaDevElemTypeToCpp elem.ArrayElemType
                currentNames <- Map.add elem.ArrayPosition newName currentNames
                sharedFinal <- Map.add elem.ArrayPosition newName sharedFinal
                // Self-zip: two operand slots on the same (array, index) peel
                // to the identical declaration -- bind once (nvcc redefinition).
                if not (Set.contains newName declared) then
                    declared <- Set.add newName declared
                    yield $"    {etStr} {newName} = {cur}[{b.IndexName}];" ]
    // Per-leaf output write: map the leaf's params to the shared peeled names
    // by array position (same convention as the host merge).
    let leafWrites =
        leafCgs |> List.mapi (fun k cg ->
            let pfn =
                cg.Bindings |> List.collect (_.Elements)
                |> List.choose (fun e -> Map.tryFind e.ArrayPosition sharedFinal |> Option.map (fun nm -> (e.ParamVarId, nm)))
                |> Map.ofList
            let nameMap = cg.Captures |> List.fold (fun acc c -> Map.add c.Id c.Name acc) pfn
            // __global__ body text: render in the device dialect.
            let rr =
                withCudaDeviceDialect (fun () ->
                    genKernelExprWithReynolds cg.KernelExpr cg.KernelParams false false nameMap pfn)
            $"    __blade_out_{k}[__blade_i] = {rr.CppExpr};")
    let sharedInParams =
        (primary.InputArrayNames, inputDevCpp) ||> List.map2 (fun n et -> $"const {et}* {n}") |> String.concat ", "
    let outParams =
        leafCgs |> List.mapi (fun k cg ->
            let et = (outElemOpt cg).Value |> cudaDevElemTypeToCpp
            $"{et}* __blade_out_{k}") |> String.concat ", "
    let kernelDef =
        [ $$"""__global__ void {{kernelName}}({{sharedInParams}}, {{outParams}}, size_t __blade_card) {"""
          "    size_t __blade_i = (size_t)blockIdx.x * blockDim.x + threadIdx.x;"
          "    if (__blade_i >= __blade_card) return;" ]
        @ recover @ bodyBinds @ leafWrites @ [ "}" ]
    let wrapInParams =
        (primary.InputArrayNames, inputElemCpp) ||> List.map2 (fun n et -> $"const {et}* {n}") |> String.concat ", "
    let wrapOutParams =
        leafCgs |> List.mapi (fun k cg ->
            let et = (outElemOpt cg).Value |> elemTypeToCpp
            $"{et}* __blade_host_out_{k}") |> String.concat ", "
    let wrapper =
        [ $"extern \"C\" void {launchName}({wrapInParams}, {wrapOutParams}) {{"
          $"    size_t __blade_card = {cardinality}UL;" ]
        @ [ for (i, n) in List.mapi (fun i n -> (i, n)) primary.InputArrayNames do
              let sz = extentLits |> List.fold (fun a n -> a * n) 1L  // inputs span the same grid cardinality
              let et = inputDevCpp.[i]
              yield $"    {et}* __blade_d_{n}; cudaMalloc(&__blade_d_{n}, {sz}UL * sizeof({et}));"
              yield $"    cudaMemcpy(__blade_d_{n}, {n}, {sz}UL * sizeof({et}), cudaMemcpyHostToDevice);" ]
        @ [ for k in 0 .. leafCgs.Length - 1 do
              let et = (outElemOpt leafCgs.[k]).Value |> cudaDevElemTypeToCpp
              yield $"    {et}* __blade_d_out_{k}; cudaMalloc(&__blade_d_out_{k}, __blade_card * sizeof({et}));" ]
        @ [ $"    size_t __blade_blocks = (__blade_card + {blockSize}UL - 1UL) / {blockSize}UL;"
            sprintf "    %s<<<(unsigned)__blade_blocks, %d>>>(%s, %s, __blade_card);"
                kernelName blockSize
                (primary.InputArrayNames |> List.map (sprintf "__blade_d_%s") |> String.concat ", ")
                (List.init leafCgs.Length (sprintf "__blade_d_out_%d") |> String.concat ", ")
            "    cudaDeviceSynchronize();" ]
        @ [ for k in 0 .. leafCgs.Length - 1 do
              let et = (outElemOpt leafCgs.[k]).Value |> cudaDevElemTypeToCpp
              yield $"    cudaMemcpy(__blade_host_out_{k}, __blade_d_out_{k}, __blade_card * sizeof({et}), cudaMemcpyDeviceToHost);" ]
        @ [ for n in primary.InputArrayNames -> $"    cudaFree(__blade_d_{n});" ]
        @ [ for k in 0 .. leafCgs.Length - 1 -> $"    cudaFree(__blade_d_out_{k});" ]
        @ [ "}" ]
    let cell = cudaKernelDefsCell ()
    cell.Append (kernelDef @ [""] @ wrapper @ [""])
    // Inline: allocate each output Array on the host, then a single launch.
    // Per-leaf extents: the co-fusion gate (rectOk over every leaf) forces
    // every binding extent to IRLit, so each table is all-literal and static;
    // the non-literal arm is kept structural (same match renders the value)
    // so a widened gate degrades to the heap form. leafExtentsOwned is
    // positionally parallel to leafNames for the registrations below.
    let leafExtentsOwned =
        leafCgs |> List.mapi (fun k cg ->
            let extentsName = $"{leafNames.[k]}_extents"
            let dims =
                cg.Bindings |> List.map (fun b ->
                    match b.Extent with
                    | IRLit (IRLitInt n) -> ($"{n}UL", true)
                    | _ -> ($"{b.ExtentArrayRef}.extents[{b.ExtentDimRef}]", false))
            emitExtentsTable "    " extentsName nDims dims)
    let inlineLines =
        (leafCgs |> List.mapi (fun k cg ->
            let lname = leafNames.[k]
            let et = (outElemOpt cg).Value |> elemTypeToCpp
            let extentsName = $"{lname}_extents"
            let (extentDecls, _) = leafExtentsOwned.[k]
            extentDecls
            @ [ $"    Array<{et}, {nDims}> {lname} = {{ allocate<typename promote<{et}, {nDims}>::type, nullptr>({extentsName}), {extentsName} }};" ]) |> List.concat)
        @ [ sprintf "    %s(%s, %s);"
                launchName
                (primary.InputArrayNames |> List.map (fun n -> $"pool_base({n}.data)") |> String.concat ", ")
                (leafNames |> List.map (fun ln -> $"pool_base({ln}.data)") |> String.concat ", ") ]
    // dealloc(D): the per-leaf HOST restore buffers this arm emits itself. The
    // caller wraps these lines with `wrapDevice`, which DROPS the shared host
    // `declCode` (hazard H7) -- so these, not the fused leafRegs, are the
    // allocations that actually appear on the all-cuda arm.
    for k in 0 .. leafCgs.Length - 1 do
        let et = (outElemOpt leafCgs.[k]).Value |> elemTypeToCpp
        registerArrayAlloc leafNames.[k] et nDims "nullptr" None
            (snd leafExtentsOwned.[k])
    Some inlineLines

/// Classification of a loop nest for MPI decomposition (`where mpi`).
type MpiShape =
    /// Dense rectangular nest: outermost level slab-decomposed across ranks.
    | MpiDense
    /// Single symmetric/antisymmetric group: flat cell-range decomposition
    /// over the packed pool.
    | MpiSimplicial
    /// Not decomposable: carries the human-readable reason for #error.
    | MpiIneligible of string

/// Classify a built loop nest for MPI eligibility. In scope: per-cell
/// array-output kernels over real (non-virtual) arrays whose element type has
/// a native MPI datatype. Everything else is rejected LOUDLY (the caller
/// emits #error) rather than silently serialized -- an inert-looking `mpi`
/// clause under the emit gate would otherwise misreport scaling results.
let classifyMpiShape (codeGen: LoopNestCodeGen) : MpiShape =
    let bindings = codeGen.Bindings
    let allReal =
        bindings |> List.forall (fun b ->
            b.Elements |> List.forall (fun e ->
                e.Virtual.IsRealArray))
    let anyCompound =
        bindings |> List.exists (fun b ->
            match b.Extent with IRCompoundMask _ | IRSparseKeys _ -> true | _ -> false)
    let anyFused = bindings |> List.exists (_.FusedRank.IsSome)
    let isRectangular =
        bindings |> List.forall (fun b ->
            b.BoundDependencies.IsEmpty && b.StrictOffset = 0)
    if codeGen.FoldWrapper.IsSome then
        MpiIneligible "fold accumulation reorders floating-point reduction"
    else
        match codeGen.OutputType with
        | IRTScalar _ ->
            MpiIneligible "scalar output is a cross-cell reduction (floating-point reassociation)"
        | ArrayElem at when isCompoundArrayType at || isSparseArrayType at ->
            MpiIneligible "compound/sparse iteration domains are not decomposed (v1)"
        | ArrayElem at ->
            match at.ElemType with
            | AnyPrimElem et when (mpiDatatypeOf et).IsSome ->
                if anyCompound then MpiIneligible "compound iteration domains are not decomposed (v1)"
                elif not allReal then MpiIneligible "virtual sources (range/reverse) are not decomposed (v1)"
                elif bindings.IsEmpty then MpiIneligible "empty loop nest"
                elif isRectangular && not anyFused then MpiDense
                elif not isRectangular && not anyFused then MpiSimplicial
                else MpiIneligible "fused joint levels are not decomposed (v1)"
            | _ -> MpiIneligible "element type has no native MPI datatype"
        | _ -> MpiIneligible "output shape is not a plain array"

/// Generate the complete code for a combinator application (L <@> f)
//  Deduced OrbIdx (iterated-wreath) output -- allocation + the traversal nest
//  (docs/plan-orbit-index-types.md section 9 step 4; plan-orbidx-bijections.md section 2).
//
//  A wreath application is `k` occurrences of ONE object, comm-tied, over a
//  common compact class `L`; the output class is `L ++ [(k,s)]`
//  (IRLoopStructure.deduceWreathTie). It bypasses the generic loop machinery entirely:
//    * the nest it needs is SEGMENT-PEELED (bijections section 2, multiple
//      straight-line sub-nests decomposed by equality-prefix length on
//      SUB-KEYS) -- `buildLoopLevelStructure` has no such concept and refuses
//      a wreath slot; ordinary levels would walk cells in the wrong order.
//    * that nest is ALREADY WRITTEN AND CHECKED: `orb_visit<Levels...>` in
//      orbit_wreath_utilities.hpp is verified (`blade test orbwreath`,
//      cross-diffed against `OrbRank.visitStream`), so it is INSTANTIATED
//      rather than re-derived in F# strings.
//
//  STORAGE ORDER is the plan's one hard invariant (bijections section 3): the pool is
//  a flat `T*` of `orb_cell_count<Levels...>(n)` cells in `orb_visit` order ==
//  `OrbRank.visitStream` order == ascending lex. A read->write roundtrip
//  cannot catch an order mismatch (both sides shift together), so the corpus
//  pins the printed cell SEQUENCE against hand-computed values, and the
//  interpreter's `visitStream` walk is diffed against it.

/// Render the C++ subscript that reads ONE tied argument at its canonical
/// sub-key, given the flat coordinate buffer the visitor hands us.
///
///   depth-1 '+' (SymIdx<r,n>)      arr[c0][c1-c0][c2-c1]...
///   depth-1 '-' (AntisymIdx<r,n>)  arr[c0][c1-c0-1][c2-c1-1]...
///   depth >= 2 (a wreath input)    arr[orb_rank<Inner...>(coords + off, n)]
///
/// The depth-1 forms are the LEFT-JUSTIFIED storage coordinates the existing
/// triangular writer produces (`canon_left_justify`'s convention), so this
/// reads exactly the cells the input's own nest wrote -- no fold, since
/// `orb_visit` hands out sub-keys already canonical for the inner class.
///
/// The wreath form goes through `orb_rank`, the SAME arithmetic that produced
/// the input pool's own order, so the two cannot disagree -- the cold-path
/// rank on a warm path, deliberately: a canonical wreath read has no cheaper
/// spelling, and depth >= 3 is the only shape needing it. This is NOT the
/// mirrored read that stays refused: the tuple is canonical by construction
/// and no character is ever applied.
let internal wreathArgRead (arrName: string) (innerLevels: (int * bool) list)
                          (extent: int64) (coordBuf: string) (baseOff: int) : string =
    let c i = $"{coordBuf}[{baseOff + i}]"
    match innerLevels with
    | [ (r, isPlus) ] ->
        let strict = if isPlus then 0 else 1
        let subs =
            [ 0 .. r - 1 ] |> List.map (fun a ->
                if a = 0 then $"[(size_t){(c 0)}]"
                else
                    let off = if strict > 0 then $" - {strict}" else ""
                    $"[(size_t)({(c a)} - {(c (a - 1))}{off})]")
        arrName + String.concat "" subs
    | _ ->
        $"{arrName}[orbit_wreath_utilities::orb_rank<{(orbLevelArgs innerLevels)}>({coordBuf} + {baseOff}, {extent})]"

/// Emit the whole wreath application: pool allocation + the instantiated
/// `orb_visit` nest. Returns None when the shape is a wreath tie but something
/// the emitter cannot render (unresolvable kernel, non-literal extent), so the
/// caller can fall through to a loud `#error` rather than emit half a nest.
let internal genWreathApply
        (ctx: CodeGenContext) (ind: string) (name: string)
        (info: ApplyInfo) (arrayNames: string list) (tie: WreathTie) : string list option =
    match resolveKernel info.Kernel with
    | None -> None
    | Some rk ->
    let elemCpp =
        match info.OutputType with
        | ArrayElem at -> elemTypeToCpp at.ElemType
        | _ -> "double"
    // THE ALLOCATION DECISION comes from `classifyOutputStorage`, the same
    // function every other output shape asks -- not from a second fold over the
    // tie. It reads the level list and extent off the OUTPUT RECORD (which
    // `mkWreathIndexRecord` built from this tie) and sizes the pool with
    // `OrbRank.cellCountChecked`, the identical checked fold
    // `bufferGroupCardinality` uses. So there is one cell count on the F# side,
    // and the emitted C++ then pins the C++ fold against it at runtime.
    // AllocWreath also carries the levels the RECORD holds, which is what gets
    // instantiated below: an emitter that used `tie.OutputLevels` here and the
    // record elsewhere could drift.
    match classifyOutputStorage info.OutputType with
    | AllocWreath (outLevels, n, cells) ->
    let outArgs = orbLevelArgs outLevels
    let innerAxes = tie.InnerLevels |> List.fold (fun a (r, _) -> a * r) 1
    let coordBuf = "__orbc"
    let cellsName = $"{name}__orbcells"
    // One local per tied argument, bound to that argument's canonical sub-key
    // read, then the kernel body rendered against those locals. This mirrors
    // genLoopNestStreamed's element-binding + nameMap discipline exactly: the
    // body never sees a coordinate, only a value.
    let kparams = rk.Callable.Params
    let binds =
        tie.Positions |> List.mapi (fun j p ->
            let arrName =
                if p < arrayNames.Length then arrayNames.[p] else $"arr{p}"
            let local = $"__orb_{(sanitizeCppName name)}_{j}"
            let readExpr = wreathArgRead arrName tie.InnerLevels n coordBuf (j * innerAxes)
            let vid = if p < kparams.Length then Some kparams.[p].VarId else None
            (local, readExpr, vid))
    // Captures are a FALLBACK, never an override -- same precedence rule as the
    // generic nest's nameMap (the emitted spelling in ctx.VarNames wins over the
    // capture's SOURCE-level name).
    let nameMap =
        let withParams =
            binds |> List.fold (fun acc (local, _, vid) ->
                match vid with Some v -> Map.add v local acc | None -> acc) ctx.VarNames
        rk.Callable.Captures
        |> List.fold (fun acc c -> if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc) withParams
    let bodyStr = exprToCppCore emptySubst nameMap rk.Callable.Body
    // The pool is a flat `new T[cells]` in orb_visit order; `delete[]` is the
    // matching free (RawAlloc), not the skeleton deallocator -- there is no
    // skeleton.
    registerAlloc (RawAlloc (name, None))
    Some
        ([ $"{ind}// OrbIdx{(ppOrbitLevels outLevels)} over extent {n}: {cells} cells, ascending-lex canonical order"
           $"{ind}const int64_t {cellsName} = orbit_wreath_utilities::orb_cell_count<{outArgs}>({n});"
           // The C++ fold and the F# fold (OrbRank.cellCountChecked, which sized
           // this very allocation through AllocWreath) are INDEPENDENT
           // implementations of section 4. Pin them against each other at runtime: a
           // disagreement is a wrong-sized pool, which nothing on the value side
           // could otherwise notice.
           $"{ind}if ({cellsName} != {cells}LL) {{ blade_rt::panic(\"BL8004\", \"OrbIdx pool size disagreement: orb_cell_count vs the compiler's iterated-binomial fold\", nullptr, 0); }}"
           $"{ind}{elemCpp}* {name} = new {elemCpp}[(size_t){cellsName}];"
           $"{ind}orbit_wreath_utilities::orb_visit<{outArgs}>({n}, [&](const int* {coordBuf}, int64_t __orbk) {{" ]
         @ (binds |> List.map (fun (local, readExpr, _) ->
                $"{ind}    {elemCpp} {local} = {readExpr};"))
         @ [ $"{ind}    {name}[__orbk] = {bodyStr};"
             $"{ind}}});" ])
    // Every other AllocSpec on a tie's output is a contradiction (the tie built
    // a sole SymWreath record, which classifyOutputStorage answers AllocWreath
    // for) or an honest refusal (a runtime extent, an overflowing fold). Either
    // way there is nothing to emit; the caller turns None into a `#error`.
    | _ -> None

let genApplyCombinator (ctx: CodeGenContext) (name: string) (info: ApplyInfo) (builder: IRBuilder) : string list =
    let ind = indentStr ctx

    // OMP LICENCE FOR THE PEELED ROW LOOP. Bound HERE, above `tryRaggedPeel`,
    // because both peel closures capture it.
    //
    // WHAT THE LICENCE MEANS AT THIS SITE. `omp(g: n)` licenses the EXTERNAL
    // S-dims `g` contributes -- the loop levels that argument brings to a nest
    // built AROUND the kernel -- and NOT a loop the body generates over that same
    // argument (the rule tests/corpus/loops/115 states, and the misreading BL4001
    // exists to name). A peeled ROW param contributes exactly ONE such level, the
    // `__g` loop, so any n >= 1 licenses it and a larger n caps there -- matching
    // `ompDepthCases.depth_2_on_rank1_arg_caps_at_1`. The `__k` loop inside a
    // body's `reduce(g, ...)` is generated BY THE BODY and takes its licence from
    // that reduce's own kernel, never from this clause.
    //
    // `List.max` over the depths, with TWO DIFFERENT JUSTIFICATIONS -- the same
    // expression, so this is worth saying rather than leaving to be re-derived:
    //   * `tryRaggedPeel` has ONE array and ONE param, so the max degenerates to
    //     that param's own depth. Nothing about co-iteration is involved.
    //   * `tryGroupedZipPeel` is the genuine `IR.coIterLicense` rule: k row params
    //     all peel at the SAME `__g`, there is no per-argument level ownership to
    //     distinguish, so the most permissive licence wins.
    //
    // Empty depths with the flag set is IR's `licenseUnresolved` fallback, and it
    // covers two source situations that must BOTH license `__g`: a bare `where
    // omp` (the parser's `Omp { Vars = [] }`) and an `omp(typo: n)` naming no
    // parameter (BL4001 warns, but the nest must not silently serialize).
    //
    // `resolveKernel`, not the `resolveCallable` the peel arms use: it peels an
    // `IRReynolds` wrapper before resolving, so it succeeds wherever
    // `resolveCallable` does, and it is what `mpiRequested` below and IR's own
    // `kernelRequestedOmp` / `ompDepths` consult -- one resolver, both paths.
    let (peelOmpRequested, peelRowLicensed) =
        match resolveKernel info.Kernel with
        | Some rk ->
            let requested = rk.Callable.IsOmpParallel
            let depths = rk.Callable.Parallelism
            (requested,
             requested && (List.isEmpty depths
                           || (depths |> List.map snd |> List.max) >= 1))
        | None -> (false, false)

    // Threading a peel whose source is a STREAMED provider read is the one shape
    // the generic path refuses outright (`genFusedLoopNestStreamed`: per-source
    // handles and per-argument buffers are shared, so a team races on them). The
    // peels bypass that check entirely, so re-apply it here.
    //
    // A DECLINE, not a raise, unlike the generic site. Reachability is dubious --
    // a streamed source carrying a ragged / `__group_outer` index type would
    // likely already miscompile at `%s.extents[0]` -- and a raise could reject a
    // program that compiles today. Declining adds no new refusals, and the marker
    // keeps it from being silent.
    //
    // Short-circuited on the map being non-empty, and only forced from inside the
    // peel arms: `exprToCppCtx` is a full render of the operand expression, and
    // `StreamedArrays` is empty in every program that reads no provider. Rendering
    // every operand of every apply -- dense paths included, none of which can
    // reach a peel -- to answer a question whose answer is almost always "no" is
    // work this site has no business doing.
    let peelSourceStreamed =
        lazy (not (Map.isEmpty ctx.StreamedArrays)
              && info.Arrays |> List.exists (fun a ->
                    Map.containsKey (exprToCppCtx ctx a) ctx.StreamedArrays))
    let peelStreamBlocker () : string option =
        if peelSourceStreamed.Force () then
            Some "the peeled source is a streamed provider read (shared per-source handles and per-argument buffers are not thread-safe)"
        else None

    // ARRAY-VALUED KERNEL ROWS over a PEELED outer axis (stage S3,
    // manifestation M-C3 -- lswosa's `family_spectra` / `cellacc` shape). The
    // kernel collapses each peeled row (or tuple of rows) to a whole DENSE
    // row, so the output is [outer] x [kernel T-dims]: rank >= 2, one row per
    // outer cell, flat row-major pool. Callers have already taken the
    // genuinely ragged/group-SHAPED results away, so everything arriving here
    // has dense trailing axes.
    //
    // SHARED BY BOTH PEELS. They differ only in where the outer count and the
    // per-row bindings come from -- a group_keys offsets table (`gk__ngroups`
    // / `gk__offsets[__g+1] - gk__offsets[__g]`) or a ragged literal's own
    // `arr.extents[0]` / `arr.lens[__g]` -- and those arrive as strings.
    // Everything that is genuinely about array-valued rows (the two sizing
    // forms, the extents ABI, the row copy, the threading rules) lives here
    // once, so a single-operand ragged literal and a k-operand grouped
    // co-iteration cannot drift apart on any of it.
    let emitRowValuedPeel
            (headerLine: string) (headerRuntimeSuffix: string)
            (ngroupsExpr: string) (rowDeclLines: string list)
            (bodyExpr: IRExpr) (bodyStr: string)
            (outElemStr: string) (outRank: int) : string list =
        // A trailing extent the static evaluator can settle is emitted as a
        // literal. Everything else -- a compiler-minted `__<op>_inferred_n`
        // param (the length a LENGTH-AGNOSTIC generic callee keeps abstract:
        // `f: V^1` -> the returned row is as long as the argument, and the
        // static type never learns the number; extents are deliberately
        // outside type identity), or a function-scope symbolic extent whose
        // rendered spelling names no declared C++ identifier -- takes the
        // size-on-first-row form below: the returned row is self-describing
        // at runtime (`__rowv.extents`), so the trailing extents are read off
        // the FIRST row and the pool is allocated then. Runtime sizing is
        // correct for every one of these shapes; the static table is kept for
        // literals so the common pinned shapes emit exactly as before.
        let innerDims =
            match info.OutputType with
            | ArrayElem a ->
                a.IndexTypes
                |> List.skip 1
                |> List.map (fun ix ->
                    match tryEvalIntIR ix.Extent with
                    | Some n -> Some (string n)
                    | None -> None)
            | _ -> []
        let innerCells =
            [ 1 .. outRank - 1 ] |> List.map (sprintf "%s_extents[%d]" name) |> String.concat " * "
        let freeLine =
            match bodyExpr with
            | IRApp (f, _, _) when freshReturnOf f = FreshPool ->
                [ $"{ind}        deallocate<typename promote<{outElemStr}, {(outRank - 1)}>::type, nullptr>(__rowv.data, __rowv.extents);" ]
            | _ -> []
        let rowCopyLines =
            [ $"{ind}        const {outElemStr}* __rows = nested_array_utilities::pool_base(__rowv.data);"
              $"{ind}        const size_t __rowc = (size_t)({innerCells});"
              $"{ind}        {outElemStr}* __rowd = nested_array_utilities::pool_base({name}.data) + (__g * __rowc);"
              $"{ind}        for (size_t __rk = 0; __rk < __rowc; __rk++) __rowd[__rk] = __rows[__rk];" ]
        if innerDims |> List.forall Option.isSome then
            [ headerLine ]
            // Heap extents: the return-extent ABI (see emitExtentsTable).
            // `Array<T,R>` stores only a POINTER to its extents, so a
            // frame-local `size_t[R]` table dangles the moment the wrapper
            // crosses a call boundary. The leading entry is a runtime read
            // (`gk__ngroups` / `arr.extents[0]`), so the table is never
            // all-literal and the helper always takes its heap form.
            @ fst (emitExtentsTable ind (name + "_extents") outRank
                       ((ngroupsExpr, false) :: (innerDims |> List.map (fun d -> (Option.get d, true)))))
            @ [ $"{ind}Array<{outElemStr}, {outRank}> {name} = {{ allocate<typename promote<{outElemStr}, {outRank}>::type, nullptr>({name}_extents), {name}_extents }};" ]
            // Array-valued rows, pool allocated UP FRONT: iteration __g copies
            // into `pool_base(name.data) + __g * __rowc`, a slice disjoint from
            // every other iteration's, and `__rowv` / `__rows` / `__rowd` are
            // declared inside the body. Same licence as the scalar arms.
            @ peelRowPragma peelOmpRequested peelRowLicensed (peelStreamBlocker ()) ind
            @ [ $$"""{{ind}}for (size_t __g = 0; __g < {{ngroupsExpr}}; __g++) {""" ]
            @ rowDeclLines
            @ [ $$"""{{ind}}    {"""
                $"{ind}        auto __rowv = {bodyStr};" ]
            @ rowCopyLines
            @ freeLine
            @ [ $$"""{{ind}}    }"""
                $"{ind}}}" ]
        else
            // Size-on-first-row form: extents start {ngroups, 0..}, iteration 0
            // fills the trailing slots from the row's own runtime extents and
            // allocates the pool. With zero rows nothing allocates and every
            // consumer iterates an honest empty shape.
            [ headerLine + headerRuntimeSuffix ]
            // Heap extents (return-extent ABI, above). This arm needs it for a
            // SECOND reason as well: the trailing slots are WRITTEN below, from
            // iteration 0's row -- so the table is mutable state the wrapper
            // reads through its pointer, and a caller that outlives this frame
            // must still see those writes.
            @ fst (emitExtentsTable ind (name + "_extents") outRank
                       ((ngroupsExpr, false) :: (innerDims |> List.map (fun d -> (defaultArg d "0", false)))))
            @ [ $$"""{{ind}}Array<{{outElemStr}}, {{outRank}}> {{name}} = { nullptr, {{name}}_extents };""" ]
            // THIS ARM CANNOT BE THREADED, and the reason is the `if (__g == 0)`
            // below rather than anything about the kernel: iteration 0 is what
            // settles the trailing extents and ALLOCATES the pool that every
            // other iteration writes into. Under a parallel for, a thread
            // holding __g = 5 can reach `pool_base(name.data)` before the
            // thread holding __g = 0 has run the allocation -- a null-base
            // write, not merely a torn one. Hoisting the allocation out would
            // mean sizing the pool before any row has been evaluated, which is
            // exactly what this arm exists because we cannot do.
            //
            // So it declines, and says so: a licensed kernel that lands here
            // would otherwise be the same silent drop this whole path removes.
            @ peelRowPragma peelOmpRequested peelRowLicensed
                  (Some (defaultArg (peelStreamBlocker ())
                            "the row pool is sized and allocated from the first iteration (length-agnostic callee), so the row loop must run serially"))
                  ind
            @ [ $$"""{{ind}}for (size_t __g = 0; __g < {{ngroupsExpr}}; __g++) {""" ]
            @ rowDeclLines
            @ [ $$"""{{ind}}    {"""
                $"{ind}        auto __rowv = {bodyStr};"
                $$"""{{ind}}        if (__g == 0) {"""
                $"{ind}            for (size_t __rx = 1; __rx < {outRank}; __rx++) {name}_extents[__rx] = __rowv.extents[__rx - 1];"
                $"{ind}            {name}.data = allocate<typename promote<{outElemStr}, {outRank}>::type, nullptr>({name}_extents);"
                $$"""{{ind}}        }""" ]
            @ rowCopyLines
            @ freeLine
            @ [ $$"""{{ind}}    }"""
                $"{ind}}}" ]

    /// The kernel body as it must be RENDERED at a peel site: inline text when
    /// it is expression-shaped, otherwise a CALL to the lifted callable.
    ///
    /// An ARRAY-LITERAL tail also routes through the call: the literal has no
    /// expression rendering (`exprToCppCore`'s catch-all), and unlike the dense
    /// nest -- whose row-write arm stores literal leaves straight into the
    /// output pool -- a peel renders the body as ONE `auto __rowv = <expr>`
    /// slot. The lifted body compiles it fine (genFuncBody's return-position
    /// IRArrayLit arm), and the row copy above already handles a call that
    /// returns a whole row. Shared by both peels so neither can regress the
    /// other's array-literal support.
    let peelBodyExpr (callable: IRCallable) : IRExpr =
        let rec chainTail e = match e with IRLet (_, _, b) -> chainTail b | t -> t
        let arrayLitTail =
            (chainTail callable.Body).IsIRArrayLit
        if kernelBodyIsExpressionShaped callable.Body && not arrayLitTail then callable.Body
        else
            let paramTypes = callable.Params |> List.map (_.Type)
            IRApp (IRVar (callable.Id, mkFuncArrow paramTypes callable.RetType),
                   callable.Params |> List.map (fun p -> IRVar (p.VarId, p.Type)),
                   callable.RetType)

    // Special case: ragged peel for grouped arrays.
    // Triggered when method_for is applied to a single grouped array
    // (recognized by Tag = "__group_outer" on its first index type).
    // This bypasses the generic loop-nest builder for two reasons:
    //   1. The inner extent is ragged (per group, from gk__offsets).
    //   2. The kernel param ('g' in lambda(g) -> g(0)) has unresolved type at
    //      typecheck time, so kernelInputRanks=[0] and the loop builder would
    //      otherwise try to iterate both dims of the rank-2 grouped array.
    // We also rewrite IRApp(g, args) -> IRIndex(g, args) on the kernel body so
    // that 'g(0)' renders as 'g_local[0]' rather than 'g_local(0)'.
    // Generalized ragged peel: handles two source patterns
    //   (a) group_by output: outer index tagged __group_outer; lengths derived
    //       from the corresponding group_keys array via ctx.GroupedArrays.
    //   (b) ragged literal: inner index tagged __raggedidx_inline; lengths in
    //       arr_lens, offsets in arr_offsets (emitted by genArrayLiteral).
    // In both cases, the same loop structure works: outer loop over rows,
    // sub-array binding for the kernel param, kernel body executed per row.
    let tryRaggedPeel () : string list option =
        if info.Arrays.Length <> 1 then None
        else
            let arrType = info.ArrayTypes.[0]
            let isGroupedOuter =
                match arrType.IndexTypes with
                | outer :: _ -> outer.IxKind = IxKGroupOuter
                | _ -> false
            // Detect ragged-or-DepIdx input: at least 2 IndexTypes, and the
            // *inner* (any non-first) carries any of the ragged tags or the
            // DepIdx-inner tag. Covers ragged literals (__raggedidx_inline),
            // function-param closed form (__raggedidx), function-param opaque
            // form (__raggedidx_opaque), and DepIdx-allocated arrays
            // (__depidx_inner -- runtime layout matches ragged once allocated).
            // All want the peel codegen path: outer iteration over rows,
            // sub-array binding for the kernel.
            let isRaggedLiteral =
                arrType.IndexTypes.Length >= 2 &&
                arrType.IndexTypes |> List.skip 1 |> List.exists (fun idx ->
                    isRaggedFamilyKind idx.IxKind || idx.IxKind = IxKDepInner)
            if not isGroupedOuter && not isRaggedLiteral then None
            else
                let arrExpr = info.Arrays.[0]
                let arrName = exprToCppCtx ctx arrExpr
                // Resolve "lengths source" -- where to read each row's length
                // and offset. For group_by, this is the group_keys metadata
                // (gk__offsets, gk__ngroups). For ragged literals, it's the
                // array's own _offsets/_lens emitted at construction.
                let lengthsSource =
                    if isGroupedOuter then
                        match Map.tryFind arrName ctx.GroupedArrays with
                        | Some gkName ->
                            Some ($"{gkName}__ngroups",
                                  $"{gkName}__offsets[__g + 1] - {gkName}__offsets[__g]")
                        | None -> None
                    else
                        // Ragged literal: lens/extents are co-emitted with
                        // the array. The outer count is in arr_extents[0];
                        // each row's length is in arr_lens[__g].
                        Some ($"{arrName}.extents[0]",
                              $"{arrName}.lens[__g]")
                match lengthsSource with
                | None -> None
                | Some (ngroupsExpr, perRowLenExpr) ->
                    // Elementwise-vs-consuming dispatch, keyed on the OUTPUT
                    // type -- the authoritative signal, because TypeCheck
                    // already made this exact decision (paramUsedAsArray):
                    // a CONSUMING kernel (lambda(g) -> reduce(g, +)) collapses
                    // the ragged inner dim, so the output is dense rank-1; an
                    // ELEMENTWISE kernel (lambda(e) -> e * 2.0) leaves it in
                    // place, so the output keeps a ragged-family inner slot.
                    // Re-deriving the choice from the param type or body here
                    // would risk disagreeing with the type the rest of the
                    // pipeline committed to.
                    let outputInnerKinds =
                        match info.OutputType with
                        | ArrayElem a when a.IndexTypes.Length >= 2 ->
                            a.IndexTypes |> List.skip 1 |> List.map (_.IxKind)
                        | _ -> []
                    let outputIsRaggedShaped =
                        outputInnerKinds |> List.exists (fun k -> isRaggedFamilyKind k || k = IxKDepInner)
                    let outputIsGroupShaped =
                        outputInnerKinds |> List.exists ((=) IxKGroupMember)
                    // ARRAY-VALUED ROWS: the kernel collapses each peeled row to
                    // a whole DENSE row (the `lambda(row) -> { ...; [s, q] }`
                    // rank-raising shape). Serviced by this peel's own
                    // array-valued arm below, for BOTH operand kinds -- the
                    // group-shaped and ragged-shaped outputs are already taken
                    // away above, so a rank >= 2 output left here has dense
                    // trailing axes.
                    let outputHasDenseTrailing =
                        match info.OutputType with
                        | ArrayElem a when a.IndexTypes.Length >= 2 ->
                            not outputIsRaggedShaped && not outputIsGroupShaped
                        | _ -> false
                    match resolveCallable info.Kernel with
                    | Some callable when callable.Params.Length = 1 && outputIsGroupShaped ->
                        // Elementwise map over a group_by result. The grouped
                        // value is an Array<T*,1> without lens/offsets wrapper
                        // members, and the group-shaped output type has no
                        // consumer support downstream (print, further peels
                        // resolve lengths through ctx.GroupedArrays, which this
                        // site cannot extend). Gate rather than miscompile;
                        // mapping BEFORE grouping is semantically equivalent.
                        Some (codegenError ctx ind "elementwise map over a group_by result is not yet supported; apply the map to the values BEFORE group_by (equivalent), or reduce per group")
                    | Some callable when callable.Params.Length = 1 && outputIsRaggedShaped ->
                        // Elementwise map over a ragged array: shape-preserving.
                        // The output is a fresh Ragged<T> that SHARES the
                        // parent's extents/lens/offsets metadata (same shape by
                        // construction) over a newly allocated contiguous pool
                        // (offsets[n] = total element count) with its own
                        // row-pointer table. Kernel applies per element; the
                        // param binds each element value.
                        let param = callable.Params.[0]
                        let inElemStr = elemTypeToCpp arrType.ElemType
                        let outElem =
                            match info.OutputType with
                            | ArrayElem a -> a.ElemType
                            | t -> t
                        let outElemStr = elemTypeToCpp outElem
                        let eName = $"{name}__e"
                        let nameMap0 = Map.add param.VarId eName ctx.VarNames
                        let nameMap =
                            callable.Captures
                            |> List.fold (fun m c -> Map.add c.Id c.Name m) nameMap0
                        let bodyStr = exprToCpp nameMap callable.Body
                        let code =
                            [ $"{ind}// ragged elementwise map over '{arrName}' (shape-preserving; shares extents/lens/offsets)"
                              $"{ind}{outElemStr}* {name}__pool = new {outElemStr}[{arrName}.offsets[{arrName}.extents[0]]];"
                              $"{ind}{outElemStr}** {name}__rows = new {outElemStr}*[{arrName}.extents[0]];"
                              $$"""{{ind}}Ragged<{{outElemStr}}> {{name}} = { {{name}}__rows, {{arrName}}.extents, {{arrName}}.lens, {{arrName}}.offsets };""" ]
                            // The ROW loop carries the licence; `__k` stays serial
                            // for a STRUCTURAL reason, not a conventional one.
                            // `omp(e: 2)` on this rank-2 ragged argument names
                            // `__k` too, but honouring it needs collapse(2) -- and
                            // the inner bound `%s.lens[__g]` is a MEMORY LOAD, not
                            // affine in `__g`, which is outside even OpenMP 5.0's
                            // non-rectangular collapse. The pragma would be
                            // ill-formed, not merely unwise, so depth caps at
                            // `__g`. (This is the same fact that makes
                            // `genNestPragma` unable to serve this site: its
                            // `collapseEligible` requires `isRectangular`, and a
                            // `lens[]`-bound level is not.)
                            //
                            // NON-GOAL, recorded so it is neither silently shipped
                            // nor silently forgotten: `#pragma omp simd` on `__k`.
                            // It is tempting (contiguous elementwise write, and
                            // simd survives BLADE_OMP_THREADS=1), but it needs
                            // `kernelBodyContainsInnerLoop` as a gate or it lands
                            // on bodies hiding an IIFE loop, where it is dead text
                            // that READS like the optimization happened.
                            //
                            // Row-disjoint: each `__g` writes its own `__rows` slot
                            // and the pool slice [offsets[__g], offsets[__g+1]).
                            @ peelRowPragma peelOmpRequested peelRowLicensed (peelStreamBlocker ()) ind
                            @ [ $$"""{{ind}}for (size_t __g = 0; __g < {{arrName}}.extents[0]; __g++) {"""
                                $"{ind}    {name}__rows[__g] = {name}__pool + {arrName}.offsets[__g];"
                                $$"""{{ind}}    for (size_t __k = 0; __k < {{arrName}}.lens[__g]; __k++) {"""
                                $"{ind}        const {inElemStr} {eName} = {arrName}[__g][__k];"
                                $"{ind}        {name}__rows[__g][__k] = {bodyStr};"
                                $$"""{{ind}}    }"""
                                $"{ind}}}" ]
                        // Owns pool + row table; extents/lens/offsets belong to
                        // the INPUT ragged. deallocate_ragged frees exactly the
                        // two owned pieces (a per-row free would be an interior
                        // free -- rows are pool+offset slices).
                        registerShapedAlloc name "deallocate_ragged" ($"{name}, {name}__pool")
                        Some code
                    | Some callable when callable.Params.Length = 1 ->
                        let param = callable.Params.[0]
                        // Rewrites `g(args)` to `g[args]` in the kernel
                        // body. Lowering's dispatch already emits IRIndex
                        // rather than IRApp(IRVar(g)), so this is a no-op
                        // in practice; kept for defense in depth.
                        let rewriter e =
                            match e with
                            | IRApp (IRVar (id, ty), args, _) when id = param.VarId ->
                                IRIndex (IRVar (id, ty), args, None)
                            | _ -> e
                        let body = mapIRExpr rewriter callable.Body
                        // Element type of the inner sub-array (for the param binding type).
                        let arrElemStr = elemTypeToCpp arrType.ElemType
                        // Element type of the OUTPUT (per-row result).
                        let outElem =
                            match info.OutputType with
                            | ArrayElem a -> a.ElemType
                            | IRTScalar _ as t -> t
                            | _ ->
                                match inferExprType body with
                                | IRTScalar _ as t -> t
                                | ArrayElem a -> a.ElemType
                                | _ -> arrType.ElemType
                        let outElemStr = elemTypeToCpp outElem
                        // Sub-array binding name.
                        let subName = $"{name}__sub"
                        let nameMap0 = Map.add param.VarId subName ctx.VarNames
                        let nameMap =
                            callable.Captures
                            |> List.fold (fun m c -> Map.add c.Id c.Name m) nameMap0
                        // Array-valued rows render as a CALL to the lifted
                        // callable when the body is not expression-shaped (or
                        // ends in an array literal); `body` above is the same
                        // tree with the defensive `g(args)` -> `g[args]`
                        // rewrite, which the call form does not need.
                        let bodyExpr =
                            if outputHasDenseTrailing then peelBodyExpr callable else body
                        let bodyStr = exprToCpp nameMap bodyExpr
                        let originLabel =
                            if isGroupedOuter then "grouped array" else "ragged literal"
                        // The peeled sub-row's C++ type must match the kernel
                        // PARAM's type, because the body's length accessor is
                        // generated from that param type (a rank-1 ragged/dep-idx
                        // param renders as RaggedRow<T>, using `.len`; anything
                        // else is Array<T,1>, using `.extents[0]`). Emitting a
                        // type that disagrees with the body's accessor produces
                        // `RaggedRow.extents[0]` / `Array.len` mismatches. So we
                        // derive the peel type from param.Type via the same
                        // predicate cppArrayTypeStr uses.
                        let paramIsRaggedRow =
                            match param.Type with
                            | ArrayElem at -> declaresAsRaggedRow at
                            | _ -> false
                        let subDeclLines =
                            if paramIsRaggedRow then
                                // RaggedRow: inline `len` scalar, no separate _extents.
                                [ $$"""{{ind}}    RaggedRow<{{arrElemStr}}> {{subName}} = { {{arrName}}[__g], {{perRowLenExpr}} };""" ]
                            else
                                // Array<T,1>: length via a materialized _extents buffer.
                                [ $"{ind}    size_t {subName}_extents[1] = {{{perRowLenExpr}}};"
                                  $$"""{{ind}}    Array<{{arrElemStr}}, 1> {{subName}} = { {{arrName}}[__g], {{subName}}_extents };""" ]
                        let outRank =
                            match info.OutputType with
                            | ArrayElem a -> arrayRank a
                            | _ -> 1
                        let code =
                            if outputHasDenseTrailing && outRank >= 2 then
                                // ARRAY-VALUED ROWS. Same emission the grouped
                                // co-iteration uses -- one helper, so a
                                // single-operand ragged literal and a k-operand
                                // zip cannot disagree about the extents ABI,
                                // the two sizing forms, or the row copy.
                                emitRowValuedPeel
                                    ($"{ind}// ragged peel over {originLabel} '{arrName}' -- array-valued rows")
                                    " (inner extents from the first row, length-agnostic callee)"
                                    ngroupsExpr subDeclLines bodyExpr bodyStr outElemStr outRank
                            else
                            [ $"{ind}// ragged peel over {originLabel} '{arrName}'" ]
                            // Return-extent ABI (see emitExtentsTable): `Array<T,R>`
                            // stores only a POINTER to its extents, so a frame-local
                            // `size_t[R]` table dangles the moment the wrapper crosses
                            // a call boundary -- the caller then sizes its own
                            // allocations off garbage (measured as bad_alloc/BL8005 in
                            // the first consumer that allocates from the shape). Every
                            // entry here is a runtime read (`gk__ngroups`), so the
                            // helper always picks its heap form. The table is
                            // deliberately NOT registered for a free: it leaves with
                            // the value whenever this result is returned, and the few
                            // bytes it costs otherwise are not worth a second
                            // ownership rule beside the pool's.
                            @ fst (emitExtentsTable ind (name + "_extents") 1 [(ngroupsExpr, false)])
                            @ [ $$"""{{ind}}Array<{{outElemStr}}, 1> {{name}} = { new {{outElemStr}}[{{ngroupsExpr}}], {{name}}_extents };""" ]
                            // Row-disjoint by construction, so no data clause is
                            // needed and adding one would be wrong: `subDeclLines`
                            // are emitted INSIDE the body (C++ block scope already
                            // makes them per-thread), `bodyStr` is expression-only
                            // so any accumulator lives in its own IIFE, and each
                            // iteration writes only `name[__g]`. Per-row reduce
                            // ORDER is untouched -- only which row a thread reaches
                            // first is reordered -- so the values are bitwise
                            // identical to the serial emission.
                            @ peelRowPragma peelOmpRequested peelRowLicensed (peelStreamBlocker ()) ind
                            @ [ $$"""{{ind}}for (size_t __g = 0; __g < {{ngroupsExpr}}; __g++) {""" ]
                            @ subDeclLines
                            @ [ $"{ind}    {name}[__g] = {bodyStr};"
                                $"{ind}}}" ]
                        // Raw `new T[n]` backing (not allocate<>). The extents
                        // table is deliberately unowned here (`None`) for the
                        // reason spelled out beside its emission above -- it
                        // leaves with the value. Scalar rank-1 outputs only: the
                        // array-valued arm allocates its pool through
                        // `allocate<>` instead, and an array-valued kernel body
                        // otherwise makes the row entries ALIASES of storage
                        // owned elsewhere.
                        (match outElem with
                         | IRTScalar _ when not (outputHasDenseTrailing && outRank >= 2) ->
                             registerAlloc (RawArrayData (name, None, None))
                         | _ -> ())
                        Some code
                    | _ -> None
    
    // MPI decomposition request: the resolved kernel opted into `mpi` AND the
    // emit gate is on (blade run --mpi N / the MPI test block). With the gate
    // off the clause is fully inert -- every path below behaves exactly as
    // before. Checked HERE, before the special-case dispatches, so ragged /
    // grouped applications of an mpi kernel error loudly instead of silently
    // taking a serial special path.
    let mpiRequested =
        match resolveKernel info.Kernel with
        | Some rk -> rk.Callable.IsMpiParallel && mpiEmitModeEnabled ()
        | None -> false
    let mpiError (reason: string) : string list =
        [refusalErrorLine ind ($"mpi: kernel for '{name}' is not MPI-eligible: {reason}")]

    // SAME-KEYS GROUPED CO-ITERATION: `method_for(zip(g1, ..., gk)) <@>
    // lambda(r1, ..., rk) -> <scalar>`, every operand a group_by result over
    // the SAME group_keys binding. TypeCheck (inferMethodFor) has already
    // refused operands grouped by different keys, so ONE offsets table drives
    // the walk and group g of every operand is peeled at the same __g -- the
    // single-operand peel with k row params instead of one.
    //
    // The kernel must COLLAPSE its rows to a scalar. A row-shaped result would
    // need a grouped OUTPUT type, whose lengths downstream consumers resolve
    // through ctx.GroupedArrays -- which this site cannot extend, exactly as
    // in the single-operand elementwise-over-group_by case. Falls through to
    // the standard-nest gate below when it cannot be served, so an
    // unsupported shape still gets the honest refusal rather than a nest that
    // knows nothing about per-row lengths.
    let tryGroupedZipPeel () : string list option =
        if info.Arrays.Length < 2 || info.Arrays.Length <> info.ArrayTypes.Length then None
        else
        // Every operand grouped, and all by the same group_keys emission.
        let gkNames =
            List.map2 (fun a (at: IRArrayType) ->
                match at.IndexTypes with
                | outer :: _ when outer.IxKind = IxKGroupOuter ->
                    Map.tryFind (exprToCppCtx ctx a) ctx.GroupedArrays
                | _ -> None) info.Arrays info.ArrayTypes
        match gkNames with
        | (Some gkName) :: _ when gkNames |> List.forall ((=) (Some gkName)) ->
            let ngroupsExpr = $"{gkName}__ngroups"
            let perRowLenExpr = $"{gkName}__offsets[__g + 1] - {gkName}__offsets[__g]"
            let outputIsRowShaped =
                match info.OutputType with
                | ArrayElem a when a.IndexTypes.Length >= 2 ->
                    a.IndexTypes |> List.skip 1 |> List.exists (fun ix ->
                        isRaggedFamilyKind ix.IxKind || ix.IxKind = IxKGroupMember || ix.IxKind = IxKDepInner)
                | _ -> false
            match resolveCallable info.Kernel with
            | Some callable when callable.Params.Length = info.Arrays.Length && outputIsRowShaped ->
                Some (codegenError ctx ind "co-iterating grouped arrays supports only a row-CONSUMING kernel (one that collapses its rows to a scalar, e.g. prodsum(ra, rb) or reduce); a row-shaped result would need a grouped output type, which has no downstream support -- map the values BEFORE group_by instead")
            | Some callable when callable.Params.Length = info.Arrays.Length ->
                let outElem =
                    match info.OutputType with
                    | ArrayElem a -> a.ElemType
                    | IRTScalar _ as t -> t
                    | _ ->
                        match inferExprType callable.Body with
                        | IRTScalar _ as t -> t
                        | ArrayElem a -> a.ElemType
                        | _ -> (List.head info.ArrayTypes).ElemType
                let outElemStr = elemTypeToCpp outElem
                // One peeled row per operand, all at the SAME __g and the same
                // per-row length. Each row's C++ type follows its own param
                // type, by the rule the single-operand peel uses: a rank-1
                // ragged/dep-idx param is a RaggedRow<T> (inline `.len`),
                // anything else an Array<T,1> with a materialized _extents.
                let rowDecls =
                    List.mapi2 (fun i (param: IRParam) (at: IRArrayType) ->
                        let subName = $"{name}__sub{i}"
                        let arrName = exprToCppCtx ctx info.Arrays.[i]
                        let elemStr = elemTypeToCpp at.ElemType
                        let paramIsRaggedRow =
                            match param.Type with
                            | ArrayElem pt -> declaresAsRaggedRow pt
                            | _ -> false
                        let lines =
                            if paramIsRaggedRow then
                                [ $$"""{{ind}}    RaggedRow<{{elemStr}}> {{subName}} = { {{arrName}}[__g], {{perRowLenExpr}} };""" ]
                            else
                                [ $"{ind}    size_t {subName}_extents[1] = {{{perRowLenExpr}}};"
                                  $$"""{{ind}}    Array<{{elemStr}}, 1> {{subName}} = { {{arrName}}[__g], {{subName}}_extents };""" ]
                        (param.VarId, subName, lines)) callable.Params info.ArrayTypes
                let nameMap =
                    let withRows =
                        rowDecls |> List.fold (fun m (vid, sub, _) -> Map.add vid sub m) ctx.VarNames
                    // Captures are a FALLBACK, never an override (same
                    // precedence rule as the kernel-body sites): a capture of
                    // a block-local `let` is renamed `__v<id>` in this scope,
                    // and ctx.VarNames already carries that emitted name --
                    // overriding it with the SOURCE name forwards an
                    // undeclared identifier.
                    callable.Captures
                    |> List.fold (fun m c -> if Map.containsKey c.Id m then m else Map.add c.Id c.Name m) withRows
                // S2 routing, in the peel's own idiom: a body that materializes
                // an array cannot render as one C++ expression, so call the
                // lifted callable instead of inlining its text. The peel's row
                // declarations already bind each param name, and the lifted
                // signature takes those same RaggedRow/Array row types, so the
                // call is the row decls handed straight through. (Array-literal
                // tails included -- see peelBodyExpr.)
                let bodyExpr = peelBodyExpr callable
                let bodyStr = exprToCpp nameMap bodyExpr
                let opNames = info.Arrays |> List.map (exprToCppCtx ctx) |> String.concat ", "
                // ARRAY-VALUED KERNEL RETURN over a GROUPED co-iteration (stage
                // S3, manifestation M-C3 -- lswosa's `family_spectra` shape).
                // The kernel collapses each pair of rows to a whole DENSE row
                // (a per-segment spectrum), so the output is
                // [ngroups] x [kernel T-dims]: rank >= 2, one group per outer
                // cell. `outputIsRowShaped` above has already taken the
                // genuinely grouped-shaped results away, so everything reaching
                // here has dense trailing axes and a flat row-major pool.
                //
                // The trailing extents come from the OUTPUT TYPE, the same
                // source the dense nest uses; the scalar form below is
                // unchanged (rank 1, raw `new T[ngroups]`).
                let outRank =
                    match info.OutputType with
                    | ArrayElem a -> arrayRank a
                    | _ -> 1
                let code =
                    if outRank >= 2 then
                        emitRowValuedPeel
                            ($"{ind}// same-keys grouped co-iteration over ({opNames}) via {gkName} -- array-valued rows")
                            ", inner extents from the first row (length-agnostic callee)"
                            ngroupsExpr (rowDecls |> List.collect (fun (_, _, lines) -> lines))
                            bodyExpr bodyStr outElemStr outRank
                    else
                        [ $"{ind}// same-keys grouped co-iteration over ({opNames}) via {gkName}" ]
                        // Heap extents (return-extent ABI, see the ragged peel above).
                        @ fst (emitExtentsTable ind (name + "_extents") 1 [(ngroupsExpr, false)])
                        @ [ $$"""{{ind}}Array<{{outElemStr}}, 1> {{name}} = { new {{outElemStr}}[{{ngroupsExpr}}], {{name}}_extents };""" ]
                        // Every operand peels at the SAME `__g` from ONE offsets
                        // table, so `peelRowLicensed`'s `List.max` over the depths
                        // is `IR.coIterLicense`'s rule literally: no per-argument
                        // level ownership to distinguish, most permissive wins.
                        // Row-disjoint exactly as the single-operand peel is -- the
                        // `rowDecls` are emitted inside the body, and each
                        // iteration writes only `name[__g]`.
                        @ peelRowPragma peelOmpRequested peelRowLicensed (peelStreamBlocker ()) ind
                        @ [ $$"""{{ind}}for (size_t __g = 0; __g < {{ngroupsExpr}}; __g++) {""" ]
                        @ (rowDecls |> List.collect (fun (_, _, lines) -> lines))
                        @ [ $"{ind}    {name}[__g] = {bodyStr};"
                            $"{ind}}}" ]
                // Raw `new T[n]` backing with an unowned extents table, exactly
                // as the single-operand peel registers for its scalar-output
                // form (and for the same recorded reason).
                (match outElem with
                 | IRTScalar _ when outRank < 2 -> registerAlloc (RawArrayData (name, None, None))
                 | _ -> ())
                Some code
            | _ -> None
        | _ -> None

    let raggedResult =
        match tryRaggedPeel () with
        | Some c -> Some c
        | None -> tryGroupedZipPeel ()
    if raggedResult.IsSome then
        if mpiRequested then mpiError "ragged/grouped iteration domains are not decomposed (v1)"
        else raggedResult.Value
    else

    // Ragged/grouped operands are handled ONLY by tryRaggedPeel (single
    // array, single-param kernel). Anything that slipped past it -- a
    // multi-array method_for mixing a ragged operand with others, or a
    // multi-param kernel over a ragged array -- would fall into the standard
    // loop nest, which knows nothing about per-row lengths: it would emit a
    // doubled-up dense nest over the placeholder inner extent and index the
    // output 2D. That is SILENTLY WRONG code, so gate it loudly here.
    // (DepIdx-tagged arrays are deliberately not gated: their dependent
    // bounds have their own standard-nest handling.) Co-iteration semantics
    // for ragged operands -- e.g. aligning a dense per-row array against
    // ragged rows -- are a language-design question for the rewrite spec.
    //
    // WHAT THE HAZARD ACTUALLY IS, and why `IxKGroupOuter` alone is not it.
    // Read the gate's own reason: "knows nothing about PER-ROW LENGTHS ... the
    // PLACEHOLDER inner extent". That is a statement about storage, and only
    // one axis kind creates it. A genuine `group_by` result is
    //     size_t grouped_extents[2] = {gk__ngroups, 0};  // inner extent is ragged
    //     Array<double*, 1> grouped = { new double*[gk__ngroups], grouped_extents };
    //     for (__g) grouped[__g] = new double[__sz];     // rows differ in length
    // -- a row-pointer table whose inner extent is a literal 0 placeholder. The
    // standard nest would read that 0 as a bound. Hazard, correctly gated.
    //
    // A GROUP-DERIVED DENSE RESULT is a different object that merely shares the
    // provenance tag. `method_for(zip(ga, gb)) <@> <row-consuming kernel>` gives
    //     size_t dots_extents[1] = {gk__ngroups};
    //     Array<double, 1> dots = { new double[gk__ngroups], dots_extents };
    // and its array-valued sibling (stage S3) gives a rank-2
    //     size_t grid_extents[2] = {gk__ngroups, 2};
    //     Array<complex<double>, 2> grid = { allocate<...>(grid_extents), ... };
    // Every extent is REAL, every row is the same length, the pool is flat. The
    // `IxKGroupOuter` on axis 0 records where the axis came from; it does not
    // claim ragged storage, and there is no per-row length to know about. An
    // elementwise map over it is an ordinary dense map -- iteration follows the
    // input record, and that record is dense.
    //
    // So the test is the RAGGED-FAMILY / GROUP-MEMBER axes, the ones whose
    // extent varies per row. `IxKGroupOuter` is admitted only when it appears
    // ALONE (no ragged or member axis anywhere in the same operand), which is
    // exactly the group-derived-dense shape and never `group_by`'s own output
    // (whose inner axis is IxKGroupMember by construction).
    //
    // sql-group-by/020 is untouched by this: mapping a `group_by` result is a
    // SINGLE array with a 1-param kernel, so it is taken by `tryRaggedPeel`'s
    // `outputIsGroupShaped` arm ("elementwise map over a group_by result") and
    // never reaches this gate. Verified: the two refusals carry different text,
    // and 020 still emits its own.
    //
    // SECOND CONDITION, and the reason it is here rather than assumed. The
    // admission also requires every axis to be an S-DIMENSION. An operand that
    // carries a `Kind = TDimension` axis is mis-iterated one layer below this
    // gate -- the grid excludes T-dims, so the nest peels a row and binds the
    // kernel param to it instead of to a cell. That is NOT a grouping problem:
    // it reproduces with no grouping at all, on a plain dense rank-2 array
    // produced by an array-valued kernel (stage S3), and it is blocked in the
    // TYPE side's operand handling, not here. Admitting a T-dim-carrying
    // operand would therefore trade this gate's clean refusal for a raw g++
    // error, so the group-derived shapes that are admitted are exactly the ones
    // whose axes are all iterable: `dots`-style rank-1 group results, and any
    // group-outer grid whose remaining axes are ordinary S-dims.
    let raggedStandardNestOperand =
        let variesPerRow (ix: IRIndexTypeG<IRExpr>) =
            match ix.IxKind with
            | IxKRagged | IxKRaggedInline | IxKRaggedOpaque | IxKGroupMember -> true
            | _ -> false
        info.ArrayTypes |> List.exists (fun at ->
            at.IndexTypes |> List.exists (fun ix ->
                match ix.IxKind with
                | IxKRagged | IxKRaggedInline | IxKRaggedOpaque
                | IxKGroupMember -> true
                | IxKGroupOuter ->
                    // Gated only when a per-row-varying axis rides along with
                    // it (a real `group_by` result). A lone group-outer axis is
                    // a dense count-of-groups and iterates like any other array.
                    at.IndexTypes |> List.exists variesPerRow
                | _ -> false))
    if raggedStandardNestOperand then
        codegenError ctx ind "method_for over a ragged or grouped operand supports only the single-array, single-row-param form (lambda(g) -> ...) or an elementwise map (lambda(e) -> ...); mixing ragged operands with other arrays or multi-param kernels is not yet supported"
    else
    
    // Pre-materialize any inline array expressions (mask, intersect, union, etc.)
    // These need to be bound to temporary variables before the loop nest can reference them.
    let mutable preCode = []
    let mutable tempCtx = ctx
    let materializedArrays =
        info.Arrays |> List.mapi (fun i arr ->
            match arr with
            | IRVar (id, _) ->
                let name = Map.tryFind id tempCtx.VarNames |> Option.defaultValue ($"arr{i}")
                (name, arr)
            | IRRange (idxTys, _) when idxTys |> List.exists (fun ix -> ix.IxKind = IxKCompound) ->
                // range<CompoundIdx<m>>: materialize the standalone
                // compound_index_t for the driver BEFORE the loop nest. The
                // loop header bounds off `<name>_cidx->cardinality` and the
                // element bindings extract per-axis coordinates via
                // `<name>_cidx->unhash(r)[c]` (genElementBindingNew). The
                // compound slot must be the range's SOLE index type for now;
                // mixing (range<CompoundIdx<m>, J>) is unsupported.
                let rname = $"__range{i}"
                (match idxTys with
                 | [ix] ->
                     (match ix.Extent with
                      | IRCompoundMask (IRVar (mid, _)) ->
                          (match Map.tryFind mid tempCtx.VarNames with
                           | Some maskName ->
                               let (idxLines, _) = genCompoundIndexFromMask maskName ix.Rank ($"{rname}_cidx")
                               preCode <- preCode @ (idxLines |> List.map (fun s -> ind + s))
                               // Owns the standalone driver index. Registered
                               // BEFORE any compound map output that shares it:
                               // newest-first pop then frees the output's data
                               // before this index dies. Do not move after.
                               registerShapedAlloc ($"{rname}_cidx")
                                   "deallocate_compound_index_only" ($"{rname}_cidx")
                           | None ->
                               preCode <- preCode @ codegenError ctx ind "range<CompoundIdx>: mask variable not found in scope at codegen")
                      | _ ->
                          preCode <- preCode @ codegenError ctx ind "range<CompoundIdx>: the mask must be a NAMED Array<Bool like ...> variable (inline mask expressions are not supported); let-bind the mask first")
                 | _ ->
                     preCode <- preCode @ codegenError ctx ind "range<CompoundIdx<m>, ...>: a compound range slot cannot be combined with other index types in one range<> (not yet supported)")
                (rname, arr)
            | IRRange (idxTys, _) when idxTys |> List.exists (fun ix -> ix.IxKind = IxKSparse) ->
                // range<SparseIdx<keys>>: materialize the standalone
                // sparse_index_t for the driver BEFORE the loop nest -- the
                // sparse twin of the compound arm above. The driver reuses the
                // `_cidx` name suffix so the shared cardinality/unhash sites
                // (genLoopBoundExpr, genElementBindingNew, the extents fill)
                // serve both tabulated kinds unchanged. Iteration visits the
                // keys in GIVEN order (never sorted).
                let rname = $"__range{i}"
                (match idxTys with
                 | [ix] ->
                     (match ix.Extent with
                      | IRSparseKeys (SkStatic _ as src) ->
                          let idxLines = genSparseIndexFromKeys src None ix.Rank ($"{rname}_cidx")
                          preCode <- preCode @ (idxLines |> List.map (fun s -> ind + s))
                          registerShapedAlloc ($"{rname}_cidx")
                              "deallocate_compound_index_only" ($"{rname}_cidx")
                      | IRSparseKeys (SkRuntime (IRVar (kid, _)) as src) ->
                          (match Map.tryFind kid tempCtx.VarNames with
                           | Some keysName ->
                               let idxLines = genSparseIndexFromKeys src (Some keysName) ix.Rank ($"{rname}_cidx")
                               preCode <- preCode @ (idxLines |> List.map (fun s -> ind + s))
                               registerShapedAlloc ($"{rname}_cidx")
                                   "deallocate_compound_index_only" ($"{rname}_cidx")
                           | None ->
                               preCode <- preCode @ codegenError ctx ind "range<SparseIdx>: keys variable not found in scope at codegen")
                      | _ ->
                          preCode <- preCode @ codegenError ctx ind "range<SparseIdx>: the keys must be a `let static` tuple list or a NAMED rank-1 tuple-array variable (inline keys expressions are not supported); let-bind the keys first")
                 | _ ->
                     preCode <- preCode @ codegenError ctx ind "range<SparseIdx<keys>, ...>: a sparse range slot cannot be combined with other index types in one range<> (not yet supported)")
                (rname, arr)
            | IRRange _ -> ($"__range{i}", arr)
            | IRVirtualReverse _ -> ($"__rev{i}", arr)
            | IRMask _ | IRIntersect _ | IRUnion _ | IRUnique _ ->
                // Auto-materialize: when a method_for receives an inline form
                // as one of its arrays, generate a temporary binding before
                // the loop nest. The lift pass deliberately leaves
                // these in IRMethodFor.Arrays slots (treated as a "blessed
                // position"), routing them through this path instead.
                //
                // Strict elem-type inference (with #error on unresolvable)
                // happens here; the shared `materializeInlineForm` helper
                // emits the C++ template.
                let tmpName = $"{name}__tmp{i}"
                let tmpId = builder.FreshId()
                let tmpType = inferExprType arr
                let (elemET, autoMaterErr) = inferElemTypeStrict tempCtx ind arr "auto-materialize"
                let elemStr = elemTypeToCpp elemET
                preCode <- preCode @ autoMaterErr
                let matStmts =
                    match materializeInlineForm emptySubst tempCtx.VarNames tmpName (lazy elemStr) arr with
                    // Statement position: the temp is declared in the SAME block
                    // as the loop nest that reads it, and nothing downstream
                    // retains a pointer into it (compound() copies the mask bits
                    // into a std::vector<bool> at construction), so it is
                    // scope-owned like any other let-level materialization.
                    | Some (s, allocs) -> registerMaterializedAllocs allocs; s
                    | None -> []
                let code = matStmts |> List.map (fun s -> ind + s)
                preCode <- preCode @ code
                tempCtx <- addVarName tmpId tmpName tempCtx
                (tmpName, IRVar (tmpId, tmpType))
            | IRSort _ | IRGroupKeys _ | IRGroupBy _ | IRGroupBucket _ | IRGroupSizes _ ->
                // Per design decision: these operations require let-binding.
                // Auto-materializing them inline would require duplicating their
                // codegen here (mask/intersect/union do it because they predate
                // the let-only convention), and we deliberately stopped paying
                // that cost. Surface a clear error instead of emitting bad C++.
                let opName =
                    match arr with
                    | IRSort _ -> "sort"
                    | IRGroupKeys _ -> "group_keys"
                    | IRGroupBy _ -> "group_by"
                    | IRGroupBucket _ -> "group_bucket"
                    | IRGroupSizes _ -> "extents"
                    | _ -> "?"
                let errCode = codegenError ctx ind ($"'{opName}' must be let-bound before use in method_for; e.g. let s = {opName}(...) then method_for(s)")
                preCode <- preCode @ errCode
                ($"arr{i}", arr)
            | _ -> ($"arr{i}", arr))
    
    let arrayNames = materializedArrays |> List.map fst
    let updatedArrays = materializedArrays |> List.map snd
    let info = { info with Arrays = updatedArrays }

    // HALO-EXTENT RUNTIME GUARD (BL8009; the runtime half of TypeCheck's
    // HaloExtentMismatch/BL3016). A halo's declared inner extent is a
    // compile-time literal, but the array read through the window can have a
    // RUNTIME extent -- a group_by count, typically -- which typecheck cannot
    // compare. The window loop is bounded by the DECLARED extent, so a
    // disagreement is an out-of-bounds read (oversized halo) or silently
    // fewer windows (undersized). One comparison per (array, slot) pair,
    // emitted ONCE before the nest -- not per element.
    //
    // A dense window read lowers to `IRBinOp(IRAdd, w, off)` with `w` typed
    // by the "__halowin|d:" tag (Lowering's window-read arm); the guard keys
    // on exactly that shape inside the kernel body. Targets whose emitted
    // name is unknown here (not in ctx.VarNames) are skipped rather than
    // guessed -- fail-open, never an undeclared identifier in the guard.
    (let haloDecl =
        info.Arrays
        |> List.collect (fun a -> match a with IRRange (ixs, _) -> ixs | _ -> [])
        |> List.choose (fun ix ->
            match ix.Tag with
            | Some tag when tag.StartsWith (haloWinTagPrefix + "d:") ->
                // The declared inner extent is the window's forward DEMAND
                // over the whole shrunk output: N = M + Shrink.
                (match ix.Extent, haloAccessOfTag tag with
                 | IRLit (IRLitInt shrunk), Some h -> Some (tag, snd (haloDemand h (0L, shrunk)))
                 | _ -> None)
            | _ -> None)
        // Same-tag ambiguity rule as TypeCheck's haloExtentClash: the tag
        // carries name + offsets, not the extent, so two anonymous halos with
        // equal offsets but different extents collide -- drop the tag rather
        // than guard one window against the other's extent.
        |> List.groupBy fst
        |> List.choose (fun (tag, entries) ->
            match entries |> List.map snd |> List.distinct with
            | [ n ] -> Some (tag, n)
            | _ -> None)
        |> Map.ofList
     if not (Map.isEmpty haloDecl) then
        match resolveCallable info.Kernel with
        | None -> ()
        | Some callable ->
            // Every window read in the body, from the one shared scan
            // (IRAccess.windowReadsOf), computed offsets included -- the
            // extent obligation does not depend on the offset.
            let mutable guards : (string * int * int64) list = []
            for r in Blade.IRAccess.windowReadsOf (fun wv -> (Blade.IRAccess.denseHaloTagOf wv).IsSome) callable.Body do
                match Blade.IRAccess.denseHaloTagOf r.Window |> Option.bind (fun t -> Map.tryFind t haloDecl) with
                | Some declared ->
                    (match Map.tryFind r.ArrayId tempCtx.VarNames with
                     // a STREAMED source has no array here to check: its extent
                     // is the store's, and the segment-run window alias carries
                     // exactly the declared one (segmentRun below)
                     | Some tname when not (Map.containsKey tname ctx.StreamedArrays) -> guards <- (tname, r.Dim, declared) :: guards
                     | _ -> ())
                | None -> ()
            let guardLines =
                guards
                |> List.distinct
                |> List.rev
                |> List.collect (fun (tname, d, declared) ->
                    [ $$"""{{ind}}if ((int64_t)({{tname}}.extents[{{d}}]) != {{declared}}LL) {"""
                      $"{ind}    std::cerr << \"Blade runtime: the halo window over '{tname}' declares an inner extent of {declared}, but '{tname}' has runtime extent \" << {tname}.extents[{d}] << \" on index slot {d} -- the window walk would read out of bounds (oversized halo) or silently emit fewer windows (undersized)\" << std::endl;"
                      $"{ind}    blade_rt::panic(\"BL8009\", \"halo extent mismatch\", nullptr, 0);"
                      $"{ind}}}" ])
            preCode <- preCode @ guardLines)

    if arrayNames.IsEmpty then
        codegenError ctx ind ($"no arrays in method_for for '{name}' -- kernel cannot be applied")
    else
    // A DEDUCED WREATH OUTPUT takes the whole application, ahead of
    // buildLoopNestCodeGen -- which would refuse a wreath INPUT outright (the
    // depth >= 3 shape) and, for depth 2, would build a two-simplex nest over a
    // pool that is not two simplices. Same predicate, same arguments as
    // deduceOutputType's, so codegen and the deduced type cannot disagree about
    // whether this application is a wreath one.
    let wreathVerdict =
        match resolveKernel info.Kernel with
        | Some rk ->
            deduceWreathTie info.ArrayTypes info.Identities
                            (rk.Callable.CommGroups @ rk.Callable.AntisymGroups)
                            (if info.HasReynolds then [] else rk.Callable.AntisymGroups)
                            info.KernelTDims
                            (info.KernelInputRanks |> List.exists (fun r -> r > 0))
                            info.HasReynolds
                            rk.Callable.SignParities
        | None -> WreathNoTie
    let wreathTie =
        match wreathVerdict with
        | WreathTied t -> Some t
        | WreathNoTie -> None
        | WreathKernelNotOdd (argPos, _, _) ->
            // Unreachable: the typecheck seam runs the same call with the same
            // arguments (SignParities is the summary it recorded) and refuses
            // the program before codegen exists. Loud, not a silent fallback.
            failwith (sprintf "internal: codegen reached a wreath tie whose kernel is not \
provably sign-odd in tied argument %d; typecheck should have refused this application" argPos)
    match wreathTie with
    | Some tie when mpiRequested ->
        mpiError "an OrbIdx (iterated-wreath) output is not decomposed (pool slicing by rank ranges is not implemented)"
    | Some tie ->
        (match genWreathApply ctx ind name info arrayNames tie with
         | Some lines -> preCode @ [""] @ lines
         | None ->
             preCode @ codegenError ctx ind (sprintf
                "OrbIdx%s output for '%s': the wreath nest needs a resolvable kernel and a COMPILE-TIME extent (the pool size is the iterated-binomial fold over the level list starting from the extent)"
                (ppOrbitLevels tie.OutputLevels) name))
    | None ->
        // Build LoopNestCodeGen (handles both outer product and co-iteration)
        // S2: a body that materializes an array is routed to a CALL of the
        // lifted callable before any emitter sees it (routeKernelBodyThroughCall).
        let codeGen = routeKernelBodyThroughCall info (buildLoopNestCodeGen info arrayNames name builder)

        // S2, kernel-body materialization (docs/plan-kernel-body-
        // materialization.md): a kernel body that cannot be rendered as an
        // inline expression -- it holds a combinator form only the
        // STATEMENT-form emitters can materialize (`let e = exp <@> (...)`,
        // a re-synthesized elementwise broadcast) -- is emitted as a CALL to
        // its lifted callable instead of inlined text. The lifted function
        // compiles these bodies correctly already (genFuncBodyScoped); the
        // exprToCppCore IRApp arm forwards its captures via
        // captureForwardName. Scoped tightly: only bodies that would
        // otherwise emit a BLADE_CODEGEN_ERROR sentinel reroute, so no
        // working nest changes emission; Reynolds keeps the inline path (a
        // permutation sum rewrites the body text, which a call cannot).
        let codeGen =
            let rec bodyNeedsStatementForm (e: IRExpr) : bool =
                let mutable found = false
                iterIRExpr (fun x ->
                    match x with
                    | IRApplyCombinator _ | IRComposeApply _ | IRReduceCompute _
                    | IRCompute (IRApplyCombinator _) -> found <- true
                    | _ -> ()) e
                found
            if codeGen.HasReynolds || not (bodyNeedsStatementForm codeGen.KernelExpr) then codeGen
            else
                match resolveKernel info.Kernel with
                | Some rk ->
                    let args = rk.Callable.Params |> List.map (fun p -> IRVar (p.VarId, p.Type))
                    { codeGen with KernelExpr = IRApp (IRVar (rk.Callable.Id, IRTUnit), args, rk.Callable.RetType) }
                | None -> codeGen

        // CO-ITERATION EXTENT GUARD (BL8011; the runtime half of TypeCheck's
        // extent agreement, BL3016/coIterClash). A co-iteration level is
        // bounded by its FIRST operand's extent and peels EVERY operand at
        // that level, so a shorter later operand is read past its end. The
        // checker compares literal extents at the call site, but inside a
        // function over `T^k` parameters the extents are the caller's, and
        // a curried call (`f(a)(b)`, `let h = f(a); h(b)`) or an inner-axis
        // disagreement (`T^2 + T^2` with equal leading extents) reached the
        // nest unchecked -- `[5, 7, <garbage>]` for a 3-vector plus a
        // 2-vector. One comparison per (level, later operand), emitted ONCE
        // before the nest, only for REAL arrays whose records are dense or
        // symmetry-packed (`Array<T, R>` with `.extents`; a range is bounded
        // by its own record, a compound/ragged operand has no `.extents`).
        //
        // WHEN LITERALS ARE TRUSTED: two equal literals on NON-parameter
        // arrays are the checker's business and the allocation's truth, so
        // the guard is skipped there (no emitted-text change for the common
        // fully-static nest). A PARAMETER's literal is not a statement about
        // the runtime array -- shape monomorphization pins a `T^k` parameter
        // from one argument and the body's unification copies it onto the
        // other (`f_HM_..._e3` typed `b` at 3 while the call passed 2), so a
        // parameter operand is always compared at runtime. Mirrored by the
        // interpreter's materializeApply, which reads runtime extents anyway.
        (let isParamOperand (pos: int) =
            match List.tryItem pos info.Arrays with
            | Some (IRVar (id, _)) -> Set.contains id ctx.ParamIds
            | _ -> false
         let hasExtents (pos: int) =
            match List.tryItem pos info.ArrayTypes with
            | Some at -> at.IndexTypes |> List.forall (fun ix -> match ix with IxDense | IxSymmetryLike -> true | _ -> false)
            | None -> false
         let guardLines =
            codeGen.Bindings
            |> List.collect (fun b ->
                match b.FusedRank, b.Elements with
                | None, e0 :: rest when not rest.IsEmpty
                                        && (match e0.Virtual with RealArray -> true | _ -> false)
                                        && hasExtents e0.ArrayPosition ->
                    let litOf (e: ElementBinding) =
                        match List.tryItem e.ArrayPosition info.ArrayTypes with
                        | Some at -> literalExtentOfArray at e.DimIndex
                        | None -> None
                    let lit0 = litOf e0
                    let param0 = isParamOperand e0.ArrayPosition
                    let boundText =
                        match lit0 with
                        | Some n when not param0 -> $"{n}LL"
                        | _ -> $"(int64_t)({e0.ArrayName}.extents[{e0.DimIndex}])"
                    rest |> List.collect (fun e ->
                        let trusted =
                            not param0 && not (isParamOperand e.ArrayPosition)
                            && lit0.IsSome && litOf e = lit0
                        match e.Virtual with
                        | RealArray when hasExtents e.ArrayPosition && not trusted ->
                            [ $$"""{{ind}}if ((int64_t)({{e.ArrayName}}.extents[{{e.DimIndex}}]) != {{boundText}}) {"""
                              $"{ind}    std::cerr << \"Blade runtime: co-iteration over '{e0.ArrayName}' and '{e.ArrayName}' needs equal extents on axis {e.DimIndex}, but '{e0.ArrayName}' has \" << {e0.ArrayName}.extents[{e0.DimIndex}] << \" and '{e.ArrayName}' has \" << {e.ArrayName}.extents[{e.DimIndex}] << \" -- the walk is bounded by the first operand, so the shorter one would be read past its end\" << std::endl;"
                              $"{ind}    blade_rt::panic(\"BL8011\", \"co-iteration extent mismatch\", nullptr, 0);"
                              $"{ind}}}" ]
                        | _ -> [])
                | _ -> [])
         if not guardLines.IsEmpty then preCode <- preCode @ guardLines)

        // STENCIL OVER SEGMENTS (docs/plans/structural/07 §2.3): a halo map
        // whose window reads go to a rank-1 STREAMED variable on a `Chunked`
        // axis runs one segment at a time. Per run, the halo's own reach
        // (HaloAccess.haloDemand) says which input cells the run's windows
        // touch -- the run plus its ghost cells -- and exactly those are
        // read from the store into a window buffer; the variable's name is
        // then bound, inside the run, to an Array whose data pointer is the
        // window shifted back by the window's start, so the nest's GLOBAL
        // subscripts land in the window unchanged. The nest itself is the
        // ordinary one with its outermost bounds rewritten to the run's
        // interior ordinals (the MpiSlab bound variables, defined per run);
        // the carousel stays off, as under any slab. Nothing of the variable
        // exists outside the window.
        // The run plan: (run boundaries, first/one-past-last centre, reach
        // below/above, the centre shift the nest applies, the streamed
        // sources (cpp name, spec)). Two shapes produce one:
        //   - a HALO map whose window reads go to a streamed rank-1 source:
        //     runs are the `Chunked` alias's, the reach is the halo's;
        //   - an ELEMENTWISE map or zip with streamed rank-1 OPERANDS
        //     (StreamingIONotes v1 refused these: its only in-nest read was a
        //     whole trailing fiber at a site, and a rank-0 result has no fiber
        //     to read): runs are blocks of the store's own chunk edge, reach
        //     zero -- a run is exactly its window, and the traversal order is
        //     the store's.
        let streamedOf (e: IRExpr) =
            match e with
            | IRVar (aid, _) ->
                Map.tryFind aid ctx.VarNames
                |> Option.bind (fun vn -> Map.tryFind vn ctx.StreamedArrays |> Option.map (fun s -> (vn, s)))
            | _ -> None
        let segmentRun =
            match info.Arrays with
            | [ IRRange ([ ix ], _) ] ->
                (match ix.Tag with
                 | Some tag ->
                     (match Blade.Types.haloAccessOfTag tag with
                      | Some h when not h.IsCompound && h.Inner <> "" ->
                          (match Blade.Types.SegmentTable.tryFind h.Inner with
                           | Some offsets ->
                               let reads =
                                   Blade.IRAccess.windowReadsOf
                                       (fun wv -> (Blade.IRAccess.denseHaloTagOf wv).IsSome) codeGen.KernelExpr
                               let streamedReads =
                                   reads
                                   |> List.choose (fun r ->
                                       Map.tryFind r.ArrayId ctx.VarNames
                                       |> Option.bind (fun vn -> Map.tryFind vn ctx.StreamedArrays |> Option.map (fun s -> (vn, s, r.Rank))))
                                   |> List.distinctBy (fun (vn, _, _) -> vn)
                               (match streamedReads with
                                | [ (vn, spec, 1) ] when spec.VarType.IndexTypes.Length = 1 ->
                                    let n = (match spec.VarType.IndexTypes.[0].Extent with IRLit (IRLitInt n) -> n | _ -> List.last offsets)
                                    let reach = Blade.Types.haloReach h
                                    Some (offsets, h.Start, n - (h.Shrink - h.Start), int64 (List.min reach), int64 (List.max reach), h.Start, [ (vn, spec) ], "stencil")
                                | [] -> None
                                | _ ->
                                    raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan
                                        "a stencil over segments streams ONE rank-1 variable through the window; bind the others with .read")))
                           | None -> None)
                      | _ -> None)
                 | None -> None)
            | arrays ->
                let sources = arrays |> List.choose streamedOf |> List.distinctBy fst
                match sources with
                | [] -> None
                // a higher-rank streamed operand is the FIBER stream of
                // StreamingIONotes v1 (a rank-1 kernel parameter over the
                // trailing axis): that path stays as it is
                | _ when sources |> List.exists (fun (_, s) -> s.VarType.IndexTypes.Length <> 1) -> None
                | (_, s0) :: _ ->
                    let n = (match s0.VarType.IndexTypes.[0].Extent with IRLit (IRLitInt n) -> n | _ -> 0L)
                    let pspec = (Blade.ProviderRegistry.tryFind s0.Provider).Value
                    let block =
                        match pspec.StreamRowsBlock with
                        | Some f -> max 1L (f s0.FilePath s0.VarName)
                        | None -> 4096L
                    let runs = [ for b in 0L .. block .. n - 1L -> b ] @ [ n ]
                    Some (runs, 0L, n, 0L, 0L, 0L, sources, "elementwise")
        // inside the run each streamed source IS an array (its window alias)
        let ctx =
            match segmentRun with
            | Some (_, _, _, _, _, _, sources, _) ->
                sources |> List.fold (fun c (vn, _) -> { c with StreamedArrays = Map.remove vn c.StreamedArrays }) ctx
            | None -> ctx
        // STREAMED provider inputs (`alias.stream`): no materialized arrays
        // exist -- the nest inlines per-fiber reads at the S/T boundary.
        // Pre-allocate one destination buffer per streamed fiber binding (a
        // comm kernel holds several fibers of one source concurrently, so a
        // per-source buffer would be clobbered).
        let (streamedMap, streamPrologue, streamNewBufs) = streamedNestSetup ctx.StreamedArrays ind [codeGen]
        registerStreamBufDecls streamNewBufs

        // Get output rank and type info
        let outputRank =
            match codeGen.OutputType with
            | ArrayElem arr -> arrayRank arr
            | IRTScalar _ -> 0
            | _ -> 0

        let outputElemType =
            match codeGen.OutputType with
            | ArrayElem arr -> elemTypeToCpp arr.ElemType
            | IRTScalar et -> primTypeToCpp et
            | t -> irTypeToCpp t

        // MPI classification of the built nest (None when not requested / gate
        // off -- the clause is then fully inert and nothing below changes).
        let mpiShape = if mpiRequested then Some (classifyMpiShape codeGen) else None

        // Branch on output shape. Array output gets the full ceremony
        // (symmetry vector, extents declaration, allocation, then loop nest
        // with indexed assignments). Scalar output gets a single scalar
        // accumulator initialized to zero, then the same loop nest with
        // `+=` accumulation (genLoopNest detects this via codeGen.OutputType).
        // The scalar branch generalizes the Cartesian-sum-reduce pattern to
        // any number of input arrays through the shared LoopNestCodeGen
        // machinery (commutativity, Reynolds, etc. all carry through).
        match codeGen.OutputType with
        | IRTScalar _ when mpiRequested ->
            mpiError "scalar output is a cross-cell reduction (floating-point reassociation)"
        | IRTScalar _ ->
            // Scalar accumulator: declare initialized to 0, then run the
            // loop nest which accumulates into it via genLoopNest's `+=`.
            let scalarDecl = $"{ind}{outputElemType} {name} = 0;"
            let loopCode = genLoopNestStreamed streamedMap codeGen tempCtx.VarNames tempCtx.Indent
            preCode @ streamPrologue @ [scalarDecl; ""] @ loopCode
        | _ when (match mpiShape with Some (MpiIneligible _) -> true | _ -> false) ->
            let reason = match mpiShape with Some (MpiIneligible r) -> r | _ -> ""
            mpiError reason
        | _ when mpiShape = Some MpiSimplicial ->
            // Flat cell-range decomposition over the packed pool (the
            // triangular sibling of the dense slab below). Like the CUDA
            // inline path, the emitter replaces the whole array-output
            // ceremony (extents/alloc/loop/gather); shapes it can't take
            // (non-literal extent, multi-group output) error loudly rather
            // than silently serialize.
            let kernelCudaBlock =
                match resolveKernel info.Kernel with
                | Some rk when rk.Callable.IsCudaKernel && cudaEmitModeEnabled () -> Some rk.Callable.CudaBlockSize
                | _ -> None
            match kernelCudaBlock with
            | Some bs ->
                // `where mpi, cuda(...)`: rank-scoped device launch over
                // this rank's flat cell-range + cell-range Allgatherv.
                (match genCudaKernelSimplicial true false codeGen name bs with
                 | Some lines -> preCode @ [""] @ lines
                 | None -> mpiError "mpi+cuda hybrid: kernel shape is not device-eligible (single sym/antisym group, literal extent, MPI-datatype element required)")
            | None ->
            let innerOmp =
                match resolveKernel info.Kernel with
                | Some rk -> rk.Callable.IsOmpParallel
                | None -> false
            match genMpiNestSimplicial innerOmp codeGen name with
            | Some lines -> preCode @ [""] @ lines
            | None -> mpiError "symmetric shape outside the supported MPI scope (single sym/antisym group with literal extent required)"
        | _ ->
            // CUDA dispatch: if the resolved kernel opted into cuda AND the case
            // is in first-kernel scope, emit a device kernel (+ .cu wrapper) and
            // an inline launch instead of the host loop. genCudaKernel returns
            // None for out-of-scope cases, falling back to the host loop below.
            let cudaInline =
                match resolveKernel info.Kernel with
                | Some rk when rk.Callable.IsCudaKernel && cudaEmitModeEnabled () && mpiRequested ->
                    // `where mpi, cuda(...)` over a DENSE rectangular nest:
                    // the simplicial hybrid is implemented (rank-scoped
                    // launches over packed cell-ranges); the dense-slab
                    // device variant is not emitted yet. Loud rather than
                    // launching a full-extent kernel inside an MPI slab.
                    raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ("mpi+cuda hybrid for dense rectangular nests is not emitted yet (the sym/antisym simplicial hybrid is) -- run with one emit gate, or make the output a packed group")))
                | Some rk when rk.Callable.IsCudaKernel && cudaEmitModeEnabled () ->
                    // CUDA emission is gated: it only fires in the dedicated CUDA
                    // phase (which compiles+links the .cu). During ordinary
                    // host-only compilation the flag is off, so the `cuda` clause
                    // stays inert (host fallback) -- otherwise the emitted
                    // `extern "C"` launch call would be an undefined symbol at link
                    // time (the .cu isn't built in the host corpus).
                    // Try the symmetric rank-2 triangular path, then the
                    // antisymmetric rank-2 strict-triangular path, then the
                    // rectangular pointwise path; None => host loop.
                    // One general simplicial kernel handles any single S-group of
                    // arity >= 2 (symmetric inclusive / antisymmetric strict, any
                    // rank); then the rectangular pointwise path; None => host loop.
                    genCudaKernelSimplicial false false codeGen name rk.Callable.CudaBlockSize
                    |> Option.orElseWith (fun () -> genCudaKernel false tempCtx.VarNames codeGen name rk.Callable.CudaBlockSize)
                | _ -> None
            match cudaInline with
            | Some launchLines -> preCode @ [""] @ launchLines
            | None ->
            // Array output: symmetry vector, extents, allocation, loop nest.
            let symmVecName = $"{name}_symm"
            // When there's no symmetry, pass `nullptr` DIRECTLY as the template
            // argument rather than routing through a named local
            // `static constexpr const size_t* R_symm = nullptr`. MSVC rejects the
            // address of a function-local static as a constant expression in the
            // `if constexpr ((bool)SYMM && ...)` inside count_leaves/build_skeleton
            // (error C2131, "unevaluable pointer value") -- even when the value is
            // nullptr. Passing the `nullptr` literal sidesteps it and matches the
            // array-literal allocation path. g++ accepts both, so no regression.
            let symmArg =
                if hasRealSymmetry codeGen.OutputSymmVec then hoistSymmDecl symmVecName codeGen.OutputSymmVec
                else "nullptr"
            // No function-local symm decl: rectangular -> nullptr (literal),
            // symmetric -> hoisted to namespace scope (see hoistSymmDecl). Either
            // way nothing symm-related is declared inside main().

            // Generate extent computation. Each entry pairs its rendered value
            // with a structural is-literal bit from the SAME match: only the
            // IRLit arm is compile-time; a compound cardinality read and the
            // fused/`.extents[]` reads are runtime. All-literal tables take the
            // static constexpr form (emitExtentsTable), which also returns
            // None as the owned name so no delete[] is registered below.
            let extentsName = $"{name}_extents"
            let extentDims =
                codeGen.Bindings |> List.map (fun b ->
                    match b.Extent with
                    | IRLit (IRLitInt n) ->
                        (string n, true)
                    | IRCompoundMask _ ->
                        // Compound-inner halo level (the only compound level
                        // that reaches the DENSE output path -- plain compound
                        // ranges take the Compound-output branch): written
                        // cells = cardinality minus the interior shrink, which
                        // rides the binding's StrictOffset (see IR loop build).
                        let sub = if b.StrictOffset > 0 then $" - {b.StrictOffset}" else ""
                        ($"{b.ExtentArrayRef}_cidx->cardinality{sub}", false)
                    | _ ->
                        // Fused joint level (arc 1): output extent = product of
                        // the source array's fused dims.
                        match b.FusedRank with
                        | Some d ->
                            let prod = [0 .. d - 1] |> List.map (sprintf "%s.extents[%d]" b.ExtentArrayRef) |> String.concat " * "
                            (prod, false)
                        | None ->
                            ($"{b.ExtentArrayRef}.extents[{b.ExtentDimRef}]", false))
            // ARRAY-VALUED KERNEL RETURN (stage S3, manifestation M-C1). The
            // loop bindings describe the OUTER grid only -- one level per
            // iterated S-dimension -- but when the kernel returns an array the
            // deduced output type carries the kernel's T-dimensions after them
            // (deduceOutputType has always appended `kernelTDims`), so
            // `outputRank` exceeds the binding count. Emitting the table from
            // the bindings alone is precisely the M-C1 bug: a rank-2 grid got
            // `{ 2 }`, the inner extent read as 0, and the program printed
            // `[[], []]` with no diagnostic.
            //
            // The missing trailing extents come from the OUTPUT TYPE's own
            // trailing index types -- the same source, and the same fix, as
            // func-arrays/011's rank-2 literal of computed rows (there: the
            // declared/row-inferred array type's trailing IndexTypes). Reading
            // them off `codeGen.OutputType` rather than off `info.KernelTDims`
            // keeps the table and the `Array<T, outputRank>` allocation derived
            // from ONE type, so they cannot disagree about rank.
            // COMPACT (SymIdx/AntisymIdx) OUTPUTS COUNT BY RANK, NOT BY ENTRY.
            // The trailing T-dims used to be found with `List.skip outerCount`
            // under the guard `List.length at.IndexTypes = outputRank` -- both of
            // which silently assume every index type is rank 1. A comm-licensed
            // application over the same array gives the output a COMPOUND leading
            // index (`SymIdx<2, I>` is ONE entry of Rank 2), so the guard was
            // false and the trailing extents were dropped again: `object_for(cov)
            // <@> (B, B)` with a row-returning kernel printed `cv = []`, the exact
            // M-C1 symptom this block exists to prevent, just on the symmetric
            // path instead of the dense one.
            //
            // Both quantities are therefore computed in FLAT rank: how many
            // trailing dims are missing (`outputRank - outerCount`), and which
            // trailing ENTRIES supply them (taken from the end until their ranks
            // sum to that). On an all-rank-1 output this is identical to the old
            // `List.skip outerCount`, so the dense path is untouched.
            let extentDims =
                let outerCount = List.length extentDims
                if outputRank <= outerCount then extentDims
                else
                    match codeGen.OutputType with
                    | ArrayElem at when arrayRank at = outputRank ->
                        // Peel entries off the END until they account for the
                        // missing flat dims exactly; a partial match means the
                        // boundary falls INSIDE a compound index, which is not a
                        // shape this rewrite can describe -- leave it alone.
                        let missing = outputRank - outerCount
                        let rec peel (acc: IRIndexType list) (taken: int)
                                     (remaining: IRIndexType list) : IRIndexType list option =
                            if taken = missing then Some acc
                            elif taken > missing then None
                            else
                                match remaining with
                                | [] -> None
                                | ix :: rest -> peel (ix :: acc) (taken + ix.Rank) rest
                        match peel [] 0 (List.rev at.IndexTypes) with
                        | Some tDimEntries ->
                            let trailing =
                                tDimEntries |> List.map (fun (ix: IRIndexType) ->
                                    match tryEvalIntIR ix.Extent with
                                    | Some n -> (string n, true)
                                    | None -> (exprToCppCtx tempCtx ix.Extent, false))
                            extentDims @ trailing
                        | None -> extentDims
                    | _ -> extentDims
            let (extentDecls, ownedExtents) = emitExtentsTable ind extentsName outputRank extentDims

            // Generate allocation as Array<T,N> wrapper. extentsName is either
            // a runtime-allocated `size_t*` or (all-literal) a static constexpr
            // table; the wrapper just stores a pointer either way (array-to-
            // pointer decay), so the same brace-init pattern works.
            let allocRhs =
                match emitAllocRhs (classifyOutputStorage codeGen.OutputType)
                          outputElemType outputRank symmArg extentsName with
                | Ok rhs -> rhs
                | Error msg -> recordCodegenRefusal msg; $"{{ nullptr, {extentsName} }};\n#error \"{msg}\""
            let allocDecl = $"{ind}Array<{outputElemType}, {outputRank}> {name} = {allocRhs};"

            // MPI dense slab mode: the nest's outermost level iterates this
            // rank's [lo, hi) slab (MpiSlab flag), bounded by the prologue
            // below; the Allgatherv afterward restores the full output on all
            // ranks (SPMD invariant -- downstream code needs no changes).
            let mpiDense = (mpiShape = Some MpiDense)
            let codeGen = if mpiDense || segmentRun.IsSome then { codeGen with MpiSlab = true } else codeGen

            // Generate loop nest. The LinAlg dispatch is tried first: a
            // recognised BLAS shape is a strictly stronger rewrite than a flat
            // traversal, and the two cannot both match anyway (gemv's operand
            // is rank 2 over a depth-1 nest, which is exactly what the flat
            // path's `ArrayRank = depth` gate rules out). Next, the flat
            // elementwise detector: an index-free elementwise nest over
            // same-shape contiguous pools collapses to ONE flat loop. The
            // detector is conservative and returns None for everything else --
            // the nested form below is always correct. `info.ArrayTypes` is
            // positionally parallel to codeGen.InputArrayNames, which is what
            // lets the detector PROVE per-operand storage-class and extent
            // agreement rather than assume it (Blade's unify does not compare
            // extents).
            let loopCode =
                // a segment run loop owns the outer bounds: neither the BLAS
                // rewrite nor the flat collapse honours them
                match (if segmentRun.IsSome then None
                       else tryGenLinAlgNest streamedMap info.ArrayTypes codeGen tempCtx.VarNames tempCtx.Indent) with
                | Some la -> la
                | None ->
                match (if segmentRun.IsSome then None
                       else tryGenFlatElementwiseNest streamedMap info.ArrayTypes codeGen tempCtx.VarNames tempCtx.Indent) with
                | Some flat -> flat
                | None -> genLoopNestStreamed streamedMap codeGen tempCtx.VarNames tempCtx.Indent

            // Per-rank slab bounds. Balanced split: q = n/P with the first
            // n%P ranks taking one extra row; P > n degenerates to empty
            // slabs (lo == hi), which is correct (zero-count Allgatherv).
            let mpiSlabPrologue =
                if not mpiDense then []
                else
                    let outerBound =
                        genLoopBoundExpr (compoundArrayNamesOf codeGen.Bindings)
                                         (List.head codeGen.Bindings)
                    [ $"{ind}size_t __blade_mpi_n_{name} = {outerBound};"
                      $"{ind}size_t __blade_mpi_q_{name} = __blade_mpi_n_{name} / (size_t)__blade_mpi_size;"
                      sprintf "%ssize_t __blade_mpi_r_%s = __blade_mpi_n_%s %% (size_t)__blade_mpi_size;" ind name name
                      $"{ind}size_t __blade_mpi_lo_{name} = (size_t)__blade_mpi_rank * __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? (size_t)__blade_mpi_rank : __blade_mpi_r_{name});"
                      $"{ind}size_t __blade_mpi_hi_{name} = __blade_mpi_lo_{name} + __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? 1 : 0);" ]

            // Post-loop gather: every rank contributed a contiguous pool range
            // [lo*inner, hi*inner) (row-major DFS pool = slab of outer rows),
            // so MPI_IN_PLACE Allgatherv on the full pool reassembles the
            // array identically on all ranks. Counts/displs are runtime-
            // filled -- P is only known at mpiexec time. MPI counts are int:
            // guard totals above 2^31-1 with MPI_Abort.
            let mpiGather =
                if not mpiDense then []
                else
                    let extentsName = $"{name}_extents"
                    let dtype =
                        match codeGen.OutputType with
                        | ArrayElem at ->
                            (match at.ElemType with
                             | AnyPrimElem et -> mpiDatatypeOf et
                             | _ -> None)
                        | _ -> None
                        |> Option.defaultValue "MPI_DOUBLE"
                    let innerProd =
                        if outputRank <= 1 then "1"
                        else
                            [1 .. outputRank - 1]
                            |> List.map (fun i -> $"{extentsName}[{i}]")
                            |> String.concat " * "
                    [ $"{ind}{{ // MPI: restore full {name} on all ranks"
                      $"{ind}    {outputElemType}* __blade_mpi_pool = nested_array_utilities::pool_base({name}.data);"
                      $"{ind}    size_t __blade_mpi_inner = {innerProd};"
                      $"{ind}    if (__blade_mpi_n_{name} * __blade_mpi_inner > 2147483647ULL) {{ std::cerr << \"error[BL8004]: element count exceeds int32 range (rank \" << __blade_mpi_rank << \")\" << std::endl; MPI_Abort(MPI_COMM_WORLD, 13); }}"
                      $"{ind}    int* __blade_mpi_counts = new int[__blade_mpi_size];"
                      $"{ind}    int* __blade_mpi_displs = new int[__blade_mpi_size];"
                      $$"""{{ind}}    for (int __r = 0; __r < __blade_mpi_size; __r++) {"""
                      $"{ind}        size_t __lo = (size_t)__r * __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? (size_t)__r : __blade_mpi_r_{name});"
                      $"{ind}        size_t __hi = __lo + __blade_mpi_q_{name} + ((size_t)__r < __blade_mpi_r_{name} ? 1 : 0);"
                      $"{ind}        __blade_mpi_counts[__r] = (int)((__hi - __lo) * __blade_mpi_inner);"
                      $"{ind}        __blade_mpi_displs[__r] = (int)(__lo * __blade_mpi_inner);"
                      $$"""{{ind}}    }"""
                      $"{ind}    MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, __blade_mpi_pool, __blade_mpi_counts, __blade_mpi_displs, {dtype}, MPI_COMM_WORLD);"
                      $"{ind}    delete[] __blade_mpi_counts; delete[] __blade_mpi_displs;"
                      $"{ind}}}" ]

            // Combine all (prepend any pre-materialized temporaries). A compound
            // output inherits the input CompoundIdx: allocate a fresh compact
            // buffer and SHARE the input compound's idx (same mask) and
            // trailing_stride -- not a dense extents/allocate<> Array. (Unary map:
            // the sole input is the compound; binary same-mask is deferred.) The
            // loop nest writes every present cell, so no zero-init is needed; the
            // dense extents/alloc lines above are unused on this path. The shared
            // idx pointer is non-owning, matching the manual-free memory model.
            match codeGen.OutputType with
            | ArrayElem at when isCompoundArrayType at ->
                let inName = codeGen.InputArrayNames |> List.tryHead |> Option.defaultValue name
                let leadRank =
                    at.IndexTypes
                    |> List.tryFind (fun idx -> idx.IxKind = IxKCompound)
                    |> Option.map (_.Rank)
                    |> Option.defaultValue 1
                // A range<CompoundIdx> DRIVER has no Compound value to share an
                // idx from; the output shares the standalone materialized index
                // (`<name>_cidx`, emitted into preCode above) with trailing
                // stride 1. A compound VALUE input shares its `.idx` and stride.
                let inputIsCompoundRange =
                    match info.Arrays |> List.tryHead with
                    | Some (IRRange (its, _)) -> its |> List.exists (fun ix -> ix.IxKind = IxKCompound)
                    | _ -> false
                let idxExpr = if inputIsCompoundRange then $"{inName}_cidx" else $"{inName}.idx"
                let strideExpr = if inputIsCompoundRange then "1" else $"{inName}.trailing_stride"
                let compDecl =
                    sprintf "%snested_array_utilities::Compound<%s, %d> %s = { new %s[%s->cardinality * %s], %s, %s };"
                        ind outputElemType leadRank name outputElemType idxExpr strideExpr idxExpr strideExpr
                // Owns the data buffer ONLY -- the idx is the input compound's
                // (or the range driver's, freed by its own registration).
                // deallocate_compound here would free an index the input still
                // holds: the one compound spot where the wrong routine is
                // silent corruption.
                registerShapedAlloc name "deallocate_compound_shared_index" name
                preCode @ streamPrologue @ [""; compDecl; ""] @ loopCode
            | ArrayElem at when isSparseArrayType at ->
                // Sparse output -- twin of the compound branch above: fresh
                // compact buffer (cardinality * stride, written in key order by
                // the nest), index SHARED from the range driver's standalone
                // `_cidx` or a sparse VALUE input's `.idx`.
                let inName = codeGen.InputArrayNames |> List.tryHead |> Option.defaultValue name
                let leadRank =
                    at.IndexTypes
                    |> List.tryFind (fun idx -> idx.IxKind = IxKSparse)
                    |> Option.map (_.Rank)
                    |> Option.defaultValue 1
                let inputIsSparseRange =
                    match info.Arrays |> List.tryHead with
                    | Some (IRRange (its, _)) -> its |> List.exists (fun ix -> ix.IxKind = IxKSparse)
                    | _ -> false
                let idxExpr = if inputIsSparseRange then $"{inName}_cidx" else $"{inName}.idx"
                let strideExpr = if inputIsSparseRange then "1" else $"{inName}.trailing_stride"
                let compDecl =
                    sprintf "%snested_array_utilities::Sparse<%s, %d> %s = { new %s[%s->cardinality * %s], %s, %s };"
                        ind outputElemType leadRank name outputElemType idxExpr strideExpr idxExpr strideExpr
                // Owns the data buffer ONLY -- the idx belongs to the driver or
                // the input sparse (same silent-corruption hazard as compound).
                registerShapedAlloc name "deallocate_sparse_shared_index" name
                preCode @ streamPrologue @ [""; compDecl; ""] @ loopCode
            | _ ->
                // Deterministic deallocation, site 1: the dense HOST output. This
                // is the only registered site that can emit a non-`nullptr` SYMM
                // free, so its template arguments must mirror allocRhs above
                // exactly -- hence the same (spec, elem, rank, symmArg, extents)
                // tuple rather than a re-derivation. `ownedExtents` is Some only
                // when the table above is heap (`new size_t[R]`) -- an
                // all-literal static table owns nothing. CUDA never reaches
                // here (cudaInline returned above).
                //
                // dealloc(D): the MPI dense-slab arm is no longer excluded. The
                // Allgatherv restores the FULL pool on every rank, after which the
                // array is an ordinary scope-owned local -- the same allocation
                // line, emitted unconditionally on this arm. The gather's own
                // `__blade_mpi_counts` / `__blade_mpi_displs` are already deleted
                // inline and stay untouched.
                match codeGen.OutputType with
                | ArrayElem at when isFreeableDenseArrayType at ->
                    registerPoolAlloc (classifyOutputStorage codeGen.OutputType)
                        outputElemType outputRank symmArg extentsName name ownedExtents
                | _ -> ()
                // The run loop of a stencil over segments (see segmentRun
                // above): per run, the interior centres it holds, the window
                // the reach demands, the read, the alias, the slab bounds.
                let segLoop =
                    match segmentRun with
                    | None -> loopCode
                    | Some (offsets, cMin, cMax, rLo, rHi, start, sources, kind) ->
                        let missing =
                            sources |> List.tryFind (fun (_, s) -> ((Blade.ProviderRegistry.tryFind s.Provider).Value).GenStreamRows.IsNone)
                        match missing with
                        | Some (_, s) ->
                            recordCodegenRefusal $"provider '{s.Provider}' does not support per-segment streamed reads ('{s.VarName}' -- bind with .read)"
                            loopCode
                        | None ->
                            let g = offsets.Length - 1
                            let table = offsets |> List.map (fun b -> $"{b}UL") |> String.concat ", "
                            let maxWin =
                                List.pairwise offsets
                                |> List.map (fun (lo, hi) -> (min hi cMax) - (max lo cMin) + (rHi - rLo))
                                |> List.fold max 1L
                            // scratch names are per OUTPUT and per source: two
                            // consumers may share one source, one zip may hold two
                            let runs = $"{name}__runs"
                            let windows =
                                sources |> List.mapi (fun k (vn, spec) ->
                                    let genRows = ((Blade.ProviderRegistry.tryFind spec.Provider).Value).GenStreamRows.Value
                                    let elemCpp = elemTypeToCpp spec.VarType.ElemType
                                    let n = (match spec.VarType.IndexTypes.[0].Extent with IRLit (IRLitInt n) -> n | _ -> List.last offsets)
                                    let ext = $"{name}__srcext{k}"
                                    let win = $"{name}__win{k}"
                                    let decl =
                                        [ $"{ind}size_t {ext}[1] = {{ {n}UL }};"
                                          $"{ind}{elemCpp}* {win} = new {elemCpp}[{maxWin}];" ]
                                    let read =
                                        (genRows spec.FilePath spec.VarName vn win "__w_lo" "__w_hi" spec.VarType
                                         |> List.map (fun s -> ind + "    " + s))
                                        @ [ $"{ind}    Array<{elemCpp}, 1> {vn} = {{ {win} - __w_lo, {ext} }};" ]
                                    (decl, read, [ $"{ind}delete[] {win};" ]))
                            let what = sources |> List.map (fun (_, s) -> $"'{s.VarName}'") |> String.concat ", "
                            let lastN = sources |> List.map (fun (_, s) -> match s.VarType.IndexTypes.[0].Extent with IRLit (IRLitInt n) -> n | _ -> List.last offsets) |> List.min
                            [ (if kind = "stencil"
                               then $"{ind}// stencil over segments: {what} streamed one run at a time, each run read with its ghost cells [{rLo}, {rHi}] (HaloAccess.haloDemand); no whole-array buffer"
                               else $"{ind}// elementwise over segments: {what} streamed one run at a time (the store's chunk edge), each run its own window; no whole-array buffer")
                              $"{ind}const size_t {runs}[{g + 1}] = {{ {table} }};" ]
                            @ (windows |> List.collect (fun (d, _, _) -> d))
                            @ [ $$"""{{ind}}for (size_t __seg = 0; __seg < {{g}}; __seg++) {"""
                                $"{ind}    size_t __c_lo = {runs}[__seg] < {cMin}UL ? {cMin}UL : {runs}[__seg];"
                                $"{ind}    size_t __c_hi = {runs}[__seg + 1] > {cMax}UL ? {cMax}UL : {runs}[__seg + 1];"
                                $"{ind}    if (__c_lo >= __c_hi) continue;"
                                $"{ind}    size_t __w_lo = __c_lo + ({rLo}L); size_t __w_hi = __c_hi + ({rHi}L);"
                                $"{ind}    if (__w_hi > {lastN}UL) __w_hi = {lastN}UL;" ]
                            @ (windows |> List.collect (fun (_, r, _) -> r))
                            @ [ $"{ind}    size_t __blade_mpi_lo_{name} = __c_lo - {start}UL;"
                                $"{ind}    size_t __blade_mpi_hi_{name} = __c_hi - {start}UL;" ]
                            @ (loopCode |> List.map (fun s -> "    " + s))
                            @ [ $"{ind}}}" ]
                            @ (windows |> List.collect (fun (_, _, f) -> f))
                preCode @ streamPrologue @ [""] @ extentDecls @ [""; allocDecl; ""]
                @ mpiSlabPrologue @ segLoop @ mpiGather

