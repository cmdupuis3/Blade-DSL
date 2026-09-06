// Fusion-tree assembly: leaf backend classification, merge/join
// compatibility, fused loop nests, compose/parallel application, and the
// deferred-read/capture collectors that feed them.
module Blade.CodeGenFusion

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

/// Execution backend a fusion leaf requests. Backends are whole-leaf: `omp`
/// threads a leaf's outer loop level, while `cuda`/`mpi` are whole-nest
/// device/domain transforms (a leaf is a device kernel launch or a
/// rank-decomposed domain, not a per-level pragma). This granularity is why
/// "cov's inner dim is cuda" really means "cov is a cuda leaf": cuda cannot
/// share a host loop header with a sibling. Serial (no clause) is the default.
type LeafBackend =
    | BkSerial
    | BkOmp
    | BkCuda of blockSize: int
    | BkMpi

/// Classify a leaf's requested backend from its resolved kernel's opt-in
/// flags (parser guarantees at most one of omp/cuda/mpi per where-clause).
/// cuda/mpi are gated by their emit modes exactly like the single-kernel
/// path: outside `blade test --cuda` / `--mpi` (or the corresponding run
/// flag) the clause is INERT and the leaf classifies as serial host -- so a
/// plain host build never spuriously rejects a `where cuda` leaf in a fusion
/// tree; the device co-fusion (and its mixed-backend conflict) engages only
/// when the backend is actually active.
let classifyLeafBackend (info: ApplyInfo) : LeafBackend =
    match resolveKernel info.Kernel with
    | Some rk when rk.Callable.IsCudaKernel && cudaEmitModeEnabled () -> BkCuda rk.Callable.CudaBlockSize
    | Some rk when rk.Callable.IsMpiParallel && mpiEmitModeEnabled () -> BkMpi
    | Some rk when rk.Callable.IsOmpParallel -> BkOmp
    | _ -> BkSerial

/// Whether two backends are the SAME host class for hard-fusion (<&!>)
/// agreement. Only serial and omp are host backends; cuda/mpi are not host.
let internal isHostBackend = function BkSerial | BkOmp -> true | _ -> false

/// Nest-level pragma for a MERGED host nest, honoring the joined host-parallel
/// decision and the staggered shape. A staggered tower must parallelize the
/// OUTER loop only (each outer index owns disjoint output slabs across every
/// leaf -- safe), never `collapse` (collapsing an inner rectangular prefix
/// would re-run a shallow leaf's assignment once per inner iteration -- a race
/// on its cell). A non-staggered merge (all leaves write at the deepest level)
/// may collapse exactly like a single nest, so it defers to genNestPragma.
let genFusedNestPragma (bindings: LoopIndexBinding list) (staggered: bool) (pragmaIndent: string) : string =
    match bindings with
    | [] -> ""
    | outer :: rest ->
        // BUILD KNOB, same rule as genNestPragma (which the non-staggered arm
        // defers to and which is gated on its own): both spellings below open a
        // team, so a serial-emission build returns "" and the caller's marker
        // names the knob. See `ompThreadEmissionEnabled`.
        if not (ompThreadEmissionEnabled ()) then ""
        elif not staggered then genNestPragma bindings pragmaIndent
        else
            let isRectangular (b: LoopIndexBinding) =
                b.BoundDependencies.IsEmpty && b.StrictOffset = 0
            let hasTriangularBelow = rest |> List.exists (fun b -> not (isRectangular b))
            if hasTriangularBelow then
                $"#pragma omp parallel for schedule(dynamic)\n{pragmaIndent}"
            else
                $"#pragma omp parallel for\n{pragmaIndent}"

/// Check that a set of fusion leaves can legally merge into ONE loop nest.
/// Leaves may have DIFFERENT depths (arities): a shallower leaf's loop levels
/// must prefix-match the deepest leaf's levels (same extent, same triangular
/// bound dependencies, same strict offset, same fused joint rank), because
/// the merged nest reuses the deepest leaf's loop headers and a shallower
/// leaf's assignment executes at its own depth. Arrays do NOT have to match
/// across leaves: each leaf peels its own arrays (identical peels are
/// deduplicated at emission).
/// Returns the primary (deepest) leaf, or a human-readable incompatibility.
let checkMergeCompatible (leafCgs: LoopNestCodeGen list) : Result<LoopNestCodeGen, string> =
    if leafCgs.IsEmpty then Error "no fusion leaves" else
    if leafCgs |> List.exists (_.Bindings.IsEmpty) then
        Error "a leaf has no loop levels (scalar application)"
    else
    let primary = leafCgs |> List.maxBy (_.Bindings.Length)
    let boundEq (a: LoopIndexBinding) (b: LoopIndexBinding) =
        // Same runtime extent: literal-equal, or resolved against the same
        // array dimension (covers a literal vs. runtime rendering of the
        // same axis -- ExtentArrayRef/DimRef name the SAME dimension, so the
        // bound value is identical either way).
        let extentEq =
            match a.Extent, b.Extent with
            | IRLit la, IRLit lb -> la = lb
            | _ -> a.ExtentArrayRef = b.ExtentArrayRef && a.ExtentDimRef = b.ExtentDimRef
        extentEq
        && a.BoundDependencies = b.BoundDependencies
        && a.StrictOffset = b.StrictOffset
        && a.FusedRank = b.FusedRank
    let incompat =
        leafCgs |> List.tryPick (fun cg ->
            cg.Bindings
            |> List.mapi (fun j b -> (j, b))
            |> List.tryPick (fun (j, b) ->
                if boundEq primary.Bindings.[j] b then None
                else Some ($"loop level {j} of '{cg.OutputName}' does not match '{primary.OutputName}' (extent or triangular structure differs)")))
    match incompat with
    | Some reason -> Error reason
    | None -> Ok primary

/// checkMergeCompatible's rule for a REDUCTION JOIN. Same structural
/// obligations (level count, bound dependencies, triangular offset, fused
/// rank) with ONE relaxation: two legs may take their extent from DIFFERENT
/// arrays. A join's legs are independent reductions that the SOURCE declared
/// share an index space -- `prodsum(s, ct)` walks `s` while `prodsum(ct, ct)`
/// walks `ct`'s source -- so requiring one array to name every level's extent
/// would refuse the shape the feature exists for.
///
/// What is checked instead is exactly what `prodsum` has always checked over
/// its own operands: a provable LITERAL disagreement is an error, an unknown
/// extent is trusted, and the nest takes its bound from the leading leg. The
/// typechecker performs the same comparison one level up (over index types)
/// and refuses there first; this is the codegen-side backstop.
let checkJoinCompatible (leafCgs: LoopNestCodeGen list) : Result<LoopNestCodeGen, string> =
    if leafCgs.IsEmpty then Error "no join legs" else
    if leafCgs |> List.exists (_.Bindings.IsEmpty) then
        Error "a join leg has no loop levels (scalar reduction)"
    else
    let primary = leafCgs |> List.maxBy (_.Bindings.Length)
    let boundOk (a: LoopIndexBinding) (b: LoopIndexBinding) =
        (match a.Extent, b.Extent with
         | IRLit la, IRLit lb -> la = lb
         | _ -> true)
        && a.BoundDependencies = b.BoundDependencies
        && a.StrictOffset = b.StrictOffset
        && a.FusedRank = b.FusedRank
    let incompat =
        leafCgs |> List.tryPick (fun cg ->
            if cg.Bindings.Length <> primary.Bindings.Length then
                Some (sprintf "leg '%s' traverses %d loop level(s) and leg '%s' traverses %d -- every leg of a join folds the same cell grid"
                          cg.OutputName cg.Bindings.Length primary.OutputName primary.Bindings.Length)
            else
                cg.Bindings
                |> List.mapi (fun j b -> (j, b))
                |> List.tryPick (fun (j, b) ->
                    if boundOk primary.Bindings.[j] b then None
                    else Some ($"loop level {j} of '{cg.OutputName}' does not match '{primary.OutputName}' (extent or triangular structure differs)")))
    match incompat with
    | Some reason -> Error reason
    | None -> Ok primary

/// Generate ONE merged loop nest for a set of fusion leaves (<&!>, fusable
/// <&>, and reduce over fused trees). Nest structure comes from the DEEPEST
/// leaf (validated by checkMergeCompatible); each leaf's assignment emits at
/// its OWN depth, so mixed-arity towers stagger:
///     for i0 { m1[i0] = ..; for i1 { m2[i0][i1] = ..; for i2 { .. } } }
/// with every leaf streaming the shared outer elements from one load. Each
/// leaf's kernel params resolve through its OWN element bindings (never
/// bridged to another leaf's arrays); peels that render identically (shared
/// arrays at shared levels) are emitted once.
///
/// `hostParallel` is the caller's JOINED host-backend decision (<&> = any leaf
/// omp, <&!> = all leaves omp): when true the outer level gets an omp pragma
/// via the staggered-aware genFusedNestPragma; a fused FOLD ignores it (racy
/// scalar accumulators; omp reduction is future work).
///
/// `mpiSlabVar = Some sv` makes the OUTER shared level iterate this rank's
/// slab instead of the full extent (MPI co-fusion: each leaf's output becomes
/// a contiguous outer-row slab, restored by a per-leaf Allgatherv). Under
/// mpi+omp hybrid co-fusion, hostParallel additionally puts a bare
/// `#pragma omp parallel for` on the cell loop inside each rank's slab.
let genFusedLoopNestStreamed (streamed: Map<string, ProviderReadSpec>) (leafCgs: LoopNestCodeGen list) (outerNames: Map<int, string>) (indent: int) (hostParallel: bool) (mpiSlabVar: string option) : string list =
    let ind n = String.replicate n "    "
    let primary = leafCgs |> List.maxBy (_.Bindings.Length)
    let staggered = leafCgs |> List.exists (fun cg -> cg.Bindings.Length <> primary.Bindings.Length)
    let emitOuterOmp = hostParallel && primary.FoldWrapper.IsNone
    // Streamed fiber reads share per-source handles and per-argument
    // buffers -- not thread-safe under a host-parallel outer loop.
    //
    // DELIBERATELY NOT GATED on `ompThreadEmissionEnabled`: the refusal is
    // about the SOURCE (an `omp` clause over streamed reads is an
    // incompatibility the user wrote), so it must not depend on a build knob.
    // The invariant `BLADE_OMP_THREADS` preserves is that it changes WHAT IS
    // EMITTED and never WHICH PROGRAMS COMPILE -- a program that is refused on
    // one deployment build is refused on all of them.
    if emitOuterOmp && not (Map.isEmpty streamed) then
        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan (sprintf "streamed provider reads are not thread-safe under omp -- bind with .read (streamed sources: %s)"
            (streamed |> Map.toList |> List.map fst |> String.concat ", "))))
    let mutable lines = []
    let mutable depth = indent

    // Per-leaf peeled-name state: (leafIdx, arrayPosition) -> current name.
    let mutable currentNames : Map<int * int, string> = Map.empty
    // Per-leaf kernel-param resolution: leafIdx -> (paramVarId -> final name).
    let mutable paramFinalNames : Map<int, Map<int, string>> =
        leafCgs |> List.mapi (fun li _ -> (li, Map.empty)) |> Map.ofList
    // Streamed sources: accumulated ABSOLUTE site coordinates per
    // (leaf, array position).
    let mutable streamSites : Map<int * int, string list> = Map.empty

    let compoundArrays = compoundArrayNamesOf primary.Bindings
    let mutable atOuterLevel = true

    // Restrict row peels, per leaf. Extra cross-leaf obligation on top
    // of restrictPeelSites' own: leaves SHARE `declaredNames`, and a RealArray
    // peel's emitted name (`A____i0`) is derived from the array + index name
    // only -- not from the leaf -- so two leaves peeling the same array at the
    // same level emit ONE declaration. If one leaf wanted the raw pointer and
    // the other the wrapper, the codes would differ and both would be emitted
    // (a g++ redeclaration). So a (level, ArrayName, RankComponent) key is
    // blocked unless EVERY leaf touching it agrees on the raw peel.
    let leafRestrictSites = leafCgs |> List.map (fun cg -> restrictPeelSites cg.Bindings) |> List.toArray
    let blockedRawKeys =
        seq {
            for lk in 0 .. leafCgs.Length - 1 do
                let cgk = leafCgs.[lk]
                for jj in 0 .. cgk.Bindings.Length - 1 do
                    for e2 in cgk.Bindings.[jj].Elements do
                        if not (Set.contains (jj, e2.ArrayPosition) leafRestrictSites.[lk]) then
                            yield (jj, e2.ArrayName, e2.RankComponent)
        } |> Set.ofSeq

    for j in 0 .. primary.Bindings.Length - 1 do
        let pBinding = primary.Bindings.[j]
        // Nest-level OpenMP pragma at the outermost level only, gated by the
        // JOINED host-parallel decision (not the primary leaf's own flag -- a
        // <&> sibling's omp configures the shared header). The outer binding's
        // IsParallel is forced on so genNestPragma's collapse/schedule logic
        // engages for the non-staggered case; staggered uses outer-only.
        //
        // The TAIL bindings keep the primary leaf's own `omp(a: n)` licences, so
        // the collapse depth is capped by them even though the head is forced.
        // That is deliberately conservative: a fused nest never threads more
        // dimensions than its primary leaf licensed, and a leaf that requested
        // no omp at all contributes no licence, so the shared header stays a
        // plain outer `parallel for`. Joining licences ACROSS leaves (taking the
        // most permissive at each shared level) is a possible refinement; it is
        // not attempted here because the shared header's levels do not
        // correspond one-to-one to a non-primary leaf's arguments.
        let pragmaPrefix =
            if atOuterLevel && emitOuterOmp then
                let forced = { pBinding with IsParallel = true } :: List.tail primary.Bindings
                genFusedNestPragma forced staggered (ind depth)
            else ""
        // Any leaf's own omp request is enough to make suppression here worth
        // reporting: a `<&>` soft join adopts a single shared header, so one
        // leaf's clause can be dropped by the JOINED decision even though that
        // leaf, standing alone, would have been parallelized.
        let suppressedMarker =
            if atOuterLevel then
                let reason =
                    // BUILD KNOB first -- see the same rule in genLoopNestStreamed.
                    if not (ompThreadEmissionEnabled ()) then ompThreadsSuppressedReason ()
                    elif primary.FoldWrapper.IsSome then
                        "fold accumulates into a shared scalar, which is not race-safe"
                    else "the fused nest's joined backend decision is not host-parallel"
                ompSuppressedMarker
                    (leafCgs |> List.exists (_.OmpRequested))
                    (pragmaPrefix <> "") reason (ind depth)
            else []
        atOuterLevel <- false
        // MPI co-fusion: the outer shared level iterates this rank's row slab.
        let header =
            match mpiSlabVar with
            | Some sv when j = 0 ->
                $"for (size_t {pBinding.IndexName} = __blade_mpi_lo_{sv}; {pBinding.IndexName} < __blade_mpi_hi_{sv}; {pBinding.IndexName}++) {{"
            | _ -> genForLoopHeader compoundArrays pBinding
        lines <- lines @ suppressedMarker @ [ind depth + pragmaPrefix + header]
        depth <- depth + 1

        // Element peels for every leaf that iterates this level, from the
        // leaf's OWN arrays. declaredNames maps emitted C++ name -> code
        // line: an identical (name, code) pair is a shared peel (emit once);
        // the same name with DIFFERENT code is a cross-leaf collision of
        // virtual param names, disambiguated by a per-leaf suffix.
        let mutable declaredNames : Map<string, string> = Map.empty
        for li in 0 .. leafCgs.Length - 1 do
            let cg = leafCgs.[li]
            if j < cg.Bindings.Length then
                let binding = cg.Bindings.[j]
                for elem in binding.Elements do
                    match Map.tryFind elem.ArrayName streamed with
                    | Some sspec ->
                        // Streamed source in a fused nest: same interception
                        // as the single-leaf nest, with cross-leaf FIBER
                        // DEDUP falling out of the name->code map -- two
                        // leaves reading the same source fiber at the same
                        // level produce byte-identical read+wrapper blocks,
                        // emitted once and shared via paramFinalNames.
                        let acc = Map.tryFind (li, elem.ArrayPosition) streamSites |> Option.defaultValue []
                        let (codeLines, fiberBound, acc') = genElementBindingStreamed binding elem sspec acc
                        streamSites <- Map.add (li, elem.ArrayPosition) acc' streamSites
                        let joined = String.concat "\n" codeLines
                        (match fiberBound with
                         | Some fname ->
                             if Map.tryFind fname declaredNames <> Some joined then
                                 for c in codeLines do lines <- lines @ [ind depth + c]
                             declaredNames <- Map.add fname joined declaredNames
                             currentNames <- Map.add (li, elem.ArrayPosition) fname currentNames
                             let pfn = Map.find li paramFinalNames
                             paramFinalNames <- Map.add li (Map.add elem.ParamVarId fname pfn) paramFinalNames
                         | None ->
                             // Intermediate level (fused site decode decls):
                             // dedup identical blocks across leaves by content.
                             if joined <> "" && Map.tryFind joined declaredNames <> Some joined then
                                 for c in codeLines do lines <- lines @ [ind depth + c]
                                 declaredNames <- Map.add joined joined declaredNames)
                    | None ->
                        let cur =
                            Map.tryFind (li, elem.ArrayPosition) currentNames
                            |> Option.defaultValue elem.ArrayName
                        let rawRow =
                            Set.contains (j, elem.ArrayPosition) leafRestrictSites.[li]
                            && not (Set.contains (j, elem.ArrayName, elem.RankComponent) blockedRawKeys)
                        let (code0, name0) = genElementBindingPeel rawRow binding elem cur
                        let (code, newName) =
                            match Map.tryFind name0 declaredNames with
                            | Some prior when prior <> code0 ->
                                let renamed = { elem with ParamName = $"{elem.ParamName}__l{li}" }
                                genElementBindingPeel rawRow binding renamed cur
                            | _ -> (code0, name0)
                        if Map.tryFind newName declaredNames <> Some code then
                            lines <- lines @ [ind depth + code]
                        declaredNames <- Map.add newName code declaredNames
                        currentNames <- Map.add (li, elem.ArrayPosition) newName currentNames
                        let pfn = Map.find li paramFinalNames
                        paramFinalNames <- Map.add li (Map.add elem.ParamVarId newName pfn) paramFinalNames

        // Assignments for the leaves whose nest ENDS at this level (all of
        // their peels are in scope here; deeper levels never see them).
        for li in 0 .. leafCgs.Length - 1 do
            let cg = leafCgs.[li]
            if cg.Bindings.Length = j + 1 then
                let pfn = Map.find li paramFinalNames
                let nameMap = pfn |> Map.fold (fun acc k v -> Map.add k v acc) outerNames
                // Fallback-only, as in genLoopNestStreamed: a captured
                // block-local emitted as `__v<id>` must not be re-spelled to
                // its source name here.
                let nameMap =
                    cg.Captures
                    |> List.fold (fun acc c -> if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc) nameMap
                let rr = genKernelExprWithReynolds cg.KernelExpr cg.KernelParams cg.HasReynolds cg.IsAntisymmetric nameMap pfn
                if cg.HasReynolds && rr.UniqueTerms < rr.TotalPerms then
                    lines <- lines @ [ind depth + $"// Reynolds: {rr.UniqueTerms}/{rr.TotalPerms} perms unique (dedup {rr.TotalPerms / max 1 rr.UniqueTerms}x)"]
                // Cell write for compute; accumulate-through-wrapper for the
                // fused fold (scalar accumulators, no cell indexing); a
                // per-iteration CONST for a reduction join's shared leaf,
                // which every later leaf at this level reads by name.
                let assign =
                    match cg.ShareDecl with
                    | Some elemCpp -> $"const {elemCpp} {cg.OutputName} = {rr.CppExpr};"
                    | None ->
                    match cg.FoldWrapper with
                    | Some wname -> $"{cg.OutputName} = {wname}({cg.OutputName}, {rr.CppExpr});"
                    | None ->
                        let outputIdx =
                            cg.Bindings |> List.map (fun b -> $"[{b.IndexName}]") |> String.concat ""
                        $"{cg.OutputName}{outputIdx} = {rr.CppExpr};"
                lines <- lines @ [ind depth + assign]

    // Close all loops
    for _ in primary.Bindings do
        depth <- depth - 1
        lines <- lines @ [ind depth + "}"]

    lines

/// The ordinary (no streamed sources) fused-nest emitter.
let genFusedLoopNest (leafCgs: LoopNestCodeGen list) (outerNames: Map<int, string>) (indent: int) (hostParallel: bool) (mpiSlabVar: string option) : string list =
    genFusedLoopNestStreamed Map.empty leafCgs outerNames indent hostParallel mpiSlabVar


/// Generate C++ code for inline object_for application (e.g., A [+] B)
let genObjectForApplication (ctx: CodeGenContext) (name: string) (objInfo: ObjectForInfo) (arrays: IRExpr list) (builder: IRBuilder) : string list =
    let ind = indentStr ctx
    
    // Get array names
    let arrayNames = arrays |> List.mapi (fun i arr ->
        match arr with
        | IRVar (id, _) -> Map.tryFind id ctx.VarNames |> Option.defaultValue ($"arr{i}")
        | _ -> $"arr{i}")
    
    // Resolve the kernel via `resolveCallable`. The wrapper closure
    // takes the kernel's regular params (elementwise) and forwards to
    // the lifted function with captures pulled by reference. Loop
    // bodies invoke the wrapper with the per-iteration array slots --
    // eliminating the need for intermediate scalar locals.
    match resolveCallable objInfo.Kernel with
    | Some callable when callable.Params.Length = 1 || callable.Params.Length = 2 ->
        let (wrapperCode, wname) = genCallableWrapper ctx.VarNames name callable
        // Output element type is the kernel's RETURN type: comparison/logical
        // kernels (`A < B`) consume numeric inputs but PRODUCE bool, and
        // array<->scalar broadcast kernels can PROMOTE (`I * 2.5` is
        // int64 -> double -- storing into an int64 result array would be a
        // float-conversion error under -Werror). Only when the return type
        // isn't a value primitive (unit, unresolved) fall back to the
        // historical param-based inference.
        let elemTypeStr =
            match callable.RetType with
            | IRTScalar et when et <> ETUnit -> primTypeToCpp et
            | _ ->
                callable.Params |> List.tryPick (fun p ->
                    match p.Type with
                    | IRTScalar et -> Some (primTypeToCpp et)
                    | ArrayElem arr -> Some (elemTypeToCpp arr.ElemType)
                    | _ -> None)
                |> Option.defaultValue (match callable.Params with p :: _ -> irTypeToCpp p.Type | [] -> "void")
        // Indent wrapper-emission lines to match surrounding scope.
        let wrapperLines = wrapperCode |> List.map (fun s -> ind + s)
        // Return-extent ABI: this wrapper crosses a call boundary, so its
        // extents table must outlive the frame -- the shared heap-vs-static
        // rule (emitExtentsTable) exists for exactly this constraint; see its
        // doc for the full rationale. Lifetime of the heap form is
        // scope-bound: under a live alloc frame (function body or for-range
        // body) each arm below registers BOTH the pool and the heap extents
        // table, so the pair is released together at scope exit. At main's
        // top level there is no frame and both keep program lifetime. The
        // static form owns nothing on either path.
        let retExtents (rank: int) (dims: (string * bool) list) : string list * string option =
            emitExtentsTable ind ($"{name}_extents") rank dims
        // One derivation of each operand extent, feeding BOTH the copy-loop
        // bound and the return-extents table -- the loop and the shape it
        // describes cannot disagree because they are the same expression.
        let operandTypes = arrays |> List.map inferExprType
        let extentAt (pos: int) (dim: int) : string * bool =
            let nm = List.item pos arrayNames
            match List.tryItem pos operandTypes with
            | Some (ArrayElem at) ->
                (literalOrRuntimeExtentOfArray at nm dim, (literalExtentOfArray at dim).IsSome)
            | _ -> ($"{nm}.extents[{dim}]", false)
        match objInfo.InputRanks, arrayNames with
        | [1; 1], [arrA; arrB] ->
            // Outer product: result[i][j] = kernel(A[i], B[j])
            let a0 = extentAt 0 0
            let b0 = extentAt 1 0
            let (aExt, bExt) = (fst a0, fst b0)
            let (extentsDecl, ownedExtents) = retExtents 2 [a0; b0]
            let allocDecl = $$"""{{ind}}Array<{{elemTypeStr}}, 2> {{name}} = { allocate<promote<{{elemTypeStr}}, 2>::type>({{name}}_extents), {{name}}_extents };"""
            let loopCode = [
                $$"""{{ind}}for (size_t __i0 = 0; __i0 < {{aExt}}; __i0++) {"""
                $$"""{{ind}}    for (size_t __i1 = 0; __i1 < {{bExt}}; __i1++) {"""
                $"{ind}        {name}[__i0][__i1] = {wname}({arrA}[__i0], {arrB}[__i1]);"
                $$"""{{ind}}    }"""
                $"{ind}}}"
            ]
            // Statement-position materializer (sole caller is genBinding's
            // IRApp(IRObjectFor) arm, directly or via genFuncBody's
            // hoistLoopApps lift), so the pool AND its extents table are
            // scope-owned -- unless the table is static, which owns nothing.
            // Dense/nullptr mirrors the allocate<> above.
            registerPoolAlloc AllocDense elemTypeStr 2 "nullptr"
                (name + "_extents") name ownedExtents
            extentsDecl @ [allocDecl; ""] @ wrapperLines @ loopCode

        | [0; 0], [arrA; arrB] ->
            // Elementwise: result[i] = kernel(A[i], B[i])
            let a0 = extentAt 0 0
            let (extentsDecl, ownedExtents) = retExtents 1 [a0]
            let allocDecl = $$"""{{ind}}Array<{{elemTypeStr}}, 1> {{name}} = { allocate<promote<{{elemTypeStr}}, 1>::type>({{name}}_extents), {{name}}_extents };"""
            let loopCode = [
                $$"""{{ind}}for (size_t __i0 = 0; __i0 < {{(fst a0)}}; __i0++) {"""
                $"{ind}    {name}[__i0] = {wname}({arrA}[__i0], {arrB}[__i0]);"
                $"{ind}}}"
            ]
            registerPoolAlloc AllocDense elemTypeStr 1 "nullptr"
                (name + "_extents") name ownedExtents
            extentsDecl @ [allocDecl; ""] @ wrapperLines @ loopCode

        | [0], [arrA] ->
            // Single-array elementwise map (array<->scalar broadcast):
            // result[i] = kernel(A[i]). The scalar is baked into the 1-param
            // kernel, so only the array is iterated.
            let a0 = extentAt 0 0
            let (extentsDecl, ownedExtents) = retExtents 1 [a0]
            let allocDecl = $$"""{{ind}}Array<{{elemTypeStr}}, 1> {{name}} = { allocate<promote<{{elemTypeStr}}, 1>::type>({{name}}_extents), {{name}}_extents };"""
            let loopCode = [
                $$"""{{ind}}for (size_t __i0 = 0; __i0 < {{(fst a0)}}; __i0++) {"""
                $"{ind}    {name}[__i0] = {wname}({arrA}[__i0]);"
                $"{ind}}}"
            ]
            registerPoolAlloc AllocDense elemTypeStr 1 "nullptr"
                (name + "_extents") name ownedExtents
            extentsDecl @ [allocDecl; ""] @ wrapperLines @ loopCode

        | _ ->
            // Unsupported configuration
            codegenError ctx ind ($"unsupported object_for configuration for '{name}'")
    | _ ->
        codegenError ctx ind ($"object_for kernel for '{name}' does not resolve to a callable")

// Binding Generation

/// Unroll an IRLet chain into a list of (varId, valueExpr) statements and a final return expression.
/// e.g., IRLet(id1, v1, IRLet(id2, v2, body)) -> statements=[(id1,v1), (id2,v2)], return=body
let rec unrollLetChain (expr: IRExpr) : (IRId * IRExpr) list * IRExpr =
    match expr with
    | IRLet (id, value, body) ->
        let (rest, final) = unrollLetChain body
        ((id, value) :: rest, final)
    | _ -> ([], expr)

// Recursive Parallel/Fusion Tree Helpers

/// Materialize an `IRComposeApply` (slot-inverted compose-apply form) into the
/// named target. Used by: the IRCompute dispatcher (`let r = (o1 >>@ o2) <@> A
/// |> compute`), parallel/fusion leaf emission (a compose-apply as a `<&>`/
/// `<&!>` leaf), and method-composition stage-1 materialization (left of
/// `@>>`). Resolves through `ctx.DeferredComputations` to the underlying
/// `IRComposeObj`, then emits chained-loop codegen: two stages (one per object
/// in the chain), each an element-wise loop with the resolved kernel --
/// direct call loops if both kernels are named C++ functions, else per-stage
/// `IRApplyCombinator` materialization via `genApplyCombinator`.
///
/// Does NOT register `name` in the returned context -- that's the caller's
/// responsibility (typically a `let`-binding handler calling `addVarName`).
let genComposeApply
    (ctx: CodeGenContext)
    (name: string)
    (info: ComposeApplyInfo)
    (outputType: IRType)
    (builder: IRBuilder)
    : string list * CodeGenContext =
    let ind = indentStr ctx
    let rec resolveDeferred e =
        match e with
        | IRVar (id, _) ->
            match Map.tryFind id ctx.DeferredComputations with
            | Some d -> resolveDeferred d
            | None ->
                // A let-bound object leaf (`let o = object_for(f)`) lives in
                // ObjectLoopBindings, not DeferredComputations; chase it here so
                // `kernel1 = o.Kernel` succeeds instead of falling through to the
                // object var (emitted as an undeclared C++ symbol).
                match Map.tryFind id ctx.ObjectLoopBindings with
                | Some d -> resolveDeferred d
                | None -> e
        | _ -> e
    let resolvedComposition = resolveDeferred info.Composition
    match resolvedComposition with
    | IRComposeObj (obj1, obj2) ->
        let rObj1 = resolveDeferred obj1
        let rObj2 = resolveDeferred obj2
        let kernel1 = match rObj1 with IRObjectFor o -> o.Kernel | _ -> rObj1
        let kernel2 = match rObj2 with IRObjectFor o -> o.Kernel | _ -> rObj2
        let arrays = info.InputArrays

        let kernelName1 = match kernel1 with IRVar (id, _) -> Map.tryFind id ctx.VarNames | _ -> None
        let kernelName2 = match kernel2 with IRVar (id, _) -> Map.tryFind id ctx.VarNames | _ -> None

        let arrName =
            match arrays with
            | [IRVar (id, _)] -> Map.tryFind id ctx.VarNames |> Option.defaultValue "arr0"
            | _ -> "arr0"
        let arrRank =
            match arrays with
            | [a] -> (match inferExprType a with ArrayElem arr -> arrayRank arr | _ -> 1)
            | _ -> 1
        let (elemType, elemTypeErrCode) =
            match arrays with
            | a :: _ ->
                match inferExprType a with
                | ArrayElem arr -> (elemTypeToCpp arr.ElemType, [])
                | IRTScalar et -> (primTypeToCpp et, [])
                | t ->
                    (elemTypeToCpp (IRTScalar ETFloat64),
                     codegenError ctx ind (sprintf ">>@: could not determine input element type (got %A) - likely a typechecker or IR bug" t))
            | [] ->
                (elemTypeToCpp (IRTScalar ETFloat64),
                 codegenError ctx ind ">>@: empty array list - likely an IR-builder bug")

        // Both stages sweep the SAME shape (stage 2 borrows stage 1's extents,
        // which borrow the input's), so the input's records settle the trip
        // count for both. Only the LITERAL is shared: each stage still names
        // its own array in the runtime fallback, so the emitted read keeps
        // pointing at the operand that stage actually iterates.
        let stageLiteral =
            match arrays with
            | a :: _ ->
                (match inferExprType a with
                 | ArrayElem at -> literalExtentOfArray at 0
                 | _ -> None)
            | [] -> None
        let stageBoundOf (srcName: string) =
            match stageLiteral with
            | Some n -> string n
            | None -> srcName + ".extents[0]"
        // A stage is EMITTABLE when it is either a NAMED C++ lambda (the
        // direct-call arm below) or a callable `genApplyCombinator` can resolve
        // (the per-stage fallback arm). Anything else has no kernel to call.
        let stageEmittable (kn: string option) (k: IRExpr) : bool =
            kn.IsSome || (resolveCallable k).IsSome
        match kernelName1, kernelName2 with
        | Some k1, Some k2 ->
            // Both kernels are named C++ lambdas - direct call loops.
            //
            // THE NAME THAT IS CALLED. A stage whose kernel is a NAMED
            // FUNCTION arrives as its lifted eta-wrapper (`__lambda_16(k) =
            // add_global(k)`), and calling the wrapper by name is not always
            // possible: a wrapper over a function that captures a main-local
            // binding (`function add_global(x) = x + scale` over a
            // module-level `let scale`) is itself a main-local
            // `std::function`, emitted in IRId order AFTER the function whose
            // body names it -- "'__lambda_16' was not declared in this
            // scope" the moment fusion declines (hygiene) and the staged path
            // is the one that runs. The single-stage nest never has this
            // problem because it inlines the wrapper's body; the same is
            // done here for the one shape a wrapper has, a call of the named
            // function on the parameter, by calling that function directly.
            let directName (kn: string) (k: IRExpr) : string =
                match resolveCallable k with
                | Some c ->
                    (match c.Params, c.Body with
                     | [ p ], IRApp (IRVar (fid, _), [ IRVar (pid, _) ], _) when pid = p.VarId ->
                         (match Map.tryFind fid ctx.VarNames with
                          | Some fname -> fname
                          | None -> kn)
                     | _ -> kn)
                | None -> kn
            //
            // EACH STAGE'S STORE CARRIES THAT STAGE'S RESULT TYPE, and the
            // sweep covers EVERY axis. Both used to be borrowed from the
            // input: `half(Int64) -> Float64 >>@ double_it` allocated the
            // intermediate as Int64 (g++: float-conversion), and a rank-2
            // input got ONE loop that handed row pointers to scalar kernels
            // (g++: cannot convert double*). The result type is the resolved
            // callable's return type (its body's, when declared `-> infer`);
            // a stage that does not resolve, or returns an array (a row map,
            // which this two-loop shape does not express), keeps the input's
            // element type as before.
            let stageElemOf (k: IRExpr) (fallback: string) : string =
                match resolveCallable k with
                | Some c ->
                    let ret =
                        match c.RetType with
                        | IRTInfer _ -> inferExprType c.Body
                        | t -> t
                    (match ret with
                     | ArrayElem _ | IRTUnit -> fallback
                     | t -> elemTypeToCpp t)
                | None -> fallback
            let s1Elem = stageElemOf kernel1 elemType
            let s2Elem = stageElemOf kernel2 s1Elem
            /// `for` over every axis of `src`, applying `kn` cell by cell into
            /// `dst`. Axis 0 takes the shared literal-or-runtime bound; the
            /// inner axes read the source's own runtime extents.
            let stageSweep (src: string) (dst: string) (kn: string) : string list =
                let pad (d: int) = String.replicate d "    "
                let subs = [ for d in 0 .. arrRank - 1 -> $"[__i{d}]" ] |> String.concat ""
                let opens =
                    [ for d in 0 .. arrRank - 1 ->
                        let p = pad d
                        let bound = if d = 0 then stageBoundOf src else $"{src}.extents[{d}]"
                        $"{ind}{p}for (size_t __i{d} = 0; __i{d} < {bound}; __i{d}++) {{" ]
                let bodyPad = pad arrRank
                let closes =
                    [ for d in arrRank - 1 .. -1 .. 0 ->
                        let p = pad d
                        $"{ind}{p}}}" ]
                opens @ [ $"{ind}{bodyPad}{dst}{subs} = {kn}({src}{subs});" ] @ closes
            let s1Name = $"{name}__s1"
            let s1Code =
                [ $"{ind}const size_t* {s1Name}_extents = {arrName}.extents;"
                  arrayAlloc { Ind = ind; Elem = s1Elem; Rank = arrRank; Name = s1Name
                               Symm = "nullptr"; Strict = None; Extents = s1Name + "_extents" } ]
                @ stageSweep arrName s1Name (directName k1 kernel1)
            // Deterministic deallocation, site 5e: the `>>@` two-stage pipeline.
            // Both stages allocate dense/nullptr under a borrowed extents pointer
            // (`s1` aliases the input's, `name` aliases `s1`'s), so neither owns
            // extents, and reverse-registration order frees `name` before `s1` --
            // the pointer `name.extents` borrows is still live at that moment.
            // `arrRank`/`arrName` are only meaningful for the single-array case.
            let singleArray = match arrays with [_] -> true | _ -> false
            if singleArray then
                registerPoolAlloc AllocDense s1Elem arrRank "nullptr" (s1Name + "_extents") s1Name None
            let s2Code =
                [ $"{ind}const size_t* {name}_extents = {s1Name}.extents;"
                  arrayAlloc { Ind = ind; Elem = s2Elem; Rank = arrRank; Name = name
                               Symm = "nullptr"; Strict = None; Extents = name + "_extents" } ]
                @ stageSweep s1Name name (directName k2 kernel2)
            if singleArray then
                registerPoolAlloc AllocDense s2Elem arrRank "nullptr" (name + "_extents") name None
            (elemTypeErrCode @ s1Code @ [""] @ s2Code, ctx)
        | _ when not (stageEmittable kernelName1 kernel1 && stageEmittable kernelName2 kernel2) ->
            // STAGED EMISSION IS TWO-STAGE (v1), and `IRComposeObj` NESTS. A
            // three-stage `o1 >>@ o2 >>@ o3` therefore arrives with a whole
            // COMPOSITION sitting in a stage slot: `kernel1` is the inner
            // `IRComposeObj`, which is neither a named C++ lambda nor a
            // resolvable callable.
            //
            // The shape only gets this far when SURFACE FUSION DECLINED --
            // fusion normally collapses the chain into one kernel before the
            // emitter sees it (loops/176), and it declines on, e.g., a stage
            // whose kernel is a BLOCK-BODIED named function, since a block is
            // not an expression to inline (loops/179).
            //
            // Left alone, the arm below handed `genApplyCombinator` a kernel it
            // could not resolve and the nest emitted `r__s1[__i0] = ((void)0);`
            // -- delivered as a raw g++ "void value not ignored as it ought to
            // be", with no BL code, no Blade line, and nothing naming the actual
            // limitation. Refuse here instead, on the BL7004 channel, so the
            // message says which ceiling was hit and what to write instead.
            let errCode =
                codegenError ctx ind
                    "a pipeline stage with a block-bodied kernel cannot fuse, and staged emission supports \
two stages (v1) -- this `>>@` chain has more than two, so one stage reaches the emitter as a composition \
rather than a kernel. Split the chain and force the halves \
(`let s = (o1 >>@ o2) <@> A |> compute` then `object_for(k3) <@> s |> compute`), or give every stage an \
expression-bodied kernel so the whole pipeline fuses into one loop"
            (errCode, ctx)
        | _ ->
            // Fallback: inline lambdas - use ApplyInfo per stage.
            // We materialize via direct `genApplyCombinator` calls
            // (rather than constructing IRBindings and going through
            // genBinding) to keep this helper independent of the
            // recursive let-binding group below.
            let s1Name = $"{name}__s1"
            let s1Id = builder.FreshId()
            let s1ElemType : IRType =
                match resolveCallable kernel1 with
                | Some callable -> callable.RetType
                | None -> IRTScalar ETFloat64
            let inputArrayTypes = arrays |> List.map (fun a ->
                match inferExprType a with
                | ArrayElem arr -> arr
                | _ -> defaultArrayType (IRTScalar ETFloat64))
            let totalInputDims = inputArrayTypes |> List.sumBy arrayRank
            let s1Type = mkArrayArrow [for _ in 1..totalInputDims -> defaultIndexType ()] s1ElemType None
            let s1Info = buildSimpleApplyInfo arrays kernel1 s1Type
            let code1 = genApplyCombinator ctx s1Name s1Info builder
            let ctx1 = addVarName s1Id s1Name ctx

            let s2OutputType =
                match outputType with
                | IRTUnit ->
                    match s1Type with
                    | ArrayElem arr -> mkArrayLike { arr with ElemType = s1ElemType }
                    | _ -> mkArrayLike (defaultArrayType s1ElemType)
                | other -> other
            let s2Info = buildSimpleApplyInfo [IRVar(s1Id, s1Type)] kernel2 s2OutputType
            let code2 = genApplyCombinator ctx1 name s2Info builder
            (code1 @ [""] @ code2, ctx1)
    | _ ->
        // Composition didn't resolve to IRComposeObj at codegen time -
        // should be impossible after the IRComposeApply split. Emit
        // codegen-time error rather than silently generating bad code.
        let errCode = codegenError ctx ind (sprintf "IRComposeApply: Composition did not resolve to IRComposeObj (got %A) - IR-builder bug" resolvedComposition)
        (errCode, ctx)

/// Attempt ONE merged loop nest materializing every leaf of a computation
/// combinator tree. This is the <&!> compute path and the opportunistic <&>
/// path: per-leaf output allocation, a single (possibly staggered) nest via
/// genFusedLoopNest, and the flat make_tuple convention. `isMandatory`
/// selects the backend rule: <&!> (true) requires every leaf's backend to be
/// IDENTICAL; <&> (false) lets absence (serial) defer to an explicit sibling.
/// Error carries a diagnosis when the leaves cannot legally share a nest --
/// the caller decides (<&!> reports it, <&> falls back to independent nests).
let tryGenMergedCompute (ctx: CodeGenContext) (name: string) (infos: ApplyInfo list) (isMandatory: bool) (builder: IRBuilder) : Result<string list * string * Map<string, string list>, string> =
    let ind = indentStr ctx
    let arrayNamesOf (info: ApplyInfo) =
        info.Arrays |> List.mapi (fun i arr ->
            match arr with
            | IRVar (id, _) -> Map.tryFind id ctx.VarNames |> Option.defaultValue ($"arr{i}")
            | IRRange _ -> $"__range{i}"
            | IRVirtualReverse _ -> $"__rev{i}"
            | _ -> $"arr{i}")
    if infos |> List.exists (_.Arrays.IsEmpty) then
        Error ($"no arrays in method_for for fused '{name}'")
    elif infos |> List.exists (fun info ->
             info.Arrays |> List.exists (fun a ->
                 match a with
                 | IRRange (its, _) -> its |> List.exists (fun ix -> ix.IxKind = IxKCompound)
                 | _ -> false)) then
        // The fused multi-output path allocates outputs through the dense
        // extents machinery and does not materialize a standalone
        // compound_index_t; a compound range here would emit references to
        // an undeclared `__rangeN_cidx`.
        Error "range<CompoundIdx> is not yet supported in fused (multi-output) loop applications; use a single-kernel method_for"
    else
        let leafNames = infos |> List.mapi (fun i _ -> $"{name}_{i}")
        // Each leaf's nest is built against its OWN arrays -- allocation
        // extents and kernel-param element bindings both come from here.
        let leafCgs = infos |> List.mapi (fun i info ->
            // S2 routing, same rule as the single-kernel site.
            routeKernelBodyThroughCall info (buildLoopNestCodeGen info (arrayNamesOf info) leafNames.[i] builder))
        let backends = infos |> List.map classifyLeafBackend
        // Deterministic deallocation, site 2: `declCode` below is built EAGERLY
        // but reaches the output only through `wrap` (the host arms). The device
        // arms use `wrapDevice`, which deliberately drops it (cuda/mpi leaves emit
        // their own host restore buffers), and the mixed-backend arm returns Error
        // and emits nothing. Registering inside the mapi would therefore emit
        // frees for C++ variables that do not exist on those paths -- a hard
        // compile error in the device gates. So collect thunks here and fire them
        // from `wrap` only, which is reached exactly once per host emission.
        let leafRegs = System.Collections.Generic.List<unit -> unit>()
        // Host output allocation -- shared by every backend (cuda/mpi restore
        // the full output on the host too).
        let declCode = leafCgs |> List.mapi (fun i cg ->
            let lname = leafNames.[i]
            let symmVecName = $"{lname}_symm"
            // Pass nullptr DIRECTLY when there's no symmetry -- a function-local
            // `static constexpr const size_t* X_symm = nullptr` can't be used
            // as a constant template arg under MSVC (C2131; the address of a
            // function-local static isn't a core-constant-expression). Only
            // emit a named decl for the non-empty (real symmetry) case.
            let symmArg =
                if hasRealSymmetry cg.OutputSymmVec then hoistSymmDecl symmVecName cg.OutputSymmVec
                else "nullptr"
            let outputRank = match cg.OutputType with ArrayElem arr -> arrayRank arr | _ -> 0
            let outputElemType = match cg.OutputType with ArrayElem arr -> elemTypeToCpp arr.ElemType | IRTScalar et -> primTypeToCpp et | t -> irTypeToCpp t
            let extentsName = $"{lname}_extents"
            // Same structural literal pairing as the single-kernel dense arm:
            // only the IRLit arm is compile-time; an all-literal table takes
            // the static constexpr form and owns nothing (emitExtentsTable
            // returns None), so the leafReg below registers no delete[].
            let extentDims =
                cg.Bindings |> List.map (fun b ->
                    match b.Extent with
                    | IRLit (IRLitInt n) -> (string n, true)
                    | _ ->
                        match b.FusedRank with
                        | Some d ->
                            let prod = [0 .. d - 1] |> List.map (sprintf "%s.extents[%d]" b.ExtentArrayRef) |> String.concat " * "
                            (prod, false)
                        | None -> ($"{b.ExtentArrayRef}.extents[{b.ExtentDimRef}]", false))
            let (extentDecls, ownedExtents) = emitExtentsTable ind extentsName outputRank extentDims
            let allocRhs =
                match emitAllocRhs (classifyOutputStorage cg.OutputType)
                          outputElemType outputRank symmArg extentsName with
                | Ok rhs -> rhs
                | Error msg -> recordCodegenRefusal msg; $"{{ nullptr, {extentsName} }};\n#error \"{msg}\""
            let allocDecl = $"{ind}Array<{outputElemType}, {outputRank}> {lname} = {allocRhs};"
            (match cg.OutputType with
             | ArrayElem at when isFreeableDenseArrayType at ->
                 leafRegs.Add(fun () ->
                     registerPoolAlloc (classifyOutputStorage cg.OutputType)
                         outputElemType outputRank symmArg extentsName lname ownedExtents)
             | _ -> ())
            extentDecls @ [allocDecl]) |> List.concat
        let tupleLine = $"""{ind}auto {name} = std::make_tuple({(leafNames |> String.concat ", ")});"""
        let childrenMap = Map.ofList [name, leafNames]
        let wrap body =
            // declCode is genuinely emitted on this path -- see site 2 above.
            // dealloc(D): the former all-host gate here is gone. `wrap` is reached
            // by exactly two arms, the merged host nest and the MPI co-fusion, and
            // BOTH prepend declCode verbatim; the MPI arm's per-leaf Allgatherv
            // leaves every leaf fully restored on every rank, so its outputs are
            // ordinary scope-owned locals. The all-cuda arm goes through
            // `wrapDevice` (which drops declCode) and registers its own host
            // restore buffers inside genCudaCoFusion instead.
            leafRegs |> Seq.iter (fun f -> f ())
            (declCode @ [""] @ body @ [""] @ [tupleLine], name, childrenMap)
        // Device paths emit their OWN output allocation inline (host restore
        // buffers), so they must NOT prepend the host declCode.
        let wrapDevice body = (body @ [""] @ [tupleLine], name, childrenMap)
        let staggered =
            let deepest = leafCgs |> List.map (_.Bindings.Length) |> List.max
            leafCgs |> List.exists (fun cg -> cg.Bindings.Length <> deepest)

        // Loop structure (bounds/triangularity) must agree regardless of
        // backend. THEN the per-level backend must agree at each shared level:
        // shared levels are one physical header, so one backend.
        checkMergeCompatible leafCgs |> Result.bind (fun _primary ->
            let backendName = function
                | BkSerial -> "serial" | BkOmp -> "omp"
                | BkCuda _ -> "cuda" | BkMpi -> "mpi"
            match backends with
            // ---- All host (serial/omp): merged host nest --------------------
            | _ when backends |> List.forall isHostBackend ->
                let anyOmp = backends |> List.exists (_.IsBkOmp)
                let allOmp = backends |> List.forall (_.IsBkOmp)
                let allSerial = backends |> List.forall (_.IsBkSerial)
                let hostParallelR =
                    if isMandatory then
                        // <&!>: every leaf's backend must be identical.
                        if allOmp then Ok true
                        elif allSerial then Ok false
                        else Error "mixed serial/omp leaves under <&!> -- mandatory fusion needs one backend at the shared level; annotate every leaf the same or use <&>"
                    else
                        // <&>: absence (serial) defers to an explicit omp sibling.
                        Ok anyOmp
                hostParallelR |> Result.map (fun hostParallel ->
                    let (sm, sp, sNew) = streamedNestSetup ctx.StreamedArrays ind leafCgs
                    registerStreamBufDecls sNew
                    wrap (sp @ genFusedLoopNestStreamed sm leafCgs ctx.VarNames ctx.Indent hostParallel None))
            // ---- All cuda: device co-fusion --------------------------------
            | _ when backends |> List.forall (_.IsBkCuda) ->
                let blockSizes = backends |> List.choose (function BkCuda b -> Some b | _ -> None) |> List.distinct
                if blockSizes.Length > 1 then
                    Error "cuda leaves request different block sizes -- a shared launch needs one block size; unify the cuda(block: N) clauses or force separately"
                elif staggered then
                    // A flat thread grid over the deepest leaf's cardinality
                    // would redundantly (racily) re-write the shallow leaves'
                    // cells; correct staggered-device fusion needs guarded
                    // writes -- a separate design. Reject with steering.
                    Error "cuda co-fusion of DIFFERENT arities (staggered nest) is not supported yet -- give the leaves equal arity or force each with |> compute"
                else
                    match genCudaCoFusion leafCgs leafNames name blockSizes.Head with
                    | Some inlineLines -> Ok (wrapDevice inlineLines)
                    | None -> Error "cuda co-fusion requires the leaves to be rectangular, non-Reynolds, boundary-safe, and share the same input arrays -- force the leaves separately with |> compute"
            // ---- All mpi: domain co-fusion ---------------------------------
            // ONE outer-row slab decomposition SHARED by every leaf: each rank
            // runs the merged (possibly staggered) nest over its [lo, hi) rows,
            // then each leaf's output -- a contiguous outer-row pool slab -- is
            // restored on all ranks by its own Allgatherv. Requires every leaf
            // dense rectangular; triangular/simplicial is not co-decomposed.
            | _ when backends |> List.forall (_.IsBkMpi) ->
                let primaryCg = leafCgs |> List.maxBy (_.Bindings.Length)
                let ineligible =
                    List.zip leafCgs (leafCgs |> List.map classifyMpiShape)
                    |> List.tryPick (fun (cg, shape) ->
                        match shape with
                        | MpiDense -> None
                        | MpiSimplicial -> Some ($"leaf '{cg.OutputName}' is triangular; mpi co-fusion decomposes dense rectangular leaves only (v1)")
                        | MpiIneligible r -> Some ($"leaf '{cg.OutputName}': {r}"))
                match ineligible with
                | Some reason -> Error reason
                | None ->
                // Hybrid mpi+omp co-fusion: the shared slab loop is ALSO the
                // (single) omp-parallel region when the leaves' inner backend
                // is omp. Leaf bindings' IsParallel carries the kernel's omp
                // opt-in; the <&!>/<&> agreement rule mirrors the host arm.
                let leafOmp = leafCgs |> List.map (fun cg -> cg.Bindings |> List.exists (_.IsParallel))
                let hostParallelR =
                    if isMandatory then
                        if leafOmp |> List.forall id then Ok true
                        elif leafOmp |> List.forall (id >> not) then Ok false
                        else Error "mixed serial/omp INNER backends under <&!> mpi co-fusion -- the shared slab is one omp region; annotate every leaf `mpi, omp(...)` the same or use <&>"
                    else
                        Ok (leafOmp |> List.exists id)
                hostParallelR |> Result.bind (fun hostParallel ->
                    let outerBound =
                        genLoopBoundExpr (compoundArrayNamesOf primaryCg.Bindings) (List.head primaryCg.Bindings)
                    // Shared row-slab bounds (balanced split; P > n -> empty slabs).
                    let prologue =
                        [ $"{ind}size_t __blade_mpi_n_{name} = {outerBound};"
                          $"{ind}size_t __blade_mpi_q_{name} = __blade_mpi_n_{name} / (size_t)__blade_mpi_size;"
                          sprintf "%ssize_t __blade_mpi_r_%s = __blade_mpi_n_%s %% (size_t)__blade_mpi_size;" ind name name
                          $"{ind}size_t __blade_mpi_lo_{name} = (size_t)__blade_mpi_rank * __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? (size_t)__blade_mpi_rank : __blade_mpi_r_{name});"
                          $"{ind}size_t __blade_mpi_hi_{name} = __blade_mpi_lo_{name} + __blade_mpi_q_{name} + ((size_t)__blade_mpi_rank < __blade_mpi_r_{name} ? 1 : 0);" ]
                    let (sm, sp, sNew) = streamedNestSetup ctx.StreamedArrays ind leafCgs
                    registerStreamBufDecls sNew
                    let nest = sp @ genFusedLoopNestStreamed sm leafCgs ctx.VarNames ctx.Indent hostParallel (Some name)
                    // Per-leaf Allgatherv: leaf k's pool holds a contiguous slab
                    // [lo*inner_k, hi*inner_k) of outer rows (inner_k = product of
                    // its non-outer extents), so MPI_IN_PLACE reassembles it.
                    let gathers =
                        leafCgs |> List.mapi (fun k cg ->
                            let lname = leafNames.[k]
                            let outputRank = match cg.OutputType with ArrayElem arr -> arrayRank arr | _ -> 1
                            let outElemCpp = match cg.OutputType with ArrayElem arr -> elemTypeToCpp arr.ElemType | _ -> "double"
                            let dtype =
                                match cg.OutputType with
                                | ArrayElem at -> (match at.ElemType with AnyPrimElem et -> mpiDatatypeOf et | _ -> None)
                                | _ -> None
                                |> Option.defaultValue "MPI_DOUBLE"
                            let extentsName = $"{lname}_extents"
                            let innerProd =
                                if outputRank <= 1 then "1"
                                else [1 .. outputRank - 1] |> List.map (fun i -> $"{extentsName}[{i}]") |> String.concat " * "
                            [ $"{ind}{{ // MPI: restore full {lname} on all ranks"
                              $"{ind}    {outElemCpp}* __blade_mpi_pool = nested_array_utilities::pool_base({lname}.data);"
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
                              $"{ind}}}" ]) |> List.concat
                    Ok (wrap (prologue @ [""] @ nest @ [""] @ gathers)))
            // ---- Mixed backends: cannot share one nest ----------------------
            | _ ->
                let names = backends |> List.map backendName |> List.distinct |> String.concat ", "
                Error ($"leaves request different execution backends ({names}) -- a fused nest has one backend per shared level; force the differing leaves separately with |> compute"))

/// <&> SOFT JOIN over independent cuda leaves that cannot share one nest:
/// each leaf keeps its OWN kernel (own block size, arity, inputs) and the
/// launches are split into a begin pass (H2D + async launch) and an end pass
/// (sync + D2H), with leaves assigned round-robin across visible devices
/// INSIDE the .cu wrappers (leaf % deviceCount -- the host half never touches
/// the CUDA API, so the g++ split build needs no cudart link). One device =>
/// the default stream serializes the leaves (correct, no overlap -- exactly
/// the soft join's "run the rest in serial"); multiple devices => the begin
/// pass genuinely overlaps them. Returns None when any leaf is not
/// device-eligible (caller falls back to fully independent nests; kernels
/// already appended for earlier leaves become dead-but-harmless .cu defs).
let tryGenCudaSoftJoin (ctx: CodeGenContext) (name: string) (infos: ApplyInfo list) (builder: IRBuilder) : (string list * string * Map<string, string list>) option =
    let backends = infos |> List.map classifyLeafBackend
    if infos.Length < 2
       || not (backends |> List.forall (_.IsBkCuda))
       || infos |> List.exists (_.Arrays.IsEmpty) then None
    else
    let arrayNamesOf (info: ApplyInfo) =
        info.Arrays |> List.mapi (fun i arr ->
            match arr with
            | IRVar (id, _) -> Map.tryFind id ctx.VarNames |> Option.defaultValue ($"arr{i}")
            | IRRange _ -> $"__range{i}"
            | IRVirtualReverse _ -> $"__rev{i}"
            | _ -> $"arr{i}")
    let leafNames = infos |> List.mapi (fun i _ -> $"{name}_{i}")
    let leafCgs = infos |> List.mapi (fun i info ->
        // S2 routing, same rule as the single-kernel site.
        routeKernelBodyThroughCall info (buildLoopNestCodeGen info (arrayNamesOf info) leafNames.[i] builder))
    let blocks = backends |> List.map (function BkCuda b -> b | _ -> 256)
    // Per-leaf emission in split mode: simplicial first, then rectangular
    // (the single-kernel dispatch order). Each returns the host output
    // allocation lines; the wrapper takes ONLY the arrays it actually reads
    // (simplicial = the first input; rectangular = all inputs).
    let pieces =
        List.zip3 leafCgs leafNames blocks
        |> List.map (fun (cg, lname, bs) ->
            match genCudaKernelSimplicial false true cg lname bs with
            | Some alloc -> Some (alloc, [List.head cg.InputArrayNames], lname)
            | None ->
                genCudaKernel true ctx.VarNames cg lname bs
                |> Option.map (fun alloc -> (alloc, cg.InputArrayNames, lname)))
    if pieces |> List.exists Option.isNone then None
    else
    let pieces = pieces |> List.map Option.get
    let header =
        [ $"    // <&> soft join: {pieces.Length} independent cuda kernels. Begin pass launches"
          "    // async round-robin over devices (inside the wrappers); end pass syncs." ]
    let allocs = pieces |> List.collect (fun (alloc, _, _) -> alloc)
    let begins =
        pieces |> List.mapi (fun k (_, args, lname) ->
            let argStr = args |> List.map (fun n -> $"pool_base({n}.data)") |> String.concat ", "
            $"    __launch_{(sanitizeCppName lname)}_begin({argStr}, {k});")
    let ends =
        pieces |> List.map (fun (_, _, lname) ->
            $"    __launch_{(sanitizeCppName lname)}_end(pool_base({lname}.data));")
    let tupleLine = $"""    auto {name} = std::make_tuple({(leafNames |> String.concat ", ")});"""
    Some (header @ allocs @ begins @ ends @ [""; tupleLine], name, Map.ofList [name, leafNames])

/// Recursively generate code for a parallel composition tree (<&>).
/// When every leaf is an unforced loop application whose loop structures can
/// legally share one nest, the leaves are MERGED into a single (possibly
/// staggered) nest -- <&> means "fuse when legal" -- so towers like
/// mean <&> cov <&> skew stream the shared arrays from one load. Otherwise
/// each leaf gets its own independent loop nest.
/// Returns (code_lines, result_variable_name, tupleChildrenMap).
/// tupleChildrenMap tracks pair structure for nested tuple destructuring.
let rec genParallelTree (ctx: CodeGenContext) (name: string) (expr: IRExpr) (builder: IRBuilder) : string list * string * Map<string, string list> =
    let ind = indentStr ctx
    // Collect all leaf expressions from the parallel/fusion tree
    let rec collectLeaves (e: IRExpr) : IRExpr list =
        match e with
        | IRParallel (left, right, _) | IRFusion (left, right) ->
            collectLeaves left @ collectLeaves right
        | IRVar (id, _) ->
            match Map.tryFind id ctx.DeferredComputations with
            | Some deferred -> collectLeaves deferred
            | None -> [e]
        | _ -> [e]
    let leaves = collectLeaves expr
    match leaves with
    | [single] ->
        // Single leaf -- generate directly, no tuple wrapping
        match single with
        | IRApplyCombinator info ->
            let code = genApplyCombinator ctx name info builder
            (code, name, Map.empty)
        | IRComposeApply info ->
            let (code, _) = genComposeApply ctx name info info.OutputType builder
            (code, name, Map.empty)
        | IRVar (id, _) ->
            let existingName = Map.tryFind id ctx.VarNames |> Option.defaultValue name
            ([], existingName, Map.empty)
        | _ ->
            let code = genScalarBinding ctx name single (inferExprType single)
            (code, name, Map.empty)
    | _ ->
        // Opportunistic fusion: all leaves unforced loop applications with
        // mergeable loop structures -> one shared nest (see doc comment).
        let mergeInfos = leaves |> List.choose (function IRApplyCombinator info -> Some info | _ -> None)
        let merged =
            if mergeInfos.Length = leaves.Length && mergeInfos.Length >= 2 then
                match tryGenMergedCompute ctx name mergeInfos false builder with
                | Ok result -> Some result
                | Error _ ->
                    // <&> is a SOFT join: leaves that cannot share one nest
                    // still run. Independent cuda leaves get the multi-device
                    // begin/end driver; anything else falls through to the
                    // fully independent per-leaf nests below.
                    tryGenCudaSoftJoin ctx name mergeInfos builder
            else None
        match merged with
        | Some result -> result
        | None ->
        // Multiple leaves -- generate each, assemble flat tuple
        let leafNames = leaves |> List.mapi (fun i _ -> $"{name}_{i}")
        let allCode =
            (leaves, leafNames) ||> List.map2 (fun leaf leafName ->
                match leaf with
                | IRApplyCombinator info ->
                    genApplyCombinator ctx leafName info builder @ [""]
                | IRComposeApply info ->
                    let (code, _) = genComposeApply ctx leafName info info.OutputType builder
                    code @ [""]
                | IRVar (id, _) ->
                    let existingName = Map.tryFind id ctx.VarNames |> Option.defaultValue leafName
                    if existingName <> leafName then
                        [$"{ind}auto& {leafName} = {existingName};"; ""]
                    else []
                | _ ->
                    genScalarBinding ctx leafName leaf (inferExprType leaf) @ [""])
            |> List.concat
        let tupleLine = $"""{ind}auto {name} = std::make_tuple({(leafNames |> String.concat ", ")});"""
        let childMap = Map.ofList [name, leafNames]
        (allCode @ [tupleLine], name, childMap)

/// Collect all leaf expressions from a fusion tree in left-to-right order.
let rec collectFusionLeaves (expr: IRExpr) : IRExpr list =
    match expr with
    | IRFusion (left, right) -> collectFusionLeaves left @ collectFusionLeaves right
    | _ -> [expr]

/// Build nested std::make_pair expression matching the tree structure of an IRFusion/IRParallel.
/// Consumes names from the list in left-to-right order.
let rec buildPairTree (expr: IRExpr) (names: string list) : string * string list =
    match expr with
    | IRFusion (left, right) | IRParallel (left, right, _) ->
        let (leftStr, names') = buildPairTree left names
        let (rightStr, names'') = buildPairTree right names'
        ($"std::make_pair({leftStr}, {rightStr})", names'')
    | _ ->
        match names with
        | n :: rest -> (n, rest)
        | [] -> (exprError "internal: no names left in buildPairTree", [])

/// Generate code for N-way mandatory fusion (<&!>).
/// Collects all leaf ApplyCombinators, generates a single fused loop nest,
/// then builds nested pair tree for the result.
/// Build named pair tree from an IRExpr tree structure and a list of leaf names.
/// Generates named intermediate std::make_pair variables for each internal node.
/// Uses __p_ prefix for intermediates to avoid collision with leaf names.
/// Returns (code_lines, result_name, tupleChildrenMap).
let genFusionTree (ctx: CodeGenContext) (name: string) (expr: IRExpr) (builder: IRBuilder) : string list * string * Map<string, string list> =
    let ind = indentStr ctx
    let rawLeaves = collectFusionLeaves expr
    
    // Resolve IRVar leaves through DeferredComputations
    let leaves = rawLeaves |> List.map (fun leaf ->
        match leaf with
        | IRVar (id, _) ->
            match Map.tryFind id ctx.DeferredComputations with
            | Some deferred -> deferred
            | None -> leaf
        | _ -> leaf)
    
    // Extract ApplyInfo from each leaf
    let infos = leaves |> List.choose (fun e ->
        match e with
        | IRApplyCombinator info -> Some info
        | _ -> None)
    
    if infos.Length < 2 || infos.Length <> leaves.Length then
        // Not all leaves are ApplyCombinators -- fall back to parallel generation
        genParallelTree ctx name expr builder
    else
        // <&!> is MANDATORY fusion: incompatible loop structures are a loud
        // codegen diagnostic, never a silent fallback to independent nests
        // (use <&> for that).
        match tryGenMergedCompute ctx name infos true builder with
        | Ok result -> result
        | Error reason ->
            (codegenError ctx ind ($"<&!> cannot fuse these computations into one loop nest: {reason} (use <&> to allow independent loops)"), name, Map.empty)

/// Compute the number of flat leaves for a type (recursing into nested tuples).
let rec tupleLeafCount (ty: IRType) : int =
    match ty with
    | IRTTuple ts -> ts |> List.sumBy tupleLeafCount
    | _ -> 1

/// For a tuple type, compute the flat child range [start, start+count) for each top-level element.
/// E.g. ((alpha,beta), gamma) -> [(0, 2); (2, 1)] meaning element 0 spans flat indices 0..1, element 1 is flat index 2.
let tupleLeafRanges (ty: IRType) : (int * int) list =
    match ty with
    | IRTTuple ts ->
        let mutable offset = 0
        ts |> List.map (fun t ->
            let count = tupleLeafCount t
            let range = (offset, count)
            offset <- offset + count
            range)
    | _ -> [(0, 1)]

/// C++-side name a binding is DECLARED under. Anonymous tuple bindings ("_")
/// get a unique synthesized name to avoid C++ redefinition errors. Shared by
/// the binding dispatcher and every per-shape generator in its recursive chain.
///
/// Sanitized, because `addVarName` sanitizes every name it records and every
/// consumer resolves through `ctx.VarNames`: a binding whose source name is a
/// C++ reserved word (`let final = ...`, `let class = ...`) used to be declared
/// raw here and referenced as `final_` / `class_` everywhere else, so the
/// program either failed to compile at the declaration (`double class = ...`)
/// or at the first use (`'final_' was not declared in this scope`). One
/// spelling, decided here. `sanitizeCppName` is idempotent, so the second
/// application inside `addVarName` is a no-op.
let bindingCppName (binding: IRBinding) : string =
    if binding.Name.StartsWith "_(" then $"__tup_{binding.Id}" else sanitizeCppName binding.Name

/// Generate C++ code for an IR binding: the DISPATCHER.
/// Each binding shape's emission lives in its own named `genXxxBinding`
/// generator below (same `let rec ... and` chain), so every path is
/// independently findable and testable; this match only destructures the
/// shape and delegates. Arms not yet extracted retain their inline bodies --
/// the migration is one generator at a time, gated by the full suite.
/// Under MPI scaffolding, a provider write must run on ONE rank only --
/// every rank executes main() (SPMD), and P processes racing on the same
/// store files would tear them. Rank 0 writes; other ranks skip. (Data is
/// identical on all ranks: distributed reads Allgatherv-restore, and mpi
/// kernel outputs are restored the same way.)
let guardProviderWrite (ind: string) (lines: string list) : string list =
    if mpiProgramOn () then
        [ ind + "if (__blade_mpi_rank == 0) { // provider write: rank 0 only (SPMD)" ]
        @ (lines |> List.map (fun s -> ind + "    " + s))
        @ [ ind + "}" ]
    else
        lines |> List.map (fun s -> ind + s)

/// Copy between a packed (SymIdx/AntisymIdx leading group + dense trailing)
/// array's storage and its canonical flat pool buffer `<flatBase>_flat`
/// (ascending-lex cells x row-major trailing block). Direction:
/// toFlat=false fills the array from the buffer (read materialization);
/// toFlat=true fills the buffer from the array (write flatten).
///
/// BOTH symmetry classes copy linearly over `pool_base`: allocate<> places
/// every scalar in ONE contiguous pool in DFS order, and each level's child
/// absolute coordinate (lastIndex + strictOff + i) is monotone in i, so the
/// DFS leaf order IS ascending-lex over canonical cells for strict (antisym)
/// exactly as for inclusive (symmetric) storage. The pool holds precisely
/// `cardinality` cells, no padding, no dead diagonal: C(n,r) strict cells for
/// antisym, C(n+r-1,r) for sym -- the same invariant genMpiNestSimplicial
/// writes through, and what ZarrTriangularSpec.md pins as "a Blade runtime
/// read is a straight pool copy".
///
/// DIAGONAL-ANCHORED subscripts (`ix[k] - ix[k-1]`) look right but are wrong:
/// they assume the strict allocator keeps a dead diagonal slot per level. It
/// does not -- strict rows are SHORTENED, so the correct subscript is
/// `ix[k] - ix[k-1] - 1` (canon_left_justify, cpp:863). The diagonal-anchored
/// form shifts the whole pool by one cell in both directions and runs one
/// cell past the end of the last row.
let genPackedPoolCopy (arrTy: IRArrayType) (arrayCpp: string) (flatBase: string) (varName: string) (toFlat: bool) : string list =
    let (lead, trailing) =
        match arrTy.IndexTypes with
        | l :: rest when l.Symmetry <> SymNone && l.Rank >= 2 -> (l, rest)
        | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"packed pool copy of '{varName}': expected a leading packed group")))
    if trailing |> List.exists (fun ix -> ix.Symmetry <> SymNone || ix.Rank <> 1) then
        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"packed pool copy of '{varName}': only one leading packed group plus dense trailing dims is supported")))
    let litOf (e: IRExpr) =
        match e with
        | IRLit (IRLitInt n) -> n
        | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"packed pool copy of '{varName}' requires literal extents")))
    let n = litOf lead.Extent
    let r = lead.Rank
    let binom (m: int64) (k: int) : int64 =
        if k < 0 || m < int64 k then 0L
        else
            let mutable num = 1L
            let mutable den = 1L
            for i in 0 .. k - 1 do
                num <- num * (m - int64 i)
                den <- den * int64 (i + 1)
            num / den
    let card =
        match lead.Symmetry with
        | SymSymmetric -> binom (n + int64 r - 1L) r
        | SymAntisymmetric -> binom n r
        | s -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan (sprintf "packed pool copy of '%s': %A groups are not supported" varName s)))
    let trailExts = trailing |> List.map (fun ix -> litOf ix.Extent)
    let trail = trailExts |> List.fold (*) 1L
    // Linear pool copy (canonical cells x trailing block, contiguous) -- one
    // shape for sym and antisym alike; see the header comment for why the
    // strict pool is compact.
    let total = card * trail
    if toFlat then
        [ $$"""{ auto* __pc_pool = nested_array_utilities::pool_base({{arrayCpp}}.data);"""
          $$"""  for (size_t __pc_i = 0; __pc_i < {{total}}; __pc_i++) { {{flatBase}}_flat[__pc_i] = __pc_pool[__pc_i]; } }""" ]
    else
        [ $$"""{ auto* __pc_pool = nested_array_utilities::pool_base({{arrayCpp}}.data);"""
          $$"""  for (size_t __pc_i = 0; __pc_i < {{total}}; __pc_i++) { __pc_pool[__pc_i] = {{flatBase}}_flat[__pc_i]; } }""" ]

/// Collect the DISTINCT ids of deferred-computation bindings that `root` reads
/// POSITIONALLY (base of an IRIndex/IRExtent/IRContains) or WHOLE (a
/// function-call argument, or assignment RHS) anywhere in the tree. Such reads
/// render the array by NAME (`A[i]`, `A.extents[..]`, `f(A)`), so the deferred
/// producer must be materialized in scope first -- generalizing the
/// single-read guard in genScalarExprBinding's IRIndex arm to reads nested
/// inside imperative blocks / for-in loops / compound scalar expressions.
///
/// Motivating case: `let A = method_for(...) <@> lambda(m) -> if c then a else
/// b |> compute` -- parseIf swallows the trailing `|> compute` into the else
/// branch, so A stays a deferred IRApplyCombinator; a later imperative `A(m)`
/// read inside a `{ for .. }` block would reference an undeclared symbol.
/// Forcing the read here closes that hole exactly as |> compute would.
///
/// Only positional reads whose base is a deferred IRVar are collected; bare
/// combinator/loop-object children are NOT descended (they force/absorb their
/// own inputs), so a nested still-deferred binding is never eagerly forced.
let collectDeferredPositionalReads (ctx: CodeGenContext) (root: IRExpr) : IRId list =
    let ordered = System.Collections.Generic.List<IRId>()
    let seen = System.Collections.Generic.HashSet<IRId>()
    let note (baseExpr: IRExpr) =
        match baseExpr with
        | IRVar (id, _) when Map.containsKey id ctx.DeferredComputations ->
            if seen.Add id then ordered.Add id
        | _ -> ()
    let rec walk (e: IRExpr) =
        match e with
        | IRIndex (a, idxs, _) -> note a; walk a; List.iter walk idxs
        | IRExtent (a, _) -> note a; walk a
        | IRContains (a, v) -> note a; walk a; walk v
        // Statement / block forms. An assignment RHS that is a whole deferred
        // array (`x = A`) renders A by name, so note it like a positional read.
        | IRLet (_, v, b) -> walk v; walk b
        | IRForRange (_, lo, hi, body) -> walk lo; walk hi; walk body
        | IRAssign (t, v) -> walk t; note v; walk v
        | IRSequence es -> List.iter walk es
        | IRConstraintCheck (c, _, _, _) -> walk c
        | IRBreakIf c -> walk c
        | IRReplicate (count, body) -> walk count; walk body
        | IRGuard (c, b) -> walk c; walk b
        // Scalar / compound-expression forms.
        | IRBinOp (_, _, l, r) -> walk l; walk r
        | IRUnaryOp (_, x) -> walk x
        | IRIf (c, t, el) -> walk c; walk t; walk el
        // A whole deferred array passed as a call argument (`f(A)`) renders A
        // by name in the argument list, so the producer must be forced first.
        // Bare IRVars in OTHER whole positions (tuple elements, let RHSes) are
        // deliberately NOT noted -- those flow into deliberately-deferred forms
        // (the deferred-computation-tuple arm, alias bindings).
        | IRApp (f, args, _) -> walk f; List.iter (fun a -> note a; walk a) args
        // prodsum's fused IIFE subscripts EVERY operand by name
        // (`__ps += a[__pt] * e[__pt]`), so a deferred operand must be forced
        // first -- same rule as IRIndex, applied to all of them.
        | IRProdSum es -> List.iter (fun a -> note a; walk a) es
        | IRTuple es | IRArrayLit (es, _) | IRStack es | IRZip es -> List.iter walk es
        | IRJoin (es, _) -> List.iter walk es
        | IRComplex (re, im) -> walk re; walk im
        | IRFma (a, b, c) -> walk a; walk b; walk c
        | IRFieldAccess (o, _) -> walk o
        | IRTupleProj (x, _, _) -> walk x
        | IRTupleCons (h, t) -> walk h; walk t
        | IRTupleDecons t -> walk t
        | IRSlice (a, _, s, e2) -> walk a; walk s; walk e2
        | IRCurry (a, i, _) -> walk a; walk i
        | IRSubset (a, _, s, l) -> walk a; walk s; walk l
        | IRPolyIndex (p, i) -> walk p; walk i
        | IRPolyTail (p, _) -> walk p
        | IRPure x | IRCompute x -> walk x
        | IRMatch (s, cases) ->
            walk s
            cases |> List.iter (fun c -> Option.iter walk c.Guard; walk c.Body)
        // Rearrangement / reduction inputs (their array operands render by name).
        | IRReduce (a, k, initOpt) -> walk a; walk k; Option.iter walk initOpt
        | IRReduceCompute (comp, k, init) -> walk comp; walk k; walk init
        | _ -> ()
    walk root
    List.ofSeq ordered

/// Deferred-computation bindings a KERNEL reads through its CAPTURE list.
///
/// A lambda kernel mentioning an enclosing binding closes over it, and
/// lambda-lifting turns that into a capture PARAMETER forwarded by name at
/// every call site (`__lambda_N(<peeled args>, c)` -- captureForwardName).
/// A still-DEFERRED capture has no C++ definition at all, only genBinding's
/// "<deferred computation>" comment, so the forwarded name is undeclared:
///
///   let c  = method_for(A) <@> lambda(x) -> x * 2.0        // deferred
///   let out = ws <@> lambda(w) -> w * reduce(c, (+)) |> compute
///
/// Forcing here is half of the fix and load-bearing on the other half:
/// `tryInferReduceCompute` must ALSO decline to splice a copy of `c`'s
/// producer into the body, or the body still spells `c`'s own inputs while
/// the call site passes `c` -- two halves of one lambda disagreeing about
/// its arity. With both, `c` is an ordinary materialized array the body
/// reads by name, exactly as `|> compute` on `c` would have given it.
///
/// Only the kernel's own capture list is consulted -- the array INPUTS are
/// forced separately by the consumer, and a nested still-deferred binding
/// the body never names is left alone.
let collectDeferredKernelCaptures (ctx: CodeGenContext) (kernel: IRExpr) : IRId list =
    match resolveKernel kernel with
    | Some rk ->
        rk.Callable.Captures
        |> List.map (_.Id)
        |> List.filter (fun id -> Map.containsKey id ctx.DeferredComputations)
        |> List.distinct
    | None -> []

/// Reading a rank-1 source array that may be a COMPOUND (or SPARSE) compact
/// view rather than a dense Array. The two runtime shapes have different
/// interfaces: `Compound<T,1>` / `Sparse<T,1>` carry their length as the
/// compact index's runtime `idx->cardinality` and their cells in `.data[i]`,
/// and expose neither `.extents` nor `operator[]`. Returns (lengthExpr,
/// elementAt) so an emitter can be written once against both.
///
/// The same idiom is spelled inline at the `contains`, `sort` and `reduce`
/// sites; this is its shared form for the emitters that consume a key or
/// value array by name.
let compactOrDenseSource (e: IRExpr) (nameStr: string) : string * (string -> string) =
    let isR1Compact =
        match inferExprType e with
        | ArrayElem at -> (isCompoundArrayType at || isSparseArrayType at) && at.IndexTypes.Length = 1
        | _ -> false
    let bound =
        if isR1Compact then $"{nameStr}.idx->cardinality"
        else
            match inferExprType e with
            | ArrayElem at -> literalOrRuntimeExtentOfArray at nameStr 0
            | _ -> $"{nameStr}.extents[0]"
    let elemAt (i: string) =
        if isR1Compact then $"{nameStr}.data[{i}]" else $"{nameStr}[{i}]"
    (bound, elemAt)

/// NEGATIVE KEY = "this row belongs to no group".
///
/// A key function is allowed to do selection: a row whose key is negative is
/// dropped from the grouping entirely (SQL's WHERE fused into GROUP BY),
/// rather than forming a group of its own. This is what lets a Welch-style
/// segment family be written as one `floor` expression with the out-of-range
/// rows keyed -1, instead of a mask -> compound -> group chain.
///
/// Emitted as a `continue` guard inside every pass that walks the key array
/// (discovery, counts, permutation), so a dropped row contributes to no
/// group's offsets and `group_by` never gathers it. `<name>__perm` stays
/// allocated at the full input length and is simply under-filled.
///
/// Only NUMERIC keys can be negative: `std::string` has no `< 0`, so a string
/// key never emits the guard. EnumIdx keys are deliberately exempt too -- there
/// the admissible values are declared up front, so a negative entry in that
/// list is a value the user asked for, not a sentinel.
let negativeKeyDrop (elemType: IRType) (keyVar: string) (indent: string) : string list =
    match IR.stripUnits elemType with
    | IRTScalar (ETInt64 | ETInt32 | ETFloat64 | ETFloat32)
    | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) ->
        [$"{indent}    if ({keyVar} < 0) continue; // negative key: row belongs to no group"]
    | _ -> []

