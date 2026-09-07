// Loop-nest codegen: element-binding peel/streamed scaffolding, OpenMP
// pragma placement, the loop-nest core (genLoopNest and the flat-
// elementwise and BLAS/LAPACK dispatch), runtime-header deployment,
// tracked-allocation bookkeeping, and array-literal/scalar bindings.
module Blade.CodeGenLoopNest

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

/// Convert IRExpr to C++ using context
let exprToCppCtx (ctx: CodeGenContext) (expr: IRExpr) : string =
    exprToCpp ctx.VarNames expr

/// Render an IRExpr with a contains-substitution map active. Intended for a
/// mask renderer that walks its predicate, hoists builds for each hoistable
/// contains, builds the substitution map, and then calls this function to
/// produce the predicate's C++ string with `set.count(...)` substituted for
/// each hoisted IRContains node.
///
/// With an empty substitution this is byte-identical to `exprToCpp`.
///
/// The substitution propagates through every renderer in the rec group:
/// `exprToCppCore`, `exprToCppWithVarCore`, `genApplyCombinatorExpr`, and
/// `materializeInlineForm`. That means a contains nested inside a method_for
/// kernel inside a mask predicate gets the same treatment as a contains
/// directly in the predicate. NOT YET WIRED UP: every current caller of
/// `materializeInlineForm` / `genApplyCombinatorExpr` (the binding-level
/// entry points) passes `emptySubst`; a future mask renderer would populate
/// a real subst map when materializing a mask whose predicate carries probes.
let exprToCppWithSubst (subst: SubstMap) (names: Map<IRId, string>) (expr: IRExpr) : string =
    exprToCppCore subst names expr

// Loop Nest Code Generation

/// Generate the element binding expression for a single array at a loop level
/// Returns (cppCode, newPeeledName) where newPeeledName is used for subsequent levels
///
/// `rawRowPeel`: when true, a
/// peel that leaves exactly ONE dimension is emitted as a raw
/// `T* BLADE_RESTRICT row = parent.data[i];` instead of the `Array<T,1>` wrapper.
/// The caller (`restrictPeelSites`) is what establishes the precondition: the
/// peeled local must be consumed EXCLUSIVELY by a deeper scalar-leaf peel
/// (`row[j]`), whose rendered text is byte-identical for a wrapper and a raw
/// pointer. Never set for a local that escapes as an `Array<T,1>` value (fiber
/// kernels, `.extents[0]` intrinsic bounds, streamed binds, captures).
///
/// Soundness of the qualifier itself: every kernel output is a fresh
/// `allocate<>`/`allocate_strict<>` pool, so no written pointer ever aliases a
/// read row; read/read aliasing (`f(A, A)`) is permitted under restrict.
let genElementBindingPeel (rawRowPeel: bool) (level: LoopIndexBinding) (elem: ElementBinding) (currentName: string)
    : string * string =
    match elem.Virtual with
    | VirtualRange offset when
        (level.Extent.IsIRCompoundMask) &&
        (match elem.SlotTag with Some t when t.StartsWith "__halowin|" -> true | _ -> false) ->
        // halo<CompoundIdx<m>>: the kernel param is a POINTER into the
        // materialized compound index's contiguous rank_to_tuple table at the
        // CENTER cell (ordinal i + start). Body reads w(o) then step it by a
        // signed subscript (IRHaloUnhash: `w[(o)][0]`) -- param-local, so the
        // reads survive kernel lifting to a standalone function. The interior
        // bound shrink is on the loop header (StrictOffset).
        let centerExpr =
            match offset with
            | None -> level.IndexName
            | Some (IRLit (IRLitInt n)) -> $"({level.IndexName} + {n}L)"
            | Some off -> $"({level.IndexName} + {(exprToCpp Map.empty off)})"
        let code =
            $"const std::array<size_t, 1>* {elem.ParamName} = &{elem.ArrayName}_cidx->rank_to_tuple[{centerExpr}];"
        (code, elem.ParamName)
    | VirtualRange _ when (match level.Extent with IRCompoundMask _ | IRSparseKeys _ -> true | _ -> false) ->
        // range<CompoundIdx<m>> / range<SparseIdx<keys>>: ONE loop level over
        // the present cells (compound: lex order; sparse: given key order); each
        // kernel param binds one COORDINATE of the current cell's tuple via
        // the materialized index's O(1) unhash (rank_to_tuple lookup). The
        // index variable `<name>_cidx` is emitted by the caller's
        // materialization step (genCompoundIndexFromMask) before the nest.
        // Offsets are not meaningful on a masked product space; TypeCheck
        // never produces one for a compound range slot.
        let code = $"int64_t {elem.ParamName} = {elem.ArrayName}_cidx->unhash({level.IndexName})[{elem.RankComponent}];"
        (code, elem.ParamName)
    | VirtualRange offset ->
        // range<I>: kernel param gets the loop index, plus offset if present.
        //
        // MULTI-RANK SLOTS LAND HERE TOO, and this arm is rank-1 shaped: it
        // binds the RAW loop counter and never consults level.BoundDependencies
        // / level.StrictOffset, which the RealArray arms below apply to reach
        // the ABSOLUTE coordinate. So range<SymIdx<r,N>> hands the kernel the
        // cell's left-justified PACKED STORAGE COORDINATES -- prefix offsets --
        // where canonical[m] = p0 + ... + pm (+ m for AntisymIdx's strict step).
        // The kernel spelling A(p0) * A(p0 + p1) is correct; A(p0) * A(p1) is
        // the silent trap. Interp.Loops.peelElement mirrors this arm, so the
        // differential gates see agreement, not the divergence from
        // TypeCheck.expandedRows ("the index value at that slot") and
        // docs/formalism.md 7.3 -- which now records this as observed behaviour
        // pending a decision, with the convention pinned by
        // tests/corpus/loops/170-173. Folding `deps + strict` in here (the
        // expression the dense arm already builds) is the canonical-index fix.
        //
        // The binding must be int64_t, NOT size_t: the param is Int64-typed in
        // Blade (and the standalone lambda signature), and a size_t binding
        // makes negative intermediates wrap unsigned before any Float64
        // conversion -- 0.5 * (i - 1) at i=0 came out as 0.5 * 2^64-1.
        // Same signedness rule for the unhash and reverse arms above/below.
        let valueExpr =
            match offset with
            | None -> level.IndexName
            | Some (IRLit (IRLitInt n)) -> $"({level.IndexName} + {n}L)"
            | Some off -> $"({level.IndexName} + {(exprToCpp Map.empty off)})"
        let code = $"int64_t {elem.ParamName} = {valueExpr};"
        (code, elem.ParamName)
    | VirtualReverse ->
        // reverse<I>: kernel param gets (extent - 1 - i)
        let extentStr =
            match level.Extent with
            | IRLit (IRLitInt n) -> string n
            | _ -> $"{elem.ArrayName}.extents[{elem.DimIndex}]"
        let code = $"int64_t {elem.ParamName} = ({extentStr} - 1 - {level.IndexName});"
        (code, elem.ParamName)
    | RealArray when level.FusedRank.IsSome ->
        // Arc 1 fused JOINT level (see IRLoopStructure.fuseJointSLevels): this single loop
        // level spans the argument's whole plain-dense S-block (d dims), so the
        // grouped triangular iteration ranges over whole argument index tuples --
        // the joint symmetry, the only one an identity group licenses
        // (docs/formalism.md section 12.4). The loop var is left-justified-relative
        // under triangular chaining, so component 0 first shifts it to the
        // ABSOLUTE compound coordinate p (deps + strict offset, mirroring the
        // dense arm's case 1) and binds it once per (level, array); every
        // component then decodes its per-dim coordinate row-major
        //   coord_rc = (p / prod_{j>rc} n_j) % n_rc
        // (matching lex enumeration and the storage bijection) and peels
        // exactly one dimension of the array.
        let d = level.FusedRank.Value
        let rc = elem.RankComponent
        let extAt j = $"{elem.ArrayName}.extents[{j}]"
        let strideAfter k =
            if k >= d - 1 then "1"
            else [k + 1 .. d - 1] |> List.map extAt |> String.concat " * "
        let pAbs = $"__p{level.Level}_a{elem.ArrayPosition}"
        let pAbsDecl =
            if rc = 0 then
                let depParts = level.BoundDependencies |> List.map (sprintf "__i%d")
                let offsetParts = if level.StrictOffset > 0 then [string level.StrictOffset] else []
                let sum =
                    match depParts @ offsetParts with
                    | [] -> level.IndexName
                    | shifts -> $"""{level.IndexName} + {(String.concat " + " shifts)}"""
                $"size_t {pAbs} = {sum}; "
            else ""
        let coordName = $"{pAbs}_c{rc}"
        let coordExpr =
            if d = 1 then pAbs
            elif rc = 0 then $"{pAbs} / ({(strideAfter 0)})"
            elif rc = d - 1 then sprintf "%s %% %s" pAbs (extAt rc)
            else sprintf "(%s / (%s)) %% %s" pAbs (strideAfter rc) (extAt rc)
        let levelsConsumed = rc + 1
        let resultRank = elem.ArrayRank - levelsConsumed
        let elemTypeStr = elemTypeToCpp elem.ArrayElemType
        let newName = $"{currentName}__{level.IndexName}_{rc}"
        let peel =
            if resultRank <= 0 then
                $"{elemTypeStr} {newName} = {currentName}[{coordName}];"
            elif rawRowPeel then
                // resultRank = 1 and every consumer is `newName[j]`: drop the
                // wrapper for a restrict-qualified row pointer.
                $"{elemTypeStr}* BLADE_RESTRICT {newName} = {currentName}.data[{coordName}];"
            else
                $"Array<{elemTypeStr}, {resultRank}> {newName} = {{ {currentName}.data[{coordName}], {currentName}.extents + 1 }};"
        let code = $"{pAbsDecl}size_t {coordName} = {coordExpr}; {peel}"
        (code, newName)
    | RealArray when (match level.Extent with IRCompoundMask _ | IRSparseKeys _ -> true | _ -> false) ->
        // Tabulated (compound/sparse) axis: peel the present-cell index `r` against the COMPACT
        // buffer (.data), not the dense .extents grid. A compound axis has no
        // bound-dependency / strict shifts (it is its own group) and the compound
        // level is the array's first level, so the index is just the loop var.
        // With no trailing dims (all-dims mask, resultRank <= 0) this is the
        // scalar leaf data[r]; with a trailing dim it yields the trailing ROW
        // base pointer (data + r*trailing_stride), which the ordinary dense peel
        // then indexes for the (single) trailing level. Ragged trailing (variable
        // per-cell extent) needs a cell_offset table and is not yet emitted.
        let levelsConsumed = elem.RankComponent + 1
        let resultRank = elem.ArrayRank - levelsConsumed
        let elemTypeStr = elemTypeToCpp elem.ArrayElemType
        let r = level.IndexName
        let newName = $"{currentName}__{r}"
        let code =
            if resultRank <= 0 then
                $"{elemTypeStr} {newName} = {currentName}.data[{r}];"
            else
                // Already a raw row pointer; the restrict qualifier is free
                // here -- a raw `T*` can never have been forwarded as
                // an `Array<T,1>` value, so no consumer analysis is needed.
                $"{elemTypeStr}* BLADE_RESTRICT {newName} = {currentName}.data + {r} * {currentName}.trailing_stride;"
        (code, newName)
    | RealArray ->
        // After indexing once, remaining rank decreases
        let levelsConsumed = elem.RankComponent + 1  // How many levels of this array consumed so far
        let resultRank = elem.ArrayRank - levelsConsumed
        let elemTypeStr = elemTypeToCpp elem.ArrayElemType
        
        // Array index into THIS level, distinguished by whether the array has
        // already been peeled (sliced) at an outer level:
        //  (1) ORIGINAL array (currentName == elem.ArrayName): still flat, so
        //      the index is the ABSOLUTE coordinate = loop var + sum of
        //      bound-dependency vars + strict offset (producer-style flat
        //      read, e.g. A[__i1 + __i0]).
        //  (2) ALREADY-SLICED sub-array: each outer peel already consumed its
        //      index via `data[__ik]`, so the within-row index is the LOCAL
        //      loop var alone -- re-adding dependency vars double-counts the
        //      outer index and reads out of bounds (the compact-symmetric
        //      elementwise-read bug: sym____i0[__i1 + __i0] should be
        //      sym____i0[__i1]).
        //      StrictOffset is likewise dropped: a peeled row of a
        //      strict-packed array is ALREADY diagonal-free (build_skeleton
        //      seeds each antisym child at `i + lastIndex + 1` and shortens
        //      the row to match, so the 0-based loop var IS the slot); adding
        //      the strict offset would walk one cell past the row's canonical
        //      span, off the pool's end on the last row. (canon_left_justify,
        //      cpp:863, uses p[k] - p[k-1] - 1 for strict; this is that
        //      expression left-justified.)
        let isSliced = currentName <> elem.ArrayName
        let arrayIndex =
            if isSliced then level.IndexName   // outer index + strictness baked into the row
            else
            let depParts = level.BoundDependencies |> List.map (sprintf "__i%d")
            let offsetParts = if level.StrictOffset > 0 then [string level.StrictOffset] else []
            match depParts @ offsetParts with
            | [] -> level.IndexName
            | shifts -> $"""{level.IndexName} + {(String.concat " + " shifts)}"""
        
        let newName = $"{currentName}__{level.IndexName}"
        let code =
            if resultRank <= 0 then
                // Scalar leaf: peel returns the element value directly.
                $"{elemTypeStr} {newName} = {currentName}[{arrayIndex}];"
            elif rawRowPeel then
                // resultRank = 1 and the ONLY consumer is the deeper
                // scalar-leaf peel `newName[__ik]` -- which renders identically
                // against a raw pointer and against the wrapper's operator[].
                // Dropping the wrapper hands g++ a restrict-qualified row and,
                // with the output row hoisted the same way, a provable
                // no-alias pair.
                $"{elemTypeStr}* BLADE_RESTRICT {newName} = {currentName}.data[{arrayIndex}];"
            else
                // Sub-array peel: construct a wrapper so the sub still
                // carries shape information (.extents shifted one level
                // deeper). The wrapper's data pointer comes from indexing
                // the parent's data; the extents pointer is parent's
                // extents+1. Indexing transparency works through operator[].
                $"Array<{elemTypeStr}, {resultRank}> {newName} = {{ {currentName}.data[{arrayIndex}], {currentName}.extents + 1 }};"
        (code, newName)

/// Wrapper-preserving entry point. Kept as the public name so out-of-module
/// callers (unit tests) are unaffected; the in-module nest emitters call
/// `genElementBindingPeel` with the analysed flag.
let genElementBindingNew (level: LoopIndexBinding) (elem: ElementBinding) (currentName: string)
    : string * string =
    genElementBindingPeel false level elem currentName

/// The set of `(nest level index, ArrayPosition)` sites whose peel may drop
/// the `Array<T,1>` wrapper for a raw `T* BLADE_RESTRICT` row pointer. Qualifies
/// only when the emission context PROVES the peeled local is consumed
/// exclusively via `name[subscript]`:
///   1. the peel leaves exactly one dimension (`resultRank = 1`), and
///   2. the chain successor (same ArrayPosition, higher RankComponent, at this
///      level or deeper) is a RealArray peel bottoming out at a SCALAR leaf
///      (`name[__ik]`) -- every level of a real-array chain shares the SAME
///      `ParamVarId`, so the successor's name overwrites this one in
///      `paramFinalNames` and the intermediate never reaches the kernel body,
///   3. no level up to and including this one is a tabulated (compound/sparse)
///      axis -- that arm's peel emits `parent.data + ...`, whose parent must
///      stay a wrapper.
///
/// Elements sharing `(ArrayName, RankComponent)` at one level emit the SAME
/// C++ declaration name (`f(A, A)` dedup via `declaredNames`), so the decision
/// is per-group and granted only when every member qualifies -- else two
/// positions would emit two DIFFERENT declarations of one name (redeclaration
/// error).
let restrictPeelSites (bindings: LoopIndexBinding list) : Set<int * int> =
    let arr = List.toArray bindings
    let n = arr.Length
    let isTabulated (b: LoopIndexBinding) =
        match b.Extent with IRCompoundMask _ | IRSparseKeys _ -> true | _ -> false
    let isReal (e: ElementBinding) = e.Virtual.IsRealArray
    let chainEligible (li: int) (e: ElementBinding) =
        if not (isReal e) then false
        elif e.ArrayRank - (e.RankComponent + 1) <> 1 then false
        elif [ for lj in 0 .. li do
                 for e2 in arr.[lj].Elements do
                   if e2.ArrayPosition = e.ArrayPosition && e2.RankComponent <= e.RankComponent then
                     yield isTabulated arr.[lj] ] |> List.exists id then false
        else
            let successors =
                [ for lj in li .. n - 1 do
                    for e2 in arr.[lj].Elements do
                      if e2.ArrayPosition = e.ArrayPosition && e2.RankComponent > e.RankComponent then
                        yield (arr.[lj], e2) ]
            match successors with
            | [ (b2, e2) ] ->
                isReal e2
                && e2.RankComponent = e.RankComponent + 1
                && e2.ArrayRank - (e2.RankComponent + 1) <= 0
                && not (isTabulated b2)
            | _ -> false
    seq {
        for li in 0 .. n - 1 do
            for (_, grp) in arr.[li].Elements |> List.groupBy (fun e -> (e.ArrayName, e.RankComponent)) do
                if grp |> List.forall (chainEligible li) then
                    for e in grp do yield (li, e.ArrayPosition)
    } |> Set.ofSeq

/// Streamed-source element binding: the source is a `alias.stream` provider
/// read -- NO materialized array exists, so instead of peeling, accumulate
/// this level's ABSOLUTE site coordinate, and at the FIBER level (exactly
/// one trailing axis remaining) emit the provider's in-nest fiber read into
/// this element's dedicated buffer plus a rank-1 wrapper bind (the same
/// `Array<T,1>` shape a fiber peel would produce, so the kernel body is
/// untouched). Returns (lines, Some wrapperName at the fiber level, updated
/// accumulated site coordinates).
let genElementBindingStreamed (level: LoopIndexBinding) (elem: ElementBinding) (spec: ProviderReadSpec) (accSites: string list)
    : string list * string option * string list =
    let fiberGen =
        match Blade.ProviderRegistry.tryFind spec.Provider with
        | Some p ->
            (match p.GenStreamFiber with
             | Some g -> g
             | None -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' does not support streamed reads (variable '{spec.VarName}')"))))
        | None -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"provider '{spec.Provider}' is not registered")))
    let litExtents =
        spec.VarType.IndexTypes
        |> List.collect (fun ix -> List.replicate ix.Rank ix.Extent)
        |> List.map (fun e ->
            match e with
            | IRLit (IRLitInt n) -> n
            | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed variable '{spec.VarName}' requires literal extents"))))
    let elemTypeStr = elemTypeToCpp elem.ArrayElemType
    let bufName = $"{elem.ArrayName}_fb_p{elem.ArrayPosition}"
    let emitFiber (sites: string list) (newName: string) : string list =
        fiberGen spec.FilePath spec.VarName elem.ArrayName bufName sites spec.VarType
        @ [ $$"""Array<{{elemTypeStr}}, 1> {{newName}} = { {{bufName}}, {{elem.ArrayName}}_fiber_ext };""" ]
    match elem.Virtual with
    | RealArray when level.FusedRank.IsSome ->
        // Fused joint level (identity-group comm over multi-dim sites): keep
        // the absolute-compound-coordinate shift and the row-major per-dim
        // decode, but with LITERAL extents (there is no array to consult),
        // and replace the chained peels with coordinate accumulation.
        let d = level.FusedRank.Value
        let rc = elem.RankComponent
        let strideAfter k =
            if k >= d - 1 then 1L
            else [k + 1 .. d - 1] |> List.fold (fun acc j -> acc * litExtents.[j]) 1L
        let pAbs = $"__p{level.Level}_a{elem.ArrayPosition}"
        let pAbsDecl =
            if rc = 0 then
                let depParts = level.BoundDependencies |> List.map (sprintf "__i%d")
                let offsetParts = if level.StrictOffset > 0 then [string level.StrictOffset] else []
                let sum =
                    match depParts @ offsetParts with
                    | [] -> level.IndexName
                    | shifts -> $"""{level.IndexName} + {(String.concat " + " shifts)}"""
                [ $"size_t {pAbs} = {sum};" ]
            else []
        let coordName = $"{pAbs}_c{rc}"
        let coordExpr =
            if d = 1 then pAbs
            elif rc = 0 then $"{pAbs} / {strideAfter 0}UL"
            elif rc = d - 1 then sprintf "%s %% %dUL" pAbs litExtents.[rc]
            else sprintf "(%s / %dUL) %% %dUL" pAbs (strideAfter rc) litExtents.[rc]
        let coordDecl = $"size_t {coordName} = {coordExpr};"
        let resultRank = elem.ArrayRank - (rc + 1)
        let sites' = accSites @ [coordName]
        if resultRank <= 0 then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed variable '{spec.VarName}': elementwise consumption is not stream-eligible -- use a fiber kernel (rank-1 array parameter over the trailing axis) or bind with .read")))
        elif resultRank = 1 then
            let newName = $"{elem.ArrayName}__{level.IndexName}_{rc}"
            (pAbsDecl @ [coordDecl] @ emitFiber sites' newName, Some newName, sites')
        else
            (pAbsDecl @ [coordDecl], None, sites')
    | RealArray ->
        // Plain dense/triangular level: the ABSOLUTE coordinate (the source
        // is never sliced -- deps + strict offset always apply).
        let arrayIndex =
            let depParts = level.BoundDependencies |> List.map (sprintf "__i%d")
            let offsetParts = if level.StrictOffset > 0 then [string level.StrictOffset] else []
            match depParts @ offsetParts with
            | [] -> level.IndexName
            | shifts -> $"""{level.IndexName} + {(String.concat " + " shifts)}"""
        let resultRank = elem.ArrayRank - (elem.RankComponent + 1)
        let sites' = accSites @ [arrayIndex]
        if resultRank <= 0 then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed variable '{spec.VarName}': elementwise consumption is not stream-eligible -- use a fiber kernel (rank-1 array parameter over the trailing axis) or bind with .read")))
        elif resultRank = 1 then
            let newName = $"{elem.ArrayName}__{level.IndexName}"
            (emitFiber sites' newName, Some newName, sites')
        else
            ([], None, sites')
    | _ ->
        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed variable '{spec.VarName}': virtual/compound binding shapes are not stream-eligible")))

/// Fiber destination buffers already declared in the CURRENT program --
/// a program with several nests over one streamed source must declare
/// each `<v>_fb_p<pos>` buffer once and reuse it (sequential nests; same
/// source => same length and element type). AsyncLocal like the symm-decl
/// collector: parallel test tasks get their own cell; program assembly
/// resets it.
let internal streamBufDeclsStorage =
    System.Threading.AsyncLocal<Set<string> ref>()

let streamBufDeclsCell () : Set<string> ref =
    let v = streamBufDeclsStorage.Value
    if isNull (box v) then
        let fresh = ref Set.empty
        streamBufDeclsStorage.Value <- fresh
        fresh
    else v

/// Streamed inputs across a set of leaf nests: the streamed map restricted
/// to their inputs, plus the per-argument fiber destination buffer
/// declarations to emit before the (merged) nest -- skipping buffers already
/// declared earlier in this program. Shared by the single-leaf and fused
/// paths so buffer naming cannot drift.
///
/// Third result: the buffer names THIS call newly declared. Callers running
/// under a deterministic-deallocation scope register them there: a
/// declaration emitted inside a function/loop body is scope-local -- its
/// C++ name dies at the closing brace -- so the frame must both delete[] it
/// on scope exit (a loop-frame declaration re-executes its `new` every
/// iteration, making the per-iteration delete a recycle) and retire it from
/// the dedup set above, or a later nest in a DIFFERENT scope would skip the
/// declaration and reference an out-of-scope name. Main-top declarations
/// (no live frame) keep the program-lifetime status quo.
let streamedNestSetup (streamedArrays: Map<string, ProviderReadSpec>) (ind: string) (leafCgs: LoopNestCodeGen list) : Map<string, ProviderReadSpec> * string list * string list =
    let streamedMap =
        leafCgs
        |> List.collect (_.InputArrayNames)
        |> List.distinct
        |> List.choose (fun n -> Map.tryFind n streamedArrays |> Option.map (fun s -> (n, s)))
        |> Map.ofList
    let declared =
        if Map.isEmpty streamedMap then []
        else
            let cell = streamBufDeclsCell ()
            leafCgs
            |> List.collect (_.Bindings)
            |> List.collect (_.Elements)
            |> List.choose (fun e ->
                match Map.tryFind e.ArrayName streamedMap with
                | Some spec when e.ArrayRank - (e.RankComponent + 1) = 1 ->
                    let bufName = $"{e.ArrayName}_fb_p{e.ArrayPosition}"
                    if Set.contains bufName cell.Value then None
                    else
                        cell.Value <- Set.add bufName cell.Value
                        let elemCpp = elemTypeToCpp e.ArrayElemType
                        let fiberLen =
                            match (List.last spec.VarType.IndexTypes).Extent with
                            | IRLit (IRLitInt n) -> n
                            | _ -> raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"streamed variable '{spec.VarName}' requires literal extents")))
                        Some ($"{ind}{elemCpp}* {bufName} = new {elemCpp}[{fiberLen}];", bufName)
                | _ -> None)
            |> List.distinct
    (streamedMap, declared |> List.map fst, declared |> List.map snd)

/// Decide the OpenMP pragma for a loop NEST, based on the bound structure of
/// its levels.
///
///   - RECTANGULAR leading levels (BoundDependencies empty AND StrictOffset =
///     0) that are also COLLAPSE-ELIGIBLE fuse into
///     `#pragma omp parallel for collapse(d)`, d = count of leading
///     rectangular+eligible levels (d >= 2 to be worth it) -- valuable for
///     short outer dimensions (a 3x3x3 nest exposes only 3 threads without
///     collapse, 27 with collapse(3)).
///   - TRIANGULAR levels (inner bound depends on outer indices, e.g. j < N -
///     i) are non-rectangular, so `collapse` is unsafe; only the OUTERMOST
///     loop is parallelized, with `schedule(dynamic)` since triangular work
///     is unbalanced across the outer index.
///
/// COLLAPSE-ELIGIBILITY is a real gate, not a formality: OpenMP 5.0 permits
/// intervening code between collapsed loop headers (Blade's per-level element
/// peels rely on this, and gcc compiles/threads it correctly -- verified
/// serial-identical across repeated 4-thread runs), but code with
/// cross-iteration side effects (an OMP API call, a batch boundary) is NOT
/// permitted and makes the level collapse-INELIGIBLE; see the
/// collapseEligible predicate below for the exact rule.
///
/// Returns the pragma string (with trailing newline+indent) for the OUTERMOST
/// loop, or "" if the nest should not be parallelized. Inner loops never carry
/// a pragma. The decision is driven entirely by per-level bound structure
/// already present in the bindings -- no index-type tag is consulted, so new
/// index types get a sensible strategy from their bound shape automatically.
let genNestPragma (bindings: LoopIndexBinding list) (pragmaIndent: string) : string =
    match bindings with
    | [] -> ""
    | outer :: rest ->
        // BUILD KNOB. Every construct this function can return creates a TEAM,
        // so a serial-emission build returns "" from all of them -- the same
        // answer an unlicensed nest gets, which is the point: the nest is then
        // byte-identical to the one the same source without `omp` produces.
        // The decline is NOT silent: the caller's `ompSuppressedMarker` names
        // this knob (see `ompThreadsSuppressedReason`). See
        // `ompThreadEmissionEnabled` for why the licence stays in source.
        if not (ompThreadEmissionEnabled ()) then ""
        elif not outer.IsParallel then ""
        else
            // A level is "rectangular" iff its bound is independent of outer indices.
            let isRectangular (b: LoopIndexBinding) =
                b.BoundDependencies.IsEmpty && b.StrictOffset = 0
            // COLLAPSE-ELIGIBILITY GATE. Currently equals rectangularity, but is
            // a SEPARATE predicate by design: it is the single extension point
            // for the future "no inter-level injection" constraint (see header).
            // When a binding gains an inter-level-injection marker (batch
            // boundary, streaming stage), add that exclusion HERE -- e.g.
            //   isRectangular b && not b.HasInterLevelInjection
            // and the collapse prefix below will correctly stop before it.
            // A level must also be LICENSED by the `omp(a: n)` depth to join the
            // collapse: `n` is a per-argument permission ("up to n dimensions of
            // this argument may carry threads"), and collapse(d) threads all d
            // fused levels. Without this the depth was inert -- `omp(a: 1)` on a
            // 2-level nest emitted collapse(2), threading a dimension belonging
            // to an argument that granted nothing. IsParallel carries the
            // licence per level (IRStorage.buildLoopNestCodeGen).
            let collapseEligible (b: LoopIndexBinding) =
                isRectangular b && b.IsParallel
            // Collapse depth = length of the leading prefix that is BOTH
            // rectangular and collapse-eligible. (takeWhile stops at the first
            // level failing either condition -- that is the gate doing its job.)
            let collapseDepth =
                bindings |> List.takeWhile collapseEligible |> List.length
            // Any triangular level anywhere below the outer loop means the
            // per-outer-iteration work is unbalanced (inner extents shrink with
            // the outer index), so dynamic scheduling is warranted.
            let hasTriangularBelow =
                rest |> List.exists (fun b -> not (isRectangular b))
            if collapseDepth >= 2 then
                // Perfect, collapse-eligible rectangular prefix of >=2 levels:
                // fuse them. (A collapsed rectangular prefix is balanced; static.)
                $"#pragma omp parallel for collapse({collapseDepth})\n{pragmaIndent}"
            elif hasTriangularBelow then
                // Outer loop rectangular (or single), but triangular work below:
                // parallelize the outer loop with dynamic schedule for balance.
                //
                // THE OUTER LOOP STAYS ASCENDING, AND THAT IS THE LOAD-BALANCING
                // CHOICE, NOT THE ABSENCE OF ONE. A performance audit proposed
                // emitting this level DESCENDING (`for (i = n; i-- > 0;)`) on the
                // theory that OpenMP hands out `schedule(dynamic)` chunks in
                // iteration order, so descending would be largest-chunk-first
                // (the LPT heuristic). That is backwards for the loop shape this
                // arm actually governs, and reversing would PESSIMIZE it:
                //
                //   * `genForLoopHeader` renders a triangular level as
                //     `for (__i1 = 0; __i1 < N - __i0; __i1++)` -- the bound
                //     SUBTRACTS the outer index. Work per outer iteration is
                //     therefore DECREASING in `__i0` (at rank 3: ~C(N-i, 2)).
                //     Ascending order already hands out the largest chunk first.
                //   * The makespan of dynamic list-scheduling is about
                //     `ideal + (duration of the LAST chunk started)`. Ascending
                //     ends on the 1-cell row; descending ends on the ~C(N,2)-cell
                //     row, i.e. it converts the audit's own good bound into the
                //     bad one it was trying to escape.
                //
                // Verified against emitted C++ for a rank-3 `comm(x, y, z)` nest
                // at N = 13: levels are `__i1 < 13 - __i0` and
                // `__i2 < 13 - __i1 - __i0`.
                //
                // (Independently, `for (i = n; i-- > 0;)` is not an OpenMP
                // canonical loop form -- the test-expr must be `var op b` and
                // there must be an incr-expr -- so that spelling could not carry
                // this pragma at all. Any future descending experiment needs a
                // signed counter or a reversed-index body, and needs a REASON,
                // which the analysis above says does not exist for this shape.)
                $"#pragma omp parallel for schedule(dynamic)\n{pragmaIndent}"
            else
                // Outer loop parallel, remaining work balanced (rectangular or
                // none): plain static parallel for.
                $"#pragma omp parallel for\n{pragmaIndent}"

/// Position (index into `bindings`) of the level that should carry the nest's
/// pragma: the OUTERMOST LICENSED one, which is not always level 0.
///
/// `omp(a: n)` licenses levels of argument `a`, and the argument owning the
/// outermost level need not be the one carrying the clause -- `omp(b: 1)` where
/// `b` owns level 1 licenses an INNER level and nothing outside it. Emitting at
/// level 0 there would thread a dimension the user never granted; emitting
/// nothing would silently ignore a clause they did write. So the pragma moves
/// inward to the first licensed level. Each outer iteration then opens a team
/// over that loop -- correct, because the licence is a statement about which
/// dimensions may be threaded, not about where the team is created.
///
/// Returns None when no level is licensed (no clause, or a clause covering
/// nothing), in which case the nest is serial.
let pragmaLevelOf (bindings: LoopIndexBinding list) : int option =
    bindings |> List.tryFindIndex (_.IsParallel)

/// Explain, IN THE GENERATED C++, why a nest whose kernel asked for OpenMP is
/// nonetheless emitted serial.
///
/// Parallelism is opt-in, so "no pragma" is the correct and expected output for
/// the vast majority of nests -- which is exactly why a DROPPED `omp(...)` clause
/// is invisible: the emitted code for "never asked" and "asked, couldn't honour"
/// is byte-identical. Suppression is legitimate in the cases below, but silent
/// suppression is not: a user who wrote `where omp(...)` and got serial code has
/// no way to tell which happened. The marker costs one comment line and lands
/// where someone debugging this actually looks.
///
/// Returns [] when the kernel never requested omp (the common case) or when the
/// pragma was in fact emitted.
let ompSuppressedMarker (requested: bool) (pragmaEmitted: bool) (reason: string)
                        (markerIndent: string) : string list =
    if requested && not pragmaEmitted then
        [ markerIndent + "// " + ompSuppressedPhrase reason ]
    else []

/// Pragma (or decline marker) for the ROW LOOP of the ragged / grouped peels --
/// `tryRaggedPeel` and `tryGroupedZipPeel` in `genApplyCombinator`.
///
/// WHY THIS IS NOT `genNestPragma`. That function takes a `LoopIndexBinding list`
/// and picks collapse-vs-dynamic from per-level bound structure. The peels build
/// no bindings at all -- they bypass the loop-nest builder PRECISELY BECAUSE the
/// inner extent is ragged (per row, from an offsets table or a `lens[]` load) and
/// no `LoopIndexBinding` can describe it. Handed a single synthesized binding,
/// `genNestPragma`'s `rest` is empty, so `hasTriangularBelow` is always false and
/// it can only ever return a plain `parallel for`: it structurally cannot express
/// "one level, data-dependent imbalance", which is the entire shape here.
/// Synthesizing a dummy binding (11 fields, including an `IRExpr` extent and a
/// `SymcomState`) to extract a string this function returns in one line would be
/// a fragile lie about the nest's structure.
///
/// `schedule(dynamic)` for the same reason `genNestPragma`'s triangular arm uses
/// it: per-iteration work is proportional to the row length, and group/row lengths
/// are DATA -- arbitrarily uneven, so a static split hands one thread the long
/// rows. Unlike the triangular case the imbalance is not even monotone in `__g`,
/// so there is no ascending/descending ordering argument to make instead; dynamic
/// is the only schedule that says anything true about this shape.
///
/// NEVER SILENT WHEN THE KERNEL ASKED. The `else` branch is a marker, not `[]`,
/// because "requested but licensed nothing" is REACHABLE: `Parser.parseOmpArgs`
/// accepts any `TokInt`, so `omp(g: 0)` parses and yields `IsOmpParallel = true`
/// with a maximum depth of 0. Returning `[]` there would recreate exactly the
/// silent-drop this function exists to fix.
///
/// The decline reasons are DISTINCT STRINGS, and the knob comes first -- the same
/// order `genLoopNestStreamed` and `genFusedLoopNestStreamed` use -- so a
/// `grep "[omp]"` census over generated code can tell a knob decline from an
/// emission-shape decline from a licence decline without reading the source.
///
/// `blocker` is "this particular emission cannot be threaded whatever the licence
/// says", carrying its own reason: a streamed provider source, or a row pool that
/// only iteration 0 can allocate. An OPTION rather than a bool so a new blocker
/// arrives with its explanation attached and cannot silently reuse a wrong one.
let peelRowPragma (requested: bool) (licensed: bool) (blocker: string option)
                  (ind: string) : string list =
    if licensed && blocker.IsNone && ompThreadEmissionEnabled () then
        [ ind + "#pragma omp parallel for schedule(dynamic)" ]
    else
        let reason =
            if not (ompThreadEmissionEnabled ()) then ompThreadsSuppressedReason ()
            else
                match blocker with
                | Some r -> r
                | None -> "the omp(...) licence covers no level of the peeled row loop"
        ompSuppressedMarker requested false reason ind

/// Does this kernel body lower to something containing a LOOP of its own?
///
/// The nest machinery sees loop LEVELS; a body's own iteration is invisible to
/// it, because every construct below lowers to an IIFE (or an inline
/// materialization) that hides a `for` inside an expression position. That gap
/// is what made `BLADE_IVDEP` land on innermost headers whose bodies contain a
/// loop -- see `ivdepEligible`. Vectorization is a statement about the loop's
/// own iterations; a loop whose body is itself a loop cannot be vectorized by
/// any compiler, so the pragma there is dead text that READS like the
/// optimization already happened.
///
/// Deliberately a whole-subtree walk over `ExprShape`, not a hand-listed set of
/// positions: a construct that hides a loop is disqualifying wherever it sits
/// (a `prodsum` under an `if`, inside a `let` value, in a call argument), and
/// the generic walker cannot silently miss a variant the way a hand-maintained
/// match arm can.
///
/// CONSERVATIVE BY CONSTRUCTION: false negatives cost only a missed
/// (pre-existing) opportunity, so the list is the constructs that ALWAYS emit a
/// loop -- the two reduction intrinsics, the membership scan, and a nested
/// combinator application, which materializes a whole nest.
let rec kernelBodyContainsInnerLoop (e: IRExpr) : bool =
    match e with
    // `[&]() { ... for (__pt) ... }()`  -- CodeGen `IRProdSum` / `reduceBound`.
    | IRProdSum _ | IRReduce _ | IRReduceCompute _
    // `[&]() { for (...) if (== x) return true; ... }()` -- `IRContains` scan.
    | IRContains _
    // A nested apply/compose materializes an entire loop nest in place.
    | IRApplyCombinator _ | IRComposeApply _ -> true
    | ExprShape (children, _) -> children |> List.exists kernelBodyContainsInnerLoop

/// Generate a for-loop header (no pragma; pragmas are nest-level, see
/// genNestPragma, and are prepended only at the outermost level by the caller).
/// Bounds are computed as: extent - sum of all dependency indices
/// Array names that are compound in this loop nest: those carrying an
/// IRCompoundMask binding. A sibling binding referencing the same array with a
/// non-mask Extent is that compound's trailing dim (bound = trailing_stride).
let compoundArrayNamesOf (bindings: LoopIndexBinding list) : Set<string> =
    bindings
    |> List.choose (fun b -> match b.Extent with IRCompoundMask _ | IRSparseKeys _ -> Some b.ExtentArrayRef | _ -> None)
    |> Set.ofList

/// The C++ expression for a loop level's (un-subtracted) upper bound. Shared
/// by genForLoopHeader (the `for` header renderer) and the MPI-dense slab
/// prologue (which needs the OUTERMOST level's extent to compute per-rank
/// [lo, hi) bounds) -- factored so the two can never drift.
let genLoopBoundExpr (compoundArrays: Set<string>) (binding: LoopIndexBinding) : string =
    match binding.Extent with
    | IRLit (IRLitInt n) -> string n
    // A compound axis iterates its present cells, so its bound is the
    // runtime cardinality of the compact index, not a dense .extents entry.
    // (The compound level carries IRCompoundMask as its Extent; ExtentArrayRef
    // is the compound array's name -> `<arr>.idx->cardinality`.) A VIRTUAL
    // compound source (range<CompoundIdx<m>>) has no Compound value to hang
    // `.idx` off; its bound is the standalone materialized index
    // `<name>_cidx->cardinality` (see genCompoundIndexFromMask).
    // IRSparseKeys rides the same arm: a sparse axis also iterates its
    // cardinality, its virtual driver uses the same `_cidx` name suffix, and
    // the Sparse wrapper exposes the same `.idx->cardinality` shape.
    | IRCompoundMask _ | IRSparseKeys _ ->
        let isVirtualCompound =
            binding.Elements
            |> List.exists (fun e ->
                e.ArrayName = binding.ExtentArrayRef &&
                (e.Virtual.IsVirtualRange))
        if isVirtualCompound
        then $"{binding.ExtentArrayRef}_cidx->cardinality"
        else $"{binding.ExtentArrayRef}.idx->cardinality"
    // A compound array's trailing dim has no dense .extents; its (single)
    // trailing extent is the compact buffer's trailing_stride. A literal
    // trailing extent already took the IRLit arm above, so this catches a
    // NON-literal trailing extent, where `.extents[dim]` would reference a
    // member the Compound layout does not have. (Multi-trailing is not yet
    // supported: trailing_stride is the product, not a per-dim extent.)
    | _ when Set.contains binding.ExtentArrayRef compoundArrays ->
        $"{binding.ExtentArrayRef}.trailing_stride"
    // Arc 1 fused JOINT level: the axis spans the array's first d dense
    // dims; its bound is the product of those extents. A literal product
    // was already folded to IRLit by IRLoopStructure.fuseJointSLevels (first arm); this
    // renders the runtime form.
    | _ when binding.FusedRank.IsSome ->
        [0 .. binding.FusedRank.Value - 1]
        |> List.map (sprintf "%s.extents[%d]" binding.ExtentArrayRef)
        |> String.concat " * "
    | _ -> $"{binding.ExtentArrayRef}.extents[{binding.ExtentDimRef}]"

let genForLoopHeader (compoundArrays: Set<string>) (binding: LoopIndexBinding) : string =
    let extentStr = genLoopBoundExpr compoundArrays binding

    // Compute bound subtraction from dependencies
    let subtraction =
        if binding.BoundDependencies.IsEmpty && binding.StrictOffset = 0 then ""
        else
            let depParts = binding.BoundDependencies |> List.map (sprintf "__i%d")
            let offsetParts = if binding.StrictOffset > 0 then [string binding.StrictOffset] else []
            depParts @ offsetParts |> String.concat " - " |> sprintf " - %s"

    $"for (size_t {binding.IndexName} = 0; {binding.IndexName} < {extentStr}{subtraction}; {binding.IndexName}++) {{"

// ---------------------------------------------------------------------------
// FLAT-POOL WRITES FOR CANONICAL COMPACT FILL NESTS
// ---------------------------------------------------------------------------
// docs/plans/plan-simplex-blocked-compute.md section 0b, finding 2, is the
// measurement this implements. A compact fill nest used to reach its output row
// through the ALLOCATION-TIME ILIFFE SKELETON (`__orow = A[__i0][__i1]`), which
// costs one pointer dereference per level per loop ENTRY. Those dereferences
// amortize over the innermost run, and a simplex's innermost run is short and
// gets shorter as rank rises (mean trip count ~ n/r), so the skeleton measured
// 1.07x at rank 3 and 1.53x at rank 4 against hand-erased C++ twins that write
// the flat pool directly -- same loops, same kernel, only the addressing
// changed.
//
// The replacement is the SAME closed form the llvm lane emits
// (`EmitLlvm.emitSimplexSerialR` over `SimplexBlocksCore.prefixTerm`, which is
// property-pinned equal to `rankOfCoords` at ranks 1-5). Level k contributes
//
//     strict:     C(n-lo,   m+1) - C(n-lo-s,   m+1)
//     symmetric:  C(n-lo+m, m+1) - C(n-lo-s+m, m+1)      (m = r-k-1)
//
// where `lo` is the level's canonical floor and `s` its STORAGE coordinate --
// which is exactly the loop counter, because the nest already iterates storage
// coordinates (`for (__i1 = 0; __i1 < n - __i0; ...)`; genForLoopHeader renders
// the floor as the bound subtraction). Every term is invariant under the levels
// inside it, so each hoists to its own level and the nest pays O(1) per cell at
// any rank. At the LAST level m = 0 and the term degenerates to the loop
// counter itself -- which is why the innermost run stays affine and
// pool-contiguous, and why the store is still `__orow[__i(r-1)] = ...` with
// BLADE_IVDEP intact.
//
// CLOSED FORM, NOT A RUNNING CURSOR. The canonical nest visits the pool in
// order, so a running `*cur++` would be exact and cost no arithmetic at all --
// and it measured the same (1.54 vs 1.57 ns/cell at rank 4, inside the noise).
// A cursor makes the rows serially dependent; the closed form leaves them
// independent, so it is the one that composes with the
// `#pragma omp parallel for schedule(dynamic)` this nest already puts on its
// outer level. Free parallelism beats a rounding error.
//
// SCOPE: the fill's WRITE POINTER, and nothing else. The skeleton is still
// allocated, and every other consumer -- lazy canonical reads, printing,
// deallocation, BLAS/CUDA streaming -- still goes through it.

/// `C(x, m)` over a C++ `size_t` expression, as the unclamped falling factorial
/// `x(x-1)...(x-m+1) / m!` (folded to a literal when `x` is one).
///
/// UNCLAMPED IS SOUND HERE, which is not self-evident: `SimplexBlocksCore.binom`
/// answers 0 for x < m, and so does this product, because one of its first m
/// factors is then exactly zero and a zero factor annihilates the unsigned
/// wraparound in the factors after it. Every evaluation point the plan below
/// emits has x >= 0 (`compactFlatWritePlan` argues each one), so the emitted
/// arithmetic and the F# reference agree on every cell the nest visits.
/// Overflow is out of reach for the same reason it is in the llvm lane: the
/// pool has to fit in memory, which bounds the falling factorial by m! times
/// the pool cardinality.
let private cppFallingBinom (x: string) (m: int) : string =
    if m <= 0 then "1"
    else
        match System.Int64.TryParse x with
        | true, v -> string (Blade.SimplexBlocksCore.binom v m)
        | _ ->
            let factors =
                [ for j in 0 .. m - 1 -> if j = 0 then x else $"({x} - {j})" ]
                |> String.concat " * "
            let mutable f = 1L
            for j in 2 .. m do f <- f * int64 j
            if f = 1L then factors else $"({factors}) / {f}"

/// The emitted pieces of a canonical compact fill nest's flat-pool write.
type CompactFlatWrite = {
    /// Hoisted once, just outside the nest: the contiguous pool underneath the
    /// skeleton that `allocate<>` built.
    PoolDecl: string
    /// Lines to emit inside loop level k's body, for k = 0 .. r-2, keyed by
    /// level. The innermost level has no entry: its term IS its loop counter.
    Hoists: Map<int, string list>
    /// Replaces the skeleton row walk at the innermost row-hoist site.
    RowDecl: string
}

/// Build the closed-form flat-write plan for `codeGen`, or None when the nest is
/// not a canonical compact fill -- in which case the caller keeps the skeleton
/// row walk, which is correct for every shape.
///
/// The gate is deliberately narrow; each clause is a fact the closed form needs:
///  1. ONE compact group spanning the WHOLE array (`IndexTypes = [ix]`,
///     `ix.Rank = r`). A mixed shape (a dense axis beside a compact group)
///     addresses through a Horner chain over groups that this does not spell,
///     so it keeps the skeleton. `classifyOutputStorage` is consulted too, so
///     the plan and the ALLOCATOR cannot disagree about the layout.
///  2. A COMPILE-TIME extent, equal at every level. A compact group has one
///     extent by construction; requiring the literal is what lets the outermost
///     binomial fold to a constant and keeps the hoisted polynomials cheap.
///  3. The nest IS the canonical simplex the formula describes: level k's bound
///     subtracts exactly the outer counters 0..k-1 plus `k` for a strict group,
///     i.e. its canonical floor is `sum(outer counters) + k*strict`. A
///     reordered, fused or slabbed nest fails this and keeps the skeleton.
///
/// Hermitian rides the symmetric arm: it shares the packed upper triangle
/// (`buildSymmVec` groups it with SymSymmetric) and differs only in the
/// conjugate-on-swap READ, which this does not touch.
let compactFlatWritePlan (codeGen: LoopNestCodeGen) (outRowName: string)
                         : CompactFlatWrite option =
    match codeGen.OutputType with
    | ArrayElem at ->
        let r = List.length codeGen.Bindings
        // (1) one whole-array compact group, agreeing with the allocator.
        let strictOpt =
            match at.IndexTypes, classifyOutputStorage codeGen.OutputType with
            | [ ix ], AllocSymmetric when ix.Rank = r && r >= 2 ->
                (match ix.Symmetry with
                 | SymSymmetric | SymHermitian -> Some false
                 | _ -> None)
            | [ ix ], AllocAntisymmetric when ix.Rank = r && r >= 2 ->
                (match ix.Symmetry with SymAntisymmetric -> Some true | _ -> None)
            | _ -> None
        // (2) one compile-time extent, shared by every level.
        let ns =
            codeGen.Bindings
            |> List.map (fun b -> match b.Extent with IRLit (IRLitInt v) -> Some v | _ -> None)
        let nOpt =
            match ns with
            | Some n0 :: _ when n0 > 0L && ns |> List.forall (fun e -> e = Some n0) -> Some n0
            | _ -> None
        match strictOpt, nOpt with
        | Some strict, Some n ->
            let sInc = if strict then 1 else 0
            // (3) the bounds ARE the canonical floors the formula assumes.
            let canonical =
                codeGen.Bindings
                |> List.mapi (fun k b ->
                    b.FusedRank.IsNone
                    && b.StrictOffset = k * sInc
                    && Set.ofList b.BoundDependencies = Set.ofList [ 0 .. k - 1 ])
                |> List.forall id
            if not canonical then None
            else
                let elemCpp = elemTypeToCpp at.ElemType
                let tag = sanitizeCppName codeGen.OutputName
                let poolName = $"__opool_{tag}"
                let hiName k = $"__ohi_{tag}_{k}"
                let endName k = $"__oend_{tag}_{k}"
                let offName k = $"__ooff_{tag}_{k}"
                let names = codeGen.Bindings |> List.map (_.IndexName)
                // Level 0's upper evaluation point is the pool's own cardinality
                // argument -- `n + (r-1)` symmetric, `n` strict -- and it is a
                // literal, so `C(., r)` folds and level 0 costs one subtraction
                // plus one polynomial.
                let hi0 = n + (if strict then 0L else int64 (r - 1))
                let hoists =
                    [ for k in 0 .. r - 2 ->
                        let m1 = r - k        // = m + 1, the binomial's lower argument
                        let hiHere = if k = 0 then string hi0 else hiName k
                        let lines =
                            [ // `hi_k = n + d_k - lo_k`, and `d` falls by one per
                              // level exactly as `lo` climbs by one under
                              // strictness, so `hi_k = lowEnd_{k-1} - 1` in BOTH
                              // classes: one chain, no per-class arithmetic. It is
                              // evaluated only inside level k's body, which runs
                              // only when level k has a trip -- precisely when
                              // this is positive.
                              if k > 0 then
                                  yield $"const size_t {(hiName k)} = {(endName (k - 1))} - 1;"
                              // `lowEnd_k = n + d_k - i_k >= d_k + 1 >= 1`: an
                              // absolute coordinate never reaches the extent.
                              yield $"const size_t {(endName k)} = {hiHere} - {(List.item k names)};"
                              yield sprintf "const size_t %s = %s(%s) - (%s);"
                                        (offName k)
                                        (if k = 0 then "" else offName (k - 1) + " + ")
                                        (cppFallingBinom hiHere m1)
                                        (cppFallingBinom (endName k) m1) ]
                        (k, lines) ]
                    |> Map.ofList
                Some { PoolDecl =
                         $"{elemCpp}* {poolName} = nested_array_utilities::pool_base({codeGen.OutputName}.data);"
                       Hoists = hoists
                       RowDecl =
                         $"{elemCpp}* BLADE_RESTRICT {outRowName} = {poolName} + {(offName (r - 2))};" }
        | _ -> None
    | _ -> None

/// Generate complete loop nest as C++ code
/// Tracks peeled names across levels and generates element bindings for all arrays at each level

// permutations / permSign moved to ReynoldsCore.fs (shared term-plan core).

// `isCommutativeOp` / `isAssociativeOp` / `foldKernelBuiltinOp` /
// `foldReorderLicensed` -- the parallel-fold reorder LICENCE -- are defined
// with the BLADE_FP_REASSOC gate near the top of this file: the expression-form
// reduce (`renderReduceExpr`) has to consult the same licence before it may
// reassociate, and F# is order-dependent. One licence predicate, one answer.

// `ompReductionOperator` -- the `reduction(<op>:acc)` operator for a builtin
// fold body -- is defined with the BLADE_FP_REASSOC gate near the top of this
// file too, because the knob's `omp simd reduction` form reads it through
// `fpReassocSimdOp`, which lives with the rest of that machinery, and F# is
// order-dependent. Path A below and the knob name the SAME operator table on
// purpose: one table, one answer, whichever construct spends it.

// `foldLaneCount` -- the lane count Path B's flat form uses below -- is defined
// with the BLADE_FP_REASSOC gate near the top of this file, because the
// `prodsum` IIFE emitter in `exprToCppCore` reads it too and F# is
// order-dependent. One definition, one evaluation order.

/// Flatten nested applications of the same commutative+associative op into a list of operands.
/// E.g. (a * b) * c -> [a; b; c]
let rec flattenAssocOp (mode: IRBinOpMode) (op: IRBinOp) (expr: IRExpr) : IRExpr list =
    match expr with
    | IRBinOp (m, o, l, r) when o = op && m = mode ->
        flattenAssocOp mode op l @ flattenAssocOp mode op r
    | _ -> [expr]

/// Generate a canonical string key for an IR expression under a given name mapping.
/// Commutative binary operations have their children sorted by canonical key,
/// and associative+commutative chains are flattened and sorted, so that e.g.
/// (a * b) * c and c * (b * a) produce the same key.
/// Used for Reynolds permutation deduplication.
let rec canonicalKey (nameMap: Map<int, string>) (expr: IRExpr) : string =
    match expr with
    | IRVar (id, _) ->
        Map.tryFind id nameMap |> Option.defaultValue ($"v{id}")
    | IRParam (name, _, _) ->
        $"p:{name}"
    | IRLit lit ->
        match lit with
        | IRLitInt n -> string n
        // Round-trip spelling: %g's 6-digit key would COLLIDE distinct
        // constants and wrongly deduplicate structurally-different
        // Reynolds terms (multiplicity miscount).
        | IRLitFloat f -> floatToCppLiteral f
        | IRLitFloat32 f -> float32ToCppLiteral f
        | IRLitBool b -> if b then "true" else "false"
        | IRLitString s -> $"\"{s}\""
        | IRLitUnit -> "()"
    | IRBinOp (mode, op, l, r) when isCommutativeOp op && isAssociativeOp op ->
        let operands = flattenAssocOp mode op expr
        let keys = operands |> List.map (canonicalKey nameMap) |> List.sort
        sprintf "(%A/%A %s)" mode op (keys |> String.concat " ")
    | IRBinOp (mode, op, l, r) when isCommutativeOp op ->
        let lk = canonicalKey nameMap l
        let rk = canonicalKey nameMap r
        let children = [lk; rk] |> List.sort
        sprintf "(%A/%A %s %s)" mode op children.[0] children.[1]
    | IRBinOp (mode, op, l, r) ->
        sprintf "(%A/%A %s %s)" mode op (canonicalKey nameMap l) (canonicalKey nameMap r)
    | IRUnaryOp (op, inner) ->
        sprintf "(u%A %s)" op (canonicalKey nameMap inner)
    | IRApp (func, args, _) ->
        let fk = canonicalKey nameMap func
        let ak = args |> List.map (canonicalKey nameMap) |> String.concat ","
        $"(call {fk} [{ak}])"
    | IRIf (cond, thn, els) ->
        $"(if {(canonicalKey nameMap cond)} {(canonicalKey nameMap thn)} {(canonicalKey nameMap els)})"
    | IRLet (id, value, body) ->
        $"(let v{id}={(canonicalKey nameMap value)} in {(canonicalKey nameMap body)})"
    | IRTupleProj (tup, idx, _) ->
        $"(proj {idx} {(canonicalKey nameMap tup)})"
    | IRTuple elems ->
        let ek = elems |> List.map (canonicalKey nameMap) |> String.concat ","
        $"(tuple {ek})"
    | IRComplex (re, im) ->
        $"(complex {(canonicalKey nameMap re)} {(canonicalKey nameMap im)})"
    | IRFma (a, b, c) ->
        $"(fma {(canonicalKey nameMap a)} {(canonicalKey nameMap b)} {(canonicalKey nameMap c)})"
    | IRFieldAccess (obj, field) ->
        $"(field {(canonicalKey nameMap obj)} {field})"
    | IRStructLit (name, fields) ->
        let fk = fields |> List.map (fun (f, e) -> $"{f}={(canonicalKey nameMap e)}") |> String.concat ","
        $"(struct {name} {{{fk}}})"
    | IRMatch (scrutinee, cases) ->
        let sk = canonicalKey nameMap scrutinee
        let ck = cases |> List.map (fun c -> sprintf "%A->%s" c.Pattern (canonicalKey nameMap c.Body)) |> String.concat "|"
        $"(match {sk} [{ck}])"
    | IRIndex (arr, indices, _) ->
        let ak = canonicalKey nameMap arr
        let ik = indices |> List.map (canonicalKey nameMap) |> String.concat ","
        $"(idx {ak} [{ik}])"
    | IRArrayLit (elems, _) ->
        let ek = elems |> List.map (canonicalKey nameMap) |> String.concat ","
        $"(arrlit [{ek}])"
    | IRExtent (arr, dim) ->
        $"(extent {(canonicalKey nameMap arr)} {dim})"
    | IRRank arr ->
        $"(rank {(canonicalKey nameMap arr)})"
    | IRPolyIndex (pack, idx) ->
        $"(polyidx {(canonicalKey nameMap pack)} {(canonicalKey nameMap idx)})"
    | IRPolyTail (pack, n) ->
        $"(polytail {(canonicalKey nameMap pack)} {n})"
    | IRNth -> "nth"
    | IRZero -> "zero"
    | IRSlice (arr, dim, start, stop) ->
        $"(slice {(canonicalKey nameMap arr)} {dim} {(canonicalKey nameMap start)} {(canonicalKey nameMap stop)})"
    | IRCurry (arr, idx, rank) ->
        $"(curry {(canonicalKey nameMap arr)} {(canonicalKey nameMap idx)} {rank})"
    | IRTranspose (arr, d1, d2) ->
        $"(transpose {(canonicalKey nameMap arr)} {d1} {d2})"
    | IRDecompact (arr, d) ->
        $"(decompact {(canonicalKey nameMap arr)} {d})"
    | IRArrayNegate arr ->
        $"(array_negate {(canonicalKey nameMap arr)})"
    | IRArrayConjugate arr ->
        $"(array_conjugate {(canonicalKey nameMap arr)})"
    | IRAssign (lhs, rhs) ->
        $"(assign {(canonicalKey nameMap lhs)} {(canonicalKey nameMap rhs)})"
    | IRForRange (vid, lo, hi, body) ->
        $"(for v{vid} {(canonicalKey nameMap lo)} {(canonicalKey nameMap hi)} {(canonicalKey nameMap body)})"
    | _ ->
        // Combinators, compute, reynolds, etc. -- won't appear in kernel bodies.
        // Use unique repr to prevent false dedup.
        sprintf "(opaque %d %A)" (expr.GetHashCode()) (expr.GetType().Name)

// RANK-RAISING MAP: a kernel body that IS an array literal of scalars.
//
// `A <@> lambda(t) -> [f(t), g(t)]` deduces a rank+1 output, and the emitter
// allocates it with the trailing literal extents already in `<name>_extents`
// (that part was always right). What had no path was the ROW VALUE: the body
// is inlined at the write site as a C++ *expression*, and an array literal has
// no expression form -- it needs an extents table, an allocate and per-cell
// stores, which are statements. exprToCpp's catch-all therefore fired and
// spliced a refusal into the row slot.
//
// Rendering it as an allocating IIFE would work and be wrong: it puts a heap
// allocation + a pool copy + a free in the innermost loop of what is otherwise
// a flat store. The row's cells are known statically and the destination is a
// contiguous span, so the literal writes STRAIGHT INTO the output pool -- no
// temporary, no copy, no free. That is what makes this shape usable as the
// per-row output of a multi-accumulator join, which is the reason it exists.
//
// The helper answers the one question the emitter needs: what are this
// literal's leaf expressions, in row-major (storage) order?

/// Leaves of a (possibly nested) array literal, row-major, when EVERY leaf is
/// a scalar. `None` for a non-literal body, for a leaf that is itself
/// array-valued (the row-typed-element literal is a different construction --
/// see `inferArrayLitType`'s `rowTypedElemArr` branch -- and does not flatten
/// to a fixed cell count here), and for a ragged nest (rows of differing
/// width, whose leaves would not tile the destination row).
let rec arrayLitScalarLeaves (e: IRExpr) : IRExpr list option =
    match e with
    | IRArrayLit (es, _) when not es.IsEmpty ->
        let subs = es |> List.map arrayLitScalarLeaves
        if subs |> List.exists Option.isNone then None
        else
            let got = subs |> List.map Option.get
            // Rectangular only: sub-literals must all contribute the same
            // number of cells (a scalar element contributes exactly one).
            let widths = got |> List.map List.length |> List.distinct
            if widths.Length > 1 then None else Some (List.concat got)
    | IRArrayLit _ -> None
    | _ ->
        match inferExprType e with
        | ArrayElem _ -> None
        | IRTTuple _ | IRTUnit -> None
        | _ -> Some [e]

/// Static cell count of an array type's axes `[first .. last]` (inclusive),
/// when every one of them has a literal extent. `None` otherwise -- a runtime
/// extent cannot be matched against a literal's fixed leaf count.
let staticCellsOfAxes (at: IRArrayType) (first: int) (last: int) : int option =
    let axes = at.IndexTypes |> List.indexed |> List.filter (fun (i, _) -> i >= first && i <= last)
    if axes.Length <> last - first + 1 then None
    else
        let ns =
            axes |> List.map (fun (_, ix) ->
                match ix.Extent with
                | IRLit (IRLitInt n) when n > 0L -> Some (int n)
                | _ -> None)
        if ns |> List.exists Option.isNone then None
        else Some (ns |> List.map Option.get |> List.fold (*) 1)

/// Reynolds kernel codegen result: C++ expression + dedup statistics.
type ReynoldsResult = {
    CppExpr: string
    TotalPerms: int
    UniqueTerms: int
}

/// Generate the kernel expression string, applying Reynolds permutation sum if needed.
/// For non-Reynolds kernels, just returns `exprToCpp nameMap kernelExpr`.
/// For Reynolds kernels, generates the sum over all permutations of the kernel parameters,
/// deduplicating structurally equivalent permutations via canonical keys.

let genKernelExprWithReynolds
    (kernelExpr: IRExpr)
    (kernelParams: IRParam list)
    (hasReynolds: bool)
    (isAntisymmetric: bool)
    (nameMap: Map<int, string>)
    (paramFinalNames: Map<int, string>) : ReynoldsResult =
    if hasReynolds && kernelParams.Length >= 2 then
        let n = kernelParams.Length
        let paramCppNames =
            kernelParams |> List.map (fun p ->
                Map.tryFind p.VarId paramFinalNames
                |> Option.defaultValue ($"__p{p.VarId}"))
        // Name map for a permutation: each kernel param's VarId maps to the
        // C++ name of the parameter it is permuted to (layered over nameMap).
        let permNameMap (perm: int list) =
            kernelParams |> List.mapi (fun i p ->
                (p.VarId, paramCppNames.[perm.[i]]))
            |> List.fold (fun acc (vid, name) -> Map.add vid name acc) nameMap
        // Enumerate + dedup the permutation terms (canonical key normalizes
        // commutative ops). The plan is rendering-independent, so a future IR
        // interpreter can reuse the exact enumeration/dedup/ordering.
        let plan = reynoldsTermPlan n isAntisymmetric (fun perm -> canonicalKey (permNameMap perm) kernelExpr)
        let totalPerms = plan.TotalPerms
        let uniqueTerms = plan.Terms.Length
        // Build the sum expression with multiplicity coefficients
        let formatTerm coeff expr =
            match isAntisymmetric with
            | true ->
                if abs coeff = 1 then
                    if coeff > 0 then expr else $"(-{expr})"
                else $"({coeff} * {expr})"
            | false ->
                if coeff = 1 then expr else $"({coeff} * {expr})"
        let sumExpr =
            plan.Terms |> List.mapi (fun i (coeff, perm) ->
                let expr = exprToCpp (permNameMap perm) kernelExpr
                let term = formatTerm coeff expr
                if i = 0 then term
                elif isAntisymmetric && coeff < 0 then
                    $" - {(formatTerm (abs coeff) expr)}"
                else $" + {term}")
            |> String.concat ""
        let cppExpr =
            if plan.Terms.IsEmpty then
                "0.0"  // Complete cancellation (e.g. antisymmetrization of symmetric kernel)
            else
                $"({sumExpr})"
        { CppExpr = cppExpr; TotalPerms = totalPerms; UniqueTerms = uniqueTerms }
    else
        { CppExpr = exprToCpp nameMap kernelExpr; TotalPerms = 1; UniqueTerms = 1 }

/// Compact output subscript for a compound-output loop nest. The present-cell
/// axis (the IRCompoundMask binding) contributes r*trailing_stride; a trailing
/// binding contributes the dense within-cell offset. Mirrors the read-side
/// addressing in genElementBindingNew (data + r*stride, then [t]). Supports <= 1
/// trailing dim (the realistic load_compound shape); multi-trailing needs a
/// strided sum and is deferred. All-dims (no trailing) -> .data[r].
let compoundOutputSubscript (bindings: LoopIndexBinding list) (outName: string) : string =
    let isComp (b: LoopIndexBinding) = match b.Extent with IRCompoundMask _ | IRSparseKeys _ -> true | _ -> false
    match bindings |> List.tryFind isComp with
    | None -> ""
    | Some cb ->
        match bindings |> List.filter (isComp >> not) with
        | [] -> $".data[{cb.IndexName}]"
        | [tb] -> $".data[{cb.IndexName} * {outName}.trailing_stride + {tb.IndexName}]"
        | tbs -> $""".data[{cb.IndexName} * {outName}.trailing_stride + {(tbs |> List.map (fun b -> b.IndexName) |> String.concat " + ")}]"""

/// --- Dense-halo carousel (sliding-window reuse) -----------------------------
/// For the INNERMOST loop level whose sole element is a dense halo window, the
/// body's simple window reads `A(w(k))` are hoisted into a span-sized set of
/// rotating scalar locals: warm-up loads before the innermost header, then one
/// shift + ONE new load at the loop tail -- instead of one load per read per
/// iteration. Ordinal contiguity makes this sound: stepping the center by one
/// evicts exactly the oldest ordinal and admits exactly one new one.
/// The transform is a pure rendering substitution (reference-keyed SubstMap):
/// values are bit-identical, and the reuse structure becomes explicit in the
/// emitted C++ -- the seam that pays off for expensive sources (hashed/sparse
/// maps, streamed windows, fused producers) where a re-read is not a cache hit.
/// Bails (None) whenever rotation could be unsound or names unresolvable:
/// Reynolds perm-rendering, any parallel level (omp collapse forbids code
/// between headers, and a split iteration space breaks rotation), MPI slab,
/// streamed sources, dynamic start offsets, spans > 8, or reads whose array /
/// prefix indices reference anything but captures, outer scope, or virtual
/// (range/window) params.
let internal planHaloCarousel
    (streamed: Map<string, ProviderReadSpec>)
    (codeGen: LoopNestCodeGen)
    (outerNames: Map<int, string>) : (SubstMap * string list * string list) option =
    if codeGen.HasReynolds || codeGen.MpiSlab || not streamed.IsEmpty
       || codeGen.Bindings.IsEmpty
       || (codeGen.Bindings |> List.exists (_.IsParallel)) then None
    else
    let inner = List.last codeGen.Bindings
    match inner.Elements with
    | [elem] when (match elem.SlotTag with
                   | Some t -> t.StartsWith "__halowin|d:"
                   | None -> false) ->
        // Center start offset (the warm-up's first center is `start`, since
        // the shrunk loop begins at 0). Dynamic starts bail.
        let startOpt =
            match elem.Virtual with
            | VirtualRange None -> Some 0L
            | VirtualRange (Some (IRLit (IRLitInt s))) -> Some s
            | _ -> None
        match startOpt with
        | None -> None
        | Some start ->
            let wid = elem.ParamVarId
            let wname = elem.ParamName
            // Names resolvable BEFORE emission: outer scope, captures, and
            // every level's virtual params (range windows / ordinals). Real
            // arrays' peeled names are emission-internal -- reads touching
            // them bail per group.
            let prefixMap =
                let fromElems =
                    codeGen.Bindings
                    |> List.collect (_.Elements)
                    |> List.choose (fun e ->
                        match e.Virtual with
                        | VirtualRange _ | VirtualReverse -> Some (e.ParamVarId, e.ParamName)
                        | RealArray -> None)
                // Captures fill gaps only -- see the note at the kernel-body
                // nameMap below: `c.Name` is the source spelling and loses to
                // whatever the enclosing scope actually emitted.
                let m0 =
                    codeGen.Captures
                    |> List.fold (fun acc c -> if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc) outerNames
                fromElems |> List.fold (fun acc (k, v) -> Map.add k v acc) m0
            let rec varIdsOf (e: IRExpr) : Set<int> =
                let self = match e with IRVar (id, _) -> Set.singleton id | _ -> Set.empty
                childrenOf e |> List.fold (fun acc c -> Set.union acc (varIdsOf c)) self
            // Window reads by NODE REFERENCE (the SubstMap contract), from
            // the one shared scan (IRAccess.windowReadsOf): this ring serves
            // reads on the LAST axis with a static offset; anything else
            // simply stays a direct read.
            let found : (IRExpr * int * IRExpr list * int) list =   // node, arrId, prefix, k
                Blade.IRAccess.windowReadsOf (function IRVar (vid, _) -> vid = wid | _ -> false) codeGen.KernelExpr
                |> List.choose (fun r ->
                    match r.Offset with
                    | Some k when r.Dim = r.Rank - 1 -> Some (r.Node, r.ArrayId, r.Prefix, k)
                    | _ -> None)
            // Groups: same array + identically-rendered prefix (outer-window
            // reads etc. -- invariant across the innermost run by the wid check).
            let renderable (aid: int) (prefix: IRExpr list) =
                Map.containsKey aid prefixMap
                && (prefix |> List.forall (fun p ->
                        let vs = varIdsOf p
                        not (Set.contains wid vs)
                        && vs |> Set.forall (fun v -> Map.containsKey v prefixMap)))
            let groups =
                found
                |> List.filter (fun (_, aid, prefix, _) -> renderable aid prefix)
                |> List.groupBy (fun (_, aid, prefix, _) ->
                    (aid, prefix |> List.map (exprToCppCore emptySubst prefixMap) |> String.concat "|"))
                |> List.filter (fun (_, reads) ->
                    let ks = reads |> List.map (fun (_, _, _, k) -> k) |> List.distinct
                    ks.Length >= 2 && (List.max ks - List.min ks + 1) <= 8)
            if groups.IsEmpty then None
            else
                // Ring buffer, head = the loop index itself. The window's
                // values stay STATIONARY in a pow2-capacity buffer; the loop
                // index (which already increments once per pass) locates the
                // logical start, so each iteration performs exactly ONE write
                // -- the new value drops into the slot the departing value
                // vacated ((i + span) & mask) -- and zero data movement.
                // Reads are buf[(i + slot) & mask]; the pow2 pad makes the
                // mod a mask (pad entries are seeded but never read live).
                let idxName = inner.IndexName
                let mutable subst : SubstMap = []
                let mutable warmup : string list = []
                let mutable tail : string list = []
                groups |> List.iteri (fun g ((aid, _), reads) ->
                    let arrS = Map.find aid prefixMap
                    let (_, _, prefix, _) = List.head reads
                    let prefixS =
                        prefix |> List.map (exprToCppCore emptySubst prefixMap >> sprintf "[%s]") |> String.concat ""
                    let ks = reads |> List.map (fun (_, _, _, k) -> k)
                    let mink = List.min ks
                    let maxk = List.max ks
                    let span = maxk - mink + 1
                    let cap = let mutable c = 1 in (while c < span do c <- c * 2); c
                    let mask = cap - 1
                    // Uniquified per nest via the output name: several halo
                    // nests can share one C++ scope (sequential lets in main).
                    let buf = $"__car_{(sanitizeCppName codeGen.OutputName)}_{g}"
                    // size_t casts: the Array wrapper's operator[] takes size_t
                    // and the wrapper also converts to a raw pointer, so an
                    // int64 subscript is ambiguous -- exact-match it instead.
                    let loadAt (ord: int64) = $"{arrS}{prefixS}[(size_t){ord}L]"
                    let inits =
                        [ for j in 0 .. span - 1 -> loadAt (start + int64 mink + int64 j) ]
                        @ List.replicate (cap - span) (loadAt (start + int64 mink + int64 (span - 1)))
                    warmup <- warmup @
                        [ $"// halo carousel: {arrS} window [{mink}..{maxk}] -- ring of {cap}, head = {idxName}, one write/step"
                          $"""std::array {buf}{{ {(String.concat ", " inits)} }};""" ]
                    // The tail prefetches the value the NEXT step will read at
                    // its far edge. On the last step there is no next step, and
                    // the ordinal is one past the array (the interior shrink
                    // ends the walk exactly at reach): guard the load. The slot
                    // it would fill is never read again, so skipping it is exact
                    // (docs/plans/structural/02, section 1.5).
                    tail <- tail @
                        [ $"if ((size_t)({wname} + {1 + maxk}L) < {arrS}.extents[{prefix.Length}]) {buf}[({idxName} + {span}UL) & {mask}UL] = {arrS}{prefixS}[(size_t)({wname} + {1 + maxk}L)];" ]
                    for (node, _, _, k) in reads do
                        subst <- (node, $"{buf}[({idxName} + {k - mink}UL) & {mask}UL]") :: subst)
                Some (subst, warmup, tail)
    | _ -> None

let genLoopNestStreamed (streamed: Map<string, ProviderReadSpec>) (codeGen: LoopNestCodeGen) (outerNames: Map<int, string>) (indent: int) : string list =
    let ind n = String.replicate n "    "
    let mutable lines = []
    let mutable depth = indent

    // Track current peeled name for each array position
    let mutable currentNames : Map<int, string> =
        codeGen.InputArrayNames |> List.mapi (fun i n -> (i, n)) |> Map.ofList

    // Track final peeled name for each param VarId (for kernel body substitution)
    let mutable paramFinalNames : Map<int, string> = Map.empty

    // Streamed sources: accumulated ABSOLUTE site coordinates per array
    // position (consumed by the fiber read at the S/T boundary).
    let mutable streamSites : Map<int, string list> = Map.empty
    
    // Generate nested loops with element bindings
    // Nest-level OpenMP pragma (collapse for rectangular, dynamic for triangular)
    // is prepended only at the outermost level.
    //
    // OpenMP thread-coverage instrumentation (test mode only): records the set
    // of distinct OpenMP threads that actually executed the outer parallel
    // region, and prints the count afterward. This empirically answers "did the
    // runtime distribute this generated loop across multiple threads?" -- the
    // ground-truth question, not a heuristic on pragma text. Race-free: each
    // thread writes ONLY its own slot in __omp_seen[], so no two threads touch
    // the same address. Gated behind ompTestModeEnabled() so user codegen is
    // never polluted.
    let ompInstrument = ompTestModeEnabled ()
    // Which level carries the nest's pragma (None = serial). Computed once: the
    // licence is a property of the nest, not of the level being emitted. Also
    // gates the coverage instrumentation -- which must follow the PRAGMA, not
    // level 0, since `omp(a: n)` can licence an inner level and leave the
    // outermost serial. Gating on the head binding would then instrument a nest
    // whose parallel region lives further in, and report it as never-threaded.
    //
    // The BUILD KNOB (`ompThreadEmissionEnabled`) forces this to None, which is
    // what makes a serial-emission build emit EXACTLY the unlicensed nest: no
    // pragma, `ompLastLevel = -1` so `BLADE_IVDEP` lands where it would have
    // without the clause, and no coverage instrumentation (there is no team to
    // observe). The dropped clause is reported by the marker below.
    let pragmaLevel =
        if codeGen.FoldWrapper.IsSome || not (ompThreadEmissionEnabled ()) then None
        else pragmaLevelOf codeGen.Bindings
    // "This nest runs inside a parallel team", which gates the thread-coverage
    // instrumentation. A Path B fold nest qualifies without carrying a `for`
    // pragma on any level: its team is the explicit region opened below, and the
    // per-thread markers land in the innermost body exactly as they do for a
    // `parallel for` nest -- which is what lets `blade test omp-coverage` answer
    // "is the reduce's parallel region genuine" with ground truth instead of
    // pragma text.
    // (`ompThreadEmissionEnabled` guards the FoldChunk half for the same reason
    // it guards `pragmaLevel`: with the knob off no team is opened anywhere in
    // this nest, so there is nothing for the coverage instrumentation to see.)
    let outerIsParallel =
        pragmaLevel.IsSome || (codeGen.FoldChunk.IsSome && ompThreadEmissionEnabled ())
    // Path B: a comm-licensed fold nest. `pragmaLevel` stays None on
    // purpose -- a `parallel for` over the outer level would race on the shared
    // accumulator, which is exactly why folds were blanket-suppressed. What
    // replaces it is an explicit team with PRIVATE accumulators and a
    // fixed-order combine, emitted around the nest below. Names are tagged with
    // the fold binding so several folds can share one C++ scope.
    // The BUILD KNOB drops the chunk plan HERE, at the consumer, and nowhere
    // else: the region below is an explicit `#pragma omp parallel` calling
    // `omp_get_max_threads`/`omp_get_num_threads`, i.e. exactly the thread
    // machinery a serial-emission build must contain none of, and this is the
    // only place it is spent. The producer (`genReduceComputeBinding`'s
    // `chunkable`) deliberately still SETS the plan, because `FoldChunk.IsSome`
    // is read elsewhere as a FACT about the fold -- see the note there.
    // Without the plan the nest is the ordinary serial fold.
    let foldChunk = if ompThreadEmissionEnabled () then codeGen.FoldChunk else None
    let fcTag = match foldChunk with Some p -> p.Tag | None -> ""
    let fcRn = $"__rn_{fcTag}"
    let fcT = $"__rT_{fcTag}"
    let fcPart = $"__rpart_{fcTag}"
    let fcHad = $"__rhad_{fcTag}"
    let fcAcc = $"__racc_{fcTag}"
    let fcHas = $"__rhas_{fcTag}"
    // The accumulator the nest body writes: the thread-private one under Path B,
    // the caller-declared shared scalar otherwise.
    let foldAccName = if foldChunk.IsSome then fcAcc else codeGen.OutputName
    // Unique region tag derived from the (unique) output name.
    let regionTag = codeGen.OutputName
    if ompInstrument && outerIsParallel then
        lines <- lines @ [
            ind depth + "// [omp-coverage] thread observation (test mode)"
            ind depth + "int __omp_maxth = omp_get_max_threads();"
            ind depth + "bool* __omp_seen = new bool[__omp_maxth]();"
            ind depth + "int* __omp_team = new int[__omp_maxth]();"
        ]

    let mutable atOuterLevel = true
    let lastBindingIdx = (List.length codeGen.Bindings) - 1
    let mutable bidx = 0
    let compoundArrays = compoundArrayNamesOf codeGen.Bindings
    // Path B prologue: hoist the outermost extent, size the team, and
    // open the explicit parallel region. Three scopes open here (block / non-
    // empty guard / parallel region) and are closed by the epilogue after the
    // nest, so `depth` advances by 3 before the first loop header.
    match foldChunk with
    | Some plan ->
        let outerBound = genLoopBoundExpr compoundArrays (List.head codeGen.Bindings)
        (ompApiUsedCell ()).Value <- true
        lines <- lines @ [
            ind depth + "// reduce over computation: comm-licensed parallel fold, outer level chunked"
            ind depth + "{"
            ind (depth + 1) + $"const size_t {fcRn} = {outerBound};"
            ind (depth + 1) + $"int {fcT} = omp_get_max_threads();"
            ind (depth + 1) + $"if ((size_t){fcT} > {fcRn}) {fcT} = (int){fcRn};"
            ind (depth + 1) + $"if ({fcT} < 1) {fcT} = 1;"
            ind (depth + 1) + $$"""if ({{fcRn}} > 0) {"""
            ind (depth + 2) + $"{plan.ElemCpp}* {fcPart} = new {plan.ElemCpp}[{fcT}];"
            // Zero-initialized: it marks which slots a thread actually wrote, so
            // the combine is correct for ANY team size the runtime hands back
            // (num_threads is a request -- see the flat path's note) and skips a
            // chunk whose inner nest contributed nothing.
            ind (depth + 2) + $"bool* {fcHad} = new bool[{fcT}]();"
            ind (depth + 2) + $"#pragma omp parallel num_threads({fcT})"
            ind (depth + 2) + "{"
            ind (depth + 3) + "const int __rnt = omp_get_num_threads();"
            ind (depth + 3) + "const int __rt = omp_get_thread_num();"
            ind (depth + 3) + $"const size_t __rlo = ({fcRn} * (size_t)__rt) / (size_t)__rnt;"
            ind (depth + 3) + $"const size_t __rhi = ({fcRn} * ((size_t)__rt + 1)) / (size_t)__rnt;"
            // Value-initialized, never READ before __rhas turns true; the
            // initializer only keeps -Wmaybe-uninitialized quiet.
            ind (depth + 3) + $"{plan.ElemCpp} {fcAcc} = {plan.ElemCpp}();"
            ind (depth + 3) + $"bool {fcHas} = false;"
        ]
        depth <- depth + 3
    | None -> ()
    // Dense-halo carousel plan (None when inapplicable): warm-up lines are
    // injected just BEFORE the innermost header, the rotation at the loop
    // tail, and the body renders through the reference-keyed SubstMap.
    let carousel = planHaloCarousel streamed codeGen outerNames
    // Which rank-1 input peels may drop the Array<T,1> wrapper for a
    // raw `BLADE_RESTRICT` row pointer (see restrictPeelSites for the proof
    // obligation). Streamed positions are handled by genElementBindingStreamed
    // and never consult this set -- their fiber bind IS an Array<T,1> value.
    let restrictSites = restrictPeelSites codeGen.Bindings
    // Hoist the innermost OUTPUT row as a raw `BLADE_RESTRICT` pointer
    // so the write target is provably distinct from every read row. Gated to
    // the shape the nest builder guarantees to be a scalar cell write:
    // a plain (non-tabulated) dense/compact array output whose rank is exactly
    // the nest depth, so `out[__i0]..[__i(n-2)]` is a `T*` row and the
    // innermost subscript is the only one left. Fold nests (scalar
    // accumulator), scalar outputs and compound/sparse outputs (flat `.data`
    // subscript) are excluded; nothing else about the nest changes.
    let outRowName = $"__orow_{(sanitizeCppName codeGen.OutputName)}"
    let outRowDecl : string option =
        match codeGen.OutputType with
        | ArrayElem at when codeGen.FoldWrapper.IsNone
                            && not (isCompoundArrayType at)
                            && not (isSparseArrayType at)
                            && List.length codeGen.Bindings >= 2
                            && arrayRank at = List.length codeGen.Bindings ->
            let prefix =
                codeGen.Bindings
                |> List.take (List.length codeGen.Bindings - 1)
                |> List.map (fun b -> $"[{b.IndexName}]")
                |> String.concat ""
            Some ($"{(elemTypeToCpp at.ElemType)}* BLADE_RESTRICT {outRowName} = {codeGen.OutputName}{prefix};")
        | _ -> None
    // `BLADE_IVDEP` (`#pragma GCC ivdep` under g++, nothing under cl/clang --
    // see cpp/blade_portability.hpp) on the innermost header, after the
    // row-peel restrict qualifiers: g++ 15.2 drops a restrict qualifier on a
    // BLOCK-SCOPE LOCAL (it only feeds restrict into its points-to solver for
    // function PARAMETERS), so
    // `ivdep` is the assertion GCC actually acts on, removing the runtime
    // alias check and scalar fallback from the vectorized loop.
    //
    // `ivdep` claims NO loop-carried dependence at all; every gate clause is load-bearing:
    //  1. `outRowDecl.IsSome` -- exactly one scalar cell written per iteration
    //     at the innermost loop variable (excludes fold/`+=`/compound writes).
    //  2. `carousel.IsNone` -- the halo carousel's ring buffer IS a real
    //     loop-carried dependence (written at the tail, read next iteration).
    //  3. no omp coverage instrumentation -- its `__omp_seen` marker rewrites
    //     the same slot every iteration (a WAW chain); test-mode only.
    //  4. the kernel body contains no loop of its own. This clause is not about
    //     SOUNDNESS like the three above -- it is about the pragma being
    //     MEANINGFUL. A `prodsum` / `reduce` / `contains` body, or a nested
    //     combinator application, lowers to an IIFE with a `for` inside it, and
    //     the nest machinery cannot see that loop (it counts nest LEVELS). The
    //     emitted `ivdep` then sat on a loop whose body is a loop, which no
    //     compiler can vectorize -- inert text that reads, to anyone inspecting
    //     the emission, as though the vectorization had already happened. That
    //     misreading is exactly what a performance audit of these emitters
    //     reported. Declining costs nothing (the loop never vectorized) and the
    //     marker below says why, in the generated C++, where someone looking at
    //     this actually looks.
    //
    // Reads are unconstrained (inputs are distinct pools from the fresh
    // output, EmitCpp.fs:48-55). Fused nests are out of scope: every leaf
    // writes its own row, so clause 1 fails by construction there.
    let bodyHasInnerLoop = kernelBodyContainsInnerLoop codeGen.KernelExpr
    let ivdepEligible =
        outRowDecl.IsSome
        && carousel.IsNone
        && not (ompInstrument && outerIsParallel)
        && not bodyHasInnerLoop
    // Last nest level that belongs to the OpenMP construct. A `collapse(d)`
    // prefix FUSES its levels into one iteration space, and g++ then rejects a
    // pragma on any inner header of that prefix outright:
    //   error: loop not permitted in intervening code in OpenMP loop body
    //   error: not enough nested loops
    // (element peels between the headers are fine -- a PRAGMA is not.) The depth
    // is read back off the pragma the nest will actually emit rather than
    // re-derived, so this cannot drift from genNestPragma's collapse rule.
    // Levels strictly below this index are ordinary nested loops and may carry
    // ivdep; a `schedule(dynamic)` outer with a triangular inner collapses
    // nothing, so its inner levels stay eligible.
    let ompLastLevel =
        match pragmaLevel with
        | None -> -1
        | Some pl ->
            let txt = genNestPragma (List.skip pl codeGen.Bindings) ""
            let m = System.Text.RegularExpressions.Regex.Match(txt, @"collapse\((\d+)\)")
            if m.Success then pl + int m.Groups.[1].Value - 1 else pl
    // CANONICAL COMPACT FILL: write the flat pool through the closed-form
    // offsets instead of walking the Iliffe skeleton (see compactFlatWritePlan
    // for the math and the gate; plan-simplex-blocked-compute.md section 0b
    // finding 2 for the measurement). Declined outright when the nest carries a
    // MULTI-LEVEL `collapse` -- the per-level hoists are statements BETWEEN loop
    // headers, which g++ rejects inside a collapsed prefix ("loop not permitted
    // in intervening code in OpenMP loop body"). No compact nest can reach that
    // case today (level 1 of a simplex is triangular, so genNestPragma's
    // collapse prefix stops at 1 and the nest gets `schedule(dynamic)` on level
    // 0 alone), which is exactly why the guard is cheap to keep honest.
    let compactFlat =
        if outRowDecl.IsNone || ompLastLevel > (defaultArg pragmaLevel 0) then None
        else compactFlatWritePlan codeGen outRowName
    match compactFlat with
    | Some plan -> lines <- lines @ [ind depth + plan.PoolDecl]
    | None -> ()
    for binding in codeGen.Bindings do
        let levelIdx = bidx
        // Generate the loop header (pragma only on the outermost loop).
        // Fused-fold nests accumulate into shared scalars -- not race-safe
        // under a parallel-for -- so the pragma is suppressed entirely
        // (an omp `reduction(...)` clause is the future upgrade path).
        let isOuter = atOuterLevel
        // The pragma goes on the outermost LICENSED level, which `omp(a: n)`
        // does not force to be level 0 (see pragmaLevelOf). genNestPragma is
        // handed the suffix starting there, so its collapse/dynamic reasoning
        // runs over exactly the levels the pragma governs.
        let pragmaPrefix =
            if codeGen.FoldWrapper.IsNone && pragmaLevel = Some bidx
            then genNestPragma (List.skip bidx codeGen.Bindings) (ind depth) else ""
        // Mark a requested-but-suppressed pragma at the outer level so the
        // dropped clause is visible rather than silent (see ompSuppressedMarker).
        let suppressedMarker =
            if isOuter then
                let reason =
                    // The BUILD KNOB is reported FIRST when it is what declined:
                    // with serial emission on, every other reason is unreachable
                    // as an explanation (the nest would have been serial anyway),
                    // and naming the knob is what tells a reader that the same
                    // source built without it WOULD be threaded.
                    if not (ompThreadEmissionEnabled ()) then ompThreadsSuppressedReason ()
                    elif codeGen.FoldWrapper.IsSome then
                        "fold accumulates into a shared scalar, which is not race-safe"
                    else "the omp(...) depth licenses no level of this nest"
                // "Emitted" means the nest gets a pragma SOMEWHERE, not
                // necessarily at this level -- an inner-licensed nest is
                // parallelized and must not be reported as serial. A Path B
                // fold nest counts as emitted: the parallel region is the
                // prologue above, not a `for` pragma on any level.
                ompSuppressedMarker codeGen.OmpRequested
                                    (pragmaLevel.IsSome || foldChunk.IsSome) reason (ind depth)
                // A Path B fold nest requested omp through `FoldChunk`, not
                // through `OmpRequested` (which the fold path leaves unset), so
                // a knob-suppressed chunk plan would otherwise vanish without a
                // trace. This is the census line for exactly that case.
                @ (if codeGen.FoldChunk.IsSome && not (ompThreadEmissionEnabled ())
                   then [ ind depth + $"// [omp] requested but emitted serial: {(ompThreadsSuppressedReason ())}" ]
                   else [])
            else []
        atOuterLevel <- false
        // MPI slab mode: the outermost level iterates this rank's slab
        // [__blade_mpi_lo_<out>, __blade_mpi_hi_<out>) -- bounds declared by
        // the slab prologue genApplyCombinator emitted before the nest.
        // Inner levels are untouched.
        let header =
            if isOuter && codeGen.MpiSlab then
                sprintf "for (size_t %s = __blade_mpi_lo_%s; %s < __blade_mpi_hi_%s; %s++) {"
                    binding.IndexName codeGen.OutputName
                    binding.IndexName codeGen.OutputName
                    binding.IndexName
            // Path B: the outermost level iterates this thread's chunk
            // [__rlo, __rhi) of [0, extent). Same substitution shape as the MPI
            // slab above; inner levels are untouched, so a triangular inner nest
            // runs exactly as it would serially for each outer index owned here.
            elif isOuter && foldChunk.IsSome then
                $"for (size_t {binding.IndexName} = __rlo; {binding.IndexName} < __rhi; {binding.IndexName}++) {{"
            else genForLoopHeader compoundArrays binding
        // Carousel warm-up: seed the rotating window locals for the first
        // center, in the scope just outside the innermost loop (re-seeded
        // per outer iteration in multi-level nests).
        if bidx = lastBindingIdx then
            match carousel with
            | Some (_, warmupLines, _) ->
                for w in warmupLines do lines <- lines @ [ind depth + w]
            | None -> ()
        // Output row hoist: same scope as the carousel warm-up (just
        // outside the innermost header, re-taken per outer iteration). The
        // flat-pool plan replaces the skeleton walk with `pool + <closed-form
        // offset>`; everything downstream (the `__orow[__i(r-1)]` store, the
        // restrict qualifier, BLADE_IVDEP) is unchanged, because the innermost
        // level's own term IS its loop counter at every rank.
        if bidx = lastBindingIdx then
            match compactFlat, outRowDecl with
            | Some plan, _ -> lines <- lines @ [ind depth + plan.RowDecl]
            | None, Some d -> lines <- lines @ [ind depth + d]
            | None, None -> ()
        // `BLADE_IVDEP` must be the LAST thing before the `for`, and only
        // on a header the OpenMP construct does not own (see ompLastLevel).
        // It is the portable spelling -- cpp/blade_portability.hpp expands it
        // to `_Pragma("GCC ivdep")` under g++ and to nothing elsewhere (cl.exe
        // warns C4068 on it and clang does not implement it). `_Pragma` and not
        // `#pragma` because a `#pragma` line cannot come out of a macro, and
        // the emission site cannot `#ifdef` it: nothing may sit between the
        // annotation and its `for`.
        let ivdepPrefix =
            if ivdepEligible && bidx = lastBindingIdx && bidx > ompLastLevel
            then $"BLADE_IVDEP\n{(ind depth)}" else ""
        // Say IN THE GENERATED C++ that the vectorization annotation was
        // declined and why -- same rationale as `ompSuppressedMarker`, and the
        // same one-comment-line cost. This case only: the three SOUNDNESS
        // clauses are self-evident from the emitted code around them (a fold's
        // `+=`, the carousel's ring buffer, the `__omp_seen` write are all
        // visible), whereas "the body hides a loop" is precisely the fact the
        // emission cannot show, which is why the inert pragma read as a real one.
        let ivdepSuppressedMarker =
            if bodyHasInnerLoop && outRowDecl.IsSome && carousel.IsNone
               && not (ompInstrument && outerIsParallel)
               && bidx = lastBindingIdx && bidx > ompLastLevel
            then [ ind depth + "// [ivdep] declined: kernel body contains an inner loop (prodsum/reduce/contains/nested apply), so this header cannot vectorize" ]
            else []
        lines <- lines @ suppressedMarker @ ivdepSuppressedMarker @ [ind depth + pragmaPrefix + ivdepPrefix + header]
        depth <- depth + 1
        // This level's share of the compact output's pool offset, hoisted here
        // because the term is invariant under every level inside it -- which is
        // what makes the addressing O(1) per cell instead of O(r). Levels
        // 0 .. r-2 only; the innermost term is its own loop counter and is spent
        // by the `__orow[...]` subscript.
        match compactFlat with
        | Some plan ->
            (match Map.tryFind levelIdx plan.Hoists with
             | Some hs -> lines <- lines @ (hs |> List.map (fun h -> ind depth + h))
             | None -> ())
        | None -> ()
        // Thread-coverage marker: record this thread as seen and the team size
        // it observes. Each thread writes ONLY its own slot (race-free). Team
        // size is captured per-slot (not a single guarded write) because
        // schedule(dynamic) does not guarantee any thread runs any iteration --
        // taking the max over slots afterward recovers the true team size.
        //
        // Placed inside the INNERMOST loop body (after ALL loop headers), not
        // the outer body: OpenMP `collapse(d)` requires the collapsed loops to
        // be perfectly nested with no intervening code (and OMP API calls are
        // explicitly forbidden between collapsed headers). Marking in the
        // innermost body is past any collapsed prefix and always legal. The
        // marker is idempotent (each thread re-sets its own slot to the same
        // values), so running it per innermost-iteration is harmless.
        if ompInstrument && outerIsParallel && bidx = lastBindingIdx then
            lines <- lines @ [
                ind depth + "{ int __tn = omp_get_thread_num(); __omp_seen[__tn] = true; __omp_team[__tn] = omp_get_num_threads(); }"
            ]
        bidx <- bidx + 1
        
        // Generate element bindings for all arrays at this level. Zipping an
        // array WITH ITSELF puts two operand slots on the same (array, index):
        // both peel to the byte-identical declaration, so an identical
        // (name, code) pair is emitted once (the second slot's params resolve
        // to the first slot's binding via paramFinalNames below); the second
        // `double A____i0 = A[__i0];` was a g++ redeclaration error.
        let mutable declaredNames : Map<string, string> = Map.empty
        for elem in binding.Elements do
            match Map.tryFind elem.ArrayName streamed with
            | Some sspec ->
                // Streamed source: no array to peel -- accumulate the site
                // coordinate; at the fiber level this emits the provider
                // read + the rank-1 wrapper the kernel body consumes.
                let acc = Map.tryFind elem.ArrayPosition streamSites |> Option.defaultValue []
                let (codeLines, fiberBound, acc') = genElementBindingStreamed binding elem sspec acc
                streamSites <- Map.add elem.ArrayPosition acc' streamSites
                for c in codeLines do
                    lines <- lines @ [ind depth + c]
                (match fiberBound with
                 | Some fname ->
                     currentNames <- Map.add elem.ArrayPosition fname currentNames
                     paramFinalNames <- Map.add elem.ParamVarId fname paramFinalNames
                 | None -> ())
            | None ->
                let currentName =
                    Map.tryFind elem.ArrayPosition currentNames
                    |> Option.defaultValue elem.ArrayName
                let rawRow = Set.contains (levelIdx, elem.ArrayPosition) restrictSites
                let (code, newName) = genElementBindingPeel rawRow binding elem currentName
                if Map.tryFind newName declaredNames <> Some code then
                    lines <- lines @ [ind depth + code]
                declaredNames <- Map.add newName code declaredNames
                currentNames <- Map.add elem.ArrayPosition newName currentNames
                // Record mapping for kernel body
                match elem.Virtual with
                | VirtualRange _ | VirtualReverse ->
                    paramFinalNames <- Map.add elem.ParamVarId elem.ParamName paramFinalNames
                | RealArray ->
                    paramFinalNames <- Map.add elem.ParamVarId newName paramFinalNames
    
    // Build name map for kernel body from final peeled names
    // Start from outer scope, then overlay kernel params (kernel params take priority)
    let nameMap = paramFinalNames |> Map.fold (fun acc k v -> Map.add k v acc) outerNames
    // Captures are a FALLBACK, not an override: `c.Name` is the SOURCE-level
    // identifier, which is wrong whenever the captured binding was renamed on
    // emission (a block-local `let a = ...` inside a function body flattens to
    // `auto __v4 = ...`). `outerNames`/`paramFinalNames` already carry the
    // emitted spelling, so only fill in ids the enclosing scope doesn't know --
    // same precedence rule as `captureForwardName`.
    let nameMap =
        codeGen.Captures
        |> List.fold (fun acc c -> if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc) nameMap
    
    // Generate kernel assignment (with Reynolds permutation sum if applicable).
    // The shape of the assignment depends on output type:
    //   - Array output: indexed slot assignment, `name[i][j]... = kernelBody;`.
    //     Each loop binding contributes one bracketed index. This is the standard
    //     case for `method_for(...) <@> kernel` returning a tensor.
    //   - Scalar output: sum accumulation, `name += kernelBody;`. The loop nest
    //     still iterates over input dimensions, but the kernel result is summed
    //     into a scalar accumulator declared by the caller (genApplyCombinator).
    //     Used when the function's return type is scalar even though the
    //     `<@>` kernel produces a per-iteration value (the Cartesian-sum-reduce
    //     pattern).
    let outputIdx =
        match codeGen.OutputType with
        | IRTScalar _ -> ""
        // Tabulated (compound/sparse) output inherits the input's index: write
        // into the compact buffer (.data[r*stride + t]), not a dense [i][j] slot.
        | ArrayElem at when isCompoundArrayType at || isSparseArrayType at ->
            compoundOutputSubscript codeGen.Bindings codeGen.OutputName
        | _ ->
            codeGen.Bindings
            |> List.map (fun b -> $"[{b.IndexName}]")
            |> String.concat ""
    let assignOp =
        match codeGen.OutputType with
        | IRTScalar _ -> "+="
        | _ -> "="

    // LAZY, because rendering is not free of side effects: `exprToCpp`'s
    // refusal path appends to the expression-sentinel cell (which becomes an
    // `#error` in the translation unit) whether or not the caller keeps the
    // string. The literal-row arm below discards this value, and an eagerly
    // rendered array-literal body left a spurious `#error` behind in an
    // otherwise perfectly good program.
    let reynoldsResult =
        lazy (
            match carousel with
            | Some (csubst, _, _) ->
                // Carousel body: same expression, window reads substituted to the
                // rotating locals (planHaloCarousel already excluded Reynolds).
                { CppExpr = exprToCppCore csubst nameMap codeGen.KernelExpr; TotalPerms = 1; UniqueTerms = 1 }
            | None ->
                genKernelExprWithReynolds codeGen.KernelExpr codeGen.KernelParams codeGen.HasReynolds codeGen.IsAntisymmetric nameMap paramFinalNames)
    if codeGen.HasReynolds && reynoldsResult.Value.UniqueTerms < reynoldsResult.Value.TotalPerms then
        lines <- lines @ [ind depth + $"// Reynolds: {reynoldsResult.Value.UniqueTerms}/{reynoldsResult.Value.TotalPerms} perms unique (dedup {reynoldsResult.Value.TotalPerms / max 1 reynoldsResult.Value.UniqueTerms}x)"]
    // When the output row was hoisted, the write goes through the
    // restrict-qualified row pointer and only the innermost subscript remains.
    let (writeTarget, writeIdx) =
        match outRowDecl with
        | Some _ -> (outRowName, $"[{(List.last codeGen.Bindings).IndexName}]")
        | None -> (codeGen.OutputName, outputIdx)
    // ARRAY-VALUED KERNEL RETURN (stage S3, manifestation M-C1). When the
    // kernel returns an array, the output type carries the kernel's
    // T-dimensions after the iterated ones, so `arrayRank > nest depth` and the
    // per-iteration product is a whole ROW, not a cell. `out[i] = <call>` (what
    // the pre-S3 emitter wrote) assigns an `Array<T,m>` wrapper into a slot the
    // C++ side types as a scalar or a row pointer -- the shape that compiled,
    // ran, and printed `[[], []]`.
    //
    // Instead: evaluate the row once, copy its pool into the output pool at the
    // row's offset, and free it when the callee owned it. The destination is
    // computed from `<name>_extents` -- the SAME table the allocation used, now
    // carrying the trailing T-dims -- rather than from a chained `out[i][j]`
    // subscript, so it is one formula for any inner rank and needs nothing from
    // the wrapper's operator[] typing.
    //
    // Gated to DENSE rectangular outputs: the flat row-major offset below is
    // exact only when no axis is packed. A symmetric/compound/sparse output
    // with an array-valued kernel falls through to the old cell assignment,
    // which the C++ compiler then rejects -- loud, and out of scope here.
    //
    // FREEING is conditional on `freshReturnOf`. A kernel that CONSTRUCTS its
    // row (the S2 call form over a materializing body) hands back storage this
    // call allocated, so not freeing it leaks one row per outer cell. A kernel
    // that PASSES ITS INPUT ROW THROUGH (`lambda(r) -> r`) hands back a view of
    // the operand -- freeing that would destroy the input. `NotFresh` (which is
    // also what an unresolvable callee reads as) leaks rather than guesses,
    // matching the surrounding block's rule 3.
    let rowWriteLines : string list option =
        match codeGen.OutputType with
        | ArrayElem at when codeGen.FoldWrapper.IsNone
                            && not (isCompoundArrayType at)
                            && not (isSparseArrayType at)
                            && not (hasRealSymmetry codeGen.OutputSymmVec)
                            && not (List.isEmpty codeGen.Bindings)
                            && arrayRank at > List.length codeGen.Bindings ->
            let rank = arrayRank at
            let outer = List.length codeGen.Bindings
            let extentsName = $"{codeGen.OutputName}_extents"
            let elemStr = elemTypeToCpp at.ElemType
            let innerCells =
                [ outer .. rank - 1 ] |> List.map (sprintf "%s[%d]" extentsName) |> String.concat " * "
            // Row-major flattening of the OUTER indices (inner indices are 0):
            // (((i0) * e1 + i1) * e2 + i2) ...
            let flatOuter =
                codeGen.Bindings
                |> List.mapi (fun i b -> (i, b.IndexName))
                |> List.fold (fun acc (i, nm) ->
                        if i = 0 then nm
                        else $"({acc}) * {extentsName}[{i}] + {nm}") ""
            let freeLine =
                let callee = match codeGen.KernelExpr with IRApp (f, _, _) -> Some f | _ -> None
                match callee with
                | Some f when freshReturnOf f = FreshPool ->
                    [ $"    deallocate<typename promote<{elemStr}, {(rank - outer)}>::type, nullptr>(__rowv.data, __rowv.extents);" ]
                | _ -> []
            // Literal row: store the cells directly, no temporary. Reynolds is
            // excluded -- symmetrizing a body means SUMMING its permuted
            // copies, and a per-cell sum of array literals is not what the
            // stores below write; that shape falls through to the generic arm.
            //
            // A BLOCK-bodied literal kernel -- scalar lets ahead of the array
            // literal (`lambda(w) -> { let x = ...; [x + y, x - y] }`, the
            // halo/segment shape) -- takes the same direct-store path: the
            // lets render as statements inside the row-write scope, visible
            // to the leaf stores. The shape check is PURE (no rendering), so
            // a declined shape leaves no sentinel side effect behind.
            let rec literalTailOf e = match e with IRLet (_, _, b) -> literalTailOf b | t -> t
            let literalLeaves =
                match literalTailOf codeGen.KernelExpr with
                | IRArrayLit _ as tail when not codeGen.HasReynolds ->
                    match arrayLitScalarLeaves tail,
                          staticCellsOfAxes at outer (rank - 1) with
                    // Leaf count MUST equal the destination row's cell count:
                    // the output extents are deduced from the literal's shape,
                    // so a disagreement means the deduction and the body have
                    // drifted apart, and writing anyway would run off the row.
                    | Some leaves, Some cells when leaves.Length = cells -> Some leaves
                    | _ -> None
                | _ -> None
            match literalLeaves with
            | Some leaves ->
                let subst = match carousel with Some (csubst, _, _) -> csubst | None -> emptySubst
                // Render the let prefix (empty for a bare literal body): each
                // non-unit value becomes a scoped local named __v<id> --
                // renderLetExpr's convention, so the leaves' IRVar references
                // resolve -- and unit values keep their statement form.
                let rec renderLetPrefix nm acc e =
                    match e with
                    | IRLet (lid, v, b) ->
                        let nm' = Map.add lid ($"__v{lid}") nm
                        let lines =
                            if isUnitExpr v then
                                match renderUnitStmts subst nm v with
                                | "" -> []
                                | s -> [$"    {s}"]
                            else [$"    auto __v{lid} = {(exprToCppCore subst nm v)};"]
                        renderLetPrefix nm' (acc @ lines) b
                    | _ -> (nm, acc)
                let (leafMap, letLines) = renderLetPrefix nameMap [] codeGen.KernelExpr
                Some ([
                        "{"
                      ] @ letLines @ [
                        $"    const size_t __rowc = (size_t)({innerCells});"
                        $"    {elemStr}* __rowd = nested_array_utilities::pool_base({codeGen.OutputName}.data) + (({flatOuter}) * __rowc);"
                      ] @ (leaves |> List.mapi (fun k leaf ->
                                $"    __rowd[{k}] = {(exprToCppCore subst leafMap leaf)};"))
                        @ [ "}" ])
            | None ->
            Some ([
                    "{"
                    $"    auto __rowv = {reynoldsResult.Value.CppExpr};"
                    $"    const {elemStr}* __rows = nested_array_utilities::pool_base(__rowv.data);"
                    $"    const size_t __rowc = (size_t)({innerCells});"
                    $"    {elemStr}* __rowd = nested_array_utilities::pool_base({codeGen.OutputName}.data) + (({flatOuter}) * __rowc);"
                    "    for (size_t __rk = 0; __rk < __rowc; __rk++) __rowd[__rk] = __rows[__rk];"
                  ] @ freeLine @ [ "}" ])
        | _ -> None
    // Lazy for `reynoldsResult`'s reason: `rowWriteLines`, when it fires, is
    // the whole write, and rendering this line anyway would re-enter the
    // kernel expression -- and its sentinel side effect -- for nothing.
    let assignLine = lazy (
        match codeGen.FoldWrapper, foldChunk with
        // Path B: accumulate into the THREAD-PRIVATE scalar, seeding it
        // from the chunk's first contributed value. The cell value is bound once
        // (the kernel expression can be a large Reynolds sum) and the branch is
        // loop-invariant after the first iteration.
        | Some wname, Some plan ->
            sprintf "{ %s __rv = %s; if (%s) %s = %s(%s, __rv); else { %s = __rv; %s = true; } }"
                plan.ElemCpp reynoldsResult.Value.CppExpr
                fcHas foldAccName wname foldAccName
                foldAccName fcHas
        // Fused fold: accumulate the kernel value through the fold-kernel
        // wrapper into the caller-declared scalar accumulator.
        | Some wname, None -> $"{codeGen.OutputName} = {wname}({codeGen.OutputName}, {reynoldsResult.Value.CppExpr});"
        | None, _ -> $"{writeTarget}{writeIdx} {assignOp} {reynoldsResult.Value.CppExpr};")
    match rowWriteLines with
    | Some rw -> for l in rw do lines <- lines @ [ind depth + l]
    | None -> lines <- lines @ [ind depth + assignLine.Value]
    // Carousel rotation: shift the window by one ordinal and load the single
    // new leading value for the next center.
    match carousel with
    | Some (_, _, tailLines) ->
        for t in tailLines do lines <- lines @ [ind depth + t]
    | None -> ()

    // Close all loops
    for _ in codeGen.Bindings do
        depth <- depth - 1
        lines <- lines @ [ind depth + "}"]

    // Path B epilogue: publish this thread's partial, close the region,
    // then combine in THREAD ORDER through the same wrapper -- a fixed order, so
    // a fixed OMP_NUM_THREADS reproduces bit-for-bit. Chunks that contributed
    // nothing (an outer index whose inner nest is empty) are skipped, and the
    // caller's seed -- already in the shared accumulator -- enters the fold first,
    // which is what makes this the serial left fold up to associativity.
    match foldChunk, codeGen.FoldWrapper with
    | Some _, Some wname ->
        lines <- lines @ [
            ind depth + $"{fcPart}[__rt] = {fcAcc};"
            ind depth + $"{fcHad}[__rt] = {fcHas};"
            ind (depth - 1) + "}"
            ind (depth - 1) + $"for (int __rt = 0; __rt < {fcT}; __rt++) if ({fcHad}[__rt]) {codeGen.OutputName} = {wname}({codeGen.OutputName}, {fcPart}[__rt]);"
            ind (depth - 1) + $"delete[] {fcPart}; delete[] {fcHad};"
            ind (depth - 2) + "}"
            ind (depth - 3) + "}"
        ]
        depth <- depth - 3
    | _ -> ()

    // [omp-coverage] after the nest: report the parallel team size and the
    // number of threads that actually did work. The harness uses:
    //   - teamsz > 1               => a genuine parallel region was created
    //   - maxth > 1 && teamsz == 1 => ERROR: pragma not honored (serial region)
    //   - maxth > 1 && teamsz > 1 && distinct == 1 => WARNING: region parallel
    //                                but scheduler put all work on one thread
    //                                (an allowed scheduler choice, not a bug)
    //   - maxth == 1               => single-core context, correctly serial
    if ompInstrument && outerIsParallel then
        lines <- lines @ [
            ind depth + "{ int __omp_distinct = 0; int __omp_teamsz = 0;"
            ind depth + "  for (int __t = 0; __t < __omp_maxth; __t++) {"
            ind depth + "    if (__omp_seen[__t]) __omp_distinct++;"
            ind depth + "    if (__omp_team[__t] > __omp_teamsz) __omp_teamsz = __omp_team[__t];"
            ind depth + "  }"
            ind depth + $"  std::cout << \"[omp-coverage] region={regionTag} teamsz=\" << __omp_teamsz << \" distinct=\" << __omp_distinct << \" maxth=\" << __omp_maxth << std::endl;"
            ind depth + "  delete[] __omp_seen; delete[] __omp_team; }"
        ]

    lines

/// The ordinary (no streamed sources) nest emitter -- every existing call
/// site goes through here; only genApplyCombinator's provider-aware paths
/// use genLoopNestStreamed directly.
let genLoopNest (codeGen: LoopNestCodeGen) (outerNames: Map<int, string>) (indent: int) : string list =
    genLoopNestStreamed Map.empty codeGen outerNames indent


// Flat-pool elementwise mode: when a materializing nest is INDEX-FREE
// ELEMENTWISE -- every operand read as a full-depth peel at the loop indices
// in order, nothing in the body mentions a loop index -- the nest collapses
// to ONE flat loop over the contiguous backing pools:
//
//     double* BLADE_RESTRICT __fp_out = pool_base(out.data);
//     const double* BLADE_RESTRICT __fp_A = pool_base(A.data);
//     BLADE_IVDEP                          // or: #pragma omp parallel for simd
//     for (size_t __fk = 0; __fk < N; __fk++)
//         __fp_out[__fk] = <body with A[coords] -> __fp_A[__fk]>;
//
// SOUNDNESS, three facts:
//  1. POOL CONTIGUITY: allocate<>/allocate_strict<> thread ONE offset through
//     ONE pool in DFS leaf order; ragged/gather pools are excluded (they fail
//     the literal-extent/IxKPlain gates and can never reach here).
//  2. VISIT ORDER == POOL ORDER: each level peels at the raw loop variable and
//     every bound equals the allocator's row length there, so the flat index
//     __fk is every operand's (and the output's) DFS pool offset alike.
//  3. PER-CELL EVALUATION IS UNCHANGED: the kernel expression is byte-identical;
//     only the leaf-read spelling changes (`__fp_A[__fk]` vs a per-iteration
//     local). Reads can't observe a write: the output pool is a fresh
//     allocation distinct from every input (EmitCpp.fs:48-55).
//
// The gate is deliberately CONSERVATIVE -- the nested fallback is always
// correct, so every uncertain shape returns None.

/// Binomial C(m, k) in int64, 0 when k < 0 or m < k. Local twin of
/// genPackedPoolCopy's -- same closed form, same overflow envelope (the caller
/// only ever feeds it literal corpus-scale extents).
let internal flatBinom (m: int64) (k: int) : int64 =
    if k < 0 || m < int64 k then 0L
    else
        let mutable num = 1L
        let mutable den = 1L
        for i in 0 .. k - 1 do
            num <- num * (m - int64 i)
            den <- den * int64 (i + 1)
        num / den

/// One storage GROUP of a flat-eligible array: (rank, symmetry class, literal
/// extent). Two arrays whose group lists are structurally equal have
/// byte-identical pool layouts, which is exactly the agreement the flat rewrite
/// needs (spec gate 3 + gate 4 -- extents are part of the signature, so
/// "provably equal extents" is decided here rather than assumed; Blade's unify
/// does not compare extents).
type internal FlatGroup = { GRank: int; GSym: SymmetryClass; GExtent: int64 }

/// Project an array type into a flat-eligible shape signature, or None.
///
/// Refused outright: virtual arrays; any non-plain index KIND (compound,
/// sparse, ragged, dep, group, irreps, orbit -- none of which is a plain
/// contiguous skeleton the loop bounds describe); `__halowin`-prefixed tags
/// (a halo window's reads are offset from the loop index, so the flat index is
/// NOT the operand's pool offset); dependent extents; non-literal extents (only a
/// compile-time constant cell count is emitted); Hermitian and wreath classes
/// (Hermitian shares symmetric STORAGE but its mirror conjugates, and the
/// wreath pool is the section 4 iterated-binomial fold, not this product) -- both are
/// simply future work, not unsoundness.
let internal flatShapeSignature (arr: IRArrayType) : FlatGroup list option =
    if arr.IsVirtual then None else
    let groups =
        arr.IndexTypes
        |> List.map (fun ix ->
            // `__halowin`, not a blanket `__`: every OTHER reserved tag
            // (__orbidx / __compoundidx / __seq / __group_* / __sparseidx /
            // __raggedidx*) carries a non-plain IxKind and is already refused by
            // the `IxKind <> IxKPlain` test beside this one. The blanket form
            // therefore refused exactly one thing on tag grounds alone: the
            // anonymous range's `__anon` (Lowering.fs, TypeCheckInfer.fs), which
            // IS a plain contiguous skeleton -- so `x0 + dx * Float64(0..n)`,
            // the documented top-rung idiom, missed the flat path that the
            // identical `Float64(range<I>)` took. Same predicate shape as the
            // lane-parallel gate in CodeGenBinding.fs.
            let tagOk = match ix.Tag with Some t -> not (t.StartsWith "__halowin") | None -> true
            if ix.IxKind <> IxKPlain || not tagOk || not ix.Dependencies.IsEmpty then None
            else
                match ix.Extent with
                | IRLit (IRLitInt n) when n > 0L ->
                    (match ix.Symmetry with
                     | SymNone when ix.Rank = 1 -> Some { GRank = 1; GSym = SymNone; GExtent = n }
                     | SymSymmetric | SymAntisymmetric when ix.Rank >= 2 ->
                         Some { GRank = ix.Rank; GSym = ix.Symmetry; GExtent = n }
                     | _ -> None)
                | _ -> None)
    if groups |> List.forall Option.isSome && not groups.IsEmpty
    then Some (groups |> List.map Option.get)
    else None

/// Number of scalars in the pool of an array with this signature = the number
/// of leaves allocate<> emits = the number of iterations the nest runs.
///   dense axis (rank 1)      -> n
///   symmetric group rank r   -> C(n + r - 1, r)   (inclusive simplex)
///   antisymmetric group r    -> C(n, r)           (STRICT simplex: rows are
///                                                  SHORTENED, no dead diagonal)
/// Groups multiply: each is a contiguous block of dims and the skeleton nests
/// them left to right.
let internal flatCellCount (sg: FlatGroup list) : int64 =
    sg |> List.fold (fun acc g ->
        let cells =
            match g.GSym with
            | SymNone -> g.GExtent
            | SymSymmetric -> flatBinom (g.GExtent + int64 g.GRank - 1L) g.GRank
            | SymAntisymmetric -> flatBinom g.GExtent g.GRank
            | _ -> 0L
        acc * cells) 1L

/// Does the built nest iterate EXACTLY the space this signature describes, one
/// leaf per innermost iteration, in DFS order? Compares every loop level's
/// extent, bound dependencies and strict offset against what the allocator's
/// recurrence implies for the group the level belongs to.
///
/// Dependency LISTS are compared as SETS: the co-iteration builder emits
/// `[base .. level-1]` ascending while the outer-product builder emits the
/// `iminMap` chain descending. Both render as the same bound subtraction (a
/// sum), so set equality is the right identity here.
let internal flatNestMatchesSignature (sg: FlatGroup list) (bindings: LoopIndexBinding list) : bool =
    let expected =
        sg
        |> List.fold (fun (baseDim, acc) g ->
            let triangular = (g.GSym = SymSymmetric || g.GSym = SymAntisymmetric)
            let levels =
                [ for k in 0 .. g.GRank - 1 ->
                    let deps = if triangular && k > 0 then Set.ofList [ baseDim .. baseDim + k - 1 ] else Set.empty
                    let strict = if g.GSym = SymAntisymmetric then k else 0
                    (g.GExtent, deps, strict) ]
            (baseDim + g.GRank, acc @ levels)) (0, [])
        |> snd
    List.length expected = List.length bindings
    && List.forall2 (fun (i, n, deps, strict) (b: LoopIndexBinding) ->
            b.FusedRank.IsNone
            // Level IS the position: `expected` is indexed by position while
            // BoundDependencies name LEVELS, so a reordered nest would compare
            // the two against each other. (Both builders emit them in order;
            // this pins that rather than assuming it.)
            && b.Level = i
            && b.IndexName = $"__i{i}"
            && (match b.Extent with IRLit (IRLitInt e) -> e = n | _ -> false)
            && Set.ofList b.BoundDependencies = deps
            && b.StrictOffset = strict)
        (expected |> List.mapi (fun i (n, d, s) -> (i, n, d, s))) bindings

/// Flat elementwise-nest detection + emission. Returns the flat loop's lines,
/// or None to fall through to `genLoopNestStreamed` unchanged.
///
/// `operandTypes` is `ApplyInfo.ArrayTypes`, positionally parallel to
/// `codeGen.InputArrayNames` (both are indexed by `ElementBinding.ArrayPosition`).
let tryGenFlatElementwiseNest
        (streamed: Map<string, ProviderReadSpec>)
        (operandTypes: IRArrayType list)
        (codeGen: LoopNestCodeGen)
        (outerNames: Map<int, string>)
        (indent: int) : string list option =
    let ind n = String.replicate n "    "
    let bindings = codeGen.Bindings
    let depth = List.length bindings
    let nArrays = List.length codeGen.InputArrayNames
    // ---- Gate 1: the nest is a plain materializing single-leaf map ----------
    //
    // FoldWrapper/FoldChunk: a fold writes a scalar accumulator, not one output
    // cell per iteration. MpiSlab: the outer level iterates a rank slab, not
    // [0, extent). HasReynolds: the body reads PERMUTED coordinates of its
    // operands, which is the opposite of index-free. Streamed sources have no
    // materialized pool at all. Multi-leaf fused trees never reach here
    // (genFusedLoopNestStreamed is a different emitter).
    //
    // ompTestModeEnabled: the omp-coverage instrumentation writes a per-thread
    // marker INSIDE the innermost body and reports ground truth about the nest
    // that ran. Rather than grow a second instrumentation site whose numbers
    // would have to be kept in step, the fast path stands down in test mode --
    // `blade test omp-coverage` then measures exactly the nest it always has.
    if codeGen.FoldWrapper.IsSome || codeGen.FoldChunk.IsSome then None
    elif codeGen.MpiSlab || codeGen.HasReynolds || codeGen.IsAntisymmetric then None
    elif not (Map.isEmpty streamed) then None
    elif ompTestModeEnabled () then None
    elif depth = 0 || nArrays = 0 then None
    elif List.length operandTypes <> nArrays then None
    elif (planHaloCarousel streamed codeGen outerNames).IsSome then None
    else
    // ---- Gate 2: the output is a plain (non-tabulated) array of nest rank ---
    match codeGen.OutputType with
    | ArrayElem outTy when not (isCompoundArrayType outTy)
                           && not (isSparseArrayType outTy)
                           && arrayRank outTy = depth ->
        // ---- Gate 3+4: one shape signature shared by the output and EVERY
        // operand. Structural equality covers the storage class, the group
        // decomposition AND the literal extents in one comparison.
        match flatShapeSignature outTy with
        | None -> None
        | Some sg when operandTypes |> List.forall (fun t -> flatShapeSignature t = Some sg) ->
            // Storage-class restriction (spec gate 3): all-dense-rectangular,
            // or ONE compact group spanning the whole array. The mixed shape
            // (a dense axis beside an antisym residual, AllocPerGroupStrict) is
            // contiguous too, but is left to a later pass -- its nest is not
            // exercised by the corpus and is out of scope here.
            let allDense = sg |> List.forall (fun g -> g.GSym = SymNone)
            let singleCompact =
                match sg with
                | [ g ] -> g.GSym = SymSymmetric || g.GSym = SymAntisymmetric
                | _ -> false
            if not (allDense || singleCompact) then None
            elif not (flatNestMatchesSignature sg bindings) then None
            else
            // ---- Gate 2 (reads): every operand is a RealArray scalar
            // full-depth peel at exactly the loop indices in order. Level k
            // must carry one element per operand position, all at
            // RankComponent = k over an array of rank = depth. This is what
            // rules out cross-indexing (A[j][i] reorders RankComponents),
            // partial peels and fiber arguments (ArrayRank > depth leaves an
            // Array<T,1> at the last level), index-variable params
            // (VirtualRange/VirtualReverse bind the loop index INTO the body)
            // and outer-product nests (a level owned by one array only).
            let elementsOk =
                bindings |> List.forall (fun b ->
                    let els = b.Elements
                    List.length els = nArrays
                    && (els |> List.map (_.ArrayPosition) |> Set.ofList) = Set.ofList [ 0 .. nArrays - 1 ]
                    && els |> List.forall (fun e ->
                            (e.Virtual.IsRealArray)
                            && e.RankComponent = b.Level
                            && e.ArrayRank = depth
                            && e.ArrayName = (codeGen.InputArrayNames |> List.item e.ArrayPosition)
                            // A `__halowin` slot tag marks a halo window, whose
                            // reads are OFFSET from the loop index; flatShapeSignature
                            // has already refused those on the array types, so this
                            // is a belt-and-braces read of the SAME tag through
                            // the element record -- and it must stay the same
                            // predicate, or the pair disagrees about `__anon`.
                            && (match e.SlotTag with Some t -> not (t.StartsWith "__halowin") | None -> true)))
            let cells = flatCellCount sg
            // OpenMP licensing. The flat loop FUSES every level, so threading
            // it threads every dimension -- which only the full licence grants.
            // A PARTIAL licence (`omp(a: 1)` over a rank-2 nest) is honoured by
            // falling back to the nest, where the pragma lands on exactly the
            // licensed prefix; over-threading here would silently exceed what
            // the user granted. Likewise a requested-but-unlicensed nest falls
            // back so genLoopNestStreamed's `// [omp] requested but emitted
            // serial` marker still appears.
            let allParallel = bindings |> List.forall (_.IsParallel)
            if not elementsOk then None
            elif cells <= 0L then None
            elif codeGen.OmpRequested && not allParallel then None
            else
            let outElem =
                match codeGen.OutputType with
                | ArrayElem at -> elemTypeToCpp at.ElemType
                | t -> irTypeToCpp t
            // Distinct operand NAMES get one pool pointer each: `f(A, A)` puts
            // two ArrayPositions on one array, and two declarations of one
            // pointer name would be a g++ redeclaration error.
            let poolOf (n: string) = $"__fp_{(sanitizeCppName n)}"
            let operandDecls =
                codeGen.InputArrayNames
                |> List.mapi (fun i n -> (i, n))
                |> List.distinctBy snd
                |> List.map (fun (i, n) ->
                    let elemCpp =
                        operandTypes |> List.tryItem i
                        |> Option.map (fun (t: IRArrayType) -> elemTypeToCpp t.ElemType)
                        |> Option.defaultValue outElem
                    $"const {elemCpp}* BLADE_RESTRICT {(poolOf n)} = nested_array_utilities::pool_base({n}.data);")
            // Kernel-body name map: each operand's leaf param renders as a pool
            // subscript instead of the per-iteration peel local. Subscript binds
            // tighter than every operator, so no parenthesization is needed.
            // Captures fill in only ids the enclosing scope does not know --
            // the same precedence rule genLoopNestStreamed uses.
            let paramFinalNames =
                bindings
                |> List.collect (_.Elements)
                |> List.fold (fun acc (e: ElementBinding) ->
                        Map.add e.ParamVarId ($"{(poolOf e.ArrayName)}[__fk]") acc)
                   Map.empty
            let nameMap = paramFinalNames |> Map.fold (fun acc k v -> Map.add k v acc) outerNames
            let nameMap =
                codeGen.Captures
                |> List.fold (fun acc c -> if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc) nameMap
            let body =
                genKernelExprWithReynolds codeGen.KernelExpr codeGen.KernelParams
                                          false false nameMap paramFinalNames
            // Pragma. Exactly one of the two, never both, and always the LAST
            // line before the `for` (an OpenMP construct rejects any
            // intervening code between itself and its loop).
            //
            // A restrict qualifier on a BLOCK-SCOPE LOCAL is dropped by GCC (it only
            // feeds restrict into its points-to solver for function
            // PARAMETERS), so the qualifiers above document intent and the
            // PRAGMA carries the dependence assertion. Both forms are
            // discharged by the same fact: exactly one array write per
            // iteration, through the fresh output pool, at the monotone flat
            // index -- no loop-carried dependence of any kind.
            //
            // BUILD KNOB. A serial-emission build drops the THREAD half and
            // keeps the VECTOR half: `#pragma omp simd`. That is not a
            // compromise, it is the decomposition -- `parallel for simd` is two
            // constructs, only the first of which outlines the body and opens a
            // team, and the dependence fact both rest on (one write per
            // iteration, fresh output pool, monotone flat index) is the same
            // fact. Falling back to `BLADE_IVDEP` here would have thrown away
            // vectorization the knob was never about. See
            // `ompThreadEmissionEnabled`.
            let threadsOn = ompThreadEmissionEnabled ()
            let pragma =
                if allParallel && threadsOn then "#pragma omp parallel for simd"
                elif allParallel then "#pragma omp simd"
                else "BLADE_IVDEP"
            // Census line, so a licensed-but-serialized flat loop is not silent.
            // It sits BEFORE the pragma: nothing may come between an OpenMP
            // construct and the `for` it governs.
            let pragmaMarker =
                if allParallel && not threadsOn
                then [ ind (indent + 1) + $"// [omp] requested but emitted serial: {(ompThreadsSuppressedReason ())}" ]
                else []
            let shapeNote =
                sg
                |> List.map (fun g ->
                    match g.GSym with
                    | SymNone -> string g.GExtent
                    | SymSymmetric -> $"sym<{g.GRank},{g.GExtent}>"
                    | SymAntisymmetric -> $"antisym<{g.GRank},{g.GExtent}>"
                    | s -> sprintf "%A" s)
                |> String.concat " x "
            Some (
                [ ind indent + $"// flat elementwise: {cells} cells [{shapeNote}] (pool DFS order == nest order)"
                  ind indent + "{" ]
                @ [ ind (indent + 1) + $"{outElem}* BLADE_RESTRICT {(poolOf codeGen.OutputName)} = nested_array_utilities::pool_base({codeGen.OutputName}.data);" ]
                @ (operandDecls |> List.map (fun d -> ind (indent + 1) + d))
                @ [ ind (indent + 1) + $"const size_t __fp_cells = {cells}UL;" ]
                @ pragmaMarker
                @ [ ind (indent + 1) + pragma
                    ind (indent + 1) + "for (size_t __fk = 0; __fk < __fp_cells; __fk++)"
                    ind (indent + 2) + $"{(poolOf codeGen.OutputName)}[__fk] = {body.CppExpr};"
                    ind indent + "}" ])
        | Some _ -> None
    | _ -> None


/// L2 dispatch at the apply-combinator site. On a `LinAlgPatterns.(|BlasL2|_|)`
/// match AND the BLAS gate on (`shimEntryPoint`), emits ONE `blade_linalg::`
/// call in place of the whole per-row nest; a decline returns None and the
/// chain in `tryGenLinAlgNest` carries on.
///
/// CodeGen decides NOTHING about the shape: the pattern owns which nests are
/// gemv, this only turns the descriptor into text (resolving the vector
/// operand's name through the kernel body's map, computing extents the same
/// way the nest's own bounds are computed).
let internal tryGenGemvDispatch
        (streamed: Map<string, ProviderReadSpec>)
        (operandTypes: IRArrayType list)
        (codeGen: LoopNestCodeGen)
        (outerNames: Map<int, string>)
        (indent: int) : string list option =
    match (Map.count streamed, codeGen.OmpRequested, operandTypes, codeGen) with
    | Blade.LinAlgPatterns.BlasL2 call ->
        match Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas call with
        | None -> None
        | Some entry ->
            // Name map: enclosing scope first, captures filling only ids it does
            // not know -- the same precedence rule genLoopNestStreamed and the
            // flat path use, so the vector resolves to the identifier the
            // kernel body would have rendered.
            let nameMap =
                codeGen.Captures
                |> List.fold (fun acc (c: CaptureInfo) ->
                        if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc)
                   outerNames
            let resolve src =
                match src with
                | Blade.LinAlgPatterns.FromNestArray n
                | Blade.LinAlgPatterns.FromNestOutput n -> Some n
                | Blade.LinAlgPatterns.FromKernelRef (IRVar (id, _)) -> Map.tryFind id nameMap
                | Blade.LinAlgPatterns.FromKernelRef _ -> None
            let named =
                call.NestOperands |> List.map (fun (role, src) -> (role, resolve src))
            let pick role =
                named |> List.tryPick (fun (r, n) -> if r = role then n else None)
            match pick Blade.LinAlgPatterns.RoleA,
                  pick Blade.LinAlgPatterns.RoleB,
                  pick Blade.LinAlgPatterns.RoleC with
            | Some aName, Some xName, Some yName ->
                // m = the row loop's own bound (literal after shape
                // monomorphization, else the runtime extent read) -- byte-for-
                // byte what the nest would have emitted.
                let mExtent =
                    genLoopBoundExpr (compoundArrayNamesOf codeGen.Bindings)
                                     (List.head codeGen.Bindings)
                // n = A's TRAILING extent: the peel gives the row
                // `A.extents + 1`, and prodsum bounds itself by that row's
                // extents[0]. Rendered from the operand TYPE when literal,
                // matching how the nest renders its own bounds -- through the
                // shared rule, which is where this site's own copy of it went.
                let nExtent =
                    match operandTypes with
                    | [ aTy ] -> literalOrRuntimeExtentOfArray aTy aName 1
                    | _ -> $"{aName}.extents[1]"
                // Pool capacity for the shim's contiguity probe.
                // `blade_gemv` stages A through an `in_view`, exactly as the L3
                // adapters do, so it needs the same bound -- the pattern above
                // already proved A dense rank-2, and this states the cell count
                // through the one shared derivation rather than restating it.
                let aCells =
                    match operandTypes with
                    | [ aTy ] -> denseCellCountOfArray aTy aName
                    | _ -> "0"
                (linalgUsedCell ()).Value <- true
                // BLOCK comment, not `//`: an inline-form materialization
                // space-joins its lines into a single-line IIFE, where a line
                // comment would swallow the rest of the statement.
                Some [ String.replicate indent "    "
                       + $"/* linalg dispatch: gemv y = {aName} * {xName} (per-row prodsum fiber) */ {entry}({mExtent}, {nExtent}, {aName}.data, {aCells}, {xName}.data, {yName}.data);" ]
            | _ -> None
    | _ -> None


/// L3 dispatch at the apply-combinator site: the comm-licensed
/// packed-covariance nest becomes ONE `blade_gram_same_*` (`?syrk`) call.
///
/// The twin of `tryGenGemvDispatch` one level up, and it turns the SAME
/// descriptor kind into text -- `LinAlgPatterns.(|BlasL3|_|)` owns which nests
/// are syrk, this owns nothing but the spelling.
///
/// C NEEDS NO STAGING AND NO CAPACITY ARGUMENT. `blade_gram_same_*` writes
/// `Crows[i][jr]` with `jr < m - i`, which is Blade's packed upper-triangular
/// row footprint exactly, so the freshly-allocated output pool is handed over
/// as it stands (`blade_linalg.hpp` records the layout proof). Only A, which
/// the shim stages through an `in_view`, carries the pool capacity the
/// contiguity probe cannot derive from a row skeleton.
///
/// THE KERNEL'S SCALAR (`prodsum(x, y) / N`) is applied AFTER the call, over
/// the same `[i][jr]` footprint the shim just wrote -- not folded into syrk's
/// `alpha`. `alpha = 1/N` would round a reciprocal and then multiply; the nest
/// this replaces divides the finished sum, and dividing the finished triangle
/// reproduces that operation for operation.
let internal tryGenSyrkDispatch
        (streamed: Map<string, ProviderReadSpec>)
        (operandTypes: IRArrayType list)
        (codeGen: LoopNestCodeGen)
        (outerNames: Map<int, string>)
        (indent: int) : string list option =
    match (Map.count streamed, codeGen.OmpRequested, operandTypes, codeGen) with
    | Blade.LinAlgPatterns.BlasL3 call ->
        match Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas call with
        | None -> None
        | Some entry ->
            let aName =
                call.NestOperands
                |> List.tryPick (fun (r, src) ->
                    match r, src with
                    | Blade.LinAlgPatterns.RoleA, Blade.LinAlgPatterns.FromNestArray n -> Some n
                    | _ -> None)
            let cName =
                call.NestOperands
                |> List.tryPick (fun (r, src) ->
                    match r, src with
                    | Blade.LinAlgPatterns.RoleC, Blade.LinAlgPatterns.FromNestOutput n -> Some n
                    | _ -> None)
            match aName, cName with
            | Some aName, Some cName ->
                // m = the outer level's own bound (literal after shape
                // monomorphization, else the runtime extent read) -- byte-for-
                // byte what the nest would have emitted. The inner level is the
                // triangle over the SAME m, which the pattern proved.
                let mExtent =
                    genLoopBoundExpr (compoundArrayNamesOf codeGen.Bindings)
                                     (List.head codeGen.Bindings)
                // n = A's TRAILING extent -- the contracted axis. Same rule as
                // gemv: rendered from the operand TYPE when literal.
                let nExtent =
                    match operandTypes with
                    | aTy :: _ -> literalOrRuntimeExtentOfArray aTy aName 1
                    | _ -> $"{aName}.extents[1]"
                let aCells =
                    match operandTypes with
                    | aTy :: _ -> denseCellCountOfArray aTy aName
                    | _ -> "0"
                // The scale, re-derived from the kernel body through the
                // pattern module's OWN peel so the two cannot disagree about
                // what a scaled contraction is.
                let nameMap =
                    codeGen.Captures
                    |> List.fold (fun acc (c: CaptureInfo) ->
                            if Map.containsKey c.Id acc then acc else Map.add c.Id c.Name acc)
                       outerNames
                let renderScalar (e: IRExpr) : string option =
                    match e with
                    | IRVar (id, _) when not (Map.containsKey id nameMap) -> None
                    | _ ->
                        let r = genKernelExprWithReynolds e codeGen.KernelParams
                                                          false false nameMap Map.empty
                        Some r.CppExpr
                // Three answers, hence the nested option: `None` = DECLINE the
                // whole dispatch (a scale this site cannot spell), `Some None`
                // = no scale to apply, `Some (Some line)` = the scaling pass.
                let scaleLine : string option option =
                    match codeGen.KernelExpr with
                    | Blade.LinAlgPatterns.ProdSumScaled (_, None) -> Some None
                    | Blade.LinAlgPatterns.ProdSumScaled (_, Some sc) ->
                        let (opStr, sExpr) =
                            match sc with
                            | Blade.LinAlgPatterns.ScaleDiv d -> ("/=", d)
                            | Blade.LinAlgPatterns.ScaleMul s -> ("*=", s)
                        match renderScalar sExpr with
                        | None -> None                       // unresolvable -> decline
                        | Some sTxt ->
                            Some (Some (String.replicate indent "    "
                                        + $"/* ... then the kernel's own scalar, over the same packed triangle */ for (size_t __sy_i = 0; __sy_i < {mExtent}; __sy_i++) for (size_t __sy_j = 0; __sy_j < {mExtent} - __sy_i; __sy_j++) {cName}[__sy_i][__sy_j] {opStr} ({sTxt});"))
                    | _ -> None
                match scaleLine with
                | None -> None
                | Some tail ->
                    (linalgUsedCell ()).Value <- true
                    // BLOCK comment, not `//`: an inline-form materialization
                    // space-joins its lines into a single-line IIFE, where a
                    // line comment would swallow the rest of the statement.
                    Some ([ String.replicate indent "    "
                            + $"/* linalg dispatch: syrk C = A * A^T (packed upper, from comm nest) */ {entry}({mExtent}, {nExtent}, {aName}.data, {aCells}, {cName}.data);" ]
                          @ Option.toList tail)
            | _ -> None
    | _ -> None


/// The linear-algebra dispatch chain at the apply-combinator site: each
/// recognised NEST shape in turn, falling through to the flat elementwise path
/// (`tryGenFlatElementwiseNest`) and then `genLoopNestStreamed`, unchanged --
/// the ordinary emitted nest the interpreter differential already covers.
///
/// Order is not load-bearing: the shapes are disjoint by depth (gemv is a
/// depth-1 nest over a rank-2 operand, syrk a depth-2 triangular nest over
/// two slots naming one array), so at most one can match.
///
/// Stands down in test mode for the same reason the flat path does: omp-
/// coverage instrumentation needs an innermost body to mark, which a
/// dispatched call has none of.
let tryGenLinAlgNest
        (streamed: Map<string, ProviderReadSpec>)
        (operandTypes: IRArrayType list)
        (codeGen: LoopNestCodeGen)
        (outerNames: Map<int, string>)
        (indent: int) : string list option =
    if ompTestModeEnabled () then None
    elif (planHaloCarousel streamed codeGen outerNames).IsSome then None
    else
    match tryGenGemvDispatch streamed operandTypes codeGen outerNames indent with
    | Some lines -> Some lines
    | None -> tryGenSyrkDispatch streamed operandTypes codeGen outerNames indent


// Symmetry Vector Generation


// Array Allocation Generation


// Function Template Generation

/// Generate template parameter list for a combinator function
let genTemplateParams (inputCount: int) (hasOutput: bool) : string =
    let inputs = 
        [0 .. inputCount - 1] 
        |> List.collect (fun i -> 
            [$"typename ITYPE{i+1}"
             $"const size_t IRANK{i+1}"
             $"const size_t* ISYM{i+1}"])
    let output =
        if hasOutput then
            ["typename OTYPE"; "const size_t ORANK"; "const size_t* OSYM"]
        else []
    inputs @ output |> String.concat ", "

/// Generate function parameter list
let genFunctionParams (inputNames: string list) (outputName: string) : string =
    let inputs =
        inputNames |> List.mapi (fun i name ->
            [$"typename promote<ITYPE{i+1}, IRANK{i+1}>::type {name}"
             $"const size_t {name}_extents[IRANK{i+1}]"])
        |> List.concat
    let output =
        [$"typename promote<OTYPE, ORANK>::type {outputName}"
         $"const size_t {outputName}_extents[ORANK]"]
    inputs @ output |> String.concat ",\n    "

// Complete Function Generation

/// Generate a complete C++ function from LoopNestCodeGen
let genFunction (codeGen: LoopNestCodeGen) (funcName: string) : string list =
    let inputCount = codeGen.InputArrayNames.Length
    
    // Template declaration
    let templateParams = genTemplateParams inputCount true
    let funcParams = genFunctionParams codeGen.InputArrayNames codeGen.OutputName
    
    // Function signature
    let signature = 
        [$"template<{templateParams}>"
         $"void {funcName}("
         $$"""    {{funcParams}}) {"""]
    
    // Body with loop nest
    let body = genLoopNest codeGen Map.empty 1
    
    // Close
    let close = ["}"]
    
    signature @ body @ close

/// Generate header includes
let genIncludes () : string list =
    ["#include <cstdint>"
     "#include <cstdlib>"  // for rand()
     "#include <cmath>"
     "#include <complex>"
     "#include <functional>"
     "#include <tuple>"
     "#include <variant>"
     "#include <string>"
     "#include <iostream>"
     "#include <iomanip>"
     "#include <chrono>"
     "#include <algorithm>"  // std::stable_sort (used by sort())
     "#include <numeric>"    // std::iota (used by sort())
     "#include <vector>"     // solve()'s LU working copy (materializeSolveForm)
     "#include <unordered_map>"  // group_keys Case 3 (dynamic ngroups via hash discovery)
     "#include <unordered_set>"  // unique() dedup, contains() hoist (future)
     // OpenMP is ENABLED (Build.compileCppWithExtra always passes -fopenmp);
     // `#pragma omp` needs no header, so <omp.h> is included only when
     // something calls the omp_* RUNTIME API: the test-mode instrumentation
     // (known here) or a comm-licensed parallel fold (only known after body
     // generation, so the assemblers append it via ompApiUsedCell -- the
     // blade_linalg-include pattern).
     (if ompTestModeEnabled () then "#include <omp.h>  // omp-coverage test-mode instrumentation" else "// #include <omp.h>")
     "#include \"nested_array_utilities.cpp\""
     "#include \"rand_runtime.hpp\""
     "#include <exception>"                 // std::exception for main()'s BL8005 catch
     "#include \"blade_runtime.hpp\""        // blade_rt::panic + BLADE_FRAME shadow stack
     ]
    // Memcheck instrumentation (BLADE_MEMCHECK=1 only): appended as an extra
    // element, never a placeholder comment, so default output stays
    // byte-identical to a build without the feature.
    @ (if memcheckEnabled () then ["#include \"blade_memcheck.hpp\""] else [])
    @
    ["using namespace nested_array_utilities;"
     "using std::cout;"
     "using std::endl;"
     ""]
    // Display-frame emitter (docs/display-frames.md). Header-only, static
    // inline and free when unused, so it is emitted unconditionally rather
    // than behind a per-program feature scan.
    @ Blade.Display.Frame.cppRuntime ()
    @
    [""
     "#define TIME std::chrono::high_resolution_clock::now()"
     "#define TIME_DIFF std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count()"
     ""]

// C++ runtime headers
//
// The Blade C++ runtime lives in cpp/*.hpp at the source root and is read
// from disk at codegen time, not embedded in the F# binary as string
// literals. Blade.fsproj copies the cpp/ directory into the build output
// via <CopyToOutputDirectory>, so AppContext.BaseDirectory + "cpp" resolves
// to the correct location regardless of where dotnet run is invoked from.
//
// The generated C++ test output picks up the headers when Main.fs writes
// them into each test's output directory alongside the .cpp file (the
// existing pattern; see Main.fs's writes of headerFile / arrayTypesHeaderFile).
// g++ then resolves `#include "nested_array_utilities.hpp"` relative to
// the .cpp file's directory -- no -I flag needed, no build-output paths
// leaked into the C++ compile line.

/// Resolve the path of a runtime header file shipped in the cpp/ directory
/// next to the compiler binary. Used by both genRuntimeHeader and
/// genRuntimeArrayTypesHeader; centralized here so the AppContext.BaseDirectory
/// and "cpp" subpath assumptions live in one place.
let internal cppRuntimeHeaderPath (filename: string) : string =
    System.IO.Path.Combine(System.AppContext.BaseDirectory, "cpp", filename)

/// Memo for `readCppRuntimeHeader`. The cpp/ headers are static files shipped
/// beside the binary: within one process their contents cannot change, so each
/// is read from disk at most once however many programs get compiled (the test
/// harness compiles ~1500 of them, each deploying the same 13 headers,
/// ~264 KB in total). Concurrent because the harness generates in parallel.
let internal cppRuntimeHeaderCache =
    System.Collections.Concurrent.ConcurrentDictionary<string, string>()

/// Read a Blade C++ runtime header from disk (memoized). Fails loudly if the
/// build hasn't copied cpp/ into the output directory -- this is a
/// configuration error rather than a compiler bug, so the message points at
/// .fsproj. Deliberately a FUNCTION over a lazily-populated table rather than
/// an eagerly-built map, so the not-found diagnostic still fires at the first
/// *use* of a missing header and never for headers nobody asked for.
let internal readCppRuntimeHeader (filename: string) : string =
    match cppRuntimeHeaderCache.TryGetValue filename with
    | true, cached -> cached
    | _ ->
        let path = cppRuntimeHeaderPath filename
        if not (System.IO.File.Exists path) then
            raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan (sprintf
                "C++ runtime header not found at: %s\n\
                 The build should copy cpp/%s into the output directory.\n\
                 Check that Blade.fsproj contains a <None Include=\"cpp/%s\">\n\
                 item with <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>."
                path filename filename)))
        let text = System.IO.File.ReadAllText path
        cppRuntimeHeaderCache.[filename] <- text
        text

/// Generate the runtime header file content (read from cpp/nested_array_utilities.hpp).
/// Main.fs writes the result alongside each test's generated .cpp so
/// `#include "nested_array_utilities.hpp"` resolves at g++ time.
let genRuntimeHeader () : string =
    readCppRuntimeHeader "nested_array_utilities.hpp"

/// Generate the array-types runtime header (read from cpp/nested_array_types.hpp).
/// Contains the wrapper structs (Array<T,N>, Ragged<T>, RaggedRow<T>, and
/// Compound<T,RANK>) that carry shape metadata alongside the data pointer.
/// It `#include`s index_types.h (compound_index_t + the tabulated index bases),
/// which is therefore deployed next to it -- see deployRuntimeHeaders.
let genRuntimeArrayTypesHeader () : string =
    readCppRuntimeHeader "nested_array_types.hpp"

/// Read the index-types runtime header: compound_index_t plus the tabulated
/// index bases. nested_array_types.hpp `#include`s it, so it must ship next to
/// every generated .cpp (via deployRuntimeHeaders) for the include to resolve.
let genIndexTypesHeader () : string =
    readCppRuntimeHeader "index_types.h"

/// The C++ runtime header set. SINGLE SOURCE OF TRUTH: a header newly
/// depended on by the runtime is added here once (and to Blade.fsproj's copy
/// set), after which it reaches every emit site via deployRuntimeHeaders.
/// Exposed as names so callers that clean up after a compile (Cli.fs) know
/// exactly which files were deployed.
let runtimeHeaderNames : string list =
    [ // BLADE_RESTRICT / BLADE_IVDEP: the portable spellings of the annotations
      // codegen emits. Bare `__restrict__` / `#pragma GCC ivdep` are rejected by
      // cl.exe, which the Windows CUDA path drives through nvcc for the HOST
      // half. Pulled in by nested_array_utilities.hpp and blade_linalg_views.hpp
      // rather than by a generated `#include`, but it must still be DEPLOYED
      // beside them.
      "blade_portability.hpp"
      "nested_array_utilities.hpp"
      "nested_array_types.hpp"
      "index_types.h"
      // Host combinadic linearize/unlinearize -- included only by MPI-mode
      // programs (genMpiNestSimplicial), but deployed unconditionally so the
      // deploy/cleanup bookkeeping stays uniform.
      "linearized_storage.hpp"
      // OrbIdx (iterated-wreath) storage: orb_cell_count / the segment-peeled
      // orb_visit nest / orb_canon / orb_rank / orb_unrank. Included only by
      // programs that deduce a wreath class (genWreathApply), but deployed
      // unconditionally so the deploy/cleanup bookkeeping stays uniform.
      "orbit_wreath_utilities.hpp"
      // `rand` module runtime (blade_rand::uniform/normal). Deployed
      // unconditionally (header-only, cheap); referenced by every program's
      // include list.
      "rand_runtime.hpp"
      // Runtime error support: blade_rt shadow call stack + panic() and the
      // BLADE_FRAME macro. Header-only, host-only (device passes see
      // no-op stubs); deployed unconditionally and included by every program.
      "blade_runtime.hpp"
      // Dense linear-algebra dispatch: blade_gemm / blade_syrk plus the
      // gram/matmul adapters, resolving to cblas under -DBLADE_HAS_BLAS and to
      // native fallbacks otherwise. INCLUDED only by programs that actually
      // emit a `blade_linalg::` call (the linalgUsedCell collector), so a
      // program using neither gram nor matmul carries no dependency surface at
      // all -- but DEPLOYED unconditionally, like linearized_storage.hpp and
      // orbit_wreath_utilities.hpp, so the deploy/cleanup bookkeeping stays
      // uniform.
      "blade_linalg.hpp"
      // The BLAS-free half of the same layer: the contiguity probe
      // and the two staging views. `blade_linalg.hpp` includes it, so it must
      // be deployed beside it -- and it is separately includable, which is what
      // lets cpp/linalg_probe_tests.cpp run without BLAS.
      "blade_linalg_views.hpp"
      // DEVICE (cuBLAS) dispatch. Two halves in one file, selected by
      // `__CUDACC__`: `extern "C"` prototypes for the host `.cpp` (g++ cannot
      // include the CUDA headers), definitions for the companion `.cu` that
      // Build.fs writes and nvcc compiles. Included only by programs that emit
      // a `blade_cuda_*` call, so a host program carries no CUDA toolchain
      // dependency -- deployed unconditionally like every other runtime header,
      // and it must be, since the `.cu` lands in the same directory and
      // includes it by relative path (as does `blade_linalg_views.hpp`, which
      // the device arm reuses for the shared contiguity probe).
      "blade_linalg_cuda.hpp"
      // Eigensolver dispatch: the `?spev`/`?hpev`/`?syev`/
      // `?heev` adapters. LAPACK-ONLY (its own `#ifndef BLADE_HAS_LAPACK
      // #error`) and included only by programs that emit a `blade_lapack::`
      // call, so a BLAS program carries no LAPACK dependency -- deployed
      // unconditionally like every other runtime header.
      "blade_lapack.hpp"
      // ASan allocator-stats export bracketing main() (one BLADE-MEMCHECK
      // line on stderr at exit). Included only by programs generated under
      // BLADE_MEMCHECK=1 -- Build.fs pairs the include with a Debug+ASan
      // compile -- but deployed unconditionally so the deploy/cleanup
      // bookkeeping stays uniform.
      "blade_memcheck.hpp" ]

/// The shipped text of one runtime header, through the same per-process memo
/// `deployRuntimeHeaders` uses -- so a consumer that only wants to *read* the
/// header set (Build.fs's executable cache hashes all 13 into its key) pays no
/// extra disk I/O beyond the one read the deploy already did. Same
/// fail-loudly-on-missing contract as the deploy path.
let runtimeHeaderText (filename: string) : string = readCppRuntimeHeader filename

/// Deploy every C++ runtime header next to a generated .cpp so its `#include`s
/// resolve at g++ time with no -I flag. These are pre-existing static files in
/// cpp/, copied verbatim -- nothing is generated or transformed.
///
/// ALL headers are deployed uniformly, unconditionally (see runtimeHeaderNames:
/// the deploy/cleanup bookkeeping is deliberately not per-program) -- but a
/// deployment whose destination already holds byte-identical content is not
/// rewritten. Only the physically redundant write is skipped: rewriting ~264 KB
/// beside every generated .cpp on every compile re-triggers an antivirus scan
/// of each file (measured multi-second variance on a one-line `blade compile`)
/// for no change in what g++ sees.
///
/// This preserves the hand-edit workflow: a deployed header edited in place
/// DIFFERS from the shipped one, so the next `blade run` overwrites it exactly
/// as before -- "if changed" is a content test, not a timestamp test.
///
/// CONCURRENT WRITERS: two compiles sharing an output directory (parallel
/// sweeps, two agents in one scratch dir) can collide on the same destination,
/// and on Windows the loser gets a sharing violation. That is NOT a failure
/// here: both processes deploy the SAME shipped bytes, so the only question is
/// whether the file on disk ends up correct. Re-check the content after a
/// failed write, retry briefly (the other writer's handle lives for
/// microseconds), and surface the IO error only if the destination genuinely
/// does not hold the header -- in which case the compile must not proceed to a
/// g++ that would read a truncated include.
let deployRuntimeHeaders (outputDir: string) : unit =
    runtimeHeaderNames
    |> List.iter (fun name ->
        let dest = System.IO.Path.Combine(outputDir, name)
        let text = readCppRuntimeHeader name
        let alreadyDeployed () =
            try System.IO.File.Exists dest && System.IO.File.ReadAllText dest = text
            with _ -> false
        if not (alreadyDeployed ()) then
            let rec attempt (retriesLeft: int) =
                try
                    System.IO.File.WriteAllText(dest, text)
                with
                // A concurrent writer got there first with identical bytes.
                | _ when alreadyDeployed () -> ()
                | _ when retriesLeft > 0 ->
                    System.Threading.Thread.Sleep 10
                    attempt (retriesLeft - 1)
                // No rule matches when the retries are spent and the file is
                // still wrong: F# re-raises, and the driver reports it.
            attempt 5)

/// Generate includes that reference external header
let genIncludesExternal () : string list =
    ["#include <cstdint>"
     "#include <cstdlib>"
     "#include <cmath>"
     "#include <complex>"
     "#include <functional>"
     "#include <tuple>"
     "#include <variant>"
     "#include <string>"
     "#include <iostream>"
     "#include <iomanip>"
     "#include <chrono>"
     "#include <algorithm>"  // std::stable_sort (used by sort())
     "#include <numeric>"    // std::iota (used by sort())
     "#include <vector>"     // solve()'s LU working copy (materializeSolveForm)
     "#include <unordered_map>"  // group_keys Case 3 (dynamic ngroups via hash discovery)
     "#include <unordered_set>"  // unique() dedup, contains() hoist (future)
     "#include <omp.h>"
     "#include \"nested_array_utilities.hpp\""
     "#include \"nested_array_types.hpp\""
     // OrbIdx (iterated-wreath) storage. Header-only and dependency-free
     // (<cstdint>/<cstddef>/<cassert>), so it is included unconditionally
     // alongside the other runtime headers rather than gated on whether this
     // particular program deduced a wreath class.
     "#include \"orbit_wreath_utilities.hpp\""
     "#include \"rand_runtime.hpp\""
     "#include <exception>"                 // std::exception for main()'s BL8005 catch
     "#include \"blade_runtime.hpp\""        // blade_rt::panic + BLADE_FRAME shadow stack
     ]
    // Memcheck instrumentation -- see the sibling include block above.
    @ (if memcheckEnabled () then ["#include \"blade_memcheck.hpp\""] else [])
    @
    ["using std::cout;"
     "using std::endl;"
     ""]
    // Display-frame emitter -- see the sibling include block above.
    @ Blade.Display.Frame.cppRuntime ()
    @
    [""
     "#define TIME std::chrono::high_resolution_clock::now()"
     "#define TIME_DIFF std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count()"
     ""]


// Full Program Generation

/// Generate a complete C++ program from multiple LoopNestCodeGen
let genProgram (functions: (string * LoopNestCodeGen) list) : string =
    let includes = genIncludes ()
    
    let funcCode = 
        functions 
        |> List.collect (fun (name, cg) -> genFunction cg name @ [""])
    
    (includes @ funcCode) |> String.concat "\n"

// Array Literal Generation

/// Extract float values from array literal for initialization
let rec extractLiteralValues (expr: IRExpr) : float list =
    match expr with
    | IRLit (IRLitFloat f) -> [f]
    | IRLit (IRLitFloat32 f) -> [float f]
    | IRLit (IRLitInt n) -> [float n]
    | IRLit (IRLitBool b) -> [if b then 1.0 else 0.0]
    | IRUnaryOp (IRNeg, IRLit (IRLitFloat f)) -> [-f]
    | IRUnaryOp (IRNeg, IRLit (IRLitFloat32 f)) -> [float -f]
    | IRUnaryOp (IRNeg, IRLit (IRLitInt n)) -> [float -n]
    | IRArrayLit (elements, _) -> elements |> List.collect extractLiteralValues
    | _ -> []

/// Compute dimensions of an array literal
let rec computeArrayDims (expr: IRExpr) : int list =
    match expr with
    | IRArrayLit (elements, _) ->
        let thisLen = elements.Length
        match elements with
        | first :: _ -> thisLen :: computeArrayDims first
        | [] -> [0]
    | _ -> []

/// Compute per-row lengths for a ragged literal. Returns the inner sub-array
/// length for each outer entry. For [[1,2,3], [4,5], [6,7,8,9]] returns [3; 2; 4].
/// For non-ragged or non-nested input, returns the empty list.
let computeRaggedRowLengths (elements: IRExpr list) : int list =
    elements |> List.choose (fun e ->
        match e with
        | IRArrayLit (inner, _) -> Some inner.Length
        | _ -> None)

// Deterministic deallocation: fresh-return facts, scope escapes, alloc registry.
//
// Module-level heap allocations are left to leak (compute-print-exit, OS
// reclaims) -- harmless for a module-level binding, but not for a FUNCTION BODY
// or `for`-range LOOP BODY array (re-allocated per call/iteration, unbounded
// growth). This block lets exactly those two scopes emit a matching
// `deallocate<>`/`deallocate_strict<>` at scope exit.
//
// Three rules make it sound:
//  1. ONLY the two scope drivers push a frame; an EMPTY stack (main()'s top
//     level) makes registerAlloc a no-op, so module bindings survive to be
//     auto-printed/EXPECT-pinned -- structural, not a per-site guard.
//  2. A registration always lands on the TOP frame, so a loop-body free can
//     only name storage allocated in THAT iteration (stack discipline, not
//     id-based reasoning).
//  3. Anything unproven stays leaked: a leak is a bug we already have, but a
//     double free or a free of a pool something still reads is a crash or
//     silently wrong numbers. Escape seeds are deliberately WIDE, propagation
//     barriers deliberately NARROW, and template arguments are recorded
//     verbatim at the allocation site instead of re-derived at exit.
//
// Deliberate exclusions (each a distinct code path, none an oversight):
// ragged/compound/dep-idx outputs; provider reads/writes; RandomInits;
// CompoundInits; CUDA/MPI *device* allocations (already own themselves); and
// streamed `_fb_p` fiber buffers registered outside a live frame.
//
// The statement-level `materializeInlineForm` consumers (site 7) each pick
// their (AllocSpec, SYMM, STRICT) triple from form-specific masks the consumer
// never sees, so each builder returns `string list * MaterializedAlloc list`
// instead of the consumer re-deriving the triple (which risks `deallocate`
// walking a different skeleton than `allocate` built). Statement-position
// consumers call `registerMaterializedAllocs`; IIFE consumers drop the list
// and leak (no scope exit to hang a free on).

/// Flatten a body's nested IRLet chains into (id, value) pairs plus the residual
/// return expression. Deliberately a verbatim mirror of genFuncBody's local
/// `deepUnroll`, including the nested-value hoist: the escape analysis must see
/// EXACTLY the statement list codegen will emit, or an allocation could land in a
/// frame whose escape set was computed from a different shape.
let rec deepUnrollBody (expr: IRExpr) : (IRId * IRExpr) list * IRExpr =
    match expr with
    | IRLet (id, value, body) ->
        let (innerLets, innerFinal) = deepUnrollBody value
        let (restLets, restFinal) = deepUnrollBody body
        match innerLets with
        | [] -> ((id, value) :: restLets, restFinal)
        | _ -> (innerLets @ [(id, innerFinal)] @ restLets, restFinal)
    | _ -> ([], expr)

/// Every sub-expression of `e`, `e` itself included. Recursion is the canonical
/// ExprShape fold, so no new IRExpr variant can hide a nested IRAssign or
/// callable reference from the seed rules below the way a hand-maintained walker
/// with a `| _ ->` arm would.
let rec allSubExprs (e: IRExpr) : IRExpr list =
    match e with
    | ExprShape (children, _) -> e :: (children |> List.collect allSubExprs)

/// A return that hands the caller an INTERIOR VIEW of storage this scope owns.
///
/// `traj((9999 : T))` at the tail of a function-local `let rec` renders as
/// `Array<T,1>{ traj.data[9999], traj.extents + 1 }` -- a wrapper POINTING INTO
/// the trajectory's pool. Nobody can free that pool: not the scope (the
/// returned view still reads it) and not the caller (it never sees the base
/// wrapper, only one row of it), so the whole array leaks on every call. The
/// escape analysis makes that explicit -- seeding the base pins it -- but
/// pinning is not a fix, it is the leak spelled out.
///
/// Returns the base binding's id when the return takes that shape AND the
/// slice can be copied out, which is the actual repair: materialize the slice
/// into its own pool before the frees, and the base becomes an ordinary scope
/// temporary. Three consumers share this ONE predicate so they cannot drift --
/// computeScopeEscapes (drop the return seed), genFuncBodyScoped's return arm
/// (emit the copy), and computeFreshReturnFacts (tell callers they now own a
/// fresh pool). A disagreement between the first two would be a use-after-free,
/// so a single decision point is not a stylistic preference here.
///
/// Guards, each load-bearing for the flat `std::copy_n` the materialization
/// emits:
///   * the base is one of THIS scope's lets. A parameter, capture, or module
///     binding is someone else's storage, correctly returned as a view (and
///     copying it would hand the caller a pool it has no reason to free);
///   * base and slice are both plain dense -- no symmetry, compact, ragged,
///     sparse, dep-idx, or virtual storage -- so the sub-block a leading index
///     selects is CONTIGUOUS in the pool and `pool_base` on the sub-skeleton
///     lands on its first cell rather than the whole pool's;
///   * every subscript is scalar and they form a strict LEADING prefix
///     (`idxs` exactly consumes the rank the slice drops), which is what makes
///     the selection a sub-block rather than a strided gather.
let returnedInteriorView (scopeLets: (IRId * IRExpr) list) (retExpr: IRExpr) : IRId option =
    let denseUnsymmetric (t: IRType) =
        match t with
        | ArrayElem at ->
            isFreeableDenseArrayType at && not at.IsVirtual
            && classifyOutputStorage t = AllocDense
            && not (hasRealSymmetry (buildSymmVec t))
        | _ -> false
    match retExpr with
    | IRIndex ((IRVar (srcId, _)) as baseExpr, idxs, _)
            when scopeLets |> List.exists (fun (id, _) -> id = srcId) ->
        let baseTy = inferExprType baseExpr
        let sliceTy = inferExprType retExpr
        match baseTy, sliceTy with
        | ArrayElem bat, ArrayElem sat
                when denseUnsymmetric baseTy && denseUnsymmetric sliceTy
                     && not (List.isEmpty idxs)
                     && arrayRank sat > 0
                     && List.length idxs = arrayRank bat - arrayRank sat
                     && idxs |> List.forall (fun i ->
                            (inferExprType i).IsIRTScalar) ->
            Some srcId
        | _ -> None
    | _ -> None

/// Classify every array-returning module function as FreshPool or NotFresh.
/// Only array-typed returns are in the domain; anything else is absent and reads
/// as NotFresh. The iteration is a fixpoint because a function may return the
/// result of calling another one; it is monotone (facts only move NotFresh ->
/// FreshPool) so it terminates.
let computeFreshReturnFacts (modul: IRModule) : Map<IRId, FreshReturn> =
    let isLifted (e: IRExpr) =
        // The two return forms genFuncBody LIFTS into a named local before
        // returning it (10277 / 10282). Both allocate under that local's name,
        // so the storage is unaliased and the caller owns it.
        match e with
        | IRCompute (IRApplyCombinator _) | IRArrayLit _ -> true
        | _ -> false
    let unrolled =
        modul.Functions
        |> List.filter (fun f -> match f.RetType with ArrayElem _ -> true | _ -> false)
        |> List.map (fun f -> (f.Id, deepUnrollBody f.Body))
    let classify (facts: Map<IRId, FreshReturn>) (lets: (IRId * IRExpr) list) (retExpr: IRExpr) : FreshReturn =
        if isLifted retExpr then FreshPool
        else
            match retExpr with
            | IRVar (rid, _) ->
                // `return r` where r is bound to a lifted form AND appears in no
                // OTHER let's value: the let's storage IS the return value, and
                // no second name can be holding it. An alias-shaped second use
                // (`let s = r(0)` before the return) demotes to NotFresh.
                let boundToLifted = lets |> List.exists (fun (id, v) -> id = rid && isLifted v)
                let otherUses =
                    lets |> List.filter (fun (id, v) -> id <> rid && Set.contains rid (collectVarRefsIR v))
                if boundToLifted && List.isEmpty otherUses then FreshPool else NotFresh
            // An interior view of a scope-local array is no longer returned as
            // a view: genFuncBodyScoped's return arm copies the slice into its
            // own pool (see returnedInteriorView), so the caller DOES own the
            // storage it receives. Classifying it NotFresh here would leave the
            // caller-side free off and simply move the leak one frame out.
            | IRIndex _ when (returnedInteriorView lets retExpr).IsSome -> FreshPool
            | IRApp (f, _, _) ->
                match resolveCallable f with
                | Some c -> Map.tryFind c.Id facts |> Option.defaultValue NotFresh
                | None -> NotFresh
            // Params, captures, IRIndex/IRSlice/IRCurry views, IRIf/IRMatch,
            // IRTuple, materializer forms, unresolved callees: all NotFresh.
            | _ -> NotFresh
    let rec fix (facts: Map<IRId, FreshReturn>) =
        let facts' =
            unrolled |> List.fold (fun acc (fid, (lets, retExpr)) ->
                match Map.tryFind fid acc with
                | Some FreshPool -> acc
                | _ ->
                    match classify acc lets retExpr with
                    | FreshPool -> Map.add fid FreshPool acc
                    | NotFresh -> acc) facts
        if facts' = facts then facts else fix facts'
    fix Map.empty

/// Does this let's value OWN a freshly allocated pool that nothing else in the
/// scope can reach? Only such a value STOPS escape propagation: when the binding
/// escapes, its inputs need not also be pinned.
///
/// Deliberately non-barrier, against a naive reading of "fresh-pool producer":
///   * IRChoice / IRFallback / IRGuard / IRComposeMeth -- their results BORROW an
///     operand's `.extents` pointer, so an escaping result must pin its operands.
///   * IRSequence / IRReplicate -- the emitter DOES now give the result its own
///     dense pool (a per-child copy nest, like stack), so these could become
///     barriers; they are held out because the emitter still does not register
///     that pool for freeing, and a barrier here would stop propagation to
///     children the frees do reach. Flip both together, never just this one.
///   * IRParallel / IRFusion / IRFunctorMap / IRZip -- deferred forms whose
///     forcing shape depends on whether the leaf is a computation or a concrete
///     array; not worth proving.
///   * every view/projection form (IRVar, IRIndex, IRSlice, IRCurry, IRSubset,
///     IRShift, IRReverse, IRDiag, IRAlign, IRTuple, IRTupleProj, IRFieldAccess,
///     IRIf, IRMatch, IRApp on a NotFresh callee).
/// The trailing `| _ ->` is intentional and must stay: "unknown => propagates"
/// is the safe default, and a new IR variant should not become a build break here
/// (a wrong barrier frees too early; a wrong non-barrier only leaks).
let rec isFreshPoolForm (e: IRExpr) : bool =
    match e with
    | IRCompute inner -> isFreshPoolForm inner
    | IRApplyCombinator _ | IRComposeApply _ -> true
    | IRArrayLit _ -> true
    | IRMask _ | IRSort _ | IRUnique _ | IRIntersect _ | IRUnion _ -> true
    | IRTranspose _ | IRDecompact _ | IRStack _ | IRJoin _ | IRGram _ | IRGramApply _ | IRMatmul _ -> true
    // eigh: BOTH pools it produces are fresh (`allocate<>` under derived names)
    // and neither borrows the operand's `.extents` pointer -- each gets its own
    // table. So an escaping (Q, LAM) need not pin S, and propagation stops here.
    | IREigh _ -> true
    // solve: x is a fresh `allocate<>` pool with its own extents table -- it
    // borrows nothing from A or b (b's values are COPIED in, not aliased), so
    // an escaping x need pin neither operand and propagation stops here.
    | IRSolve _ -> true
    | IRArrayNegate _ | IRArrayConjugate _ -> true
    | IRReduce _ | IRReduceCompute _ | IRProdSum _ -> true
    | IRApp (f, _, _) -> freshReturnOf f = FreshPool
    | _ -> false

/// May a binding whose value is a bare reference to a scope-local STAGING let be
/// emitted as a plain ALIAS, instead of genVarAliasBinding's defensive deep copy?
///
/// The copy exists because an ASSIGNABLE binding must not share storage with a
/// value some other name can still reach: `let mut a = Z` followed by
/// `a(i) = ...` would otherwise corrupt `Z`. When the source is a scope-local
/// staging let that SOLELY OWNS a freshly allocated pool, there is no `Z` to
/// protect and the copy is pure duplication -- and that is precisely the
/// recursive-array elaboration's shape (`{ let buf = zeros(...); ...; buf }`),
/// whose double materialization cost 2x the resident footprint of every
/// `let rec` trajectory.
///
/// `scopeLets` is the enclosing let list: a block's own chain at module level,
/// or the FLATTENED function body inside a frame (genFuncBody's deepUnroll
/// dissolves the block, so the same shape arrives as an ordinary sibling let).
/// `selfId` is the alias binding's own id where it appears in that list, so it
/// is not counted as a rival name. Conditions:
///  (1) the staging let OWNS its pool (isFreshPoolForm: a view/alias value could
///      be sharing a USER binding's storage, which the copy exists to protect);
///  (2) no assign in the scope can have leaked that storage to a target OUTSIDE
///      the scope (an outer whole-array or row assign aliases pools; assigns
///      whose target is scope-internal die with the scope); and
///  (3) no OTHER let in the scope is a bare reference to the same staging let.
///      Two aliases of one pool would each act as its owner, so a write through
///      either would be visible through the other. A block chain cannot pose
///      this (it has exactly one value, which is not itself one of `scopeLets`);
///      a flattened function body can.
let canAliasStagingLet (scopeLets: (IRId * IRExpr) list) (selfId: IRId option) (srcId: IRId) : bool =
    let scopeIds = scopeLets |> List.map fst |> Set.ofList
    let srcOwnsPool =
        scopeLets |> List.exists (fun (id, v) -> id = srcId && isFreshPoolForm v)
    let assignLeaksSrc =
        scopeLets
        |> List.collect (fun (_, v) -> allSubExprs v)
        |> List.exists (fun e ->
            match e with
            | IRAssign (target, value) when Set.contains srcId (collectVarRefsIR value) ->
                let targetInScope =
                    match target with
                    | LVVar tid -> Set.contains tid scopeIds
                    | LVIndex (IRVar (tid, _), _) -> Set.contains tid scopeIds
                    | _ -> false
                not targetInScope
            | _ -> false)
    let rivalAlias =
        scopeLets
        |> List.exists (fun (id, v) ->
            Some id <> selfId
            && (match v with IRVar (vid, _) -> vid = srcId | _ -> false))
    Set.contains srcId scopeIds && srcOwnsPool && not assignLeaksSrc && not rivalAlias

// Whole-array mut reassignment as copy-into-place.
//
// `STG = t` inside a step function REBINDS the wrapper by default (`STG.data`
// repointed at t's pool, original abandoned), making a captured mut a per-call
// allocation sink: the RHS temp must be escape-seeded, so every step leaks a
// whole array. When the mut is the SOLE OWNER of its pool (no other binding
// names its wrapper, no sub-view, never returned), rebinding and copying are
// OBSERVATIONALLY IDENTICAL -- copy t's elements INTO the mut's existing pool
// (allocated once at main level, never scope-freed) and the RHS temp stays
// iteration-owned, dying with its own scope.
//
// Sole ownership is the entire safety argument, so the position analysis is an
// ALLOWLIST: the mut may occur only as (1) a whole-array assign target, (2) a
// FULL scalar-index base, (3) an `extents` read base, or (4) a reduce/prodsum
// operand. Everything else -- a bare `IRVar` anywhere, a PARTIAL index (a
// sub-view aliasing the pool), any unlisted form -- disqualifies. A missed
// disqualification silently changes numbers; a spurious one only leaves the
// leak in place.
//
// Scope is module-level `let mut` only: a block-local mut is already
// scope-freed at its own frame's exit, with far less to gain.

/// The per-slot literal extents of a plain-dense, statically-shaped array type.
/// None whenever ANY slot is symmetric, compact, ragged, dep-indexed, virtual,
/// dependent, rank>1, or has a non-literal extent -- in all of those the pool
/// cardinality is not the product of the extents, so neither the shape-identity
/// test nor the `std::copy_n` count would be sound.
let internal staticDenseExtents (at: IRArrayType) : int list option =
    if not (isFreeableDenseArrayType at) || at.IsVirtual then None
    else
        let slots =
            at.IndexTypes |> List.map (fun ix ->
                if ix.Rank = 1 && ix.Symmetry = SymNone && ix.IxKind = IxKPlain
                   && List.isEmpty ix.Dependencies then
                    match ix.Extent with
                    | IRLit (IRLitInt n) when n > 0L -> Some (int n)
                    | _ -> None
                else None)
        if List.isEmpty slots || slots |> List.exists Option.isNone then None
        else Some (slots |> List.map Option.get)

/// Values that emit as a REAL C++ array variable owning a fresh pool under the
/// binding's own name -- so `pool_base(<name>.data)` is meaningful. Note the
/// deliberate absence of a bare `IRApplyCombinator` / `IRComposeApply`: those are
/// DEFERRED (their statement arms emit nothing), so the name does not exist yet.
let internal isMaterializedFreshArray (v: IRExpr) : bool =
    match v with
    | IRCompute inner -> isFreshPoolForm inner
    | IRArrayLit _ -> true
    | IRMask _ | IRSort _ | IRUnique _ | IRIntersect _ | IRUnion _
    | IRTranspose _ | IRDecompact _ | IRStack _ | IRJoin _ | IRGram _ | IRGramApply _ | IRMatmul _ | IRSolve _
    | IRArrayNegate _ | IRArrayConjugate _ -> true
    | _ -> false

/// Every `let` value reachable from an expression, at any nesting depth.
let rec internal letValuesIn (e: IRExpr) : (IRId * IRExpr) list =
    let here = match e with IRLet (id, v, _) -> [(id, v)] | _ -> []
    match e with
    | ExprShape (children, _) -> here @ (children |> List.collect letValuesIn)

/// Does `IRVar(mid)` occur anywhere in `e` OUTSIDE the four blessed positions?
/// See the allowlist rationale above; the trailing generic arm recurses through
/// ExprShape, and reaching a bare `IRVar(mid)` there IS the disqualification.
let rec internal occursOutsideBlessedPositions (mid: IRId) (e: IRExpr) : bool =
    let scan (c: IRExpr) = occursOutsideBlessedPositions mid c
    // A slot in which a BARE `IRVar(mid)` is a transient read: accept it there,
    // but keep scanning any other expression that occupies the slot.
    let blessed (c: IRExpr) =
        match c with
        | IRVar (id, _) when id = mid -> false
        | _ -> scan c
    match e with
    | IRVar (id, _) -> id = mid
    | IRAssign (target, value) ->
        (match target with
         | LVVar _ -> false                              // the whole-array write itself
         | LVIndex (b, idxs) -> blessed b || (idxs |> List.exists scan)
         | _ -> scan target)
        || scan value
    | IRIndex (b, idxs, _) ->
        // A FULL read yields a scalar and copies the cell out; a PARTIAL read
        // yields a sub-view that ALIASES the pool (memfree/016's `brow = STG(0)`).
        let isFullRead = (inferExprType e).IsIRTScalar
        (if isFullRead then blessed b else scan b) || (idxs |> List.exists scan)
    | IRExtent (b, _) -> blessed b
    | IRReduce (a, k, i) ->
        blessed a || scan k || (match i with Some x -> scan x | None -> false)
    | IRReduceCompute (c, k, i) -> blessed c || scan k || scan i
    | IRProdSum args -> args |> List.exists blessed
    | ExprShape (children, _) -> children |> List.exists scan

/// Module-level mut array bindings eligible for copy-into-place, mapped to the
/// static element count of their pool. Installed alongside computeFreshReturnFacts
/// (it needs the facts for `isFreshPoolForm`'s IRApp arm, so it must run after).
let computeCopyInPlaceMuts (modul: IRModule) : Map<IRId, int> =
    let exprs =
        (modul.Bindings |> List.map (_.Value))
        @ (modul.Functions |> List.map (_.Body))
    // Both whole-module walks are LAZY: a program with no mut array bindings --
    // the overwhelming majority -- must not pay a tree traversal for a candidate
    // set that turns out empty.
    let allValues =
        lazy ((modul.Bindings |> List.map (fun b -> (b.Id, b.Value)))
              @ (exprs |> List.collect letValuesIn))
    let valueOf (id: IRId) =
        allValues.Force () |> List.tryPick (fun (i, v) -> if i = id then Some v else None)
    let subs = lazy (exprs |> List.collect allSubExprs)
    // (a) array-typed, dense, statically shaped, and owning a pool of its own.
    let candidates =
        modul.Bindings
        |> List.choose (fun b ->
            if not b.IsMutable then None
            else
                match b.Type with
                | ArrayElem at ->
                    match staticDenseExtents at with
                    | Some dims ->
                        let ownsPool =
                            Map.containsKey b.Id modul.RandomInits
                            || (match b.Value with
                                // `let mut a = Z` deep-copies (site 6), so the
                                // binding owns the copy, not Z's pool.
                                | IRVar _ -> true
                                | v -> isMaterializedFreshArray v)
                        if ownsPool then Some (b.Id, (at, dims)) else None
                    | None -> None
                | _ -> None)
    // (c) every whole-array assign to it takes a plain IRVar of a MATERIALIZED
    // array with identical static shape and element type.
    let assignsOk (mid: IRId) (at: IRArrayType) (dims: int list) =
        let rhss =
            subs.Force ()
            |> List.choose (fun e ->
                match e with
                | IRAssign (LVVar tid, value) when tid = mid -> Some value
                | _ -> None)
        not (List.isEmpty rhss)
        && rhss |> List.forall (fun v ->
            match v with
            | IRVar (rid, _) ->
                (match inferExprType v with
                 | ArrayElem rat ->
                     staticDenseExtents rat = Some dims && rat.ElemType = at.ElemType
                 | _ -> false)
                && (match valueOf rid with
                    | Some rv -> isMaterializedFreshArray rv
                    | None -> false)
            | _ -> false)
    candidates
    |> List.choose (fun (mid, (at, dims)) ->
        // (b) sole ownership: no alias-shaped occurrence anywhere in the module.
        if exprs |> List.exists (occursOutsideBlessedPositions mid) then None
        elif not (assignsOk mid at dims) then None
        else Some (mid, dims |> List.fold (*) 1))
    |> Map.ofList

/// Which scope a frame is being computed for. A function scope may have a return
/// expression whose reachable ids must be pinned; a loop body never returns.
type ScopeKind =
    | FuncScope of retExpr: IRExpr option
    | LoopScope

/// Binding ids in this scope whose allocations must NOT be freed at scope exit.
/// Seeds are intentionally over-approximate (a missed seed frees too early; an
/// extra seed only leaks), then propagated backwards through every non-barrier
/// let so that an escaping value pins whatever storage it may alias.
let computeScopeEscapes (ctx: CodeGenContext) (kind: ScopeKind) (scopeLets: (IRId * IRExpr) list) : Set<IRId> =
    // Deep, so an assign or a callable reference buried in a nested loop body,
    // match arm, or ApplyInfo subtree cannot be missed.
    let subs = scopeLets |> List.collect (fun (_, v) -> allSubExprs v)
    // Seed 1 -- whole-array reassignment. `a = someOther;` makes a.data alias
    // someOther's pool, so BOTH sides must survive: the target because its
    // original pool is no longer the one it names, the RHS because the target now
    // shares it. Widened past LVVar-only to every assign shape with a
    // not-obviously-scalar RHS, which also covers `rec(i) = <row>`.
    let assignSeeds =
        subs |> List.collect (fun e ->
            match e with
            // Copy-in-place: an assign that will compile to `std::copy_n` into the
            // target's OWN pool creates no aliasing at all -- that is the whole
            // point of the transform -- so it seeds NEITHER side. The RHS temp
            // becomes ordinary scope-owned storage and is freed at scope exit;
            // the target keeps the single pool it has always had.
            | IRAssign (target, value) when (copyInPlaceAssign target value).IsSome -> []
            | IRAssign (target, value) ->
                let targetIds = match target with LVVar tid -> [tid] | _ -> []
                let valueIds =
                    match inferExprType value with
                    | IRTScalar _ | IRTUnit -> []
                    | _ -> collectVarRefsIR value |> Set.toList
                targetIds @ valueIds
            | _ -> [])
    // Seed 2 -- captures. A step function's `[&]` capture of an outer mut (and
    // any kernel lambda's captures) keeps reading that storage after the scope's
    // own use of it ends.
    let captureSeeds =
        subs |> List.collect (fun e ->
            match resolveCallable e with
            | Some c -> c.Captures |> List.map (_.Id)
            | None -> [])
    // Seed 3 -- provider write sources. Program-global and cheap; a written array
    // is flattened and handed to a provider writer emitted elsewhere.
    let providerSeeds =
        ctx.ProviderWrites |> Map.toList |> List.map (fun (_, spec) -> spec.SourceId)
    // Function-scope return seed. A scalar/unit return provably cannot carry
    // array storage, so it contributes nothing -- without that narrowing
    // `return reduce(B, +)` would pin every `|> compute` temporary in the helper
    // and the whole feature would be inert inside functions. The two LIFTED
    // return forms are skipped too: they are handled by suppressing the `__retN`
    // NAME at the splice point, and seeding their inputs would defeat the point.
    let retSeeds =
        match kind with
        | LoopScope | FuncScope None -> []
        | FuncScope (Some retExpr) ->
            match retExpr with
            | IRCompute (IRApplyCombinator _) | IRArrayLit _ -> []
            // `return f(x, y, z)` where f is a FreshPool callee: the returned
            // wrapper is a fresh pool with its own extents, so it can alias
            // none of the argument bindings -- and the fallthrough return arm
            // evaluates the call into a local (`auto __rv = f(...);`) BEFORE
            // the scope frees are emitted, so x/y/z are dead by the time they
            // are freed. Seeding them only pinned one array per helper call
            // (measured on 09_qg_atmosphere: three 64x64 fields leaked per
            // H_single invocation, every timestep). NotFresh callees keep the
            // wide seeding: their return may hand back an argument itself.
            | IRApp (f, _, _) when freshReturnOf f = FreshPool -> []
            // An INTERIOR VIEW of a scope-local array (`return traj(k)`). The
            // return arm materializes the slice into its own pool before these
            // frees are emitted, so the base is ordinary scope-owned storage
            // from here on. Seeding it would pin the WHOLE array for the life
            // of the program -- and since a function frame runs per CALL, that
            // is an unbounded leak, not a one-off (measured: a 10000x4
            // trajectory per `integrate` invocation). Paired with
            // genFuncBodyScoped's matching arm through returnedInteriorView;
            // narrowing here WITHOUT that arm would be a use-after-free.
            | IRIndex _ when (returnedInteriorView scopeLets retExpr).IsSome -> []
            | _ ->
                match inferExprType retExpr with
                | IRTScalar _ | IRTUnit -> []
                | _ -> collectVarRefsIR retExpr |> Set.toList
    // A SCALAR-typed let holds no storage, so nothing it read can be aliased
    // through it: propagation stops there exactly as at a fresh pool. Without
    // this barrier a kernel capturing `m` in `let m = reduce(y, (+)) / n`
    // seeded m (capture seed), propagation walked into m's value, and y --
    // merely READ to compute a scalar -- was pinned and leaked on every call
    // (measured: the demean / normalize shape leaked its input-sized pool
    // per invocation).
    let scalarValued (value: IRExpr) =
        match inferExprType value with
        | IRTScalar _ | IRTUnit -> true
        | _ -> false
    let rec propagate (acc: Set<IRId>) =
        let acc' =
            scopeLets |> List.fold (fun s (id, value) ->
                if Set.contains id s && not (isFreshPoolForm value) && not (scalarValued value)
                then Set.union s (collectVarRefsIR value)
                else s) acc
        // Scratch reuse: an escaping reuser's storage IS its donor's, so the
        // donor escapes with it (chains resolve to the root by iteration).
        let acc'' =
            Blade.Types.PoolReuseTable.donors ()
            |> Map.fold (fun s reuser donor -> if Set.contains reuser s then Set.add donor s else s) acc'
        if acc'' = acc then acc else propagate acc''
    propagate (Set.ofList (assignSeeds @ captureSeeds @ providerSeeds @ retSeeds))

/// Hoist FreshPool-returning calls out of ARGUMENT position into fresh lets.
/// `g(f(x))` evaluates f's fresh array as a C++ temporary no let ever names,
/// so the scope tracker cannot register it and its pool leaks every call
/// (measured on 09_qg_atmosphere: `__spectra_1(uhat(ll, ph))` and
/// `__spectra_2(flux(u, 0.0, q))` each dropped a 64x64 field per timestep).
/// Hoisted into `let t = f(x)`, the binding takes the ordinary site-3/3b
/// registration and scope-exit free. Guards are exactly site 3's: FreshPool
/// callee, dense + nullptr, no symmetry -- anything else stays inline (and
/// keeps leaking rather than risking a wrong free). Only IRApp trees are
/// walked: a lambda in argument position must keep its body expression-shaped.
/// C++ argument evaluation order is unspecified, so serializing sibling
/// arguments through lets changes nothing observable.
let hoistFreshPoolCallArgs (builder: IRBuilder) (e: IRExpr) : (IRId * IRExpr) list * IRExpr =
    let hoistable (arg: IRExpr) =
        match arg with
        | IRApp (f, _, _) when freshReturnOf f = FreshPool ->
            (match inferExprType arg with
             | ArrayElem at ->
                 isFreeableDenseArrayType at
                 && classifyOutputStorage (inferExprType arg) = AllocDense
                 && not (hasRealSymmetry (buildSymmVec (inferExprType arg)))
             | _ -> false)
        | _ -> false
    let rec go (e: IRExpr) : (IRId * IRExpr) list * IRExpr =
        match e with
        | IRApp (f, args, ty) ->
            let processed = args |> List.map go
            let innerLets = processed |> List.collect fst
            let (extraLets, finalArgs) =
                processed |> List.fold (fun (ls, acc) (_, a) ->
                    if hoistable a then
                        let tmp = builder.FreshId()
                        (ls @ [(tmp, a)], acc @ [IRVar (tmp, inferExprType a)])
                    else (ls, acc @ [a])) ([], [])
            (innerLets @ extraLets, IRApp (f, finalArgs, ty))
        | _ -> ([], e)
    go e

/// One tracked allocation, with everything needed to free it recorded at
/// REGISTRATION time. TemplateArgs in particular is stored verbatim rather than
/// recomputed at scope exit, so a later hoist cannot make the free's template
/// arguments drift from the allocation's.
type TrackedAlloc =
    /// allocate<> / allocate_strict<> pool + pointer skeleton, freed by the
    /// mirrored runtime routine. OwnedExtentsName is Some only where this site
    /// itself did `new size_t[R]` (genApplyCombinator and the fused leaves);
    /// static-constexpr and borrowed-alias extents are None.
    | PoolAlloc of
        ArrayName: string *
        Routine: string *
        TemplateArgs: string *
        OwnedExtentsName: string option *
        OwnerBindingId: IRId option
    /// Streamed `_fb_p` fiber buffers: plain `delete[] <name>;`.
    /// Registered by the streamedNestSetup call sites when a frame is live.
    | RawAlloc of Name: string * OwnerBindingId: IRId option
    /// An `Array<T,1> N = { new T[n], ext }` whose backing is a raw `new[]`
    /// rather than an allocate<> pool (the mask / sort / unique / union /
    /// intersect family): `delete[] N.data;`. The tracked NAME is the wrapper
    /// `N`, not `N.data`, so the return-suppression token test in genFuncBody
    /// still recognises `return N;`. OwnedExtentsName mirrors PoolAlloc's --
    /// Some where the site did its own `new size_t[R]` -- and is deleted under
    /// the SAME spare/free decision as the backing, which is the whole reason
    /// it is a field here instead of a separate RawAlloc registration.
    | RawArrayData of Name: string * OwnedExtentsName: string option * OwnerBindingId: IRId option
    /// Ragged / compound teardown. Unlike PoolAlloc there is no template-argument
    /// mirror to preserve: the ragged and compound layouts have no per-level span
    /// formula, so the free is a fixed runtime call whose arguments are all NAMES
    /// chosen at the emission site. Storing the fully rendered argument list
    /// (rather than a wrapper name plus a convention) is what keeps a site that
    /// shares its input's tables from being freed as though it owned them.
    /// Name is the wrapper (for suppression/token tests); Routine is the
    /// unqualified nested_array_utilities function; Args the complete call text.
    | ShapedAlloc of
        Name: string *
        Routine: string *
        Args: string *
        OwnerBindingId: IRId option

let internal trackedAllocName (t: TrackedAlloc) : string =
    match t with
    | PoolAlloc (n, _, _, _, _) -> n
    | RawAlloc (n, _) -> n
    | RawArrayData (n, _, _) -> n
    | ShapedAlloc (n, _, _, _) -> n

let internal trackedAllocOwner (t: TrackedAlloc) : IRId option =
    match t with
    | PoolAlloc (_, _, _, _, o) -> o
    | RawAlloc (_, o) -> o
    | RawArrayData (_, _, o) -> o
    | ShapedAlloc (_, _, _, o) -> o

type AllocScopeKind = SFunc | SLoop

/// One live scope's bookkeeping. Mutable because registration happens deep inside
/// string-building code that cannot thread a return value back out; the frame is
/// reached through the AsyncLocal stack instead.
type AllocScope = {
    Kind: AllocScopeKind
    /// Newest FIRST (prepend). Plain iteration at pop time is therefore
    /// registration-REVERSE order, which is what guarantees a result that borrows
    /// an operand's `.extents` is freed before that operand.
    mutable Allocs: TrackedAlloc list
    mutable Escapes: Set<IRId>
    mutable SuppressNames: Set<string>
    mutable CurrentOwner: IRId option
    /// Streamed fiber buffers (`_fb_p*`) DECLARED while this frame was live.
    /// Their C++ names die with the frame's closing brace, so the pop (and the
    /// exception-path truncate) retires them from streamBufDeclsCell -- a later
    /// nest in a different scope must re-declare, not reference a dead name.
    mutable StreamBufNames: Set<string>
}

let internal allocScopeStackStorage =
    System.Threading.AsyncLocal<AllocScope list ref>()

let allocScopeStackCell () : AllocScope list ref =
    let v = allocScopeStackStorage.Value
    if isNull (box v) then
        let fresh = ref []
        allocScopeStackStorage.Value <- fresh
        fresh
    else v

let resetAllocScopeStack () : unit =
    (allocScopeStackCell ()).Value <- []

let currentAllocScope () : AllocScope option =
    match (allocScopeStackCell ()).Value with
    | top :: _ -> Some top
    | [] -> None

let allocScopeDepth () : int = (allocScopeStackCell ()).Value.Length

/// Drop any frames left above `depth`. A codegen path that raises (a
/// BladeDiagnosticException from a provider or backend limit) must not leave a
/// stale frame active: the NEXT module-level binding's allocations would register
/// into it and its frees would be emitted in the wrong scope.
let truncateAllocScopeStack (depth: int) : unit =
    let cell = allocScopeStackCell ()
    let n = cell.Value.Length
    if n > depth then
        // Frames dropped on an exception path still retire their streamed
        // fiber-buffer names from the program-wide dedup set (see
        // AllocScope.StreamBufNames) -- the declarations they cover were never
        // (or will never be) emitted into a scope the next binding can see.
        let sc = streamBufDeclsCell ()
        for f in cell.Value |> List.take (n - depth) do
            sc.Value <- Set.difference sc.Value f.StreamBufNames
        cell.Value <- cell.Value |> List.skip (n - depth)

let pushAllocScope (kind: AllocScopeKind) (escapes: Set<IRId>) : unit =
    let cell = allocScopeStackCell ()
    let frame = { Kind = kind; Allocs = []; Escapes = escapes
                  SuppressNames = Set.empty; CurrentOwner = None; StreamBufNames = Set.empty }
    cell.Value <- frame :: cell.Value

let popAllocScope () : AllocScope option =
    let cell = allocScopeStackCell ()
    match cell.Value with
    | top :: rest -> cell.Value <- rest; Some top
    | [] -> None

/// The free-side mirror of emitAllocRhs (217-247), case for case, reusing
/// hoistSymmDecl with the identical `%s_anti` / `%s_strict` names keyed off
/// extentsName (the hoist collector is idempotent per distinct decl, so
/// re-hoisting the same mask is safe). Returns (routine, templateArgs); an
/// AllocUnsupported spec returns Error and the caller skips registration
/// entirely -- that allocation site emitted a `#error`, so there is nothing valid
/// to free.
let deallocArgsFor
        (spec: AllocSpec)
        (elemType: string) (rank: int) (symmArg: string) (extentsName: string)
        : Result<string * string, string> =
    match spec with
    | AllocAntisymmetric ->
        let allOnes = List.replicate rank 1
        let maskName = hoistSymmDecl ($"{extentsName}_anti") allOnes
        Ok ("deallocate", $"typename promote<{elemType}, {rank}>::type, {maskName}, false")
    | AllocDense | AllocSymmetric ->
        Ok ("deallocate", $"typename promote<{elemType}, {rank}>::type, {symmArg}")
    | AllocPerGroupStrict strictVec ->
        let strictName = hoistSymmDecl ($"{extentsName}_strict") strictVec
        Ok ("deallocate_strict", $"typename promote<{elemType}, {rank}>::type, {symmArg}, {strictName}")
    | AllocWreath _ ->
        // The wreath pool is a plain `new T[cells]`, freed by its own
        // `delete[]` registration (registerRawArrayData at the emit site), not
        // by `deallocate`'s skeleton walk -- there is no skeleton to walk.
        Error "Blade codegen: an OrbIdx (iterated-wreath) pool is a flat new[] buffer; it is freed by \
delete[] at its emit site, not through the skeleton deallocator"
    | AllocUnsupported reason ->
        Error ($"Blade codegen: unsupported storage has no representable free -- {reason}")

/// Record an allocation against the innermost live scope. An EMPTY stack (main's
/// top level) is a silent no-op -- that single rule is what keeps module bindings
/// alive for auto-print / EXPECT pins without a per-site guard.
let registerAlloc (t: TrackedAlloc) : unit =
    match currentAllocScope () with
    | None -> ()
    | Some frame ->
        let stamped =
            match t with
            | PoolAlloc (n, r, a, ex, _) -> PoolAlloc (n, r, a, ex, frame.CurrentOwner)
            | RawAlloc (n, _) -> RawAlloc (n, frame.CurrentOwner)
            | RawArrayData (n, ex, _) -> RawArrayData (n, ex, frame.CurrentOwner)
            | ShapedAlloc (n, r, a, _) -> ShapedAlloc (n, r, a, frame.CurrentOwner)
        frame.Allocs <- stamped :: frame.Allocs

/// The one call every allocation site makes. Template arguments are resolved
/// here, at the allocation, so the emitted free mirrors the emitted allocate
/// character for character. `extentsName` is consumed only for hoist naming, so
/// dense/`nullptr` sites may pass `<name>_extents` harmlessly.
let registerPoolAlloc
        (spec: AllocSpec) (elemType: string) (rank: int) (symmArg: string)
        (extentsName: string) (arrayName: string) (ownedExtents: string option) : unit =
    match currentAllocScope () with
    | None -> ()   // main top level: nothing is scope-freed
    | Some _ ->
        match deallocArgsFor spec elemType rank symmArg extentsName with
        | Ok (routine, args) -> registerAlloc (PoolAlloc (arrayName, routine, args, ownedExtents, None))
        | Error _ -> ()

/// Register the allocations a `materialize*Form` builder reported (site 7).
/// The template arguments come STRAIGHT from the descriptor -- the builder
/// already resolved SYMM / STRICT (hoisting the masks it needed), so this is a
/// character-for-character mirror of `EmitCpp.arrayAlloc` rather than a second
/// derivation from an AllocSpec. Statement-position consumers call this;
/// expression / IIFE consumers discard their descriptor list.
let registerMaterializedAllocs (ms: MaterializedAlloc list) : unit =
    match currentAllocScope () with
    | None -> ()   // main top level: nothing is scope-freed
    | Some _ ->
        for m in ms do
            match m with
            | MatPool (n, elem, rank, symm, strict, ownedExtents) ->
                let (routine, args) =
                    match strict with
                    | Some strictArg ->
                        ("deallocate_strict",
                         $"typename promote<{elem}, {rank}>::type, {symm}, {strictArg}")
                    | None ->
                        ("deallocate", $"typename promote<{elem}, {rank}>::type, {symm}")
                registerAlloc (PoolAlloc (n, routine, args, ownedExtents, None))
            | MatPoolSpec (n, spec, elem, rank, symm, extentsName, ownedExtents) ->
                match deallocArgsFor spec elem rank symm extentsName with
                | Ok (routine, args) -> registerAlloc (PoolAlloc (n, routine, args, ownedExtents, None))
                // The site emitted `#error`; there is no valid free for the pool.
                // The extents table IS valid (it was built before the refusal),
                // but a translation unit carrying `#error` never links, so
                // dropping it too keeps the two halves of a refused site
                // together rather than emitting a lone delete for a shape
                // nothing will read.
                | Error _ -> ()
            | MatRawData (n, ownedExtents) -> registerAlloc (RawArrayData (n, ownedExtents, None))
            | MatRawBuf n -> registerAlloc (RawAlloc (n, None))

/// Register a ragged/compound teardown (the W2 runtime routines). No
/// template-argument derivation: the emission site already knows the exact
/// call, and the rendered argument text is what keeps a site that SHARES its
/// input's tables (ragged map metadata, compound map idx) from being freed as
/// though it owned them. No-op with no live frame, exactly like
/// registerPoolAlloc (module bindings stay alive for auto-print / EXPECT pins).
let registerShapedAlloc (wrapperName: string) (routine: string) (args: string) : unit =
    match currentAllocScope () with
    | None -> ()
    | Some _ -> registerAlloc (ShapedAlloc (wrapperName, routine, args, None))

/// Register an allocation whose template arguments were emitted DIRECTLY through
/// `EmitCpp.arrayAlloc` (the CUDA / MPI host peels: their SYMM and STRICT masks
/// are hoisted locally, exactly like the materializer forms'). Mirrors arrayAlloc
/// argument for argument; `ownedExtents` is Some where the site did its own
/// `new size_t[R]`.
let registerArrayAlloc
        (arrayName: string) (elem: string) (rank: int)
        (symm: string) (strict: string option) (ownedExtents: string option) : unit =
    match currentAllocScope () with
    | None -> ()   // main top level: nothing is scope-freed
    | Some _ ->
        let (routine, args) =
            match strict with
            | Some strictArg ->
                ("deallocate_strict",
                 $"typename promote<{elem}, {rank}>::type, {symm}, {strictArg}")
            | None ->
                ("deallocate", $"typename promote<{elem}, {rank}>::type, {symm}")
        registerAlloc (PoolAlloc (arrayName, routine, args, ownedExtents, None))

/// Register the fiber destination buffers a streamedNestSetup call NEWLY
/// declared. Under a live frame each gets a scope-exit `delete[]`
/// (per-iteration recycle when the frame is a loop body) and is remembered for
/// dedup retirement at pop. With no frame (main top level) this is a no-op and
/// the buffer keeps its program lifetime. The buffers are plain host `new[]`
/// staging on every backend path, so the delete is uniformly safe.
let registerStreamBufDecls (names: string list) : unit =
    match currentAllocScope () with
    | None -> ()
    | Some frame ->
        for n in names do
            frame.StreamBufNames <- Set.add n frame.StreamBufNames
            registerAlloc (RawAlloc (n, None))

/// SCRATCH REUSE (Blade.Optimize.planPoolReuse, Types.PoolReuseTable): turn
/// `name`'s pool declaration -- `Array<E, R> name = { allocate<typename
/// promote<E, R>::type, nullptr>(ext), ext };`, the one line every dense
/// allocation site emits for a plan-admitted shape -- into an alias of the
/// donor's data with the reuser's own extents table. Anchored on the NAME,
/// so it is indifferent to which emitter wrote the line. The flag says
/// whether a line matched: the caller spares a scope free ONLY then, so an
/// unexpected declaration shape keeps its own pool and its own free rather
/// than leaking.
let rewritePoolAlias (lines: string list) (name: string) (donor: string) : string list * bool =
    let pat =
        System.Text.RegularExpressions.Regex(
            @"^(\s*)Array<(.+?), (\d+)> " + System.Text.RegularExpressions.Regex.Escape name
            + @" = \{ allocate<typename promote<.+?>::type, nullptr>\((\w+)\), (\w+) \};\s*$")
    let mutable matched = false
    let out =
        lines |> List.map (fun l ->
            let m = pat.Match l
            if m.Success && not matched then
                matched <- true
                $"{m.Groups.[1].Value}Array<{m.Groups.[2].Value}, {m.Groups.[3].Value}> {name} = {{ {donor}.data, {m.Groups.[4].Value} }}; /* pool reuse: {name} takes {donor}'s dead pool */"
            else l)
    (out, matched)

/// Exempt one emitted C++ name from this scope's frees (used for the lifted
/// `__retN` return temporaries, whose storage leaves with the return value).
let suppressAllocName (n: string) : unit =
    match currentAllocScope () with
    | None -> ()
    | Some frame -> frame.SuppressNames <- Set.add n frame.SuppressNames

/// A cursor into the live frame's registration list, and its companion that
/// spares EVERYTHING registered after it (`suppressAllocsSince`).
///
/// Why the pair exists rather than another `suppressAllocName` call. A RETURN
/// position lifts its value into a `__retN` and must spare that value's storage
/// from the scope's frees, but "that value's storage" is not always one pool
/// named `__retN`: `materializeEighForm` declares TWO under derived names
/// (`__retN__q` / `__retN__lam`) and binds `__retN` to a make_tuple of the two
/// wrappers, and `genSequenceBinding` declares one per child (`__retN_0`, ...).
/// Sparing only the binding name emitted `deallocate(...)` for the real pools
/// and then returned wrappers pointing at freed memory -- a use-after-free the
/// caller reads as garbage (eigh reproduces it whenever the BLAS route is live;
/// the synthesized Jacobi fallback hides it by never taking this arm).
///
/// Naming conventions are not a sound basis for the spare decision -- every
/// multi-pool form invents its own suffixes, and the next one to be added would
/// reintroduce the bug silently. What IS sound is the registration record
/// itself: an allocation emitted while the return binding was being rendered
/// belongs to the returned value. Mark the list, render, spare the delta.
///
/// The trade this makes is deliberate and one-directional. A form that
/// registers a genuine SCRATCH pool inside its own emission would now leak that
/// pool for the frame's lifetime instead of freeing it. A leak bounded by one
/// call is strictly preferable to handing the caller a dangling pointer, and
/// the frame's other allocations (everything registered before the mark) are
/// unaffected, so nothing that used to be freed on the ordinary let path stops
/// being freed.
///
/// The FRAME is captured with the cursor rather than re-read at the suppress
/// call: an emitter that pushed a nested scope and left it live would otherwise
/// stamp the suppression onto the wrong frame.
let allocRegistrationMark () : (AllocScope * int) option =
    match currentAllocScope () with
    | None -> None
    | Some frame -> Some (frame, List.length frame.Allocs)

let suppressAllocsSince (mark: (AllocScope * int) option) : unit =
    match mark with
    | None -> ()
    | Some (frame, before) ->
        let now = List.length frame.Allocs
        // Newest-FIRST list, so the delta is the prefix.
        if now > before then
            for a in frame.Allocs |> List.truncate (now - before) do
                frame.SuppressNames <- Set.add (trackedAllocName a) frame.SuppressNames

let registeredAllocNames () : string list =
    match currentAllocScope () with
    | None -> []
    | Some frame -> frame.Allocs |> List.map trackedAllocName

/// Stamp the owning binding id for allocations emitted from here on. Set ONLY by
/// the two scope drivers (genFuncBody's per-let fold, genForRangeBinding's body
/// fold) -- never re-stamped by genBinding -- so every allocation emitted while
/// one source-level let is being rendered (including __lhs / __rhs / __s1 /
/// __gbody sub-temporaries minted with fresh ids) carries THAT let's id.
/// Suppression is therefore all-or-nothing per let, which is what keeps a result
/// borrowing an operand's `.extents` pointer from outliving that operand's free.
let setAllocOwner (owner: IRId option) : unit =
    match currentAllocScope () with
    | None -> ()
    | Some frame -> frame.CurrentOwner <- owner

/// Whole-identifier-token occurrence test. Used only by genFuncBody's fallthrough
/// return arm, where the returned expression is already rendered to text and the
/// question is whether it NAMES a registered allocation (`return r;`,
/// `return std::make_tuple(r0, r1);`) -- a substring test would false-positive on
/// `r_extents` or `rows`.
let containsIdentToken (text: string) (name: string) : bool =
    System.Text.RegularExpressions.Regex.IsMatch(
        text,
        "(?<![A-Za-z0-9_])" + System.Text.RegularExpressions.Regex.Escape(name) + "(?![A-Za-z0-9_])")

/// Render one frame's frees at `ind`. Iteration is over the newest-first
/// list, i.e. registration-reverse order. An allocation is skipped when its
/// owning let escapes or its name was suppressed. An empty survivor list
/// emits no lines at all, so unaffected programs are byte-identical.
let internal renderFrameFrees (ind: string) (frame: AllocScope) : string list =
    frame.Allocs
    |> List.collect (fun a ->
        let ownerEscapes =
            match trackedAllocOwner a with
            | Some oid -> Set.contains oid frame.Escapes
            | None -> false
        if ownerEscapes || Set.contains (trackedAllocName a) frame.SuppressNames then []
        else
            match a with
            | PoolAlloc (n, routine, args, ownedExtents, _) ->
                [ $"{ind}{routine}<{args}>({n}.data, {n}.extents);" ]
                @ (match ownedExtents with
                   | Some ex -> [$"{ind}delete[] {ex};"]
                   | None -> [])
            | RawAlloc (n, _) -> [$"{ind}delete[] {n};"]
            | RawArrayData (n, ownedExtents, _) ->
                [ $"{ind}delete[] {n}.data;" ]
                @ (match ownedExtents with
                   | Some ex -> [$"{ind}delete[] {ex};"]
                   | None -> [])
            | ShapedAlloc (_, routine, args, _) ->
                [ $"{ind}nested_array_utilities::{routine}({args});" ])

/// Pop the innermost scope and render its frees at `ind`.
let popAllocScopeFrees (ind: string) : string list =
    match popAllocScope () with
    | None -> []
    | Some frame ->
        // Retire this frame's streamed fiber-buffer declarations from the
        // program-wide dedup set: their C++ names die at this scope's closing
        // brace, so a later nest must re-declare its own buffer.
        let sc = streamBufDeclsCell ()
        sc.Value <- Set.difference sc.Value frame.StreamBufNames
        renderFrameFrees ind frame

/// Render the innermost frame's frees at `ind` WITHOUT popping -- the early-exit
/// path of a `while`-guarded rec-array loop (IRBreakIf) leaves the loop before
/// the bottom-of-body frees run, so the break emits the frees accumulated SO FAR
/// this iteration and then breaks. The frame stays live (the non-breaking path
/// still frees at the bottom), and streamed fiber-buffer declarations are NOT
/// retired (their C++ names remain in scope past the break).
let peekAllocScopeFrees (ind: string) : string list =
    match currentAllocScope () with
    | None -> []
    | Some frame -> renderFrameFrees ind frame

/// Generate code to allocate and initialize an array from literal values
let genArrayLiteral (ctx: CodeGenContext) (varName: string) (elements: IRExpr list) (arrType: IRArrayType) : string list =
    let ind = indentStr ctx
    let elemType = elemTypeToCpp arrType.ElemType
    let rank = arrayRank arrType
    
    // Ragged literal path (RaggedIdx-tagged inner index): emits offsets table,
    // flat backing buffer, and row-pointer table from the literal's structure.
    // Storage is all STACK-allocated in the enclosing function's scope, which
    // works only while the literal doesn't outlive its declaration scope
    // (construct-print-exit in main()); returning it or storing it longer-lived
    // needs upgrading to `new T[total]` / `new T*[n]` heap allocation -- local
    // to this case, doesn't affect the type system.
    //
    // DepIdx-annotated literal path (DepIdx-inner-tagged index): the lens table
    // comes from evaluating the inner record's Extent formula (e.g. `Idx<3-i>`)
    // for each i; once computed, the runtime layout matches ragged
    // (`_lens`/`_offsets`/flat backing/row pointers), so downstream codegen uses
    // the same machinery. evalDepIdxExtent only reduces static arithmetic with
    // the outer IRVar substituted for `i`; formulas referencing free/runtime
    // values surface as a codegen error (runtime-extent formulas deferred).
    if isDepIdxArrayType arrType then
        // Find the outer record (its IRId is the one substituted for `i` in
        // the inner extent) and the inner record (carries the formula).
        let outerOpt =
            arrType.IndexTypes |> List.tryFind (fun idx -> idx.IxKind = IxKDepOuter)
        let innerOpt =
            arrType.IndexTypes |> List.tryFind (fun idx -> idx.IxKind = IxKDepInner)
        match outerOpt, innerOpt with
        | Some outer, Some inner ->
            let outerExtentOpt = tryEvalIntIR outer.Extent
            match outerExtentOpt with
            | None ->
                [refusalErrorLine ind ($"Blade codegen: DepIdx outer extent is not a compile-time integer for binding '{varName}'")]
            | Some n ->
                // Evaluate the inner formula for each i in [0..n).
                let lenResults =
                    [0 .. (int n) - 1]
                    |> List.map (fun i -> evalDepIdxExtent outer.Id i inner.Extent)
                if lenResults |> List.exists Option.isNone then
                    [refusalErrorLine ind ($"Blade codegen: DepIdx inner extent formula not statically evaluable for binding '{varName}' (runtime-extent formulas are not yet supported)")]
                else
                    let lens = lenResults |> List.map Option.get
                    // Verify literal row counts match the formula-computed lens.
                    let actualRowLengths = computeRaggedRowLengths elements
                    let mismatch =
                        actualRowLengths.Length <> lens.Length ||
                        List.zip actualRowLengths lens |> List.exists (fun (a, b) -> a <> b)
                    if mismatch then
                        let expected = lens |> List.map string |> String.concat ", "
                        let actual = actualRowLengths |> List.map string |> String.concat ", "
                        [refusalErrorLine ind ($"Blade codegen: DepIdx literal row lengths [{actual}] do not match formula-computed lens [{expected}] for binding '{varName}'")]
                    else
                        let total = lens |> List.sum
                        let allValues = extractLiteralValues (IRArrayLit (elements, arrType))
                        if allValues.Length <> total then
                            [refusalErrorLine ind ($"Blade codegen: DepIdx literal value count ({allValues.Length}) does not match sum of formula-computed lens ({total}) for binding '{varName}'")]
                        else
                            // Layout is identical to ragged from here on.
                            let nRows = lens.Length
                            let lensList = lens |> List.map string |> String.concat ", "
                            let offsets = lens |> List.scan (fun acc len -> acc + len) 0
                            let offsetsList = offsets |> List.map string |> String.concat ", "
                            // Float elements need round-trip literals (see
                            // floatToCppLiteral); integral elements keep the
                            // bare spelling (a `.0` suffix would be a C++
                            // narrowing error in the braced initializer).
                            let renderFlat (v: float) =
                                if elemType.Contains "double" || elemType.Contains "float"
                                then floatToCppLiteral v else sprintf "%g" v
                            let flatValues = allValues |> List.map renderFlat |> String.concat ", "
                            let extentsDecl = $"{ind}static constexpr const size_t {varName}_extents[1] = {{{nRows}}};"
                            let lensDecl = $"{ind}static constexpr const size_t {varName}_lens[{nRows}] = {{{lensList}}};"
                            let offsetsDecl = $"{ind}static constexpr const size_t {varName}_offsets[{nRows + 1}] = {{{offsetsList}}};"
                            let flatDecl = $"{ind}{elemType} {varName}__flat[{total}] = {{{flatValues}}};"
                            // Row pointer array (stack-allocated). The Ragged
                            // wrapper holds a pointer to this array.
                            let rowPtrsDecl = $"{ind}{elemType}* {varName}__rows[{nRows}];"
                            let rowPtrsInit =
                                [ $$"""{{ind}}for (size_t __ri = 0; __ri < {{nRows}}; __ri++) {"""
                                  // Reads from the static-constexpr global declared above; the wrapper isn't yet constructed.
                                  $"{ind}    {varName}__rows[__ri] = &{varName}__flat[{varName}_offsets[__ri]];"
                                  $"{ind}}}" ]
                            // Wrap into Ragged<T>: data + extents + lens + offsets.
                            let wrapperDecl = $"{ind}Ragged<{elemType}> {varName} = {{ {varName}__rows, {varName}_extents, {varName}_lens, {varName}_offsets }};"
                            [extentsDecl; lensDecl; offsetsDecl; flatDecl; rowPtrsDecl] @ rowPtrsInit @ [wrapperDecl]
        | _ ->
            [refusalErrorLine ind ($"Blade codegen: DepIdx array type missing outer or inner record for binding '{varName}' (typechecker bug)")]
    elif isRaggedArrayType arrType then
        let rowLengths = computeRaggedRowLengths elements
        let n = rowLengths.Length
        let total = rowLengths |> List.sum
        // Flat list of all element values, in row-major order
        let allValues = extractLiteralValues (IRArrayLit (elements, arrType))
        if allValues.Length <> total then
            // Sanity check: number of leaf values must match sum of row lengths.
            [refusalErrorLine ind ($"Blade codegen: ragged literal value count ({allValues.Length}) does not match sum of row lengths ({total}) for binding '{varName}'")]
        else
            let lensList = rowLengths |> List.map string |> String.concat ", "
            let offsets =
                rowLengths |> List.scan (fun acc len -> acc + len) 0
            let offsetsList = offsets |> List.map string |> String.concat ", "
            // Same float/integral literal split as the DepIdx branch above.
            let renderFlat (v: float) =
                if elemType.Contains "double" || elemType.Contains "float"
                then floatToCppLiteral v else sprintf "%g" v
            let flatValues = allValues |> List.map renderFlat |> String.concat ", "
            let extentsDecl = $"{ind}static constexpr const size_t {varName}_extents[1] = {{{n}}};"
            let lensDecl = $"{ind}static constexpr const size_t {varName}_lens[{n}] = {{{lensList}}};"
            let offsetsDecl = $"{ind}static constexpr const size_t {varName}_offsets[{n + 1}] = {{{offsetsList}}};"
            let flatDecl = $"{ind}{elemType} {varName}__flat[{total}] = {{{flatValues}}};"
            // Row pointer array (stack-allocated, separate name from the
            // wrapper so they don't collide). The Ragged<T> wrapper bundles
            // it with the lens/offsets/extents.
            let rowPtrsDecl = $"{ind}{elemType}* {varName}__rows[{n}];"
            let rowPtrsInit =
                [ $$"""{{ind}}for (size_t __ri = 0; __ri < {{n}}; __ri++) {"""
                  // Reads from the static-constexpr `<name>_offsets` global
                  // declared just above. The Ragged wrapper itself isn't yet
                  // in scope -- it's constructed AFTER this loop runs.
                  $"{ind}    {varName}__rows[__ri] = &{varName}__flat[{varName}_offsets[__ri]];"
                  $"{ind}}}" ]
            let wrapperDecl = $"{ind}Ragged<{elemType}> {varName} = {{ {varName}__rows, {varName}_extents, {varName}_lens, {varName}_offsets }};"
            [extentsDecl; lensDecl; offsetsDecl; flatDecl; rowPtrsDecl] @ rowPtrsInit @ [wrapperDecl]
    elif arrType.IndexTypes |> List.exists (fun ix ->
             ix.Rank >= 2 &&
             (match ix.Symmetry with
              | SymSymmetric | SymAntisymmetric | SymHermitian -> true
              | SymNone | SymWreath -> false)) then
        // COMPACT path: a SymIdx / AntisymIdx / HermitianIdx group is ONE axis
        // over r dimensions whose stored cells are a left-justified simplex.
        // Two things separate it from the rectangular path below:
        //
        //   1. The extents table is the group's COMPONENT extents (n repeated
        //      r times), not the literal's own row lengths. `computeArrayDims`
        //      reads the first row, which for a strict (antisym) group is n-1
        //      wide -- an extents table that would make every compact read
        //      compute the wrong storage bound.
        //   2. The allocation carries the group's SYMM mask (and DIAGONALS =
        //      false for antisym), so `allocate<>` builds the shrinking
        //      skeleton the literal was checked against.
        //
        // The ASSIGNMENTS are then the plain nested ones: `checkCompactArrayLit`
        // accepted the literal only in the storage's own shape, so the literal's
        // index path IS the storage coordinate -- `A[1][0]` is the canonical cell
        // (1,1) for SymIdx<2,n> (canon_left_justify: p1 = p0 + c1).
        let ty = mkArrayLike arrType
        let componentExtents =
            arrType.IndexTypes |> List.collect (fun ix -> List.replicate (max 1 ix.Rank) ix.Extent)
        let rank = componentExtents.Length
        let extentInts =
            componentExtents |> List.map (fun e ->
                match e with IRLit (IRLitInt n) -> Some (int n) | _ -> None)
        let rec walkLeaves (idxPath: int list) (e: IRExpr) : (int list * IRExpr) list =
            match e with
            | IRArrayLit (children, _) ->
                children |> List.mapi (fun i c -> walkLeaves (idxPath @ [i]) c) |> List.concat
            | leaf -> [(idxPath, leaf)]
        let leaves = walkLeaves [] (IRArrayLit (elements, arrType))
        if extentInts |> List.exists Option.isNone then
            [refusalErrorLine ind ($"Blade codegen: compact array literal for '{varName}' needs compile-time extents")]
        elif leaves |> List.exists (fun (path, _) -> path.Length <> rank) then
            // Array-VALUED leaves (a computed row) would need a deep copy into a
            // shrinking row; the checker refuses that shape, so reaching here is
            // a front-end change, not user input.
            [refusalErrorLine ind ($"Blade codegen: compact array literal for '{varName}' has a leaf at the wrong depth (typechecker bug)")]
        else
            let extentsName = $"{varName}_extents"
            // `static constexpr`, exactly like the rectangular literal path
            // below and unlike the fill_random compact path (whose stack table
            // is safe only because fill_random is confined to a top-level let):
            // the Array wrapper keeps a POINTER to this table, and a literal is
            // also the statement form of a FUNCTION RETURN -- a stack table
            // would dangle the moment the frame died, handing the caller
            // garbage extents rather than a compile error.
            let extentsDecl =
                sprintf "%sstatic constexpr const size_t %s[%d] = { %s };" ind extentsName rank
                    (extentInts |> List.map (Option.get >> string) |> String.concat ", ")
            let symmVec = buildSymmVec ty
            let symmArg =
                if hasRealSymmetry symmVec then hoistSymmDecl ($"{varName}_symm") symmVec
                else "nullptr"
            let allocLines =
                match emitAllocRhs (classifyOutputStorage ty) elemType rank symmArg extentsName with
                | Ok rhs -> [$"{ind}Array<{elemType}, {rank}> {varName} = {rhs};"]
                | Error msg -> [refusalErrorLine ind ($"compact literal '{varName}': {msg}")]
            let initCode =
                leaves |> List.map (fun (path, leaf) ->
                    let suffix = path |> List.map (sprintf "[%d]") |> String.concat ""
                    $"{ind}{varName}{suffix} = {(exprToCpp ctx.VarNames leaf)};")
            [extentsDecl] @ allocLines @ initCode
    else
        // Rectangular path: existing behavior.
        let structuralDims = computeArrayDims (IRArrayLit (elements, arrType))
        // Rows-of-computed-arrays: when the literal's nesting is shallower
        // than the declared rank -- elements are array-VALUED expressions
        // (e.g. `method_for(..) |> compute` results bound to names) rather
        // than nested bracket literals -- computeArrayDims only sees the
        // bracket levels. The missing inner extents come from the array
        // type's trailing IndexTypes (the typechecker has already verified
        // each element against exactly those index types). Without this,
        // the extents table was emitted short ({2} for a rank-2 array, so
        // extents[1] read as 0) and every downstream shape consumer -- the
        // auto-print loop, method_for fibers over rows -- saw zero-length
        // rows: a silent miscompile (M = [], prodsum = 0).
        let dims =
            if structuralDims.Length >= rank then structuralDims
            elif arrType.IndexTypes |> List.forall (fun ix -> ix.Rank = 1) then
                let tail =
                    arrType.IndexTypes
                    |> List.skip structuralDims.Length
                    |> List.map (fun ix ->
                        match ix.Extent with
                        | IRLit (IRLitInt n) -> Some (int n)
                        | _ -> None)
                if tail |> List.forall Option.isSome
                then structuralDims @ (tail |> List.map Option.get)
                else structuralDims
            else structuralDims
        if dims.IsEmpty then
            [$"{ind}// Empty array literal"]
        elif dims.Length < rank then
            // Inner extents couldn't be recovered statically (parametric or
            // compound index types). Refuse loudly rather than emit the
            // short-extents table that silently reads as empty.
            [refusalErrorLine ind ($"Blade codegen: array literal for '{varName}' nests {dims.Length} level(s) but the declared rank is {rank}, and the missing inner extents are not static -- bind the rows to a fully-literal array or annotate with static Idx<n> extents")]
        else
            // Generate extents declaration
            let extentsValues = dims |> List.map string |> String.concat ", "
            let extentsDecl = $"{ind}static constexpr const size_t {varName}_extents[{rank}] = {{{extentsValues}}};"
            
            // Generate allocation as Array<T,N> wrapper. Single brace-init
            // bundles the data pointer (from allocate<>) with the extents
            // pointer (the static-constexpr global emitted above).
            let allocDecl =
                arrayAlloc { Ind = ind; Elem = elemType; Rank = rank; Name = varName
                             Symm = "nullptr"; Strict = None; Extents = varName + "_extents" }
            // Deterministic deallocation, site 4: the rectangular literal path
            // only. The extents table is `static constexpr` (never owned); the
            // ragged/DepIdx literal paths above and the #error paths never reach
            // here. Symm is nullptr by construction, so the free mirrors trivially.
            if isFreeableDenseArrayType arrType then
                registerPoolAlloc AllocDense elemType rank "nullptr" (varName + "_extents") varName None

            // Generate initialization
            let values = extractLiteralValues (IRArrayLit (elements, arrType))
            
            // Decide between fast scalar path and per-element expression path.
            // The fast path uses `%g` formatting and is correct only when all
            // elements extracted to a complete float list (length matches the
            // dim product). Struct literals, computed values, or any element
            // that extractLiteralValues returns nothing for falls into the
            // expression path, which renders via exprToCpp (handles
            // IRStructLit, IRApp, etc. uniformly).
            //
            // Pre-Phase-D, the fast path was the only path: when any element
            // wasn't a scalar literal, no init was emitted at all (silent
            // miscompile reading uninitialized memory).
            let totalExpected = dims |> List.fold (*) 1
            let useFastScalarPath = values.Length = totalExpected && totalExpected > 0
            
            // Generalize over rank N. The two paths diverge only in how each
            // leaf is rendered:
            //   - fast path: %g-formatted floats from extractLiteralValues,
            //     in row-major order
            //   - per-element path: walk the nested IRArrayLit, render each
            //     leaf via exprToCpp (covers struct lits, computed values)
            // Both produce assignments of the form `name[i_0][i_1]...[i_n-1] = E;`.
            //
            // Pattern follows extractLiteralValues / computeArrayDims -- recurse
            // through IRArrayLit nesting, treating non-IRArrayLit nodes as
            // leaves. The old rank-1 / rank-2 / TODO-for-higher dispatch is
            // gone; rank-3+ now works at parity with rank-1 and rank-2.
            let rec enumerateIndexPaths (ds: int list) : int list list =
                match ds with
                | [] -> [[]]
                | n :: rest ->
                    let tails = enumerateIndexPaths rest
                    [for i in 0 .. n - 1 do
                        for t in tails do
                            yield i :: t]
            let rec walkLeaves (idxPath: int list) (e: IRExpr) : (int list * IRExpr) list =
                match e with
                | IRArrayLit (children, _) ->
                    children
                    |> List.mapi (fun i c -> walkLeaves (idxPath @ [i]) c)
                    |> List.concat
                | leaf -> [(idxPath, leaf)]
            let formatIndexPath (path: int list) : string =
                path |> List.map (sprintf "[%d]") |> String.concat ""
            
            let initCode =
                if useFastScalarPath then
                    // Row-major enumeration of (i0,...,i(n-1)) tuples zipped with
                    // the flat value list. extractLiteralValues already walks
                    // in row-major order, so the alignment is exact.
                    let paths = enumerateIndexPaths dims
                    List.zip paths values |> List.map (fun (path, v) ->
                        // Round-trip literal (see floatToCppLiteral); plain
                        // assignment converts implicitly for integral
                        // element types, so no narrowing concern here.
                        $"{ind}{varName}{(formatIndexPath path)} = {(floatToCppLiteral v)};")
                else
                    // Per-element path: walk the nested IRArrayLit. Index path
                    // accumulates as we descend; leaves render via exprToCpp.
                    walkLeaves [] (IRArrayLit (elements, arrType))
                    |> List.collect (fun (path, leaf) ->
                        if path.Length >= rank then
                            [$"{ind}{varName}{(formatIndexPath path)} = {(exprToCpp ctx.VarNames leaf)};"]
                        else
                            // Array-valued leaf (a computed row): deep-copy the
                            // remaining dims elementwise. Assigning the Array
                            // wrapper into the row slot would alias the row to
                            // the source's buffer instead of copying (and for
                            // deeper rank gaps doesn't even compile).
                            //
                            // The loop bounds come from the DECLARED extents;
                            // annotation-vs-actual extent mismatches are not
                            // (yet) rejected by unify for computed arrays, so
                            // guard each copied dim at runtime -- a mismatch
                            // must be a loud exit(1), not an OOB read.
                            let subDims = dims |> List.skip path.Length
                            let srcName = $"""__cpsrc_{(path |> List.map string |> String.concat "_")}"""
                            let loopVars = subDims |> List.mapi (fun j _ -> $"__cp{j}")
                            let idxSuffix = loopVars |> List.map (sprintf "[%s]") |> String.concat ""
                            let srcDecl =
                                $"{ind}    const auto& {srcName} = ({(exprToCpp ctx.VarNames leaf)});"
                            let guards =
                                subDims |> List.mapi (fun j n ->
                                    $"{ind}    if ({srcName}.extents[{j}] != {n}) {{ std::cerr << \"Blade runtime: array literal row {(formatIndexPath path)} of '{varName}' has extent \" << {srcName}.extents[{j}] << \" in dim {j}, but the declared type expects {n}\" << std::endl; blade_rt::panic(\"BL8006\", \"array literal extent mismatch\", nullptr, 0); }}")
                            let opens =
                                List.zip loopVars subDims
                                |> List.mapi (fun j (v, n) ->
                                    $$"""{{ind}}    {{(String.replicate j "    ")}}for (size_t {{v}} = 0; {{v}} < {{n}}; {{v}}++) {""")
                            let body =
                                sprintf "%s    %s%s%s%s = %s%s;"
                                    ind (String.replicate subDims.Length "    ")
                                    varName (formatIndexPath path) idxSuffix
                                    srcName idxSuffix
                            let closes =
                                [for j in subDims.Length - 1 .. -1 .. 0 -> $"""{ind}    {(String.replicate j "    ")}}}"""]
                            [$"{ind}{{"] @ [srcDecl] @ guards @ opens @ [body] @ closes @ [$"{ind}}}"])
            
            [extentsDecl; allocDecl] @ initCode

/// Generate code for a scalar binding
let genScalarBinding (ctx: CodeGenContext) (name: string) (value: IRExpr) (ty: IRType) : string list =
    let ind = indentStr ctx
    // Defense for upstream type-inference cache misses: when the binding's
    // declared type is IRTInfer or IRTUnit but the value isn't actually
    // unit-valued, re-derive the type from the value via inferExprType.
    // This catches cases where the lift pass's structFieldsCache may have
    // missed an IRFieldAccess and labeled the binding IRTUnit.
    let resolvedTy = 
        match ty with 
        | IRTInfer _ -> inferExprType value 
        | IRTUnit when not (isUnitExpr value) ->
            let inferred = inferExprType value
            if inferred = IRTUnit then ty else inferred
        | t -> t
    // No `auto <name> = <expr>;` fallback for a shape-bearing RHS (IRMask/
    // IRSort/IRVar/IRFieldAccess/IRIntersect/IRUnion) that resolves to
    // IRTUnit: those RHS shapes always resolve to a non-IRTUnit type by this
    // point (there is ONE struct-fields cache -- IR.fs's AsyncLocal registry,
    // which `setCodegenStructFieldsCache` merely forwards into -- and it is
    // per-flow specifically so parallel tasks can't race and return a stale
    // IRTUnit). If a regression reaches this branch anyway, the
    // expression-statement form below deliberately produces invalid C++
    // rather than papering over it with auto-deduction -- diagnose the
    // upstream resolution bug instead.
    match resolvedTy with
    | IRTUnit ->
        if isUnitExpr value then 
            []
        else
            // Genuinely unit-valued: emit as expression statement
            let valueStr = exprToCppCtx ctx value
            [$"{ind}{valueStr};"]
    | _ ->
        // Array-typed bindings render as Array<T,N>/Ragged<T> wrappers when the
        // RHS produces one (IRFieldAccess, IRVar resolving to a wrapper,
        // IRMask/IRSort/IRIntersect/IRUnion, and IRApp -- function calls
        // returning IRTArray emit `Array<T, N>` at genFuncDef, so the let-bound
        // result must match, not the raw `promote<T, N>::type` pointer that
        // would lose `.extents`). RHSes producing bare pointers (IRIndex
        // peeling a sub-array, IRApp returning T*) render bare instead, to
        // avoid a brace-init mismatch.
        //
        // Rank-1 ragged-family SUB-VIEW binding (`let row = r(i)` on a ragged/
        // DepIdx parent, or `let g0 = grouped(i)` on group_by): binding as a
        // raw T* would lose the row LENGTH, so downstream length-dependent ops
        // (reduce, extents, print) lack accessors. Bind as RaggedRow<T>
        // instead: a ragged/DepIdx parent's operator[] already returns
        // RaggedRow{ptr,len}; a grouped parent's length comes from the
        // group_keys offsets table (ctx.GroupedArrays), same as the peel path.
        let raggedRowSubview =
            match value, resolvedTy with
            | IRIndex (parent, [idxExpr], _), ArrayElem rowTy when isRaggedRowType rowTy ->
                let valueStr = exprToCppCtx ctx value
                let elemStr = elemTypeToCpp rowTy.ElemType
                let isGroupRow = rowTy.IndexTypes.[0].IxKind = IxKGroupMember
                if isGroupRow then
                    let parentName = exprToCppCtx ctx parent
                    match Map.tryFind parentName ctx.GroupedArrays with
                    | Some gkName ->
                        let idxStr = exprToCppCtx ctx idxExpr
                        Some [$"{ind}RaggedRow<{elemStr}> {name} = {{ {valueStr}, {gkName}__offsets[({idxStr}) + 1] - {gkName}__offsets[{idxStr}] }};"]
                    | None -> None  // grouped parent not registered (non-var producer): fall through to raw
                else
                    Some [$"{ind}RaggedRow<{elemStr}> {name} = {valueStr};"]
            | _ -> None
        if raggedRowSubview.IsSome then raggedRowSubview.Value
        else
        // Plain dense PARTIAL positional read: `let r0 = A(i)` supplies FEWER
        // subscripts than the array's rank, so the result is a row/slab sub-view
        // (residual rank >= 1), not a scalar. The raw path below would bind it as
        // `promote<T, R>::type` -- a bare data pointer that has lost `.extents`,
        // so every downstream `.extents` consumer (auto-print, method_for/zip)
        // fails to compile. Bind the Array<T, R> wrapper directly, mirroring the
        // loop-peel slice idiom (genElementBindingNew): the data pointer steps
        // through the consumed leading dims and the extents pointer shifts past
        // them, e.g. `Array<double,1> r0 = { A.data[0L], A.extents + 1 };`.
        //
        // Scoped to fully plain-dense rectangular arrays (every axis IxKPlain /
        // SymNone / arity-1) so the consumed-dims count equals the
        // subscript count and the extents shift is exact. Compound partial reads
        // take the IRTuple arm in producesWrapper; ragged/dep-idx rows take
        // raggedRowSubview above; and a flat single-subscript into PACKED
        // symmetric storage returns a row pointer under compact semantics that
        // must NOT be re-wrapped here -- all excluded by the axis predicate.
        // NOT gated on `ix.Kind = SDimension` -- Kind describes one apply, not
        // the value's storage (see densePartialSubviewExpr's note; both twins
        // must answer identically or a let-bound slice and an inline slice
        // diverge in shape).
        let densePartialSubview =
            match value, resolvedTy with
            | IRIndex (arr, indices, _), ArrayElem residTy
                    when not (List.isEmpty indices)
                         && indices |> List.forall (function IRTuple _ -> false | _ -> true) ->
                match inferExprType arr with
                | ArrayElem arrTy
                        when arrTy.IndexTypes.Length > indices.Length
                             && arrTy.IndexTypes |> List.mapi (fun d ix -> (d, ix))
                                |> List.forall (fun (d, ix) ->
                                    (ix.IxKind = IxKPlain
                                     || (d < indices.Length && ix.IxKind = IxKGroupOuter))
                                    && ix.Symmetry = SymNone
                                    && ix.Rank = 1) ->
                    let arrStr = exprToCppCtx ctx arr
                    let subscripts =
                        indices
                        |> List.map (fun i -> $"[{(exprToCppCtx ctx i)}]")
                        |> String.concat ""
                    Some [$"{ind}{(cppArrayTypeStr residTy)} {name} = {{ {arrStr}.data{subscripts}, {arrStr}.extents + {indices.Length} }};"]
                | _ -> None
            | _ -> None
        if densePartialSubview.IsSome then densePartialSubview.Value
        else
        let rec producesWrapperOf value =
            match value with
            | IRFieldAccess _ -> true
            | IRVar _ -> true                // assume wrapper (most producers migrated)
            | IRMask _ | IRSort _ | IRIntersect _ | IRUnion _ | IRUnique _ -> true
            | IRApp _ -> true                // function-call returns wrapped Array
            | IRTupleProj _ -> true          // tuple elements carry wrappers (irTypeToCpp IRTTuple)
            // An array-valued SELECT is a wrapper exactly when both branches
            // are -- it renders as a C++ ternary, whose type is the branches'
            // common type. This is what a recursive array's out-of-prefix lag
            // read binds (`if n - 3 >= 0 then __lag0_m else __zs0_m`, both
            // Array<T, N> wrappers); declaring it on the raw `promote<>::type`
            // path instead would drop the extents every `.data`/`.extents`
            // consumer downstream needs. Branch DISAGREEMENT keeps the raw
            // path, so no arm gets a wrapper it cannot produce.
            | IRIf (_, t, e) -> producesWrapperOf t && producesWrapperOf e
            | IRIndex (a, (IRTuple coords) :: _, _) ->
                // A PARTIAL sparse read (formalism 3.5) produces a wrapper:
                // Sparse<T, RR> for a residual-rank >= 2 read
                // (make_partial_sparse_gather) or Array<T, 1|2> for a dense
                // rank-1 residual (make_sparse_gather_dense[_trail]). A FULL
                // tabulated read stays on the raw path (scalar, or a
                // trailing-row T* sub-view via .row()), as does ordinary
                // positional peeling on non-tabulated arrays. (Compound heads
                // never classify as partial since the flat-subscript
                // conversion -- the compound arm here is vestigial but
                // harmless, and keeps the predicate kind-agnostic.)
                (match inferExprType a with
                 | ArrayElem at when isCompoundArrayType at || isSparseArrayType at ->
                     let k =
                         at.IndexTypes
                         |> List.tryFind (fun ix -> ix.IxKind = IxKCompound || ix.IxKind = IxKSparse)
                         |> Option.map (_.Rank)
                         |> Option.defaultValue coords.Length
                     (match classifyCompoundIndexTuple k coords with
                      | CompoundPartial _ -> true
                      | CompoundFull -> false)
                 | _ -> false)
            | _ -> false
        let producesWrapper = producesWrapperOf value
        let cppType =
            match resolvedTy with
            | ArrayElem arr when producesWrapper -> cppArrayTypeStr arr
            | _ -> irTypeToCpp resolvedTy
        let valueStr = exprToCppCtx ctx value
        // Deterministic deallocation, site 3: `let r = f(a)` where the CALLEE
        // allocated the pool. Restricted to FreshPool callees -- a NotFresh return
        // may hand back its own parameter, so `r.data == a.data` and registering
        // `r` would double-free. Restricted further to AllocDense with no real
        // symmetry: the callee allocated with ITS output type and we only know the
        // binding's, and for a symmetric/strict return a disagreement is heap
        // corruption rather than a compile error. Dense + nullptr is the one
        // combination that cannot disagree. The extents came from inside the callee
        // and travel with the wrapper, so nothing is owned here.
        // The `IRVar` arm of producesWrapper (plain `let b = a`, a wrapper copy
        // sharing the pool) is deliberately NOT covered.
        (match value, resolvedTy with
         | IRApp (fn, _, _), ArrayElem at when
                producesWrapper
                && freshReturnOf fn = FreshPool
                && isFreeableDenseArrayType at
                && classifyOutputStorage resolvedTy = AllocDense
                && not (hasRealSymmetry (buildSymmVec resolvedTy)) ->
             registerPoolAlloc AllocDense (elemTypeToCpp at.ElemType) (arrayRank at)
                 "nullptr" (name + "_extents") name None
         | _ -> ())
        [$"{ind}{cppType} {name} = {valueStr};"]

// Loop Application Code Generation

/// Build a simple (no symmetry) ApplyInfo for applying a unary kernel to arrays.
/// Used by >>@ and @>> to construct stage-2 pipeline applications.
let defaultIndexType () = { Id = 0; Rank = 1; Extent = IRLit (IRLitInt 0); Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
/// Build a default IRArrayType.
let defaultArrayType (et: IRType) = { ElemType = et; IndexTypes = [defaultIndexType ()]; IsVirtual = false; Identity = None }

let buildSimpleApplyInfo (arrays: IRExpr list) (kernel: IRExpr) (outputType: IRType) : ApplyInfo =
    let arrayTypes = arrays |> List.map (fun a -> 
        match inferExprType a with 
        | ArrayElem arr -> arr 
        | _ -> defaultArrayType (IRTScalar ETFloat64))
    let identities = arrays |> List.mapi (fun i _ -> AIDLiteral i)
    let sDims = arrayTypes |> List.map arrayRank
    let totalSDims = List.sum sDims
    {
        Loop = IRMethodFor {
            Arrays = arrays
            Identities = identities
            ArrayTypes = arrayTypes
            SDimsPerArray = sDims
            TotalSDims = totalSDims
            SharedIndexTypes = []
        }
        Kernel = kernel
        Arrays = arrays
        Identities = identities
        ArrayTypes = arrayTypes
        SharedIndexTypes = []
        SymcomStates = List.replicate totalSDims SCNeither
        TriangularLevels = List.replicate totalSDims false
        SDimsPerArray = sDims
        KernelInputRanks = List.replicate (List.length arrays) 0
        KernelOutputRank = 0
        KernelTDims = []
        SpeedupFactor = 1L
        ReynoldsSpeedup = 1L
        HasReynolds = false
        OutputType = outputType
        IsCoIteration = false
    }

