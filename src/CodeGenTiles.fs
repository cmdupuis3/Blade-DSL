/// Revision reuse: cache pure output tiles across Icechunk snapshots
/// (docs/plans/structural/04-revision-reuse.md, v1 = leading-axis tiles).
///
/// An Icechunk checkout is resolved at compile time to an immutable snapshot
/// and every chunk of every variable the program reads is baked into the
/// executable as a `(chunk id, offset, length)` table -- a content identity
/// per chunk that is stable across commits that did not touch that chunk.
/// For a top-level binding that is a pure, index-local traversal (an
/// elementwise map or co-iteration over whole-variable dense icechunk reads)
/// the emitter runs the nest one TILE at a time -- a tile is one chunk of the
/// leading axis, all trailing axes -- and gives every tile a key: the SHA-256
/// of the task's text, the output geometry, and the content identities of
/// exactly the input chunks the tile reads. A local store keyed that way
/// (blade_tilecache.hpp) lets a run against a later snapshot skip both the
/// recomputation and the chunk reads of every tile whose dependencies did not
/// move, with byte-identical stdout: values are cached, never text, and the
/// print pass reads the same materialized array.
///
/// This file is the PLANNER (what is admitted, and each tile's key); the
/// emission lives at the three seams it feeds: the need-masked provider read
/// (CodeGenBinding.genProviderReadBinding), the tile run loop around the nest
/// (CodeGenCuda.genApplyCombinator), and the remainder read after it. Both
/// gates are opt-in: BLADE_TILE_CACHE unset means no plan, and a program that
/// plans nothing emits byte for byte what it emitted before.
///
/// Refused into the ordinary emission path (never approximated): reductions,
/// effects and mutation in the kernel, captured values, non-icechunk or
/// non-dense (masked / windowed / streamed / packed) inputs, computed inputs,
/// outputs that are not plain literal-extent dense arrays, MPI programs.
module Blade.CodeGenTiles

open System
open Blade.Types
open Blade.IR
open Blade.CodeGenState

/// The compile-time half of the BLADE_TILE_CACHE gate (the run-time half is
/// blade_tilecache.hpp's `dir()`, same grammar): unset / `0` / `off` /
/// `false` -> off; `1` / `on` / `true` -> the default directory; an absolute
/// path -> that directory; anything else -> off. Read per call, like every
/// other env gate.
let tileCacheEnabled () : bool =
    match Environment.GetEnvironmentVariable "BLADE_TILE_CACHE" with
    | null | "" -> false
    | v ->
        match v.Trim().ToLowerInvariant() with
        | "0" | "off" | "false" | "" -> false
        | "1" | "on" | "true" -> true
        | _ -> IO.Path.IsPathRooted (v.Trim())

let private sha256Hex (s: string) : string =
    use sha = Security.Cryptography.SHA256.Create()
    sha.ComputeHash(Text.Encoding.UTF8.GetBytes s) |> Array.map (sprintf "%02x") |> String.concat ""

let private literalExtents (at: IRArrayType) : int64 list option =
    let exts = at.IndexTypes |> List.map (fun ix -> match ix.Extent with IRLit (IRLitInt n) -> Some n | _ -> None)
    if exts |> List.forall Option.isSome then Some (exts |> List.map Option.get) else None

let private plainDense (at: IRArrayType) =
    at.IndexTypes |> List.forall (fun ix -> ix.IxKind = IxKPlain && ix.Symmetry = SymNone && ix.Rank = 1)

/// A whole-variable dense icechunk read: the only input kind with a
/// compile-time content identity per chunk.
let private tileableRead (spec: ProviderReadSpec) =
    spec.Provider = "icechunk" && spec.MaskName.IsNone && spec.Window.IsNone && not spec.Streamed
    && plainDense spec.VarType

/// The first clause of `IRMono.tilePlainInfo` a combinator fails, named, for
/// the verbose decline line.
let private plainInfoWhy (info: ApplyInfo) : string option =
    let plainIx (ix: IRIndexType) = ix.IxKind = IxKPlain && ix.Symmetry = SymNone
    let clauses : (string * (unit -> bool)) list =
        [ "reynolds", (fun () -> not info.HasReynolds)
          "speedup metadata", (fun () -> info.SpeedupFactor = 1L && info.ReynoldsSpeedup = 1L)
          "kernel T-dims", (fun () -> info.KernelTDims.IsEmpty)
          "kernel output rank", (fun () -> info.KernelOutputRank = 0)
          "kernel input ranks", (fun () -> info.KernelInputRanks |> List.forall ((=) 0))
          "symcom states", (fun () -> info.SymcomStates |> List.forall (fun s -> s = SCNeither))
          "triangular levels", (fun () -> info.TriangularLevels |> List.forall not)
          "array index kinds", (fun () -> info.ArrayTypes |> List.forall (fun at -> at.IndexTypes |> List.forall plainIx))
          "loop provenance", (fun () -> match info.Loop with IRMethodFor _ | IRObjectFor _ -> true | _ -> false)
          "co-iteration", (fun () -> info.Arrays.Length = 1 || (info.IsCoIteration && not info.SharedIndexTypes.IsEmpty))
          // The symcom / triangular lists are per LOOP LEVEL by codegen time
          // (the fusion pass sees them per array, earlier); every entry must
          // be neutral either way, so only the array-parallel lists are held
          // to lockstep here.
          "per-array lists", (fun () ->
              info.Identities.Length = info.Arrays.Length && info.ArrayTypes.Length = info.Arrays.Length
              && info.SDimsPerArray.Length = info.Arrays.Length && info.KernelInputRanks.Length = info.Arrays.Length) ]
    clauses |> List.tryPick (fun (name, ok) -> if ok () then None else Some name)

/// Every binding id referenced by `e`.
let private referencedIds (e: IRExpr) : Set<IRId> =
    let mutable acc = Set.empty
    iterIRExpr (fun n -> match n with IRVar (id, _) -> acc <- Set.add id acc | _ -> ()) e
    acc

/// Plan every admitted binding of `modul`. Returns the plans keyed by the
/// tiled binding's C++ name (genApplyCombinator's key) and the hoisted
/// probes keyed by the provider-read binding they attach to.
let planTiles (modul: IRModule) : Map<string, TilePlan> * Map<IRId, TilePlan> =
    if not (tileCacheEnabled ()) || mpiProgramOn () then Map.empty, Map.empty
    else
    let callables = Collections.Generic.Dictionary<IRId, IRCallable>()
    for f in modul.Functions do callables.[f.Id] <- f
    let bindingById = modul.Bindings |> List.map (fun b -> b.Id, b) |> Map.ofList
    let cppNameOf (b: IRBinding) = Blade.CodeGenFusion.bindingCppName b
    // ids referenced by each top-level binding's value and by any function body
    let bindingRefs = modul.Bindings |> List.map (fun b -> b.Id, referencedIds b.Value) |> Map.ofList
    let functionRefs = modul.Functions |> List.fold (fun acc f -> Set.union acc (referencedIds f.Body)) Set.empty
    let claimed = Collections.Generic.HashSet<IRId>()
    let plans = Collections.Generic.List<TilePlan>()
    let verbose =
        match Environment.GetEnvironmentVariable "BLADE_TILE_CACHE_VERBOSE" with
        | null | "" | "0" -> false
        | _ -> true
    let decline (b: IRBinding) (why: string) =
        if verbose then eprintfn "[tiles] plan %s: declined -- %s" b.Name why
    for b in modul.Bindings do
        match b.Value with
        | IRCompute (IRApplyCombinator info) ->
            match plainInfoWhy info with
            | Some why ->
                let kinds = info.ArrayTypes |> List.map (fun at -> at.IndexTypes |> List.map (fun ix -> sprintf "%A/%A" ix.IxKind ix.Symmetry) |> String.concat ",") |> String.concat " | "
                let lens = $"arrays={info.Arrays.Length} ids={info.Identities.Length} types={info.ArrayTypes.Length} sdims={info.SDimsPerArray.Length} symcom={info.SymcomStates.Length} tri={info.TriangularLevels.Length} kranks={info.KernelInputRanks.Length}"
                decline b $"not a plain dense elementwise map / co-iteration ({why}; index kinds {kinds}; {lens})"
            | None ->
            let kernel = Blade.IRMono.tileKernelOf callables info
            let inputs =
                info.Arrays |> List.map (fun a ->
                    match a with
                    | IRVar (id, _) ->
                        (match Map.tryFind id modul.ProviderReads, Map.tryFind id bindingById with
                         | Some spec, Some rb when tileableRead spec -> Some (id, rb, spec)
                         | _ -> None)
                    | _ -> None)
            let outType =
                match b.Type with
                | ArrayElem at when plainDense at -> literalExtents at |> Option.map (fun exts -> at, exts)
                | _ -> None
            match kernel, outType with
            | None, _ -> decline b "the kernel is not a resolvable callable"
            | _, None -> decline b "the output is not a plain dense literal-extent array"
            | Some k, Some (outAt, outExts) ->
                if not (inputs |> List.forall Option.isSome) then decline b "an operand is not a whole-variable dense icechunk read"
                elif not (Blade.IRMono.tilePlainKernel k info.Arrays.Length) then decline b "the kernel carries structure metadata (comm / omp / cuda / mpi / arity-poly / static) or its arity disagrees"
                elif not (Blade.IRMono.tilePureBody callables Set.empty k.Body) then decline b "the kernel is not pure (display, assignment, or an unresolvable call)"
                elif not k.Captures.IsEmpty then decline b "the kernel captures values (v1 keys capture-free tasks only)"
                else
                let inputs = inputs |> List.map Option.get
                // every input resolves, and all share the output's shape and one chunk grid
                let resolved =
                    inputs |> List.map (fun (id, rb, spec) ->
                        match Blade.IcechunkProvider.resolveArray spec.FilePath spec.VarName with
                        | Ok ra -> Some (id, rb, spec, ra)
                        | Error _ -> None)
                if not (resolved |> List.forall Option.isSome) then decline b "an input variable did not resolve"
                else
                    let resolved = resolved |> List.map Option.get
                    let (_, _, _, ra0) = resolved.Head
                    let shape = ra0.Meta.Shape
                    let chunks = ra0.Meta.Chunks
                    let sameGrid =
                        resolved |> List.forall (fun (_, _, _, ra) -> ra.Meta.Shape = shape && ra.Meta.Chunks = chunks)
                        && shape = outExts && not shape.IsEmpty
                    if not sameGrid then decline b "the inputs and the output do not share one shape and chunk grid"
                    elif resolved |> List.exists (fun (id, _, _, _) -> claimed.Contains id) then decline b "an input is already claimed by an earlier tiled binding"
                    else
                        let grid = Blade.ZarrProvider.gridDims shape chunks
                        let nt = int grid.Head
                        let trailingGrid = grid |> List.tail |> List.fold (*) 1L
                        let trailing = shape |> List.tail |> List.fold (*) 1L
                        let n0 = shape.Head
                        let c0 = chunks.Head
                        let leadBounds = [ for t in 0 .. nt -> min n0 (int64 t * c0) ]
                        let elemCpp = elemTypeToCpp outAt.ElemType
                        let outName = cppNameOf b
                        // The task text: the kernel as the IR prints it (its body
                        // over its parameters), the operands' order, the output
                        // type. Compiler-generated ids can drift between programs
                        // (a spurious miss, never a wrong hit); for one program
                        // compiled against two snapshots they are identical.
                        let paramText = k.Params |> List.map (fun p -> $"{p.Name}:{Blade.IRPrint.ppIRType p.Type}") |> String.concat ","
                        let taskText =
                            String.concat "\n"
                                [ $"kernel({paramText})"
                                  Blade.IRPrint.ppIRExpr 0 k.Body
                                  $"arrays={info.Arrays.Length};coiter={info.IsCoIteration}"
                                  $"out={Blade.IRPrint.ppIRType b.Type}" ]
                        let taskHash = sha256Hex taskText
                        let extText = shape |> List.map string |> String.concat "x"
                        let tags =
                            outAt.IndexTypes |> List.map (fun ix -> defaultArg ix.Tag "-") |> String.concat ","
                        let inputLines (t: int) =
                            resolved |> List.mapi (fun kx (_, _, spec, ra) ->
                                let repo =
                                    match Blade.IcechunkProvider.parseKey spec.FilePath with
                                    | Ok key -> Blade.IcechunkProvider.canonicalRepoPath key.RepoPath
                                    | Error _ -> spec.FilePath
                                let node = Blade.IcechunkProvider.base32Encode ra.Node.Id
                                let user = sha256Hex ra.Node.UserDataJson
                                let refs =
                                    [ for j in 0L .. trailingGrid - 1L ->
                                        let flat = int (int64 t * trailingGrid + j)
                                        if flat < ra.Table.Length then Blade.IcechunkProvider.chunkIdentityText ra.Table.[flat] else "-" ]
                                    |> String.concat ","
                                $"in[{kx}]={repo};var={spec.VarName};node={node};user={user};access=identity;chunks={refs}")
                        let keys =
                            [ for t in 0 .. nt - 1 ->
                                let material =
                                    [ "blade-tile-v1"
                                      $"task={taskHash}"
                                      $"out={elemCpp};{extText};{tags};lead=[{leadBounds.[t]},{leadBounds.[t + 1]});t={t}" ]
                                    @ inputLines t
                                    |> String.concat "\n"
                                sha256Hex material ]
                        // Read avoidance needs the probe hoisted to the input's read,
                        // which is sound only when nothing between the read and this
                        // binding observes the input (a partially assembled array must
                        // reach no other consumer) and no function body names it.
                        let hoisted =
                            resolved |> List.forall (fun (id, rb, _, _) ->
                                rb.Id < b.Id
                                && not (Set.contains id functionRefs)
                                && (modul.Bindings |> List.forall (fun ob ->
                                        ob.Id <= rb.Id || ob.Id >= b.Id
                                        || not (Set.contains id (Map.find ob.Id bindingRefs)))))
                        // Read avoidance is only REAL for an input nothing
                        // else observes: the print pass reads every top-level
                        // binding unless `--print` selects (structural/04,
                        // 3.5), and a later binding or a function body may
                        // read it too. An observed input still has its
                        // remainder fetched after the tiled nest; an
                        // unobserved one never fetches the chunks its hit
                        // tiles would have needed.
                        let printed (nm: string) =
                            match printSelection () with
                            | None -> true
                            | Some names -> Set.contains nm names
                        let inputPlans =
                            resolved |> List.map (fun (id, rb, spec, ra) ->
                                let cpp = cppNameOf rb
                                let need = $"{cpp}__need"
                                let don = $"{cpp}__done"
                                let (p1, p2, rel) =
                                    Blade.IcechunkProvider.CppIcechunk.genReadVarPhased spec.FilePath spec.VarName cpp spec.VarType need don
                                let otherReaders =
                                    Set.contains id functionRefs
                                    || (modul.Bindings |> List.exists (fun ob ->
                                            ob.Id <> b.Id && ob.Id <> rb.Id
                                            && Set.contains id (Map.find ob.Id bindingRefs)))
                                { ReadId = id; CppName = cpp; Spec = spec
                                  ChunkCount = ra.Table.Length; TrailingGrid = trailingGrid
                                  Observed = otherReaders || printed rb.Name
                                  Phase1 = p1; Phase2 = p2; Release = rel })
                        for (id, _, _, _) in resolved do claimed.Add id |> ignore
                        if verbose then
                            let unread = inputPlans |> List.filter (fun i -> not i.Observed) |> List.map (fun i -> i.CppName)
                            let note =
                                if unread.IsEmpty then ""
                                else sprintf "; no reader but the nest for %s (unneeded chunks never fetched)" (String.concat ", " unread)
                            eprintfn "[tiles] plan %s: %d tile(s) over %d input(s), probe %s%s" b.Name nt inputPlans.Length (if hoisted then "hoisted to the read" else "at the binding") note
                        plans.Add
                            { Output = outName; BindingId = b.Id; Inputs = inputPlans
                              Tiles = nt; LeadBounds = leadBounds; Trailing = trailing
                              ElemCpp = elemCpp; Keys = keys; Hoisted = hoisted }
        | _ -> ()
    let byOutput = plans |> Seq.map (fun p -> p.Output, p) |> Map.ofSeq
    let byRead =
        plans |> Seq.filter (fun p -> p.Hoisted)
        |> Seq.collect (fun p -> p.Inputs |> List.map (fun i -> i.ReadId, p))
        |> Map.ofSeq
    byOutput, byRead
