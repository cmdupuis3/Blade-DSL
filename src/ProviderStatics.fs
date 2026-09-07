/// Bridges `let static A = alias.read(sample.vars.A)` to FOLD the store's
/// payload at compile time (staging contract clause 1: inputs are
/// immutable, so fold freely). Providers compile before StaticEval and
/// neither may reference the other, so the reader is registered into
/// StaticEval's provider hook here by TypeCheck.typeCheck, ahead of every
/// resolveStatics pass -- the PPL elaboration's own statics inherit the
/// fold for free.
///
/// Also the registry install point: each provider's ProviderSpec is
/// assembled/registered here, and the registered name set is bridged into
/// StaticEval so resolveStatics recognizes `import netcdf as nc`-style
/// imports.
///
/// Provenance not freshness: each fold records (path, variable, sha256) and
/// prints a provenance note at compile time; the hash log doubles as a
/// future incremental-fold memoization key.
module Blade.ProviderStatics

open Blade.StaticEval

/// (store path, variable name, sha256 hex) per compile-time fold.
let provenance = ResizeArray<string * string * string>()

let private hashCache = System.Collections.Concurrent.ConcurrentDictionary<string, string>()

/// SHA256 hex of a single file's bytes, memoized (the NetCDF fingerprint).
let private fileHash (path: string) : string =
    hashCache.GetOrAdd(path, fun p ->
        use sha = System.Security.Cryptography.SHA256.Create()
        sha.ComputeHash(System.IO.File.ReadAllBytes p)
        |> Array.map (sprintf "%02x")
        |> String.concat "")

/// Fold ceiling in elements. Beyond it the fold refuses: "large and closed"
/// inputs belong to the runtime/streaming schedule instead (see the
/// fold/residualize/stream table in ppl/NOTES.md).
let foldCeiling = 65536

/// Shape a flat row-major buffer into nested SVTuples by dim extents
/// (a rank-0 variable folds to its bare scalar).
let shapeValue (lens: int list) (leaf: int -> StaticValue) : StaticValue =
    let rec go (lens: int list) (offset: int) : StaticValue * int =
        match lens with
        | [] -> (leaf offset, offset + 1)
        | n :: rest ->
            let mutable off = offset
            let items =
                [ for _ in 1 .. n ->
                    let (v, off') = go rest off
                    off <- off'
                    v ]
            (SVTuple items, off)
    fst (go lens 0)

// NetCDF ProviderSpec (surface module name "netcdf"); assembled here so
// the provider implementation file needs no registry knowledge of its own.

let private netcdfAdapt (d: Blade.NetcdfProvider.NcVarData) : Blade.ProviderRegistry.ProviderVarData =
    { DimLengths = d.DimLengths
      Payload =
        match d.Payload with
        | Blade.NetcdfProvider.NcFloats xs -> Blade.ProviderRegistry.PFloats xs
        | Blade.NetcdfProvider.NcInts xs -> Blade.ProviderRegistry.PInts xs }

let netcdfSpec : Blade.ProviderRegistry.ProviderSpec = {
    Name = "netcdf"
    LoadAsModule = Blade.NetcdfProvider.loadAsModule
    ReadVarData = fun path varName ->
        Blade.NetcdfProvider.readVarData path varName |> Result.map netcdfAdapt
    GenReadVar = Blade.NetcdfProvider.CppNetcdf.genReadVar
    GenReadPacked = None  // packed (SymIdx/AntisymIdx) NetCDF I/O: future arc
    ReadWreathPool = None // OrbIdx (iterated-wreath) NetCDF I/O: same arc, refused
    GenReadCompoundVar = Some Blade.NetcdfProvider.CppNetcdf.genReadCompoundVar
    GenWriteVar = Blade.NetcdfProvider.CppNetcdf.genWriteVar
    GenStreamOpen = Some Blade.NetcdfProvider.CppNetcdf.genStreamOpen
    GenStreamFiber = Some Blade.NetcdfProvider.CppNetcdf.genStreamFiber
    GenStreamRowsOpen = None
    GenStreamRows = None
    StreamRowsBlock = None
    Includes = Blade.NetcdfProvider.CppNetcdf.genIncludes
    VarDimNames = fun path varName ->
        try
            let file = Blade.NetcdfProvider.load path
            file.Vars
            |> List.tryFind (fun v -> v.Name = varName)
            |> Option.map (fun v -> v.Dims |> List.map (_.Name))
        with _ -> None
    Fingerprint = fileHash
    VersionStamp = fun path ->
        try System.IO.File.GetLastWriteTimeUtc(path).Ticks
        with _ -> 0L
    LinkNeeds = "libnetcdf (NETCDF_DIR)"
}

/// Fold memoization: several resolveStatics passes run per compilation, so
/// the payload is read and provenance recorded ONCE, keyed on (provider,
/// path, var, versionStamp) -- a long-lived process re-reads only when the
/// store actually changed.
let private foldCache =
    System.Collections.Concurrent.ConcurrentDictionary<string * string * string * int64, Result<StaticValue, string>>()

let private readAndFoldUncached (provider: string) (path: string) (varName: string) : Result<StaticValue, string> =
    match Blade.ProviderRegistry.tryFind provider with
    | None ->
        Error $"provider '{provider}' is not registered -- was ProviderStatics.install () run?"
    | Some spec ->
        match spec.ReadVarData path varName with
        | Error e ->
            Error $"provider fold of '{varName}' from '{path}' failed: {e}"
        | Ok data ->
            let count = data.DimLengths |> List.fold (*) 1
            if count > foldCeiling then
                Error $"'{varName}' has {count} elements -- beyond the {foldCeiling}-element fold ceiling; large closed inputs take the runtime schedule (bind with a plain `let ... |> {provider}.read`)"
            else
                let h = spec.Fingerprint path
                provenance.Add((path, varName, h))
                eprintfn "[provenance] folded %s from %s@%s" varName path (h.Substring(0, min 12 h.Length))
                match data.Payload with
                | Blade.ProviderRegistry.PFloats xs -> Ok (shapeValue data.DimLengths (fun i -> SVFloat xs.[i]))
                | Blade.ProviderRegistry.PInts xs -> Ok (shapeValue data.DimLengths (fun i -> SVInt xs.[i]))

let private readAndFold (provider: string) (path: string) (varName: string) : Result<StaticValue, string> =
    let stamp =
        match Blade.ProviderRegistry.tryFind provider with
        | Some spec -> spec.VersionStamp path
        | None -> 0L
    foldCache.GetOrAdd((provider, path, varName, stamp), fun _ -> readAndFoldUncached provider path varName)

/// Axis extents of a store: dim name -> extent, read from the provider's
/// own metadata module -- the same read TypeCheck performs at `let store =
/// alias.load(...)`, pulled earlier since module elaborations resolve
/// `store.index.<dim>` before type checking runs. Memoized on the same key
/// as the payload fold; an unreadable store or unregistered provider yields
/// no axes, and type checking re-opens the store to diagnose the real fault.
let private axisCache =
    System.Collections.Concurrent.ConcurrentDictionary<string * string * string * int64, Map<string, int>>()

let private storeAxesUncached (provider: string) (path: string) (root: string) : Map<string, int> =
    match Blade.ProviderRegistry.tryFind provider with
    | None -> Map.empty
    | Some spec ->
        try
            let pm = spec.LoadAsModule (Blade.IR.IRBuilder()) root path
            pm.Types
            |> List.choose (function
                | Blade.IR.IRTDIndexType (n, idx) ->
                    match idx.Extent with
                    | Blade.IR.IRLit (Blade.IR.IRLitInt v) -> Some (n, int v)
                    | _ -> None
                | _ -> None)
            |> Map.ofList
        with _ -> Map.empty

let private axisExtent (provider: string) (path: string) (root: string) (dim: string) : int option =
    let stamp =
        match Blade.ProviderRegistry.tryFind provider with
        | Some spec -> spec.VersionStamp path
        | None -> 0L
    axisCache.GetOrAdd((provider, path, root, stamp), fun _ -> storeAxesUncached provider path root)
    |> Map.tryFind dim

/// Idempotent installation: register every provider spec, then bridge the
/// compile-time readers and the provider-name set into StaticEval's hooks --
/// and the axis-tag DECODERS into Types', which is the same seam one layer
/// over: a refusal message renders in TypeLower/TypeEnv, both of which compile
/// long before any provider and must not name one.
let install () =
    Blade.ProviderRegistry.register netcdfSpec
    Blade.ProviderRegistry.register Blade.ZarrProvider.spec
    Blade.ProviderRegistry.DimChunks.registerReader "zarr" Blade.ZarrProvider.dimChunkEdges
    Blade.ProviderRegistry.register Blade.CsvProvider.spec
    Blade.ProviderRegistry.register Blade.IcechunkProvider.spec
    registerProviderReader readAndFold
    registerProviderIndexReader axisExtent
    registerProviderNames (Blade.ProviderRegistry.names () |> Set.ofList)
    // Diagnostics-only: what turns `__icaxis|lat@wx:9f3a1c...` into `lat` in a
    // refusal message, and what recovers the mint table's recorded reason for a
    // split. Unregistered, both sites print exactly what they printed before.
    Blade.Types.registerProviderAxisTagDecoder Blade.IcechunkProvider.tryProviderTagName
    Blade.Types.registerProviderAxisSplitReason Blade.IcechunkProvider.trySplitReasonOfTag
