// Provider registry: the seam between the core compiler and data providers.
// A provider is a Blade-surface MODULE (`import netcdf as nc`) implemented
// under providers/; this registry reifies the provider contract (metadata
// load, compile-time fold read, runtime C++ read/write emission) as one
// record per provider, keyed by surface module name. Core files dispatch
// through `tryFind` rather than naming any provider directly; specs are
// registered by ProviderStatics.install () ahead of every pipeline pass.
// Compiles after IR.fs, before the provider implementations that construct
// their ProviderSpec against these types; StaticEval stays registry-free.
module Blade.ProviderRegistry

open Blade.IR
open Blade.Types

/// Provider-neutral compile-time payload for the static fold: dimension
/// extents plus the row-major flat buffer (float or int64 per elem type).
type ProviderVarData = {
    DimLengths: int list
    Payload: ProviderPayload
}
and ProviderPayload =
    | PFloats of float[]
    | PInts of int64[]

/// Options for a packed (SymIdx/AntisymIdx) read emission.
type PackedReadOpts = {
    /// Emit the MPI-distributed read: each rank does its balanced chunk of
    /// I/O, then MPI_Allgatherv restores the full pool on every rank. Set
    /// only with MPI scaffolding and an unwindowed read.
    Distribute: bool
    /// Sub-simplex window [lo, hi): materialize only cells with every
    /// coordinate in [lo, hi); the declared arrType is the WINDOW type.
    Window: (int64 * int64) option
}

/// The provider contract. One record per provider, registered under the
/// surface module name ("netcdf", "zarr").
type ProviderSpec = {
    /// Surface module name and registry key: `import <Name> as <alias>`.
    Name: string
    /// Compile-time metadata -> IRModule (named index types + dims/vars
    /// structs). Args: builder, moduleName (receiving binding's name), store path.
    LoadAsModule: IRBuilder -> string -> string -> IRModule
    /// Compile-time whole-payload read for the static fold (args: store path, variable name).
    ReadVarData: string -> string -> Result<ProviderVarData, string>
    /// Runtime C++ dense reader. Args: path, varName, cppVarName, arrType.
    GenReadVar: string -> string -> string -> IRArrayType -> string list
    /// Runtime C++ PACKED (SymIdx/AntisymIdx) reader: emits code assembling
    /// the canonical flat pool (ascending-lex cells x trailing block,
    /// row-major, or the window pool) into `<cppVarName>_flat` only -- the
    /// codegen intercept does allocation, copy, and release. None rejects
    /// packed reads and writes loudly. Args: path, varName, cppVarName, arrType, opts.
    GenReadPacked: (string -> string -> string -> IRArrayType -> PackedReadOpts -> string list) option
    /// ITERATED-WREATH (OrbIdx depth >= 2) pool capability plus its F#-side
    /// canonical-pool reader. Presence is the provider's wreath flag at every
    /// seam (C++ read via GenReadPacked, C++ write via GenWriteVar, and the
    /// interpreter's materialization all gate on it). None means the
    /// provider refuses wreath arrays loudly everywhere (every provider but
    /// zarr; see providers/ZarrTriangularSpec.md spec_version 2). Returns
    /// DimLengths = `[cardinality]` (the pool as one axis) with the payload
    /// in ascending-lex canonical order. Args: store path, variable name.
    ReadWreathPool: (string -> string -> Result<ProviderVarData, string>) option
    /// Runtime C++ compound (masked) reader; None rejects load_compound
    /// loudly. Args: path, varName, maskName, cppVarName, varArrType, maskArrType.
    GenReadCompoundVar: (string -> string -> string -> string -> IRArrayType -> IRArrayType -> string list) option
    /// Runtime C++ dense writer: reads the source array from an already-
    /// populated flat buffer `<cppVarName>_flat` (write intercept emits the
    /// flatten prologue). Args: path, varName, cppVarName, arrType, dimNames.
    GenWriteVar: string -> string -> string -> IRArrayType -> string list -> string list
    /// STREAMED fiber reads (`alias.stream`): emits (a) a hoisted prologue --
    /// open handles, metadata checks, `<cppVarName>_fiber_ext` -- and (b) the
    /// in-nest per-fiber read filling a caller DESTINATION buffer given the
    /// bound SITE index expressions (one per leading dense axis; allocated
    /// per kernel argument, since a comm kernel binds several fibers
    /// concurrently). None rejects `.stream` loudly. Open args: path,
    /// varName, cppVarName, arrType. Fiber args: adds destBufName and site
    /// index expressions. Handles are read-only, left open for the program's lifetime.
    GenStreamOpen: (string -> string -> string -> IRArrayType -> string list) option
    GenStreamFiber: (string -> string -> string -> string -> string list -> IRArrayType -> string list) option
    /// #include lines injected when a module reads/writes via this provider
    /// (packed/simplex reads also pull linearized_storage.hpp separately).
    Includes: unit -> string list
    /// Dimension names of a stored variable, so writing a provider-loaded
    /// array back out preserves them. None when the store carries none
    /// (writers fall back to synthesized dim<i>); must not throw on unreadable stores.
    VarDimNames: string -> string -> string list option
    /// Content fingerprint of a store for fold provenance (sha256 hex).
    Fingerprint: string -> string
    /// Cheap change stamp for fold memoization (e.g. mtime ticks; multi-file stores take the max over their files).
    VersionStamp: string -> int64
    /// Documentation of link-time needs (Build.fs remains scan-based).
    LinkNeeds: string
}

let private registry =
    System.Collections.Concurrent.ConcurrentDictionary<string, ProviderSpec>()

/// Idempotent registration (last write wins), mirroring StaticEval's builtin registry convention.
let register (spec: ProviderSpec) : unit =
    registry.[spec.Name] <- spec

let tryFind (name: string) : ProviderSpec option =
    match registry.TryGetValue name with
    | true, s -> Some s
    | _ -> None

/// Registered provider module names, sorted (diagnostics + StaticEval name-set bridge).
let names () : string list =
    registry.Keys |> List.ofSeq |> List.sort

/// The chunk edge of each dimension of a loaded store, by store BINDING
/// name (docs/plans/structural/07 §3.1): what `type A = Chunked<s.index.d,
/// store>` inherits. A provider that knows its chunk grid registers a
/// reader (store path -> dim -> edge, dims chunked uniformly across the
/// store's variables only); the load site records the answer beside the
/// IDE module so type registration never re-opens the store. Same
/// AsyncLocal discipline as IdeStores (parallel test compilation).
module DimChunks =
    open System.Threading

    let private readers =
        System.Collections.Concurrent.ConcurrentDictionary<string, string -> Map<string, int64>>()
    let private table = new AsyncLocal<Map<string, Map<string, int64>>>()
    let private entries () = match box table.Value with null -> Map.empty | _ -> table.Value

    /// A provider's reader: store path -> (dimension -> chunk edge).
    let registerReader (provider: string) (f: string -> Map<string, int64>) =
        readers.[provider] <- f

    let reset () = table.Value <- Map.empty

    /// Record the edges for a store binding, if its provider has a reader.
    let record (binding: string) (provider: string) (path: string) =
        match readers.TryGetValue provider with
        | true, f ->
            let edges = try f path with _ -> Map.empty
            table.Value <- Map.add binding edges (entries ())
        | _ -> ()

    /// The edge of `dim` in the store bound to `binding`, if recorded.
    let tryFind (binding: string) (dim: string) : int64 option =
        match Map.tryFind binding (entries ()) with
        | Some edges -> Map.tryFind dim edges
        | None -> None

    /// Whether the binding's provider recorded anything at all (to tell
    /// "unchunked dimension" from "provider has no chunk reader").
    let hasStore (binding: string) : bool = Map.containsKey binding (entries ())

/// IDE side-channel: the provider IRModule built at each `let store =
/// alias.load(path)` site, keyed by the store binding name. Lets Ide.fs
/// render dims/vars/index-type hovers by reusing the module already built
/// instead of re-opening the (possibly native) data file. AsyncLocal since
/// the test suite compiles programs in parallel, each in its own async context.
module IdeStores =
    open System.Threading

    let private store = new AsyncLocal<Map<string, IRModule>>()
    let private modules () = match box store.Value with null -> Map.empty | _ -> store.Value

    /// Fresh compilation: cleared before an IDE check runs typeCheck.
    let reset () =
        store.Value <- Map.empty
        DimChunks.reset ()

    /// Record the module built at a provider load site (last write wins).
    let record (name: string) (pm: IRModule) =
        store.Value <- Map.add name pm (modules ())

    /// IDE-facing: the module recorded for a store binding, if any.
    let tryFind (name: string) : IRModule option = Map.tryFind name (modules ())
