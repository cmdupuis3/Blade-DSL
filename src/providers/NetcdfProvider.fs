// Blade-DSL NetCDF Type Provider: compile-time metadata extraction (dims, var names, types, shapes) for IR array types; data I/O is generated C++ calling the NetCDF C API.

module Blade.NetcdfProvider

open System.Runtime.InteropServices
open Blade.IR
open Blade.Types

// NetCDF Metadata Types

type NcDim = {
    Name: string
    Length: int64
}

type NcVar = {
    Name: string
    Dims: NcDim list   // slowest-changing first (C/row-major order)
    TypeCode: int      // NC_* type constant
}

type NcFile = {
    Path: string
    Dims: NcDim list
    Vars: NcVar list
}

// P/Invoke Bindings to libnetcdf

module private NcFFI =

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_open(string path, int mode, int& fileId)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_close(int fileId)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_ndims(int fileId, int& ndims)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_dimids(int fileId, int& ndims, int[] dimids)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_dim(int fileId, int dimid, byte[] name, int64& length)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_nvars(int fileId, int& nvars)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_varids(int fileId, int& nvars, int[] varids)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_varname(int fileId, int varid, byte[] name)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_vartype(int fileId, int varid, int& xtype)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_varndims(int fileId, int varid, int& ndims)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_inq_vardimid(int fileId, int varid, int[] dimids)

    // Data reads for the compile-time fold (provider-backed statics): libnetcdf
    // converts any numeric type to the requested C type (double + longlong
    // cover the whole ncTypeToElemType surface) -- the same functions
    // CppNetcdf.genReadVar emits as source text.
    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_get_var_double(int fileId, int varid, double[] data)

    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_get_var_longlong(int fileId, int varid, int64[] data)

    // The teardown twin of the calls above. netcdf-c >= 4.9 only; a build
    // without it raises EntryPointNotFoundException at the call, which the
    // registration below swallows -- such a build has no thread-spawning
    // closure to shut down either.
    [<DllImport("netcdf", CallingConvention = CallingConvention.Cdecl)>]
    extern int nc_finalize()

    let check (status: int) (msg: string) =
        if status <> 0 then failwith $"NetCDF error ({status}): {msg}"

    // WHICH FILE the externs above bind to is decided by the process-wide
    // resolver in Platforms.fs, not by the ambient search path: it takes
    // NETCDF_DIR through the same `findSharedLib` that assembles the g++ link
    // line, so the in-process load and the link line can no longer disagree
    // about the filename. They did disagree -- these externs are declared
    // under the stem "netcdf", a Windows probe for that looks only for
    // netcdf.dll, and MSYS2's package ships libnetcdf.dll -- which left
    // `blade doctor` reporting netcdf healthy while every compile-time
    // provider fold failed to load it.
    //
    // Forced HERE, from the one entry point that reaches an extern first
    // (every other wrapper below needs a fileId only `openFile` can hand it),
    // because nothing otherwise guarantees this module is initialized before
    // the first P/Invoke.
    let ensureNativeLibrary () = Blade.Platforms.ensureNativeResolver ()

    // BLADE.EXE ITSELF NEEDS THE SAME TEARDOWN THE GENERATED PROGRAMS GET.
    //
    // The compile-time fold loads libnetcdf into the COMPILER's process, and
    // on builds whose closure spawns threads (MSYS2's links libcurl and the
    // AWS C++ SDK), process exit then deadlocks in DLL_PROCESS_DETACH exactly
    // as it did in the emitted programs -- every test passes, the totals
    // print, and Blade.exe never returns to its caller. Generated code got
    // `std::atexit(nc_finalize)`; this is the managed twin: ProcessExit fires
    // while the CLR is still orderly, before the loader starts detaching.
    //
    // Registered on first USE, not at resolver install: the resolver is
    // shared with libm, and a process that never touched netcdf should not
    // call into it at exit. EntryPointNotFoundException = a pre-4.9 netcdf,
    // which has no finalizer and no such closure; anything else at exit is
    // swallowed too -- a teardown hiccup must not turn a green run red.
    let private exitFinalizer =
        lazy (
            System.AppDomain.CurrentDomain.ProcessExit.Add (fun _ ->
                try nc_finalize () |> ignore with _ -> ()))

    let registerExitFinalizer () = exitFinalizer.Force ()

// Safe Wrappers

module private NcQuery =

    let openFile (path: string) (mode: int) =
        NcFFI.ensureNativeLibrary ()
        NcFFI.registerExitFinalizer ()
        let mutable id = 0
        NcFFI.nc_open(path, mode, &id) |> fun s -> NcFFI.check s $"opening '{path}'"
        id

    let closeFile (fileId: int) =
        NcFFI.nc_close(fileId) |> fun s -> NcFFI.check s "closing file"

    let getDimIds (fileId: int) =
        let mutable ndims = 0
        NcFFI.nc_inq_ndims(fileId, &ndims) |> fun s -> NcFFI.check s "querying ndims"
        if ndims = 0 then [||]
        else
            let ids = Array.zeroCreate ndims
            let mutable n = ndims
            NcFFI.nc_inq_dimids(fileId, &n, ids) |> fun s -> NcFFI.check s "querying dimids"
            ids

    let getDim (fileId: int) (dimId: int) =
        let buf : byte[] = Array.zeroCreate 256
        let mutable length = 0L
        NcFFI.nc_inq_dim(fileId, dimId, buf, &length) |> fun s -> NcFFI.check s $"querying dim {dimId}"
        let nul = System.Array.IndexOf(buf, 0uy)
        let len = if nul >= 0 then nul else buf.Length
        let name = System.Text.Encoding.ASCII.GetString(buf, 0, len)
        { Name = name; Length = length }

    let getVarIds (fileId: int) =
        let mutable nvars = 0
        NcFFI.nc_inq_nvars(fileId, &nvars) |> fun s -> NcFFI.check s "querying nvars"
        if nvars = 0 then [||]
        else
            let ids = Array.zeroCreate nvars
            let mutable n = nvars
            NcFFI.nc_inq_varids(fileId, &n, ids) |> fun s -> NcFFI.check s "querying varids"
            ids

    let getVar (fileId: int) (varId: int) (dimLookup: Map<int, NcDim>) =
        let nameBuf : byte[] = Array.zeroCreate 256
        NcFFI.nc_inq_varname(fileId, varId, nameBuf) |> fun s -> NcFFI.check s $"querying var {varId} name"
        let nul = System.Array.IndexOf(nameBuf, 0uy)
        let len = if nul >= 0 then nul else nameBuf.Length
        let name = System.Text.Encoding.ASCII.GetString(nameBuf, 0, len)

        let mutable xtype = 0
        NcFFI.nc_inq_vartype(fileId, varId, &xtype) |> fun s -> NcFFI.check s $"querying var {varId} type"

        let mutable ndims = 0
        NcFFI.nc_inq_varndims(fileId, varId, &ndims) |> fun s -> NcFFI.check s $"querying var {varId} ndims"

        let dimIds = Array.zeroCreate ndims
        NcFFI.nc_inq_vardimid(fileId, varId, dimIds) |> fun s -> NcFFI.check s $"querying var {varId} dimids"

        let dims =
            dimIds
            |> Array.toList
            |> List.map (fun did ->
                match Map.tryFind did dimLookup with
                | Some dim -> dim
                | None -> failwith $"Dimension ID {did} not found")

        { Name = name; Dims = dims; TypeCode = xtype }

// File Loading (compile-time metadata extraction)

/// Loads all metadata from a NetCDF file (opens read-only, extracts dims/vars, closes).
let load (path: string) : NcFile =
    if not (System.IO.File.Exists path) then
        failwith $"NetCDF file not found: '{path}' (resolved against cwd '{(System.IO.Directory.GetCurrentDirectory())}')"
    let fileId = NcQuery.openFile path 0  // NC_NOWRITE = 0
    try
        let dimIds = NcQuery.getDimIds fileId
        let dims = dimIds |> Array.map (NcQuery.getDim fileId) |> Array.toList
        let dimLookup = dimIds |> Array.mapi (fun i id -> (id, dims.[i])) |> Map.ofArray
        let varIds = NcQuery.getVarIds fileId
        let vars = varIds |> Array.map (fun vid -> NcQuery.getVar fileId vid dimLookup) |> Array.toList
        { Path = path; Dims = dims; Vars = vars }
    finally
        NcQuery.closeFile fileId

// Compile-time DATA read (provider-backed statics)

/// A variable's payload read at compile time: dimension extents plus the
/// row-major flat buffer (host-ordered; libnetcdf handles format/chunking/endianness).
type NcVarData = {
    DimLengths: int list
    Payload: NcPayload
}
and NcPayload =
    | NcFloats of float[]
    | NcInts of int64[]

/// Reads a variable's full payload at compile time. Float-coded variables
/// arrive as doubles; every integer coding arrives as int64 (mirrors ncTypeToElemType's collapse).
let readVarData (path: string) (varName: string) : Result<NcVarData, string> =
    try
        let fileId = NcQuery.openFile path 0  // NC_NOWRITE
        try
            let dimIds = NcQuery.getDimIds fileId
            let dims = dimIds |> Array.map (NcQuery.getDim fileId) |> Array.toList
            let dimLookup = dimIds |> Array.mapi (fun i id -> (id, dims.[i])) |> Map.ofArray
            let hit =
                NcQuery.getVarIds fileId
                |> Array.tryPick (fun vid ->
                    let v = NcQuery.getVar fileId vid dimLookup
                    if v.Name = varName then Some (vid, v) else None)
            match hit with
            | None -> Error $"variable '{varName}' not found in '{path}'"
            | Some (vid, v) ->
                let lens = v.Dims |> List.map (fun d -> int d.Length)
                let count = lens |> List.fold (*) 1
                match v.TypeCode with
                | 5 | 6 ->  // NC_FLOAT, NC_DOUBLE
                    let buf : float[] = Array.zeroCreate (max count 1)
                    NcFFI.check (NcFFI.nc_get_var_double(fileId, vid, buf)) $"reading '{varName}'"
                    Ok { DimLengths = lens; Payload = NcFloats buf }
                | _ ->
                    let buf : int64[] = Array.zeroCreate (max count 1)
                    NcFFI.check (NcFFI.nc_get_var_longlong(fileId, vid, buf)) $"reading '{varName}'"
                    Ok { DimLengths = lens; Payload = NcInts buf }
        finally
            NcQuery.closeFile fileId
    with ex ->
        Error ex.Message

// Mapping to Blade IR Types

/// Maps a NetCDF type code to the nearest Blade ElemType (per-code mapping below).
let ncTypeToElemType (tc: int) : ElemType =
    match tc with
    | 1  -> ETInt64     // NC_BYTE     (signed 8-bit  -> Int)
    | 2  -> ETInt32     // NC_CHAR     (8-bit char    -> Int)
    | 3  -> ETInt64     // NC_SHORT    (16-bit signed -> Int)
    | 4  -> ETInt64     // NC_INT      (32-bit signed -> Int)
    | 5  -> ETFloat32   // NC_FLOAT
    | 6  -> ETFloat64   // NC_DOUBLE
    | 7  -> ETInt64     // NC_UBYTE    (unsigned 8    -> Int)
    | 8  -> ETInt64     // NC_USHORT   (unsigned 16   -> Int)
    | 9  -> ETInt64     // NC_UINT     (unsigned 32   -> Int)
    | 10 -> ETInt64     // NC_INT64
    | 11 -> ETInt64     // NC_UINT64   (unsigned 64   -> Int)
    | _  -> failwith $"Unsupported NetCDF type code: {tc}"

/// Builds a named IRIndexType from an NcDim; the name is this index space's nominal identity.
let ncDimToNamedIndexType (builder: IRBuilder) (dim: NcDim) : string * IRIndexType =
    let idx = {
        Id = builder.FreshId()
        Rank = 1
        Extent = IRLit (IRLitInt dim.Length)
        Symmetry = SymNone
        Tag = None; IxKind = IxKPlain
        Kind = SDimension
        Dependencies = []
    }
    (dim.Name, idx)

/// Builds an IRArrayType for a variable, reusing the module's named index
/// types (dimMap: dim name -> IRIndexType) so variables sharing a dimension get the same reference.
let ncVarToArrayType (dimMap: Map<string, IRIndexType>) (var: NcVar) : IRArrayType =
    let indexTypes =
        var.Dims
        |> List.map (fun dim ->
            match Map.tryFind dim.Name dimMap with
            | Some idx -> idx
            | None -> failwith $"Dimension '{dim.Name}' not found in module")
    {
        ElemType = IRTScalar (ncTypeToElemType var.TypeCode)
        IndexTypes = indexTypes
        IsVirtual = false
        Identity = Some (AIDVariable var.Name)
    }

/// Converts an NcFile into an IRModule using structs for dims/vars.
/// Coordinate variables (1D arrays named after their dimension) go in
/// `dims`; all other data variables go in `vars`. Named index types live
/// at module scope. Access: sample.dims.xdim, sample.vars.A.
///
/// Produces IR equivalent to:
///
///   module sample
///       type xdim = Idx<20>
///       type ydim = Idx<30>
///       type zdim = Idx<50>
///
///       struct sample__dims = {
///           xdim: Array<Int64, Idx<xdim>>
///           ydim: Array<Int64, Idx<ydim>>
///           zdim: Array<Int64, Idx<zdim>>
///       }
///
///       struct sample__vars = {
///           A: Array<Float32, Idx<zdim>, Idx<ydim>, Idx<xdim>>
///       }
///
/// The structs are namespaced by `moduleName` rather than the bare
/// "dims"/"vars": one flat TypeDefs map means literal names would let a
/// second load silently overwrite the first; `fieldFor` resolves the
/// suffix, so `.dims`/`.vars` stay unchanged.
///
/// externalDimMap lets a schema supply shared index types across files.
let ncFileToModule
    (builder: IRBuilder)
    (moduleName: string)
    (file: NcFile)
    (externalDimMap: Map<string, IRIndexType> option)
    : IRModule =

    // Build named index types
    let (indexTypeDefs, dimMap) =
        match externalDimMap with
        | Some dm ->
            ([], dm)
        | None ->
            let pairs = file.Dims |> List.map (ncDimToNamedIndexType builder)
            let typeDefs =
                pairs |> List.map (fun (name, idx) -> IRTDIndexType(name, idx))
            let dm = pairs |> Map.ofList
            (typeDefs, dm)

    // dims struct: coordinate arrays (one per dimension)
    let dimsFields =
        file.Dims |> List.map (fun dim ->
            let idx = dimMap.[dim.Name]
            let arrType = mkArrayArrow [idx] (IRTScalar ETInt64) (Some (AIDVariable dim.Name))
            (dim.Name, arrType))

    let dimsStruct = IRTDStruct($"{moduleName}__dims", dimsFields)

    // vars struct: data variables only (exclude coordinate variables)
    let dimNames = file.Dims |> List.map _.Name |> Set.ofList
    let isCoordinateVar (v: NcVar) =
        dimNames.Contains v.Name
        && v.Dims.Length = 1
        && v.Dims.[0].Name = v.Name

    let varsFields =
        file.Vars
        |> List.filter (not << isCoordinateVar)
        |> List.map (fun v ->
            let arrType = ncVarToArrayType dimMap v
            (v.Name, mkArrayLike arrType))

    let varsStruct = IRTDStruct($"{moduleName}__vars", varsFields)

    {
        Name = moduleName
        Types = indexTypeDefs @ [dimsStruct; varsStruct]
        Functions = []
        Bindings = []
        StaticFunctionUsage = Map.empty
        ProviderReads = Map.empty
        ProviderWrites = Map.empty
        RandomInits = Map.empty
        CompoundInits = Map.empty
        SparseInits = Map.empty
        MutableArrayLets = Set.empty
        DerivedFuncOrigins = Map.empty
    }

/// Convenience: load a file and produce a module in one step.
let loadAsModule (builder: IRBuilder) (moduleName: string) (path: string) : IRModule =
    let file = load path
    ncFileToModule builder moduleName file None

// NetCDF C++ Code Generation Helpers: produce the C++ fragments that do
// the actual data I/O at runtime.

module CppNetcdf =

    /// Map Blade ElemType to the nc_type constant name for generated code
    let elemTypeToNcMacro = function
        | ETFloat32 -> "NC_FLOAT"
        | ETFloat64 -> "NC_DOUBLE"
        | ETInt32   -> "NC_INT"
        | ETInt64   -> "NC_INT64"
        | _         -> "NC_DOUBLE"  // fallback

    /// Wraps a fallible nc_* call: captures its status into <cppVarName>_ncstat
    /// and exits loudly on failure (an ignored status silently leaves the data
    /// buffer uninitialized). `callExpr` omits its trailing semicolon; `context`
    /// is spliced into a C++ string literal, so it must avoid double quotes.
    let private ncChecked (cppVarName: string) (context: string) (callExpr: string) : string list =
        [
            $"{cppVarName}_ncstat = {callExpr};"
            $"if ({cppVarName}_ncstat != NC_NOERR) {{ std::cerr << \"NetCDF error ({context}): \" << nc_strerror({cppVarName}_ncstat) << std::endl; std::exit(1); }}"
        ]

    /// Runtime shape guard (BL8012), emitted between `nc_inq_varid` and the
    /// first `nc_get_var*` of a variable: the variable's rank and every
    /// dimension length on the OPENED handle must equal the extents lowering
    /// baked from the file that was present at compile time. `nc_get_var_*`
    /// writes the variable's WHOLE current extent into the caller's buffer,
    /// and the readers size that buffer from the baked extents -- so a file
    /// whose dimensions grew since compilation overran the buffer, and one
    /// that shrank or reordered its axes read garbage into the cells the
    /// program indexes, both silently (plan-fortran-killer-2.md section 7).
    /// The interpreter's materializeProviderRead is the twin.
    ///
    /// `v` is the binding's C++ stem (owner of `_ncid` / `_ncstat`); `tag`
    /// keeps the guard's temporaries distinct per variable within one stem
    /// (a compound read guards the variable AND its mask); `expected` are
    /// C++ expressions for the baked extents, in dimension order.
    let private ncShapeGuard (v: string) (tag: string) (varIdExpr: string) (humanName: string)
                             (filePath: string) (expected: string list) : string list =
        let rank = expected.Length
        let mismatch (detail: string) =
            [ $"    std::cerr << \"Blade runtime: NetCDF variable '{humanName}' in '{filePath}' {detail} << std::endl;"
              "    blade_rt::panic(\"BL8012\", \"provider shape mismatch\", nullptr, 0);" ]
        [ $"int {v}_{tag}_ndims;" ]
        @ ncChecked v $"querying the rank of '{humanName}' in '{filePath}'"
            $"nc_inq_varndims({v}_ncid, {varIdExpr}, &{v}_{tag}_ndims)"
        @ [ $"if ({v}_{tag}_ndims != {rank}) {{" ]
        @ mismatch $"has rank \" << {v}_{tag}_ndims << \" at run time; the program was compiled against rank {rank}\""
        @ [ "}" ]
        @ (if rank = 0 then [] else
            [ $"int {v}_{tag}_dimids[{rank}];" ]
            @ ncChecked v $"querying the dimensions of '{humanName}' in '{filePath}'"
                $"nc_inq_vardimid({v}_ncid, {varIdExpr}, {v}_{tag}_dimids)"
            @ [ for d in 0 .. rank - 1 do
                    yield $"size_t {v}_{tag}_dimlen_{d};"
                    yield! ncChecked v $"querying dimension {d} of '{humanName}' in '{filePath}'"
                              $"nc_inq_dimlen({v}_ncid, {v}_{tag}_dimids[{d}], &{v}_{tag}_dimlen_{d})"
                    yield $"if ({v}_{tag}_dimlen_{d} != (size_t)({expected.[d]})) {{"
                    yield! mismatch $"has dimension {d} of length \" << {v}_{tag}_dimlen_{d} << \" at run time; the program was compiled against \" << {expected.[d]}"
                    yield "}" ])

    /// Generate C++ code to open a NetCDF file and read a variable
    let genReadVar (filePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let rank = arrType.IndexTypes.Length
        // NetCDF only supports primitive numeric types; non-primitive
        // elements are unsupported here (would need new NetCDF machinery).
        let primElem =
            match arrType.ElemType with
            | IRTScalar et -> et
            | _ -> ETFloat64  // NetCDF doesn't support compound elem types yet.
        let elemCpp =
            match primElem with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32   -> "int"
            | ETInt64   -> "long long"
            | _         -> "double"

        let extentsFromDims =
            arrType.IndexTypes
            |> List.mapi (fun i idx ->
                match idx.Extent with
                | IRLit (IRLitInt n) -> $"size_t {cppVarName}_extent_{i} = {n};"
                | _ -> $"size_t {cppVarName}_extent_{i} = /* dynamic */;")

        let extentNames =
            arrType.IndexTypes |> List.mapi (fun i _ -> $"{cppVarName}_extent_{i}")
        let ncGetSuffix =
            match primElem with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32 -> "int"
            | ETInt64 -> "longlong"
            | _ -> "double"
        // Flat read: NetCDF variables are stored contiguous (row-major), so
        // read into a flat buffer first; each nc_* call is status-checked so a
        // silent failure can't hand the copy loop an uninitialized buffer.
        let flatRead =
            [
                $"// Read {varName} from {filePath}"
                $"int {cppVarName}_ncid, {cppVarName}_varid, {cppVarName}_ncstat;"
            ]
            @ ncChecked cppVarName $"opening '{filePath}' to read {varName}"
                $"nc_open(\"{filePath}\", NC_NOWRITE, &{cppVarName}_ncid)"
            @ ncChecked cppVarName $"locating variable '{varName}' in '{filePath}'"
                $"nc_inq_varid({cppVarName}_ncid, \"{varName}\", &{cppVarName}_varid)"
            @ extentsFromDims
            // BL8012 before the buffer exists: nc_get_var fills the variable's
            // CURRENT extent, the buffer below is sized from the baked one.
            @ ncShapeGuard cppVarName "var" $"{cppVarName}_varid" varName filePath extentNames
            @ [
                $"""{elemCpp}* {cppVarName}_flat = new {elemCpp}[{(String.concat " * " extentNames)}];"""
            ]
            @ ncChecked cppVarName $"reading variable '{varName}' from '{filePath}'"
                $"nc_get_var_{ncGetSuffix}({cppVarName}_ncid, {cppVarName}_varid, {cppVarName}_flat)"
            @ [
                $"nc_close({cppVarName}_ncid);"
            ]
        // Materialize the nested-pointer Array indexed as <v>[i][j]...: allocate<>
        // builds the nested structure, the flat buffer is copied in (runtime-
        // bounded loops compile fast, unlike a baked literal) and released.
        // ProviderReads routes a maskless spec here, vs genReadCompoundVar.
        let idxVars = [ for i in 0 .. rank - 1 -> $"{cppVarName}_i{i}" ]
        let openLoops =
            idxVars |> List.mapi (fun d iv ->
                let ind = String.replicate d "    "
                $"{ind}for (size_t {iv} = 0; {iv} < {extentNames.[d]}; {iv}++) {{")
        let nestedSub = idxVars |> List.map (sprintf "[%s]") |> String.concat ""
        // Row-major flat index (Horner): (((i0)*ext1 + i1)*ext2 + i2)... matches
        // NetCDF's contiguous storage order.
        let flatIdx =
            let mutable acc = idxVars.[0]
            for i in 1 .. rank - 1 do
                acc <- $"({acc}) * {extentNames.[i]} + {idxVars.[i]}"
            acc
        let bodyInd = String.replicate rank "    "
        let materialize =
            [
                $"""size_t {cppVarName}_extents[] = {{ {(String.concat ", " extentNames)} }};"""
                $"Array<{elemCpp}, {rank}> {cppVarName} = {{ allocate<typename promote<{elemCpp}, {rank}>::type, nullptr>({cppVarName}_extents), {cppVarName}_extents }};"
            ]
            @ openLoops
            @ [ $"{bodyInd}{cppVarName}{nestedSub} = {cppVarName}_flat[{flatIdx}];" ]
            @ [ for d in rank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
            @ [ $"delete[] {cppVarName}_flat;" ]
        flatRead @ materialize

    /// Reads a variable as a COMPOUND (masked) array, triggered only by
    /// load_compound: `maskName` is any integer array (nonzero = present);
    /// the dense var is scattered into a compact buffer of cardinality ==
    /// popcount(mask). All RANK dims are compound, so the result is a scalar
    /// nested_array_utilities::Compound<T, RANK> (heap-allocated, non-owning
    /// per the allocate<> convention).
    ///
    /// Scatter ordering: compound_index_t::enumerate walks tuples row-major,
    /// assigning rank = row-major prefix popcount, matching nc_get_var's own
    /// row-major reads -- so one sequential copy over set cells reproduces
    /// the compact layout exactly, no per-cell linearize() needed.
    let genReadCompoundVar
            (filePath: string) (varName: string) (maskName: string)
            (cppVarName: string) (varArrType: IRArrayType) (maskArrType: IRArrayType) : string list =
        // The mask covers the leading (compound) dims; remaining variable dims
        // are regular trailing dims folded into a runtime trailing_stride.
        let leadRank = maskArrType.IndexTypes.Length
        let primElem =
            match varArrType.ElemType with
            | IRTScalar et -> et
            | _ -> ETFloat64
        let elemCpp =
            match primElem with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32   -> "int"
            | ETInt64   -> "long long"
            | _         -> "double"
        let maskElem =
            match maskArrType.ElemType with
            | IRTScalar et -> et
            | _ -> ETInt64
        let maskCpp =
            match maskElem with
            | ETInt32 -> "int"
            | ETInt64 -> "long long"
            | ETBool  -> "signed char"
            | _       -> "long long"
        // nc_get_var_<suffix> for an ElemType (mask reads via schar when bool).
        let ncGet et =
            match et with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32   -> "int"
            | ETInt64   -> "longlong"
            | ETBool    -> "schar"
            | _         -> "double"

        let extentsFromDims =
            varArrType.IndexTypes
            |> List.mapi (fun i idx ->
                match idx.Extent with
                | IRLit (IRLitInt n) -> $"size_t {cppVarName}_extent_{i} = {n};"
                | _ -> $"size_t {cppVarName}_extent_{i} = /* dynamic */;")
        let extentNames =
            varArrType.IndexTypes |> List.mapi (fun i _ -> $"{cppVarName}_extent_{i}")
        let v = cppVarName
        let leadExtentNames = extentNames |> List.truncate leadRank
        let trailExtentNames = extentNames |> List.skip leadRank
        let gridExpr = leadExtentNames |> String.concat " * "
        let trailExpr = match trailExtentNames with | [] -> "1" | xs -> String.concat " * " xs
        let totalExpr = extentNames |> String.concat " * "
        let leadExtentsInit = leadExtentNames |> String.concat ", "

        [
            $"// Read compound {varName} (masked by {maskName}) from {filePath}"
            $"int {v}_ncid, {v}_varid, {v}_maskid, {v}_ncstat;"
        ]
        @ ncChecked v $"opening '{filePath}' to read {varName}"
            $"nc_open(\"{filePath}\", NC_NOWRITE, &{v}_ncid)"
        @ extentsFromDims
        @ [
            // grid = masked leading cells; trail = regular trailing stride; total = dense size
            $"size_t {v}_grid = {gridExpr};"
            $"size_t {v}_trail = {trailExpr};"
            $"size_t {v}_total = {totalExpr};"
            // dense variable (all dims)
            $"{elemCpp}* {v}_dense = new {elemCpp}[{v}_total];"
        ]
        @ ncChecked v $"locating variable '{varName}' in '{filePath}'"
            $"nc_inq_varid({v}_ncid, \"{varName}\", &{v}_varid)"
        // BL8012 twins: the dense variable against all its baked extents, the
        // mask against the leading (compound) ones -- each before its read.
        @ ncShapeGuard v "var" $"{v}_varid" varName filePath extentNames
        @ ncChecked v $"reading variable '{varName}' from '{filePath}'"
            $"nc_get_var_{ncGet primElem}({v}_ncid, {v}_varid, {v}_dense)"
        @ [
            // integer mask over the leading masked dims -- size is grid, not total
            $"{maskCpp}* {v}_maskraw = new {maskCpp}[{v}_grid];"
        ]
        @ ncChecked v $"locating mask '{maskName}' in '{filePath}'"
            $"nc_inq_varid({v}_ncid, \"{maskName}\", &{v}_maskid)"
        @ ncShapeGuard v "mask" $"{v}_maskid" maskName filePath leadExtentNames
        @ ncChecked v $"reading mask '{maskName}' from '{filePath}'"
            $"nc_get_var_{ncGet maskElem}({v}_ncid, {v}_maskid, {v}_maskraw)"
        @ [
            $"nc_close({v}_ncid);"
            // int -> std::vector<bool> (nonzero = present): the load_compound conversion
            $"std::vector<bool> {v}_maskvec({v}_grid);"
            $"for (size_t {v}_i = 0; {v}_i < {v}_grid; {v}_i++) {v}_maskvec[{v}_i] = ({v}_maskraw[{v}_i] != 0);"
            $"delete[] {v}_maskraw;"
            // compound index over the leading masked dims
            $"std::array<size_t, {leadRank}> {v}_extents = {{ {leadExtentsInit} }};"
            $"compound_index_t<{leadRank}>* {v}_idx = new compound_index_t<{leadRank}>(\"{varName}\", {v}_extents, {v}_maskvec);"
            // compact backing: present leading cells x trailing block
            $"{elemCpp}* {v}_compact = new {elemCpp}[{v}_idx->cardinality * {v}_trail];"
            // scatter: copy each present cell's trailing block (row-major
            // prefix-popcount order); string-concatenated so the count can't drift.
            ("{ size_t " + v + "_r = 0; for (size_t " + v + "_c = 0; " + v + "_c < " + v + "_grid; " + v + "_c++) if (" + v + "_maskvec[" + v + "_c]) { for (size_t " + v + "_t = 0; " + v + "_t < " + v + "_trail; " + v + "_t++) " + v + "_compact[" + v + "_r * " + v + "_trail + " + v + "_t] = " + v + "_dense[" + v + "_c * " + v + "_trail + " + v + "_t]; " + v + "_r++; } }")
            $"delete[] {v}_dense;"
            // bundle into the non-owning Compound wrapper (trailing_stride = _trail; 1 when all dims are masked)
            $"nested_array_utilities::Compound<{elemCpp}, {leadRank}> {v} {{ {v}_compact, {v}_idx, {v}_trail }};"
        ]

    /// Generates C++ to write a variable to a NetCDF file. dimNames provides
    /// the dimension names from the module's IRTDIndexType defs.
    let genWriteVar (filePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (dimNames: string list) : string list =
        // Same primitive-elem extraction as genReadVar.
        let primElem =
            match arrType.ElemType with
            | IRTScalar et -> et
            | _ -> ETFloat64  // NetCDF compound types unsupported.
        let elemCpp =
            match primElem with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32   -> "int"
            | ETInt64   -> "long long"
            | _         -> "double"

        let ncType = elemTypeToNcMacro primElem
        let rank = arrType.IndexTypes.Length

        let dimDefs =
            arrType.IndexTypes
            |> List.mapi (fun i idx ->
                let dimName =
                    if i < dimNames.Length then dimNames.[i]
                    else $"dim{i}"
                let extent =
                    match idx.Extent with
                    | IRLit (IRLitInt n) -> string n
                    | _ -> "0 /* unlimited */"
                ncChecked cppVarName $"defining dimension '{dimName}' in '{filePath}'"
                    ($"nc_def_dim({cppVarName}_ncid, \"{dimName}\", {extent}, &{cppVarName}_dimids[{i}])"))
            |> List.concat

        [
            $"// Write {varName} to {filePath}"
            $"int {cppVarName}_ncid, {cppVarName}_varid, {cppVarName}_ncstat;"
            $"int {cppVarName}_dimids[{rank}];"
        ]
        @ ncChecked cppVarName $"creating '{filePath}' to write {varName}"
            $"nc_create(\"{filePath}\", NC_CLOBBER | NC_NETCDF4, &{cppVarName}_ncid)"
        @ dimDefs
        @ ncChecked cppVarName $"defining variable '{varName}' in '{filePath}'"
            ($"nc_def_var({cppVarName}_ncid, \"{varName}\", {ncType}, {rank}, {cppVarName}_dimids, &{cppVarName}_varid)")
        @ ncChecked cppVarName $"ending define mode for '{filePath}'"
            $"nc_enddef({cppVarName}_ncid)"
        @ ncChecked cppVarName $"writing variable '{varName}' to '{filePath}'"
            (sprintf "nc_put_var_%s(%s_ncid, %s_varid, %s_flat)"
                (match primElem with
                 | ETFloat32 -> "float"
                 | ETFloat64 -> "double"
                 | ETInt32 -> "int"
                 | ETInt64 -> "longlong"
                 | _ -> "double")
                cppVarName cppVarName cppVarName)
        // nc_close flushes buffered writes, so its status matters here (unlike the read paths).
        @ ncChecked cppVarName $"closing '{filePath}' after writing {varName}"
            $"nc_close({cppVarName}_ncid)"

    /// STREAMED fiber reads: hoisted prologue -- opens the file once and
    /// declares the start/count vectors for nc_get_vara; the handle stays open for the program's lifetime.
    let genStreamOpen (filePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let v = cppVarName
        let primElem =
            match arrType.ElemType with
            | IRTScalar et -> et
            | _ -> ETFloat64
        let elemCpp =
            match primElem with
            | ETFloat32 -> "float"
            | ETFloat64 -> "double"
            | ETInt32   -> "int"
            | ETInt64   -> "long long"
            | _         -> "double"
        let rank = arrType.IndexTypes.Length
        if rank < 2 then
            failwith $"NetCDF stream of '{varName}': needs at least one site dim plus the trailing fiber axis (rank >= 2)"
        if arrType.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone || ix.Rank <> 1) then
            failwith $"NetCDF stream of '{varName}': dense variables only"
        let extents =
            arrType.IndexTypes |> List.map (fun ix ->
                match ix.Extent with
                | IRLit (IRLitInt n) -> n
                | _ -> failwith $"NetCDF stream of '{varName}' requires literal extents")
        let fiberLen = List.last extents
        ignore elemCpp
        [ $"// Stream {varName} from {filePath} (fiber reads inlined at the S/T boundary)"
          $"int {v}_ncid, {v}_varid, {v}_ncstat;" ]
        @ ncChecked v $"opening '{filePath}' to stream {varName}"
            $"nc_open(\"{filePath}\", NC_NOWRITE, &{v}_ncid)"
        @ ncChecked v $"locating variable '{varName}' in '{filePath}'"
            $"nc_inq_varid({v}_ncid, \"{varName}\", &{v}_varid)"
        // BL8012 at open, not at the first fiber: libnetcdf would refuse an
        // out-of-range nc_get_vara (NC_EEDGE), but only inside the nest, after
        // the program has begun emitting output from the fibers before it.
        @ ncShapeGuard v "var" $"{v}_varid" varName filePath (extents |> List.map string)
        @ [ $"size_t {v}_fiber_ext[1] = {{ {fiberLen} }};"
            $"size_t {v}_start[{rank}]; size_t {v}_count[{rank}];" ]
        @ [ for d in 0 .. rank - 2 -> $"{v}_count[{d}] = 1;" ]
        @ [ $"{v}_count[{rank - 1}] = {fiberLen};"
            $"{v}_start[{rank - 1}] = 0;" ]

    /// STREAMED fiber reads: the in-nest read -- sets the site coordinates and pulls one trailing-axis fiber into the destination buffer.
    let genStreamFiber (filePath: string) (varName: string) (cppVarName: string) (destBuf: string) (siteExprs: string list) (arrType: IRArrayType) : string list =
        let v = cppVarName
        let suffix =
            match arrType.ElemType with
            | IRTScalar ETFloat32 -> "float"
            | IRTScalar ETFloat64 -> "double"
            | IRTScalar ETInt32 -> "int"
            | IRTScalar ETInt64 -> "longlong"
            | _ -> "double"
        [ for d in 0 .. siteExprs.Length - 1 -> $"{v}_start[{d}] = (size_t)({siteExprs.[d]});" ]
        @ ncChecked v $"streaming a fiber of '{varName}' from '{filePath}'"
            $"nc_get_vara_{suffix}({v}_ncid, {v}_varid, {v}_start, {v}_count, {destBuf})"

    /// Extract dimension names from a module's index type definitions
    let dimNamesFromModule (modul: IRModule) : string list =
        modul.Types
        |> List.choose (function
            | IRTDIndexType (name, _) -> Some name
            | _ -> None)

    /// Generate required C++ includes for NetCDF, plus the teardown helper.
    ///
    /// EVERY LINE RETURNED HERE MUST BE UNIQUE. CodeGen.providerIncludes runs
    /// `List.distinct` over the concatenation of all providers' lines -- right
    /// for deduplicating headers, silently destructive for anything else. Two
    /// nested guards ending in a bare `#endif` cost the inner one, which
    /// surfaced as "unterminated #else" in every emitted provider program. So
    /// the `#endif`s carry distinguishing comments (good practice regardless)
    /// and the helper body is one line rather than one closed by a bare `}`,
    /// which would collide with any other provider that ever emits one.
    ///
    /// netcdf_meta.h comes along for its NC_VERSION_* macros, which are what
    /// gate the `nc_finalize()` call the main wrapper emits (see
    /// CodeGen.moduleUsesNetcdf). netcdf.h does NOT pull it in itself, and a
    /// netcdf too old to have it is also too old to have nc_finalize -- so
    /// guarding the include keeps that build compiling exactly as it did.
    let genIncludes () : string list =
        [ "#include <netcdf.h>"
          "#include <cstdlib>"
          "#if defined(__has_include)"
          "#  if __has_include(<netcdf_meta.h>)"
          "#    include <netcdf_meta.h>"
          "#  endif"
          "#endif  // __has_include"
          ""
          "// Shut the netcdf closure down in order, exactly once."
          "//"
          "// Some libnetcdf builds link a large closure -- MSYS2's pulls in libcurl"
          "// and the AWS C++ SDK, whose CRT runs an event-loop thread pool. Every"
          "// ordinary way out of a C++ program (returning from main, std::exit, even"
          "// _exit) ends at ExitProcess, which terminates those workers and THEN runs"
          "// DLL_PROCESS_DETACH across the closure; a thread killed while holding a"
          "// lock a detach handler then wants deadlocks the process, after it has"
          "// already printed every correct answer."
          "//"
          "// Registered with std::atexit rather than only called at the end of main,"
          "// because the error paths leave through std::exit(1) -- a failed nc_open,"
          "// or blade_rt::panic -- and those must terminate too. Idempotent, so the"
          "// explicit call the MPI path makes for ordering costs nothing."
          "#if defined(NC_VERSION_MAJOR) && (NC_VERSION_MAJOR > 4 || (NC_VERSION_MAJOR == 4 && NC_VERSION_MINOR >= 9))"
          "static bool __blade_nc_finalized = false;"
          "static void __blade_nc_finalize() { if (!__blade_nc_finalized) { __blade_nc_finalized = true; nc_finalize(); } }"
          "#else"
          "// netcdf older than 4.9 has no nc_finalize, and no such closure either."
          "static void __blade_nc_finalize() {}"
          "#endif  // netcdf >= 4.9" ]
