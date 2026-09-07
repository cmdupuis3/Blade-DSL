// Zarr provider tests. Fully hermetic: metadata/parse/codegen tests are
// pure, and the live tests GENERATE their fixture stores on the fly via
// ZarrProvider.ZarrWrite (pure .NET file writes — no external library, no
// committed binary fixture; contrast NetcdfTests' sample.nc + libnetcdf).
// Only the e2e compile+run blocks need g++, and they skip gracefully
// without it (Build.isSkipError), mirroring NetcdfTests' discipline.
module Blade.Tests.ZarrTests

open System
open System.IO
open Blade
open Blade.Ast
open Blade.Parser
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.TypeEnv
open Blade.Lowering
open Blade.CodeGen
open Blade.ZarrProvider
open Blade.Build
open Blade.Tests.TestHarness

let runZarrTests () =
    printHeader "Zarr Provider Tests"
    let mutable passed = 0
    let mutable failed = 0

    let check (name: string) (condition: bool) (detail: string) =
        if condition then
            printfn "  PASS: %s" name
            passed <- passed + 1
        else
            printfn "  FAIL: %s — %s" name detail
            failed <- failed + 1

    let isError (r: Result<'a, string>) (needle: string) =
        match r with
        | Error e -> e.Contains needle
        | Ok _ -> false

    // "Check the environment FIRST." Probe once, here, so every e2e section
    // below can treat a failing baseline as a genuine failure instead of an
    // ambiguous "maybe the toolchain is missing" skip.
    let zCaps = Blade.Build.capabilities.Value

    /// Verdict for a failing BASELINE/ORACLE build (the `.read` reference, the
    /// serial reference, ...). Policy: a genuine missing-toolchain condition is
    /// a SKIP; a lowering error, a compile failure, or a nonzero exit of the
    /// baseline is a FAILURE. Every one of these arms used to print SKIP
    /// unconditionally, which silently DELETED the differential assertions that
    /// depend on the baseline -- the block stayed green with nothing tested.
    let baselineFailed (what: string) (e: string) =
        if not zCaps.HasGpp then printfn "  SKIP %s: g++ not found (%s)" what e
        elif isSkipError e then printfn "  SKIP %s: %s" what e
        else check ($"{what}: baseline builds and runs") false e

    // Fixture stores live under tests/fixtures/zarr_stores/ (not the repo root).
    // The SAME relative string resolves at the compiler cwd (compile-time
    // metadata loads) and, mirrored under generated_cpp_tests, at the exe
    // cwd (runtime reads/writes) — ZarrWrite/create_directories make parents.
    let fixStore (name: string) = "tests/fixtures/zarr_stores/" + name

    // ---------------------------------------------------------------
    // 1. Dtype mapping (pure)
    // ---------------------------------------------------------------
    printfn "\n--- dtype mapping ---"
    check "v2 <f8 -> ETFloat64/8"
        (match zarrDtypeV2 "<f8" with Ok d -> d.Elem = ETFloat64 && d.ByteSize = 8 && d.IsFloat | _ -> false) ""
    check "v2 <f4 -> ETFloat32/4"
        (match zarrDtypeV2 "<f4" with Ok d -> d.Elem = ETFloat32 && d.ByteSize = 4 | _ -> false) ""
    check "v2 <i4 -> ETInt64/4 (integer collapse)"
        (match zarrDtypeV2 "<i4" with Ok d -> d.Elem = ETInt64 && d.ByteSize = 4 && not d.IsFloat | _ -> false) ""
    check "v2 |i1 -> ETInt64/1"
        (match zarrDtypeV2 "|i1" with Ok d -> d.Elem = ETInt64 && d.ByteSize = 1 | _ -> false) ""
    check "v2 <u2 -> ETInt64/2"
        (match zarrDtypeV2 "<u2" with Ok d -> d.Elem = ETInt64 && d.ByteSize = 2 | _ -> false) ""
    check "v2 >f8 rejected (big-endian)"
        (isError (zarrDtypeV2 ">f8") "big-endian") ""
    check "v2 |b1 rejected (bool)"
        (isError (zarrDtypeV2 "|b1") "unsupported dtype") ""
    check "v3 float32 -> ETFloat32"
        (match zarrDtypeV3 "float32" with Ok d -> d.Elem = ETFloat32 | _ -> false) ""
    check "v3 uint64 -> ETInt64"
        (match zarrDtypeV3 "uint64" with Ok d -> d.Elem = ETInt64 && d.ByteSize = 8 | _ -> false) ""
    check "v3 bool rejected"
        (isError (zarrDtypeV3 "bool") "unsupported data_type") ""

    // ---------------------------------------------------------------
    // 2. v2 .zarray parsing (pure JSON)
    // ---------------------------------------------------------------
    printfn "\n--- v2 metadata parse ---"
    let v2good = """{"zarr_format": 2, "shape": [5, 7], "chunks": [2, 3], "dtype": "<f8",
                     "compressor": null, "fill_value": -1.5, "order": "C", "filters": null}"""
    (match parseArrayMetaV2 "A" "/tmp/s/A" v2good (Some """{"_ARRAY_DIMENSIONS": ["x", "y"]}""") with
     | Ok m ->
         check "v2 parse: shape/chunks" (m.Shape = [5L; 7L] && m.Chunks = [2L; 3L]) (sprintf "%A" m)
         check "v2 parse: dtype f8" (m.Dtype.Code = "f8") m.Dtype.Code
         check "v2 parse: fill -1.5" (m.FillValue = FillFloat -1.5) (sprintf "%A" m.FillValue)
         check "v2 parse: dim names from _ARRAY_DIMENSIONS" (m.DimNames = Some ["x"; "y"]) (sprintf "%A" m.DimNames)
         check "v2 parse: '.' separator, no prefix" (m.ChunkKeySep = "." && m.ChunkKeyPrefix = "") ""
     | Error e -> check "v2 parse: good array parses" false e)
    check "v2 parse: blosc compressor rejected BY NAME"
        (isError (parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<f8","compressor":{"id":"blosc","cname":"lz4"},"fill_value":0,"order":"C","filters":null}""" None) "blosc") ""
    check "v2 parse: order F rejected"
        (isError (parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<f8","compressor":null,"fill_value":0,"order":"F","filters":null}""" None) "order 'F'") ""
    check "v2 parse: filters rejected"
        (isError (parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":[{"id":"delta"}]}""" None) "filters") ""
    check "v2 parse: null fill -> FillNone"
        (match parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<i8","compressor":null,"fill_value":null,"order":"C","filters":null}""" None with
         | Ok m -> m.FillValue = FillNone | _ -> false) ""
    check "v2 parse: \"NaN\" fill"
        (match parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<f8","compressor":null,"fill_value":"NaN","order":"C","filters":null}""" None with
         | Ok m -> (match m.FillValue with FillFloat f -> Double.IsNaN f | _ -> false) | _ -> false) ""
    check "v2 parse: '/' dimension_separator honored"
        (match parseArrayMetaV2 "A" "d" """{"shape":[4],"chunks":[2],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null,"dimension_separator":"/"}""" None with
         | Ok m -> m.ChunkKeySep = "/" | _ -> false) ""
    check "v2 parse: rank mismatch rejected"
        (isError (parseArrayMetaV2 "A" "d" """{"shape":[4,5],"chunks":[2],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}""" None) "rank") ""

    // ---------------------------------------------------------------
    // 3. v3 zarr.json parsing (pure JSON)
    // ---------------------------------------------------------------
    printfn "\n--- v3 metadata parse ---"
    let v3good = """{"zarr_format": 3, "node_type": "array", "shape": [6, 4], "data_type": "float32",
                     "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [3, 4]}},
                     "chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}},
                     "fill_value": 0, "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                     "dimension_names": ["t", "p"]}"""
    (match parseArrayMetaV3 "B" "/tmp/s/B" v3good with
     | Ok m ->
         check "v3 parse: shape/chunks" (m.Shape = [6L; 4L] && m.Chunks = [3L; 4L]) (sprintf "%A" m)
         check "v3 parse: dtype f4" (m.Dtype.Code = "f4") m.Dtype.Code
         check "v3 parse: dimension_names" (m.DimNames = Some ["t"; "p"]) (sprintf "%A" m.DimNames)
         check "v3 parse: 'c' prefix + '/' separator" (m.ChunkKeyPrefix = "c" && m.ChunkKeySep = "/") ""
     | Error e -> check "v3 parse: good array parses" false e)
    check "v3 parse: gzip codec rejected BY NAME"
        (isError (parseArrayMetaV3 "B" "d" """{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2]}},"fill_value":0,"codecs":[{"name":"bytes"},{"name":"gzip","configuration":{"level":5}}]}""") "gzip") ""
    check "v3 parse: big-endian bytes codec rejected"
        (isError (parseArrayMetaV3 "B" "d" """{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2]}},"fill_value":0,"codecs":[{"name":"bytes","configuration":{"endian":"big"}}]}""") "big-endian") ""
    check "v3 parse: v2 chunk_key_encoding (no prefix, '.' default)"
        (match parseArrayMetaV3 "B" "d" """{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2]}},"fill_value":0,"codecs":[{"name":"bytes"}],"chunk_key_encoding":{"name":"v2"}}""" with
         | Ok m -> m.ChunkKeyPrefix = "" && m.ChunkKeySep = "." | _ -> false) ""
    check "v3 parse: missing chunk_shape rejected"
        (isError (parseArrayMetaV3 "B" "d" """{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"float64","chunk_grid":{"name":"regular"},"fill_value":0}""") "chunk_shape") ""
    check "v3 parse: non-regular chunk_grid rejected"
        (isError (parseArrayMetaV3 "B" "d" """{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"float64","chunk_grid":{"name":"rectilinear"},"fill_value":0}""") "regular") ""

    // ---------------------------------------------------------------
    // 4. Chunk keys + grid math (pure)
    // ---------------------------------------------------------------
    printfn "\n--- chunk keys + grid math ---"
    let mkMeta prefix sep = {
        Name = "A"; ArrayDir = "d"; Shape = [5L; 7L]; Chunks = [2L; 3L]
        Dtype = { Code = "f8"; Elem = ETFloat64; ByteSize = 8; IsFloat = true }
        DimNames = None; FillValue = FillNone; Codec = CodecIdentity; Blade = None
        Version = 2; ChunkKeySep = sep; ChunkKeyPrefix = prefix }
    check "key v2 [0;1] -> 0.1" (chunkKey (mkMeta "" ".") [0L; 1L] = "0.1") (chunkKey (mkMeta "" ".") [0L; 1L])
    check "key v2 rank-0 -> 0" (chunkKey (mkMeta "" ".") [] = "0") ""
    check "key v3 [2;3] -> c/2/3" (chunkKey (mkMeta "c" "/") [2L; 3L] = "c/2/3") (chunkKey (mkMeta "c" "/") [2L; 3L])
    check "key v3 rank-0 -> c" (chunkKey (mkMeta "c" "/") [] = "c") ""
    check "key v2 '/'-separated -> 0/1" (chunkKey (mkMeta "" "/") [0L; 1L] = "0/1") ""
    check "gridDims [5;7]/[2;3] = [3;3]" (gridDims [5L; 7L] [2L; 3L] = [3L; 3L]) (sprintf "%A" (gridDims [5L; 7L] [2L; 3L]))
    check "gridCoords count = 9" ((gridCoords [5L; 7L] [2L; 3L]).Length = 9) ""
    check "rowMajorStrides [5;7] = [7;1]" (rowMajorStrides [5; 7] = [7; 1]) (sprintf "%A" (rowMajorStrides [5; 7]))
    check "rowMajorStrides [2;3;4] = [12;4;1]" (rowMajorStrides [2; 3; 4] = [12; 4; 1]) ""

    // ---------------------------------------------------------------
    // 5. Module construction from a mock store (pure; mirrors NetcdfTests 2-3b)
    // ---------------------------------------------------------------
    printfn "\n--- zarrStoreToModule (mock store) ---"
    let mockDt code elem bs isF = { Code = code; Elem = elem; ByteSize = bs; IsFloat = isF }
    let mockStore = {
        Path = "/mock/store"; Version = 2
        Arrays = [
            { Name = "A"; ArrayDir = "/mock/store/A"; Shape = [4L; 3L]; Chunks = [4L; 3L]
              Dtype = mockDt "f4" ETFloat32 4 true; DimNames = Some ["x"; "y"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
            { Name = "x"; ArrayDir = "/mock/store/x"; Shape = [4L]; Chunks = [4L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = Some ["x"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
        ]
    }
    let builder = IRBuilder()
    let modul = zarrStoreToModule builder "sample" mockStore None
    let indexDefs = modul.Types |> List.choose (function IRTDIndexType (n, it) -> Some (n, it) | _ -> None)
    check "mock module: named index types x, y"
        (indexDefs |> List.map fst |> List.sort = ["x"; "y"]) (sprintf "%A" (List.map fst indexDefs))
    let dimsFields =
        modul.Types |> List.tryPick (function IRTDStruct ("sample__dims", fs) -> Some fs | _ -> None) |> Option.defaultValue []
    let varsFields =
        modul.Types |> List.tryPick (function IRTDStruct ("sample__vars", fs) -> Some fs | _ -> None) |> Option.defaultValue []
    check "mock module: dims has x and y" (dimsFields |> List.map fst |> List.sort = ["x"; "y"]) (sprintf "%A" (List.map fst dimsFields))
    check "mock module: vars has A only (x is a coordinate array)"
        (varsFields |> List.map fst = ["A"]) (sprintf "%A" (List.map fst varsFields))
    let elemOfArrow (t: IRType) =
        match t with
        | ArrayElem at -> Some at.ElemType
        | _ -> None
    check "mock module: coordinate x keeps its ACTUAL f8 elem (Zarr divergence from NetCDF's Int64)"
        (dimsFields |> List.tryFind (fun (n, _) -> n = "x")
         |> Option.bind (snd >> elemOfArrow) = Some (IRTScalar ETFloat64))
        (sprintf "%A" (dimsFields |> List.tryFind (fun (n, _) -> n = "x") |> Option.bind (snd >> elemOfArrow)))
    check "mock module: unnamed-dim y coordinate defaults to Int64"
        (dimsFields |> List.tryFind (fun (n, _) -> n = "y")
         |> Option.bind (snd >> elemOfArrow) = Some (IRTScalar ETInt64))
        ""
    (let aIdxIds =
        varsFields |> List.tryPick (fun (n, t) ->
            if n <> "A" then None else
            match t with
            | ArrayElem at -> Some (at.IndexTypes |> List.map (_.Id))
            | _ -> None)
     let xId = indexDefs |> List.tryPick (fun (n, it) -> if n = "x" then Some it.Id else None)
     check "mock module: A's first index IS the shared x index type (same Id)"
         (match aIdxIds, xId with
          | Some (a0 :: _), Some x -> a0 = x
          | _ -> false)
         (sprintf "A ids %A, x id %A" aIdxIds xId))
    check "mock module: conflicting dim extents rejected"
        (try
            let bad = { mockStore with
                          Arrays = mockStore.Arrays @ [ { mockStore.Arrays.[0] with Name = "C"; Shape = [9L; 3L] } ] }
            zarrStoreToModule (IRBuilder()) "s" bad None |> ignore
            false
         with ex -> ex.Message.Contains "conflicting extents") ""

    // ---------------------------------------------------------------
    // 5b. TWO loads in one program (regression — silent miscompile)
    //
    // env.TypeDefs is a flat Map and registerTypeDef is a blind Map.add, while
    // field access re-resolves the struct NAME at every use site
    // (TypeCheck.structFieldTypesOf). So when both loads emitted structs under
    // the literal names "dims"/"vars", the second load overwrote the first and
    // every `first.vars.X` in the program silently type-checked against the
    // SECOND store's fields — wrong element type / wrong rank where the names
    // collided, spurious "field not found" where they didn't. No diagnostic.
    //
    // The fix is per-binding struct names ("<binding>__dims"/"<binding>__vars",
    // the convention CsvProvider established). Registering second AFTER first
    // is the ordering that used to clobber. Mirrors NetcdfTests 3b-2.
    // ---------------------------------------------------------------
    printfn "\n--- two loads in one program (no cross-store clobber) ---"
    let storeFirst = {
        Path = "/mock/first"; Version = 2
        Arrays = [
            // Shares the name "A" with the second store, but f4 / rank 2.
            { Name = "A"; ArrayDir = "/mock/first/A"; Shape = [4L; 3L]; Chunks = [4L; 3L]
              Dtype = mockDt "f4" ETFloat32 4 true; DimNames = Some ["x"; "y"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
            { Name = "only_first"; ArrayDir = "/mock/first/only_first"; Shape = [4L]; Chunks = [4L]
              Dtype = mockDt "f4" ETFloat32 4 true; DimNames = Some ["x"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
            // Coordinate array for x -> lands in first__dims.
            { Name = "x"; ArrayDir = "/mock/first/x"; Shape = [4L]; Chunks = [4L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = Some ["x"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
        ]
    }
    let storeSecond = {
        Path = "/mock/second"; Version = 2
        Arrays = [
            // Same name "A", deliberately different: f8 / rank 1.
            { Name = "A"; ArrayDir = "/mock/second/A"; Shape = [7L]; Chunks = [7L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = Some ["depth"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
            { Name = "only_second"; ArrayDir = "/mock/second/only_second"; Shape = [7L]; Chunks = [7L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = Some ["depth"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
            { Name = "depth"; ArrayDir = "/mock/second/depth"; Shape = [7L]; Chunks = [7L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = Some ["depth"]
              FillValue = FillInt 0L; Codec = CodecIdentity; Blade = None; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
        ]
    }
    let twoBuilder = IRBuilder()
    let modFirst = zarrStoreToModule twoBuilder "first" storeFirst None
    let modSecond = zarrStoreToModule twoBuilder "second" storeSecond None
    let structNamesOf (m: IRModule) =
        m.Types |> List.choose (function IRTDStruct (n, _) -> Some n | _ -> None) |> List.sort
    check "two loads: struct names are namespaced per binding"
        (structNamesOf modFirst = ["first__dims"; "first__vars"]
         && structNamesOf modSecond = ["second__dims"; "second__vars"])
        (sprintf "first %A second %A" (structNamesOf modFirst) (structNamesOf modSecond))

    let (envZ1, _) = registerProviderModule (emptyEnv ()) "first" modFirst
    let (envZ2, _) = registerProviderModule envZ1 "second" modSecond

    // Walk the same chain TypeCheck does: binding struct -> section field ->
    // IRTNamed <struct> -> member field. A live lookup at each hop, which is
    // exactly why a clobbered TypeDefs entry retyped earlier use sites.
    let zMemberOf (binding: string) (section: string) (field: string) : IRType option =
        let fieldsOf structName =
            match lookupTypeDef structName envZ2 with
            | Some (TDIStruct (_, _, fields, _)) -> Some fields
            | _ -> None
        fieldsOf binding
        |> Option.bind (List.tryPick (fun (n, t) -> if n = section then Some t else None))
        |> Option.bind (function IRTNamed sn -> fieldsOf sn | _ -> None)
        |> Option.bind (List.tryPick (fun (n, t) -> if n = field then Some t else None))

    // The core assertion: after the SECOND load registers, the FIRST store's
    // shared-name member still resolves to the FIRST store's type.
    check "two loads: first.vars.A keeps first's type (Float32, rank 2)"
        (match zMemberOf "first" "vars" "A" with
         | Some (ArrayElem a) -> a.ElemType = IRTScalar ETFloat32 && a.IndexTypes.Length = 2
         | _ -> false)
        (sprintf "got %A" (zMemberOf "first" "vars" "A"))
    check "two loads: second.vars.A keeps second's type (Float64, rank 1)"
        (match zMemberOf "second" "vars" "A" with
         | Some (ArrayElem a) -> a.ElemType = IRTScalar ETFloat64 && a.IndexTypes.Length = 1
         | _ -> false)
        (sprintf "got %A" (zMemberOf "second" "vars" "A"))
    check "two loads: first.vars.only_first resolves"
        ((zMemberOf "first" "vars" "only_first").IsSome) "clobbered vars struct would lose it"
    check "two loads: second.vars.only_second resolves"
        ((zMemberOf "second" "vars" "only_second").IsSome) ""
    check "two loads: first.vars has no member of the second store"
        ((zMemberOf "first" "vars" "only_second").IsNone) "second store's field leaked into first"
    check "two loads: first.dims.x resolves"
        ((zMemberOf "first" "dims" "x").IsSome) "clobbered dims struct would lose it"
    check "two loads: second.dims.depth resolves"
        ((zMemberOf "second" "dims" "depth").IsSome) ""
    check "two loads: first.dims has no member of the second store"
        ((zMemberOf "first" "dims" "depth").IsNone) "second store's dim leaked into first"
    // Bare "dims"/"vars" must not be registered at all — that was the clobber.
    check "two loads: bare 'vars'/'dims' are not registered globally"
        ((lookupTypeDef "vars" envZ2).IsNone && (lookupTypeDef "dims" envZ2).IsNone)
        "bare names would clobber across stores"

    // ---------------------------------------------------------------
    // 6. Live store roundtrips (generated fixtures; F#-only, no g++)
    // ---------------------------------------------------------------
    printfn "\n--- live store write -> load -> read (v2 and v3) ---"
    let scratch = Path.Combine(Path.GetTempPath(), "blade_zarr_tests_" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory scratch |> ignore
    let aData = [| for i in 0 .. 34 -> float i * 1.5 |]
    let xCoord = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
    let bData = [| for i in 0 .. 11 -> int32 (100 + i) |]
    let mkVars () : ZarrWrite.WriteVar list = [
        { Name = "A"; DimNames = Some ["x"; "y"]; Shape = [5L; 7L]; Chunks = [2L; 3L]
          FillValue = FillFloat -1.0; Data = ZarrWrite.WF64 aData; OmitChunks = []; Blade = None }
        { Name = "x"; DimNames = Some ["x"]; Shape = [5L]; Chunks = [5L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 xCoord; OmitChunks = []; Blade = None }
        // B: 3x4 int32, chunks 2x2, chunk (1,1) omitted -> fill 99 there.
        { Name = "B"; DimNames = Some ["r"; "c"]; Shape = [3L; 4L]; Chunks = [2L; 2L]
          FillValue = FillInt 99L; Data = ZarrWrite.WI32 bData; OmitChunks = [[1L; 1L]]; Blade = None }
    ]
    let expectB () =
        // Row-major 3x4; chunk (1,1) covers rows 2, cols 2..3 -> fill 99.
        [| for r in 0 .. 2 do
             for c in 0 .. 3 ->
               if r >= 2 && c >= 2 then 99L else int64 (100 + r * 4 + c) |]
    for (version, writer) in [ (2, ZarrWrite.writeStoreV2); (3, ZarrWrite.writeStoreV3) ] do
        let root = Path.Combine(scratch, $"store_v{version}")
        writer root (mkVars ())
        (try
            let store = load root
            check ($"v{version}: load discovers 3 arrays")
                (store.Version = version && (store.Arrays |> List.map (_.Name) |> List.sort) = ["A"; "B"; "x"])
                (sprintf "version %d arrays %A" store.Version (store.Arrays |> List.map (_.Name)))
            (match tryFindArray store "A" with
             | Some m ->
                 check ($"v{version}: A meta roundtrip (shape/chunks/dims/fill)")
                     (m.Shape = [5L; 7L] && m.Chunks = [2L; 3L] && m.DimNames = Some ["x"; "y"] && m.FillValue = FillFloat -1.0)
                     (sprintf "%A" m)
             | None -> check ($"v{version}: A found") false "")
            (match readVarData root "A" with
             | Ok { DimLengths = [5; 7]; Payload = ZFloats got } ->
                 check ($"v{version}: A values roundtrip through multi-chunk assembly (edge chunks)")
                     (got = aData) (sprintf "first few: %A vs %A" (Array.truncate 5 got) (Array.truncate 5 aData))
             | Ok d -> check ($"v{version}: A values roundtrip") false (sprintf "unexpected payload %A" d.DimLengths)
             | Error e -> check ($"v{version}: A values roundtrip") false e)
            (match readVarData root "B" with
             | Ok { Payload = ZInts got } ->
                 check ($"v{version}: B omitted chunk reads as fill (99), int32 widened to int64")
                     (got = expectB ()) (sprintf "got %A" got)
             | Ok _ -> check ($"v{version}: B fill/widening") false "not ints"
             | Error e -> check ($"v{version}: B fill/widening") false e)
         with ex -> check ($"v{version}: store loads") false ex.Message)
    // Missing chunk + null fill = loud error, not silent zeros.
    (let root = Path.Combine(scratch, "store_nullfill")
     ZarrWrite.writeStoreV2 root [
        { Name = "N"; DimNames = None; Shape = [4L]; Chunks = [2L]
          FillValue = FillNone; Data = ZarrWrite.WF64 [| 1.0; 2.0; 3.0; 4.0 |]; OmitChunks = [[1L]]; Blade = None } ]
     check "null fill + missing chunk -> loud refusal"
         (isError (readVarData root "N") "refusing to invent data") (sprintf "%A" (readVarData root "N")))

    // ---------------------------------------------------------------
    // 7. C++ generator string checks
    // ---------------------------------------------------------------
    printfn "\n--- CppZarr generators ---"
    let f64Idx (builder: IRBuilder) n =
        { Id = builder.FreshId(); Rank = 1; Extent = IRLit (IRLitInt n); Symmetry = SymNone
          Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    let wArrType =
        let b = IRBuilder()
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [f64Idx b 5L; f64Idx b 7L]
          IsVirtual = false; Identity = Some (AIDVariable "A") }
    let wCode = CppZarr.genWriteVar "out_store" "A" "A" wArrType ["x"; "y"] |> String.concat "\n"
    check "genWriteVar: creates the array directory" (wCode.Contains "create_directories" && wCode.Contains "out_store/A") wCode
    check "genWriteVar: writes v2 .zarray with compressor null"
        (wCode.Contains ".zarray" && wCode.Contains "zarr_format" && wCode.Contains "compressor") ""
    check "genWriteVar: records _ARRAY_DIMENSIONS" (wCode.Contains "_ARRAY_DIMENSIONS" && wCode.Contains "\\\"x\\\"") ""
    check "genWriteVar: single whole-array chunk 0.0" (wCode.Contains "/0.0") ""
    check "genWriteVar: binary chunk write from the flat buffer"
        (wCode.Contains "A_flat" && wCode.Contains "sizeof(double) * 35") ""
    check "genWriteVar: loud on stream failure" (wCode.Contains "Zarr error" && wCode.Contains "std::exit(1)") ""
    // genReadVar needs real store metadata: reuse the v2 fixture from section 6.
    (let root = Path.Combine(scratch, "store_v2")
     let rCode = CppZarr.genReadVar root "A" "A" wArrType |> String.concat "\n"
     check "genReadVar: metadata existence check, loud" (rCode.Contains ".zarray" && rCode.Contains "Zarr error") ""
     check "genReadVar: fstream chunk reads with computed keys"
         (rCode.Contains "std::ifstream" && rCode.Contains "std::to_string(A_c0)") ""
     check "genReadVar: fill_value branch for missing chunks" (rCode.Contains "A_fillv") ""
     check "genReadVar: short-read guard" (rCode.Contains "gcount()") ""
     check "genReadVar: materializes nested Array (allocate + promote)"
         (rCode.Contains "allocate<typename promote<double, 2>::type") ""
     check "genReadVar: releases buffers" (rCode.Contains "delete[] A_flat" && rCode.Contains "delete[] A_cbuf") "")

    // ---------------------------------------------------------------
    // 8. Registry + surface gates
    // ---------------------------------------------------------------
    printfn "\n--- registry + module-surface gates ---"
    Blade.ProviderStatics.install ()
    check "registry: zarr registered"
        (match Blade.ProviderRegistry.tryFind "zarr" with Some s -> s.Name = "zarr" | None -> false) ""
    check "registry: csv + icechunk + netcdf registered too"
        ((Blade.ProviderRegistry.names ()) |> List.sort = ["csv"; "icechunk"; "netcdf"; "zarr"]) (sprintf "%A" (Blade.ProviderRegistry.names ()))
    check "registry: zarr rejects load_compound (no compound reader)"
        (match Blade.ProviderRegistry.tryFind "zarr" with Some s -> s.GenReadCompoundVar.IsNone | None -> false) ""
    check "registry: zarr needs no link flags"
        (match Blade.ProviderRegistry.tryFind "zarr" with Some s -> s.LinkNeeds.Contains "none" | None -> false) ""
    let typeErrOf (src: string) : string =
        match Parser.parseProgram src with
        | Error e -> e.Message
        | Ok program ->
            match TypeCheck.typeCheck program with
            | Error errs -> errs |> List.map TypeEnv.formatCompileError |> String.concat "; "
            | Ok _ -> ""
    check "old spelling `import Providers.NetCDF` -> steering error"
        ((typeErrOf "import Providers.NetCDF as NetCDF\nlet x = 1\n").Contains "import netcdf as")
        (typeErrOf "import Providers.NetCDF as NetCDF\nlet x = 1\n")
    check "`from zarr import load` -> selective import rejected"
        ((typeErrOf "from zarr import load\nlet x = 1\n").Contains "selective import")
        (typeErrOf "from zarr import load\nlet x = 1\n")
    check "bare `|> read` is a hard break (unbound identifier)"
        ((typeErrOf "let x = 5 |> read\n").Contains "read")
        (typeErrOf "let x = 5 |> read\n")

    // ---------------------------------------------------------------
    // 9. Static fold e2e (hermetic — needs no g++, no external library)
    // ---------------------------------------------------------------
    printfn "\n--- provider statics: zarr fold + ceiling ---"
    (let foldRoot = fixStore "zarr_fold_store"
     (try Directory.Delete(foldRoot, true) with _ -> ())
     ZarrWrite.writeStoreV2 foldRoot [
        { Name = "x"; DimNames = Some ["x"]; Shape = [6L]; Chunks = [3L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]; OmitChunks = []; Blade = None }
        { Name = "A"; DimNames = Some ["x"]; Shape = [6L]; Chunks = [6L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in 1 .. 6 -> float (i * i) |]; OmitChunks = []; Blade = None } ]
     let foldSource = """
import zarr as z

let sample = z.load("tests/fixtures/zarr_stores/zarr_fold_store")
let static xd = sample.dims.x |> z.read
let static n = length(xd)
let static ps = prodsum(xd, xd)
let a = n
let b = ps
"""
     (try
         match Parser.parseProgram foldSource with
         | Error e -> check "zarr fold: parses" false e.Message
         | Ok program ->
             match TypeCheck.typeCheck program with
             | Error errs ->
                 check "zarr fold: typechecks (fold succeeded)" false
                     (errs |> List.map TypeEnv.formatCompileError |> String.concat "; ")
             | Ok _ ->
                 check "zarr fold: typechecks (fold succeeded)" true ""
                 match Blade.StaticEval.resolveStatics program.Modules.Head.Decls with
                 | Ok (se, _) ->
                     check "zarr fold: length(xd) = 6"
                         (Map.tryFind "n" se.Values = Some (Blade.StaticEval.SVInt 6L))
                         (sprintf "got %A" (Map.tryFind "n" se.Values))
                     check "zarr fold: prodsum(xd, xd) = 91"
                         (Map.tryFind "ps" se.Values = Some (Blade.StaticEval.SVFloat 91.0))
                         (sprintf "got %A" (Map.tryFind "ps" se.Values))
                 | Error e -> check "zarr fold: resolveStatics" false e
      with ex -> check "zarr fold: runs" false ex.Message)
     // Fold ceiling: 70000 > 65536 elements refuses with steering.
     let bigRoot = fixStore "zarr_fold_big"
     (try Directory.Delete(bigRoot, true) with _ -> ())
     ZarrWrite.writeStoreV2 bigRoot [
        { Name = "big"; DimNames = Some ["n"]; Shape = [70000L]; Chunks = [70000L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 (Array.zeroCreate 70000); OmitChunks = []; Blade = None } ]
     let bigSource = """
import zarr as z

let sample = z.load("tests/fixtures/zarr_stores/zarr_fold_big")
let static v = sample.vars.big |> z.read
"""
     (try
         match Parser.parseProgram bigSource with
         | Error e -> check "zarr fold ceiling: parses" false e.Message
         | Ok program ->
             match TypeCheck.typeCheck program with
             | Error errs ->
                 let msg = errs |> List.map TypeEnv.formatCompileError |> String.concat "; "
                 check "zarr fold ceiling: 70000 elements refused with steering"
                     (msg.Contains "fold ceiling") msg
             | Ok _ -> check "zarr fold ceiling: 70000 elements refused with steering" false "typechecked (fold went through?)"
      with ex -> check "zarr fold ceiling: runs" false ex.Message))

    // ---------------------------------------------------------------
    // 9b. Provider axes reach the module elaborations (hermetic)
    // ---------------------------------------------------------------
    // spectra/math/sgs/ppl specialize their generated code to STATIC extents,
    // and they run BEFORE the type check that registers `<store>.index.<dim>`
    // — so they resolve the axis against the store's metadata themselves. The
    // assertion is code IDENTITY: `type X = sample.index.x` must generate
    // exactly what the hand-copied `type X = Idx<6>` generates. Reuses the
    // section-9 fold fixture (dim x, extent 6).
    printfn "\n--- provider axes: spectra transform over sample.index.x ---"
    (let axisSource (axis: string) = sprintf """
import spectra as sp
import zarr as z

let sample = z.load("tests/fixtures/zarr_stores/zarr_fold_store")
type X = %s
type Sig = Array<Float64 like X>
let A: Sig = sample.vars.A |> z.read
let S = sp.fft(A)
"""
                                        axis
     (try
         match lower (axisSource "sample.index.x"), lower (axisSource "Idx<6>") with
         | Ok irProv, Ok irLit ->
             check "provider axis: sp.fft over sample.index.x elaborates" true ""
             let (cppProv, _) = CodeGen.genSelfContainedProgramFromIR irProv "zarr_axis_probe"
             let (cppLit, _) = CodeGen.genSelfContainedProgramFromIR irLit "zarr_axis_probe"
             check "provider axis: generates exactly what Idx<6> generates"
                 (cppProv = cppLit) "generated program differs from the literal-extent twin"
         | Error e, _ ->
             check "provider axis: sp.fft over sample.index.x elaborates" false e
         | _, Error e ->
             check "provider axis: Idx<6> twin lowers" false e
      with ex -> check "provider axis: runs" false ex.Message))

    // ---------------------------------------------------------------
    // 10. Runtime dense read e2e (g++; store generated on the fly)
    // ---------------------------------------------------------------
    // Multi-chunk WITH edge chunks and one missing chunk (fill -1), so the
    // runtime path exercises key formatting, intersection copy, and fill.
    printfn "\n--- dense read e2e: method_for(z.read(s.vars.A)) <@> (x -> x+x) ---"
    let e2eDir = "./generated_cpp_tests"
    if not (Directory.Exists e2eDir) then Directory.CreateDirectory e2eDir |> ignore
    for (version, writer) in [ (2, ZarrWrite.writeStoreV2); (3, ZarrWrite.writeStoreV3) ] do
        let storeName = fixStore ($"zarr_e2e_v{version}")
        let storeInDir = Path.Combine(e2eDir, storeName)
        let e2eVars : ZarrWrite.WriteVar list = [
            { Name = "A"; DimNames = Some ["x"; "y"]; Shape = [5L; 7L]; Chunks = [2L; 3L]
              FillValue = FillFloat -1.0; Data = ZarrWrite.WF64 aData; OmitChunks = [[2L; 2L]]; Blade = None } ]
        // Twice: at the compiler's cwd (compile-time metadata resolves the
        // relative path here) and beside the exe (runtime reads resolve
        // against the executable's working directory) — the same split as
        // NetcdfTests' sample.nc copy.
        (try Directory.Delete(storeName, true) with _ -> ())
        (try Directory.Delete(storeInDir, true) with _ -> ())
        writer storeName e2eVars
        writer storeInDir e2eVars
        let readSource = sprintf """
import zarr as z

let sample = z.load("%s")
let A = sample.vars.A |> z.read
let out = method_for(A) <@> lambda(x) -> x + x |> compute
"""
                             storeName
        try
            match lower readSource with
            | Ok ir ->
                check ($"e2e v{version}: ProviderReads spec (provider=zarr, maskless)")
                    (ir.Modules.[0].ProviderReads |> Map.exists (fun _ s -> s.Provider = "zarr" && s.VarName = "A" && s.MaskName = None))
                    ""
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_read_e2e_v{version}")
                check ($"e2e v{version}: emits fstream reads, no netcdf dependency")
                    (cppCode.Contains "std::ifstream" && not (cppCode.Contains "netcdf.h")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_read_e2e_v{version}.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     check ($"e2e v{version}: compiles (pure std C++ — no link flags)") true ""
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          check ($"e2e v{version}: runs (exit 0)") true ""
                          // Ground truth via the F# read path (fill -1 in the
                          // omitted chunk region), kernel doubles it.
                          (match readVarData storeInDir "A" with
                           | Ok { Payload = ZFloats truth } ->
                               let expected = truth |> Array.map (fun x -> x + x)
                               // Shape-tolerant flatten (TestHarness): `out`
                               // is rank 2 and prints NESTED since 7ac4d3a.
                               (match tryParsePrintedFloats "out" runOut with
                                | None -> check ($"e2e v{version}: values match ground truth") false "no parseable out = [...] line"
                                | Some parsed ->
                                    let ok =
                                        parsed.Length = expected.Length
                                        && Array.forall2 (fun a b -> abs (a - b) <= 1e-9 * max 1.0 (abs b)) parsed expected
                                    check ($"e2e v{version}: values match ground truth (2*A incl. fill region)")
                                        ok
                                        ($"{parsed.Length} vs {expected.Length} values"))
                           | Ok _ -> check ($"e2e v{version}: values match ground truth") false "truth not floats"
                           | Error e -> check ($"e2e v{version}: values match ground truth") false e)
                          // Missing store at runtime fails loudly (metadata check).
                          if version = 2 then
                              let missingDir = Path.Combine(Path.GetTempPath(), "blade_zarr_missing_" + Guid.NewGuid().ToString("N"))
                              Directory.CreateDirectory missingDir |> ignore
                              (try
                                  let exeCopy = Path.Combine(missingDir, Path.GetFileName exePath)
                                  File.Copy(exePath, exeCopy, true)
                                  (match runExecutable exeCopy with
                                   | Ok (code, missOut) ->
                                       check "e2e: missing store at runtime fails loudly (nonzero + Zarr error)"
                                           (code <> 0 && missOut.Contains "Zarr error")
                                           ($"exit {code}: {(missOut.Substring(0, min 200 missOut.Length))}")
                                   | Error e -> check "e2e: missing store fails loudly" false e)
                               finally
                                  try Directory.Delete(missingDir, true) with _ -> ())
                      | Ok (code, runOut) -> check ($"e2e v{version}: runs (exit 0)") false ($"exit {code}: {runOut}")
                      | Error e -> check ($"e2e v{version}: runs (exit 0)") false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP zarr read e2e v%d (compile skipped): %s" version e
                     else check ($"e2e v{version}: compiles") false e)
            | Error e -> check ($"e2e v{version}: lowers") false e
        with ex -> check ($"e2e v{version}") false ex.Message

    // ---------------------------------------------------------------
    // 10c. Provider-inherited segmentation (docs/plans/structural/07 §3.1):
    // `type CX = Chunked<sample.index.x, store>` takes the store's own chunk
    // edge on x (recorded at the load site by ProviderRegistry.DimChunks),
    // and the structural grouping / ungroup pipeline runs over it. A store
    // whose variables chunk x differently refuses at the declaration.
    // ---------------------------------------------------------------
    printfn "
--- inherited segmentation: Chunked<sample.index.x, store> ---"
    let inheritedSegmentationE2E () =
        let segStore = fixStore "zarr_segments"
        let segInDir = Path.Combine(e2eDir, segStore)
        let segVars : ZarrWrite.WriteVar list = [
            { Name = "A"; DimNames = Some ["x"]; Shape = [10L]; Chunks = [4L]
              FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in 1 .. 10 -> float i |]; OmitChunks = []; Blade = None } ]
        (try Directory.Delete(segStore, true) with _ -> ())
        (try Directory.Delete(segInDir, true) with _ -> ())
        ZarrWrite.writeStoreV3 segStore segVars
        ZarrWrite.writeStoreV3 segInDir segVars
        let segSource = sprintf """
import zarr as z

let sample = z.load("%s")
type CX = Chunked<sample.index.x, store>
let A = sample.vars.A |> z.read
let seg = segments(CX)
let sizes = extents(seg)
let g = group_by(A, seg)
let sums = method_for(g) <@> lambda(r) -> reduce(r, (+)) |> compute
let back = ungroup(g)
"""
                            segStore
        try
            match lower segSource with
            | Ok ir ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_segments_inherited"
                check "inherited segmentation: emits the structural grouping (no __perm)"
                    (cppCode.Contains "structural" && not (cppCode.Contains "seg__perm")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, "zarr_segments_inherited.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          let has (line: string) = runOut.Contains line
                          check "inherited segmentation: sizes = [4, 4, 2]" (has "sizes = [4, 4, 2]") runOut
                          check "inherited segmentation: per-segment sums" (has "sums = [10, 26, 19]") runOut
                          check "inherited segmentation: ungroup restores the axis" (has "back = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]") runOut
                      | Ok (code, out) -> check "inherited segmentation: runs" false ($"exit {code}: {out}")
                      | Error e -> check "inherited segmentation: runs" false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP inherited segmentation (compile skipped): %s" e
                     else check "inherited segmentation: compiles" false e)
            | Error e -> check "inherited segmentation: lowers" false e
        with ex -> check "inherited segmentation" false ex.Message
        // the refusal: two variables chunk x differently
        let mixStore = fixStore "zarr_segments_mixed"
        (try Directory.Delete(mixStore, true) with _ -> ())
        ZarrWrite.writeStoreV3 mixStore [
            { Name = "A"; DimNames = Some ["x"]; Shape = [10L]; Chunks = [4L]
              FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in 1 .. 10 -> float i |]; OmitChunks = []; Blade = None }
            { Name = "B"; DimNames = Some ["x"]; Shape = [10L]; Chunks = [5L]
              FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in 1 .. 10 -> float i |]; OmitChunks = []; Blade = None } ]
        let mixSource = sprintf """
import zarr as z

let sample = z.load("%s")
type CX = Chunked<sample.index.x, store>
"""
                            mixStore
        (match lower mixSource with
         | Error e -> check "inherited segmentation: non-uniform chunking refuses with a steer" (e.Contains "does not chunk 'x' uniformly") e
         | Ok _ -> check "inherited segmentation: non-uniform chunking refuses with a steer" false "lowered")

    inheritedSegmentationE2E ()

    // ---------------------------------------------------------------
    // 10d. The FILE level (docs/plans/structural/07 §2.7, decisions D1/D3/D5):
    // `type T = Chunked<s1.index.x, [[s1, store], [s2, store]]>` tiles x over
    // two stores in order, each keeping its own chunk grid (a dependent chunk
    // level: 3-cell chunks in the first file, 2-cell in the second). `files(T)`
    // is the file grouping (labelled by the store names), `segments(T)` the
    // innermost chunk grouping, and `ungroup([a1, a2], T)` names a variable
    // that lives in both stores over the tiled axis. Provider-agnostic: only
    // the stores' extents and edges are consulted, never a coordinate value.
    // ---------------------------------------------------------------
    printfn "
--- file level: Chunked<s1.index.x, [[s1, store], [s2, store]]> ---"
    let fileLevelE2E () =
        let st1 = fixStore "zarr_tile_a"
        let st2 = fixStore "zarr_tile_b"
        let mk (name: string) (n: int) (lo: int) (edge: int64) : ZarrWrite.WriteVar list =
            [ { Name = "A"; DimNames = Some ["x"]; Shape = [int64 n]; Chunks = [edge]
                FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in lo .. lo + n - 1 -> float i |]; OmitChunks = []; Blade = None } ]
        for (store, vars) in [ (st1, mk "A" 6 1 3L); (st2, mk "A" 4 7 2L) ] do
            let inDir = Path.Combine(e2eDir, store)
            (try Directory.Delete(store, true) with _ -> ())
            (try Directory.Delete(inDir, true) with _ -> ())
            ZarrWrite.writeStoreV3 store vars
            ZarrWrite.writeStoreV3 inDir vars
        let src = sprintf """
import zarr as z

let s1 = z.load("%s")
let s2 = z.load("%s")
type T = Chunked<s1.index.x, [[s1, store], [s2, store]]>
let a1 = s1.vars.A |> z.read
let a2 = s2.vars.A |> z.read
let a = ungroup([a1, a2], T)
let fs = files(T)
let fsizes = extents(fs)
let seg = segments(T)
let ssizes = extents(seg)
let gf = group_by(a, fs)
let fsums = method_for(gf) <@> lambda(r) -> reduce(r, (+)) |> compute
let gs = group_by(a, seg)
let ssums = method_for(gs) <@> lambda(r) -> reduce(r, (+)) |> compute
let back = ungroup(gs)
"""
                            st1 st2
        try
            match lower src with
            | Ok ir ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_segments_files"
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, "zarr_segments_files.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          let has (line: string) = runOut.Contains line
                          check "file level: the tiled variable" (has "a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]") runOut
                          check "file level: files(T) sizes = [6, 4]" (has "fsizes = [6, 4]") runOut
                          check "file level: segments(T) sizes = [3, 3, 2, 2] (per-file chunk grids)" (has "ssizes = [3, 3, 2, 2]") runOut
                          check "file level: per-file sums" (has "fsums = [21, 34]") runOut
                          check "file level: per-chunk sums" (has "ssums = [6, 15, 15, 19]") runOut
                          check "file level: ungroup of the chunk grouping restores the axis" (has "back = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]") runOut
                      | Ok (code, out) -> check "file level: runs" false ($"exit {code}: {out}")
                      | Error e -> check "file level: runs" false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP file level (compile skipped): %s" e
                     else check "file level: compiles" false e)
            | Error e -> check "file level: lowers" false e
        with ex -> check "file level" false ex.Message
        // refusals: a wrong row count, and a store without the dimension
        let bad1 = sprintf """
import zarr as z

let s1 = z.load("%s")
let s2 = z.load("%s")
type T = Chunked<s1.index.x, [[s1, store], [s2, store]]>
let a1 = s1.vars.A |> z.read
let a = ungroup([a1], T)
"""
                             st1 st2
        (match lower bad1 with
         | Error e -> check "file level: wrong row count refuses" (e.Contains "tiles 2 store(s) but 1 row(s)") e
         | Ok _ -> check "file level: wrong row count refuses" false "lowered")
        let bad2 = sprintf """
import zarr as z

let s1 = z.load("%s")
let s2 = z.load("%s")
type T = Chunked<s1.index.x, [[s1, store], [s2, 3]]>
let a1 = s1.vars.A |> z.read
let a2 = s2.vars.A |> z.read
let a = ungroup([a2, a1], T)
"""
                             st1 st2
        (match lower bad2 with
         | Error e -> check "file level: a row of the wrong extent refuses" (e.Contains "has extent 4 but store 0") e
         | Ok _ -> check "file level: a row of the wrong extent refuses" false "lowered")
    fileLevelE2E ()

    // ---------------------------------------------------------------
    // 10e. PER-SEGMENT streamed reads (docs/plans/structural/07 §3.4): a rank-1
    // variable bound with `.stream` and grouped by a structural grouping is
    // read one run at a time, straight into each group's row -- the emitted
    // C++ has no whole-array buffer for it. A KEY grouping over a streamed
    // variable keeps refusing with a steer to `.read`.
    // ---------------------------------------------------------------
    printfn "
--- per-segment stream: group_by(s.vars.A |> z.stream, segments(CX)) ---"
    let streamedSegmentsE2E () =
        let segStore = fixStore "zarr_segments"    // written by 10c (10 cells, chunked at 4)
        let src = sprintf """
import zarr as z

let sample = z.load("%s")
type CX = Chunked<sample.index.x, store>
let A = sample.vars.A |> z.stream
let seg = segments(CX)
let g = group_by(A, seg)
let sums = method_for(g) <@> lambda(r) -> reduce(r, (+)) |> compute
let counts = method_for(g) <@> lambda(r) -> extents(r) |> compute
"""
                            segStore
        try
            match lower src with
            | Ok ir ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_segments_streamed"
                check "per-segment stream: rows are read per run (marker present)"
                    (cppCode.Contains "per-segment STREAMED rows") ""
                check "per-segment stream: no whole-array buffer for A"
                    (not (cppCode.Contains "A_flat = new") && not (cppCode.Contains "Array<double, 1> A = ")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, "zarr_segments_streamed.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          let has (line: string) = runOut.Contains line
                          check "per-segment stream: per-run sums" (has "sums = [10, 26, 19]") runOut
                          check "per-segment stream: per-run counts" (has "counts = [4, 4, 2]") runOut
                      | Ok (code, out) -> check "per-segment stream: runs" false ($"exit {code}: {out}")
                      | Error e -> check "per-segment stream: runs" false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP per-segment stream (compile skipped): %s" e
                     else check "per-segment stream: compiles" false e)
            | Error e -> check "per-segment stream: lowers" false e
        with ex -> check "per-segment stream" false ex.Message
        // a key grouping over a streamed variable still refuses
        let bad = sprintf """
import zarr as z

let sample = z.load("%s")
let A = sample.vars.A |> z.stream
let keys = [0, 0, 1, 1, 2, 2, 0, 1, 2, 0]
let gk = group_keys(keys)
let g = group_by(A, gk)
let sums = method_for(g) <@> lambda(r) -> reduce(r, (+)) |> compute
"""
                            segStore
        (match lower bad with
         | Ok ir ->
             (try
                 let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_segments_streamed_bad"
                 let refusals = CodeGen.takeCodegenRefusalDiagnostics cppCode
                 check "per-segment stream: a key grouping over a streamed variable refuses with a steer"
                     (refusals |> List.exists (fun r -> (string r).Contains "needs a structural grouping") || cppCode.Contains "needs a structural grouping") cppCode
              with ex -> check "per-segment stream: a key grouping over a streamed variable refuses with a steer" (ex.Message.Contains "needs a structural grouping") ex.Message)
         | Error e -> check "per-segment stream: key grouping program lowers" false e)
    streamedSegmentsE2E ()

    // ---------------------------------------------------------------
    // 10f. STREAMED FOLD (decision D6 of docs/plans/structural/07): `reduce`
    // over a rank-1 `.stream` variable is the plain flat fold at the API and
    // walks the store one block at a time in storage order -- the same
    // operation sequence as the flat fold over a materialized copy, so the
    // answer is bitwise identical and no reorder licence is involved. Both
    // a builtin operator and a lambda kernel; and `segments(A)` off the
    // annotation's `Chunked` slot. The decision is recorded for `blade plan`.
    // ---------------------------------------------------------------
    printfn "
--- streamed fold: reduce(s.vars.A |> z.stream, (+)) ---"
    let streamedFoldE2E () =
        let segStore = fixStore "zarr_segments"    // 10 cells 1..10, chunked at 4
        let src = sprintf """
import zarr as z

let sample = z.load("%s")
type CX = Chunked<sample.index.x, store>
let A: Array<Float like CX> = sample.vars.A |> z.stream
let total = reduce(A, (+))
let biggest = reduce(A, lambda(x, y) -> if x > y then x else y)
let shifted = reduce(A, (+), 100.0)
let seg = segments(A)
let sizes = extents(seg)
"""
                            segStore
        try
            Blade.Effects.Decisions.start ()
            match lower src with
            | Ok ir ->
                let decisions = Blade.Effects.Decisions.drain ()
                check "streamed fold: `blade plan` records segment-streaming for the folds"
                    (decisions |> List.filter (fun d -> d.Rule = "segment-streaming" && d.Outcome = Blade.Effects.Applied) |> List.length >= 3)
                    (sprintf "%A" (decisions |> List.map (fun d -> d.Rule + ":" + d.Subject)))
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_segments_fold"
                check "streamed fold: block-wise emission, no whole-array buffer"
                    (cppCode.Contains "STREAMED fold" && not (cppCode.Contains "A_flat = new") && not (cppCode.Contains "Array<double, 1> A = ")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, "zarr_segments_fold.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          let has (line: string) = runOut.Contains line
                          check "streamed fold: builtin (+) = 55" (has "total = 55") runOut
                          check "streamed fold: lambda max = 10" (has "biggest = 10") runOut
                          check "streamed fold: seeded (+) = 155" (has "shifted = 155") runOut
                          check "streamed fold: segments(A) off the annotation" (has "sizes = [4, 4, 2]") runOut
                      | Ok (code, out) -> check "streamed fold: runs" false ($"exit {code}: {out}")
                      | Error e -> check "streamed fold: runs" false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP streamed fold (compile skipped): %s" e
                     else check "streamed fold: compiles" false e)
            | Error e ->
                Blade.Effects.Decisions.drain () |> ignore
                check "streamed fold: lowers" false e
        with ex -> check "streamed fold" false ex.Message
    streamedFoldE2E ()

    // ---------------------------------------------------------------
    // 10b. Dimension names that collide with C-library globals.
    //
    // Every store dimension derives a named index type, and codegen emits one
    // `using <name> = int64_t;` per index type into the GLOBAL namespace. A
    // store with a dimension named `time` -- the geoscience default, so the
    // common case rather than an exotic one -- therefore emitted
    // `using time = int64_t;` against <ctime>'s `time`, and g++ refused the
    // whole translation unit with "redeclared as different kind of entity".
    // `j0` covers the same hazard from <cmath>'s Bessel functions.
    //
    // The mangling is an EMISSION concern (CodeGenState.indexTypeCppName), so
    // the fixture asserts both halves: the Blade-visible names are untouched
    // (`sample.dims.time` still resolves in source), and the emitted C++ never
    // carries the bare alias.
    // ---------------------------------------------------------------
    printfn "\n--- reserved-identifier dim names: `time` / `j0` ---"
    (let rsvStore = fixStore "zarr_reserved_dims"
     let rsvInDir = Path.Combine(e2eDir, rsvStore)
     let rsvVars : ZarrWrite.WriteVar list = [
        // Coordinate array for the `time` dimension (1-D and same-named, the
        // shape xarray writes), so `sample.dims.time` is readable too.
        { Name = "time"; DimNames = Some ["time"]; Shape = [3L]; Chunks = [3L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| 10.0; 20.0; 30.0 |]
          OmitChunks = []; Blade = None }
        { Name = "T"; DimNames = Some ["time"; "j0"]; Shape = [3L; 2L]; Chunks = [3L; 2L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| 1.0 .. 6.0 |]
          OmitChunks = []; Blade = None } ]
     (try Directory.Delete(rsvStore, true) with _ -> ())
     (try Directory.Delete(rsvInDir, true) with _ -> ())
     ZarrWrite.writeStoreV3 rsvStore rsvVars
     ZarrWrite.writeStoreV3 rsvInDir rsvVars
     let rsvSource = sprintf """
import zarr as z

let sample = z.load("%s")
let T = sample.vars.T |> z.read
let ts = sample.dims.time |> z.read
let total = reduce(T, (+), axes = 2)
let tsum = reduce(ts, (+))
"""
                         rsvStore
     /// `name = <number>` on its own line, InvariantCulture.
     let printedScalar (name: string) (out: string) : float option =
        out.Split('\n')
        |> Array.tryPick (fun l ->
            let l = l.Trim()
            let prefix = name + " = "
            if l.StartsWith prefix then
                match Double.TryParse(l.Substring prefix.Length,
                                      Globalization.NumberStyles.Float,
                                      Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some v
                | _ -> None
            else None)
     try
        match lower rsvSource with
        | Ok ir ->
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_reserved_dims"
            check "reserved dims: no bare `using time =` / `using j0 =` emitted"
                (not (cppCode.Contains "using time = ") && not (cppCode.Contains "using j0 = "))
                "bare alias still emitted -- g++ would refuse the TU"
            check "reserved dims: mangled aliases emitted instead"
                (cppCode.Contains "using time_ = int64_t;" && cppCode.Contains "using j0_ = int64_t;")
                "expected time_ / j0_ aliases"
            CodeGen.deployRuntimeHeaders e2eDir
            let rsvCpp = Path.Combine(e2eDir, "zarr_reserved_dims.cpp")
            File.WriteAllText(rsvCpp, cppCode)
            (match compileCpp rsvCpp e2eDir with
             | Ok exePath ->
                 // The gate this section exists for: before the mangling, g++
                 // died here on the `time` alias alone.
                 check "reserved dims: compiles (the alias no longer redeclares a C-library global)" true ""
                 (match runExecutable exePath with
                  | Ok (0, runOut) ->
                      check "reserved dims: total = sum(T) = 21"
                          (match printedScalar "total" runOut with
                           | Some v -> abs (v - 21.0) <= 1e-9
                           | None -> false)
                          runOut
                      check "reserved dims: tsum = sum(time coord) = 60"
                          (match printedScalar "tsum" runOut with
                           | Some v -> abs (v - 60.0) <= 1e-9
                           | None -> false)
                          runOut
                  | Ok (code, runOut) -> check "reserved dims: runs (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "reserved dims: runs (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP zarr reserved-dim e2e (compile skipped): %s" e
                 else check "reserved dims: compiles" false e)
        | Error e ->
            // Names the Blade-side half of the invariant: mangling happens at
            // EMISSION, so `sample.dims.time` must still resolve in source.
            check "reserved dims: lowers (`time` is still the Blade-visible dim name)" false e
     with ex -> check "reserved dims e2e" false ex.Message)

    // ---------------------------------------------------------------
    // 11. Write -> read roundtrip e2e (the Blade-side writer)
    // ---------------------------------------------------------------
    printfn "\n--- write e2e: z.read |> z.write -> F# reads it back ---"
    (let inStore = fixStore "zarr_wrt_in"
     let outStore = fixStore "zarr_wrt_out"
     let inDirFull = Path.Combine(e2eDir, inStore)
     let outDirFull = Path.Combine(e2eDir, outStore)
     let wrtVars : ZarrWrite.WriteVar list = [
        { Name = "A"; DimNames = Some ["x"; "y"]; Shape = [4L; 3L]; Chunks = [2L; 2L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| for i in 0 .. 11 -> float i + 0.25 |]; OmitChunks = []; Blade = None } ]
     (try Directory.Delete(inStore, true) with _ -> ())
     (try Directory.Delete(inDirFull, true) with _ -> ())
     (try Directory.Delete(outDirFull, true) with _ -> ())
     ZarrWrite.writeStoreV3 inStore wrtVars       // compile-time metadata (compiler cwd)
     ZarrWrite.writeStoreV3 inDirFull wrtVars     // runtime read (exe cwd)
     let writeSource = sprintf """
import zarr as z

let sample = z.load("%s")
let A = sample.vars.A |> z.read
let w = z.write("%s", A)
"""
                           inStore outStore
     try
        match lower writeSource with
        | Ok ir ->
            check "write e2e: ProviderWrites spec recorded (provider=zarr)"
                (ir.Modules.[0].ProviderWrites |> Map.exists (fun _ s -> s.Provider = "zarr" && s.VarName = "A" && s.FilePath = outStore))
                ($"{(Map.count ir.Modules.[0].ProviderWrites)} write specs")
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_write_e2e"
            check "write e2e: emits flatten + filesystem writer"
                (cppCode.Contains "create_directories" && cppCode.Contains ".zarray") ""
            CodeGen.deployRuntimeHeaders e2eDir
            let cppFile = Path.Combine(e2eDir, "zarr_write_e2e.cpp")
            File.WriteAllText(cppFile, cppCode)
            (match compileCpp cppFile e2eDir with
             | Ok exePath ->
                 check "write e2e: compiles" true ""
                 (match runExecutable exePath with
                  | Ok (0, _) ->
                      check "write e2e: runs (exit 0)" true ""
                      (match readVarData outDirFull "A" with
                       | Ok { DimLengths = [4; 3]; Payload = ZFloats got } ->
                           check "write e2e: written store reads back exactly (F# reader)"
                               (got = [| for i in 0 .. 11 -> float i + 0.25 |]) (sprintf "got %A" got)
                       | Ok d -> check "write e2e: written store reads back" false (sprintf "shape %A" d.DimLengths)
                       | Error e -> check "write e2e: written store reads back" false e)
                      (match readVarData outDirFull "A" with
                       | Ok _ ->
                           let store = load outDirFull
                           check "write e2e: written store carries dim names (from provider index types)"
                               (match tryFindArray store "A" with
                                | Some m -> m.DimNames = Some ["x"; "y"]
                                | None -> false)
                               ""
                       | Error _ -> ())
                  | Ok (code, runOut) -> check "write e2e: runs (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "write e2e: runs (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP zarr write e2e (compile skipped): %s" e
                 else check "write e2e: compiles" false e)
        | Error e -> check "write e2e: lowers" false e
     with ex -> check "write e2e" false ex.Message)

    // ---------------------------------------------------------------
    // 12. load_compound rejection at codegen (zarr has no compound reader)
    // ---------------------------------------------------------------
    printfn "\n--- load_compound: loud zarr rejection ---"
    (let lcStore = fixStore "zarr_lc"
     (try Directory.Delete(lcStore, true) with _ -> ())
     ZarrWrite.writeStoreV2 lcStore [
        { Name = "A"; DimNames = Some ["x"]; Shape = [4L]; Chunks = [4L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 [| 1.0; 2.0; 3.0; 4.0 |]; OmitChunks = []; Blade = None }
        { Name = "M"; DimNames = Some ["x"]; Shape = [4L]; Chunks = [4L]
          FillValue = FillInt 0L; Data = ZarrWrite.WI64 [| 1L; 0L; 1L; 1L |]; OmitChunks = []; Blade = None } ]
     let lcSource = """
import zarr as z

let sample = z.load("tests/fixtures/zarr_stores/zarr_lc")
let data = z.load_compound(sample.vars.A, sample.vars.M) |> z.read
"""
     match lower lcSource with
     | Ok ir ->
         (try
             CodeGen.genSelfContainedProgramFromIR ir "zarr_lc" |> ignore
             check "load_compound via zarr: rejected loudly at codegen" false "codegen succeeded?"
          with ex ->
             check "load_compound via zarr: rejected loudly at codegen"
                 (ex.Message.Contains "does not support load_compound") ex.Message)
     | Error e ->
         check "load_compound via zarr: lowers (rejection is codegen's job)" false e)

    // ---------------------------------------------------------------
    // 13. blade layout attribute: parse + validation (pure)
    // ---------------------------------------------------------------
    printfn "\n--- blade packed layout: attribute parse ---"
    let bladeZattrs extra = sprintf """{"blade": {"spec_version": 1, "layout": "packed", "order": "ascending-lex", "index_types": [%s]}}""" extra
    let v2packed shape = sprintf """{"shape":[%s],"chunks":[%s],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}""" shape shape
    (match parseArrayMetaV2 "C" "d" (v2packed "10") (Some (bladeZattrs """{"kind": "sym", "rank": 2, "extent": 4}""")) with
     | Ok m ->
         check "blade parse: sym r2 n4 accepted (card 10)"
             (match m.Blade with
              | Some l -> l.Group.Sym = SymSymmetric && l.Group.Rank = 2 && l.Group.Extent = 4L && l.DenseDims = []
              | None -> false)
             (sprintf "%A" m.Blade)
     | Error e -> check "blade parse: sym r2 n4 accepted (card 10)" false e)
    check "blade parse: antisym r2 n4 accepted (card 6)"
        (match parseArrayMetaV2 "C" "d" (v2packed "6") (Some (bladeZattrs """{"kind": "antisym", "rank": 2, "extent": 4}""")) with
         | Ok m -> (match m.Blade with Some l -> l.Group.Sym = SymAntisymmetric | None -> false)
         | Error _ -> false) ""
    check "blade parse: mixed sym x dense accepted"
        (match parseArrayMetaV2 "C" "d" """{"shape":[6,3],"chunks":[6,3],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}"""
                   (Some (bladeZattrs """{"kind": "sym", "rank": 2, "extent": 3}, {"kind": "dense", "extent": 3}""")) with
         | Ok m -> (match m.Blade with Some l -> l.DenseDims = [3L] | None -> false)
         | Error _ -> false) ""
    check "blade parse: cardinality mismatch is LOUD"
        (isError (parseArrayMetaV2 "C" "d" (v2packed "9") (Some (bladeZattrs """{"kind": "sym", "rank": 2, "extent": 4}"""))) "cardinality 10") ""
    check "blade parse: herm reserved"
        (isError (parseArrayMetaV2 "C" "d" (v2packed "10") (Some (bladeZattrs """{"kind": "herm", "rank": 2, "extent": 4}"""))) "reserved") ""
    check "blade parse: unknown kind rejected"
        (isError (parseArrayMetaV2 "C" "d" (v2packed "10") (Some (bladeZattrs """{"kind": "diag", "rank": 2, "extent": 4}"""))) "unknown kind") ""
    check "blade parse: dense-first rejected (packed group must lead)"
        (isError (parseArrayMetaV2 "C" "d" """{"shape":[3,10],"chunks":[3,10],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}"""
                      (Some (bladeZattrs """{"kind": "dense", "extent": 3}, {"kind": "sym", "rank": 2, "extent": 4}"""))) "FIRST") ""
    // The version gate is closed UPWARD, not at 1: spec_version 2 (the orbit
    // head, section 18b) is implemented and a sym head under it is valid, so
    // the "future version" probe moved to 3.
    check "blade parse: future spec_version rejected"
        (isError (parseArrayMetaV2 "C" "d" (v2packed "10") (Some """{"blade": {"spec_version": 3, "layout": "packed", "index_types": [{"kind": "sym", "rank": 2, "extent": 4}]}}""")) "spec_version") ""
    check "blade parse: v3 attributes carry the layout too"
        (match parseArrayMetaV3 "C" "d" """{"zarr_format":3,"node_type":"array","shape":[10],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[5]}},"fill_value":0,"codecs":[{"name":"bytes"}],"attributes":{"blade":{"spec_version":1,"layout":"packed","order":"ascending-lex","index_types":[{"kind":"sym","rank":2,"extent":4}]}}}""" with
         | Ok m -> (match m.Blade with Some l -> l.Group.Rank = 2 && l.Group.Extent = 4L | None -> false)
         | Error e -> false) ""
    check "binom/packedCardinality: sym(2,4)=10, antisym(2,4)=6, sym(3,3)=10"
        (packedCardinality { Sym = SymSymmetric; Rank = 2; Extent = 4L; Levels = [] } = 10L
         && packedCardinality { Sym = SymAntisymmetric; Rank = 2; Extent = 4L; Levels = [] } = 6L
         && packedCardinality { Sym = SymSymmetric; Rank = 3; Extent = 3L; Levels = [] } = 10L) ""

    // ---------------------------------------------------------------
    // 14. Packed module typing (mirrors source-level SymIdx lowering)
    // ---------------------------------------------------------------
    printfn "\n--- blade packed layout: module typing ---"
    (let packedStore = {
        Path = "/mock/tri"; Version = 2
        Arrays = [
            { Name = "C"; ArrayDir = "/mock/tri/C"; Shape = [10L]; Chunks = [10L]
              Dtype = mockDt "f8" ETFloat64 8 true; DimNames = None
              FillValue = FillFloat 0.0; Codec = CodecIdentity
              Blade = Some { Group = { Sym = SymSymmetric; Rank = 2; Extent = 4L; Levels = [] }; DenseDims = []; Blocks = None }
              Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" }
        ] }
     let m = zarrStoreToModule (IRBuilder()) "tri" packedStore None
     let cType =
         m.Types |> List.tryPick (function IRTDStruct ("tri__vars", fs) -> Some fs | _ -> None)
         |> Option.defaultValue []
         |> List.tryPick (fun (n, t) -> if n = "C" then (match t with ArrayElem at -> Some at | _ -> None) else None)
     check "packed typing: C is Array<f64 like SymIdx<2,4>> (Symmetry/Rank/Extent match source lowering)"
         (match cType with
          | Some at ->
              at.IndexTypes.Length = 1
              && at.IndexTypes.[0].Symmetry = SymSymmetric
              && at.IndexTypes.[0].Rank = 2
              && (match at.IndexTypes.[0].Extent with IRLit (IRLitInt 4L) -> true | _ -> false)
              && at.IndexTypes.[0].IxKind = IxKPlain
          | None -> false)
         (sprintf "%A" cType))

    // ---------------------------------------------------------------
    // 15-17. Packed e2e: independent oracle -> read -> compute; and
    // read -> write roundtrip preserving exact pool order + metadata.
    // The oracle pool is computed with an INDEPENDENT F# enumeration
    // (ascending-lex loops here, not shared with provider code).
    // ---------------------------------------------------------------
    printfn "\n--- blade packed e2e: read + write roundtrips (sym and antisym) ---"
    let triOracle (strict: bool) (n: int) : float[] =
        [| for i in 0 .. n - 1 do
             for j in (if strict then i + 1 else i) .. n - 1 ->
               float ((i + 1) * 10 + (j + 1)) |]
    // Fold rejection first (hermetic, no g++): packed vars refuse to fold.
    (let foldTri = fixStore "zarr_tri_foldreject"
     (try Directory.Delete(foldTri, true) with _ -> ())
     ZarrWrite.writeStoreV2 foldTri [
        { Name = "C"; DimNames = None; Shape = [10L]; Chunks = [10L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 (triOracle false 4); OmitChunks = []
          Blade = Some { Group = { Sym = SymSymmetric; Rank = 2; Extent = 4L; Levels = [] }; DenseDims = []; Blocks = None } } ]
     match Blade.ProviderRegistry.tryFind "zarr" with
     | Some s ->
         check "packed fold: refused with steering"
             (match s.ReadVarData foldTri "C" with
              | Error e -> e.Contains "do not fold"
              | Ok _ -> false) ""
     | None -> check "packed fold: registry has zarr" false "")
    for (kind, sym, strict) in [ ("sym", SymSymmetric, false); ("antisym", SymAntisymmetric, true) ] do
        let n = 4
        let pool = triOracle strict n
        let card = int64 pool.Length
        let inStore = fixStore ($"zarr_tri_{kind}")
        let outStore = fixStore ($"zarr_tri_{kind}_out")
        let layout : BladeLayout = { Group = { Sym = sym; Rank = 2; Extent = int64 n; Levels = [] }; DenseDims = []; Blocks = None }
        let triVars : ZarrWrite.WriteVar list = [
            { Name = "C"; DimNames = None; Shape = [card]; Chunks = [card]
              FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
              Blade = Some layout } ]
        (try Directory.Delete(inStore, true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, inStore), true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        ZarrWrite.writeStoreV2 inStore triVars
        ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, inStore)) triVars
        let triSource = sprintf """
import zarr as z

let s = z.load("%s")
let C = s.vars.C |> z.read
let out = method_for(C) <@> lambda(x) -> x + x |> compute
let w = z.write("%s", C)
"""
                            inStore outStore
        try
            match lower triSource with
            | Ok ir ->
                check ($"packed {kind}: read spec is packed (provider=zarr)")
                    (ir.Modules.[0].ProviderReads |> Map.exists (fun _ s ->
                        s.Provider = "zarr" && s.VarType.IndexTypes |> List.exists (fun ix -> ix.Symmetry = sym && ix.Rank = 2)))
                    ""
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_tri_{kind}_e2e")
                // BOTH classes materialize by a linear pool_base copy under a
                // hoisted SYMM: allocate<>'s DFS pool order IS ascending-lex
                // for strict (antisym) storage exactly as for inclusive (sym)
                // storage, and the strict pool is compact (C(n,r) cells, no
                // dead diagonal). The antisym arm used to unrank each cell and
                // index the skeleton with `ix[k] - ix[k-1]`, which shifted the
                // whole pool by one; per-cell unranking here is now a defect.
                check ($"packed {kind}: codegen materializes the packed pool by linear copy")
                    (cppCode.Contains "pool_base"
                     && cppCode.Contains (if strict then "_anti" else "_symm")
                     && not (cppCode.Contains "antisymmetric::unlinearize")) ""
                check ($"packed {kind}: linearized_storage header included")
                    (cppCode.Contains "linearized_storage.hpp") ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_tri_{kind}_e2e.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     check ($"packed {kind}: compiles") true ""
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          check ($"packed {kind}: runs (exit 0)") true ""
                          // The doubled kernel output must be the doubled pool
                          // IN ORDER, cell for cell. A set comparison (or one
                          // that whitelists spurious 0.0) cannot see a pool
                          // rotated by one, which is precisely how the antisym
                          // strict-subscript defects presented — so this is a
                          // strict ordered comparison, and the print order of a
                          // packed array is part of the contract: it walks the
                          // canonical pool.
                          // Shape-tolerant flatten (TestHarness): a packed
                          // rank-2 group prints NESTED (genPrintArraySymAware
                          // routes rank 2 through genPrintNested2, row shrink
                          // and all), and flattening the nested walk IS the
                          // canonical pool order.
                          (match tryParsePrintedFloats "out" runOut with
                           | Some got ->
                               // EXACT elementwise equality in pool order, not
                               // the old
                               //   Set.isSubset expected got
                               //   && Set.isSubset (Set.remove 0.0 got) expected
                               // which was vacuous in two ways: Set.ofArray
                               // discarded MULTIPLICITY (a pool with repeated
                               // values passed even if the kernel emitted the
                               // wrong number of them), and `Set.remove 0.0 got`
                               // whitelisted an UNBOUNDED number of spurious
                               // zeros -- precisely the signature of a packed
                               // read that emitted unwritten padding cells. That
                               // whitelist was in fact absorbing a real defect:
                               // the antisym strict-storage off-by-one fixed on
                               // this branch, which shifted the pool and
                               // appended a 0.0. With the shift fixed, pool
                               // order is deterministic, so this pins ORDER too
                               // rather than sorting both sides -- a sorted
                               // compare would hide a future re-ordering
                               // regression in exactly the storage path the
                               // off-by-one lived in.
                               let expected = pool |> Array.map (fun x -> x + x)
                               check ($"packed {kind}: kernel values = 2x oracle pool (exact, in pool order)")
                                   (got = expected)
                                   (sprintf "got %A expected %A" got expected)
                           | None -> check ($"packed {kind}: kernel values") false "no parseable out = [...] line")
                          // Write roundtrip: exact pool order + blade metadata.
                          let outFull = Path.Combine(e2eDir, outStore)
                          (match readVarData outFull "C" with
                           | Ok { Payload = ZFloats got } ->
                               check ($"packed {kind}: written pool is EXACTLY the input pool (canonical order preserved)")
                                   (got = pool) (sprintf "got %A" (Array.truncate 6 got))
                           | Ok _ -> check ($"packed {kind}: written pool exact") false "not floats"
                           | Error e -> check ($"packed {kind}: written pool exact") false e)
                          (try
                              let wstore = load outFull
                              check ($"packed {kind}: written store carries the blade attribute")
                                  (match tryFindArray wstore "C" with
                                   | Some m -> m.Blade = Some layout
                                   | None -> false) ""
                           with ex -> check ($"packed {kind}: written store loads") false ex.Message)
                      | Ok (code, runOut) -> check ($"packed {kind}: runs (exit 0)") false ($"exit {code}: {runOut}")
                      | Error e -> check ($"packed {kind}: runs (exit 0)") false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP packed %s e2e (compile skipped): %s" kind e
                     else check ($"packed {kind}: compiles") false e)
            | Error e -> check ($"packed {kind}: lowers") false e
        with ex -> check ($"packed {kind} e2e") false ex.Message

    // ---------------------------------------------------------------
    // 17b. KERNEL-PRODUCED packed pool -> write (write side, unmasked)
    // ---------------------------------------------------------------
    // The read -> write roundtrip above CANNOT see a write-side pool shift:
    // the read materialization and the write flatten share one offset
    // formula, so a rotation applied on the way in is undone on the way out
    // and the store still matches byte for byte. The write path is only
    // pinned when the pool it flattens was built by a KERNEL (canonical
    // storage, no provider read in the chain) and is compared against an
    // independent oracle. That is this block. It is what the antisym
    // strict-subscript defect escaped through: `blade run` printed the right
    // six cells while the chunk on disk held the pool shifted left by one
    // with a fill 0.0 appended.
    printfn "\n--- blade packed e2e: kernel-produced pool -> write (independent oracle) ---"
    let sign (p: int list) =
        let a = List.toArray p
        let mutable s = 0
        for i in 0 .. a.Length - 1 do
            for j in i + 1 .. a.Length - 1 do
                if a.[i] > a.[j] then s <- s + 1
        if s % 2 = 0 then 1.0 else -1.0
    let rec perms (xs: int list) =
        match xs with
        | [] -> [ [] ]
        | _ -> xs |> List.collect (fun x -> perms (List.filter ((<>) x) xs) |> List.map (fun r -> x :: r))
    // (label, extent, rank, kernel body over params p0..p{rank-1}, scalar oracle
    //  on one ordered cell's A-values, symmetry keyword, SymmetryClass)
    // The oracle Reynolds-folds the SAME scalar function over every parameter
    // permutation with the group's sign — an enumeration written here, not
    // shared with any compiler code.
    let kernelWrites =
        [ "sym2",     4, 2, "p0 * 10.0 + p1",  (fun (v: float list) -> v.[0] * 10.0 + v.[1]),  "Symmetric",     SymSymmetric
          "antisym2", 4, 2, "p0 * 10.0 + p1",  (fun (v: float list) -> v.[0] * 10.0 + v.[1]),  "Antisymmetric", SymAntisymmetric
          // Rank 3 compounds the per-level strict shift: the retired formula
          // was wrong at every level beyond the first, not just the last.
          "antisym3", 5, 3, "p0 * p0 * p1",    (fun (v: float list) -> v.[0] * v.[0] * v.[1]), "Antisymmetric", SymAntisymmetric ]
    for (label, n, rank, body, scalarF, symKw, sym) in kernelWrites do
        let strict = (sym = SymAntisymmetric)
        let avals = [ for i in 0 .. n - 1 -> float (i + 1) ]
        // Canonical cells in ascending-lex order (i0 <= i1 <= .. for sym,
        // strictly increasing for antisym) — enumerated directly here.
        let rec cells (lo: int) (k: int) : int list list =
            if k = 0 then [ [] ]
            else [ for i in lo .. n - 1 do
                     for rest in cells (if strict then i + 1 else i) (k - 1) -> i :: rest ]
        let oracle =
            [| for c in cells 0 rank ->
                 perms [ 0 .. rank - 1 ]
                 |> List.sumBy (fun p ->
                     let vs = p |> List.map (fun q -> avals.[c.[q]])
                     (if strict then sign p else 1.0) * scalarF vs) |]
        let ps = [ 0 .. rank - 1 ] |> List.map (sprintf "p%d") |> String.concat ", "
        let outStore = fixStore ($"zarr_kw_{label}_out")
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        let src =
            sprintf """
import zarr as z

let A = [%s]
let g = lambda(%s) where comm(%s) -> %s
let C = method_for(%s) <@> reynolds(g, %s) |> compute
let w = z.write("%s", C)
"""
                (avals |> List.map (sprintf "%.1f") |> String.concat ", ")
                ps ps body
                (List.replicate rank "A" |> String.concat ", ")
                symKw outStore
        try
            match lower src with
            | Ok ir ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_kw_{label}_e2e")
                check ($"kernel-write {label}: pool flatten is a linear pool_base copy")
                    (cppCode.Contains "pool_base" && not (cppCode.Contains "antisymmetric::unlinearize")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_kw_{label}_e2e.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          // The chunk on disk must equal the oracle pool cell
                          // for cell — no rotation, no trailing fill.
                          (match readVarData (Path.Combine(e2eDir, outStore)) "C" with
                           | Ok { DimLengths = dl; Payload = ZFloats got } ->
                               check ($"kernel-write {label}: written pool = independent oracle (exact, in order)")
                                   (dl = [oracle.Length] && got = oracle)
                                   (sprintf "shape %A got %A expected %A" dl got oracle)
                           | Ok _ -> check ($"kernel-write {label}: written pool") false "not floats"
                           | Error e -> check ($"kernel-write {label}: written pool") false e)
                          // ... and the in-process print must agree with it, so
                          // a shift cannot hide by moving between the two.
                          // Shape-tolerant flatten (TestHarness): C is a
                          // packed rank-2 group and prints NESTED; the
                          // flattened walk is the canonical pool order.
                          (match tryParsePrintedFloats "C" runOut with
                           | Some got ->
                               check ($"kernel-write {label}: printed pool = written pool = oracle")
                                   (got = oracle) (sprintf "got %A expected %A" got oracle)
                           | None -> check ($"kernel-write {label}: printed pool") false "no parseable C = [...] line")
                      | Ok (code, runOut) -> check ($"kernel-write {label}: runs (exit 0)") false ($"exit {code}: {runOut}")
                      | Error e -> check ($"kernel-write {label}: runs (exit 0)") false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP kernel-write %s e2e (compile skipped): %s" label e
                     else check ($"kernel-write {label}: compiles") false e)
            | Error e -> check ($"kernel-write {label}: lowers") false e
        with ex -> check ($"kernel-write {label} e2e") false ex.Message

    // ---------------------------------------------------------------
    // 18. Mixed sym x dense packed read -> write roundtrip
    // ---------------------------------------------------------------
    printfn "\n--- blade packed e2e: mixed sym x dense ---"
    (let n = 3
     let trail = 2
     let symCells = [ for i in 0 .. n - 1 do for j in i .. n - 1 -> (i, j) ]
     let pool =
         [| for (i, j) in symCells do
              for t in 0 .. trail - 1 ->
                float (100 * (i + 1) + 10 * (j + 1) + t) |]
     let card = int64 symCells.Length
     let layout : BladeLayout = { Group = { Sym = SymSymmetric; Rank = 2; Extent = int64 n; Levels = [] }; DenseDims = [int64 trail]; Blocks = None }
     let mixVars : ZarrWrite.WriteVar list = [
        { Name = "D"; DimNames = Some ["cells"; "t"]; Shape = [card; int64 trail]; Chunks = [card; int64 trail]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
          Blade = Some layout } ]
     let inStore = fixStore "zarr_tri_mixed"
     let outStore = fixStore "zarr_tri_mixed_out"
     (try Directory.Delete(inStore, true) with _ -> ())
     (try Directory.Delete(Path.Combine(e2eDir, inStore), true) with _ -> ())
     (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
     ZarrWrite.writeStoreV2 inStore mixVars
     ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, inStore)) mixVars
     let mixSource = sprintf """
import zarr as z

let s = z.load("%s")
let D = s.vars.D |> z.read
let w = z.write("%s", D)
"""
                         inStore outStore
     try
        match lower mixSource with
        | Ok ir ->
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_tri_mixed_e2e"
            CodeGen.deployRuntimeHeaders e2eDir
            let cppFile = Path.Combine(e2eDir, "zarr_tri_mixed_e2e.cpp")
            File.WriteAllText(cppFile, cppCode)
            (match compileCpp cppFile e2eDir with
             | Ok exePath ->
                 (match runExecutable exePath with
                  | Ok (0, _) ->
                      check "packed mixed: runs (exit 0)" true ""
                      (match readVarData (Path.Combine(e2eDir, outStore)) "D" with
                       | Ok { DimLengths = dl; Payload = ZFloats got } ->
                           check "packed mixed: pool x trailing roundtrips exactly"
                               (dl = [int card; trail] && got = pool) (sprintf "shape %A" dl)
                       | Ok _ -> check "packed mixed: roundtrip" false "not floats"
                       | Error e -> check "packed mixed: roundtrip" false e)
                  | Ok (code, out) -> check "packed mixed: runs (exit 0)" false ($"exit {code}: {out}")
                  | Error e -> check "packed mixed: runs (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP packed mixed e2e (compile skipped): %s" e
                 else check "packed mixed: compiles" false e)
        | Error e -> check "packed mixed: lowers" false e
     with ex -> check "packed mixed e2e" false ex.Message)

    // ---------------------------------------------------------------
    // 18b. spec_version 2: the `orbit` (iterated-wreath) head — metadata
    // ---------------------------------------------------------------
    // Pure JSON/typing probes, hermetic (no g++). The e2e half is 18c.
    printfn "\n--- blade orbit head (spec_version 2): attribute parse ---"
    let orbAttr (spec: int) (levels: string) (extent: int) (extraEntries: string) (layoutAndDecomp: string) =
        sprintf """{"blade": {"spec_version": %d, "layout": %s, "order": "ascending-lex", "index_types": [{"kind": "orbit", "levels": [%s], "extent": %d}%s]}}"""
            spec layoutAndDecomp levels extent extraEntries
    let v2arr (shape: string) =
        sprintf """{"shape":[%s],"chunks":[%s],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}""" shape shape
    // [(2,+),(2,+)] at n = 3 folds 3 -> C(4,2) = 6 -> C(7,2) = 21.
    (match parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 2 """[2, "+"], [2, "+"]""" 3 "" "\"packed\"")) with
     | Ok m ->
         check "orbit parse: [(2,+),(2,+)] n=3 -> SymWreath group, 21-cell pool"
             (match m.Blade with
              | Some l ->
                  l.Group.Sym = SymWreath && l.Group.Levels = [(2, true); (2, true)]
                  && l.Group.Extent = 3L && l.Group.Rank = 4 && l.DenseDims = [] && l.Blocks = None
              | None -> false)
             (sprintf "%A" m.Blade)
     | Error e -> check "orbit parse: [(2,+),(2,+)] n=3" false e)
    // The Riemann twin: mixed character, 4 -> C(4,2) = 6 -> C(7,2) = 21.
    (match parseArrayMetaV2 "R" "d" (v2arr "21") (Some (orbAttr 2 """[2, "-"], [2, "+"]""" 4 "" "\"packed\"")) with
     | Ok m ->
         check "orbit parse: [(2,-),(2,+)] n=4 -> 21-cell pool (mixed character)"
             (match m.Blade with
              | Some l -> l.Group.Levels = [(2, false); (2, true)] && l.Group.Extent = 4L
              | None -> false)
             (sprintf "%A" m.Blade)
     | Error e -> check "orbit parse: [(2,-),(2,+)] n=4" false e)
    check "orbit parse: cardinality is the ITERATED fold (packedCardinalityChecked)"
        (packedCardinalityChecked { Sym = SymWreath; Rank = 4; Extent = 3L; Levels = [(2, true); (2, true)] } = Ok 21L
         && packedCardinalityChecked { Sym = SymWreath; Rank = 4; Extent = 4L; Levels = [(2, false); (2, true)] } = Ok 21L
         && packedCardinalityChecked { Sym = SymWreath; Rank = 4; Extent = 4L; Levels = [(2, true); (2, true)] } = Ok 55L) ""
    // THE version gate: a v1 reader must refuse an orbit head loudly. Our own
    // v1 path is that reader, so this pins it rather than assuming it.
    check "orbit parse: kind 'orbit' under spec_version 1 is refused BY NAME"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 1 """[2, "+"], [2, "+"]""" 3 "" "\"packed\""))) "spec_version 2 head") ""
    // One spelling per class on disk.
    check "orbit parse: a DEPTH-1 orbit head is illegal (sym/antisym is the spelling)"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "6") (Some (orbAttr 2 """[2, "+"]""" 3 "" "\"packed\""))) "depth >= 2") ""
    check "orbit parse: an EMPTY level list is illegal"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "3") (Some (orbAttr 2 "" 3 "" "\"packed\""))) "depth >= 2") ""
    check "orbit parse: a rank-1 level is illegal (it normalizes away -> two spellings)"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 2 """[1, "+"], [2, "+"], [2, "+"]""" 3 "" "\"packed\""))) "rank 1 is not >= 2") ""
    check "orbit parse: a malformed sign is refused"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 2 """[2, "+"], [2, "*"]""" 3 "" "\"packed\""))) "is not \"+\" or \"-\"") ""
    check "orbit parse: a missing levels array is refused"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21")
                     (Some """{"blade": {"spec_version": 2, "layout": "packed", "index_types": [{"kind": "orbit", "extent": 3}]}}""")) "missing its 'levels'") ""
    // Pool-length mismatch: a LOAD ERROR, never a reinterpretation. 20 != 21.
    check "orbit parse: pool length != cardinality is a loud load error"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "20") (Some (orbAttr 2 """[2, "+"], [2, "+"]""" 3 "" "\"packed\""))) "cardinality 21 but the pool dimension is 20") ""
    check "orbit parse: trailing dense dims are refused (not yet supported)"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21, 2")
                     (Some (orbAttr 2 """[2, "+"], [2, "+"]""" 3 """, {"kind": "dense", "extent": 2}""" "\"packed\""))) "not yet supported") ""
    check "orbit parse: 'packed-blocks' is not defined for an orbit head"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 2 """[2, "+"], [2, "+"]""" 3 "" "\"packed-blocks\""))) "not defined for an orbit") ""
    check "orbit parse: spec_version 3 is still refused (the version gate is closed upward)"
        (isError (parseArrayMetaV2 "W" "d" (v2arr "21") (Some (orbAttr 3 """[2, "+"], [2, "+"]""" 3 "" "\"packed\""))) "spec_version 3 is not supported") ""
    check "orbit parse: sym heads still work under spec_version 2 (superset, not replacement)"
        (match parseArrayMetaV2 "C" "d" (v2arr "10")
                   (Some """{"blade": {"spec_version": 2, "layout": "packed", "index_types": [{"kind": "sym", "rank": 2, "extent": 4}]}}""") with
         | Ok m -> (match m.Blade with Some l -> l.Group.Sym = SymSymmetric && l.Group.Levels = [] | None -> false)
         | Error _ -> false) ""
    check "orbit parse: v3 (zarr.json) carries the same head"
        (match parseArrayMetaV3 "W" "d" """{"zarr_format":3,"node_type":"array","shape":[21],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[21]}},"fill_value":0,"codecs":[{"name":"bytes"}],"attributes":{"blade":{"spec_version":2,"layout":"packed","order":"ascending-lex","index_types":[{"kind":"orbit","levels":[[2,"-"],[2,"+"]],"extent":4}]}}}""" with
         | Ok m -> (match m.Blade with Some l -> l.Group.Sym = SymWreath && l.Group.Levels = [(2, false); (2, true)] | None -> false)
         | Error _ -> false) ""

    printfn "\n--- blade orbit head: module typing + attribute round trip ---"
    (let orbLayout : BladeLayout =
        { Group = { Sym = SymWreath; Rank = 4; Extent = 3L; Levels = [(2, true); (2, true)] }
          DenseDims = []; Blocks = None }
     let orbStore = {
        Path = "/tmp/orb"; Version = 2
        Arrays = [ { Name = "W"; ArrayDir = "/tmp/orb/W"; Shape = [21L]; Chunks = [21L]
                     Dtype = { Code = "f8"; Elem = ETFloat64; ByteSize = 8; IsFloat = true }
                     DimNames = None; FillValue = FillFloat 0.0; Codec = CodecIdentity
                     Blade = Some orbLayout; Version = 2; ChunkKeySep = "."; ChunkKeyPrefix = "" } ] }
     let m = zarrStoreToModule (IRBuilder()) "orb" orbStore None
     let wType =
        m.Types |> List.tryPick (function
            | IRTDStruct ("orb__vars", fields) -> fields |> List.tryPick (fun (n, t) -> if n = "W" then Some t else None)
            | _ -> None)
     // The record must be THE one mkWreathIndexRecord builds — same Rank (raw
     // axes), same IROrbitClass carrier, same "__orbidx" sentinel — because a
     // loaded class and a deduced one have to be the same type, not two records
     // that merely print alike.
     check "orbit typing: W types as the SymWreath record (levels, extent, raw-axis rank, sentinel tag)"
         (match wType with
          | Some (ArrayElem at) ->
              (match at.IndexTypes with
               | [ ix ] ->
                   ix.Symmetry = SymWreath && ix.Rank = 4 && ix.Tag = Some "__orbidx"
                   && ix.IxKind = IxKOrbit
                   && Blade.IR.orbitLevelsOf ix = [(2, true); (2, true)]
                   && Blade.IR.orbitBaseExtent ix = IRLit (IRLitInt 3L)
               | _ -> false)
          | _ -> false)
         (sprintf "%A" wType)
     check "orbit typing: the pool dim does NOT join the module's shareable dimensions"
         (m.Types |> List.forall (fun t -> not t.IsIRTDIndexType)) ""
     // F#-writer round trip: the attribute we emit is the attribute we parse.
     let orbFix = fixStore "zarr_orb_attr"
     (try Directory.Delete(orbFix, true) with _ -> ())
     let orbVars : ZarrWrite.WriteVar list = [
        { Name = "W"; DimNames = None; Shape = [21L]; Chunks = [21L]
          FillValue = FillFloat 0.0
          Data = ZarrWrite.WF64 [| for i in 1 .. 21 -> float i |]; OmitChunks = []
          Blade = Some orbLayout } ]
     ZarrWrite.writeStoreV2 orbFix orbVars
     let attrText = File.ReadAllText (Path.Combine(orbFix, "W", ".zattrs"))
     check "orbit write (F#): the attribute says spec_version 2 + kind orbit + outermost-last levels"
         (attrText.Contains "\"spec_version\": 2"
          && attrText.Contains "\"kind\": \"orbit\""
          && attrText.Contains "\"levels\": [[2, \"+\"], [2, \"+\"]]"
          && attrText.Contains "\"extent\": 3") attrText
     check "orbit write (F#): the store reloads to the SAME layout record"
         (match load orbFix |> fun s -> tryFindArray s "W" with
          | Some m2 -> m2.Blade = Some orbLayout
          | None -> false) ""
     check "orbit write (F#): readPackedPool returns the 21 cells in order"
         (match load orbFix |> fun s -> tryFindArray s "W" |> Option.get |> readPackedPool with
          | Ok { DimLengths = [21]; Payload = ZFloats xs } -> xs = [| for i in 1 .. 21 -> float i |]
          | other -> printfn "    (got %A)" other; false) ""
     // v3 too — the head is version-agnostic.
     let orbFix3 = fixStore "zarr_orb_attr_v3"
     (try Directory.Delete(orbFix3, true) with _ -> ())
     ZarrWrite.writeStoreV3 orbFix3 orbVars
     check "orbit write (F#): v3 store carries and reloads the same head"
         (match load orbFix3 |> fun s -> tryFindArray s "W" with
          | Some m3 -> m3.Blade = Some orbLayout && m3.Version = 3
          | None -> false) "")

    // ---------------------------------------------------------------
    // 18c. spec_version 2 e2e: deduced wreath -> write -> read -> use
    // ---------------------------------------------------------------
    // Both directions of the store boundary, on BOTH backends. Values are
    // hand-derived (corpus 200 / 213 derive them from the kernel definition,
    // not from a pool dump), never a read->write roundtrip: both sides of a
    // roundtrip shift together, so a roundtrip cannot see an order mismatch
    // (plan-orbidx-bijections §3, the antisym storage post-mortem).
    printfn "\n--- blade orbit e2e: deduced wreath -> z.write -> z.read ---"
    /// Compiled stdout for a source, or a skip/failure reason.
    let compiledStdout (label: string) (src: string) : Result<string, string> =
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir ->
            let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir label
            CodeGen.deployRuntimeHeaders e2eDir
            let cppFile = Path.Combine(e2eDir, label + ".cpp")
            File.WriteAllText(cppFile, cpp)
            match compileCpp cppFile e2eDir with
            | Error e -> Error e
            | Ok exePath ->
                match runExecutable exePath with
                | Ok (0, out) -> Ok out
                | Ok (code, out) -> Error ($"exit {code}: {out}")
                | Error e -> Error e
    /// Normalized stdout comparison, mirroring InterpDiff.normalize (timing
    /// lines out, CRLF -> LF, trimmed).
    let normOut (s: string) =
        s.Replace("\r\n", "\n").Split('\n')
        |> Array.filter (fun l -> not (l.Contains "completed in") && not (l.Contains "input allocation took"))
        |> Array.map (_.TrimEnd())
        |> String.concat "\n" |> fun t -> t.Trim()
    /// The interpreter walk of the same program (the second backend).
    let interpStdout (label: string) (src: string) : Result<string, string> =
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir ->
            let r = Blade.Interp.Run.runProgram ir label Blade.Interp.Value.defaultLimits
            if r.ExitCode = 0 then Ok r.Stdout
            else Error ($"interp exit {r.ExitCode}: {(r.Stderr.Trim())}")
    let orbCases =
        [ // (label, extent, inner reynolds keyword or "", inner kernel, outer
          //  kernel, LEVELS, hand-derived pool, probe (name, subscript, value))
          // 1. The all-'+' tie: S = A(x)A symmetric, W = S + S over the pair-of-pairs
          //    class OrbIdx<[(2,+),(2,+)], 3>. 21 cells; corpus 200's hand-derived list.
          ("orb_tied", 3, "", "x * y", "p + q", [(2, true); (2, true)],
           [| 2.0; 3.0; 4.0; 5.0; 7.0; 10.0; 4.0; 5.0; 6.0; 8.0; 11.0; 6.0; 7.0; 9.0; 12.0; 8.0; 10.0; 13.0; 12.0; 15.0; 18.0 |],
           [ "canonical", "W(0, 1, 0, 1)", 4.0
             "mirrored",  "W(1, 0, 0, 1)", 4.0
             "outer",     "W(2, 2, 0, 0)", 10.0
             "outerSwapped", "W(0, 0, 2, 2)", 10.0
             "bothMirrored", "W(2, 1, 1, 0)", 8.0 ])
          // 2. The Riemann twin: OrbIdx<[(2,-),(2,+)], 4>. Mixed character —
          //    a signed mirror AND a zero set, neither of which the all-'+'
          //    class can exercise. Corpus 213's hand-derived values.
          ("orb_riemann", 4, "Antisymmetric", "2.0 * x + y", "p * q", [(2, false); (2, true)],
           [| 1.0; 2.0; 3.0; 1.0; 2.0; 1.0; 4.0; 6.0; 2.0; 4.0; 2.0; 9.0; 3.0; 6.0; 3.0; 1.0; 2.0; 1.0; 4.0; 2.0; 1.0 |],
           [ "canonical",  "W(0, 1, 2, 3)", 1.0
             "oneMirror",  "W(1, 0, 2, 3)", -1.0
             "bothMirror", "W(1, 0, 3, 2)", 1.0
             "blockSwap",  "W(2, 3, 0, 1)", 1.0
             "crossed",    "W(0, 2, 1, 3)", 4.0
             "zeroFirst",  "W(0, 0, 1, 2)", 0.0
             "zeroSecond", "W(1, 2, 3, 3)", 0.0 ]) ]
    for (label, n, innerSym, innerBody, outerBody, levels, pool, probes) in orbCases do
        // Two independent renderings of the SAME level list: the surface
        // spelling (unused by the write program, which DEDUCES the class — it is
        // here so the case row states what is expected) and the on-disk JSON the
        // attribute must carry, outermost-last in both.
        let levelsJson =
            "[" + (levels |> List.map (fun (r, plus) -> sprintf "[%d, \"%s\"]" r (if plus then "+" else "-")) |> String.concat ", ") + "]"
        let outStore = fixStore ($"zarr_{label}_out")
        (try Directory.Delete(outStore, true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        let avals = [ for i in 1 .. n -> sprintf "%.1f" (float i) ] |> String.concat ", "
        let innerKernel = if innerSym = "" then "g" else $"reynolds(g, {innerSym})"
        let writeSrc =
            sprintf """
import zarr as z

let A = [%s]
let g = lambda(x, y) where comm(x, y) -> %s
let S = method_for(A, A) <@> %s |> compute
let h = lambda(p, q) where comm(p, q) -> %s
let W = method_for(S, S) <@> h |> compute
let w = z.write("%s", W)
"""
                    avals innerBody innerKernel outerBody outStore
        try
            match lower writeSrc with
            | Error e -> check ($"{label}: write program lowers") false e
            | Ok ir ->
                // The write must go through the ORBIT arm, not the depth-1
                // packed one: a wreath array IS its flat pool, so nothing may
                // reach W through a skeleton (`pool_base(W.data)`) and the
                // packed arm's pool copy (`__pc_pool`, genPackedPoolCopy) must
                // not appear at all.
                //
                // BOTH negatives are SCOPED, and must stay scoped. This used to
                // scan the whole program for the bare word "pool_base", which was
                // only ever true by accident: the program also builds the compact
                // intermediate S, and since 39e0a0c a canonical compact fill
                // reaches its output row through `pool_base(S.data)` instead of
                // the Iliffe skeleton. That is an unrelated array and says nothing
                // about how W is written -- but it turned this into a false red.
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"{label}_write")
                check ($"{label}: write emits a flat pool copy + the orb_cell_count pin (no skeleton route for W)")
                    (cppCode.Contains "orb_cell_count" && cppCode.Contains "_flat[__ow_i]"
                     && not (cppCode.Contains "pool_base(W.data)")
                     && not (cppCode.Contains "__pc_pool")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"{label}_write.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Error e ->
                     if isSkipError e then printfn "  SKIP %s e2e (compile skipped): %s" label e
                     else check ($"{label}: write compiles") false e
                 | Ok exePath ->
                 match runExecutable exePath with
                 | Ok (code, out) when code <> 0 ->
                     check ($"{label}: write runs (exit 0)") false ($"exit {code}: {out}")
                 | Error e -> check ($"{label}: write runs (exit 0)") false e
                 | Ok (_, writeOut) ->
                     check ($"{label}: write runs (exit 0)") true ""
                     let written = Path.Combine(e2eDir, outStore)
                     // (a) The pool on disk equals the hand-derived cells, in
                     // order — the independent-oracle comparison, not a roundtrip.
                     (match readVarData written "W" with
                      | Ok { DimLengths = dl; Payload = ZFloats got } ->
                          check ($"{label}: written pool = hand-derived cells (exact, in canonical order)")
                              (dl = [pool.Length] && got = pool)
                              (sprintf "shape %A got %A expected %A" dl got pool)
                      | Ok _ -> check ($"{label}: written pool") false "not floats"
                      | Error e -> check ($"{label}: written pool") false e)
                     // ... and the in-process print agrees, so a shift cannot
                     // hide by moving between the two.
                     // Shape-tolerant flatten (TestHarness). A wreath pool
                     // prints FLAT today (genPrintArrayWreath), so this is a
                     // no-op flatten -- used anyway so a printer-shape change
                     // cannot rot this site the way rank-2 nesting rotted the
                     // packed ones.
                     (match tryParsePrintedFloats "W" writeOut with
                      | Some got ->
                          check ($"{label}: printed pool = written pool") (got = pool) (sprintf "got %A" got)
                      | None -> check ($"{label}: printed pool") false "no parseable W = [...] line")
                     // (b) The attribute: spec_version 2, orbit head, exact
                     // level list and extent.
                     let attrText = File.ReadAllText (Path.Combine(written, "W", ".zattrs"))
                     check ($"{label}: written attribute is spec_version 2 + kind orbit + the exact levels")
                         (attrText.Contains "\"spec_version\": 2"
                          && attrText.Contains "\"kind\": \"orbit\""
                          && attrText.Contains ($"\"extent\": {n}")
                          && attrText.Contains ($"\"levels\": {levelsJson}"))
                         ($"attr {attrText} (wanted levels {levelsJson})")
                     // (c) Reload: the store types back as the same class, and
                     // the RELOADED array serves mirrored / zero-set reads.
                     (try Directory.Delete(outStore, true) with _ -> ())
                     let copyDir (src: string) (dst: string) =
                         for f in Directory.GetFiles(src, "*", SearchOption.AllDirectories) do
                             let rel = Path.GetRelativePath(src, f)
                             let target = Path.Combine(dst, rel)
                             Directory.CreateDirectory (Path.GetDirectoryName target) |> ignore
                             File.Copy(f, target, true)
                     // Compiler cwd copy (metadata + the interpreter's read)
                     // beside the exe-dir copy the binary reads: the same
                     // two-copy scheme every other e2e block uses.
                     copyDir written outStore
                     let probeLines =
                         probes |> List.map (fun (nm, expr, _) -> $"let {nm} = {expr}") |> String.concat "\n"
                     let readSrc =
                         sprintf """
import zarr as z

let s = z.load("%s")
let W = s.vars.W |> z.read
%s
"""
                                 outStore probeLines
                     match compiledStdout ($"{label}_read") readSrc with
                     | Error e ->
                         if isSkipError e then printfn "  SKIP %s read e2e: %s" label e
                         else check ($"{label}: read compiles and runs") false e
                     | Ok readOut ->
                         check ($"{label}: read compiles and runs") true ""
                         let scalarOf (nm: string) : float option =
                             readOut.Split('\n')
                             |> Array.tryPick (fun l ->
                                 let l = l.Trim()
                                 if l.StartsWith (nm + " = ") then
                                     match Double.TryParse(l.Substring((nm + " = ").Length).Trim(),
                                                           Globalization.NumberStyles.Float,
                                                           Globalization.CultureInfo.InvariantCulture) with
                                     | true, v -> Some v
                                     | _ -> None
                                 else None)
                         let missing =
                             probes |> List.choose (fun (nm, _, v) ->
                                 match scalarOf nm with
                                 | Some got when abs (got - v) < 1e-9 -> None
                                 | other -> Some (nm, v, other))
                         check ($"{label}: RELOADED array serves every mirrored / zero-set read")
                             (List.isEmpty missing)
                             (sprintf "wrong/missing %A in:\n%s" missing readOut)
                         // Shape-tolerant flatten (TestHarness) -- see the
                         // write-side W parse above for why.
                         let reloadedPool = tryParsePrintedFloats "W" readOut
                         check ($"{label}: RELOADED array prints the pool in canonical order ({pool.Length} cells)")
                             (reloadedPool = Some pool)
                             (sprintf "got %A expected %A" reloadedPool pool)
                         // (d) BOTH BACKENDS. The interpreter walks the same
                         // store from the compiler cwd copy; its stdout must be
                         // byte-identical to the compiled binary's.
                         (match interpStdout ($"{label}_read") readSrc with
                          | Error e -> check ($"{label}: interpreter reads the orbit store") false e
                          | Ok interpOut ->
                              check ($"{label}: interp stdout == compiled stdout (both backends agree)")
                                  (normOut interpOut = normOut readOut)
                                  ($"interp:\n{(normOut interpOut)}\ncompiled:\n{(normOut readOut)}")))
            with ex -> check ($"{label} e2e") false ex.Message

    // A DEPTH-1 class spelled `OrbIdx<[(2,+)], n>` must write as "sym", not
    // "orbit": it normalizes to the SymIdx record at lowering, so the orbit
    // head never sees it, and the store keeps ONE spelling per class.
    printfn "\n--- blade orbit head: depth-1 OrbIdx writes as sym/antisym ---"
    for (kw, kindStr, cells) in [ ("+", "sym", 6); ("-", "antisym", 3) ] do
        let outStore = fixStore ($"zarr_orb_d1_{kindStr}_out")
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        let symKw = if kw = "+" then "Symmetric" else "Antisymmetric"
        let src =
            sprintf """
import zarr as z

let A = [1.0, 2.0, 3.0]
let g = lambda(x, y) where comm(x, y) -> 2.0 * x + y
let C: Array<Float64 like OrbIdx<[(2,%s)], 3>> = method_for(A, A) <@> reynolds(g, %s) |> compute
let w = z.write("%s", C)
"""
                    kw symKw outStore
        try
            match lower src with
            | Error e -> check ($"orbit depth-1 {kindStr}: lowers") false e
            | Ok ir ->
                let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_orb_d1_{kindStr}")
                // Pinned on the EMITTED attribute, no run required: the writer
                // bakes the JSON as a literal.
                check ($"orbit depth-1 {kindStr}: emitted attribute says kind '{kindStr}', spec_version 1, NOT orbit")
                    (cpp.Contains ($"kind\\\": \\\"{kindStr}")
                     && cpp.Contains "spec_version\\\": 1"
                     && not (cpp.Contains "\\\"orbit\\\"")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_orb_d1_{kindStr}.cpp")
                File.WriteAllText(cppFile, cpp)
                (match compileCpp cppFile e2eDir with
                 | Error e ->
                     if isSkipError e then printfn "  SKIP orbit depth-1 %s e2e: %s" kindStr e
                     else check ($"orbit depth-1 {kindStr}: compiles") false e
                 | Ok exePath ->
                     match runExecutable exePath with
                     | Ok (0, _) ->
                         check ($"orbit depth-1 {kindStr}: written store is a depth-1 packed head (reload pins it)")
                             (match load (Path.Combine(e2eDir, outStore)) |> fun s -> tryFindArray s "C" with
                              | Some m ->
                                  (match m.Blade with
                                   | Some l ->
                                       l.Group.Levels = []
                                       && l.Group.Sym = (if kw = "+" then SymSymmetric else SymAntisymmetric)
                                       && l.Group.Rank = 2 && m.Shape = [int64 cells]
                                   | None -> false)
                              | None -> false) ""
                     | Ok (code, out) -> check ($"orbit depth-1 {kindStr}: runs") false ($"exit {code}: {out}")
                     | Error e -> check ($"orbit depth-1 {kindStr}: runs") false e)
        with ex -> check ($"orbit depth-1 {kindStr}") false ex.Message

    // ---------------------------------------------------------------
    // 18d. Corrupted orbit stores + the providers that still refuse
    // ---------------------------------------------------------------
    printfn "\n--- blade orbit head: corrupt stores and non-zarr providers refuse ---"
    (let badPool = fixStore "zarr_orb_badpool"
     (try Directory.Delete(badPool, true) with _ -> ())
     // A hand-built store whose pool is one cell short of the class's fold.
     // ZarrWrite would refuse to build the inconsistency, so it is written by
     // hand — which is exactly the shape an external writer's bug takes.
     Directory.CreateDirectory (Path.Combine(badPool, "W")) |> ignore
     File.WriteAllText(Path.Combine(badPool, ".zgroup"), "{\"zarr_format\": 2}")
     File.WriteAllText(Path.Combine(badPool, "W", ".zarray"),
        "{\"zarr_format\": 2, \"shape\": [20], \"chunks\": [20], \"dtype\": \"<f8\", \"compressor\": null, \"fill_value\": 0, \"order\": \"C\", \"filters\": null}")
     File.WriteAllText(Path.Combine(badPool, "W", ".zattrs"),
        "{\"blade\": {\"spec_version\": 2, \"layout\": \"packed\", \"order\": \"ascending-lex\", \"index_types\": [{\"kind\": \"orbit\", \"levels\": [[2, \"+\"], [2, \"+\"]], \"extent\": 3}]}}")
     File.WriteAllBytes(Path.Combine(badPool, "W", "0"), Array.zeroCreate (20 * 8))
     let src = sprintf """
import zarr as z

let s = z.load("%s")
let W = s.vars.W |> z.read
"""
                     badPool
     check "orbit corrupt: a short pool fails the LOAD, loudly, with both numbers"
         (match (try lower src with ex -> Error ex.Message) with
          | Error e -> e.Contains "cardinality 21" && e.Contains "pool dimension is 20"
          | Ok _ -> false)
         (match (try lower src with ex -> Error ex.Message) with Error e -> e | Ok _ -> "lowered"))
    // Depth-1 orbit head on disk: refused at load, not silently read as a
    // SymIdx whose cardinality happens to agree.
    (let d1Store = fixStore "zarr_orb_depth1"
     (try Directory.Delete(d1Store, true) with _ -> ())
     Directory.CreateDirectory (Path.Combine(d1Store, "W")) |> ignore
     File.WriteAllText(Path.Combine(d1Store, ".zgroup"), "{\"zarr_format\": 2}")
     File.WriteAllText(Path.Combine(d1Store, "W", ".zarray"),
        "{\"zarr_format\": 2, \"shape\": [6], \"chunks\": [6], \"dtype\": \"<f8\", \"compressor\": null, \"fill_value\": 0, \"order\": \"C\", \"filters\": null}")
     File.WriteAllText(Path.Combine(d1Store, "W", ".zattrs"),
        "{\"blade\": {\"spec_version\": 2, \"layout\": \"packed\", \"order\": \"ascending-lex\", \"index_types\": [{\"kind\": \"orbit\", \"levels\": [[2, \"+\"]], \"extent\": 3}]}}")
     File.WriteAllBytes(Path.Combine(d1Store, "W", "0"), Array.zeroCreate (6 * 8))
     let src = sprintf """
import zarr as z

let s = z.load("%s")
let W = s.vars.W |> z.read
"""
                     d1Store
     check "orbit corrupt: a depth-1 orbit head on disk is refused (its cardinality WOULD have matched)"
         (match (try lower src with ex -> Error ex.Message) with
          | Error e -> e.Contains "depth >= 2"
          | Ok _ -> false) "")
    // CSV and NetCDF have no wreath pools: both still refuse, by name.
    for (alias, importLine) in [ ("c", "import csv as c"); ("nc", "import netcdf as nc") ] do
        let src =
            sprintf """
%s

let A = [1.0, 2.0, 3.0]
let g = lambda(x, y) where comm(x, y) -> x * y
let S = method_for(A, A) <@> g |> compute
let h = lambda(p, q) where comm(p, q) -> p + q
let W = method_for(S, S) <@> h |> compute
let w = %s.write("generated_cpp_tests/orb_refuse_out", W)
"""
                    importLine alias
        let outcome =
            try
                match lower src with
                | Error e -> e
                | Ok ir ->
                    let (_, _) = CodeGen.genSelfContainedProgramFromIR ir ($"orb_refuse_{alias}")
                    "emitted (no refusal)"
            with ex -> ex.Message
        check ($"orbit refusal: provider '{alias}' still refuses a wreath write")
            (outcome.Contains "stores no OrbIdx pools") outcome

    // ---------------------------------------------------------------
    // 19. Simplex-blocks: math identities (Phase 0, pure)
    // ---------------------------------------------------------------
    printfn "\n--- simplex-blocks: math identities ---"
    (let roundtrip strict n r =
        let count = if strict then binom n r else binom (n + int64 r - 1L) r
        seq { 0L .. count - 1L }
        |> Seq.forall (fun rank ->
            let c = SimplexBlocks.unrankToCoords strict n r rank
            SimplexBlocks.rankOfCoords strict n c = rank)
     check "rank/unrank roundtrip: sym n=5 r=3 (35 cells)" (roundtrip false 5L 3) ""
     check "rank/unrank roundtrip: antisym n=6 r=3 (20 cells)" (roundtrip true 6L 3) "")
    (let sumCells strict n B r =
        let T = (n + B - 1L) / B
        seq { 0L .. SimplexBlocks.blockCount r T - 1L }
        |> Seq.sumBy (fun b ->
            SimplexBlocks.blockCellCount strict n B (SimplexBlocks.unrankToCoords false T r b))
     check "block cells sum to cardinality: sym n=5 B=2 r=2 (ragged tile)"
         (sumCells false 5L 2L 2 = 15L) (string (sumCells false 5L 2L 2))
     check "block cells sum to cardinality: antisym n=5 B=2 r=2"
         (sumCells true 5L 2L 2 = 10L) (string (sumCells true 5L 2L 2))
     check "block cells sum to cardinality: antisym n=4 B=1 r=2 (diagonal blocks EMPTY)"
         (sumCells true 4L 1L 2 = 6L) (string (sumCells true 4L 1L 2)))
    check "antisym B=1: every repeated-tile block is empty (the diagonal specialness)"
        (seq { 0L .. SimplexBlocks.blockCount 2 4L - 1L }
         |> Seq.forall (fun b ->
             let tiles = SimplexBlocks.unrankToCoords false 4L 2 b
             let cells = SimplexBlocks.blockCellCount true 4L 1L tiles
             if tiles.[0] = tiles.[1] then cells = 0L else cells = 1L)) ""
    check "maxBlockCells bounds every block (sym + antisym, n=7 B=3 r=3)"
        (let T = 3L
         seq { 0L .. SimplexBlocks.blockCount 3 T - 1L }
         |> Seq.forall (fun b ->
             let tiles = SimplexBlocks.unrankToCoords false T 3 b
             SimplexBlocks.blockCellCount false 7L 3L tiles <= SimplexBlocks.maxBlockCells 3 3L
             && SimplexBlocks.blockCellCount true 7L 3L tiles <= SimplexBlocks.maxBlockCells 3 3L)) ""
    check "enumBlockCells: counts match closed form + cells canonical (sym n=5 B=2 r=2)"
        (let T = 3L
         seq { 0L .. SimplexBlocks.blockCount 2 T - 1L }
         |> Seq.forall (fun b ->
             let tiles = SimplexBlocks.unrankToCoords false T 2 b
             let cells = SimplexBlocks.enumBlockCells false 5L 2L tiles |> List.ofSeq
             int64 cells.Length = SimplexBlocks.blockCellCount false 5L 2L tiles
             && cells |> List.forall (fun c -> c.[0] <= c.[1]))) ""
    (let bijective strict n B r order =
        let map = SimplexBlocks.blocksCellMap strict n B r order
        let hits = map |> Array.filter (fun p -> p >= 0)
        let card = if strict then binom n r else binom (n + int64 r - 1L) r
        int64 hits.Length = card && (hits |> Array.sort) = [| 0 .. int card - 1 |]
     check "blocksCellMap: bijection onto the pool (sym n=5 B=2)" (bijective false 5L 2L 2 OrderLex) ""
     check "blocksCellMap: bijection onto the pool (antisym n=4 B=1, empty rows)" (bijective true 4L 1L 2 OrderLex) ""
     check "blocksCellMap: bijection under PATH order (sym n=8 B=2, T=4)" (bijective false 8L 2L 2 OrderPath) "")
    check "pathRows: a permutation (r=2, T=4)"
        (let rows = SimplexBlocks.pathRows 2 4L
         (rows |> Array.sort) = [| 0L .. int64 rows.Length - 1L |]) ""
    check "pathRows: non-power-of-two grid refused"
        (try SimplexBlocks.pathRows 2 3L |> ignore; false
         with ex -> ex.Message.Contains "power-of-two") ""
    (let (phys, pool) = SimplexBlocks.paddingReport false 100L 16L 2
     printfn "  INFO: padding sym n=100 B=16 r=2: physical %d cells vs pool %d (%.1f%% overhead)"
         phys pool (100.0 * float (phys - pool) / float pool))

    // ---------------------------------------------------------------
    // 20. Simplex-blocks: store I/O (Phase 1)
    // ---------------------------------------------------------------
    printfn "\n--- simplex-blocks: store write -> load -> pool roundtrip ---"
    let sbLayout sym strict rank n tile order : BladeLayout =
        let T = (n + tile - 1L) / tile
        { Group = { Sym = sym; Rank = rank; Extent = n; Levels = [] }
          DenseDims = []
          Blocks = Some { Tile = tile; Grid = T; Order = order } }
    let sbPool strict n =
        triOracle strict (int n)
    let sbRoundtrip (name: string) (sym, strict) (n: int64) (tile: int64) order (writer: string -> ZarrWrite.WriteVar list -> unit) =
        let layout = sbLayout sym strict 2 n tile order
        let pool = sbPool strict n
        let root = Path.Combine(scratch, name)
        (try Directory.Delete(root, true) with _ -> ())
        writer root [
            { Name = "C"; DimNames = None; Shape = [int64 pool.Length]; Chunks = [int64 pool.Length]
              FillValue = FillFloat -9.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
              Blade = Some layout } ]
        let store = load root
        match tryFindArray store "C" with
        | None -> check ($"{name}: array found") false ""
        | Some m ->
            check ($"{name}: physical shape is [blockCount, tile^r]")
                (match m.Blade with
                 | Some l -> l.Blocks.IsSome && m.Shape.Length = 2
                 | None -> false)
                (sprintf "%A" m.Shape)
            (match readPackedPool m with
             | Ok { DimLengths = [len]; Payload = ZFloats got } ->
                 check ($"{name}: pool roundtrips exactly through block rows")
                     (len = pool.Length && got = pool) ($"len {len}")
             | Ok d -> check ($"{name}: pool roundtrips") false (sprintf "%A" d.DimLengths)
             | Error e -> check ($"{name}: pool roundtrips") false e)
    sbRoundtrip "sb_sym_ragged_v2" (SymSymmetric, false) 5L 2L OrderLex ZarrWrite.writeStoreV2
    sbRoundtrip "sb_antisym_ragged_v2" (SymAntisymmetric, true) 5L 2L OrderLex ZarrWrite.writeStoreV2
    sbRoundtrip "sb_antisym_B1_v2" (SymAntisymmetric, true) 4L 1L OrderLex ZarrWrite.writeStoreV2
    sbRoundtrip "sb_sym_path_v3" (SymSymmetric, false) 8L 2L OrderPath ZarrWrite.writeStoreV3
    // Differential: same pool via flat "packed" and via blocks reads identically.
    (let pool = sbPool false 5L
     let flatRoot = Path.Combine(scratch, "sb_diff_flat")
     (try Directory.Delete(flatRoot, true) with _ -> ())
     ZarrWrite.writeStoreV2 flatRoot [
        { Name = "C"; DimNames = None; Shape = [int64 pool.Length]; Chunks = [int64 pool.Length]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
          Blade = Some { Group = { Sym = SymSymmetric; Rank = 2; Extent = 5L; Levels = [] }; DenseDims = []; Blocks = None } } ]
     let flatPool =
         match load flatRoot |> fun s -> tryFindArray s "C" |> Option.get |> readPackedPool with
         | Ok { Payload = ZFloats xs } -> xs
         | _ -> [||]
     let blocksPool =
         match load (Path.Combine(scratch, "sb_sym_ragged_v2")) |> fun s -> tryFindArray s "C" |> Option.get |> readPackedPool with
         | Ok { Payload = ZFloats xs } -> xs
         | _ -> [||]
     check "differential: flat-packed and simplex-blocks stores read the SAME pool"
         (flatPool = pool && blocksPool = pool) "")
    // Mixed trailing dims through block rows.
    (let n = 3L
     let trail = 2
     let symCells = [ for i in 0 .. int n - 1 do for j in i .. int n - 1 -> (i, j) ]
     let pool = [| for (i, j) in symCells do for t in 0 .. trail - 1 -> float (100 * (i + 1) + 10 * (j + 1) + t) |]
     let layout = { Group = { Sym = SymSymmetric; Rank = 2; Extent = n; Levels = [] }
                    DenseDims = [int64 trail]
                    Blocks = Some { Tile = 2L; Grid = 2L; Order = OrderLex } }
     let root = Path.Combine(scratch, "sb_mixed")
     (try Directory.Delete(root, true) with _ -> ())
     ZarrWrite.writeStoreV2 root [
        { Name = "D"; DimNames = None; Shape = [int64 symCells.Length; int64 trail]; Chunks = [int64 symCells.Length; int64 trail]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
          Blade = Some layout } ]
     match load root |> fun s -> tryFindArray s "D" |> Option.get |> readPackedPool with
     | Ok { DimLengths = [6; 2]; Payload = ZFloats got } ->
         check "mixed sym x dense through block rows roundtrips" (got = pool) ""
     | Ok d -> check "mixed sym x dense through block rows roundtrips" false (sprintf "%A" d.DimLengths)
     | Error e -> check "mixed sym x dense through block rows roundtrips" false e)
    // Parse rejections.
    (let blocksAttr tile grid extra =
        sprintf """{"blade": {"spec_version": 1, "layout": "packed-blocks", "order": "ascending-lex", "index_types": [{"kind": "sym", "rank": 2, "extent": 5}], "decomposition": {"scheme": "simplex-blocks", "tile": %d, "grid": %d%s}}}""" tile grid extra
     let v2phys shape = sprintf """{"shape":[%s],"chunks":[%s],"dtype":"<f8","compressor":null,"fill_value":0,"order":"C","filters":null}""" shape shape
     check "blocks parse: good store accepted (shape [6,4], n=5 B=2)"
         (match parseArrayMetaV2 "C" "d" (v2phys "6, 4") (Some (blocksAttr 2 3 "")) with
          | Ok m -> (match m.Blade with Some l -> l.Blocks = Some { Tile = 2L; Grid = 3L; Order = OrderLex } | None -> false)
          | Error _ -> false) ""
     check "blocks parse: wrong physical shape LOUD"
         (isError (parseArrayMetaV2 "C" "d" (v2phys "5, 4") (Some (blocksAttr 2 3 ""))) "does not match [blockCount") ""
     check "blocks parse: grid/tile mismatch LOUD"
         (isError (parseArrayMetaV2 "C" "d" (v2phys "6, 4") (Some (blocksAttr 2 4 ""))) "does not match ceil") ""
     check "blocks parse: path order on non-power-of-two grid LOUD"
         (isError (parseArrayMetaV2 "C" "d" (v2phys "6, 4") (Some (blocksAttr 2 3 ", \"block_order\": \"path\""))) "power-of-two") ""
     check "blocks parse: unknown scheme LOUD"
         (isError (parseArrayMetaV2 "C" "d" (v2phys "6, 4") (Some """{"blade": {"spec_version": 1, "layout": "packed-blocks", "index_types": [{"kind": "sym", "rank": 2, "extent": 5}], "decomposition": {"scheme": "hilbert", "tile": 2, "grid": 3}}}""")) "hilbert") "")

    // ---------------------------------------------------------------
    // 21. Simplex-blocks: runtime read e2e (Phase 2)
    // ---------------------------------------------------------------
    // A Blade program reads a BLOCKS store and writes a flat "packed" store;
    // F# compares the pools exactly — pinning the emitted per-block
    // reassembly (tile unrank + branch-free bounds + linearize) end to end.
    printfn "\n--- simplex-blocks: runtime read e2e ---"
    let sbE2E (name: string) (sym, strict) (n: int64) (tile: int64) order =
        let layout = sbLayout sym strict 2 n tile order
        let pool = sbPool strict n
        let inStore = fixStore ($"zarr_sb_{name}")
        let outStore = fixStore ($"zarr_sb_{name}_out")
        let vars : ZarrWrite.WriteVar list = [
            { Name = "C"; DimNames = None; Shape = [int64 pool.Length]; Chunks = [int64 pool.Length]
              FillValue = FillFloat -9.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
              Blade = Some layout } ]
        (try Directory.Delete(inStore, true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, inStore), true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        ZarrWrite.writeStoreV2 inStore vars
        ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, inStore)) vars
        let src = sprintf """
import zarr as z

let s = z.load("%s")
let C = s.vars.C |> z.read
let w = z.write("%s", C)
"""
                      inStore outStore
        try
            match lower src with
            | Ok ir ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_sb_{name}_e2e")
                check ($"sb e2e {name}: emits per-block reassembly")
                    (cppCode.Contains "simplex-blocks" && cppCode.Contains "symmetric::unlinearize") ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_sb_{name}_e2e.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, _) ->
                          check ($"sb e2e {name}: runs (exit 0)") true ""
                          (match readVarData (Path.Combine(e2eDir, outStore)) "C" with
                           | Ok { Payload = ZFloats got } ->
                               check ($"sb e2e {name}: pool through C++ blocks read == oracle pool")
                                   (got = pool) (sprintf "got %A" (Array.truncate 6 got))
                           | Ok _ -> check ($"sb e2e {name}: pool matches") false "not floats"
                           | Error e -> check ($"sb e2e {name}: pool matches") false e)
                      | Ok (code, out) -> check ($"sb e2e {name}: runs (exit 0)") false ($"exit {code}: {out}")
                      | Error e -> check ($"sb e2e {name}: runs (exit 0)") false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP sb e2e %s (compile skipped): %s" name e
                     else check ($"sb e2e {name}: compiles") false e)
            | Error e -> check ($"sb e2e {name}: lowers") false e
        with ex -> check ($"sb e2e {name}") false ex.Message
    sbE2E "sym" (SymSymmetric, false) 5L 2L OrderLex
    sbE2E "antisym" (SymAntisymmetric, true) 5L 2L OrderLex
    sbE2E "path" (SymSymmetric, false) 8L 2L OrderPath

    // ---------------------------------------------------------------
    // 22. Window reads: z.read_window(var, lo, hi) (Phase 3b)
    // ---------------------------------------------------------------
    printfn "\n--- read_window: sub-simplex window reads ---"
    (let n = 6L
     let winPool =
         [| for i in 2 .. 5 do
              for j in i .. 5 ->
                float ((i + 1) * 10 + (j + 1)) |]
     for (label, blocks) in [ ("blocks", Some { Tile = 2L; Grid = 3L; Order = OrderLex }); ("flat", None) ] do
        let layout : BladeLayout = { Group = { Sym = SymSymmetric; Rank = 2; Extent = n; Levels = [] }; DenseDims = []; Blocks = blocks }
        let pool = sbPool false n
        let inStore = fixStore ($"zarr_win_{label}")
        let outStore = fixStore ($"zarr_win_{label}_out")
        let vars : ZarrWrite.WriteVar list = [
            { Name = "C"; DimNames = None; Shape = [int64 pool.Length]; Chunks = [int64 pool.Length]
              FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 pool; OmitChunks = []
              Blade = Some layout } ]
        (try Directory.Delete(inStore, true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, inStore), true) with _ -> ())
        (try Directory.Delete(Path.Combine(e2eDir, outStore), true) with _ -> ())
        ZarrWrite.writeStoreV2 inStore vars
        ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, inStore)) vars
        let src = sprintf """
import zarr as z

let s = z.load("%s")
let W = z.read_window(s.vars.C, 2, 6)
let w = z.write("%s", W)
"""
                      inStore outStore
        try
            match lower src with
            | Ok ir ->
                check ($"window {label}: spec carries Window=(2,6) and the WINDOW type (extent 4)")
                    (ir.Modules.[0].ProviderReads |> Map.exists (fun _ s ->
                        s.Window = Some (2L, 6L)
                        && (match s.VarType.IndexTypes with
                            | lead :: _ -> (match lead.Extent with IRLit (IRLitInt 4L) -> lead.Symmetry = SymSymmetric | _ -> false)
                            | [] -> false)))
                    ""
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"zarr_win_{label}_e2e")
                check ($"window {label}: emits the extraction pass")
                    (cppCode.Contains "window [2, 6) extraction") ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"zarr_win_{label}_e2e.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     (match runExecutable exePath with
                      | Ok (0, _) ->
                          check ($"window {label}: runs (exit 0)") true ""
                          (match readVarData (Path.Combine(e2eDir, outStore)) "W" with
                           | Ok { Payload = ZFloats got } ->
                               check ($"window {label}: window pool == oracle sub-simplex (translated SymIdx<2,4>)")
                                   (got = winPool) (sprintf "got %A" got)
                           | Ok _ -> check ($"window {label}: window pool") false "not floats"
                           | Error e -> check ($"window {label}: window pool") false e)
                          (try
                              let ws = load (Path.Combine(e2eDir, outStore))
                              check ($"window {label}: written window store types as SymIdx<2,4>")
                                  (match tryFindArray ws "W" with
                                   | Some m -> (match m.Blade with Some l -> l.Group.Extent = 4L | None -> false)
                                   | None -> false) ""
                           with ex -> check ($"window {label}: out store loads") false ex.Message)
                      | Ok (code, out) -> check ($"window {label}: runs (exit 0)") false ($"exit {code}: {out}")
                      | Error e -> check ($"window {label}: runs (exit 0)") false e)
                 | Error e ->
                     if isSkipError e then printfn "  SKIP window %s e2e (compile skipped): %s" label e
                     else check ($"window {label}: compiles") false e)
            | Error e -> check ($"window {label}: lowers") false e
        with ex -> check ($"window {label} e2e") false ex.Message)
    check "window: out-of-range bounds rejected at typecheck"
        ((typeErrOf "import zarr as z\nlet s = z.load(\"tests/fixtures/zarr_stores/zarr_win_blocks\")\nlet W = z.read_window(s.vars.C, 2, 7)\n").Contains "bounds")
        (typeErrOf "import zarr as z\nlet s = z.load(\"tests/fixtures/zarr_stores/zarr_win_blocks\")\nlet W = z.read_window(s.vars.C, 2, 7)\n")
    // The IDE payload agrees: a window read is a provider-read binding, so
    // the fast tier's `providerRead` names the store member (the 3-arg
    // read_window arm of Ide.readOperandProvenance).
    (let ideSrc = "import zarr as z\nlet s = z.load(\"tests/fixtures/zarr_stores/zarr_win_blocks\")\nlet W = z.read_window(s.vars.C, 2, 6)\n"
     let (ideJson, ideCode) = Blade.Ide.ideCheckSource "zarr_win_ide.blade" ideSrc
     check "window: ide payload providerRead = {store s, member vars.C}"
         (ideCode = 0 && ideJson.Contains "\"providerRead\":{\"store\":\"s\",\"member\":\"vars.C\"}")
         (if ideJson.Length > 400 then ideJson.Substring(0, 400) else ideJson))
    // The dims/vars split is Zarr's too (isCoordinateArr), so the same
    // wrong-section mistake is available here -- and BL3018 answers it with
    // the sibling accessor, in both directions. The seam is shared with
    // NetCDF and CSV: one field-access resolution site, not three.
    let wrongSection = """
import zarr as z
let store = z.load("tests/fixtures/zarr_stores/zarr_e2e_v2")
let bad = store.vars.x
"""
    check "wrong section: a coordinate under .vars is refused, naming .dims"
        ((typeErrOf wrongSection).Contains "struct store__vars has no field 'x'"
         && (typeErrOf wrongSection).Contains "store.dims.x")
        (typeErrOf wrongSection)
    let wrongSectionRev = """
import zarr as z
let store = z.load("tests/fixtures/zarr_stores/zarr_e2e_v2")
let bad = store.dims.A
"""
    check "wrong section: a data array under .dims is refused, naming .vars"
        ((typeErrOf wrongSectionRev).Contains "struct store__dims has no field 'A'"
         && (typeErrOf wrongSectionRev).Contains "store.vars.A")
        (typeErrOf wrongSectionRev)
    check "window: dense variables rejected with steering"
        ((typeErrOf "import zarr as z\nlet s = z.load(\"tests/fixtures/zarr_stores/zarr_e2e_v2\")\nlet W = z.read_window(s.vars.A, 0, 2)\n").Contains "PACKED")
        (typeErrOf "import zarr as z\nlet s = z.load(\"tests/fixtures/zarr_stores/zarr_e2e_v2\")\nlet W = z.read_window(s.vars.A, 0, 2)\n")

    // ---------------------------------------------------------------
    // 23. MPI-distributed packed read (Phase 3a; needs mpiexec, skips
    // gracefully). Differential: serial build (gate off) vs mpiexec -n 1/3
    // (gate on) — identical stdout, identical written pool, and the mpi
    // build's read is genuinely rank-scoped (Allgatherv restoration).
    // ---------------------------------------------------------------
    printfn "\n--- zarr mpi: distributed simplex-blocks read (differential) ---"
    (let mpiPool = sbPool false 5L
     let mpiLayout : BladeLayout =
         { Group = { Sym = SymSymmetric; Rank = 2; Extent = 5L; Levels = [] }; DenseDims = []
           Blocks = Some { Tile = 2L; Grid = 3L; Order = OrderLex } }
     let aData = [| for i in 0 .. 11 -> float i * 0.5 |]
     let mpiVars : ZarrWrite.WriteVar list = [
        { Name = "A"; DimNames = Some ["x"; "y"]; Shape = [4L; 3L]; Chunks = [4L; 3L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 aData; OmitChunks = []; Blade = None }
        { Name = "C"; DimNames = None; Shape = [int64 mpiPool.Length]; Chunks = [int64 mpiPool.Length]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 mpiPool; OmitChunks = []
          Blade = Some mpiLayout } ]
     let inStore = fixStore "zarr_mpi_in"
     let outStore = fixStore "zarr_mpi_out"
     let outFull = Path.Combine(e2eDir, outStore)
     (try Directory.Delete(inStore, true) with _ -> ())
     (try Directory.Delete(Path.Combine(e2eDir, inStore), true) with _ -> ())
     ZarrWrite.writeStoreV2 inStore mpiVars
     ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, inStore)) mpiVars
     let src = sprintf """
import zarr as z

let s = z.load("%s")
let A = s.vars.A |> z.read
let C = s.vars.C |> z.read
let R = method_for(A) <@> lambda(x) where mpi -> x * 2.0 |> compute
let w = z.write("%s", C)
"""
                   inStore outStore
     // Environment first: g++ + a linkable MS-MPI + mpiexec. With these ruled
     // out up front, a failing serial reference below is a real failure.
     if not zCaps.HasGpp then
         printfn "  SKIP zarr mpi differential: g++ not found"
     elif not Blade.Build.hasMpiLink.Value then
         printfn "  SKIP zarr mpi differential: g++ cannot link MS-MPI (-lmsmpi)"
     elif Blade.Build.mpiexecPath.Value.IsNone then
         printfn "  SKIP zarr mpi differential: mpiexec not found"
     else
     try
        try
            // Serial reference (emit gate OFF: the mpi clause is inert).
            CodeGen.setMpiEmitMode false
            // Keep the REASON the serial reference failed. This used to collapse
            // every error to `None` and print one SKIP line, so a lowering
            // error, a compile failure and a crashing oracle were
            // indistinguishable -- and all three silently deleted every zarr-mpi
            // assertion below while the block still reported green.
            let serialOut : Result<string, string> =
                match lower src with
                | Error e -> Error ($"lower: {e}")
                | Ok ir ->
                    let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_mpi_ref"
                    CodeGen.deployRuntimeHeaders e2eDir
                    let f = Path.Combine(e2eDir, "zarr_mpi_ref.cpp")
                    File.WriteAllText(f, cpp)
                    (match compileCpp f e2eDir with
                     | Error e when isSkipError e -> Error e
                     | Error e -> Error ($"compile: {e}")
                     | Ok exe ->
                         (try Directory.Delete(outFull, true) with _ -> ())
                         (match runExecutable exe with
                          | Ok (0, out) -> Ok out
                          | Ok (code, out) -> Error ($"exit {code}: {(out.Substring(0, min 300 out.Length))}")
                          | Error e -> Error e))
            match serialOut with
            | Error e -> baselineFailed "zarr mpi differential" e
            | Ok refOut ->
                (match readVarData outFull "C" with
                 | Ok { Payload = ZFloats got } ->
                     check "zarr mpi: serial reference writes the oracle pool" (got = mpiPool) ""
                 | _ -> check "zarr mpi: serial reference writes the oracle pool" false "")
                // MPI build (emit gate ON).
                CodeGen.setMpiEmitMode true
                match lower src with
                | Error e -> check "zarr mpi: lowers under the emit gate" false e
                | Ok ir ->
                    let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir "zarr_mpi_e2e"
                    check "zarr mpi: emits the distributed read (rank-scoped + Allgatherv)"
                        (cpp.Contains "distributed simplex-blocks read" && cpp.Contains "MPI_Allgatherv") ""
                    check "zarr mpi: provider write is rank-0 guarded"
                        (cpp.Contains "provider write: rank 0 only") ""
                    CodeGen.deployRuntimeHeaders e2eDir
                    let f = Path.Combine(e2eDir, "zarr_mpi_e2e.cpp")
                    File.WriteAllText(f, cpp)
                    (match compileCpp f e2eDir with
                     | Ok exe ->
                         for ranks in [1; 3] do
                             (try Directory.Delete(outFull, true) with _ -> ())
                             (match runExecutableMpi ranks exe with
                              | Ok (0, out) ->
                                  // Drop the wall-clock timing line — it differs
                                  // by nature between any two runs.
                                  let normalize (s: string) =
                                      s.Split('\n')
                                      |> Array.filter (fun l -> not (l.Contains "completed in"))
                                      |> Array.map (_.TrimEnd())
                                      |> String.concat "\n"
                                      |> fun x -> x.Trim()
                                  // An empty serial reference would make this
                                  // "" = "", a pass with no evidence.
                                  check ($"zarr mpi -n {ranks}: stdout identical to serial")
                                      (not (String.IsNullOrWhiteSpace (normalize refOut))
                                       && normalize out = normalize refOut)
                                      (sprintf "mpi: %s | serial: %s"
                                          (out.Substring(0, min 200 out.Length))
                                          (let r = normalize refOut in if r = "" then "<EMPTY -- nothing to compare>" else r.Substring(0, min 120 r.Length)))
                                  (match readVarData outFull "C" with
                                   | Ok { Payload = ZFloats got } ->
                                       check ($"zarr mpi -n {ranks}: gathered pool == oracle (write from rank 0)")
                                           (got = mpiPool) ""
                                   | _ -> check ($"zarr mpi -n {ranks}: gathered pool == oracle") false "read-back failed")
                              | Ok (code, out) -> check ($"zarr mpi -n {ranks}: runs (exit 0)") false ($"exit {code}: {(out.Substring(0, min 200 out.Length))}")
                              | Error e -> check ($"zarr mpi -n {ranks}: runs (exit 0)") false e)
                     | Error e ->
                         if isSkipError e then printfn "  SKIP zarr mpi e2e (compile skipped): %s" e
                         else check "zarr mpi: compiles" false e)
        finally
            CodeGen.setMpiEmitMode false
     with ex -> check "zarr mpi differential" false ex.Message)

    // ---------------------------------------------------------------
    // 24. Streaming reads (z.stream): fiber reads inlined at the S/T
    // boundary. THE gate is differential: the same program with `.read`
    // (materialize) and `.stream` must produce identical stdout, while the
    // streamed build must show the in-nest fiber reads and NO whole-array
    // materialization.
    // ---------------------------------------------------------------
    printfn "\n--- z.stream: inline fiber reads (differential vs .read) ---"
    (let strmData = [| for i in 0 .. 11 -> float ((i * 7) % 13) + 0.5 |]
     let strmVars : ZarrWrite.WriteVar list = [
        { Name = "A"; DimNames = Some ["s"; "t"]; Shape = [4L; 3L]; Chunks = [2L; 2L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 strmData; OmitChunks = []; Blade = None } ]
     let strmStore = fixStore "zarr_strm"
     (try Directory.Delete(strmStore, true) with _ -> ())
     (try Directory.Delete(Path.Combine(e2eDir, strmStore), true) with _ -> ())
     ZarrWrite.writeStoreV2 strmStore strmVars
     ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, strmStore)) strmVars
     // 2D-site store for the fused joint-symmetry case.
     let strm2Data = [| for i in 0 .. 17 -> float ((i * 5) % 11) + 0.25 |]
     let strm2Vars : ZarrWrite.WriteVar list = [
        { Name = "B"; DimNames = Some ["p"; "q"; "t"]; Shape = [3L; 2L; 3L]; Chunks = [2L; 2L; 2L]
          FillValue = FillFloat 0.0; Data = ZarrWrite.WF64 strm2Data; OmitChunks = []; Blade = None } ]
     let strm2Store = fixStore "zarr_strm2"
     (try Directory.Delete(strm2Store, true) with _ -> ())
     (try Directory.Delete(Path.Combine(e2eDir, strm2Store), true) with _ -> ())
     ZarrWrite.writeStoreV2 strm2Store strm2Vars
     ZarrWrite.writeStoreV2 (Path.Combine(e2eDir, strm2Store)) strm2Vars

     // Compare COMPUTE outputs only: the .read build additionally prints
     // the materialized source array (A/B = [...]), which the streamed
     // build correctly has nothing to print. The timing line differs by
     // nature.
     let normalize (s: string) =
         s.Split('\n')
         |> Array.filter (fun l ->
             not (l.Contains "completed in")
             && not (l.TrimStart().StartsWith "A = [")
             && not (l.TrimStart().StartsWith "B = ["))
         |> Array.map (_.TrimEnd())
         |> String.concat "\n"
         |> fun x -> x.Trim()
     /// Compare the two builds' COMPUTE output, and refuse to call an empty
     /// comparison a match.
     ///
     /// `normalize` deliberately drops the data-bearing `A = [`/`B = [` lines,
     /// because the `.read` build materializes and prints the source array while
     /// the streamed build has nothing there to print -- that is the one
     /// difference the builds are allowed to have. But nothing checked that
     /// ANYTHING survived the filter: if both sides normalized to "", the
     /// assertion `normalize out = normalize refOut` was `"" = ""`, a pass with
     /// zero evidence. This returns false in that case, and additionally
     /// requires the `.read` baseline to have actually PRINTED the source array
     /// it is supposed to materialize -- so the filter is only ever removing
     /// content that was really there.
     let sameCompute (out: string) (refOut: string) : bool * string =
         let a, b = normalize out, normalize refOut
         let refPrintedSource =
             refOut.Split('\n')
             |> Array.exists (fun l ->
                 let t = l.TrimStart()
                 t.StartsWith "A = [" || t.StartsWith "B = [")
         if a = "" || b = "" then
             (false, $"nothing left to compare after normalization (stream={a.Length} chars, read={b.Length} chars); the comparison would be vacuous")
         elif not refPrintedSource then
             (false, ".read baseline never printed the materialized source array (A/B = [...]) -- it did not materialize, so this is not a read-vs-stream differential")
         elif a <> b then (false, $"stream: {a} / read: {b}")
         else (true, "")
     let compileRun (testName: string) (src: string) : Result<string * string, string> =
         match lower src with
         | Error e -> Error ($"lower: {e}")
         | Ok ir ->
             let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir testName
             CodeGen.deployRuntimeHeaders e2eDir
             let f = Path.Combine(e2eDir, testName + ".cpp")
             File.WriteAllText(f, cpp)
             match compileCpp f e2eDir with
             // Preserve the skip marker so baselineFailed can tell a toolchain
             // skip from a genuine compile failure.
             | Error e when isSkipError e -> Error e
             | Error e -> Error ($"compile: {e}")
             | Ok exe ->
                 match runExecutable exe with
                 | Ok (0, out) -> Ok (out, cpp)
                 | Ok (code, out) -> Error ($"exit {code}: {(out.Substring(0, min 300 out.Length))}")
                 | Error e -> Error e
     let differential (label: string) (mkSrc: string -> string) =
         match compileRun ($"strm_{label}_read") (mkSrc "read") with
         | Error e ->
             baselineFailed ($"stream differential {label} (.read baseline)") e
         | Ok (refOut, _) ->
             (match compileRun ($"strm_{label}_stream") (mkSrc "stream") with
              | Error e -> check ($"stream {label}: streamed build runs") false e
              | Ok (out, cpp) ->
                  let (ok, why) = sameCompute out refOut
                  check ($"stream {label}: stdout identical to .read") ok why
                  check ($"stream {label}: fiber buffers + stream prologue emitted")
                      (cpp.Contains "_fb_p" && cpp.Contains "// Stream ") "")

     // (b) cov-like: comm pair over 1D sites, SymIdx<2,4> output.
     differential "cov" (fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.%s
let m2 = method_for(A, A) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>) where comm(x, y) -> prodsum(x, y) / 3.0 |> compute
"""
                                         verb)
     // (c) skew-like: comm triple, SymIdx<3,4> output.
     differential "skew" (fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.%s
let m3 = method_for(A, A, A) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>, z2: Array<Float64 like TimeIdx>) where comm(x, y, z2) -> prodsum(x, y, z2) / 3.0 |> compute
"""
                                         verb)
     // (a) mean-like: arity-1 fiber map (skips with a note if the shape
     // isn't supported by the .read baseline either).
     differential "mean" (fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.%s
let mu = method_for(A) <@> lambda(x: Array<Float64 like TimeIdx>) -> prodsum(x, x) / 3.0 |> compute
"""
                                         verb)
     // (d) 2D sites + time: the user's headline case — joint-symmetric cov
     // over (p, q) sites (fused compound level) with streamed fibers.
     differential "cov2d" (fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm2")
let B = sd.vars.B |> z.%s
let m2 = method_for(B, B) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>) where comm(x, y) -> prodsum(x, y) / 3.0 |> compute
"""
                                         verb)
     // (g) FUSED trees over provider I/O — the <&>/<&!> baseline: a
     // mean<&!>cov tower (staggered depths) and a cov<&>cov soft join,
     // each differential .read vs .stream. The hard join additionally
     // pins CROSS-LEAF FIBER DEDUP: mean's s1-level fiber IS cov's first
     // argument — the wrapper bind must appear exactly once.
     let fusedTower = fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.%s
let (mu, m2) = (method_for(A) <@> lambda(x: Array<Float64 like TimeIdx>) -> prodsum(x, x) / 3.0) <&!> (method_for(A, A) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>) where comm(x, y) -> prodsum(x, y) / 3.0) |> compute
"""
                                       verb
     (match compileRun "strm_fused_read" (fusedTower "read") with
      | Error e -> baselineFailed "fused stream differential (.read baseline)" e
      | Ok (refOut, _) ->
          (match compileRun "strm_fused_stream" (fusedTower "stream") with
           | Error e -> check "stream fused <&!>: streamed build runs" false e
           | Ok (out, cpp) ->
               let (ok, why) = sameCompute out refOut
               check "stream fused <&!>: stdout identical to .read" ok why
               let wrapperBinds =
                   cpp.Split('\n')
                   |> Array.filter (fun l -> l.Contains "= { A_fb_p0, A_fiber_ext }")
                   |> Array.length
               check "stream fused <&!>: shared s1 fiber bound ONCE (cross-leaf dedup)"
                   (wrapperBinds = 1) ($"{wrapperBinds} wrapper binds of A_fb_p0")))
     let fusedSoft = fun verb -> sprintf """
import zarr as z

type TimeIdx = Idx<3>
let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.%s
let (m2a, m2b) = (method_for(A, A) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>) where comm(x, y) -> prodsum(x, y) / 3.0) <&> (method_for(A, A) <@> lambda(x: Array<Float64 like TimeIdx>, y: Array<Float64 like TimeIdx>) where comm(x, y) -> prodsum(x, y) * 2.0) |> compute
"""
                                      verb
     (match compileRun "strm_soft_read" (fusedSoft "read") with
      | Error e -> baselineFailed "soft-fused stream differential (.read baseline)" e
      | Ok (refOut, _) ->
          (match compileRun "strm_soft_stream" (fusedSoft "stream") with
           | Error e -> check "stream fused <&>: streamed build runs" false e
           | Ok (out, _) ->
               let (ok, why) = sameCompute out refOut
               check "stream fused <&>: stdout identical to .read" ok why))

     // (f) elementwise consumption of a streamed source: loud reject.
     (let src = """
import zarr as z

let sd = z.load("tests/fixtures/zarr_stores/zarr_strm")
let A = sd.vars.A |> z.stream
let out = method_for(A) <@> lambda(x) -> x + x |> compute
"""
      match lower src with
      | Ok ir ->
          (try
              CodeGen.genSelfContainedProgramFromIR ir "strm_elem_reject" |> ignore
              check "stream: elementwise consumption rejected loudly" false "codegen succeeded?"
           with ex ->
              check "stream: elementwise consumption rejected loudly"
                  (ex.Message.Contains "not stream-eligible") ex.Message)
      | Error e -> check "stream: elementwise reject case lowers" false e)
     // (e) netcdf streaming differential (needs sample.nc + libnetcdf).
     if File.Exists "tests/fixtures/sample.nc" then
         (try
             Directory.CreateDirectory(Path.Combine(e2eDir, "tests", "fixtures")) |> ignore
             File.Copy("tests/fixtures/sample.nc", Path.Combine(e2eDir, "tests", "fixtures", "sample.nc"), true)
             let ncDiff (label: string) (mkSrc: string -> string) =
                 match compileRun ($"strm_{label}_read") (mkSrc "read") with
                 // A missing libnetcdf shows up as a link failure (g++), or as
                 // BL2007's "cannot load its native library" when the checker's
                 // compile-time metadata read cannot load the DLL -- both are
                 // genuine environment conditions, recognize them as skips;
                 // everything else is a failure. Previously every error printed
                 // SKIP and deleted the differential.
                 | Error e when e.Contains "-lnetcdf" || e.Contains "netcdf.h"
                                || e.Contains "cannot load its native library" ->
                     printfn "  SKIP nc stream differential: libnetcdf not available for g++ (%s)" e
                 | Error e -> baselineFailed "nc stream differential (.read baseline)" e
                 | Ok (refOut, _) ->
                     (match compileRun ($"strm_{label}_stream") (mkSrc "stream") with
                      | Error e -> check ($"stream {label}: streamed build runs") false e
                      | Ok (out, cpp) ->
                          let (ok, why) = sameCompute out refOut
                          check ($"stream {label}: stdout identical to .read") ok why
                          check ($"stream {label}: nc_get_vara fiber reads inlined")
                              (cpp.Contains "nc_get_vara") "")
             ncDiff "nc_cov" (fun verb -> sprintf """
import netcdf as nc

let sample = nc.load("tests/fixtures/sample.nc")
let A = sample.vars.A |> nc.%s
let m2 = method_for(A, A) <@> lambda(x: Array<Float32 like xdim>, y: Array<Float32 like xdim>) where comm(x, y) -> prodsum(x, y) |> compute
"""
                                              verb)
          // The missing-fixture case is already handled by the File.Exists gate
          // below, so an exception in here is a genuine failure (a throwing
          // codegen, a bad copy, ...) -- not something to swallow as a SKIP.
          with ex -> check "nc stream differential" false ex.Message)
     else
         printfn "  SKIP nc stream differential: sample.nc not found")

    // Cleanup the temp scratch (e2e stores stay in generated_cpp_tests
    // beside the .cpp files, like the netcdf fixtures).
    (try Directory.Delete(scratch, true) with _ -> ())

    // ---------------------------------------------------------------
    // Summary
    // ---------------------------------------------------------------
    printFooter "Zarr Provider" [$"{passed} passed"; $"{failed} failed"]
    if failed > 0 then 1 else 0
