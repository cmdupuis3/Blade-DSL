// NetCDF provider tests. Tests against a mock NcFile run always; live-load
// tests need sample.nc + libnetcdf and skip otherwise. Extracted verbatim
// from Main.fs (audit Â§2.3).
module Blade.Tests.NetcdfTests

open System
open Blade
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
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
open Blade.TypedAst
open Blade.TypeCheck
open Blade.Unify
open Blade.TypeEnv
open Blade.Zonk
open Blade.Lowering
open Blade.CodeGen
open Blade.NetcdfProvider
open Blade.Build
open Blade.Tests.TestHarness
open Blade.Tests.Expect

// ============================================================================
// NetCDF Provider Tests
// ============================================================================

let runNetcdfTests () =
    printHeader "NetCDF Provider Tests"
    let mutable passed = 0
    let mutable failed = 0
    
    let check (name: string) (condition: bool) (detail: string) =
        if condition then
            printfn "  PASS: %s" name
            passed <- passed + 1
        else
            printfn "  FAIL: %s â€” %s" name detail
            failed <- failed + 1

    // ------------------------------------------------------------------
    // SKIP-vs-FAIL policy for the live-load blocks below.
    //
    // The rule (already stated in prose further down, next to the compound
    // map): a genuine MISSING-LIBRARY or MISSING-FIXTURE condition is a SKIP,
    // because the environment cannot run the test at all. Everything else -- a
    // lowering error, a codegen exception, a compile failure, a nonzero exit --
    // is a FAILURE.
    //
    // The code did not match that rule. Every live block ended in a bare
    // `| ex -> printfn "  SKIP ...: %s" ex.Message` catch-all, and several
    // routed lowering errors to a printfn as well. Because `failed` is only
    // incremented by `check`, none of those paths could ever fail the block:
    // the assertions downstream of the error simply stopped running and the
    // block still reported all-green. `unexpected` is the honest catch-all.
    // ------------------------------------------------------------------
    let unexpected (what: string) (ex: exn) =
        match ex with
        | :? System.DllNotFoundException ->
            printfn "  SKIP %s: libnetcdf not available" what
        | :? System.IO.FileNotFoundException ->
            printfn "  SKIP %s: sample.nc not found" what
        | _ ->
            check ($"{what}: no unexpected exception") false
                ($"{(ex.GetType().Name)}: {ex.Message}")

    // The string-side twin of `unexpected`'s DllNotFoundException arm: the
    // checker surfaces a typecheck-TIME missing-library condition as BL2007
    // ("provider ... cannot load its native library") through the ordinary
    // Error channel, not as an escaping exception -- so lower/typecheck
    // Error arms need this recognizer to apply the same SKIP policy.
    let nativeLibUnavailable (e: string) = e.Contains "cannot load its native library"

    // ---------------------------------------------------------------
    // Test 1: ncTypeToElemType mapping
    // ---------------------------------------------------------------
    printfn "\n--- Type Code Mapping ---"
    
    check "NC_FLOAT (5) -> ETFloat32"
        (NetcdfProvider.ncTypeToElemType 5 = ETFloat32) ""
    check "NC_DOUBLE (6) -> ETFloat64"
        (NetcdfProvider.ncTypeToElemType 6 = ETFloat64) ""
    check "NC_INT (4) -> ETInt64"
        (NetcdfProvider.ncTypeToElemType 4 = ETInt64) ""
    check "NC_SHORT (3) -> ETInt64"
        (NetcdfProvider.ncTypeToElemType 3 = ETInt64) ""
    check "NC_UBYTE (7) -> ETInt64"
        (NetcdfProvider.ncTypeToElemType 7 = ETInt64) ""
    check "NC_CHAR (2) -> ETInt32"
        (NetcdfProvider.ncTypeToElemType 2 = ETInt32) ""
    
    let unsupportedThrows =
        try NetcdfProvider.ncTypeToElemType 99 |> ignore; false
        with _ -> true
    check "Unsupported type code throws" unsupportedThrows ""

    // ---------------------------------------------------------------
    // Test 2: Module construction from mock NcFile
    // ---------------------------------------------------------------
    printfn "\n--- Module Construction (mock data) ---"

    let mockFile : NetcdfProvider.NcFile = {
        Path = "sample.nc"
        Dims = [
            { Name = "lat"; Length = 180L }
            { Name = "lon"; Length = 360L }
            { Name = "time"; Length = 12L }
        ]
        Vars = [
            { Name = "A"; Dims = [
                { Name = "lat"; Length = 180L }
                { Name = "lon"; Length = 360L }
                { Name = "time"; Length = 12L }
              ]; TypeCode = 6 }  // NC_DOUBLE
        ]
    }

    let builder = IRBuilder()
    let modul = NetcdfProvider.ncFileToModule builder "sample" mockFile None

    // Helper to find structs by name
    let findStruct name (m: IRModule) =
        m.Types |> List.tryPick (function
            | IRTDStruct (n, fields) when n = name -> Some fields
            | _ -> None)

    check "Module name is 'sample'"
        (modul.Name = "sample") ($"got '{modul.Name}'")

    // 3 index types + dims struct + vars struct = 5 type defs
    check "Module has 5 type defs (3 idx + 2 structs)"
        (modul.Types.Length = 5) ($"got {modul.Types.Length}")

    let idxTypeNames =
        modul.Types |> List.choose (function
            | IRTDIndexType (name, _) -> Some name
            | _ -> None)
    
    check "Index type names are lat, lon, time"
        (idxTypeNames = ["lat"; "lon"; "time"])
        (sprintf "got %A" idxTypeNames)

    let latExtent =
        modul.Types |> List.tryPick (function
            | IRTDIndexType ("lat", idx) ->
                match idx.Extent with IRLit (IRLitInt n) -> Some n | _ -> None
            | _ -> None)
    check "lat extent is 180"
        (latExtent = Some 180L) (sprintf "got %A" latExtent)

    // Every dimension becomes a global `using <name> = int64_t;`, so a
    // dimension name that a C library already declares at global scope takes
    // the whole TU down: `time` -- the geoscience default, and the name this
    // very mock uses -- collided with <ctime>'s `time`, and g++ refused with
    // "redeclared as different kind of entity". The Blade-visible index type
    // is still named `time` (asserted above); only the emitted C++ spelling
    // moves, and only for names that actually collide.
    let mockTypeDefLines = genTypeDefs modul
    let mockUsings = mockTypeDefLines |> List.filter (fun l -> l.StartsWith "using")
    check "dim `time` emits a mangled C++ alias, never a bare `using time =`"
        (List.contains "using time_ = int64_t;" mockUsings
         && not (List.contains "using time = int64_t;" mockUsings))
        (sprintf "got %A" mockUsings)
    check "dims colliding with nothing keep their spelling (lat, lon)"
        (List.contains "using lat = int64_t;" mockUsings
         && List.contains "using lon = int64_t;" mockUsings)
        (sprintf "got %A" mockUsings)

    let timeExtent =
        modul.Types |> List.tryPick (function
            | IRTDIndexType ("time", idx) ->
                match idx.Extent with IRLit (IRLitInt n) -> Some n | _ -> None
            | _ -> None)
    check "time extent is 12"
        (timeExtent = Some 12L) (sprintf "got %A" timeExtent)

    // ---------------------------------------------------------------
    // Test 3: Struct structure
    // ---------------------------------------------------------------
    printfn "\n--- Struct Structure ---"

    let dimsFields = findStruct "sample__dims" modul
    check "dims struct exists"
        (dimsFields.IsSome) ""
    check "dims has 3 fields (lat, lon, time)"
        (dimsFields.Value.Length = 3)
        ($"got {(match dimsFields with Some f -> f.Length | None -> 0)}")
    check "dims field names"
        (dimsFields.Value |> List.map fst = ["lat"; "lon"; "time"])
        (sprintf "got %A" (dimsFields.Value |> List.map fst))

    let varsFields = findStruct "sample__vars" modul
    check "vars struct exists"
        (varsFields.IsSome) ""
    check "vars has 1 field (A)"
        (varsFields.Value.Length = 1)
        ($"got {(match varsFields with Some f -> f.Length | None -> 0)}")

    let varAType = varsFields.Value |> List.tryPick (fun (n, t) -> if n = "A" then Some t else None)
    check "vars.A exists" (varAType.IsSome) ""

    match varAType with
    | Some (ArrayElem arr) ->
        check "A element type is Float64"
            (arr.ElemType = IRTScalar ETFloat64) (sprintf "got %A" arr.ElemType)
        check "A has 3 index types"
            (arr.IndexTypes.Length = 3) ($"got {arr.IndexTypes.Length}")
        check "A index types have no tags"
            (arr.IndexTypes |> List.forall (fun i -> i.Tag = None)) ""
        check "A identity is AIDVariable 'A'"
            (arr.Identity = Some (AIDVariable "A")) (sprintf "got %A" arr.Identity)
    | _ ->
        check "A is an array type" false ""

    // ---------------------------------------------------------------
    // Test 3b: Loaded-var typing (registerProviderModule)
    // The type-check seam that makes `let sample = NetCDF.load(...)` type
    // `sample` to a module struct, so `sample.vars.A` resolves to A's real
    // Array type rather than a fresh type var. Exercised with the mock module
    // above; the compile-time file read is bypassed, as in these tests.
    // ---------------------------------------------------------------
    printfn "\n--- Loaded-var typing (registerProviderModule) ---"

    let env0 = emptyEnv ()
    let (envP, moduleTy) = registerProviderModule env0 "sample" modul

    check "sample binds to module struct type"
        (moduleTy = IRTNamed "sample") (sprintf "got %A" moduleTy)

    let fieldOf structName fieldName =
        match lookupTypeDef structName envP with
        | Some (TDIStruct (_, _, fields, _)) ->
            fields |> List.tryPick (fun (n, t) -> if n = fieldName then Some t else None)
        | _ -> None

    check "sample.vars -> sample__vars struct"
        (fieldOf "sample" "vars" = Some (IRTNamed "sample__vars")) (sprintf "got %A" (fieldOf "sample" "vars"))
    check "sample.dims -> sample__dims struct"
        (fieldOf "sample" "dims" = Some (IRTNamed "sample__dims")) (sprintf "got %A" (fieldOf "sample" "dims"))

    // Bare "dims"/"vars" must NOT be registered: they would clobber across
    // stores exactly the way bare dimension names would (see below).
    check "bare 'vars' is not registered globally"
        ((lookupTypeDef "vars" envP).IsNone) "bare name would clobber across stores"
    check "bare 'dims' is not registered globally"
        ((lookupTypeDef "dims" envP).IsNone) "bare name would clobber across stores"

    match fieldOf "sample__vars" "A" with
    | Some (ArrayElem arr) ->
        check "sample.vars.A resolves to a rank-3 array"
            (arr.IndexTypes.Length = 3) ($"got {arr.IndexTypes.Length}")
        check "sample.vars.A element type is Float64"
            (arr.ElemType = IRTScalar ETFloat64) (sprintf "got %A" arr.ElemType)
    | other ->
        check "sample.vars.A resolves to an array type" false (sprintf "got %A" other)

    // The `.index` surface: the axis types the provider derived from the file
    // are registered under `<binding>.index.<dim>`, so an annotation can say
    // `Array<Float64 like sample.index.x>` instead of hand-copying the extent.
    // Qualified only — a bare `x` would clobber across stores.
    let axisOf dim =
        match lookupTypeDef ($"sample.index.{dim}") envP with
        | Some (TDIIndexType (_, idx, _)) -> Some idx
        | _ -> None

    let fileDims =
        modul.Types |> List.choose (function IRTDIndexType (n, idx) -> Some (n, idx) | _ -> None)
    check "provider declares at least one named axis"
        (not fileDims.IsEmpty) ($"got {fileDims.Length}")

    for (dimName, srcIdx) in fileDims do
        match axisOf dimName with
        | Some idx ->
            check ($"sample.index.{dimName} is registered") true ""
            check ($"sample.index.{dimName} keeps the file's extent")
                (idx.Extent = srcIdx.Extent) (sprintf "got %A want %A" idx.Extent srcIdx.Extent)
            // Tag must stay as the provider built it (None): a synthesized tag
            // here would make annotated arrays fail to unify with the store's.
            check ($"sample.index.{dimName} keeps the provider's tag")
                (idx.Tag = srcIdx.Tag) (sprintf "got %A want %A" idx.Tag srcIdx.Tag)
        | None ->
            check ($"sample.index.{dimName} is registered") false "not found"

    // Bare dimension names must NOT leak into the global type namespace.
    for (dimName, _) in fileDims do
        check ($"bare '{dimName}' is not registered globally")
            ((lookupTypeDef dimName envP).IsNone) "bare name would clobber across stores"

    // ---------------------------------------------------------------
    // Test 3b-2: TWO loads in one program (regression — silent miscompile)
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
    // is the ordering that used to clobber.
    // ---------------------------------------------------------------
    printfn "\n--- Two loads in one program (no cross-store clobber) ---"

    let mockFirst : NetcdfProvider.NcFile = {
        Path = "first.nc"
        Dims = [
            { Name = "lat"; Length = 180L }
            { Name = "lon"; Length = 360L }
        ]
        Vars = [
            // Shares the name "A" with the second store, but Float64 / rank 2.
            { Name = "A"; Dims = [
                { Name = "lat"; Length = 180L }
                { Name = "lon"; Length = 360L }
              ]; TypeCode = 6 }  // NC_DOUBLE
            { Name = "only_first"; Dims = [ { Name = "lat"; Length = 180L } ]; TypeCode = 6 }
        ]
    }
    let mockSecond : NetcdfProvider.NcFile = {
        Path = "second.nc"
        Dims = [ { Name = "depth"; Length = 40L } ]
        Vars = [
            // Same name "A", deliberately different: Float32 / rank 1.
            { Name = "A"; Dims = [ { Name = "depth"; Length = 40L } ]; TypeCode = 5 }  // NC_FLOAT
            { Name = "only_second"; Dims = [ { Name = "depth"; Length = 40L } ]; TypeCode = 5 }
        ]
    }

    let twoBuilder = IRBuilder()
    let modFirst = NetcdfProvider.ncFileToModule twoBuilder "first" mockFirst None
    let modSecond = NetcdfProvider.ncFileToModule twoBuilder "second" mockSecond None

    let (env1, _) = registerProviderModule (emptyEnv ()) "first" modFirst
    let (env2, _) = registerProviderModule env1 "second" modSecond

    // Walk the same chain TypeCheck does: binding struct -> section field ->
    // IRTNamed <struct> -> member field. A live lookup at each hop, which is
    // exactly why a clobbered TypeDefs entry retyped earlier use sites.
    let memberOf (binding: string) (section: string) (field: string) : IRType option =
        let fieldsOf structName =
            match lookupTypeDef structName env2 with
            | Some (TDIStruct (_, _, fields, _)) -> Some fields
            | _ -> None
        fieldsOf binding
        |> Option.bind (List.tryPick (fun (n, t) -> if n = section then Some t else None))
        |> Option.bind (function IRTNamed sn -> fieldsOf sn | _ -> None)
        |> Option.bind (List.tryPick (fun (n, t) -> if n = field then Some t else None))

    check "two loads: struct names are namespaced per binding"
        ((findStruct "first__vars" modFirst).IsSome && (findStruct "second__vars" modSecond).IsSome
         && (findStruct "first__dims" modFirst).IsSome && (findStruct "second__dims" modSecond).IsSome)
        (sprintf "first %A second %A"
            (modFirst.Types |> List.choose (function IRTDStruct (n, _) -> Some n | _ -> None))
            (modSecond.Types |> List.choose (function IRTDStruct (n, _) -> Some n | _ -> None)))

    // The core assertion: after the SECOND load registers, the FIRST store's
    // shared-name member still resolves to the FIRST store's type.
    check "two loads: first.vars.A keeps first's type (Float64, rank 2)"
        (match memberOf "first" "vars" "A" with
         | Some (ArrayElem a) -> a.ElemType = IRTScalar ETFloat64 && a.IndexTypes.Length = 2
         | _ -> false)
        (sprintf "got %A" (memberOf "first" "vars" "A"))
    check "two loads: second.vars.A keeps second's type (Float32, rank 1)"
        (match memberOf "second" "vars" "A" with
         | Some (ArrayElem a) -> a.ElemType = IRTScalar ETFloat32 && a.IndexTypes.Length = 1
         | _ -> false)
        (sprintf "got %A" (memberOf "second" "vars" "A"))

    // Non-overlapping members: under the clobber these produced a spurious
    // "field not found" (first) / resolved fine only by luck (second).
    check "two loads: first.vars.only_first resolves"
        ((memberOf "first" "vars" "only_first").IsSome) "clobbered vars struct would lose it"
    check "two loads: second.vars.only_second resolves"
        ((memberOf "second" "vars" "only_second").IsSome) ""
    check "two loads: first.vars has no member of the second store"
        ((memberOf "first" "vars" "only_second").IsNone) "second store's field leaked into first"

    // Same story for dims (the coordinate-array struct).
    check "two loads: first.dims.lat resolves"
        ((memberOf "first" "dims" "lat").IsSome) "clobbered dims struct would lose it"
    check "two loads: second.dims.depth resolves"
        ((memberOf "second" "dims" "depth").IsSome) ""
    check "two loads: first.dims has no member of the second store"
        ((memberOf "first" "dims" "depth").IsNone) "second store's dim leaked into first"

    // ---------------------------------------------------------------
    // Test 3c: load_compound view transform (compoundViewType)
    // Pure type transform -- a bool mask covering all of a variable's dims
    // (matched by index Id) collapses them into one CompoundIdx, i.e. a scalar
    // Compound<T, RANK>. No data read; that happens at |> NetCDF.read.
    // ---------------------------------------------------------------
    printfn "\n--- load_compound view transform (compoundViewType) ---"

    let cBuilder = IRBuilder()
    let cId1 = cBuilder.FreshId()
    let cId2 = cBuilder.FreshId()
    let cId3 = cBuilder.FreshId()
    let mkIdx (id: IRId) (ext: int64) : IRIndexType =
        { Id = id; Rank = 1; Extent = IRLit (IRLitInt ext)
          Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    let cVarArr : IRArrayType =
        { ElemType = IRTScalar ETFloat32
          IndexTypes = [mkIdx cId1 20L; mkIdx cId2 30L; mkIdx cId3 50L]
          IsVirtual = false; Identity = Some (AIDVariable "B") }
    let cMaskArr : IRArrayType =
        { ElemType = IRTScalar ETInt64
          IndexTypes = [mkIdx cId1 20L; mkIdx cId2 30L; mkIdx cId3 50L]
          IsVirtual = false; Identity = Some (AIDVariable "B_mask") }
    let cMaskIR = IRLit IRLitUnit

    match compoundViewType (cBuilder.FreshId()) cVarArr cMaskArr cMaskIR with
    | Ok (ArrayElem arr) ->
        check "load_compound: collapses to a single compound index"
            (arr.IndexTypes.Length = 1) ($"got {arr.IndexTypes.Length}")
        check "load_compound: compound index has rank 3"
            (arr.IndexTypes.[0].Rank = 3) ($"got {arr.IndexTypes.[0].Rank}")
        check "load_compound: tagged __compoundidx"
            (arr.IndexTypes.[0].IxKind = IxKCompound) (sprintf "got %A" arr.IndexTypes.[0].Tag)
        check "load_compound: carries the mask as the compound extent"
            arr.IndexTypes.[0].Extent.IsIRCompoundMask ""
        check "load_compound: element type preserved (Float32)"
            (arr.ElemType = IRTScalar ETFloat32) (sprintf "got %A" arr.ElemType)
    | Ok other -> check "load_compound: result is an array" false (sprintf "got %A" other)
    | Error e -> check "load_compound: all-dims integer mask succeeds" false e

    // A non-integer mask is rejected (a float variable is data, not a presence
    // mask; per-slice-length integer masks are a future RaggedIdx sibling).
    let cFloatMask = { cMaskArr with ElemType = IRTScalar ETFloat64 }
    check "load_compound: rejects a non-integer (float) mask"
        (match compoundViewType (cBuilder.FreshId()) cVarArr cFloatMask cMaskIR with Error _ -> true | Ok _ -> false) ""

    // Partial coverage (mask covers a leading prefix) is SUPPORTED: the masked
    // prefix collapses to a CompoundIdx and the remaining dim stays a regular
    // trailing slot (formalism CompoundIdx<mask>, Idx<...>).
    let cPartialMask = { cMaskArr with IndexTypes = [mkIdx cId1 20L; mkIdx cId2 30L] }
    (match compoundViewType (cBuilder.FreshId()) cVarArr cPartialMask cMaskIR with
     | Ok (ArrayElem parr) ->
         check "load_compound: partial mask -> compound + trailing slot"
             (parr.IndexTypes.Length = 2) ($"got {parr.IndexTypes.Length} slots")
         check "load_compound: partial compound index has rank 2"
             (parr.IndexTypes.[0].Rank = 2 && parr.IndexTypes.[0].IxKind = IxKCompound)
             (sprintf "got rank %d tag %A" parr.IndexTypes.[0].Rank parr.IndexTypes.[0].Tag)
         check "load_compound: trailing dim preserved (3rd var dim)"
             (parr.IndexTypes.[1].Id = cId3) ($"got Id {parr.IndexTypes.[1].Id}")
     | Ok other -> check "load_compound: partial result is an array" false (sprintf "got %A" other)
     | Error e -> check "load_compound: partial (leading-prefix) mask succeeds" false e)

    // A NON-PREFIX mask (skips a dimension / out of order) is rejected -- only a
    // contiguous leading prefix is supported.
    let cReorderedMask = { cMaskArr with IndexTypes = [mkIdx cId1 20L; mkIdx cId3 50L] }
    check "load_compound: rejects a non-prefix mask"
        (match compoundViewType (cBuilder.FreshId()) cVarArr cReorderedMask cMaskIR with Error _ -> true | Ok _ -> false) ""

    // A mask with MORE dimensions than the variable cannot be a leading prefix.
    let cId4 = cBuilder.FreshId()
    let cTooLongMask = { cMaskArr with IndexTypes = cMaskArr.IndexTypes @ [mkIdx cId4 7L] }
    check "load_compound: rejects a mask longer than the variable"
        (match compoundViewType (cBuilder.FreshId()) cVarArr cTooLongMask cMaskIR with Error _ -> true | Ok _ -> false) ""

    // A rank-1 leading mask over a rank-3 variable: one CompoundIdx (rank 1) plus
    // TWO regular trailing slots, preserved in order.
    let cMask1of3 = { cMaskArr with IndexTypes = [mkIdx cId1 20L] }
    (match compoundViewType (cBuilder.FreshId()) cVarArr cMask1of3 cMaskIR with
     | Ok (ArrayElem qarr) ->
         check "load_compound: rank-1 mask over rank-3 -> compound + 2 trailing slots"
             (qarr.IndexTypes.Length = 3) ($"got {qarr.IndexTypes.Length} slots")
         check "load_compound: rank-1 leading compound index has rank 1"
             (qarr.IndexTypes.[0].Rank = 1 && qarr.IndexTypes.[0].IxKind = IxKCompound)
             (sprintf "got rank %d tag %A" qarr.IndexTypes.[0].Rank qarr.IndexTypes.[0].Tag)
         check "load_compound: rank-1 leading preserves trailing dims in order"
             (qarr.IndexTypes.[1].Id = cId2 && qarr.IndexTypes.[2].Id = cId3)
             ($"got Ids {qarr.IndexTypes.[1].Id}, {qarr.IndexTypes.[2].Id}")
     | Ok other -> check "load_compound: rank-1 result is an array" false (sprintf "got %A" other)
     | Error e -> check "load_compound: rank-1 leading mask succeeds" false e)

    // ---------------------------------------------------------------
    // dimsMatch: compound construction matches on shared INDEX-SPACE IDENTITY,
    // not on equal extents. Two arrays that share a NAMED index type carry the
    // same Tag but get FRESH Ids per reference (source-level named types) --
    // these MUST match. Two anonymous arrays of equal shape do NOT share an
    // index space and must be REJECTED (formalism 14.6). This is the fix that
    // lets source-level `compound(dense, mask)` work when the user names the
    // shared index types (the provider path still matches via shared Id).
    // ---------------------------------------------------------------
    let mkNamedIdx (id: IRId) (tag: string) (ext: int64) : IRIndexType =
        { Id = id; Rank = 1; Extent = IRLit (IRLitInt ext)
          Symmetry = SymNone; Tag = Some tag; IxKind = ixKindOfTag (Some tag); Kind = SDimension; Dependencies = [] }
    // Positive: dense and mask share NAMED index types (same tags) but have
    // DISTINCT Ids per reference -- the source-level literal case.
    let nDense : IRArrayType =
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [ mkNamedIdx (cBuilder.FreshId()) "Lat" 2L
                         mkNamedIdx (cBuilder.FreshId()) "Lon" 2L
                         mkNamedIdx (cBuilder.FreshId()) "Depth" 4L ]
          IsVirtual = false; Identity = Some (AIDVariable "dense") }
    let nMask : IRArrayType =
        { ElemType = IRTScalar ETBool
          IndexTypes = [ mkNamedIdx (cBuilder.FreshId()) "Lat" 2L
                         mkNamedIdx (cBuilder.FreshId()) "Lon" 2L ]  // fresh Ids, same tags
          IsVirtual = false; Identity = Some (AIDVariable "mask") }
    (match compoundViewType (cBuilder.FreshId()) nDense nMask cMaskIR with
     | Ok (ArrayElem narr) ->
         check "compound: matches named index types by tag (distinct Ids)"
             (narr.IndexTypes.Length = 2 && narr.IndexTypes.[0].Rank = 2
              && narr.IndexTypes.[0].IxKind = IxKCompound)
             (sprintf "got %d slots, rank %d, tag %A" narr.IndexTypes.Length narr.IndexTypes.[0].Rank narr.IndexTypes.[0].Tag)
         check "compound: preserves the trailing named dim (Depth)"
             (narr.IndexTypes.[1].Tag = Some "Depth") (sprintf "got %A" narr.IndexTypes.[1].Tag)
     | Ok other -> check "compound: named-tag result is an array" false (sprintf "got %A" other)
     | Error e -> check "compound: named index types (same tag) succeed" false e)
    // Negative (14.6): anonymous arrays of EQUAL shape but NO shared identity
    // (distinct Ids, no tags, bare literal extents) must be REJECTED -- a
    // coincidental shape match is not a shared index space.
    let aDense : IRArrayType =
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [ mkIdx (cBuilder.FreshId()) 2L; mkIdx (cBuilder.FreshId()) 2L ]
          IsVirtual = false; Identity = Some (AIDVariable "adense") }
    let aMask : IRArrayType =
        { ElemType = IRTScalar ETBool
          IndexTypes = [ mkIdx (cBuilder.FreshId()) 2L; mkIdx (cBuilder.FreshId()) 2L ]
          IsVirtual = false; Identity = Some (AIDVariable "amask") }
    check "compound: rejects anonymous arrays of equal shape (no shared identity, 14.6)"
        (match compoundViewType (cBuilder.FreshId()) aDense aMask cMaskIR with Error _ -> true | Ok _ -> false) ""

    // buildRawLoopLevels: a CompoundIdx slot is ONE loop axis (it iterates its
    // present cells / cardinality), NOT leadRank dense grid levels. Take the
    // partial compound [compoundIdx<2>, trailing Idx] and confirm it produces 2
    // levels (compound + trailing), not 3. The compound level carries the mask
    // rank in SourceRank for the codegen consumer; the trailing dim stays its
    // own dense level. (Foundation for compound iteration; the compacted bound
    // and compact address are emitted by the codegen consumer in a later step.)
    (match compoundViewType (cBuilder.FreshId()) cVarArr cPartialMask cMaskIR with
     | Ok (ArrayElem compArr) ->
         let rawLevels = buildRawLoopLevels [compArr] (computeSDimsPerArray [compArr])
         check "compound iteration: CompoundIdx slot is one loop axis (not leadRank)"
             (rawLevels.Length = 2) ($"got {rawLevels.Length} levels")
         check "compound iteration: compound level tagged __compoundidx, mask rank in SourceRank"
             (rawLevels.Length = 2 && rawLevels.[0].IndexSpace.Tag = Some "__compoundidx" && rawLevels.[0].IndexSpace.SourceRank = 2)
             ($"got {rawLevels.Length} levels")
         check "compound iteration: trailing dim stays its own dense level"
             (rawLevels.Length = 2 && rawLevels.[1].IndexSpace.Tag = None) ""
     | Ok other -> check "compound iteration: partial compound is an array" false (sprintf "got %A" other)
     | Error e -> check "compound iteration: partial compound builds" false e)

    // genForLoopHeader: a compound axis bounds on the runtime cardinality of the
    // compact index (idx->cardinality), not a dense .extents entry, and carries
    // no triangular subtraction (a compound axis is its own group, no deps).
    let compBinding : LoopIndexBinding =
        { Level = 0; IndexName = "__i0"; Extent = IRCompoundMask (IRLit IRLitUnit)
          ExtentArrayRef = "data"; ExtentDimRef = 0
          BoundDependencies = []; StrictOffset = 0; FusedRank = None; IsParallel = false
          State = SCNeither; Elements = [] }
    let compoundNames = CodeGen.compoundArrayNamesOf [compBinding]
    let compHdr = CodeGen.genForLoopHeader compoundNames compBinding
    check "compound iteration: loop header bounds on idx->cardinality"
        (compHdr.Contains "__i0 < data.idx->cardinality;") ($"got: {compHdr}")

    // A compound array's trailing dim: a literal extent keeps its literal bound
    // (IRLit arm); a NON-literal extent bounds on trailing_stride, since the
    // Compound layout has no dense .extents to index.
    let litTrailB : LoopIndexBinding =
        { Level = 1; IndexName = "__i1"; Extent = IRLit (IRLitInt 50L)
          ExtentArrayRef = "data"; ExtentDimRef = 1
          BoundDependencies = []; StrictOffset = 0; FusedRank = None; IsParallel = false
          State = SCNeither; Elements = [] }
    let dynTrailB : LoopIndexBinding =
        { litTrailB with Extent = IRVar (999, IRTScalar ETInt64) }
    let litHdr = CodeGen.genForLoopHeader compoundNames litTrailB
    check "compound iteration: literal trailing extent keeps its literal bound"
        (litHdr.Contains "__i1 < 50;") ($"got: {litHdr}")
    let dynHdr = CodeGen.genForLoopHeader compoundNames dynTrailB
    check "compound iteration: non-literal trailing extent bounds on trailing_stride"
        (dynHdr.Contains "__i1 < data.trailing_stride;") ($"got: {dynHdr}")

    // genElementBindingNew compound access: the compound axis peels against the
    // compact .data buffer. All-dims (no trailing, ArrayRank 1) -> scalar leaf
    // data[r]; partial (trailing, ArrayRank 2) -> trailing row base
    // (data + r*trailing_stride), which the dense peel then indexes.
    let mkCompElem (arrRank: int) : ElementBinding =
        { ArrayPosition = 0; ArrayName = "data"; ParamName = "x"; ParamVarId = -1
          DimIndex = 0; RankComponent = 0; ArrayElemType = IRTScalar ETFloat64
          ArrayRank = arrRank; Virtual = RealArray; SlotTag = None }
    let (leafCode, _) = CodeGen.genElementBindingNew compBinding (mkCompElem 1) "data"
    check "compound iteration: all-dims access peels the compact leaf data[r]"
        (leafCode.Contains "= data.data[__i0];") ($"got: {leafCode}")
    let (rowCode, _) = CodeGen.genElementBindingNew compBinding (mkCompElem 2) "data"
    check "compound iteration: partial access peels the trailing row base"
        (rowCode.Contains "data.data + __i0 * data.trailing_stride") ($"got: {rowCode}")

    // compoundOutputSubscript: the compact WRITE address mirrors the read.
    // all-dims (compound binding only) -> .data[r]; partial (one trailing) ->
    // .data[r * out.trailing_stride + t].
    let trailB : LoopIndexBinding =
        { Level = 1; IndexName = "__i1"; Extent = IRLit (IRLitInt 4)
          ExtentArrayRef = "out"; ExtentDimRef = 1
          BoundDependencies = []; StrictOffset = 0; FusedRank = None; IsParallel = false
          State = SCNeither; Elements = [] }
    check "compound iteration: all-dims output subscript is .data[r]"
        (CodeGen.compoundOutputSubscript [compBinding] "out" = ".data[__i0]")
        (CodeGen.compoundOutputSubscript [compBinding] "out")
    check "compound iteration: partial output subscript is .data[r*stride + t]"
        (CodeGen.compoundOutputSubscript [compBinding; trailB] "out" = ".data[__i0 * out.trailing_stride + __i1]")
        (CodeGen.compoundOutputSubscript [compBinding; trailB] "out")

    // Stage 1 -- multi-index range<I1, ..., In> surface. A comma-separated range
    // parses to one virtual array spanning all listed index types (which uncurry
    // into nested loop levels in IR). A single-index range stays a 1-element list
    // (no behavior change). Validated at the parser, independent of lowering.
    let rangeArityOf (src: string) : int option =
        match parseProgram src with
        | Ok program ->
            program.Modules.[0].Decls
            |> List.map (_.Value)
            |> List.tryPick (fun d ->
                match d with
                | DeclLet b -> (match b.Value.Kind with ExprKind.ExprRange tys -> Some (List.length tys) | _ -> None)
                | _ -> None)
        | Error _ -> None
    check "range surface: single-index range parses to a 1-element list"
        (rangeArityOf "let r = range<Idx<5>>" = Some 1)
        (sprintf "got %A" (rangeArityOf "let r = range<Idx<5>>"))
    check "range surface: multi-index range<I1, I2> parses to a 2-element list"
        (rangeArityOf "let r = range<Idx<3>, Idx<4>>" = Some 2)
        (sprintf "got %A" (rangeArityOf "let r = range<Idx<3>, Idx<4>>"))
    check "range surface: range<I1, I2, I3> parses to a 3-element list"
        (rangeArityOf "let r = range<Idx<2>, Idx<3>, Idx<4>>" = Some 3)
        (sprintf "got %A" (rangeArityOf "let r = range<Idx<2>, Idx<3>, Idx<4>>"))

    // Baseline: single-index range<I> as a method_for driver, end to end. range<I>
    // emits the index, the kernel maps it. This is the virtual-array-driver +
    // index-param path that multi-index range (stage 1b) extends, and it had no
    // prior coverage -- validate it before changing the param binding.
    printfn "\n--- range<I> driver map (baseline) ---"
    (try
        match lower "let r = method_for(range<Idx<5>>) <@> lambda(i) -> i + 1 |> compute\n" with
        | Ok ir ->
            check "range<I> map: lowers" true ""
            let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir "range_map_baseline"
            let outDir = "./generated_cpp_tests"
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            CodeGen.deployRuntimeHeaders outDir
            let cppFile = Path.Combine(outDir, "range_map_baseline.cpp")
            File.WriteAllText(cppFile, cpp)
            (match compileCpp cppFile outDir with
             | Ok exe ->
                 check "range<I> map: compiles" true ""
                 (match runExecutable exe with
                  | Ok (0, _) -> check "range<I> map: runs (exit 0)" true ""
                  | Ok (c, o) -> check "range<I> map: runs (exit 0)" false ($"exit {c}: {o}")
                  | Error e -> check "range<I> map: runs (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP range<I> map compile: %s" e
                 else check "range<I> map: compiles" false e)
        | Error e -> check "range<I> map: lowers" false e
     with ex -> unexpected "range<I> map" ex)

    // Stage 1b: multi-index range<I1, I2> as a driver. The kernel receives one
    // param per index-type slot (i, j) via per-slot param binding; the two slots
    // uncurry into nested loops. Validates the param binding end to end.
    printfn "\n--- range<I1, I2> driver map (stage 1b) ---"
    (try
        match lower "let r = method_for(range<Idx<3>, Idx<4>>) <@> lambda(i, j) -> i + j |> compute\n" with
        | Ok ir ->
            check "range<I1,I2> map: lowers (per-slot params bind)" true ""
            let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir "range2_map"
            let outDir = "./generated_cpp_tests"
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            CodeGen.deployRuntimeHeaders outDir
            let cppFile = Path.Combine(outDir, "range2_map.cpp")
            File.WriteAllText(cppFile, cpp)
            (match compileCpp cppFile outDir with
             | Ok exe ->
                 check "range<I1,I2> map: compiles" true ""
                 (match runExecutable exe with
                  | Ok (0, _) -> check "range<I1,I2> map: runs (exit 0)" true ""
                  | Ok (c, o) -> check "range<I1,I2> map: runs (exit 0)" false ($"exit {c}: {o}")
                  | Error e -> check "range<I1,I2> map: runs (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP range2 compile: %s" e
                 else check "range<I1,I2> map: compiles" false e)
        | Error e -> check "range<I1,I2> map: lowers (per-slot params bind)" false e
     with ex -> unexpected "range<I1,I2> map" ex)

    // Stage 2 probe (multi-rank, no compound/unhash): range<SymIdx<2,N>> is one
    // index type of RANK 2, so by the rank rule the kernel gets two params (i, j)
    // -- the two triangular positions, which are loop vars (no unhash needed).
    // This exercises the rank-slot param binding for a multi-rank index type.
    //
    // Skip-tolerant ONLY for the not-yet-built parts: a lowering or codegen gap
    // for a symmetric range output is a known frontier, so those stay SKIPs. But
    // a program that COMPILED and then exited nonzero is not a gap -- it is a
    // wrong program, and it used to be reported as `SKIP ... run (exit %d)`,
    // which is the one outcome this probe should never absorb. Likewise a
    // failure to launch the binary at all.
    printfn "\n--- range<SymIdx<2,N>> driver (stage 2: multi-rank slots) ---"
    (try
        match lower "let s = method_for(range<SymIdx<2, 5>>) <@> lambda(i, j) -> i + j |> compute\n" with
        | Ok ir ->
            let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir "range_sym_probe"
            let outDir = "./generated_cpp_tests"
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            CodeGen.deployRuntimeHeaders outDir
            let f = Path.Combine(outDir, "range_sym_probe.cpp")
            File.WriteAllText(f, cpp)
            (match compileCpp f outDir with
             | Ok exe ->
                 (match runExecutable exe with
                  | Ok (0, _) -> check "range<SymIdx<2,N>> map: runs (multi-rank params bind)" true ""
                  | Ok (c, o) ->
                      check "range<SymIdx<2,N>> map: runs (multi-rank params bind)" false
                          ($"compiled, then exited {c}: {o}")
                  | Error e ->
                      check "range<SymIdx<2,N>> map: runs (multi-rank params bind)" false
                          ($"compiled, but could not be run: {e}"))
             | Error e when isSkipError e -> printfn "  SKIP range<SymIdx> compile: %s" e
             // A codegen gap for a symmetric range output is a known frontier;
             // keep it a SKIP, but say so explicitly rather than by omission.
             | Error e -> printfn "  SKIP range<SymIdx> compile (known codegen frontier): %s" e)
        | Error e -> printfn "  SKIP range<SymIdx> lower (known lowering frontier): %s" e
     with ex -> unexpected "range<SymIdx<2,N>>" ex)

    // ---------------------------------------------------------------
    // Test 4: Index type sharing within a module
    // ---------------------------------------------------------------
    printfn "\n--- Index Type Sharing ---"
    
    let mockFile2 : NetcdfProvider.NcFile = {
        Path = "multi.nc"
        Dims = [
            { Name = "lat"; Length = 180L }
            { Name = "lon"; Length = 360L }
        ]
        Vars = [
            { Name = "temperature"; Dims = [
                { Name = "lat"; Length = 180L }
                { Name = "lon"; Length = 360L }
              ]; TypeCode = 6 }
            { Name = "pressure"; Dims = [
                { Name = "lat"; Length = 180L }
                { Name = "lon"; Length = 360L }
              ]; TypeCode = 5 }  // NC_FLOAT
        ]
    }
    
    let builder2 = IRBuilder()
    let modul2 = NetcdfProvider.ncFileToModule builder2 "climate" mockFile2 None
    let vars2 = findStruct "climate__vars" modul2
    
    check "vars has 2 fields" (vars2.Value.Length = 2) ""
    
    // Both variables should reference the same IRIndexType (same Id)
    let tempIdxIds =
        match vars2.Value |> List.tryPick (fun (n,t) -> if n = "temperature" then Some t else None) with
        | Some (ArrayElem a) -> a.IndexTypes |> List.map (_.Id)
        | _ -> []
    let pressIdxIds =
        match vars2.Value |> List.tryPick (fun (n,t) -> if n = "pressure" then Some t else None) with
        | Some (ArrayElem a) -> a.IndexTypes |> List.map (_.Id)
        | _ -> []
    
    check "temperature and pressure share same lat index Id"
        (tempIdxIds.Length >= 1 && pressIdxIds.Length >= 1
         && tempIdxIds.[0] = pressIdxIds.[0]) ""
    
    check "temperature and pressure share same lon index Id"
        (tempIdxIds.Length >= 2 && pressIdxIds.Length >= 2
         && tempIdxIds.[1] = pressIdxIds.[1]) ""

    check "temperature is Float64, pressure is Float32"
        (match vars2.Value.[0] |> snd, vars2.Value.[1] |> snd with
         | ArrayElem a1, ArrayElem a2 ->
             a1.ElemType = IRTScalar ETFloat64 && a2.ElemType = IRTScalar ETFloat32
         | _ -> false) ""

    // ---------------------------------------------------------------
    // Test 5: External dim map (schema extensibility)
    // ---------------------------------------------------------------
    printfn "\n--- External Dim Map (schema hook) ---"
    
    let schemaBuilder = IRBuilder()
    let sharedLat = {
        Id = schemaBuilder.FreshId()
        Rank = 1
        Extent = IRLit (IRLitInt 180L)
        Symmetry = SymNone
        Tag = None; IxKind = IxKPlain
        Kind = SDimension
        Dependencies = []
    }
    let sharedLon = {
        Id = schemaBuilder.FreshId()
        Rank = 1
        Extent = IRLit (IRLitInt 360L)
        Symmetry = SymNone
        Tag = None; IxKind = IxKPlain
        Kind = SDimension
        Dependencies = []
    }
    let externalMap = Map.ofList [("lat", sharedLat); ("lon", sharedLon)]
    
    let modul3 = NetcdfProvider.ncFileToModule schemaBuilder "file1" mockFile2 (Some externalMap)
    let modul4 = NetcdfProvider.ncFileToModule schemaBuilder "file2" mockFile2 (Some externalMap)
    
    // With external map, no IRTDIndexType defs are generated
    let idx3 = modul3.Types |> List.choose (function IRTDIndexType _ -> Some () | _ -> None)
    check "External map: no IRTDIndexType defs generated"
        (idx3.IsEmpty) ($"got {idx3.Length}")
    
    // Both modules' vars should reference the shared lat/lon Ids
    let vars3 = findStruct "file1__vars" modul3
    let vars4 = findStruct "file2__vars" modul4
    check "External map: both modules share same lat Id"
        (match vars3, vars4 with
         | Some f3, Some f4 ->
             match f3.[0] |> snd, f4.[0] |> snd with
             | ArrayElem a1, ArrayElem a2 ->
                 a1.IndexTypes.[0].Id = sharedLat.Id
                 && a2.IndexTypes.[0].Id = sharedLat.Id
             | _ -> false
         | _ -> false) ""

    // ---------------------------------------------------------------
    // Test 6: C++ codegen helpers
    // ---------------------------------------------------------------
    printfn "\n--- C++ Code Generation ---"
    
    let dimNames = NetcdfProvider.CppNetcdf.dimNamesFromModule modul
    check "dimNamesFromModule returns [lat; lon; time]"
        (dimNames = ["lat"; "lon"; "time"]) (sprintf "got %A" dimNames)
    
    match varAType with
    | Some (ArrayElem arrType) ->
        let readCode = NetcdfProvider.CppNetcdf.genReadVar "sample.nc" "A" "A" arrType
        check "genReadVar produces nc_open call"
            (readCode |> List.exists (fun s -> s.Contains "nc_open")) ""
        check "genReadVar produces nc_get_var_double"
            (readCode |> List.exists (fun s -> s.Contains "nc_get_var_double")) ""
        check "genReadVar produces nc_close"
            (readCode |> List.exists (fun s -> s.Contains "nc_close")) ""
        
        let writeCode = NetcdfProvider.CppNetcdf.genWriteVar "out.nc" "A" "A" arrType dimNames
        check "genWriteVar produces nc_create call"
            (writeCode |> List.exists (fun s -> s.Contains "nc_create")) ""
        check "genWriteVar uses dimension names from module"
            (writeCode |> List.exists (fun s -> s.Contains "\"lat\"")
             && writeCode |> List.exists (fun s -> s.Contains "\"lon\"")
             && writeCode |> List.exists (fun s -> s.Contains "\"time\"")) ""
    | _ -> ()

    // Dimension NAMES as `tryProviderWrite` derives them (the other half of
    // the genWriteVar check above, which is handed its names ready-made).
    // A named index type used to arrive here as "dim0": the derivation looked
    // the slot up by `IRIndexType.Id` against the module's IRTDIndexType
    // defs, and `lowerIndexType` stamps a FRESH Id per use, so the match never
    // fired and every user-declared axis name was dropped on the floor.
    // Lowering-level (not codegen): no libnetcdf, no sample.nc.
    printfn "\n--- write dimension names (lowering) ---"
    let writeSpecDimNames (source: string) : Result<string list, string> =
        lower source
        |> Result.bind (fun ir ->
            match ir.Modules.[0].ProviderWrites |> Map.toList with
            | [ (_, spec) ] -> Ok spec.DimNames
            | specs -> Error $"expected exactly one write spec, got {specs.Length}")
    let namedAxisWrite = """
import netcdf as nc
type LatIdx = Idx<2>
type LonIdx = Idx<3>
let A: Array<Float64 like LatIdx, LonIdx> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
let _ = nc.write("out.nc", A)
"""
    (let name = "write dims: named index types reach the store's dimension names"
     match writeSpecDimNames namedAxisWrite with
     | Ok names -> check name (names = ["LatIdx"; "LonIdx"]) (sprintf "got %A" names)
     | Error e -> check name false e)
    // Anonymous axes have no name to carry: dim<i> stands, as before.
    let anonAxisWrite = """
import netcdf as nc
let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
let _ = nc.write("out.nc", A)
"""
    (let name = "write dims: anonymous axes still synthesize dim<i>"
     match writeSpecDimNames anonAxisWrite with
     | Ok names -> check name (names = ["dim0"; "dim1"]) (sprintf "got %A" names)
     | Error e -> check name false e)
    // One index type in TWO slots is one dimension used twice, but the writers
    // emit one definition per slot -- `nc_def_dim` would fail NC_ENAMEINUSE on
    // the repeat. A repeated tag therefore names no slot.
    let repeatedAxisWrite = """
import netcdf as nc
type SqIdx = Idx<2>
let A: Array<Float64 like SqIdx, SqIdx> = [[1.0, 2.0], [3.0, 4.0]]
let _ = nc.write("out.nc", A)
"""
    (let name = "write dims: a repeated index type falls back rather than colliding"
     match writeSpecDimNames repeatedAxisWrite with
     | Ok names -> check name (names = ["dim0"; "dim1"]) (sprintf "got %A" names)
     | Error e -> check name false e)

    // genReadCompoundVar: load_compound's materializer. Reads the dense var and
    // the integer mask, converts nonzero -> bool, builds compound_index_t, and
    // scatters into a compact buffer (verified against the real cpp/ runtime).
    let compoundReadBuilder = IRBuilder()
    let crDim id n : IRIndexType =
        { Id = id; Rank = 1; Extent = IRLit (IRLitInt n)
          Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    let crVarArr : IRArrayType =
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [crDim (compoundReadBuilder.FreshId()) 2L; crDim (compoundReadBuilder.FreshId()) 3L]
          IsVirtual = false; Identity = Some (AIDVariable "B") }
    let crMaskArr : IRArrayType =
        { crVarArr with ElemType = IRTScalar ETInt64; Identity = Some (AIDVariable "B_mask") }
    let compoundReadCode =
        NetcdfProvider.CppNetcdf.genReadCompoundVar "f.nc" "B" "B_mask" "B" crVarArr crMaskArr
    let crHas (sub: string) = compoundReadCode |> List.exists (fun s -> s.Contains sub)
    check "genReadCompoundVar reads the dense var (nc_get_var_double)"
        (crHas "nc_get_var_double") ""
    check "genReadCompoundVar reads the integer mask (nc_get_var_longlong)"
        (crHas "nc_get_var_longlong") ""
    check "genReadCompoundVar converts mask nonzero -> bool"
        (crHas "std::vector<bool>" && crHas "!= 0") ""
    check "genReadCompoundVar builds a rank-2 compound_index_t"
        (crHas "compound_index_t<2>") ""
    check "genReadCompoundVar allocates compact sized by cardinality"
        (crHas "cardinality") ""
    check "genReadCompoundVar bundles a Compound wrapper"
        (crHas "nested_array_utilities::Compound<double, 2>") ""

    // Partial: a [2,3] mask over a [2,3,4] variable. The leading [2,3] become a
    // rank-2 CompoundIdx; the trailing dim (4) folds into a runtime stride that
    // the scatter copies as whole blocks and that is passed into Compound.
    let crVarArr3 : IRArrayType =
        { crVarArr with IndexTypes = crVarArr.IndexTypes @ [crDim (compoundReadBuilder.FreshId()) 4L] }
    let compoundReadCode3 =
        NetcdfProvider.CppNetcdf.genReadCompoundVar "f.nc" "B" "B_mask" "B" crVarArr3 crMaskArr
    let cr3Has (sub: string) = compoundReadCode3 |> List.exists (fun s -> s.Contains sub)
    check "genReadCompoundVar(partial) compound index is rank 2 (the mask rank)"
        (cr3Has "compound_index_t<2>") ""
    check "genReadCompoundVar(partial) computes a trailing stride and grid"
        (cr3Has "_trail =" && cr3Has "_grid =") ""
    check "genReadCompoundVar(partial) scatters whole trailing blocks"
        (cr3Has "_r * B_trail + B_t" && cr3Has "_c * B_trail + B_t") ""
    check "genReadCompoundVar(partial) passes trailing_stride into Compound"
        (cr3Has "B_idx, B_trail }") ""

    // Element-type coverage: a Float32 variable reads via nc_get_var_float and
    // bundles Compound<float, ...>.
    let crVarF32 = { crVarArr with ElemType = IRTScalar ETFloat32 }
    let f32Code = NetcdfProvider.CppNetcdf.genReadCompoundVar "f.nc" "B" "B_mask" "B" crVarF32 crMaskArr
    let f32Has (s: string) = f32Code |> List.exists (fun l -> l.Contains s)
    check "genReadCompoundVar(Float32 var) reads via nc_get_var_float" (f32Has "nc_get_var_float") ""
    check "genReadCompoundVar(Float32 var) bundles Compound<float" (f32Has "nested_array_utilities::Compound<float,") ""

    // Mask-type coverage: an Int32 mask reads via nc_get_var_int into an int buffer.
    let crMaskI32 = { crMaskArr with ElemType = IRTScalar ETInt32 }
    let i32Code = NetcdfProvider.CppNetcdf.genReadCompoundVar "f.nc" "B" "B_mask" "B" crVarArr crMaskI32
    let i32Has (s: string) = i32Code |> List.exists (fun l -> l.Contains s)
    check "genReadCompoundVar(Int32 mask) reads mask via nc_get_var_int" (i32Has "nc_get_var_int(") ""
    check "genReadCompoundVar(Int32 mask) allocates an int mask buffer" (i32Has "int* B_maskraw") ""

    // Rank-1 leading mask: a 1-D mask over a 2-D variable -> compound_index_t<1>,
    // the single leading dim as the grid, the trailing dim folded into a stride.
    // (Runtime behaviour sandbox-verified separately: size 8, correct blocks.)
    let crVar2 =
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [crDim (compoundReadBuilder.FreshId()) 3L; crDim (compoundReadBuilder.FreshId()) 4L]
          IsVirtual = false; Identity = Some (AIDVariable "C") }
    let crMask1 =
        { ElemType = IRTScalar ETInt64
          IndexTypes = [crDim (compoundReadBuilder.FreshId()) 3L]
          IsVirtual = false; Identity = Some (AIDVariable "C_mask") }
    let r1Code = NetcdfProvider.CppNetcdf.genReadCompoundVar "f.nc" "C" "C_mask" "C" crVar2 crMask1
    let r1Has (s: string) = r1Code |> List.exists (fun l -> l.Contains s)
    check "genReadCompoundVar(rank-1 leading) builds compound_index_t<1>" (r1Has "compound_index_t<1>") ""
    check "genReadCompoundVar(rank-1 leading) bundles Compound<double, 1>" (r1Has "nested_array_utilities::Compound<double, 1>") ""

    // ---------------------------------------------------------------
    // Test 7: Live load (requires libnetcdf + sample.nc)
    // ---------------------------------------------------------------
    printfn "\n--- Live Load (sample.nc) ---"
    
    try
        let liveFile = NetcdfProvider.load "tests/fixtures/sample.nc"
        printfn "  Loaded '%s': %d dims, %d vars" liveFile.Path liveFile.Dims.Length liveFile.Vars.Length
        
        for dim in liveFile.Dims do
            printfn "    dim %-12s length=%d" dim.Name dim.Length
        
        let hasA = liveFile.Vars |> List.exists (fun v -> v.Name = "A")
        check "sample.nc contains variable A" hasA ""
        
        if hasA then
            let liveBuilder = IRBuilder()
            let liveModule = NetcdfProvider.ncFileToModule liveBuilder "sample" liveFile None
            
            let liveDimsFields = findStruct "sample__dims" liveModule
            let liveVarsFields = findStruct "sample__vars" liveModule
            
            check "Live dims struct exists"
                (liveDimsFields.IsSome) ""
            check "Live vars struct exists"
                (liveVarsFields.IsSome) ""
            check "Live vars has field for A"
                (liveVarsFields.Value |> List.exists (fun (n, _) -> n = "A")) ""
            
            printfn "\n  Module IR:"
            printfn "    module %s" liveModule.Name
            let names = indexNameMap liveModule
            for td in liveModule.Types do
                match td with
                | IRTDIndexType (name, idx) ->
                    let ext = match idx.Extent with IRLit (IRLitInt n) -> string n | _ -> "?"
                    printfn "      type %s = Idx<%s>" name ext
                | IRTDStruct (name, fields) ->
                    printfn "      struct %s = {" name
                    for (fname, ftype) in fields do
                        printfn "        %s: %s" fname (ppIRTypeIn names ftype)
                    printfn "      }"
                | _ -> ()
    with ex -> unexpected "live load" ex

    // ---------------------------------------------------------------
    // Test 8: Blade program with import and provider load
    // ---------------------------------------------------------------
    printfn "\n--- Blade Program Import (sample.nc) ---"

    let bladeSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
"""
    
    // Test parse
    match parseProgram bladeSource with
    | Ok program ->
        check "Parse succeeds" true ""
        let decls = program.Modules.[0].Decls |> List.map (_.Value)
        
        check "First decl is DeclImport"
            decls.[0].IsDeclImport
            (sprintf "got %A" decls.[0])
        
        check "Import has correct qualified name"
            (match decls.[0] with
             | DeclImport (["netcdf"], ImportQualified (Some "NetCDF")) -> true
             | _ -> false)
            (sprintf "got %A" decls.[0])

        check "Second decl is DeclLet"
            decls.[1].IsDeclLet
            (sprintf "got %A" decls.[1])

        // Test lowering (requires sample.nc + libnetcdf)
        try
            match lower bladeSource with
            | Ok ir ->
                check "Lower succeeds" true ""
                let modul = ir.Modules.[0]
                let names = indexNameMap modul
                
                printfn "\n  Lowered module: %s" modul.Name
                printfn "  Types: %d" modul.Types.Length
                for td in modul.Types do
                    match td with
                    | IRTDIndexType (name, idx) ->
                        let ext = match idx.Extent with IRLit (IRLitInt n) -> string n | _ -> "?"
                        printfn "    type %s = Idx<%s>" name ext
                    | IRTDStruct (name, fields) ->
                        printfn "    struct %s = {" name
                        for (fname, ftype) in fields do
                            printfn "      %s: %s" fname (ppIRTypeIn names ftype)
                        printfn "    }"
                    | _ -> ()

                // Verify types were produced
                let idxTypes = modul.Types |> List.choose (function IRTDIndexType (n, _) -> Some n | _ -> None)
                check "Provider produced index types"
                    (idxTypes.Length >= 3) (sprintf "got %A" idxTypes)

                // Structs are namespaced by the receiving binding (`let sample = ...`).
                let hasVarsStruct = modul.Types |> List.exists (function IRTDStruct ("sample__vars", _) -> true | _ -> false)
                check "Provider produced vars struct" hasVarsStruct ""

                let hasDimsStruct = modul.Types |> List.exists (function IRTDStruct ("sample__dims", _) -> true | _ -> false)
                check "Provider produced dims struct" hasDimsStruct ""

                // Verify vars struct has field A
                let varAExists =
                    modul.Types |> List.exists (function
                        | IRTDStruct ("sample__vars", fields) ->
                            fields |> List.exists (fun (n, _) -> n = "A")
                        | _ -> false)
                check "vars struct has field A" varAExists ""

            | Error e when nativeLibUnavailable e ->
                printfn "  SKIP Lower succeeds: %s" e
            | Error e ->
                printfn "  Lower error: %s" e
                check "Lower succeeds" false e
        with ex -> unexpected "lower" ex

    | Error e ->
        check "Parse succeeds" false ($"{e.Line}:{e.Col} {e.Message}")

    // ---------------------------------------------------------------
    // load_compound |> NetCDF.read: full lowering + codegen wiring (slice 2b).
    // Needs sample.nc (with a data variable B and an integer mask B_mask, the
    // mask covering a leading prefix of B's dims) + libnetcdf; skips gracefully
    // otherwise. When the fixture is present this asserts that lowering recorded
    // ProviderReads (tryCompoundRead fired and recovered path/var/mask) and that
    // codegen emitted the compound reader from it (the genBinding intercept) --
    // exercising tryCompoundRead -> ProviderReads carrier -> genReadCompoundVar
    // end to end, which the isolated unit tests above do not.
    // ---------------------------------------------------------------
    printfn "\n--- load_compound |> NetCDF.read (sample.nc) ---"
    let lcSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let data = NetCDF.load_compound(sample.vars.B, sample.vars.B_mask) |> NetCDF.read
"""
    try
        match lower lcSource with
        | Ok ir ->
            let modul = ir.Modules.[0]
            check "load_compound|>read: ProviderReads populated"
                (not (Map.isEmpty modul.ProviderReads))
                ($"got {modul.ProviderReads.Count} entries")
            (match modul.ProviderReads |> Map.toList |> List.tryHead |> Option.map snd with
             | Some spec ->
                 check "load_compound|>read: recovered var name B"
                     (spec.VarName = "B") ($"got '{spec.VarName}'")
                 check "load_compound|>read: recovered mask name B_mask"
                     (spec.MaskName = Some "B_mask") (sprintf "got %A" spec.MaskName)
             | None -> ())
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "load_compound_e2e"
            check "load_compound|>read: codegen emits compound_index_t"
                (cppCode.Contains "compound_index_t<") ""
            check "load_compound|>read: codegen emits int->bool mask conversion"
                (cppCode.Contains "_maskvec" && cppCode.Contains "!= 0") ""
            check "load_compound|>read: codegen bundles a Compound wrapper"
                (cppCode.Contains "nested_array_utilities::Compound<") ""

            // End-to-end compile + link + run, in-harness. compileCpp now links
            // libnetcdf when the .cpp includes <netcdf.h>. The run's cwd is the
            // exe's own directory, so place the fixture there before running;
            // exit 0 means nc_open found the data and the var + mask were read
            // and the compound materialized without crashing.
            let lcOutDir = "./generated_cpp_tests"
            if not (Directory.Exists lcOutDir) then Directory.CreateDirectory lcOutDir |> ignore
            CodeGen.deployRuntimeHeaders lcOutDir
            let lcCppFile = Path.Combine(lcOutDir, "load_compound_e2e.cpp")
            File.WriteAllText(lcCppFile, cppCode)
            (match compileCpp lcCppFile lcOutDir with
             | Ok exePath ->
                 check "load_compound e2e: compiles and links libnetcdf" true ""
                 Directory.CreateDirectory(Path.Combine(lcOutDir, "tests", "fixtures")) |> ignore
                 File.Copy("tests/fixtures/sample.nc", Path.Combine(lcOutDir, "tests", "fixtures", "sample.nc"), true)
                 (match runExecutable exePath with
                  | Ok (0, _) -> check "load_compound e2e: runs to completion (exit 0)" true ""
                  | Ok (code, out) -> check "load_compound e2e: runs to completion (exit 0)" false ($"exit {code}: {out}")
                  | Error e -> check "load_compound e2e: runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP load_compound e2e (compile skipped): %s" e
                 else check "load_compound e2e: compiles and links libnetcdf" false e)
        | Error e ->
            // A lowering error is a FAILURE, not a skip -- except the BL2007
            // missing-LIBRARY condition, which is `unexpected`'s SKIP policy
            // arriving through the Error channel. The fixture-gap excuse
            // in the old message is not a lowering condition: a missing sample.nc
            // raises FileNotFoundException (handled below), and a sample.nc
            // lacking B/B_mask is a broken fixture the block must report, since
            // every assertion above depends on it.
            if nativeLibUnavailable e then printfn "  SKIP load_compound|>read: %s" e
            else check "load_compound|>read: lowers" false ($"lower error: {e}")
    with ex -> unexpected "load_compound|>read" ex

    // ---------------------------------------------------------------
    // Unary map over a compound: method_for(data) <@> (x -> x+x) |> compute.
    // The output inherits data's CompoundIdx (shared mask): the map iterates the
    // present cells (cardinality) x trailing dims, reads the compact buffer, and
    // writes a fresh compact buffer that SHARES data's idx + trailing_stride.
    // Float-safe kernel (x+x, not x*2.0) since B is Float32 (-Werror=float-conversion).
    // This is a SEPARATE block so it cannot regress the read-only e2e above.
    // Value correctness is covered by the sandbox; here we confirm the path
    // lowers, the compound-map codegen fires, and the program compiles + runs.
    // A lowering failure is a FAILURE of this block (it used to "route to the
    // SKIP arm", which is exactly the accounting hole: the separate-block
    // isolation is what makes it safe to fail here, not a reason to hide it).
    printfn "\n--- compound map: method_for(data) <@> (x -> x+x) (sample.nc) ---"
    let lcMapSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let data = NetCDF.load_compound(sample.vars.B, sample.vars.B_mask) |> NetCDF.read
let out = method_for(data) <@> lambda(x) -> x + x |> compute
"""
    try
        match lower lcMapSource with
        | Ok ir ->
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "compound_map_e2e"
            check "compound map: output alloc sizes by cardinality + shares the input idx"
                (cppCode.Contains "data.idx->cardinality * data.trailing_stride") ""
            check "compound map: writes the compact output subscript (r*stride + t)"
                (cppCode.Contains ".trailing_stride + __") ""
            let lcOutDir = "./generated_cpp_tests"
            if not (Directory.Exists lcOutDir) then Directory.CreateDirectory lcOutDir |> ignore
            CodeGen.deployRuntimeHeaders lcOutDir
            let mapCppFile = Path.Combine(lcOutDir, "compound_map_e2e.cpp")
            File.WriteAllText(mapCppFile, cppCode)
            (match compileCpp mapCppFile lcOutDir with
             | Ok exePath ->
                 check "compound map e2e: compiles and links libnetcdf" true ""
                 Directory.CreateDirectory(Path.Combine(lcOutDir, "tests", "fixtures")) |> ignore
                 File.Copy("tests/fixtures/sample.nc", Path.Combine(lcOutDir, "tests", "fixtures", "sample.nc"), true)
                 (match runExecutable exePath with
                  | Ok (0, _) -> check "compound map e2e: runs to completion (exit 0)" true ""
                  | Ok (code, runOut) -> check "compound map e2e: runs to completion (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "compound map e2e: runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP compound map e2e (compile skipped): %s" e
                 else check "compound map e2e: compiles and links libnetcdf" false e)
        | Error e when nativeLibUnavailable e ->
            printfn "  SKIP compound map: %s" e
        | Error e ->
            check "compound map: lowers (method_for over a compound)" false
                ($"lower error: {e}")
    with ex -> unexpected "compound map" ex

    // ---------------------------------------------------------------
    // Dense provider read: method_for(sample.vars.A |> NetCDF.read) <@> (x -> x+x).
    // The dense analog of the compound path -- exercises tryPlainRead (a maskless
    // ProviderReadSpec) -> the genBinding ProviderReads intercept -> genReadVar,
    // which now materializes a nested Array (allocate + flat->nested copy) that a
    // method_for can index. A is Float32 in sample.nc, so the kernel is float-safe
    // (x + x, not x * 2.0, for -Werror=float-conversion). Skips gracefully without
    // libnetcdf / sample.nc, like the compound tests above.
    //
    // Beyond exit code, this block asserts the actual OUTPUT VALUES against
    // ground truth read via the F# libnetcdf binding, and that a missing .nc
    // at runtime exits nonzero with a NetCDF error instead of printing the
    // uninitialized buffer.
    // ---------------------------------------------------------------
    printfn "\n--- dense read: method_for(sample.vars.A |> NetCDF.read) <@> (x -> x+x) (sample.nc) ---"
    let denseReadSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let A = sample.vars.A |> NetCDF.read
let out = method_for(A) <@> lambda(x) -> x + x |> compute
"""
    try
        match lower denseReadSource with
        | Ok ir ->
            check "dense read: ProviderReads has a maskless spec for A"
                (let modul = ir.Modules.[0]
                 modul.ProviderReads |> Map.exists (fun _ spec -> spec.VarName = "A" && spec.MaskName = None))
                "expected a maskless ProviderRead for A"
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "dense_read_e2e"
            check "dense read: codegen emits genReadVar (nc_open + nc_get_var_float)"
                (cppCode.Contains "nc_open" && cppCode.Contains "nc_get_var_float") ""
            check "dense read: codegen materializes a nested Array (allocate + promote)"
                (cppCode.Contains "allocate<typename promote<float, 3>::type") ""
            let drOutDir = "./generated_cpp_tests"
            if not (Directory.Exists drOutDir) then Directory.CreateDirectory drOutDir |> ignore
            CodeGen.deployRuntimeHeaders drOutDir
            let drCppFile = Path.Combine(drOutDir, "dense_read_e2e.cpp")
            File.WriteAllText(drCppFile, cppCode)
            (match compileCpp drCppFile drOutDir with
             | Ok exePath ->
                 check "dense read e2e: compiles and links libnetcdf" true ""
                 Directory.CreateDirectory(Path.Combine(drOutDir, "tests", "fixtures")) |> ignore
                 File.Copy("tests/fixtures/sample.nc", Path.Combine(drOutDir, "tests", "fixtures", "sample.nc"), true)
                 (match runExecutable exePath with
                  | Ok (0, runOut) ->
                      check "dense read e2e: runs to completion (exit 0)" true ""
                      // Value assertion: the exit code alone cannot catch a bad read
                      // (a silently failed nc_open used to hand the copy loop an
                      // uninitialized buffer -- denormal heap garbage -- and still
                      // exit 0). Ground truth comes from libnetcdf itself via the F#
                      // binding (the compile-time read path), so the fixture can be
                      // regenerated freely without touching pinned values here.
                      (match NetcdfProvider.readVarData "tests/fixtures/sample.nc" "A" with
                       | Ok { Payload = NetcdfProvider.NcFloats truth } ->
                           let expected = truth |> Array.map (fun x -> x + x)  // kernel is x + x
                           // Shape-tolerant flatten (TestHarness). `out` is
                           // rank 3 and prints FLAT today, so this is a no-op
                           // flatten -- used anyway so a printer-shape change
                           // (rank-2 nesting took six zarr tests down) cannot
                           // rot this site.
                           (match tryParsePrintedFloats "out" runOut with
                            | None ->
                                check "dense read e2e: out values match libnetcdf ground truth (2*A)" false
                                    "no parseable 'out = [...]' line in program output"
                            | Some parsed ->
                                if parsed.Length <> expected.Length then
                                    check "dense read e2e: out values match libnetcdf ground truth (2*A)" false
                                        ($"expected {expected.Length} values, got {parsed.Length}")
                                else
                                    // Float32 data printed at precision 15 round-trips exactly and
                                    // x+x is exact in fp, so the tolerance only guards the parse.
                                    let mutable firstBad = -1
                                    for i in 0 .. expected.Length - 1 do
                                        if firstBad < 0 && abs (parsed.[i] - expected.[i]) > 1e-6 * max 1.0 (abs expected.[i]) then
                                            firstBad <- i
                                    check "dense read e2e: out values match libnetcdf ground truth (2*A)"
                                        (firstBad < 0)
                                        (if firstBad < 0 then ""
                                         else sprintf "first mismatch at flat index %d: expected %.9g, got %.9g"
                                                  firstBad expected.[firstBad] parsed.[firstBad]))
                       | Ok _ ->
                           check "dense read e2e: out values match libnetcdf ground truth (2*A)" false
                               "A did not read back as floats"
                       | Error e ->
                           check "dense read e2e: out values match libnetcdf ground truth (2*A)" false
                               ($"ground-truth read failed: {e}"))
                      // A missing .nc at RUNTIME must fail loudly, not print garbage:
                      // the same exe run from a fresh dir without sample.nc has to exit
                      // nonzero with a NetCDF error on stderr (netcdf.dll resolves via
                      // PATH, so the exe still launches from the bare directory).
                      let missingDir = Path.Combine(Path.GetTempPath(), "blade_nc_missing_" + Guid.NewGuid().ToString("N"))
                      Directory.CreateDirectory missingDir |> ignore
                      (try
                          let exeCopy = Path.Combine(missingDir, Path.GetFileName exePath)
                          File.Copy(exePath, exeCopy, true)
                          (match runExecutable exeCopy with
                           | Ok (code, missOut) ->
                               check "dense read e2e: missing sample.nc at runtime fails loudly (nonzero exit + NetCDF error)"
                                   (code <> 0 && missOut.Contains "NetCDF error")
                                   ($"exit {code}: {(missOut.Substring(0, min 200 missOut.Length))}")
                           | Error e ->
                               check "dense read e2e: missing sample.nc at runtime fails loudly (nonzero exit + NetCDF error)" false e)
                       finally
                          try Directory.Delete(missingDir, true) with _ -> ())
                  | Ok (code, runOut) -> check "dense read e2e: runs to completion (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "dense read e2e: runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP dense read e2e (compile skipped): %s" e
                 else check "dense read e2e: compiles and links libnetcdf" false e)
        | Error e when nativeLibUnavailable e ->
            printfn "  SKIP dense read: %s" e
        | Error e ->
            check "dense read: lowers (plain provider var read)" false ($"lower error: {e}")
    with ex -> unexpected "dense read" ex

    // ---------------------------------------------------------------
    // Shape guard (BL8012): the dense reader's baked extents vs the file at
    // RUN time. Lowering bakes A as xdim x ydim x zdim = 20 x 30 x 50 from
    // sample.nc; the exe must refuse -- before nc_get_var writes anything --
    // when the file it opens at run time declares different dimensions, and
    // must still run clean against the unchanged fixture (no false positive).
    // The mismatched files are generated with ncgen, from the same install
    // as libnetcdf (NETCDF_DIR\bin, else PATH); without ncgen the negative
    // halves SKIP. Two mismatches: a longer leading dimension (the overrun
    // case) and a lower rank (the garbage-read case).
    // ---------------------------------------------------------------
    printfn "\n--- shape guard: dense read refuses a file whose dimensions changed (BL8012) ---"
    let shapeGuardSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let A = sample.vars.A |> NetCDF.read
let out = method_for(A) <@> lambda(x) -> x + x |> compute
"""
    /// Run ncgen on a CDL text, producing `outNc`. None = ncgen unavailable
    /// (SKIP); Some (Error e) = ncgen ran and failed (FAIL: the CDL is ours).
    let tryNcgen (cdl: string) (outNc: string) : Result<unit, string> option =
        let candidates =
            [ match Environment.GetEnvironmentVariable "NETCDF_DIR" with
              | null | "" -> ()
              | d -> yield Path.Combine(d, "bin", "ncgen.exe")
              yield "ncgen" ]
        let cdlPath = Path.ChangeExtension(outNc, ".cdl")
        File.WriteAllText(cdlPath, cdl)
        let tryOne (exe: string) : Result<unit, string> option =
            try
                let psi = ProcessStartInfo(exe, $"-o \"{outNc}\" \"{cdlPath}\"")
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                psi.UseShellExecute <- false
                psi.CreateNoWindow <- true
                use p = Process.Start(psi)
                let err = p.StandardError.ReadToEnd()
                p.WaitForExit()
                if p.ExitCode = 0 && File.Exists outNc then Some (Ok ())
                else Some (Error $"ncgen exit {p.ExitCode}: {err}")
            with
            | :? System.ComponentModel.Win32Exception -> None   // not found: try the next
            | ex -> Some (Error ex.Message)
        candidates |> List.tryPick tryOne
    try
        match lower shapeGuardSource with
        | Ok ir ->
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "shape_guard_e2e"
            let iRank = cppCode.IndexOf "nc_inq_varndims"
            let iRead = cppCode.IndexOf "nc_get_var_float"
            check "shape guard: codegen queries rank + dimension lengths BEFORE nc_get_var (BL8012)"
                (iRank >= 0 && iRead > iRank && cppCode.Contains "nc_inq_dimlen" && cppCode.Contains "BL8012")
                ($"varndims at {iRank}, get_var at {iRead}")
            let sgOutDir = "./generated_cpp_tests"
            if not (Directory.Exists sgOutDir) then Directory.CreateDirectory sgOutDir |> ignore
            CodeGen.deployRuntimeHeaders sgOutDir
            let sgCppFile = Path.Combine(sgOutDir, "shape_guard_e2e.cpp")
            File.WriteAllText(sgCppFile, cppCode)
            (match compileCpp sgCppFile sgOutDir with
             | Ok exePath ->
                 check "shape guard e2e: compiles and links libnetcdf" true ""
                 Directory.CreateDirectory(Path.Combine(sgOutDir, "tests", "fixtures")) |> ignore
                 File.Copy("tests/fixtures/sample.nc", Path.Combine(sgOutDir, "tests", "fixtures", "sample.nc"), true)
                 (match runExecutable exePath with
                  | Ok (0, _) -> check "shape guard e2e: the unchanged fixture still reads (exit 0, no false positive)" true ""
                  | Ok (code, runOut) ->
                      check "shape guard e2e: the unchanged fixture still reads (exit 0, no false positive)" false
                          ($"exit {code}: {(runOut.Substring(0, min 300 runOut.Length))}")
                  | Error e -> check "shape guard e2e: the unchanged fixture still reads (exit 0, no false positive)" false e)
                 // Negative halves: the same exe against a regenerated sample.nc.
                 let negative (label: string) (cdl: string) (wantSubstring: string) =
                     let badDir = Path.Combine(Path.GetTempPath(), "blade_nc_shape_" + Guid.NewGuid().ToString("N"))
                     Directory.CreateDirectory(Path.Combine(badDir, "tests", "fixtures")) |> ignore
                     try
                         let badNc = Path.Combine(badDir, "tests", "fixtures", "sample.nc")
                         match tryNcgen cdl badNc with
                         | None -> printfn "  SKIP shape guard e2e (%s): ncgen not found (NETCDF_DIR\\bin or PATH)" label
                         | Some (Error e) -> check $"shape guard e2e ({label}): ncgen builds the mismatched fixture" false e
                         | Some (Ok ()) ->
                             let exeCopy = Path.Combine(badDir, Path.GetFileName exePath)
                             File.Copy(exePath, exeCopy, true)
                             (match runExecutable exeCopy with
                              | Ok (code, badOut) ->
                                  let name = $"shape guard e2e ({label}): aborts with BL8012 before reading, prints no data"
                                  check name
                                      (code <> 0
                                       && badOut.Contains "error[BL8012]"
                                       && badOut.Contains "provider shape mismatch"
                                       && badOut.Contains wantSubstring
                                       && not (badOut.Contains "out = "))
                                      ($"exit {code}: {(badOut.Substring(0, min 400 badOut.Length))}")
                              | Error e -> check $"shape guard e2e ({label}): aborts with BL8012 before reading, prints no data" false e)
                     finally
                         try Directory.Delete(badDir, true) with _ -> ()
                 // xdim 20 -> 21: A grows by 1500 floats past the baked buffer.
                 negative "longer leading dim"
                     "netcdf sample {\ndimensions:\n zdim = 50 ;\n ydim = 30 ;\n xdim = 21 ;\nvariables:\n float A(xdim, ydim, zdim) ;\n}\n"
                     "dimension 0 of length 21"
                 // A loses its trailing axis: rank 2 against a rank-3 type.
                 negative "lower rank"
                     "netcdf sample {\ndimensions:\n ydim = 30 ;\n xdim = 20 ;\nvariables:\n float A(xdim, ydim) ;\n}\n"
                     "has rank 2"
             | Error e ->
                 if isSkipError e then printfn "  SKIP shape guard e2e (compile skipped): %s" e
                 else check "shape guard e2e: compiles and links libnetcdf" false e)
        | Error e when nativeLibUnavailable e ->
            printfn "  SKIP shape guard: %s" e
        | Error e ->
            check "shape guard: lowers (plain provider var read)" false ($"lower error: {e}")
    with ex -> unexpected "shape guard" ex

    // ---------------------------------------------------------------
    // Shape guard, INTERPRETER lane: Run.materializeProviderRead is the twin
    // of CppNetcdf.ncShapeGuard and must raise BL8012 on the same
    // disagreement. The interpreter opens the file named in the IR at run
    // time, so the block lowers against a PRIVATE copy of sample.nc, checks
    // the copy still interprets, regenerates the copy with ncgen (a longer
    // leading dim), and interprets the same IR again. No g++ involved.
    // ---------------------------------------------------------------
    printfn "\n--- shape guard, interpreter lane: BL8012 from materializeProviderRead ---"
    let itDir = Path.Combine(Path.GetTempPath(), "blade_nc_shape_interp_" + Guid.NewGuid().ToString("N"))
    try
        try
            Directory.CreateDirectory itDir |> ignore
            let itNc = Path.Combine(itDir, "sample.nc")
            File.Copy("tests/fixtures/sample.nc", itNc, true)
            let itSource =
                "import netcdf as NetCDF\n\n"
                + "let sample = NetCDF.load(\"" + itNc.Replace('\\', '/') + "\")\n"
                + "let A = sample.vars.A |> NetCDF.read\n"
                + "let out = method_for(A) <@> lambda(x) -> x + x |> compute\n"
            match lower itSource with
            | Ok ir ->
                let run () = Blade.Interp.Run.runProgram ir "shape_guard_interp" Blade.Interp.Value.defaultLimits
                let good = run ()
                check "shape guard interp: the unchanged copy interprets (exit 0, no false positive)"
                    (good.ExitCode = 0 && good.Stdout.Contains "out = ")
                    ($"exit {good.ExitCode}: {(good.Stderr.Substring(0, min 300 good.Stderr.Length))}")
                (match tryNcgen
                        "netcdf sample {\ndimensions:\n zdim = 50 ;\n ydim = 30 ;\n xdim = 21 ;\nvariables:\n float A(xdim, ydim, zdim) ;\n}\n"
                        itNc with
                 | None -> printfn "  SKIP shape guard interp (negative): ncgen not found (NETCDF_DIR\\bin or PATH)"
                 | Some (Error e) -> check "shape guard interp: ncgen rebuilds the private copy" false e
                 | Some (Ok ()) ->
                     let bad = run ()
                     check "shape guard interp: the regenerated copy (longer leading dim) raises BL8012, prints no data"
                         (bad.ExitCode <> 0
                          && bad.Stderr.Contains "BL8012"
                          && bad.Stderr.Contains "length 21"
                          && not (bad.Stdout.Contains "out = "))
                         ($"exit {bad.ExitCode}: {(bad.Stderr.Substring(0, min 400 bad.Stderr.Length))}"))
            | Error e when nativeLibUnavailable e -> printfn "  SKIP shape guard interp: %s" e
            | Error e -> check "shape guard interp: lowers (plain provider var read)" false ($"lower error: {e}")
        with ex -> unexpected "shape guard interp" ex
    finally
        try Directory.Delete(itDir, true) with _ -> ()

    // ---------------------------------------------------------------
    // Destination passing (plan-fortran-killer-2.md section 4, gate 1): a
    // dense provider write hands libnetcdf the array's own pool instead of
    // flattening into a fresh buffer. Emission pin: the write prologue
    // aliases `pool_base(...)` and allocates/frees no `_flat` buffer.
    // End to end: write a 2 x 3 literal, read it back through the F#
    // binding, compare every value -- the aliasing is only right if DFS pool
    // order really is the store's row-major order.
    // ---------------------------------------------------------------
    printfn "\n--- destination passing: nc.write hands the pool to libnetcdf (no flatten copy) ---"
    let dpSource = """
import netcdf as nc
type RowIdx = Idx<2>
type ColIdx = Idx<3>
let A: Array<Float64 like RowIdx, ColIdx> = [[1.5, 2.5, 3.5], [4.5, 5.5, 6.5]]
let _ = nc.write("dp_out.nc", A)
"""
    try
        match lower dpSource with
        | Ok ir ->
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "dest_passing_write"
            check "destination passing: the dense write aliases pool_base and allocates no flat copy"
                (cppCode.Contains "_flat = pool_base(" && not (cppCode.Contains "_flat = new "))
                (if cppCode.Contains "_flat = new " then "a `_flat = new` allocation survives" else "no pool_base alias emitted")
            let dpOutDir = "./generated_cpp_tests"
            if not (Directory.Exists dpOutDir) then Directory.CreateDirectory dpOutDir |> ignore
            CodeGen.deployRuntimeHeaders dpOutDir
            let dpCppFile = Path.Combine(dpOutDir, "dest_passing_write.cpp")
            File.WriteAllText(dpCppFile, cppCode)
            (match compileCpp dpCppFile dpOutDir with
             | Ok exePath ->
                 check "destination passing e2e: compiles and links libnetcdf" true ""
                 let outNc = Path.Combine(Path.GetDirectoryName exePath, "dp_out.nc")
                 (try File.Delete outNc with _ -> ())
                 (match runExecutable exePath with
                  | Ok (0, _) ->
                      check "destination passing e2e: the write runs to completion (exit 0)" true ""
                      (match NetcdfProvider.readVarData outNc "A" with
                       | Ok { DimLengths = dims; Payload = NetcdfProvider.NcFloats got } ->
                           let want = [| 1.5; 2.5; 3.5; 4.5; 5.5; 6.5 |]
                           check "destination passing e2e: the file holds the array in row-major order, every value intact"
                               (dims = [2; 3] && got = want)
                               (sprintf "dims %A, values %A" dims got)
                       | Ok other -> check "destination passing e2e: the file holds the array in row-major order, every value intact" false (sprintf "unexpected payload %A" other.DimLengths)
                       | Error e -> check "destination passing e2e: the file holds the array in row-major order, every value intact" false e)
                  | Ok (code, runOut) -> check "destination passing e2e: the write runs to completion (exit 0)" false ($"exit {code}: {(runOut.Substring(0, min 300 runOut.Length))}")
                  | Error e -> check "destination passing e2e: the write runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP destination passing e2e (compile skipped): %s" e
                 else check "destination passing e2e: compiles and links libnetcdf" false e)
        | Error e when nativeLibUnavailable e -> printfn "  SKIP destination passing: %s" e
        | Error e -> check "destination passing: lowers (nc.write of a dense literal)" false ($"lower error: {e}")
    with ex -> unexpected "destination passing" ex

    // ---------------------------------------------------------------
    // fill_random builtin (general codegen, hermetic -- no NetCDF): a random-fill
    // array constructor whose shape comes from the annotation. Exercises the
    // TExprFillRandom -> RandomInits -> genBinding path (allocate<> + the runtime
    // fill_random), then a method_for map over the result. Placed here to reuse
    // the compile+run harness; it needs neither libnetcdf nor sample.nc, so a
    // lower error is a real failure, not a skip.
    // ---------------------------------------------------------------
    printfn "\n--- fill_random: method_for(fill_random(1000)) <@> (x -> x+x) (hermetic) ---"
    let fillRandomSource = """
type LatIdx = Idx<4>
type LonIdx = Idx<3>
type TimeIdx = Idx<5>
let A: Array<Int64 like LatIdx, LonIdx, TimeIdx> = fill_random(1000)
let out = method_for(A) <@> lambda(x) -> x + x |> compute
"""
    try
        match lower fillRandomSource with
        | Ok ir ->
            check "fill_random: RandomInits populated"
                (let modul = ir.Modules.[0]
                 not (Map.isEmpty modul.RandomInits))
                "expected a RandomInits entry for A"
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "fill_random_e2e"
            check "fill_random: codegen emits allocate + fill_random"
                (cppCode.Contains "fill_random(" && cppCode.Contains "allocate<typename promote<") ""
            let frOutDir = "./generated_cpp_tests"
            if not (Directory.Exists frOutDir) then Directory.CreateDirectory frOutDir |> ignore
            CodeGen.deployRuntimeHeaders frOutDir
            let frCppFile = Path.Combine(frOutDir, "fill_random_e2e.cpp")
            File.WriteAllText(frCppFile, cppCode)
            (match compileCpp frCppFile frOutDir with
             | Ok exePath ->
                 check "fill_random e2e: compiles" true ""
                 (match runExecutable exePath with
                  | Ok (0, _) -> check "fill_random e2e: runs to completion (exit 0)" true ""
                  | Ok (code, runOut) -> check "fill_random e2e: runs to completion (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "fill_random e2e: runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP fill_random e2e (compile skipped): %s" e
                 else check "fill_random e2e: compiles" false e)
        | Error e ->
            check "fill_random: lowers" false ($"lower error: {e}")
    // Hermetic block (no libnetcdf, no sample.nc), so there is no skip condition
    // here at all: any exception is a failure.
    with ex ->
        check "fill_random: no unexpected exception" false
            ($"{(ex.GetType().Name)}: {ex.Message}")

    // ---------------------------------------------------------------
    // Relational pipeline over a provider read: the SQL builtins consume an
    // ordinary provider-materialized Array<T like I>. Reads the rank-1 xdim
    // coordinate var (fixture values 1..20, same contract the static-fold
    // test below pins) and runs WHERE -> COUNT -> SUM -> ORDER BY on it:
    // mask/compound/extents/reduce/sort over file-backed data in one program.
    // Deterministic fixture values, so the EXPECTs are pinned inline.
    // ---------------------------------------------------------------
    printfn "\n--- relational pipeline: mask/compound/reduce/sort over a provider read (sample.nc) ---"
    // `sample.DIMS.xdim`, not `.vars`: xdim is a COORDINATE variable, which
    // ncFileToModule's isCoordinateVar files under the dims struct. This
    // source read `.vars.xdim` for a long time -- a field that does not exist
    // on sample__vars -- and the field-access checker answered a miss with a
    // fresh type variable, so it typechecked into an array of unknown extent.
    // That is the whole of the "KNOWN GAP" this block used to pin on a
    // mask/relational extent seam; the seam was never involved. BL3018 now
    // refuses the wrong accessor by name -- pinned in the block below.
    let relSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let xd = sample.dims.xdim |> NetCDF.read
let high = mask(xd, lambda(x) -> x > 10)
let sel = compound(xd, high)
let n_high = extents(sel)
let total_high = reduce(sel, (+))
let ranked = sort(sel, lambda(x) -> -x)
let top = ranked(0)
"""
    try
        match lower relSource with
        | Ok ir ->
            check "relational pipeline: lowers" true ""
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "provider_relational_e2e"
            // The same marker, asserted in the OPPOSITE direction from the
            // pin it replaces. With the accessor corrected the extent folds to
            // a literal (`size_t xd_extent_0 = 20;`), so a placeholder here is
            // a regression, not a known gap -- and the e2e below (compile +
            // run + the n_high/total_high/top value pins) is no longer guarded
            // off behind it, which is what kept it dormant.
            let gapMarker = "/* dynamic */"
            check "relational pipeline: provider read has a resolved extent (no unresolved-extent placeholder)"
                (not (cppCode.Contains gapMarker))
                ($"generated C++ still contains '{gapMarker}' -- a provider-read extent went unresolved")
            let relOutDir = "./generated_cpp_tests"
            if not (Directory.Exists relOutDir) then Directory.CreateDirectory relOutDir |> ignore
            CodeGen.deployRuntimeHeaders relOutDir
            let relCppFile = Path.Combine(relOutDir, "provider_relational_e2e.cpp")
            File.WriteAllText(relCppFile, cppCode)
            (match compileCpp relCppFile relOutDir with
             | Ok exePath ->
                 check "relational pipeline e2e: compiles and links libnetcdf" true ""
                 Directory.CreateDirectory(Path.Combine(relOutDir, "tests", "fixtures")) |> ignore
                 File.Copy("tests/fixtures/sample.nc", Path.Combine(relOutDir, "tests", "fixtures", "sample.nc"), true)
                 (match runExecutable exePath with
                  | Ok (0, runOut) ->
                      check "relational pipeline e2e: runs to completion (exit 0)" true ""
                      let hasLine (expect: string) =
                          runOut.Split('\n') |> Array.exists (fun l -> l.Trim() = expect)
                      check "relational pipeline e2e: COUNT after WHERE (n_high = 10)"
                          (hasLine "n_high = 10") ($"output was: {runOut}")
                      check "relational pipeline e2e: SUM after WHERE (total_high = 155)"
                          (hasLine "total_high = 155") ""
                      check "relational pipeline e2e: ORDER BY desc head (top = 20)"
                          (hasLine "top = 20") ""
                  | Ok (code, runOut) -> check "relational pipeline e2e: runs to completion (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "relational pipeline e2e: runs to completion (exit 0)" false e)
             | Error e ->
                 if isSkipError e then printfn "  SKIP relational pipeline e2e (compile skipped): %s" e
                 else check "relational pipeline e2e: compiles and links libnetcdf" false e)
        | Error e when nativeLibUnavailable e ->
            printfn "  SKIP relational pipeline: %s" e
        | Error e ->
            check "relational pipeline: lowers" false ($"lower error: {e}")
    with ex -> unexpected "relational pipeline" ex

    // ---------------------------------------------------------------
    // The wrong section accessor is refused BY NAME (BL3018).
    //
    // ncFileToModule splits the file's variables in two: isCoordinateVar
    // routes a rank-1 variable whose single dim is its own name to the
    // `<mod>__dims` struct, everything else to `<mod>__vars`. So `xdim` is a
    // field of sample__dims and NOT of sample__vars, and `sample.vars.xdim`
    // names nothing.
    //
    // It used to typecheck: the field-access checker resolved a miss to a
    // fresh type variable, so the accessor produced an array whose index-type
    // extent was a symbolic IRExpr, and the mistake surfaced -- if at all --
    // far downstream in the provider emitter, as a message about runtime
    // NetCDF extents. Two assertions here, both about the MESSAGE rather than
    // the mere fact of rejection: it must name the missing field, and it must
    // name the accessor that works.
    // ---------------------------------------------------------------
    printfn "\n--- wrong section accessor: sample.vars.<coordinate> is refused (sample.nc) ---"
    let wrongAccessorSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let xd = sample.vars.xdim |> NetCDF.read
let s = reduce(xd, (+))
"""
    try
        match lower wrongAccessorSource with
        | Ok _ ->
            check "wrong section accessor: sample.vars.xdim is rejected" false
                "the program typechecked -- a field that does not exist on sample__vars resolved to a fresh type variable"
        | Error e when nativeLibUnavailable e ->
            printfn "  SKIP wrong section accessor: %s" e
        | Error e ->
            check "wrong section accessor: the diagnostic names the missing field and its struct"
                (e.Contains "struct sample__vars has no field 'xdim'")
                ($"got: {e}")
            check "wrong section accessor: the diagnostic names the accessor that works"
                (e.Contains "sample.dims.xdim")
                ($"got: {e}")
            check "wrong section accessor: the diagnostic lists the fields that DO exist"
                (e.Contains "available fields:" && e.Contains "A")
                ($"got: {e}")
    with ex -> unexpected "wrong section accessor" ex

    // ---------------------------------------------------------------
    // Provider-backed statics: the compile-time fold (ProviderStatics)
    // ---------------------------------------------------------------
    // Unconditional: shapeValue nests a flat buffer row-major.
    printfn "\n--- provider statics: shapeValue + live fold ---"
    (let v = Blade.ProviderStatics.shapeValue [2; 3] (fun i -> Blade.StaticEval.SVInt (int64 i))
     let expected =
         Blade.StaticEval.SVTuple [
             Blade.StaticEval.SVTuple [Blade.StaticEval.SVInt 0L; Blade.StaticEval.SVInt 1L; Blade.StaticEval.SVInt 2L]
             Blade.StaticEval.SVTuple [Blade.StaticEval.SVInt 3L; Blade.StaticEval.SVInt 4L; Blade.StaticEval.SVInt 5L] ]
     check "shapeValue: 2x3 row-major nesting" (v = expected) (sprintf "got %A" v)
     let scalar = Blade.ProviderStatics.shapeValue [] (fun _ -> Blade.StaticEval.SVFloat 7.5)
     check "shapeValue: rank-0 folds to the bare scalar" (scalar = Blade.StaticEval.SVFloat 7.5) (sprintf "got %A" scalar))

    // Live fold: `let static xd = sample.dims.xdim |> NetCDF.read` folds through
    // libnetcdf; xdim = 1..20 in the fixture, so length = 20 and the
    // static prodsum is sum i^2 = 2870. Skips without libnetcdf/sample.nc.
    let foldSource = """
import netcdf as NetCDF

let sample = NetCDF.load("tests/fixtures/sample.nc")
let static xd = sample.dims.xdim |> NetCDF.read
let static n = length(xd)
let static ps = prodsum(xd, xd)
let a = n
let b = ps
"""
    (try
        Blade.ProviderStatics.install ()
        match Parser.parseProgram foldSource with
        | Error e -> check "static fold: parses" false e.Message
        | Ok program ->
            match TypeCheck.typeCheck program with
            | Error errs when errs |> List.exists (TypeEnv.formatCompileError >> nativeLibUnavailable) ->
                printfn "  SKIP static fold: %s"
                    (errs |> List.map TypeEnv.formatCompileError |> String.concat "; ")
            | Error errs ->
                check "static fold: typechecks (fold succeeded)" false
                    (errs |> List.map TypeEnv.formatCompileError |> String.concat "; ")
            | Ok _ ->
                check "static fold: typechecks (fold succeeded)" true ""
                // The folded values land in resolveStatics' env directly:
                match Blade.StaticEval.resolveStatics program.Modules.Head.Decls with
                | Ok (se, _) ->
                    check "static fold: length(xd) = 20"
                        (Map.tryFind "n" se.Values = Some (Blade.StaticEval.SVInt 20L))
                        (sprintf "got %A" (Map.tryFind "n" se.Values))
                    check "static fold: prodsum(xd, xd) = 2870"
                        (Map.tryFind "ps" se.Values = Some (Blade.StaticEval.SVFloat 2870.0))
                        (sprintf "got %A" (Map.tryFind "ps" se.Values))
                | Error e -> check "static fold: resolveStatics" false e
     with ex -> unexpected "provider static fold" ex)

    // ---------------------------------------------------------------
    // Missing file at COMPILE time: `NetCDF.load("<absent>")` must fail
    // cleanly THROUGH the lowering boundary (Result.Error), not throw an
    // unhandled exception. The unguarded throw used to escape lowering and
    // kill the REPL / compile driver outright. Hermetic: the path is
    // intentionally absent, so neither libnetcdf nor sample.nc is required.
    // ---------------------------------------------------------------
    printfn "\n--- missing file: NetCDF.load of an absent path fails cleanly at compile time ---"
    (try
        Blade.ProviderStatics.install ()
        let missingSource = """
import netcdf as NetCDF

let arrays = NetCDF.load("definitely_absent_9f3a.nc")
"""
        match lower missingSource with
        | Ok _ ->
            check "missing file: lowering rejects an absent netcdf path" false
                "expected Error, got Ok"
        | Error msg ->
            check "missing file: lowering returns Error (no unhandled exception)" true ""
            check "missing file: error names the missing file and cwd"
                (msg.Contains "not found" && msg.Contains "definitely_absent_9f3a.nc")
                ($"got: {msg}")
     with
     | ex ->
        // The whole point of the fix: lowering must NOT throw here.
        check "missing file: lowering does not throw an unhandled exception" false
            ($"unexpected exception: {ex.Message}"))

    // ---------------------------------------------------------------
    // Summary
    // ---------------------------------------------------------------
    printFooter "NetCDF Provider" [$"{passed} passed"; $"{failed} failed"]
    if failed > 0 then 1 else 0

