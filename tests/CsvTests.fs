// CSV provider tests. Fully hermetic: fixtures are plain-text files written
// on the fly (CsvProvider.CsvWrite / File.WriteAllText — no external
// library, no committed fixture). Only the e2e compile+run blocks need g++,
// and they skip gracefully without it (Build.isSkipError), mirroring
// ZarrTests' discipline. Fixtures are written TWICE — at the compiler cwd
// (compile-time metadata) and under ./generated_cpp_tests (exe cwd) — the
// same split as NetcdfTests' sample.nc / ZarrTests' stores.
module Blade.Tests.CsvTests

open System
open System.IO
open Blade
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.Lowering
open Blade.CodeGen
open Blade.CsvProvider
open Blade.Build
open Blade.Tests.TestHarness

let runCsvTests () =
    printHeader "CSV Provider Tests"
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

    // Fixture files live under tests/fixtures/csv_files/ (not the repo root).
    let fixDir = "tests/fixtures/csv_files"
    Directory.CreateDirectory fixDir |> ignore
    let fixFile (name: string) = fixDir + "/" + name
    let e2eDir = "./generated_cpp_tests"
    if not (Directory.Exists e2eDir) then Directory.CreateDirectory e2eDir |> ignore
    Directory.CreateDirectory (Path.Combine(e2eDir, fixDir)) |> ignore
    /// Write a fixture at BOTH resolution roots (compiler cwd + exe cwd).
    let writeFixture (name: string) (lines: string list) =
        CsvWrite.writeRaw (fixFile name) lines
        CsvWrite.writeRaw (Path.Combine(e2eDir, fixFile name)) lines

    // ---------------------------------------------------------------
    // 1. Sniffing: header vs matrix detection (pure parse)
    // ---------------------------------------------------------------
    printfn "\n--- sniffing ---"
    let tmp = Path.Combine(Path.GetTempPath(), "blade_csv_" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory tmp |> ignore
    let tmpFile (name: string) (text: string) =
        let p = Path.Combine(tmp, name)
        File.WriteAllText(p, text)
        p

    (let p = tmpFile "headered.csv" "time,temp\n0.0,14.0\n1.0,16.5\n"
     match parseFile p with
     | Ok (f, _) ->
         check "sniff: non-numeric first row -> headered"
             (match f.Shape with CsvTable (["time"; "temp"], 2) -> true | _ -> false)
             (sprintf "%A" f.Shape)
     | Error e -> check "sniff: headered parses" false e)
    (let p = tmpFile "matrix.csv" "1.0,2.0,3.0\n4.0,5.0,6.0\n"
     match parseFile p with
     | Ok (f, _) ->
         check "sniff: all-numeric first row -> matrix"
             (match f.Shape with CsvMatrix (2, 3) -> true | _ -> false)
             (sprintf "%A" f.Shape)
     | Error e -> check "sniff: matrix parses" false e)
    // Mixed first row (some numeric labels): still a header — labels are
    // arbitrary strings, "2" is a legal EnumIdx label.
    (let p = tmpFile "mixedhdr.csv" "time,2\n0.0,14.0\n"
     match parseFile p with
     | Ok (f, _) ->
         check "sniff: partially-numeric first row -> headered (labels are strings)"
             (match f.Shape with CsvTable (["time"; "2"], 1) -> true | _ -> false)
             (sprintf "%A" f.Shape)
     | Error e -> check "sniff: mixed header parses" false e)
    check "sniff: duplicate label rejected"
        (isError (parseFile (tmpFile "dup.csv" "a,a\n1.0,2.0\n")) "duplicate column label 'a'") ""
    check "sniff: header-only file rejected"
        (isError (parseFile (tmpFile "hdronly.csv" "a,b\n")) "no data rows") ""

    // ---------------------------------------------------------------
    // 2. Dtype inference (whole-table)
    // ---------------------------------------------------------------
    printfn "\n--- dtype inference ---"
    (match parseFile (tmpFile "ints.csv" "1,2\n-3,+4\n") with
     | Ok (f, _) -> check "dtype: all integer literals -> Int64" (f.Elem = ETInt64) (sprintf "%A" f.Elem)
     | Error e -> check "dtype: ints parse" false e)
    (match parseFile (tmpFile "mixed.csv" "1,2\n3,4.5\n") with
     | Ok (f, _) -> check "dtype: one decimal cell -> Float64" (f.Elem = ETFloat64) (sprintf "%A" f.Elem)
     | Error e -> check "dtype: mixed parses" false e)
    (match parseFile (tmpFile "sci.csv" "1e5,2\n3,4\n") with
     | Ok (f, _) -> check "dtype: scientific notation is Float64 (value-integral is irrelevant)" (f.Elem = ETFloat64) (sprintf "%A" f.Elem)
     | Error e -> check "dtype: sci parses" false e)
    (match parseFile (tmpFile "specials.csv" "nan,inf\n-inf,1.5\n") with
     | Ok (f, _) -> check "dtype: nan/inf specials accepted as Float64" (f.Elem = ETFloat64) (sprintf "%A" f.Elem)
     | Error e -> check "dtype: specials parse" false e)

    // ---------------------------------------------------------------
    // 3. Format tolerance and rejections
    // ---------------------------------------------------------------
    printfn "\n--- format rules ---"
    (let lf = parseFile (tmpFile "lf.csv" "1.0,2.0\n3.0,4.0\n")
     let crlf = parseFile (tmpFile "crlf.csv" "1.0,2.0\r\n3.0,4.0\r\n")
     match lf, crlf with
     | Ok (a, _), Ok (b, _) ->
         check "format: CRLF parses identically to LF" (a.Shape = b.Shape && a.Elem = b.Elem) ""
     | _ -> check "format: CRLF parses identically to LF" false "parse error")
    (let bom = "﻿" + "a,b\n1.0,2.0\n"
     match parseFile (tmpFile "bom.csv" bom) with
     | Ok (f, _) ->
         check "format: UTF-8 BOM stripped from first label"
             (match f.Shape with CsvTable (["a"; "b"], 1) -> true | _ -> false)
             (sprintf "%A" f.Shape)
     | Error e -> check "format: BOM parses" false e)
    check "format: no trailing newline tolerated"
        (match parseFile (tmpFile "notrail.csv" "1.0,2.0\n3.0,4.0") with
         | Ok (f, _) -> f.Shape = CsvMatrix (2, 2)
         | Error _ -> false) ""
    check "format: ragged row rejected with line number"
        (isError (parseFile (tmpFile "ragged.csv" "1.0,2.0\n3.0\n")) "ragged row" &&
         isError (parseFile (tmpFile "ragged.csv" "1.0,2.0\n3.0\n")) "line 2") ""
    check "format: empty cell rejected with line number"
        (isError (parseFile (tmpFile "emptycell.csv" "1.0,\n3.0,4.0\n")) "empty cell") ""
    check "format: interior blank line rejected"
        (isError (parseFile (tmpFile "blank.csv" "1.0,2.0\n\n3.0,4.0\n")) "blank line") ""
    check "format: quote character rejected"
        (isError (parseFile (tmpFile "quote.csv" "\"a\",b\n1.0,2.0\n")) "quote") ""
    check "format: empty file rejected"
        (isError (parseFile (tmpFile "empty.csv" "")) "empty") ""
    check "format: non-numeric data cell rejected (strings deferred)"
        (isError (parseFile (tmpFile "strdata.csv" "a,b\n1.0,x\n")) "non-numeric cell 'x'") ""
    check "format: missing file error carries path and cwd"
        (isError (parseFile (Path.Combine(tmp, "nope.csv"))) "resolved against cwd") ""

    // ---------------------------------------------------------------
    // 4. readVarData payloads (fold + interpreter source of truth)
    // ---------------------------------------------------------------
    printfn "\n--- readVarData ---"
    (let p = tmpFile "rv_matrix.csv" "1.5,2.5\n3.5,4.5\n5.5,6.5\n"
     match readVarData p "data" with
     | Ok { DimLengths = [3; 2]; Payload = Blade.ProviderRegistry.PFloats xs } ->
         check "readVarData: matrix payload row-major" (xs = [| 1.5; 2.5; 3.5; 4.5; 5.5; 6.5 |]) (sprintf "%A" xs)
     | Ok d -> check "readVarData: matrix payload row-major" false (sprintf "%A" d.DimLengths)
     | Error e -> check "readVarData: matrix payload row-major" false e)
    (let p = tmpFile "rv_table.csv" "a,b\n1,2\n3,4\n"
     match readVarData p "data" with
     | Ok { DimLengths = [2; 2]; Payload = Blade.ProviderRegistry.PInts xs } ->
         check "readVarData: headered payload excludes header, Int64" (xs = [| 1L; 2L; 3L; 4L |]) (sprintf "%A" xs)
     | Ok d -> check "readVarData: headered payload excludes header, Int64" false (sprintf "%A" d.DimLengths)
     | Error e -> check "readVarData: headered payload excludes header, Int64" false e)
    check "readVarData: unknown variable steered to 'data'"
        (isError (readVarData (tmpFile "rv2.csv" "1.0\n") "A") "the only variable is 'data'") ""

    // ---------------------------------------------------------------
    // 5. loadAsModule metadata (EnumIdx synthesis, unique struct names)
    // ---------------------------------------------------------------
    printfn "\n--- loadAsModule ---"
    (let p = tmpFile "meta_table.csv" "time,temp,pressure\n0.0,14.0,101.2\n1.0,16.5,101.0\n"
     let builder = IRBuilder()
     let pm = loadAsModule builder "obs" p
     let enums = pm.Types |> List.choose (function IRTDEnumIdx (n, idx, vs) -> Some (n, idx, vs) | _ -> None)
     check "meta: headered load synthesizes one EnumIdx"
         (match enums with
          | [(n, _, vs)] -> n = "obs_cols" && vs = [EVString "time"; EVString "temp"; EVString "pressure"]
          | _ -> false)
         (sprintf "%A" (enums |> List.map (fun (n, _, _) -> n)))
     let varsStructs = pm.Types |> List.choose (function IRTDStruct (n, fields) -> Some (n, fields) | _ -> None)
     check "meta: vars struct uniquely named obs__vars with var 'data'"
         (match varsStructs with
          | [(n, [("data", _)])] -> n = "obs__vars"
          | _ -> false)
         (sprintf "%A" (varsStructs |> List.map fst))
     match varsStructs with
     | [(_, [(_, vt)])] ->
         (match vt with
          | ArrayElem arrTy ->
              check "meta: data is 2 x 3 Float64, column axis tagged obs_cols"
                  (arrTy.ElemType = IRTScalar ETFloat64
                   && arrTy.IndexTypes.Length = 2
                   && arrTy.IndexTypes.[0].Extent = IRLit (IRLitInt 2L)
                   && arrTy.IndexTypes.[1].Extent = IRLit (IRLitInt 3L)
                   && arrTy.IndexTypes.[1].Tag = Some "obs_cols")
                  (sprintf "%A" arrTy.IndexTypes)
          | _ -> check "meta: data is an array" false (sprintf "%A" vt))
     | _ -> ())
    (let p = tmpFile "meta_matrix.csv" "1,2\n3,4\n5,6\n"
     let builder = IRBuilder()
     let pm = loadAsModule builder "m" p
     check "meta: matrix load synthesizes NO EnumIdx (plain Idx axes)"
         (pm.Types |> List.forall (fun t -> not t.IsIRTDEnumIdx)) ""
     let varsStructs = pm.Types |> List.choose (function IRTDStruct (n, fields) -> Some (n, fields) | _ -> None)
     check "meta: matrix data is 3 x 2 Int64, untagged axes"
         (match varsStructs with
          | [(_, [("data", ArrayElem arrTy)])] ->
              arrTy.ElemType = IRTScalar ETInt64
              && arrTy.IndexTypes |> List.map (_.Extent) = [IRLit (IRLitInt 3L); IRLit (IRLitInt 2L)]
              && arrTy.IndexTypes |> List.forall (fun ix -> ix.Tag = None)
          | _ -> false)
         (sprintf "%A" varsStructs))

    // ---------------------------------------------------------------
    // 6. Lowering: two loads in one program (struct-name isolation),
    //    label subscript folding, unknown label diagnostic
    // ---------------------------------------------------------------
    printfn "\n--- lowering + label folds ---"
    writeFixture "lw_obs.csv"
        [ "time,temp,pressure"; "0.0,14.0,101.2"; "1.0,16.5,101.0"; "2.0,18.0,100.8" ]
    writeFixture "lw_grid.csv"
        [ "1.0,2.0,3.0"; "4.0,5.0,6.0" ]
    let twoLoadSource = sprintf """
import csv as c

let t = c.load("%s")
let obs = t.vars.data |> c.read
let m = c.load("%s")
let V = m.vars.data |> c.read

let t1 = obs(1, "temp")
let rowsum = method_for(V) <@> lambda(r: Array<Float64 like Idx<3>>) -> reduce(r, (+)) |> compute
"""
                            (fixFile "lw_obs.csv") (fixFile "lw_grid.csv")
    (match lower twoLoadSource with
     | Ok ir ->
         check "lower: two csv loads in one program"
             (ir.Modules.[0].ProviderReads |> Map.filter (fun _ s -> s.Provider = "csv") |> Map.count = 2) ""
     | Error e -> check "lower: two csv loads in one program" false e)
    (let badSource = sprintf """
import csv as c
let t = c.load("%s")
let obs = t.vars.data |> c.read
let bad = obs(1, "tmp")
"""
                         (fixFile "lw_obs.csv")
     match lower badSource with
     | Ok _ -> check "lower: unknown label rejected" false "lowered without error"
     | Error e ->
         check "lower: unknown label rejected with available list"
             (e.Contains "'tmp' is not a value of EnumIdx 't_cols'" && e.Contains "time, temp, pressure") e)

    // Field ACCESS against the provider struct (BL3018), the sibling of the
    // EnumIdx label check above: that one validates a column SUBSCRIPT, this
    // one the struct field naming the array. CSV emits a vars section only --
    // no coordinate arrays exist in a flat table -- so `.dims` is itself the
    // unknown field, and the census in the message is what tells you so.
    (let noSuchVar = sprintf """
import csv as c
let t = c.load("%s")
let bad = t.vars.nope
"""
                         (fixFile "lw_obs.csv")
     match lower noSuchVar with
     | Ok _ -> check "lower: unknown vars field rejected" false "lowered without error"
     | Error e ->
         check "lower: unknown vars field rejected, listing the fields that exist"
             (e.Contains "struct t__vars has no field 'nope'" && e.Contains "available fields: data") e)
    (let noDimsSection = sprintf """
import csv as c
let t = c.load("%s")
let bad = t.dims.time
"""
                             (fixFile "lw_obs.csv")
     match lower noDimsSection with
     | Ok _ -> check "lower: .dims on a csv binding rejected" false "lowered without error"
     | Error e ->
         check "lower: .dims on a csv binding rejected (csv emits no dims section)"
             (e.Contains "struct t has no field 'dims'" && e.Contains "available fields: vars") e)

    // ---------------------------------------------------------------
    // 7. Static fold (compile-time payload) + fold ceiling
    // ---------------------------------------------------------------
    printfn "\n--- static fold ---"
    (let foldSource = sprintf """
import csv as c
let m = c.load("%s")
let static V = m.vars.data |> c.read
"""
                          (fixFile "lw_grid.csv")
     match lower foldSource with
     // Name trimmed to what is actually asserted: this arm only observes that
     // lowering succeeded, so claiming "(2x3 payload)" overstated it -- the
     // payload itself is pinned by the read/round-trip blocks further down.
     | Ok _ -> check "fold: static csv read lowers (fold did not reject)" true ""
     | Error e -> check "fold: static csv read lowers (fold did not reject)" false e)
    (let bigName = "lw_big.csv"
     // 65537 single-column rows: one over the fold ceiling.
     CsvWrite.writeRaw (fixFile bigName) [ for i in 0 .. 65536 -> $"{i}.0" ]
     let bigSource = sprintf """
import csv as c
let m = c.load("%s")
let static V = m.vars.data |> c.read
"""
                         (fixFile bigName)
     (match lower bigSource with
      | Ok _ -> check "fold: over-ceiling static read refused" false "lowered without error"
      | Error e -> check "fold: over-ceiling static read refused with steering" (e.Contains "fold ceiling") e)
     try File.Delete (fixFile bigName) with _ -> ())

    // ---------------------------------------------------------------
    // 8. Interpreter parity: dense reads materialize via ReadVarData;
    //    writes classify unsupported.
    // ---------------------------------------------------------------
    printfn "\n--- interpreter ---"
    (match lower twoLoadSource with
     | Ok ir ->
         let r = Blade.Interp.Run.runProgram ir "csv_interp" Blade.Interp.Value.defaultLimits
         check "interp: dense reads run (exit ok)"
             (r.ExitCode = Blade.Interp.Run.ExitOk) (sprintf "%A: %s" r.ExitCode r.Stderr)
         check "interp: label subscript value (t1 = 16.5)" (r.Stdout.Contains "t1 = 16.5") r.Stdout
         check "interp: matrix row sums" (r.Stdout.Contains "rowsum = [6, 15]") r.Stdout
     | Error e -> check "interp: dense reads run" false e)
    (let writeSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.read
let _ = c.write("%s", V)
"""
                           (fixFile "lw_grid.csv") (fixFile "lw_interp_out.csv")
     match lower writeSource with
     | Ok ir ->
         let r = Blade.Interp.Run.runProgram ir "csv_interp_wr" Blade.Interp.Value.defaultLimits
         check "interp: csv write classifies unsupported (falls to compiled path)"
             (r.ExitCode = Blade.Interp.Run.ExitUnsupported) (sprintf "%A" r.ExitCode)
     | Error e -> check "interp: csv write lowers" false e)

    // ---------------------------------------------------------------
    // 9. E2e compile+run: headered + matrix + label subscript (g++)
    // ---------------------------------------------------------------
    printfn "\n--- e2e read (g++) ---"
    (match lower twoLoadSource with
     | Ok ir ->
         check "e2e: ProviderReads specs (provider=csv, var=data)"
             (ir.Modules.[0].ProviderReads |> Map.forall (fun _ s -> s.Provider = "csv" && s.VarName = "data"))
             ""
         let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "csv_read_e2e"
         check "e2e: emits fstream reads, no netcdf dependency"
             (cppCode.Contains "std::ifstream" && not (cppCode.Contains "netcdf.h")) ""
         CodeGen.deployRuntimeHeaders e2eDir
         let cppFile = Path.Combine(e2eDir, "csv_read_e2e.cpp")
         File.WriteAllText(cppFile, cppCode)
         (match compileCpp cppFile e2eDir with
          | Ok exePath ->
              check "e2e: compiles (pure std C++ — no link flags)" true ""
              (match runExecutable exePath with
               | Ok (0, runOut) ->
                   check "e2e: runs (exit 0)" true ""
                   check "e2e: label subscript t1 = 16.5" (runOut.Contains "t1 = 16.5") runOut
                   check "e2e: rank-1 row sums (2-D EXPECTs are unchecked — reduce instead)"
                       (runOut.Contains "rowsum = [6, 15]") runOut
                   // NESTED, not flat: every rank-2 array prints through
                   // genPrintNested2 (CodeGen), which landed in 7ac4d3a
                   // (2026-08-03) and is the intended shape -- a rank-2
                   // literal's own spelling, so the line round-trips as
                   // source. This needle predated it (a32e6d7, 2026-07-21)
                   // and read flat, which is why it was the CSV gate's one
                   // standing red. The corpus validator accepts BOTH shapes
                   // for a 2-D pin (Expect.fs, ExpectedArray2D), so nothing
                   // there caught the drift; a raw substring check like this
                   // one has no such tolerance and must name the real shape.
                   check "e2e: headered payload (header skipped)"
                       (runOut.Contains "obs = [[0, 14, 101.2], [1, 16.5, 101], [2, 18, 100.8]]") runOut
                   // Runtime shape drift: the exe baked 3 data rows; append a
                   // 4th to the RUNTIME copy and expect a loud abort.
                   let runtimeFix = Path.Combine(e2eDir, fixFile "lw_obs.csv")
                   let orig = File.ReadAllText runtimeFix
                   (try
                       File.WriteAllText(runtimeFix, orig + "3.0,15.5,100.9\n")
                       (match runExecutable exePath with
                        | Ok (code, driftOut) ->
                            check "e2e: runtime row-count drift aborts loudly"
                                (code <> 0 && driftOut.Contains "CSV error" && driftOut.Contains "more data rows")
                                ($"exit {code}: {(driftOut.Substring(0, min 200 driftOut.Length))}")
                        | Error e -> check "e2e: runtime row-count drift aborts loudly" false e)
                    finally
                       File.WriteAllText(runtimeFix, orig))
                   // Missing file at runtime: run from a dir without fixtures.
                   let missingDir = Path.Combine(Path.GetTempPath(), "blade_csv_missing_" + Guid.NewGuid().ToString("N"))
                   Directory.CreateDirectory missingDir |> ignore
                   (try
                       let exeCopy = Path.Combine(missingDir, Path.GetFileName exePath)
                       File.Copy(exePath, exeCopy, true)
                       (match runExecutable exeCopy with
                        | Ok (code, missOut) ->
                            check "e2e: missing file at runtime fails loudly (nonzero + CSV error)"
                                (code <> 0 && missOut.Contains "CSV error")
                                ($"exit {code}: {(missOut.Substring(0, min 200 missOut.Length))}")
                        | Error e -> check "e2e: missing file fails loudly" false e)
                    finally
                       try Directory.Delete(missingDir, true) with _ -> ())
               | Ok (code, runOut) -> check "e2e: runs (exit 0)" false ($"exit {code}: {runOut}")
               | Error e -> check "e2e: runs (exit 0)" false e)
          | Error e ->
              if isSkipError e then printfn "  SKIP csv read e2e (compile skipped): %s" e
              else check "e2e: compiles" false e)
     | Error e -> check "e2e: lowers" false e)

    // ---------------------------------------------------------------
    // 10. E2e write round-trip: float dtype stability + int passthrough
    // ---------------------------------------------------------------
    printfn "\n--- e2e write (g++) ---"
    // Whole-valued floats: the writer must force decimal points so the
    // output re-loads as Float64 (the 2.0 -> "2" -> Int64 flip guard).
    writeFixture "wr_in.csv" [ "2.0,4.0"; "6.0,0.5" ]
    writeFixture "wr_int_in.csv" [ "1,2"; "3,4" ]
    (let writeSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.read
let n = c.load("%s")
let K = n.vars.data |> c.read
let _ = c.write("%s", V)
let _ = c.write("%s", K)
"""
                           (fixFile "wr_in.csv") (fixFile "wr_int_in.csv")
                           (fixFile "wr_out.csv") (fixFile "wr_int_out.csv")
     match lower writeSource with
     | Ok ir ->
         check "write e2e: ProviderWrites specs recorded"
             (ir.Modules.[0].ProviderWrites |> Map.count = 2) ""
         let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "csv_write_e2e"
         let cppFile = Path.Combine(e2eDir, "csv_write_e2e.cpp")
         File.WriteAllText(cppFile, cppCode)
         (match compileCpp cppFile e2eDir with
          | Ok exePath ->
              (match runExecutable exePath with
               | Ok (0, _) ->
                   check "write e2e: runs (exit 0)" true ""
                   // The exe cwd is generated_cpp_tests — outputs land there.
                   let outPath = Path.Combine(e2eDir, fixFile "wr_out.csv")
                   (match readVarData outPath "data" with
                    | Ok { DimLengths = [2; 2]; Payload = Blade.ProviderRegistry.PFloats xs } ->
                        check "write e2e: floats round-trip exactly AND stay Float64"
                            (xs = [| 2.0; 4.0; 6.0; 0.5 |]) (sprintf "%A" xs)
                    | Ok d -> check "write e2e: floats stay Float64" false (sprintf "dtype flipped: %A" d.Payload)
                    | Error e -> check "write e2e: floats round-trip" false e)
                   let intOutPath = Path.Combine(e2eDir, fixFile "wr_int_out.csv")
                   (match readVarData intOutPath "data" with
                    | Ok { DimLengths = [2; 2]; Payload = Blade.ProviderRegistry.PInts xs } ->
                        check "write e2e: ints round-trip as Int64" (xs = [| 1L; 2L; 3L; 4L |]) (sprintf "%A" xs)
                    | Ok d -> check "write e2e: ints round-trip as Int64" false (sprintf "%A" d.Payload)
                    | Error e -> check "write e2e: ints round-trip as Int64" false e)
                   // Full Blade round-trip: a second program loads what the
                   // first wrote. Copy the runtime output up to the compiler
                   // cwd so its compile-time metadata resolves.
                   File.Copy(outPath, fixFile "wr_out.csv", true)
                   let reloadSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.read
let rowsums = method_for(V) <@> lambda(r: Array<Float64 like Idx<2>>) -> reduce(r, (+)) |> compute
let total = reduce(rowsums, (+))
"""
                                          (fixFile "wr_out.csv")
                   (match lower reloadSource with
                    | Ok ir2 ->
                        let r = Blade.Interp.Run.runProgram ir2 "csv_reload" Blade.Interp.Value.defaultLimits
                        check "write e2e: Blade re-load of written file (total = 12.5)"
                            (r.ExitCode = Blade.Interp.Run.ExitOk && r.Stdout.Contains "total = 12.5")
                            r.Stdout
                    | Error e -> check "write e2e: Blade re-load lowers" false e)
               | Ok (code, runOut) -> check "write e2e: runs (exit 0)" false ($"exit {code}: {runOut}")
               | Error e -> check "write e2e: runs (exit 0)" false e)
          | Error e ->
              if isSkipError e then printfn "  SKIP csv write e2e (compile skipped): %s" e
              else check "write e2e: compiles" false e)
     | Error e -> check "write e2e: lowers" false e)

    // ---------------------------------------------------------------
    // 11. Unsupported surfaces rejected loudly (stream / load_compound)
    // ---------------------------------------------------------------
    printfn "\n--- rejections ---"
    (let streamSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.stream
let out = method_for(V) <@> lambda(x) -> x + 0.0 |> compute
"""
                            (fixFile "lw_grid.csv")
     match lower streamSource with
     | Ok ir ->
         (try
             CodeGen.genSelfContainedProgramFromIR ir "csv_stream_reject" |> ignore
             check "reject: .stream on csv fails loudly" false "codegen accepted it"
          with ex ->
             check "reject: .stream on csv fails loudly"
                 (ex.Message.Contains "does not support streamed reads") ex.Message)
     | Error e ->
         // Also acceptable: rejected before codegen -- but the message has to
         // name the refusal, not merely echo the source. The old needle was
         // `e.Contains "stream"`, and "stream" is a substring of the very source
         // text being compiled (`c.stream`), so essentially any error that
         // quoted the offending line satisfied it -- including errors about
         // something else entirely. Pin the real refusal wording, verified
         // against `blade compile` on this exact source:
         //   error[BL7001]: provider 'csv' does not support streamed reads
         //                  (variable 'data' - bind with .read)
         // A front-end rejection must be at least as specific.
         check "reject: .stream on csv fails loudly"
             (e.Contains "does not support streamed reads" || e.Contains "BL7001") e)

    // ---------------------------------------------------------------
    // 12. Write-path regressions (two bugs found while authoring the
    //     windowing corpus, both pre-existing on master @ d3e005d)
    // ---------------------------------------------------------------
    printfn "\n--- write-path regressions ---"

    // (a) A binding whose name is a C++ RESERVED WORD. `bindingCppName` used
    // to declare it raw (`Array<double,2> final = ...`) while `addVarName`
    // recorded the sanitized `final_`, so every consumer -- the provider
    // write's flatten loop included -- referenced a name that did not exist
    // ('final_' was not declared in this scope). The print LABEL must still
    // read `final`, which is what splits the two spellings apart.
    writeFixture "wr_kw_in.csv" [ "2.0,4.0"; "6.0,0.5" ]
    (let kwSource = sprintf """
import csv as c
let m = c.load("%s")
let final = m.vars.data |> c.read
let class = final(0, 0)
let _ = c.write("%s", final)
"""
                        (fixFile "wr_kw_in.csv") (fixFile "wr_kw_out.csv")
     match lower kwSource with
     | Ok ir ->
         let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "csv_write_kw"
         let cppFile = Path.Combine(e2eDir, "csv_write_kw.cpp")
         File.WriteAllText(cppFile, cppCode)
         (match compileCpp cppFile e2eDir with
          | Ok exePath ->
              check "kw-name: reserved-word bindings compile (g++)" true ""
              (match runExecutable exePath with
               | Ok (0, runOut) ->
                   // Labels keep the SOURCE spelling; only the C++ identifier
                   // is mangled. A corpus EXPECT pin reads these lines.
                   check "kw-name: print labels keep the source name"
                       (runOut.Contains "final = [[2, 4], [6, 0.5]]" && runOut.Contains "class = 2") runOut
                   let kwOut = Path.Combine(e2eDir, fixFile "wr_kw_out.csv")
                   (match readVarData kwOut "data" with
                    | Ok { DimLengths = [2; 2]; Payload = Blade.ProviderRegistry.PFloats xs } ->
                        check "kw-name: write of a reserved-word binding round-trips"
                            (xs = [| 2.0; 4.0; 6.0; 0.5 |]) (sprintf "%A" xs)
                    | Ok d -> check "kw-name: write of a reserved-word binding round-trips" false (sprintf "%A" d.Payload)
                    | Error e -> check "kw-name: write of a reserved-word binding round-trips" false e)
               | Ok (code, runOut) -> check "kw-name: runs (exit 0)" false ($"exit {code}: {runOut}")
               | Error e -> check "kw-name: runs (exit 0)" false e)
          | Error e ->
              if isSkipError e then printfn "  SKIP kw-name e2e (compile skipped): %s" e
              else check "kw-name: reserved-word bindings compile (g++)" false e)
     | Error e -> check "kw-name: lowers" false e)

    // (b) `c.write` outside module scope. Only the module decl loop
    // intercepts a write into ProviderWrites; one nested in a block used to
    // lower as an ordinary method call on the alias VALUE and reach g++ as
    // `c.write(std::string("out.csv"), __v5)` -> 'c' was not declared in this
    // scope. It is now a checker refusal (BL3007) that names the fix.
    (let nestedSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.read
let x = {
    let y = V
    let _ = c.write("%s", y)
    1.0
}
"""
                            (fixFile "wr_kw_in.csv") (fixFile "wr_nested_out.csv")
     match lower nestedSource with
     | Ok _ -> check "reject: c.write inside a block is refused at check time" false "it type-checked"
     | Error e ->
         check "reject: c.write inside a block is refused at check time"
             (e.Contains "MODULE-LEVEL declaration form") e)

    // The same refusal inside a FUNCTION body -- the other half of "anything
    // that is not literally a top-level let RHS".
    (let fnSource = sprintf """
import csv as c
let m = c.load("%s")
let V = m.vars.data |> c.read
function dump(A: Array<Float64 like Idx<2>, Idx<2>>) -> Float64 = {
    let _ = c.write("%s", A)
    0.0
}
let z = dump(V)
"""
                        (fixFile "wr_kw_in.csv") (fixFile "wr_fn_out.csv")
     match lower fnSource with
     | Ok _ -> check "reject: c.write inside a function body is refused" false "it type-checked"
     | Error e ->
         check "reject: c.write inside a function body is refused"
             (e.Contains "MODULE-LEVEL declaration form") e)

    // ---------------------------------------------------------------
    // 9. Annotation vs store shape (BL3016)
    //
    // `unify` does not compare extents (deliberate general policy) and the
    // provider read arm returns its operand's type unchanged, so an
    // annotation used to win the TYPE while the file still won the
    // ALLOCATION. On the 2x3 lw_grid fixture,
    //     let bad: Array<Float64 like Idx<5>, Idx<9>> = g.vars.data |> c.read
    // typechecked clean in both `check` and `ide check`, emitted
    // `new double[6]` from the file's real shape, indexed it `bad[4][8]`,
    // and segfaulted (exit 139). Covered here rather than in tests/corpus
    // because there is no provider corpus category to pin it in.
    // ---------------------------------------------------------------
    printfn "\n--- annotation vs store shape ---"
    /// A program over the 2x3 matrix fixture whose only variable part is the
    /// declaration of V, so every spelling of the ascription is one string.
    let gridProgram (decl: string) =
        sprintf """
import csv as c
let g = c.load("%s")
%s
let s = reduce(V, (+), axes = 2)
"""
                (fixFile "lw_grid.csv") decl
    /// `lower` hands back FORMATTED text, not the BLxxxx code, so key on the
    /// phrase only this arm's message carries.
    let saysStoreShape (e: string) = e.Contains "typed BY THE STORE"
    let rejectsShape (name: string) (decl: string) (alsoContains: string) =
        match lower (gridProgram decl) with
        | Ok _ -> check name false "it type-checked (the OOB read is back)"
        | Error e ->
            check name (saysStoreShape e && e.Contains alsoContains) e
    // The audit's own repro, and its mirror on the trailing slot: the message
    // must name the offending SLOT, not just report a disagreement.
    rejectsShape "reject: annotation over-states the leading extent"
        "let V: Array<Float64 like Idx<5>, Idx<3>> = g.vars.data |> c.read"
        "extent 5 on index slot 1"
    rejectsShape "reject: annotation over-states the trailing extent"
        "let V: Array<Float64 like Idx<2>, Idx<9>> = g.vars.data |> c.read"
        "extent 9 on index slot 2"
    // Every spelling that reaches a checking position, not just the pipe:
    // the `alias.read(view)` application, the `(e) : T` ascription, and the
    // annotation written through a named index alias (which is how the audit
    // first hit it -- the alias hides the extent from the reader too).
    rejectsShape "reject: same clash through the c.read(view) call spelling"
        "let V: Array<Float64 like Idx<5>, Idx<3>> = c.read(g.vars.data)"
        "index slot 1"
    rejectsShape "reject: same clash through the `(e) : T` ascription spelling"
        "let V = (g.vars.data |> c.read) : Array<Float64 like Idx<5>, Idx<3>>"
        "index slot 1"
    rejectsShape "reject: same clash hidden behind a named index alias"
        "type Wrong = Idx<5>\nlet V: Array<Float64 like Wrong, Idx<3>> = g.vars.data |> c.read"
        "index slot 1"
    // The other direction, and the point of the gate being extent-only: an
    // annotation that AGREES with the store still passes, including the one
    // that matters -- giving the store's anonymous axes nominal names, which
    // is the whole reason to annotate a read. A gate that refused these would
    // have read as "annotations on provider reads are refused".
    (match lower (gridProgram "let V: Array<Float64 like Idx<2>, Idx<3>> = g.vars.data |> c.read") with
     | Ok _ -> check "accept: annotation that matches the store's shape" true ""
     | Error e -> check "accept: annotation that matches the store's shape" false e)
    (match lower (gridProgram "type Row = Idx<2>\ntype Col = Idx<3>\nlet V: Array<Float64 like Row, Col> = g.vars.data |> c.read") with
     | Ok _ -> check "accept: naming the store's axes at their true extents" true ""
     | Error e -> check "accept: naming the store's axes at their true extents" false e)

    (try Directory.Delete(tmp, true) with _ -> ())

    printfn "\n=== CSV Provider Tests: %d passed, %d failed ===" passed failed
    if failed = 0 then 0 else 1
