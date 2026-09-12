// Standalone C++ wreath-class storage tests (cpp/orb_wreath_tests.cpp):
// segment-peeled traversal order, closed-form cardinality, rank/unrank
// bijection, canonicalization signs, overflow walls. Modeled on AllocTests.fs
// (audit §2.3 precedent): the property under test is a C++ compile-time and
// runtime invariant of src/cpp/orbit_wreath_utilities.hpp, so this compiles
// and runs the shipped copy directly rather than going through the Blade
// source pipeline.
module Blade.Tests.OrbWreathTests

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Blade.Build
open Blade.Tests.TestHarness
open Blade.OrbRank

/// Parse a menu spec ("2-,2+", "1+", "[]") into a Level list. Specs are
/// innermost-first, which is exactly the order OrbRank's outermost-last
/// Level list reads left to right.
let private parseSpec (s: string) : Level list option =
    if s = "[]" then Some [] else
    let parseTok (t: string) =
        if t.Length < 2 then None else
        let body = t.Substring(0, t.Length - 1)
        match System.Int32.TryParse body, t.[t.Length - 1] with
        | (true, r), '+' -> Some(r, OPlus)
        | (true, r), '-' -> Some(r, OMinus)
        | _ -> None
    let toks = s.Split(',') |> Array.map parseTok
    if toks |> Array.exists Option.isNone then None
    else Some(toks |> Array.map Option.get |> List.ofArray)

/// Run a subprocess in cppDir and return (exitCode, stdout, stderr), or None
/// on timeout.
let private runTool (path: string) (args: string) (cppDir: string) : (int * string * string) option =
    let psi = ProcessStartInfo(path, args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.WorkingDirectory <- cppDir
    use p = Process.Start(psi)
    let out = Blade.Runtime.readToEndOffPool p.StandardOutput
    let err = Blade.Runtime.readToEndOffPool p.StandardError
    if not (p.WaitForExit(60000)) then
        (try p.Kill(true) with _ -> ())
        None
    else Some(p.ExitCode, out.Result, err.Result)

/// Cross-implementation diff (adversarial-review hardening, 2026-08-01): the
/// C++ `--dump` stream and F# OrbRank.visitStream are INDEPENDENT
/// constructions of the same §2 ascending-lex order, and they drifted once
/// already before review caught it -- so the diff runs on every suite pass,
/// not as a review-time ritual. The class list is NOT kept here: the exe's
/// `--specs` enumerates its own generated menu (the d<=2 r<=3 closure plus
/// the depth-3 rank-2 closure), so menu growth extends this diff
/// automatically and a hand copy cannot drift. One extent, n = 4: the cost
/// scales with CELLS (both emitters enumerate canonical tuples only, 1540
/// max over the closure), and n = 4 is the smallest extent at which every
/// closure class has a non-empty stream -- at n = 3 the strict-over-strict
/// rank-3 classes degenerate to 0 or 1 cells and the diff proves nothing.
let private runDumpDiffs (exePath: string) (cppDir: string) : int * int * string list =
    let mutable p = 0
    let mutable f = 0
    let mutable names : string list = []
    let cases =
        match runTool exePath "--specs" cppDir with
        | None ->
            f <- f + 1
            names <- names @ [ "--specs timed out" ]
            resultLine Fail "--specs enumerates the C++ menu" "timed out"
            []
        | Some(code, out, err) when code <> 0 ->
            f <- f + 1
            names <- names @ [ "--specs failed" ]
            resultLine Fail "--specs enumerates the C++ menu" ($"exit {code}: {(err.Trim())}")
            []
        | Some(_, out, _) ->
            let specs =
                out.Replace("\r\n", "\n").Split('\n')
                |> Array.filter (fun l -> l.Trim() <> "")
                |> Array.toList
            if specs.Length < 51 then
                f <- f + 1
                names <- names @ [ "--specs menu too small" ]
                resultLine Fail "--specs enumerates the C++ menu"
                           ($"only {specs.Length} specs (closure is 51)")
                []
            else
                p <- p + 1
                resultLine Pass "--specs enumerates the C++ menu" ($"{specs.Length} specs")
                specs
                |> List.map (fun spec ->
                    match parseSpec spec with
                    | Some levels -> spec, levels, 4
                    | None -> spec, [], -1)
    // A systematically hanging exe must not burn 60 s x 51 specs: after three
    // consecutive timeouts the rest of the sweep is abandoned as one failure.
    let mutable consecTimeouts = 0
    for (spec, levels, n) in cases do
        let name = $"dump diff \"{spec}\" n={n} vs OrbRank.visitStream"
        if consecTimeouts >= 3 then ()
        elif n < 0 then
            f <- f + 1; names <- names @ [ name ]
            resultLine Fail name "unparseable spec from --specs"
        else
            match runTool exePath ($"--dump \"{spec}\" {n}") cppDir with
            | None ->
                consecTimeouts <- consecTimeouts + 1
                f <- f + 1; names <- names @ [ name ]
                resultLine Fail name "--dump timed out"
                if consecTimeouts >= 3 then
                    resultLine Fail "dump diff sweep" "3 consecutive timeouts -- abandoning the remaining specs"
            | Some(code, _, err) when code <> 0 ->
                consecTimeouts <- 0
                f <- f + 1; names <- names @ [ name ]
                resultLine Fail name ($"--dump exit {code}: {(err.Trim())}")
            | Some(_, out, _) ->
                consecTimeouts <- 0
                // A malformed line must score a FAIL, not throw out of the
                // whole block (Array.map int raises on garbage output).
                let parsed =
                    try
                        out.Replace("\r\n", "\n").Split('\n')
                        |> Array.filter (fun l -> l.Trim() <> "")
                        |> Array.map (fun l ->
                            l.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map int
                            |> List.ofArray)
                        |> List.ofArray
                        |> Some
                    with _ -> None
                match parsed with
                | None ->
                    f <- f + 1; names <- names @ [ name ]
                    resultLine Fail name "--dump emitted a non-numeric line"
                | Some got ->
                    let want = visitStream levels n |> List.ofSeq
                    if got = want then
                        p <- p + 1
                        resultLine Pass name ($"{got.Length} cells, exact stream match")
                    else
                        f <- f + 1; names <- names @ [ name ]
                        let detail =
                            if got.Length <> want.Length then
                                $"C++ emits {got.Length} cells, F# {want.Length}"
                            else
                                let i = List.zip got want |> List.findIndex (fun (a, b) -> a <> b)
                                sprintf "first divergence at %d: C++ %A, F# %A" i (List.item i got) (List.item i want)
                        resultLine Fail name detail
    (p, f, names)

/// Run the standalone C++ wreath-storage test suite (cpp/orb_wreath_tests.cpp).
///
/// The test .cpp and orbit_wreath_utilities.hpp are both shipped in cpp/ next
/// to the compiler binary (AppContext.BaseDirectory/cpp), copied there by
/// Blade.fsproj. Compiling in that directory means the suite exercises the
/// EXACT header a future codegen consumer will include — not a stale copy.
///
/// Returns 0 on all-pass or skip (g++ absent); 1 on any compile/run/check
/// failure. Same skip convention as AllocTests: Skipped = 1 so a
/// toolchain-less box is distinguishable from a vacuously green block.
let runOrbWreathTests () : Blade.Tests.TestHarness.BlockResult =
    let cppDir = Path.Combine(AppContext.BaseDirectory, "cpp")
    let testSrc = Path.Combine(cppDir, "orb_wreath_tests.cpp")
    let caps = capabilities.Value
    printHeader "Orbit Wreath Storage Tests"
    let blockName = "Orb Wreath"
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile C++ wreath tests)."
        { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    elif not (File.Exists testSrc) then
        eprintfn "orb_wreath_tests.cpp not found at: %s" testSrc
        eprintfn "Check that Blade.fsproj copies cpp/orb_wreath_tests.cpp to the output dir."
        { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["orb_wreath_tests.cpp missing"] }
    else
        let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
        let exePath = Path.ChangeExtension(testSrc, exeExt)
        // C++20: the header's level lists are template packs with `if constexpr`
        // sign dispatch (c++17 suffices for neither the pack idioms nor the
        // concepts-adjacent diagnostics the file leans on).
        let args = $"-std=c++20 {(optFlags ())} -o \"{exePath}\" \"{testSrc}\""
        let psi = ProcessStartInfo("g++", args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        psi.WorkingDirectory <- cppDir
        use cproc = Process.Start(psi)
        let cOut = Blade.Runtime.readToEndOffPool cproc.StandardOutput
        let cErr = Blade.Runtime.readToEndOffPool cproc.StandardError
        // 300s (was 120s), matching Build.fs's own budget: this is the
        // slowest single translation unit in the tree and the Phase 0 flag
        // bump made it slower still. MEASURED (g++ 15.2, ucrt64, idle):
        // 36.4s at -O2, 53.0s at -O3 -march=native -ffp-contract=off; on a
        // BUSY box the same compile took 74.7s, i.e. 62% of the old cap.
        let cExited = cproc.WaitForExit(300000)
        if not cExited then
            (try cproc.Kill(true) with _ -> ())
            printfn "C++ compilation TIMED OUT (300s)"
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile timeout>"] }
        elif cproc.ExitCode <> 0 then
            printfn "C++ compilation FAILED:"
            printfn "%s" (cOut.Result + "\n" + cErr.Result)
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile failed>"] }
        else
            let rpsi = ProcessStartInfo(exePath)
            rpsi.RedirectStandardOutput <- true
            rpsi.RedirectStandardError <- true
            rpsi.UseShellExecute <- false
            rpsi.CreateNoWindow <- true
            rpsi.WorkingDirectory <- cppDir
            use rproc = Process.Start(rpsi)
            let rOut = Blade.Runtime.readToEndOffPool rproc.StandardOutput
            let rErr = Blade.Runtime.readToEndOffPool rproc.StandardError
            let rExited = rproc.WaitForExit(60000)
            if not rExited then
                (try rproc.Kill(true) with _ -> ())
                rproc.WaitForExit(5000) |> ignore
                printfn "orb wreath test binary TIMED OUT (60s)"
            printf "%s" rOut.Result
            if not (String.IsNullOrWhiteSpace rErr.Result) then eprintf "%s" rErr.Result
            // Parse the "=== p passed, f failed" summary for the grand total,
            // and collect "FAIL  <name>" line names. Exit code stays the
            // verdict — but only once the summary confirms checks actually ran
            // (exit 0 + no output must not score as a vacuous pass).
            let outText = rOut.Result.Replace("\r\n", "\n")
            let m = System.Text.RegularExpressions.Regex.Match(outText, @"===\s*(\d+) passed, (\d+) failed")
            let pPassed = if m.Success then int m.Groups.[1].Value else 0
            let pFailed = if m.Success then int m.Groups.[2].Value else 0
            let failNames =
                outText.Split('\n')
                |> Array.choose (fun l ->
                    let fm = System.Text.RegularExpressions.Regex.Match(l, @"^FAIL\s+(.+)$")
                    if fm.Success then Some (fm.Groups.[1].Value.Trim()) else None)
                |> Array.toList
            if not rExited then
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<run timeout>"] }
            elif not m.Success then
                printFooter blockName ["FAILED"]
                printfn "  no '=== p passed, f failed' summary in output -- cannot confirm any check ran"
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
                  FailedNames = ["<no summary line>"] }
            elif rproc.ExitCode = 0 && pFailed = 0 then
                // Self-checks green -> cross-diff the two independent stream
                // emitters (C++ --dump vs F# visitStream) while the exe is
                // fresh; the diff counts into this block's totals.
                let (dPassed, dFailed, dNames) = runDumpDiffs exePath cppDir
                printFooter blockName [ if dFailed = 0 then "all passed" else "FAILED" ]
                { Block = blockName; Passed = pPassed + dPassed; Failed = dFailed; Skipped = 0
                  FailedNames = dNames }
            else
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = pPassed
                  Failed = max pFailed 1
                  Skipped = 0
                  FailedNames = (if failNames.IsEmpty then ["<nonzero exit>"] else failNames) }
