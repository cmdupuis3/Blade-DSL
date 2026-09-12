// Standalone C++ allocation-layout tests (cpp/alloc_layout_tests.cpp):
// contiguity, DFS leaf ordering, closed-form cardinality. Extracted
// verbatim from Main.fs (audit §2.3).
module Blade.Tests.AllocTests

open System
open Blade
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Blade.Build
open Blade.Tests.TestHarness

/// Run the standalone C++ allocation-layout test suite (cpp/alloc_layout_tests.cpp).
///
/// These tests verify runtime-layout invariants of the contiguous-backing
/// allocate<> that the value-checking Blade tests structurally cannot catch:
/// single-pool contiguity, DFS leaf ordering, and closed-form cardinality.
/// They are C++ (the property under test is a C++ runtime invariant), so this
/// runs them directly rather than through the Blade source pipeline — the same
/// category as `test normalize` / `test unify`, just in C++.
///
/// The test .cpp and the runtime headers are both shipped in cpp/ next to the
/// compiler binary (AppContext.BaseDirectory/cpp), copied there by Blade.fsproj.
/// Compiling in that directory means the test exercises the EXACT headers the
/// codegen path uses — not a stale copy — which is the point of syncing it here.
///
/// Returns 0 on all-pass or skip (g++ absent); 1 on any compile/run/check failure.
let runAllocLayoutTests () : Blade.Tests.TestHarness.BlockResult =
    let cppDir = Path.Combine(AppContext.BaseDirectory, "cpp")
    let testSrc = Path.Combine(cppDir, "alloc_layout_tests.cpp")
    let caps = capabilities.Value
    printHeader "Allocation Layout Tests"
    let blockName = "Alloc Layout"
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile C++ layout tests)."
        // Skipped = 1, not 0: a Skipped = 0 return made a toolchain-less box
        // print "0 passed, 0 failed" for this block, indistinguishable from a
        // block that genuinely had nothing to do. Same convention as
        // DiffOracle/InterpDiff.
        { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    elif not (File.Exists testSrc) then
        eprintfn "alloc_layout_tests.cpp not found at: %s" testSrc
        eprintfn "Check that Blade.fsproj copies cpp/alloc_layout_tests.cpp to the output dir."
        { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["alloc_layout_tests.cpp missing"] }
    else
        let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
        let exePath = Path.ChangeExtension(testSrc, exeExt)
        // Compile in cppDir so #include "nested_array_utilities.hpp" resolves to
        // the shipped headers, exactly as g++ resolves them for generated tests.
        let args = $"-std=c++17 {(optFlags ())} -o \"{exePath}\" \"{testSrc}\""
        let psi = ProcessStartInfo("g++", args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        psi.WorkingDirectory <- cppDir
        use cproc = Process.Start(psi)
        let cOut = Blade.Runtime.readToEndOffPool cproc.StandardOutput
        let cErr = Blade.Runtime.readToEndOffPool cproc.StandardError
        // WaitForExit(ms) returns false on TIMEOUT; ExitCode is meaningless then.
        let cExited = cproc.WaitForExit(60000)
        if not cExited then
            (try cproc.Kill(true) with _ -> ())
            printfn "C++ compilation TIMED OUT (60s)"
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile timeout>"] }
        elif cproc.ExitCode <> 0 then
            printfn "C++ compilation FAILED:"
            printfn "%s" (cOut.Result + "\n" + cErr.Result)
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile failed>"] }
        else
            // Run the compiled test; stream its [PASS]/[FAIL] lines through.
            let rpsi = ProcessStartInfo(exePath)
            rpsi.RedirectStandardOutput <- true
            rpsi.RedirectStandardError <- true
            rpsi.UseShellExecute <- false
            rpsi.CreateNoWindow <- true
            rpsi.WorkingDirectory <- cppDir
            use rproc = Process.Start(rpsi)
            let rOut = Blade.Runtime.readToEndOffPool rproc.StandardOutput
            let rErr = Blade.Runtime.readToEndOffPool rproc.StandardError
            let rExited = rproc.WaitForExit(30000)
            if not rExited then
                (try rproc.Kill(true) with _ -> ())
                // Let the kill settle so ExitCode below is readable.
                rproc.WaitForExit(5000) |> ignore
                printfn "alloc layout test binary TIMED OUT (30s)"
            printf "%s" rOut.Result
            if not (String.IsNullOrWhiteSpace rErr.Result) then eprintf "%s" rErr.Result
            // Parse the "ALLOC TESTS: p/n passed" summary for the grand total,
            // and collect the names of any "[FAIL]: <name>" lines. Exit code is
            // still the source of truth for the verdict.
            let outText = rOut.Result.Replace("\r\n", "\n")
            let mutable pPassed = 0
            let mutable pTotal = 0
            let m = System.Text.RegularExpressions.Regex.Match(outText, @"ALLOC TESTS:\s*(\d+)/(\d+)\s*passed")
            if m.Success then
                pPassed <- int m.Groups.[1].Value
                pTotal <- int m.Groups.[2].Value
            let failNames =
                outText.Split('\n')
                |> Array.choose (fun l ->
                    let fm = System.Text.RegularExpressions.Regex.Match(l, @"\[FAIL\]:\s*(.+)$")
                    if fm.Success then Some (fm.Groups.[1].Value.Trim()) else None)
                |> Array.toList
            let pFailed = if pTotal >= pPassed then pTotal - pPassed else failNames.Length
            // Exit code is the source of truth (0 iff all checks passed) -- but
            // only once we know the binary actually ran its checks. Without the
            // summary line, exit 0 + no output scored `Passed = 0; Failed = 0`,
            // i.e. a silent vacuous pass for the whole block.
            if not rExited then
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<run timeout>"] }
            elif not m.Success then
                printFooter blockName ["FAILED"]
                printfn "  no 'ALLOC TESTS: p/n passed' summary in output -- cannot confirm any check ran"
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
                  FailedNames = ["<no ALLOC TESTS summary line>"] }
            elif rproc.ExitCode = 0 then
                printFooter blockName ["all passed"]
                { Block = blockName; Passed = pPassed; Failed = 0; Skipped = 0; FailedNames = [] }
            else
                printFooter blockName ["FAILED"]
                // A nonzero exit must cost at least one Failed even when the
                // summary claims p = n (e.g. the binary aborted after printing).
                { Block = blockName; Passed = pPassed; Failed = max 1 pFailed; Skipped = 0
                  FailedNames = (if failNames.IsEmpty then [$"<exit {rproc.ExitCode}>"] else failNames) }
