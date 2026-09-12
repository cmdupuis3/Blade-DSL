// Differential-vs-oracle harness (plan Phase 4): run corpus programs
// through TWO compiler binaries — this one and a PINNED oracle build — and
// require byte-identical printed values (not merely identical pass/fail).
// This is the plan's central structural idea made into tooling: the v7
// prototype's validated behavior is the ground truth the evolving compiler
// is diffed against, value by value.
//
// Pinning an oracle: copy a fully-gated build to ./oracle, e.g.
//   Copy-Item bin\Release\net10.0 oracle -Recurse
// The harness skips cleanly (with that hint) when no oracle is pinned.
// Deliberately NOT part of the default suite: it g++-compiles every test
// twice. Run standalone: `blade test diff-oracle [category]`.
module Blade.Tests.DiffOracle

open System
open System.IO
open System.Diagnostics
open Blade.Build
open Blade.Tests.TestHarness
open Blade.Tests.Corpus

/// Phase 4's dense slice: literals, scalar bindings, dense method_for,
/// compute, printing. Categories grow as later phases claim more surface.
/// (The 2026-07-21 re-pin absorbed recursive-arrays and stack-join: the
/// oracle now parses `let rec` and compiles stack/join, so their former
/// capability-skips diff for real.)
let denseSlice = [ "basic"; "loops"; "guards"; "recursive-arrays"; "stack-join" ]

/// Corrected-semantics slice: corpus tests whose values INTENTIONALLY
/// diverge from the pinned oracle after a semantics correction — the
/// mechanism the plan calls for ("the differential test for that slice
/// should assert disagreement"). Divergence for a listed name CONFIRMS the
/// correction; agreement means the old unsound path did not fire for that
/// shape. Ground truth is the hand-computed EXPECT values in the corpus,
/// never the oracle.
///
/// EMPTY since the 2026-07-21 re-pin (post imperative-removal arc). The
/// three entries that served against the 2026-07-12 pin — "Fusion
/// Different Arrays" (merged-nest fusion fix) and the two "Mut Array
/// Copy" names (deep-copy semantics fix) — were retired with it: those
/// corrections are now IN the oracle and pinned by corpus EXPECTs, so the
/// divergences they asserted can no longer occur. (History: the arc-1
/// joint-product-symmetry names retired at the 2026-07-12 pin; the
/// signed-iteration loops/066 entry retired mid-arc when the file moved
/// to `let rec` and capability-skipped the old oracle.)
let correctedSlice : Set<string> = Set.empty

/// Run `<exe> run <srcFile>` and capture stdout. The generous timeout covers
/// the g++ compile that `blade run` performs internally.
let private runBlade (exePath: string) (srcFile: string) : Result<string, string> =
    try
        let psi = ProcessStartInfo(exePath, $"run \"{srcFile}\"")
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        use proc = Process.Start(psi)
        let outT = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let errT = Blade.Runtime.readToEndOffPool proc.StandardError
        if not (proc.WaitForExit(180000)) then
            (try proc.Kill() with _ -> ())
            Error "timed out (>180s)"
        elif proc.ExitCode <> 0 then
            Error ($"exit {proc.ExitCode}: {(errT.Result.Trim())}")
        else Ok outT.Result
    with ex -> Error ex.Message

/// Value lines only: timing lines vary run to run; line endings normalize.
/// Heap pointer addresses are masked: the struct printer currently renders an
/// array-typed field as its raw pointer (e.g. `samples: 0x1f3f4a878b0`), which
/// differs on EVERY run — without masking, structs/013 flakes against any
/// oracle. (Printing something useful instead of a pointer is a separate
/// printer backlog item.)
let private normalize (s: string) : string =
    s.Replace("\r\n", "\n").Split('\n')
    |> Array.filter (fun l -> not (l.Contains "completed in"))
    |> Array.map (fun l ->
        System.Text.RegularExpressions.Regex.Replace(l.TrimEnd(), "0x[0-9a-fA-F]+", "0xPTR"))
    |> String.concat "\n"
    |> fun t -> t.Trim()

/// Byte-exact comparison needs both sides built with contraction off, even
/// though the user-facing default is `fast` (Build.fs): the pin covers the
/// in-process compile directly and reaches the oracle subprocess via env
/// inheritance (older pinned oracles default to off on their own). Restored
/// on exit.
let private pinFpContractOff () =
    let prior = System.Environment.GetEnvironmentVariable("BLADE_FP_CONTRACT")
    System.Environment.SetEnvironmentVariable("BLADE_FP_CONTRACT", "off")
    { new System.IDisposable with
        member _.Dispose() =
            System.Environment.SetEnvironmentVariable("BLADE_FP_CONTRACT", prior) }

let runDiffOracleTests (oracleExe: string) (categories: string list) : BlockResult =
    printHeader "Differential vs Pinned Oracle"
    let blockName = "Diff Oracle"
    use _fpPin = pinFpContractOff ()
    if not (File.Exists oracleExe) then
        printfn "Skipped: no pinned oracle at %s" (Path.GetFullPath oracleExe)
        printfn "         Pin one from a fully-gated build:  Copy-Item bin\\Release\\net10.0 oracle -Recurse"
        { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    elif not capabilities.Value.HasGpp then
        printfn "Skipped: requires g++."
        { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    else
        let thisExe = Environment.ProcessPath
        printfn "current: %s" thisExe
        printfn "oracle:  %s" (Path.GetFullPath oracleExe)
        let tmpRoot = Path.Combine(Path.GetTempPath(), "blade_diff_oracle")
        let mineDir = Path.Combine(tmpRoot, "mine")
        let theirsDir = Path.Combine(tmpRoot, "theirs")
        (try Directory.Delete(tmpRoot, true) with _ -> ())
        Directory.CreateDirectory(mineDir) |> ignore
        Directory.CreateDirectory(theirsDir) |> ignore
        let mutable passed = 0
        let mutable failed = 0
        let mutable skipped = 0
        let mutable failedNames : string list = []
        for cat in categories do
            printSubHeader ($"category: {cat}")
            for (name, source) in category cat do
                if name.EndsWith "(rejects)" || name.EndsWith "(aborts)" then
                    // Neither shape has values to diff: a reject-probe never
                    // reaches codegen, and an `(aborts)` test terminates non-zero BY
                    // DESIGN (its `// ABORT:` pin is what the main harness checks),
                    // which `runBlade` cannot distinguish from a real failure.
                    skipped <- skipped + 1
                else
                    let safe = sanitizeFileName name + ".blade"
                    let mineSrc = Path.Combine(mineDir, safe)
                    let theirsSrc = Path.Combine(theirsDir, safe)
                    File.WriteAllText(mineSrc, source)
                    File.WriteAllText(theirsSrc, source)
                    match runBlade thisExe mineSrc, runBlade oracleExe theirsSrc with
                    | Ok mine, Ok theirs when normalize mine = normalize theirs ->
                        passed <- passed + 1
                        if Set.contains name correctedSlice then
                            resultLine Pass name "oracle agrees (old unsound path did not fire for this shape)"
                        else
                            resultLine Pass name "values identical"
                    | Ok mine, Ok theirs when Set.contains name correctedSlice ->
                        // Corrected-semantics slice: this divergence is the point.
                        passed <- passed + 1
                        resultLine Pass name "INTENTIONAL divergence from pinned oracle confirmed (corrected semantics; corpus EXPECTs are ground truth)"
                    | Ok mine, Ok theirs ->
                        failed <- failed + 1
                        failedNames <- failedNames @ [name]
                        resultLine Fail name "VALUES DIVERGE from oracle"
                        printfn "    current: %s" ((normalize mine).Split('\n') |> Array.truncate 3 |> String.concat " | ")
                        printfn "    oracle:  %s" ((normalize theirs).Split('\n') |> Array.truncate 3 |> String.concat " | ")
                    | Error e, _ ->
                        failed <- failed + 1
                        failedNames <- failedNames @ [name]
                        resultLine Fail name ($"current binary failed: {e}")
                    | _, Error e ->
                        // Oracle can't run it (e.g. feature added after pinning):
                        // that's a skip with a note, not a divergence.
                        skipped <- skipped + 1
                        resultLine Skip name ($"oracle failed: {e}")
        printFooter blockName
            [ $"{passed} passed"; $"{failed} failed"; $"{skipped} skipped" ]
        { Block = blockName; Passed = passed; Failed = failed; Skipped = skipped; FailedNames = failedNames }
