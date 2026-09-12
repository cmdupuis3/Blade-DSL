// The BLADE_LLVM lane's own test block (`blade test llvm`).
//
// Four gates, because the lane can drift in four independent ways:
//
//   0. COMPARE RULES (runLlvmCompareRuleTests) -- assertions on the output
//      comparator itself, in both directions: what it must accept AND what it
//      must reject. A differential that cannot say no is decoration, and the
//      specific way this lane's earlier ad-hoc sweeps broke was a silently
//      empty capture reading as 100% agreement.
//
//   1. EMISSION PINS (runLlvmGoldenTests) -- three tiny programs under
//      tests/fixtures/llvm/ whose emitted .ll text is pinned byte for byte
//      against a committed golden. Needs NO toolchain: it is pure emission, so
//      it asserts something even on a machine with neither clang nor g++. The
//      pins are against OUR OWN output, not against any external notion of
//      correct IR -- their job is to make an emitter change LOUD, not to judge
//      it. Regenerate deliberately with BLADE_LLVM_GOLDEN_UPDATE=1.
//
//   2. FACT LAYER (runLlvmFactTests) -- what the emission pins structurally
//      cannot say. A pin records ONE environment and reports drift as "line 14
//      differs"; this block names each fact and each knob, and asserts every
//      one in both directions, because the fact-layer failures that matter
//      (`reassoc` on a map, `readonly` on a written pool) change no output
//      until an optimizer believes them. Toolchain-free, like the pins.
//
//   3. DIFFERENTIAL (runLlvmDifferentialTests) -- for each .blade file of a
//      configurable corpus category list, compile and run BOTH back ends and
//      compare stdout. This is the gate that says the lane is CORRECT, and it
//      is deliberately a lane-vs-lane comparison rather than a re-check of the
//      corpus EXPECT pins: the C++ lane is the byte-pinned oracle already, so
//      agreeing with it is the strictest available statement, and it covers
//      every printed value including the ones no pin mentions.
//
// Why this block drives the two back ends DIRECTLY instead of setting
// BLADE_LLVM and calling the ordinary runner: the whole existing harness
// reaches codegen through `CliCommands.compileFile` /
// `CodeGen.genSelfContainedProgramFromIR`, which are the C++ lane by
// construction -- BLADE_LLVM has no effect on any of it, and `CliCommands` is
// compiled AFTER the test modules anyway, so it is not even nameable here. So
// this file owns a two-lane driver: one front end pass per lane, then
// `EmitLlvm.tryEmitProgramNamed` on one side and
// `CodeGen.genSelfContainedProgramFromIR` on the other.
//
// SKIP POLICY (the house rule: a missing capability skips, never fails):
//   * no clang            -> the whole differential block skips (Skipped = 1).
//   * no g++              -> ditto; without the oracle there is nothing to
//                            compare against.
//   * llvm lane REFUSES   -> that file skips, named, and its reason is counted
//                            into the histogram printed at the end. A refusal
//                            is the lane working as designed (whole-program or
//                            nothing), so it must never redden the block.
//   * front end refuses   -> that file skips (reject probes live in these
//                            categories too).
// A MISMATCH is the only interesting failure, and it fails loudly with the
// first three differing tokens and the paths of both kept artifacts.
module Blade.Tests.LlvmTests

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Blade
open Blade.Tests.TestHarness

// ---------------------------------------------------------------------------
// Output comparison: tokenized near-equality
// ---------------------------------------------------------------------------

/// Split program output into comparison tokens. Whitespace separates, and the
/// printer's structural punctuation is split OFF into tokens of its own, so a
/// number never carries a bracket into its parse: `x = [1, 2]` tokenizes as
/// `x` `=` `[` `1` `,` `2` `]` rather than as `[1,` and `2]`. A minus sign is
/// deliberately NOT a separator -- it belongs to the number it precedes.
let private tokenize (s: string) : string[] =
    let sb = Text.StringBuilder()
    let toks = ResizeArray<string>()
    let flush () =
        if sb.Length > 0 then
            toks.Add(sb.ToString())
            sb.Clear() |> ignore
    for ch in s do
        if Char.IsWhiteSpace ch then flush ()
        elif ch = '[' || ch = ']' || ch = '(' || ch = ')' || ch = ',' || ch = '=' then
            flush ()
            toks.Add(string ch)
        else sb.Append ch |> ignore
    flush ()
    toks.ToArray()

/// Parse a token as a FINITE double, or None. `nan`/`inf`/`-inf` deliberately
/// come back None so they fall through to the exact-string path: comparing
/// them numerically would make NaN unequal to itself, and would let `inf`
/// match `-inf` through no arithmetic anyone wants.
let private parseNum (t: string) : float option =
    match Double.TryParse(t, Globalization.NumberStyles.Float,
                          Globalization.CultureInfo.InvariantCulture) with
    | true, v when not (Double.IsNaN v) && not (Double.IsInfinity v) -> Some v
    | _ -> None

/// Near-equality for one token pair: identical text always passes; two finite
/// numbers pass within relative 1e-9, falling back to absolute 1e-12 once both
/// magnitudes are below 1e-9 (where a relative test is meaningless).
///
/// ZERO IS EXCLUDED FROM THE TOLERANCE on purpose. `0` and `-0` are one ULP
/// apart and would pass any absolute test, but the two lanes print them
/// DIFFERENTLY by design (the printer census pins `-0.0` to "-0"), so a signed
/// zero divergence is exactly the kind of printer drift this block exists to
/// catch. Both-zero therefore demands identical text, which the fast path
/// above already granted when it is true.
let private tokenEq (a: string) (b: string) : bool =
    if String.Equals(a, b, StringComparison.Ordinal) then true
    else
        match parseNum a, parseNum b with
        | Some x, Some y when x = 0.0 && y = 0.0 -> false   // signed zero: text decides
        | Some x, Some y ->
            let scale = max (abs x) (abs y)
            if scale < 1e-9 then abs (x - y) <= 1e-12
            else abs (x - y) <= 1e-9 * scale
        | _ -> false

/// Drop the `<name> completed in <t>s` line both lanes print. It is a wall
/// clock reading and never matches.
let private stripTiming (s: string) : string =
    s.Replace("\r\n", "\n").Trim().Split('\n')
    |> Array.filter (fun l -> not (l.Contains "completed in"))
    |> String.concat "\n"

/// Compare two runs. Ok () or the first `maxDiffs` differing token positions,
/// rendered as `#<idx> cpp=<tok> llvm=<tok>`. A length difference is reported
/// at the position where one side ran out, with `<end>` for the missing token.
let private compareRuns (maxDiffs: int) (cppOut: string) (llvmOut: string) : Result<unit, string list> =
    let a = tokenize (stripTiming cppOut)
    let b = tokenize (stripTiming llvmOut)
    let n = max a.Length b.Length
    let diffs = ResizeArray<string>()
    let mutable i = 0
    while i < n && diffs.Count < maxDiffs do
        let ta = if i < a.Length then a.[i] else "<end>"
        let tb = if i < b.Length then b.[i] else "<end>"
        if ta = "<end>" || tb = "<end>" || not (tokenEq ta tb) then
            diffs.Add($"#{i} cpp={ta} llvm={tb}")
        i <- i + 1
    if diffs.Count = 0 then Ok () else Error (List.ofSeq diffs)

// ---------------------------------------------------------------------------
// Block 0: the comparator's own rules
// ---------------------------------------------------------------------------
//
// A differential block is only worth its runtime if its comparator can say NO.
// The failure mode that motivates this block is specific and has already
// happened once on this lane: a driver whose output capture was silently broken
// compared "" with "" and reported 100% agreement. Every rule below is
// therefore asserted in BOTH directions -- what must match, and what must not.

/// Assertions over `compareRuns`, run in-process with no toolchain at all.
let runLlvmCompareRuleTests () : BlockResult =
    printHeader "LLVM Compare Rules"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let check (name: string) (cpp: string) (llvm: string) (shouldMatch: bool) =
        let got = compareRuns 3 cpp llvm
        match got, shouldMatch with
        | Ok (), true ->
            passed <- passed + 1
            resultLine Pass name "agrees"
        | Error diffs, false ->
            passed <- passed + 1
            resultLine Pass name ($"differs at {(List.head diffs)}")
        | Ok (), false ->
            failed <- failed + 1
            failedNames <- failedNames @ [ name ]
            resultLine Fail name "the comparator accepted output it must reject"
        | Error diffs, true ->
            failed <- failed + 1
            failedNames <- failedNames @ [ name ]
            resultLine Fail name ($"""the comparator rejected matching output: {(String.concat " | " diffs)}""")
    // The trap this block exists for: an output capture that silently returns
    // nothing must NOT read as agreement with a program that printed values.
    check "empty vs printed" "" "x = 1" false
    check "printed vs empty" "x = 1" "" false
    check "both empty" "" "" true
    // Exact agreement, including through the printer's punctuation.
    check "identical scalars" "x = 42\ny = true" "x = 42\ny = true" true
    check "identical arrays" "x = [1, 2, 3]" "x = [1, 2, 3]" true
    check "identical nested rank 2" "x = [[1, 2], [3, 4]]" "x = [[1, 2], [3, 4]]" true
    // The wall-clock line differs on every single run and must be invisible.
    check "timing line ignored" "p completed in 1e-07s\nx = 1" "p completed in 9.5s\nx = 1" true
    // Relative tolerance: 1e-10 apart passes, 1e-3 apart does not.
    check "relative 1e-10 apart" "x = 1" "x = 1.0000000001" true
    check "relative 1e-3 apart" "x = 1" "x = 1.001" false
    // The real near-equal this lane produces: the C++ oracle's FMA residual
    // against the llvm lane's exact zero (corpus loops/143). Absolute floor.
    check "fma residual vs exact zero" "d = -3.5527136788005e-15" "d = 0" true
    check "absolute floor respected" "d = 1e-9" "d = 2e-9" false
    // Non-finite tokens compare as TEXT, never as arithmetic.
    check "nan equals nan" "x = nan" "x = nan" true
    check "nan differs from zero" "x = nan" "x = 0" false
    check "inf differs from -inf" "x = inf" "x = -inf" false
    check "inf equals inf" "x = inf" "x = inf" true
    // Signed zero is a printer fact, not a numeric one: the census pins -0.0
    // to "-0", so the two lanes disagreeing here is a real regression.
    check "signed zero is a difference" "x = 0" "x = -0" false
    // Structure: element values, element COUNT, and non-numeric text.
    check "array element differs" "x = [1, 2, 3]" "x = [1, 9, 3]" false
    check "array length differs" "x = [1, 2, 3]" "x = [1, 2]" false
    check "binding name differs" "x = 1" "y = 1" false
    check "bool differs" "x = true" "x = false" false
    check "string differs" "s = hello" "s = world" false
    printFooter "LLVM Compare Rules"
        [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = "LLVM Compare Rules"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = failedNames }

// ---------------------------------------------------------------------------
// Skip-reason histogram
// ---------------------------------------------------------------------------

/// Collapse a skip/refusal reason into a bucket key: quoted identifiers become
/// `'~'` and digit runs become `#`, so `the binding 'arr1d' has type
/// Array<Float64 like Idx<3>>` and its thousand siblings land in ONE row.
/// Truncated, because refusal reasons are full sentences by design.
let private reasonKey (r: string) : string =
    let sb = Text.StringBuilder()
    let mutable inQuote = false
    let mutable prevHash = false
    for ch in r do
        if ch = '\'' then
            if not inQuote then sb.Append "'~'" |> ignore
            inQuote <- not inQuote
            prevHash <- false
        elif inQuote then ()
        elif Char.IsDigit ch then
            if not prevHash then sb.Append '#' |> ignore
            prevHash <- true
        else
            sb.Append ch |> ignore
            prevHash <- false
    let s = sb.ToString().Trim()
    if s.Length > 78 then s.Substring(0, 78) + "..." else s

/// Print the reason histogram, most frequent first. This is the block's real
/// coverage report: every row is a construct the lane does not yet emit.
let private printReasonHistogram (reasons: string list) =
    if not reasons.IsEmpty then
        printfn ""
        printfn "  Skip reasons (%d skipped, by frequency):" reasons.Length
        reasons
        |> List.countBy reasonKey
        |> List.sortByDescending snd
        |> List.iter (fun (k, n) -> printfn "    %4d  %s" n k)

// ---------------------------------------------------------------------------
// Process helper
// ---------------------------------------------------------------------------

/// Run a produced executable capturing stdout and stderr SEPARATELY (the
/// shared `Build.runExecutable` merges them, which would make a stderr-only
/// difference read as a stdout mismatch). 60s guard: every corpus program in
/// these categories finishes in well under a second, so a longer run is a hang,
/// and a hung differential sweep is worse than a failed one.
let private runExeCapture (exeFile: string) : Result<int * string * string, string> =
    try
        let full = Path.GetFullPath exeFile
        let psi = ProcessStartInfo(full)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        psi.WorkingDirectory <- Path.GetDirectoryName full
        use proc = Process.Start psi
        let outTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let errTask = Blade.Runtime.readToEndOffPool proc.StandardError
        if proc.WaitForExit 60000 then Ok (proc.ExitCode, outTask.Result, errTask.Result)
        else
            (try proc.Kill() with _ -> ())
            Error "execution timed out after 60s"
    with ex -> Error ($"execution exception: {ex.Message}")

// ---------------------------------------------------------------------------
// Block 1: emission golden pins
// ---------------------------------------------------------------------------

/// Directory holding the pinned programs and their .ll goldens. Same two-root
/// rule as tests/Corpus.fs: the source tree wins when running from the repo
/// root (so a pin edit takes effect with no rebuild), the deployed copy
/// answers otherwise.
let private goldenRoot : Lazy<string> =
    lazy
        let candidates =
            [ Path.Combine(".", "tests", "fixtures", "llvm")
              Path.Combine(AppContext.BaseDirectory, "tests", "fixtures", "llvm") ]
        match candidates |> List.tryFind Directory.Exists with
        | Some d -> d
        | None ->
            failwith $"""llvm golden fixtures not found. Looked in: {(candidates |> List.map Path.GetFullPath |> String.concat " ; ")}"""

/// The pinned programs, in report order. Scalars, an outlined function with a
/// branch, and a dense array with a fold -- one representative of each emission
/// shape the lane has.
let private goldenNames = [ "scalar_arith"; "function_branch"; "array_reduce" ]

/// Env gate, read per call (never a module-level `let`, per the repo's
/// environment discipline): rewrite the goldens instead of asserting them.
let private goldenUpdateEnabled () : bool =
    match Environment.GetEnvironmentVariable "BLADE_LLVM_GOLDEN_UPDATE" with
    | "1" | "on" -> true
    | _ -> false

/// Line endings are normalized on BOTH sides before comparing. The emitter
/// joins with '\n', but `core.autocrlf=true` gives this repo CRLF working
/// trees, so a literal byte comparison would fail on a fresh checkout for a
/// reason that has nothing to do with the emitter. Everything else -- spacing,
/// register numbering, declaration order, blank lines -- is compared exactly.
let private normalizeLl (s: string) = s.Replace("\r\n", "\n")

/// Run `body` with a set of environment variables pinned, restoring every one
/// afterwards (including back to UNSET, which `null` does).
///
/// Needed by two blocks for two different reasons: the emission pins are
/// against the DEFAULT numeric policy, so a developer whose shell happens to
/// carry `BLADE_FP_REASSOC=1` must not see three phantom failures; and the
/// fact block asserts each knob's effect, which means setting it.
let private withEnv (pins: (string * string option) list) (body: unit -> 'a) : 'a =
    let saved = pins |> List.map (fun (k, _) -> k, Environment.GetEnvironmentVariable k)
    for (k, v) in pins do
        Environment.SetEnvironmentVariable(k, (match v with Some s -> s | None -> null))
    try body ()
    finally for (k, v) in saved do Environment.SetEnvironmentVariable(k, v)

/// The knobs that change emitted text. Pinned OFF around any assertion about
/// default emission; listed once so a knob added later is added in one place.
let private numericKnobs =
    [ "BLADE_FP_REASSOC"; "BLADE_FP_CONTRACT"; "BLADE_LLVM_FACTS"; "BLADE_LLVM_BRICKS" ]

let private defaultEmissionEnv = numericKnobs |> List.map (fun k -> k, None)

let runLlvmGoldenTests () : BlockResult =
    printHeader "LLVM Emission Pins"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let updating = goldenUpdateEnabled ()
    if updating then
        printfn "BLADE_LLVM_GOLDEN_UPDATE is on: pins are being REWRITTEN, not asserted.\n"
    let pass name detail =
        passed <- passed + 1
        resultLine Pass name detail
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [name]
        resultLine Fail name detail
    let root = goldenRoot.Value
    for name in goldenNames do
        let srcPath = Path.Combine(root, name + ".blade")
        let pinPath = Path.Combine(root, name + ".ll")
        try
            if not (File.Exists srcPath) then
                fail name ($"missing pinned program {srcPath}")
            else
                let source = File.ReadAllText srcPath
                match Lowering.lower source with
                | Error e -> fail name ($"lowering refused the pinned program: {e}")
                | Ok ir ->
                    match IRValidate.validateIR ir with
                    | Error errs -> fail name ($"""IR validation refused: {(String.concat "; " errs)}""")
                    | Ok ir ->
                        // The program NAME reaches the emitted text (it labels
                        // the timing line), so it must be the fixture stem here
                        // and in the update path alike, or the pin would never
                        // match what `blade emit` produces for the same file.
                        // The pins record DEFAULT emission, so the numeric
                        // knobs are forced off for the call itself -- an
                        // inherited BLADE_FP_REASSOC would otherwise fail all
                        // three pins for a reason that is not a drift.
                        match withEnv defaultEmissionEnv (fun () -> EmitLlvm.tryEmitProgramNamed name ir) with
                        | Error reason ->
                            // A pinned program is chosen to be inside the
                            // lane's coverage. Refusing one is a regression,
                            // not a skip.
                            fail name ($"the llvm lane refused a PINNED program: {reason}")
                        | Ok ll when updating ->
                            File.WriteAllText(pinPath, ll)
                            pass name ($"pin rewritten ({(normalizeLl ll |> fun s -> s.Split('\n').Length)} lines)")
                        | Ok ll ->
                            if not (File.Exists pinPath) then
                                fail name ($"no pin at {pinPath} -- run with BLADE_LLVM_GOLDEN_UPDATE=1 to create it")
                            else
                                let expected = normalizeLl (File.ReadAllText pinPath)
                                let actual = normalizeLl ll
                                if expected = actual then
                                    pass name ($"{(actual.Split('\n').Length)} lines pinned")
                                else
                                    // Park the actual beside the pin so the
                                    // reader can diff two files instead of
                                    // reading a truncated message.
                                    let actualPath = pinPath + ".actual"
                                    (try File.WriteAllText(actualPath, ll) with _ -> ())
                                    let el = expected.Split('\n')
                                    let al = actual.Split('\n')
                                    let firstDiff =
                                        let n = max el.Length al.Length
                                        let mutable i = 0
                                        let mutable found = -1
                                        while i < n && found < 0 do
                                            let e = if i < el.Length then el.[i] else "<end>"
                                            let a = if i < al.Length then al.[i] else "<end>"
                                            if e <> a then found <- i
                                            i <- i + 1
                                        found
                                    let e = if firstDiff < el.Length then el.[firstDiff] else "<end>"
                                    let a = if firstDiff < al.Length then al.[firstDiff] else "<end>"
                                    fail name
                                        (sprintf "emission drifted at line %d: pinned %s | emitted %s (wrote %s; BLADE_LLVM_GOLDEN_UPDATE=1 re-pins)"
                                            (firstDiff + 1) e a actualPath)
        with ex -> fail name ($"exception: {ex.Message}")
    printFooter "LLVM Emission Pins"
        [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = "LLVM Emission Pins"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = failedNames }

// ---------------------------------------------------------------------------
// Block 2: the fact layer
// ---------------------------------------------------------------------------
//
// The emission pins above would catch a fact-layer change, but only as "line
// 14 differs" -- they cannot say WHICH fact moved, and they say nothing at all
// about the three knobs, because a pin records exactly one environment. This
// block asserts each fact and each knob by name, in BOTH directions: the text
// that must appear, and the text that must NOT.
//
// The negative half is the load-bearing half. `reassoc` on a map's `fmul` and
// `readonly` on a written-through pool are both silent miscompiles: they
// change no output until an optimizer believes them, and then they change the
// wrong one. So every fact here is paired with a program where it must be
// absent.

/// Emit one source under a pinned environment, or fail loudly. Emission only:
/// no toolchain is touched, so this block runs anywhere.
let private emitUnder (pins: (string * string option) list) (name: string) (source: string) : Result<string, string> =
    withEnv (defaultEmissionEnv @ pins) (fun () ->
        match Lowering.lower source with
        | Error e -> Error ("lowering refused: " + e)
        | Ok ir ->
            match IRValidate.validateIR ir with
            | Error errs -> Error ("IR validation refused: " + String.concat "; " errs)
            | Ok ir ->
                match EmitLlvm.tryEmitProgramNamed name ir with
                | Error r -> Error ("the llvm lane refused: " + r)
                | Ok ll -> Ok ll)

/// A licensed fold (`(+)` is a recognized builtin body, so `foldReorderLicensed`
/// holds) over a map whose arithmetic carries NO license -- both shapes in one
/// program, which is what makes "exactly the accumulator chain" checkable.
let private licensedFoldSource = """
type Ix = Idx<8>
let xs: Array<Float like Ix> = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let scaled = xs * 0.5
let total = reduce(scaled, (+))
"""

/// An UNLICENSED fold: the kernel is neither a recognized builtin body nor
/// `comm`-declared, so the knob must not reach it.
let private unlicensedFoldSource = """
type Ix = Idx<8>
function skew(a: Float64, b: Float64) -> Float64 = { a * 0.75 + b }
let xs: Array<Float like Ix> = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let total = reduce(xs, skew)
"""

/// An array parameter nothing writes through (`readonly` must appear) next to
/// a self-recursive function (`norecurse` must not, and the module loses its
/// termination claim).
let private readonlyAndRecursionSource = """
type Ix = Idx<6>
function total(row: T^1) -> T^0 = reduce(row, (+))
function fact(n: Int64) -> Int64 = { if n <= 1 then 1 else n * fact(n - 1) }
let xs: Array<Float like Ix> = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let s = total(xs)
let f = fact(5)
"""

/// A module WITH an element write: no array parameter anywhere may claim
/// `readonly`, not even the one that is only read.
/// Written in the corpus's own spelling (`Idx<3>` inline, not a named alias):
/// a NAMED index type would want a tagged subscript and this block's output
/// would carry three BL4003 warnings that have nothing to do with the facts.
let private elementWriteSource = """
function accumulate_into(x: Array<Float like Idx<3>>, out: mut Array<Float like Idx<3>>) -> Float = {
    out(0) = x(0)
    x(0)
}
let src = [1.0, 2.0, 3.0]
let mut dst = [0.0, 0.0, 0.0]
let r = accumulate_into(src, dst)
"""

/// Drop `;` comment lines before asserting on emitted text. The module banner
/// NAMES the knobs it documents ("per-instruction `contract`"), so a bare
/// substring search over the whole file would find `contract` in a module that
/// emitted none -- a false pass in the direction that matters least and a
/// false failure in the direction that matters most.
let private instructionsOnly (ll: string) =
    ll.Replace("\r\n", "\n").Split('\n')
    |> Array.filter (fun l -> not (l.TrimStart().StartsWith ";"))
    |> String.concat "\n"

let runLlvmFactTests () : BlockResult =
    printHeader "LLVM Fact Layer"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let pass name detail =
        passed <- passed + 1
        resultLine Pass name detail
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [ name ]
        resultLine Fail name detail
    /// `name`: what is asserted. `want`/`deny`: substrings that must appear /
    /// must not. Both lists are checked, so one case states a fact and its
    /// boundary together.
    ///
    /// The emitted PROGRAM NAME is a fixed "facts", never the case name: the
    /// name becomes a string constant in the module (it labels the timing
    /// line), so a case called "contract=off is off" would plant the word
    /// `contract` in its own module and fail itself. That is not hypothetical
    /// -- it is how the first run of this block failed.
    let check name pins source (want: string list) (deny: string list) =
        match emitUnder pins "facts" source with
        | Error e -> fail name e
        | Ok raw ->
            let ll = instructionsOnly raw
            let missing = want |> List.filter (fun s -> not (ll.Contains s))
            let present = deny |> List.filter ll.Contains
            if not missing.IsEmpty then
                fail name ($"""missing from the emitted .ll: {(String.concat " | " missing)}""")
            elif not present.IsEmpty then
                fail name ($"""present in the emitted .ll but must not be: {(String.concat " | " present)}""")
            else
                pass name ($"{want.Length} asserted, {deny.Length} denied")

    printSubHeader "the kill switch"
    // The switch's own rules, asserted directly rather than through emission:
    // a bisection knob that lies about which class it disabled is worse than
    // no knob.
    let switchCase name (value: string option) (expect: (string * bool) list) =
        withEnv [ "BLADE_LLVM_FACTS", value ] (fun () ->
            let wrong =
                expect
                |> List.filter (fun (cls, want) -> EmitLlvm.factEnabled cls <> want)
                |> List.map fst
            if wrong.IsEmpty then pass name "as declared"
            else fail name ($"""wrong answer for: {(String.concat ", " wrong)}"""))
    switchCase "unset licenses every class" None
        [ "fnattrs", true; "paramattrs", true; "fmf", true ]
    switchCase "a bare off kills every class" (Some "off")
        [ "fnattrs", false; "paramattrs", false; "fmf", false ]
    switchCase "0 is a spelling of off" (Some "0")
        [ "fnattrs", false; "paramattrs", false; "fmf", false ]
    switchCase "class:off kills exactly one class" (Some "fmf:off")
        [ "fnattrs", true; "paramattrs", true; "fmf", false ]
    switchCase "two classes at once" (Some "fmf:off,paramattrs:off")
        [ "fnattrs", true; "paramattrs", false; "fmf", false ]
    switchCase "an unrecognized token disables nothing" (Some "banana")
        [ "fnattrs", true; "paramattrs", true; "fmf", true ]

    printSubHeader "function and parameter attributes"
    check "a non-recursive module claims termination" [] licensedFoldSource
        [ "define i32 @main() #0"
          "attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn }" ]
        // `noalias` on anything but the allocator's return is the miscompile
        // this lane is most exposed to; assert it appears nowhere else.
        [ "ptr noalias %a"; "define internal noalias" ]
    check "the allocator's return is the one noalias" [] licensedFoldSource
        [ "declare noalias align 64 ptr @blade_alloc_cells(i64 noundef, i64 noundef)" ] []
    check "recursion costs the module its termination claim" [] readonlyAndRecursionSource
        [ "attributes #1 = { nofree norecurse nosync nounwind }"   // the callers
          "attributes #2 = { nofree nosync nounwind }"             // fact itself
          "ptr noundef readonly %a0" ]                             // and it is read-only
        // Group 0's text is the termination claim; nothing may carry it while
        // a possibly-non-terminating callee is in the module.
        [ "norecurse nosync nounwind willreturn" ]
    check "an element write anywhere denies every readonly" [] elementWriteSource
        [ "ptr noundef %a0" ] [ "readonly" ]
    check "paramattrs:off strips the parameter facts" [ "BLADE_LLVM_FACTS", Some "paramattrs:off" ]
        readonlyAndRecursionSource
        [ "attributes #1 ="; "(ptr %a0)" ] [ "noundef"; "readonly"; "noalias" ]
    check "fnattrs:off strips the groups" [ "BLADE_LLVM_FACTS", Some "fnattrs:off" ]
        licensedFoldSource
        [ "define i32 @main() {" ] [ "attributes #" ]

    printSubHeader "fast-math flags"
    check "default emission is flag-free" [] licensedFoldSource
        [ "fadd double"; "fmul double" ] [ "reassoc"; "nsz"; "contract" ]
    check "the license reaches exactly the accumulator chain"
        [ "BLADE_FP_REASSOC", Some "1" ] licensedFoldSource
        // The fold's add is tagged; the map's multiply -- same program, same
        // element type, no license -- is not.
        [ "fadd reassoc nsz double"; "fmul double" ] [ "fmul reassoc" ]
    check "an unlicensed kernel stays ordered with the knob on"
        [ "BLADE_FP_REASSOC", Some "1" ] unlicensedFoldSource
        [ "fadd double" ] [ "reassoc" ]
    check "contract is per-instruction and opt-in"
        [ "BLADE_FP_CONTRACT", Some "fast" ] licensedFoldSource
        [ "fmul contract double"; "fadd contract double" ] [ "reassoc" ]
    check "contract=off is off" [ "BLADE_FP_CONTRACT", Some "off" ] licensedFoldSource
        [ "fmul double" ] [ "contract" ]
    check "both licenses compose on the accumulator"
        [ "BLADE_FP_REASSOC", Some "1"; "BLADE_FP_CONTRACT", Some "fast" ] licensedFoldSource
        [ "fadd reassoc nsz contract double"; "fmul contract double" ] []
    check "fmf:off outranks the numeric knobs"
        [ "BLADE_FP_REASSOC", Some "1"; "BLADE_FP_CONTRACT", Some "fast"
          "BLADE_LLVM_FACTS", Some "fmf:off" ] licensedFoldSource
        [ "fadd double" ] [ "reassoc"; "contract" ]

    printSubHeader "the whole switch"
    // FACTS=off must return the lane to the shape that passed its differential
    // before this layer existed -- the property that makes the switch a usable
    // bisection tool rather than a partial rollback.
    (let name = "FACTS=off emits the pre-fact module"
     let hostile =
         [ "BLADE_FP_REASSOC", Some "1"; "BLADE_FP_CONTRACT", Some "fast"
           "BLADE_LLVM_FACTS", Some "off" ]
     match emitUnder hostile "facts_off" licensedFoldSource with
     | Error e -> fail name e
     | Ok raw ->
         let off = instructionsOnly raw
         let bad =
             [ "attributes #"; "noundef"; "noalias"; "readonly"; "reassoc"; "contract"; " #0" ]
             |> List.filter off.Contains
         if bad.IsEmpty then pass name "no fact text survives"
         else fail name ($"""fact text survived the kill switch: {(String.concat " | " bad)}"""))

    printFooter "LLVM Fact Layer"
        [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = "LLVM Fact Layer"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = failedNames }

// ---------------------------------------------------------------------------
// Block 3: the simplex-blocks agreement pins
// ---------------------------------------------------------------------------
//
// `Blade.SimplexBlocksCore` and `Blade.ZarrProvider.SimplexBlocks` are TWO
// COPIES of the same decomposition, kept apart on purpose: the provider's
// consumer is the zarr/MPI ownership split, which needs equal-size triangular
// quadtree units, while the compute side coarsens off-diagonal triangle pairs
// into dense bricks and therefore produces UNEQUAL units
// (docs/plans/plan-simplex-blocked-compute.md section 6, the MPI row: "do not
// unify ... the two schemes share the leaf structure and the SimplexBlocks
// identities, nothing more").
//
// Two copies of anything drift. So this block is the differential-twin
// discipline applied module against module: over a grid of (n, B, r,
// symmetry), every shared identity must answer the SAME value in both, and
// the decomposition must actually decompose -- cells summing to the pool
// cardinality, blocks partitioning the canonical set, bricks canonical by
// construction. Pure integer arithmetic, so it runs anywhere with no
// toolchain at all.

let runSimplexAgreementTests () : BlockResult =
    printHeader "Simplex Blocks Agreement"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let check (name: string) (ok: bool) (detail: string) =
        if ok then
            passed <- passed + 1
            resultLine Pass name detail
        else
            failed <- failed + 1
            failedNames <- failedNames @ [ name ]
            resultLine Fail name detail
    let core = "Blade.SimplexBlocksCore"
    let extents = [ 1L; 2L; 3L; 4L; 5L; 6L; 7L; 8L; 9L; 11L; 13L; 16L; 17L; 31L ]
    let edges = [ 1L; 2L; 3L; 4L; 5L; 8L ]
    let ranks = [ 2; 3 ]

    printSubHeader "identities against the provider's copy"
    // Every shared function, over the whole grid, in both symmetries.
    let mutable mismatches = []
    let mutable cases = 0
    for strict in [ false; true ] do
        for r in ranks do
            for n in extents do
                for b in edges do
                    let t = SimplexBlocksCore.tileCount n b
                    cases <- cases + 1
                    if SimplexBlocksCore.blockCount r t <> ZarrProvider.SimplexBlocks.blockCount r t then
                        mismatches <- $"blockCount r={r} T={t}" :: mismatches
                    for tile in 0L .. t - 1L do
                        if SimplexBlocksCore.tileWidth n b tile <> ZarrProvider.SimplexBlocks.tileWidth n b tile then
                            mismatches <- $"tileWidth n={n} B={b} t={tile}" :: mismatches
                    for tiles in SimplexBlocksCore.blockSequence r t do
                        let mine = SimplexBlocksCore.blockCellCount strict n b tiles
                        let theirs = ZarrProvider.SimplexBlocks.blockCellCount strict n b tiles
                        if mine <> theirs then
                            mismatches <-
                                sprintf "blockCellCount strict=%b n=%d B=%d tiles=%A (%d vs %d)"
                                    strict n b tiles mine theirs :: mismatches
                        let myCells = SimplexBlocksCore.enumBlockCells strict n b tiles |> Seq.toList
                        let theirCells = ZarrProvider.SimplexBlocks.enumBlockCells strict n b tiles |> Seq.toList
                        if myCells <> theirCells then
                            mismatches <- sprintf "enumBlockCells strict=%b n=%d B=%d tiles=%A" strict n b tiles :: mismatches
    check ($"{core} == provider over {cases} (n, B, r, symmetry) cases")
        mismatches.IsEmpty
        (if mismatches.IsEmpty then "blockCount, tileWidth, blockCellCount, enumBlockCells all agree"
         else $"{mismatches.Length} disagreements, first: {(List.head (List.rev mismatches))}")

    // rank/unrank is the other half of the shared surface: it is what names a
    // pool cell, so a divergence here would mean the two modules disagree
    // about WHICH cell a coordinate tuple is.
    let mutable rankBad = []
    for strict in [ false; true ] do
        for r in ranks do
            for n in [ 3L; 4L; 5L; 7L; 11L ] do
                let total =
                    if strict then SimplexBlocksCore.binom n r
                    else SimplexBlocksCore.binom (n + int64 r - 1L) r
                for k in 0L .. total - 1L do
                    let mine = SimplexBlocksCore.unrankToCoords strict n r k
                    let theirs = ZarrProvider.SimplexBlocks.unrankToCoords strict n r k
                    if mine <> theirs then rankBad <- sprintf "unrank strict=%b n=%d r=%d k=%d" strict n r k :: rankBad
                    if SimplexBlocksCore.rankOfCoords strict n mine <> k then
                        rankBad <- sprintf "rank round-trip strict=%b n=%d r=%d k=%d" strict n r k :: rankBad
                    if ZarrProvider.SimplexBlocks.rankOfCoords strict n mine <> k then
                        rankBad <- sprintf "provider rank round-trip strict=%b n=%d r=%d k=%d" strict n r k :: rankBad
    check "rankOfCoords / unrankToCoords agree and round-trip" rankBad.IsEmpty
        (if rankBad.IsEmpty then "every cell of every grid, both directions"
         else $"{rankBad.Length} failures, first: {(List.head (List.rev rankBad))}")

    printSubHeader "the decomposition decomposes"
    // Sum of block cell counts = pool cardinality. This is the identity the
    // whole design rests on: if the blocks did not exactly tile the simplex,
    // a bricked traversal would miss cells or visit them twice.
    let mutable sumBad = []
    for strict in [ false; true ] do
        for r in ranks do
            for n in extents do
                for b in edges do
                    let t = SimplexBlocksCore.tileCount n b
                    let summed =
                        SimplexBlocksCore.blockSequence r t
                        |> Seq.sumBy (SimplexBlocksCore.blockCellCount strict n b)
                    let pool =
                        if strict then SimplexBlocksCore.binom n r
                        else SimplexBlocksCore.binom (n + int64 r - 1L) r
                    if summed <> pool then
                        sumBad <- sprintf "strict=%b r=%d n=%d B=%d: %d blocks-cells vs %d pool" strict r n b summed pool :: sumBad
    check "sum of blockCellCount = C(n+r-1, r) / C(n, r)" sumBad.IsEmpty
        (if sumBad.IsEmpty then "every grid point, both symmetries" else $"{sumBad.Length} failures, first: {(List.head (List.rev sumBad))}")

    // The blocks PARTITION the canonical set, and their cells arrive in an
    // order whose ranks are a permutation of [0, N) -- no cell twice, none
    // missing, none outside the pool.
    let mutable partBad = []
    for strict in [ false; true ] do
        for r in ranks do
            for n in [ 3L; 5L; 8L; 11L ] do
                for b in edges do
                    let t = SimplexBlocksCore.tileCount n b
                    let ranksSeen =
                        SimplexBlocksCore.blockSequence r t
                        |> Seq.collect (SimplexBlocksCore.enumBlockCells strict n b)
                        |> Seq.map (SimplexBlocksCore.rankOfCoords strict n)
                        |> Seq.toList
                    let pool =
                        if strict then SimplexBlocksCore.binom n r
                        else SimplexBlocksCore.binom (n + int64 r - 1L) r
                    if List.sort ranksSeen <> [ 0L .. pool - 1L ] then
                        partBad <- sprintf "strict=%b r=%d n=%d B=%d" strict r n b :: partBad
    check "blocks partition the canonical cell set exactly once" partBad.IsEmpty
        (if partBad.IsEmpty then "ranks are a permutation of [0, pool)" else $"{partBad.Length} failures, first: {(List.head (List.rev partBad))}")

    printSubHeader "the rank-2 compute additions"
    // packedOffset2 is the closed form the emitter lays down; it must name the
    // same cell the combinadic rank does, or the emitted GEP is simply wrong.
    let mutable offBad = []
    for strict in [ false; true ] do
        for n in extents do
            let s = if strict then 1L else 0L
            for i in 0L .. n - 1L do
                for j in i + s .. n - 1L do
                    if SimplexBlocksCore.packedOffset2 strict n i j <> SimplexBlocksCore.rankOfCoords strict n [| i; j |] then
                        offBad <- sprintf "strict=%b n=%d (%d,%d)" strict n i j :: offBad
            if SimplexBlocksCore.poolCells2 strict n <> (if strict then SimplexBlocksCore.binom n 2 else SimplexBlocksCore.binom (n + 1L) 2) then
                offBad <- sprintf "poolCells2 strict=%b n=%d" strict n :: offBad
    check "packedOffset2 = rankOfCoords (the emitter's closed form)" offBad.IsEmpty
        (if offBad.IsEmpty then "every canonical rank-2 cell of every extent" else $"{offBad.Length} failures, first: {(List.head (List.rev offBad))}")

    // THE RANK-r GENERALIZATION, held to the same standard: the hockey-stick
    // closed form must name the cell the combinadic rank names, at every rank
    // the lane can emit -- and it must PARTITION, i.e. the offsets of the
    // canonical cells are exactly 0 .. poolCells-1 with no gap and no
    // collision. A closed form that is merely injective would still corrupt a
    // pool; the surjectivity half is what proves no cell is stranded.
    let mutable offRBad = []
    for strict in [ false; true ] do
        for r in 1 .. 5 do
            // Small extents on purpose: the cell count is C(n+r-1, r), so this
            // grid is already ~10^4 tuples at r = 5 and it is exhaustive.
            for n in [ 1L; 2L; 3L; 4L; 5L; 7L; 8L ] do
                let cells = SimplexBlocksCore.poolCellsR strict n r
                let seen = System.Collections.Generic.HashSet<int64>()
                for k in 0L .. cells - 1L do
                    let coords = SimplexBlocksCore.unrankToCoords strict n r k
                    let closed = SimplexBlocksCore.packedOffsetR strict n coords
                    if closed <> k then
                        offRBad <- sprintf "strict=%b r=%d n=%d rank=%d -> %d" strict r n k closed :: offRBad
                    if not (seen.Add closed) then
                        offRBad <- sprintf "collision strict=%b r=%d n=%d at %d" strict r n k :: offRBad
                if int64 seen.Count <> cells then
                    offRBad <- sprintf "coverage strict=%b r=%d n=%d: %d of %d" strict r n seen.Count cells :: offRBad
    check "packedOffsetR = rankOfCoords, and partitions the pool (ranks 1-5)" offRBad.IsEmpty
        (if offRBad.IsEmpty then "every canonical cell, ranks 1-5, sym and antisym" else $"{offRBad.Length} failures, first: {(List.head (List.rev offRBad))}")

    // The bricks themselves: they cover the domain exactly once, an
    // off-diagonal brick is a FULL RECTANGLE every one of whose cells is
    // canonical (that is the entire point -- no mask, no guard), and the
    // enumeration order is the canonical ascending-lex block order.
    let mutable brickBad = []
    for strict in [ false; true ] do
        for n in extents do
            for b in edges do
                let bricks = SimplexBlocksCore.bricks2 strict n b
                let s = if strict then 1L else 0L
                let cells =
                    [ for br in bricks do
                        for i in br.RowLo .. br.RowHi - 1L do
                            let lo = if br.IsDiagonal then i + s else br.ColLo
                            for j in lo .. br.ColHi - 1L -> (i, j) ]
                let expected =
                    [ for i in 0L .. n - 1L do for j in i + s .. n - 1L -> (i, j) ]
                if List.sort cells <> expected then
                    brickBad <- sprintf "coverage strict=%b n=%d B=%d" strict n b :: brickBad
                for br in bricks do
                    let (t1, t2) = br.Tiles
                    if not br.IsDiagonal then
                        // Dense rectangle, every cell canonical, no exceptions.
                        let rows = br.RowHi - br.RowLo
                        let cols = br.ColHi - br.ColLo
                        if rows * cols <> SimplexBlocksCore.blockCellCount strict n b [| t1; t2 |] then
                            brickBad <- sprintf "dense-brick count strict=%b n=%d B=%d tiles=(%d,%d)" strict n b t1 t2 :: brickBad
                        if not (SimplexBlocksCore.isDenseBrick [| t1; t2 |]) then
                            brickBad <- sprintf "isDenseBrick disagrees strict=%b n=%d B=%d tiles=(%d,%d)" strict n b t1 t2 :: brickBad
                        if br.RowHi > br.ColLo then
                            brickBad <- $"off-diagonal brick is not strictly ordered n={n} B={b} tiles=({t1},{t2})" :: brickBad
                    elif SimplexBlocksCore.isDenseBrick [| t1; t2 |] then
                        brickBad <- $"diagonal brick claims distinct tiles n={n} B={b} tiles=({t1},{t2})" :: brickBad
                // Ascending-lex block order, filtered to the non-empty blocks.
                let order = bricks |> List.map (_.Tiles)
                let expectedOrder =
                    SimplexBlocksCore.blockSequence 2 (SimplexBlocksCore.tileCount n b)
                    |> Seq.filter (fun ts -> SimplexBlocksCore.blockCellCount strict n b ts > 0L)
                    |> Seq.map (fun ts -> (ts.[0], ts.[1]))
                    |> Seq.toList
                if order <> expectedOrder then
                    brickBad <- sprintf "block order strict=%b n=%d B=%d" strict n b :: brickBad
    check "bricks2 covers once, densely, in ascending-lex block order" brickBad.IsEmpty
        (if brickBad.IsEmpty then "coverage, density, ordering" else $"{brickBad.Length} failures, first: {(List.head (List.rev brickBad))}")

    // The profitability table in plan section 3 -- the numbers the B/depth
    // policy is argued from. Pinned so a change to the formula has to face
    // them.
    let fracCases =
        [ 2, 4L, 0.75; 2, 8L, 0.875; 2, 16L, 0.9375; 2, 32L, 0.969
          3, 4L, 0.375; 3, 8L, 0.656; 3, 16L, 0.820; 4, 4L, 0.094; 4, 16L, 0.666 ]
    let fracBad =
        fracCases
        |> List.filter (fun (r, t, want) -> abs (SimplexBlocksCore.denseBrickFraction r t - want) > 0.001)
    check "denseBrickFraction reproduces the plan's table" fracBad.IsEmpty
        (if fracBad.IsEmpty then $"{fracCases.Length} rows"
         else $"{fracBad.Length} rows disagree")

    // The derived policy after the 2026-08-18 measurement: never block by
    // default. S0 bricks lost or tied at every benchmarked extent, so the
    // serial triangle -- the fastest measured path -- is the only default;
    // bricked emission is opt-in via an explicit BLADE_LLVM_BRICKS=<B>.
    check "autoTileEdge never blocks by default (measured 2026-08-18 policy)"
        (SimplexBlocksCore.autoTileEdge 8L = None && SimplexBlocksCore.autoTileEdge 64L = None
         && SimplexBlocksCore.autoTileEdge 65L = None && SimplexBlocksCore.autoTileEdge 997L = None)
        "None at every extent"

    // The fold's combine order, as arithmetic. `emitCompactFold` accumulates
    // ONE PARTIAL PER BRICK and joins the partials in ascending-lex block
    // order; over exact (integer-valued) data that must equal the serial pool-
    // order walk cell for cell, which is the invariant the licensed-path
    // testing policy asks for instead of a float tolerance (plan section 7).
    let mutable foldBad = []
    for strict in [ false; true ] do
        for n in [ 3L; 5L; 8L; 11L; 16L; 17L ] do
            for b in edges do
                let cell (i: int64) (j: int64) = i * 1000L + j + 7L
                let s = if strict then 1L else 0L
                let serial =
                    [ for i in 0L .. n - 1L do for j in i + s .. n - 1L -> cell i j ] |> List.sum
                let bricked =
                    SimplexBlocksCore.bricks2 strict n b
                    |> List.sumBy (fun br ->
                        // The brick's own partial, then the join.
                        [ for i in br.RowLo .. br.RowHi - 1L do
                            let lo = if br.IsDiagonal then i + s else br.ColLo
                            for j in lo .. br.ColHi - 1L -> cell i j ] |> List.sum)
                if serial <> bricked then
                    foldBad <- sprintf "strict=%b n=%d B=%d: %d vs %d" strict n b serial bricked :: foldBad
    check "per-brick partials in block order = the serial pool-order fold" foldBad.IsEmpty
        (if foldBad.IsEmpty then "exact arithmetic, every grid point, both symmetries"
         else $"{foldBad.Length} failures, first: {(List.head (List.rev foldBad))}")

    printSubHeader "the licence gate and the measurement knob"
    // THE GATE, asserted by name in both directions. A fold whose kernel is
    // not `comm`-licensed must never brick, because grouping a fold by brick
    // reassociates it -- and no environment variable may buy that licence.
    // (A map passes `licensed = true` unconditionally: distinct cells,
    // independent writes, plan section 7.)
    check "an unlicensed fold never bricks, at any extent or knob setting"
        ([ None; Some "off"; Some "2"; Some "64"; Some "banana" ]
         |> List.forall (fun knob ->
                withEnv [ "BLADE_LLVM_BRICKS", knob ] (fun () ->
                    [ 4L; 65L; 997L; 100000L ] |> List.forall (fun n -> EmitLlvm.brickTileEdge false 0L n = None))))
        "licensed = false answers None for every (n, BLADE_LLVM_BRICKS)"
    check "a licensed fold bricks exactly where the derived policy says"
        (withEnv [ "BLADE_LLVM_BRICKS", None ] (fun () ->
            EmitLlvm.brickTileEdge true 0L 8L = None
            && EmitLlvm.brickTileEdge true 0L 64L = None
            && EmitLlvm.brickTileEdge true 0L 65L = None
            && EmitLlvm.brickTileEdge true 0L 997L = None))
        "unset + no reuse hint = the derived policy (serial everywhere; bricks are knob-only)"
    check "BLADE_LLVM_BRICKS off / <number> / junk"
        (withEnv [ "BLADE_LLVM_BRICKS", Some "off" ] (fun () -> EmitLlvm.brickTileEdge true 0L 997L = None)
         && withEnv [ "BLADE_LLVM_BRICKS", Some "0" ] (fun () -> EmitLlvm.brickTileEdge true 0L 997L = None)
         && withEnv [ "BLADE_LLVM_BRICKS", Some "3" ] (fun () -> EmitLlvm.brickTileEdge true 0L 7L = Some 3L)
         // A pinned edge that leaves ONE tile is not a decomposition, so it
         // declines rather than emitting a block loop with a single block.
         && withEnv [ "BLADE_LLVM_BRICKS", Some "9" ] (fun () -> EmitLlvm.brickTileEdge true 0L 7L = None)
         && withEnv [ "BLADE_LLVM_BRICKS", Some "banana" ] (fun () -> EmitLlvm.brickTileEdge true 0L 997L = None))
        "off forces serial, a number pins B (T >= 2 still required), junk defers to the (serial) policy"
    // The reuse hint (plan-simplex-blocked-compute.md section 0, third block):
    // a row-operand working set at or past 8 MiB flips BrickAuto to the
    // divisor-preferred reuse edge; below it, and for every plain map
    // (RowOpBytes = 0), the serial default stands; the licence still
    // dominates the hint.
    check "the reuse hint bricks past the threshold, divisor preferred"
        (withEnv [ "BLADE_LLVM_BRICKS", None ] (fun () ->
            EmitLlvm.brickTileEdge true 9000000L 6006L = Some 66L
            && EmitLlvm.brickTileEdge true 9000000L 6007L = Some 64L
            && EmitLlvm.brickTileEdge true 8388607L 6006L = None
            && EmitLlvm.brickTileEdge false 9000000L 6006L = None))
        "hint >= 8 MiB bricks (66 divides 6006; prime 6007 falls back to 64 ragged); one byte under stays serial; unlicensed stays None"

    printSubHeader "what the emitter does with it"
    let countOf (needle: string) (text: string) =
        let mutable n = 0
        let mutable i = text.IndexOf(needle, StringComparison.Ordinal)
        while i >= 0 do
            n <- n + 1
            i <- text.IndexOf(needle, i + 1, StringComparison.Ordinal)
        n
    // A four-element symmetric map: one tile under any policy, so its emission
    // must be identical whether or not bricking is switched off -- which is
    // what keeps every symmetric corpus file on exactly its pre-brick nest.
    let smallSym = """
type Ix = Idx<4>
let v: Array<Float like Ix> = [1.0, 2.0, 3.0, 4.0]
let s = method_for(v, v) <@> lambda(x, y) where comm(x, y) -> x * y |> compute
"""
    // Large enough that the pre-measurement auto policy would have bricked it;
    // after 2026-08-18 the default is serial at EVERY extent, so the default
    // emission must equal forced-serial here too -- and an explicit tile edge
    // must still decompose it (that is how the next variant gets A/B'd).
    let bigSym = """
type Bx = Idx<97>
let v = method_for(range<Bx>) <@> lambda(i) -> 1.0 * i |> compute
let s = method_for(v, v) <@> lambda(x, y) where comm(x, y) -> x * y |> compute
"""
    let emitBrick knob name src = emitUnder [ "BLADE_LLVM_BRICKS", knob ] name src
    (match emitBrick None "blocks" smallSym, emitBrick (Some "off") "blocks" smallSym,
           emitBrick (Some "2") "blocks" smallSym with
     | Ok dflt, Ok off, Ok pinned ->
         check "a single-tile domain emits the serial triangle by default" (dflt = off)
             "default emission == BLADE_LLVM_BRICKS=off, byte for byte"
         check "pinning B below the extent does decompose it" (pinned <> off)
             "BLADE_LLVM_BRICKS=2 changes the emitted nest"
     | a, b, c ->
         let err = [ a; b; c ] |> List.tryPick (function Error e -> Some e | _ -> None)
         check "small symmetric map emits" false (defaultArg err "unknown"))
    (match emitBrick None "blocks" bigSym, emitBrick (Some "off") "blocks" bigSym,
           emitBrick (Some "48") "blocks" bigSym with
     | Ok dflt, Ok off, Ok pinned ->
         check "the default emission is the serial triangle at every extent" (dflt = off)
             "default emission == BLADE_LLVM_BRICKS=off, byte for byte, at n = 97"
         check "an explicit tile edge still decomposes past the old threshold" (pinned <> off)
             "BLADE_LLVM_BRICKS=48 changes the emitted nest at n = 97"
     | a, b, c ->
         let err = [ a; b; c ] |> List.tryPick (function Error e -> Some e | _ -> None)
         check "large symmetric map emits" false (defaultArg err "unknown"))
    // THE FLAT-ELEMENTWISE PROMISE (plan section 6, row 1). Row-base
    // arithmetic is exactly one `sdiv` per compact coordinate computation, so
    // adding an elementwise map over a packed pool must add exactly ONE -- the
    // one its own printer spends -- and none for the map itself. Anything more
    // means the map grew a triangular nest it has no use for.
    (match emitBrick None "blocks" smallSym,
           emitBrick None "blocks" (smallSym + "let t = s * 3.0\n") with
     | Ok bare, Ok mapped ->
         let d = countOf "sdiv i64" (instructionsOnly mapped) - countOf "sdiv i64" (instructionsOnly bare)
         check "an elementwise map over a packed pool costs no coordinate math" (d = 1)
             ($"row-base divisions added by `s * 3.0`: {d} (1 = its printer's, 0 = the map's)")
     | a, b ->
         let err = [ a; b ] |> List.tryPick (function Error e -> Some e | _ -> None)
         check "elementwise-over-compact emits" false (defaultArg err "unknown"))

    printFooter "Simplex Blocks Agreement"
        [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = "Simplex Blocks Agreement"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = failedNames }

// ---------------------------------------------------------------------------
// Block 4: the two-lane differential
// ---------------------------------------------------------------------------

/// Which corpus categories the differential sweeps by default. `basic`,
/// `functions` and `loops` are the scalar/dense core; `symmetry`,
/// `index-types` and `reynolds` are where the packed simplex pools live, and
/// they joined the default sweep when the lane learned to emit them;
/// `recursive-arrays` joined with the IRForRange arm (the ordered serial
/// loop: `let rec` and whole-rank folds). Every other category is reachable
/// standalone with `blade test llvm <category>`.
let defaultCategories = [ "basic"; "functions"; "loops"; "symmetry"; "index-types"; "reynolds"; "recursive-arrays" ]

/// What happened to one file. `Matched` carries a note so an agreement that
/// asserts NOTHING -- both lanes printed no values at all -- is visible in the
/// log rather than indistinguishable from a real comparison.
type private CaseOutcome =
    | Matched of note: string
    | SkippedCase of reason: string
    | FailedCase of detail: string

/// The scratch root, CWD-relative like every other block's (see CLAUDE.md:
/// two `blade test` processes must not share a working directory).
let private outputDir = "./generated_cpp_tests"

/// Remove this case's artifacts from a previous run, so a compile that fails
/// cannot be papered over by a stale executable of the same name.
let private clearArtifacts (dir: string) (stems: string list) =
    for stem in stems do
        for ext in [ ".cpp"; ".cu"; ".ll"; ".exe"; ".out" ] do
            let f = Path.Combine(dir, stem + ext)
            try if File.Exists f then File.Delete f with _ -> ()

/// Run ONE corpus file through both back ends.
///
/// Order is deliberate: the llvm lane gets the first look, because a refusal
/// is in-process and free while the C++ oracle costs a g++ invocation. Each
/// lane lowers its OWN IRProgram from the same source, so neither back end can
/// observe state the other left on a shared tree.
let private runCase (dir: string) (name: string) (source: string) : CaseOutcome =
    let stem = Build.sanitizeFileName name
    let cppStem = "llvmdiff_cpp_" + stem
    let llStem = "llvmdiff_ll_" + stem
    try
        if name.EndsWith "(rejects)" || name.EndsWith "(aborts)" then
            SkippedCase "a reject/abort probe (nothing to compare)"
        else
        match Lowering.lower source with
        | Error _ -> SkippedCase "the front end refuses this program"
        | Ok ir0 ->
        match IRValidate.validateIR ir0 with
        | Error _ -> SkippedCase "IR validation refuses this program"
        | Ok ir0 ->
        match EmitLlvm.tryEmitProgramNamed stem ir0 with
        | Error reason -> SkippedCase ("llvm lane refused: " + reason)
        | Ok ll ->
            clearArtifacts dir [ cppStem; llStem ]
            // ---- C++ oracle -------------------------------------------------
            // A SECOND lowering, not a reuse of ir0: the two back ends install
            // their own analysis contexts over the IR they consume, and the
            // oracle has to be exactly what `blade test basic` would have
            // compiled -- not an IRProgram another emitter has already walked.
            match Lowering.lower source with
            | Error _ -> SkippedCase "the front end refuses this program"
            | Ok ir1 ->
            match IRValidate.validateIR ir1 with
            | Error _ -> SkippedCase "IR validation refuses this program"
            | Ok ir1 ->
            let (cppCode, _warnings) = CodeGen.genSelfContainedProgramFromIR ir1 stem
            // BOTH refusal channels are drained unconditionally -- they are
            // process-wide cells, and leaving one loaded would attribute this
            // file's refusal to the next file in the sweep.
            let unhandled = CodeGen.takeUnhandledIRNodeDiagnostics ()
            let refusals = CodeGen.takeCodegenRefusalDiagnostics cppCode
            let cudaSide = CodeGen.getCudaFileContent ()
            if not unhandled.IsEmpty then
                SkippedCase "the C++ oracle has no arm for this program"
            elif not refusals.IsEmpty then
                SkippedCase "the C++ oracle refuses this program"
            elif cudaSide.IsSome then
                SkippedCase "the C++ oracle emits a device kernel (no serial oracle)"
            elif Build.inferBackendReq cppCode <> Build.CpuOnly then
                SkippedCase "the C++ oracle needs a non-CPU backend"
            else
            let cppFile = Path.Combine(dir, cppStem + ".cpp")
            File.WriteAllText(cppFile, cppCode)
            match Build.compileCpp cppFile dir with
            | Error e when e.StartsWith "Skipped:" -> SkippedCase ("C++ oracle: " + e)
            | Error _ -> SkippedCase "the C++ oracle does not compile"
            | Ok cppExe ->
            match runExeCapture cppExe with
            | Error e -> SkippedCase ("the C++ oracle does not run: " + e)
            | Ok (code, _, _) when code <> 0 -> SkippedCase ($"the C++ oracle exits {code}")
            | Ok (_, cppOut, _) ->
            // ---- llvm lane --------------------------------------------------
            let llFile = Path.Combine(dir, llStem + ".ll")
            File.WriteAllText(llFile, ll)
            match Build.compileLlvmProgram llFile dir with
            | Error e when e.StartsWith "Skipped:" -> SkippedCase e
            | Error e -> FailedCase ($"""llvm compile failed ({llFile}): {(e.Replace("\n", " ") |> fun s -> if s.Length > 400 then s.Substring(0, 400) + "..." else s)}""")
            | Ok llExe ->
            match runExeCapture llExe with
            | Error e -> FailedCase ($"llvm run failed ({llFile}): {e}")
            | Ok (code, out, err) when code <> 0 ->
                FailedCase ($"llvm exe exits {code} ({llFile}): {(err.Trim())}")
            | Ok (_, llvmOut, _) ->
                match compareRuns 3 cppOut llvmOut with
                | Ok () ->
                    // Keep only what a reader would want: matched artifacts
                    // are regenerable and 300 of them fill a scratch dir.
                    clearArtifacts dir [ cppStem; llStem ]
                    Matched (if stripTiming cppOut = "" then "agreed, but neither lane printed a value" else "")
                | Error diffs ->
                    FailedCase
                        ($"""stdout differs -- {(String.concat " | " diffs)} (kept {cppFile} and {llFile})""")
    with ex -> FailedCase ($"harness exception: {ex.Message}")

/// The differential sweep over a category list.
let runLlvmDifferentialTestsFor (categories: string list) : BlockResult =
    printHeader "LLVM Differential (llvm lane vs C++ lane)"
    let block = "LLVM Differential"
    let skipBlock reason =
        printfn "Skipped: %s" reason
        printFooter block [ "0 passed"; "0 failed"; "1 skipped" ]
        { Block = block; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    // Toolchain gates FIRST, and both of them skip rather than fail: without
    // clang there is no lane, without g++ there is no oracle.
    match Build.resolveClang () with
    | None ->
        skipBlock "no clang found (set BLADE_LLVM_CLANG, or install C:\\msys64\\clang64)."
    | Some clang ->
    if not Build.capabilities.Value.HasGpp then
        skipBlock "requires g++ for the C++ oracle lane."
    else
    printfn "clang: %s" clang
    printfn "categories: %s\n" (String.concat ", " categories)
    let dir = outputDir
    Directory.CreateDirectory dir |> ignore
    // Both lanes' runtimes, deployed once: the C++ headers the oracle
    // #includes with bare quotes, and the C shim every .ll links against.
    CodeGen.deployRuntimeHeaders dir
    EmitLlvm.deployShim dir
    let mutable passed = 0
    let mutable failed = 0
    let mutable skipped = 0
    let mutable failedNames = []
    let mutable reasons = []
    for cat in categories do
        let tests =
            try Ok (Blade.Tests.Corpus.category cat)
            with ex -> Error ex.Message
        match tests with
        | Error msg ->
            printSubHeader ($"{cat} -- UNAVAILABLE")
            printfn "  %s" msg
            failed <- failed + 1
            failedNames <- failedNames @ [ $"category {cat}" ]
        | Ok tests ->
            printSubHeader ($"{cat} ({tests.Length} files)")
            for (name, source) in tests do
                match runCase dir name source with
                | Matched note ->
                    passed <- passed + 1
                    resultLine Pass name note
                | SkippedCase reason ->
                    skipped <- skipped + 1
                    reasons <- reason :: reasons
                    resultLine Skip name reason
                | FailedCase detail ->
                    failed <- failed + 1
                    failedNames <- failedNames @ [ name ]
                    resultLine Fail name detail
    printReasonHistogram (List.rev reasons)
    // What a sweep is FOR, stated as a number rather than left to be inferred
    // from a skip count.
    //
    // The denominator is the programs the lane could in principle have taken:
    // skips that are not the lane's doing -- reject/abort probes with nothing
    // to compare, programs the shared FRONT END refuses, oracle-side declines
    // -- are excluded rather than counted against it. Including them would
    // move the coverage figure every time the corpus gained a reject probe,
    // which says nothing about the back end and would make the one number here
    // worth reading untrustworthy.
    let laneRefusals =
        reasons |> List.filter (fun r -> r.StartsWith "llvm lane refused:") |> List.length
    let emitted = passed + failed
    let comparable = emitted + laneRefusals
    if comparable > 0 then
        printfn "\n  Lane coverage: %d of %d comparable programs emitted by the llvm lane (%.1f%%)"
            emitted comparable (100.0 * float emitted / float comparable)
        printfn "    %d refused by the lane (the histogram above is the worklist); %d skipped for reasons that are not the lane's"
            laneRefusals (skipped - laneRefusals)
    printFooter block
        [ $"{passed} passed"; $"{failed} failed"; $"{skipped} skipped" ]
    { Block = block; Passed = passed; Failed = failed; Skipped = skipped; FailedNames = failedNames }

/// The default sweep (`blade test llvm`).
let runLlvmDifferentialTests () : BlockResult =
    runLlvmDifferentialTestsFor defaultCategories

// ---------------------------------------------------------------------------
// Block 5: the three-way blocked-simplex gate
// ---------------------------------------------------------------------------
//
// The ordinary differential compares TWO arms and would pass a lane that never
// bricked anything: the derived policy declines to block a domain of eight
// elements, and every symmetric corpus file has one. So this block runs each
// symmetric program THREE ways --
//
//   C++ oracle | llvm with BLADE_LLVM_BRICKS=off | llvm with the tile edge
//   pinned small enough to actually decompose the corpus extents
//
// -- and demands agreement across all three. The middle arm is the control:
// if it and the C++ lane agree but the bricked arm does not, the failure is in
// the decomposition and nowhere else. Two pinned edges (2 and 3) are run
// because they land the RAGGED LAST TILE differently on the same extent, and
// the last tile is where a block decomposition goes wrong.
//
// A fourth assertion has nothing to do with values: the bricked .ll must
// DIFFER from the serial one. A knob that silently did nothing would make
// every comparison above pass while proving nothing at all.

/// The programs this gate runs: symmetric corpus files by their `// TEST:`
/// name, plus the two synthesized fixtures. Named rather than pattern-matched
/// so a corpus file that moves or is renamed FAILS here instead of quietly
/// dropping out of the gate.
let private simplexCorpusCases =
    [ "symmetry", "Triangular Iteration"
      "symmetry", "Output Type: Same Array + Comm"
      "symmetry", "Reduce Inside Inlined Fiber Kernel"
      "symmetry", "Antisymm pin (declared) - strict triangle without reynolds"
      "symmetry", "Antisymmetric triangular storage (1D strict simplex)"
      "symmetry", "Flat Elementwise Sym Compact"
      "loops", "Range SymIdx Prefix Offsets (r=2)"
      "loops", "Range SymIdx Named Alias Carries The Component Tag"
      // RANK 3. The blocked arms run serial at rank 3 (the blocked schedule is
      // rank-2 only), so for these the gate is really "the rank-r nest agrees
      // with the C++ lane three times over" -- which is exactly what wants
      // watching while the arithmetic is new. `Range SymIdx Prefix Offsets
      // (r=3)` is the sharpest of them: it pins the packed COORDINATE the
      // emitter hands every cell, so a wrong prefix term shows up as a wrong
      // printed value rather than as a crash. The literals cover the
      // canonicalizing read (mirrors and the diagonal); the antisym pair
      // covers the cumulative strict shift that a previous rank-3 build got
      // wrong (index-types/040's header records that bug).
      "loops", "Range SymIdx Prefix Offsets (r=3)"
      "index-types", "Rank-3 Symmetric Literal"
      "index-types", "Unannotated Triangular Literal Infers Rank-3 Symmetric"
      "index-types", "AntisymIdx Rank-3 Consumed By Kernel"
      "symmetry", "Output Type: Three-Way Same" ]

/// The synthesized programs (tests/fixtures/llvm/): a whole packed pool
/// printed cell by cell at ragged extents, and the large prime-extent case.
let private simplexFixtures = [ "simplex_pool"; "simplex_large" ]

/// One arm of the three-way run: a name and the `BLADE_LLVM_BRICKS` value.
let private brickArms = [ "serial", "off"; "bricked-b2", "2"; "bricked-b3", "3" ]

let runLlvmSimplexGate () : BlockResult =
    printHeader "LLVM Blocked Simplex (three-way)"
    let block = "LLVM Blocked Simplex"
    let skipBlock reason =
        printfn "Skipped: %s" reason
        printFooter block [ "0 passed"; "0 failed"; "1 skipped" ]
        { Block = block; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    match Build.resolveClang () with
    | None -> skipBlock "no clang found (set BLADE_LLVM_CLANG, or install C:\\msys64\\clang64)."
    | Some _ ->
    if not Build.capabilities.Value.HasGpp then skipBlock "requires g++ for the C++ oracle lane."
    else
    let mutable passed = 0
    let mutable failed = 0
    let mutable skipped = 0
    let mutable failedNames = []
    let pass name detail = passed <- passed + 1; resultLine Pass name detail
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [ name ]
        resultLine Fail name detail
    let dir = outputDir
    Directory.CreateDirectory dir |> ignore
    CodeGen.deployRuntimeHeaders dir
    EmitLlvm.deployShim dir
    // Gather the sources: corpus by name, then the fixtures from disk.
    let sources =
        [ for (cat, testName) in simplexCorpusCases do
            let found =
                try Blade.Tests.Corpus.category cat |> List.tryFind (fun (n, _) -> n = testName)
                with _ -> None
            yield ($"{cat}/{testName}", found)
          for fx in simplexFixtures do
            let path = Path.Combine(goldenRoot.Value, fx + ".blade")
            let found =
                if File.Exists path then Some (fx, File.ReadAllText path) else None
            yield ($"fixture/{fx}", found) ]
    for (label, found) in sources do
        match found with
        | None -> fail label "source not found (a corpus test was renamed, or a fixture is missing)"
        | Some (name, source) ->
            let stem = Build.sanitizeFileName name
            try
                // ---- the C++ oracle, once -------------------------------
                let cppStem = "simplex_cpp_" + stem
                clearArtifacts dir [ cppStem ]
                let cppOut =
                    match Lowering.lower source with
                    | Error e -> Error ("front end: " + e)
                    | Ok ir ->
                        match IRValidate.validateIR ir with
                        | Error errs -> Error ("IR validation: " + String.concat "; " errs)
                        | Ok ir ->
                            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir stem
                            CodeGen.takeUnhandledIRNodeDiagnostics () |> ignore
                            CodeGen.takeCodegenRefusalDiagnostics cppCode |> ignore
                            CodeGen.getCudaFileContent () |> ignore
                            let f = Path.Combine(dir, cppStem + ".cpp")
                            File.WriteAllText(f, cppCode)
                            match Build.compileCpp f dir with
                            | Error e -> Error ("C++ oracle does not compile: " + e)
                            | Ok exe ->
                                match runExeCapture exe with
                                | Error e -> Error ("C++ oracle does not run: " + e)
                                | Ok (code, _, _) when code <> 0 -> Error ($"C++ oracle exits {code}")
                                | Ok (_, out, _) -> Ok out
                match cppOut with
                | Error reason ->
                    skipped <- skipped + 1
                    resultLine Skip label reason
                | Ok cppText ->
                    // ---- the llvm arms --------------------------------
                    let arms =
                        brickArms
                        |> List.map (fun (armName, knob) ->
                            let llStem = $"simplex_ll_{(Build.sanitizeFileName armName)}_{stem}"
                            clearArtifacts dir [ llStem ]
                            let emitted =
                                withEnv (defaultEmissionEnv @ [ "BLADE_LLVM_BRICKS", Some knob ]) (fun () ->
                                    match Lowering.lower source with
                                    | Error e -> Error ("front end: " + e)
                                    | Ok ir ->
                                        match IRValidate.validateIR ir with
                                        | Error errs -> Error ("IR validation: " + String.concat "; " errs)
                                        | Ok ir ->
                                            match EmitLlvm.tryEmitProgramNamed stem ir with
                                            | Error r -> Error ("llvm lane refused: " + r)
                                            | Ok ll -> Ok ll)
                            match emitted with
                            | Error e -> (armName, Error e, "")
                            | Ok ll ->
                                let f = Path.Combine(dir, llStem + ".ll")
                                File.WriteAllText(f, ll)
                                match Build.compileLlvmProgram f dir with
                                | Error e -> (armName, Error ($"""compile failed: {(e.Replace("\n", " "))}"""), ll)
                                | Ok exe ->
                                    match runExeCapture exe with
                                    | Error e -> (armName, Error ("run failed: " + e), ll)
                                    | Ok (code, _, err) when code <> 0 ->
                                        (armName, Error ($"exits {code}: {(err.Trim())}"), ll)
                                    | Ok (_, out, _) -> (armName, Ok out, ll))
                    let refused = arms |> List.tryPick (fun (a, r, _) -> match r with Error e -> Some (a, e) | _ -> None)
                    match refused with
                    | Some (armName, e) when e.StartsWith "llvm lane refused" || e.StartsWith "front end" ->
                        skipped <- skipped + 1
                        resultLine Skip label ($"{armName}: {e}")
                    | Some (armName, e) -> fail label ($"{armName}: {e}")
                    | None ->
                        let diffs =
                            arms
                            |> List.choose (fun (armName, r, _) ->
                                match r with
                                | Ok out ->
                                    (match compareRuns 3 cppText out with
                                     | Ok () -> None
                                     | Error ds -> Some ($"""{armName}: {(String.concat " | " ds)}"""))
                                | Error _ -> None)
                        if not diffs.IsEmpty then
                            fail label ($"""stdout differs from the C++ lane -- {(String.concat " ;; " diffs)}""")
                        else
                            // The knob must be LIVE: a bricked emission that
                            // is textually the serial one proves nothing.
                            let text a = arms |> List.pick (fun (n, _, t) -> if n = a then Some t else None)
                            let serial = text "serial"
                            let bricked = text "bricked-b2"
                            let liveness =
                                if name = "simplex_large" || name = "simplex_pool" then
                                    if serial = bricked then Some "the bricked arm emitted the SAME .ll as the serial arm"
                                    else None
                                else None
                            match liveness with
                            | Some why -> fail label why
                            | None -> pass label ($"3 arms agree ({(stripTiming cppText).Length} chars of output)")
            with ex -> fail label ($"harness exception: {ex.Message}")
    printFooter block
        [ $"{passed} passed"; $"{failed} failed"; $"{skipped} skipped" ]
    { Block = block; Passed = passed; Failed = failed; Skipped = skipped; FailedNames = failedNames }

// ---------------------------------------------------------------------------
// Block 6: the benchmark (`blade test llvm-bench`)
// ---------------------------------------------------------------------------
//
// Two questions, two protocols, one block. Neither half can fail on a slow
// number -- a benchmark that reddens on a ratio is a flake generator, and the
// `timing` block already set that precedent here. Both halves DO fail on a
// refusal, a build error or a value disagreement, because those are coverage
// regressions wearing a benchmark's clothes: every fixture below is known to
// compile on both lanes, so one of them ceasing to is news.
//
// (a) CODEGEN SPEED -- the question the lane was actually built to answer
//     (plan-llvm-backend.md R6: "skipping the C++ front end entirely" is the
//     strongest honest case for this backend). Timed region is IRProgram to
//     executable: emit the text, write it, invoke the toolchain, link. LOWERING
//     IS OUTSIDE THE STOPWATCH and is redone per arm per rep, because it is
//     shared work neither back end can be credited or charged for.
//
//     THE EXECUTABLE CACHE IS PINNED OFF. `compileCppWithExtraSource` consults
//     a content-addressed exe cache under %LOCALAPPDATA%\Blade\exe-cache
//     (Build.fs:837-846) and `compileLlvmProgram` has no such thing, so leaving
//     it on would time a file copy against a compiler. It is pinned off for the
//     table and measured separately in one extra row, since "how long until I
//     have an exe" with the shipped defaults is a fair question with a
//     different answer.
//
// (b) RUNTIME -- three shapes at non-power-of-two extents (the ~7x cache
//     artifact at 2^k is documented in CLAUDE.md and would drown every effect
//     here), arms rotated round-robin so no arm is always first into a cold
//     cache, one warmup discarded per arm per round, 9 reps x 3 rounds = 27
//     samples per arm, medians reported with the observed spread.
//
//     TWO CLOCKS ARE REPORTED. The primary is the program's own
//     `<name> completed in <t>s` line, which both lanes emit around the same
//     region (all bindings, before the auto-print pass): C++ through
//     std::chrono::high_resolution_clock, the llvm lane through the shim's
//     `blade_now`. The secondary is the wall time of the whole process, which
//     is the same measurement on both sides and carries ~10-15 ms of Windows
//     process startup that compresses every ratio toward 1.0. Where they
//     disagree, the internal clock is the one about compute and the external
//     one is the one a user feels.
//
//     Values are compared across arms BEFORE any timing is reported. A fast
//     wrong answer is not a result.

/// Median of a sample; even counts average the two middle values. Sorts a
/// copy, so the caller's list is untouched.
let private median (xs: float list) : float =
    match xs with
    | [] -> nan
    | _ ->
        let a = xs |> List.sort |> Array.ofList
        if a.Length % 2 = 1 then a.[a.Length / 2]
        else (a.[a.Length / 2 - 1] + a.[a.Length / 2]) / 2.0

/// Rotate a list left by `n` (negative and oversized `n` both wrap). Used to
/// move a different arm to the front of every rep, so the arm that pays for a
/// cold page cache is not always the same one.
let private rotate (n: int) (xs: 'a list) : 'a list =
    match xs with
    | [] -> []
    | _ ->
        let k = ((n % xs.Length) + xs.Length) % xs.Length
        List.skip k xs @ List.take k xs

/// Load a bench fixture from tests/fixtures/llvm/ (same two-root rule as the
/// goldens: the source tree wins from the repo root, so a fixture edit takes
/// effect with no rebuild).
let private benchSource (name: string) : Result<string, string> =
    let path = Path.Combine(goldenRoot.Value, name + ".blade")
    if File.Exists path then
        try Ok (File.ReadAllText path) with ex -> Error ($"unreadable: {ex.Message}")
    else Error ($"fixture missing: {path}")

/// Parse the `<name> completed in <t>s` line the two lanes both print. None
/// when the line is absent, which the caller treats as a failed measurement
/// rather than as a zero.
let private parseCompleted (out: string) : float option =
    let marker = "completed in "
    out.Replace("\r\n", "\n").Split('\n')
    |> Array.tryPick (fun line ->
        let i = line.IndexOf marker
        if i < 0 then None
        else
            let tail = line.Substring(i + marker.Length).Trim()
            let tail = if tail.EndsWith "s" then tail.Substring(0, tail.Length - 1) else tail
            match Double.TryParse(tail, Globalization.NumberStyles.Float,
                                  Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | _ -> None)

/// Which back end an arm drives.
type private RtLane =
    | LaneCpp
    | LaneLlvm

/// One timed build: how long the whole IRProgram-to-executable half took, and
/// how many bytes of source text the back end produced on the way.
type private BuildSample = { Ms: float; Bytes: int64 }

/// Build one program through one lane, timing emit + write + toolchain.
///
/// `progName` is the name baked into the artifact (it reaches the `completed
/// in` line and, on the llvm lane, a string constant in the .ll); `stem` names
/// the files, and must differ between arms or two arms would fight over one
/// .exe path.
let private timedBuild (lane: RtLane) (dir: string) (progName: string) (stem: string)
                       (source: string) : Result<BuildSample, string> =
    match Lowering.lower source with
    | Error e -> Error ("front end: " + e)
    | Ok ir ->
    match IRValidate.validateIR ir with
    | Error errs -> Error ("IR validation: " + String.concat "; " errs)
    | Ok ir ->
        clearArtifacts dir [ stem ]
        let sw = Stopwatch.StartNew()
        let outcome =
            match lane with
            | LaneCpp ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir progName
                // Both refusal channels are process-wide cells: leaving one
                // loaded would attribute this program's refusal to the next.
                CodeGen.takeUnhandledIRNodeDiagnostics () |> ignore
                CodeGen.takeCodegenRefusalDiagnostics cppCode |> ignore
                CodeGen.getCudaFileContent () |> ignore
                let f = Path.Combine(dir, stem + ".cpp")
                File.WriteAllText(f, cppCode)
                match Build.compileCpp f dir with
                | Error e -> Error ("g++: " + e.Replace("\n", " "))
                | Ok _ -> Ok f
            | LaneLlvm ->
                match EmitLlvm.tryEmitProgramNamed progName ir with
                | Error r -> Error ("llvm lane refused: " + r)
                | Ok ll ->
                    let f = Path.Combine(dir, stem + ".ll")
                    File.WriteAllText(f, ll)
                    match Build.compileLlvmProgram f dir with
                    | Error e -> Error ("clang: " + e.Replace("\n", " "))
                    | Ok _ -> Ok f
        sw.Stop()
        match outcome with
        | Error e -> Error e
        | Ok f -> Ok { Ms = sw.Elapsed.TotalMilliseconds; Bytes = FileInfo(f).Length }

/// The six programs the codegen table covers, in report order: two pure
/// scalar, three dense (the pipeline being the large-kernel one of the set),
/// one compact symmetric.
let private codegenPrograms =
    [ "bench_scalar_chain",      "scalar"
      "bench_scalar_calls",      "scalar + calls"
      "bench_dense_pipeline",    "dense, large kernel"
      "bench_dense_elementwise", "dense 1e7 map"
      "bench_dense_fold",        "dense 1e7 fold"
      "bench_sym_map",           "symmetric n=2003" ]

let private codegenReps = 5

/// Env pins for the codegen table: the numeric knobs at their defaults (so a
/// developer's shell cannot move the numbers) and the exe cache OFF (see the
/// block banner -- it exists on one lane only).
let private codegenEnv = defaultEmissionEnv @ [ "BLADE_EXE_CACHE", Some "0" ]

let runLlvmBenchCodegen () : BlockResult =
    printHeader "LLVM Bench: codegen speed (IRProgram -> executable)"
    let block = "LLVM Bench Codegen"
    let skipBlock reason =
        printfn "Skipped: %s" reason
        printFooter block [ "0 passed"; "0 failed"; "1 skipped" ]
        { Block = block; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    match Build.resolveClang () with
    | None -> skipBlock "no clang found (set BLADE_LLVM_CLANG, or install C:\\msys64\\clang64)."
    | Some clang ->
    if not Build.capabilities.Value.HasGpp then skipBlock "requires g++ for the C++ lane."
    else
    printfn "clang: %s" clang
    printfn "protocol: %d alternating reps per program, median; exe cache pinned OFF.\n" codegenReps
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let dir = outputDir
    Directory.CreateDirectory dir |> ignore
    CodeGen.deployRuntimeHeaders dir
    EmitLlvm.deployShim dir
    // Warm both toolchains before the first measured rep: the first invocation
    // of a 100 MB compiler binary pages it in, and the llvm lane's shim .o is
    // compiled once per output directory. Charging either to whichever program
    // happens to be first in the list would be an artifact, not a result.
    let rows = ResizeArray<string * float * float * int64 * int64 * float>()
    withEnv codegenEnv (fun () ->
        match benchSource (fst codegenPrograms.[0]) with
        | Error _ -> ()
        | Ok warmSrc ->
            timedBuild LaneCpp dir "warmup" "bench_warm_cpp" warmSrc |> ignore
            timedBuild LaneLlvm dir "warmup" "bench_warm_ll" warmSrc |> ignore)
    for (prog, kind) in codegenPrograms do
        match benchSource prog with
        | Error e ->
            failed <- failed + 1
            failedNames <- failedNames @ [ prog ]
            resultLine Fail prog e
        | Ok source ->
            let cppStem = "bench_cpp_" + Build.sanitizeFileName prog
            let llStem = "bench_ll_" + Build.sanitizeFileName prog
            let cppSamples = ResizeArray<BuildSample>()
            let llSamples = ResizeArray<BuildSample>()
            let mutable err = None
            withEnv codegenEnv (fun () ->
                for rep in 0 .. codegenReps - 1 do
                    // Alternate which lane goes first: a rep's second build
                    // runs against a warmer file cache than its first, and
                    // fixing the order would hand that to one lane every time.
                    let order =
                        if rep % 2 = 0 then [ LaneCpp, cppStem, cppSamples; LaneLlvm, llStem, llSamples ]
                        else [ LaneLlvm, llStem, llSamples; LaneCpp, cppStem, cppSamples ]
                    for (lane, stem, sink) in order do
                        if err.IsNone then
                            match timedBuild lane dir prog stem source with
                            | Error e -> err <- Some e
                            | Ok s -> sink.Add s)
            match err with
            | Some e ->
                failed <- failed + 1
                failedNames <- failedNames @ [ prog ]
                resultLine Fail prog e
            | None ->
                let cppMs = median [ for s in cppSamples -> s.Ms ]
                let llMs = median [ for s in llSamples -> s.Ms ]
                let cppBytes = (Seq.head cppSamples).Bytes
                let llBytes = (Seq.head llSamples).Bytes
                let ratio = if cppMs > 0.0 then llMs / cppMs else nan
                rows.Add($"{prog} ({kind})", cppMs, llMs, cppBytes, llBytes, ratio)
                passed <- passed + 1
                // The medians drive the table; the extremes go on the per-test
                // line, where a reader deciding whether to believe a 4x ratio
                // can see whether the two samples even overlap.
                resultLine Pass prog
                    (sprintf "g++ %.0f ms (%.0f-%.0f), clang %.0f ms (%.0f-%.0f), ratio %.2fx"
                        cppMs (Seq.min [ for s in cppSamples -> s.Ms ]) (Seq.max [ for s in cppSamples -> s.Ms ])
                        llMs (Seq.min [ for s in llSamples -> s.Ms ]) (Seq.max [ for s in llSamples -> s.Ms ])
                        ratio)
    if rows.Count > 0 then
        printfn ""
        printfn "  %-44s %9s %9s %7s %10s %10s" "program" "g++ ms" "clang ms" "ratio" "cpp bytes" "ll bytes"
        printfn "  %s" (String.replicate 94 "-")
        for (name, cppMs, llMs, cppB, llB, ratio) in rows do
            printfn "  %-44s %9.0f %9.0f %6.2fx %10d %10d" name cppMs llMs ratio cppB llB
        let ratios = [ for (_, _, _, _, _, r) in rows -> r ]
        let medRatio = median ratios
        printfn "  %s" (String.replicate 94 "-")
        printfn "  median ratio (clang lane / g++ lane): %.2fx  -- %s"
            medRatio
            (if medRatio < 1.0 then sprintf "the llvm lane reaches an executable %.2fx FASTER" (1.0 / medRatio)
             else sprintf "the llvm lane is %.2fx SLOWER to an executable" medRatio)
        // One extra row for the shipped defaults, where the C++ lane has a
        // content-addressed exe cache and the llvm lane does not. This is not
        // a codegen measurement; it is what a user re-running an unchanged
        // program actually waits for.
        match benchSource "bench_dense_pipeline" with
        | Error _ -> ()
        | Ok src ->
            let cachedMs =
                withEnv (defaultEmissionEnv @ [ "BLADE_EXE_CACHE", None ]) (fun () ->
                    // Prime the cache, then measure hits.
                    timedBuild LaneCpp dir "bench_dense_pipeline" "bench_cache_cpp" src |> ignore
                    [ for _ in 1 .. codegenReps ->
                        match timedBuild LaneCpp dir "bench_dense_pipeline" "bench_cache_cpp" src with
                        | Ok s -> s.Ms
                        | Error _ -> nan ] |> median)
            printfn "  exe cache (C++ lane only, shipped default ON): a WARM rebuild of"
            printfn "    bench_dense_pipeline costs %.0f ms instead of a g++ invocation." cachedMs
        // The one llvm-lane cost the table above amortizes away: the C shim's
        // object file, compiled once per output directory and reused by every
        // link after it. Deleting it and timing one more build prices it, so
        // the 4x claim is not resting on a hidden subsidy.
        match benchSource "bench_scalar_chain" with
        | Error _ -> ()
        | Ok src ->
            for f in Directory.GetFiles(dir, "blade_llvm_shim.*") do
                if not (f.EndsWith ".c") then (try File.Delete f with _ -> ())
            let coldMs =
                withEnv codegenEnv (fun () ->
                    match timedBuild LaneLlvm dir "bench_scalar_chain" "bench_shim_ll" src with
                    | Ok s -> s.Ms
                    | Error _ -> nan)
            printfn "  llvm shim: the build that also has to compile blade_llvm_shim.c costs"
            printfn "    %.0f ms (once per output directory; every later link reuses the .o)." coldMs
    printFooter block [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = block; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }

// --- runtime ---------------------------------------------------------------

/// One runtime arm: a label, the lane, and the env pins in force during
/// EMISSION. Nothing is pinned at run time -- every knob here is read at
/// codegen time and baked into the artifact, which is exactly why an arm is a
/// separate executable rather than a separate invocation.
type private RtArm =
    { Label: string
      Lane: RtLane
      Pins: (string * string option) list }

/// One runtime shape: a fixture and the arms to race over it. `Arms` must lead
/// with the C++ arm -- it is the value oracle every other arm is compared to.
type private RtShape =
    { Name: string
      Fixture: string
      Note: string
      Arms: RtArm list }

let private rtRounds = 3
let private rtReps = 9

let private armCpp = { Label = "cpp"; Lane = LaneCpp; Pins = [] }
let private armLlvm = { Label = "llvm"; Lane = LaneLlvm; Pins = [] }

let private rtShapes =
    [ { Name = "dense elementwise, n = 9,999,991"
        Fixture = "bench_dense_elementwise"
        Note = "memory-bandwidth bound: two ~80 MB pools"
        Arms = [ armCpp; armLlvm ] }
      { Name = "dense licensed fold, n = 9,999,991"
        Fixture = "bench_dense_fold"
        Note = "integer-valued cells: exact under any association, so the reassociated arms are value-comparable"
        Arms =
          [ armCpp
            { Label = "cpp-reassoc"; Lane = LaneCpp; Pins = [ "BLADE_FP_REASSOC", Some "1" ] }
            armLlvm
            { Label = "llvm-reassoc"; Lane = LaneLlvm; Pins = [ "BLADE_FP_REASSOC", Some "1" ] } ] }
      { Name = "symmetric map, n = 2003 (C(2004,2) = 2,007,006 cells)"
        Fixture = "bench_sym_map"
        Note = "the shape the brief named; ~3 ms of compute, close to first-touch noise"
        Arms =
          [ armCpp
            { Label = "llvm-serial"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "off" ] }
            { Label = "llvm-bricked"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "64" ] } ] }
      { Name = "symmetric map, n = 6007 (C(6008,2) = 18,048,028 cells)"
        Fixture = "bench_sym_large"
        Note = "9x the work: this is where the bricks verdict is read"
        Arms =
          [ armCpp
            { Label = "llvm-serial"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "off" ] }
            { Label = "llvm-bricked"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "64" ] } ] }
      { Name = "symmetric map, n = 6006 (2*3*7*11*13; C(6007,2) = 18,039,021 cells)"
        Fixture = "bench_sym_divisible"
        Note = "the divisibility control: B=66 divides 6006 exactly, B=64 leaves a ragged 54-wide last tile"
        Arms =
          [ armCpp
            { Label = "llvm-serial"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "off" ] }
            { Label = "llvm-brick-64-ragged"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "64" ] }
            { Label = "llvm-brick-66-exact"; Lane = LaneLlvm; Pins = [ "BLADE_LLVM_BRICKS", Some "66" ] } ] }
      // THE GRAM (row-operand reuse) SHAPE. Removed from this block once,
      // when a single run cost ~10 s because `reduce(x * y, (+))` allocated
      // and refilled a 402-cell temp PER OUTPUT CELL. The fold-fusion fix
      // consumes the producer instead, a run costs ~0.2-0.9 s, and thirty
      // reps across four arms is a normal bench again. It earns its place:
      // this is the shape where the derived brick policy fires (bm = 9.2 MB
      // clears the 8 MiB reuse threshold) and where bricks WIN -- the map
      // shapes above are the control that says they lose without reuse.
      { Name = "symmetric gram, n = 3001, d = 402 (row-operand reuse; bm = 9.2 MB)"
        Fixture = "bench_sym_gram_small"
        Note = "the shape class where S0 bricks win: serial re-streams bm per output row, bricked once per row-tile; all arms reassoc-licensed (cells are exact quarters, so association cannot move the printed value)"
        Arms =
          [ { armCpp with Pins = [ "BLADE_FP_REASSOC", Some "1" ] }
            { Label = "llvm-auto"; Lane = LaneLlvm; Pins = [ "BLADE_FP_REASSOC", Some "1" ] }
            { Label = "llvm-serial"; Lane = LaneLlvm; Pins = [ "BLADE_FP_REASSOC", Some "1"; "BLADE_LLVM_BRICKS", Some "off" ] }
            { Label = "llvm-brick-64"; Lane = LaneLlvm; Pins = [ "BLADE_FP_REASSOC", Some "1"; "BLADE_LLVM_BRICKS", Some "64" ] } ] } ]

/// Build one arm's executable, or say why not.
let private buildRtArm (dir: string) (shape: RtShape) (arm: RtArm) (source: string) : Result<string, string> =
    let stem = Build.sanitizeFileName ($"rtb_{arm.Label}_{shape.Fixture}")
    clearArtifacts dir [ stem ]
    withEnv (defaultEmissionEnv @ arm.Pins @ [ "BLADE_EXE_CACHE", Some "0" ]) (fun () ->
        match Lowering.lower source with
        | Error e -> Error ("front end: " + e)
        | Ok ir ->
        match IRValidate.validateIR ir with
        | Error errs -> Error ("IR validation: " + String.concat "; " errs)
        | Ok ir ->
            match arm.Lane with
            | LaneCpp ->
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir shape.Fixture
                CodeGen.takeUnhandledIRNodeDiagnostics () |> ignore
                CodeGen.takeCodegenRefusalDiagnostics cppCode |> ignore
                CodeGen.getCudaFileContent () |> ignore
                let f = Path.Combine(dir, stem + ".cpp")
                File.WriteAllText(f, cppCode)
                match Build.compileCpp f dir with
                | Error e -> Error ("g++: " + e.Replace("\n", " "))
                | Ok exe -> Ok exe
            | LaneLlvm ->
                match EmitLlvm.tryEmitProgramNamed shape.Fixture ir with
                | Error r -> Error ("llvm lane refused: " + r)
                | Ok ll ->
                    let f = Path.Combine(dir, stem + ".ll")
                    File.WriteAllText(f, ll)
                    match Build.compileLlvmProgram f dir with
                    | Error e -> Error ("clang: " + e.Replace("\n", " "))
                    | Ok exe -> Ok exe)

let runLlvmBenchRuntime () : BlockResult =
    printHeader "LLVM Bench: runtime"
    let block = "LLVM Bench Runtime"
    let skipBlock reason =
        printfn "Skipped: %s" reason
        printFooter block [ "0 passed"; "0 failed"; "1 skipped" ]
        { Block = block; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    match Build.resolveClang () with
    | None -> skipBlock "no clang found (set BLADE_LLVM_CLANG, or install C:\\msys64\\clang64)."
    | Some _ ->
    if not Build.capabilities.Value.HasGpp then skipBlock "requires g++ for the C++ lane."
    else
    printfn "protocol: %d rounds x %d reps (+1 warmup discarded per arm per round), arms rotated; medians.\n"
        rtRounds rtReps
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let dir = outputDir
    Directory.CreateDirectory dir |> ignore
    CodeGen.deployRuntimeHeaders dir
    EmitLlvm.deployShim dir
    for shape in rtShapes do
        printSubHeader shape.Name
        printfn "  (%s)" shape.Note
        match benchSource shape.Fixture with
        | Error e ->
            failed <- failed + 1
            failedNames <- failedNames @ [ shape.Fixture ]
            resultLine Fail shape.Fixture e
        | Ok source ->
            let built = shape.Arms |> List.map (fun a -> a, buildRtArm dir shape a source)
            let buildErr = built |> List.tryPick (fun (a, r) -> match r with Error e -> Some (a.Label, e) | _ -> None)
            match buildErr with
            | Some (label, e) ->
                failed <- failed + 1
                failedNames <- failedNames @ [ $"{shape.Fixture}/{label}" ]
                resultLine Fail shape.Fixture ($"arm {label} did not build: {e}")
            | None ->
                let exes = built |> List.map (fun (a, r) -> a, (match r with Ok e -> e | Error _ -> ""))
                // ---- values before timing --------------------------------
                let firstRuns = exes |> List.map (fun (a, exe) -> a, runExeCapture exe)
                let runErr =
                    firstRuns |> List.tryPick (fun (a, r) ->
                        match r with
                        | Error e -> Some (a.Label, e)
                        | Ok (code, _, err) when code <> 0 -> Some (a.Label, $"exits {code}: {(err.Trim())}")
                        | _ -> None)
                match runErr with
                | Some (label, e) ->
                    failed <- failed + 1
                    failedNames <- failedNames @ [ $"{shape.Fixture}/{label}" ]
                    resultLine Fail shape.Fixture ($"arm {label} did not run: {e}")
                | None ->
                    let outs = firstRuns |> List.map (fun (a, r) -> a.Label, (match r with Ok (_, o, _) -> o | Error _ -> ""))
                    let oracle = snd outs.Head
                    let mismatches =
                        outs
                        |> List.skip 1
                        |> List.choose (fun (label, o) ->
                            match compareRuns 3 oracle o with
                            | Ok () -> None
                            | Error ds -> Some ($"""{label}: {(String.concat " | " ds)}"""))
                    if not mismatches.IsEmpty then
                        failed <- failed + 1
                        failedNames <- failedNames @ [ shape.Fixture ]
                        resultLine Fail shape.Fixture
                            ($"""arms disagree BEFORE timing -- {(String.concat " ;; " mismatches)}""")
                    else
                    // ---- the rotated timing loop -----------------------
                    let inner = Dictionary<string, ResizeArray<float>>()
                    let outer = Dictionary<string, ResizeArray<float>>()
                    for (a, _) in exes do
                        inner.[a.Label] <- ResizeArray<float>()
                        outer.[a.Label] <- ResizeArray<float>()
                    let mutable measureErr = None
                    let runOnce (label: string) (exe: string) (record: bool) =
                        let sw = Stopwatch.StartNew()
                        let r = runExeCapture exe
                        sw.Stop()
                        match r with
                        | Ok (0, out, _) ->
                            if record then
                                match parseCompleted out with
                                | Some secs ->
                                    inner.[label].Add(secs * 1000.0)
                                    outer.[label].Add(sw.Elapsed.TotalMilliseconds)
                                | None -> measureErr <- Some (label, "no `completed in` line in the output")
                        | Ok (code, _, err) -> measureErr <- Some (label, $"exits {code}: {(err.Trim())}")
                        | Error e -> measureErr <- Some (label, e)
                    for round in 0 .. rtRounds - 1 do
                        for (a, exe) in exes do
                            if measureErr.IsNone then runOnce a.Label exe false
                        for rep in 0 .. rtReps - 1 do
                            for (a, exe) in rotate (round * rtReps + rep) exes do
                                if measureErr.IsNone then runOnce a.Label exe true
                    match measureErr with
                    | Some (label, e) ->
                        failed <- failed + 1
                        failedNames <- failedNames @ [ $"{shape.Fixture}/{label}" ]
                        resultLine Fail shape.Fixture ($"arm {label} failed mid-measurement: {e}")
                    | None ->
                        let baseline = median (List.ofSeq inner.[(fst exes.Head).Label])
                        printfn ""
                        printfn "    %-14s %11s %11s %11s %9s %11s"
                            "arm" "inner med" "inner min" "inner max" "vs cpp" "outer med"
                        printfn "    %s" (String.replicate 76 "-")
                        for (a, _) in exes do
                            let ins = List.ofSeq inner.[a.Label]
                            let outs2 = List.ofSeq outer.[a.Label]
                            let m = median ins
                            printfn "    %-14s %9.3f ms %9.3f ms %9.3f ms %8.2fx %9.3f ms"
                                a.Label m (List.min ins) (List.max ins)
                                (if baseline > 0.0 then m / baseline else nan)
                                (median outs2)
                        printfn ""
                        passed <- passed + 1
                        resultLine Pass shape.Fixture
                            ($"{exes.Length} arms agree; {(rtRounds * rtReps)} samples each")
    printFooter block [ $"{passed} passed"; $"{failed} failed"; "0 skipped" ]
    { Block = block; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }

/// `blade test llvm-bench` -- the two benchmark halves and a roll-up. Like
/// `blade test llvm`, standalone only: it spawns hundreds of native compiler
/// and executable invocations and has no business inside the default suite.
let runLlvmBench () : int =
    let blocks = [ runLlvmBenchCodegen (); runLlvmBenchRuntime () ]
    printGrandTotal blocks
    if blocks |> List.sumBy (_.Failed) = 0 then 0 else 1

// ---------------------------------------------------------------------------
// Entry points for `blade test llvm [category]`
// ---------------------------------------------------------------------------

/// `blade test llvm` -- both blocks plus a combined roll-up. NOT part of the
/// default `blade test` suite (same policy as cuda/mpi/omp/timing): it spawns
/// two native compilers per corpus file, and it must not be able to redden a
/// run of the C++ lane's own suite.
let runLlvmTests () : int =
    let blocks =
        [ runLlvmCompareRuleTests ()
          runLlvmGoldenTests ()
          runLlvmFactTests ()
          runSimplexAgreementTests ()
          runLlvmSimplexGate ()
          runLlvmDifferentialTests () ]
    printGrandTotal blocks
    if blocks |> List.sumBy (_.Failed) = 0 then 0 else 1

/// `blade test llvm <category>` -- the differential over one named corpus
/// directory (the literal tests/corpus/<dir> name, as `test interp <dir>`
/// takes). The reserved word `all` (alias `corpus`) sweeps EVERY single-file
/// category instead of the seven the lane was grown against, and reports what
/// fraction of the corpus the lane can actually emit. The reserved word
/// `goldens` (alias `pins`) runs the THREE
/// toolchain-free blocks instead -- compare rules, emission pins and the fact
/// layer -- which finish instantly and are what you want while iterating on
/// the emitter. `facts` runs the fact block alone.
let runLlvmCategory (cat: string) : int =
    match cat.ToLower().TrimStart('-') with
    | "all" | "corpus" ->
        // The differential over EVERY single-file corpus category, not the
        // seven `defaultCategories` sweeps. Those seven are the ones the lane
        // was grown against, so `blade test llvm` measures the lane where it is
        // known to work; this measures it against the whole corpus, which is
        // the only way the coverage figure means what it sounds like.
        //
        // Expect a much lower one, and expect that to be the POINT: the skip
        // histogram it prints is the ranked worklist for making the lane a
        // back end the suite could actually run on. Long -- two native
        // compilers per file across ~1900 files -- so it stays a verb you ask
        // for, never part of `blade test`.
        let r = runLlvmDifferentialTestsFor (Blade.Tests.Corpus.singleFileCategories ())
        if r.Failed = 0 then 0 else 1
    | "goldens" | "pins" ->
        let r = runLlvmCompareRuleTests ()
        let g = runLlvmGoldenTests ()
        let f = runLlvmFactTests ()
        let s = runSimplexAgreementTests ()
        if r.Failed + g.Failed + f.Failed + s.Failed = 0 then 0 else 1
    | "facts" ->
        let f = runLlvmFactTests ()
        if f.Failed = 0 then 0 else 1
    | "blocks" | "simplex" | "bricks" ->
        let a = runSimplexAgreementTests ()
        let g = runLlvmSimplexGate ()
        if a.Failed + g.Failed = 0 then 0 else 1
    | "bench" ->
        // The same thing `blade test llvm-bench` runs, spelled as a
        // sub-word for symmetry with `goldens` / `facts` / `blocks`.
        runLlvmBench ()
    | _ ->
        let r = runLlvmDifferentialTestsFor [ cat ]
        if r.Failed = 0 then 0 else 1
