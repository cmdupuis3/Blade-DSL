// Optimization-layer emission pins (src/Optimize.fs -- the semantic-
// equivalence layer). The corpus proves VALUES, and every pass in the layer
// is value-preserving by charter, so the corpus is structurally blind to a
// pass that silently stops firing (or fires where it must not). These pins
// read the emitted C++ instead -- the same rationale as FlatPathTests.
//
// Freeze-idiom recognition is the sharpest case: the recognized and
// unrecognized emissions produce byte-identical output at runtime, so ONLY
// an emission pin can distinguish "early exit derived" from "runs the whole
// budget". The contract half matters just as much in the other direction:
// the recognized idiom must NOT acquire the `while` spelling's BL8010
// budget abort (an optimization may change cost, never contract), and a
// guard that reads the step ordinal outside a lag-1 prefix read must
// DECLINE (not absorbing: such a guard can flip back true after a freeze).
//
// Pure lowering + codegen: no g++, no toolchain. Always runs.
module Blade.Tests.OptimizeTests

open Blade
open Blade.Lowering
open Blade.Tests.TestHarness

let private cppOfSource (testName: string) (src: string) : Result<string, string> =
    try
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir testName))
    with ex -> Error ($"codegen raised: {ex.Message}")

/// The guard-break shape the rec-array machinery emits: `if (!(__vN)) {`.
/// Generated-name-anchored, so the runtime preamble's switch-case breaks and
/// user-visible identifiers cannot match it.
let private guardBreakCount (cpp: string) =
    System.Text.RegularExpressions.Regex.Matches(cpp, @"if \(!\(__v\d+\)\) \{").Count

let private bl8010Count (cpp: string) =
    cpp.Split('\n') |> Array.filter (fun l -> l.Contains "BL8010") |> Array.length

// ---------------------------------------------------------------------------
// Fixtures. Newton for sqrt(2) in three spellings over one budget.
// ---------------------------------------------------------------------------

/// The freeze idiom: unguarded arm, `if G then STEP else prefix(n-1)`,
/// lag-1 guard. Recognition must derive the break and must NOT add the abort.
let private freezeIdiom =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if abs(prefix(n - 1) * prefix(n - 1) - 2.0) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

/// The `while` spelling of the same recurrence: break AND the BL8010 abort.
let private whileSpelling =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n while abs(prefix(n - 1) * prefix(n - 1) - 2.0) > tol -> prefix :: (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5\n"
    + "let root = xs(29)\n"

/// A guard reading the step ordinal OUTSIDE a lag-1 prefix read. Freezing a
/// slice does not freeze `n`, so falseness is not absorbing and recognition
/// must decline -- the loop stays a plain full-budget ternary.
let private ordinalGuardDeclines =
    "type It = Idx<10>\n"
    + "let rec ys: Array<Float like It> =\n"
    + "    match ys with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if n < 5 then prefix(n - 1) * 2.0 else prefix(n - 1))\n"
    + "let last = ys(9)\n"

/// A guard that CALLS a user function with a `mut` parameter. The call
/// mutates the guard's own input, so its first false answer is not
/// absorbing: run to budget, the counter climbs and the guard flips back
/// true; recognized, the trajectory froze at 1 and six of seven `tick`
/// calls vanished (plan-fortran-killer-2.md appendix A). Recognition must
/// decline any non-intrinsic callee -- pure or not, this seam sees names.
let private effectfulGuardDeclines =
    "type C = Idx<1>\n"
    + "type It = Idx<8>\n"
    + "function tick(c: mut Array<Float like C>) -> Bool = {\n"
    + "    c((0 : C)) += 1.0\n"
    + "    c((0 : C)) > 2.0\n"
    + "}\n"
    + "let mut counter: Array<Float like C> = [0.0]\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if tick(counter) then prefix(n - 1) + 1.0 else prefix(n - 1))\n"
    + "let last = xs((7 : It))\n"
    + "let calls = counter((0 : C))\n"

/// The same shape with a PURE user helper in the guard. Semantically this
/// freeze would be sound, but the recognizer cannot tell `residual` from
/// `tick` by name, so it must decline here too until callee purity is
/// discharged against the typed body (the P0 follow-up). Pinning the
/// decline keeps that limitation visible; re-admitting it is a deliberate
/// change to this line, not drift.
let private pureHelperGuardDeclines =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "function residual(x: Float) -> Float = abs(x * x - 2.0)\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if residual(prefix(n - 1)) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

/// A guard whose `abs` is SHADOWED by a user binding of the same name. The
/// intrinsic spelling no longer means the intrinsic, so the callee test must
/// consult the scope, not the name table alone.
let private shadowedIntrinsicGuardDeclines =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "function abs(x: Float) -> Float = if x < 0.0 then 0.0 - x else x\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if abs(prefix(n - 1) * prefix(n - 1) - 2.0) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

// ---------------------------------------------------------------------------

let private runCase (name: string) (src: string) (wantBreaks: int) (wantAborts: int) =
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let breaks = guardBreakCount cpp
        let aborts = bl8010Count cpp
        if breaks = wantBreaks && aborts = wantAborts then
            resultLine Pass name ($"{breaks} guard break(s), {aborts} abort(s)")
            true
        else
            resultLine Fail name ($"expected {wantBreaks} guard break(s) / {wantAborts} abort(s), got {breaks} / {aborts}")
            false

let runOptimizeTests () =
    printHeader "Blade-DSL: Optimization Layer Tests"
    let results =
        [ // Recognition derives the early exit; the abort stays absent --
          // cost changed, contract untouched.
          runCase "freeze_idiom_break_no_abort" freezeIdiom 1 0
          // The `while` spelling keeps its contract: break AND abort.
          runCase "while_spelling_break_and_abort" whileSpelling 1 1
          // Not absorbing -> declined: no break, no abort, full budget.
          runCase "ordinal_guard_declines" ordinalGuardDeclines 0 0
          // A guard that calls a user function declines -- effectful,
          // provably pure, or an intrinsic name the program shadows.
          runCase "effectful_guard_declines" effectfulGuardDeclines 0 0
          runCase "pure_helper_guard_declines" pureHelperGuardDeclines 0 0
          runCase "shadowed_intrinsic_guard_declines" shadowedIntrinsicGuardDeclines 0 0 ]
    let passed = results |> List.filter id |> List.length
    let failed = results.Length - passed
    printFooter "Optimization Layer" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Optimization Layer"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = if failed = 0 then [] else ["see above"] }
