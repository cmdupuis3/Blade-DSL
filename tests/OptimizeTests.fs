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

/// The same shape with a PURE user helper in the guard. `residual`'s effect
/// summary (Blade.Effects, computed from its typed body) is repeatable, so
/// recognition ADMITS the call and derives the break -- the P0 step-2
/// re-admission. v1 declined this by name; the flip is deliberate.
let private pureHelperGuardRecognized =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "function residual(x: Float) -> Float = abs(x * x - 2.0)\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if residual(prefix(n - 1)) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

/// A guard whose `abs` is SHADOWED by a user FUNCTION of the same name. The
/// intrinsic spelling no longer means the intrinsic, so the callee test must
/// consult the scope, not the name table alone -- and what it finds is a
/// declared function with a repeatable summary, so recognition proceeds on
/// that function's own evidence.
let private shadowedIntrinsicPureRecognized =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "function abs(x: Float) -> Float = if x < 0.0 then 0.0 - x else x\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if abs(prefix(n - 1) * prefix(n - 1) - 2.0) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

/// A guard calling a helper that LOOKS pure but calls the effectful `tick`
/// underneath. The summary joins transitively, so the outer helper is not
/// repeatable and recognition declines.
let private transitiveEffectfulGuardDeclines =
    "type C = Idx<1>\n"
    + "type It = Idx<8>\n"
    + "function tick(c: mut Array<Float like C>) -> Bool = {\n"
    + "    c((0 : C)) += 1.0\n"
    + "    c((0 : C)) > 2.0\n"
    + "}\n"
    + "let mut counter: Array<Float like C> = [0.0]\n"
    + "function probe(x: Float) -> Bool = tick(counter) && x < 1000.0\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if probe(prefix(n - 1)) then prefix(n - 1) + 1.0 else prefix(n - 1))\n"
    + "let last = xs((7 : It))\n"

/// A guard calling a pure helper that itself calls another pure helper: the
/// summaries compose through the call chain, so recognition proceeds.
let private transitivePureGuardRecognized =
    "type It = Idx<30>\n"
    + "let tol = 0.000000001\n"
    + "function sq_err(x: Float) -> Float = x * x - 2.0\n"
    + "function residual(x: Float) -> Float = abs(sq_err(x))\n"
    + "let rec xs: Array<Float like It> =\n"
    + "    match xs with\n"
    + "    | zero -> zero\n"
    + "    | zero :: s -> zero :: 1.0\n"
    + "    | prefix :: n -> prefix :: (if residual(prefix(n - 1)) > tol then (prefix(n - 1) + 2.0 / prefix(n - 1)) * 0.5 else prefix(n - 1))\n"
    + "let root = xs(29)\n"

/// An elementwise chain the fusion pass fuses into one nest (plan-fortran-
/// killer.md arc 1): the decision record must say so.
let private fusionChain =
    "type I = Idx<5>\n"
    + "let a: Array<Float like I> = [1.0, 2.0, 3.0, 4.0, 5.0]\n"
    + "let b: Array<Float like I> = [2.0, 3.0, 4.0, 5.0, 6.0]\n"
    + "let c: Array<Float like I> = [0.5, 0.5, 0.5, 0.5, 0.5]\n"
    + "let d: Array<Float like I> = [1.0, 1.0, 1.0, 1.0, 1.0]\n"
    + "let y = a + b * c - d\n"
    + "let total = reduce(y, (+))\n"

// ---------------------------------------------------------------------------

/// The decision record for a source: install a collector, lower, drain.
let private decisionsOf (src: string) : Result<Blade.Effects.Decision list, string> =
    Blade.Effects.Decisions.start ()
    let r =
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok _ -> Ok (Blade.Effects.Decisions.drain ())
    Blade.Effects.Decisions.drain () |> ignore
    r

let private decisionCase (name: string) (src: string) (rule: string)
                         (want: Blade.Effects.Decision -> bool) (describe: string) =
    match decisionsOf src with
    | Error e -> resultLine Fail name e; false
    | Ok ds ->
        let mine = ds |> List.filter (fun d -> d.Rule = rule)
        if mine |> List.exists want then
            resultLine Pass name ($"{mine.Length} `{rule}` decision(s); {describe}")
            true
        else
            let seen = mine |> List.map Blade.Effects.Decisions.render |> String.concat " | "
            let shown = if seen = "" then "no decisions" else seen
            resultLine Fail name ($"wanted {describe}; saw: {shown}")
            false

let private applied (d: Blade.Effects.Decision) =
    match d.Outcome with Blade.Effects.Applied -> true | _ -> false
let private declinedMentioning (needle: string) (d: Blade.Effects.Decision) =
    match d.Outcome with
    | Blade.Effects.Declined why -> why.Contains needle
    | _ -> false

/// Reverse-mode AD of an additive recurrence (docs/plans/structural/01): the
/// recurrence lowers to ONE direct loop in the primal and the adjoint is the
/// same loop run backwards, so the gradient of tests/corpus/ad/008 emits
/// exactly three counted loops -- the primal's, the forward replay's, and
/// the descending adjoint's -- and none of the triangular unroll's `__rk` /
/// `__rm` ordinals. The corpus proves the VALUES (008 and 027-029 at
/// 127/257/509); only an emission pin can tell O(n) from O(n^2).
let private recarrayGradEmission () =
    let name = "recarray_grad_linear_emission"
    let path = "tests/corpus/ad/008_recarray_grad.blade"
    if not (System.IO.File.Exists path) then
        resultLine Fail name $"missing {path} (run from the repo root)"
        false
    else
    match cppOfSource "recarray_grad_linear_emission" (System.IO.File.ReadAllText path) with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let loops = System.Text.RegularExpressions.Regex.Matches(cpp, @"for \(int64_t ").Count
        let triangular = cpp.Contains "__rk" || cpp.Contains "__rm"
        let descending = System.Text.RegularExpressions.Regex.IsMatch(cpp, @"- 1L\) - __k\d+\)")
        if loops = 3 && not triangular && descending then
            resultLine Pass name "3 counted loops (primal, replay, descending adjoint); no triangular ordinals"
            true
        else
            resultLine Fail name ($"expected 3 counted loops, no __rk/__rm, a descending index; got {loops} loop(s), triangular={triangular}, descending={descending}")
            false

/// The dense-halo carousel's tail prefetch must be guarded: on the last
/// step it read one cell past the array (docs/plans/structural/02, 1.5).
/// The guard compares the ordinal against the windowed array's extent.
let private haloCarouselTailGuarded () =
    let name = "halo_carousel_tail_guarded"
    let src =
        "type H = Idx<9>\n"
        + "let a: Array<Float like H> = [1.0, 2.0, 4.0, 7.0, 11.0, 16.0, 22.0, 29.0, 37.0]\n"
        + "let d = method_for(halo<H, [-1, 0, 1]>) <@> lambda(w) -> a(w(1)) - a(w(-1)) |> compute\n"
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let carousel = cpp.Contains "halo carousel"
        let guarded = System.Text.RegularExpressions.Regex.IsMatch(cpp, @"if \(\(size_t\)\([^)]*\) < a\.extents\[0\]\) __car_")
        if carousel && guarded then
            resultLine Pass name "carousel emitted; tail prefetch guarded by a.extents[0]"
            true
        else
            resultLine Fail name ($"carousel={carousel}, guarded={guarded}")
            false

/// A reduction join's share is read by a DIRECT-FOLD leg (docs/plans/
/// structural/03, defect D2): `reduce(e, (+))` beside `prodsum(e, v)` over
/// the named deferred map `e` spells the producer ONCE in the joint loop --
/// the share's `const` -- and the fold leg accumulates that name. The corpus
/// (loops/205) proves the values; only an emission pin can tell one
/// exponential per iteration from two.
let private joinShareReadByDirectFold () =
    let name = "join_share_read_by_direct_fold"
    let src =
        "type J = Idx<11>\n"
        + "let k = method_for(range<J>) <@> lambda(j) -> 0.2 * Float64(j) - 0.4 |> compute\n"
        + "let v = method_for(range<J>) <@> lambda(j) -> 1.0 + 0.1 * Float64(j) |> compute\n"
        + "let e = method_for(k) <@> lambda(b) -> exp(-b * b)\n"
        + "let z, u = object_for(<&!>) <@> (reduce(e, (+)), prodsum(e, v))\n"
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        // One in the lifted kernel body, one in the share's per-iteration const.
        let exps = System.Text.RegularExpressions.Regex.Matches(cpp, @"std::exp\(").Count
        let shared = cpp.Contains "sharing e per iteration"
        let legReadsShare = System.Text.RegularExpressions.Regex.IsMatch(cpp, @"_j0\(\w+_0, e\);")
        if exps = 2 && shared && legReadsShare then
            resultLine Pass name "2 std::exp sites (kernel body + share const); the fold leg reads `e`"
            true
        else
            resultLine Fail name ($"expected 2 std::exp sites, the share note and `_j0(.., e)`; got exps={exps}, shared={shared}, legReadsShare={legReadsShare}")
            false

/// `reduce(method_for(q, k) <@> lambda(a, b) -> f(a, b), (+))` under the
/// default `axes = 1` streams (docs/plans/structural/03, piece D): the
/// checker rewrites the deferred outer product into an outer apply whose row
/// kernel is the fused fold over `k`, so the emission has NO rank-2 pool, no
/// row-mode `__pfrow`/`__pfsrc` scaffolding, and the exponential is applied
/// inside a fold wrapper. The corpus (loops/206) proves the values.
let private outerProductPartialFoldStreams () =
    let name = "outer_product_partial_fold_streams"
    let src =
        "type I = Idx<7>\n"
        + "type J = Idx<11>\n"
        + "function score(a: Float64, b: Float64) -> Float64 = -(a - b) * (a - b) * 8.0\n"
        + "let q = method_for(range<I>) <@> lambda(i) -> 0.3 * Float64(i) - 0.5 |> compute\n"
        + "let k = method_for(range<J>) <@> lambda(j) -> 0.2 * Float64(j) - 0.4 |> compute\n"
        + "let z = reduce(method_for(q, k) <@> lambda(a, b) -> exp(score(a, b)), (+))\n"
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let pools = System.Text.RegularExpressions.Regex.Matches(cpp, @"Array<double, 2>").Count
        let rowMode = cpp.Contains "__pfrow" || cpp.Contains "__pfsrc"
        let fusedFold = System.Text.RegularExpressions.Regex.IsMatch(cpp, @"= __wrap_\d+_\w+\(\w+, std::exp\(")
        if pools = 0 && not rowMode && fusedFold then
            resultLine Pass name "no rank-2 pool, no row-mode scaffolding; exp folded inside the wrapper"
            true
        else
            resultLine Fail name ($"expected no Array<double, 2>, no __pfrow/__pfsrc, a fused exp fold; got pools={pools}, rowMode={rowMode}, fusedFold={fusedFold}")
            false

/// SCRATCH REUSE (plan-fortran-killer-2 section 4, gate 2;
/// Blade.Optimize.planPoolReuse). The demean shape: `y`, a reduce of `y`, `z
/// = y - m`, and the return `z * 0.5`. The return is written into `y`'s dead
/// pool: its declaration is an alias of `y.data`, and `y`'s scope free is
/// spared while `z`'s stays. One reuse in the whole program (only the
/// shape-specialized body has literal extents).
let private poolReuseSrc =
    "type I = Idx<1000>\n"
    + "let v = method_for(range<I>) <@> lambda(i) -> 0.01 * Float64(i) |> compute\n"
    + "function demean(x: T^1) -> T^1 = {\n"
    + "    let y = x * 2.0 + 1.0\n"
    + "    let m = reduce(y, (+)) / Float64(extents(y))\n"
    + "    let z = y - m\n"
    + "    z * 0.5\n"
    + "}\n"
    + "let out = demean(v)\n"
    + "let s = reduce(out, (+))\n"

let private poolReuseReturnTakesDeadPool () =
    let name = "pool_reuse_return_takes_dead_pool"
    match cppOfSource name poolReuseSrc with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let aliases = System.Text.RegularExpressions.Regex.Matches(cpp, @"pool reuse: ").Count
        let retAlias = System.Text.RegularExpressions.Regex.IsMatch(cpp, @"Array<double, 1> __ret\d+ = \{ __v\d+\.data, __ret\d+_extents \};")
        if aliases = 1 && retAlias then
            resultLine Pass name "the specialized body's return aliases the dead pool; one reuse in the program"
            true
        else
            resultLine Fail name ($"expected exactly one pool-reuse alias on a __ret declaration; got aliases={aliases}, retAlias={retAlias}")
            false

/// Two let-bound temporaries share one root pool in a chain (`p`, then `q`
/// after `p` is dead, then `s` after `q` is dead; `r` reads `q` so it keeps
/// its own), and the decision record says so: pools 4 -> 2.
let private poolReuseChainSrc =
    "type I = Idx<1000>\n"
    + "let v = method_for(range<I>) <@> lambda(i) -> 0.01 * Float64(i) |> compute\n"
    + "function chain(x: T^1) -> Float64 = {\n"
    + "    let p = x * 2.0\n"
    + "    let m = reduce(p, (+)) + 0.0\n"
    + "    let q = x - m\n"
    + "    let r = q * 3.0\n"
    + "    let k = reduce(r, (+)) + 0.0\n"
    + "    let s = x + k\n"
    + "    reduce(s, (+))\n"
    + "}\n"
    + "let r = chain(v)\n"

let private poolReuseChainSharesRoot () =
    let name = "pool_reuse_chain_shares_root"
    match cppOfSource name poolReuseChainSrc with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        // Both the generic and the specialized body qualify here (the
        // extents are literal in both), so two bodies x two reusers.
        let aliases = System.Text.RegularExpressions.Regex.Matches(cpp, @"pool reuse: __v\d+ takes __v\d+'s dead pool").Count
        if aliases = 4 then
            resultLine Pass name "two reusers per body take the root's pool"
            true
        else
            resultLine Fail name ($"expected 4 pool-reuse aliases (2 bodies x 2 reusers); got {aliases}")
            false

/// The escape-analysis leak the same work found: a kernel capturing a scalar
/// that was computed FROM an array (`m = reduce(y, (+)) + 0.0`, then `y - m`)
/// used to pin `y` (propagation walked into the scalar's value) so it leaked
/// on every call. A scalar holds no storage; now every function-body pool in
/// this program is freed: deallocate count = allocate count - 1 (the one
/// module-level pool is never scope-freed). No reuse fires here: `z` reads
/// `y` and the return is a scalar.
let private scalarCaptureSrc =
    "type I = Idx<1000>\n"
    + "let v = method_for(range<I>) <@> lambda(i) -> 0.01 * Float64(i) |> compute\n"
    + "function f9(x: T^1) -> Float64 = {\n"
    + "    let y = x * 2.0\n"
    + "    let m = reduce(y, (+)) + 0.0\n"
    + "    let z = y - m\n"
    + "    reduce(z, (+))\n"
    + "}\n"
    + "let r = f9(v)\n"

let private scalarCaptureNoLongerPinsSource () =
    let name = "scalar_capture_no_longer_pins_source"
    match cppOfSource name scalarCaptureSrc with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let allocs = System.Text.RegularExpressions.Regex.Matches(cpp, @"(?<!de)allocate<typename promote").Count
        let frees = System.Text.RegularExpressions.Regex.Matches(cpp, @"deallocate<typename promote").Count
        let aliases = cpp.Contains "pool reuse:"
        if frees = allocs - 1 && not aliases then
            resultLine Pass name ($"{frees} frees for {allocs} allocations (module pool excepted); no reuse")
            true
        else
            resultLine Fail name ($"expected frees = allocs - 1 and no reuse; got allocs={allocs}, frees={frees}, aliases={aliases}")
            false

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
          // Callee judgment by EFFECT SUMMARY (Blade.Effects): an effectful
          // helper declines (directly or through a pure-looking wrapper);
          // a pure helper, a chain of pure helpers, and a pure user
          // function shadowing an intrinsic name are all admitted.
          runCase "effectful_guard_declines" effectfulGuardDeclines 0 0
          runCase "transitive_effectful_guard_declines" transitiveEffectfulGuardDeclines 0 0
          runCase "pure_helper_guard_recognized" pureHelperGuardRecognized 1 0
          runCase "transitive_pure_guard_recognized" transitivePureGuardRecognized 1 0
          runCase "shadowed_intrinsic_pure_recognized" shadowedIntrinsicPureRecognized 1 0
          // The decision record: what each pass decided and why.
          decisionCase "decision_freeze_applied" freezeIdiom "freeze-recognition" applied
              "freeze-recognition applied"
          decisionCase "decision_freeze_declined_effectful" effectfulGuardDeclines "freeze-recognition"
              (declinedMentioning "`tick`") "declined naming `tick`"
          decisionCase "decision_freeze_declined_ordinal" ordinalGuardDeclines "freeze-recognition"
              (declinedMentioning "step ordinal") "declined for the step ordinal"
          decisionCase "decision_fusion_applied" fusionChain "elementwise-fusion" applied
              "elementwise-fusion applied"
          // Reverse-mode AD of an additive recurrence is O(n): loop count pin.
          recarrayGradEmission ()
          // The halo carousel's last-step prefetch stays inside the pool.
          haloCarouselTailGuarded ()
          // Streaming reductions (structural/03): the join share read by a
          // direct-fold leg, and the deferred outer product's partial fold.
          joinShareReadByDirectFold ()
          outerProductPartialFoldStreams ()
          // Scratch reuse across barriers (fortran-killer-2 section 4, gate
          // 2): the alias declarations, the decision record, and the
          // escape-analysis leak the work found.
          poolReuseReturnTakesDeadPool ()
          poolReuseChainSharesRoot ()
          scalarCaptureNoLongerPinsSource ()
          decisionCase "decision_pool_reuse_applied" poolReuseChainSrc "pool-reuse" applied
              "pool-reuse applied" ]
    let passed = results |> List.filter id |> List.length
    let failed = results.Length - passed
    printFooter "Optimization Layer" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Optimization Layer"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = if failed = 0 then [] else ["see above"] }
