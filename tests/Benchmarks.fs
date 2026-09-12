// Differential timing harness: measures the (r!)^d speedup of comm-annotation
// and symmetric-type forms vs their dense equivalents, against the exact
// finite-n cell-count prediction (tests/Oracles.fs). Warns, never fails, on
// slow ratios. Extracted verbatim from Main.fs (audit §2.3).
module Blade.Tests.Benchmarks

open System
open Blade
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
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
open Blade.Build
open Blade.Tests.TestHarness
open Blade.Tests.Oracles

// ===========================================================================
// DIFFERENTIAL TIMING HARNESS
// ===========================================================================
// Measures the runtime SPEEDUP that symmetry exploitation buys, against the
// theoretical (r!)^d ceiling (r = rank of a symmetric group, d = number of
// symmetric dimensions). Two DISTINCT mechanisms are measured, because they
// are fundamentally different:
//
//   (1) COMM-ANNOTATION speedup (iteration count). A `where comm(...)` kernel
//       iterates only canonical tuples (triangular), evaluating the kernel ~r!
//       times less often than the un-annotated dense form. Storage is
//       identical; the win is in HOW MANY times the kernel body runs. The
//       triangular iteration is licensed when the comm-grouped positions share
//       an INDEX-TYPE IDENTITY (same array / tag / named index type — see the
//       Family 1 note and IR.indexSpacesMatch), not by array identity alone.
//       Compared: same kernel, comm vs no-comm.
//
//   (2) SYMMETRIC-TYPE speedup (storage + compute). A SymIdx-typed result
//       stores only C(n+r-1, r) elements and computes only those; its
//       decompact()ed form materializes the full dense n^r block. Compared:
//       the compact symmetric computation vs its decompacted equivalent.
//
// MEASUREMENT: the generated programs run in SPLIT-TIMING mode (see
// CodeGen.setSplitTimingMode), which emits two clocks — one around input-data
// allocation ("<name> input allocation took <t>s") and one around ONLY the
// computation ("<name> completed in <t>s"). The harness parses the latter, so
// the reported ratio reflects COMPUTE time, with input allocation excluded.
// (An older Blade prototype showed allocation can be a large, non-trivial
// fraction of the total, so isolating it materially changes the ratio.) Note
// that a single fused `compute` binding still allocates its OWN output inside
// the timed region — the split separates INPUT setup from the computation, not
// output-allocation from kernel math; the latter is not separable without a
// finer codegen split. We still take the MEDIAN of several runs after a warmup
// (timing is noisy) and use large extents so compute dominates.
//
// POLICY: a genuine error (compile/run/parse failure) is a failure. So is a
// measured ratio BELOW lowerFrac x the cell-count prediction: that is not
// timing noise, it is the symmetry not being exploited — the one thing this
// block exists to detect. (Every arm used to be a PASS, WARN included, which
// made the whole block unfalsifiable: no measurement could ever turn it red.)
// An implausibly HIGH ratio stays a WARN, since overshoot is a benign
// cache-locality effect and the upper bound is only an artifact heuristic.
// The bands are deliberately loose so scheduler noise does not fail the block.

/// Compile an .edgi snippet, run it `runs` times after one warmup, and return
/// the MEDIAN wall time (seconds) parsed from the "<name> completed in <t>s"
/// line the codegen emits. Returns Error on any compile/run/parse failure.
let private timeEdgiProgramOnly (outputDir: string) (caseName: string) (edgiSrc: string) (runs: int) (onlyBinding: string option) : Result<float, string> =
    try
        match lower edgiSrc with
        | Error e -> Error ($"lower failed: {e}")
        | Ok ir0 ->
        // Hard-fail on validation errors rather than timing invalid IR
        // (was `| Error _ -> ir0`).
        match IRValidate.validateIR ir0 with
        | Error validationErrors ->
            Error ($"""IR validation failed: {(String.concat "; " validationErrors)}""")
        | Ok ir ->
            let safeName = "timing_" + caseName.Replace(" ", "_").Replace("=", "")
            // Split-timing codegen: the emitted program reports input-allocation
            // and compute as separate clocks; the "completed in" line we parse
            // below then measures ONLY the compute region, not setup. When
            // onlyBinding is Some name, the compute clock starts precisely at
            // that binding, so producers/decompact chains are attributed to
            // setup and only the final kernel is timed. Restore both flags
            // immediately after so nothing else in this async flow is affected.
            CodeGen.setSplitTimingMode true
            CodeGen.setSplitTimingOnlyBinding onlyBinding
            let (cppCode, _w) = CodeGen.genSelfContainedProgramFromIR ir safeName
            CodeGen.setSplitTimingOnlyBinding None
            CodeGen.setSplitTimingMode false
            let srcPath = Path.Combine(outputDir, safeName + ".cpp")
            File.WriteAllText(srcPath, cppCode)
            let srcAbs = Path.GetFullPath srcPath
            let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
            let exeAbs = Path.ChangeExtension(srcAbs, exeExt)
            let cpsi = ProcessStartInfo("g++", $"-std=c++17 {(optFlags ())} -fopenmp -o \"{exeAbs}\" \"{srcAbs}\"")
            cpsi.RedirectStandardError <- true
            cpsi.UseShellExecute <- false
            cpsi.WorkingDirectory <- Path.GetDirectoryName(srcAbs)
            use cproc = Process.Start(cpsi)
            let cerr = Blade.Runtime.readToEndOffPool cproc.StandardError
            // Guard ExitCode behind WaitForExit's return: reading ExitCode on a
            // still-running (timed-out) process throws "Process must exit ...".
            // Dense baselines emit large C++ whose -O2 compile can be slow, so
            // allow generous headroom and fail gracefully on a genuine overrun.
            if not (cproc.WaitForExit(300000)) then
                (try cproc.Kill() with _ -> ())
                Error "compile timed out (>300s)"
            elif cproc.ExitCode <> 0 then
                Error ($"compile failed: {cerr.Result}")
            else
                // Parse "<...> completed in <t>s" from one run's stdout.
                let runOnce () : Result<float, string> =
                    let rpsi = ProcessStartInfo(exeAbs)
                    rpsi.RedirectStandardOutput <- true
                    rpsi.RedirectStandardError <- true
                    rpsi.UseShellExecute <- false
                    rpsi.WorkingDirectory <- Path.GetDirectoryName(exeAbs)
                    use rproc = Process.Start(rpsi)
                    let rout = Blade.Runtime.readToEndOffPool rproc.StandardOutput
                    // Kill a runaway run on timeout so rout.Result returns
                    // instead of blocking indefinitely on a process that may
                    // never exit. A timed-out or nonzero-exit run is an ERROR,
                    // not a sample: relying on "no completed in" alone would
                    // silently accept a bogus time from a run that printed the
                    // line and then aborted.
                    let rExited = rproc.WaitForExit(180000)
                    if not rExited then
                        (try rproc.Kill() with _ -> ())
                        rproc.WaitForExit(5000) |> ignore
                    let m = System.Text.RegularExpressions.Regex.Match(rout.Result, @"completed in\s+([0-9.eE+-]+)s")
                    if not rExited then Error "run timed out (>180s)"
                    elif rproc.ExitCode <> 0 then Error ($"run exited {rproc.ExitCode}")
                    elif m.Success then
                        (match System.Double.TryParse(m.Groups.[1].Value, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                         | true, v -> Ok v
                         | _ -> Error ($"could not parse elapsed '{m.Groups.[1].Value}'"))
                    else Error "no 'completed in' line in output"
                // Warmup (discarded), then `runs` timed runs; take the median.
                match runOnce () with
                | Error e -> Error ($"warmup run failed: {e}")
                | Ok _ ->
                    let mutable samples = []
                    let mutable err = None
                    for _ in 1 .. max 1 runs do
                        match err with
                        | Some _ -> ()
                        | None ->
                            (match runOnce () with
                             | Ok v -> samples <- v :: samples
                             | Error e -> err <- Some e)
                    match err with
                    | Some e -> Error e
                    | None ->
                        let sorted = samples |> List.sort
                        let median = sorted.[sorted.Length / 2]
                        Ok median
    with ex -> Error ($"exception: {ex.Message}")

/// Default split-timing: clock starts at the first compute binding (whole
/// computation timed, setup excluded). Thin wrapper over the binding-targeted
/// form below.
let private timeEdgiProgram (outputDir: string) (caseName: string) (edgiSrc: string) (runs: int) : Result<float, string> =
    timeEdgiProgramOnly outputDir caseName edgiSrc runs None

/// Run the differential timing harness. Reports, per case, the measured
/// speedup ratio against the exact finite-n cell-count prediction (and the
/// asymptotic (r!)^d ceiling for context), warning (not failing) when the
/// ratio falls below a lower fraction of the prediction (symmetry under-
/// exploited) or implausibly far above it (likely a measurement artifact).
let runDifferentialTimingTests () : Blade.Tests.TestHarness.BlockResult =
    let outputDir = "./generated_cpp_tests"
    printHeader "Differential Timing"
    let caps = capabilities.Value
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile timing cases)."
        // Skipped = 1, not 0: a Skipped = 0 return made a toolchain-less box
        // print "0 passed, 0 failed" with no skip note. Same convention as
        // DiffOracle/InterpDiff.
        { Block = "Differential Timing"; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    else
        Directory.CreateDirectory(outputDir) |> ignore
        // The generated .cpp `#include`s the C++ runtime headers with plain
        // quotes and the g++ invocation below passes no -I, so the headers must
        // sit next to the source. Deploy them HERE rather than relying on some
        // other test block having already populated the shared output dir: this
        // block is excluded from the default `blade test` aggregate, so when it
        // runs standalone (`blade test timing`) nothing else has deployed them
        // and every case fails with "nested_array_utilities.hpp: No such file".
        CodeGen.deployRuntimeHeaders outputDir
        let runs = 5
        let mutable passed = 0
        let mutable warned = 0
        let mutable failed = 0
        let mutable failedNames = []

        // One timing case: build the symmetric/comm variant and the dense
        // variant, time both, report the ratio vs the (r!)^d ceiling.
        // `mkSym` and `mkDense` produce the two .edgi sources for extent n.
        let runRatioCase (label: string) (r: int) (d: int) (n: int)
                         (expectedRatio: float)
                         (symSrc: string) (denseSrc: string) =
            let ceiling = (fact r) ** float d
            let tSym = timeEdgiProgram outputDir (label + "_sym") symSrc runs
            let tDense = timeEdgiProgram outputDir (label + "_dense") denseSrc runs
            match tSym, tDense with
            | Error e, _ ->
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"symmetric variant: {e}")
                failed <- failed + 1
                failedNames <- failedNames @ [label]
            | _, Error e ->
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"dense variant: {e}")
                failed <- failed + 1
                failedNames <- failedNames @ [label]
            | Ok ts, Ok td ->
                // ratio = dense / symmetric (how many times faster the
                // symmetric form is). Guard against a zero/degenerate sym time.
                let ratio = if ts > 0.0 then td / ts else 0.0
                // The WARN threshold is measured against expectedRatio — the
                // ACHIEVABLE target at this problem size. For d=1 this is the
                // asymptotic (r!)^d ceiling (large n reaches it); for product
                // symmetry at small n it is the EXACT finite-n prediction
                // (LM)^r / [C(L+r-1,r)·C(M+r-1,r)], since the asymptote is
                // unreachable when (LM)^r forbids large n. Display shows the
                // achievable target and, when it differs, the asymptotic ceiling.
                let targetStr =
                    if abs (expectedRatio - ceiling) < 1e-9 then
                        // Exact finite-n target coincides with the asymptote
                        // (large n): show one figure.
                        sprintf "exact=%.2fx (=asymptote (r!)^d=%.0fx)" expectedRatio ceiling
                    else
                        sprintf "exact finite-n=%.2fx (asymptote (r!)^d=%.0fx)" expectedRatio ceiling
                let detail =
                    sprintf "n=%d r=%d d=%d | sym=%.4gs dense=%.4gs | ratio=%.2fx vs %s"
                        n r d ts td ratio targetStr
                // The prediction (expectedRatio) is a CELL-COUNT limit: how many
                // fewer iterations the symmetric arm performs. The measured
                // wall-clock ratio is NOT a pure function of cell count — the
                // symmetric result has a much smaller working set, so better
                // cache locality can make it faster PER CELL and push the ratio
                // ABOVE the cell-count prediction (a real, benign effect, e.g.
                // r=2 measured 4.43x vs cell-count 3.75x). So this is a tolerance
                // BAND, not a one-sided floor:
                //   - below lowerFrac·prediction  -> FAIL (genuine shortfall: the
                //     symmetry is not being exploited — the failure signal, and
                //     the reason this block exists. This arm used to report PASS
                //     with a "WARN:" prefix in the detail string, so the one
                //     outcome the block is meant to catch could never turn it
                //     red. lowerFrac is loose (0.70) precisely so that scheduler
                //     noise stays inside the band and only a real regression —
                //     triangular iteration silently reverting to full-hypercube —
                //     crosses it.)
                //   - within [lowerFrac·pred, upperMult·pred] -> clean pass
                //     (the expected zone, including a cache-bonus overshoot);
                //   - above upperMult·prediction   -> WARN (implausibly fast —
                //     likely a measurement artifact, e.g. a degenerate dense arm
                //     doing near-zero work, NOT a real speedup). Stays a PASS:
                //     overshoot is benign and the bound is a heuristic.
                let lowerFrac = 0.70
                let upperMult = 5.0
                if ratio < lowerFrac * expectedRatio then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label
                        (sprintf "%s -- below %.0f%% of cell-count prediction (symmetry under-exploited)" detail (lowerFrac * 100.0))
                    failed <- failed + 1
                    failedNames <- failedNames @ [label]
                elif ratio > upperMult * expectedRatio then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label
                        (sprintf "%s -- WARN: %.1fx the cell-count prediction (implausible; check for measurement artifact)" detail (ratio / expectedRatio))
                    warned <- warned + 1
                    passed <- passed + 1
                else
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label detail
                    passed <- passed + 1

        // -------------------------------------------------------------------
        // FAMILY 1 — COMM-ANNOTATION (iteration-count) speedup, d = 1 group.
        // The only difference between the two variants is the `where comm(...)`
        // clause, which switches full-hypercube iteration to canonical-tuple
        // (triangular) iteration. Theoretical ceiling: r! (one symmetric group).
        //
        // WHAT LICENSES THE TRIANGULAR ITERATION (arc-1 corrected model):
        // `comm` declares the arguments interchangeable FOR ITERATION (it is
        // NOT an assertion that g(x,y) = g(y,x) — a comm kernel can still be
        // Reynolds-antisymmetrized to nonzero). The compiler grants triangular
        // iteration (SCCommutative/SCBoth) only when the comm-grouped
        // positions hold the SAME ARRAY (identity at the call site). Nominal
        // index-type identity across DISTINCT arrays licenses nothing — the
        // old §14.6 "shared index spaces are the payoff" rule is refuted
        // (proofs.md shared_units_insufficient) and was removed; see corpus
        // symmetry/014 for the pinned rectangular behavior. For a repeated
        // MULTI-DIM array the license is the JOINT simplex over compound
        // index tuples (r! once, docs/formalism.md §12.4), realized by level
        // fusion (IRLoopStructure.fuseJointSLevels) — never per-dimension.
        //
        // These cases use method_for(A, A, ...) — the same array repeated —
        // which is exactly the licensed situation.
        // -------------------------------------------------------------------
        // Heavy kernel body, written as a block-bodied lambda with a chain of
        // local `let` accumulators (Blade DOES support let bindings in a `{ }`
        // block body; its `in` keyword is for co-iteration, not an OCaml-style
        // let-in binder). Each stage is data-dependent on the previous accX and
        // a cycled input variable, so -O2 cannot fold or hoist the chain; a
        // modulo keeps values bounded (no int64 overflow UB). Distinct accX
        // names avoid relying on shadowing semantics. Raising `stages` raises
        // per-element compute, letting the dense n^r baseline reach 1-5s at a
        // SMALL n (so dense storage stays well under the memory ceiling).
        // Returns the full "{ ... accN }" block string for the lambda body.
        //   acc0 = v0
        //   acc{s+1} = (accS * v_{(s+1) mod r} + C_s) % M
        let heavyBody (vars: string list) (stages: int) : string =
            let r = List.length vars
            let m = 1000003   // prime modulus, keeps values bounded
            let sb = System.Text.StringBuilder()
            sb.Append("{\n") |> ignore
            sb.Append($"    let acc0 = {(List.head vars)}\n") |> ignore
            for s in 0 .. stages - 1 do
                let v = vars.[(s + 1) % r]
                let c = 2 * s + 1
                sb.Append(sprintf "    let acc%d = (acc%d * %s + %d) %% %d\n" (s+1) s v c m) |> ignore
            sb.Append($"    acc{stages}\n") |> ignore
            sb.Append("}") |> ignore
            sb.ToString()

        // Unary heavy kernel (one input var) for the elementwise-on-symmetric
        // family: same bounded Horner-mod accumulator chain, single argument.
        let heavyBodyUnary (var: string) (stages: int) : string =
            let m = 1000003
            let sb = System.Text.StringBuilder()
            sb.Append("{\n") |> ignore
            sb.Append($"    let acc0 = {var}\n") |> ignore
            for s in 0 .. stages - 1 do
                let c = 2 * s + 1
                sb.Append(sprintf "    let acc%d = (acc%d * %s + %d) %% %d\n" (s+1) s var c m) |> ignore
            sb.Append($"    acc{stages}\n") |> ignore
            sb.Append("}") |> ignore
            sb.ToString()

        let commCase (r: int) (n: int) (stages: int) =
            // Integer array values 1..n (int64 arrays; the kernel uses % so
            // values must be integral, not floats).
            let aLit = [ for i in 1 .. n -> string i ] |> String.concat ", "
            let vars = [ for k in 0 .. r-1 -> string (char (int 'a' + k)) ]
            let varList = String.concat ", " vars
            let body = heavyBody vars stages
            let args = List.replicate r "A" |> String.concat ", "
            // comm variant: kernel carries comm over all args, and the repeated
            // array A gives shared index-type identity -> triangular iteration.
            let symSrc =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let k = lambda({varList}) where comm({varList}) -> {body}\n" +
                "let result = L <@> k |> compute\n"
            // dense variant: identical kernel, NO comm -> full hypercube iteration.
            let denseSrc =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let k = lambda({varList}) -> {body}\n" +
                "let result = L <@> k |> compute\n"
            runRatioCase ($"comm r={r} n={n}") r 1 n (exactSimplexRatio r [n]) symSrc denseSrc
        // n chosen so the dense n^r array stays under the 2GB cap (int64):
        // r=2 -> 15000^2 = 1.8GB, r=3 -> 620^3 = 1.9GB, r=4 -> 124^4 = 1.9GB.
        // stages=5 Horner steps target ~1-5s dense compute (tune if needed).
        // n chosen so the dense n^r array stays ~0.45-0.5 GB (int64) — a ~75%
        // cut from the earlier 2GB sizing, to clear the memory ceiling:
        // r=2 -> 7500^2 = 0.45GB, r=3 -> 390^3 = 0.47GB, r=4 -> 87^4 = 0.46GB.
        // stages raised to ~20 to keep dense compute in the 1-5s band despite
        // the smaller element count (per-element work compensates for fewer
        // elements). Tune `stages` if observed times fall outside 1-5s.
        // -------------------------------------------------------------------
        // FAMILY 3 — PRODUCT SYMMETRY (r!)^d at d = 2 (THE headline test),
        // via the FIBER (dimensionally-curried) construct that actually
        // expresses product symmetry (per formalism §9.2, §14.5-14.6 and the
        // S-dim definition S = rank − irank). A 3-D array
        // A: Array<.., LatIdx, LonIdx, TimeIdx> is repeated in a comm kernel
        // whose two args are TimeIdx FIBERS: k(a: Array<.. TimeIdx>, b: ..)
        // where comm(a, b). The Time fiber is CONSUMED inside the kernel (a
        // heavy per-element reduce), so the symmetric iteration grid is exactly
        // the two outer dims Lat, Lon. comm makes the two array positions
        // interchangeable; because Lat and Lon are each shared S-dims, EACH gets
        // independent triangular iteration -> (2!)^2 = 4x. (The earlier flat
        // scalar form A<Idx,Idx> with scalar args was malformed for product
        // symmetry: with nothing curried as a fiber, all dims fell into one grid
        // and collapsed to a single (r*d)! simplex. The type probe + this fiber
        // form are the corrected expression.)
        //
        // sym arm = comm (per-dim triangular on Lat,Lon); dense arm = no comm
        // (full Lat×Lon×Lat×Lon grid). Both consume the Time fiber identically,
        // so the fiber reduce cost cancels and the ratio isolates (2!)^2.
        //
        // Sizing: L=M=30 outer dims, T=40 fiber. dense grid = (L*M)^2 ≈ 8.1e5
        // outer cells, each a heavy T-element reduce -> ~1-5s. Arrays are tiny
        // (L*M*T = 36000 elements); the cost is in the grid×fiber product.
        let prodSymFiberCase (rArgs: int) (lDim: int) (mDim: int) (tDim: int) (stages: int) =
            // 3-D int array A[Lat][Lon][Time] built by the internal fill_random
            // constructor (rand() % 1000). This is a timing/ratio test: the values
            // never enter the comparison (runRatioCase measures only the sym/dense
            // wall-clock ratio, never results), so a random fill is equivalent to
            // any literal here and keeps the generated source tiny (was a 72k-
            // element literal at r=2 -> ~25s g++ compile).
            // Heavy per-fiber reduce: Horner-mod binop consumes the Time fiber,
            // putting T*stages-ish work in each outer (Lat,Lon) cell.
            let binop =
                let muls = [ for _s in 1 .. stages -> " * x" ] |> String.concat ""
                sprintf "lambda(acc, x) -> (acc + x%s) %% 1000003" muls
            let typeDecl =
                $"type LatIdx = Idx<{lDim}>\ntype LonIdx = Idx<{mDim}>\ntype TimeIdx = Idx<{tDim}>\n"
            let arrDecl = "let A: Array<Int64 like LatIdx, LonIdx, TimeIdx> = fill_random(1000)\n"
            // r fiber params (a, b, c, ...) all in one comm group; each reduced
            // and summed. method_for(A, A, ..) repeats A r times. Correct product
            // symmetry makes EACH outer dim (Lat, Lon) a rank-r symmetric group
            // independently -> (r!)^d. The dense (no-comm) arm iterates the full
            // (L*M)^r grid.
            let vars = [ for k in 0 .. rArgs - 1 -> string (char (int 'a' + k)) ]
            let paramList =
                vars |> List.map (fun v -> $"{v}: Array<Int64 like TimeIdx>") |> String.concat ", "
            let varCsv = String.concat ", " vars
            let reduceSum = vars |> List.map (fun v -> $"reduce({v}, {binop})") |> String.concat " + "
            let args = List.replicate rArgs "A" |> String.concat ", "
            let kernelComm =
                $"let k = lambda({paramList}) where comm({varCsv}) -> {reduceSum}\n"
            let kernelNoComm =
                $"let k = lambda({paramList}) -> {reduceSum}\n"
            let symSrc = typeDecl + arrDecl + $"let L = method_for({args})\n" + kernelComm + "let result = L <@> k |> compute\n"
            let denseSrc = typeDecl + arrDecl + $"let L = method_for({args})\n" + kernelNoComm + "let result = L <@> k |> compute\n"
            // Exact finite-n product-symmetry target: each of the two outer
            // axes (Lat, Lon) is a rank-r symmetric group, so the achievable
            // limit is the per-axis product ext^r / C(ext+r-1, r). Approaches
            // (r!)^d only as the extents → ∞, which is unreachable at r≥3 since
            // dense scales as (LM)^r.
            let expectedRatio = exactSimplexRatio rArgs [lDim; mDim]
            runRatioCase ($"prodsym-fiber r={rArgs} d=2 L={lDim} M={mDim} T={tDim}") rArgs 2 (lDim * mDim) expectedRatio symSrc denseSrc
        // T kept SMALL (small result allocation). The runtime lever is `stages`
        // (inline Horner length): it raises per-cell FLOPs WITHOUT growing the
        // result allocation. (The array data is built by fill_random, so it no
        // longer bloats the source -- the dense literal it replaced blew up
        // compile time, a 7.3MB source at T=900.) A moderate stages (~60) lifts
        // each arm into the ~0.15-0.7s
        // range — well clear of the timer underflow that hit r=4 (was ~1.3ms) —
        // while keeping the generated Horner expression and the data small. (We do
        // NOT chase a 5s wall-clock: the dense cell count (L*M)^r times a heavy
        // enough kernel is unnecessary, and the ratio is independent of stages —
        // it cancels between the sym and dense arms.)
        prodSymFiberCase 2 30 30 80 60
        prodSymFiberCase 3 8 8 60 60
        prodSymFiberCase 4 5 5 40 60

        commCase 2 7500 20
        commCase 3 390 20
        commCase 4 87 20

        // -------------------------------------------------------------------
        // FAMILY 2 — SYMMETRIC-TYPE (storage) speedup, d = 1 group.
        // The symmetric computation produces a SymIdx result (compact storage,
        // canonical-tuple compute); the dense variant decompact()s it to the
        // full n^r block.
        //
        // HONEST SCOPE NOTE: the dense arm here is `reynolds` THEN `decompact`.
        // The reynolds step already computed only the C(n+r-1, r) canonical
        // values — decompact merely WIDENS that into n^r storage by scattering.
        // So this family measures the symmetric form against the cost of
        // *materializing* the dense block (storage + scatter), NOT against a
        // from-scratch dense RECOMPUTE. The two are different: a true dense
        // recompute (evaluating the kernel at every one of n^r cells) would
        // show the full r! compute advantage, but Blade has no surface form
        // that forces a non-symmetric recompute of a symmetric quantity. Family
        // 1 (comm vs no-comm) is the clean iteration-count measurement; treat
        // Family 2 as the storage/scatter overhead of decompaction. Its ratio
        // is therefore expected to sit well below the r! ceiling and will
        // usually trip the WARN — that is informative, not a regression.
        // -------------------------------------------------------------------
        // FAMILY 2 (reframed) — ELEMENTWISE-ON-SYMMETRIC speedup. Both arms
        // first build the SAME symmetric array `sym` with a PLAIN comm kernel
        // (NOT reynolds — reynolds is heavier machinery in the compiler and was
        // implicated in an output-type bug when its result was fed into a
        // downstream elementwise map; a plain comm kernel produces the same
        // SymIdx storage with simpler lowering). Then:
        //   symmetric arm: map a heavy unary kernel over `sym` directly — the
        //     array stays SymIdx, so the map iterates only the C(n+r-1, r)
        //     canonical elements.
        //   dense arm: decompact `sym` to the full n^r block, then map the SAME
        //     heavy kernel over all n^r elements.
        // Elementwise work scales with ELEMENT COUNT, so the ratio tracks
        // n^r / C(n+r-1, r) -> r! (the storage+iteration speedup), the same
        // combinatorial win Family 1 shows via iteration alone.
        let symTypeCase (r: int) (n: int) (stages: int) =
            let aLit = [ for i in 1 .. n -> string i ] |> String.concat ", "
            let vars = [ for k in 0 .. r-1 -> string (char (int 'a' + k)) ]
            let varList = String.concat ", " vars
            let args = List.replicate r "A" |> String.concat ", "
            // Cheap symmetric producer (sums args under comm) — keeps producer
            // cost low so the timed elementwise map dominates.
            let prodSum = vars |> String.concat " + "
            let hBody = heavyBodyUnary "e" stages
            // symmetric arm: build sym via comm, heavy elementwise map over compact sym.
            let symSrc =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let g = lambda({varList}) where comm({varList}) -> {prodSum}\n" +
                "let sym = L <@> g |> compute\n" +
                $"let h = lambda(e) -> {hBody}\n" +
                "let result = method_for(sym) <@> h |> compute\n"
            // dense arm: build a genuinely dense n^r array via a NO-COMM
            // producer (full hypercube iteration — the same dense construction
            // Family 1 uses), then map the SAME unary heavy kernel h over it.
            // This avoids decompact entirely: decompact-then-map iterated the
            // widened array at less than n^r work (its time fell with rank),
            // which made the ratio under-report. A no-comm producer yields a
            // real dense n^r array so the dense map does full n^r work, giving
            // a clean baseline directly comparable to the symmetric arm (same h
            // per element, n^r vs C(n+r-1,r) elements -> ratio tracks r!).
            let denseSrc =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let g = lambda({varList}) -> {prodSum}\n" +
                "let dense = L <@> g |> compute\n" +
                $"let h = lambda(e) -> {hBody}\n" +
                "let result = method_for(dense) <@> h |> compute\n"
            runRatioCase ($"symtype r={r} n={n}") r 1 n (exactSimplexRatio r [n]) symSrc denseSrc
        // dense n^r ~0.45-0.5 GB, same n as Family 1.
        // Family 2 (elementwise-over-symmetric) timing. The feature it needs —
        // an elementwise rank-0 kernel over an already-symmetric array, staying
        // compact — now works end-to-end (type deduction + codegen + value
        // checks all green). Same n as Family 1, so the canonical element count
        // C(n+r-1,r), the dense count n^r, and the theoretical r! ceiling all
        // MATCH Family 1's. The ratios are therefore directly comparable: this
        // family's speedup comes from SIMPLE SYMMETRY (iterating an already-
        // compact symmetric array) vs Family 1's from COMMUTATIVITY (triangular
        // iteration license over dense-stored arrays). Both should approach the
        // same r! ceiling via different mechanisms.
        symTypeCase 2 7500 20
        symTypeCase 3 390 20
        symTypeCase 4 87 20


        // -------------------------------------------------------------------
        // DECOMPACT PROBE — verifies decompact actually expands compact -> dense.
        // Both arms share the SAME triangular producer (sym = comm-grouped
        // method_for, C(n+r-1,r) work) so producer cost cancels. They differ
        // ONLY in what the heavy unary kernel h is mapped over:
        //   COMPACT arm:   method_for(sym)              -> C(n+r-1,r) evaluations
        //   DECOMPACT arm: method_for(decompact(sym,0)) -> should be n^r evals
        // If decompact correctly materializes and is iterated as a dense n^r
        // array, the decompact arm does r! times MORE work than the compact arm,
        // so decompact/compact ~ r! (2, ~5.8, ~22 for r=2,3,4). If decompact
        // instead under-iterated (visited only the compact cells), both arms
        // would do ~equal work and the ratio would collapse to ~1.0. So here
        // HIGH ratio = healthy (decompact expands), ratio ~1.0 = under-iteration.
        // (Direct codegen inspection already confirmed the dense n^r read; this
        // probe is the standing regression guard.) Reported as a readout.
        let runDecompactProbe (r: int) (n: int) (stages: int) =
            let aLit = [ for i in 1 .. n -> string i ] |> String.concat ", "
            let vars = [ for k in 0 .. r-1 -> string (char (int 'a' + k)) ]
            let varList = String.concat ", " vars
            let args = List.replicate r "A" |> String.concat ", "
            let prodSum = vars |> String.concat " + "
            let hBody = heavyBodyUnary "e" stages
            let producer =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let g = lambda({varList}) where comm({varList}) -> {prodSum}\n" +
                "let sym = L <@> g |> compute\n" +
                $"let h = lambda(e) -> {hBody}\n"
            // Compact arm: heavy map directly over the compact symmetric array.
            let compactSrc = producer + "let result = method_for(sym) <@> h |> compute\n"
            // Decompact arm: widen to dense, then heavy map over the dense array.
            // Decompact arm: FULLY densify the compact symmetric array by
            // chaining r-1 decompacts (the "to-the-right peel": each step frees
            // the next dim, freed Idx dims accumulate on the left, the residual
            // group stays last — decompact at dims 0,1,..,r-2). The final array
            // is a genuine dense n^r block, so its heavy map should match the
            // dense reference arm (dec/dense ~ 1.0). A single decompact would
            // free only one dim (leaving an r-1 symmetric tail), which is why
            // the earlier single-decompact arm read below the dense ceiling at
            // r>=3 — correct single-dim fission, but not full densification.
            let decompactChain =
                let mutable lines = [ "let dc0 = decompact(sym, 0)\n" ]
                for k in 1 .. r - 2 do
                    lines <- lines @ [ $"let dc{k} = decompact(dc{(k-1)}, {k})\n" ]
                let lastName = $"dc{(r - 2)}"
                String.concat "" lines + $"let dense = {lastName}\n"
            let decompactSrc =
                producer +
                decompactChain +
                "let result = method_for(dense) <@> h |> compute\n"
            // Fully-dense reference arm: build a genuine n^r dense array via a
            // NO-COMM rectangular producer, then the same heavy map. This is the
            // known-dense ceiling. If the decompact arm iterates a true n^r
            // array, its map time should match THIS arm; if it tracks the
            // compact arm instead, decompact under-iterates. (Its producer is
            // rectangular n^r, but only the map matters for the decompact-vs-
            // dense comparison since both run the identical map afterward.)
            let denseSrc =
                $"let A = [{aLit}]\n" +
                $"let L = method_for({args})\n" +
                $"let g2 = lambda({varList}) -> {prodSum}\n" +
                "let dense = L <@> g2 |> compute\n" +
                $"let h = lambda(e) -> {hBody}\n" +
                "let result = method_for(dense) <@> h |> compute\n"
            let label = $"decompact-probe r={r} n={n}"
            // Time ONLY the final map (binding "result"): the producer and the
            // decompact chain are attributed to setup, so what we compare is
            // purely the heavy map over each arm's array. The decompact and
            // dense arms should then emit identical map code over identical
            // dense shapes and run in the same time (dec/dense ~ 1.0) — that
            // equivalence IS the certification that chained decompaction yields
            // a genuinely dense array indistinguishable from a native one.
            let tC = timeEdgiProgramOnly outputDir (label + "_compact") compactSrc runs (Some "result")
            let tD = timeEdgiProgramOnly outputDir (label + "_decompact") decompactSrc runs (Some "result")
            let tF = timeEdgiProgramOnly outputDir (label + "_dense") denseSrc runs (Some "result")
            match tC, tD, tF with
            | Error e, _, _ ->
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"compact arm: {e}")
                failed <- failed + 1
                failedNames <- failedNames @ [label]
            | _, Error e, _ ->
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"decompact arm: {e}")
                failed <- failed + 1
                failedNames <- failedNames @ [label]
            | _, _, Error e ->
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"dense arm: {e}")
                failed <- failed + 1
                failedNames <- failedNames @ [label]
            | Ok tc, Ok td, Ok tf ->
                // Three reference points for the same heavy map:
                //   compact   = C(n+r-1,r) work (floor)
                //   dense     = genuine n^r work (ceiling, ~constant across rank)
                //   decompact = the chained-decompact result, now FULLY densified
                //               (r-1 decompacts), so it should match `dense`.
                // Healthy: dec/dense ~ 1.0 (full densification) AND dec/comp ~ r!
                // (it does r! times more work than the compact arm). If the chain
                // failed to fully densify (e.g. a residual symmetric tail), the
                // decompact arm would run faster than dense -> dec/dense < 1.
                let dOverC = if tc > 0.0 then td / tc else 0.0   // ~r! when fully dense
                let dOverF = if tf > 0.0 then td / tf else 0.0   // ~1.0 when fully dense
                let rFact = [1 .. r] |> List.fold (*) 1
                // Exact finite-n limit for the compact (single rank-r simplex)
                // arm: n^r / C(n+r-1, r). dec/comp should approach this, not the
                // r! asymptote, at finite n (the same gap the comm family shows).
                let exactComp = exactSimplexRatio r [n]
                let detail =
                    sprintf "n=%d r=%d | compact=%.4gs decompact=%.4gs dense=%.4gs | dec/comp=%.2fx (exact~%.2f, asymptote r!=%d) dec/dense=%.2fx (full~1.0)"
                        n r tc td tf dOverC exactComp rFact dOverF
                // Healthy: chained decompact matches the dense ceiling
                // (dec/dense near 1). A shortfall (dec/dense well below 1) means
                // the chain did not fully densify — e.g. a residual symmetric
                // tail still iterated triangularly.
                // dec/dense < 0.6 is a FAILURE, not a warning: the probe's own
                // conclusion in that case is "the chain did not fully densify",
                // i.e. decompact is broken. It used to report PASS (with "WARN:"
                // only in the detail text), so the probe could not fail on the
                // exact defect it measures. The 0.6 floor leaves ~40% headroom
                // for timing noise below the ideal ratio of 1.0.
                if dOverF >= 0.6 then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label detail
                    passed <- passed + 1
                else
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label
                        (sprintf "%s -- chained decompact runs well under the dense ceiling (dec/dense < 0.6) -> not fully densified" detail)
                    failed <- failed + 1
                    failedNames <- failedNames @ [label]
        // n values are smaller than the (r!)^d families because the decompact
        // arm now CHAINS r-1 decompacts, each materializing a full dense n^r
        // intermediate; several coexist plus the dense-reference array. Sized so
        // peak residency stays ~250MB. The probe reports ratios, so absolute n
        // does not affect what it measures.
        runDecompactProbe 2 2700 20
        runDecompactProbe 3 195 20
        runDecompactProbe 4 50 20

        let metric =
            [ sprintf "%d passed" passed ]
            @ (if warned > 0 then [ sprintf "%d warned" warned ] else [])
            @ (if failed > 0 then [ sprintf "%d failed" failed ] else [])
        printFooter "Differential Timing" metric
        { Block = "Differential Timing"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }

