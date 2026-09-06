// Halo access-description pins (src/Types.fs `HaloAccess`, src/IRAccess.fs)
// and the reverse-mode halo ROUTE differential (docs/plans/structural/02,
// sections 2.1 and 3.3).
//
// The corpus proves VALUES under whichever route the gate selects, and the
// two routes -- the kept map with a gather adjoint, the lowered map with a
// scatter adjoint -- are the same function by construction. That is exactly
// what the corpus cannot see: one route silently taking over from the
// other, or the gather drifting from the scatter by an accumulation-order
// change that only shows in the last bit. So these pins (a) read the
// emitted C++ for the ROUTE (the gather's guarded loops exist only with
// the gate on, and the kept map adds one carousel), and (b) run BOTH
// routes' executables on the same programs and compare their printed
// output byte-for-byte, the timing line excepted (it is the one line that
// is not a value). The emission and record pins need no toolchain; the
// differential skips without g++ (`Build.capabilities.HasGpp`).
module Blade.Tests.AccessTests

open System.IO
open Blade
open Blade.Types
open Blade.Lowering
open Blade.Tests.TestHarness

let private gateVar = "BLADE_AD_HALO_GATHER"

/// Run `f` with the route gate set to `value` (None = unset: the default,
/// gather), restoring whatever was there. The gate is read per call by
/// design (GradCommon.haloGatherEnabled), which is what makes this work.
let private withGate (value: string option) (f: unit -> 'a) : 'a =
    let prev = System.Environment.GetEnvironmentVariable gateVar
    System.Environment.SetEnvironmentVariable(gateVar, (match value with Some s -> s | None -> null))
    try f () finally System.Environment.SetEnvironmentVariable(gateVar, prev)

let private cppOf (name: string) (gate: string option) (src: string) : Result<string, string> =
    withGate gate (fun () ->
        try
            match lower src with
            | Error e -> Error $"lower: {e}"
            | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir name))
        with ex -> Error $"codegen raised: {ex.Message}")

/// The gather loop's window guard, `((0L <= __vN) && (__vN < ML))`:
/// generated-name-anchored, so user identifiers cannot match it.
let private gatherGuardCount (cpp: string) =
    System.Text.RegularExpressions.Regex.Matches(cpp, @"\(\(0L <= __v\d+\) && \(__v\d+ < \d+L\)\)").Count

let private carouselCount (cpp: string) =
    cpp.Split('\n') |> Array.filter (fun l -> l.Contains "// halo carousel:") |> Array.length

let private corpusSource (file: string) : Result<string, string> =
    let path = Path.Combine("tests", "corpus", "ad", file)
    if File.Exists path then Ok (File.ReadAllText path)
    else Error $"missing {path} (run from the repo root)"

// ---------------------------------------------------------------------------
// The record.
// ---------------------------------------------------------------------------

/// `HaloAccess` over the one-sided lag set `[-2, -4]` (loops/078, ad/032):
/// the reach includes the centre, the interior starts at 4 and loses 4,
/// the demand over the whole interior is the inner extent, and the
/// transpose of input ordinal 5 is the two windows that read it.
let private recordPins () =
    let name = "halo_access_record_lag_set"
    let h = haloAccessOf "T" [ -2; -4 ] false
    let parsed = haloAccessOfTag "__halowin|d:T|-2,-4"
    let reach = haloReach h
    let demand = haloDemand h (0L, 4L)
    let contributors = haloContributors h 4L 5L
    let checks =
        [ "Start = 4", h.Start = 4L
          "Shrink = 4", h.Shrink = 4L
          "reach = [-4; -2; 0]", reach = [ -4; -2; 0 ]
          "offset 0 in reach", haloOffsetInReach h 0
          "offset -3 in reach", haloOffsetInReach h -3
          "offset 1 outside", not (haloOffsetInReach h 1)
          "offset -5 outside", not (haloOffsetInReach h -5)
          "demand [0,4) = [0,8)", demand = (0L, 8L)
          "contributors of 5 = [(-2,3); (0,1)]", contributors = [ (-2, 3L); (0, 1L) ]
          "tag parses to the same record", parsed = Some { h with Inner = "T" } ]
    match checks |> List.tryFind (fun (_, ok) -> not ok) with
    | None -> resultLine Pass name $"{checks.Length} record facts"; true
    | Some (what, _) -> resultLine Fail name $"failed: {what}"; false

// ---------------------------------------------------------------------------
// Route emission.
// ---------------------------------------------------------------------------

/// Gate on: the map is kept (one more carousel than the scatter route --
/// the differentiated function replays it) and the adjoint is gather loops
/// with the window guard, one per offset the kernel reads. Gate off: no
/// guard anywhere; the map has lowered into a construction loop.
let private routeEmission () =
    let name = "halo_route_emission"
    match corpusSource "030_halo_stencil_grad_linear.blade" with
    | Error e -> resultLine Fail name e; false
    | Ok src ->
        match cppOf (name + "_gather") None src, cppOf (name + "_scatter") (Some "0") src with
        | Error e, _ | _, Error e -> resultLine Fail name e; false
        | Ok on, Ok off ->
            let guardsOn = gatherGuardCount on
            let guardsOff = gatherGuardCount off
            let carOn = carouselCount on
            let carOff = carouselCount off
            if guardsOn = 2 && guardsOff = 0 && carOn = carOff + 1 then
                resultLine Pass name $"gather: 2 guarded loops, {carOn} carousels; scatter: no guard, {carOff} carousels"
                true
            else
                resultLine Fail name $"expected gather 2 guards / scatter 0, carousels on = off + 1; got guards {guardsOn}/{guardsOff}, carousels {carOn}/{carOff}"
                false

/// A computed offset is outside the gather's read discipline: with the gate
/// ON the map still takes the scatter route (no guard emitted).
let private computedOffsetDeclines () =
    let name = "halo_gather_declines_computed_offset"
    match corpusSource "033_halo_stencil_grad_computed_offset.blade" with
    | Error e -> resultLine Fail name e; false
    | Ok src ->
        match cppOf name None src with
        | Error e -> resultLine Fail name e; false
        | Ok cpp ->
            let guards = gatherGuardCount cpp
            if guards = 0 then resultLine Pass name "no gather guard: the scatter route took it"; true
            else resultLine Fail name $"expected no gather guard, found {guards}"; false

// ---------------------------------------------------------------------------
// The differential.
// ---------------------------------------------------------------------------

let private outputDir = Path.Combine("generated_cpp_tests", "access")

/// Compile and run `src` under `gate`: the printed lines minus the timing
/// line. `Error "Skipped: ..."` when the toolchain is missing.
let private runUnder (name: string) (gate: string option) (src: string) : Result<string list, string> =
    match cppOf name gate src with
    | Error e -> Error e
    | Ok cpp ->
        Directory.CreateDirectory outputDir |> ignore
        // the runtime headers live beside every generated .cpp, as the CLI
        // deploys them before each compile
        CodeGen.deployRuntimeHeaders outputDir
        let cppFile = Path.Combine(outputDir, name + ".cpp")
        File.WriteAllText(cppFile, cpp)
        match Build.compileCpp cppFile outputDir with
        | Error e when e.StartsWith "Skipped:" -> Error e
        | Error e -> Error $"compile ({cppFile}): {e}"
        | Ok exe ->
            match Build.runExecutable exe with
            | Error e -> Error $"run: {e}"
            | Ok (code, out) when code <> 0 -> Error $"exit {code}: {out.Trim()}"
            | Ok (_, out) ->
                out.Split('\n')
                |> Array.map (fun l -> l.TrimEnd('\r'))
                |> Array.filter (fun l -> not (l.Contains " completed in ") && l <> "")
                |> List.ofArray
                |> Ok

/// The gather route's executable under AddressSanitizer (the memcheck
/// profile, clang64): a gather loop that reads one cell past a buffer can
/// still print the right numbers when the heap happens to be zero there --
/// the lag-set extent defect was exactly that, and only ASan saw it. Some
/// verdict, or None when the memcheck toolchain is not available.
let private memcheckGather (name: string) (gate: string option) (src: string) : bool option =
    match cppOf name gate src with
    | Error e -> resultLine Fail (name + "_asan") e; Some false
    | Ok cpp ->
        let cppFile = Path.Combine(outputDir, name + "_asan.cpp")
        File.WriteAllText(cppFile, cpp)
        match Build.compileCppMemcheck (Some src) [] cppFile outputDir with
        | Error e when e.StartsWith "Skipped:" -> resultLine Skip (name + "_asan") e; None
        | Error e ->
            // the same C++ just compiled under g++; an ASan-profile compile
            // failure is the sanitizer toolchain's, not a route disagreement
            let first = e.Split('\n').[0]
            resultLine Skip (name + "_asan") $"memcheck compile unavailable: {first}"
            None
        | Ok exe ->
            match Build.runExecutable exe with
            | Ok (0, _) -> resultLine Pass (name + "_asan") "gather route clean under AddressSanitizer"; Some true
            | Ok (code, out) ->
                let summary =
                    out.Split('\n') |> Array.tryFind (fun l -> l.Contains "SUMMARY:" || l.Contains "ERROR:")
                    |> Option.defaultValue $"exit {code}"
                resultLine Fail (name + "_asan") (summary.Trim()); Some false
            | Error e -> resultLine Fail (name + "_asan") e; Some false

/// Both routes on one program; Some pass/fail, None = skipped.
let private differential (name: string) (src: string) : bool option =
    match runUnder (name + "_gather") None src with
    | Error e when e.StartsWith "Skipped:" -> resultLine Skip name e; None
    | Error e -> resultLine Fail name e; Some false
    | Ok gather ->
        match runUnder (name + "_scatter") (Some "0") src with
        | Error e when e.StartsWith "Skipped:" -> resultLine Skip name e; None
        | Error e -> resultLine Fail name e; Some false
        | Ok scatter ->
            if gather = scatter && not gather.IsEmpty then
                resultLine Pass name $"{gather.Length} printed line(s) byte-identical under both routes"
                (match memcheckGather name None src with
                 | Some false -> Some false
                 | _ -> Some true)
            else
                let firstDiff =
                    Seq.zip (Seq.append gather (Seq.initInfinite (fun _ -> "<none>")))
                            (Seq.append scatter (Seq.initInfinite (fun _ -> "<none>")))
                    |> Seq.truncate (max gather.Length scatter.Length)
                    |> Seq.tryFind (fun (g, s) -> g <> s)
                let detail =
                    match firstDiff with
                    | Some (g, s) -> $"first difference: gather `{g}` vs scatter `{s}`"
                    | None -> $"line counts {gather.Length} vs {scatter.Length}"
                resultLine Fail name detail
                Some false

/// A five-point nonlinear kernel over a non-power-of-two extent whose
/// gradient is analytically ZERO on the interior for linear data -- what
/// prints there is pure roundoff, so agreement is agreement of the
/// accumulation ORDER, the property the descending-offset gather claims.
let private wideProgram =
    "import ad as ad\n"
    + "type H = Idx<127>\n"
    + "function f(a: Array<Float like H>) -> Float = {\n"
    + "    let d = method_for(halo<H, [-2, 0, 2]>) <@> lambda(w) -> a(w(2)) * a(w(-2)) - a(w(0)) * a(w(0)) |> compute\n"
    + "    reduce(d, (+))\n"
    + "}\n"
    + "let a0: Array<Float like H> = 1.0 + 0.013 * Float64(0..127)\n"
    + "let mut da: Array<Float like H> = 0.0 * Float64(0..127)\n"
    + "let fv = ad.grad(f)(a0, da)\n"
    + "let gsum = reduce(da, (+))\n"

/// A captured differentiable SCALAR alongside the windowed array: the
/// gather's scalar accumulator loop against the scatter's inline
/// accumulation, plus two windowed targets.
let private scalarCaptureProgram =
    "import ad as ad\n"
    + "type H = Idx<9>\n"
    + "function f(a: Array<Float like H>, s: Float) -> Float = {\n"
    + "    let d = method_for(halo<H, [-1, 0, 1]>) <@> lambda(w) -> s * a(w(1)) * a(w(-1)) - a(w(0)) |> compute\n"
    + "    reduce(d, (+))\n"
    + "}\n"
    + "let a0: Array<Float like H> = 1.0 + 0.5 * Float64(0..9)\n"
    + "let mut da: Array<Float like H> = 0.0 * Float64(0..9)\n"
    + "let (fv, ds) = ad.grad(f)(a0, 0.7, da)\n"
    + "let gsum = reduce(da, (+))\n"

/// Two DIFFERENT windowed arrays, both differentiable: two cotangent
/// buffers, one gather loop each.
let private twoArrayProgram =
    "import ad as ad\n"
    + "type H = Idx<11>\n"
    + "function f(a: Array<Float like H>, b: Array<Float like H>) -> Float = {\n"
    + "    let d = method_for(halo<H, [-1, 0, 1]>) <@> lambda(w) -> a(w(1)) * b(w(-1)) + exp(b(w(0))) |> compute\n"
    + "    reduce(d, (+))\n"
    + "}\n"
    + "let a0: Array<Float like H> = 1.0 + 0.1 * Float64(0..11)\n"
    + "let b0: Array<Float like H> = 0.5 - 0.07 * Float64(0..11)\n"
    + "let mut da: Array<Float like H> = 0.0 * Float64(0..11)\n"
    + "let mut db: Array<Float like H> = 0.0 * Float64(0..11)\n"
    + "let fv = ad.grad(f)(a0, b0, da, db)\n"

let runAccessTests () =
    printHeader "Blade-DSL: Halo Access Tests"
    let pins =
        [ recordPins ()
          routeEmission ()
          computedOffsetDeclines () ]
    let diffs =
        if not Build.capabilities.Value.HasGpp then
            resultLine Skip "halo_route_differential" "g++ not available"
            [ None ]
        else
            let fromCorpus (name: string) (file: string) =
                match corpusSource file with
                | Error e -> resultLine Fail name e; Some false
                | Ok src -> differential name src
            [ fromCorpus "halo_route_differential_linear" "030_halo_stencil_grad_linear.blade"
              fromCorpus "halo_route_differential_nonlinear" "031_halo_stencil_grad_nonlinear.blade"
              fromCorpus "halo_route_differential_lag_set" "032_halo_stencil_grad_lag_set.blade"
              differential "halo_route_differential_wide_127" wideProgram
              differential "halo_route_differential_scalar_capture" scalarCaptureProgram
              differential "halo_route_differential_two_arrays" twoArrayProgram ]
    let passed = (pins |> List.filter id |> List.length) + (diffs |> List.filter (fun d -> d = Some true) |> List.length)
    let failed = (pins |> List.filter not |> List.length) + (diffs |> List.filter (fun d -> d = Some false) |> List.length)
    let skipped = diffs |> List.filter Option.isNone |> List.length
    printFooter "Halo Access" [$"{passed} passed"; $"{failed} failed"; $"{skipped} skipped"]
    { Block = "Halo Access"; Passed = passed; Failed = failed; Skipped = skipped
      FailedNames = if failed = 0 then [] else ["see above"] }
