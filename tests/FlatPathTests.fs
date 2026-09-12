// Flat-elementwise REACH tests -- which index tags reach the flat path
// (docs/plan-cpp-perf-exploitation.md phase 3) and which still decline.
//
// The corpus proves the VALUES, and it cannot see this decision at all: there
// is no corpus pin for emitted C++ text, and the flat rewrite is
// value-preserving by construction (same cells, same DFS order, same kernel).
// A regression that silently stopped flattening therefore leaves every corpus
// test green while the loop loses its BLADE_RESTRICT pool pointers and its
// BLADE_IVDEP / `omp simd` pragma -- which for a `sqrt` map is the difference
// between vectorized and not.
//
// That is not hypothetical: it is what the ANONYMOUS RANGE lived with. Its tag
// is `__anon`, and both halves of the gate (`flatShapeSignature` and the
// element-level `SlotTag` twin beside `elementsOk`) refused any `__`-prefixed
// tag. Every other reserved tag -- __orbidx, __compoundidx, __seq, __group_*,
// __sparseidx, __raggedidx* -- also carries a non-plain IxKind and was already
// refused on that ground, so the blanket prefix test excluded exactly one
// thing: the one reserved tag that IS a plain contiguous skeleton. The idiom
// CLAUDE.md calls the top rung, `x0 + dx * Float64(0..n)`, quietly emitted a
// worse loop than the identical `Float64(range<I>)`.
//
// So the pins here are PARITY pins: the anonymous and named spellings of one
// program must reach the same decision, and `__halowin` -- the tag the gate
// still names, whose reads are OFFSET from the loop index -- must still not.
//
// Pure lowering + codegen: no g++, no toolchain. Always runs.
module Blade.Tests.FlatPathTests

open Blade
open Blade.Lowering
open Blade.Tests.TestHarness

let private cppOfSource (testName: string) (src: string) : Result<string, string> =
    try
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir testName))
    with ex -> Error ($"codegen raised: {ex.Message}")

/// The emitter's own census line, one per flattened nest.
let private flatLoopCount (cpp: string) =
    cpp.Split('\n')
    |> Array.filter (fun l -> l.Contains "// flat elementwise:")
    |> Array.length

// ---------------------------------------------------------------------------
// Fixtures
//
// Extent 7 throughout, per the repo's non-power-of-two stride discipline. Each
// pair is ONE program written two ways: the only difference is which spelling
// of the index range it uses, so any difference in the decision is the tag's.
// ---------------------------------------------------------------------------

/// Affine chain over a BOUND (pool-backed) cast of an index range. The chain
/// `x0 + dx * t` is one fused nest (fuseElementwiseChainsModule collapses the
/// two binops), and it must take the flat path -- this is the parity claim's
/// home now that fusion exists: same program, two range spellings, one flat
/// decision.
let private affineAnon =
    "let t = Float64(0..7)\n"
    + "let axis = 1.5 + 0.25 * t\n"

let private affineNamed =
    "type I = Idx<7>\n"
    + "let t = Float64(range<I>)\n"
    + "let axis = 1.5 + 0.25 * t\n"

/// The bare-range spelling of the same axis. Fusion folds the cast map INTO
/// the chain, so the whole idiom emits ONE index-driven loop
/// (`axis[i] = 1.5 + 0.25 * (double)i`) with no array operand left to
/// flatten -- the flat census line is correctly ABSENT, and the win shows as
/// the allocation count instead: exactly one pool (the output). If this pin
/// starts seeing flat lines or extra pools, fusion stopped firing here.
let private affineBareRange = "let axis = 1.5 + 0.25 * Float64(0..7)\n"

/// The same chain, plus a `sqrt` map -- the shape `-fno-math-errno` exists to
/// let g++ vectorize, and the one that most wants the flat form.
let private sqrtAnon =
    "let t = Float64(0..7)\n"
    + "let squares = t * t\n"
    + "let roots = sqrt(squares)\n"

let private sqrtNamed =
    "type I = Idx<7>\n"
    + "let t = Float64(range<I>)\n"
    + "let squares = t * t\n"
    + "let roots = sqrt(squares)\n"

/// A halo stencil (corpus loops/072). `w(1)` and `w(-1)` read NEIGHBORS, so the
/// flat index is not the operand's pool offset and the rewrite would be wrong,
/// not merely unprofitable. This is the case the surviving `__halowin` test is
/// for, and the reason the fix narrowed the predicate instead of deleting it.
let private haloStencil =
    "let A: Array<Float64 like Idx<7>> = [1.0, 2.0, 4.0, 7.0, 11.0, 16.0, 22.0]\n"
    + "let d = method_for(halo<Idx<7>, [-1, 0, 1]>) <@> lambda(w) -> A(w(1)) - A(w(-1)) |> compute\n"

// ---------------------------------------------------------------------------
// Cases
// ---------------------------------------------------------------------------

/// Assert an exact flat-loop count. Exact rather than `> 0`: the anon/named
/// parity claim is that the SAME nests flatten, and a count that drifted up
/// would be as much a change in the decision as one that drifted down.
let private runCountCase (name: string) (src: string) (expected: int) =
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let got = flatLoopCount cpp
        if got = expected then
            resultLine Pass name ($"{got} flat loop(s)")
            true
        else
            resultLine Fail name ($"expected {expected} flat loop(s), got {got}")
            false

/// The parity form: two spellings of one program must agree, whatever the
/// count. Stated separately from the counts above so that a future emitter
/// change which legitimately moves BOTH still leaves this pin meaningful.
let private runParityCase (name: string) (anonSrc: string) (namedSrc: string) =
    match cppOfSource (name + "_anon") anonSrc, cppOfSource (name + "_named") namedSrc with
    | Error e, _ | _, Error e -> resultLine Fail name e; false
    | Ok anonCpp, Ok namedCpp ->
        let a, n = flatLoopCount anonCpp, flatLoopCount namedCpp
        if a = n && a > 0 then
            resultLine Pass name ($"both spellings flatten {a} loop(s)")
            true
        else
            resultLine Fail name ($"anon flattened {a}, named flattened {n}")
            false

/// The bare-range fusion pin: no flat census line (nothing pool-backed left
/// to flatten) AND exactly one `allocate<` (the output). Together these say
/// "the chain became one index-driven loop with zero temporaries".
let private runBareRangeFusionCase (name: string) (src: string) =
    match cppOfSource name src with
    | Error e -> resultLine Fail name e; false
    | Ok cpp ->
        let flats = flatLoopCount cpp
        let allocs =
            cpp.Split('\n') |> Array.filter (fun l -> l.Contains "allocate<") |> Array.length
        if flats = 0 && allocs = 1 then
            resultLine Pass name "fused to one index-driven loop, 1 pool"
            true
        else
            resultLine Fail name ($"expected 0 flat lines / 1 pool, got {flats} / {allocs}")
            false

let runFlatPathTests () =
    printHeader "Blade-DSL: Flat Elementwise Path Tests"
    let results =
        [ // The fused chain over the bound pool is ONE nest.
          runCountCase "affine_anon_range_flattens" affineAnon 1
          runCountCase "affine_named_range_flattens" affineNamed 1
          runParityCase "affine_anon_matches_named" affineAnon affineNamed
          // Bare range: fusion folds the cast in; the win is the pool count.
          runBareRangeFusionCase "affine_bare_range_fuses" affineBareRange
          // `t * t`, then `sqrt` -- plus the cast map, which has a real array
          // operand here because `t` is bound.
          runParityCase "sqrt_chain_anon_matches_named" sqrtAnon sqrtNamed
          // The tag the gate still names, and must keep naming.
          runCountCase "halo_stencil_declines" haloStencil 0 ]
    let passed = results |> List.filter id |> List.length
    let failed = results.Length - passed
    printFooter "Flat Elementwise Path" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Flat Elementwise Path"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = if failed = 0 then [] else ["see above"] }
