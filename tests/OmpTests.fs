// OpenMP thread-coverage tests: verify emitted pragmas form genuine parallel
// regions, and that values stay correct under forced multi-threading.
// Extracted verbatim from Main.fs (audit §2.3). Requires g++; skips otherwise.
module Blade.Tests.OmpTests

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
open Blade.Tests.Expect

// ============================================================================
// Pragma-emission tests (pure codegen; no g++, no threads, always run)
// ============================================================================
//
// The thread-coverage block below answers "does an emitted pragma form a real
// parallel team". These answer the question BEFORE it: "is a pragma emitted at
// all for the clause the user wrote". That gap is not academic — `where omp`
// on a NAMED FUNCTION used as a kernel was silently dropped for a long time,
// producing serial code with no diagnostic, because every existing omp test
// wrote the clause on an inline or let-bound LAMBDA. A lambda kernel keeps its
// where-clause on the TypedLambdaInfo; a named-function kernel is eta-expanded
// into a wrapper lambda built with no where-clause, so the clause has to be
// surfaced explicitly onto the wrapper (TypeCheck.etaExpandFunctionKernel).
// Both spellings are pinned here so the two paths cannot drift again.
//
// These assert on the generated C++ STRING rather than on runtime behaviour, so
// they need no toolchain and run in the default suite.

/// Pin one environment variable for the duration of a scope, restoring the
/// prior value on exit. Same use-guard idiom as `DiffOracle.pinFpContractOff`
/// and `LinAlgTests.pinEnv`, and it works for the same reason: every gate it
/// targets is read PER CALL, so a mid-process set takes effect immediately. A
/// `null` value UNSETS the variable.
let private pinEnv (name: string) (value: string) =
    let prior = System.Environment.GetEnvironmentVariable(name)
    System.Environment.SetEnvironmentVariable(name, value)
    { new System.IDisposable with
        member _.Dispose() = System.Environment.SetEnvironmentVariable(name, prior) }

/// Pin `BLADE_OMP_THREADS` UNSET for a whole block.
///
/// WHY EVERY OMP BLOCK NEEDS THIS. `BLADE_OMP_THREADS=1|0|off` is a BUILD knob
/// (CodeGen.ompThreadEmissionEnabled) that suppresses every thread-level OpenMP
/// construct, and it is meant to be set GLOBALLY on a deployment box. A user
/// with it set in their environment would otherwise turn these suites VACUOUS
/// rather than red: `blade test omp-pragma` would find no pragmas to assert on,
/// `omp-coverage` would find no teams to observe, and both would report their
/// own emptiness as success. Pinning it unset makes every block below measure
/// the DEFAULT emission whatever the ambient environment says; the arms that
/// deliberately exercise the knob pin it back on for their own scope.
let private pinOmpThreadsUnset () = pinEnv "BLADE_OMP_THREADS" null

/// Lower + generate, returning the C++ source. No compiler involved.
///
/// `lowerCaptured`, not `lower`: these cases assert EMISSION SHAPE, and several
/// of their kernels (a commutative body applied to one array twice) legitimately
/// earn a BL4010 storage suggestion. `lower` would print it straight to stderr
/// from inside the pipeline, un-attributed, in the middle of a run whose tests
/// all pass — the same leak the corpus lanes now capture. There is no pin
/// mechanism for these hand-written sources, so the warnings are simply dropped.
let private cppOf (testName: string) (src: string) : Result<string, string> =
    try
        match fst (lowerCaptured src) with
        | Error e -> Error ($"lower: {e}")
        | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir testName))
    with ex -> Error ($"codegen raised: {ex.Message}")

/// A kernel body and the two arrays it folds over, spelled once. Each case
/// below varies ONLY how the kernel is written and where the clause sits.
let private ompPragmaCases : (string * string * bool) list =
    // (name, source, expectPragma)
    let arrays = "let A = [1.0, 2.0, 3.0]\nlet B = [4.0, 5.0, 6.0]\n"
    [ // The regression case: clause on a named function, function used as the
      // object_for kernel. Eta-expanded — the clause must survive the wrapper.
      ("named_function_object_for",
       "function cov(a: Float64, b: Float64) where omp(a: 1) = a * b\n" + arrays +
       "let m = object_for(cov) <@> (A, B) |> compute\n", true)
      // Same drop, reached through the OTHER eta site: a bare named function as
      // the RIGHT operand of <@>.
      ("named_function_method_for_apply",
       "function cov(a: Float64, b: Float64) where omp(a: 1) = a * b\n" + arrays +
       "let m = method_for(A, B) <@> cov |> compute\n", true)
      // Control: the spelling every pre-existing omp test uses. Passed before
      // the fix and must keep passing.
      ("let_bound_lambda",
       "let k = lambda(x, y) where omp(x: 1) -> x * y\n" + arrays +
       "let m = object_for(k) <@> (A, B) |> compute\n", true)
      // Control: inline lambda, the third spelling.
      ("inline_lambda",
       arrays +
       "let m = method_for(A, B) <@> lambda(x, y) where omp(x: 1) -> x * y |> compute\n", true)
      // Negatives — parallelism is OPT-IN. Identical programs minus the clause
      // must stay serial; without these the fix could degenerate into
      // "parallelize every named-function kernel".
      ("named_function_no_clause",
       "function cov(a: Float64, b: Float64) = a * b\n" + arrays +
       "let m = object_for(cov) <@> (A, B) |> compute\n", false)
      ("let_bound_lambda_no_clause",
       "let k = lambda(x, y) -> x * y\n" + arrays +
       "let m = object_for(k) <@> (A, B) |> compute\n", false) ]

/// The `omp(a: n)` DEPTH, checked as an exact pragma string rather than mere
/// presence. `n` is a LICENCE — "up to n dimensions of this argument may carry
/// threads" — counted per-argument, outermost first. It caps the structural
/// collapse/dynamic strategy instead of replacing it, so these cases pin the
/// interaction of the two rather than the licence alone.
///
/// Before this was implemented the depth was inert (written by
/// extractParallelism, read by nothing), so every case below emitted whatever
/// the bound structure alone dictated — `omp(a: 1)` on a 2-level nest produced
/// `collapse(2)`, threading a dimension of `b`, which granted nothing.
let private ompDepthCases : (string * string * string) list =
    // (name, source, exact expected pragma line)
    let arrays = "let A = [1.0, 2.0, 3.0]\nlet B = [4.0, 5.0, 6.0]\n"
    let apply = "let m = object_for(k) <@> (A, B) |> compute\n"
    let kern clause = $"function k(a: Float64, b: Float64) where {clause} = a * b\n"
    [ // One dimension licensed of a 2-level collapsible nest: collapse would
      // thread b's level too, so it must NOT be used.
      ("depth_1_of_2_no_collapse", kern "omp(a: 1)" + arrays + apply,
       "#pragma omp parallel for")
      // Both arguments licensed: collapse(2) is now permitted.
      ("depth_1_1_collapses_2", kern "omp(a: 1, b: 1)" + arrays + apply,
       "#pragma omp parallel for collapse(2)")
      // Per-argument counting: `a` is rank-1 and owns exactly one level, so a
      // depth of 2 cannot reach into b's level. (Under a whole-nest "budget"
      // reading this would collapse(2) — that reading is NOT what is
      // implemented; the depth counts levels OF THE NAMED ARGUMENT.)
      ("depth_2_on_rank1_arg_caps_at_1", kern "omp(a: 2)" + arrays + apply,
       "#pragma omp parallel for")
      // A licence on an argument owning an INNER level parallelizes that level
      // rather than silently doing nothing or threading the unlicensed outer.
      ("inner_arg_licence_moves_pragma", kern "omp(b: 1)" + arrays + apply,
       "#pragma omp parallel for")
      // The canonical documented case (quickstart-2 "Parallelism"): ONE argument
      // owning BOTH levels, so the depth alone scales how many are threaded.
      // This is the pair the old inert-depth behaviour could not distinguish —
      // it emitted collapse(2) for both.
      ("one_arg_two_levels_depth_1",
       "function k(a: Float64) where omp(a: 1) = a * 2.0\n" +
       "let M = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n" +
       "let m = object_for(k) <@> (M) |> compute\n",
       "#pragma omp parallel for")
      // Depth 2 on the same nest threads BOTH levels. Since Phase 3
      // (docs/plan-cpp-perf-exploitation.md) this nest is index-free
      // elementwise over one contiguous pool, so it is emitted as a single
      // flat loop over all cells and the pragma is `parallel for simd` rather
      // than `collapse(2)` over the two headers. Same licence, same threaded
      // dimensions — collapse(2) fuses exactly the iteration space the flat
      // loop already IS, so the fused form subsumes it (and adds SIMD, which
      // the flat write pattern licenses). The pair this case exists to
      // separate is unaffected: depth 1 above still threads one level only.
      ("one_arg_two_levels_depth_2",
       "function k(a: Float64) where omp(a: 2) = a * 2.0\n" +
       "let M = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n" +
       "let m = object_for(k) <@> (M) |> compute\n",
       "#pragma omp parallel for simd")
      // TUPLE PARAMETER (docs/plan-tuples-vs-arg-packs.md 6c). A `Tuple<k>`
      // parameter is ONE schema node whose levels are its k rows in order, and
      // the apply seam realizes it by replacing it with k row params -- which
      // deletes the name the licence resolves by. These two cases are the pin
      // that the licence survives that expansion AND keeps its depth meaning:
      // one level licensed of a two-row node is a plain `parallel for`, both
      // levels licensed is `collapse(2)`. Before the split, BOTH of these were
      // a hard BL3999 refusal (any where-clause + any tuple param).
      ("tuple_param_depth_1_no_collapse",
       "let A: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n" +
       "let B: Array<Float64 like Idx<3>> = [10.0, 20.0, 30.0]\n" +
       "let f = lambda(p: Tuple<2>) where omp(p: 1) -> p[0] * p[1]\n" +
       "let m = object_for(f) <@> (A, B) |> compute\n",
       "#pragma omp parallel for")
      ("tuple_param_depth_2_collapses",
       "let A: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n" +
       "let B: Array<Float64 like Idx<3>> = [10.0, 20.0, 30.0]\n" +
       "let f = lambda(p: Tuple<2>) where omp(p: 2) -> p[0] * p[1]\n" +
       "let m = object_for(f) <@> (A, B) |> compute\n",
       "#pragma omp parallel for collapse(2)")
      // The same licence reached through the NAMED-function eta wrapper, which
      // renames the params ONCE (to `__k<uid>_<i>`) before the tuple expansion
      // renames them again. The two remappings have to compose.
      ("tuple_param_named_function_depth_1",
       "function f(p: Tuple<2>) where omp(p: 1) = p[0] * p[1]\n" +
       "let A: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n" +
       "let B: Array<Float64 like Idx<3>> = [10.0, 20.0, 30.0]\n" +
       "let m = object_for(f) <@> (A, B) |> compute\n",
       "#pragma omp parallel for") ]

/// Comm-licensed parallel REDUCTIONS (Phase 2 of
/// docs/plan-cpp-perf-exploitation.md). `where ... omp` on a FOLD kernel opts
/// the reduction into a parallel fold; which SHAPE it gets is decided by the
/// licence, and the two shapes are textually unmistakable:
///
///   Path A — builtin `+`/`*` body: `#pragma omp parallel for simd
///     reduction(<op>:acc)` over the flat sweep, no runtime API at all.
///   Path B — any other licensed kernel (and every reduce over a deferred
///     computation): an explicit team with per-thread partials, which is the
///     only shape that calls `omp_get_max_threads` / `omp_get_thread_num`.
///
/// Asserting `mustNotContain` matters as much as `mustContain` here: the two
/// paths differ in reassociation guarantees (Path A's combine order is
/// unspecified; Path B's is fixed by chunk index), so a kernel silently
/// switching paths changes a property tests downstream rely on.
let private ompReduceCases : (string * string * string list * string list) list =
    // (name, source, mustContain, mustNotContain)
    let arr = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]\n"
    [ // Path A: builtin body, no comm needed — `+` carries commutativity and
      // associativity outright.
      ("reduce_builtin_lambda_omp",
       arr + "let s = reduce(A, lambda(a, b) where omp -> a + b)\n",
       ["#pragma omp parallel for simd reduction(+:s)"],
       ["omp_get_max_threads"])
      ("reduce_builtin_product_omp",
       arr + "let s = reduce(A, lambda(a, b) where omp -> a * b)\n",
       ["#pragma omp parallel for simd reduction(*:s)"], [])
      // Negative control: parallelism is OPT-IN. The identical program minus
      // the clause must emit the old serial accumulator loop.
      ("reduce_builtin_lambda_no_clause",
       arr + "let s = reduce(A, lambda(a, b) -> a + b)\n",
       ["// reduce: accumulator loop, eager"], ["#pragma omp"])
      // Negative control: an operator section cannot carry a where-clause, so
      // the most common fold spelling of all stays serial.
      ("reduce_section_no_clause",
       arr + "let s = reduce(A, (+))\n",
       ["// reduce: accumulator loop, eager"], ["#pragma omp"])
      // Path B via a NAMED function — the eta-prone spelling, and the one whose
      // body is invisible at the reduce seam (TypeEnv.FuncFoldBuiltin exists for
      // exactly this). Body is deliberately not a bare builtin op.
      ("reduce_named_comm_function",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" + arr +
       "let s = reduce(A, myAdd)\n",
       ["#pragma omp parallel num_threads("; "omp_get_thread_num()"],
       ["#pragma omp parallel for"])
      // Path B via an inline comm lambda.
      ("reduce_inline_comm_lambda",
       arr + "let s = reduce(A, lambda(a, b) where comm(a, b), omp -> (a + b) * 1.0)\n",
       ["#pragma omp parallel num_threads("], ["#pragma omp parallel for"])
      // Path B with an explicit init: the seed stays in the shared accumulator
      // and enters the fixed-order combine first, so nothing about the chunk
      // shape changes.
      ("reduce_named_comm_with_init",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" + arr +
       "let s = reduce(A, myAdd, 100.0)\n",
       ["#pragma omp parallel num_threads("], [])
      // Path B over a DEFERRED computation: no intermediate array is
      // materialized, and the OUTERMOST level is the one chunked.
      //
      // `axes = 2` IS THE FEATURE'S SPELLING, not a decoration. The chunked
      // fold collapses the whole cell grid into one scalar accumulator, and
      // since the 2026-08-09 ruling on `reduce` (docs/features/sql.md 10) the
      // full fold is `axes = rank` -- the default is the innermost axis only.
      // A rank-2 deferred computation reduced WITHOUT the count is a partial
      // fold to a rank-1 array, which is a different program: it takes the
      // row-wise `<@>` route, materializes the grid, and never reaches the
      // fused reduction terminal that owns this emission. Dropping the count
      // here is what made this case (and the two knob arms below) vacuous.
      ("reduce_over_computation_chunked",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" + arr +
       "let B = [2.0, 3.0, 4.0, 5.0, 6.0, 7.0]\n" +
       "let s = reduce(method_for(A, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n",
       ["comm-licensed parallel fold, outer level chunked"
        "#pragma omp parallel num_threads("
        "for (size_t __i0 = __rlo; __i0 < __rhi; __i0++)"], [])
      // Same computation, clause dropped: back to the materialize-then-fold
      // desugar, and no parallel region anywhere.
      ("reduce_over_computation_no_clause",
       "function myAdd(a: Float64, b: Float64) where comm(a, b) = (a + b) * 1.0\n" + arr +
       "let B = [2.0, 3.0, 4.0, 5.0, 6.0, 7.0]\n" +
       "let s = reduce(method_for(A, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n",
       [], ["#pragma omp"])
      // ---- the EXPRESSION-form reduce: declined, but no longer silent -------
      // A reduce in expression position (kernel body, inline arithmetic) opens
      // no team ON PURPOSE -- its context is routinely already inside a parallel
      // region, so a nested team would be a pessimisation or the wrong size (see
      // the note atop `renderReduceExpr`). The decline is legitimate; being
      // SILENT about it was the bug, and these cases are what keep it audible.
      //
      // A BLOCK comment is asserted, not just the phrase: every arm of that
      // emitter space-joins its statements into ONE line, so a `//` marker would
      // swallow the fold. That is a compile error, which makes the comment STYLE
      // load-bearing rather than cosmetic.
      ("reduce_in_kernel_body_marks_dropped_omp",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" +
       "let M = [[1.0, 2.0, 3.0, 4.0, 5.0], [4.0, 5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0, 13.0]]\n" +
       "let r = method_for(M) <@> lambda(row) -> reduce(row, myAdd, 0.0) |> compute\n",
       ["/* [omp] requested but emitted serial: reduce in expression position"],
       ["#pragma omp"; "omp_get_"])
      // The same drop reached through the PARTIAL FOLD, which is what makes this
      // marker matter: since the 2026-08-09 `reduce` ruling the default folds the
      // innermost axis only, and `partialFold` desugars that into the row-wise
      // form above -- so this perfectly ordinary rank-2 `reduce` lands in
      // expression position and its `omp` clause buys nothing. `axes = 2` is the
      // spelling that gets the parallel fold (reduce_over_computation_chunked).
      ("reduce_partial_default_marks_dropped_omp",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" +
       "let M = [[1.0, 2.0, 3.0, 4.0, 5.0], [4.0, 5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0, 13.0]]\n" +
       "let r = reduce(M, myAdd, 0.0)\n",
       ["/* [omp] requested but emitted serial: reduce in expression position"],
       ["#pragma omp"; "omp_get_"])
      // Negative control, and the one that keeps the two above honest: the marker
      // reports a DROPPED CLAUSE, so a kernel that never asked must not carry it.
      // Without this the emitter could pass both cases by marking every reduce.
      ("reduce_in_kernel_body_no_clause_no_marker",
       "function myAdd(a: Float64, b: Float64) where comm(a, b) = (a + b) * 1.0\n" +
       "let M = [[1.0, 2.0, 3.0, 4.0, 5.0], [4.0, 5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0, 13.0]]\n" +
       "let r = reduce(M, myAdd, 0.0)\n",
       [], ["[omp]"; "#pragma omp"]) ]

/// The BL-coded refusal for an UNLICENSED `omp` on a fold kernel. Pinned in the
/// diagnostics corpus too (049_fold_omp_needs_license.blade); repeated here so
/// the codegen-side block that owns the two emission paths also owns the
/// statement that there is no third, silent one.
let private ompReduceDiagCases : (string * string) list =
    // (name, source) — each must fail to lower, with BL4016 in the message.
    let arr = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]\n"
    [ ("omp_without_comm_antisym_body",
       arr + "let s = reduce(A, lambda(a, b) where omp -> a - b)\n")
      ("omp_without_comm_nonbuiltin_body",
       arr + "let s = reduce(A, lambda(a, b) where omp -> (a + b) * 2.0)\n")
      ("omp_named_function_without_comm",
       "function halfSum(a: Float64, b: Float64) where omp = (a + b) * 0.5\n" + arr +
       "let s = reduce(A, halfSum)\n") ]

/// Emission SHAPE pins for the three decisions that are invisible in values and
/// unreachable from the pragma-text cases above: how the native (BLAS-gate-off)
/// gram/matmul arms are threaded and ordered, whether the loop nest annotates a
/// header it cannot vectorize, and which DIRECTION a threaded triangular outer
/// level runs in. All three are pure codegen assertions -- no toolchain, no
/// threads -- for the same reason the cases above are: the emitted text is the
/// artifact, and a value test cannot see any of it.
///
/// `mustNotContain` carries as much weight as `mustContain` in every case here:
/// each is an OLD emission whose disappearance is the actual claim (the strided
/// `B[__mt][__mj]` read, the register accumulator, the per-iteration operand
/// subscripts, an `ivdep` on a loop containing a loop).
let private emissionShapeCases : (string * string * string list * string list) list =
    // (name, source, mustContain, mustNotContain)
    //
    // Non-power-of-two extents throughout (3x5, 5x7, 4x5): a bound rendered
    // from the wrong axis is invisible at square or equal extents.
    let matmulSrc =
        "import math as m\n" +
        "type M35 = Array<Float64 like Idx<3>, Idx<5>>\n" +
        "type M57 = Array<Float64 like Idx<5>, Idx<7>>\n" +
        "let A: M35 = [[1.0, 2.0, 3.0, 4.0, 5.0], [6.0, 7.0, 8.0, 9.0, 10.0], [11.0, 12.0, 13.0, 14.0, 15.0]]\n" +
        "let B: M57 = [[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0], [8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0], [15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0], [22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0], [29.0, 30.0, 31.0, 32.0, 33.0, 34.0, 35.0]]\n" +
        "let P = m.matmul(A, B)\n"
    let gramSrc =
        "type M35 = Array<Float64 like Idx<3>, Idx<5>>\n" +
        "type M45 = Array<Float64 like Idx<4>, Idx<5>>\n" +
        "let A: M35 = [[1.0, 2.0, 3.0, 4.0, 5.0], [6.0, 7.0, 8.0, 9.0, 10.0], [11.0, 12.0, 13.0, 14.0, 15.0]]\n" +
        "let C: M45 = [[2.0, 3.0, 5.0, 7.0, 11.0], [13.0, 17.0, 19.0, 23.0, 29.0], [31.0, 37.0, 41.0, 43.0, 47.0], [53.0, 59.0, 61.0, 67.0, 71.0]]\n" +
        "let G = gram(A, A)\n" +
        "let H = gram(A, C)\n"
    // Rank-2 nest whose kernel body is a `prodsum` over the peeled row fibers.
    // The nest sees TWO levels; the `__pt` loop lives inside the body's IIFE and
    // is invisible to it -- which is exactly how `ivdep` came to sit on a header
    // whose body is a loop.
    let fiber = "Array<Float64 like Idx<5>>"
    let rowsDecl = "let R = [[1.0, 2.0, 3.0, 4.0, 5.0], [6.0, 7.0, 8.0, 9.0, 10.0], [11.0, 12.0, 13.0, 14.0, 15.0]]\n"
    let prodsumSrc =
        rowsDecl +
        $"let Cv = method_for(R, R) <@> lambda(a: {fiber}, b: {fiber}) -> prodsum(a, b) |> compute\n"
    let loopFreeSrc =
        rowsDecl +
        $"let D = method_for(R, R) <@> lambda(a: {fiber}, b: {fiber}) -> a(0) * b(0) |> compute\n"
    // Rank-3 triangular comm nest with an omp licence: the `schedule(dynamic)`
    // outer level. Extent 13 -- prime, and not 3, so a bound that lost a
    // dependency subtraction is visible.
    let triSrc =
        "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0]\n" +
        "let L = method_for(A, A, A)\n" +
        "let k = lambda(x, y, z) where comm(x, y, z), omp(x: 1) -> x * y + z\n" +
        "let R = L <@> k |> compute\n"
    [ // ---- matmul: i-t-j, in-place row accumulation, threaded outer ----------
      ("matmul_native_arm_reordered_i_t_j", matmulSrc,
       [ // outer level threaded (via the portable macro -- see
         // cpp/blade_portability.hpp for why this is not a `#pragma` line:
         // these emitters' output can be space-joined into a one-line IIFE).
         // Spelled with the header it governs so the _DYNAMIC variant cannot
         // satisfy it by prefix.
         "BLADE_OMP_PARALLEL_FOR\nfor (size_t __mi = 0; __mi < 3; __mi++) {"
         // the accumulator is the output row itself, hoisted and restrict-qualified
         "double* BLADE_RESTRICT __mcrow = &P[__mi][0];"
         // ... which therefore has to be zeroed first, at the literal n
         "for (size_t __mj = 0; __mj < 7; __mj++) { __mcrow[__mj] = double(); }"
         // t OUTSIDE j: this ordering IS the change. Literal `5` = A's trailing
         // extent, baked from A's own index record rather than read at runtime.
         "for (size_t __mt = 0; __mt < 5; __mt++) {"
         "const double __ma = A[__mi][__mt];"
         "const double* BLADE_RESTRICT __mbrow = &B[__mt][0];"
         // unit-stride inner loop, and ivdep is TRUE here (fresh output pool)
         "BLADE_IVDEP"
         "__mcrow[__mj] += __ma * __mbrow[__mj];" ],
       [ // the strided column walk that made the old order slow
         "B[__mt][__mj]"
         // the register accumulator the old order needed
         "__macc"
         // runtime extent reads for bounds the operand types already pin
         "__mt < A.extents[1]"
         "__mj < B.extents[1]" ])
      // ---- gram: both arms threaded, rows hoisted, order UNCHANGED -----------
      ("gram_native_arms_threaded_and_hoisted", gramSrc,
       [ // same-array arm is triangular (inner span `3 - __gi`), so dynamic
         "BLADE_OMP_PARALLEL_FOR_DYNAMIC"
         "for (size_t __gjr = 0; __gjr < 3 - __gi; __gjr++) {"
         // distinct arm is rectangular, so the static schedule
         "BLADE_OMP_PARALLEL_FOR\nfor (size_t __gi = 0; __gi < 3; __gi++) {"
         // both operand rows hoisted out of the contraction loop
         "const double* BLADE_RESTRICT __growi = &A[__gi][0];"
         "const double* BLADE_RESTRICT __growj = &A[__gj][0];"
         "const double* BLADE_RESTRICT __growj = &C[__gj][0];"
         // contraction reads go through the hoists; `k` is still innermost and
         // still bounded by the LITERAL trailing extent
         "for (size_t __gk = 0; __gk < 5; __gk++) {"
         "__gacc += __growi[__gk] * nested_array_utilities::conj_scalar(__growj[__gk]);" ],
       [ // the per-iteration double subscript the hoists replaced
         "A[__gi][__gk]"
         "C[__gj][__gk]"
         // gram's inner loop is a REDUCTION into `__gacc`: a loop-carried
         // dependence, so `ivdep` there would be a false claim AND inert.
         // (matmul's inner loop is not a reduction, which is why it gets one.)
         "BLADE_IVDEP\nfor (size_t __gk" ])
      // ---- ivdep is declined, loudly, when the body hides a loop -------------
      ("ivdep_declined_on_prodsum_body", prodsumSrc,
       [ "// [ivdep] declined: kernel body contains an inner loop" ],
       [ "BLADE_IVDEP" ])
      // Control: identical nest shape, loop-free body. Without this the fix
      // could degenerate into "never emit ivdep", which is not the claim.
      ("ivdep_kept_on_loop_free_body", loopFreeSrc,
       [ "BLADE_IVDEP" ],
       [ "// [ivdep] declined" ])
      // ---- the threaded triangular outer level runs ASCENDING ----------------
      // Pinned as a DECISION, not as an accident of never having considered it.
      // A performance audit proposed reversing this level to descending, on the
      // theory that `schedule(dynamic)` hands out chunks in iteration order so
      // descending would be largest-chunk-first (LPT). It is backwards for this
      // shape: `genForLoopHeader` SUBTRACTS the outer index from a triangular
      // level's bound (below: `13 - __i0`, `13 - __i1 - __i0`), so work per
      // outer iteration DECREASES in `__i0` and ascending order is already
      // largest-first. Reversing would end the schedule on the ~C(13,2)-cell
      // row instead of the 1-cell row -- turning the good makespan bound into
      // the bad one. See the derivation at CodeGen.genNestPragma.
      ("triangular_outer_level_stays_ascending", triSrc,
       [ "#pragma omp parallel for schedule(dynamic)"
         "for (size_t __i0 = 0; __i0 < 13; __i0++) {"
         // the bounds that make ascending the largest-first order
         "for (size_t __i1 = 0; __i1 < 13 - __i0; __i1++) {"
         "for (size_t __i2 = 0; __i2 < 13 - __i1 - __i0; __i2++) {" ],
       [ // the two descending spellings, neither of which is an OpenMP
         // canonical loop form anyway
         "__i0-- > 0"
         "__i0 = 13;" ]) ]

/// Which loop index the pragma precedes, for the inner-licence case. Presence
/// alone cannot distinguish "parallelized the licensed inner level" from
/// "parallelized the unlicensed outer level".
let private ompPlacementCases : (string * string * string) list =
    // (name, source, index variable the pragma must immediately precede)
    let arrays = "let A = [1.0, 2.0, 3.0]\nlet B = [4.0, 5.0, 6.0]\n"
    let apply = "let m = object_for(k) <@> (A, B) |> compute\n"
    let kern clause = $"function k(a: Float64, b: Float64) where {clause} = a * b\n"
    [ ("outer_licence_pragma_on_i0", kern "omp(a: 1)" + arrays + apply, "__i0")
      ("inner_licence_pragma_on_i1", kern "omp(b: 1)" + arrays + apply, "__i1") ]

/// The RAGGED / GROUPED PEEL row loop (`CodeGen.peelRowPragma`).
///
/// `tryRaggedPeel` and `tryGroupedZipPeel` hand-roll their `__g` row loop and
/// bypass the generic loop-nest builder, which is the only place `genNestPragma`
/// attaches a pragma. A `where omp(...)` clause on a kernel applied to a grouped
/// or ragged array was therefore ACCEPTED AND SILENTLY DROPPED -- and because
/// "asked and got serial" is byte-identical to "never asked", the drop was
/// invisible. These cases pin both directions of the fix.
///
/// A SEPARATE TABLE from `ompDepthCases`, which asserts exactly one pragma
/// PROGRAM-WIDE. That is a different claim than "one pragma for this nest" once
/// group-by emission is in the program, and conflating them would make a future
/// pragma elsewhere in the group-by lowering read as a failure here.
///
/// `None` means NO pragma anywhere and no `[omp]` marker either -- parallelism is
/// opt-in, and the clause-deleted twins are the claim that silence stays the
/// default so existing corpus output is unchanged.
///
/// KERNELS ARE LAMBDAS, NOT NAMED FUNCTIONS, deliberately: `checkOmpInternalLoop`
/// fires only on function decls, and warns when a licensed param is an operand of
/// an inner apply whose kernel carries no clause. A named-function spelling of
/// the zip case could earn a stray BL4001 that has nothing to do with what these
/// cases measure. The corpus uses the lambda spelling for these shapes anyway.
let private peelPragmaCases : (string * string * string option) list =
    // (name, source, the single expected pragma line -- None = no pragma at all)
    let dynamicPragma = Some "#pragma omp parallel for schedule(dynamic)"
    // Grouped: uneven group sizes, the shape `schedule(dynamic)` exists for.
    let grouped body =
        "let region = [0, 1, 0, 1, 1]\n" +
        "let temps = [20.0, 25.0, 30.0, 22.0, 27.0]\n" +
        "let gk = group_keys(region)\n" +
        "let grouped = group_by(temps, gk)\n" + body
    let groupedZip body =
        "let region = [0, 1, 0, 1, 1]\n" +
        "let a = [1.0, 2.0, 3.0, 4.0, 5.0]\n" +
        "let b = [10.0, 20.0, 30.0, 40.0, 50.0]\n" +
        "let gk = group_keys(region)\n" +
        "let ga = group_by(a, gk)\nlet gb = group_by(b, gk)\n" + body
    let raggedLit body =
        "let lens: Array<Int64 like Idx<3>> = [3, 2, 1]\n" +
        "let r: Array<Float64 like Idx<3>, RaggedIdx<lens>> = [[1.0, 2.0, 3.0], [4.0, 5.0], [6.0]]\n" + body
    let raggedInline body =
        "let r = [[1.0, 2.0, 3.0], [4.0, 5.0], [6.0, 7.0, 8.0, 9.0]]\n" + body
    let row = "Array<Float64 like RaggedIdx<_>>"
    [ // ---- grouped, single operand: the reported case ---------------------
      ("peel_grouped_reduce_licensed",
       grouped ($"let s = method_for(grouped) <@> lambda(g: {row}) where omp(g: 1) -> reduce(g, (+)) |> compute\n"),
       dynamicPragma)
      ("peel_grouped_reduce_unlicensed",
       grouped ($"let s = method_for(grouped) <@> lambda(g: {row}) -> reduce(g, (+)) |> compute\n"),
       None)
      // ---- grouped co-iteration (tryGroupedZipPeel) ------------------------
      ("peel_grouped_zip_licensed",
       groupedZip ($"let d = method_for(zip(ga, gb)) <@> lambda(ra: {row}, rb: {row}) where omp(ra: 1) -> prodsum(ra, rb) |> compute\n"),
       dynamicPragma)
      ("peel_grouped_zip_unlicensed",
       groupedZip ($"let d = method_for(zip(ga, gb)) <@> lambda(ra: {row}, rb: {row}) -> prodsum(ra, rb) |> compute\n"),
       None)
      // ---- ragged LITERAL, row-consuming ----------------------------------
      ("peel_ragged_literal_licensed",
       raggedLit ($"let s = method_for(r) <@> lambda(g: {row}) where omp(g: 1) -> reduce(g, (+)) |> compute\n"),
       dynamicPragma)
      ("peel_ragged_literal_unlicensed",
       raggedLit ($"let s = method_for(r) <@> lambda(g: {row}) -> reduce(g, (+)) |> compute\n"),
       None)
      // ---- ragged ELEMENTWISE map (the two-loop arm) ----------------------
      // The pragma must land on `__g`; the `__k` header below it stays bare.
      // Honouring `omp(e: 2)` would need collapse(2), and the inner bound
      // `r.lens[__g]` is a memory load rather than affine in `__g` -- outside
      // even OpenMP 5.0's non-rectangular collapse, so it would be ill-formed.
      ("peel_ragged_elementwise_licensed",
       raggedInline "let d = method_for(r) <@> lambda(e) where omp(e: 1) -> e * 2.0 |> compute\n",
       dynamicPragma)
      ("peel_ragged_elementwise_unlicensed",
       raggedInline "let d = method_for(r) <@> lambda(e) -> e * 2.0 |> compute\n",
       None) ]

/// `omp(g: 0)`: REQUESTED, but licensing no level. `parseOmpArgs` accepts any
/// `TokInt`, so this parses with `IsOmpParallel = true` and a maximum depth of 0
/// -- which makes `peelRowPragma`'s licence-decline branch reachable, and that
/// branch is the whole difference between declining out loud and the silent drop
/// this change exists to fix.
///
/// NOT expressible in `peelPragmaCases`: its `None` arm means "no pragma AND no
/// marker" (never asked), whereas this case must emit no pragma AND a marker.
/// The two expectations are opposites, which is why this is its own case.
let private peelDeclineMarkerCase : string * string =
    ("peel_depth_zero_reports_serial",
     "let region = [0, 1, 0, 1, 1]\n" +
     "let temps = [20.0, 25.0, 30.0, 22.0, 27.0]\n" +
     "let gk = group_keys(region)\n" +
     "let grouped = group_by(temps, gk)\n" +
     "let s = method_for(grouped) <@> lambda(g: Array<Float64 like RaggedIdx<_>>) where omp(g: 0) -> reduce(g, (+)) |> compute\n")

// ============================================================================
// BLADE_OMP_THREADS -- the serial-emission BUILD knob
// ============================================================================
//
// The LICENCE in source ("this kernel is safe to thread") and the BUILD KNOB
// ("this deployment spends licensed parallelism") are separate decisions; the
// knob turns the second off without touching the first. See
// `CodeGen.ompThreadEmissionEnabled` for the measurement that motivated it (a
// pragma'd row map at OMP_NUM_THREADS=1 runs 1.86x SLOWER than the same code
// emitted without the pragma -- GCC's outlining is a compile-time cost no
// runtime thread count recovers).
//
// Every case below runs the SAME sources the blocks above run, so a licence
// that stopped reaching codegen would fail there first and these would not
// silently take its place.

/// What each formerly-threaded site emits with the knob on. `mustNotContain`
/// carries the actual claim in every case: the point of the knob is the ABSENCE
/// of team-creating constructs, and `omp_get_*` absence is what says the
/// program acquired no OpenMP runtime dependency either.
let private ompThreadsKnobCases : (string * string * string list * string list) list =
    // (name, source, mustContain, mustNotContain)
    // Non-power-of-two extents throughout, per the repo's stride discipline.
    let arrays = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]\nlet B = [4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]\n"
    let arr = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]\n"
    let marker = "// [omp] requested but emitted serial: BLADE_OMP_THREADS=1"
    [ // ---- loop nests: genNestPragma's three forms all go away --------------
      ("knob_licensed_map_emits_no_team",
       "function cov(a: Float64, b: Float64) where omp(a: 1) = a * b\n" + arrays +
       "let m = object_for(cov) <@> (A, B) |> compute\n",
       [ marker; "for (size_t __i0 = 0; __i0 < 7; __i0++) {" ],
       [ "#pragma omp parallel"; "omp_get_" ])
      ("knob_collapse_form_suppressed",
       "function k(a: Float64, b: Float64) where omp(a: 1, b: 1) = a * b + 1.0\n" +
       "let P = [1.5, 2.5, 3.5, 4.5, 5.5]\nlet Q = [0.25, 0.5, 0.75, 1.25]\n" +
       "let M = object_for(k) <@> (P, Q) |> compute\n",
       [ marker ], [ "#pragma omp parallel"; "collapse("; "omp_get_" ])
      // The triangular arm: `schedule(dynamic)` is a `parallel for` clause, so
      // it goes with the construct rather than surviving on a serial loop.
      ("knob_triangular_dynamic_suppressed",
       "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0]\n" +
       "let L = method_for(A, A, A)\n" +
       "let k = lambda(x, y, z) where comm(x, y, z), omp(x: 1) -> x * y + z\n" +
       "let R = L <@> k |> compute\n",
       [ marker; "for (size_t __i1 = 0; __i1 < 13 - __i0; __i1++) {" ],
       [ "#pragma omp parallel"; "schedule(dynamic)"; "omp_get_" ])
      // ---- the flat elementwise fast path KEEPS ITS VECTORIZATION ----------
      // `parallel for simd` is two constructs; only the first opens a team.
      // Dropping to `BLADE_IVDEP` here would throw away a vectorization the
      // knob was never about, so this case is the one that pins the split.
      ("knob_flat_elementwise_keeps_omp_simd",
       "function k(a: Float64) where omp(a: 2) = a * 2.0\n" +
       "let M = [[1.0, 2.0, 3.0, 4.0, 5.0], [4.0, 5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0, 13.0]]\n" +
       "let m = object_for(k) <@> (M) |> compute\n",
       [ marker; "#pragma omp simd" ],
       [ "#pragma omp parallel"; "omp_get_" ])
      // ---- reduce Path A: same split, through the portable macro -----------
      ("knob_reduce_path_a_keeps_simd_reduction",
       arr + "let s = reduce(A, lambda(a, b) where omp -> a + b)\n",
       [ marker; "BLADE_OMP_SIMD_REDUCTION(+:s)" ],
       [ "#pragma omp parallel"; "omp_get_" ])
      ("knob_reduce_path_a_product",
       arr + "let s = reduce(A, lambda(a, b) where omp -> a * b)\n",
       [ "BLADE_OMP_SIMD_REDUCTION(*:s)" ],
       [ "#pragma omp parallel"; "omp_get_" ])
      // ---- reduce Path B: nothing to keep, so the serial arm ---------------
      // Path B IS the threading (an explicit team with per-thread partials), so
      // unlike Path A there is no vector half to preserve. The kernel lands on
      // the arm an UNLICENSED kernel takes, and the marker is what keeps the
      // dropped clause from being silent there.
      ("knob_reduce_path_b_named_comm_serial",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" + arr +
       "let s = reduce(A, myAdd)\n",
       [ marker; "// reduce: accumulator loop, eager" ],
       [ "#pragma omp"; "omp_get_"; "__rpart_s" ])
      // `axes = 2` for the reason spelled out at `reduce_over_computation_chunked`:
      // without it this is a partial fold that never had a chunk plan to drop,
      // so the knob arm and its `_control_knob_off` twin both go vacuous.
      ("knob_reduce_over_computation_serial",
       "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" + arr +
       "let B = [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]\n" +
       "let s = reduce(method_for(A, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n",
       [ marker ],
       [ "#pragma omp"; "omp_get_"; "outer level chunked" ])
      // ---- the intrinsic emitters (gram / matmul) --------------------------
      // These emit the portable MACRO, not a `#pragma` line, because their
      // output can be space-joined into a one-line IIFE -- which is also why
      // their suppression marker is a BLOCK comment. Both facts are pinned.
      // ---- the ragged / grouped PEEL row loop ------------------------------
      // The peel hand-rolls its `__g` loop outside the nest builder, so it needs
      // its own knob check; without this case the knob could silently stop
      // covering the newest thread-emitting site. The loop below generates a
      // knob-OFF control twin requiring `#pragma omp parallel` to be present,
      // which is what keeps this arm from passing vacuously.
      ("knob_ragged_peel_row_loop_suppressed",
       "let region = [0, 1, 0, 1, 1]\n" +
       "let temps = [20.0, 25.0, 30.0, 22.0, 27.0]\n" +
       "let gk = group_keys(region)\n" +
       "let grouped = group_by(temps, gk)\n" +
       "let s = method_for(grouped) <@> lambda(g: Array<Float64 like RaggedIdx<_>>) where omp(g: 1) -> reduce(g, (+)) |> compute\n",
       [ marker; "for (size_t __g = 0; __g < gk__ngroups; __g++) {" ],
       [ "#pragma omp parallel"; "schedule(dynamic)"; "omp_get_" ])
      // ---- the intrinsic emitters (gram / matmul) --------------------------
      // These emit the portable MACRO, not a `#pragma` line, because their
      // output can be space-joined into a one-line IIFE -- which is also why
      // their suppression marker is a BLOCK comment. Both facts are pinned.
      ("knob_native_matmul_gram_macros_suppressed",
       "import math as m\n" +
       "type M35 = Array<Float64 like Idx<3>, Idx<5>>\n" +
       "type M57 = Array<Float64 like Idx<5>, Idx<7>>\n" +
       "let A: M35 = [[1.0, 2.0, 3.0, 4.0, 5.0], [6.0, 7.0, 8.0, 9.0, 10.0], [11.0, 12.0, 13.0, 14.0, 15.0]]\n" +
       "let B: M57 = [[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0], [8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0], [15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0], [22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0], [29.0, 30.0, 31.0, 32.0, 33.0, 34.0, 35.0]]\n" +
       "let P = m.matmul(A, B)\nlet G = gram(A, A)\n",
       [ "/* [omp] thread pragma suppressed: BLADE_OMP_THREADS=1"
         // the loops themselves are untouched -- only the macro line moved
         "for (size_t __mi = 0; __mi < 3; __mi++) {"
         "for (size_t __gi = 0; __gi < 3; __gi++) {"
         // ivdep is a VECTORIZATION assertion, not a thread construct: it stays
         "BLADE_IVDEP" ],
       [ "BLADE_OMP_PARALLEL_FOR"; "#pragma omp parallel"; "omp_get_" ]) ]

/// The strongest statement the knob makes: with it on, a LICENSED program emits
/// the SAME C++ as the identical program with the clause deleted. "Serial
/// emission" is not "some other serial shape" -- `pragmaLevel` goes to None, so
/// `ompLastLevel` goes to -1 and `BLADE_IVDEP` lands exactly where the
/// unlicensed nest puts it, and every downstream decision follows.
///
/// Compared modulo the `[omp]` census markers, which the licensed program emits
/// BY DESIGN (a dropped clause must not be silent) and the unlicensed one has
/// nothing to report.
let private ompThreadsEquivalenceCases : (string * string * string) list =
    // (name, licensed source, the same program with the clause deleted)
    let arrays = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]\nlet B = [4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]\n"
    let tri = "let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0]\nlet L = method_for(A, A, A)\n"
    // `clause` is the ONLY textual difference between the two programs, so the
    // comparison cannot accidentally be measuring some other edit.
    let grouped (clause: string) =
        "let region = [0, 1, 0, 1, 1]\n" +
        "let temps = [20.0, 25.0, 30.0, 22.0, 27.0]\n" +
        "let gk = group_keys(region)\n" +
        "let grouped = group_by(temps, gk)\n" +
        $"let s = method_for(grouped) <@> lambda(g: Array<Float64 like RaggedIdx<_>>) {clause}-> reduce(g, (+)) |> compute\n"
    [ ("knob_equals_unlicensed_map",
       "function cov(a: Float64, b: Float64) where omp(a: 1) = a * b\n" + arrays +
       "let m = object_for(cov) <@> (A, B) |> compute\n",
       "function cov(a: Float64, b: Float64) = a * b\n" + arrays +
       "let m = object_for(cov) <@> (A, B) |> compute\n")
      // The ragged/grouped PEEL. Stronger here than for the nest cases: the peel
      // has no `pragmaLevel`/`ompLastLevel` state to reset, so byte-identity is a
      // direct statement that adding the licence changes NOTHING about the
      // emitted row loop except the pragma line itself -- no reordered
      // declarations, no altered subscripts, and in particular no reassociated
      // per-row reduction.
      ("knob_equals_unlicensed_grouped_peel",
       grouped "where omp(g: 1) ", grouped "")
      ("knob_equals_unlicensed_triangular",
       tri + "let k = lambda(x, y, z) where comm(x, y, z), omp(x: 1) -> x * y + z\n" +
       "let R = L <@> k |> compute\n",
       tri + "let k = lambda(x, y, z) where comm(x, y, z) -> x * y + z\n" +
       "let R = L <@> k |> compute\n") ]

/// Assert that `omp` reaches codegen as a pragma for every spelling of a
/// kernel, and reaches it for NO spelling that omitted the clause.
let runOmpPragmaTests () : Blade.Tests.TestHarness.BlockResult =
    printHeader "OpenMP Pragma Emission"
    // Everything in this block asserts the DEFAULT emission, so the serial
    // build knob is pinned unset for it -- see `pinOmpThreadsUnset`. The
    // knob's own arms at the bottom re-pin it ON for their own scope.
    use _ompThreadsPin = pinOmpThreadsUnset ()
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [name]
        resultLine Fail name detail
    for (name, src, expectPragma) in ompPragmaCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            let hasPragma = cpp.Contains "#pragma omp"
            // A dropped clause is exactly the silent case: no pragma AND no
            // marker. Assert the marker's absence too, so a future change that
            // "fixes" a case by suppressing it loudly is still caught here.
            let hasMarker = cpp.Contains "[omp] requested but emitted serial"
            if hasPragma <> expectPragma then
                fail name
                    (sprintf "expected %s, got %s%s"
                        (if expectPragma then "a pragma" else "no pragma")
                        (if hasPragma then "a pragma" else "none")
                        (if hasMarker then " (nest reports omp requested but suppressed)" else ""))
            elif hasMarker then
                fail name "unexpected omp-suppressed marker in generated code"
            else
                passed <- passed + 1
                resultLine Pass name
                    (if expectPragma then "pragma emitted" else "serial (no clause)")
    // ---- depth-as-licence: the exact pragma, not just its presence ----
    for (name, src, expectedPragma) in ompDepthCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            // Compare the whole pragma line: `collapse(2)` vs plain is exactly
            // the distinction the licence controls, and `Contains` on the plain
            // form would match the collapse form as a prefix.
            let pragmaLines =
                cpp.Split('\n')
                |> Array.map (_.Trim())
                |> Array.filter (fun l -> l.StartsWith "#pragma omp")
                |> Array.toList
            match pragmaLines with
            | [actual] when actual = expectedPragma ->
                passed <- passed + 1
                resultLine Pass name actual
            | [actual] -> fail name ($"expected `{expectedPragma}`, got `{actual}`")
            | [] -> fail name ($"expected `{expectedPragma}`, got no pragma")
            | many -> fail name ($"""expected one pragma, got {many.Length}: {(String.concat " | " many)}""")
    // ---- placement: WHICH loop the pragma governs ----
    for (name, src, expectedIdx) in ompPlacementCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            // The emitter writes the pragma and the loop header it governs as
            // consecutive lines, so the next non-blank line after the pragma is
            // that header.
            let lines = cpp.Split('\n') |> Array.map (_.Trim())
            let governed =
                lines
                |> Array.tryFindIndex (fun l -> l.StartsWith "#pragma omp")
                |> Option.bind (fun i ->
                    lines
                    |> Array.skip (i + 1)
                    |> Array.tryFind (fun l -> l <> "")
                    |> Option.map (fun header ->
                        // "for (size_t __iN = ..." -> "__iN"
                        header.Split([|' '; '('; ')'; '='|])
                        |> Array.tryFind (fun t -> t.StartsWith "__i")
                        |> Option.defaultValue header))
            match governed with
            | Some idx when idx = expectedIdx ->
                passed <- passed + 1
                resultLine Pass name ($"pragma governs {idx}")
            | Some idx -> fail name ($"pragma governs {idx}, expected {expectedIdx}")
            | None -> fail name "no pragma found"
    // ---- the RAGGED / GROUPED PEEL row loop ----
    // Count, exact string, AND placement in one assertion, because for this site
    // each is separately falsifiable: the peel could emit the right pragma on the
    // wrong loop (`group_keys` builds its own `__g` loop earlier in the program),
    // or the right count with the wrong schedule.
    //
    // Placement is checked as "the next non-blank line AFTER the pragma", reusing
    // the ompPlacementCases rule rather than searching for `for (size_t __g`
    // directly -- a direct search would find the group_keys build loop and pass
    // for the wrong reason.
    for (name, src, expectedPragma) in peelPragmaCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            let lines = cpp.Split('\n') |> Array.map (_.Trim())
            let pragmaLines = lines |> Array.filter (fun l -> l.StartsWith "#pragma omp") |> Array.toList
            let governedIsRowLoop () =
                lines
                |> Array.tryFindIndex (fun l -> l.StartsWith "#pragma omp")
                |> Option.bind (fun i -> lines |> Array.skip (i + 1) |> Array.tryFind (fun l -> l <> ""))
                |> Option.map (fun header -> header.StartsWith "for (size_t __g = 0;")
                |> Option.defaultValue false
            match expectedPragma, pragmaLines with
            | None, [] ->
                // Parallelism is opt-in, so no pragma is only half the claim: the
                // decline marker must be absent too, or "never asked" would be
                // indistinguishable from "asked and was refused".
                if cpp.Contains "[omp]" then
                    fail name "expected no pragma and no [omp] marker, but a marker was emitted"
                else
                    passed <- passed + 1
                    resultLine Pass name "serial, and silent (no clause)"
            | None, many ->
                fail name ($"""expected no pragma, got {many.Length}: {(String.concat " | " many)}""")
            | Some expected, [actual] when actual = expected ->
                if not (governedIsRowLoop ()) then
                    fail name "pragma emitted but does not immediately precede the peel's `__g` row loop"
                elif cpp.Contains "[omp] requested but emitted serial" then
                    fail name "pragma emitted AND a requested-but-serial marker -- the two are contradictory"
                else
                    passed <- passed + 1
                    resultLine Pass name ($"{actual} on __g")
            | Some expected, [actual] -> fail name ($"expected `{expected}`, got `{actual}`")
            | Some expected, [] -> fail name ($"expected `{expected}`, got no pragma")
            | Some expected, many ->
                fail name ($"""expected one `{expected}`, got {many.Length}: {(String.concat " | " many)}""")
    // ---- the peel's licence DECLINE is audible ----
    (let (name, src) = peelDeclineMarkerCase
     match cppOf name src with
     | Error e -> fail name e
     | Ok cpp ->
         let expected = "// [omp] requested but emitted serial: the omp(...) licence covers no level of the peeled row loop"
         if cpp.Contains expected then
             passed <- passed + 1
             resultLine Pass name "decline reported in the emitted C++"
         else
             fail name "omp(g: 0) declined SILENTLY -- no licence-decline marker in the emitted C++")
    // ---- emission SHAPE: native gram/matmul arms, ivdep, loop direction ----
    // Compared against the source with every line's LEADING INDENT stripped:
    // these assertions are about which statements are emitted and in what
    // order, never about how deep the emitter happened to indent them, and a
    // multi-line pattern would otherwise have to restate the nesting depth of
    // whatever construct the form was materialized inside.
    for (name, src, mustContain, mustNotContain) in emissionShapeCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            let flat =
                cpp.Split('\n') |> Array.map (_.TrimStart()) |> String.concat "\n"
            let missing = mustContain |> List.filter (fun s -> not (flat.Contains s))
            let present = mustNotContain |> List.filter flat.Contains
            if not missing.IsEmpty then
                fail name ($"""generated C++ lacks: {(String.concat " | " (missing |> List.map (_.Replace("\n", " \\n "))))}""")
            elif not present.IsEmpty then
                fail name ($"""generated C++ still contains: {(String.concat " | " (present |> List.map (_.Replace("\n", " \\n "))))}""")
            else
                passed <- passed + 1
                resultLine Pass name "emission shape as expected"
    // ---- comm-licensed parallel REDUCTIONS: which emission path fired ----
    for (name, src, mustContain, mustNotContain) in ompReduceCases do
        match cppOf name src with
        | Error e -> fail name e
        | Ok cpp ->
            let missing = mustContain |> List.filter (fun s -> not (cpp.Contains s))
            let present = mustNotContain |> List.filter cpp.Contains
            // A licensed fold calls the OpenMP runtime API, which needs the
            // header — `#pragma omp` alone does not. Asserted as "included, not
            // commented out", the shape genIncludes can regress to.
            let needsHeader = mustContain |> List.exists (fun s -> s.Contains "omp_get_")
            let headerOk =
                not needsHeader
                || (cpp.Split('\n')
                    |> Array.exists (fun l -> l.Trim().StartsWith "#include <omp.h>"))
            if not missing.IsEmpty then
                fail name ($"""generated C++ lacks: {(String.concat " | " missing)}""")
            elif not present.IsEmpty then
                fail name ($"""generated C++ unexpectedly contains: {(String.concat " | " present)}""")
            elif not headerOk then
                fail name "calls the omp_* runtime API but <omp.h> is not included"
            else
                passed <- passed + 1
                resultLine Pass name "emission path as expected"
    // ---- the unlicensed case is a hard error, not a silent serial fold ----
    for (name, src) in ompReduceDiagCases do
        // lowerDiag, not lower: the CODE is the assertion, and the plain
        // string channel renders the message without it.
        match fst (Lowering.lowerDiag None src) with
        | Ok _ -> fail name "compiled cleanly; expected BL4016"
        | Error diags ->
            let codes = diags |> List.map (fun (d: Blade.Diagnostics.Diagnostic) -> d.Code)
            if List.contains "BL4016" codes then
                passed <- passed + 1
                resultLine Pass name "BL4016"
            else fail name ($"""expected BL4016, got: {(String.concat ", " codes)}""")
    // ---- BLADE_OMP_THREADS=1: what each formerly-threaded site emits ----
    // The pin is scoped to these arms and restored immediately, so nothing
    // above (or in any later block) can see it. Same discipline as the
    // BLADE_FP_REASSOC / BLADE_BLAS pins in the omp-reduce block.
    for (name, src, mustContain, mustNotContain) in ompThreadsKnobCases do
        let emitted =
            use _knob = pinEnv "BLADE_OMP_THREADS" "1"
            cppOf name src
        match emitted with
        | Error e -> fail name e
        | Ok cpp ->
            let flat =
                cpp.Split('\n') |> Array.map (_.TrimStart()) |> String.concat "\n"
            let missing = mustContain |> List.filter (fun s -> not (flat.Contains s))
            let present = mustNotContain |> List.filter flat.Contains
            if not missing.IsEmpty then
                fail name ($"""knob-on C++ lacks: {(String.concat " | " (missing |> List.map (_.Replace("\n", " \\n "))))}""")
            elif not present.IsEmpty then
                fail name ($"""knob-on C++ still contains: {(String.concat " | " present)}""")
            else
                passed <- passed + 1
                resultLine Pass name "serial emission as expected"
    // ---- and the same programs with the knob OFF still get their pragma ----
    // Without this pair the arms above could pass by the SOURCE having lost its
    // licence rather than by the knob suppressing it.
    for (name, src, _, _) in ompThreadsKnobCases do
        let ctlName = name + "_control_knob_off"
        match cppOf ctlName src with
        | Error e -> fail ctlName e
        | Ok cpp ->
            let threaded =
                cpp.Contains "#pragma omp parallel" || cpp.Contains "BLADE_OMP_PARALLEL_FOR"
            if not threaded then
                fail ctlName "knob OFF emitted no thread construct either (the source lost its licence; the knob arm above is vacuous)"
            elif cpp.Contains "BLADE_OMP_THREADS" then
                fail ctlName "knob OFF emitted a suppression marker"
            else
                passed <- passed + 1
                resultLine Pass ctlName "threaded with the knob off"
    // ---- knob-on emission == the SAME PROGRAM WITHOUT THE CLAUSE ----
    for (name, licensedSrc, unlicensedSrc) in ompThreadsEquivalenceCases do
        // The same testName for both, so the generated banner/timing lines
        // (which embed it) cannot be what differs.
        let licensed =
            use _knob = pinEnv "BLADE_OMP_THREADS" "1"
            cppOf name licensedSrc
        match licensed, cppOf name unlicensedSrc with
        | Error e, _ -> fail name ($"knob-on emit: {e}")
        | _, Error e -> fail name ($"unlicensed emit: {e}")
        | Ok onCpp, Ok unlicCpp ->
            // The census markers are the ONE intended difference: the licensed
            // program has a dropped clause to report and the unlicensed one has
            // nothing to say.
            let dropMarkers (s: string) =
                s.Split('\n') |> Array.filter (fun l -> not (l.Contains "[omp]")) |> String.concat "\n"
            if dropMarkers onCpp <> dropMarkers unlicCpp then
                fail name "knob-on emission differs from the same program without the omp clause"
            elif not (onCpp.Contains "[omp] requested but emitted serial: BLADE_OMP_THREADS=1") then
                fail name "knob-on emission carries no census marker (a dropped clause must not be silent)"
            else
                passed <- passed + 1
                resultLine Pass name "byte-identical to the unlicensed program (modulo the census marker)"
    printFooter "OpenMP Pragma" [$"{passed} passed"; $"{failed} failed"]
    { Block = "OpenMP Pragma"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }

/// Run OpenMP thread-coverage tests. Generates representative loop-nest
/// programs with codegen TEST MODE on (which injects per-region thread
/// observation), compiles with -fopenmp, runs with OMP_NUM_THREADS forced > 1,
/// parses the emitted "[omp-coverage] region=... teamsz=K distinct=D maxth=M"
/// lines, and applies the rule:
///   - maxth <= 1            : single-core context — cannot test parallelism;
///                             reported as a skip-ish PASS (not a failure).
///   - maxth > 1, teamsz <= 1: ERROR — a loop that should be an OpenMP-parallel
///                             loop ran as a serial region (pragma not honored).
///   - maxth > 1, teamsz > 1 : PASS — a genuine parallel team was formed. (If
///                             distinct == 1, the scheduler put all work on one
///                             thread; that is an allowed scheduler choice, so
///                             it is reported as a WARNING, not a failure.)
///
/// Returns 0 if no errors (warnings allowed), 1 if any error.
let runOmpCoverageTests () : Blade.Tests.TestHarness.BlockResult =
    let caps = capabilities.Value
    printHeader "OpenMP Thread-Coverage Tests"
    // This block's whole question is "does an emitted pragma form a real team",
    // which a globally-set BLADE_OMP_THREADS=1 would answer by deleting the
    // pragma -- and the block would then report its own vacuity as a pass (zero
    // [omp-coverage] lines is already a hard failure below, so it would in fact
    // report it as a FAILURE unrelated to any real regression). Pinned unset.
    use _ompThreadsPin = pinOmpThreadsUnset ()
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile the -fopenmp coverage programs)."
        // Skipped = 1, not 0: a Skipped = 0 return made a toolchain-less box
        // print "0 passed, 0 failed" for this block with no skip note. Same
        // convention as DiffOracle/InterpDiff.
        { Block = "OpenMP Coverage"; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    else
        // Representative programs exercising each parallelization strategy.
        // Source strings are defined as separate bindings (not inline in the
        // list) so the triple-quoted content does not disturb F# offside parsing.
        //
        // COVERAGE programs (just need to compile + form a parallel region):
        //   rect (collapse), symmetric (dynamic outer), and a partial-comm 3-arg
        //   kernel (mixed symmetry structure). Antisymmetric STRICT iteration is
        //   not expressed by a simple clause (it requires AntisymIdx typing), so
        //   it is intentionally omitted here rather than guessed at.
        let rectSrc =
            "let A = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]\n" +
            "let B = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]\n" +
            "let L = method_for(A, B)\n" +
            "let f = lambda(x, y) where omp(x: 1) -> x * y\n" +
            "let result = L <@> f |> compute\n"
        let symSrc =
            "let A = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]\n" +
            "let L = method_for(A, A)\n" +
            "let k = lambda(x, y) where comm(x, y), omp(x: 1) -> x * y\n" +
            "let result = L <@> k |> compute\n"
        // Partial comm: 3-arg kernel with comm on a subset (proven form, see
        // Test_Symmetry). Exercises a mixed symmetry nest through genNestPragma.
        let mixedSrc =
            "let A = [1.0,2.0,3.0,4.0]\n" +
            "let L = method_for(A, A, A)\n" +
            "let k = lambda(x, y, z) where comm(x, y), omp(x: 1) -> x * y * z\n" +
            "let result = L <@> k |> compute\n"
        // Phase 2 Path B: a comm-licensed fold over a DEFERRED computation. Its
        // parallel region is an explicit team with per-thread partials, not a
        // `parallel for`, so pragma text alone cannot say whether a team really
        // forms — this is the ground-truth check that it does.
        // `axes = 2` is the full fold's spelling (see the note at
        // `reduce_over_computation_chunked`); without it this is a partial fold
        // with no team to observe, and the coverage check goes vacuous.
        let foldSrc =
            "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n" +
            "let A = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]\n" +
            "let B = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]\n" +
            "let s = reduce(method_for(A, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n"
        let programs =
            [ ("rect_outer_product", rectSrc)
              ("symmetric_triangular", symSrc)
              ("mixed_partial_comm", mixedSrc)
              ("comm_licensed_fold", foldSrc) ]
        let outputDir = "./generated_omp_coverage"
        Directory.CreateDirectory(outputDir) |> ignore
        // Write runtime headers into the output dir so the generated programs'
        // #include "nested_array_utilities.hpp" / "nested_array_types.hpp"
        // resolve at g++ time (same as the main test path does).
        CodeGen.deployRuntimeHeaders outputDir
        let mutable errors = 0
        let mutable warnings = 0
        let mutable passed = 0
        let mutable failedNames = []
        // Force a multi-thread environment for the run so the gate is meaningful.
        let forcedThreads = "4"
        for (name, src) in programs do
            // Generate with codegen test-mode ON (injects instrumentation), then
            // restore so nothing else in the process is affected.
            setOmpTestMode true
            let outcome =
                try
                    let safeName = sanitizeFileName name
                    match lower src with
                    | Error e -> Error ($"lower failed: {e}")
                    | Ok ir0 ->
                        // A validation error is a hard failure. The old
                        // `| Error _ -> ir0` ("validation errors don't block this
                        // probe") generated C++ from invalid IR, so a validator
                        // regression on these programs was invisible here.
                        match IRValidate.validateIR ir0 with
                        | Error validationErrors ->
                            Error ($"""IR validation failed: {(String.concat "; " validationErrors)}""")
                        | Ok ir ->
                            let (cppCode, _warnings) = CodeGen.genSelfContainedProgramFromIR ir name
                            let srcFile = Path.Combine(outputDir, safeName + ".cpp")
                            File.WriteAllText(srcFile, cppCode)
                            Ok srcFile
                with ex -> Error ($"codegen failed: {ex.Message}")
            setOmpTestMode false
            match outcome with
            | Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name ($"generation: {e}"); errors <- errors + 1; failedNames <- failedNames @ [name]
            | Ok srcFile ->
                let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
                // Use ABSOLUTE paths for g++. srcFile is relative to the process
                // cwd; passing it relative while also setting WorkingDirectory to
                // the output dir caused g++ to resolve it against that dir (a
                // doubled path). Absolute paths make the working dir irrelevant.
                let srcAbs = Path.GetFullPath(srcFile)
                let exeAbs = Path.ChangeExtension(srcAbs, exeExt)
                let cpsi = ProcessStartInfo("g++", $"-std=c++17 {(optFlags ())} -fopenmp -o \"{exeAbs}\" \"{srcAbs}\"")
                cpsi.RedirectStandardError <- true
                cpsi.UseShellExecute <- false
                use cproc = Process.Start(cpsi)
                let cerr = Blade.Runtime.readToEndOffPool cproc.StandardError
                // WaitForExit(ms) returns false on TIMEOUT; ExitCode is
                // meaningless then, so a hung g++ used to be read as success.
                let cExited = cproc.WaitForExit(60000)
                if not cExited then
                    (try cproc.Kill(true) with _ -> ())
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name "compile timed out (60s)"
                    errors <- errors + 1
                    failedNames <- failedNames @ [name]
                elif cproc.ExitCode <> 0 then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name ($"compile: {cerr.Result}")
                    errors <- errors + 1
                    failedNames <- failedNames @ [name]
                else
                    // Run with OMP_NUM_THREADS forced.
                    let rpsi = ProcessStartInfo(exeAbs)
                    rpsi.RedirectStandardOutput <- true
                    rpsi.RedirectStandardError <- true
                    rpsi.UseShellExecute <- false
                    rpsi.WorkingDirectory <- Path.GetDirectoryName(exeAbs)
                    rpsi.Environment.["OMP_NUM_THREADS"] <- forcedThreads
                    use rproc = Process.Start(rpsi)
                    let rout = Blade.Runtime.readToEndOffPool rproc.StandardOutput
                    let rerr = Blade.Runtime.readToEndOffPool rproc.StandardError
                    let rExited = rproc.WaitForExit(30000)
                    if not rExited then (try rproc.Kill(true) with _ -> ())
                    let lines = rout.Result.Split('\n') |> Array.filter (_.Contains("[omp-coverage]"))
                    if not rExited then
                        Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name "run timed out (30s)"
                        errors <- errors + 1
                        failedNames <- failedNames @ [name]
                    elif lines.Length = 0 then
                        // Every program in `programs` carries an explicit
                        // `omp(x: 1)` clause, and CodeGen emits the
                        // [omp-coverage] instrumentation exactly when
                        // ompInstrument && outerIsParallel. So zero coverage
                        // lines means the pragma was NOT emitted for an
                        // annotated kernel -- the very condition this block
                        // exists to detect. It used to print a Skip line and
                        // increment nothing at all: not passed, not failed, not
                        // skipped, so the block silently shrank to nothing.
                        Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name
                            (sprintf "no [omp-coverage] line emitted for an omp()-annotated kernel (exit %d)%s"
                                rproc.ExitCode
                                (if String.IsNullOrWhiteSpace rerr.Result then "" else " stderr: " + rerr.Result.Trim()))
                        errors <- errors + 1
                        failedNames <- failedNames @ [name]
                    for line in lines do
                        // parse "region=R teamsz=K distinct=D maxth=M"
                        // None (not -1) on a regex miss: a sentinel -1 flowed
                        // into `maxth <= 1`, which is the UNCONDITIONAL-PASS
                        // arm below, so an unparseable coverage line scored a
                        // "single-core, cannot test parallelism" pass.
                        let getField (k: string) =
                            let m = System.Text.RegularExpressions.Regex.Match(line, k + "=(\\d+)")
                            if m.Success then Some (int m.Groups.[1].Value) else None
                        let teamszO = getField "teamsz"
                        let distinctO = getField "distinct"
                        let maxthO = getField "maxth"
                        let missing =
                            [ if teamszO.IsNone then "teamsz"
                              if distinctO.IsNone then "distinct"
                              if maxthO.IsNone then "maxth" ]
                        let teamsz = defaultArg teamszO 0
                        let distinct = defaultArg distinctO 0
                        let maxth = defaultArg maxthO 0
                        if not missing.IsEmpty then
                            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name
                                ($"""unparseable coverage line (missing {(String.concat ", " missing)}): {(line.Trim())}""")
                            errors <- errors + 1
                            failedNames <- failedNames @ [name]
                        elif maxth <= 1 then
                            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass name ($"single-core: maxth={maxth}, cannot test parallelism")
                            passed <- passed + 1
                        elif teamsz <= 1 then
                            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name ($"parallel loop ran serially (teamsz={teamsz}, maxth={maxth}) -- pragma not honored")
                            errors <- errors + 1
                            failedNames <- failedNames @ [name]
                        elif distinct <= 1 then
                            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass name ($"WARNING: parallel team formed (teamsz={teamsz}) but scheduler used 1 thread (distinct={distinct})")
                            warnings <- warnings + 1
                            passed <- passed + 1
                        else
                            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass name ($"teamsz={teamsz}, distinct={distinct}, maxth={maxth}")
                            passed <- passed + 1

        // -------------------------------------------------------------------
        // VALUE CORRECTNESS UNDER FORCED MULTI-THREADING (#2)
        // -------------------------------------------------------------------
        // The coverage checks above confirm a parallel region forms, but NOT
        // that the parallelized computation produces CORRECT values. A data
        // race in the triangular outer-parallelization (the disjoint-slab
        // assumption) would show as wrong values only under threading — which
        // neither the coverage checks nor the main value suite (default
        // threading) would catch. Here we run a symmetric computation with
        // KNOWN expected values under OMP_NUM_THREADS=4, repeated several times
        // (races are nondeterministic, so one run can pass by luck), and assert
        // the values are correct every time.
        //
        // N=12 symmetric: C(13,2)=78 elements, large enough that the scheduler
        // genuinely distributes the outer loop. Expected values computed here
        // (not hand-written): for comm(x,y)->x*y over A=[1..N], the left-
        // justified symmetric order is A[i]*A[j] for i<=j.
        let nVal = 12
        let aVals = [ for i in 1 .. nVal -> float i ]
        let expectedSym =
            [ for i in 0 .. nVal - 1 do
                for j in i .. nVal - 1 do
                    yield aVals.[i] * aVals.[j] ]
        let aLit = aVals |> List.map (sprintf "%g") |> String.concat ","
        let expectedLit = expectedSym |> List.map (sprintf "%g") |> String.concat ", "
        let valSrc =
            $"let A = [{aLit}]\n" +
            "let L = method_for(A, A)\n" +
            // omp clause so this genuinely runs parallel under OMP_NUM_THREADS=4
            // — otherwise (post-flip) it would be serial and the env var inert,
            // defeating the race-detection purpose of the repeated runs.
            "let k = lambda(x, y) where comm(x, y), omp(x: 1) -> x * y\n" +
            "let result = L <@> k |> compute\n" +
            $"// EXPECT: result = [{expectedLit}]\n"
        printSubHeader "Value correctness under forced threading (N=12 symmetric)"
        setOmpTestMode false  // value test: no instrumentation, just real codegen
        let valOutcome =
            try
                match lower valSrc with
                | Error e -> Error ($"lower failed: {e}")
                | Ok ir0 ->
                    // Hard-fail on validation errors (was `| Error _ -> ir0`).
                    match IRValidate.validateIR ir0 with
                    | Error validationErrors ->
                        Error ($"""IR validation failed: {(String.concat "; " validationErrors)}""")
                    | Ok ir ->
                        let (cppCode, _w) = CodeGen.genSelfContainedProgramFromIR ir "omp_value_check"
                        let sf = Path.Combine(outputDir, "omp_value_check.cpp")
                        File.WriteAllText(sf, cppCode)
                        Ok (Path.GetFullPath sf)
            with ex -> Error ($"codegen failed: {ex.Message}")
        match valOutcome with
        | Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail "omp_value_check" ($"generation: {e}"); errors <- errors + 1; failedNames <- failedNames @ ["omp_value_check"]
        | Ok srcAbs ->
            let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
            let exeAbs = Path.ChangeExtension(srcAbs, exeExt)
            let cpsi = ProcessStartInfo("g++", $"-std=c++17 {(optFlags ())} -fopenmp -o \"{exeAbs}\" \"{srcAbs}\"")
            cpsi.RedirectStandardError <- true
            cpsi.UseShellExecute <- false
            use cproc = Process.Start(cpsi)
            let cerr = Blade.Runtime.readToEndOffPool cproc.StandardError
            let cExited = cproc.WaitForExit(60000)
            if not cExited then
                (try cproc.Kill(true) with _ -> ())
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail "omp_value_check" "compile timed out (60s)"
                errors <- errors + 1
                failedNames <- failedNames @ ["omp_value_check"]
            elif cproc.ExitCode <> 0 then
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail "omp_value_check" ($"compile: {cerr.Result}")
                errors <- errors + 1
                failedNames <- failedNames @ ["omp_value_check"]
            else
                // Malformed-pin gate, same rule as Runner.fs: a `// EXPECT:`
                // line that does not parse is a DROPPED assertion, and
                // checkExpectedValues over an empty pin list returns Ok (),
                // so without this the whole race check could pass vacuously.
                let malformed = parseMalformedExpectLines valSrc
                let expected = parseExpectedValues valSrc
                let mutable allRunsOk = true
                if not malformed.IsEmpty then
                    allRunsOk <- false
                    printfn "    unparseable EXPECT pin(s): %s" (String.concat " | " malformed)
                elif expected.IsEmpty then
                    allRunsOk <- false
                    printfn "    no EXPECT pin parsed -- the value check would be vacuous"
                else
                    // Repeat: a race may pass on some runs and fail on others.
                    for run in 1 .. 5 do
                        let rpsi = ProcessStartInfo(exeAbs)
                        rpsi.RedirectStandardOutput <- true
                        rpsi.RedirectStandardError <- true
                        rpsi.UseShellExecute <- false
                        rpsi.WorkingDirectory <- Path.GetDirectoryName(exeAbs)
                        rpsi.Environment.["OMP_NUM_THREADS"] <- forcedThreads
                        use rproc = Process.Start(rpsi)
                        let rout = Blade.Runtime.readToEndOffPool rproc.StandardOutput
                        let rerrV = Blade.Runtime.readToEndOffPool rproc.StandardError
                        let rExited = rproc.WaitForExit(30000)
                        if not rExited then
                            (try rproc.Kill(true) with _ -> ())
                            rproc.WaitForExit(5000) |> ignore
                            allRunsOk <- false
                            printfn "    run %d: TIMED OUT (30s)" run
                        elif rproc.ExitCode <> 0 then
                            // A crashed run prints little or nothing; with no
                            // exit-code gate the value check over the truncated
                            // stdout was the only verdict.
                            allRunsOk <- false
                            printfn "    run %d: exit %d %s" run rproc.ExitCode (rerrV.Result.Trim())
                        else
                            match checkExpectedValues expected rout.Result with
                            | Ok () -> ()
                            | Error errs ->
                                allRunsOk <- false
                                printfn "    run %d: VALUE MISMATCH (possible race): %s" run (String.concat "; " errs)
                if allRunsOk then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass "omp_value_check" ($"correct values across 5 runs under OMP_NUM_THREADS={forcedThreads}")
                    passed <- passed + 1
                else
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail "omp_value_check" "values incorrect under threading -- likely a data race in parallelization"
                    errors <- errors + 1
                    failedNames <- failedNames @ ["omp_value_check"]

        printFooter "OpenMP Coverage" [$"{passed} passed"; $"{errors} error(s)"; $"{warnings} warning(s)"]
        { Block = "OpenMP Coverage"; Passed = passed; Failed = errors; Skipped = 0; FailedNames = failedNames }

// ============================================================================
// Comm-licensed parallel reductions: VALUES under real threads
// ============================================================================
//
// The pragma block above answers "which emission path fired". This one answers
// the question no string check can: does the parallel fold compute what the
// serial fold computes, when a real OpenMP runtime really splits the axis?
//
// Everything here is a DIFFERENTIAL against the same program with the `omp`
// clause removed — not against hand-written expected values — so the oracle
// cannot drift from the feature. Two properties are asserted separately:
//
//   * VALUE, within 1e-9 of serial. Float tolerance lives HERE and nowhere
//     else: the corpus files for this feature (loops/110, loops/111) are also
//     run by the interpreter differential, which demands byte-identical output,
//     so they are restricted to integer-valued data where every association is
//     exact. This block is not part of that gate and may use awkward values,
//     which is the point — it is where reassociation is actually exercised.
//
//   * RUN-TO-RUN IDENTITY at a fixed OMP_NUM_THREADS, for Path B ONLY. Path B's
//     chunk boundaries and combine order are fixed functions of the team size,
//     so repeated runs reproduce bit-for-bit; Path A hands the combine to the
//     OpenMP runtime, whose order is unspecified by the standard, so asserting
//     it there would be pinning an implementation detail (a legal runtime could
//     fail it).
//
// Also here, and deliberately not a reduce: a COMPILED-AND-RUN case of
// `where omp(a: 1, b: 1)` on a 2-level dense map. `collapse(2)` fuses the two
// headers into one iteration space, and g++ rejects `#pragma GCC ivdep` on a
// header the construct owns ("loop not permitted in intervening code in OpenMP
// loop body" / "not enough nested loops"). The omp-pragma block asserts the
// pragma TEXT and never compiles it; the omp-coverage block compiles but with
// test-mode instrumentation, which disables ivdep by its own gate. Neither can
// see the interaction — so it gets a plain compile+run here.

/// A single compiled program: source -> exe path, ready to run. `testMode` is
/// forced OFF (the coverage instrumentation suppresses Phase 1's ivdep, which
/// is exactly what the collapse case below needs to exercise).
let private compileProgram (outputDir: string) (name: string) (src: string) : Result<string, string> =
    try
        setOmpTestMode false
        match lower src with
        | Error e -> Error ($"lower failed: {e}")
        | Ok ir0 ->
            match IRValidate.validateIR ir0 with
            | Error errs -> Error ($"""IR validation failed: {(String.concat "; " errs)}""")
            | Ok ir ->
                let (cppCode, _w) = CodeGen.genSelfContainedProgramFromIR ir name
                let srcAbs = Path.GetFullPath(Path.Combine(outputDir, sanitizeFileName name + ".cpp"))
                File.WriteAllText(srcAbs, cppCode)
                let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
                let exeAbs = Path.ChangeExtension(srcAbs, exeExt)
                let cpsi = ProcessStartInfo("g++", $"-std=c++17 {(optFlags ())} -fopenmp -o \"{exeAbs}\" \"{srcAbs}\"")
                cpsi.RedirectStandardError <- true
                cpsi.UseShellExecute <- false
                use cproc = Process.Start(cpsi)
                let cerr = Blade.Runtime.readToEndOffPool cproc.StandardError
                if not (cproc.WaitForExit(120000)) then
                    (try cproc.Kill(true) with _ -> ())
                    Error "compile timed out (120s)"
                elif cproc.ExitCode <> 0 then Error ($"compile: {(cerr.Result.Trim())}")
                else Ok exeAbs
    with ex -> Error ($"codegen raised: {ex.Message}")

/// Run a compiled program with OMP_NUM_THREADS forced, returning stdout.
let private runProgram (exeAbs: string) (threads: string) : Result<string, string> =
    let rpsi = ProcessStartInfo(exeAbs)
    rpsi.RedirectStandardOutput <- true
    rpsi.RedirectStandardError <- true
    rpsi.UseShellExecute <- false
    rpsi.WorkingDirectory <- Path.GetDirectoryName(exeAbs)
    rpsi.Environment.["OMP_NUM_THREADS"] <- threads
    use rproc = Process.Start(rpsi)
    let rout = Blade.Runtime.readToEndOffPool rproc.StandardOutput
    let rerr = Blade.Runtime.readToEndOffPool rproc.StandardError
    if not (rproc.WaitForExit(60000)) then
        (try rproc.Kill(true) with _ -> ())
        Error "run timed out (60s)"
    elif rproc.ExitCode <> 0 then
        Error ($"exit {rproc.ExitCode}: {(rerr.Result.Trim())}")
    else Ok rout.Result

/// Every `name = value` scalar line of a program's stdout, as floats. The
/// auto-printer emits one per top-level binding, which is what the differential
/// compares.
let private scalarBindings (stdout: string) : Map<string, float> =
    stdout.Split('\n')
    |> Array.choose (fun line ->
        let m = System.Text.RegularExpressions.Regex.Match(
                    line.Trim(), @"^([A-Za-z_][A-Za-z0-9_]*) = (-?[0-9.eE+-]+)$")
        if m.Success then
            match System.Double.TryParse(m.Groups.[2].Value,
                                         System.Globalization.NumberStyles.Float,
                                         System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some (m.Groups.[1].Value, v)
            | _ -> None
        else None)
    |> Map.ofArray

/// Comm-licensed parallel reduction VALUE tests. Requires g++; skips otherwise.
let runOmpReduceTests () : Blade.Tests.TestHarness.BlockResult =
    let caps = capabilities.Value
    printHeader "OpenMP Comm-Licensed Reductions"
    // Pinned unset for the block: every differential below compares an
    // `omp`-licensed build against its serial twin, and a global
    // BLADE_OMP_THREADS=1 would make both sides the SAME serial program --
    // every arm would pass while measuring nothing. The knob's own arm at the
    // end re-pins it for its own scope.
    use _ompThreadsPin = pinOmpThreadsUnset ()
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile the -fopenmp reduction programs)."
        { Block = "OpenMP Reduce"; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    else
        let outputDir = "./generated_omp_reduce"
        Directory.CreateDirectory(outputDir) |> ignore
        CodeGen.deployRuntimeHeaders outputDir
        let forcedThreads = "4"
        let mutable passed = 0
        let mutable failed = 0
        let mutable failedNames = []
        let fail name detail =
            failed <- failed + 1
            failedNames <- failedNames @ [name]
            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name detail
        let pass name detail =
            passed <- passed + 1
            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass name detail

        // Awkward, NON-integer data: reassociation is only observable when the
        // partial sums are not exact. 240 elements so a 4-thread split gives
        // four genuinely different chunks.
        let n = 240
        let vals = [ for i in 1 .. n -> 1.0 / float i + float (i % 7) * 0.3 ]
        let aLit = vals |> List.map (_.ToString("R", System.Globalization.CultureInfo.InvariantCulture)) |> String.concat ", "
        let arrDecl = $"let A = [{aLit}]\n"
        // A second array for the deferred-computation (2-level nest) case; kept
        // short so the fused nest stays a quick compile.
        let bVals = [ for i in 1 .. 9 -> 0.5 + 1.0 / float (i + 2) ]
        let bLit = bVals |> List.map (_.ToString("R", System.Globalization.CultureInfo.InvariantCulture)) |> String.concat ", "
        let bDecl = $"let B = [{bLit}]\n"
        let commFn = "function myAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n"
        let serialFn = "function myAdd(a: Float64, b: Float64) where comm(a, b) = (a + b) * 1.0\n"
        // Compound (masked) operand: reduce walks the flat `.data` buffer of
        // present cells, which is the OTHER contiguous 1-D sweep Path A covers.
        let maskDecl = "let m = mask(A, lambda(x) -> x > 1.0)\nlet C = compound(A, m)\n"

        // (name, ompSource, serialSource, pathB?)
        let cases : (string * string * string * bool) list =
            [ ("path_a_builtin_sum_dense",
               arrDecl + "let s = reduce(A, lambda(a, b) where omp -> a + b)\n",
               arrDecl + "let s = reduce(A, lambda(a, b) -> a + b)\n",
               false)
              ("path_a_builtin_sum_compound",
               arrDecl + maskDecl + "let s = reduce(C, lambda(a, b) where omp -> a + b)\n",
               arrDecl + maskDecl + "let s = reduce(C, lambda(a, b) -> a + b)\n",
               false)
              ("path_b_named_comm_function",
               commFn + arrDecl + "let s = reduce(A, myAdd)\n",
               serialFn + arrDecl + "let s = reduce(A, myAdd)\n",
               true)
              ("path_b_named_comm_with_init",
               commFn + arrDecl + "let s = reduce(A, myAdd, 100.0)\n",
               serialFn + arrDecl + "let s = reduce(A, myAdd, 100.0)\n",
               true)
              ("path_b_inline_comm_lambda",
               arrDecl + "let s = reduce(A, lambda(a, b) where comm(a, b), omp -> (a + b) * 1.0)\n",
               arrDecl + "let s = reduce(A, lambda(a, b) where comm(a, b) -> (a + b) * 1.0)\n",
               true)
              // `axes = 2`: the full fold to the scalar `s` this case compares.
              // The default (innermost axis only) would bind `s` to a rank-1
              // array and the comparison would have nothing to read.
              ("path_b_reduce_over_computation",
               commFn + bDecl + "let s = reduce(method_for(B, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n",
               serialFn + bDecl + "let s = reduce(method_for(B, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n",
               true) ]

        for (name, ompSrc, serialSrc, isPathB) in cases do
            match compileProgram outputDir (name + "_omp") ompSrc,
                  compileProgram outputDir (name + "_serial") serialSrc with
            | Error e, _ -> fail name ($"omp build: {e}")
            | _, Error e -> fail name ($"serial build: {e}")
            | Ok ompExe, Ok serialExe ->
                match runProgram ompExe forcedThreads, runProgram serialExe "1" with
                | Error e, _ -> fail name ($"omp run: {e}")
                | _, Error e -> fail name ($"serial run: {e}")
                | Ok ompOut, Ok serialOut ->
                    let ompVals = scalarBindings ompOut
                    let serVals = scalarBindings serialOut
                    // `s` is the fold's binding in every case above. Its absence
                    // would make the comparison vacuous, so it is checked.
                    match Map.tryFind "s" ompVals, Map.tryFind "s" serVals with
                    | Some pv, Some sv ->
                        let diff = abs (pv - sv)
                        let tol = 1e-9 * max 1.0 (abs sv)
                        if diff > tol then
                            fail name (sprintf "parallel %.17g vs serial %.17g (|diff| = %g)" pv sv diff)
                        elif not isPathB then
                            pass name (sprintf "value matches serial (|diff| = %g)" diff)
                        else
                            // Path B determinism: a second run at the same team
                            // size must reproduce the first byte for byte.
                            match runProgram ompExe forcedThreads with
                            | Error e -> fail name ($"second omp run: {e}")
                            | Ok again ->
                                let strip (s: string) =
                                    s.Split('\n')
                                    |> Array.filter (fun l -> not (l.Contains "completed in"))
                                    |> String.concat "\n"
                                if strip again <> strip ompOut then
                                    fail name "run-to-run output differs at fixed OMP_NUM_THREADS=4 (chunking is not deterministic)"
                                else
                                    pass name (sprintf "value matches serial (|diff| = %g); identical across 2 runs" diff)
                    | _ ->
                        fail name "no scalar binding 's' in program output (comparison would be vacuous)"

        // ---- Round C: lane-boundary battery -------------------------------
        // Path B's chunk is now swept by K strided lane accumulators (K fixed
        // at codegen; CodeGen.foldLaneCount). Every arm of that sweep has an
        // element-count boundary, and getting any of them wrong silently drops
        // or double-counts elements — the fold still prints A number, just the
        // wrong one. So the battery walks n across all of them:
        //
        //   n < K            -> the short-chunk arm (no lanes at all)
        //   n = K            -> lanes seeded, main loop and tail both empty
        //   n = K + 1        -> one tail element, landing on lane 0
        //   n = 2K - 1       -> maximal tail (K-1 elements, lanes 0..K-2)
        //   n not div by K   -> main loop plus a partial tail
        //   n < thread count -> chunks smaller than K under a 4-thread request
        //
        // K is deliberately NOT read from the compiler here: the battery is
        // written to straddle any K up to 16, so it keeps its meaning if the
        // constant is retuned. Values are non-integer (reassociation is real)
        // and the oracle is the same program with `omp` dropped, at 1e-9.
        //
        // An INTEGER-data twin runs at n = 2K-1 and demands EXACT equality —
        // that is the property the corpus files (loops/110, loops/111) and the
        // interpreter differential rely on, so it is pinned here directly
        // rather than inferred.
        let laneKernelOmp = "function laneAdd(a: Float64, b: Float64) where comm(a, b), omp = (a + b) * 1.0\n"
        let laneKernelSer = "function laneAdd(a: Float64, b: Float64) where comm(a, b) = (a + b) * 1.0\n"
        let laneLit (count: int) (f: int -> float) =
            // A decimal point is forced: an integer-valued literal printed by
            // "R" comes out as `1`, which the checker reads as Int64 and then
            // refuses against the Float64 kernel.
            [ for i in 1 .. count ->
                let s = (f i).ToString("R", System.Globalization.CultureInfo.InvariantCulture)
                if s.Contains "." || s.Contains "E" then s else s + ".0" ]
            |> String.concat ", "
        let awkward i = 1.0 / float i + float (i % 5) * 0.7
        let laneCases : (string * int * bool) list =
            // (label, n, integerData)
            [ ("n1_below_lanes",        1,  false)
              ("n3_below_lanes",        3,  false)
              ("n4_at_lanes",           4,  false)
              ("n5_lanes_plus_one",     5,  false)
              ("n7_maximal_tail",       7,  false)
              ("n8_two_full_lanes",     8,  false)
              ("n15_maximal_tail_k8",  15,  false)
              ("n17_indivisible",      17,  false)
              ("n3_below_thread_count", 3,  false)
              ("n33_indivisible",      33,  false)
              ("n15_integer_exact",    15,  true) ]
        for (label, n, integerData) in laneCases do
            let name = "lane_boundary_" + label
            let lit = laneLit n (fun i -> if integerData then float (i % 9 + 1) else awkward i)
            let decl = $"let A = [{lit}]\n"
            let body = "let s = reduce(A, laneAdd)\n"
            match compileProgram outputDir (name + "_omp") (laneKernelOmp + decl + body),
                  compileProgram outputDir (name + "_serial") (laneKernelSer + decl + body) with
            | Error e, _ -> fail name ($"omp build: {e}")
            | _, Error e -> fail name ($"serial build: {e}")
            | Ok ompExe, Ok serialExe ->
                match runProgram ompExe forcedThreads, runProgram serialExe "1" with
                | Error e, _ -> fail name ($"omp run: {e}")
                | _, Error e -> fail name ($"serial run: {e}")
                | Ok ompOut, Ok serialOut ->
                    match Map.tryFind "s" (scalarBindings ompOut), Map.tryFind "s" (scalarBindings serialOut) with
                    | Some pv, Some sv ->
                        let diff = abs (pv - sv)
                        let tol = if integerData then 0.0 else 1e-9 * max 1.0 (abs sv)
                        if diff > tol then
                            fail name (sprintf "n=%d: parallel %.17g vs serial %.17g (|diff| = %g, tol = %g)" n pv sv diff tol)
                        else
                            // Determinism re-check: the lane count is a compile-time
                            // constant, so a fixed team size must still reproduce the
                            // run bit for bit.
                            match runProgram ompExe forcedThreads with
                            | Error e -> fail name ($"second omp run: {e}")
                            | Ok again ->
                                let strip (s: string) =
                                    s.Split('\n')
                                    |> Array.filter (fun l -> not (l.Contains "completed in"))
                                    |> String.concat "\n"
                                if strip again <> strip ompOut then
                                    fail name ($"n={n}: run-to-run output differs at fixed OMP_NUM_THREADS=4")
                                else
                                    pass name (sprintf "n=%d %s: |diff| = %g; identical across 2 runs"
                                                   n (if integerData then "exact" else "vs serial") diff)
                    | _ -> fail name "no scalar binding 's' in program output (comparison would be vacuous)"

        // ---- BLADE_FP_REASSOC: the reassociation opt-in -------------------
        // The knob licenses the emitter to reassociate a SERIAL floating-point
        // accumulation chain -- `prodsum`'s fiber sweep, a reduce over a
        // deferred computation, and an UNQUALIFIED builtin `reduce` (one the
        // user never marked `omp`).
        //
        // TWO EMITTED FORMS, chosen per site by measurement (CodeGen.fs
        // `fpReassocSimdStmts` / `fpReassocLaneStmts`, each of which carries the
        // numbers that chose it):
        //
        //   omp simd   `#pragma omp simd reduction(<op>:acc)` over the plain
        //              serial loop. Emitted by the `prodsum` IIFE and by the
        //              reduce-over-computation nest. Needs a builtin `+`/`*`
        //              body (a reduction clause names an OPERATOR) and a scalar
        //              element type.
        //   K lanes    K independent named accumulators combined in fixed
        //              ascending order, no pragma at all. Emitted by `reduce`
        //              over a MATERIALIZED array (both its statement and its
        //              expression form), and everywhere the simd form is not
        //              available -- above all for a `comm`-declared kernel,
        //              whose combine is a CALL.
        //
        // Everything above this point, and every other suite, runs with the
        // knob at its DEFAULT (off), which is what makes "off means
        // byte-identical" checkable at all. The pin below is per-COMPILE and
        // restored immediately, so nothing outside these arms can see it. (Test
        // blocks run sequentially in one process -- see Cli.fs -- so a
        // process-global env pin is safe here, the same way InterpDiff pins
        // BLADE_FP_CONTRACT for its block.)
        //
        // Three properties are asserted, and they are not the same property:
        //
        //   1. VALUE. Knob-on agrees with knob-off to 1e-9 relative on awkward
        //      non-representable data, and EXACTLY on integer-valued data.
        //      Integer exactness holds under ANY summation order -- including
        //      the vectorizer's, which is why it survives the simd form
        //      unchanged -- and it is the property loops/110-111 and the
        //      interpreter differential depend on.
        //   2. DETERMINISM, in the form the contract at `fpReassocEnabled` now
        //      states it: for a FIXED BINARY the answer is identical across two
        //      runs and across OMP_NUM_THREADS values. NEITHER form creates a
        //      team (`omp simd` is a SIMD construct, and the lane form has no
        //      pragma at all), so thread count cannot enter the answer. What is
        //      deliberately NOT asserted any more is reproducibility across
        //      compilers or flags: under the simd form the summation order is
        //      the vectorizer's, and the project policy is that optimized
        //      Release builds are not bit-reproducible across builds. (The lane
        //      arms do still have the stronger property -- a fixed function of
        //      the data and K -- but it is not separately pinned here, because
        //      one binary cannot observe it.)
        //   3. LICENCE + LIVENESS, at the emission level. Positive controls
        //      prove each site emits the FORM it is supposed to (without them
        //      the value arms could pass vacuously by never firing, and a site
        //      could silently swap forms), and a negative control proves an
        //      UNLICENSED user kernel -- no builtin body, no `comm` -- is
        //      emitted byte-identically with the knob on.
        //
        // n walks every boundary of the strided sweep: below the lanes (1, 3),
        // maximal tail (7), exactly the lanes (8), one past (9), maximal tail
        // above one full stride (15), indivisible (17). Getting any of them
        // wrong drops or double-counts elements silently -- the fold still
        // prints A number. The boundaries still matter under the simd form: the
        // vectorizer's own scalar remainder is exercised by exactly the same
        // awkward trip counts.
        let withReassoc (on: bool) (f: unit -> 'a) : 'a =
            let prior = System.Environment.GetEnvironmentVariable("BLADE_FP_REASSOC")
            System.Environment.SetEnvironmentVariable("BLADE_FP_REASSOC", (if on then "1" else "0"))
            try f ()
            finally System.Environment.SetEnvironmentVariable("BLADE_FP_REASSOC", prior)
        // The dot-shaped arms below (`reduce(<unforced zip>, (+))`) are EXACTLY
        // the shape `LinAlgPatterns.BlasL1` recognises, so in an environment
        // where BLAS is enabled (OPENBLAS_DIR set, or `blade test linalg`'s
        // BLADE_BLAS=1) they would emit one `blade_linalg::blade_dot` call and
        // never reach the fold nest at all -- the lane assertions would pass
        // vacuously and the value arms would compare two identical BLAS calls.
        // Pinned OFF for the whole reassoc block (no arm here wants a dispatch)
        // and restored immediately, same discipline as the knob pin above.
        let withBlasOff (f: unit -> 'a) : 'a =
            let prior = System.Environment.GetEnvironmentVariable("BLADE_BLAS")
            System.Environment.SetEnvironmentVariable("BLADE_BLAS", "0")
            try f ()
            finally System.Environment.SetEnvironmentVariable("BLADE_BLAS", prior)
        let emitOnly (nm: string) (src: string) : Result<string, string> =
            try
                match lower src with
                | Error e -> Error ($"lower failed: {e}")
                | Ok ir0 ->
                    match IRValidate.validateIR ir0 with
                    | Error errs -> Error (String.concat "; " errs)
                    | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir nm))
            with ex -> Error ($"codegen raised: {ex.Message}")
        let stripTiming (s: string) =
            s.Split('\n') |> Array.filter (fun l -> not (l.Contains "completed in")) |> String.concat "\n"

        // (label, n, integerData, sourceOf n)
        let fprSrcReduce (lit: string) =
            $"let A = [{lit}]\nlet s = reduce(A, lambda(a, b) -> a + b)\n"
        let fprSrcProdsum (lit: string) (lit2: string) =
            $"let A = [{lit}]\nlet B = [{lit2}]\nlet s = prodsum(A, B)\n"
        // THREE operand streams: the comoment3 fiber kernel's shape, and the
        // one the arity-aware lane count (`laneCountForStreams`) puts at K = 5.
        let fprSrcProdsum3 (l1: string) (l2: string) (l3: string) =
            $"let A = [{l1}]\nlet B = [{l2}]\nlet C = [{l3}]\nlet s = prodsum(A, B, C)\n"
        // reduce over an UNFORCED zip -- the dot shape. This lowers through
        // `genReduceComputeBinding` (reduce over a deferred computation), a
        // different emitter from the two above: its element is not a subscript
        // but the nest's whole per-iteration body evaluated at the lane index.
        let fprSrcDot (lit: string) (lit2: string) =
            $"let x = [{lit}]\nlet y = [{lit2}]\n"
              + "let P = method_for(zip(x, y)) <@> lambda(a: Float64, b: Float64) -> a * b\n"
              + "let s = reduce(P, (+))\n"
        // A `comm`-DECLARED kernel whose body is NOT a bare builtin op. This is
        // the licence class the simd form cannot spell -- no operator to name in
        // a reduction clause -- so it is what still gets the K-lane form, at
        // every site. `+ 0.0` is what keeps `foldKernelBuiltinOp` from
        // recognising the body while leaving the arithmetic (and hence the
        // expected value) identical to a plain sum.
        let fprCommKernel =
            "function myK(a: Float64, b: Float64) where comm(a, b) = a + b + 0.0\n"
        let fprSrcCommReduce (lit: string) =
            fprCommKernel + $"let A = [{lit}]\nlet s = reduce(A, myK)\n"
        // comm kernel over a deferred computation: the reduce-over-computation
        // site's lane fallback. The 3-arg form is required there (a fused fold
        // cannot seed from its first element).
        let fprSrcCommDot (lit: string) (lit2: string) =
            fprCommKernel + $"let x = [{lit}]\nlet y = [{lit2}]\n"
              + "let P = method_for(zip(x, y)) <@> lambda(a: Float64, b: Float64) -> a * b\n"
              + "let s = reduce(P, myK, 0.0)\n"
        let fprSrcCommDot3 (l1: string) (l2: string) (l3: string) =
            fprCommKernel + $"let x = [{l1}]\nlet y = [{l2}]\nlet z = [{l3}]\n"
              + "let P = method_for(zip(x, y, z)) <@> lambda(a: Float64, b: Float64, c: Float64) -> a * b * c\n"
              + "let s = reduce(P, myK, 0.0)\n"
        let fprNs = [1; 3; 7; 8; 9; 15; 17]
        // K = 5 for three streams, so its boundaries are a DIFFERENT set of n:
        // below the lanes (1, 3), exactly the lanes (5), maximal tail (4, 9),
        // one past (6), indivisible (11, 17).
        let fprNs3 = [1; 3; 4; 5; 6; 9; 11; 17]
        let fprCases : (string * string) list =
            [ for n in fprNs do
                yield ($"reduce_n{n}", fprSrcReduce (laneLit n awkward))
                yield ($"prodsum_n{n}",
                       fprSrcProdsum (laneLit n awkward) (laneLit n (fun i -> awkward (i + 3))))
                yield ($"dot_n{n}",
                       fprSrcDot (laneLit n awkward) (laneLit n (fun i -> awkward (i + 3))))
              for n in fprNs3 do
                yield ($"prodsum3_n{n}",
                       fprSrcProdsum3 (laneLit n awkward)
                                      (laneLit n (fun i -> awkward (i + 3)))
                                      (laneLit n (fun i -> awkward (i + 7))))
              // Integer-valued twins: EXACT equality is demanded of these.
              yield ("reduce_n15_integer_exact", fprSrcReduce (laneLit 15 (fun i -> float (i % 9 + 1))))
              yield ("prodsum_n15_integer_exact",
                     fprSrcProdsum (laneLit 15 (fun i -> float (i % 9 + 1)))
                                   (laneLit 15 (fun i -> float (i % 5 + 2))))
              yield ("prodsum3_n11_integer_exact",
                     fprSrcProdsum3 (laneLit 11 (fun i -> float (i % 9 + 1)))
                                    (laneLit 11 (fun i -> float (i % 5 + 2)))
                                    (laneLit 11 (fun i -> float (i % 3 + 1))))
              yield ("dot_n15_integer_exact",
                     fprSrcDot (laneLit 15 (fun i -> float (i % 9 + 1)))
                               (laneLit 15 (fun i -> float (i % 5 + 2))))
              // The K-LANE form's own value arms. The four cases above all
              // reach a site that now emits `omp simd`, so without these the
              // lane emitter -- still live for every `comm`-declared kernel --
              // would have no value coverage at all here.
              for n in fprNs do
                yield ($"comm_reduce_n{n}", fprSrcCommReduce (laneLit n awkward))
                yield ($"comm_dot_n{n}",
                       fprSrcCommDot (laneLit n awkward) (laneLit n (fun i -> awkward (i + 3))))
              yield ("comm_reduce_n15_integer_exact",
                     fprSrcCommReduce (laneLit 15 (fun i -> float (i % 9 + 1))))
              yield ("comm_dot_n15_integer_exact",
                     fprSrcCommDot (laneLit 15 (fun i -> float (i % 9 + 1)))
                                   (laneLit 15 (fun i -> float (i % 5 + 2)))) ]
        for (label, src) in fprCases do
            let name = "fp_reassoc_" + label
            let exact = label.EndsWith "integer_exact"
            match withReassoc true (fun () -> withBlasOff (fun () -> compileProgram outputDir (name + "_on") src)),
                  withReassoc false (fun () -> withBlasOff (fun () -> compileProgram outputDir (name + "_off") src)) with
            | Error e, _ -> fail name ($"reassoc-on build: {e}")
            | _, Error e -> fail name ($"reassoc-off build: {e}")
            | Ok onExe, Ok offExe ->
                match runProgram onExe "1", runProgram offExe "1" with
                | Error e, _ -> fail name ($"reassoc-on run: {e}")
                | _, Error e -> fail name ($"reassoc-off run: {e}")
                | Ok onOut, Ok offOut ->
                    match Map.tryFind "s" (scalarBindings onOut), Map.tryFind "s" (scalarBindings offOut) with
                    | Some ov, Some fv ->
                        let diff = abs (ov - fv)
                        let tol = if exact then 0.0 else 1e-9 * max 1.0 (abs fv)
                        if diff > tol then
                            fail name (sprintf "reassoc %.17g vs serial %.17g (|diff| = %g, tol = %g)" ov fv diff tol)
                        else
                            // Thread-count independence AND run-to-run identity,
                            // on the SAME BINARY -- the whole of what the knob
                            // now promises. Neither form takes anything from a
                            // team (`omp simd` is a SIMD construct and creates
                            // none; the lane form has no pragma at all), so both
                            // must hold outright. Note what is NOT compared: two
                            // DIFFERENTLY BUILT binaries, which the contract at
                            // `fpReassocEnabled` no longer claims agree.
                            match runProgram onExe "4", runProgram onExe "1" with
                            | Error e, _ -> fail name ($"reassoc-on run at 4 threads: {e}")
                            | _, Error e -> fail name ($"reassoc-on second run: {e}")
                            | Ok at4, Ok again ->
                                if stripTiming at4 <> stripTiming onOut then
                                    fail name "output differs between OMP_NUM_THREADS=1 and 4 (a reassociated form took something from a team)"
                                elif stripTiming again <> stripTiming onOut then
                                    fail name "output differs across two runs of the same binary at a fixed thread count"
                                else
                                    pass name (sprintf "%s: |diff| = %g; identical at 1 vs 4 threads and across 2 runs"
                                                   (if exact then "exact" else "vs serial") diff)
                    | _ -> fail name "no scalar binding 's' in program output (comparison would be vacuous)"

        // Emission controls. Without these the value arms could all pass by the
        // knob never firing at all -- AND, now that there are two forms, a site
        // could silently swap one for the other and every value arm would still
        // pass. Each case therefore pins WHICH form its site emits, which is a
        // per-site MEASUREMENT (see the comment at each `fpReassocEnabled ()`
        // site in CodeGen.fs) and not an arbitrary choice: a regression that
        // flips one of these is a performance regression the values cannot see.
        let fprEmitCases : (string * string * string) list =
            // (label, source, expected form: "simd" | "lanes" | "unchanged")
            [ // reduce over a MATERIALIZED array -> lanes (measured 1.12x
              // bandwidth-bound / 1.90x cache-resident over the simd form).
              ("builtin_reduce_lanes",
               fprSrcReduce (laneLit 17 awkward), "lanes")
              // prodsum's fiber IIFE -> simd (2.60x on the gemv fiber and 3.36x
              // at three streams, where the lanes gave 0.96x and 1.80x).
              ("prodsum_simd",
               fprSrcProdsum (laneLit 17 awkward) (laneLit 17 (fun i -> awkward (i + 3))), "simd")
              ("prodsum3_simd",
               fprSrcProdsum3 (laneLit 17 awkward)
                              (laneLit 17 (fun i -> awkward (i + 3)))
                              (laneLit 17 (fun i -> awkward (i + 7))), "simd")
              // reduce-over-deferred-computation (the dot shape) -> simd (1.64x
              // bandwidth-bound / 3.81x cache-resident; the lanes were at
              // PARITY with the serial chain in both regimes).
              ("dot_reduce_over_computation_simd",
               fprSrcDot (laneLit 17 awkward) (laneLit 17 (fun i -> awkward (i + 3))), "simd")
              // A `comm`-declared kernel is licensed but its combine is a CALL,
              // so no reduction clause can name it: BOTH sites fall back to the
              // lanes. This is the arm that keeps `fpReassocLaneStmts` live at
              // the reduce-over-computation site.
              ("comm_reduce_lanes",
               fprSrcCommReduce (laneLit 17 awkward), "lanes")
              ("comm_dot_over_computation_lanes",
               fprSrcCommDot (laneLit 17 awkward) (laneLit 17 (fun i -> awkward (i + 3))), "lanes")
              // Unlicensed: body is not a bare builtin op and no `comm` is
              // declared, so the knob grants nothing. `where omp` is absent too,
              // so this is the serial arm either way.
              ("unlicensed_kernel_stays_serial",
               "function myK(a: Float64, b: Float64) = (a + b) * 1.0000001\n"
                 + $"let A = [{(laneLit 17 awkward)}]\n"
                 + "let s = reduce(A, myK)\n", "unchanged") ]
        for (label, src, expectForm) in fprEmitCases do
            let name = "fp_reassoc_emission_" + label
            match withReassoc true (fun () -> withBlasOff (fun () -> emitOnly name src)),
                  withReassoc false (fun () -> withBlasOff (fun () -> emitOnly name src)) with
            | Error e, _ -> fail name ($"emit (knob on): {e}")
            | _, Error e -> fail name ($"emit (knob off): {e}")
            | Ok onCpp, Ok offCpp ->
                let hasLanes (s: string) = s.Contains "__rlane" || s.Contains "__pl0"
                // The macro, not a raw `#pragma`: cpp/blade_portability.hpp owns
                // the spelling (and its OpenMP-4.0 gate), and codegen emits only
                // the macro -- so a raw pragma appearing here would itself be
                // the regression.
                let hasSimd (s: string) = s.Contains "BLADE_OMP_SIMD_REDUCTION"
                if hasLanes offCpp || hasSimd offCpp then
                    fail name "knob OFF emitted a reassociated form (the default must never reassociate)"
                else
                    match expectForm with
                    | "lanes" when not (hasLanes onCpp) ->
                        fail name "knob ON emitted no lane accumulators (the gate never fired, or the site swapped to simd)"
                    | "lanes" when hasSimd onCpp ->
                        fail name "knob ON emitted BOTH forms at a lane site"
                    | "simd" when not (hasSimd onCpp) ->
                        fail name "knob ON emitted no omp simd reduction (the gate never fired, or the site swapped to lanes)"
                    | "simd" when hasLanes onCpp ->
                        fail name "knob ON emitted lane accumulators at a simd site (the measured-worse form)"
                    | "unchanged" when onCpp <> offCpp ->
                        fail name "unlicensed kernel: knob ON changed the emission (the knob is not a licence)"
                    | "lanes" -> pass name "K-lane form, only with the knob on"
                    | "simd" -> pass name "omp simd reduction form, only with the knob on"
                    | "unchanged" -> pass name "byte-identical with the knob on (unlicensed, stays serial)"
                    | other -> fail name ($"test bug: unknown expected form {other}")

        // ---- The lane COUNT is a function of the operand-stream count -------
        // `laneCountForStreams` divides a fixed register/ILP budget among the
        // concurrent value streams one lane iteration keeps live: K = 8 at one
        // and two streams (the repo's measured anchor), K = floor(16/s) beyond.
        // The count is part of the lane form's EVALUATION ORDER, so it is
        // pinned here as emitted text, not left to be inferred from a timing.
        //
        // THE SOURCES CHANGED WITH THE FORMS, THE RULE DID NOT. `prodsum` used
        // to be where the arity-aware count was observable; it now emits `omp
        // simd`, where the partial count is the vectorizer's and there is
        // nothing to pin. The rule is still live -- and still observable --
        // wherever the LANE form is what gets emitted, which is any
        // `comm`-declared kernel, so the multi-stream arms move to a comm fold
        // over a deferred computation of the same arity.
        //
        // The three-stream arm is the one with a measurement behind it: 8 lanes
        // on a three-stream body ran 2.2-2.7x SLOWER than 5 lanes in the
        // comoment3 shape (61 vars x 2003 samples), while 8 lanes on the
        // two-stream form helped. A regression that silently restores 8 here
        // restores that.
        let distinctLanes (prefix: string) (s: string) =
            System.Text.RegularExpressions.Regex.Matches(s, prefix + @"(\d+)")
            |> Seq.cast<System.Text.RegularExpressions.Match>
            |> Seq.map (fun m -> int m.Groups.[1].Value)
            |> Seq.distinct |> Seq.length
        let fprLaneCountCases : (string * string * string * int) list =
            // (label, source, lane-local prefix, expected distinct lanes)
            [ ("reduce_one_stream", fprSrcReduce (laneLit 33 awkward), "__rlane", 8)
              ("comm_reduce_one_stream", fprSrcCommReduce (laneLit 33 awkward), "__rlane", 8)
              ("comm_dot_two_streams",
               fprSrcCommDot (laneLit 33 awkward) (laneLit 33 (fun i -> awkward (i + 3))), "__rlane", 8)
              ("comm_dot3_three_streams",
               fprSrcCommDot3 (laneLit 33 awkward)
                              (laneLit 33 (fun i -> awkward (i + 3)))
                              (laneLit 33 (fun i -> awkward (i + 7))), "__rlane", 5) ]
        for (label, src, prefix, expected) in fprLaneCountCases do
            let name = "fp_reassoc_lane_count_" + label
            match withReassoc true (fun () -> withBlasOff (fun () -> emitOnly name src)) with
            | Error e -> fail name ($"emit (knob on): {e}")
            | Ok cpp ->
                let got = distinctLanes prefix cpp
                if got <> expected then
                    fail name ($"expected {expected} lanes ({prefix}0..{prefix}{(expected - 1)}), emitted {got}")
                else pass name ($"{got} lanes")

        // ---- Phase 1 regression: ivdep must not land inside collapse(2) ----
        // Text-only assertions cannot see this; only g++ can.
        let collapseSrc =
            "function k(a: Float64, b: Float64) where omp(a: 1, b: 1) = a * b + 1.0\n" +
            "let P = [1.5, 2.5, 3.5, 4.5, 5.5]\n" +
            "let Q = [0.25, 0.5, 0.75, 1.25]\n" +
            "let M = object_for(k) <@> (P, Q) |> compute\n"
        let collapseName = "collapse2_dense_map_compiles"
        match compileProgram outputDir collapseName collapseSrc with
        | Error e -> fail collapseName e
        | Ok exeAbs ->
            match runProgram exeAbs forcedThreads with
            | Error e -> fail collapseName ($"run: {e}")
            | Ok out ->
                // 5x4 outer product through the kernel. A rank-2 array prints
                // NESTED — `M = [[a, b, ...], [...], ...]` — so the old flat
                // `\[([^\]]*)\]` parse stopped at the first inner `]` and read
                // one row, not the array. Match the whole bracketed value and
                // then read the ROWS, which is also a STRONGER assertion than a
                // flatten-and-parse would be: it pins the 5x4 shape, not just
                // the 20 values in DFS order.
                let expectedRows =
                    [ for p in [1.5; 2.5; 3.5; 4.5; 5.5] ->
                        [ for q in [0.25; 0.5; 0.75; 1.25] -> p * q + 1.0 ] ]
                let parseNum (s: string) =
                    match System.Double.TryParse(s.Trim(),
                                                 System.Globalization.NumberStyles.Float,
                                                 System.Globalization.CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | _ -> None
                let rows =
                    let m = System.Text.RegularExpressions.Regex.Match(out, @"M = (\[\[.*\]\])")
                    if not m.Success then []
                    else
                        System.Text.RegularExpressions.Regex.Matches(m.Groups.[1].Value, @"\[([^\[\]]*)\]")
                        |> Seq.cast<System.Text.RegularExpressions.Match>
                        |> Seq.map (fun r -> r.Groups.[1].Value.Split(',') |> Array.toList |> List.choose parseNum)
                        |> Seq.toList
                if List.length rows <> List.length expectedRows
                   || List.exists2 (fun (a: float list) (b: float list) -> a.Length <> b.Length) rows expectedRows then
                    fail collapseName
                        (sprintf "expected a %dx%d nested print, parsed rows of lengths %A"
                             (List.length expectedRows) 4 (rows |> List.map List.length))
                elif List.exists2 (fun (ra: float list) rb ->
                                       List.exists2 (fun (a: float) b -> abs (a - b) > 1e-9) ra rb)
                                  rows expectedRows then
                    fail collapseName "collapse(2) map produced wrong values under 4 threads"
                else
                    pass collapseName "compiles under g++ and computes correctly (ivdep/collapse interaction)"

        // ---- BLADE_OMP_THREADS: same numbers, different threading ----------
        // The knob is a THREADING decision, never a numeric one: what it
        // changes is whether a team is created, and the emitted arithmetic on
        // each side is one the suite already accepts (the licensed omp form, or
        // the serial form its own differentials compare against). So a
        // knob-on/knob-off pair must agree to the same tolerance an
        // omp-vs-serial pair does -- and this arm is the one that COMPILES AND
        // RUNS both, which no text assertion in the omp-pragma block can do.
        //
        // Non-integer data, deliberately: on integer-valued f64 every
        // summation order is exact and the comparison could not tell a genuine
        // agreement from an arithmetic that got lost. The Path A arm is the
        // sharp one -- suppressed Path A keeps `omp simd reduction`, so both
        // sides reassociate, just at different widths.
        let withOmpThreads (v: string) (f: unit -> 'a) : 'a =
            let prior = System.Environment.GetEnvironmentVariable("BLADE_OMP_THREADS")
            System.Environment.SetEnvironmentVariable("BLADE_OMP_THREADS", v)
            try f ()
            finally System.Environment.SetEnvironmentVariable("BLADE_OMP_THREADS", prior)
        let knobN = 33
        let knobLit = laneLit knobN awkward
        let knobDecl = $"let A = [{knobLit}]\n"
        let knobCases : (string * string) list =
            // (label, source) -- one per suppression shape the knob has.
            [ ("path_a_builtin_sum",
               knobDecl + "let s = reduce(A, lambda(a, b) where omp -> a + b)\n")
              ("path_b_named_comm",
               commFn + knobDecl + "let s = reduce(A, myAdd)\n")
              // `axes = 2` for the same reason as the differential case above.
              ("path_b_reduce_over_computation",
               commFn + bDecl + "let s = reduce(method_for(B, B) <@> lambda(x, y) -> x * y, myAdd, 0.0, axes = 2)\n") ]
        for (label, src) in knobCases do
            let name = "omp_threads_knob_" + label
            match withOmpThreads "1" (fun () -> compileProgram outputDir (name + "_serial") src),
                  withOmpThreads null (fun () -> compileProgram outputDir (name + "_threaded") src) with
            | Error e, _ -> fail name ($"knob-on build: {e}")
            | _, Error e -> fail name ($"knob-off build: {e}")
            | Ok serialExe, Ok threadedExe ->
                // The knob-on binary is run at OMP_NUM_THREADS=4 ON PURPOSE:
                // it has no parallel construct left, so the runtime setting
                // must be inert. If it is not, a team survived somewhere.
                match runProgram serialExe forcedThreads, runProgram threadedExe forcedThreads with
                | Error e, _ -> fail name ($"knob-on run: {e}")
                | _, Error e -> fail name ($"knob-off run: {e}")
                | Ok serialOut, Ok threadedOut ->
                    match Map.tryFind "s" (scalarBindings serialOut),
                          Map.tryFind "s" (scalarBindings threadedOut) with
                    | Some sv, Some tv ->
                        let diff = abs (sv - tv)
                        let tol = 1e-9 * max 1.0 (abs tv)
                        if diff > tol then
                            fail name (sprintf "knob-on %.17g vs knob-off %.17g (|diff| = %g)" sv tv diff)
                        else
                            match runProgram serialExe "1" with
                            | Error e -> fail name ($"knob-on run at 1 thread: {e}")
                            | Ok at1 ->
                                if stripTiming at1 <> stripTiming serialOut then
                                    fail name "knob-on output depends on OMP_NUM_THREADS (a thread construct survived suppression)"
                                else
                                    pass name (sprintf "|diff| = %g; knob-on output independent of OMP_NUM_THREADS" diff)
                    | _ -> fail name "no scalar binding 's' in program output (comparison would be vacuous)"

        printFooter "OpenMP Reduce" [$"{passed} passed"; $"{failed} failed"]
        { Block = "OpenMP Reduce"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }
