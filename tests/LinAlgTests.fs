// Linear-algebra dispatch EMISSION tests — Phases 5 / 5b / 5c of
// docs/plan-cpp-perf-exploitation.md.
//
// The corpus proves the VALUES (and `blade test interp math` proves they are
// byte-identical to the interpreter). This block proves the ROUTING: that a
// recognised shape reaches the `blade_linalg.hpp` shim when BLAS is available,
// that it emits Blade's OWN loops when it is not, that the header is included
// exactly when a route fires, and — as much as anything here — that a program
// using no route at all carries no dependency surface.
//
// Why assert on emitted TEXT. The routing decision is very nearly invisible in
// the values (BLAS agrees with Blade's loops to within a ULP), so a regression
// that silently stopped dispatching — or, worse, silently started — would leave
// every value test green. Only the emitted text can tell the two apart. The
// negative assertions matter as much as the positive ones: `cblas_` must NOT
// appear in generated code (the call spelling lives in the header), and neither
// the include nor `blade_linalg::` may appear when the gate is off.
//
// THE GATE IS PER-CASE (Phase 5c). `LinAlgPatterns.blasAvailable` re-reads the
// environment at every consultation — deliberately a function, never a frozen
// module value — so a `use`-guard around one emit is enough to pin it. Every
// case therefore declares which side of the gate it exercises, and the SHAPE
// negatives run with the gate ON so that their decline is proven to be about
// the shape rather than about availability.
//
// Pure codegen — no g++, no toolchain, no BLAS runtime. Always runs: pinning
// BLADE_BLAS=1 makes codegen emit shim CALLS, which needs no OpenBLAS
// installation because nothing here is compiled. (Compiling them is the
// corpus's job, and the corpus stays default-off.)
//
// The SECOND block in this file (`runLinAlgProbeTests`, also under `blade test
// linalg`) is the one thing emitted text cannot show: what the shim's runtime
// contiguity probe DECIDES. It compiles and runs cpp/linalg_probe_tests.cpp, so
// it needs g++ and skips without it — but it needs no BLAS, because the probe
// and the staging views live in `blade_linalg_views.hpp`, the BLAS-free half of
// the layer (Phase 5d).
module Blade.Tests.LinAlgTests

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Blade
open Blade.Build
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Lowering
open Blade.Tests.TestHarness

/// Pin one environment variable for the duration of a scope, restoring the
/// prior value on exit. Same use-guard idiom as `DiffOracle.pinFpContractOff`,
/// and it works for the same reason: every gate here is read per-call, so a
/// mid-process set takes effect immediately.
let private pinEnv (name: string) (value: string) =
    let prior = System.Environment.GetEnvironmentVariable(name)
    System.Environment.SetEnvironmentVariable(name, value)
    { new System.IDisposable with
        member _.Dispose() = System.Environment.SetEnvironmentVariable(name, prior) }

/// The HOST BLAS availability gate.
let private pinBlas (on: bool) = pinEnv "BLADE_BLAS" (if on then "1" else "0")

/// The DEVICE cuBLAS availability gate (Round D). A SECOND, INDEPENDENT gate —
/// which is why every emit below pins BOTH. Leaving this one to the ambient
/// environment would make the host cases non-deterministic on a machine where
/// someone had exported `BLADE_CUBLAS`: `resolveNodeRoute` tries the device
/// first, so a stray export would silently turn every gram/matmul positive into
/// a device route and fail the suite for a reason that has nothing to do with
/// the code.
let private pinCublas (on: bool) = pinEnv "BLADE_CUBLAS" (if on then "1" else "0")

/// Lower + generate under pinned gates, returning the C++ source. No compiler
/// involved. (Same helper shape as OmpTests.cppOf.)
let private cppOfGates (blasOn: bool) (cublasOn: bool) (testName: string) (src: string) : Result<string, string> =
    use _blas = pinBlas blasOn
    use _cublas = pinCublas cublasOn
    try
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir testName))
    with ex -> Error ($"codegen raised: {ex.Message}")

/// The host-only emit: BLAS as asked, cuBLAS explicitly OFF.
let private cppOf (blasOn: bool) (testName: string) (src: string) : Result<string, string> =
    cppOfGates blasOn false testName src

let private shimInclude = "#include \"blade_linalg.hpp\""
let private cudaShimInclude = "#include \"blade_linalg_cuda.hpp\""

/// (name, blasGateOn, source, mustContain, mustNotContain).
///
/// Every case asserts the absence of `cblas_` in the generated text: the
/// compiler NEVER writes a BLAS call itself, on any machine, with any
/// environment variable set. Which cblas routine runs is the header's business;
/// codegen's only decision is dispatch-or-not.
let private emissionCases : (string * bool * string * string list * string list) list =
    let realMat =
        "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
    let realMatB =
        "let B: Array<Float64 like Idx<4>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]\n"
    // The unit-carrying twins of the two above, element for element: same
    // extents, same literals, same storage, one `Unit` annotation apart. The
    // pair is what makes the units cases an A/B rather than an assertion about
    // one program.
    let unitMat =
        "Unit meter\n"
        + "let A: Array<Float64<meter> like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
    let unitMatB =
        "Unit second\n"
        + "let B: Array<Float64<second> like Idx<4>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]\n"
    // ---- Phase 5b fixtures ----
    let vecX = "let x: Array<Float64 like Idx<5>> = [1.0, 2.0, 3.0, 4.0, 5.0]\n"
    let vecY = "let y: Array<Float64 like Idx<5>> = [2.0, 3.0, 4.0, 5.0, 6.0]\n"
    /// An UNFORCED zip whose kernel is the product of the two co-iterated
    /// leaves — the deferred computation `reduce` folds without materializing.
    let deferredProd =
        "let P = method_for(zip(x, y)) <@> lambda(a: Float64, b: Float64) -> a * b\n"
    let matA =
        "type M = Idx<3>\ntype N = Idx<4>\n"
        + "let A: Array<Float64 like M, N> = [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]\n"
    let vecXv = "let xv: Array<Float64 like N> = [1.0, 2.0, 3.0, 4.0]\n"
    // ---- L3 syrk fixtures: the packed-covariance nest ----
    // 3 rows x 4 samples, so `m` (3) and `n` (4) are distinct in the emitted
    // call and a transposed argument order could not pass unnoticed.
    let covMat =
        "type T = Idx<4>\n"
        + "let CA: Array<Float64 like Idx<3>, T> = [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]\n"
    /// The pair kernel, parameterised by its where-clause and its body, so the
    /// plain / scaled / omp cases differ ONLY in the part under test.
    let covKernel (whereClause: string) (body: string) =
        "lambda(x: Array<Float64 like T>, y: Array<Float64 like T>) where "
        + whereClause + " -> " + body + " |> compute\n"
    [ // gram(A, A) — the symmetric rank-k update. Blade's result is PACKED
      // upper-triangular storage, which is why the route is its own adapter
      // rather than a bare `blade_syrk` call.
      //
      // Each positive case also pins the POOL CAPACITY argument that follows
      // every skeleton operand (Phase 5d). The shim's contiguity probe cannot
      // derive a pool's cell count from a row skeleton — row pointers give row
      // starts, not row lengths — so the emitter supplies it, and an emission
      // that dropped it would restore the n = 2 packed-symmetric false accept
      // with no runtime symptom. cpp/linalg_probe_tests.cpp proves what the
      // probe then does with it.
      ("gram_same_array_routes_to_syrk_adapter", true,
       realMat + "let G = gram(A, A)\n",
       [ shimInclude; "blade_linalg::blade_gram_same_d("; "linalg dispatch: gram(A, A)"
         "A.data, (A.extents[0] * A.extents[1])" ],
       [ "cblas_"; "#include <cblas.h>" ])
      // gram(A, B) — distinct operands, dense result: C = A * B^T, a gemm with
      // B transposed.
      // The dense OUTPUT's capacity is not inferred from a type — it is
      // allocated m x p a few lines above the call, so the emitter states it
      // directly as the extent product.
      ("gram_distinct_routes_to_gemm_adapter", true,
       realMat + realMatB + "let G = gram(A, B)\n",
       [ shimInclude; "blade_linalg::blade_gram_distinct_d("; "linalg dispatch: gram(A, B)"
         "A.data, (A.extents[0] * A.extents[1])"
         "B.data, (B.extents[0] * B.extents[1])"
         // Output capacity is BAKED from the fixture's literal Idx<3>/Idx<4>
         // extents (literalOrRuntimeExtentOfArray); operand cell counts keep
         // the runtime spelling (denseCellCountExpr, deliberately untouched).
         "G.data, (3 * 4)" ],
       [ "cblas_"; "#include <cblas.h>"; "blade_gram_same_" ])
      // matmul — the first-class intrinsic. `__math_matmul` must NOT survive
      // into the output (it is a pre-inference marker), and no synthesized
      // `__math_<n>` triple-loop function may be generated for it either.
      ("matmul_routes_to_gemm_adapter", true,
       "import math as m\n" +
       "let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n" +
       "let B: Array<Float64 like Idx<3>, Idx<2>> = [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]\n" +
       "let C = m.matmul(A, B)\n",
       [ shimInclude; "blade_linalg::blade_matmul_d("; "linalg dispatch: matmul(A, B)"
         "A.data, (A.extents[0] * A.extents[1])"
         "B.data, (B.extents[0] * B.extents[1])"
         // Baked output capacity: literal Idx<2> x Idx<2> from the fixture.
         "C.data, (2 * 2)" ],
       [ "cblas_"; "#include <cblas.h>"; "__math_matmul"; "double __math_1" ])
      // ================= TYPED DISPATCH (Round A) =================
      // Every entry point is `blade_<route>_<p>`, p ∈ {s,d,c,z}, and the letter
      // is appended by `shimEntryPoint` from the classified `Precision`. That
      // makes the ROUTINE FAMILY AND WIDTH assertable from generated text,
      // which is the only place a mis-dispatch would ever be visible: BLAS and
      // Blade's loops agree to a ULP, so no value test can tell them apart.
      //
      // Every case runs with the gate ON, so a wrong letter — or a decline —
      // is provably about the ELEMENT TYPE and not about availability.
      //
      // COMPLEX same-array gram is HERMITIAN: `_z` binds `cblas_zherk`, NOT
      // zsyrk. Blade's own complex scalar loop conjugates the second factor
      // (`conj_scalar`), i.e. it already computes A·A^H — so herk keeps the
      // semantics exactly and syrk would silently compute a different matrix.
      ("complex_gram_same_routes_to_zherk", true,
       "let A: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(2.0, 1.0)], [complex(3.0, 0.0), complex(4.0, -1.0)]]\n" +
       "let G = gram(A, A)\n",
       [ shimInclude; "blade_linalg::blade_gram_same_z("; "linalg dispatch: gram(A, A)" ],
       [ "cblas_"; "blade_gram_same_d"; "conj_scalar" ])
      // COMPLEX distinct gram is A·B^H: `_z` binds `cblas_zgemm` with
      // **CblasConjTrans** on B, because the scalar loop conjugates B's element
      // exactly as in the same-array case. A different classifier arm, so
      // pinned separately.
      ("complex_gram_distinct_routes_to_zgemm", true,
       "let A: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(2.0, 1.0)], [complex(3.0, 0.0), complex(4.0, -1.0)]]\n" +
       "let B: Array<Complex128 like Idx<3>, Idx<2>> = [[complex(1.0, 0.0), complex(0.0, 1.0)], [complex(2.0, 0.0), complex(0.0, 2.0)], [complex(3.0, 0.0), complex(0.0, 3.0)]]\n" +
       "let G = gram(A, B)\n",
       [ shimInclude; "blade_linalg::blade_gram_distinct_z("; "linalg dispatch: gram(A, B)" ],
       [ "cblas_"; "blade_gram_distinct_d"; "blade_gram_same_"; "conj_scalar" ])
      // FLOAT32 gram, both arms: `ssyrk` / `sgemm`.
      ("f32_gram_same_routes_to_ssyrk", true,
       "let A: Array<Float32 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n" +
       "let G = gram(A, A)\n",
       [ shimInclude; "blade_linalg::blade_gram_same_s(" ],
       [ "cblas_"; "blade_gram_same_d"; "__gacc" ])
      ("f32_gram_distinct_routes_to_sgemm", true,
       "let A: Array<Float32 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n" +
       "let B: Array<Float32 like Idx<4>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]\n" +
       "let G = gram(A, B)\n",
       [ shimInclude; "blade_linalg::blade_gram_distinct_s(" ],
       [ "cblas_"; "blade_gram_distinct_d"; "__gacc" ])
      // INTEGER stays DECLINED everywhere: `precisionOf` answers None for an
      // int element type, so there is no letter and no routine family. Blade's
      // own loops are the only correct answer, and this is the shape of every
      // future non-BLAS element type.
      ("int_gram_same_stays_on_scalar_loops", true,
       "let A: Array<Int64 like Idx<3>, Idx<2>> = [[1, 2], [3, 4], [5, 6]]\n" +
       "let G = gram(A, A)\n",
       [ "__gacc" ],
       [ shimInclude; "blade_linalg::"; "cblas_" ])
      // MIXED precisions never reach the router at all: `gram` REFUSES
      // operands whose element types differ (rejectionCases below), because
      // Blade has explicit numeric casts and an implicit widening inside a
      // contraction would change the arithmetic without naming the
      // conversion. Once a cast has made the pair agree there is nothing
      // mixed left to decline, so this is a typecheck rule and not a routing
      // one. The earlier version of this case asserted that mixed precision
      // compiled and fell back to the native loops -- but the emitter
      // converted neither operand, so `complex<float> * double` and
      // double-products-into-a-float-accumulator went to g++ instead.
      // NO GRATUITOUS DEPENDENCY SURFACE. A program with no linalg route must
      // not name the header at all — the include is collector-driven, not
      // unconditional, even though the file itself is deployed unconditionally.
      ("no_linalg_program_excludes_the_header", true,
       "let A = [1.0, 2.0, 3.0]\nlet s = reduce(A, (+))\n",
       [],
       [ shimInclude; "blade_linalg::"; "cblas_" ])

      // ================= Phase 5b: L1 dot =================
      // `reduce(<unforced zip under `*`>, (+))` over two rank-1 f64 arrays.
      // The whole fused accumulator loop disappears: no wrapper lambda, no
      // `for (size_t __i0 ...)`, one call carrying the SEED.
      ("dot_reduce_over_deferred_zip_routes_to_dot", true,
       vecX + vecY + deferredProd + "let s = reduce(P, (+))\n",
       [ shimInclude; "blade_linalg::blade_dot_d("; "linalg dispatch: dot(x, y)"
         // seed = (+)'s implicit identity, passed through to the shim so the
         // native fallback starts its accumulator exactly where the loop did.
         "x.data, y.data, 0.0)" ],
       [ "cblas_"; "for (size_t __i0" ])
      // The 3-arg form: the user's `init` is the seed, and it reaches the shim.
      ("dot_with_explicit_init_carries_the_seed", true,
       vecX + vecY + deferredProd + "let s = reduce(P, (+), 100.0)\n",
       [ "blade_linalg::blade_dot_d("; "x.data, y.data, 100.0)" ],
       [ "cblas_"; "for (size_t __i0" ])
      // PRECEDENCE (the load-bearing negative). An `omp`-licensed fold kernel
      // keeps Phase 2's chunked parallel fold. Firing the dot route here would
      // silently convert an EXPLICIT user reorder licence into serial code in
      // every build without BLAS, with nothing in the emitted text to show for
      // it — so the licence outranks the dispatch heuristic, always.
      ("dot_declines_when_the_fold_kernel_is_omp_licensed", true,
       vecX + vecY + deferredProd
       + "let s = reduce(P, lambda(p: Float64, q: Float64) where comm(p, q), omp -> p + q, 0.0)\n",
       [ "comm-licensed parallel fold"; "omp_get_num_threads()" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])
      // ELEMENT TYPE. The shim's v1 domain is real f64 (`ddot`). An integer
      // fold keeps its accumulator loop and names nothing.
      ("dot_declines_on_integer_elements", true,
       "let x: Array<Int64 like Idx<5>> = [1, 2, 3, 4, 5]\n"
       + "let y: Array<Int64 like Idx<5>> = [2, 3, 4, 5, 6]\n"
       + "let P = method_for(zip(x, y)) <@> lambda(a: Int64, b: Int64) -> a * b\n"
       + "let s = reduce(P, (+))\n",
       [ "for (size_t __i0" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])
      // The MATERIALIZED twin is deliberately NOT matched in v1: `x * y` is
      // synthesized WITH `|> compute`, so the reduce sees a real array (the
      // ordinary `IRReduce` path) and the intermediate is already paid for —
      // dispatching the second half alone saves far less than fusing the two.
      ("dot_declines_on_the_materialized_array_form", true,
       vecX + vecY + "let P = x * y\nlet s = reduce(P, (+))\n",
       [ "reduce: accumulator loop" ],
       [ "blade_linalg::"; shimInclude ])
      // FLOAT32 dot -> `sdot`.
      ("f32_dot_routes_to_sdot", true,
       "let x: Array<Float32 like Idx<5>> = [1.0, 2.0, 3.0, 4.0, 5.0]\n"
       + "let y: Array<Float32 like Idx<5>> = [2.0, 3.0, 4.0, 5.0, 6.0]\n"
       + "let P = method_for(zip(x, y)) <@> lambda(a: Float32, b: Float32) -> a * b\n"
       + "let s = reduce(P, (+))\n",
       [ shimInclude; "blade_linalg::blade_dot_s(" ],
       [ "cblas_"; "blade_dot_d"; "(x____i0 * y____i0)" ])
      // COMPLEX dot -> `zdotu`, and the `_z` name is the assertion that it is
      // dotU. Blade's zip product is `a * b` with NO conjugation anywhere, so
      // `zdotc` (Σ conj(x_i)·y_i) would silently return a different number —
      // most visibly for dot(x, x), where dotc gives the real squared norm and
      // Blade's fold gives the complex Σ x_i². A conjugating inner product is a
      // DIFFERENT operation and needs its own surface form; it is deliberately
      // not implemented, so nothing here should ever route to a `dotc`.
      ("complex_dot_routes_to_zdotu", true,
       "let x: Array<Complex128 like Idx<3>> = [complex(1.0, 1.0), complex(2.0, 0.0), complex(3.0, -1.0)]\n"
       + "let y: Array<Complex128 like Idx<3>> = [complex(2.0, 0.0), complex(1.0, 1.0), complex(0.0, 1.0)]\n"
       + "let P = method_for(zip(x, y)) <@> lambda(a: Complex128, b: Complex128) -> a * b\n"
       + "let s = reduce(P, (+))\n",
       [ shimInclude; "blade_linalg::blade_dot_z(" ],
       [ "cblas_"; "blade_dot_d"; "(x____i0 * y____i0)" ])
      // INTEGER dot stays DECLINED: no BLAS routine family for int elements.
      ("dot_declines_on_integer_elements_typed", true,
       "let x: Array<Int64 like Idx<5>> = [1, 2, 3, 4, 5]\n"
       + "let y: Array<Int64 like Idx<5>> = [2, 3, 4, 5, 6]\n"
       + "let P = method_for(zip(x, y)) <@> lambda(a: Int64, b: Int64) -> a * b\n"
       + "let s = reduce(P, (+))\n",
       [ "(x____i0 * y____i0)" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])

      // ================= Phase 5b: L2 gemv =================
      // A per-row apply whose kernel is `prodsum(row, x)`: rank-2 operand
      // peeled one level, shared rank-1 vector, rank-1 output. The whole nest
      // — row peel, prodsum IIFE and all — becomes one call.
      ("gemv_per_row_prodsum_fiber_routes_to_gemv", true,
       matA + vecXv
       + "let yv = method_for(A) <@> lambda(row: Array<Float64 like N>) -> prodsum(row, xv) |> compute\n",
       [ shimInclude; "blade_linalg::blade_gemv_d("; "linalg dispatch: gemv y = A * xv"
         // m from the row loop's own bound, n from A's TRAILING extent — both
         // literal after shape monomorphization, exactly as the nest would —
         // and A's pool capacity, because gemv stages A through the same
         // `in_view` the L3 adapters use (Phase 5d). `xv` and `yv` are rank-1
         // pools handed over directly: no skeleton, hence no probe, hence no
         // capacity argument.
         "blade_linalg::blade_gemv_d(3, 4, A.data, (A.extents[0] * A.extents[1]), xv.data, yv.data)" ],
       // The NEST is gone: no row peel, no per-row output write. (The lifted
       // kernel lambda itself is still emitted as a now-unused function — it
       // always was, since the nest inlined the body rather than calling it —
       // so its `prodsum` IIFE is not a usable negative here.)
       [ "cblas_"; "A____i0"; "yv[__i0]" ])
      // SELF-DOT PER ROW is not a matrix-vector product. `prodsum(er, er)` (the
      // row-norm idiom the math corpus uses) matches every structural gate;
      // only the "second operand comes from outside the kernel" guard separates
      // them, and routing it would pass the peeled row as `x` for every row.
      ("gemv_declines_on_per_row_self_dot", true,
       matA
       + "let rowsums = method_for(A) <@> lambda(er: Array<Float64 like N>) -> prodsum(er, er) |> compute\n",
       [ "__ps = 0" ],
       [ "blade_linalg::"; shimInclude ])
      // ARGUMENT ORDER is not cosmetic: `IRProdSum` takes its loop bound from
      // its FIRST operand, so `prodsum(xv, row)` is bounded by the VECTOR's
      // extent, not by A's trailing extent. Blade's unify does not compare
      // extents, so the two can disagree — declining is the honest answer.
      ("gemv_declines_when_the_vector_comes_first", true,
       matA + vecXv
       + "let yv = method_for(A) <@> lambda(row: Array<Float64 like N>) -> prodsum(xv, row) |> compute\n",
       [ "xv.extents[0]" ],
       [ "blade_linalg::"; shimInclude ])
      // PRECEDENCE, same rule as dot: an `omp` request on the row kernel keeps
      // the nest, so the pragma the user asked for still lands on it.
      ("gemv_declines_when_the_row_kernel_requests_omp", true,
       matA + vecXv
       + "let yv = method_for(A) <@> lambda(row: Array<Float64 like N>) where omp -> prodsum(row, xv) |> compute\n",
       [ "#pragma omp parallel for" ],
       [ "blade_linalg::"; shimInclude ])
      // FLOAT32 gemv -> `sgemv`. The `_s` letter is exactly the width a `dgemv`
      // mis-dispatch would have got wrong.
      ("f32_gemv_routes_to_sgemv", true,
       "type M = Idx<3>\ntype N = Idx<4>\n"
       + "let A: Array<Float32 like M, N> = [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]\n"
       + "let xv: Array<Float32 like N> = [1.0, 2.0, 3.0, 4.0]\n"
       + "let yv = method_for(A) <@> lambda(row: Array<Float32 like N>) -> prodsum(row, xv) |> compute\n",
       [ shimInclude; "blade_linalg::blade_gemv_s(" ],
       // The NEST is gone. (The lifted kernel lambda is still emitted as an
       // unused function carrying its own prodsum IIFE — it always was — so
       // `__ps` is not a usable negative; the row peel and the per-row write
       // are.)
       [ "cblas_"; "blade_gemv_d"; "A____i0"; "yv[__i0]" ])
      // COMPLEX gemv -> `zgemv`, CblasNoTrans. `prodsum(row, x)` conjugates
      // nothing, so there is nothing for the complex instance to do differently
      // from the real one beyond its width.
      ("complex_gemv_routes_to_zgemv", true,
       "type M = Idx<2>\ntype N = Idx<2>\n"
       + "let A: Array<Complex128 like M, N> = [[complex(1.0, 0.0), complex(2.0, 0.0)], [complex(3.0, 0.0), complex(4.0, 0.0)]]\n"
       + "let xv: Array<Complex128 like N> = [complex(1.0, 0.0), complex(1.0, 1.0)]\n"
       + "let yv = method_for(A) <@> lambda(row: Array<Complex128 like N>) -> prodsum(row, xv) |> compute\n",
       [ shimInclude; "blade_linalg::blade_gemv_z(" ],
       [ "cblas_"; "blade_gemv_d"; "A____i0"; "yv[__i0]" ])
      // INTEGER gemv stays DECLINED.
      ("int_gemv_stays_on_the_per_row_nest", true,
       "type M = Idx<2>\ntype N = Idx<2>\n"
       + "let A: Array<Int64 like M, N> = [[1, 2], [3, 4]]\n"
       + "let xv: Array<Int64 like N> = [1, 2]\n"
       + "let yv = method_for(A) <@> lambda(row: Array<Int64 like N>) -> prodsum(row, xv) |> compute\n",
       [ "__ps = 0" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])

      // ================= L3 syrk: the packed-covariance nest =================
      // `method_for(A, A) <@> lambda(ri, rj) where comm(ri, rj) -> prodsum(ri, rj)`
      // is a covariance / Gram matrix: a two-level nest, level 0 rectangular
      // and level 1 the INCLUSIVE triangle the `comm` licence created, two
      // peels of dim 0 of ONE array, output a rank-2 SymSymmetric packed pool.
      // That is `C = A·Aᵀ`, upper triangle, packed — i.e. `?syrk` — and the
      // whole nest (row peels, triangular bound, prodsum IIFE) becomes one call.
      //
      // The pool capacity rides along for A exactly as it does for gram/gemv
      // (the shim stages A through the same `in_view`); C carries NONE, because
      // `blade_gram_same_*` writes `Crows[i][jr]` with `jr < m - i`, which is
      // Blade's packed row footprint cell for cell — no view, no probe.
      ("syrk_comm_prodsum_nest_routes_to_syrk", true,
       covMat
       + "let m2 = method_for(CA, CA) <@> " + covKernel "comm(x, y)" "prodsum(x, y)",
       [ shimInclude; "blade_linalg::blade_gram_same_d("
         "linalg dispatch: syrk C = A * A^T (packed upper, from comm nest)"
         "blade_linalg::blade_gram_same_d(3, 4, CA.data, (CA.extents[0] * CA.extents[1]), m2.data);" ],
       // The NEST is gone: no row peel, no triangular inner bound, no scale
       // pass. (The lifted kernel lambda survives as a now-unused function
       // carrying its own prodsum IIFE — it always did — so `__ps` is not a
       // usable negative here; the peel and the inner bound are.)
       [ "cblas_"; "CA____i0"; "__i1 < 3 - __i0"; "__sy_i" ])
      // THE `/N` FORM, which is what a real centred covariance carries
      // (corpus: `sgs/005_filter_stress_comoment`, `ppl/065_staged_moment_tower`).
      // The kernel's scalar is applied AFTER the call over the same `[i][jr]`
      // footprint the shim just wrote — deliberately NOT folded into syrk's
      // `alpha`, since `alpha = 1/N` rounds a reciprocal and then multiplies
      // whereas the nest this replaces divides the finished sum.
      ("syrk_scaled_nest_applies_the_kernel_scalar_after_the_call", true,
       covMat
       + "let c2 = method_for(CA, CA) <@> " + covKernel "comm(x, y)" "prodsum(x, y) / 4.0",
       [ "blade_linalg::blade_gram_same_d(3, 4, CA.data, (CA.extents[0] * CA.extents[1]), c2.data);"
         "for (size_t __sy_i = 0; __sy_i < 3; __sy_i++) for (size_t __sy_j = 0; __sy_j < 3 - __sy_i; __sy_j++) c2[__sy_i][__sy_j] /= (4.0);" ],
       [ "cblas_"; "CA____i0"; "__i1 < 3 - __i0" ])
      // ***PRECEDENCE, AND IT IS THE OPPOSITE OF L1/L2's.*** An `omp` request
      // on the kernel does NOT decline this route. Copying L2's hardcoded
      // `ompRequested -> None` would have made the pattern fire on nothing:
      // the covariance kernels it exists for are precisely the ones written
      // with `omp`. The answer now comes from `LinAlgPatterns.ompPolicy`, whose
      // L3 row is `BlasWins` — OpenBLAS's `?syrk` is itself multithreaded AND
      // register-blocked AND packed, so honouring the pragma instead of the
      // route would cost the user the parallelism they asked for.
      ("syrk_still_routes_when_the_comm_kernel_requests_omp", true,
       covMat
       + "let o2 = method_for(CA, CA) <@> " + covKernel "comm(x, y), omp" "prodsum(x, y)",
       [ shimInclude; "blade_linalg::blade_gram_same_d("
         "linalg dispatch: syrk C = A * A^T (packed upper, from comm nest)" ],
       [ "cblas_"; "#pragma omp parallel for"; "CA____i0" ])
      // DISTINCT ARRAYS are `gram(A, B)` — a DENSE result, a different route
      // and a different adapter. Without `comm` the nest is also rectangular,
      // not triangular, so nothing about syrk's half-work applies.
      ("syrk_declines_on_two_distinct_arrays", true,
       covMat
       + "let CB: Array<Float64 like Idx<2>, T> = [[1.0, 0.0, 0.0, 1.0], [0.0, 1.0, 1.0, 0.0]]\n"
       + "let d1 = method_for(CA, CB) <@> lambda(x: Array<Float64 like T>, y: Array<Float64 like T>) -> prodsum(x, y) |> compute\n",
       [ "__ps = 0"; "CB____i1" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])
      // SAME ARRAY BUT NO `comm`: a full rectangular sweep into a DENSE
      // rank-2 output. syrk computes a triangle into packed storage, so the
      // output storage class alone rules this out.
      ("syrk_declines_without_the_comm_licence", true,
       covMat
       + "let d2 = method_for(CA, CA) <@> lambda(x: Array<Float64 like T>, y: Array<Float64 like T>) -> prodsum(x, y) |> compute\n",
       [ "__ps = 0"; "for (size_t __i1 = 0; __i1 < 3;" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])
      // INTEGER elements: `precisionOf` answers None, so there is no routine
      // family and no letter — the same decline every other route makes.
      ("syrk_declines_on_integer_elements", true,
       "type T = Idx<4>\n"
       + "let CI: Array<Int64 like Idx<3>, T> = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]\n"
       + "let d3 = method_for(CI, CI) <@> lambda(x: Array<Int64 like T>, y: Array<Int64 like T>) where comm(x, y) -> prodsum(x, y) |> compute\n",
       [ "__ps = 0" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])
      // ***COMPLEX DECLINES, and not for want of an entry point.***
      // `blade_gram_same_z` exists and binds `zherk` — A·A^H, conjugating the
      // second factor — because that is what Blade's complex GRAM NODE loop
      // computes. The matched kernel body here is `prodsum(ri, rj)`, which
      // conjugates NOTHING, so this nest is A·Aᵀ; routing it to the Hermitian
      // adapter would silently return a different matrix. A conjugating
      // surface form would be a different match, not a precision widening.
      ("syrk_declines_on_complex_elements", true,
       "let CZ: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(2.0, 1.0)], [complex(3.0, 0.0), complex(4.0, -1.0)]]\n"
       + "let d4 = method_for(CZ, CZ) <@> lambda(x: Array<Complex128 like Idx<2>>, y: Array<Complex128 like Idx<2>>) where comm(x, y) -> prodsum(x, y) |> compute\n",
       [ "__ps = 0" ],
       [ "blade_linalg::blade_gram_same_"; shimInclude; "cblas_" ])
      // FLOAT32 declines in THIS increment, and the negative is pinned so the
      // narrowing is a decision on the record rather than an oversight.
      // `blade_gram_same_s` exists, is layout-identical and is already reached
      // by the gram NODE route (`f32_gram_same_routes_to_ssyrk` above), so
      // widening is one conjunct in `(|BlasL3|_|)` plus the positive twin of
      // this case — deliberately left until it is exercised.
      ("syrk_declines_on_float32_in_this_increment", true,
       "type T = Idx<4>\n"
       + "let CF: Array<Float32 like Idx<3>, T> = [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]\n"
       + "let d6 = method_for(CF, CF) <@> lambda(x: Array<Float32 like T>, y: Array<Float32 like T>) where comm(x, y) -> prodsum(x, y) |> compute\n",
       [ "__ps = 0" ],
       [ "blade_linalg::blade_gram_same_"; shimInclude ])
      // REYNOLDS reads PERMUTED coordinates, so the cell value is a SUM over
      // the orbit and not the contraction at all (`2 * prodsum` here). The
      // shared `nestModeOk` gate refuses it for every nest route.
      ("syrk_declines_under_a_reynolds_wrapper", true,
       covMat
       + "let d5 = method_for(CA, CA) <@> reynolds(lambda(x: Array<Float64 like T>, y: Array<Float64 like T>) where comm(x, y) -> prodsum(x, y)) |> compute\n",
       [ "Reynolds:"; "__ps = 0" ],
       [ "blade_linalg::"; shimInclude; "cblas_" ])

      // ============ Phase 5c: the GATE-OFF side (the default build) ============
      // With BLAS unavailable, no route is emitted and the native math comes
      // from Blade's OWN pre-existing emission paths. These four cases are the
      // exact positives above with the gate flipped, so together each pair
      // isolates the gate as the only difference. The `blade_linalg` negatives
      // are the load-bearing half: since Phase 5c the header has NO native
      // fallbacks and `#error`s without the define, so emitting the include
      // here would produce a program that does not compile.
      //
      // gram(A, A): the packed upper-triangular write, row-shortened, one
      // accumulator per canonical cell.
      ("gate_off_gram_same_emits_packed_triangular_loops", false,
       realMat + "let G = gram(A, A)\n",
       // The triangular bound is baked from the fixture's literal Idx<3>.
       [ "for (size_t __gi"; "__gjr < 3 - __gi"; "G[__gi][__gjr] = __gacc;" ],
       [ shimInclude; "blade_linalg::"; "cblas_" ])
      // gram(A, B): the dense scatter over all (i, j).
      // The `__gj` axis is UNROLL-AND-JAMMED (R = 4), so its induction variable
      // is hoisted and the loop is a bare `for (; ...)` -- the old
      // "for (size_t __gj" marker no longer appears even though the loop
      // plainly does. What this case asserts is "gate off emits the NATIVE
      // dense loops, not a shim call", so it now pins BOTH emitted bodies: the
      // jammed tile and the scalar remainder. That is strictly more than
      // before, since a jam that silently stopped firing would now fail here
      // rather than pass unnoticed.
      ("gate_off_gram_distinct_emits_dense_loops", false,
       realMat + realMatB + "let G = gram(A, B)\n",
       [ "for (size_t __gi"; "__gj + 4 <="
         "G[__gi][__gj + 0] = __gacc0;"; "G[__gi][__gj] = __gacc;" ],
       [ shimInclude; "blade_linalg::"; "cblas_" ])
      // matmul: the reordered i-t-j triple loop (unit-stride B, row-accumulator
      // in C). Per OUTPUT CELL the summands are still added in ascending t —
      // the same per-cell order Interp/ArrayOps.matmulArray uses, which is what
      // keeps `interp math` a byte-identity test of the code an ordinary build
      // actually runs (verified byte-identical under -ffp-contract=off).
      ("gate_off_matmul_emits_native_triple_loop", false,
       "import math as m\n" +
       "let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n" +
       "let B: Array<Float64 like Idx<3>, Idx<2>> = [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]\n" +
       "let C = m.matmul(A, B)\n",
       [ "for (size_t __mi"; "for (size_t __mj"; "for (size_t __mt"
         "const double __ma = A[__mi][__mt];"
         "__mcrow[__mj] += __ma * __mbrow[__mj];" ],
       [ shimInclude; "blade_linalg::"; "cblas_"; "__math_matmul" ])
      // dot: the fused fold nest, unchanged from before any dispatch existed.
      ("gate_off_dot_emits_the_fold_nest", false,
       vecX + vecY + deferredProd + "let s = reduce(P, (+))\n",
       [ "for (size_t __i0"; "(x____i0 * y____i0)" ],
       [ shimInclude; "blade_linalg::" ])
      // gemv: the per-row peel plus the prodsum IIFE, likewise unchanged.
      ("gate_off_gemv_emits_the_per_row_nest", false,
       matA + vecXv
       + "let yv = method_for(A) <@> lambda(row: Array<Float64 like N>) -> prodsum(row, xv) |> compute\n",
       [ "A____i0"; "yv[__i0]"; "__ps = 0" ],
       [ shimInclude; "blade_linalg::" ])
      // syrk: the comm-licensed triangular nest, row peels and prodsum IIFE
      // and all — the emission an ordinary (BLAS-free) build gets, which is
      // also the one `interp math` proves byte-identical to the interpreter.
      // The scale stays where the kernel put it: INSIDE the cell expression,
      // not in a second pass.
      ("gate_off_syrk_emits_the_triangular_comm_nest", false,
       covMat
       + "let c2 = method_for(CA, CA) <@> " + covKernel "comm(x, y)" "prodsum(x, y) / 4.0",
       [ "CA____i0"; "for (size_t __i1 = 0; __i1 < 3 - __i0"; "__ps = 0"; "/ 4.0)" ],
       [ shimInclude; "blade_linalg::"; "__sy_i" ])
      // ...and with the gate off the `omp` request keeps its pragma, because
      // there is no route to hand the parallelism to. The gate is the ONLY
      // difference between this and `syrk_still_routes_when_the_comm_kernel_
      // requests_omp` above, which is what makes the pair isolate it.
      ("gate_off_syrk_omp_nest_keeps_its_pragma", false,
       covMat
       + "let o2 = method_for(CA, CA) <@> " + covKernel "comm(x, y), omp" "prodsum(x, y)",
       [ "#pragma omp parallel for schedule(dynamic)"; "CA____i0" ],
       [ shimInclude; "blade_linalg::" ])

      // ============ UNITS ARE NOT A PRECISION ============
      // A unit annotation must not cost the route. `Float64<meter>` STORES as a
      // bare `double` -- the wrapper erases entirely at codegen, so the emitted
      // pool is `Array<double, 2>` with or without it -- and the output's units
      // are computed by the type system from the operand units, never read back
      // off the precision letter. So the dispatch is the unit-free one, letter
      // and all.
      //
      // `precisionOf` used to match bare `IRTScalar` only, which silently
      // declined EVERY route for EVERY unit-annotated array: dimensioned
      // physics code got scalar loops while its dimensionless twin got `dsyrk`,
      // with no diagnostic and no value difference to notice it by. These are
      // the positives that would have failed; `unit_gram_emits_the_unit_free_
      // text` below is the same claim stated as byte equality.
      ("gram_same_routes_through_a_unit_annotation", true,
       unitMat + "let G = gram(A, A)\n",
       [ shimInclude; "blade_linalg::blade_gram_same_d("; "linalg dispatch: gram(A, A)"
         "A.data, (A.extents[0] * A.extents[1])" ],
       [ "cblas_"; "__gacc" ])
      // DISAGREEING units on the two operands still route. `agreedPrecision`
      // asks for one PRECISION, not one type: meter and second are both `d`,
      // and `gram` multiplies the stored doubles either way. What the units
      // change is the RESULT type (meter * second), which the type system
      // derives on its own.
      ("gram_distinct_routes_with_disagreeing_units", true,
       unitMat + unitMatB + "let G = gram(A, B)\n",
       [ shimInclude; "blade_linalg::blade_gram_distinct_d("; "linalg dispatch: gram(A, B)"
         "G.data, (3 * 4)" ],
       [ "cblas_"; "blade_gram_same_"; "__gacc" ]) ]

let runLinAlgEmissionTests () : BlockResult =
    printHeader "LinAlg Dispatch Emission"
    // Several cases here pin `#pragma omp parallel for` -- the "the nest keeps
    // its pragma rather than being dispatched" half of the precedence rule.
    // `BLADE_OMP_THREADS=1|0|off` (CodeGen.ompThreadEmissionEnabled) is a BUILD
    // knob meant to be set globally on a serial deployment box, and it would
    // delete exactly those pragmas -- turning a precedence assertion into a
    // spurious red. Pinned unset for the block; same use-guard as `pinBlas`.
    use _ompThreads = pinEnv "BLADE_OMP_THREADS" null
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [name]
        resultLine Fail name detail
    for (name, blasOn, src, mustContain, mustNotContain) in emissionCases do
        match cppOf blasOn name src with
        | Error e -> fail name e
        | Ok cpp ->
            let missing = mustContain |> List.filter (fun s -> not (cpp.Contains s))
            let present = mustNotContain |> List.filter cpp.Contains
            if not missing.IsEmpty then
                fail name ($"""generated C++ lacks: {(String.concat " | " missing)}""")
            elif not present.IsEmpty then
                fail name ($"""generated C++ unexpectedly contains: {(String.concat " | " present)}""")
            else
                passed <- passed + 1
                resultLine Pass name (if blasOn then "routing as expected (gate on)"
                                      else "native emission as expected (gate off)")

    // ---- a unit annotation changes NO emitted byte ----
    //
    // The strongest form of the units claim, and the one that needs no pinned
    // substrings: compile `gram(A, A)` with and without `Unit meter` on the
    // element type and the two C++ programs must be IDENTICAL. That is the
    // whole soundness argument for stripping units in `precisionOf` stated as
    // a test -- units erase at codegen, so there is nothing left for the
    // precision letter to be wrong about -- and it is what a substring pin
    // cannot say: an equality catches a divergence ANYWHERE in the program,
    // including places no one thought to assert.
    //
    // Both sides are emitted under the same program NAME, since that string is
    // stamped into the timing line and is the one difference that is expected.
    let unitAbName = "unit_gram_emits_the_unit_free_text"
    let plainSrc =
        "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
        + "let G = gram(A, A)\n"
    let unitSrc =
        "Unit meter\n"
        + "let A: Array<Float64<meter> like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
        + "let G = gram(A, A)\n"
    match cppOf true unitAbName plainSrc, cppOf true unitAbName unitSrc with
    | Error e, _ | _, Error e -> fail unitAbName e
    | Ok plainCpp, Ok unitCpp ->
        if plainCpp <> unitCpp then
            // Report the first differing line rather than two whole programs.
            let firstDiff =
                Seq.zip (plainCpp.Split '\n') (unitCpp.Split '\n')
                |> Seq.tryFind (fun (p, u) -> p <> u)
                |> Option.map (fun (p, u) -> $"plain: {p.Trim()} || unit: {u.Trim()}")
                |> Option.defaultValue "(programs differ only in length)"
            fail unitAbName $"unit-annotated emission diverged -- {firstDiff}"
        elif not (unitCpp.Contains "blade_linalg::blade_gram_same_d(") then
            // Guard against the pair agreeing because NEITHER routes, which
            // would make the equality vacuous.
            fail unitAbName "both sides declined the route -- equality is vacuous"
        else
            passed <- passed + 1
            resultLine Pass unitAbName "byte-identical to the unit-free twin, both routed"

    // ---- matmul: a non-f64 operand is REJECTED, not silently routed ----
    //
    // matmul is the one route whose gate sits at TYPECHECK rather than at
    // classification: `TypeCheck.inferMatmul` (:4581-4583) requires Float64
    // elements, so a f32/complex/int operand never reaches codegen at all and
    // `LinAlgPatterns.classifyMatmul`'s own `isRealDouble` conjunct is a
    // backstop that cannot fire from the surface. Both layers are load-bearing
    // and neither is tested by the emission cases above, which only ever see
    // programs that typecheck — hence this block.
    //
    // WHY THIS IS THE RIGHT BEHAVIOUR, not a narrowing. The synthesized
    // `matmulDecl` this intrinsic replaced declared `Array<Float64 …>` params
    // (git show ffcecbc:src/math/compiler/MathDecls.fs:121), but Blade's
    // direct-application seam does not unify param types with argument types,
    // so a non-f64 call TYPECHECKED and then failed in g++ with a C++ template
    // error. Measured on the still-synthesized siblings that use the identical
    // machinery: `m.svd` / `m.unfold` with f32, complex128 or int64 operands
    // all report OK from `blade check` and then die at compile with
    // "could not convert 'A' from 'Array<long long int,[...]>' to
    // 'Array<double,[...]>'". So the set of BUILDABLE programs is unchanged;
    // only the diagnostic moved, from an unintelligible C++ error to a named
    // rule at the seam that owns it.
    let matmulHeader =
        "import math as m\n"
    let elemGateMsg = "matmul: both arguments must have Float64 elements"
    let rejectionCases : (string * string * string) list =
        [ ("matmul_rejects_f32_operands",
           matmulHeader
           + "let A: Array<Float32 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n"
           + "let B: Array<Float32 like Idx<3>, Idx<2>> = [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]\n"
           + "let C = m.matmul(A, B)\n",
           elemGateMsg)
          ("matmul_rejects_complex128_operands",
           matmulHeader
           + "let A: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(2.0, 0.0)], [complex(3.0, 0.0), complex(4.0, 0.0)]]\n"
           + "let C = m.matmul(A, A)\n",
           elemGateMsg)
          ("matmul_rejects_int64_operands",
           matmulHeader
           + "let A: Array<Int64 like Idx<2>, Idx<2>> = [[1, 2], [3, 4]]\n"
           + "let C = m.matmul(A, A)\n",
           elemGateMsg)
          // gram's element-type rule, stated where matmul's lives. The old
          // rule chose a RESULT width ("the complex operand, else the left
          // one") and converted neither operand, so a mixed pair typechecked
          // and then died in g++. Refusing sends the user to the explicit
          // cast, which is the one pathway that names the conversion.
          ("gram_rejects_mixed_precision_operands",
           "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
           + "let B: Array<Float32 like Idx<4>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]\n"
           + "let G = gram(A, B)\n",
           "must share one element type") ]
    for (name, src, expectedFragment) in rejectionCases do
        // Gate ON, so a decline can only be about the ELEMENT TYPE.
        match cppOf true name src with
        | Ok cpp ->
            fail name (sprintf "expected rejection %A, but the program compiled (names blade_linalg: %b)"
                           expectedFragment (cpp.Contains "blade_linalg::"))
        | Error e when e.Contains expectedFragment ->
            passed <- passed + 1
            resultLine Pass name "rejected at typecheck (gate on)"
        | Error e -> fail name ($"rejected, but not for the pinned reason: {e}")

    // ---- the availability gate itself ----
    // Pinned because it is now shared with Build.fs's flag injection: a change
    // in these semantics silently changes which g++ line a program gets. The
    // unset case reads OPENBLAS_DIR, so it is exercised with that pinned too.
    let gateCases =
        [ "gate_forced_on",   Some "1",   None,                  true
          "gate_forced_on_word", Some "on", None,                true
          "gate_forced_off",  Some "0",   Some "C:\\opt\\blas",  false
          "gate_forced_off_word", Some "off", Some "C:\\opt\\blas", false
          "gate_unset_follows_openblas_dir", None, Some "C:\\opt\\blas", true
          "gate_unset_without_openblas_dir", None, None,         false ]
    for (name, blasVar, openblasVar, expected) in gateCases do
        let priorB = System.Environment.GetEnvironmentVariable("BLADE_BLAS")
        let priorO = System.Environment.GetEnvironmentVariable("OPENBLAS_DIR")
        try
            System.Environment.SetEnvironmentVariable("BLADE_BLAS", Option.toObj blasVar)
            System.Environment.SetEnvironmentVariable("OPENBLAS_DIR", Option.toObj openblasVar)
            let actual = LinAlgPatterns.blasAvailable ()
            if actual = expected then
                passed <- passed + 1
                resultLine Pass name (sprintf "%b" actual)
            else fail name (sprintf "expected %b, got %b" expected actual)
        finally
            System.Environment.SetEnvironmentVariable("BLADE_BLAS", priorB)
            System.Environment.SetEnvironmentVariable("OPENBLAS_DIR", priorO)

    // ---- the policy table is the documented contract, so pin it ----
    // A row moving from Native to ViaShim (or vanishing) is a deliberate
    // decision that should require editing a test, not a silent drift.
    //
    // These need NO gate pin, and that is the point of keeping `routingOf`
    // free of the availability question (Phase 5c): the table states whether a
    // shape is WORTH a BLAS call, which is a property of the routine and must
    // read the same on a machine with OpenBLAS and one without. Availability
    // enters one level down, in `shimEntryPoint`.
    let policyCases =
        [ "host_gemm_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Gemm, LinAlgPatterns.ViaShim
          "host_syrk_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Syrk, LinAlgPatterns.ViaShim
          "host_dot_via_shim",  LinAlgPatterns.HostBlas, LinAlgPatterns.Dot,  LinAlgPatterns.ViaShim
          "host_gemv_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Gemv, LinAlgPatterns.ViaShim
          // Same paying-L1-reduction policy as dot, but NOT MATCHED (no
          // sqrt-shape case exists). The row is here so "routed via the shim
          // but never recognised" stays readable from the table rather than
          // being an undocumented gap.
          "host_nrm2_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Nrm2, LinAlgPatterns.ViaShim
          // The recorded "matched but routed native" decisions: L1-elementwise
          // is bandwidth-bound and the flat loop already vectorises it.
          "host_axpy_native",   LinAlgPatterns.HostBlas, LinAlgPatterns.Axpy, LinAlgPatterns.Native
          "host_scal_native",   LinAlgPatterns.HostBlas, LinAlgPatterns.Scal, LinAlgPatterns.Native
          // ---- CudaBlas (Round D) ----
          // The two L3 rows LANDED and are pinned ViaShim; the L1/L2 rows are
          // pinned Native. Both halves matter: these four pins existed
          // precisely so that landing (or losing) a device route is a
          // deliberate table edit rather than a silent flip, and the
          // Native/ViaShim split IS the policy — L3 amortises a PCIe round trip
          // (O(mnk) flops against O(mn + nk) bytes), L1/L2 cannot.
          "cuda_gemm_via_shim", LinAlgPatterns.CudaBlas, LinAlgPatterns.Gemm, LinAlgPatterns.ViaShim
          "cuda_syrk_via_shim", LinAlgPatterns.CudaBlas, LinAlgPatterns.Syrk, LinAlgPatterns.ViaShim
          "cuda_dot_native",    LinAlgPatterns.CudaBlas, LinAlgPatterns.Dot,  LinAlgPatterns.Native
          "cuda_gemv_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Gemv, LinAlgPatterns.Native
          "cuda_nrm2_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Nrm2, LinAlgPatterns.Native
          "cuda_axpy_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Axpy, LinAlgPatterns.Native
          "cuda_scal_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Scal, LinAlgPatterns.Native ]
    for (name, backend, routine, expected) in policyCases do
        let actual = LinAlgPatterns.routingOf backend routine
        if actual = expected then
            passed <- passed + 1
            resultLine Pass name (sprintf "%A" actual)
        else fail name (sprintf "expected %A, got %A" expected actual)

    // ---- the OMP-vs-BLAS precedence table, pinned per LEVEL ----
    //
    // This used to be a hardcoded `ompRequested -> None` inside each matcher,
    // which read as a universal rule; it is not one. L1/L2 decline (the user's
    // explicit reorder licence outranks a dispatch heuristic, and the no-BLAS
    // fallback for dot/gemv is serial), L3 does NOT (OpenBLAS's `?syrk` is
    // itself multithreaded, register-blocked and packed, so the route honours
    // the licence better than the pragma does — and declining would have made
    // the L3 nest pattern fire on nothing, since covariance kernels carry
    // `omp`).
    //
    // Pinned for the same reason `routingOf` is: these are DECISIONS. Flipping
    // one silently changes which programs dispatch, with nothing in the values
    // to show for it.
    let precedenceCases =
        [ "omp_precedence_L1_omp_wins",  LinAlgPatterns.L1, LinAlgPatterns.OmpWins
          "omp_precedence_L2_omp_wins",  LinAlgPatterns.L2, LinAlgPatterns.OmpWins
          "omp_precedence_L3_blas_wins", LinAlgPatterns.L3, LinAlgPatterns.BlasWins ]
    for (name, lvl, expected) in precedenceCases do
        let actual = LinAlgPatterns.ompPrecedenceOf lvl
        let declines = LinAlgPatterns.ompRequestDeclines lvl
        if actual = expected && declines = (expected = LinAlgPatterns.OmpWins) then
            passed <- passed + 1
            resultLine Pass name (sprintf "%A (declines dispatch: %b)" actual declines)
        else fail name (sprintf "expected %A, got %A (declines %b)" expected actual declines)

    // ---- eigh: the (precision × SYMMETRY) route table (Round B) ----
    //
    // No surface form reaches these yet — `math.eigh` still elaborates to the
    // synthesized Jacobi source, and routing it is the next increment — so the
    // classifier is exercised DIRECTLY on operand types. That is the same
    // deliberate shape `blade_symv` has: a verified route waiting for a
    // surface. What it pins is the axis a precision-only widening gets wrong,
    // namely that SYMMETRY selects the routine family.
    let mkIx rank sym kind : IRIndexType =
        { Id = 0; Rank = rank; Extent = IRLit (IRLitInt 4L); Symmetry = sym
          Tag = None; Kind = SDimension; IxKind = kind; Dependencies = [] }
    let mkArr (elem: IRType) (ixs: IRIndexType list) : IRArrayType =
        { ElemType = elem; IndexTypes = ixs; IsVirtual = false; Identity = None }
    let packed sym elem = mkArr elem [ mkIx 2 sym IxKPlain ]
    let dense elem = mkArr elem [ mkIx 1 SymNone IxKPlain; mkIx 1 SymNone IxKPlain ]
    let eighCases =
        [ // PACKED, real symmetric -> ?spev, the zero-conversion route.
          "eigh_packed_sym_f64_is_dspev", packed SymSymmetric (IRTScalar ETFloat64),
            Some "blade_lapack::blade_eigh_packed_d"
          "eigh_packed_sym_f32_is_sspev", packed SymSymmetric (IRTScalar ETFloat32),
            Some "blade_lapack::blade_eigh_packed_s"
          // PACKED, Hermitian complex -> ?hpev.
          "eigh_packed_herm_c128_is_zhpev", packed SymHermitian (IRTScalar ETComplex128),
            Some "blade_lapack::blade_eigh_packed_z"
          "eigh_packed_herm_c64_is_chpev", packed SymHermitian (IRTScalar ETComplex64),
            Some "blade_lapack::blade_eigh_packed_c"
          // A REAL Hermitian matrix IS symmetric, so it takes the real packed
          // entry point. A theorem, not a convenience.
          "eigh_packed_herm_f64_is_dspev", packed SymHermitian (IRTScalar ETFloat64),
            Some "blade_lapack::blade_eigh_packed_d"
          // ***THE COMPLEX-SYMMETRIC TRAP.*** A complex array carrying
          // SymSymmetric is complex-SYMMETRIC (A = Aᵀ, no conjugation) — not
          // Hermitian, not normal, and LAPACK has NO eigensolver for it: there
          // is no zsyev and no zspev. Its spectrum is complex and its
          // eigenvectors are not orthogonal, so the right routine is the
          // general zgeev — a different operation with a different result type,
          // not a precision swap. It MUST decline to the native path.
          "eigh_complex_symmetric_DECLINES_c128", packed SymSymmetric (IRTScalar ETComplex128), None
          "eigh_complex_symmetric_DECLINES_c64", packed SymSymmetric (IRTScalar ETComplex64), None
          // Antisymmetric (skew) has no ?spev either: imaginary spectrum.
          "eigh_antisymmetric_declines", packed SymAntisymmetric (IRTScalar ETFloat64), None
          // DENSE (symmetry asserted by the caller, per the eigh surface's own
          // "symmetry is ASSUMED, not checked"): ?syev real, ?heev complex.
          "eigh_dense_f64_is_dsyev", dense (IRTScalar ETFloat64),
            Some "blade_lapack::blade_eigh_dense_d"
          "eigh_dense_f32_is_ssyev", dense (IRTScalar ETFloat32),
            Some "blade_lapack::blade_eigh_dense_s"
          "eigh_dense_c128_is_zheev", dense (IRTScalar ETComplex128),
            Some "blade_lapack::blade_eigh_dense_z"
          "eigh_dense_c64_is_cheev", dense (IRTScalar ETComplex64),
            Some "blade_lapack::blade_eigh_dense_c"
          // No BLAS/LAPACK routine family for integers, at any shape.
          "eigh_int_declines_packed", packed SymSymmetric (IRTScalar ETInt64), None
          "eigh_int_declines_dense", dense (IRTScalar ETInt64), None ]
    for (name, operand, expected) in eighCases do
        // Gate ON: a decline must be about the operand TYPE, not availability.
        use _gate = pinBlas true
        let actual =
            LinAlgPatterns.classifyEigh operand
            |> Option.bind (LinAlgPatterns.shimEntryPoint LinAlgPatterns.HostBlas)
        if actual = expected then
            passed <- passed + 1
            resultLine Pass name (match actual with Some e -> e | None -> "declined -> native")
        else fail name (sprintf "expected %A, got %A" expected actual)

    // The LAPACK gate is its OWN predicate and its own define, even though it
    // resolves through the same OpenBLAS install: a BLAS-only program must not
    // advertise a LAPACK dependency. With the gate off, every eigh route
    // declines — which is what keeps the synthesized Jacobi path the default
    // and the verification truth.
    for (name, operand) in
        [ "eigh_packed_declines_when_gate_off", packed SymSymmetric (IRTScalar ETFloat64)
          "eigh_dense_declines_when_gate_off", dense (IRTScalar ETFloat64) ] do
        use _gate = pinBlas false
        let actual =
            LinAlgPatterns.classifyEigh operand
            |> Option.bind (LinAlgPatterns.shimEntryPoint LinAlgPatterns.HostBlas)
        if actual = None then
            passed <- passed + 1
            resultLine Pass name "declined -> synthesized Jacobi"
        else fail name (sprintf "expected None, got %A" actual)

    // ---- the precision classifier, and the entry-point NAMES it selects ----
    // `precisionOf` is the S|D|C|Z generalisation of the old boolean
    // `isRealDouble`; the letter it yields is appended to every shim entry
    // point, so a wrong answer here is a mis-dispatch by construction. Integers
    // (and everything else with no BLAS routine family) must answer None.
    /// `T<meter>`: the element type a `Unit meter` declaration puts on an
    /// array, built directly so the classifier is tested without a parse.
    let unitOf (et: ElemType) =
        IRTUnitAnnotated (IRTScalar et,
                          { Nominal = Some "meter"
                            Dims = Map.ofList [ "meter", 1 ]
                            Scale = scaleOne })
    let precisionCases =
        [ "prec_f32_is_s",        IRTScalar ETFloat32,    Some LinAlgPatterns.PrecS
          "prec_f64_is_d",        IRTScalar ETFloat64,    Some LinAlgPatterns.PrecD
          "prec_complex64_is_c",  IRTScalar ETComplex64,  Some LinAlgPatterns.PrecC
          "prec_complex128_is_z", IRTScalar ETComplex128, Some LinAlgPatterns.PrecZ
          "prec_int64_is_none",   IRTScalar ETInt64,      None
          "prec_int32_is_none",   IRTScalar ETInt32,      None
          "prec_bool_is_none",    IRTScalar ETBool,       None
          // A UNIT annotation is stripped: `Float64<meter>` is `d`, because it
          // stores as a bare `double` and the wrapper erases at codegen. This
          // used to answer None, which declined every route for every
          // dimensioned array -- silently, since the scalar loops compute the
          // same values. All four letters are pinned, not just `d`, so the
          // stripping cannot be re-narrowed to one width.
          "prec_unit_f64_is_d",   unitOf ETFloat64,       Some LinAlgPatterns.PrecD
          "prec_unit_f32_is_s",   unitOf ETFloat32,       Some LinAlgPatterns.PrecS
          "prec_unit_c64_is_c",   unitOf ETComplex64,     Some LinAlgPatterns.PrecC
          "prec_unit_c128_is_z",  unitOf ETComplex128,    Some LinAlgPatterns.PrecZ
          // Stripping the unit does NOT promote an element type that had no
          // routine family to begin with: a dimensioned integer is still None.
          "prec_unit_int64_is_none", unitOf ETInt64,      None ]
    for (name, ty, expected) in precisionCases do
        let actual = LinAlgPatterns.precisionOf ty
        if actual = expected then
            passed <- passed + 1
            resultLine Pass name (sprintf "%A" actual)
        else fail name (sprintf "expected %A, got %A" expected actual)

    // ====================================================================
    // ROUND D — the DEVICE (cuBLAS) backend
    // ====================================================================
    //
    // Emission-only, exactly like everything above: pinning BLADE_CUBLAS=1
    // makes codegen emit `blade_cuda_*` calls, which needs no CUDA toolkit and
    // no GPU because nothing here is compiled. What a real device then computes
    // is `cpp/cublas_swap_tests.cu`'s job (the swap table, verified against a
    // host reference on real hardware), and the corpus programs' job end to end.
    //
    // Emitted TEXT is the only place the backend choice is visible — host
    // cblas, device cuBLAS and Blade's own loops all agree to within rounding —
    // which is the same argument the precision letter rests on, one axis over.
    // So every case pins the ENTRY-POINT NAME, the dispatch MARKER (which names
    // the backend) and the INCLUDE (which is what Build.fs sniffs to decide
    // whether to run nvcc at all).
    let cudaEmissionCases : (string * bool * bool * string * string list * string list) list =
        let realMat =
            "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
        let realMatB =
            "let B: Array<Float64 like Idx<4>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]\n"
        let vecX = "let x: Array<Float64 like Idx<5>> = [1.0, 2.0, 3.0, 4.0, 5.0]\n"
        let vecY = "let y: Array<Float64 like Idx<5>> = [2.0, 3.0, 4.0, 5.0, 6.0]\n"
        let deferredProd =
            "let P = method_for(zip(x, y)) <@> lambda(a: Float64, b: Float64) -> a * b\n"
        // (name, cublasOn, blasOn, source, mustContain, mustNotContain)
        [ // ---- the three L3 node routes, device ----
          // Note what is asserted absent: `blade_linalg.hpp` and
          // `blade_linalg::`. With the host gate OFF there is no host route to
          // fall back to, so naming either would mean the device arm had leaked
          // a dependency it does not have.
          ("cublas_gram_same_routes_to_device_syrk", true, false,
           realMat + "let G = gram(A, A)\n",
           [ cudaShimInclude; "blade_cuda_gram_same_d("; "cublas dispatch: gram(A, A)"
             "A.data, (A.extents[0] * A.extents[1])" ],
           [ shimInclude; "blade_linalg::"; "cblas_"; "__gacc" ])
          ("cublas_gram_distinct_routes_to_device_gemm", true, false,
           realMat + realMatB + "let G = gram(A, B)\n",
           [ cudaShimInclude; "blade_cuda_gram_distinct_d("; "cublas dispatch: gram(A, B)"
             "A.data, (A.extents[0] * A.extents[1])"
             "B.data, (B.extents[0] * B.extents[1])"
             // Baked output capacity, same rule as the host adapter pin above.
             "G.data, (3 * 4)" ],
           [ shimInclude; "blade_linalg::"; "cblas_"; "blade_cuda_gram_same_" ])
          ("cublas_matmul_routes_to_device_gemm", true, false,
           "import math as m\n" +
           "let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n" +
           "let B: Array<Float64 like Idx<3>, Idx<2>> = [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]\n" +
           "let C = m.matmul(A, B)\n",
           [ cudaShimInclude; "blade_cuda_matmul_d("; "cublas dispatch: matmul(A, B)"
             "C.data, (2 * 2)" ],
           [ shimInclude; "blade_linalg::"; "cblas_"; "__macc" ])
          // COMPLEX same-array gram is HERMITIAN on the device too: `_z` binds
          // cublasZherk, not Zsyrk. The letter is the only place that shows.
          ("cublas_complex_gram_same_routes_to_device_zherk", true, false,
           "let A: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(2.0, 1.0)], [complex(3.0, 0.0), complex(4.0, -1.0)]]\n" +
           "let G = gram(A, A)\n",
           [ cudaShimInclude; "blade_cuda_gram_same_z(" ],
           [ "blade_cuda_gram_same_d"; "blade_linalg::"; "conj_scalar" ])
          // FLOAT32 rides the same table.
          ("cublas_f32_gram_same_routes_to_device_ssyrk", true, false,
           "let A: Array<Float32 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n" +
           "let G = gram(A, A)\n",
           [ cudaShimInclude; "blade_cuda_gram_same_s(" ],
           [ "blade_cuda_gram_same_d"; "blade_linalg::"; "__gacc" ])
          // INTEGER declines on the device exactly as on the host: `precisionOf`
          // answers None, so there is no letter and no routine family, and the
          // program carries NO device dependency at all.
          ("cublas_int_gram_declines_to_native_loops", true, false,
           "let A: Array<Int64 like Idx<3>, Idx<2>> = [[1, 2], [3, 4], [5, 6]]\n" +
           "let G = gram(A, A)\n",
           [ "__gacc" ],
           [ cudaShimInclude; "blade_cuda_"; shimInclude; "blade_linalg::" ])

          // ---- L1 / L2 DECLINE, which is the policy and not a gap ----
          // Both are PCIe-bound under per-call offload, so their CudaBlas rows
          // are Native. With the host gate off there is no other route either,
          // so what must appear is Blade's OWN emitted arithmetic.
          ("cublas_dot_declines_to_native_fold", true, false,
           vecX + vecY + deferredProd + "let s = reduce(P, (+))\n",
           [ "__wrap" ],
           [ cudaShimInclude; "blade_cuda_"; shimInclude; "blade_linalg::" ])
          ("cublas_gemv_declines_to_native_nest", true, false,
           "type M = Idx<3>\ntype N = Idx<4>\n"
           + "let A: Array<Float64 like M, N> = [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]\n"
           + "let xv: Array<Float64 like N> = [1.0, 2.0, 3.0, 4.0]\n"
           + "let yv = method_for(A) <@> lambda(row: Array<Float64 like N>) -> prodsum(row, xv) |> compute\n",
           [ "__ps" ],
           [ cudaShimInclude; "blade_cuda_"; shimInclude; "blade_linalg::" ])

          // ---- THE FALLBACK CHAIN, which is the reason `resolveNodeRoute`
          // exists. Device gate ON *and* host gate ON: the L3 node route takes
          // the device, the L1 route that the device declines still takes the
          // HOST — it is not stripped. A `shimEntryPoint CudaBlas` asked
          // unconditionally would have lost that dot route silently, turning
          // "BLADE_CUBLAS=1" into a regression for every shape the device does
          // not take. Both headers appear, which is also why the two "used"
          // collectors cannot be one cell.
          ("cublas_and_host_gates_compose_per_route", true, true,
           realMat + vecX + vecY + deferredProd
           + "let s = reduce(P, (+))\n"
           + "let G = gram(A, A)\n",
           [ cudaShimInclude; "blade_cuda_gram_same_d("; "cublas dispatch: gram(A, A)"
             shimInclude; "blade_linalg::blade_dot_d("; "linalg dispatch: dot(x, y)" ],
           [ "cblas_" ])

          // ---- GATE OFF: the device backend is INVISIBLE ----
          // Default-off is the whole point of an opt-in performance-model
          // change, so the negative is pinned as its own case rather than
          // inferred from the positives.
          ("cublas_gate_off_gram_keeps_host_route", false, true,
           realMat + "let G = gram(A, A)\n",
           [ shimInclude; "blade_linalg::blade_gram_same_d("; "linalg dispatch: gram(A, A)" ],
           [ cudaShimInclude; "blade_cuda_" ])
          ("cublas_gate_off_gram_keeps_native_loops", false, false,
           realMat + "let G = gram(A, A)\n",
           [ "__gacc" ],
           [ cudaShimInclude; "blade_cuda_"; shimInclude; "blade_linalg::" ]) ]
    for (name, cublasOn, blasOn, src, mustContain, mustNotContain) in cudaEmissionCases do
        match cppOfGates blasOn cublasOn name src with
        | Error e -> fail name e
        | Ok cpp ->
            let missing = mustContain |> List.filter (fun s -> not (cpp.Contains s))
            let present = mustNotContain |> List.filter cpp.Contains
            if not missing.IsEmpty then
                fail name ($"""generated C++ lacks: {(String.concat " | " missing)}""")
            elif not present.IsEmpty then
                fail name ($"""generated C++ unexpectedly contains: {(String.concat " | " present)}""")
            else
                passed <- passed + 1
                resultLine Pass name (sprintf "cublas=%b blas=%b" cublasOn blasOn)

    // ---- the cuBLAS availability gate itself ----
    //
    // Pinned because its DEFAULT is the deliberate part. Unlike its two
    // siblings, unset does NOT fall back to a "is the library installed" probe:
    // offloading changes a program's performance model (v1 pays a device
    // allocation and two transfers per call), so a machine that merely HAS a GPU
    // has not asked for it. The two cases that state this are
    // `cublas_gate_unset_does_not_follow_openblas_dir` — proving it does not
    // ride the sibling variable — and `cublas_gate_unset_is_off`.
    let cublasGateCases =
        [ "cublas_gate_forced_on",      Some "1",   None,                 true
          "cublas_gate_forced_on_word", Some "on",  None,                 true
          "cublas_gate_forced_off",     Some "0",   None,                 false
          "cublas_gate_forced_off_word", Some "off", None,                false
          "cublas_gate_unset_is_off",   None,       None,                 false
          "cublas_gate_unset_does_not_follow_openblas_dir", None, Some "C:\\opt\\blas", false
          // A value that is neither an on- nor an off-word is OFF, not an
          // error: the gate is a switch, and the safe reading of a typo is "do
          // not silently move the program to another machine".
          "cublas_gate_garbage_is_off", Some "yes", None,                 false ]
    for (name, cublasVar, openblasVar, expected) in cublasGateCases do
        let priorC = System.Environment.GetEnvironmentVariable("BLADE_CUBLAS")
        let priorO = System.Environment.GetEnvironmentVariable("OPENBLAS_DIR")
        try
            System.Environment.SetEnvironmentVariable("BLADE_CUBLAS", Option.toObj cublasVar)
            System.Environment.SetEnvironmentVariable("OPENBLAS_DIR", Option.toObj openblasVar)
            let actual = LinAlgPatterns.cublasAvailable ()
            if actual = expected then
                passed <- passed + 1
                resultLine Pass name (sprintf "%b" actual)
            else fail name (sprintf "expected %b, got %b" expected actual)
        finally
            System.Environment.SetEnvironmentVariable("BLADE_CUBLAS", priorC)
            System.Environment.SetEnvironmentVariable("OPENBLAS_DIR", priorO)

    printFooter "LinAlg Dispatch" [$"{passed} passed"; $"{failed} failed"]
    { Block = "LinAlg Dispatch"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }


/// Run the standalone C++ contiguity-probe tests (cpp/linalg_probe_tests.cpp).
///
/// The emission block above proves gram/matmul/dot/gemv REACH the shim. This one
/// proves what the shim's `row_major_base` then decides at runtime —
/// specifically that the degenerate n = 2 packed-symmetric skeleton, whose row
/// starts are identical to a dense 2x2's over a pool one cell shorter, is
/// REFUSED rather than handed to BLAS (docs/plan-cpp-perf-exploitation.md,
/// Phase 5d).
///
/// No value test can see this: the probe's accept and refuse arms are
/// value-identical by construction, so a false accept produces correct output
/// and an out-of-bounds read. The property is a C++ runtime invariant about
/// pointer arithmetic, so it is tested in C++ against the SHIPPED headers —
/// same category and same harness shape as `blade test alloc` (AllocTests.fs).
///
/// NEEDS g++, NOT BLAS. The probe test includes `blade_linalg_views.hpp`, the
/// BLAS-free half of the layer, so it compiles and runs on a machine with no
/// OpenBLAS — which is the whole point of that split. Returns Skipped = 1 when
/// g++ is absent; never fails for toolchain reasons.
let runLinAlgProbeTests () : BlockResult =
    let blockName = "LinAlg Probe"
    printHeader "LinAlg Contiguity Probe"
    let cppDir = Path.Combine(AppContext.BaseDirectory, "cpp")
    let testSrc = Path.Combine(cppDir, "linalg_probe_tests.cpp")
    let caps = capabilities.Value
    if not caps.HasGpp then
        printfn "Skipped: g++ not found (cannot compile C++ probe tests)."
        { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    elif not (File.Exists testSrc) then
        eprintfn "linalg_probe_tests.cpp not found at: %s" testSrc
        eprintfn "Check that Blade.fsproj copies cpp/linalg_probe_tests.cpp to the output dir."
        { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
          FailedNames = ["linalg_probe_tests.cpp missing"] }
    else
        let exeExt = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe" else ".out"
        let exePath = Path.ChangeExtension(testSrc, exeExt)
        // Compile IN cppDir so the `#include "blade_linalg_views.hpp"` resolves
        // to the shipped header the codegen path deploys — a stale copy would
        // defeat the point of running this at all. No -DBLADE_HAS_BLAS and no
        // -I/-l: the views header needs none.
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
        let cExited = cproc.WaitForExit(60000)
        if not cExited then
            (try cproc.Kill(true) with _ -> ())
            printfn "C++ compilation TIMED OUT (60s)"
            printFooter blockName ["FAILED"]
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile timeout>"] }
        elif cproc.ExitCode <> 0 then
            printfn "C++ compilation FAILED:"
            printfn "%s" (cOut.Result + "\n" + cErr.Result)
            printFooter blockName ["FAILED"]
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
            let rExited = rproc.WaitForExit(30000)
            if not rExited then
                (try rproc.Kill(true) with _ -> ())
                rproc.WaitForExit(5000) |> ignore
                printfn "linalg probe test binary TIMED OUT (30s)"
            printf "%s" rOut.Result
            if not (String.IsNullOrWhiteSpace rErr.Result) then eprintf "%s" rErr.Result
            let outText = rOut.Result.Replace("\r\n", "\n")
            let m =
                System.Text.RegularExpressions.Regex.Match(
                    outText, @"LINALG PROBE TESTS:\s*(\d+)/(\d+)\s*passed")
            let pPassed = if m.Success then int m.Groups.[1].Value else 0
            let pTotal = if m.Success then int m.Groups.[2].Value else 0
            let failNames =
                outText.Split('\n')
                |> Array.choose (fun l ->
                    let fm = System.Text.RegularExpressions.Regex.Match(l, @"\[FAIL\]:\s*(.+)$")
                    if fm.Success then Some (fm.Groups.[1].Value.Trim()) else None)
                |> Array.toList
            let pFailed = if pTotal >= pPassed then pTotal - pPassed else failNames.Length
            // Same doctrine as AllocTests: the summary line must be present
            // before an exit 0 is read as a pass, so a binary that aborted
            // before running any check cannot score a vacuous 0/0.
            if not rExited then
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<run timeout>"] }
            elif not m.Success then
                printFooter blockName ["FAILED"]
                printfn "  no 'LINALG PROBE TESTS: p/n passed' summary in output -- cannot confirm any check ran"
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
                  FailedNames = ["<no LINALG PROBE TESTS summary line>"] }
            elif rproc.ExitCode = 0 then
                printFooter blockName ["all passed"]
                { Block = blockName; Passed = pPassed; Failed = 0; Skipped = 0; FailedNames = [] }
            else
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = pPassed; Failed = max 1 pFailed; Skipped = 0
                  FailedNames = (if failNames.IsEmpty then [$"<exit {rproc.ExitCode}>"] else failNames) }


/// The four-tier BLAS/LAPACK resolution (docs/plans/plan-toolchain-packaging.md):
/// pure in-process unit checks of `resolveBlasTier` / `blasAvailable` /
/// `lapackAvailable` / `blasFlavor` / `blasBuildFlags`, plus `Toolchain.get`'s
/// env-over-file precedence. No g++, no BLAS runtime, no filesystem beyond a
/// temp prefix -- always runs.
///
/// Every case pins the WHOLE configuration surface (the six variables plus
/// BLADE_TOOLCHAIN_FILE), because the ambient environment on a dev box
/// legitimately carries OPENBLAS_DIR -- one unpinned variable would make
/// these outcomes machine-dependent. BLADE_TOOLCHAIN_FILE is pinned to a
/// NONEXISTENT path rather than null: null would fall back to a
/// blade.toolchain.json beside the test binary, which a configured machine
/// may genuinely have.
let runBlasTierTests () : BlockResult =
    printHeader "BLAS Tier Resolution"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let check name cond detail =
        if cond then
            passed <- passed + 1
            resultLine Pass name detail
        else
            failed <- failed + 1
            failedNames <- failedNames @ [name]
            resultLine Fail name detail
    use _a = pinEnv "BLADE_BLAS" null
    use _b = pinEnv "BLADE_BLAS_LINK" null
    use _c = pinEnv "OPENBLAS_DIR" null
    use _d = pinEnv "BLADE_LAPACK_LINK" null
    use _e = pinEnv "BLADE_BLAS_FLAVOR" null
    use _f = pinEnv "BLADE_BLAS_INCLUDE" null
    use _g = pinEnv "BLADE_LAPACK_INCLUDE" null
    use _h = pinEnv "BLADE_TOOLCHAIN_FILE" (Path.Combine(Path.GetTempPath(), "blade_no_such_toolchain.json"))

    // ---- tier selection ----
    check "unconfigured -> TierOff, both gates off"
        (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOff
         && not (LinAlgPatterns.blasAvailable ())
         && not (LinAlgPatterns.lapackAvailable ()))
        "default-off preserved"
    do
        use _t = pinEnv "OPENBLAS_DIR" "/nonexistent/openblas-prefix"
        check "OPENBLAS_DIR -> TierOpenBlasDir, both gates on"
            (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOpenBlasDir
             && LinAlgPatterns.blasAvailable ()
             && LinAlgPatterns.lapackAvailable ())
            "prefix shorthand"
    do
        use _t = pinEnv "OPENBLAS_DIR" "/nonexistent/openblas-prefix"
        use _u = pinEnv "BLADE_BLAS" "0"
        check "BLADE_BLAS=0 beats OPENBLAS_DIR -> TierOff"
            (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOff
             && not (LinAlgPatterns.blasAvailable ()))
            "explicit off wins"
    do
        use _t = pinEnv "BLADE_BLAS" "1"
        check "BLADE_BLAS=1 alone -> TierSystem"
            (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierSystem
             && LinAlgPatterns.lapackAvailable ())
            "bare -lopenblas tier"
        check "TierSystem expansion is bare -lopenblas"
            (LinAlgPatterns.blasBuildFlags true false = (" -DBLADE_HAS_BLAS", " -lopenblas"))
            "exact compile/link halves"
    do
        use _t = pinEnv "BLADE_BLAS_LINK" "-lmkl_rt"
        check "BLADE_BLAS_LINK -> TierExplicit, BLAS on"
            (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierExplicit
             && LinAlgPatterns.blasAvailable ())
            "vendor-neutral tier"
        check "TierExplicit without BLADE_LAPACK_LINK -> LAPACK off"
            (not (LinAlgPatterns.lapackAvailable ()))
            "decoupled: BLIS-style BLAS-only install"
        do
            use _u = pinEnv "BLADE_BLAS" "1"
            check "explicit link beats BLADE_BLAS=1 -> TierExplicit"
                (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierExplicit)
                "tier order"
        do
            use _u = pinEnv "BLADE_LAPACK_LINK" "-llapacke_custom"
            check "TierExplicit with BLADE_LAPACK_LINK -> LAPACK on"
                (LinAlgPatterns.lapackAvailable ())
                "lapack gate follows its own link var"
            let (compileHalf, linkHalf) = LinAlgPatterns.blasBuildFlags true true
            check "explicit expansion: defines + verbatim link, BLAS first"
                (compileHalf.Contains "-DBLADE_HAS_BLAS"
                 && compileHalf.Contains "-DBLADE_HAS_LAPACK"
                 && linkHalf = " -lmkl_rt -llapacke_custom")
                "linker order preserved"
    // ---- flavor define ----
    do
        use _t = pinEnv "BLADE_BLAS_LINK" "-lmkl_rt"
        use _u = pinEnv "BLADE_BLAS_FLAVOR" "MKL"
        let (compileHalf, _) = LinAlgPatterns.blasBuildFlags true false
        check "BLADE_BLAS_FLAVOR=MKL (case-insensitive) -> -DBLADE_BLAS_MKL"
            (LinAlgPatterns.blasFlavor () = LinAlgPatterns.FlavorMkl
             && compileHalf.Contains " -DBLADE_BLAS_MKL")
            "header indirection define"
    do
        use _t = pinEnv "BLADE_BLAS_LINK" "-lopenblas"
        let (compileHalf, _) = LinAlgPatterns.blasBuildFlags true false
        check "flavor unset -> no MKL define"
            (LinAlgPatterns.blasFlavor () = LinAlgPatterns.FlavorOpenBlas
             && not (compileHalf.Contains "BLADE_BLAS_MKL"))
            "default flavor is openblas"
    // ---- include-dir lists (PathSeparator-delimited) ----
    do
        use _t = pinEnv "BLADE_BLAS_LINK" "-lmkl_rt"
        use _u = pinEnv "BLADE_BLAS_INCLUDE" ($"/inc/one{Path.PathSeparator}/inc/two")
        let (compileHalf, _) = LinAlgPatterns.blasBuildFlags true false
        check "BLADE_BLAS_INCLUDE list -> one -I per dir"
            (compileHalf.Contains " -I\"/inc/one\"" && compileHalf.Contains " -I\"/inc/two\"")
            "path-separator split"
    // ---- OPENBLAS_DIR expansion against a real prefix ----
    let tmpPrefix = Path.Combine(Path.GetTempPath(), $"blade_tier_prefix_{(System.Diagnostics.Process.GetCurrentProcess().Id)}")
    do
        try
            // Empty prefix: no library file anywhere -> the -L/-l fallback.
            Directory.CreateDirectory(tmpPrefix) |> ignore
            use _t = pinEnv "OPENBLAS_DIR" tmpPrefix
            let (compileHalf, linkHalf) = LinAlgPatterns.blasBuildFlags true false
            check "OpenBlasDir expansion, no lib present -> -L fallback"
                (compileHalf.Contains (sprintf " -I\"%s\"" (Path.Combine(tmpPrefix, "include")))
                 && linkHalf = sprintf " -L\"%s\" -lopenblas" (Path.Combine(tmpPrefix, "lib")))
                "include + -L/-lopenblas"
            // Now materialize the OS-conventional shared library and expect
            // the DIRECT path link (MinGW links a DLL's export table; ld
            // accepts a .so path verbatim).
            let libDir =
                Path.Combine(tmpPrefix, (if Platforms.os = Platforms.Windows then "bin" else "lib"))
            Directory.CreateDirectory(libDir) |> ignore
            let libPath = Path.Combine(libDir, "libopenblas" + Platforms.sharedLibExtension)
            File.WriteAllText(libPath, "")
            let (_, linkHalf2) = LinAlgPatterns.blasBuildFlags true false
            check "OpenBlasDir expansion, lib present -> direct path link"
                (linkHalf2 = $" \"{libPath}\"")
                "Platforms.findSharedLib hit"
        finally
            try Directory.Delete(tmpPrefix, true) with _ -> ()
    // ---- toolchain file: env-over-file precedence ----
    let tmpFile suffix (content: string) =
        let p = Path.Combine(Path.GetTempPath(), $"blade_tier_{suffix}_{(System.Diagnostics.Process.GetCurrentProcess().Id)}.json")
        File.WriteAllText(p, content)
        p
    do
        let fileA = tmpFile "gate_on" "{\"BLADE_BLAS\": \"1\"}"
        try
            use _t = pinEnv "BLADE_TOOLCHAIN_FILE" fileA
            check "toolchain file alone configures the gate"
                (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierSystem)
                "file supplies BLADE_BLAS=1"
            do
                use _u = pinEnv "BLADE_BLAS" "0"
                check "env var beats the toolchain file"
                    (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOff)
                    "env is live, file is fallback"
        finally
            try File.Delete fileA with _ -> ()
    do
        // Non-string members are skipped and malformed JSON degrades to
        // unconfigured -- a broken toolchain file must never crash a check.
        let fileB = tmpFile "nonstring" "{\"BLADE_BLAS\": 1}"
        let fileC = tmpFile "malformed" "not json at all {"
        try
            do
                use _t = pinEnv "BLADE_TOOLCHAIN_FILE" fileB
                check "non-string JSON member is ignored"
                    (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOff)
                    "number-valued BLADE_BLAS skipped"
            do
                use _t = pinEnv "BLADE_TOOLCHAIN_FILE" fileC
                check "malformed toolchain file degrades to unconfigured"
                    (LinAlgPatterns.resolveBlasTier () = LinAlgPatterns.TierOff)
                    "no crash, empty map"
        finally
            try File.Delete fileB with _ -> ()
            try File.Delete fileC with _ -> ()

    printFooter "BLAS Tier Resolution" [$"{passed} passed"; $"{failed} failed"]
    { Block = "BLAS Tier Resolution"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }
