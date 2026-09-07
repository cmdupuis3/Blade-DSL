// Eigensolver dispatch EMISSION + REJECTION tests — Phase 6 / Round B2 of
// docs/plan-cpp-perf-exploitation.md.
//
// Sibling of LinAlgTests, one level up, and it exists for the same reason: the
// routing decision is very nearly invisible in the VALUES, so only the emitted
// text can tell "dispatched to LAPACK" from "ran the synthesized Jacobi". Here
// that gap is wider than it is for BLAS, not narrower — an eigensolver's output
// is not unique (eigenvector signs are arbitrary, and inside a degenerate
// eigenvalue's subspace any orthonormal basis is correct), so the two arms are
// PERMANENTLY outside byte-identity and no value comparison could ever
// distinguish a correct dispatch from a correct non-dispatch.
//
// WHAT THIS BLOCK IS FOR, stated once:
//
//   * gate ON, the (precision × symmetry) route each surface operand reaches —
//     asserted as the entry-point NAME, since `blade_eigh_dense_z` is visibly a
//     `zheev` call and a mis-dispatch shows nowhere else;
//   * the MIXED-ELEMENT tuple a complex operand produces
//     (`std::tuple<Array<std::complex<double>, 2>, Array<double, 1>>`) — Q
//     complex, LAM real, because a Hermitian matrix's eigenvalues are real. It
//     is the first value the surface produces whose tuple elements differ in
//     element type, and typing LAM complex would be a silent storage-width
//     error at the shim boundary rather than a type error;
//   * gate OFF, that the synthesized Jacobi path is BYTE-FOR-BYTE what it was —
//     no marker, no `blade_lapack::`, no include. That arm is the verification
//     truth the corpus and both differentials run, so a regression that started
//     dispatching by default would invalidate every one of them at once;
//   * the SWEEPS rule: an explicit sweep budget keeps Jacobi even with the gate
//     on, because a stated budget is a request for that ALGORITHM and LAPACK has
//     no analogue of it;
//   * the typecheck rejections `inferEigh` owns, including the two symmetry rows
//     that have no LAPACK routine at all.
//
// THE REJECTION CASES SPELL `__math_eigh` DIRECTLY, and that is deliberate.
// `math.eigh`'s sugar cannot reach them: `MathElaborate.arrayShape` resolves a
// declared shape one plain axis at a time and refuses a compact `SymIdx<2, n>`
// operand with BL5200 before any marker is emitted (identically gate-on and
// gate-off — see the packed note below). The marker is the internal spelling the
// elaborator produces, import-gated exactly like `__math_matmul`, so naming it
// is how a test reaches a rule the sugar cannot — and the rules being reached
// are real ones that a widened `arrayShape` would immediately expose.
//
// Pure codegen — no g++, no toolchain, no LAPACK runtime. Always runs: pinning
// BLADE_BLAS=1 makes codegen emit shim CALLS, which needs no OpenBLAS install
// because nothing here is compiled.
module Blade.Tests.LapackTests

open System
open Blade
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

/// Pin the LAPACK availability gate for the duration of one emit, restoring the
/// prior value on exit. Same use-guard idiom as `LinAlgTests.pinBlas`, and it
/// works for the same reason: `LinAlgPatterns.lapackAvailable` is a FUNCTION
/// that re-reads the environment at every consultation, never a module-level
/// binding that would freeze the read at first touch.
///
/// It pins `BLADE_BLAS` because LAPACK rides the same environment resolution —
/// OpenBLAS bundles LAPACKE, so one install answers both — while remaining a
/// separate PREDICATE with a separate define and its own include-sniff arm in
/// Build.fs. That separation is what keeps a gram/matmul program from
/// advertising a LAPACK dependency it does not have, and it is pinned below.
let private pinLapack (on: bool) =
    let prior = Environment.GetEnvironmentVariable("BLADE_BLAS")
    Environment.SetEnvironmentVariable("BLADE_BLAS", (if on then "1" else "0"))
    { new IDisposable with
        member _.Dispose() = Environment.SetEnvironmentVariable("BLADE_BLAS", prior) }

/// Lower + generate under a pinned gate, returning the C++ source.
///
/// THE GATE MUST BE PINNED ACROSS *LOWERING*, not just codegen — a difference
/// from LinAlgTests worth stating, because it is the whole architecture of this
/// round. A BLAS route is decided at EMISSION (`shimEntryPoint`), so pinning
/// around codegen alone would do. The eigh decision is made far earlier, in
/// `MathElaborate`, which runs inside typecheck: gate off, no `__math_eigh`
/// marker is ever produced and the synthesized Jacobi decl is generated instead.
/// `lower` is what runs that stage, so it sits inside the guard.
let private cppOf (lapackOn: bool) (testName: string) (src: string) : Result<string, string> =
    use _gate = pinLapack lapackOn
    try
        match lower src with
        | Error e -> Error ($"lower: {e}")
        | Ok ir -> Ok (fst (CodeGen.genSelfContainedProgramFromIR ir testName))
    with ex -> Error ($"codegen raised: {ex.Message}")

let private lapackInclude = "#include \"blade_lapack.hpp\""

// ----------------------------------------------------------------------------
// Fixtures
// ----------------------------------------------------------------------------

let private symF64 =
    "import math as m\n"
    + "let S: Array<Float64 like Idx<4>, Idx<4>> = [[4.0, 1.0, 0.0, 2.0], [1.0, 3.0, 1.0, 0.0], [0.0, 1.0, 2.0, 1.0], [2.0, 0.0, 1.0, 5.0]]\n"

let private symF32 =
    "import math as m\n"
    + "let S: Array<Float32 like Idx<3>, Idx<3>> = [[2.0, 1.0, 0.0], [1.0, 3.0, 1.0], [0.0, 1.0, 4.0]]\n"

/// A 3x3 system for the `solve` route. n = 3, not a power of two, and A is
/// square-but-not-symmetric on purpose: `?gesv` has no symmetry axis, so an
/// operand that happened to be symmetric would leave "did symmetry accidentally
/// matter" untested.
let private sysF64 =
    "import math as m\n"
    + "let A: Array<Float64 like Idx<3>, Idx<3>> = [[1.0, 1.0, 1.0], [2.0, 4.0, 1.0], [1.0, 0.0, 2.0]]\n"
    + "let bv: Array<Float64 like Idx<3>> = [6.0, 13.0, 7.0]\n"

/// The pivot-scan line the NATIVE LU arm emits and nothing else does. Pinned by
/// name in both directions below, because "did we dispatch" is otherwise
/// invisible for solve in a way it is not for eigh: BOTH arms of solve produce a
/// correct x to ~1e-14, so no value comparison distinguishes them and the
/// emitted text is again the only witness.
let private nativeLuMarker = "size_t __sp = __sk;"

/// THE PIVOT TIE-BREAK, pinned as text. Strict `>` means the FIRST row
/// attaining the maximum magnitude wins; `>=` would take the last. Both
/// factorize correctly and the two differ only in WHICH matrix they factorize,
/// so the difference shows up as a last-digit disagreement between the
/// interpreter and the compiled binary and nowhere else. That makes this
/// one character exactly the kind of thing a text pin is for.
let private nativePivotTieBreak = "if (__sm > __sbig) { __sbig = __sm; __sp = __si; }"

/// A genuinely Hermitian 3x3 (A = A^H): real diagonal, conjugate off-diagonal.
let private hermC128 =
    "import math as m\n"
    + "let S: Array<Complex128 like Idx<3>, Idx<3>> = "
    + "[[complex(2.0, 0.0), complex(1.0, 1.0), complex(0.0, 0.0)], "
    + "[complex(1.0, -1.0), complex(3.0, 0.0), complex(0.0, 1.0)], "
    + "[complex(0.0, 0.0), complex(0.0, -1.0), complex(4.0, 0.0)]]\n"

/// (name, lapackGateOn, source, mustContain, mustNotContain).
///
/// Every case asserts the absence of `LAPACKE_` in the generated text: the
/// compiler NEVER writes a LAPACK call itself, on any machine, with any
/// environment variable set. Which LAPACKE routine runs — and the whole
/// col-major/conj layout bridge — is the header's business; codegen's only
/// decision is dispatch-or-not, and which (precision × symmetry) entry point.
let private emissionCases : (string * bool * string * string list * string list) list =
    [ // ================= GATE ON: the route matrix =================
      // Dense f64 -> ?syev. Also the shape pins for the whole emission design:
      // TWO pools under derived names, one dispatch call taking them as OUT
      // parameters, and the tuple the EXISTING destructuring consumes unchanged
      // (`std::get<0>` / `std::get<1>` are emitted by the pre-existing
      // tuple-binding path, not by anything this round added).
      ("eigh_dense_f64_routes_to_dsyev", true,
       symF64 + "let (Q, LAM) = m.eigh(S)\n",
       [ lapackInclude; "blade_lapack::blade_eigh_dense_d("
         "lapack dispatch: eigh(S) -> (Q, LAM), dense operand"
         // n comes off the operand's extents table, not from a baked literal.
         "blade_lapack::blade_eigh_dense_d(S.extents[0], S.data,"
         "std::tuple<Array<double, 2>, Array<double, 1>>"
         "std::make_tuple("; "std::get<0>("; "std::get<1>(" ],
       // The marker is pre-inference and must not survive; no synthesized
       // `__math_<n>` Jacobi function may be generated either; and a LAPACK
       // program must not drag in the BLAS header.
       [ "LAPACKE_"; "__math_eigh"; "__math_1"; "blade_linalg.hpp" ])
      // Dense f32 -> ?syev at single precision. The letter is appended by
      // `shimEntryPoint` from the classified Precision, so this is the pin that
      // a widening cannot silently get wrong.
      ("eigh_dense_f32_routes_to_ssyev", true,
       symF32 + "let (Q, LAM) = m.eigh(S)\n",
       [ lapackInclude; "blade_lapack::blade_eigh_dense_s("
         "std::tuple<Array<float, 2>, Array<float, 1>>" ],
       [ "LAPACKE_"; "__math_eigh"; "blade_eigh_dense_d(" ])
      // ***THE MIXED-ELEMENT TUPLE.*** A complex Hermitian operand routes to
      // ?heev and produces Q complex beside LAM REAL. Both halves are pinned in
      // one string because they are one fact: `std::complex<double>** V` beside
      // `double* lam` is what the shim's signature says, and a tuple typed
      // `<complex, complex>` would compile and then hand LAPACK a pointer of the
      // wrong width.
      ("eigh_dense_c128_routes_to_zheev_with_real_eigenvalues", true,
       hermC128 + "let (Q, LAM) = m.eigh(S)\n",
       [ lapackInclude; "blade_lapack::blade_eigh_dense_z("
         "std::tuple<Array<std::complex<double>, 2>, Array<double, 1>>" ],
       [ "LAPACKE_"; "__math_eigh"; "blade_eigh_dense_d(" ])
      // PACKED -> ?spev, THE ZERO-CONVERSION ROUTE. Reached through the marker
      // because the sugar cannot express it (see the module header and the
      // dedicated note in the packed-reachability case below). What it pins:
      // the operand goes in as `pool_base(A.data)` — a flat packed pool, read
      // verbatim under COL_MAJOR/'L' by the proven real self-duality — and `n`
      // still comes from `.extents[0]`, which for a rank-2 compact pool holds
      // the LOGICAL extent, not the packed cell count.
      ("eigh_packed_sym_f64_routes_to_dspev", true,
       "let A: Array<Float64 like SymIdx<2, 4>> = fill_random(10)\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       [ lapackInclude; "blade_lapack::blade_eigh_packed_d("
         "lapack dispatch: eigh(S) -> (Q, LAM), packed upper-triangular operand"
         "blade_lapack::blade_eigh_packed_d(A.extents[0], nested_array_utilities::pool_base(A.data),"
         "std::tuple<Array<double, 2>, Array<double, 1>>" ],
       [ "LAPACKE_"; "blade_eigh_dense_" ])

      // ================= SOLVE: THE TWO-REAL-ARMS ROUTE =================
      // Gate ON, `m.solve(A, b)` becomes one `?gesv` call. What this pins
      // beyond the entry point: `n` comes off the operand's extents table, A
      // goes in as its ROW SKELETON and b as its flat pool (the shim owns the
      // column-major bridge), and the BL8007 panic is emitted AT THE CALL SITE
      // from `info` rather than inside the header -- which is how the singular
      // message stays one string shared with the native arm.
      ("solve_f64_routes_to_dgesv", true,
       sysF64 + "let x = m.solve(A, bv)\n",
       [ lapackInclude; "blade_lapack::blade_solve_d("
         "lapack dispatch: solve(A, b) -> x, dense square operand, single right-hand side"
         "blade_lapack::blade_solve_d(x__n, A.data, bv.data, x.data);"
         "blade_rt::panic(\"BL8007\"" ],
       // The marker is pre-inference and must not survive; the NATIVE LU must
       // be gone entirely (this is the half that would silently pass if the
       // gate were consulted at the wrong level and both arms were emitted);
       // and a LAPACK program must not drag in the BLAS header.
       [ "LAPACKE_"; "__math_solve"; nativeLuMarker; "std::fabs("; "blade_linalg.hpp" ])
      // ***THE ARM EIGH DOES NOT HAVE.*** Gate OFF, solve still compiles and
      // still solves -- as Blade's own partial-pivoted LU, named here line by
      // line. For eigh the gate-off arm is synthesized SOURCE and the
      // corresponding assertion is "a `__math_1` function exists"; here there is
      // no function, the loops are emitted inline at the binding, and that
      // difference is the whole design. This arm is also the byte-identity
      // truth `blade test interp math` runs, so a regression that started
      // dispatching by default would invalidate that gate rather than fail it.
      ("solve_gate_off_is_native_lu", false,
       sysF64 + "let x = m.solve(A, bv)\n",
       [ nativeLuMarker; nativePivotTieBreak
         // The singular guard is in BOTH arms and must be, so it is asserted in
         // both cases rather than treated as a dispatch marker.
         "blade_rt::panic(\"BL8007\""
         // The working copy: A is never factorized in place, which is what
         // makes two solves over one A give the same answer twice. Small
         // systems now take that copy on the STACK -- the malloc/free pair was
         // a flat ~57 ns at every n, i.e. most of what a small solve costs
         // (src/microkernels/small_solve.c) -- with the heap path unchanged
         // above the threshold. Asserting the selection line pins BOTH facts:
         // that a working copy exists at all, and that the small-n path avoids
         // the allocation.
         "double x__lu__stk[64];"
         "if (x__n <= 8) { x__lu = x__lu__stk; }" ],
       [ lapackInclude; "blade_lapack::"; "__math_solve"; "LAPACKE_" ])
      // A solve program with the gate on names the LAPACK header and NOT the
      // BLAS one -- the converse of `blas_program_carries_no_lapack_dependency`
      // below, asserted for the second LAPACK routine because `lapackUsedCell`
      // is set from a new emission site and a collector wired to the wrong cell
      // would make every solve program advertise a BLAS dependency.
      ("solve_program_carries_no_blas_dependency", true,
       sysF64 + "let x = m.solve(A, bv)\n",
       [ lapackInclude ],
       [ "#include \"blade_linalg.hpp\""; "blade_linalg::" ])

      // ================= LU: THE FACTORIZATION KEPT =================
      // `m.lu(A)` is `?getrf` and each `m.lu_solve[_t]` is `?getrs` ('N' or
      // 'T'); one factor call for any number of solves. The native markers
      // must be gone, as for solve.
      ("lu_factor_once_routes_to_getrf_getrs", true,
       sysF64 + "let f = m.lu(A)\nlet x = m.lu_solve(f, bv)\nlet y = m.lu_solve_t(f, bv)\n",
       [ lapackInclude; "blade_lapack::blade_lu_d("; "blade_lapack::blade_lu_solve_d("
         "lapack dispatch: lu(A) -> (LU, piv), dense square operand"
         "lapack dispatch: lu_solve(LU, piv, b) -> x, plain"
         "lapack dispatch: lu_solve(LU, piv, b) -> x, transposed"
         "blade_rt::panic(\"BL8007\"" ],
       [ "LAPACKE_"; "__math_lu"; "/* lu factor"; "/* lu solve"; "blade_linalg.hpp" ])
      // Gate OFF: Blade's own factor loop (once) and the two apply loops,
      // the byte-identity truth `blade test interp math` compares.
      ("lu_gate_off_is_native_factor_and_applies", false,
       sysF64 + "let f = m.lu(A)\nlet x = m.lu_solve(f, bv)\nlet y = m.lu_solve_t(f, bv)\n",
       [ "/* lu factor: partial-pivoted, multipliers stored below the diagonal, pivot rows in piv */"
         "/* lu solve: pivots, then L (unit lower), then U */"
         "/* lu solve transposed: U^T (forward), L^T (backward), then the pivots undone */"
         nativePivotTieBreak
         "blade_rt::panic(\"BL8007\"" ],
       [ lapackInclude; "blade_lapack::"; "__math_lu"; "LAPACKE_" ])

      // ================= GATE ON, BUT DECLINED =================
      // An explicit SWEEPS budget keeps the synthesized Jacobi EVEN WITH THE
      // GATE ON. A stated sweep count is a request for the cyclic-Jacobi
      // algorithm, and LAPACK's blocked tridiagonal reduction has no analogue of
      // it — routing anyway would silently ignore a parameter the user typed.
      // This is the only case in the file where the gate is on and no route
      // fires, so it is also what proves the elaborator's condition is a
      // CONJUNCTION rather than the gate alone.
      ("eigh_explicit_sweeps_keeps_jacobi_even_gate_on", true,
       symF64 + "let (Q, LAM) = m.eigh(S, 20)\n",
       [ "std::tuple<Array<double, 2>, Array<double, 1>> __math_1(" ],
       [ lapackInclude; "blade_lapack::"; "__math_eigh"; "LAPACKE_" ])

      // ================= GATE OFF: the verification truth =================
      // The SAME program with the gate off must be the synthesized Jacobi, and
      // must name nothing LAPACK-related at all. This is the load-bearing pin of
      // the whole round: the corpus, `blade test interp math` and `blade test
      // diff-oracle math` all run this arm, and an eigensolver's output is not
      // unique, so if dispatch ever leaked into the default build those suites
      // would not merely fail — they would stop meaning anything.
      ("eigh_gate_off_is_synthesized_jacobi", false,
       symF64 + "let (Q, LAM) = m.eigh(S)\n",
       [ "std::tuple<Array<double, 2>, Array<double, 1>> __math_1(" ],
       [ lapackInclude; "blade_lapack::"; "__math_eigh"; "LAPACKE_" ])
      // Complex gate-off behaves the same way. Worth its own case because the
      // gate-ON complex route is the one that changes the RESULT TYPE (mixed
      // tuple), so "gate off is unchanged" has to be asserted for it separately
      // rather than inferred from the f64 row.
      ("eigh_gate_off_complex_is_synthesized_jacobi", false,
       hermC128 + "let (Q, LAM) = m.eigh(S)\n",
       [ "__math_1" ],
       [ lapackInclude; "blade_lapack::"; "__math_eigh" ])

      // ================= DEPENDENCY SURFACE =================
      // A BLAS program must not advertise a LAPACK dependency. `lapackAvailable`
      // is a separate PREDICATE, `-DBLADE_HAS_LAPACK` a separate define,
      // `lapackUsedCell` a separate collector, and this is what all three are
      // for: with the gate fully ON, a gram program still names only the BLAS
      // header. Sharing one collector would make every gram program include the
      // eigensolver header and every eigh program include the BLAS one.
      ("blas_program_carries_no_lapack_dependency", true,
       "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
       + "let G = gram(A, A)\n",
       [ "#include \"blade_linalg.hpp\""; "blade_linalg::blade_gram_same_d(" ],
       [ lapackInclude; "blade_lapack::" ])
      // ...and the converse: an eigh program names only the LAPACK header. Pinned
      // in the dense f64 case above via `blade_linalg.hpp` in mustNotContain; a
      // program with NEITHER carries neither, which is the third state.
      ("plain_program_carries_neither_dependency", true,
       "let A: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n"
       + "let s = reduce(A, (+))\n",
       [],
       [ lapackInclude; "blade_lapack::"; "#include \"blade_linalg.hpp\""; "blade_linalg::" ]) ]

/// (name, source, expectedMessageFragment) — typecheck REJECTIONS owned by
/// `TypeCheck.inferEigh`, all run with the gate ON so a refusal is provably
/// about the OPERAND and not about availability.
///
/// The admissibility rule delegates to `LinAlgPatterns.classifyEigh` rather than
/// restating the (precision × symmetry) matrix — the point of that matrix being
/// that symmetry selects the routine FAMILY, and that one row of it has no
/// routine at all. What `inferEigh` adds is the MESSAGE: each decline names its
/// own reason, derived from the operand's own index record, instead of one
/// generic "unsupported". These pins are what stop that from rotting back into a
/// single catch-all.
let private rejectionCases : (string * string * string) list =
    [ // ***THE COMPLEX-SYMMETRIC TRAP.*** A complex array carrying SymSymmetric
      // is complex-SYMMETRIC (A = A^T, no conjugation): not Hermitian, not
      // normal, complex spectrum, non-orthogonal eigenvectors. LAPACK has NO
      // eigensolver for it — there is no zsyev and no zspev — so the right
      // routine is the general zgeev, a different operation with a different
      // result TYPE. This is the row a precision-only widening gets wrong,
      // exactly as `zsyrk`-for-`zherk` would at the BLAS level, and it must be a
      // named refusal rather than a quiet re-binding to the Hermitian route.
      ("eigh_rejects_complex_symmetric",
       "let A: Array<Complex128 like SymIdx<2, 3>> = fill_random(6)\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       "COMPLEX SYMMETRIC")
      // Antisymmetric (skew) has a purely imaginary spectrum: no ?spev either.
      ("eigh_rejects_antisymmetric",
       "let A: Array<Float64 like AntisymIdx<2, 4>> = fill_random(6)\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       "ANTISYMMETRIC")
      // No BLAS/LAPACK routine family for integers, at any shape: `precisionOf`
      // answers None, so there is no letter and no family to dispatch to.
      ("eigh_rejects_integer_elements",
       "let A: Array<Int64 like Idx<3>, Idx<3>> = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       "no eigensolver")
      // Rank-1 is not a matrix.
      ("eigh_rejects_rank1",
       "let A: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       "must be rank-2 square")
      // Rank-2 but not square. Checked only when BOTH extents are statically
      // known, which is the same discipline `inferMatmul`'s contracted-extent
      // check uses — unify never compares extents, so a static disagreement is
      // the only kind this seam can see.
      ("eigh_rejects_non_square",
       "let A: Array<Float64 like Idx<3>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]\n"
       + "let (Q, LAM) = __math_eigh(A)\n",
       "must be SQUARE")

      // ---- the rules `TypeCheck.inferSolve` owns ----
      //
      // Spelled through the `__math_solve` marker for the same reason the eigh
      // rows are: `m.solve`'s sugar rejects these earlier, from the DECLARED
      // shapes, with BL5200 (corpus math/072, /073 pin that surface). These
      // rows reach the checker's own copy of the rule -- the one a hand-written
      // marker, or a future widening of `arrayShape`, would meet.
      //
      // Unlike eigh's rejections, none of these is about symmetry: `?gesv`
      // factorizes any square matrix, so what is left to refuse is exactly
      // shape and element type, and each is named separately.
      ("solve_rejects_non_float64_elements",
       "let A: Array<Complex128 like Idx<2>, Idx<2>> = [[complex(1.0, 0.0), complex(0.0, 0.0)], [complex(0.0, 0.0), complex(1.0, 0.0)]]\n"
       + "let bv: Array<Complex128 like Idx<2>> = [complex(1.0, 0.0), complex(2.0, 0.0)]\n"
       + "let x = __math_solve(A, bv)\n",
       "Float64 elements")
      // A rank-2 COMPACT operand: one index slot of arity 2, not two plain
      // axes. Declined rather than densified, because LU overwrites its copy
      // with a factor that is NOT symmetric -- a packed pool is the wrong shape
      // to hold it, and quietly staging a dense copy would be a storage-class
      // change behind the user's back. `decompact` first.
      ("solve_rejects_compact_matrix",
       "let A: Array<Float64 like SymIdx<2, 3>> = fill_random(6)\n"
       + "let bv: Array<Float64 like Idx<3>> = [1.0, 2.0, 3.0]\n"
       + "let x = __math_solve(A, bv)\n",
       "rank-2 dense SQUARE matrix")
      // A rank-2 right-hand side. THE MATRIX-RHS CASE, refused by name rather
      // than by falling through a generic shape message: `?gesv` already takes
      // `nrhs`, so this is a recorded not-yet rather than an impossibility, and
      // the refusal is where it will be lifted from.
      ("solve_rejects_matrix_rhs",
       "let A: Array<Float64 like Idx<2>, Idx<2>> = [[1.0, 0.0], [0.0, 1.0]]\n"
       + "let B: Array<Float64 like Idx<2>, Idx<2>> = [[1.0, 2.0], [3.0, 4.0]]\n"
       + "let x = __math_solve(A, B)\n",
       "b must be a rank-1 dense vector")
      // Square A, wrong-length b. The checker's own copy of the agreement rule
      // (corpus math/073 pins the elaborator's).
      ("solve_rejects_extent_disagreement",
       "let A: Array<Float64 like Idx<3>, Idx<3>> = [[2.0, 1.0, 0.0], [1.0, 3.0, 1.0], [0.0, 1.0, 2.0]]\n"
       + "let bv: Array<Float64 like Idx<4>> = [1.0, 2.0, 3.0, 4.0]\n"
       + "let x = __math_solve(A, bv)\n",
       "b's extent must match A's dimension")
      // The apply half's own copy of the agreement rule (corpus math/084 pins
      // the surface spelling).
      ("lu_solve_rejects_extent_disagreement",
       "let LU: Array<Float64 like Idx<3>, Idx<3>> = [[2.0, 4.0, 1.0], [0.5, -2.0, 1.5], [0.5, 0.5, -0.25]]\n"
       + "let piv: Array<Int64 like Idx<3>> = [1, 2, 2]\n"
       + "let bv: Array<Float64 like Idx<4>> = [1.0, 2.0, 3.0, 4.0]\n"
       + "let x = __math_lu_solve(LU, piv, bv)\n",
       "b's extent must match the factor's dimension") ]

let runLapackEmissionTests () : BlockResult =
    printHeader "LAPACK Eigensolver Dispatch"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    let fail name detail =
        failed <- failed + 1
        failedNames <- failedNames @ [name]
        resultLine Fail name detail

    for (name, gateOn, src, mustContain, mustNotContain) in emissionCases do
        match cppOf gateOn name src with
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
                resultLine Pass name (if gateOn then "routing as expected (gate on)"
                                      else "synthesized Jacobi as expected (gate off)")

    for (name, src, expectedFragment) in rejectionCases do
        match cppOf true name src with
        | Ok cpp ->
            fail name (sprintf "expected rejection %A, but the program compiled (names blade_lapack: %b)"
                           expectedFragment (cpp.Contains "blade_lapack::"))
        | Error e when e.Contains expectedFragment ->
            passed <- passed + 1
            resultLine Pass name "rejected at typecheck (gate on)"
        | Error e -> fail name ($"rejected, but not for the pinned reason: {e}")

    // ---- the packed route's REACHABILITY, recorded as a test ----
    //
    // `math.eigh` cannot express a compact operand: `MathElaborate.arrayShape`
    // resolves a declared shape one plain axis at a time, so a `SymIdx<2, n>`
    // argument is refused with BL5200 before any marker is emitted. Pinned HERE,
    // with the gate ON and OFF, because the two halves are separate claims and
    // only the second one is reassuring:
    //
    //   * the route exists and works (the `eigh_packed_sym_f64_routes_to_dspev`
    //     emission case above proves it, via the marker); and
    //   * the surface refusal is NOT gate-dependent — identical message either
    //     way — so this round introduced no configuration-dependent difference
    //     in which programs build.
    //
    // That makes packed eigh the same shape `blade_symv` already has: a verified
    // route waiting on a surface. Teaching `arrayShape` compact axes is the one
    // change that lands it, and this test is what will notice.
    let packedSurfaceSrc =
        "import math as m\n"
        + "let A: Array<Float64 like SymIdx<2, 4>> = fill_random(10)\n"
        + "let (Q, LAM) = m.eigh(A)\n"
    for (name, gateOn) in
        [ "packed_surface_refused_by_arrayShape_gate_on", true
          "packed_surface_refused_by_arrayShape_gate_off", false ] do
        match cppOf gateOn name packedSurfaceSrc with
        | Ok _ -> fail name "expected BL5200 from MathElaborate.arrayShape, but the program compiled"
        | Error e when e.Contains "statically known" ->
            passed <- passed + 1
            resultLine Pass name ($"""refused identically (gate {(if gateOn then "on" else "off")})""")
        | Error e -> fail name ($"refused, but not for the pinned reason: {e}")

    // ---- the availability gate itself ----
    //
    // Pinned because it is shared THREE ways now — `shimEntryPoint` (which route
    // name, if any), `MathElaborate` (marker or synthesized source), and
    // Build.fs (`-DBLADE_HAS_LAPACK` plus the include/link flags). Three readers
    // of one predicate is exactly the configuration where a semantics change
    // goes unnoticed in two of them; and the failure mode is not subtle, it is a
    // program that emits `blade_lapack::` calls into a header whose own
    // `#ifndef BLADE_HAS_LAPACK / #error` then refuses to compile.
    let gateCases =
        [ "lapack_gate_forced_on",        Some "1",   None,                 true
          "lapack_gate_forced_on_word",   Some "on",  None,                 true
          "lapack_gate_forced_off",       Some "0",   Some "C:\\opt\\blas", false
          "lapack_gate_forced_off_word",  Some "off", Some "C:\\opt\\blas", false
          "lapack_gate_unset_follows_openblas_dir", None, Some "C:\\opt\\blas", true
          "lapack_gate_unset_without_openblas_dir", None, None,             false ]
    for (name, blasVar, openblasVar, expected) in gateCases do
        let priorB = Environment.GetEnvironmentVariable("BLADE_BLAS")
        let priorO = Environment.GetEnvironmentVariable("OPENBLAS_DIR")
        try
            Environment.SetEnvironmentVariable("BLADE_BLAS", Option.toObj blasVar)
            Environment.SetEnvironmentVariable("OPENBLAS_DIR", Option.toObj openblasVar)
            let actual = LinAlgPatterns.lapackAvailable ()
            if actual = expected then
                passed <- passed + 1
                resultLine Pass name (sprintf "%b" actual)
            else fail name (sprintf "expected %b, got %b" expected actual)
        finally
            Environment.SetEnvironmentVariable("BLADE_BLAS", priorB)
            Environment.SetEnvironmentVariable("OPENBLAS_DIR", priorO)

    // ---- the Eigh policy row ----
    // Read with no gate pin, deliberately: `routingOf` states whether a shape is
    // WORTH a library call, which is a property of the routine and must read the
    // same on a machine with OpenBLAS and one without. Availability enters one
    // level down, in `shimEntryPoint`. The CudaBlas row is pinned so landing
    // cuSOLVER is a deliberate table edit rather than a silent flip.
    let policyCases =
        [ "host_eigh_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Eigh, LinAlgPatterns.ViaShim
          "cuda_eigh_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Eigh, LinAlgPatterns.Native
          // Solve's rows read the same as Eigh's, and mean something weaker:
          // `Native` here is a real fallback (Blade's own emitted LU), not the
          // absence of the operation. Pinned so landing cuSOLVER's
          // getrf/getrs pair is a deliberate table edit.
          "host_solve_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Solve, LinAlgPatterns.ViaShim
          "cuda_solve_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Solve, LinAlgPatterns.Native
          // The factor-once pair (?getrf / ?getrs), read as Solve's rows are.
          "host_getrf_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Getrf, LinAlgPatterns.ViaShim
          "host_getrs_via_shim", LinAlgPatterns.HostBlas, LinAlgPatterns.Getrs, LinAlgPatterns.ViaShim
          "cuda_getrf_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Getrf, LinAlgPatterns.Native
          "cuda_getrs_native",   LinAlgPatterns.CudaBlas, LinAlgPatterns.Getrs, LinAlgPatterns.Native ]
    for (name, backend, routine, expected) in policyCases do
        let actual = LinAlgPatterns.routingOf backend routine
        if actual = expected then
            passed <- passed + 1
            resultLine Pass name (sprintf "%A" actual)
        else fail name (sprintf "expected %A, got %A" expected actual)

    printFooter "LAPACK Eigensolver Dispatch" [$"{passed} passed"; $"{failed} failed"]
    { Block = "LAPACK Eigensolver Dispatch"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = failedNames }
