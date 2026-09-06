// The combined test corpus (`allTests`) and the full-suite entry points.
// Extracted from Main.fs (audit §2.3). The OpenMP-coverage and CUDA blocks
// are OPT-IN here: CUDA needs nvcc + a GPU + (on Windows) cl.exe on PATH —
// i.e. a run from the "x64 Native Tools Command Prompt for VS" — and the
// OpenMP block forces multi-threaded runs. The MPI, timing, interpreter-
// differential and pinned-oracle blocks are opt-in for the same reason:
// each needs a resource the build does not produce (mpiexec, a quiet machine,
// a second full pipeline per test, a pinned ./oracle/Blade.exe).
// Everything else — including every pure in-process F# unit block — always
// runs, so the grand total is the whole cheap suite by default.
module Blade.Tests.RunAll

open Blade.Tests.Basic
open Blade.Tests.Loops
open Blade.Tests.Symmetry
open Blade.Tests.Reynolds
open Blade.Tests.Arity
open Blade.Tests.Functions
open Blade.Tests.Structs
open Blade.Tests.SumTypes
open Blade.Tests.Interfaces
open Blade.Tests.Modules
open Blade.Tests.Guards
open Blade.Tests.Combinators
open Blade.Tests.Tuples
open Blade.Tests.RecursiveArrays
open Blade.Tests.StackJoin
open Blade.Tests.Bracketed
open Blade.Tests.IndexTypes
open Blade.Tests.Mutability
open Blade.Tests.Static
open Blade.Tests.Ppl
open Blade.Tests.Math
open Blade.Tests.Rand
open Blade.Tests.Display
open Blade.Tests.Spectra
open Blade.Tests.Fallback
open Blade.Tests.Sgs
open Blade.Tests.Units
open Blade.Tests.Sqlish
open Blade.Tests.InferenceProbes
open Blade.Tests.FuncArrays
open Blade.Tests.Normalize
open Blade.Tests.Unify
open Blade.Tests.ValidateArrow
open Blade.Tests.ExprAttrs
open Blade.Tests.CodeGenSubst
open Blade.Tests.Runner
open Blade.Tests.AllocTests
open Blade.Tests.OmpTests
open Blade.Tests.CudaTests
open Blade.Tests.Differential
open Blade.Tests.Benchmarks

// ============================================================================
// Test Collections
// ============================================================================

/// memfree — scope-exit deallocation hazards. Each member pins VALUES that
/// must be invariant under the deterministic-free change: per-iteration
/// combinator temps, aliases and views of them, captured muts, escaping
/// results, deferred forces, packed symmetric temps, and nested recursions.
/// Split from memfree-stress so this half can ride the interpreter
/// differential gate (InterpDiff.currentSlice). Declared here (not in a
/// Test_*.fs) so the category lives next to its one consumer.
let memfreeTests = Blade.Tests.Corpus.category "memfree"

/// memfree-stress — the allocation-churn MEMORY gate (011): ~30k iterations
/// each materializing a 2 MiB temp. Its own category because the interpreter
/// must NOT walk it (see InterpDiff.currentSlice) and because it OOMs by
/// construction until scope-exit frees land.
let memfreeStressTests = Blade.Tests.Corpus.category "memfree-stress"

/// deferred-concrete — `<$>` / `<|>` / sequence / guard applied to CONCRETE
/// arrays, i.e. the combinators forced through the value pipeline rather than
/// left deferred. The category existed only inside InterpDiff.currentSlice, so
/// its seven files ran ONLY under the opt-in interpreter gate and never in a
/// default `blade test` — the compiled half of the differential was never
/// exercised on its own. Declared here (not in a Test_*.fs) for the same reason
/// memfree is: the category's consumers are `allTests` below and the interp
/// slice, so it lives next to the former. Reachable from Cli.fs as
/// `Blade.Tests.RunAll.deferredConcreteTests`, like memfree.
let deferredConcreteTests = Blade.Tests.Corpus.category "deferred-concrete"

/// The wholly-negative categories carry no "(rejects)" marker in their own
/// `// TEST:` names -- every file in them is meant to be refused, so the marker
/// would be noise -- and Runner's classifier keys on exactly that marker. The
/// STANDALONE verbs supply it (`blade test uniterrors` and friends wrap the
/// list in Cli.fs's `asRejectProbes`), and the full-suite lane below did not:
/// the same files were read as ORDINARY tests, so being correctly refused at
/// lowering counted as a failure. That is why nine unit-errors probes were red
/// in `[All]` while `blade test uniterrors` reported 14/14 green.
///
/// Kept identical to Cli.fs's helper on purpose; if one grows a rule the other
/// must too, or the two lanes disagree again.
let private asRejectProbes (tests: (string * string) list) =
    tests
    |> List.map (fun (name, source) ->
        (if name.EndsWith "(rejects)" then name else name + " (rejects)"), source)

/// All tests combined
let allTests =
    basicTests @ intrinsicsTests @ castsTests @ adTests @ adJvpTests @ adJvpCombTests @ mlE2eTests @ mlOpsTests @ mlEquivTests @ loopTests @ symmetryTests @ reynoldsTests @ arityTests @ functionTests
    @ structTests @ structAbortTests @ structMutualTests @ sumTypeTests @ interfaceTests @ moduleTests @ guardTests @ guardCombinatorTests @ zeroCombinatorTests @ sequenceCombinatorTests @ tupleViewTests @ tupleTests @ replicateTests @ anonRangeTests @ recursiveArrayTests @ bracketedTests
    @ indexTypeTests @ mutabilityTests @ asRejectProbes mutabilityErrorTests @ staticTests @ pplTests @ mathTests @ randTests @ displayTests @ asRejectProbes displayErrorTests @ spectraTests @ fallbackTests @ stackJoinTests @ sgsTests @ unitTests @ asRejectProbes unitErrorTests
    @ foreignKeyTests @ maskTests @ setOpTests @ uniqueContainsTests @ semijoinTests @ groupByTests @ sortTests @ reduceTests @ extentsTests @ extentsMultiRankTests @ regressionTests @ sqlCombinedTests @ v24dProbes
    @ inferenceProbes
    @ funcArrayTests
    @ deferredConcreteTests
    @ memfreeTests @ memfreeStressTests

/// Which optional, toolchain-heavy blocks the full suite should include.
/// All default to OFF: the CUDA block needs the x64 Native Tools prompt on
/// Windows, the OpenMP-coverage block forces multi-threaded runs, and the
/// differential-timing block compiles and repeatedly runs large programs
/// (it dominates the suite's wall time). The two differential gates are off
/// for the same reason plus an external-resource one: the interpreter gate
/// compiles AND interprets the whole supported corpus slice (two full
/// pipelines per test), and the pinned-oracle gate needs a second Blade build
/// sitting at ./oracle/Blade.exe that only a release-gating workflow pins.
/// Enable with `blade test --omp --cuda --timing --mpi --interp --diff-oracle`,
/// or run the blocks standalone (`blade test omp-coverage`, `blade test cuda`,
/// `blade test timing`, `blade test mpi`, `blade test interp`,
/// `blade test diff-oracle`).
type FullSuiteOptions = {
    IncludeOmp        : bool
    IncludeCuda       : bool
    IncludeTiming     : bool
    IncludeMpi        : bool
    IncludeInterpDiff : bool
    IncludeDiffOracle : bool
}

let defaultFullSuiteOptions =
    { IncludeOmp = false
      IncludeCuda = false
      IncludeTiming = false
      IncludeMpi = false
      IncludeInterpDiff = false
      IncludeDiffOracle = false }

/// Includes both the single-file test corpus (`allTests`) and the multi-file
/// module/import corpus (`multiFileTests`). External-dependency tests
/// (NetCDF provider tests in particular) are NOT included here — they have
/// their own entry point because they require `libnetcdf` and a sample data
/// file that may not be present in CI / local dev environments.
///
/// `extraBlocks` lets a LATER-compiled module contribute blocks to the grand
/// total: the CLI smoke test lives in Cli.fs (it exercises Cli.compileToExe),
/// which the F# compile order places after this file, so it cannot be
/// referenced here directly. Cli.fs passes it in.
let runAllTestsFullWith (extraBlocks: (unit -> Blade.Tests.TestHarness.BlockResult) list) (opts: FullSuiteOptions) =
    let outputDir = "./generated_cpp_tests"
    let r1 = runTestCategoryFull "All" allTests outputDir
    let r2 = runMultiFileTestsFull "Multi-File Modules" multiFileTests outputDir
    // Phase B: F# unit tests for the exprAttrs computation. Runs after
    // the source-program tests; reports separately so it doesn't muddy
    // the source-test counts.
    let attrs = runAttrsTests ()
    // Phase C Step 2: F# unit tests for the codegen substitution mechanism.
    let subst = runCodeGenSubstTests ()
    // IR-level F# unit tests for the type normalizer (splitting mixed-kind
    // groups, idempotence, flat-vs-nested equivalence). Constructs IRType
    // values directly and calls normalize — no Blade source, no C++ toolchain —
    // so it belongs with the other in-process unit blocks and always runs.
    // (Also reachable standalone as `blade test normalize`.)
    let normalize = runNormalizeTests ()
    // Display frames (Blade-REPL/docs/display-frames.md): the frame BYTES, the
    // escape table, and the two channels -- the REPL sentinel line on raw
    // interpreter stdout and `ide serve`'s display array on the real response
    // encoder. Drives the interpreter and the session engine directly, no g++,
    // so it runs unconditionally beside the other in-process blocks.
    // (Also reachable standalone as `blade test display-frames`.)
    let displayFrames = runDisplayTests ()
    // The GR render lane (Blade-REPL/docs/gr-graphics-plan.md 4.2): the
    // renderPlot frame bytes, the request's argument rules, and the worker
    // protocol against a fake helper this block writes itself. No GR, no g++,
    // no editor -- the single case that needs a real gr-render + GR install
    // reports Skip until both exist. (Also `blade test gr-render`.)
    let grRender = Blade.Tests.GrRender.runGrRenderTests ()
    // TypeCheck-level F# unit tests for the unify §5.3 fast path: flat-vs-split
    // arrows, inference-var binding, dist-type ordering/axis-tag rejection.
    // Same shape as the normalize block — pure IRType construction plus a call
    // to unify, so it is cheap and unconditional here.
    // (Also reachable standalone as `blade test unify`.)
    let unify = runUnifyTests ()
    // IR-level F# unit tests for the validateArrowShape gate at
    // mkVirtualArrayArrow entry: which array/arrow shapes the constructor must
    // refuse. Pure in-process construction, no pipeline, so it runs alongside
    // the normalize/unify blocks rather than only on demand.
    // (Also reachable standalone as `blade test validate-arrow`.)
    let validateArrow = runValidateArrowTests ()
    // Canonical ExprShape traversal: round-trips, walker completeness (§3.2).
    let shape = Blade.Tests.Shape.runShapeTests ()
    // Oracle review: differential-harness oracles vs hand-computed truth (Phase 0.2).
    let oracles = Blade.Tests.OracleReview.runOracleTests ()
    // The OrbIdx bijection layer (OrbRank.fs, plan-orbidx-bijections Phase 2):
    // the segment-peeled traversal stream and the arithmetic rank/unrank pair,
    // pinned against a brute-force canonicalization of every raw tuple — as a
    // SET and as an ORDER, since §3's hard constraint is that rank order = the
    // nest's visit order and a read->write roundtrip cannot catch an order
    // mismatch. Plus the hand-unrolled depth-2 E/B/A nest, the depth-1
    // triangular-offset anchors, and the int64 wall at depth 3 / n = 1000.
    let orbRank = Blade.Tests.OrbRankReview.runOrbRankTests ()
    // Compiler-native CG tables (WignerTables.fs) vs closed forms (ML arc).
    let wigner = Blade.Tests.WignerTablesReview.runWignerTablesTests ()
    // Sym^j(V_l) occurrence tables (SymPowerTables.fs, stage 2b-i): exact
    // rational E-kernel/Gram re-verification, counts vs the weight-peel,
    // the derived realization phase rule, bit-pins, and the extended
    // realCG completeness pins for the k ≤ 4 chain range.
    let symPower = Blade.Tests.SymPowerTablesReview.runSymPowerTablesTests ()
    // The Sym^k label-basis ORACLE (stage 2b-iii): the convention's own
    // vectors (polyLabels + T_{j,l} + CG chains + the sector constant) vs
    // Casimir-Lagrange isotypic projectors re-derived exactly and
    // independently, plus the k = 2 value-level M-pin against stage 1.
    let polyOracle = Blade.Tests.PolyOracleReview.runPolyOracleTests ()
    // The so(3) GENERATOR TABLES and the radical-vector discharge
    // (MLLieDischarge.fs, stage 6c). Keystone first: the EXP-PIN closes the
    // convention loop by exponentiating the exact tables and comparing against
    // the Wigner action fit from an independent transcription of the real
    // solid harmonics (the same object Rotations.applyRep performs). Then the
    // exact algebra (skew-symmetry per radical component, the so(3) brackets
    // and the Casimir, all l <= 4), the known answers incl. the
    // triple-product triple and the |x|^2·x thesis pin, the three negative
    // controls, and the composition-vs-engine differential.
    let lieTables = Blade.Tests.LieTablesReview.runLieTablesTests ()
    // The Sn permutation-module counting layer (MLPermSpec.fs, stage 5a-i):
    // RGS partition enumeration vs the Stirling recurrence vs an independent
    // block-insertion route, the witness-unitriangularity certificate (the
    // Coq keystone's numerical shadow), and the perm_weight_dim /
    // perm_bias_dim sizing arithmetic. Pure integer, no pipeline.
    let permSpec = Blade.Tests.PermSpecReview.runPermSpecTests ()
    // The coarsening-indicator COMPLETENESS oracle (stage 5a-ii): the exact
    // rational Reynolds projector (1/N!)Σ_σ M(σ)^⊗m against B(BᵀB)⁻¹Bᵀ over ℚ,
    // with the closed-form Gram N^{b(γ∨π)} predicted from an independent
    // union-find join. Entrywise equality of BigInteger fractions — no float
    // and no tolerance anywhere, which is the half BladePartition.v cites
    // rather than proves (completeness of the orbit basis).
    let permOracle = Blade.Tests.PermOracleReview.runPermOracleTests ()
    // The constrained-record COUNTING layer (StructIdxSpec.fs, stage C1 of
    // the retired constrained-index-types plan §7): box enumeration over the per-field
    // inclusive bounds with the flat-filter vs arrow-heads certificate (set
    // AND order — order agreement is what catches an offset bug), the CGm112
    // anchor and its 3/7/9 lo-sweep against an independent triple-loop dense
    // count, the fence and `idx_card(R)` end to end through resolveStatics,
    // the negative controls (box cap, non-Int field, unbounded field,
    // non-`static struct`, and the fuel bomb with its witness cell), and the
    // shared fold budget itself - depth vs steps, the wide-but-shallow fold
    // only a step bound catches, and the idx_card cycle a syntactic builtin
    // can open through its own re-entry.
    let structIdxSpec = Blade.Tests.StructIdxSpecReview.runStructIdxSpecTests ()
    // The INDEPENDENT third route over the same solution sets: a separately
    // coded recursive per-field enumerator compared against StructIdxSpec's
    // entries as SET and as ORDER, plus hand-written lex tables so that two
    // agreeing programs can still be caught being wrong together.
    let structIdxOracle = Blade.Tests.StructIdxOracle.runStructIdxOracleTests ()
    // The point-group counting layer (MLPointSpec.fs, stage 5b-0): the frozen
    // {C4, D4} tables and their integrity certificate (closure vs declared
    // order, FS indicators nu = 2 - e, J^2 = -Id and J-generator commutation,
    // the R-Burnside trap sum d^2/e = |G|), the 9-vs-5 FS contrast, and the
    // twin pin — the generic e-weighted core instantiated at O(3) labels must
    // equal MLSpec.homDim/homBlocks on a 15-spec sweep, which is what earns
    // the abstraction without rerouting O(3) through it. Pure integer.
    let pointSpec = Blade.Tests.PointSpecReview.runPointSpecTests ()
    // The point-group Hom-basis COMPLETENESS oracle (stage 5b-0): the emitted
    // [Id, J] columns vs the exact rational Reynolds projector
    // (1/|G|)Sum_g rho_W(g) M rho_V(g)^T, entrywise over Q with the closed-form
    // Gram d*I_e per cell. Three negative controls run live (dropped J column,
    // the naive e = 1 sizing formula, a spurious diag(1,-1) End column that
    // dies at R90). BigInteger fractions throughout - no float, no tolerance.
    let pgOracle = Blade.Tests.PgOracleReview.runPgOracleTests ()
    // Compiler-native Cartesian<->irreps bridge constants (CartesianBridge.fs)
    // vs closed forms + the y_to harmonic constants (sgs closure arc).
    let cartBridge = Blade.Tests.CartesianBridgeReview.runCartesianBridgeTests ()
    // Error locations: parse/type errors point at the right line (§3.4).
    let spans = Blade.Tests.Spans.runSpanTests ()
    // Diagnostics core: renderer golden shapes + BLxxxx registry contract.
    let diagCore = Blade.Tests.DiagnosticsCore.runDiagnosticsCoreTests ()
    // Diagnostics corpus: broken sources with pinned codes/spans (strict).
    let diagCorpus = Blade.Tests.DiagCorpus.runDiagCorpusTests ()
    // Stage-6a certificate SUGGESTIONS (BL4011): the ml-equiv corpus's
    // `// SUGGEST:` pins, strict in both directions so SILENCE is assertable
    // (a warning changes no value, so the value corpus cannot pin it).
    let certSuggest = Blade.Tests.DiagCorpus.runCertSuggestTests ()
    // B3 of the retired equivariance-in-types plan: the DIFFERENTIAL between the typed
    // rep-status deduction (DeduceRep/TypedCertProposals) and the same
    // stage-6a seam inference the block above pins, run over the same corpus.
    // Recall in one direction, zero false proposals in the other; engine-only
    // files carry `// TYPED-EXEMPT: engine` until the C2 port, typed-only wins
    // carry `// TYPED-SUGGEST:` pins. Red here BLOCKS the suite by design —
    // that is the phase-B ship criterion, not a flaky test.
    let repDiff = Blade.Tests.RepDifferential.runRepDifferentialTests ()
    // C1 of the same plan: the typed walker's SECOND OPINION on every
    // certificate the seam already checked. The differential above asks whether
    // the two agree about what to PROPOSE; this asks whether they ever
    // CONTRADICT each other about a declared theorem. Red here means the
    // compiler holds two incompatible proofs of the same statement, so it
    // blocks the suite by design — the LieGuardFailure posture.
    let repCheck = Blade.Tests.RepCheckAgreement.runRepCheckAgreementTests ()
    // C3 of the same plan: the REJECTION side, which neither block above can
    // see — both of them only look at programs that compile. This one measures
    // what the typed walker would say about the programs the seam REFUSES, by
    // shadowing the `ml.equiv` pin so the seam falls silent. Its assertions are
    // the harness's own health (the out-of-band re-validation must reproduce
    // the live C1 census file for file) plus the alarming direction: the typed
    // side must never CONFIRM a certificate the seam rejects.
    let repReject = Blade.Tests.RepRejectCensus.runRepRejectCensusTests ()
    // C++ runtime-layout tests for the contiguous-backing allocate<>.
    // Verifies layout invariants the value-checking source tests cannot catch.
    // Skips cleanly if g++ absent.
    let alloc = runAllocLayoutTests ()
    // C++ wreath-class storage invariants (segment-peeled traversal order,
    // rank/unrank bijection): same category as alloc, skips cleanly sans g++.
    let orbWreath = Blade.Tests.OrbWreathTests.runOrbWreathTests ()
    // OpenMP pragma emission: verifies a `where omp(...)` clause reaches codegen
    // as a pragma for EVERY kernel spelling (named function via either eta site,
    // let-bound lambda, inline lambda) and for no unannotated one. Pure codegen
    // string checks — no toolchain, no threads — so unlike the coverage block
    // below this runs unconditionally. (Also `blade test omp-pragma`.)
    let ompPragma = runOmpPragmaTests ()
    // LinAlg dispatch emission (Phase 5 of docs/plan-cpp-perf-exploitation.md):
    // verifies gram/matmul reach `blade_linalg::` rather than an inline loop or
    // an inline cblas call, that the shim header is included exactly when a
    // route fires (and NOT otherwise), and that the routing policy table still
    // says what it documents. Pure codegen string checks — no toolchain, no
    // BLAS — so it runs unconditionally. (Also `blade test linalg`.)
    let linalgEmit = Blade.Tests.LinAlgTests.runLinAlgEmissionTests ()
    // The runtime half of the same layer (Phase 5d): the shim's contiguity
    // probe must REFUSE the n = 2 packed-symmetric skeleton, whose row starts
    // are indistinguishable from a dense 2x2's over a pool one cell shorter.
    // Compiles and runs cpp/linalg_probe_tests.cpp, so it needs g++ and reports
    // Skipped without it — but needs no BLAS (it includes the BLAS-free
    // blade_linalg_views.hpp). (Also `blade test linalg`.)
    let linalgProbe = Blade.Tests.LinAlgTests.runLinAlgProbeTests ()
    // The four-tier BLAS/LAPACK configuration resolution (off / explicit
    // BLADE_BLAS_LINK / OPENBLAS_DIR prefix / bare system) plus Toolchain's
    // env-over-file precedence. Pure in-process unit checks with the whole
    // env surface pinned — no toolchain needed. (Also `blade test linalg`.)
    let blasTier = Blade.Tests.LinAlgTests.runBlasTierTests ()
    // `blade doctor` structural pins: row set/order, JSON shape, and the
    // pinned-surface determinism (BLADE_BLAS=0 -> blas row off). Probes run
    // for real; machine-dependent statuses are not asserted.
    // (Also `blade test doctor`.)
    let doctorBlock = Blade.Tests.DoctorTests.runDoctorTests ()
    // `blade setup` pure halves: parseArgs + the toolchain-file
    // merge/remove roundtrip through the live reader. (Also `blade test setup`.)
    let setupBlock = Blade.Tests.SetupTests.runSetupTests ()
    // Factory flat emission: the chained factory sugar and by-nominal
    // argument routing elaborate BEFORE typing, so a chain must emit
    // byte-identical C++ to its flat spelling, with exactly one call and no
    // std::function/partial-application residue. Pure codegen string checks
    // — no toolchain — so it runs unconditionally.
    let factoryFlat = Blade.Tests.Functions.runFactoryFlattenTests ()
    // Gather elision: a group_by whose every consumer reads only extents(row)
    // skips the per-group allocation and the O(n) copy. The corpus pins the
    // values on both sides of that decision; only an emission-shape check can
    // see the decision, since eliding and gathering print the same numbers.
    // Pure codegen string checks — no toolchain — so it runs unconditionally.
    let gatherElision = Blade.Tests.Sqlish.runGatherElisionTests ()
    // Eigensolver dispatch (Phase 6 / Round B2): verifies `math.eigh` reaches
    // `blade_lapack::blade_eigh_*` gate-on (right precision AND right symmetry
    // family), that a complex operand's tuple is Q-complex/LAM-REAL, that the
    // gate-off and explicit-sweeps paths are still the synthesized Jacobi with
    // no LAPACK dependency named anywhere, and that `inferEigh` rejects the rows
    // LAPACK has no routine for. The gate-off pin is the load-bearing one: the
    // corpus, `interp math` and `diff-oracle math` all run that arm, and an
    // eigensolver's output is not unique, so leaked dispatch would not merely
    // fail those suites but strip them of meaning. Pure codegen string checks —
    // no toolchain, no LAPACK — so it runs unconditionally. (Also `blade test
    // lapack`.)
    let lapackEmit = Blade.Tests.LapackTests.runLapackEmissionTests ()
    // Shape-monomorphization REACH (Phase 4, second increment): which call
    // sites earn a specialized copy — cross-module, self-recursive — and which
    // must still decline (extent-changing recursion, a foreign extent NAME).
    // The decision is invisible in the values, and it fails silently in both
    // directions: a lost spec is only a lost 1.77x, a wrongly-taken one is an
    // out-of-bounds loop bound. Pure codegen string checks, no toolchain.
    // (Also `blade test shapespec`.)
    let shapeSpec = Blade.Tests.ShapeSpecTests.runShapeSpecTests ()
    // Flat-elementwise REACH: which index tags reach the flat path. Same
    // silent-in-both-directions shape as the block above, and the same remedy —
    // emission text, since the flat rewrite preserves values exactly. Chiefly
    // PARITY pins between the anonymous range and `range<I>`: the `__anon` tag
    // was refused for years by a blanket `__` prefix test that every other
    // reserved tag was already refused by on IxKind grounds. Pure codegen
    // string checks, no toolchain. (Also `blade test flat-path`.)
    let flatPath = Blade.Tests.FlatPathTests.runFlatPathTests ()
    // The optimization layer's emission pins (src/Optimize.fs): every pass
    // there is value-preserving by charter, so the corpus cannot see one
    // that silently stops firing -- or one that fires and smuggles in a
    // contract (the freeze idiom must derive the break WITHOUT acquiring
    // the `while` spelling's budget abort). Pure codegen string checks, no
    // toolchain. (Also `blade test optimize`.)
    let optimizeLayer = Blade.Tests.OptimizeTests.runOptimizeTests ()
    // The halo access record and the reverse-mode halo ROUTE differential
    // (docs/plans/structural/02): emission pins for which route fired, and
    // the gather's output compared byte-for-byte with the scatter's on the
    // same programs. The record and emission pins need no toolchain; the
    // differential skips without g++. (Also `blade test access`.)
    let accessLayer = Blade.Tests.AccessTests.runAccessTests ()
    // File-based module resolution (src/ModuleResolve.fs) + stdlib/units/SI.blade:
    // the search path, the transitive walk, cycle/duplicate/missing refusals,
    // and the two claims the corpus cannot make — that a file with NO imports
    // still emits byte-identical C++, and that a unit which crossed a module
    // boundary still rejects a dimension mismatch. Front-end only apart from one
    // value case, which skips cleanly without g++. (Also `blade test module-resolve`.)
    let moduleResolve = Blade.Tests.ModuleResolveTests.runModuleResolveTests ()
    // The icechunk `checkout` desugar (src/ProviderDesugar.fs): a raw-AST ->
    // raw-AST rewrite that `TypeCheck.typeCheck` runs on EVERY program, whether
    // or not it mentions a provider. No store, no registry, no g++, never
    // skips — the same shape as the normalize/unify blocks — so it is
    // unconditional here rather than reachable only through the standalone
    // verb. (Also reachable standalone as `blade test provider-desugar`.)
    let providerDesugar = Blade.Tests.ProviderDesugarTests.runProviderDesugarBlock ()
    // OpenMP thread-coverage: verifies emitted pragmas form genuine parallel
    // regions when cores are available. Opt-in (see FullSuiteOptions).
    let omp =
        if opts.IncludeOmp then Some (runOmpCoverageTests ())
        else
            printfn "\nOpenMP coverage: not run (opt-in; enable with 'blade test --omp' or run 'blade test omp-coverage')."
            None
    // Comm-licensed parallel reductions (Phase 2): compiles the omp and serial
    // spellings of each fold and diffs the VALUES, which no string check can do.
    // Same opt-in gate as the coverage block — it compiles and runs real
    // programs — and skips cleanly when g++ is absent.
    let ompReduce =
        if opts.IncludeOmp then Some (runOmpReduceTests ())
        else
            printfn "OpenMP reductions: not run (opt-in; enable with 'blade test --omp' or run 'blade test omp-reduce')."
            None
    // Device buffer dimensional-type tests (CUDA streaming foundation). Pure F#.
    let bufType = runBufferTypeTests ()
    // `where cuda` hardware tests (differential vs host-loop oracle). Opt-in;
    // even when requested they skip cleanly if nvcc/GPU/cl.exe are absent.
    let cuda =
        if opts.IncludeCuda then Some (runCudaTests ())
        else
            printfn "CUDA kernel tests: not run (opt-in; enable with 'blade test --cuda' from the x64 Native Tools prompt)."
            None
    // Round D's cuBLAS swap-table verification. Same opt-in phase and the same
    // capability gate as the kernel tests above (nvcc + GPU + cl.exe), because
    // it needs exactly the same things; separate block so a swap-table failure
    // is not reported as a `where cuda` codegen failure.
    let cublasSwap =
        if opts.IncludeCuda then Some (runCublasSwapTests ())
        else None
    // `where mpi` decomposition tests (differential vs serial oracle under
    // mpiexec). Opt-in; even when requested they skip cleanly if g++ /
    // -lmsmpi / mpiexec are absent.
    let mpi =
        if opts.IncludeMpi then Some (Blade.Tests.MpiTests.runMpiTests ())
        else
            printfn "MPI decomposition tests: not run (opt-in; enable with 'blade test --mpi' or run 'blade test mpi')."
            None
    // Differential symmetry harness: every symmetry case vs an independent F#
    // oracle over randomized inputs. Skips cleanly when g++ absent.
    let diff = runDifferentialSymmetryTest ()
    // Type-structure tests: assert deduced IR types of bindings (no codegen/run).
    let typeStruct = Blade.Tests.TypeStructure.runTypeStructureTests ()
    // Differential timing: measured (r!)^d speedup of comm-annotation and
    // symmetric-type forms vs their dense equivalents. Reports ratios; warns
    // (never fails) on a slow ratio. Skips cleanly when g++ absent. Opt-in:
    // it compiles + repeatedly runs large programs and dominates wall time.
    let timing =
        if opts.IncludeTiming then Some (runDifferentialTimingTests ())
        else
            printfn "Differential timing: not run (opt-in; enable with 'blade test --timing' or run 'blade test timing')."
            None
    // Interpreter differential gate: the tree-walking IR interpreter vs the
    // compiled binary over the supported corpus slice, byte-identical
    // normalized stdout required. Opt-in: it runs BOTH pipelines for every test
    // in the slice (compile+link+run, then a full interpreter walk), so it
    // roughly doubles the corpus cost of a suite run. When requested it still
    // skips cleanly if g++ is absent — the compiled binary is its reference.
    let interpDiff =
        if opts.IncludeInterpDiff then
            Some (Blade.Tests.InterpDiff.runInterpDiffTests Blade.Tests.InterpDiff.currentSlice)
        else
            printfn "Interpreter differential: not run (opt-in; enable with 'blade test --interp' or run 'blade test interp')."
            None
    // Pinned-oracle differential gate: this binary vs the pinned ./oracle build
    // over the dense corpus slice, identical printed VALUES required. Opt-in
    // because it depends on an external resource nothing in the build produces:
    // a second, previously-gated Blade at ./oracle/Blade.exe. It reports a
    // clean SKIP (not a failure) when that exe or g++ is missing, so enabling
    // it on a machine without a pinned oracle is harmless — it is off by
    // default so a plain `blade test` doesn't spend the corpus twice.
    let diffOracle =
        if opts.IncludeDiffOracle then
            Some (Blade.Tests.DiffOracle.runDiffOracleTests "./oracle/Blade.exe" Blade.Tests.DiffOracle.denseSlice)
        else
            printfn "Diff vs pinned oracle: not run (opt-in; enable with 'blade test --diff-oracle' or run 'blade test diff-oracle'; needs a pinned ./oracle/Blade.exe, else it skips)."
            None
    // Caller-supplied blocks (see doc comment): currently the CLI smoke test.
    let extras = extraBlocks |> List.map (fun run -> run ())

    // Grand-total roll-up (#4): one line per block, a total, and failed names.
    let blocks =
        [ yield r1; yield r2; yield attrs; yield subst
          yield normalize; yield unify; yield validateArrow; yield displayFrames; yield grRender
          yield shape; yield oracles; yield orbRank; yield wigner; yield symPower; yield polyOracle; yield lieTables; yield permSpec; yield permOracle; yield structIdxSpec; yield structIdxOracle; yield pointSpec; yield pgOracle; yield cartBridge; yield spans; yield diagCore; yield diagCorpus; yield certSuggest; yield repDiff; yield repCheck; yield repReject; yield alloc; yield orbWreath
          yield ompPragma; yield linalgEmit; yield linalgProbe; yield blasTier; yield doctorBlock; yield setupBlock; yield factoryFlat; yield gatherElision; yield lapackEmit; yield shapeSpec; yield flatPath; yield optimizeLayer; yield accessLayer; yield moduleResolve; yield providerDesugar
          match omp with Some b -> yield b | None -> ()
          match ompReduce with Some b -> yield b | None -> ()
          yield bufType
          match cuda with Some b -> yield b | None -> ()
          match cublasSwap with Some b -> yield b | None -> ()
          match mpi with Some b -> yield b | None -> ()
          yield diff; yield typeStruct
          match timing with Some b -> yield b | None -> ()
          match interpDiff with Some b -> yield b | None -> ()
          match diffOracle with Some b -> yield b | None -> ()
          yield! extras ]
    Blade.Tests.TestHarness.printGrandTotal blocks
    let anyFailed = blocks |> List.sumBy (_.Failed)
    if anyFailed = 0 then 0 else 1

/// Full suite with no caller-supplied blocks (standalone/back-compat form).
let runAllTestsFull (opts: FullSuiteOptions) = runAllTestsFullWith [] opts

/// Run all tests with generate only
let runAllTestsGenOnly () =
    let outputDir = "./generated_cpp_tests"
    runTestCategoryGenOnly "All" allTests outputDir

let runAllTests () =
    let r1 = runTestCategory "All" allTests
    let r2 = runMultiFileTests "Multi-File Modules" multiFileTests
    if r1 = 0 && r2 = 0 then 0 else 1
