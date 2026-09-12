# Beyond symmetry: the Fortran-killer audit

2026-09-05 follow-up: [round two](plan-fortran-killer-2.md) rebases this historical
audit against current master and corrects several comparative claims in section 1.
In particular, freeze recognition/guarded recurrences and scaled units now exist;
symmetry alone does not license Cholesky; and a contraction flag is not a universal
reproducibility guarantee. Treat the original findings below as their dated snapshot.

Status: RESEARCHED 2026-08-29 (four-agent probe audit). 2026-08-30, on
feat/expr-fusion: arc 1's fusion pass LANDED (fuseElementwiseChainsModule +
flat-path gate integration; `a + b*c - d` = one loop, and the bare-range axis
idiom = one index-driven loop, zero temporaries); `where repro` LANDED
(BLADE_REPRO_FN + reorder-licence veto + routing scope; the arc-1/C6
reproducibility direction, chosen clause-granular by the author); bugs
B1/B2/B8/B9/B10/B17 FIXED (four spun-off sessions, cherry-picked). Arc 4
refined into plan-distributed-memory.md (chunk-wise, per the author's
directive). Arc 2 (`while` guard) DEFERRED by decision. Arcs 3, 5, 6 open.

Four parallel audits (single-node perf floor, parallelism/scale-out, ecosystem/IO/interop,
expressiveness/correctness) asked one question: aside from symmetry, what would make Blade
displace Fortran? Each ran emit/check/run probes against the prebuilt binary from private
scratch dirs; claims below marked VERIFIED were probe-confirmed (emitted C++ read, or
runtime output observed), READ-ONLY were inferred from source. Probe files lived in the
session scratchpad (ephemeral); every load-bearing repro is inlined here.

## 0. Verdict

Blade's per-loop code quality, single-node OpenMP derivation, and safety story are already
at or beyond Fortran. The audit found the distance to "Fortran killer" concentrated in five
arcs, and — the sharpest meta-finding — **at least eight verified places where Blade's own
never-silent / fastest-way-is-the-only-way invariant is violated**: same computation, two
spellings, 3x apart (fusion, anon-range); licences silently dropped (omp on materialized
folds, wreaths, LLVM-lane lambdas); flagship features silently at war (units disable BLAS).
The `plan-fastest-way-principles` framework (T1 SAME-EMIT gate, T2 decision census; authored
2026-08-28, on a branch, not in this tree) would have caught most of these — this audit is
its concrete case list.

Where Blade already beats Fortran, today, with no work:

- Parallelism as a *checked licence*: `omp` on a non-commutative fold is BL4016, an
  antisymmetric body is BL4013; Fortran's `!$omp reduction` on a bad operator is silently
  wrong. Strategy (`collapse`, `schedule(dynamic)`) is derived from bound structure.
- Ragged/grouped/masked/triangular iteration parallelizes by construction with the right
  schedule; Fortran has no ragged type to derive from.
- Units, index provenance, extent refusals; `blade doctor`; Rust-class diagnostics with 82
  stable codes shipped as machine-readable data; REPL/notebooks/IDE protocol; compile-time
  data folding with sha256 provenance; versioned-store reads (icechunk); `.stream`
  out-of-core reads. Fortran has none of these.
- 8 of 10 archetypal Fortran program shapes are cleanly expressible now (VERIFIED:
  elementwise, reduction, 2-D halo stencil, Thomas tridiagonal via `let rec` +
  `reverse<N>`, table lookup, sort, histogram, sparse matvec). The two impaired shapes
  (iterate-to-convergence, adaptive stepping) share one root cause — Arc 2.

## 1. Arc 1 — Fusion: make the recommended spelling the fast spelling

The #1 single-node gap. VERIFIED by emitted C++: `a + b * c - d` emits **3 loops and 2 heap
temporaries**; the semantically identical `method_for(zip(a,b,c,d)) <@> lambda(w,x,y,z) ->
w + x*y - z` emits **1 loop and 0 temporaries** (flat pool, `BLADE_RESTRICT`, `BLADE_IVDEP`,
confirmed vectorizing at 32-byte width). The governing rule: Blade fuses exactly one
producer into its consumer; deeper nesting materializes. Fortran scalarizes the whole
expression tree — Blade loses on *loop count*, not loop quality.

The fix is unusually safe because the target form already exists: after lowering, a binop
chain is nested `compute(applyCombinator(zip(...)))`; the pass is zip-flattening + kernel
composition into the shape `tryGenFlatElementwiseNest` (`src/CodeGenLoopNest.fs:2168`)
already emits optimally. Legality = three predicates that all exist: `ExprAttrs.IsPure`
(computed, currently unconsumed — `src/IRPrint.fs:501`), single consumption (the LLVM
lane's `ReadOnce` census, `plan-deferred-combinators.md` §0), and shared
`flatShapeSignature`. Bitwise-safe (cell-independent maps), so default-on, no licence.
Place it in Lowering so the interpreter twin inherits it by construction. Emit a
`// [fusion] declined: <reason>` marker in the omp-marker style. ~1 week for depth-2 plus
fixpoint. Expected ~2-3x memory-traffic reduction on bandwidth-bound chains. Risks: kernel
body duplication (cap node count), register pressure (cap fan-in at 8).

Related verified cliffs in the same arc (each < 1 day, see §6): missing `-fno-math-errno`
(sqrt loops do not vectorize today), the `__anon` tag refused by the flat-elementwise gate
(the documented top-rung idiom `x0 + dx * Float64(0..n)` emits strictly worse code than the
rung below it), the `let rec` zero-prefill cliff (4001 literal statements at N=4000).

Follow-ons, lower priority: `BLADE_RESTRICT` on read peels via the existing freshness facts
(`computeFreshReturnFacts`); 64-byte pool alignment on the C++ lane (LLVM lane already
does it); a named `reproducible` FP mode — contract off + reassoc off + pinned march is
exactly what the diff harnesses already pin, and "bit-identical across machines, guaranteed
by the compiler" is a claim ifx defaults cannot make (~1 day). Read
`src/microkernels/SURVEY.md` + its README corrections before building anything here.

## 2. Arc 2 — Convergence: the `while` guard on `let rec`

VERIFIED, and better than expected: iterate-to-convergence **is expressible today** via
static budget + idempotent freeze — Newton for sqrt(2) freezes bitwise at step 5 with the
iteration count recoverable in-language, and a genuine adaptive stepper (state `[t, y, dt]`,
reject-and-retry by freezing `t`) runs correctly. What's missing is cost control: no early
exit (the emitted loop runs all NMAX iterations unconditionally), **the full trajectory is
allocated** (a 256³ Jacobi solve at 500 iterations ≈ 67 GB — the real blocker), and
non-convergence is indistinguishable from convergence.

Proposal — a guard on the existing inductive arm, nothing else changes:

```blade
type It = Idx<200>                       // a BUDGET, not a trip count
let rec u: Array<Float like It, Y, X> =
    match u with
    | zero -> zero
    | zero :: s -> zero :: u0
    | prefix :: n while residual(prefix(n - 1)) > tol -> prefix :: sweep(prefix(n - 1))
```

Semantics: defined up to the first `n` where the guard is false; frozen (last slice repeats)
after. Backward-compatible with the hand-written freeze idiom. Extent stays static, so the
decidability fence (formalism.md v1 bounds) is untouched — the guard can only stop early.
Three wins in dependency order: (1) early exit = one `break`; (2) bounded storage by riding
the rolling-window branch (`claude/recursive-array-materialization-381708`) — K+1 slots
regardless of budget; (3) **guard still true at n = NMAX-1 becomes a BL8xxx runtime abort
naming the array and budget** — a Blade solve that did not converge cannot silently pretend
it did, which Fortran structurally cannot offer. Add `converged_at(u)`. ~1-2 weeks + the
interpreter twin. AD interaction is the real risk and the real opportunity: `while`
*declares* a fixed point, which is exactly the structure the implicit-function-theorem
adjoint needs (see Arc 6).

Quick win in the same arc: `while` and `do` today die as `BL2001: Unbound variable` — a
Fortran programmer's literal first keystrokes. `for` already has a curated steer
(`src/ParserPatterns.fs:352`); reuse the mold.

## 3. Arc 3 — Interop: `foreign` declarations and `--lib` emission

The disqualifying strategic gap. VERIFIED: no FFI in (no `extern` in the grammar; BLAS/
LAPACK/NetCDF are baked-in compiler routes), no library out (every emitted program is a
whole-program `int main`; no `-shared` path outside CUDA plumbing). Sixty years of
numerical libraries are unreachable, and Blade cannot be a kernel inside someone else's
coupled model — which is Fortran's actual ecological niche.

Design direction (audit C): a `foreign "C" / "fortran" module` block where the declaration
is a **checked contract, not an escape hatch** — units cross by erasure but are enforced at
every call site (the FFI feature no other language has; catches exactly the km-vs-m class
that sinks coupled models); index types are the ABI contract (extents passed, provenance
checked — `advect(u_on_grid_A, out_on_grid_B)` is a compile error); `where layout(a:
col_major | packed_lower)` makes marshalling declared-not-hand-written (SymIdx pools are
already self-dual with LAPACK packed layout, proven in `src/cpp/blade_lapack.hpp:32-51`);
`mut` already has C out-parameter semantics; foreign functions are opaque leaves — AD,
`comm` deduction, and the interpreter refuse them loudly (new BL code; `where
adjoint(...)`/`where pure` opt back in). `blade doctor` gains a probe row per foreign
module. P0 (C, dense row-major, no mut) ≈ 2-3 weeks.

`blade compile --lib`: `export function` → `extern "C"` wrappers taking flat pool +
extents, generated header, near-free ctypes shim. The pool-flattening wrapper machinery
already exists for the CUDA boundary (`src/CodeGenCuda.fs:877, 1456`). ~1-2 weeks; needs an
explicit init/teardown pair for module state and the netcdf atexit finalizer.

LAPACK surface, structure-directed: `solve` on `Array<F like SymIdx<2,N>>` → `?ppsv`
(packed Cholesky, zero conversion), on `HermitianIdx` → `?hpsv`, general → today's `?gesv`;
`cholesky` refuses non-symmetric index types. The dispatch key is the index type the
compiler already tracks; `LinAlgPatterns.fs` has the route-table shape and `Eigh` already
dispatches packed-vs-dense. Missing surface named by `blade_lapack.hpp:280-308` itself:
other precisions, nrhs>1, packed solve; plus cholesky/QR/SVD-route/lstsq/det/cond. ~3-5
days per routine once the first lands. Every route extends the existing
outside-byte-identity doctrine — extend the discipline, don't erode it.

## 4. Arc 4 — Distributed memory: today's `--mpi` is not it

Headline finding (VERIFIED by exhaustive census): the entire emitted MPI vocabulary is
`Init/Comm_rank/Comm_size/Allgatherv/Abort/Finalize`. Every rank allocates **every array**
in full; each `where mpi` kernel slices its outer loop into slabs and then
`MPI_Allgatherv`s the whole array back to all ranks. **Memory does not scale out — only
FLOPs do.** No Allreduce (a distributed Blade program cannot compute a global sum — folds
and scalar outputs are refused by `classifyMpiShape`), no Send/Recv, no teams, no RMA, no
derived datatypes. And `halo<>` stencils are MPI-ineligible (virtual source), so the
canonical distributed-HPC pattern is exactly the refused shape. `where mpi` appears nowhere
in the features census, and `blade emit` cannot even show the decomposed C++ (no `--mpi`
flag on emit). Today's `--mpi` is `DO CONCURRENT` across processes, not coarrays.

The arc, in order:

1. **`DistIdx` — decomposition as an index type** (`type DLat = Dist<Lat, blocked>`).
   Arrays over a `Dist` axis allocate only their local slab; ownership is a compile-time
   function of (rank, size), so a non-local read is refused *at compile time* the same way
   a `LonIdx` subscript on a `LatIdx` array is. Beats coarrays because `a[p]` names the
   image (compiler can prove nothing); here locality is in the type. Neither coarrays, UPC,
   nor Chapel type-check locality. Reuses the existing slab arithmetic
   (`src/CodeGenCuda.fs:3164-3174`) verbatim. ~6-8 weeks; defer Dist-on-compact-groups;
   interpreter runs the size=1 degenerate.
2. **Distributed reductions for free**: a fold whose kernel already carries the reorder
   licence (`comm` or recognized builtin) is by that same licence safe to Allreduce. No new
   annotation — the existing licence is the interface; same BL4016 refusal; determinism
   contract "fixed rank count ⇒ fixed combine order" that `co_sum` cannot state. ~1-2 weeks
   and worth doing even before DistIdx.
3. **`halo<>` becomes the ghost-cell declaration**: on a Dist axis, the offset list *is*
   the ghost depth — derive Isend/Irecv to neighbors, interior/boundary split, overlap.
   Also add the missing boundary policies (`periodic`/`clamp`/`reflect`) even in the serial
   case — today halo just shrinks to the interior. ~3-4 weeks on top of #1.
4. `<&>` as a task boundary (best effort-to-payoff in the parallelism lane, independent of
   MPI): VERIFIED that two legs over different index spaces emit four sequential loops —
   the operator declares independence and codegen discards it. Emit `omp task` per
   non-fused leg when legs share no operand. ~3-5 days.

GPU: do **not** extend `CodeGenCuda.fs` (structurally capped at one device expression per
kernel; no shared memory/tiling/streams possible). The offload story is
`plan-mlir-backend.md`'s — its §6 layout-division idea is the uniquely-Blade angle.

## 5. Arc 5 — Data reality: the pitch must survive first contact

The real competitor is Python-wrapping-Fortran (xarray/dask/scipy), and today every
Pangeo/CMIP6/ERA5 store defeats the provider lane: Zarr reads **uncompressed only** (and
writes `compressor: null`, so it is closed under itself and open to nothing in the wild),
no remote stores (s3/gs/https explicitly rejected), no CF metadata at all (`units`,
`scale_factor`, `_FillValue`, calendars never read), store paths must be compile-time
string literals (no argv/env — one binary per input file, vs Fortran's one-binary-10,000-
runs namelist workflow).

Ranked: (1) compressed codecs — the seam is already cut (`ZarrProvider.fs:51, :275`);
zstd/blosc via vendored managed decode on the F# side (icechunk already vendors ZstdSharp)
+ first `LinkNeeds` on the C++ side, ~1 week each. (2) **CF `units` as types** — read the
attribute, surface it as a checkable ascription (`file says "K", you ascribed
Float<meter>` = BL30xx). In xarray units are documentation; in Blade they'd be types.
Nobody else has this. ~2 weeks with a UDUNITS-string parser. (3) Runtime paths with
compile-time shape + runtime validation (the CSV provider already emits exactly this check
pattern); `blade.argv`/`blade.env`. ~1 week; gate compile-time folding off when the path is
dynamic. (4) A package manager eventually (`blade.toml` + git-tag vendoring onto the
existing `$BLADE_STDLIB` search root, ~1 week) — the actual constraint is that the stdlib
is three files and can only grow inside the compiler repo.

Units-scale boundary worth revisiting once CF lands: dimensions-only means `hour`, `km`,
`hPa`, `degC` cannot exist, and the prescribed fix (`* 1000.0`) is an unchecked magic
number at exactly the site units exist to protect.

## 6. Arc 6 — AD through recurrences (the moonshot)

VERIFIED: only additive prefix recurrences differentiate; RK4/Newton/logistic are refused
(BL5500 — an *honest* refusal, and the derivative-unknown-vs-zero discipline in the corpus
is exactly right). But the adjoint of a time-stepping solver is the single highest-value
artifact in scientific computing (MITgcm/TAF teams hand-write them over months), and Blade
already stores the trajectory — the Arc-2 "full materialization problem" *is* the
checkpoint tape. Generalize the additive pre-pass to any step differentiable in the
previous slice: forward sweep stores states, adjoint sweep over `reverse<It>` accumulates
lambda_n = (dstep/du)^T lambda_{n+1}. With the Arc-2 `while` guard, a converged fixed point
gets the one-step implicit adjoint instead of unrolling. 3-6 weeks; the standard
checkpointing tradeoff (AD opts back into full storage) is the honest framing.

## 7. Bug census

Severity S (soundness: wrong answer or crash, no diagnostic), I (never-silent invariant
violated), P (silent perf cliff), D (DX/diagnostic), C (cosmetic/doc). All VERIFIED unless
noted.

| # | Sev | Finding | Where / repro | Fix sketch |
|---|-----|---------|---------------|------------|
| B1 | S | Zip through abstract `T^1` params: no extent check, silent OOB — `addup(q6, p3)` prints 216, answer is 66 | known seam, now with wrong-answer repro; kernel is the style guide's recommended construct | propagate extent-agreement obligation to call site, or runtime check in the non-static case |
| B2 | S | `let`-ascription on a provider read launders true shape → compiles, segfaults (exit 139) | read arm returns operand type (`src/TypeCheckInfer.fs:220`); unify never compares extents (`src/Unify.fs:744`); literals get BL3007, this path gets nothing | compare ascription extents + index Ids against provider var type at the read arm |
| B3 | S | Match arms not type-unified across a guarded arm: array-vs-scalar ternary passes `check`, dies as raw g++ error | `match x with \| n if n > 100 -> arr \| _ -> 0` | unify arm types; at minimum route through BL7004 |
| B4 | S | `nc.write` silently clobbers: second write to same path erases the first (NC_CLOBBER per write, `NetcdfProvider.fs:648`) | ncdump shows only the last var | refuse second write to same literal path (compile-time); real fix: file-scoped writer |
| B5 | I | Whole-rank fold over materialized rank≥2 drops the omp licence with **no marker**, plus a gratuitous full-array `copy_n` | `reduce(A, myAdd, 0.0, axes = 2)` serial while the deferred spelling threads; `genForRangeBinding` (`src/CodeGenBinding.fs:3836`) has no omp awareness; the marker elsewhere *recommends* this shape | marker now; parallel fold later; drop the copy |
| B6 | I | OrbIdx wreath output drops omp licence silently — wreath emitter bypasses `genLoopNestStreamed`, never reaches `ompSuppressedMarker` | `src/CodeGenCuda.fs:1886` | emit the decline marker |
| B7 | I | LLVM lane: `where cuda`/`where mpi` on a **lambda** kernel silently ignored (named functions refuse correctly); mirror of the omp eta-expansion bug the C++ lane already fixed | `EmitLlvm.fs:3123` checks too late; `.ll` byte-identical to unannotated | check on the resolved kernel in `applyToArr` (~3 lines) + 2 llvm pins |
| B8 | I | Units silently disable ALL BLAS/LAPACK routing — `precisionOf` never strips `IRTUnitAnnotated` (`src/LinAlgPatterns.fs:267-273`) | A/B: `Float64 like P,N` → `blade_gram_same_d`; `Float64<meter>` → scalar loops, no note | one arm reusing `UnitPrimElem` (`src/IR.fs:642`). Best cost/benefit fix in the audit |
| B9 | I/P | Anon range (`__anon` tag) permanently refused by the flat-elementwise gate — the documented top-rung idiom emits the slow shape | `flatShapeSignature` (`src/CodeGenLoopNest.fs:2095`) refuses all `__`; only `__anon` is `__`-tagged AND IxKPlain | `"__"` → `"__halowin"`, matching `src/CodeGenBinding.fs:2956` |
| B10 | P | sqrt/fabs/floor/ceil loops do not vectorize — `-fno-math-errno` missing; A/B shows 32-byte vectorization with it; no errno surface exists in src/cpp | `src/Build.fs:70` (+ `:92` llvm) | add the flag; bit-exact |
| B11 | P | `let rec` zero-prefill emits one statement per cell under 4096: N=4000 → 4001 literal stores (also dead — every cell overwritten) | `src/TypeCheckInfer.fs:10113-10125` | threshold → ~64 or `std::fill_n`; best: elide dead prefill |
| B12 | P | `sp.ifft` is O(n²) unconditionally — no `isPow2` branch, while `fft` and both 2-D passes have one (READ-ONLY) | `src/spectra/compiler/SpectraDecls.fs:116` | mirror `fftDecl` with invTwiddles + 1/n |
| B13 | P | Pool alignment lane divergence: LLVM allocator `align 64`, C++ lane plain `new` (16B) | `src/EmitLlvm.fs:855` vs `src/cpp/nested_array_utilities.hpp:239` | `align_val_t(64)` both sides of allocate/deallocate |
| B14 | D | BL4003's suggested fix is rejected: `table(k)` warns "consider `(k : Tab)`" but `(k : Tab)` on a *variable* is BL3001 (literal works) | boundary confirmed | accept var ascription or change the advice |
| B15 | D | BL7001: array literal in an `if` branch (`if p then [x,x,x] else [0.0,0.0,0.0]`) — bare literal return is fine; lands on the adaptive-stepping idiom | workaround: bind each with `\|> compute` | add the IRArrayLit-in-expression rule or name the branch in the note |
| B16 | D | `while`/`do` die as bare `BL2001: Unbound variable` — no BL1003-style steer like `for` has | `src/ParserPatterns.fs:352` is the mold | highest first-impression value per effort in the audit |
| B17 | D | `nc.write` drops named index types: `StationIdx` writes as `"dim0"` — output only self-describing when round-tripped from a store | Id match misses at `Lowering.fs:1843-1852` (same root family as B2) | fix the binding, or BL4010-class note |
| B18 | C | `blade --help` says `<file.edgi>` throughout (`CliCommands.fs:18-47`); `src/Build.fs:863` compiles `-std=c++17` while CLAUDE.md says C++20; `IRStorage.fs:856` cites a formalism §17.3 that does not exist (real: §5.1); `plan-llvm-backend.md:47` claims omp is refused when the lane accepts-and-serializes with no marker; pragma-carrying loops emit the pragma unindented; BL2001 caret points at the wrong match arm; sparse-literal wildcard read rejects with a message describing exactly what the code does; ide-check fast tier renders `T<meter / second>` head unresolved | — | batch of trivia |

## 8. Quick-win batch (each < 1 day, all verified unless noted)

Perf: B10 flag; B9 one token; B11 threshold; B8 one arm; 64B alignment (B13); integer/bool
folds shouldn't need `BLADE_FP_REASSOC` (exactly associative; census'd at 2.5-4.8x).
Invariant: decline markers for B5 + B6; fix the misadvising marker text
(`src/CodeGenExpr.fs:1096`); B7's early check; reconcile the llvm-plan omp claim.
DX: B16 steer; B14; `blade explain BLxxxx` (diagnostics.json already holds all 82
explanations — the verb is a JSON read); B18's `.edgi`; `blade emit --mpi/--cuda` so
decomposed output is inspectable (~10 lines); a `where mpi` row in features.md §16 stating
plainly: SPMD, replicated storage, slab + Allgatherv, folds/scalars/virtual-sources
ineligible (today it is possible to read the whole census and believe Blade has distributed
memory). Eco: refuse second `nc.write` to same path (B4); default `plot.*` axis labels to
`display.unit_label` (already collapses to a literal at typecheck); a `tests/corpus/
providers/` category porting a few hermetic CSV tests (1962 corpus files, zero touch a
provider); `trigamma` (named by ad/014 as the one gap to second-order gamma AD); ifft
pow-2 branch (B12).

## 9. Not recommended

- Extending `CodeGenCuda.fs` — structurally capped; the MLIR plan is the successor.
- A dynamic task-graph runtime — the DAG is statically known; `<&>`-as-task gets 90%.
- Nested OpenMP parallelism, `omp target` — the single-level honest-decline model is right.
- Stack-allocating via wrapper erasure — `plan-static-array-erasure.md` is a REFUTED
  verdict; the live residue is literal extent coverage in ~10 emitters (measured 1.77x)
  and the allocator call itself, which are different changes.
- Re-proposing rolling-window storage — already on `claude/recursive-array-materialization-381708`; Arc 2 rides it.

## 10. Cross-references

Arc 1 composes with `plan-unroll-and-jam.md` (fold ILP) and `plan-deferred-combinators.md`
(shares the ReadOnce census — build it once). Arc 2 rides the rec-array materialization
branch and feeds Arc 6, which extends `plan-forward-mode-ad.md`/`plan-ad-combinators.md`.
Arc 3's LAPACK routing extends `LinAlgPatterns.fs`'s existing table; its diff-testing
doctrine is `blade_lapack.hpp:19-30`. Arc 4's slab math reuses the existing mpi prologue;
GPU defers to `plan-mlir-backend.md`. Arc 5's runtime-shape half is
`plan-llvm-runtime-shapes.md`'s arc — don't duplicate. The enforcement framework for the
whole invariant-violation class is the fastest-way-principles plan (on its branch).

Audit note: the graft index covers only `protocol/` JS/TS + two Python files — the F#
compiler is unindexed. All four agents discovered this independently and fell back to
grep/read. Worth a `graft build` over `src/` if agents are expected to route through it.
