# Cosmology non-Gaussianity — f_NL polyspectra as a lighthouse domain

Status: RESEARCHED (Fable four-agent pass 2026-08-29) — nothing built. Two
probes were run against the existing binary: one CONFIRMED a live soundness
bug (§5.0, chip raised), one confirmed per-row `sp.fft` typechecks (§6).
Deliverables planned: two notebooks (§4), zero compiler work required for v1.
The compiler asks this domain generates are ranked in §5 and staged in §7.

The pitch under evaluation: CMB and large-scale-structure bispectrum /
trispectrum estimation (the f_NL industry) is the intersection of Blade's two
existing pillars — symmetric moment tensors of a field, decomposed in
harmonics, glued with Wigner 3j symbols. Small expert community, flagship
science, bespoke aging codes.

## 1. Verdict

The domain is a near-perfect fit, better than the pitch knew:

- **The thesis sentence is already true in the language.** `ppl.cumulants(F, k)`
  over a `Bin × Cell` filter-bank array IS the binned polyspectrum estimator —
  k=2 the binned power spectrum, k=3 the bispectrum, k=4 the *connected*
  trispectrum (the cumulant subtracts the Gaussian disconnected piece, which is
  exactly the τ_NL/g_NL physics). The order of the polyspectrum is the arity of
  one former. Likewise `sp.polyspec(x1..xk)`'s arity IS the order
  (`src/spectra/compiler/SpectraElaborate.fs:11-12`), and exact `wigner3j` +
  Clebsch-Gordan tables already live compiler-side
  (`src/ml/compiler/WignerTables.fs:40-169`, Gaunt-validated).
- **A flat-sky v1 needs no compiler work.** Fully synthetic, seeded, zero
  external files — a strict advantage over every other data notebook in the
  repo. Full-sphere (a_lm + 3j contractions) is honestly blocked (no SHT, no
  Wigner surface) and is staged, not promised (§7).
- **The stress test paid off immediately.** The research pass surfaced one
  confirmed silent-wrongness soundness bug (`where comm` × `conj`, §5.0), put
  cosmology's name on the exact decision `plan-compact-sym-folds.md` §4 is
  parked on, and identified the constrained-index C2 layer's first real
  customer. The user question "would this raise core-language issues?" —
  emphatically yes, and mostly issues the repo has already half-named.

## 2. Why this domain (external landscape, researched 2026-08-29)

**The "bespoke code, handful of people" claim is directly evidenced.**
- The flagship Planck PR4 non-Gaussianity paper (2025, arXiv:2504.00884,
  A&A 702 A204) has **five authors**. Result: f_NL^local = −0.1 ± 5.0,
  f_NL^equil = 6 ± 46, f_NL^ortho = −8 ± 21, using 600 sims for the linear
  term / error bars.
- The original KSW and Modal (Fergusson/Shellard) Fortran pipelines are
  non-public; CMB-BEST's own paper (arXiv:2305.14646) states there was "no
  publicly available code" reproducing Planck bispectrum-shape constraints.
- healpy's maintainer sought 20%-FTE NASA funding for maintenance of the
  field's foundational SHT wrapper and was declined
  (zonca.dev/posts/2021-11-08-fund-healpy).

**A modernization wave is live (2023–2025), driven by ~4 individuals:**
CMB-BEST (Python/Cython, Sohn), PolySpec (Philcox; 14 trispectrum templates,
the tightest g_NL limits, arXiv:2502.06931), PolyBin3D (JAX GPU backend),
s2fft (differentiable JAX SHT, McEwen group), BFast (JAX LSS bispectrum,
Flöss). Blade would enter a live current, not a graveyard — and its angle
(structure *derived from types*, not hand-ported to GPU) is one none of these
occupy.

**Timing.** SPHEREx — whose primary science driver is σ(f_NL^local) ≈ 0.5 —
launched 2025-03-11 and is releasing data weekly (QR2 since Oct 2025); DESI
published f_NL^local = −3.6 +9.0/−9.1 in 2025 (arXiv:2411.17623). CMB-S4 /
LiteBIRD / Euclid forecasts all fund estimator work this decade.

**Demo data exists off the shelf.** The Elsner & Wandelt simulation suite
(planck.mpa-garching.mpg.de/cmb/fnl-simulations) ships 1000 realizations of
paired Gaussian + non-Gaussian-correction a_lm at lmax=1024, 31 MB per file,
combined as `alm = alm_g + f_NL·alm_ng` — you dial in the truth. Quijote-PNG
covers the LSS side (256³ grids, local/equil/ortho variants). Neither is
needed for v1 (synthetic), both are named swap-in points.

**Algorithmic ground truth for the narrative.** Naive estimation is
O(lmax⁵); separable (KSW) templates factor it to O(lmax³). Relative cost
ladder per the CMB-BEST paper: KSW ≈ 1, Modal ≈ 30, CMB-BEST ≈ 10⁴ (buying
generality). Linear-term corrections use 200–600 MC sims in practice. Review:
Meerburg et al. arXiv:2107.10802.

## 3. What the repo already has (audited, file:line-verified)

| Pipeline object | Blade today | Status |
|---|---|---|
| Bispectrum/trispectrum of 1-D signals | `sp.polyspec(x1..xk)`, arity = order, k ∈ 2..4 | EXISTS (`SpectraElaborate.fs:241-266`; corpus spectra/031, 033). Rank-1 real inputs only; dense output capped at 65536 cells (k=3 ⇒ n ≤ 256, k=4 ⇒ n ≤ 40) |
| Binned polyspectrum estimator | `ppl.moments/cumulants(F, k)` → `SymIdx<k, NB>` packed | EXISTS (`src/ppl/compiler/PplElaborate.fs:1-30`); last declared axis is the sample axis |
| Wigner 3j / CG / real-basis U_l | `WignerTables.fs` (Racah), memoized, Gaunt-validated | EXISTS, compiler-internal only; no Blade surface. Factorial table 0..170 ⇒ l ≲ 56, overrun is an exception not a diagnostic. No 6j/9j |
| C_l-style per-ring reduction | `group_keys`/`group_by` + peel + `reduce(r,(+))/extents(r)` | EXISTS in all three lanes, with units, `omp` (`schedule(dynamic)`), and AD (corpus ad-jvp-comb/043-045, sql-group-by/030/033) |
| a_lm shape | `DepIdx<Idx<L+1>, lambda(l) -> Idx<2*l+1>>` | Type EXISTS (index-types/089); **no runtime producer** — literals only (BL4018; `plan-llvm-runtime-shapes.md` §C1) |
| Reality condition a_{l,−m} = (−1)^m conj(a_lm) | — | MISSING; `HermitianIdx` is a rank-2 transposition class, not a single-axis phased mirror. (Doc defect found in passing: formalism.md:351 says HermitianIdx stores n²; implementation stores the inclusive triangle — chip raised) |
| Symmetric b_{l1l2l3} storage | `SymIdx<3,N>` via `range<SymIdx>` former or `ppl.moments` | EXISTS — but **no fold**: every reduce/prodsum spelling over compact storage refuses BL3999 (~6 sites; `plan-compact-sym-folds.md` P1-P10). Only `decompact`-then-fold runs |
| Triangle + parity support constraint | `static struct … where` (C1 counting layer, `idx_card`) | HALF-EXISTS. The corpus anchor is literally a CG m-selection rule (index-types/145); the dependent-bound refusal test (152) is literally the triangle inequality; box cap 100 000 ⇒ lmax ≤ 45. C2 (the type) unbuilt |
| Constrained support that folds | `SparseIdx` with 3-tuple keys | EXISTS (184, 185) — the only compact class whose `reduce` works; loses canonicalization, comm licensing, and arithmetic ranking |
| FFTs | `sp.fft/ifft/fft2/ifft2` | EXISTS. 1-D `ifft` is ALWAYS O(n²) (no radix-2 branch, `SpectraDecls.fs:116-126`); 2-D is radix-2 per axis — so heavy MC work belongs in 2-D. No `fft3`, no complex→complex fft |
| Gaussian sims | `rand.normal(key, [r,c])`, runtime Int64 key, static shape | EXISTS; per-realization keys inside a `let rec` slice structurally admitted |
| Fisher / covariance | `gram(A,A)` (complex → genuine SymHermitian/zherk), `math.solve/eigh` | EXISTS (math/061, index-types/067) |
| Units through spectra ops | — | MISSING: `classifyElem` matches only zero-argument Float spellings (`SpectraElaborate.fs:109-113`), so `Float<microkelvin>` is refused with a message that never mentions units |

## 4. The notebooks (v1 — no compiler changes required)

Two notebooks, both fully synthetic and seeded (`rand.normal`), no external
files, no `.fsx` tooling — they run from a fresh clone.

### NB1 `examples/fnl_bispectrum_1d.bladenb` — "The bispectrum you can hold" (~14 cells, < 2 s)

In 1-D the *whole* bispectrum fits in one call, and watching the compiler
refuse it one mode later is the fastest way to understand why nobody in
cosmology ever computes the dense object. Arc: seeded GRF via
`sp.fft`→`sqrt(P)·`→`sp.ifft` at n=64; verify with `sp.power`; inject local
f_NL via an `object_for >>@` pipeline; `sp.polyspec(d,d,d)` → the full 64×64
bispectrum, heatmapped (squeezed-corner enhancement visible), Gaussian control
alongside; then **three intentional-refusal cells, all BL5400, all free**
(a failing cell never joins the session — the mandelbrot BL1003 pattern):
n=512 over the 65536-cell cap, order-4 at n=41 (the exact edge; 40 works),
and k=5 (pinned today by spectra/041). Close on the honest scaling: n=256 is
the largest bispectrum this compiler will build, and in 2-D it won't offer
the object at all → NB2.

### NB2 `examples/fnl_flat_sky.bladenb` — "f_NL on the flat sky" (FLAGSHIP, ~28 cells, ≈ 4–7 s full session)

64×64 grid (radix-2 both axes, under the spectra cap, the exact grid
`09_qg_atmosphere` already proves through `fft2`/`ifft2`), 8 log-spaced
k-shells, f_NL_true = 0.30. Condensed arc (full cell table in the research
transcript; Δwork ≈ interpreter-step estimates against the 1e9 budget):

1. **Setup** (cells 1–5): index types `Y/X/Cell/Bin`, k-grid `kmag`, input
   power `P0·k^(−n)` — the k-grid is data with its own type.
2. **A Gaussian universe** (6–8): white noise → `fft2` → `sqrt(Pk)·` →
   `ifft2`, σ normalized out; Parseval pin.
3. **Injection as a pipeline value** (9–11): `δ = φ + f_NL(φ²−⟨φ²⟩)` as
   `object_for >>@ object_for`; the Gaussian control is the same pipeline at
   f_NL=0. One-sweep skewness via `<&!>` fused accumulators against the
   closed form 6·f_NL·σ⁴; paired heatmaps at shared color scale.
4. **The filter bank** (12–13): 8 shell masks, band maps `ifft2(mask·δ̂)`,
   flattened (`WARN: BL4003`, the documented escape hatch — explained in
   prose as lseof does) and `stack`ed to `F : Array<Float64 like Bin, Cell>`.
5. **The ladder is one former** (14–17): `ppl.moments(F,2)` (36 cells,
   diagonal = binned P̄), `ppl.cumulants(F,3)` (120 cells; control
   consistent with zero), and the static combinatorics cell pinning
   `[36,120,330]` vs `[64,512,4096]` — the r! saving as physics: a scalar
   field's legs are unordered.
6. **The refusal that is the physics** (18–20): intentional-fail cell
   `reduce(B3,(+),axes=3)` → **BL3999**. The canonical-vs-mirrored fold
   ambiguity the compiler refuses to guess IS the Δ_{l1l2l3} degeneracy
   factor of the binned estimator; `decompact` twice crosses to the
   ordered-triple convention where Δ disappears. Theory template with zero
   index arithmetic: `method_for(Pb,Pb,Pb) <@> lambda(a,b,c)
   where comm(a,b,c) -> 2·(ab+bc+ca)`.
7. **Estimate** (21): triangle counts from the *same former* on the mask
   bank; inverse-variance weighted `f̂_binned` + Fisher σ, pinned at 2 dp.
8. **KSW — the estimator that never forms the object** (22–23): one `fft2`,
   one `zip` co-iteration, one `ifft2`, one fused sweep; slope
   **self-calibrated** on sims re-injected at f_NL=1 (as real pipelines do —
   this is also the mitigation for the normalization-convention risk).
9. **Null distribution** (24–25): `let rec` over `Trial = Idx<24>` with
   per-trial rand keys — "200 independent experiments without a loop",
   capped at 24 trials because MC cost lives here; empirical σ vs Fisher σ.
10. **Order 4** (26–27): `cumulants(F,4)` vs `moments(F,4)` (330 cells
    each); their difference checked against the disconnected Gaussian piece
    P_iP_j + 2 perms. The expensive cell — last, and labeled as such.
11. **What the compiler knew** (28): three refusals (two BL5400 caps,
    BL3999), each naming a real constraint of the science; future-work
    pointers (§7).

Narrative hook already in-repo: `examples/matched_moments.bladenb:44` says
"CMB non-Gaussianity is by definition structure beyond the power spectrum" —
NB2 is the follow-through on a promise the repo already made. Cite it.

### Verification recipe and pins

- House recipe: full `ide serve` drive with all cells `lane:"interp"`, plus a
  concatenated `blade run` with `BLADE_FP_CONTRACT=off` (pins must be the
  strict values; the compiled lane's FMAs diverge otherwise).
- Notebook `// EXPECT:` pins are documentation, not harness-enforced (no
  notebook category in `CliSelfTests.fs`) — the enforced assertions are the
  round-trip ones: f̂ recovers f_NL_true within the Fisher σ; empirical σ
  agrees with Fisher σ. Rounded pins via the visible-cast idiom
  `Float64(Int64(floor(x*100.0+0.5)))/100.0`.
- If exact pins are wanted, an `.fsx` oracle mirror under `examples/tools/`
  (the `qg_reference.fsx` mold) — normalization bookkeeping (unnormalized
  forward fft2, 1/(rc) inverse, ppl's 1/N, KSW's N_cells, the template's 2,
  the Δ factor) is the identified silent killer; budget an afternoon for
  factors of N and 2.

### Top risks (from the design pass)

1. **BL3999** — everything estimator-side must route through double
   `decompact`; if `decompact` on a `ppl.cumulants` output (vs a
   `method_for` output) hits a different path, the estimator act re-plumbs.
   **Probe first, before writing anything else** (§6).
2. **The spectra AST bomb is paid per cell forever** — `cplxZerosLit`
   emits one node per output cell and every notebook cell re-lowers the
   session. NB2 stays at 64×64; never put a 65536-cell polyspec above other
   cells.
3. **1-D `ifft` is O(n²) always** — anyone "improving" the notebook by
   raising 1-D n hits a wall with no warning. MC lives in 2-D by design.
4. **Normalization conventions** (mitigated by self-calibration, above).
5. **Full-sphere is blocked, not deferred** — no SHT, no Wigner surface,
   `ml.y_to` caps at lmax=2. Said plainly in the closing cell; see §7.

Also noted: `plot.blade` draws one trace per figure (measured-vs-theory as
ratio plots or paired panels); `plot.heatmap` does not typecheck
extents(y)-vs-rows(z) agreement (`stdlib/plot.blade:377-380` uses independent
type vars), so a transposed grid silently draws wrong — consider an assert
cell.

## 5. Language issues this domain raises (ranked by leverage)

### 5.0 CONFIRMED SOUNDNESS BUG — `where comm` on a conjugating complex kernel silently computes mirrored cells wrong

Probe (run 2026-08-29, compiled lane, this branch's binary):

```blade
let A: Array<Complex128 like Idx<2>> = [complex(1.0, 2.0), complex(3.0, 4.0)]
let result = method_for(A, A) <@> lambda(x, y) where comm(x, y) -> x * conj(y) |> compute
// result = [[(5,0), (11,2)], [(25,0)]]
// result(1,0) prints (11,2); the true value of (3+4i)(1-2i) is (11,-2)
```

Mechanism: the parity lattice `PInv | PNeg | PBottom` (`src/Deduce.fs:19-22`)
has no conjugation element; the swapped-body compare has no
Var-vs-`UnaryOp(OpConj,…)` arm, so the kernel deduces PBottom — the trusted
value. The declared-comm contradiction check fires only on PNeg
(`src/TypeCheckInfer.fs:7583-7589`; the comment at :7565-7571 names "the
silent-corruption case" it guards — this is the same case one lattice element
over). The output classes as SymSymmetric, whose mirror is TfIdentity in all
three lanes, so the differential twins agree on the wrong answer and no diff
gate can fire. The Hermitian certificate that would catch it
(`deduceConjCommutes`, BL4015) is gated to already-Hermitian output types and
is never consulted. There is no corpus test declaring `where comm` on a
complex kernel at all.

This is the covariance/visibility/density-matrix/Gram kernel —
`f(a,b) = a·conj(b)` — so it reaches far beyond cosmology. The safe route
exists and is pinned (`gram(A,A)` → genuine SymHermitian) but nothing steers
a hand-rolled kernel to it. Fix, cheapest first: (i) add `PConj` to the
parity lattice + the `mirrorEq` arm, extend the contradiction check so
declared `comm` + deduced PConj is a hard error like declared `comm` + PNeg;
(ii) the constructive follow-up is a `where herm(a,b)` conjunct routing to
SymHermitian storage — completing a column (kernel declaration, pair-swap
deduction, lowering entry, Reynolds variant) that antisym has and Hermitian
lacks throughout. **Chip raised (task_1fd5a982); worth fixing regardless of
the notebooks.**

### 5.1 Compact symmetric folds (BL3999) — cosmology is the demand-driver the parked plan needs

The language can build an r!-compact tensor (`range<SymIdx>`, `ppl.moments`,
comm formers) and cannot sum it — every fold spelling refuses at ~6 typecheck
sites; the only route is `decompact`, which materializes dense n^r and
surrenders the whole saving at exactly the step that matters (fatal past
lmax ≈ 100–500: SymIdx<3,·> dense at lmax=2000 is ~10 GiB).
`plan-compact-sym-folds.md` (EXPLORATORY) is parked on the semantics
decision; the f_NL estimator sharpens it, because the physics wants **both**
readings with names: the full-domain (multiplicity-weighted) sum is the
physical Σ over ordered triples, and the canonical-cell sum is the standard
estimator convention with an explicit Δ_{l1l2l3}. That is precisely the
plan's §4.3 recommendation (full-domain for `reduce`) plus §4.4 ("the
canonical fold still deserves a name"). `emitCompactFold` already exists in
the LLVM lane implementing canonical semantics and must be aligned when the
door opens.

### 5.2 Constrained symmetric support — the C2 layer's first real customer

b_{l1l2l3} is symmetric AND supported only on triangle-admissible,
parity-even triples (~1/4 of the simplex). `PlacementClass`
(`src/Types.fs:42-45`) is a two-way split — combinatorial (closed-form
combinadic, no predicate hook) or tabulated (hashed, `Symmetry = SymNone`
stamped unconditionally) — with no bridge. Meanwhile the predicate machinery
is half-built *for this exact shape*: the `static struct … where` counting
layer (C1, `idx_card`) ships with a two-route enumeration certificate, its
corpus anchor is a CG m-selection rule, and its dependent-bound refusal test
is literally the triangle inequality. The 3j support IS the bispectrum
support — one predicate, two consumers. Proper feature: a fourth placement
case, `PlaceConstrainedCombinatorial of SymmetryClass * predicate`
(canonical simplex enumeration filtered by a static predicate, prefix-sum
offsets over survivors); the `Types.fs` exhaustiveness comment says the
codebase is deliberately pre-wired for a new case forcing every dispatch site
to update. Generalizes to band matrices, sparse triangular factors, causal /
attention masks — no other customer exists in the repo yet, which is exactly
why the demand record matters. Interim: `SparseIdx` with 3-tuple keys is the
one compact class whose `reduce` works today; box-capped constrained structs
count (not iterate) to lmax ≤ 45.

### 5.3 Separability as declared structure — the doctrine-compliant middle rung

The f_NL industry exists because separable templates factor O(lmax⁵) to
O(lmax³) (KSW). Blade has no factorization recognition anywhere (BLAS routing
is template-matching on lowered nests; `>>@` fusion is substitution, not
algebra), and by the repo's own P3 doctrine (`plan-fastest-way-principles.md`:
advise, never rewrite; "if the rewrite can't be written in the surface
language, it isn't a suggestion") **auto-deriving KSW is off-thesis** — the
factored pipeline is perfectly writable as `object_for` stages, and that is
the idiomatic notebook spelling. The on-thesis feature is a **verified
declaration**: `where separable(...)` whose discharge obligation (does the
declared product-of-per-axis-factors equal the written kernel on the admitted
fragment?) runs through the existing `Constraints.Discharge` slot, with the
ML Lie discharger (`MLPolyExtract`/`MLLieDischarge`, exact polynomial
normalization + float shadow guard) as the proven mold. It would be the first
`where` attribute whose discharge unlocks an asymptotic rewrite — the most
interesting research-grade item this domain surfaces. Leverage: separable
convolutions, tensor networks, operator splitting.

### 5.4 The a_lm species: ragged producer + a Symmetry × IxKind product

The triangular shape is expressible (`DepIdx`, statically evaluable) and
per-row ragged reduction with a row-dependent normalizer genuinely works in
all three lanes — but (i) there is **no runtime producer** for a ragged array
(literal-only; BL4018; `plan-llvm-runtime-shapes.md` §C1 names the fix —
give `IRRaggedLookup` an `RlStatic | RlRuntime` twin on the SparseKeysSource
mold), and (ii) ragged axes cannot carry a mirror class (every ragged record
is stamped SymNone; the reality condition a_{l,−m} = (−1)^m·conj(a_lm) is a
single-axis phased mirror no index type expresses — structurally AntisymIdx's
sign machinery composed with conjugation on a flipped axis). Notebook-scale
workaround is cheap: dense padded (l,m) with implicit zeros, arithmetic
2l+1 normalizer, m ∈ [0..l] by convention (2× + 2× waste, no compiler check
on the phase convention). Demand outside cosmology is thinner than 5.1–5.3.

### 5.5 The table-emission wall (and the Wigner surface)

Exposing `wigner3j` to users is a decision, not a build —
`registerStaticBuiltin` (`src/StaticEval.fs:203-212`) is the proven
one-registration path (`ml.sh_spec` precedent), and the elaborator mold makes
a `wigner` domain one line plus fsproj entries. The wall is emission:
baked tables become one C++ line per cell (`genArrayLiteral`), which is what
`maxOutCells = 65536` defends — the polyspec/fft2 caps are architectural, not
knobs, and `static function` is no escape (SVTuple write-back lands in the
same emitter; StaticEval's step budget caps ~10⁴ cells; the erasure plan is
REFUTED). The broad cheap fix is **startup-computed tables** — emit a
generated fill loop instead of N literals (precedent: `let rec`'s zero-fill
switches representation at 4096 cells) — which simultaneously lifts the
spectra caps, the ML CG gathers, and the uncapped non-pow-2 twiddle bake
(`SpectraDecls.fs:105`). The lazy alternative (function-backed arrays) does
not exist in any form and stays speculative. Also: the 3j factorial table
0..170 caps l ≲ 56 and overruns as an exception, not a diagnostic; no 6j/9j.

### 5.6 Smaller items surfaced (each independently actionable)

- **Units cannot cross the spectra boundary** in either direction —
  `classifyElem` matches only zero-argument Float spellings
  (`SpectraElaborate.fs:109-113`). One-line-shaped fix plus an output-unit
  decision (DFT of μK is μK; `power` μK²; order-k polyspec μK^k). A cubic
  estimator is where unit bookkeeping earns its keep; everything outside
  spectra already threads units through grouped peels correctly.
- **`mask()` is rank-1 only and fails at codegen, not typecheck**
  (`CodeGenExpr.fs:1444-1445` vs the rank-blind `inferMask`); and
  `mask`/`compound` over a SymIdx array is unguarded — a rank-3 symmetric
  group is one index record, so `compoundViewType` would stamp a Rank=1
  compound slot: the identical trap `reduce` special-cases at
  `TypeCheckInfer.fs:2748-2752`, with no guard here.
- **No complex→complex FFT** (`fft` is Float64→Complex128; `ifft` discards
  the imaginary part on input) and **1-D `ifft` has no radix-2 branch**.
- **`ppl.moments`' single-leading-axis pool path** emits one named binding
  per canonical cell (~1.8k at d=20, ~177k at d=100) — the worse-scaling
  path selected by the simpler type, failing as g++ compile time, not a
  diagnostic. The multi-axis path emits a real loop. Same re-architecture as
  the emission wall above.
- **`range<SymIdx>` prefix-offset trap** (formalism.md:794-828, pinned
  loops/170-173) sits directly on the l1≤l2≤l3 idiom — every canonical
  kernel must spell `A(p0)·A(p0+p1)·A(p0+p1+p2)`, and the naive spelling
  passes a spot check on the first row. Any cosmology code review must check
  this first.
- **HermitianIdx doc defect**: formalism.md:351 claims n² storage; the
  implementation stores the inclusive triangle (chip already raised).

## 6. Probe ledger

Settled this pass (scratchpad files against the existing binary; nothing in
the repo touched):

- **P0 `comm` × `conj`**: CONFIRMED WRONG (§5.0). Compiled lane; the
  interp lane is predicted to agree (same TfIdentity mirror) but was not run.
- **P1 per-row `sp.fft`**: `method_for(rings) <@> lambda(row: Array<Float64
  like N>) -> sp.fft(row)` **typechecks** (only a cosmetic BL4003 on an
  untagged peek read). The SHT φ-transform is one annotated lambda; note the
  `row: T^1` spelling will NOT work (no declared array shape for the
  elaborator witness).

Open, ordered — run these before writing any notebook code:

1. `decompact` on a `ppl.cumulants` output (vs a `method_for` output), then
   `reduce(..., axes=3)` — R1's residual; re-plumbs the estimator act if it
   fails.
2. `ppl.moments/cumulants` on an annotated `let` whose RHS is `stack(...)`
   (the filter-bank assembly, A1).
3. Runtime rand key `1000 + n` inside a `let rec` slice (the null-MC cell,
   A4).
4. `object_for >>@ object_for` applied to a rank-2 array (A3; fallback:
   `method_for(A) <@>` composed lambda).
5. `sparse(vals, keys)` with runtime 3-tuple keys + `reduce` — the
   constrained-support interim route at rank 3.
6. `let rec` with a `Float<u>` element (traced to refuse; confirms the §5.6
   units item's second half).
7. One timed cell in the interp lane (all wall-clock figures derive from a
   single anchor and could be off 2–3×).

## 7. Staging

- **V1 — the two notebooks** (§4). No compiler work. Flat-sky, synthetic,
  lmax-equivalent scales 8–64 where every cliff is comfortable.
- **V1.5 — three cheap compiler wins the notebooks would immediately use**:
  the PConj soundness fix (5.0), units through spectra (5.6),
  `wigner3j`/`clebsch` as static builtins (5.5) — the last enabling a
  selection-rules cell (the `06_cg_selection_rules` bridge) without an SHT.
- **V2 — the walls**: the compact-fold decision (5.1: full-domain `reduce` +
  a named canonical fold, aligning `emitCompactFold`); loop-emission for
  generated tables (5.5, lifts the spectra caps and the ppl pool path);
  `fft3` + rank-≥2 `polyspec` inputs (the 3-D LSS box / Quijote-PNG route).
- **V3 — the species**: `PlaceConstrainedCombinatorial` (5.2);
  the runtime ragged producer (5.4); `where herm` (5.0's constructive half);
  the `where separable` discharge experiment (5.3).
- **V4 — full sphere**: SHT assembly from proven parts (rank-2 `let rec`
  Legendre ladder with implicit-zero triangular startup + nested rec for the
  diagonal seed + per-ring FFTs per P1), Gaunt tables, then real data —
  Elsner & Wandelt a_lm via an `examples/tools/*.fsx` → Zarr conversion
  (the no-python rule), a Planck flat-sky patch as a one-cell swap in NB2.

## 8. External sources (key)

Planck PR4 PNG: arXiv:2504.00884 · Planck 2018 PNG: arXiv:1905.05697 ·
CMB-BEST: arXiv:2305.14646, github.com/Wuhyun/CMB-BEST · PolySpec:
github.com/oliverphilcox/PolySpec, arXiv:2502.05258/2502.06931 · PolyBin3D:
arXiv:2404.07249 · Triumvirate: arXiv:2304.03643 · s2fft: arXiv:2311.14670 ·
BFast: github.com/tsfloss/BFast · SPHEREx: arXiv:1412.4872,
spherex.caltech.edu · DESI f_NL: arXiv:2411.17623 · Elsner & Wandelt sims:
planck.mpa-garching.mpg.de/cmb/fnl-simulations · Quijote-PNG:
quijote-simulations.readthedocs.io/en/latest/png.html · Review:
arXiv:2107.10802 · healpy funding: zonca.dev/posts/2021-11-08-fund-healpy.
