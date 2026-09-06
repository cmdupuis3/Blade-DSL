# 06 — Enumerable constrained domains: skip the excluded space

**Status (2026-09-06): DESIGN, nothing built.** Elaborates item 6 of
[`plan-structural-performance-opportunities.md`](../plan-structural-performance-opportunities.md)
("Require constrained domains to skip excluded space"). Every claim below is either
marked *verified* (read in the current source at HEAD `e7abf38`, or emitted/run with
`bin/Release/net10.0/Blade.exe` from a private temp directory on 2026-09-06) or marked
*inference*. Line numbers are from the working tree at the time of writing.

Companions: the C2 support-domain proposal in
[`plan-cosmology-nongaussianity.md`](../plan-cosmology-nongaussianity.md) §5.2, the
failed compute gate in [`plan-simplex-blocked-compute.md`](../plan-simplex-blocked-compute.md)
§0, the fold-semantics decision in [`plan-compact-sym-folds.md`](../plan-compact-sym-folds.md)
§4, and the retired `docs/plan-constrained-index-types.md` (deleted; readable as
`git show 9cfab68:docs/plan-constrained-index-types.md`), whose §3.1 fixed the `where`
conjunct surface and whose §8 lists the deferrals this design re-opens.

The one-sentence thesis: **the counting layer proves a predicate's solution set exists
and how big it is; nothing today turns that predicate into a loop whose trip count is
the solution count.** Every runtime route that iterates a restricted support in Blade
today first pays the full box.

---

## 1. Verified current state

### 1.1 The counting layer (C1) — a certificate, not an enumerator

- `static struct R { f: Int<min=a, max=b>, ... } where p1, p2` is index-*eligible* only
  with the `static` marker (`src/StructIdxFence.fs:136-153`, `structStaticFenceOf`);
  every field must be an `Int` with statically foldable bounds (`isIntFieldType`
  `:88-94` — `Nat` deliberately excluded; a bound naming an earlier field fails as
  "undefined variable" in `foldBound` `:111-116`, pinned by
  `tests/corpus/index-types/152_idx_card_dependent_bound.blade`).
- The conjuncts are `Ast.Expr` (`TyDeclStruct ... constraints: Expr list`,
  `src/Ast.fs:671`) and are folded **per cell** by `evalConjunctsAtCell`
  (`src/StructIdxFence.fs:174-201`) under `StaticEval.cellBudget = { Steps = 10_000;
  Depth = 512 }` (`src/StaticEval.fs:395`). Field bounds desugar into ordinary
  conjuncts (`Ast.fs:816-817`, `:864-865`). `abs` folds (`StaticEval.fs:808`), so
  `where abs(i - j) <= 3` is accepted by the counting layer (verified: `idx_card` on
  such a struct typechecks).
- `src/StructIdxSpec.fs` enumerates by two routes and certifies them against each
  other on **every** call: route 1 `routeFlat` (`:181-212`) is an odometer over the
  whole box; route 2 `routeHeads` (`:265-298`) is the `ck` arrow of
  `proofs/BladeCompound.v:50-66` executed by brute force — `hasCompletion` (`:220-256`)
  searches the *remaining fields' full sub-box* for a witness. `certify` (`:303-343`)
  compares set and order. Both routes share one memoized predicate (`memoize`
  `:166-175`), so the cost of a call is Θ(box volume) predicate folds; neither route
  visits O(solutions) cells. The box cap `maxBoxCells = 100_000` (`:101`) is enforced
  in `enumerateBox` (`:350-372`) *before* enumeration, with the message "the CAP IS ON
  THE BOX, not on the solution count" (pinned by `index-types/153`).
- The only consumer is the syntactic static builtin `idx_card(R)` (`idxCard`
  `:466-486`, registered by `install` `:491`, called from `src/TypeCheck.fs:292` and
  `src/Ide.fs:2174`). It returns `SVInt card`. **No code path lowers a struct name to
  an index type**: `range<Band>` over a static struct is refused today with
  `src/TypeCheckInfer.fs:1384`'s message ("'Band' names neither a value in scope nor a
  declared index type") — verified. Corpus 145's header says it plainly: "C1 IS THE
  COUNTING HALF ONLY … `range<CGm112>` lowering, field values, offsets … are C2."
- Measured (check time, wall clock minus a 0.44 s trivial-program baseline): box
  10 000 → 0.27 s; box 90 000 (a 300×300 band, `where i - j <= 3, j - i <= 3`) →
  0.71 s; box 259 081 (509×509) → refused. Roughly 8 µs per box cell for the two
  routes plus memo. This is fine for a *certificate*; it is not a route a runtime loop
  can inherit.

### 1.2 What "iterating a constrained domain" is today — three spellings, emitted and run

There is no runtime constrained-struct route, so the honest probe is the three ways a
band `|i − j| ≤ w` over `Idx<509>` can be spelled now. All three were emitted
(`blade emit`) and run (`blade run`) from a private temp dir; totals are bit-identical
(`5660.15225600001` at n = 509, `16886.24608` at n = 1021).

| spelling | emitted work (verified in the .cpp) | end-to-end wall (single runs, whole program) |
|---|---|---|
| **predicate scan** — `method_for(range<I>, range<I>) <@> lambda(i, j) -> if i - j <= w && j - i <= w then A(i)*A(j) else 0.0`, then `reduce(., (+), axes = 2)` | dense 509×509 nest with the predicate in the kernel body; the reduce then *copies* the n² array and folds a second 509×509 nest — 2·n² cells, n² storage | n=509: 0.83 ms; n=1021: 3.22 ms |
| **mask + compound** — `let m = (mask nest)`, `let dense = (n² nest)`, `compound(dense, m)`, `reduce(B, (+))` | n² bool nest + n² value nest + `compound_index_t<2>` built from a `std::vector<bool>` of n² bits (`hash()` → `enumerate(0, idx)` scans the whole product space, `src/cpp/index_types.h:245-260`, `:289-300`) + an n² scatter (`compactScatter`, emitted line `for (B_c < B_idx_grid)`); only the fold is O(card): `for (__ri < B.idx->cardinality * ...) total = f(total, B.data[__ri])` (`src/CodeGenExpr.fs:1076-1085` compound arm) | n=509: 1.51 ms |
| **`range<CompoundIdx<m>>`** — `method_for(range<CompoundIdx<m>>) <@> lambda(i, j) -> A(i)*A(j)`, `reduce(R, (+))` | n² mask nest + the same n²-scan index build (`genCompoundIndexFromMask`, `src/CodeGenExprSupport.fs:422-432`); the driver is ONE loop `for (__i0 < __range0_cidx->cardinality)` with `int64_t i = __range0_cidx->unhash(__i0)[0]; ... [1]` (`src/CodeGenLoopNest.fs:85-92`; bound from `genLoopBoundExpr` `:718-742`); output `Compound<double,2>` shares the index | n=509: 1.08 ms; n=1021: 3.90 ms |

Two facts fall out. (1) **The table-backed routes are slower than the scan at these
sizes**, because they save only the *output* storage while doing n² discovery work
(mask nest + product-space scan + hash-table build) — the memo's warning ("another
representation that saves storage while doing the old amount of discovery work") is
already measurable. (2) The kernel parameters of a compound/sparse range are
**untagged** `Int64` (the `__compoundidx` sentinel tag hits the `_ ->` arm of
`elemTypeForIterationIndex`, `src/TypeLower.fs:1270-1280`), so `A(i)` inside the kernel
draws BL4003 "indexed with untagged integer" warnings (observed on both probes). Any
new domain type over a *named* base index must fix that or inherit the warning spray.

Single runs, not medians; the millisecond-scale differences are within the usual
noise band, but the *direction* is forced by the emitted work and is the baseline the
gate in §4 must beat.

### 1.3 Tabulated routes — how CompoundIdx and SparseIdx iterate

- **CompoundIdx**: `TyCompoundIdx mask` lowers to an index record with
  `Extent = IRCompoundMask (IRVar mask)`, `IxKind = IxKCompound`, `Rank = rank(mask)`
  (`src/TypeLower.fs:1093-1127`). `buildRawLoopLevels` gives a compound slot exactly
  **one** loop level regardless of rank (`src/IRLoopStructure.fs:65-70`,
  `levelCount = match idx with IxCompound -> 1 | _ -> idx.Rank`); the virtual driver
  gets one element binding per mask dimension (`src/IRStorage.fs:1118-1127`). Runtime:
  `compound_index_t<RANK> : abstract_sorted_hashed_idx_t` (`index_types.h:237-300`) —
  lex-sorted `rank_to_tuple` + `tuple_to_rank` hash, `cardinality = popcount(mask)`,
  `prefix_range` by bisection (`:264`). Interpreter twin `buildCompoundIndex`
  (`src/Interp/ArrayOps.fs:1382-1403`) is the same product-space scan.
- **SparseIdx**: keys either fold under the static contract and are **baked** as a
  literal table (`resolveSparseKeysSource`, `src/TypeLower.fs:917-943`, `SkStatic`;
  emitted by `genSparseIndexFromKeys`, `src/CodeGenExprSupport.fs:446-451`) or are a
  runtime tuple array (`SkRuntime`). `sparse_index_t<RANK> : abstract_hashed_idx_t`
  (`index_types.h:334-352`) keeps **given order** (iteration order == key order,
  pinned by `index-types/175`), O(|keys|) construction, O(1) hash membership
  (`present`). No grid, no predicate. This is the one compact class whose `reduce`
  works (`plan-cosmology-nongaussianity.md` §3 table; the `.data` walk in
  `CodeGenExpr.fs:1076-1085` covers compound and sparse alike).
- Both kinds share the `IxCompound` family in the kind active pattern
  (`src/IR.fs:1296-1298`) and `PlaceTabulated` in `placementOf` (`src/IR.fs:600-611`).
  `allocRoutineFor PlaceTabulated` is a stub — "no caller passes PlaceTabulated yet"
  (`src/IRStorage.fs:464-486`).

### 1.4 The one enumerable continuation Blade already has: the simplex nest

`range<SymIdx<2, 509>>` emits (verified, `sym_range` probe):

```cpp
for (size_t __i0 = 0; __i0 < 509; __i0++) {
    const size_t __oend_S_0 = 510 - __i0;
    const size_t __ooff_S_0 = (129795) - ((__oend_S_0 * (__oend_S_0 - 1)) / 2);
    int64_t p0 = __i0;
    double* BLADE_RESTRICT __orow_S = __opool_S + __ooff_S_0;
    BLADE_IVDEP
    for (size_t __i1 = 0; __i1 < 509 - __i0; __i1++) { int64_t p1 = __i1; __orow_S[__i1] = ...; }
}
```

The inner bound is an affine function of the prefix, encoded as
`LoopIndexBinding.BoundDependencies : int list` + `StrictOffset : int` with the meaning
`upper = Extent − Σ deps − strict`, lower = 0 (`src/IRStorage.fs:49-63`; derived at
`:1010-1011` for co-iteration and `:1085-1116` for the outer-product path, where
`deps = [baseDim .. level-1]` when the level is triangular and `strictOffset = |deps|`
for antisym). The storage base per row is the closed form `SimplexBlocksCore.prefixTerm`
(`src/SimplexBlocksCore.fs:215`) hoisted once per level. **This is exactly an
"enumerable continuation of every valid prefix", specialised to the ordering
constraints `i_k ≤ i_{k+1}` / `i_k < i_{k+1}`.** Two caveats the design must not
inherit: kernel params are *packed storage coordinates* (prefix offsets), not values
(`docs/formalism.md:809-829`, `loops/170-173`, `CodeGenLoopNest.fs:93-108`); and the
bound machinery has no notion of a prefix-dependent **lower** bound.

`halo<I, [offsets]>` is the other affine-shrink construct: static offsets, dense inner
extent shrunk by `hi − lo` (`src/TypeLower.fs:1560-1640`). It is an O(n·|offsets|)
*stencil* iteration over a dense array, not an index space with its own storage.

### 1.5 The SimplexBlocks lesson

`plan-simplex-blocked-compute.md` §0: S0 brick iteration over the packed pool passed its
correctness gate (three-way cell-for-cell agreement) and **failed its performance gate**
(1.07x slower at n = 6007 on maps; the "parity-or-better with ≥ 1 clear win" bar); the
win (1.70x) appeared only on a row-operand shape under reassociation, and an unset knob
turned out to auto-brick the control arm. Three rules carried into §4: measure the
**complete consumer** (fill + fold + read), control the knob explicitly, and let a
structural argument ("bricks pay zero canonicalization") count for nothing until the
stopwatch agrees.

### 1.6 Lane coverage of tabulated domains (what a new domain kind must also touch)

- Interpreter: `tryCompoundRangeMap`/`materializeCompoundRangeMap`
  (`src/Interp/Loops.fs:1114-1180`), `materializeSparseRangeMap` (`:1223`), general nest
  `interpretNest` with `bound = Extent − Σ BoundDependencies − StrictOffset` (`:1596-1600`,
  `peelElement` `:1679`); print parity `src/Interp/Print.fs:211`.
- Fusion refuses `range<CompoundIdx>` in fused multi-output applications
  (`src/CodeGenFusion.fs:779-785`). CUDA has a compound-range arm
  (`src/CodeGenCuda.fs:2649`). LLVM lane refuses compound initialisers
  (`src/EmitLlvm.fs:4082`). AD only re-walks `ExprCompound` syntactically
  (`src/GradExpand.fs:642`, `:770`); there is no derivative rule over a compound domain.
- `PlacementClass` (`src/Types.fs:42-45`) has three cases; adding one forces
  `IRStorage.fs:277-298` (cardinality), `:464-486` (allocator) and `IR.fs:600-611`
  (`placementOf`) to update — the "deliberately pre-wired" exhaustiveness the cosmology
  plan §5.2 relies on. `IRIndexTypeG.Dependencies : IRId list` exists "for triangular
  iteration" (`src/Types.fs:908-918`) but is `[]` at every constructor (verified by grep).

---

## 2. Classification of constraint shapes

Fields `x₁..x_r` in declaration order (= nesting order, first field outermost —
`StructIdxSpec.fs` header); box `B = ∏ [lo_k, hi_k]` inclusive. An **enumerable
continuation** is a function from a prefix `(x₁..x_{k−1})` to the admissible set of
`x_k` such that (i) it is computable in O(1) arithmetic on the prefix and (ii) every
admissible value has at least one completion (no dead prefixes). Both halves matter:
(i) without (ii) is the predicate scan in disguise; (ii) without (i) is the `ck` arrow
executed by search — route 2 today.

### 2.1 Class A — closed-form continuation

| sub-class | conjunct shape (after constant folding) | continuation at level k | cells visited | vs today |
|---|---|---|---|---|
| **A1 difference constraints** (bands, causal masks, orderings) | `±x_a ± x_b ≤ c`, `x_a ≤ c`, `x_a ≥ c` — two variables per inequality, unit coefficients | interval `[max(lo_k, x_j + c⁻_j …), min(hi_k, x_j + c⁺_j …)]`; outer levels' bounds come from **Fourier–Motzkin projection**, which is closed on difference constraints (shortest-path closure over ≤ r variables, done once at compile time), so (ii) holds exactly | = \|D\|; band width w: `n(2w+1) − w(w+1)` = O(nw) | scan n²; compound n² build + card iterate |
| **A2 affine equality, one dependent coordinate** (`m₁ + m₂ == m_out`, `k₁ + k₂ + k₃ == 0`) | `Σ a_j x_j == c` with the *innermost* field's coefficient ±1 | innermost is **solved**: `x_r = c − Σ a_j x_j`, one cell iff in `[lo_r, hi_r]`; that condition is a linear inequality on the prefix — A1 when the other coefficients are ±1 (CG: `m₂ ∈ [−l_out − m₁, l_out − m₁] ∩ [−l₂, l₂]`), otherwise a bounded (not solved) interval with waste | = \|D\| when projected exactly; \|D\| + waste otherwise | box scan |
| **A3 congruence / parity** | `x_k % m == c`, `(Σ x_j) % 2 == 0` | arithmetic progression: first admissible value ≥ current lower bound, step m | = \|D\| | box scan |
| **A4 product of independent ranges** | none | dense nest | n^r | already `range<I, J>` |
| **A5 triangle-inequality regions** (`\|l₁−l₂\| ≤ l₃ ≤ l₁+l₂`) | A1 in `l₃` given `(l₁, l₂)` | interval | ≈ box/2 (≈ box/4 with parity) — **same asymptotic order**; a constant-factor shrink, not an O(·) win | box scan |

Ordering constraints `x_k ≤ x_{k+1}` / `x_k < x_{k+1}` are A1, and the existing simplex
nest (§1.4) is A1 restricted to upper bounds with lower bound 0 — the design therefore
*generalises* `BoundDependencies`/`StrictOffset` rather than sitting beside it.

Modulo-n conservation (`(k₁ + k₂ + k₃) % n == 0`) is A2+A3 combined: the innermost is
solved up to a residue class, i.e. a progression of step n intersected with the box —
still O(1) per prefix. *Inference*: I have not written that case out; the gate in §4
uses the CG-style plain equality.

### 2.2 Class B — table-backed successor

| shape | continuation | cost | today |
|---|---|---|---|
| arbitrary static predicate (function call, `\|\|`, `if`, non-linear) with box ≤ cap | enumerate at compile time with the existing certified routes, **bake the solution list as a static key table** — this *is* `SparseIdx` `SkStatic` (`TypeLower.fs:917-943`), lex order guaranteed by `certify` | O(card) iteration, table load per cell, hash lookup for reads; compile time Θ(box) | `idx_card` only |
| sparse keys given by data (edge lists, CG tables) | existing `SparseIdx` given-order table | O(\|keys\|) build, O(1) hash membership; a **join** iterates the smaller key set and probes the other's `tuple_to_rank` — O(\|K₁\|+\|K₂\|) instead of the O(\|K₁\|·\|K₂\|) product scan (*inference*: no join emitter over two sparse key sets exists today; `present()` is the primitive it would use) | exists |
| data-derived rectilinear mask | existing `CompoundIdx`: lex-sorted table with `prefix_range` bisection | Θ(grid) build + O(card) iterate | exists (§1.2 measured) |

Class B for a box **above the cap** stays a refusal. Emitting the predicate as a
runtime filter over the box would reintroduce exactly the scan this item exists to
remove; the user can already spell that with a mask.

### 2.3 Class C — mixed (closed-form outer levels over a tabulated inner) — out of scope.

### 2.4 The symmetry-compatibility obligation, stated precisely

Let `D ⊆ B` be the logical support, `G ≤ S_r` the declared permutation action on
coordinates (with character χ = sign for `AntisymIdx`, conjugation for `HermitianIdx`),
and `C = { i₁ ≤ … ≤ i_r }` the canonical chamber whose stored cells are
`D_canon = D ∩ C`.

**Obligation.** Compact storage of a `D`-supported array is sound iff `D` is
`G`-invariant: `σ·x ∈ D ⇔ x ∈ D` for every `σ ∈ G`, `x ∈ B`. Then (a) every orbit of
`D` meets `C`, so `D_canon` represents `D` exactly; (b) the compact read
(sort + χ, `canon_fold`/`canon_transform` as emitted at `sym_range` line 266) remains a
bijection between logical `D`-cells and stored cells; (c) the full-domain fold of
`plan-compact-sym-folds.md` §4.3 (fold each stored cell `m(cell)` times, `m` = orbit
size) is correct, because the whole orbit of a stored cell lies in `D`.

**Why filtering `C` does not prove it.** Restricting a predicate `P` to `C` and storing
`{x ∈ C | P(x)}` is always *possible*; it is *meaningful* only if `P(σ·x) = P(x)`. For
`P = (i − j ≤ w)` alone (an upper band) the chamber restriction is the full triangle's
band half, but the logical cell `(j, i)` with `j − i > w` would read a stored cell that
`P` excludes on the mirror — a plausible number, wrong (the corpus phrase from
`index-types/208`). For `|i − j| ≤ w` the predicate is invariant and the storage is
sound. The check is decidable: for class A, substitute each generator (adjacent
transposition) into the constraint system and compare normal forms; for class B,
evaluate `P` on every box cell and its images (bounded by the same cap the enumeration
already pays). Two further points: `AntisymIdx`'s unstored diagonal makes `P` on the
diagonal irrelevant to storage but the full-domain fold still treats those cells as
zero, consistently; and a constrained domain **never licenses symmetric storage by
itself** — the licence remains array identity in commuting positions (CLAUDE.md;
`shared_units_insufficient` in `docs/proofs.md`).

**Offsets.** Once `D_canon ≠ C`, the combinadic (`prefixTerm`) is no longer the storage
bijection; offsets must be re-derived over survivors (the cosmology plan's
`PlaceConstrainedCombinatorial … prefix-sum offsets over survivors`). This is why v1
below takes `Symmetry = SymNone` only.

---

## 3. Design

### 3.1 Recognition — from the `static struct` conjuncts, not a new bound family

The retired plan's §3.1 arbitration already made `where p1, p2, p3` (and the
parenthesised / array spellings, flattened by `Ast.structConjuncts`, `src/Ast.fs:854`)
the normative surface, and the counting layer already normalises bounds to inclusive
boxes (`StructIdxFence.fs:118-133`). Recognising the conjuncts keeps **one predicate,
two readings, three consumers** (construction guard `synthesizeStructChecks`,
`TypeCheckInfer.fs:12038-12065`; `idx_card`; iteration) and gives the enumerator its
certificate for free: below the cap, the closed-form enumeration must equal
`enumerateBox`'s set *and order* — the house discipline, as a third route.

Recogniser (new, in `StructIdxFence.fs` beside the fence; pure over `Expr`):

1. Split top-level `&&` (`ExprBinOp (_, OpAnd, ...)`) into conjuncts; drop
   `__ppl_*` licence conjuncts (`StaticEval.isPplLicenseConjunct`).
2. Normalise each conjunct to `lin(x) ⋈ 0`, `⋈ ∈ {≤, <, ==}` (`OpLe/OpLt/OpEq`,
   `src/Ast.fs:64-67`; `≥`/`>` by swapping), where `lin` is an integer linear form over
   field names: `ExprVar f` → coefficient 1; `ExprLit`; `OpAdd/OpSub`; `OpMul` with one
   side folding to a static integer (`StaticEval.evalExprWith` on the constant side);
   `abs(e) <= c` → the two conjuncts `e <= c`, `-e <= c`; `e % m == c` → A3 tag. A
   `static function` call whose body is a single expression may be inlined
   syntactically (bodies live in `env.StaticFunctions`, as `TypeLower.fs:1173` already
   reads them for `DepIdx`) — *v2 nicety*; v1 treats calls as class B.
3. Classify: all conjuncts two-variable unit-coefficient inequalities → A1; one
   equality with innermost coefficient ±1 plus A1 → A2; congruences on one variable or
   on the innermost sum → A3; anything else → B.
4. For A1/A2: Fourier–Motzkin projection in nesting order (eliminate `x_r`, then
   `x_{r−1}`, …) to obtain, per level, `lower_k = max(lo_k, {x_j + c})`,
   `upper_k = min(hi_k, {x_j + c})` as lists of affine terms over earlier fields.
   Because difference constraints are closed under projection, the lists are finite
   and small (≤ r terms each). Emptiness (`lower > upper` for every prefix) is
   detected here and becomes the derived-empty warning the counting layer explicitly
   leaves to "whichever later layer makes a `range<R>` no-op loop reachable"
   (`StructIdxSpec.fs` header, caps section).
5. Output a `DomainPlan`:

```fsharp
type LevelBound = { Base: int64; Terms: (int * int64) list }   // max/min over Base and x_j + c (j = level index)
type LevelPlan  = { Field: string; Lo: LevelBound; Hi: LevelBound; Step: int64 (* A3, else 1 *);
                    Solved: (int64 * (int * int64) list) option (* A2 innermost: c + Σ a_j x_j *) }
type DomainPlan = { Name: string; Fields: FieldBox list; Levels: LevelPlan list;
                    Card: int64 (* closed form or counted *); Class: A1 | A2 | A3 | B of int64 list list }
```

### 3.2 IR representation

- **New index kind** `IxKConstrained` with sentinel `"__constrainedidx"` (`Types.fs:126-175`,
  `ixKindSentinel`/`ixKindOfTag` `:159`, `:472`), `Rank = r`, `Symmetry = SymNone`,
  `Tag = Some "__constrainedidx:<StructName>"` so the kind maps from the prefix like
  `IxKIrreps`. It must **not** join the `IxCompound` family (`IR.fs:1296-1298`) — a
  constrained slot contributes `Rank` nested levels (`IRLoopStructure.fs:70` already
  does that for every non-`IxCompound` kind), not one table-driven level.
- **New extent carrier** `IRDomainSpec of DomainPlan` beside `IRCompoundMask`/
  `IRSparseKeys` (`IR.fs:233`, `:253`; children walker `:1849-1852`; the two
  extent-carrier arms `:2383`, `:2980`). Class B plans lower to the *existing*
  `IRSparseKeys (SkStatic entries)` instead — no new machinery, given-order = lex order
  by `certify`.
- **New placement** `PlaceEnumerable` in `PlacementClass` (`Types.fs:42-45`);
  `placementOf` (`IR.fs:600-611`) maps `IRDomainSpec` to it; `IRStorage.fs:277-298`
  returns `Card` (static); `allocRoutineFor` (`:464-486`) returns a flat pool of `Card`
  cells (dense allocator over a rank-1 extent) plus a **prefix base table**.
- **`LoopIndexBinding` gains two optional fields** (`IRStorage.fs:49-63`):
  `LowerBound : LevelBound option` and `UpperBound : LevelBound option`, `None` at every
  existing constructor (`:1010-1011`, `:1152-1153`) so no existing site changes
  behaviour. `IRDomainSpec` levels set both; `Step` and `Solved` ride the same record.
  Every structural comparison of bindings must include them (`CodeGenFusion.fs:104`,
  `:141`), and `genNestPragma`'s `isRectangular` (`CodeGenLoopNest.fs:516`) must treat
  `Some _` as non-rectangular (→ outer-only `schedule(dynamic)`, `:483-484`, the
  triangular policy; collapse stays disabled).
- Kernel parameters bind **field values** in declaration order — `int64_t <p> =
  __i<k>` (the loop variable *is* the value because the loop runs from `lower_k`),
  never a packed offset. Tag: see Decision 2.

### 3.3 Emitted C++ per class (rank 2 shown; `pool`, `base` per §3.2)

Prologue (once per domain value; O(n₁), hoistable to module scope for a static struct):

```cpp
// base[i] = number of cells with first coordinate < i (prefix sum of row lengths)
int64_t R_base[N1 + 1]; R_base[0] = 0;
for (int64_t i = lo1; i <= hi1; ++i) { int64_t jl = max(lo2, i - w), jh = min(hi2, i + w);
                                       R_base[i - lo1 + 1] = R_base[i - lo1] + max<int64_t>(0, jh - jl + 1); }
```

A1 (band):

```cpp
for (int64_t i = lo1; i <= hi1; ++i) {
    const int64_t jl = max(lo2, i - w), jh = min(hi2, i + w);          // hoisted per level
    double* BLADE_RESTRICT row = pool + R_base[i - lo1];
    BLADE_IVDEP
    for (int64_t j = jl; j <= jh; ++j) row[j - jl] = K(i, j);
}
```

A2 (solved innermost, e.g. `m_out = m1 + m2` over a projected A1 prefix): the innermost
loop disappears — `const int64_t m_out = m1 + m2; pool[R_base2[...]] = K(m1, m2, m_out);`
with the in-bounds test present only when projection was inexact (an `if`, never a
loop). A3: `for (int64_t j = first_ge(jl, m, c); j <= jh; j += m)`. Class B: the existing
`sparse_index_t` driver (§1.3), unchanged.

Reads: `X(i, j)` full-arity → `pool[R_base[i - lo1] + (j - jl(i))]` guarded by
`jl(i) <= j <= jh(i)` (panic BL8006-style on an absent cell, mirroring compound's
missing-cell contract, `nested_array_types.hpp:139-141`). Folds: the flat `.data` walk
over `Card` cells, i.e. the arm at `CodeGenExpr.fs:1076-1085` widened from
`isCompoundArrayType || isSparseArrayType` to include the new kind (`CodeGenState.fs:1331`,
`:1339` get a sibling predicate). The fold order over present cells is lex, identical
to the compound route and to the zero-padded scan — which is why the three totals in
§1.2 agree and why the gate can demand bit identity.

### 3.4 Consumers

- `method_for(range<R>) <@> k` — the driver; output type `Array<T like R>` (one slot of
  the new kind, `Card` cells). Single-slot rule as for compound (`TypeCheckInfer.fs:1386-1391`).
- `reduce(X, (+))` on `Array<T like R>` — flat walk (§3.3). `axes =` other than the
  full fold: refuse in v1 (partial folds need the per-prefix row view).
- `X(i, j)` reads — §3.3. Partial reads `X(i)` (a rank-1 row over `[jl(i), jh(i)]`,
  the `BoundedIdx` remainder of formalism §3.6): refuse in v1.
- `compound(X, m)`, `mask`, `decompact`, `transpose`, `<|:>`, `stack`/`join`, AD, MPI,
  CUDA, LLVM: refuse with a named message; no silent fallthrough into the dense
  machinery (the CUDA compound arm at `CodeGenCuda.fs:2649` and the fusion refusal at
  `CodeGenFusion.fs:779-785` are the templates).
- `omp(...)` on the kernel: outer-level `schedule(dynamic)` only, from the bound shape;
  a fold over the flat pool takes the existing `parallel for simd reduction` route
  (`docs/features.md:330`) untouched.

### 3.5 Interpreter twin

- `interpretNest` (`src/Interp/Loops.fs:1596-1600`, `:1679` `peelElement`): evaluate
  `LowerBound`/`UpperBound`/`Step` from `idxVals` of earlier levels; iterate
  `[lower, upper]` by `step`; bind the param to the loop value. A `Solved` level
  evaluates the affine form and runs zero or one iteration.
- A `materializeConstrainedRangeMap` beside `materializeCompoundRangeMap` (`:1133`)
  producing a value with `Data`, `Base` table and the plan; reads and the flat fold in
  `src/Interp/ArrayOps.fs`; print parity in `src/Interp/Print.fs:211`.
- Certificate inside the interpreter: for boxes ≤ cap, assert the interpreted cell
  count equals `StructIdxSpec.structCard` — the interp lane is where a cheap runtime
  cross-check is free.

### 3.6 Refusals and fallbacks

| condition | behaviour | where |
|---|---|---|
| conjunct not in class A, box ≤ cap | class B: baked static table (`SkStatic`); **advisory** that the domain is table-enumerated (Decision 4) | recogniser → `TypeLower` |
| conjunct not in class A, box > cap | refuse (existing cap message; a dedicated code per Decision 5) | `enumerateBox` |
| empty domain | warn, `Card = 0`, `range<R>` is a no-op loop | recogniser |
| dependent bounds `Int<min=l1, max=4>` | now **expressible** as A1 conjuncts; `index-types/152` exists to force this deferral to be closed deliberately — it flips in the same change | fence |
| `where comm(...)` on a kernel over `range<R>`, or a symmetric output over the new kind | refuse in v1 (BL4017-style "licenses nothing here"); v2 requires the §2.4 invariance check | typecheck |
| `range<R, J>` multi-slot | refuse like compound | `TypeCheckInfer.fs:1386` |
| certificate disagreement (closed form vs the two routes, below cap) | `failwith` — compiler bug, house rule | `StructIdxSpec.certify` |

### 3.7 Decisions for the user (not chosen here)

1. **Spelling of the index type.** Use the `static struct` name directly —
   `range<R>`, `Array<T like R>` — as corpus 145's header and the retired plan's C2
   intend; or add a dedicated closed-form family (`BandIdx<I, w>`) that needs no
   recogniser. The struct route is assumed below; it is the only one whose count is
   already certified.
2. **Field types and tag flow.** Today a field must be `Int` (`Nat` excluded by design,
   `StructIdxFence.fs:88-94`) and a named index type as a field type is refused outright
   (BL4003 "Index types cannot appear as struct fields", observed). Either (a) allow
   `i: I` for a declared `type I = Idx<N>` to mean the box `[0, N)` *and* give the
   kernel param tag `I` (closing the BL4003 warning spray of §1.2), or (b) keep `Int`
   fields and accept untagged params. (a) touches the struct-field validation rule
   (`src/TypeCheckValidate.fs:94` region) and the fence.
3. **Kernel parameter binding**: positional in declaration order (like compound/sparse
   ranges) vs field-named (the retired plan's deferral C3). Positional assumed.
4. **Class B fallback policy**: silent (hides a performance cliff), advisory warning
   (BL4010-style, consistent with the fastest-way advisory channel), or refusal.
   Advisory assumed.
5. **Diagnostic code**: keep the fence's generic BL3999 `Other` or mint a code
   ("constrained domain not enumerable" / "domain enumerated by table") — five touch
   points including `protocol/surface.json` and `protocol/data/diagnostics.json`.
6. **Admit A5 (triangle) in v1?** It costs nothing extra (it is A1 in the innermost
   field) but must be documented as a constant-factor shrink, not an asymptotic win.
7. **Absent-cell reads**: panic (compound's contract) vs zero-fallback semantics via
   `<|:>`.

---

## 4. First gate

**Programs** (all in `tests/fixtures/` or a private bench dir; extents 509 and 1021,
never a power of two):

- G1 band: `static struct Band { i: Int<min=0, max=N-1>, j: … } where i - j <= w, j - i <= w`,
  w ∈ {3, 16}; complete consumer = `method_for(range<Band>) <@> lambda(i, j) -> A(i) * A(j)`,
  `reduce(R, (+))`, one full-arity read `R(3, 5)`.
- G2 affine: the CG anchor `m1 + m2 == m_out` at `l = 40` (box `(2l+1)³ = 531 441` —
  **above the cap**, so the gate also demonstrates that class A is not box-capped),
  consumer = fill + fold + one read.
- Three arms per program: (a) predicate scan (§1.2 row 1), (b) mask + `range<CompoundIdx<m>>`
  (row 3), (c) the new route. For G2, arm (b) is `SparseIdx` over a `let static` list of
  the 7 (small l) or runtime-built (large l) keys.

**Correctness**: totals and reads bit-identical across arms (the fold order over present
cells is lex in all three; §1.2 shows this already holds for (a) vs (b)); interp lane
agrees (`blade test interp index-types`, `--diff-oracle`); `idx_card(Band)` equals the
closed-form `n(2w+1) − w(w+1)` (3551 at n=509, w=3; 7135 at n=1021).

**Counting the visited cells** — two instruments, both required:
1. *Emitted bounds*: the `.cpp` for arm (c) must contain no loop pair with two
   `N`-extent headers over the domain; the inner header must read `j <= jh` with `jh`
   derived from `i` (grep-able), and G2's innermost level must be an assignment, not a
   loop.
2. *Runtime census*: a measurement-only, per-call env knob (`BLADE_NEST_CENSUS`, read
   like `BLADE_LLVM_BRICKS`) that adds one counter increment to every kernel-body
   emission; the census for arm (c) must equal `Card` exactly, and arm (a)'s must equal
   `n²`. The knob must be **off** in every timed arm and the control arm must be pinned
   explicitly (the SimplexBlocks unset-knob trap).

**Timing**: 27 interleaved samples per arm, medians, whole program (fill + fold + read),
both extents, both widths; report ratios (a)/(c) and (b)/(c). Expectation from cell
counts at n = 1021, w = 3: card/n² ≈ 0.7 %, so a ≥ 5x end-to-end win over (a) is the
bar after allowing the O(n) prologue, allocation and process overhead; parity with (b)
is not acceptable — (b) is the "saves storage, same discovery work" route.

**STOP conditions** (any one): values differ between arms or lanes; arm (c) fails to
beat arm (a) by ≥ 3x at n = 1021, w = 3 (that would mean the constant factors of the
prologue/base table dominate and the design needs re-thinking, not tuning); the
recogniser needs more than the difference-constraint + solved-equality class to cover G1
and G2; the interp twin requires a second bound-evaluation code path rather than reading
the same `LevelBound` record.

---

## 5. Size, risk, files

**Size: M–L** (rank 2 A1/A3 + solved-innermost A2, C++ and interpreter lanes, no
symmetry). Rough: recogniser + third route 400–500 lines F#; IR/typecheck/lowering
300–400; C++ emitter 400–500; interpreter 250–350; runtime header 100–150; corpus 12–15
files; docs. Adding `PlaceEnumerable` and `IxKConstrained` forces exhaustive matches to
update, which is the desired blast radius.

**Risks** (in order of likelihood):
1. `LoopIndexBinding` is compared structurally in fusion (`CodeGenFusion.fs:104`,
   `:141`) and matched by shape in the compact flat-write plan
   (`CodeGenLoopNest.fs:902-903`, `:2172`); new fields must participate or a band nest
   could be mistaken for a rectangular/simplex one.
2. Inheriting the `range<SymIdx>` packed-coordinate convention by accident — the
   `VirtualRange` peel arm (`CodeGenLoopNest.fs:93-108`) binds the raw counter; the new
   arm must bind the value (loop runs from `lower_k`) and both lanes must agree
   (`Interp/Loops.fs:1684-1700`).
3. Tag flow (Decision 2): without it every kernel over `range<R>` indexing a tagged array
   warns BL4003, as both probes did.
4. Baked extents / shape monomorphisation (`param-extent-baking`): `Card` is static, so
   functions taking `Array<T like R>` bake correctly, but a *runtime* mask-derived domain
   must stay on the compound route — the two kinds must not unify.
5. The cap semantics change: class A is uncapped, class B stays capped — corpus 153
   stays valid only because its conjuncts (`a == 50, b == 50, c == 50`) are three
   *solved* equalities; the recogniser would classify it A2 and enumerate one cell. Either
   153's struct gets a non-linear conjunct, or its pin changes — decide before landing.
6. Diff gates skip refused programs, so an LLVM/CUDA refusal keeps `--diff-oracle` green
   without proving anything about those lanes; that is acceptable for v1 if the refusal
   is by name.

**Files to touch, in order:**
1. `src/StructIdxFence.fs` — recogniser (`Expr` → `DomainPlan option`), FM projection;
   optional named-index field types (Decision 2).
2. `src/StructIdxSpec.fs` — `routeClosedForm` as third route, certified against
   `routeFlat`/`routeHeads` below the cap; `Card` from the plan above it.
3. `tests/Test_StructIdxSpec.fs`, `tests/Test_StructIdxOracle.fs` — third-route
   assertions on the anchor family (CGm112, band, parity).
4. `src/Types.fs` — `IxKConstrained` + sentinel; `PlaceEnumerable`.
5. `src/IR.fs` — `IRDomainSpec`; `placementOf`; walker arms.
6. `src/IRStorage.fs` — `LoopIndexBinding.LowerBound/UpperBound/Step/Solved`;
   cardinality; allocator; `buildLoopNestCodeGen` level planning.
7. `src/IRLoopStructure.fs` — carry the plan through `IndexSpaceInfo`.
8. `src/TypeLower.fs` — `lowerIndexType` arm for a `TyNamed` resolving to a static
   struct (registry: `TDIIndexType`, `TypeEnv.fs:86`; `TypeLower.fs:1169`);
   `elemTypeForIterationIndex`.
9. `src/TypeCheckInfer.fs` — `range<R>` (`:1318-1400`), `Array<T like R>` annotations,
   the refusal list of §3.6.
10. `src/CodeGenLoopNest.fs` — `genForLoopHeader`/`genLoopBoundExpr` (`:718-780`) with
    lower bounds and steps; peel arm; base-table prologue; `genNestPragma`.
11. `src/CodeGenExpr.fs`, `src/CodeGenState.fs`, `src/CodeGenBinding.fs` — reads, flat
    fold, allocation/teardown of pool + base table.
12. `src/cpp/nested_array_types.hpp` — `Constrained<T, RANK>` wrapper (data, base,
    bounds closure) or a `Compound`-shaped twin with closed-form `linearize`.
13. `src/Interp/Loops.fs`, `src/Interp/ArrayOps.fs`, `src/Interp/Print.fs`.
14. `src/CodeGenFusion.fs`, `src/CodeGenCuda.fs`, `src/EmitLlvm.fs`, `src/Grad*.fs` —
    named refusals.
15. `src/Diagnostics.fs`, `src/Unify.fs`, `src/TypeEnv.fs`, `protocol/surface.json`,
    `protocol/data/diagnostics.json` — only if Decision 5 mints a code.
16. Corpus: `tests/corpus/index-types/260+` (band values, band fold, CG solved
    innermost, parity progression, empty-domain warning, class-B advisory, multi-slot
    refusal, comm refusal, absent-cell read); flip `152`; resolve `153` (risk 5).
17. Docs: `docs/formalism.md` §3.5 (a new index-type row) and §7.3; `docs/features.md`;
    `docs/plans/README.md` index row for this file.

---

## 6. Recommendation

**GO — narrowly.** Build the rank-2 class A1/A3 enumerator with the solved-innermost
A2 extension, on the C++ and interpreter lanes, `Symmetry = SymNone` only, gated by §4.
The design reuses what exists (the certified counting layer, the `where` conjunct
surface, the simplex nest's per-level bound machinery, `SparseIdx` as the class-B
fallback) and adds no new syntax beyond letting a `static struct` name stand where an
index type stands — which corpus 145 already promises. The measured baseline (§1.2) shows
the gap is real: today's table-backed route is *slower* than the scan at n ≈ 1000.

Do **not** start the symmetric/constrained combination (the cosmology bispectrum
customer) until `plan-compact-sym-folds.md` §4's fold decision lands and the §2.4
invariance check is specified against it; and do not sequence this ahead of the memo's
first three experiments unless the cosmology notebook is picked up, since band storage
has no in-repo customer yet — the demand record is the cosmology plan's §5.2.
