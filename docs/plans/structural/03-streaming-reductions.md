# 03 — Streaming across reduction boundaries (bounded-memory pairwise pipelines)

Status: RESEARCHED + MEASURED 2026-09-06; nothing built. Elaborates item 3 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md)
("Make whole reductions compose into bounded-memory execution").

Baseline: the working tree read on 2026-09-06 (HEAD `e7abf38`, one commit past `bd019dc`;
`src/` unchanged by this work), Release binary built 2026-09-06 00:43. Every claim below is
either **verified** (read in the current source at the cited `file:line`, or observed in C++
emitted / executed from a private temp directory) or marked **inferred**. No `dotnet build`,
no `blade test`, no edits under `src/` or `tests/`. Probe programs and their emitted C++ live
in `%TEMP%\blade-stream-03` (session-local; the load-bearing excerpts are reproduced here).

## 0. Verdict

The memo's own first instruction was to compare against the best existing Blade spelling
before proposing machinery. That comparison is decisive: **the bounded-memory behaviour the
memo asks for already exists by composition.** A per-row kernel whose body is a reduction
join (`object_for(<&!>) <@> (reduce(e, (+)), prodsum(e, v))` over a deferred `e`) — or a
one-pass fold with a tuple-valued running state — evaluates the softmax-normalized weighted
sum `y[i] = Σ_j exp(s(i,j)) v[j] / Σ_j exp(s(i,j))` with **no M×N pool**: peak working set
5 MB at 3061×3449 versus 166 MB for the materialized spelling, and 1.6× faster end to end
(§1.8). Nothing in the compiler needs to learn a "summary" to reach this.

What the comparison also surfaced, and what this document recommends acting on:

1. Two **defects** in the existing machinery, both small (§3.3, D1/D2): a tuple-projection
   result inside a row kernel leaves the kernel's output type unpinned and it falls to the
   *loop-index type* (the emitted C++ then fails under `-Werror=float-conversion`); and a
   reduction join's `reduce(e, op)` leg does not read the per-iteration share it declares,
   so the shared producer is spelled twice per iteration.
2. One **contained compiler change** with a verified payoff (§3.3, C): the natural spelling
   `reduce(method_for(A, B) <@> k, (+))` materializes the M×N today on BOTH the default
   (partial) route and the `axes = 2` route (§1.3). A typecheck-only rewrite of the partial
   route into the per-row shape removes it, riding routes that already exist.
3. The general "registered summary with init/step/merge/finalize + compiler-derived tiling"
   is **not warranted now**: on the CPU lane its ceiling over the existing spelling is the
   second `score` pass (≤ 2× arithmetic, zero memory) and one `exp` per pair in the online
   form. The one place a compiler-owned fold reaches something no spelling can — symmetric
   pair sums per particle in N²/2 work without an N×N pool — belongs to
   [plan-compact-sym-folds.md](../plan-compact-sym-folds.md) §5.6, not here.

Recommendation (§6): **DEFER** the compiler feature; **GO** on the idiom package and the two
fixes; schedule the partial-fold rewrite inside plan-deferred-combinators' D-phases.

---

## 1. Verified current state

### 1.1 The `reduce` routes

`inferReduce` (`src/TypeCheckInfer.fs:2203`) infers the operand once (`tArrCache`, `:2221`,
so ids do not shift) and dispatches in this order — verified against the code, matching
plan-deferred-combinators §1.2:

| order | route | where | what it builds |
|---|---|---|---|
| 1 | leading-axis fold (array-valued kernel, rank ≥ 2) | `leadingAxisFold`, `:2520`; taken at `:2631` | `let mut acc = copy(G(0)); for j { acc = kernel(acc, G(j)) \|> compute }` — a fresh array **per step** (`:2590` names the source; see p6 in §1.8) |
| 2 | partial fold (scalar kernel, rank ≥ 2, `axes` < rank; **the default**) | `partialFold`, `:2330`; taken at `:2638` | rewritten to `(method_for(SRC) <@> lambda(row) -> reduce(row, op[, init])) \|> compute`. `SRC` must be a **named buffer** (`:2406` `srcIsNamed = array.Kind.IsExprVar`); anything else — including a deferred `A <@> k` — is bound to `let __pfsrc… = <operand>` first (`:2407`), which **forces it** |
| 3 | rank-k full fold (`axes` = rank ≥ 2, plain dense, scalar elem) | `rankKDesugar`, `:2651`; taken at `:2741` | `let __rksrc = <operand>` (`:2719`) + hand-built `StmtForIn` nest with one scalar accumulator; the source `let` **materializes a deferred operand**. Declines only when the fold kernel carries `omp` (`deferredWithOmpKernel`, `:2665`), sending it to route 4 so the licence is not dropped |
| 4 | fused reduction terminal | `tryInferReduceCompute`, `:1760`, called at `:2763` after `checkFoldOmpLicense` (`:2751`) | `TExprReduce` over the spliced deferred apply / `<&!>` tree with a seed always filled → `IRReduceCompute` (`src/Lowering.fs:788-796`) |
| 5 | base rank-1 array fold | `:2765` onward | `IRReduce` |

Consequence (verified in emitted C++, §1.3): **only** route 4 avoids an intermediate array,
and a deferred rank-≥2 operand reaches route 4 only when the fold is the full fold AND the
fold kernel is `omp`-annotated (corpus `tests/corpus/loops/111_reduce_omp_comm_kernel.blade`
line 22 is exactly that case). The unannotated `(+)` full fold and every partial fold over a
deferred computation materialize it.

### 1.2 The fused terminal and the join: what they emit

`IRReduceCompute (computation, kernel, init)` is documented at `src/IR.fs:148-169`; the join
encoding (tuple kernel + tuple seed = one fold per leg) at `:159-168`. Lowering:
`src/Lowering.fs:788-796`. C++ dispatch `src/CodeGenBinding.fs:81` →
`genReduceComputeBinding` (`:2767`) → `genReduceComputeBindingCore` (`:2795`), which routes
the tuple-kernel form to `genReduceJoinCore` (`:2801` → `:3253`).

Emitted shape, observed (probe p2, per-row kernel `__lambda_57`; §2.1 has the source):

```cpp
// pass 1: running max, seed -1e300
double __v28 = -(1E+300);
for (size_t __i0 = 0; __i0 < 53; __i0++) { double k____i0 = k[__i0];
    __v28 = __wrap_47___v28(__v28, score(__v21, k____i0)); }
// pass 2: the join -- ONE loop, one accumulator per leg
// __v31 = <deferred computation (reduction-join operand)>
// reduction join: 2 leg(s), sharing __v31 per iteration
double __v36_0 = 0.0;  double __v36_1 = 0.0;
for (size_t __i0 = 0; __i0 < 53; __i0++) {
    double k____i0 = k[__i0];  double v____i0 = v[__i0];
    const double __v31 = std::exp((score(__v21, k____i0) - __v28));            // the share
    __v36_0 = __wrap_53___v36_j0(__v36_0, std::exp((score(__v21, k____i0) - __v28)));  // leg 0 re-spells it
    __v36_1 = __wrap_56___v36_j1(__v36_1, (__v31 * v____i0));                  // leg 1 reads it
}
auto __v36 = std::make_tuple(__v36_0, __v36_1);
```

So: **one traversal** (verified), **no temporaries** beyond scalar accumulators (verified),
and one **sharing gap** (verified): the `reduce(e, (+))` leg recomputes the producer while
the `prodsum(e, v)` leg reads `__v31`. Cause, read in the emitter: `deferredOperand`
(`src/CodeGenBinding.fs:3302-3309`) recognizes a shared name only in a leg's `Arrays`
slots and `repoint` (`:3348`) rewires exactly those; a leg whose traversal **is** the shared
computation has no operand slot, because typecheck spliced the *resolved* apply as the leaf
(`collect`, `src/TypeCheckInfer.fs:1764-1783`: "a COPY of the computation, not a reference
to the binding"). The share note at `:3435` is therefore only half true for such legs.
Whether g++ at `-O3` CSEs the duplicated `std::exp(score(...))` (both callees are pure and
visible in the TU) was **not verified** at the object level; the emitted C++ carries two.

Other properties of the terminal, all read in the source:

- A `(+)`/`(*)` section seeds with its identity; **any other kernel requires the 3-arg
  init** (`src/TypeCheckInfer.fs:2761-2762`): a fused nest cannot seed-with-first.
- A single-leaf fold whose kernel is `omp`-licensed gets the **outermost level chunked**
  across a team with per-thread partials and a fixed-order combine (`chunkable`,
  `src/CodeGenBinding.fs:2888-2896`, `FoldChunk`); the join form stays serial.
- `BLADE_FP_REASSOC` lanes ride the same licence predicate (`:2986`, `:3077-3140`).
- **Tuple-valued accumulators work** (probes p7/p8b): a map whose kernel returns
  `(score, 1.0, x)` fused into a fold whose kernel is `tuple × tuple → tuple` emits
  `std::tuple<double,double,double> __v35 = std::make_tuple(-(1E+300), 0.0, 0.0);` and a
  per-cell `__v35 = __wrap(__v35, std::make_tuple(score(...), 1.0, v____i0))` — nothing is
  materialized. This is the enabling fact for the one-pass online form (§2.2).

### 1.3 A `method_for(A, B) <@> k |> reduce` pipeline materializes the M×N today

Probe p4 (37×53, non-power-of-two; both bindings in one program):

```blade
let z   = reduce(method_for(q, k) <@> lambda(a, b) -> exp(score(a, b)), (+))            // default axes = 1
let tot = reduce(method_for(q, k) <@> lambda(a, b) -> exp(score(a, b)), (+), axes = 2)  // full fold
```

Emitted `main` (verbatim structure):

```cpp
// __v29 = <deferred computation>
static constexpr const size_t __v29_extents[2] = { 37, 53 };
Array<double, 2> __v29 = { allocate<...>(__v29_extents), __v29_extents };   // route 2's __pfsrc
for (__i0 < 37) { ... for (__i1 < 53) __orow___v29[__i1] = std::exp(score(q____i0, k____i1)); }
Array<double, 1> z = ...; for (__i0 < 37) { Array<double,1> __v29____i0 = { __v29.data[__i0], ... };
    z[__i0] = [&]() { ... double __r = __v29____i0[0]; for (__ri = 1; ...) __r = __wrap_54(__r, __v29____i0[__ri]); return __r; }(); }
// __v43 = <deferred computation>
double __v44 = 0.0;
Array<double, 2> __v43 = ...;                                                 // route 3's __rksrc
for (__i0 < 37) { for (__i1 < 53) __orow___v43[__i1] = std::exp(score(q____i0, k____i1)); }
for (__k45 < 37L) for (__k46 < 53L) __v44 = (__v44 + __v43[__k45][__k46]);
double tot = __v44;
```

Two full M×N pools, two full passes each. This is the memo's "logical score array need
never be materialized" case, and today it is materialized on both routes. (The
`|> compute`-then-`reduce` spelling and the named-operand spelling `let S = …; reduce(S,
(+))` materialize as well — the latter additionally puts `S` on the auto-print list when it
is forced on read, `forceDeferredArrayInput` / `forceDeferredPositionalReads`,
`src/CodeGenBinding.fs:1869` / `:1904`.)

### 1.4 What the deferred-`reduce` machinery behind the declines already supports

Read in `tryInferReduceCompute` (`src/TypeCheckInfer.fs:1760-1935`) and the emitters:

Supported: an anonymous `|> compute` at the operand root is seen through (`:1770-1783`,
`anonymousUnwrap` `:1882`); a single unforced apply of any rank (full fold); an `<&!>` tree
of applies with a shared fold; the join (per-leg kernel/seed) in both spellings (Form 2
`reduce([...], (<&!>))` at `:1648-1660` via `joinLegListOf` `:2081` / `inferReductionJoin`
`:2101`; Form 1 `object_for(<&!>) <@> (...)` at `:4720`); tuple-valued accumulators (§1.2);
`omp` chunking of a single leaf (§1.2); one level of named-deferred sharing across legs
(`src/CodeGenBinding.fs:3309-3325`, with the gap of §1.2); the L1 dot dispatch for
`reduce(<zip under *>, (+))` over two rank-1 f64 pools (`:2900` neighbourhood).

Declined (each a forced materialization or a refusal): composed applies (`:1794`, and per
leg at `:2126`); body-local or captured **named** lets unless the fold kernel is parallel
(`alreadyMaterializedLet`, `:1864-1875`; `foldKernelIsParallel`, `:1856`) — the S2 forcing
regime `forceBareCombinatorLets` (`src/Lowering.fs:332-348`) with the join-operand exemption
`joinDeferrableIdsMany` (`:287-330`); compact symmetric / antisymmetric / Hermitian output
(`:1928`, the BL3999 family owned by plan-compact-sym-folds); ragged / dependent-inner /
grouped / compound inputs (`unsupportedFusedInput`, `:1900-1908`, mirrored in
`genReduceComputeBindingCore` and `genReduceJoinCore` `:3277-3284`); cuda/mpi legs in a
join (`:3286-3293`); sharing deeper than one level (`:3319-3324`). The `<&!>` tree has **no
partial form** (`operandRank` returns `None` for a tree, `:2265-2275`), so "join two per-row
partial folds at module level" cannot be written — the per-row kernel is the only place a
join over `j` can live when the result is indexed by `i`.

### 1.5 The fusion pass never crosses a reduce

`optimizeModule` (`src/Optimize.fs:158-165`) runs `foldConstMatchesModule` then
`fuseElementwiseChainsModule` (`src/IRMono.fs:1057`). Its `plainInfo` gate (`:1112-1135`)
admits only dense `IxKPlain`/`SymNone` records, rank-0 scalar kernels, no comm/omp
metadata, no Reynolds, and — for arity ≥ 2 — genuine co-iterations (`IsCoIteration`,
because splicing into an outer product would change meaning). Nested computes reached
through a `let` are out of scope by its own comment (`:1045-1049`). It fuses map chains into
one kernel; it has no arm for `IRReduce`/`IRReduceCompute` and no notion of a consumer that
normalizes a producer. Extending *it* is the wrong lever (the memo says as much); the
reduce-side machinery of §1.2/§1.4 is the relevant one.

### 1.6 `ppl.mstate` / `mstate_merge` are an elaboration-time state, not a runtime fold

`elabMState` (`src/ppl/compiler/PplElaborate.fs:797-844`) takes an **annotated module-level
array with a statically known variable-axis extent** and emits straight-line declarations:
the pool sweep plus one `ArrayLit` per central comoment order (Pébay's subset-lattice
formula generated cell-wise). `elabMStateMerge` (`:846-888`) merges two such states with
compile-time weights `cA = -nB/n`, `cB = nA/n` into new straight-line declarations;
`elabMStateCumulants` (`:890-931`) finalizes. The state is a `Map<string, MStateInfo>`
threaded through the declaration walk (`:4436-4443`); the merge is a monoid and the corpus
pins associativity (`tests/corpus/ppl/026_mstate_associative.blade`;
`examples/05_streaming_telemetry.blade` is the worked example). It is exactly the memo's
"init / merge / finalize" precedent — but at elaboration time over static shapes, with no
runtime tiling and no connection to `reduce`. It shows the *shape* of a declared summary; it
is not a mechanism to generalize to user kernels.

### 1.7 Licences and the floating-point contract already in place

- `foldReorderLicensed` (`src/CodeGenExprSupport.fs:1160-1164`): a fold may be regrouped
  across threads/lanes iff the kernel is not repro-vetoed AND (declared `comm`, or a comm
  group, or a recognised builtin body `p0 <op> p1` with `op` commutative+associative —
  `foldKernelBuiltinOp`, `:1120-1137`). "Associativity is the part `omp` itself asserts"
  (`:1096-1097`, the trust model).
- `checkFoldOmpLicense` (`src/TypeCheckInfer.fs:2949-2976`) refuses `omp` without that
  licence — BL4016 (`src/Diagnostics.fs:324-328`).
- `where repro` (`src/Ast.fs:379-388`) vetoes every reorder licence
  (`foldKernelReproVetoed`, `src/CodeGenExprSupport.fs:1145-1152`), turns off contraction
  and library routing in the body (`src/CodeGen.fs:1316-1343`), and is carried by named
  functions only (`src/TypeCheckInfer.fs:9157-9163`).
- `BLADE_FP_REASSOC` supplies the opt-in for lane reassociation on the same predicate
  (`src/CodeGenBinding.fs:2688-2700`).

So the licence a *tiled* summary would need already has a name and a predicate; the
unlicensed path of any new schedule must be the serial left fold the interpreter performs.

### 1.8 Probe ledger

All probes: `score(a, b) = -(a-b)²·8`, `q` over `I`, `k`,`v` over `J`, emitted with
`blade emit`, compiled with `blade compile -o` (g++ 15.2 `-O3 -march=native
-ffp-contract=fast`), run from a private directory. Sizes are non-powers of two.

| probe | spelling | emitted structure | result |
|---|---|---|---|
| p1 | naive: `Sc` (M×N) → per-row max → `E` (M×N) → `z`,`u` per row → `u / z` | 2 rank-2 pools, 5 passes | reference |
| p2 | per-row kernel, max pass + join (§2.1) | 0 rank-2 pools; 2 loops per row | `y` = p1 to 15 digits (first 10 cells compared) |
| p2b | per-row kernel, one-pass join without max (bounded kernel; §2.3) | 0 pools; 1 loop per row | `y` = p1 to 15 digits |
| p3 | p2 with `where omp(i: 1)` on the row lambda | `#pragma omp parallel for` on the row loop (also inside a function body: o_a.cpp:307) | same |
| p4 | `reduce(deferred M×N, (+))` default and `axes = 2` | **both materialize** (§1.3) | — |
| p5 | M×D output: row kernel returns a computed rank-1 row | per row: one J-vector pool (freed), one D-row (copied into `Y`, freed) | compiles/emits; not run |
| p6 | one-pass online via **leading-axis fold** over a J×3 state grid | J×3 pool per row + a fresh 3-vector **per fold step** (`__lambda_59_HM…` returns an `Array`) | `y` agrees to ~1e-15; 7.7× slower than p2 at 37×53 |
| p7 | fold with a **tuple** state, trivial kernel | `std::tuple<…>` accumulator, nothing materialized | compiles |
| p8 | one-pass online with tuple state (§2.2) | as p7 with the real merge | **g++ error**: `y` typed `Array<I, 1>` (defect D1) |
| p8b | p8 with `(s[2] / s[1] : Float64)` | as p7 | max abs Δ vs p2 = 1.0e-15 over all 37 cells |
| p9 | p8b with `where comm(a, b)` on the tuple merge | accepted; no emission change (no `omp`) | compiles |

Measurements (main-body wall time printed by the program; three interleaved rounds; peak
working set polled at 1 ms from PowerShell — MSYS `time -v` reports ~6 MB for every variant
and is not usable on Windows for this; 16 logical cores; shared host, so treat times as
indicative):

| variant | 1531 × 1723 | peak WS | 3061 × 3449 | peak WS |
|---|---|---|---|---|
| naive (p1 shape, function-wrapped) | 22.6 / 22.8 / 22.6 ms | 27.7–45.3 MB¹ | 87.1 / 89.7 / 90.9 ms | **166.2 MB** |
| two-pass per-row join (p2 shape) | 13.1 / 13.1 / 13.8 ms | 5.0–5.3 MB | 54.6 / 55.7 / 55.4 ms | **5.1 MB** |
| join + `omp` over rows (p3 shape) | 3.0 / 4.1 / 3.7 ms | 3.3–5.8 MB | 8.2 / 7.6 / 11.2 ms | 5.7 MB |
| one-pass online tuple fold (p8b shape) | 18.2 / 17.9 / 17.4 ms | 5.1–5.3 MB | 73.4 / 76.3 / 80.5 ms | 5.1 MB |

¹ two 21 MB pools touched progressively; the 1 ms poll caught different phases. Expected
≈ 42 MB + 5 MB baseline.

The checksum `reduce(y, (+))` printed identically to 15 significant digits for all four
variants at both sizes (`-6.10518263983325`; `-1.4192556386139`).

Reading: the streaming spellings remove the quadratic storage entirely and are *faster*
serially (memory traffic of three passes over 84 MB pools exceeds the second `score` pass);
the one-pass online form pays two `exp` per pair against the two-pass form's one `exp` +
one extra `score`, and loses to it here (§3.2 explains why that is the honest ceiling for
compiler machinery on this lane).

---

## 2. The best existing spelling

### 2.1 Softmax-normalized weighted sum: per-row two-pass reduction join

```blade
type I = Idx<37>
type J = Idx<53>
function score(a: Float64, b: Float64) -> Float64 = -(a - b) * (a - b) * 8.0
// q: Array<Float64 like I>, k, v: Array<Float64 like J>

let y = (method_for(range<I>) <@> lambda(i) -> {
    let qi = q(i)
    // pass 1: row maximum (a lambda kernel needs the 3-arg seed on the fused terminal)
    let m = reduce(method_for(k) <@> lambda(b) -> score(qi, b),
                   lambda(a, b) -> if a > b then a else b, -1.0e300)
    // pass 2: the shared producer stays DEFERRED -- naming it is the sharing declaration --
    // and the join folds both legs in one traversal of j
    let e = method_for(k) <@> lambda(b) -> exp(score(qi, b) - m)
    let z, u = object_for(<&!>) <@> (reduce(e, (+)), prodsum(e, v))
    u / z
}) |> compute
```

Allocates: `y` (M doubles) and nothing else — `e` never exists (`// __v31 = <deferred
computation (reduction-join operand)>`), and the S2 forcing regime spares it because its
only consumers are join legs (`joinDeferrableIdsMany`, `src/Lowering.fs:287-330`). Rows are
independent, so `where omp(i: 1)` on the row lambda is the parallel structure (p3), and no
fold licence is involved because the fold stays serial per row.

Falls short in: (i) `score` is evaluated twice per pair (max pass + join pass); (ii) the
join's `reduce(e, (+))` leg re-spells `exp(...)` instead of reading the share (§1.2, defect
D2) — a third transcendental per pair unless g++ CSEs it; (iii) there is no `-inf` literal
in the surface, so the max seed is a magic constant (or `score(qi, k(0))`, which costs one
more evaluation per row); (iv) the max pass and the sum pass cannot be fused without a
running state — which is §2.2.

### 2.2 The same, in one pass: a tuple-valued running state

```blade
let y = (method_for(range<I>) <@> lambda(i) -> {
    let qi = q(i)
    let s = reduce(method_for(zip(k, v)) <@> lambda(b, x) -> (score(qi, b), 1.0, x),  // embed
                   lambda(a, b) -> {                                                  // merge
                       let m = if a[0] > b[0] then a[0] else b[0]
                       let ca = exp(a[0] - m)
                       let cb = exp(b[0] - m)
                       (m, a[1] * ca + b[1] * cb, a[2] * ca + b[2] * cb)
                   }, (-1.0e300, 0.0, 0.0))                                           // empty state
    (s[2] / s[1] : Float64)   // ascription REQUIRED today: without it y is typed Array<I, 1> (D1)
}) |> compute
```

Allocates: `y` only; the state is a `std::tuple<double, double, double>` register triple
(§1.2). This is the online-softmax merge of the memo (`m = max`, rescale by `exp(mA − m)`,
`exp(mB − m)`), and it is expressible **today** because the map may return a tuple per cell
and the fold kernel may be an endomorphism on tuples. The empty state `(-1e300, 0, 0)` gives
`exp(-1e300 − s₀) = 0` exactly on the first merge; a genuinely empty row finalizes to `0/0`
(NaN), which is the correct "no data" answer and is what `init` is for (`src/IR.fs:157`:
init "defines the empty result").

Falls short in: (i) **two** `exp` per pair (both sides rescale), where a tile-level step
would pay one per element plus one per tile — the standard FlashAttention amortization;
(ii) the `merge` is symmetric and associative in exact arithmetic but nothing checks it —
`where comm(a, b)` is *accepted* on the tuple kernel (p9) but whether the parity deduction
ran on a tuple body or fell to the `PBottom` trust case (`src/TypeCheckInfer.fs:7679`) is
**unverified**; (iii) the ascription is a workaround for D1; (iv) the fold is
sequential per row — a parallel merge across chunks of `j` would need the licence of §1.7
on a tuple-typed kernel, which `chunkable` (`src/CodeGenBinding.fs:2888`) has not been
exercised with.

### 2.3 Scientific analogue: kernel-weighted mean (Nadaraya–Watson) — one pass, no max

For a bounded kernel (`exp(-(x_i − x_j)²/2h²) ≤ 1`) no stabilization is needed and the
one-pass join is the whole program:

```blade
let smoothed = (method_for(range<I>) <@> lambda(i) -> {
    let xi = x(i)
    let w = method_for(xs) <@> lambda(b) -> exp(-(xi - b) * (xi - b) / (2.0 * h * h))
    let z, u = object_for(<&!>) <@> (reduce(w, (+)), prodsum(w, y))
    u / z
}) |> compute
```

(p2b; identical to p2 to 15 digits on this data because the row max cancels analytically.)
`examples/lswosa.blade:120-133` is the in-repo precedent: six sums per (cell, frequency)
in one traversal of the cell's samples, with the trig producers deferred by naming.

### 2.4 Matrix-valued output (`v: J × D`, `y: M × D`)

The row kernel may return a computed rank-1 row (p5 — accepted, not only array literals):

```blade
let Y = (method_for(range<I>) <@> lambda(i) -> {
    let qi = q(i)
    let m = reduce(method_for(k) <@> lambda(b) -> score(qi, b), lambda(a, b) -> if a > b then a else b, -1.0e300)
    let e = (method_for(k) <@> lambda(b) -> exp(score(qi, b) - m)) |> compute   // ONE J-vector per row
    let z = reduce(e, (+))
    (method_for(Vt) <@> lambda(vrow: T^1) -> prodsum(e, vrow) / z) |> compute   // Vt: D × J
}) |> compute
```

Per row this allocates one J-vector (`__v40`, freed at row end) and one D-row (copied into
`Y`'s row, freed) — bounded by O(J + D), never M×J. The D-vector *accumulator* form (one
exp per pair, D multiply-adds into a running row) is not reachable without a rank-1 fold
state, and the only rank-1-state fold today is the leading-axis fold, which allocates per
step (p6, `src/TypeCheckInfer.fs:2520-2600`; plan-deferred-combinators D4).

### 2.5 Where the spellings fall short — the honest list

1. Arithmetic: 2× `score` (two-pass) or 2× `exp` (one-pass) per pair versus a tiled step.
2. Defect D2: the join's direct-fold leg does not consume the share it declares.
3. Defect D1: tuple projections in a row kernel leave the output type unpinned → `Array<I,1>`.
4. The natural module-level spelling `reduce(method_for(A, B) <@> k, (+))` materializes
   (§1.3); the user must know to write the per-row kernel.
5. No `-inf`/`+inf` literal; seeds are magic constants.
6. A D-vector running state needs a fold state of rank 1 without per-step allocation.
7. Symmetric pairwise kernels: writing the per-row kernel over `(x, x)` forfeits the `r!`
   saving `where comm` grants the outer product, and the compact-storage partial fold is
   refused (`compactGate`, `src/TypeCheckInfer.fs:2352-2360`). Neither route gives
   "per-particle pair sums in N²/2 work without an N×N pool".

Items 1 and 7 are the only ones that need compiler *machinery*; 2–5 are fixes/idioms; 6 is
plan-deferred-combinators D4.

---

## 3. Design

### 3.1 What a declared summary would be (recorded, not proposed for now)

A summary over element type `E` with state `S` and aggregate `R`:

| part | signature | today's spelling | needed for |
|---|---|---|---|
| `embed` | `E → S` | the map kernel returning a tuple (`(score, 1.0, x)`) | — |
| `merge` | `S × S → S`, associative; commutative for regrouping | the fold kernel on tuples | tiles / threads |
| `step` | `S × E → S` (by definition `merge(s, embed(e))`; declared separately only for cost) | absent — the fold applies `merge` per element | one `exp` per element |
| `finalize` | `S → R` | the row-kernel tail (`s[2] / s[1]`) | — |
| empty state | `S` | the 3-arg `init` | empty rows |
| licence | `merge` reorderable | `where comm(a, b)` on the merge → `foldReorderLicensed` | any non-serial schedule |

A **checked relationship** to the requested aggregate — `finalize(foldₘ(embed ⃗x)) =
aggregate(⃗x)` on an admitted fragment — cannot be established by the compiler for a
user-written merge: it is a per-summary theorem (Coq in `proofs/` for a fixed catalogue —
online-softmax, Welford/Pébay moments as `mstate` already realizes — plus differential-twin
and integer-valued corpus pins for the rest). In a first version "checked" would honestly
mean "catalogued and pinned", exactly `ppl.mstate`'s standing.

Tile placement: the state lives per **output cell** (per row `i`); the tile is a contiguous
span of the reduced axis `j`, derived from static extents and row-contiguous storage. The
existing `FoldChunk` plan (`src/CodeGenBinding.fs:2888-2896`) is already "per-chunk partial
+ fixed-order combine" for licensed folds; a tiled summary is that plan with `step` inside
the chunk and `merge` as the combine — and `RowBinPlan` (plan-compact-sym-folds §5.6) is its
triangular sibling. Declaration surface, if ever built: an elaborated former in the style of
`PplElaborate.fs:84`'s `formerNames`, e.g. `summary softmax_state = { embed …; step …;
merge … where comm(a, b); finalize … }`, lowering to exactly the §2.2 fold with `step`
substituted for `merge ∘ embed` inside tiles. No new IR: the §2.2 shape already lowers.

### 3.2 Why the machinery is not warranted now

On the C++ lane the memory result is already O(M) by spelling (§1.8). What a compiler tile
buys over §2.1/§2.2 is arithmetic only: one `score` pass (≤ 2×) or one `exp` per pair (≤ 2×
of the transcendental budget), workload-dependent, and against a baseline that already beats
the materialized form. FlashAttention's payoff
([arXiv:2205.14135](https://arxiv.org/abs/2205.14135)) is HBM traffic on a GPU; Flashlight
([arXiv:2511.02043](https://arxiv.org/abs/2511.02043)) compiles attention *variants* through
PyTorch's compiler — "compile FlashAttention" is not an open claim. Blade's specific claim
would have to be summaries over **named, ragged, symmetry-bearing** domains; §3.4 shows the
ragged and named cases already compose by spelling, and the symmetric case is owned by
another plan. Building a registry + tiler now would add a fourth reduce route (§1.1) and its
three-consumer tour (C++ / interpreter / LLVM, plan-deferred-combinators §2.5) to save at
most 2× on an already-linear-memory kernel. Revisit under the conditions in §6.

### 3.3 What is warranted: three contained pieces

**A. The idiom package (no compiler change).** Make §2.1–§2.4 the documented answer.

- `examples/10_pairwise_normalized_sums.blade`: kernel-weighted mean (one pass), softmax
  row (two-pass join and one-pass tuple state), matrix-valued output; `// EXPECT:` pins at
  small non-power-of-two extents; README entry.
- Corpus, `tests/corpus/loops/` (next free ids 200+): `200_per_row_reduction_join_streams`
  (pins `y`; a `WARN`-clean run), `201_tuple_state_online_fold` (pins agreement with the
  two-pass form to a printed precision, integer-valued data per the licensed-path policy).
- `CLAUDE.md` style table row: *normalized / weighted pairwise sums* → "per-row kernel with a
  reduction join over the deferred producer, or a tuple-state fold", not "an M×N array then
  two partial folds". `docs/quickstart-2.md` cross-reference.
- Optional stdlib: `stats.blade` already houses the row-kernel statistics; a
  `softmax_merge(a: (Float64, Float64, Float64), b: (Float64, Float64, Float64))` is
  declarable (tuple-typed params: `tests/corpus/tuples/008`, `015`), but a named fold
  kernel eta-expands into `lambda(a, b) -> f(a, b)` and D1 must be fixed first.

**B. Defect D1 — tuple projection leaves the kernel output unpinned.**
`inferTupleIndex`'s pack arm answers `env.Subst.Fresh()` (`src/TypeCheckInfer.fs:4171-4173`,
"result type is fresh — codegen resolves via std::get"). Inside a row kernel, `s[2] / s[1]`
therefore has an unresolved type at the apply's output unification, and the emitted row map
is `Array<I, 1> y` (p8: the loop-index tag became the element type; g++
`-Werror=float-conversion` refuses at `y[__i0] = __lambda_45(...)`). The seed's type
`(Float64, Float64, Float64)` is known at the reduce site, so the projection can be typed
from the resolved `IRTTuple` when available; the second half — *where* an unresolved kernel
output defaults to the index tag — was not located and needs a trace (search the apply's
`OutputType` resolution for a `range<…>` loop). Pin with a positive corpus test (no
ascription needed) and keep p8b's ascribed form compiling. Effort S–M.

**C. Defect D2 — join share not consumed by direct-fold legs.** Cause in §1.2. The fix
needs an identity between a leg's spliced leaf and the shared binding, which the typecheck
splice erased (`:1764-1783`). Cheapest: in `inferReductionJoin` (`:2101`), when a leg is
`reduce(<name>, op[, init])` and `<name>` is a join-deferrable deferred apply, keep the leaf
as the *variable* (the join emitter already resolves variables through
`ctx.DeferredComputations`, `src/CodeGenBinding.fs:3260-3272`) and let `repoint` treat a
leaf that *is* a shared id like a repointed slot: its kernel becomes `lambda(x) -> x` over
the share variable. Interpreter unaffected (per-iteration CSE of a pure map,
`src/Interp/Loops.fs:1889-1893`); LLVM lane consumes producers natively. Gate: p2's row
kernel emits one `std::exp` per iteration; `test interp loops` and the join corpus stay
green. Effort M (touches typecheck + emitter; ids may shift → golden re-pin).

**D. The partial-fold rewrite (the one compiler change with a verified payoff).**
Target: `reduce(<unforced outer-product apply>, op[, init])` with `axes = 1` (default) —
§1.3's `z`. Today `partialFold` binds the deferred operand to `__pfsrc` (`:2406-2407`).
Instead, when the operand is an anonymous unforced `TExprApply info` with
`info.Loop = method_for(A₁, …, Aᵣ)`, `r ≥ 2`, `not info.IsCoIteration`, a rank-0 kernel
`k` of `r` params, plain dense index kinds (the `unsupportedFusedInput` predicate,
`:1900`), and a fold kernel that is a `(+)`/`(*)` section or a 3-arg reduce (so the inner
fused terminal has its seed, `:2761`) and carries no `omp`, rewrite to

```
(method_for(A₁) <@> lambda(a₁) ->
     reduce(method_for(A₂, …, Aᵣ) <@> lambda(p₂, …, pᵣ) -> k(a₁, p₂, …, pᵣ), op[, init])) |> compute
```

which is exactly the §2.1 shape and rides routes that exist: outer apply + inner fused
terminal (`:1760`) with the kernel captured by value. No IR, emitter, or interpreter change.
Result type is unchanged (indexed by `A₁`'s index type). Fold order per row is row-major
left fold from the identity/init; the materialized route seeds with the first element, so
bits agree except `0.0 + (−0.0)` sign-of-zero for `(+)`. Named operands (`let S = …;
reduce(S, (+))`) stay on today's route (they may be printed/read elsewhere — the
sole-consumer analysis is plan-deferred-combinators D0). `range<I, J>` loops: v2, needs the
range's tag list split at the surface. Effort M; risk = id-shift golden churn (that plan's
§3.7). Gate in §4.

### 3.4 Interactions

- **Masks / WHERE**: `mask`/`compound` are eager with runtime extents (deferred plan §3.4);
  a filtered `j`-axis enters the row kernel as a materialized rank-1 array and the join over
  it is unchanged. Empty selections finalize through the seed (`init` defines the empty
  result). Per-row masks (`mask(k, pred_i)`) allocate O(J) per row — bounded.
- **Ragged groups**: the fused terminal refuses ragged/grouped inputs (§1.4), but a group's
  row view is a plain rank-1 array inside the row kernel — `examples/lswosa.blade:126-133`
  runs a six-leg join per `group_by` cell today. Per-group streaming works by spelling.
- **Arity-dependent outputs**: §2.4; D-vector state blocked on the leading-axis fold's
  per-step materialization (D4 of the deferred plan), not on anything here.
- **`where comm` symmetry**: §2.5 item 7 — a genuine gap, but the compiler-owned answer is
  a canonical-domain fold that scatters each visited pair into two row states (`acc[i]`,
  `acc[j]`), which is plan-compact-sym-folds §5.6's `RowBinPlan` with a per-row accumulator
  vector; parallelism conflicts on the scatter make it a licensed-only schedule. Leave it
  there; do not build a separate mechanism.
- **AD**: a tuple-state fold and a join are both differentiable shapes only insofar as
  `Grad*` handles `IRReduceCompute` with tuple kernels — **unverified**; out of scope.

### 3.5 Floating-point contract

Regrouping a merge across tiles or threads is not bitwise associative. The rule is the
existing one and needs no new kind: a non-serial schedule requires `foldReorderLicensed`
(§1.7) on the merge — declared `comm(a, b)` on the tuple kernel (accepted syntactically,
p9; deduction coverage on tuple bodies to be verified when the schedule is built), vetoed by
`where repro`, refused for `omp` without licence (BL4016). The unlicensed path is the serial
per-element left fold of §2.2, which is what the interpreter performs. A summary's
`finalize` is outside the licence (it runs once per state). `BLADE_FP_REASSOC` must not
lane a tuple merge unless `fpReassocSimdOp` (`src/CodeGenExprSupport.fs:1373`) can say what
the op is — it cannot for a tuple kernel, so today's answer (serial) is the right one.

### 3.6 Interpreter and LLVM twins

- Interpreter: `forceReduceCompute` (`src/Interp/Loops.fs:1855`) resolves each leg's fold via
  `resolveBinaryFold` (`:225-234`, any binary callable, values are generic so tuple states
  are `VTuple`) and folds in nest order with `OutFold` (`:1781`). The tuple-state fold under
  the interpreter was **not exercised** here (no single-file interpreter verb; `blade test
  interp` was out of bounds for this session) — the first corpus test of §3.3 A must run
  under `blade test interp loops`.
- LLVM lane: `emitReduceCompute` (`src/EmitLlvm.fs:3799`) takes `scalarTyOf cl.RetType` for
  the accumulator and falls back to the leaf element (`:3826-3829`); a tuple-typed merge
  likely **refuses by name** there (inferred, not run). Acceptable under the lane's
  all-or-absent contract; note it in the corpus test's header.
- Any tiled schedule (§3.1) would need an arm in all three lanes — the cost that §3.2 says is
  not yet paid for.

---

## 4. First gate

### 4.1 Executed here (the memo's gate, on the existing spellings)

Pairwise kernel + normalization at growing M, N (1531×1723, 3061×3449): bounded intermediate
storage **demonstrated** — 5 MB peak working set for all three streaming spellings versus
166 MB for the materialized one at the larger size (§1.8; measured by polling
`PeakWorkingSet64` at 1 ms, three interleaved rounds). End-to-end time: two-pass join 1.6×
faster than materialized; OMP-over-rows 8–11× on 16 logical cores; one-pass online 1.3×
slower than two-pass. Numerical error: checksums identical to 15 digits across all variants
at both sizes; per-cell max |Δ| = 1.0e-15 between one-pass and two-pass at 37×53. A
hand-tiled C++ reference was **not** written; against the current spellings the memory
question is settled without it, and the arithmetic ceiling (§3.2) is a derivation.

### 4.2 Gate for piece D (the partial-fold rewrite)

1. `blade emit` of §1.3's `z` shows **no rank-2 pool** and one fused inner loop per row;
   `tot` (`axes = 2`, unannotated) is out of scope for D and may still materialize.
2. Values bitwise equal to today's route on integer-valued data (fold order argument in
   §3.3 D); on float data equal up to the sign-of-zero corner, pinned by a corpus test at
   non-power-of-two extents.
3. `blade test` full suite, `blade test interp loops`, `blade test diff-oracle loops` green
   at the TOTAL line; WARN pins and `--strict-pins` swept (ids shift → budget a golden
   re-pin, plan-deferred-combinators §3.7).
4. Timing: the rewritten `z` within noise of the hand-written §2.1 shape at 3061×3449 (they
   should be the same C++).

STOP if: the rewrite needs a new IR node or emitter arm (it must ride the existing outer
apply + fused terminal); or the inner fused fold's order differs from the row-mode fold
beyond the −0.0 corner (bit pins would move); or it changes print presence of any binding.

### 4.3 Gate for reviving the summary machinery (§3.1) — conditions, not a plan

Revisit only if a real workload shows the two-pass per-row join ≥ 1.5× slower than a
hand-tiled C++ reference at equal memory on the CPU lane, or when a GPU lane makes memory
traffic the cost, or when the symmetric per-particle pair sum (§2.5 item 7) is wanted and
plan-compact-sym-folds §5.6 has landed its `RowBinPlan`. Until then the memo's own
preference applies: library construction over compiler machinery.

---

## 5. Size, risk, files

| piece | effort | risk | files (ordered) |
|---|---|---|---|
| A idiom package | 1 day | low (docs + pinned examples; interpreter twin of the tuple fold is the one unknown) | `examples/10_pairwise_normalized_sums.blade`, `examples/README.md`, `tests/corpus/loops/200_*.blade`, `tests/corpus/loops/201_*.blade`, `CLAUDE.md` (style table), `docs/quickstart-2.md`, `docs/plans/README.md` (index this doc) |
| B defect D1 | S–M | medium: second half of the root cause not yet located | `src/TypeCheckInfer.fs:4171-4173` (`inferTupleIndex` pack arm); the apply output-type resolution for `range<…>` loops (to be found); new corpus test in `tests/corpus/tuples/` |
| C defect D2 | M | medium: identity plumbing across the typecheck splice; golden churn | `src/TypeCheckInfer.fs:2101` (`inferReductionJoin`), `:1764-1783` (`collect`); `src/CodeGenBinding.fs:3302-3325` (`deferredOperand`/`sharedIds`), `:3348-3362` (`repoint`), `:3380-3400` (leg leaves); join corpus (`tests/corpus/loops/1xx` join tests) |
| D partial-fold rewrite | M (2–4 days) | high on golden churn, low on semantics | `src/TypeCheckInfer.fs:2330-2470` (`partialFold`, new branch before the `srcIsNamed` split at `:2406`); corpus `tests/corpus/loops/202_deferred_outer_product_partial_fold.blade`; `docs/formalism.md` §10.2 note; plan-deferred-combinators §5 (add as D0-adjacent phase) |
| summary machinery | L | high | deferred — see §4.3 |

Shared-checkout note: the corpus deployed-copy trap applies to new `.blade` files (check the
test COUNT went up), and `generated_cpp_tests/` is cwd-relative.

## 6. Recommendation

**DEFER** the compiler feature ("registered/declared summaries with compiler-derived
tiling"): the memory behaviour it targets is already available by idiomatic composition, and
its remaining payoff on this lane is a bounded arithmetic factor. **GO** on the three
contained items: publish the per-row reduction-join / tuple-state idiom as the documented
answer (A), fix the tuple-projection typing hole (B) and the join sharing gap (C), and — as
the one compiler change this investigation justifies — schedule the partial-fold rewrite (D)
as a phase of plan-deferred-combinators with the gate of §4.2. Re-open the summary design
only under §4.3's conditions.
