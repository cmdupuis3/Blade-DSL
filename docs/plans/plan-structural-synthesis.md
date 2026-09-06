# Structural performance opportunities: synthesis of the six elaborations

Status: SYNTHESIS, 2026-09-06; items 1-3 landed the same day (section 2). Six Fable agents each elaborated one opportunity from
[plan-structural-performance-opportunities.md](plan-structural-performance-opportunities.md)
into an implementation-grade design under [structural/](structural/) (4,220 lines in all;
every claim there is cited to current source, and every agent ran its own emission or
run probes from a private directory). This document ranks what they found, records the
verdicts, lists the decisions that are the user's, and states the implementation order
that followed. It is deliberately short: the designs are in the six files, and a reader
who wants the mechanism should open the one named in each row.

## 1. Verdicts

| # | Opportunity | Verdict | Size | Needs a user decision? | Document |
|---|---|---|---|---|---|
| 1 | Linear reverse AD for additive recurrences | **GO** | 1-2 days, 3 files, no new IR node, no interpreter work | No | [01-linear-reverse-ad.md](structural/01-linear-reverse-ad.md) |
| 2 | Shared halo access description (demand + backward gather) | **GO** for the first gate; compact/ownership/revision deferred | ~2.5 weeks (P1a 2-3 d, P1b 4-6 d, P1c 3-5 d) | One diagnostic code (BL4019); no syntax | [02-access-descriptions.md](structural/02-access-descriptions.md) |
| 3 | Streaming across reduction boundaries | **DEFER** the compiler feature; **GO** on an idiom package + two defect fixes + one typecheck rewrite | S-M each | No | [03-streaming-reductions.md](structural/03-streaming-reductions.md) |
| 4 | Reuse across immutable dataset revisions | **GO, narrowly** (elementwise first; halo as the second consumer of #2) | ~1,200-1,600 lines | No syntax; an opt-in on-disk store | [04-revision-reuse.md](structural/04-revision-reuse.md) |
| 5 | Structured operators (Gram application without a Gram) | **GO** for one applied-action node; operator values / factorizations deferred to a co-design with fortran-killer-2 6.2 | 3.5-6 days, ~30 files | **Yes: seven** (spelling, home, sugar, complex convention, value-form verb, loop order, advisory) | [05-structured-operators.md](structural/05-structured-operators.md) |
| 6 | Enumerable constrained domains | **GO, narrowly** (rank 2, dense, classes A1/A2/A3) | M-L, 17 files | **Yes: seven** (index spelling, field types / tag flow, param binding, fallback policy, diagnostic code, triangle admission, absent-cell reads) | [06-enumerable-domains.md](structural/06-enumerable-domains.md) |

Three findings changed the memo's own picture:

- **#3's premise is already met by composition.** A per-row kernel whose body is a
  reduction join over a *deferred* producer computes the softmax-normalized weighted sum
  with no M x N pool: measured 5 MB vs 166 MB peak and 1.6x faster serially at 3061 x 3449.
  The compiler feature would buy a bounded arithmetic factor on this lane. What the probe
  did find were two defects (a tuple projection leaves a row kernel's output unpinned and
  g++ rejects the map; a join's `reduce` leg re-spells its shared producer instead of
  reading the share) and one route that materializes when it need not (the default partial
  fold over an anonymous outer-product apply).
- **#5's rewrite is not a rewrite.** `(A A^T) v` and `A (A^T v)` differ bitwise (~1e-14
  relative at N = 601), so the factored form is a *different declared computation*, which
  settles the memo's open semantics question in favour of an explicit node and against any
  licence-gated rewrite. Measured 18-20x faster and O(N D) rather than O(N^2) storage.
- **#2 found two soundness holes in passing.** A window read outside the declared halo set
  (`a(w(2))` on `halo<H, [-1, 0, 1]>`) passes `check` and reads one past the pool in the
  compiled lane while the interpreter would panic -- a latent differential red no corpus
  test exercises; and the halo carousel's tail prefetch over-reads by one cell on every
  valid stencil's last iteration. Both close when consumers read one access description
  instead of re-deriving it (nine readers today).

## 2. Implementation order and what landed

Ordered by (confidence x value) / size, with items needing a user decision held until the
decision is taken. The first three are being implemented on `feat/fortran-killer-2`
alongside the second Fortran-killer audit's P0-P2 work; the status column is kept current.

| Order | Item | Status |
|---|---|---|
| 1 | #1 linear reverse AD: one direct-loop lowering for both modes, the `CarryLoop` recognizer, the discipline exemption, the descending `NFor` adjoint; corpus at 127/257/509 with analytic pins; an emission pin counting loops | **landed** (`tests/corpus/ad/027-029`, `OptimizeTests.recarray_grad_linear_emission`; 008's pins unchanged; both lanes) |
| 2 | #3 pieces A-C: the idiom package (`examples/10`, `tests/corpus/loops/200-201`, a CLAUDE.md row), defect D1 (a literal tuple projection takes the element's type), defect D2 (join share consumed by direct-fold legs) | A and B **landed**; C (D2) open -- it needs the typecheck splice and the join emitter to agree on a shared-leaf identity, and the id-shift churn it risks is better taken with piece D |
| 3 | #2 P1a: literal window offsets outside the declared REACH refused (BL4019), the carousel tail guarded; the shared `HaloAccess` record and the one IR window-read matcher as their single source | **landed** (`tests/corpus/loops/202`, `OptimizeTests.halo_carousel_tail_guarded`; `Blade.Types.HaloAccess`, `src/IRAccess.fs`; the checker, both BL8009 guards, the carousel and the AD extent read the record) |
| 4 | #3 piece D: the partial-fold rewrite over an anonymous outer-product apply (a phase of plan-deferred-combinators, its own gate) | queued |
| 5 | #2 P1b: the backward gather for halo cotangents (routes S and G, bitwise gate) | **landed** on `feat/halo-cotangents` (`tests/corpus/ad/030-033`, `ad-jvp-comb/108`, `blade test access`: six programs byte-identical under both routes, ASan-clean); the gate found and fixed the tangent lane's one-sided-lag-set extent bug |
| 6 | #4 revision reuse, elementwise first, with the scale run as a real stop condition | queued (after #2 P1b, whose record it consumes for halos) |
| 7 | #5 `gram_apply` node + matrix-free adjoint | **waiting on decisions D1-D7** |
| 8 | #6 enumerable domains, rank 2 dense | **waiting on decisions 1-7**; the agent also advises sequencing it after items 1-3 and behind the compact-fold decision |

## 3. Decisions for the user

Collected verbatim from the two elaborations that flagged them; each is a surface or
policy choice this synthesis does not take.

**Structured operators** (05, section 3.3):

- D1 spelling of the applied action: `gram_apply(A, B, x)` (recommended for step 1) vs
  `gram(A, B) <@> x` (reserved for the value form) vs `gram(A, B)(x)` (rejected: application
  on arrays is indexing).
- D2 home: core keyword beside `gram` (recommended) vs the `math` package (`m.gram_apply`).
- D3 one-operand sugar `gram_apply(A, x)`: consistency with the earlier decision against
  unary `gram(V)` suggests none.
- D4 complex convention: conjugate (`B^H x`, matching `gram`; recommended) vs unconjugated.
- D5 value-form spelling and materialization verb (step 2; listed so D1 does not foreclose
  it).
- D6 native loop order for the `B^H x` half (bit-identical either way; a machine-shaped
  choice to pin once in both twins).
- D7 an advisory (never a rewrite) when the checker sees the materialized-then-applied
  shape; off until the fastest-way advisory channel exists.

**Enumerable domains** (06, section 3.7):

1. Spelling: the `static struct` name standing where an index type stands (`range<R>`,
   `Array<T like R>`; assumed) vs a dedicated closed-form family (`BandIdx<I, w>`).
2. Field types and tag flow: allow `i: I` for a declared `type I = Idx<N>` (closes the
   BL4003 warning spray) vs keep `Int` fields and untagged params.
3. Kernel parameter binding: positional in declaration order (assumed) vs field-named.
4. Class B fallback policy: silent vs advisory warning (assumed) vs refusal.
5. Diagnostic code: the fence's generic BL3999 vs a minted code (five touch points).
6. Admit the triangle class (A5) in v1, documented as a constant-factor shrink.
7. Absent-cell reads: panic (compound's contract) vs zero-fallback via `<|:>`.

## 4. What the synthesis declines to claim

No speedup figure in this document is a promise for a workload; the two measured numbers
(#3's 5 MB vs 166 MB and 1.6x; #5's 18-20x) are single-machine probes at one size each,
recorded in the elaborations with their conditions. The claim the memo wanted tested --
that one access description can drive canonical storage, reverse accumulation and data
dependencies together -- is narrowed by #2 to "one record serves the extent guards, the
carousel and the backward gather, provably bitwise-identical to the scatter", with compact
storage, ownership and revision propagation kept as interfaces until that gate passes.
