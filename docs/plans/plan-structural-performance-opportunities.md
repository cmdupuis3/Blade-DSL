# Structural performance opportunities: planning audit

Status: RESEARCHED 2026-09-05; no implementation proposed here is built by this audit.
Baseline: checkout at `7d1053e`, plus the existing working-tree planning documents.

Companion: [the second Fortran audit](plan-fortran-killer-2.md) appeared in the shared workspace during this review. Its shared legality facts, consumer-directed storage, and reusable numerical operators overlap this memo. Treat those as common foundations, not separate competing projects. This memo develops the access/adjoint connection, reduction summaries, cross-snapshot reuse, and the concrete reverse-AD complexity finding.

Blade's strongest next opportunity is to carry structural facts through the **consumer** of a computation: which cells it needs, how it aggregates them, which inputs its derivative touches, and which results survive a dataset revision. The reviewed implementation is much further along at deriving output shape and storage than at exploiting this information across whole pipelines.

This is a shortlist, not six simultaneous projects. Recommended first work: eliminate the verified quadratic reverse-AD path and prototype one shared access description for a fixed halo. Pursue streaming across reduction boundaries next if a comparison with the best existing Blade spelling shows a real gap.

“New” below means absent as a developed proposal in the plans reviewed. It is not a claim of worldwide priority. The prior-art check establishes close competitors; original contributions would require a narrower claim and a fuller literature review. Complexity estimates are derivations for the stated examples, not measured speedups.

| Opportunity | Relationship to existing work | Expected benefit | First decision gate |
|---|---|---|---|
| Preserve recurrences through reverse AD | Known general ambition; a concrete asymptotic defect deserves its own first milestone | O(n²) to O(n) work for additive carries | Generated loops, derivative correctness, scaling |
| Share access descriptions across demand, adjoints, and ownership | Extends the companion audit's dependency/storage foundation | Less I/O and temporary storage; parallel backward gathers where preimages are enumerable | One halo description serves two consumers |
| Stream across reduction and normalization boundaries | New generalization of existing fusion and moment states | Remove quadratic intermediate storage in pairwise pipelines | Beat an explicit reduction-join implementation |
| Reuse computation across immutable dataset revisions | New extension of Icechunk provenance | Recompute affected tiles rather than entire fields | One changed chunk; cold/warm results identical |
| Keep structured operators executable without forming their tensors | Separability is already proposed in the cosmology plan; broaden the consumer contract | Asymptotic work and storage reductions | Gram-operator application without a Gram allocation |
| Turn support predicates into efficient enumerators | C2 already proposed; strengthen its algorithmic requirement | O(nw) band traversal rather than O(n²) filtering | Generated bounds skip excluded space |

**1. Give recurrence AD a linear first milestone.**

This is the highest-confidence immediate finding. `src/GradExpand.fs:1145` gives JVP a direct recurrence loop, with comments explicitly contrasting its O(n) work with reverse mode's O(n²). The reverse branch at `src/GradExpand.fs:1188` instead reconstructs every additive prefix independently with triangular loops. `tests/corpus/ad/008_recarray_grad.blade` documents and exercises that route.

The installed Release binary was used to emit that corpus program into a private temporary directory. Its primal function has one recurrence loop; `h__grad` has nested triangular loops in both its reconstructed primal work and its backward work. Thus this is confirmed in generated C++, not inferred solely from an old plan. No runtime speedup was measured.

For `s[t] = s[t-1] + f(x[t])` and a loss using the trajectory, the mathematical reverse rule is a backward carry: combine the direct cotangent of `s[t]` with the carry from later steps, then apply the local derivative of `f`. It requires linear work. A final-element-only loss is an even simpler first case. Keep the recurrence visible until this rule has been selected rather than destroying it into independent accumulations before differentiation.

Then extend the same representation to bounded-lag recurrences and choose saved states from what the backward computation actually needs. This is a more useful progression than treating all reverse-through-recurrence support as one moonshot. Ordinary nonlinear trajectories may still need full storage or checkpoint recomputation. Rolling storage by itself is already known work, not a new proposal.

Two boundaries matter. Changing gradient accumulation order can change floating-point bits, so mathematical equivalence is not sufficient to promise compatibility with all existing pins. Also, a successful `while` guard does not by itself authorize an implicit derivative: differentiating the finite executed algorithm and differentiating an exact fixed-point equation are different contracts. The latter additionally needs a suitable residual equation, differentiability, and a nonsingular linearization.

This is performance catch-up, not an original AD algorithm: [Futhark has published specialized reverse rules for scans and reduce-by-index](https://arxiv.org/abs/2310.03568). Blade's distinctive extension would be deriving those rules from its structural recurrence surface, including zero-history semantics, compact slices, and downstream storage demand.

First gate: final-only and whole-trajectory losses at non-power-of-two lengths such as 127, 257, and 509; inspect loop complexity; check against analytic derivatives and finite differences; validate primal and numerical contracts separately. Do not promise O(n) reverse AD for arbitrary user recursion.

**2. Describe an access once, then use it forwards and backwards.**

For an output coordinate `o`, represent the input coordinates its kernel reads. Initially this can be a small closed family of descriptions: identity, axis permutation, fixed halo offsets, and an explicit gather map. This is compiler information derived from existing constructs; it need not add user-written schedules or a general predicate solver.

That one description has several useful interpretations:

- Forward demand: the input region required for a requested output tile.
- Reverse demand: the outputs contributing to a particular input cotangent.
- Ownership: whether the required input region is local and which boundary values must move.
- Revision propagation: the output tiles affected by changed input chunks.

Blade already has the ingredients, but in separate forms: virtual/index bindings in `src/IRStorage.fs`, halo handling in `src/Lowering.fs`, per-form cotangent reindexing in `src/GradSweeps.fs:273`, provider windows, and the distribution rules in `plan-distributed-memory.md`. The reviewed paths do not expose one shared compositional access representation serving these consumers.

For a stencil, transposing the relation gives a backward gather over the opposite offsets. Each input cotangent can have one owning iteration, avoiding conflicting scatter updates. For an arbitrary sparse gather, build an inverse CSR map once in O(E) time, where E is the number of gather entries, and reuse it when the topology is reused. Include duplicate entries, boundary semantics, and contributor order. An unknown preimage must decline this schedule; it must never be treated as empty.

The especially interesting Blade extension is **canonical storage**. An access description over a symmetric or antisymmetric pool must also carry how the logical value is reconstructed: permutation, sign, conjugation, and zero sets. Its reverse action then determines the correct cotangent contribution. A symmetric off-diagonal pool cell used twice in a logical dense computation receives both contributions; it is not automatically averaged or multiplied by an unconditional factorial. Orbit sizes change on diagonals, and conjugation needs an explicit real/complex differentiation convention.

This could make one index contract derive fast forward execution, race-free backward execution, and local communication. That combination is a credible research hypothesis. The pieces have substantial precedent: [Dex preserves AD parallelism with typed accumulation effects](https://arxiv.org/abs/2104.05372), and [SySTeC generates symmetry-aware sparse tensor code](https://arxiv.org/abs/2406.09266). A claim should therefore name the compositional connection and its demonstrated coverage, not claim invention of parallel AD or symmetric compilation.

Demand elimination must also preserve observable behavior: purity alone does not prove that skipped work cannot abort. Respect explicit materializations and restrict omitted evaluations to a fragment whose totality and effects are understood, consistent with `Optimize.fs`'s cost-only charter.

First gate: one fixed-halo description drives both input-window selection and a backward gather. Compare against direct evaluation and a scatter reference, including boundaries. Only then add compact signed accesses. Stop if this requires unrelated duplicate analyses at both consumers; that would undermine the proposed shared abstraction.

**3. Make whole reductions compose into bounded-memory execution.**

Elementwise fusion removes temporary maps. A larger opportunity is crossing a reduction whose result is used to normalize, weight, or contract the producer. An example is

`y[i] = sum_j exp(score(i,j)) * v[j] / sum_j exp(score(i,j))`.

The logical score array is M by N, but the output has only M by D cells. A tiled evaluation can keep a bounded score tile and a small state per active output row rather than materializing the M by N intermediate. The arithmetic can remain quadratic in sequence lengths: the claim here is reduced storage and memory traffic, not fewer pair interactions.

For a stable exponential normalization, the state can contain a maximum `m`, a rescaled denominator `z`, and a rescaled weighted numerator `u`. Merging states A and B uses `m = max(mA,mB)` and rescales the two `z` and `u` values by `exp(mA-m)` and `exp(mB-m)`. Empty states and infinities require explicit cases. These are familiar equations; the compiler opportunity is making such states compose with user kernels, masks, ragged groups, and arity-dependent outputs.

There are important existing Blade precedents. Reduction joins already share work in one traversal, and `ppl.mstate`/`mstate_merge` already expose domain-specific mergeable statistics (`src/ppl/compiler/PplElaborate.fs`). The current optimizer entry at `src/Optimize.fs:162` runs constant-match folding and elementwise-chain fusion; extending its scope does not mean inventing another map-fusion pass. Start by comparing with a manually composed, idiomatic reduction join. If that already gives the required memory behavior, prefer a library construction or a recommendation over compiler machinery.

A bounded design would allow a registered or declared summary with initialization, step/merge, and finalization functions, together with a checked relationship to the requested aggregate on an admitted fragment. The compiler derives tile placement from extents and storage; users declare the computation's structure. Do not silently assume every fold has a compact summary, or that regrouping floating-point state is bitwise associative.

[FlashAttention established the I/O payoff of tiled attention](https://arxiv.org/abs/2205.14135). More directly, [Flashlight now generates fused attention variants through PyTorch compiler extensions](https://arxiv.org/abs/2511.02043). Consequently “automatically compile FlashAttention” is not an original research claim. The Blade-specific target would be verified summaries that work over named, ragged, and symmetry-bearing domains and compose with scientific statistics as well as attention.

First gate: a pairwise scientific kernel plus a normalization/weighted reduction, with growing M and N. Demonstrate bounded intermediate storage, compare with the strongest existing Blade spelling and a hand-tiled reference, and measure end-to-end time and numerical error. Avoid committing to a GPU backend project before this semantic and memory result exists.

**4. Make versioned scientific computation reuse unchanged work.**

Icechunk already gives Blade a strong starting point: immutable checkout resolution and content-sensitive axis provenance. `src/providers/IcechunkProvider.fs:1561` constructs chunk tables; its fingerprint machinery around `:1821` distinguishes content identities. The existing `SessionMemo` in `src/Interp/Run.fs:33` caches values under a session-prefix rule. It is not a dependency cache that updates individual output tiles when source chunks change.

For a pure local pipeline, cache output tiles by the kernel and captured parameters, input chunk identities, index identities, and relevant numerical/backend settings. A new snapshot can reuse a tile only when all those dependencies match. Shared coordinate identity alone is insufficient: two arrays can have the same axes and different values.

For an elementwise kernel, one changed input chunk changes the corresponding output chunk. For a radius-h halo, it also invalidates the neighbouring output tiles whose read regions intersect that chunk. The access descriptions in item 2 give this propagation directly. If only a small fraction of a large field changes, avoided computation and data reads can dwarf inner-loop optimization. Metadata scans and cache validation still cost time, so the changed fraction is not itself a promised wall-clock speedup.

Start with reuse of complete, unchanged pure tiles. Updating global statistics by subtracting old contributions is a later feature: it may drift numerically, and many summaries are not invertible. A fixed reduction tree permits reuse of unaffected subtrees, but adopting that tree changes the result relative to a prescribed serial floating-point fold. Preserve the existing tree or require an appropriate contract. Mutation, external effects, and unverified mutable external chunk references also need explicit exclusion or invalidation.

[Differential dataflow already supports incremental iterative computation](https://www.frankmcsherry.org/differential/dataflow/2015/04/07/differential.html). The research opportunity is the link between Blade's nominal scientific coordinates, immutable provider content, typed access regions, and compiled kernels. This could make repeated scientific analysis respond to dataset revisions without a separately authored incremental program.

First gate: two snapshots with one changed chunk, first through an elementwise map and then through a halo. Require identical cold/warm outputs and count actual chunks read and output tiles recomputed. Keep the prototype local; distribution is not a prerequisite.

**5. Preserve structured operators until their consumers decide what to compute.**

This is the strongest potential asymptotic win, but its seed is already in `plan-cosmology-nongaussianity.md`, section 5.3, which proposes declared and verified separability. It should not be presented as a newly discovered gap.

For a real A with N rows and D features, applying its Gram operator to a vector can use `(A A^T)v = A(A^T v)`. Forming the Gram matrix first costs O(N²D) work and O(N²) storage; the factored application costs O(ND) work and O(D) intermediate storage, in addition to inputs and output. The saving applies when the matrix itself is not needed. A kernel matrix with arbitrary nonlinear pairwise entries generally has no such factorization.

The useful generalization is to keep a declared operator's action, adjoint action, and factorization available through loop composition and AD. Candidate workloads include low-rank covariance operators, separable convolutions, and matrix-free scientific solvers. An explicitly requested materialization or observable intermediate must still happen. Constructor-derived structure is a stronger starting point than trying to guess low rank from an arbitrary buffer's values.

`src/LinAlgPatterns.fs` currently classifies local contraction shapes and routes them to libraries; its tail at `:1403` also records an unrouted dense nest shape. Library routing and algebraic factorization are different decisions. A new backend will not on its own supply the latter.

Floating-point distributivity and reassociation are not exact. Proving the real-number identity does not authorize changing a `where repro` computation, and an `omp` license is not a universal algebraic-rewrite license. The factored operator may instead be the explicitly declared computation from the outset. A general checked-factorization optimizer would need a separately specified numerical contract.

[Galley already performs cost-based decomposition and planning of tensor programs](https://arxiv.org/abs/2408.14706). Blade's distinctive target would be combining checked operator structure with index provenance, symmetry deduction, and derivative generation. First gate: a Gram-operator application with no Gram allocation, compared against the materialized and explicitly factored references. Establish semantics before considering general contraction-order search.

**6. Require constrained domains to skip excluded space.**

The C2 support-domain proposal in the cosmology plan is worthwhile, but compact allocation alone is not enough. `src/StructIdxSpec.fs:26` describes two enumeration routes; the current counting layer is capped by a 100,000-cell bounding box and uses whole-cell predicates. It is a counting certificate, not yet a scalable runtime iteration contract.

For a band of width w, useful iteration should visit O(nw) cells, rather than scan n² cells and discard most of them. For an affine conservation constraint, solve or bound the dependent coordinate instead of enumerating its entire range. For sparse joins, choose lookup or intersection over an existing key structure. Triangle inequalities can shrink a domain without changing its asymptotic order; do not label every support restriction an asymptotic improvement.

The missing performance requirement is an enumerable continuation of each valid prefix: bounds, congruence progressions, or table-backed successors with known order. Keep an arbitrary static predicate as a small-domain fallback. Symmetry adds another obligation: the logical support must be compatible with the declared group action; filtering a canonical domain does not automatically prove that compatibility.

This is an extension of existing plans and established structured-tensor techniques. Its strategic value is avoiding another representation that saves storage while doing the old amount of discovery work. The failed SimplexBlocks compute gate is a useful reminder to measure the complete consumer.

**Execution order and research claims.**

Run three bounded experiments first: linear additive reverse AD; a shared halo access description; and one reduction-crossing streaming pipeline. Each must name its best existing Blade baseline, the semantic contract, the emitted storage/work change, and a stop condition. Broader checkpointing, sparse inverse maps, and incremental snapshots should follow evidence from those experiments, not precede it.

The best candidate for an original Blade contribution is a compositional account in which **the same index/access evidence determines canonical storage, reverse accumulation, and data dependencies across execution and revisions**. That is a thesis to test, not a confirmed priority claim. C++ and Fortran can implement specialized versions; the proposed advantage is that ordinary Blade declarations derive them together and reject incompatible compositions.

Existing near-term work still matters. Do not replace the measured unroll-and-jam proposal with a grand optimizer, revive refuted static-array erasure, rebrand already planned rolling storage as new, or assume MLIR produces algorithmic optimization automatically. The audit also found two existing planning files missing from the README index (`plan-compact-sym-folds.md` and `plan-deferred-combinators.md`); both were included in this review despite that omission, and both were added to the index on 2026-09-05.

Validation performed for this memo: plan/census review, targeted current-source inspection, primary-source prior-art checks, and emission/inspection of the existing recurrence-AD corpus example. No compiler source or corpus files changed, no full suite was run, and no runtime speedups are claimed. Graft was tried first, but the available index covered the JavaScript/Python surface rather than the F# compiler, requiring direct targeted searches for the compiler findings.
