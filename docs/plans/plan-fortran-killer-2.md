# Beyond symmetry, round two: make scientific computations compose safely

Status: IN PROGRESS. 2026-09-05: the three CORRECTNESS items LANDED on master
(section 3 step 1, appendix A, appendix B, section 7's NetCDF prerequisite BL8012).
2026-09-06, on `feat/fortran-killer-2`: P0 steps 2-5 (Blade.Effects summaries, the
decision record, `blade plan`), P1 destination passing gate 1 (dense provider
writes alias the pool), and P2 6.1 (units on reverse-mode cotangents) LANDED; each
carries a "Landed" note below with its tests. Still planning only: P1 indexed RNG,
6.2 factor-once, 6.3 matrix-free linearization, 7's manifests / run records /
restartable state -- each waits on a surface decision listed in
[plan-structural-synthesis.md](plan-structural-synthesis.md) section 3 or here.
Source snapshot: master at `7d1053e`. Two Sol/medium agents reviewed numerical
operators and storage/data workflows; the parent reviewed optimization contracts,
random generation, and the first audit's comparative claims.

This is a companion to [the first audit](plan-fortran-killer.md), not a replacement
backlog for it. FFI/library emission, compressed/remote data, distributed storage,
and nonlinear recurrence AD remain important. The new question is what becomes
possible when Blade's existing facilities work together rather than individually.

The strongest pitch is **scientific code whose memory use, numerical assumptions,
and data dependencies remain inspectable as it is composed and parallelized**.
Fortran programmers can implement these mechanisms. Blade could remove substantial
manual bookkeeping while checking the relationships that bookkeeping represents.
No speedup over Fortran was measured in this planning round.

## 1. Rebase the argument before choosing more features

Several premises in the historical thread are stale or too strong:

| Earlier premise | Current evidence or correction | Planning consequence |
|---|---|---|
| Chained elementwise expressions always materialize each operator | `IRMono.fs:1057` contains the landed fusion pass; `Optimize.fs:162` orchestrates it and constant-match folding | Target unavoidable materialization, not a second implementation of direct-nest fusion |
| Best-effort convergence always executes the budget | Freeze recognition and guarded recurrences have landed; recognition has a correctness defect below | Harden the analysis before expanding it; storage remains a separate question |
| Units cannot represent hours or kilometers | `tests/corpus/units/045_scaled_unit_declaration.blade` and `046_scale_composes_multiplicatively.blade` exercise scaled units | Use existing scale machinery; the SI module's dimensions-only commentary is stale. Affine units and CF decoding are separate work |
| Disabling contraction gives universal reproducibility | `blade_portability.hpp:184` applies the contraction override on GCC; other macro branches differ. `CodeGen.fs:1401` explicitly documents an unenforced main-local case | State the supported platform, call-graph, library, and FP-environment envelope |
| A named reproducibility mode is unavailable in the Fortran ecosystem | Intel documents `ifx -fp-model=consistent`; GNU Fortran exposes IEEE arithmetic and exception modules | The advantage must be propagation and enforcement of a source contract, not inventing a flag |
| Symmetric storage licenses Cholesky | Symmetry does not establish positive definiteness; LAPACK's positive-definite drivers require both | A checked property or successful factorization is evidence; an index storage class alone is insufficient |
| A false convergence guard establishes a differentiable fixed point | A guard can mean any stopping condition, including an invalid-value comparison | Implicit differentiation requires an explicit equation and mathematical hypotheses; never infer it from `while` or freeze syntax |
| Both backends consuming one optimized tree proves the optimization | They can agree on the same incorrect rewrite or mirrored numerical algorithm | Compare optimization on/off and use independent mathematical oracles |

External baseline: [Intel's ifort-to-ifx guide](https://www.intel.com/content/www/us/en/developer/articles/guide/porting-guide-for-ifort-to-ifx.html)
documents consistent FP settings; [GNU's IEEE modules](https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html)
document arithmetic/environment support. [LAPACK DPOSV](https://netlib.org/lapack/explore-html/de/d6c/group__posv_ga4844053bd30fe88a17a7e08d93bbae4b.html)
requires positive definiteness. These are baseline capabilities, not missing Fortran features.

## 2. Shortlist and recommended order

| Priority | Addition | What is new relative to round one | First useful proof |
|---|---|---|---|
| P0, prerequisite | Shared effect/dependency facts and auditable optimization decisions | One legality model across rewrites, rather than separate syntactic gates | Effectful freeze probe preserves both state and calls with optimization on/off |
| P1 | Execution directed by consumers, with storage reuse across barriers | Optimize values that fusion cannot eliminate; attach a memory estimate to the resulting plan | A pipeline ending in a provider write saves a full result/flattening allocation |
| P1, independent | Random samples addressed by logical identity | Same logical experiment after rechunking, reordering, or partial recomputation | Indexed uniform draws agree bitwise across partitions and traversal orders |
| P2, contained first step | Units on reverse-mode derivatives | Compose existing scaled units with AD instead of stripping units at the boundary | A meter-valued objective differentiated by velocity yields seconds |
| P2 | Reusable checked numerical operators | Combine solves, validation, and derivative actions without repeated work | Repeated RHS and derivative actions reuse one validated factorization |
| P2 | Executable input contracts and run records | Whole-run validation and provenance, beyond runtime paths and IDE hints | Same executable accepts compatible inputs and rejects schema drift before an unsafe read |
| P3 | Restartable recursive state | Operational recovery derived from recurrence dependencies, distinct from AD checkpointing | Interrupted/resumed final state equals uninterrupted execution under a pinned environment |

These are milestones, not parallel commitments. Ship P0 first. Prototype the two
P1 items independently; advance either only after its concrete workload passes.
The numerical and data milestones can then share the same effect and identity facts.

## 3. P0: make equivalence evidence reusable

### What exists and what is missing

`Optimize.fs` has the right charter: change cost, never contract. Its freeze
recognizer currently runs on the AST before resolved callable effects are available.
`guardAdmissible` (`Optimize.fs:106`) accepts ordinary calls based on their argument
shapes and treats the head name as invariant. An invariant function name does not
establish that calling it is pure or returns the same result.

There are already two different purity analyses. Fusion's module-local `pureBody`
(`IRMono.fs:1090`) checks assignments and display effects and resolves callees.
`IRPrint.exprAttrs` (`IRPrint.fs:485`) marks display effects, but its general child
walk is not a complete mutation/exception/unknown-call model. Neither should be
promoted to a universal proof merely because it returns a Boolean called `IsPure`.

### Proposed implementation sequence

1. Immediately make freeze recognition decline unresolved/effectful callees. Keep
   a small audited intrinsic allowlist if resolved analysis cannot yet run there.
   This is a correctness repair, not a new language restriction on valid programs.
   **Landed (2026-09-05).** `recognizeFreezeIdiom` now takes the caller's
   `calleeAdmissible` judgment; `inferRecArray` passes "a scalar intrinsic
   (the math table, `abs`/`real`/`imag`/`arg`, `atan2`/`log_base`, `fma`, the
   numeric casts) that `lookupVar` finds unshadowed". Every other head --
   effectful, provably pure, or a shadowed intrinsic name -- declines, and the
   program compiles unchanged. Pins: `tests/OptimizeTests.fs`
   (`effectful_guard_declines`, `pure_helper_guard_declines`,
   `shadowed_intrinsic_guard_declines`: zero breaks, zero aborts) and
   `tests/corpus/recursive-arrays/020_freeze_idiom_effectful_guard_declined.blade`
   (appendix A's program, `last = 6`, `calls = 7`, both lanes). Step 2 -- keeping
   the candidate and discharging purity against the typed callee, which would
   re-admit `pure_helper_guard_declines` -- is the open follow-up.
2. Retain an AST candidate plus provenance, then discharge its obligations against
   typed/resolved bodies before replacing the recurrence. Avoid trusting source
   spelling when a builtin name can be shadowed.
3. Derive conservative summaries for reads, writes/aliases, I/O, possible failure,
   unknown calls, and FP-contract requirements. For recursive call graphs compute
   a conservative fixed point; unknown is not pure. Distinguish repeatable
   evaluation from movable evaluation: a pure expression that may fail cannot
   necessarily be hoisted past a conditional.
4. Reuse the facts for freeze, let-bound fusion, destination reuse, and later CSE.
   Record each decision with rule ID/version, source origin, proved obligations,
   missing evidence, and selected emission route. Preserve spans through cloning.
5. Expose that record through the existing IDE protocol and a proposed CLI plan
   view. This is diagnostic data, not permission to branch in Blade on inferred
   symmetry/equivariance or optimizer outcomes.

**Landed (2026-09-06), steps 2-5 in their first form.** `src/Effects.fs` holds the
summary lattice (`Mutates`, `EmitsOutput`, `ReadsExternal`, `MayFail`, `Unknown`; the
REPEATABLE / MOVABLE distinction of step 3) and the decision record.
`TypeCheckSupport.effectsOfBody` computes one summary per function declaration from
the TYPED body with callees resolved through `TypeEnv.FuncEffects` (declaration
order; a function's own recursive calls read as pure, a lambda-valued head or a
missing summary is Unknown), the typed declaration and the IR callable carry it, and
two consumers read it: freeze recognition (a guard may call any REPEATABLE declared
function -- the pure-helper and pure-shadowing cases the step-1 allowlist declined
are admitted, effectful and transitively effectful helpers still decline; pinned in
`tests/OptimizeTests.fs` and `tests/corpus/recursive-arrays/021`) and the fusion
pass (`IRMono.pureBody` takes a repeatable summary as proof and walks only unmarked
callees). Both passes record a `Blade.Effects.Decision` -- rule, version, subject,
span, applied or declined with the first reason, evidence -- into a per-flow
collector; `blade plan <file> [--json]` installs it, lowers, and prints the record.
Not done: the IDE-protocol exposure (the protocol package is shared with external
clients and a new message is a surface decision), and callee purity for lambdas
(they stay Unknown; fusion's IR walk still covers them). **Landed (2026-09-07)**,
the two consumers this paragraph named: let-level CSE over REPEATABLE values
(`Blade.Optimize.cseModule`: straight-line bodies only, a body with any assignment or
loop declines wholesale, deferred join operands untouched, callees judged by their
summary; rule `cse`, `tests/OptimizeTests.fs` + `tests/corpus/functions/132`) and
destination reuse in the form of scratch reuse (section 4, gate 2 below; rule
`pool-reuse`). The `segment-streaming` record (v2) now also states what the
streamed variable would hold materialized, the first number a cost model for that
choice needs.

The equivalence registry should be ordered and bounded, with stable tie breaking.
An explicit greedy ordering is a useful v1; an unrestricted e-graph or exhaustive
search is not required. Separate **legality** from **estimated profitability**.
Literal byte counts are facts; cache behavior and execution times are estimates.
Any later machine profile belongs in the plan/cache identity.

### Acceptance and refusal criteria

Test optimization on/off independently in each backend, comparing values, visible
mutation, output events, and failures. Add negative cases for mutable captures,
aliased state, helper calls, dynamic ordinals, deeper lags, shadowed intrinsics,
guard failures, and signed-zero/NaN boundaries. Separately test emitted allocation,
loop, and call counts: equal outputs alone cannot detect a lost optimization.

Fusion must also preserve rounding/explicit-FMA boundaries under the chosen FP
contract. Cell independence does not by itself prove that removing a rounded
intermediate store is bitwise neutral. The newly landed `fma` intrinsic
(`tests/corpus/intrinsics/020_fma.blade`) remains an explicit single-rounding
operation even inside a reproducible function.

Do not require a user `pure` assertion to compensate for missing compiler facts.
Decline the cost transformation while preserving the original program. This work
earns its place if two existing optimization families can consume the same facts;
avoid building an elaborate proof framework with only one consumer.

## 4. P1: let the consumer determine where results live

### Proposed scope

Model each materialized value as logical data plus a destination/lifetime plan.
Consumers include another computation, a returned array, a provider sink, and a
terminal reduction. Start with dense local arrays and synchronous sinks.

Existing seams:

- `CodeGenFusion.fs:602,620` allocates intermediate and result pools in a particular
  staged composition path. This is not a claim that every `>>@` pipeline allocates;
  kernel composition and direct-nest fusion already eliminate other cases.
- `CodeGenLoopNest.fs:3396,3581,3893` records pool ownership, escapes, and scope frees.
  Freshness and escape facts are useful inputs, not a proof of safe last-use reuse.
- `CodeGenBinding.fs:1472` prepares provider writes by flattening materialized arrays.
- [Streaming I/O P5](../../src/providers/StreamingIONotes.md) already proposes write
  terminals. This milestone extends that direction to destination passing across
  functions and reuse at barriers that must remain materialized.

First pass: write a producer directly into a fresh caller-owned result or a bounded
provider staging buffer. Second pass: recycle a dead, nonescaping allocation only
after all data views and borrowed extent/side-table pointers are dead. Compatibility
includes element type, capacity, alignment, layout, and ownership of metadata.
Do not equate distinct index names with disjoint storage.

**Landed (2026-09-06), gate 1 for the write terminal.** A dense provider write no
longer flattens: `allocate<>` places every scalar of a nested array in one
contiguous pool in DFS (row-major) order -- exactly the Horner index the copy loop
computed -- so `genProviderWriteBinding` aliases `pool_base(src.data)` as the
writer's buffer and emits no allocation, copy, or free (`src/CodeGenBinding.fs`).
Gated on plain dense index kinds; compound / sparse / ragged / packed / wreath
sources keep their copies. `tests/NetcdfTests.fs` pins the emission and round-trips
a 2 x 3 literal through libnetcdf. Gate 2 (scratch reuse across barriers) is not
started; it needs the effect summaries above plus escape facts, as the paragraphs
say.

The planner should report peak live bytes, unavoidable barriers, and why each
candidate pool is retained. Optimize whole-function lifetimes before attempting
global scheduling or distributed reuse.

**Landed (2026-09-07), gate 2 in its first form: scratch reuse inside a function
body.** `Blade.Optimize.planPoolReuse` (run at the end of module lowering, after the
second forcing pass) plans, per body, which fresh dense pools can take an earlier
pool that is DEAD (nothing at or after that point reads it -- liveness sees
through deferred join operands and callable captures) and UNALIASED (every position
naming it is a fresh-pool form or a call-free scalar; a view, an alias, a tuple, a
call to a declared callable, an assignment or a loop declines it), of the same
element type and the same LITERAL extents. The RETURN position is a reuser too. The
plan lives in `Types.PoolReuseTable`; codegen rewrites the reuser's declaration into
`{ donor.data, own_extents }` (`rewritePoolAlias`, anchored on the name so every
emitter's line qualifies) and spares the right scope free, and an escaping reuser
carries its donor into the escape set. Every body with two or more candidate pools
records a `pool-reuse` decision with the pairs and the peak live pool bytes before
and after (literal extents only) -- the planner's report, for `blade plan`. Found and
fixed on the way: `computeScopeEscapes` propagated through SCALAR lets, so a kernel
capturing `m = reduce(y, (+)) / n` pinned `y` and leaked it per call (the demean /
normalize shape); a scalar holds no storage and is now a barrier like a fresh pool.
Pins: `tests/OptimizeTests.fs` (the return alias, the chain's two reusers per body,
the leak, the decision) and `tests/corpus/memfree/022` (values in both lanes -- the
interpreter never reuses -- including the view-alias negative). Not done: runtime
extents (the generic body of a `T^1` function keeps its allocations), the second half
of gate 2 (reuse across REPEATED executions, i.e. per-call scratch that survives the
frame), and peak RSS measurement against a Fortran baseline.

### Demonstrator and gates

Use a dense signal-processing pipeline with a real whole-array dependency between
two pointwise stages, then a local Zarr write. Compare the same unfused barrier and
operation order on both sides. Gate 1 is removing the final full-result/flattening
copy using a write terminal. Gate 2 is reusing scratch across repeated executions.

Require at least one full-array allocation saved, explain the measured live-pool
count, and measure peak RSS separately from allocation counts. An out-of-place
barrier may need two simultaneously live working arrays: never promise a universal
one-buffer result. Check escape-through-tuples, partial views, mutation, and async
lifetimes before broadening the admitted set. Run memcheck and backend differentials.

Compare with a Fortran implementation using preallocated work arrays and the same
library routines. The advantage sought is comparable memory behavior from natural
composable source. If that requires a hand-written buffer schedule in Blade, the
proposal has missed its purpose. If reuse finds few real candidates, retain the
independently useful destination-passing subset and stop there.

## 5. P1: random draws belong to the experiment, not its execution schedule

### Evidence and design

`rand_runtime.hpp:48,184` seeds a per-call `mt19937_64` from a key and fills a dense
pool sequentially. `Interp/RandMirror.fs:11` mirrors flat pool order. This provides
repeatability for the same call; it does not provide cheap random access to sample
number i or an identity preserved when work is partitioned into separate calls.
The runtime already names a counter-based backend as a possible future direction.

Introduce a versioned indexed generator whose conceptual address is:

`(algorithm_version, experiment_key, stream_key, logical_sample_id, draw_id, attempt_id)`

Use a published counter generator with defined bit operations and reference vectors,
not a new hash invented for Blade. Dense v1 can use a global linear sample ordinal
under a fixed declared domain schema. Later sparse/particle domains need persistent
IDs: a local pool position, rank, compiler-minted type ID, or unstable hash is not
a sample identity. Axis permutations preserve samples only when the mapping to
their original IDs is preserved explicitly.

The runtime receives global offsets from the existing iteration structure and,
later, [chunked storage](plan-distributed-memory.md). Rejection samplers get a
per-sample attempt counter so rejection in one cell cannot shift all later cells.
Changing transform algorithms changes outputs: preserve the existing version or
make migration explicit; do not silently replace today's pinned stream.

This algorithmic idea already exists in [Random123](https://www.thesalmons.org/john/random123/papers/random123sc11.pdf).
The Blade opportunity is to derive addresses from logical index domains and carry
them through chunking and restart. GNU Fortran's [RANDOM_NUMBER documentation](https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fNUMBER.html)
describes per-thread state; a Fortran program can also use a counter-based library.

### Staging, accuracy, and gates

Start with uniform and Bernoulli: integer generator bits and the mapping to
representable values have a manageable portability contract. Add normal and rejection
families only with a versioned transformation and explicit libm/FP assumptions.
Portable raw bits do not make `log`/`cos` or rejection decisions universally bitwise.

Demonstrate identical indexed uniforms when samples are generated whole, in uneven
chunks, in reverse chunk order, and as selected subranges. No prefix replay or full
array allocation should be needed to generate a late chunk. Later add different
thread/rank counts and checkpoint restart without changing logical addresses.
Test address collisions/overflow, multiple streams, and the generator's reference
vectors. Statistical validation supplements these tests; it does not prove independence.

**Landed (2026-09-07): the indexed families.** `rand.<fam>_at(key, stream, offset,
params.., shape)` for every family (uniform, normal, exponential, gamma, poisson,
bernoulli, beta, categorical): cell i is logical sample `offset + i` and its value
is a pure function of the address (algorithm "philox4x32-10 v1": Philox key = the
64-bit experiment key; counter = [sample lo, sample hi, 32-bit stream key, per-sample
draw counter]; one block = two raw 64-bit words mapped to [0, 1) as an mt19937_64
word is). The eight transforms are shared with the sequenced families -- they became
templates on the raw source in `cpp/rand_runtime.hpp`, an interface in the F#
mirror -- so a rejection sampler is chunk-invariant because each sample owns its
draw counter, not because anything was rewritten. The stream key must lie in
[0, 2^32) and the offset be non-negative (literals refused at typecheck, runtime
values abort BL8001 in both lanes). Pins: the three Random123 `kat_vectors` lines
for philox4x32-10 against the F# twin (`tests/RandMirrorTests.fs`, `blade test
rand-mirror`), the interpreter differential tying the C++ header to that twin, and
`tests/corpus/rand/019-022` (whole vs uneven chunks vs reverse order vs subrange
identical to the bit for uniform; the same for every other family including the
rejection samplers; stream / key separation; the two guards). Not done: the
sequenced families keep their pinned mt19937_64 streams (no migration); the
checkpoint/restart story (section 7) and thread/rank-count invariance need only the
address, which is what landed. The framing chosen with the user -- rand as a MODULE
that is a thin wrapper over a C++ generator -- is the shape it took: the module
rewrites the call, the header owns the generator, and nothing in Blade source can
see the bits.

Before extension, fix the existing Poisson defect in Appendix B. Use a suitable
large-parameter algorithm or an explicit supported-domain refusal. Identical outputs
from the two implementations are not a probability-distribution accuracy test.

Do not advertise an entire Monte Carlo result as bitwise reproducible just because
its samples are. Reductions still have an order. Fixed logical reduction blocks plus
an ordered combine can decouple order from worker count; order-independent/binned
accumulation is a different numerical algorithm and needs an explicit contract.
[Open MPI's reduction documentation](https://docs.open-mpi.org/en/v5.0.10/man-openmpi/man3/MPI_Reduce.3.html)
permits reassociation, so using a fixed number of ranks alone is insufficient.

## 6. P2: reusable checked numerical operators

### 6.1 Units on cotangents: the smallest distinctive addition

Forward mode already preserves the primal unit on tangent seeds (`Grad.fs:256`).
Reverse mode refuses unit-bearing parameters (`GradNormalize.fs:687`) and unit-bearing
losses (`Grad.fs:112`), because generated array gradient buffers copy the parameter's
type (`Grad.fs:241`). This is a concrete seam where two existing Blade strengths fail
to compose. It is a better near-term target than a completely new solver syntax.

Derive the cotangent element unit as objective-unit / parameter-unit, preserving
the parameter's index domain while changing its scalar unit. Use resolved unit
algebra, including scale; do not manipulate source unit names as strings. Update
scalar return gradients, mutable gradient buffers, seed typing, accumulation zeros,
and alias resolution together. The operation's semantics must remain clear when
different parameters have different scales.

Demonstrate a trajectory objective measured in meters with a velocity parameter
measured in meters/second: the derivative must be measured in seconds. A buffer
annotated meters must fail. Then compare the same calculation expressed in meters
and kilometers, checking the derivative's expected coordinate scaling, not merely
its printed unit label. Include scalar and array parameters and aliases.

For heterogeneous residuals, make each component dimensionless with declared scales
before taking a norm or comparing to a dimensionless tolerance. Units cannot choose
scientifically meaningful scales or fix ill-conditioning. If unit resolution must
move earlier than AD expansion, plan that pipeline change explicitly; do not retain
today's parameter-type buffer as an approximation. Affine units remain separate.

**Landed (2026-09-06).** Reverse mode accepts unit-carrying parameters and losses.
The gradient ABI declares each cotangent as <loss>/<parameter> on the parameter's
own index domain (`Grad.cotangentTy`: scalar returns, mutable array buffers, and the
scalar cotangent locals), the seed stays dimensionless (it is d(loss)/d(loss)), and
the cotangents of INTERMEDIATE locals are ascribed <loss>/<value> from a symbolic
element-unit walk over the normalized body (`Grad.unitGuessOfBody`, mirroring the
checker's rules on the forms the AD subset admits; anything it does not know leaves
that cotangent bare, which the checker then refuses loudly in a chain rather than
accepting wrongly). Unit resolution did NOT move earlier: the ordinary checker
resolves the surface unit expressions, scale included, and holds every `+=` to
them -- the derivative code is unit-checked for free. Two things are refused because
the transform cannot see a conversion factor: a `+`/`-` join between magnitudes of
one dimension inside the synthesized body (the `__ad_body` conjunct, registered in
`Constraints.fs`, makes `convertScaleTo` refuse instead of inserting the factor) and
a unit-annotated `let` inside the differentiated body (its annotation would not
survive normalization). Demonstrated in `tests/corpus/ad/025` (a meter-valued
objective differentiated by a meters/second parameter yields seconds; a two-level
chain through an m2 intermediate to an m4 loss; a dimensionless ratio) and refused in
`ad/017` and `ad/026`. Scaled units are otherwise supported (the same magnitude
throughout is fine); the affine case remains separate.

### 6.2 Factor once, solve and differentiate repeatedly

The extension should make repeated scientific operations share validated structure,
workspaces, and derivative actions. It should not grow a list of unconnected intrinsic
names. The first milestone is one dense real solve family, followed by a matrix-free
operator only where current AD can support it.

Today the elaborator maps `solve` directly to `IRSolve` (`MathElaborate.fs:205`,
`IR.fs:185`), and the native solve works on a fresh LU copy (`CodeGenExpr.fs:3117`).
`tests/corpus/math/070_solve_exact_3x3.blade:39` even contains repeated solves of the
same matrix. The proposal is persistent factor/workspace identity and derivative
reuse, not simply exposing more LAPACK entry points.

Separate logical operator input/output domains from the algorithm's buffers. A
factorization value should carry the element precision, input/output index domains,
unit relationship, pivot/storage convention, and evidence about the matrix it factors.
An immutable snapshot or ownership restriction is needed so a mutation cannot leave
a cached factorization referring to obsolete data.

Permit repeated solves, transpose solves, and derivative actions to consume that
factorization without refactoring. Never pick a positive-definite solver merely
because the array uses symmetric storage. A successful checked factorization can
establish the required property for its snapshot; an unchecked claim cannot.

For `A x = b`, the local derivative relation is `A dx = db - dA x`. A transpose
solve supplies the real-valued reverse action. These identities assume a nonsingular
operator and require appropriate handling of numerical failure. They do not imply
that differentiating pivot-selection code or arbitrary iteration stopping decisions
produces a useful derivative.

Expose an optional checked result with solution, residual/backward-error indicator,
conditioning information where supported, and a failure status. An explicit checked
surface may validate more than today's `solve`; an optimizer may not silently add a
new abort to existing calls. A small residual alone is not a small forward error for
an ill-conditioned system. Use established expert drivers where suitable: [LAPACK
DPOSVXX](https://www.netlib.org/lapack/explore-html/d5/d95/group__posvxx_ga48edd52284bc03902274c5a275bf00ea.html)
already illustrates error bounds, condition information, and factor reuse.

**Landed (2026-09-07), the first milestone: one dense real solve family with a
factorization VALUE.** `m.lu(A)` keeps the partial-pivoted LU `m.solve` computes as an
ordinary tuple `(LU, piv)` -- the packed L\U factor (n x n Float64) and the pivot rows
(n Int64, 0-based) -- immutable by construction (A was copied), and `m.lu_solve(f, b)` /
`m.lu_solve_t(f, b)` (or the two-halves spelling `(LU, piv, b)`) apply it to any
right-hand side, plain or transposed, without refactoring. Same working copy, same
strict-`>` pivot rule, same fused elimination order, so `lu_solve(lu(A), b)` is BITWISE
`solve(A, b)`; the interpreter twins (`luArrays`, `luSolveArray`) reproduce both arms
byte for byte. LAPACK route: `?getrf` / `?getrs` ('N'/'T') through
`blade_lapack::blade_lu_d` / `blade_lu_solve_d` with the same column-major bridge as
solve's adapter and the pivots converted to the stored 0-based convention; policy rows
`Getrf` / `Getrs` pinned. This is the operator-value form docs/plans/structural/05 step
2 asked to co-design: structure carried by the NODE and an ordinary tuple, no new
type, no forcing rules -- a factorization is consumed by actions, never by cells, and a
tuple already refuses cell reads. Pins: `tests/corpus/math/083-086` (the exact 3 x 3:
factor values, two right-hand sides, the two-halves spelling, the transpose solve,
`d = 0` against `solve`; the extent refusal; the singular abort at factor time; the
function-body shape) and `tests/LapackTests.fs` (both gates, the rejection, the policy
rows). The derivative actions landed with 6.3 the same day (below). Not done: the
checked result (residual / conditioning / status) and mixed-precision refinement.

Gate the initial demonstrator on several RHS vectors plus a derivative action:
count one factorization; compare solutions/residuals against the same LAPACK backend
from Fortran; independently check the derivative identity on well-conditioned cases;
reject wrong index domains/units and surface singular/invalid inputs. Do not judge
only runtime: record memory and numerical quality at equal accuracy requirements.

Mixed-precision refinement is a later, explicit policy over this checked interface,
not automatic narrowing based on a tolerance. Accumulate/check the residual in the
promised precision, bound retries, and define a higher-precision fallback or failure.
LAPACK already supplies [mixed-precision refinement drivers](https://www.netlib.org/lapack/explore-html/db/d57/group__posv__mixed_gae2473f2512d331fbdec3b1e2b63fc821.html);
Blade's contribution would be safe composition and provenance of the chosen route.

**Co-design note (2026-09-07).** The first member of the "operator consumed by actions,
not by cells" family landed as structural/05 step 1: `gram_apply(A, B, x)` = A·(Bᴴx), an
eager node with a native arm, a transposed-gemv BLAS route, an interpreter twin, and AD
rules whose adjoint action is itself a `gram_apply`. A factorization VALUE (this section)
and an operator value (structural/05 step 2) are one ABI/ownership design -- an immutable
snapshot consumed by solve / transpose-solve / derivative actions -- and should be decided
together; the node exists so that design has a concrete consumer and a fixture for the
`dot(w, Jv) = dot(Jᵀw, v)` identity of 6.3.

### 6.3 Matrix-free linearization: a later extension with its own gate

**Landed (2026-09-07), the JVP/VJP primitive.** `ad.jvp(f)` accepts ARRAY-valued
returns (and tuples mixing arrays and scalars): `f__jvp(args..., seeds...)` returns
`(y, J v)`, the tangent of an array local being the array expression the sweep already
carried. `ad.vjp(f)` is the SEEDED reverse mode for an array-valued `f`:
`f__vjp(args..., w, buffers...)` takes the output's cotangent `w` (typed as the return)
after the original parameters, accumulates `Jᵀ w` into the same `mut` cotangent buffers
`ad.grad` uses, and returns the primal array; the body's value must be a NAMED array
(`let y = ...` then `y`), because the seed is accumulated cell by cell onto that name's
cotangent buffer. Same reverse sweep as grad (`Grad.synthesizeRev`, one code path, mode
flag), same subset, same BL5500 family. The pair is one resolved function's forward and
adjoint actions with no Jacobian formed; `dot(w, J v) = Σ dot(Jᵀ w, v)` is pinned
exactly on `gram_apply` (corpus `ad-jvp-comb/110`, values by hand) and on the LU
solves (`113`).

The LU derivative actions (6.2's open item) ride the same sweeps: a body that factors
once (`let f = m.lu(A)`) and applies the factors differentiates by MORE SOLVES against
the same factors, never by differentiating the factorization -- forward
`A dx = db − dA x` (one row map for `dA x`, one `lu_solve`), reverse
`bbar += A⁻ᵀ xbar` (the transposed solve IS the reverse action) and
`Abar += −(A⁻ᵀ xbar) xᵀ`; roles swap for `lu_solve_t`. The factor binding is
structural (no tangent, no cotangent buffer: `GradNormalize.analyze` skips it,
`GradCommon.luFactorsOf` maps factor → matrix for the arms). Refused with a named
message: a factor the sweep cannot trace to a named matrix (an alias; the two-halves
spelling dies earlier on the tuple-pattern rule) -- `ad-jvp-comb/114`. Pins: `113`
(grad / jvp / vjp through `lu_solve` and `lu_solve_t`, exact 3×3 values, both
transpose identities). Not done: the linearization VALUE grouping the actions, the
Newton-GMRES demonstrator, implicit adjoints of a declared residual equation.

`ad.jvp` previously required scalar or all-scalar-tuple returns (`Grad.fs:280`), so
an array residual cannot directly supply a Jacobian-vector product. A vector-output
pullback surface is also missing; `ad.grad` is scalar-loss oriented. First expose
array-output JVP and seeded VJP actions from one resolved residual function, using
specialized generated functions rather than requiring a general closure runtime.
An eventual linearization value may group those actions, but its spelling is open.

The numerical target is Newton-GMRES for a nonlinear Poisson inverse problem. It
needs no dense Jacobian, a separately chosen preconditioner, explicit boundary
conditions, bounded iteration/storage, and truthful primal/adjoint failure states.
Derive an implicit adjoint only for a declared residual equation with the required
local nonsingularity/differentiability assumptions. Do not differentiate a solver's
discrete stopping decisions or infer the equation from an absorbing guard.

Acceptance: no N-squared Jacobian allocation; on small cases compare to a dense
Jacobian and direct solve; test directional derivatives and the adjoint identity
`dot(w, Jv) = dot(J_transpose_w, v)`. Require a transpose action/solve, except where
its equality to the forward action is actually established. Use unit-aware dual
pairings and sweep primal/adjoint tolerances. Finite differences are an independent
check with truncation/cancellation limits, not a universal oracle.

[PETSc shell matrices](https://petsc.org/main/manualpages/Mat/MATSHELL/) already
support application-defined operator actions, and [JAX custom_linear_solve](https://docs.jax.dev/en/latest/_autosummary/jax.lax.custom_linear_solve.html)
already supports implicit solve derivatives. Blade's proposed advantage is deriving
the paired actions and checking their domains/units. If operator specialization
explodes code size or array-return AD requires a wholesale rewrite, stop at the
useful JVP/VJP primitive and postpone the solver framework.

## 7. P2/P3: inputs and restart state are part of the computation

### Executable input contracts

`IR.fs:1355` has `ProviderReadSpec`; `ProviderRegistry.fs:36` already supplies
metadata/fingerprint seams. Aggregate dependencies into a manifest of logical input
name, variable, element type, ordered axes, extents, storage interpretation, units
when known, and identity policy. Reuse the existing provider model and Build.fs's
compiler/target/runtime cache identity (`Build.fs:515,672`); do not add a second cache.

Distinguish schema compatibility, version identity, and exact content identity.
Compile-time folded values require their recorded content identity; runtime ensemble
inputs may share a schema without sharing data. Equal dimension lengths do not prove
equal coordinates or physical grids. An input contract must state whether coordinate
identity is required, and validate it when relevant.

There is also a safety prerequisite: the NetCDF dense reader (`NetcdfProvider.fs:433`)
allocates from baked dimensions and then calls `nc_get_var_*`. Source inspection
shows no intervening rank/extent validation in that path. A changed file could exceed
the buffer; this is a source-supported risk, not a reproduced crash this round.
Validate on the opened handle before reading and protect against concurrent dataset
changes with snapshot/locking guarantees or an explicit immutability requirement.
**Landed (2026-09-05): BL8012 "provider shape mismatch".** `CppNetcdf.ncShapeGuard`
runs between `nc_inq_varid` and the first read at all three NetCDF sites (dense,
compound variable + mask, stream open) and compares the opened variable's rank and
every `nc_inq_dimlen` against the baked extents; the interpreter's
`materializeProviderRead` is the twin. `tests/NetcdfTests.fs` compiles against
`sample.nc`, then runs the same exe against ncgen-built files with a longer leading
dimension and with a lower rank, asserting the abort lands before any output. Equal
lengths on a different grid are still not detected: coordinate identity is the
manifest's job, as the paragraphs above say. Snapshot/locking is not addressed.
The library exposes [variable inquiries](https://docs.unidata.ucar.edu/netcdf-c/current/group__variables.html)
and [dimension lengths](https://docs.unidata.ucar.edu/netcdf-c/current/group__dimensions.html).

First demo: compile once against a schema, run two compatible inputs, then reject
a swapped-axis/wrong-shape input with a Blade diagnostic before unsafe reads. Runtime
paths and CF decoding are dependencies from round one, not already built features.
Start with dense CSV and NetCDF checks before inventing a universal store abstraction.

A run record should capture observed input identities, executable/compiler/runtime
identity, FP policy, selected library routes, RNG version/keys, and completion status.
It makes reproducing a result practical, but is not a mathematical guarantee of
cross-platform equality. Provide logical input names so records need not expose
machine-specific paths.

**Landed (2026-09-07): the manifest and the run record, v1.** `src/RunRecord.fs`
derives the INPUT MANIFEST of a lowered program: one entry per provider read
(`IRModule.ProviderReads`, named by its receiving binding -- provider, path, variable,
element type, ordered axes with static extents, storage interpretation
dense/packed/compound/window/stream, units when the element carries one) plus one per
input the COMPILER folded (`let static ... |> alias.read`; `ProviderStatics` logs
each compilation's folds with the SHA-256 the fold was taken over). The identity
policy is recorded per entry: `content` for a fold (the hash IS the identity the
executable's values depend on), `version` for a runtime read (size + mtime observed
at run; the values depend on content nobody hashed). `blade plan` prints the manifest
after the decisions (`--json` adds an `inputs` array). Codegen bakes the same table
into every program as a static row table plus a file-scope `blade_rr::AtExit`
(`src/cpp/blade_run_record.hpp`) whose destructor runs on the normal return and on the
runtime's failure exit alike; when `BLADE_RUN_RECORD=<path>` is set (or `blade run
--run-record path`) it writes ONE JSON object: program, status (`ok`, or the BLxxxx
code + message `blade_rt::panic` left behind), executable path/size/mtime + the
toolchain (`__VERSION__`) + the compiler version, the build policy (march,
fp-contract, reassoc) and the library routes actually linked (BLAS / LAPACK / cuBLAS)
-- these travel as `-DBLADE_RR_*` defines on the g++ command line so the emitted
.cpp carries no environment and the executable cache keys on them -- the RNG
generator when the program draws (`philox4x32-10` v1), and per input the manifest
row plus what the run observed (exists, file/directory, size or entry count,
mtime). Rank 0 writes under MPI. Pins: `tests/RunRecordTests.fs` (manifest,
renderings, folded identity, emission determinism and environment-freedom, and with
g++ the ok record, the no-pin no-file case, and the BL8007 abort record); `blade
test run-record`. Not done: RNG stream keys (runtime values), a content hash on
runtime reads (policy, not omission), coordinate identity (equal shapes on a
different grid), a schema-compatibility check on resume -- the demo's "reject a
swapped-axis input" is BL8012's today, at the read, not the manifest's.

### Restartable recursive arrays

Use the already explicit step ordinal and lag reads to derive a restart envelope:
committed ordinal, required lag history, state schema, stopping state, executable/FP
identity, and input-manifest digest. This is fault recovery, distinct from the AD tape
in round one's recurrence-adjoint proposal. It adds observable writes and therefore
must be a requested checkpoint policy, never a silent equivalence optimization.

Dense serial v1 can commit generation directories and publish the new checkpoint only
after all data is complete. Filesystem/provider durability and atomic replacement must
be verified for the chosen platform; do not assume a directory rename is universally
atomic. Reject incompatible resumes. Later distributed recovery needs collective
commit semantics, not one independent checkpoint per rank.

Demo: interrupt a fixed-step RK4 evolution after a checkpoint and resume. Require
the same final state and future suffix as an uninterrupted run under the pinned
executable and FP environment. Minimal lag state cannot recreate an already discarded
trajectory: promising the complete trajectory requires persisting that prefix too.
Persist or conservatively reject opaque state, effects, and history accesses whose
bounded dependencies cannot be established. Indexed RNG makes sample regeneration
possible without storing a sequential stream per worker.

## 8. Proof-of-value campaign and explicit deferrals

Use three workloads, each with an idiomatic Blade and competent Fortran baseline:

| Workload | Feature combination | Evidence to collect |
|---|---|---|
| Signal pipeline with a whole-array barrier and data output | P0 + destination passing + input validation | Live bytes, allocations, bytes copied/read/written, equal values, diagnostic quality |
| Independent stochastic trajectories over stable sample IDs | Indexed RNG + bounded recursive storage, later restart | Sample identity across schedules, distribution tests, memory per sample, resume equivalence |
| Repeated small/medium linear solves with sensitivity calculations | Factor reuse + checked results + derivative actions | Factorization count, residuals, conditioning, derivative identities, workspace size |

Pin compiler versions, libraries, FP flags, input semantics, and operation order.
Separate compile time from steady-state execution and report peak RSS separately.
Follow `src/microkernels/README.md`: interleaved A/B medians, quiet host, non-power-of-two
sizes and tail cases, inspect emitted code, and expect attractive hypotheses to lose.
Do not transfer a microkernel speedup to a workflow without measuring that workflow.

Before timing, establish mathematical correctness independently of the interpreter:
manufactured solutions/residuals, known distribution behavior, analytic derivative
identities, and exact/reference generator vectors. Then use the interpreter to catch
backend divergence and optimization on/off to catch shared-rewrite mistakes.

Defer general proof search, user-visible matching on deduced symmetry/equivariance,
automatic implicit differentiation of arbitrary stopping loops, arbitrary mixed-
precision rewriting, a new distributed scheduler, and universal checkpoint portability.
These expand semantic commitments faster than they establish an adoption advantage.
Keep the existing interop/--lib plan: a useful kernel that can enter an existing
Fortran/Python workflow is a stronger adoption demonstration than a compulsory rewrite.

## Appendix A. Confirmed freeze-recognition wrong answer

This entire program was run with the current prebuilt Release binary, using a
private temporary working directory. Only `BLADE_FREEZE_IDIOM` changed between runs.

```blade
type C = Idx<1>
type It = Idx<8>
function tick(c: mut Array<Float like C>) -> Bool = {
    c((0 : C)) += 1.0
    c((0 : C)) > 2.0
}
let mut counter: Array<Float like C> = [0.0]
let rec xs: Array<Float like It> =
    match xs with
    | zero -> zero
    | zero :: s -> zero :: 1.0
    | prefix :: n -> prefix :: (if tick(counter) then prefix(n - 1) + 1.0 else prefix(n - 1))
let last = xs((7 : It))
let calls = counter((0 : C))
```

| Recognition | Observed trajectory | `last` | `calls` |
|---|---|---|---|
| off (`0`) | `[1, 1, 1, 2, 3, 4, 5, 6]` | 6 | 7 |
| on (`1`, also the default) | `[1, 1, 1, 1, 1, 1, 1, 1]` | 1 | 1 |

Severity: confirmed semantic miscompilation. The guard's first false result is not
absorbing because its input state is mutated by the guard itself. Source seam:
`Optimize.fs:130` accepts the ordinary call. This is not a complaint about numerical
tolerance or the semantics of the explicit `while` form.

**Fixed (2026-09-05).** Reproduced on the pre-fix binary exactly as tabled, then
the recognizer was given the callee judgment described in section 3 step 1: a
guard may call only unshadowed scalar intrinsics, so `tick(counter)` declines and
the program compiles to the full-budget `if`/`else`. Post-fix, recognition on and
off both give `[1, 1, 1, 2, 3, 4, 5, 6]`, `last = 6`, `calls = 7`; this program is
`tests/corpus/recursive-arrays/020_freeze_idiom_effectful_guard_declined.blade`.

## Appendix B. Confirmed Poisson parameter collapse

```blade
import rand as r
let a = r.poisson(7, 1000.0, 4)
let b = r.poisson(7, 10000.0, 4)
```

Observed compiled output:

```text
a = [738, 757, 773, 783]
b = [738, 757, 773, 783]
```

`rand_runtime.hpp:153` computes `L = exp(-lam)` and multiplies uniforms until the
product reaches L. For both parameters L underflows to zero, so the algorithm loses
its dependence on the requested mean. The comment acknowledges termination at large
lambda, but termination does not make the resulting samples Poisson. The same
algorithm appears at `Interp/RandMirror.fs:151`; that mirror was inspected, not run
for this probe. A safe supported-domain refusal is a reasonable first repair while
a numerically suitable large-parameter sampler is planned.

**Fixed (2026-09-05), with a sampler rather than a refusal.** `next_poisson` now
splits at `kPoissonKnuthMaxLam = 10`: below it the product route runs unchanged,
at and above it Hormann's PTRS transformed rejection (numpy's constants and test
order, with a self-contained `loggam`) draws two uniforms per iteration. Both routes
and the split are mirrored in `RandMirror.fs`, and the PTRS functions carry a
no-contraction attribute so g++'s FMA licence cannot desynchronize the mirror at a
floor/compare knife edge. The split sits at Hormann's stated domain (mu >= 10) and
numpy's cost crossover: below 10 Knuth costs at most ~11 uniforms and no libm call,
above it PTRS costs ~2.2 uniforms whatever lambda is. It was first placed at 500 to
preserve every then-pinned draw; on 2026-09-06 the pins were re-evaluated instead
(no corpus pin used a lambda in [10, 500)). Post-fix: lambda = 1000 gives
`[1036, 965, 971, 1017]`, lambda = 10000 gives `[10114, 9891, 9908, 10053]`; both
moments agree with lambda at n = 4096, `lo`/`hi` pins straddle the split, and the
interpreter lane reproduces the draws bit for bit
(`tests/corpus/rand/018_poisson_large_lambda.blade`, `blade test interp rand`). NaN
and negative lambda keep their pre-existing behaviour.

Probe binary identification: `bin/Release/net10.0/Blade.dll` SHA256
`D2721D07A7B84371D9303B02F76DBD24EF3D27D5ED20CFF0F5F791665B36F71A`.
The binary was not rebuilt, so this identifies the tested artifact without claiming
a fresh source-to-binary verification. Three small `blade run` invocations total;
no benchmark campaign or full test suite was run. Both bugs remain unfixed by design
in this planning-only round.
