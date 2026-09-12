# Blade Feature Module: Equivariance and Equivariant Neural Networks

Status: **mixed, and the mix matters** — Part II (the equivariant ML
library) is substantially LANDED, well past "implementation upcoming"; Part
I's original core-language hook (§1-4) is SUPERSEDED by a later design
decision and is kept only for the historical record. Read the status note
just below before anything else in this doc. This module doc
remains the canonical home for (a) the **core-language equivariance hook**
(formerly formalism v10 §8 and §4.15.4 — moved out of the core formalism
because it is annotation-layer, not core semantics), and (b) the
**equivariant ML library** built on it. Detailed construct listings remain
in `blade_ml_spec_v10.md`; this doc states the semantics and the contract.

Blade has two orthogonal symmetry systems:

| System | Group kind | Affects | Cost |
|--------|-----------|---------|------|
| Index types (`SymIdx`, `AntisymIdx`, ...) | Discrete permutations (Sₙ) | Storage layout + iteration | Real speedups |
| Equivariance annotations (this module) | Continuous groups (SO(3), SE(3), O(3), ...) | Type checking only | Zero runtime cost |

They compose: a stress tensor is stored as `Array<Float like SymIdx<2,
IrrepsIdx<spec>>>` — triangular storage from the index type — and a function
producing or consuming it under `σ' = RσRᵀ` is checked against that claim by
a `where ml.equiv(SO3)` pin on its SIGNATURE, not a value-level annotation on
the tensor itself. (This updates the original design's
`with equiv(SO<3>, L2_even)` sketch below to the shipped spelling — see the
status note.)

---

## Status note (2026-07-28) — read this first

This doc predates most of what actually shipped. In place of Part I's §1-4
design (a value-level `with equiv(G, rep)` annotation checked per-expression
by the unifier, with `EquivIdx<n, G, ρ>` as the index-level carrier of the
same rep data), the shipped surface is:

- **Rep DATA lives in index types, not value-level annotations, and unifies
  like any other type.** `IrrepsIdx<spec>` (§6 below, landed) carries an
  O(3) irrep spec GROUP-LESSLY; `PgIrrepsIdx<GROUP, spec>` is the finite
  point-group sibling (`C4`, `D4` shipped as the first roster members). This
  much of §4's original thesis held up — it is the CLAIM that moved.
- **The CLAIM — "this function is equivariant" — is a deduced-then-pinned
  SIGNATURE ATTRIBUTE, checked by elaboration-seam walkers, not a
  per-expression unifier judgment.** `where ml.equiv(G)` (O(3), SO(3), and
  now point groups) and `where ml.galilean(u, ...)` (Galilean boosts) are
  the pin spellings, checked by `MLEquiv.fs` / `MLGalilean.fs` at the
  `MLElaborate` pass-1/pass-2 seam — the same lattice-plus-pins shape as
  `comm`/`anticomm`, not the full per-expression refinement Part I sketched.
  The precise framing is that a pin is a **POLYMORPHISM LICENSE**: `comm` on
  a kernel licenses viewing one function at two signatures (`Idx<M> ->
  Idx<N> -> T` and `SymIdx<2,M> -> SymIdx<2,N> -> T` are the same function;
  the pin permits the compact retyping), and `where ml.equiv(G)` licenses
  the analogous thing for representation-typed signatures. See
  the retired equivariance-in-types plan §0 for
  the refinement in full.
- **Deduction proposes what the checker would accept.** Uncertified
  functions get speculative `where ml.equiv(G)` / `where ml.galilean(...)`
  suggestions on the BL4011 / BL4014 warning channels, plus structured
  `deduced[]` entries in `ide check --json` (`kind` "equiv" | "galilean").
  Generator-based (Lie-algebra) discharge now backs hand-written bodies
  that composition-only checking used to reject; synthesized `ml.derive_*`
  functions carry their equivariance certificate BY CONSTRUCTION (Schur
  bases), so they satisfy a pin with no proof search at all.
- **Synthesis, not hand-derivation, is the primary user surface for §2's
  inference table.** `ml.derive_linear` (degree 1), the compacted
  `ml.derive_sym_tp` / `ml.derive_alt_tp` (bilinear self-products), and
  `ml.derive_poly<k>` (degree-k homogeneous polynomials, k ≤ 4, with k = 1
  degenerating exactly to `derive_linear`) each emit the complete Schur
  basis of the admissible hypothesis space as ordinary Blade source — the
  parameter count is a theorem, not a guess. `ml.derive_perm_linear` is the
  Sₙ (index-action, not representation-action) sibling; `ml.derive_pg_linear`
  is the point-group sibling.
- Two plan documents carried the authoritative detail this note summarizes.
  Both have since been retired; this note is now the surviving record. The
  transforms-as-types plan covered the synthesis/certification mechanism —
  landed through its stage 6c, generator-based deduction, as of 2026-07-27.
  The equivariance-in-types plan covered the follow-on round moving the
  *deduction* lattice itself from the elaboration seam to typecheck-time,
  without changing what a user writes; its phases A+B were in progress as of
  2026-07-28, phase B being `src/DeduceRep.fs`.

Read Part I §1-4 below as **archival**: it records the design that was
superseded, not the shipped surface. Part II (§5 onward) mixes landed
material (§5-§8, §11, §11b — each dated inline) with open items (§12); read
each section's own status markers, some of which this pass also updated.

---

## Part I — Core-language hook (annotation + inference framework)

> **SUPERSEDED (2026-07-28).** Everything in §1-4 below — the value-level
> `with equiv(G, rep)` annotation, checked per-expression by the unifier,
> with `EquivIdx<n, G, ρ>` as the index-level carrier of the same rep data —
> was never built, and a 2026-07-28 review decision explicitly closed the
> door on building it as written. Kept for the record; nothing here is
> current surface.
>
> **The adopted factoring, in one paragraph.** Rep DATA — which group acts
> on an axis, in which representation — belongs in the type and is unified
> and propagated by the type checker; this much §4 got right, and it
> shipped as `IrrepsIdx<spec>` / `PgIrrepsIdx<G,spec>`. Rep CLAIM — "this
> function is equivariant" — did NOT become a per-expression unifier
> constraint. It is a deduced-then-pinned SIGNATURE ATTRIBUTE, exactly the
> shape `comm`/`anticomm` already use: a lattice walker proposes it, a
> `where ml.equiv(G)` clause pins it. The sharper name for this, settled at
> review, is a **POLYMORPHISM LICENSE**: a pin licenses a TYPE
> TRANSFORMATION on one underlying function, the way `comm` on a kernel
> licenses viewing `... -> Idx<M> -> Idx<N> -> T` and
> `... -> SymIdx<2,M> -> SymIdx<2,N> -> T` as two signatures of the SAME
> function rather than two functions. Post-restriction (see
> the retired equivariance-in-types plan stage A3), an `ml.equiv(O3)` claim is meant
> to license viewing the function at restricted subgroup types the same
> way. So the claim lives in the signature — it gates call-site discharge
> and is part of the type in the broad sense — but it is not solved
> per-expression by the unifier the way `EquivIdx` propagation through
> `+`/`cross`/`⊗` would have required. Folding the claim into every
> expression's type, as §1-§4 do, would have entangled subsumption and
> variance with the solver and bloated every diagnostic; the
> lattice-plus-pins pattern had already shipped three times (rank,
> symmetry, arity) before equivariance became its fourth instance instead
> of a bespoke fifth mechanism.
>
> Full detail lived in the retired transforms-as-types plan §3.5 (the
> deduction lattice as the continuous twin of `Deduce.fs`) and the retired
> equivariance-in-types plan §0 (the polymorphism-license refinement,
> review-confirmed 2026-07-28).

The core language provides the annotation mechanism and inference framework
only; group-specific rules (which representations exist, what `cross` returns)
live in domain libraries. This is the boundary that keeps the core group-theory-free.

### 1. Annotations

```blade
let v: Array<Float like Idx<3>> with equiv(G, rep)
let energy: Float with invariant(G)          // sugar for equiv(G, trivial)

function norm(v: T^1 with equiv(G, rep)) -> T^0 with invariant(G)
function scale(s: T^0 with invariant(G), v: T^1 with equiv(G, rep)) -> T^1
// output annotation omitted → inferred: equiv(G, rep)
```

Unannotated values are non-equivariant: freely mixable with each other, not
passable to equivariant parameters. Adoption is gradual — if inference fails,
the result is non-equivariant, not an error.

### 2. Inference rules

| Operation | Inputs | Output |
|-----------|--------|--------|
| `a + b`, `a - b` | same rep ρ | ρ |
| `s * v`, `v / s` | invariant, ρ | ρ |
| `dot(a, b)` | ρ, ρ | invariant |
| `norm(v)` | ρ | invariant |
| `cross(a, b)` | ρ, ρ | domain-library rule (pseudovector for O(3) vectors) |
| `a ⊗ b` | ρ₁, ρ₂ | ρ₁ ⊗ ρ₂ (CG decomposition) |
| `sum`/`mean` over an index | ρ | ρ (rank reduced) |
| `min`/`max` | invariant only | invariant (ordering requires invariance) |

Inference flows through expressions; explicit annotations are checked against
the inferred representation.

### 3. Errors detected at compile time

1. **Representation mismatch**: `vector + pseudovector` under O(3).
2. **Equivariance breaking**: component extraction `v(0)` from an equivariant
   array (hint: use `norm`, `dot` with a reference direction).
3. **Wrong output declaration**: declaring `cross : vector` when inference gives
   pseudovector.
4. **Index/equivariance incompatibility**: `AntisymIdx` storage with a
   symmetric representation (`L2_even`), and vice versa.
5. **Missing annotation**: passing plain data to an equivariant parameter.

### 4. Equivariant index types

```blade
type EquivIdx<n, G, ρ>                       // dimension, group, representation
type VectorIdx       = EquivIdx<3, SO<3>, standard>
type PseudovectorIdx = EquivIdx<3, SO<3>, adjoint>
type ScalarIdx       = EquivIdx<1, SO<3>, trivial>
```

`EquivIdx` is the index-type-level carrier of representation data; the `with
equiv` annotation is the value-level carrier. Both feed the same checker.

---

## Part II — The equivariant ML library

Goal: type-safe, zero-overhead E(3)-equivariant networks. Positioning vs e3nn:

| Aspect | e3nn | Blade |
|--------|------|-------|
| Irreps spec | string `"16x0e + 8x1o"` | static spec array |
| Error detection | runtime (during training) | compile time |
| Tensor wrapping | `GeometricTensor` | native arrays with `IrrepsIdx` |
| CG iteration | dense matrices | sparse index types |

### 5. Irreps

```blade
type Parity = Even | Odd
type Irrep<L: Nat, p: Parity>                 // L0e, L1o, L2e, ... named aliases
static function dim(ir)    = 2 * L + 1
static spec = [(L0e, 16), (L1o, 8), (L2e, 4)] // 16 scalars, 8 vectors, 4 rank-2
                                              // total dim 16·1 + 8·3 + 4·5 = 60
```

Everything downstream is parameterized by static spec arrays — this is the
`static function` / `let static` machinery of the core language doing
representation theory at compile time.

### 6. `IrrepsIdx<spec>` — the block-structured index

```blade
type IrrepsIdx<spec> = DepIdx<
    Idx<length(spec)>,                                    // block
    lambda(b) -> Idx<mult(spec(b))>, Idx<dim(irrep(spec(b)))>  // (mult, m)
>
```

The DepIdx equation is the SEMANTIC reading: iteration yields `(block,
multiplicity, m-component)` triples; extent is `total_dim(spec)`. Coiteration
with edges/nodes requires named index types (`type EdgeIdx = Idx<E>`) per the
core structural identity rules.

**v7 implementation (landed 2026-07-14)**: a flat-dense PRIMITIVE index type,
not a DepIdx lowering — every cell is stored (extent = cardinality =
`total_dim(spec)`, no compression), and the block structure is carried as
nominal identity, so all dense codegen paths apply unchanged. The identity
rules:

- Two `IrrepsIdx` annotations with DIFFERENT specs never unify, even at equal
  `total_dim` (the whole point — `[(0,0,2),(1,1,2),(2,0,1)]` vs `[(0,0,13)]`
  are both 13 cells and still distinct). Enforced in unification AND at
  direct function application.
- Aliases are NOMINATIVE: `type Feat = IrrepsIdx<s>` and
  `type Hidden = IrrepsIdx<s>` are distinct types; anonymous
  `IrrepsIdx<s>` unifies with either (named-vs-anonymous permissiveness).
- An `IrrepsIdx` array unifies with a plain `Idx<total_dim>` annotation or
  parameter (gradual adoption; generated op signatures interoperate).
- The spec argument resolves under the full static contract: a `let static`
  name or an inline literal `IrrepsIdx<[(0, 0, 2), (1, 1, 2)]>`. Call syntax
  in the angle brackets (`IrrepsIdx<sh_spec(2)>`) is not in the
  simple-expression grammar — bind a `let static` first.
- `range<IrrepsIdx<spec>>` is the flat dense range (bound = `total_dim`).

Block navigation is a static-builtin surface (`irreps_len(spec)`,
`irreps_l/parity/mult/dim(spec, b)`, `irreps_offset(spec, b)`), so
block-structured loop nests fold at compile time:
`x(irreps_offset(spec, b) + mu * irreps_dim(spec, b) + m)`. The
`for (b, mu, m) in axis` sugar is deferred until DepIdx iteration codegen
lands. Corpus: `index-types/111–119`, `ml-ops/005–008`.

### 7. Clebsch-Gordan machinery

Dependent + constrained records (core features) encode the selection rules in
types:

```blade
struct CGPath {
    l1, l2: Nat<angular_momentum>,
    l_out:  Nat<angular_momentum, min=abs(l1-l2), max=l1+l2>,
    p1, p2, p_out: Parity
} where p_out == parity_mul(p1, p2)

struct CGIndex<path: CGPath> {
    m1:    Int<min=-path.l1,    max=path.l1>,
    m2:    Int<min=-path.l2,    max=path.l2>,
    m_out: Int<min=-path.l_out, max=path.l_out>
} where m1 + m2 == m_out
```

`cg<path>(idx)` is a static function; the compiler generates CG tables at
compile time for all paths actually used. Iteration over `CGIndex` visits only
the sparse nonzero support (`m1 + m2 = m_out`), never a dense (2l1+1)(2l2+1)(2l_out+1) box.

### 8. Operations

| Operation | Structure | Key property |
|-----------|-----------|--------------|
| `tensor_product<cfg>(in1, in2, weights)` | loop over `TensorPaths<cfg>` (SparseIdx of valid paths) → multiplicities → `CGIndex` | output irreps must be reachable (`all_valid_outputs`) |
| `SphericalHarmonics.Y<L>(v)` / `Y_to<L_max>(v)` | `Idx<3>` → `IrrepsIdx<sh_spec>` | the only L-raising primitive; explicit low-L polynomials, recurrence above |
| `linear<spec_in, spec_out>(input, weights)` | per-block matrix multiply over multiplicities, shared across m | mixes multiplicities within an irrep only (`all_irreps_present`); cross-irrep mixing would break equivariance |
| `gated_activation(features)` | scalars: `silu` directly; higher L: sigmoid-gated by scalar block | nonlinearities on L>0 components directly would break equivariance |
| `norm_activation(features)` | higher L scaled by `silu(‖v‖)/‖v‖` | norm is invariant, scaling is safe |
| `scatter_add(values, targets, n)` | edges → nodes accumulation | many-to-one message aggregation |
| `gather(features, sources)` | nodes → edges collection | one-to-one |

Weight shapes are `DepIdx` types over paths/blocks (`WeightIdx<cfg>`,
`LinearWeightIdx`), so a wrong-shaped weight array is a type error, not a
training-time surprise.

### 9. Reynolds interactions (why this module wants the core symmetry system)

- **Symmetric message passing**: undirected edges via `reynolds(interaction)` —
  symmetric output from an asymmetric kernel, 2× triangular savings, 4× with
  identical arrays.
- **CG exchange symmetry**: for L1 = L2, `cg[m1,m2,m] = ±cg[m2,m1,m]` compacts
  self-tensor-product weight spaces. Correction (2026-07-26): "antisymmetric
  paths vanish" holds only at multiplicity 1 — at mult > 1 the m-antisymmetric
  coupling pairs with the antisymmetric multiplicity component (the Λ²⊗Λ²
  Cauchy term) and survives; measured compaction is 30–42%, not 2–4×. Exact
  rule and worked counts: retired transforms-as-types plan §3.2 (the counts
  themselves are re-verified in `proofs/BladeSymPower.v`).
- **Higher-order interactions**: n-body kernels under `reynolds` get n!
  (triangular) × n! (identity collapse) — 36× at n=3, 576× at n=4.
- **Antisymmetric Reynolds**: determinant-like alternating sums;
  diagonal terms vanish by construction.

Note (post-Coq correction): these factorial counts are per identity group over
the compound iteration space, per the corrected product-symmetry doctrine
(formalism §12, proofs.md). The n-body speedup table is the r! joint speedup —
it does not claim per-dimension factorization.

### 10. Worked example

The equivariant convolution (`blade_ml_spec_v10.md` §12): gather source
features per edge, `Y_to<2>` on edge vectors, `tensor_product` into messages,
scatter-add to targets. Composes items 5–8 plus core loop objects; also the
canonical consumer of [graphs-trees.md](graphs-trees.md) trace indices once
those land.

### 11. AD posture

**Reverse-mode `grad` is implemented (v7, v1 subset)** as an AST-level
source-to-source transform (`Grad.fs`, pre-typecheck): the synthesized
derivative is ordinary Blade source that flows through the standard
typechecker/lowering/codegen — so gradients of symmetric computations will
inherit triangular storage from the existing symmetry system rather than
from AD-specific logic.

**ABI.** `grad(f)` (call-shaped special form; `f` a same-module top-level
function returning `Float`) rewrites to `f__grad`, whose signature is f's
parameters followed by **one `mut` out-buffer per Float-array parameter**
(same type; ACCUMULATED into — callers zero them, PyTorch-style), returning
the primal, or `(primal, dscalar…)` when f has Float scalar parameters.
Int/int-array parameters (edge lists, sizes) are non-differentiable. Data
enters by module-scope capture, so a loss function's parameters are exactly
its trainables.

**v1 subset** (clean errors outside it): lets, additive accumulation
(`+=`/`-=`, scalar and array-element), element construction writes, additive
`reduce(..., (+))` folds and rank-1 additive recursive arrays, scalar
arithmetic and the math intrinsics, array reads at data-dependent indices
(gather; adjoint is scatter), and calls to other AD-able functions (inlined). Adjoint loops run in the same direction
— exact for the accumulation subset; the discipline that makes it exact is
enforced (no non-additive scalar overwrites, no reads of loop-outliving
accumulators mid-loop, no array recurrences, no read-then-later-write).
Loop bodies are replayed inside the adjoint loop (recompute-based; no tape).

**Verification** follows the module's differential-oracle stance: hand VJPs
+ finite differences + gradient-rotation-invariance in `ml/`
(`Autodiff.fs`, `Tests_Autodiff.fs`), value-pinned corpus tests (`ad/`),
and the end-to-end training example (`ml-e2e/001`) whose loss trajectory,
gradient snapshots, and final weights reproduce `ml/TrainingOracle.fs` to
printed precision — including loss AND gradients invariant under rotated
inputs.

**Forward mode is implemented (`ad.jvp`)**, same pass and pipeline slot as
`grad`: `ad.jvp(f)` rewrites to `f__jvp` — original params, then one
tangent input per differentiable param (`__t_<p>`, typed with the primal
annotation verbatim, units included), returning a `(primal, tangent)`
tuple. No mut buffers; wrt-selection is a zero seed (which retires the
wrt-lists item for forward mode). The jvp subset is a STRICT SUPERSET of
grad's: forward tangents flow in program order, so non-additive
overwrites, general immediate-predecessor recurrences (O(n), no
triangular unroll), product folds, if/else (branch of tangents under the
same condition, subgradient at the boundary), and unit-carrying
params/returns all differentiate — the reverse-only refusals were
reverse-sweep artifacts. The transforms compose in one expression:
`ad.jvp(ad.grad(f))` is the blessed HVP route (tangent buffers pair the
gradient buffers; corpus pins the Hessian columns, the symmetry residual,
and FD-of-gradient), and `ad.jvp(ad.jvp(f))` is the second directional
derivative. Rejects carry BL5501 (grad's stay BL5500). Corpus `ad-jvp/`
(in the default suite and the interp slice); in-program jvp-vs-grad
residual pins are the differential gate. Equivariance posture mirrors
grad's: tangents are not re-judged (equivariance of the directional
derivative is a theorem given the primal).

Remaining: combinator rules (`<@>`/`>>@`/…) for FORWARD mode (reverse mode
lowers eager rank-0 maps as of 2026-08-26 — `GradNormalize.expandEagerMap`,
with the surviving reverse refusals enumerated in
`docs/plans/plan-equivariant-nn-notebooks.md` §5/census row 15; the C-track;
`proofs/BladeJacobian.v`'s `tangent_joint_swap` already licenses
joint-pair symmetric tangent storage for when kernels land),
triangular-tape exploitation for symmetric intermediates, wrt-lists for
reverse mode, if/match in REVERSE-differentiated code, taping for
nonlinear loop recurrences in reverse mode, stencil/decomposition
interactions, framework bindings.

### 11b. v7 implementation status (ops elaboration)

The ops landed in v7 (2026-07-12) as **compile-time elaboration to Blade
source** (`MLSpec.fs` + `MLElaborate.fs` + `WignerTables.fs`; user
decision over opaque builtins): for each op × static config used, the
compiler synthesizes an ordinary Blade function with real-basis CG tables
baked as constants, so `grad()` differentiates through the generated ops
via its normal inliner and codegen is unchanged.

Surface (v1 — ordinary required-static arguments; angle-bracket static
args are future sugar):

```blade
let static spec_h = [(0, 0, 2), (1, 1, 2), (2, 0, 1)]   // (l, parity, mult), parity 0=e/1=o
let static cfg1  = (spec_in, sh_spec(2), spec_h)         // (spec1, spec2, specOut)
let static w1dim = tp_weight_dim(cfg1)                   // sizing builtins:
let static w2dim = linear_weight_dim(spec_h, spec_h)     // total_dim, sh_spec, ...

let sh  = y_to(2, x, y, z)                    // real solid harmonics, lmax <= 2 (v1)
let out = tensor_product(cfg1, x1, sh, w)     // uvw fully-connected, path-validated
let z   = linear(spec_in, spec_out, w, x)     // block-diagonal, first-match blocks
let g   = gated(spec, x)                      // scalar double-duty gates (F2 rule)

// batched row forms over flat row-major storage (N static): the per-node
// case of graph networks, with no hand-written row-extract/write-back loops
let g1 = gated_rows(spec, N, x_rows)
let h1 = linear_rows(spec_in, spec_out, N, w, x_rows)
```

Checks at elaboration: `all_valid_outputs` for tensor products, block-0
scalars for `gated`, static-ness of configs — all clean compile errors.
Value pins: corpus `ml-ops/` (op-level) and `ml-e2e/002`, which re-runs the
entire §10 training example through elaborated ops and reproduces the
`ml/TrainingOracle` pins exactly (same loop order and product association
as the reference — agreement to the ulp).

**CGIndex basis decision (F1 resolution, user-guided)**: the complex-basis
rule `m1 + m2 == m_out` and the real-basis support are DIFFERENT
constraints, so they get different types. `CGIndex` = the real-basis
sparse support (what the spec's own real harmonics and `tensor_product`
need; iteration = the compiler's real-CG nonzero entries).
`CGIndexComplex` = the m-selection rule, reserved for complex-basis
pipelines. The §7 struct sketch's `where m1 + m2 == m_out` describes
`CGIndexComplex`, not the type the ops consume.

**IrrepsIdx landed (2026-07-14)**: `IrrepsIdx<spec>` is a primitive index
type (§6) and the elaborated ops now STAMP it on their generated
signatures — feature params and results of `y_to` / `tensor_product` /
`linear` / `gated` carry the anonymous irreps type of their spec (weight
buffers stay `Idx<wdim>`: path-major weight spaces are not irreps spaces;
likewise the row-stacked `_rows` buffers, whose extent is
`nRows * total_dim`). Unannotated call sites are unaffected (irreps vs
plain unifies); an annotation or argument with the WRONG spec is a type
error. Corpus: `ml-ops/005` (accept + values), `ml-ops/006` (reject).

Not yet in v7: dependent records for user-defined `CGPath` (formalism v10
§17.13.1), `y_to` above lmax 2, angle-bracket static args, per-edge fused
convolution elaboration, `for (b, mu, m)` structured iteration over an
irreps axis (waits on DepIdx iteration codegen).

### 12. Open items

From ml-spec §13 plus module-level gaps:

1. Path filtering (skip zero-weight paths at compile time)
2. Equivariant attention
3. GPU fused-kernel codegen for tensor products
4. Sparse tensor products (compile-time path pruning from weight structure)
5. Memory layout choice: block-contiguous vs m-contiguous per operation
6. ~~`poly(...)` × equivariance (arity-polymorphic equivariant kernels)~~ —
   **CLOSED for the multilinear fragment** (retired transforms-as-types
   plan §3.4): `poly(...) + comm + identity group over IrrepsIdx-typed arrays =
   derive_poly<k>` — the arity-polymorphic surface and the symmetric power
   are the same object seen from the value side and the type side. No claim
   is made for a non-multilinear comm kernel (e.g. `max(a,b)`).
7. User-defined representations beyond built-in L0..Ln
8. Automatic CG path enumeration
