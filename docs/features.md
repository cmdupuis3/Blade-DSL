# Blade Features Catalog

Exhaustive list of Blade language features. This document is the census; semantics live
in [formalism.md](formalism.md) and the per-module feature docs.

---

## 1. Scalar values and primitive types

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Base numeric types | `Int32/Int64/Float32/Float64/`<br>`Complex64/Complex128` | Core | Double-check for exhaustiveness |
| Explicit numeric casts | `Float32(x)`, `Float64(extents(a))`,<br>`Int64(floor(x))`, `Complex128(r)` | Core | Scalar type name in call position; shadowable plain-call intrinsic. Complex→real refuses (BL3019: project with `real`/`imag`/`abs`/`arg`); float→int only through a floor/ceil visible at the cast site. Arrays lift elementwise. Implicit mixed-type promotion of a non-literal operand warns BL3020 |
| Fused multiply-add | `fma(a, b, c)` | Core | `a*b + c` rounded ONCE; Float64 scalars; shadowable plain-call intrinsic. Its own IR node, so it is bit-identical across the compiled, interpreted and LLVM lanes under any `BLADE_FP_CONTRACT` (unlike `a*b + c`). The building block of error-free transformations / double-double arithmetic. AD: partials b, a, 1 |
| Type variables | `A -> B -> ...` | Core | Same letter = same type in a signature |
| Complex conjugates | `conj(x)` | Core |  |
| Units of measure | `Unit meters`, `Float<velocity>`,<br> unit arithmetic | Core | Annotations on primitive types only |
| Unit-carrying type variables | `T<time>^r`, `T<time>` | Core | The caret marks the head as a VARIABLE rather than a named type; `^0` is optional, so `T<time>` and `T<time>^0` are the same type (one lowering, so units, unification, monomorphization and diagnostics cannot drift between them). A misspelled unit is BL3015 in either spelling. A head naming a real type keeps its ordinary meaning |
| Bounded primitives | `Float<min=0, max=1>` | Planned | Runtime-checked bounds |
| Mutually constrained types | `type V1 ... and V2 ...`<br>`where <constraint>` | Core | Joint assignment required |
| Boolean Operators |  `&&`/`\|\|`/`!` | Core | |

## 2. Bindings, mutability, staticness

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Let-binding | `let static`/`let`/`let mut` | Core | `static` denotes "statically evaluable" and is also the immutable tier — there is no separate `const` keyword |
| Parameter borrowing | `x: T` immutable, `x: mut T` mutable | Core |  |
| `static` literals | `let static a = 5` | Core |  |
| `static function` |  | Core | compile-time evaluable; usable in type positions (`Idx<triangle(n)>`) |
| `static` parameters | `f(N : static Nat)`  | Planned | usable in return types |
| Static type functions | `static type Vec<N> = ...` | Planned | type-returning vs value-returning split keeps the type system decidable |
| Fused assignment | `+=`/`-=`/`*=`/`/=` | Core | Mutable variables only |

## 3. Arrays and array types

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Arrays as functions | `A(i, j) ≡ A(i)(j)` | Core | `Array<T, I, J> ≅ I → J → T` |
| Three-phase type model | abstract `T^r(σ)` =><br> index-typed `T^(I₁,...)` =><br> concrete `Array<V like I₁,...>` | Core |  |
| Array literals | `[1, 2, 3]` | Core | nested for rank ≥ 2 |
| Ragged literals | `[[1, 2, 3], [4, 5]]` | Core | Array literal rows of uneven length build `RaggedIdx`-typed arrays |
| Dimensional currying | `let A: T^3 = ...;`<br>`let B: T^2 = A(i)` | Core | Partial indexing yields lower rank; cache-optimality by construction |
| Poly-indexing | `A(indices)` with tuple;<br> `all_indices(A)` iteration | Core | Index multiple dimensions at once with a tuple of indices |
| Computational (lambda) indices | `Dual`, `Symbolic`, thunks | Speculative | v10 §5.5; structural vs computational index separation |
| Arrays of functions | `models(lat, lon)(params)(t)` | Core | Free mixing of functions and indexing |
| Extent tuples | `extents(A)` | Core | Static-first evaluation; rejects ragged/grouped dims with guidance |
| Mutation of array elements | `A(i) = v`, `A(i,j) += v` | Core | Allowed for mutable arrays, but unidiomatic |

## 4. Array combinators (ArrayExpr layer)

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Array type lifting | `pure` | Core | Lifts an `Array` to an `ArrayExpr`. Usually implicit, but available explicitly. |
| Hard transpose | `transpose` | Core | Symmetric identity and antisym negation. |
| Zip | `zip(A, B, C)` (n-ary, tuple elements, symmetry intersection) | Core | Turns a tuple of arrays into an array of tuples. Checks symmetry intersection. |
| Outermost stack | `stack(A, B, C)` (new leftmost dimension, fresh symmetry class) | Core | Stack arrays along a new leftmost dimension. |
| Concatenation | `join(A, B, ..., d)` | Core | Concatenate arrays along dimension `d` |
| Segmentation | `split(A, d, i)` | Speculative | `A, B == split(join(A,B,d), d, i)` |
| Array fallback | `<\|:>` (nullptr-safe sparse access) | Planned | Partial-depth allocation in C++ codegen for non-final dims |
| Dimension decoupling | `decompact(A, n: Nat)` | Core | Expand symmetric/antisymmetric compact storage to dense along an axis |
|  | `diag`, `subset`, `split`, `reverse` (array op), `shift` | Speculative | Superseded by virtual arrays? |
|  | `align` / `stencil` (sugar) with `StencilSpec`, boundary modes | Speculative | Superseded by virtual arrays? |


## 5. Index types

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Simple index type | `Idx<n: Nat>` | Core | Index type of length `n` |
| Enumerated index type | `EnumIdx<S: Enum>` | Core | enumerated categories; string/sparse key domains. Also drives `group_keys` case 2 |
| Bounded index type | `BoundedIdx<l: Nat, u: Nat>` | Core | Internal use only. Erases to runtime bounds |
| Symmetric index type | `SymIdx<r:Nat, n:Nat>` | Core | `r` mutually symmetric dimensions of length `n` |
| Antisymmetric index type | `AntisymIdx<r, n>` | Core | `r` sign-tracked mutually antisymmetric dimensions |
| Hermitian index type | `HermitianIdx<n>` | Core | 2-D Hermitian index type. `A(i,j) = conj(A(j,i))` |
| Compound index type | `CompoundIdx<mask: bool^r>` | Core | `r`-dimensional sorted (lex-enumerated) semi-dense index type ideal for relatively dense grids; inherits dimensions from the `mask` array. |
| Ragged index type | `RaggedIdx<lengths>` | Core |  |
| Dependent index type | `DepIdx<I, f: Nat -> Idx<N>>` | Core |  Static function `f` maps each index of `I` to a new `Idx` |
| Equivariant index type | `EquivIdx<n, G, ρ>` | Planned? | group-representation-annotated indices |
| Sparse index type | `SparseIdx<keys>` | Core | explicit valid-key enumeration (rank-1 array of Nat tuples; `static` list or runtime tuple-array) with hash-table lookup. Tuple indexing with wildcards for partial gathers. Built with  `sparse(values, keys)` |
| Orbit (iterated-wreath) index type | `OrbIdx<[(r₁,s₁), ..., (r_d,s_d)], n>` | Partial | flat list of `(rank, ±)` levels, OUTERMOST-LAST, over one extent; group `S_{r₁} ≀ ... ≀ S_{r_d}` on `∏rᵢ` raw axes, character the product of the level signs. **Depth ≤ 1 is fully supported and is not a new type**: `[]` normalizes to `Idx<n>`, `[(r,+)]` to `SymIdx<r,n>`, `[(r,-)]` to `AntisymIdx<r,n>` — the same records, so the same storage, iteration and printing. Rank-1 levels drop at either sign. **Depth ≥ 2 is DEDUCED-ONLY**: a `comm` tie over a repeated compact argument produces the class — gated for soundness when the inner class carries a `-` level, where the tie additionally requires the kernel provably sign-odd in each argument (`h(-p,q) = -h(p,q)`, e.g. `p * q`; refused with BL4015 otherwise, `p + q` included) — and such a value is allocated (closed-form iterated binomial), filled by the segment-peeled `orb_visit` nest, printed, subscripted at any raw tuple `W(i,j,k,l)` (flat coordinates; a mirrored tuple returns the signed cell, a zero-set tuple returns 0), fully decompacted with `decompact(W, 0)`, and round-tripped through a Zarr store (the spec_version 2 `"orbit"` head over the flat canonical pool — [providers/ZarrTriangularSpec.md](../src/providers/ZarrTriangularSpec.md); depth-1 classes keep their `sym`/`antisym` spelling on disk). Still refused with BL4003: WRITING the class down as an annotation (a Zarr store is now a producer, but the annotation also admits classes nothing produces), `reduce`/`prodsum` over the pool (decompact first), partial subscripts, partial (per-level) decompaction, `transpose`, provider I/O outside Zarr. See [plan-orbit-index-types.md](plan-orbit-index-types.md) and [plan-orbidx-decompaction.md](plan-orbidx-decompaction.md) |
| Nested/mixed symmetry | `NestedSymIdx` (elasticity),<br> `RiemannIdx` (curvature) | Speculative | Cardinality formulas specified. `RiemannIdx` is shorthand for `OrbIdx<[(2,-), (2,+)], n>` |

### 5a. Index type features

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Named index types | `type LatIdx = Idx<360>` | Core | Named index types are required for comparison. Unnamed index types are always considered distinct. |
| Index type tags | `Idx<n: Nat, Tag: String\|Enum>` | Core | String or enum-valued. Tags are for type comparison only. (Maybe redundant?) |
| Index type composition | `Sym<I,I>`, `Antisym<I,I>` | Planned |  |
| Index transforms | `flip`, `rename`, `subset`, `align` | Speculative | All explicit, no implicit conversions. Superseded by virtual arrays? |


## 6. Virtual Arrays

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Range | `range<Idx<n>>` | Core | Emit raw index as an array argument |
| Reverse | `reverse<Idx<n>>` | Core | Emit indices in reverse order |
| Halo | `halo<Idx<n>, [-1, 0, 1]>` | Core | Apply a rolling window centered at `0` |

## 7. Functions and kernels

| Feature | Status | Notes / sources |
|---------|--------|-----------------|
| Functions | Core |  |
| Commutativity groups → commutativity vector | Core |  |
| Lambdas (`lambda(a, b) -> ...`) | Core | Anonymous functions |
| Sectioned operators `(+)`, `(/) x`; single-wildcard partial application `f(_, y)` | Speculative | Multiple wildcards rejected by design |
| Reynolds operators — `reynolds(g)`, `reynolds(g, Antisymmetric)`, partial positions | Core | The surface combinator is the VALUE-LEVEL wrapper (permutes kernel arguments; H = Sₙ by construction) — output symmetry still follows H ∩ Stab, so identity is required (dense output for distinct arrays, pinned by reynolds/013). The proof tower's per-dimension INDEX-LEVEL Reynolds (`reynolds_full_product_symmetry`, lossless canonical access) is a distinct prospective operator, not currently a surface construct |
| `gram` — Gram-matrix construction (dense / symmetric / Hermitian) | Core | `corpus/index-types` 066–069; differential oracle in `tests/Oracles.fs` (Gram-Hermitian was an oracle lesson); not in v10 |
| `hermitian` — adjoint operator | Core | `corpus/index-types` 070; not in v10 |
| `zero` kernel and zero-arity base cases (`f(())` = identity element) | Core | Monadic zero. |
| Arithmetic symmetry annotations (`(+)` Symmetric, `(-)` Antisymmetric, ...) driving comm inference | Core | v10 §7.1.2 |
| Elementwise vs bracketed operators: `+` vs `[+]` | Core | Elementwise ops (e.g. `+` or `(+)`) provide sugar for `A op B = method_for(zip(A, B)) <@> op`, as opposed to bracketed ops, which use Blade-native outer-product spaces: `A [op] B = method_for(A, B) <@> op`. |
| Geometric primitives (`norm`, `dot`, `cross`) with equivariance signatures | Speculative |  |

## 8. Loop objects and iteration

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Loop combinators | `method_for(A₁...Aₙ)` <br> `object_for(f)` | Core | Builds a loop nest of deferred depth and type |
| S-dimensions |  | Core | S-dimensions derived from rank gap at kernel call. Rank gap = arg rank - parameter rank |
| T-dimensions |  | Core | Derived from kernel output dimensions |
| Virtual arrays | `range<I>`, `reverse<I>`, etc. | Core | Index type maps to yield or reorder indices. Behaves as an array with no content. |
| Anonymous ranges | `m..n` | Core | Half-open, equivalent to `range<Idx<n-m>> + m`; a first-class rank-1 array value: lifts elementwise (`x0 + dx * Float64(0..n)`), folds (`reduce(0..n, (+))`), materializes when bound bare or `\|> compute`d |
| Multi-dimensional for-loops | `for (A, B) <@> ...` | Core | Shorthand for `object_for` and `method_for`; allows co-iterations with `in` |
| Co-iteration | `for (A, B) in range<I> <@> ...` | Core | Iterate elementwise over a shared index space. |


## 9. Polymorphism

| Feature | Usage | Status | Description / Notes |
|---------|-------|--------|---------------------|
| Rank polymorphism | `let mean1 = mean(A: T^2)`<br>`let mean2 = mean(B: T^3)` | Core | All kernels are implicitly rank-polymorphic according to S/T dimension classification |
| Arity polymorphism | `function moment(A: Poly<T^k>)` | Core | Stricter than variadic functions, arity-polymorphism guarantees well-typed functions returning different types depending on arity |
| Rank | `rank(A: T^r)` | Core | Static function; integer rank of array |
| Arity | `arity(A: Poly<T^k>)` | Core | Static function; integer arity of poly-pack |
| Poly-pack destructuring | `let (head, tail) = args` | `args[k]`, `nth`; nested tuples; identity groups (neighboring identical arrays only) | Core | |
| Arg pack indexing | `args[k]` | Core | The `k`th element of poly-pack `args`. Also valid for general tuple arg packs. |
|  | `nth()` | Core | Vestigial? |

## 10. Combinator algebra

Laws are stated in the formalism. The MonadPlus laws hold exactly as stated — left zero, both identities,
left distribution (plus right zero), and right distribution provably fails for
the computation monad.

| Combinator |  | Role |
|-----------|--|------|
| Apply | `<@>` | Apply kernel to loop / arrays to object-loop |
| Pipe | `\|>` | Apply the preceding arg as the last argument of the subsequent arg; `f(a) == a \|> f` |
| Monadic | `>>=`, `pure`, `<$>` | Monadic bind, pure, functor. Computation monad (bind = loop-nest flat_map at the value level). `<$>` is FUSED into the producing kernel before typecheck (see Compose-apply); `>>=` is not |
| Loop join |`<&>` | Parallel composition with automatic prefix fusion |
| Force join | `<&!>` | Mandatory fusion; same-MethodLoop restriction |
| Reduction join | `object_for(<&!>) <@> (r₁, …, r_k)`<br>`reduce([r₁, …, r_k], (<&!>))` | k REDUCTIONS (`prodsum`, `reduce`) in one traversal → `Tuple<k>`; per-leg fold and seed, so the legs may differ in both. Legs naming the same **deferred** map (`let ct = cos <@> ph`, no `compute`) evaluate it once per cell — sharing declared by the NAME. 1 leg = identity, 0 refused. `docs/plan-reduction-joins.md` |
| Product | `<*>` | Array product = MethodLoop concatenation; identity `method_for()` |
| Compose-apply | `>>@` | ObjectLoop (kernel) composition. **Pipelines are FUSED** — `>>@`, `@>>` and `<$>` collapse into one map over the composed kernel before typecheck (`Grad.fuseProgram`), which is the proved duality below, so chains of any length, multi-operand first stages, and all three spellings inside a function body all emit. A fused kernel carries the first stage's `where` clause with its parameter references renamed, so `comm`-licensed triangular storage and `omp` licences survive. Stages fusion cannot prove (`reynolds(...)`, block bodies, unresolvable operands, a second-stage `where`) fall through to the staged compose path unchanged; in DIFFERENTIATED code the same declines become refusals |
| Apply-compose | `@>>` | Within-MethodLoop sequential composition <br>Compose-apply duality `(o_f >>@ o_g) <@> A ≡ (m <@> f) @>> (m <@> g)` |
| Guard | `guard(p, c)` | Conditional computation; false → zeros of c's shape |
| Choice | `<\|>` | Choice; MonadPlus with `zero` |
| Collections | `sequence`, `replicate` |  |
| Compute | `\|> compute` | Materialization |

### 10a. Combinator Idioms

Some higher-order combinators are particularly useful for applying complex function iteratively.

| Idiom |  | Description |
|-----------|--|------|
| List compose | `object_for(>>)` | Compose each function sequentially |
| List apply | `object_for(<@>)` | Apply an array of functions to an array of arguments |
| Join all | `object_for(<&>)` | Loop-join an array of computations as much as possible |
| First choice | `object_for(<\|>)` | Select the first `true` computation |

## 11. Relational (SQL-like) operations

Full semantics in [features/sql.md](features/sql.md). All implemented and tested
(`corpus/sql-*`, 81 tests across 12 categories).

| Operation | SQL analogue | One-liner |
|-----------|--------------|-----------|
| `mask(A, pred)` | WHERE predicate | Bool presence array over A's own index space; combine with `&&`/`\|\|` |
| `compound(A, m)` | WHERE materialization | Compact CompoundIdx view; coordinate-based reads; cardinality = pass count |
| `intersect(A, B)` | INTERSECT | Value-based, dedups, first-occurrence order from A |
| `union(A, B)` | UNION | Dedups both sides, A's occurrences first |
| `unique(A)` | DISTINCT | First-occurrence dedup |
| `contains(A, x)` | IN / EXISTS | Membership; linear scan; safe on empty compounds |
| `compound(A, mask(A, x -> contains(B, x)))` | Semijoin | Idiom, multiplicity-preserving; hash fusion planned, not implemented |
| `... !contains(B, x)` | Antijoin | Idiom |
| `group_keys(k₁, k₂, ...)` | GROUP BY keys | CSR grouping structure; static (Idx / EnumIdx) and dynamic dispatch |
| `group_by(values, gk)` | GROUP BY | Rank-2 ragged result; per-group kernels/reduces; elementwise map rejected by design |
| `Chunked<I, K>` / `Chunked<s.index.d, store>` / `Chunked<s1.index.d, [[s1, store], [s2, 2]]>` | segmented axis | The alias IS the axis; a regular grid, a store-inherited grid (zarr), or stores tiling the dimension with their own grids. Coordinates never consulted. docs/plans/structural/07, sql.md 7c |
| `segments(A)` / `files(A)` / `segments(C0, C1)` / `segments(a)` | structural GROUP BY | Groupings with no key array and no permutation; `files` is the labelled file level of a tiled axis; the two-alias form is the tile grouping of a rank-2 array in slot order; over a value, the tiling its annotation declared. Statically evaluable for literal grids |
| `.stream` on a `Chunked` rank-1 axis | out-of-core consumers | `reduce` folds block by block in storage order (bitwise the flat fold), `group_by` under a structural grouping reads one run per group, a halo map runs one segment at a time with its ghost cells; each recorded for `blade plan` (`segment-streaming`). Zarr today |
| `ungroup(G[, A])` / `ungroup([r1, ..], A)` | inverse of a structural grouping | Rows written back over the axis; the row form names a multi-store variable over a tiled axis. `join` untouched |
| `sort(A, keyFn)` | ORDER BY | Stable, key-extractor (not comparator); dense result |
| `reduce(A[, kernel[, init]][, axes = n])` | Aggregates | Default `(+)`; folds RIGHT-TO-LEFT, the innermost `n` axes with `n = 1` by default (rank k in, rank k−n out; `axes = rank(A)` is the full fold to a scalar — named slot, since the 3rd positional argument is the seed; `n` must be an integer literal, 1 ≤ n ≤ rank). 3-arg init form seeds EACH folded group and defines the empty result (landed, arc 4) — without init, statically-empty rejected and dynamic extents guarded |
| `extents(A)` | COUNT(*) | Cardinality on compound = post-WHERE count |
| Foreign keys | FK joins | Integer / EnumIdx arrays as references; capture-and-index idiom |


## 12. Symmetry system

| Feature | Status | Notes / sources |
|---------|--------|-----------------|

## 13. Data model

| Feature | Status | Notes / sources |
|---------|--------|-----------------|
| Tuples: literals, exact + wildcard destructuring, `head :: tail`, unit `()`; no positional access | Core | singleton collapse `(a) = a`; bare commas on BOTH sides of a `let` (`let a, b = c, d`), parens optional; a pattern whose name count matches neither the top-level width nor the flattened leaf count is an error |
| Tuple annotations: `Tuple<N>` (width only, elements inferred) and `Tuple<T1, ..., Tk>` (components written) | Core | a lone integer literal is the width, any other list is a component list of width k >= 2; the two may not be mixed. `Tuple<A, B>` is the SAME type as the written `(A, B)`. Only written components are checked at the call (`argument i, component j`) and only they survive to codegen — the width-only form's element slots are inference variables nothing instantiates, so it cannot carry arrays or nested tuples |
| Structs (named fields, no methods); functional update `{ x = 3.0, ..p }` | Core |  |
| Dependent records (later fields' bounds depend on earlier fields) | Core | CGPath example |
| Constrained records (`where` clause on struct) | Core | checked at construction |
| Mutually constrained records (`type P1 = ... and P2 = ... where ...`) | Core |  |
| Sum types / variants with payloads; `Option`, `Result` | Core |  |
| Pattern matching (`match ... with`, guards, tuple patterns, sum-type payloads); `if/then/else` as sugar | Core |  |
| Interfaces + `impl` (signatures only, no inheritance; interface composition) | Core |  |
| Struct FK fields | Core | `corpus/sql-foreign-keys` 006; [features/sql.md](features/sql.md) §12 |
| Type providers | Core | `load` for identifier; lazy `read` with eager `write` for granular streaming I/O |

## 14. Program structure and syntax

| Feature | Status | Notes / sources |
|---------|--------|-----------------|
| Newline statement separation, delimiter-aware; optional `;`; inline vs block bodies | Core |  |
| Modules (`module` declarations) | Core |  |
| Imports (`import` / `from` / `as` keywords) | Core | dotted names resolve to files: `import units.SI` -> `units/SI.blade`, searched under the stdlib roots (`$BLADE_STDLIB`, then `stdlib/` beside the binary and upwards) then the importing file's directory; transitive, cycle- and duplicate-checked (`src/ModuleResolve.fs`, `blade test module-resolve`) |
| Standard library (`stdlib/`) | Partly Done | `units.SI` only: the 7 SI base units + the coherent derived units. No prefixed/offset units — the unit algebra carries dimensions, not scale factors |
| Pseudo-native mathematics (rank-0 collapse foundation; `A + B` needs no constructor commitment) | Core |  |
| Named infix operators `a :name: b` (uniform lowest precedence) | Planned |  |
| `print` / expression output | Core | EXPECT-comment test convention |

## 15. Providers and I/O

| Format | Status | Notes / sources |
|---------|--------|-----------------|
| NetCDF | Core |  |
| Zarr | Core |  |
| Triangular Zarr file format | Partly Done | Provides Zarr storage spec for natively triangular or wreath-shaped tensors |
| Icechunk | Partly Done | Versioned (transactional) Zarr: `repo.checkout` with branch/tag/snapshot unit markers, compile-time snapshot pinning, cross-checkout axis identity. Writes, `read_window`, and packed-blocks reads pending. `.stream` and `load_compound` are REFUSED outright (`GenStreamOpen` / `GenReadCompoundVar` are `None`; the refusals are pinned in the icechunk test lane), not silently degraded — docs/plans/plan-icechunk-provider.md |

## 16. Backends and performance

| Feature | Status | Notes / sources |
|---------|--------|-----------------|
| C++ codegen (via g++) | Core |  |
| OpenMP parallelism via `omp(arg: depth)` clause | Core | depth counts S-dim levels per argument, outermost first, and the levels are the EXTERNAL ones the argument contributes to a nest built around the function (a caller's co-iteration) — a loop the body itself generates is licensed on that loop's own kernel; BL4001 warns on the confusion. A `Tuple<k>` parameter is one node: `omp(p: n)` licenses its first n rows (`comm`/`anticomm` on a tuple parameter stay refused) |
| CUDA backend via `cuda` clause | Core | requires x64 Native Tools environment |
| Lazy computation | Core | `compute` and `read` |
| OpenBLAS lowering for `gram()` (`cblas_?syrk`/`cblas_?herk` same-array / `cblas_?gemm` distinct) | Experimental | All four precisions `s`/`d`/`c`/`z` (complex same-array is a Hermitian `herk` rank-k update; distinct is `gemm` with `ConjTrans`); operands must agree in precision — mixed or unit-annotated element types keep the scalar loops; opt-in via `BLADE_BLAS=1` (or `OPENBLAS_DIR` set; `BLADE_BLAS=0` forces off). Output layout unchanged (packed symmetric / dense) — BLAS fills a staging buffer, repack lands Blade storage. Emitted `#include <cblas.h>` keys Build.fs's `-I`/link resolution (`OPENBLAS_DIR`, netcdf-style). Measured: gram p=1024×n=16384, 8.6 s loops → 0.55 s 1-thread / 0.15 s 16-thread; values agree to ~1e-14 rel |
| Comm-licensed parallel reductions: bare `omp` on a fold kernel | Core | See below; `tests/OmpTests.fs` (`blade test omp-reduce`, `omp-pragma`, `omp-coverage`), corpus `loops/110–111`, `diagnostics/049` |
| Serial-emission build knob `BLADE_OMP_THREADS` | Core | See 16.0; `CodeGen.ompThreadEmissionEnabled` |

### 16.0 `BLADE_OMP_THREADS` — spending the licence per deployment

`omp` / `omp(a: n)` in source is a **licence**: a statement about the kernel
(which dimensions are safe to thread, which folds are safe to reassociate). It
is part of the program. Whether a given **build** spends that licence is a
property of the machine the binary will run on, and that is what this
environment variable decides. One source, per-deployment builds.

| `BLADE_OMP_THREADS` | Emission |
|---|---|
| unset, or `2`, `8`, … , or anything unparseable | **Default.** Thread pragmas are emitted wherever the source licensed them; the degree of parallelism remains the runtime's `OMP_NUM_THREADS` |
| `1`, `0`, `off` | **Serial emission.** Every thread-level construct is suppressed — `parallel for`, `collapse`, `schedule(dynamic)`, the reductions' explicit teams, and the native `gram`/`matmul` threading macros |

**Why it is not the same as `OMP_NUM_THREADS=1` at runtime.** GCC outlines a
`parallel for` body into a separate function called through the OpenMP runtime;
that is a *compile-time* decision whose cost is paid even when the team turns
out to hold one thread. Measured on a licensed row map:

| | |
|---|---|
| pragma emitted, `OMP_NUM_THREADS=1` | 488 µs |
| **no pragma emitted** | **263 µs** (1.86× faster) |
| pragma emitted, multi-threaded | 187 µs (parity with hand-written C++) |

**`omp simd` is never suppressed.** A `simd` construct creates no team and is
not outlined, so it costs nothing at one thread. Sites that emitted
`parallel for simd` therefore drop to `omp simd`, and the Path A reduction below
drops to `omp simd reduction(<op>:acc)` — the thread half goes, the vector half
stays.

**Guarantees.** Serial emission changes *what is emitted*, never *which programs
compile* and never the values: a suppressed nest is byte-identical to the same
program with the clause deleted, and each decline leaves a
`// [omp] requested but emitted serial: BLADE_OMP_THREADS=…` line in the
generated C++, so a dropped clause is never silent. A numeric value ≥ 2 does
**not** bake `num_threads(n)` into the binary — v1 leaves the degree to the
runtime deliberately, since Path B's determinism contract is stated in terms of
the team size it actually gets. `-fopenmp` stays on the compile line in both
modes (`omp simd` needs it). MPI is out of scope: under `where mpi, omp(...)`
only the `omp` half is gated.

### 16.1 Parallel reductions (`reduce(xs, k)` with `omp` on `k`)

`reduce` is serial by default, like every other loop in Blade. It parallelizes
when the **fold kernel** carries `omp` in its where-clause:

```blade
reduce(xs, lambda(a, b) where omp -> a + b)                  // builtin body
reduce(xs, lambda(a, b) where comm(a, b), omp -> f(a, b))    // declared comm
function myAdd(a: Float64, b: Float64) where comm(a, b), omp = ...
reduce(xs, myAdd)                                            // named, same rule
```

`omp` here is the **bare** form (no parentheses): a fold walks one axis, so
there is no per-argument depth to name — the only question is whether the axis
may be reordered. The parenthesised `omp(a: n)` form is unchanged and still
means "up to n dimensions of argument `a` may carry threads" for map kernels.

**The licence.** Chunking a fold hands different associations and different
orders to different threads, so the kernel must be commutative *and*
associative. Two things grant it:

- **`comm(a, b)` declared on the kernel** — the same word `<@>` uses to opt into
  symmetric storage, and already cross-checked against the body's deduced parity
  (a provably antisymmetric body is `BL4013`, not a licence).
- **A recognised builtin body** — exactly `a + b`, `a * b`, `a && b`, `a || b`
  over the two parameters. These carry both properties outright and need nothing
  declared.

`omp` on a fold kernel with **neither** is `BL4016`, a hard error, not a silent
serial fallback: "asked and got serial" and "never asked" emit byte-identical
C++, so a dropped clause would be invisible.

**Two emission paths.**

| | When | Emitted shape |
|---|---|---|
| **A** | builtin `+`/`*` body over a real arithmetic element type, flat 1-D sweep (dense rank-1, or a CompoundIdx/SparseIdx `.data` walk) | one `#pragma omp parallel for simd reduction(<op>:acc)`; the accumulator is seeded exactly as the serial fold seeds it and the original value participates in the combine |
| **B** | every other licensed kernel, and every reduce over a deferred computation | an explicit team with contiguous per-thread chunks, each seeded from its own first value, combined in thread order through the fold wrapper. Needs no identity element, so any user kernel works |

Path B over a deferred computation chunks the **outermost** loop level only:
each thread runs the whole inner nest serially over the outer indices it owns,
so an inner triangular level is correct without any special handling. It also
materializes no intermediate array.

**Caveat: the reorder is real.** `comm` is checked; **associativity is not** —
no deduction establishes it, so `omp` on a fold kernel is your explicit reorder
licence, the same trust model as `comm`'s escape hatch. In floating point,
addition is not associative, so a parallel fold can differ from the serial one
in the last ULPs. Path B is deterministic for a fixed `OMP_NUM_THREADS` (chunk
boundaries and combine order are fixed functions of the team size); Path A hands
the combine to the OpenMP runtime, whose order the standard leaves unspecified.
Integer-valued data is exact under every association and reproduces bit-for-bit
either way.

**Not parallelized (deliberately):** the expression form of `reduce` (an IIFE
inside a kernel body or arithmetic — it routinely already sits inside a parallel
region; hoist it to its own `let`), multi-leaf `<&!>` fused fold trees, and
reduce over compact symmetric/antisymmetric/Hermitian storage (rejected at
typecheck for all folds — `decompact` first).

**`where repro` — the reproducibility demand (named functions only).** The
inverse of the reorder licence: the function's emitted body must keep the
interpreter's operation sequence. Discharged as (a) `BLADE_REPRO_FN` on the
emitted definition — `noinline` + `-ffp-contract=off` on GCC, so the body
neither contracts to FMA nor gets re-inlined into a contraction-licensed
caller; (b) call form everywhere a named function already has it (direct
calls, kernel positions via the eta wrapper — loops call the function rather
than inlining its arithmetic); (c) a veto inside `foldReorderLicensed`, which
covers the OpenMP fold paths, every `BLADE_FP_REASSOC` lane, and the LLVM
lane's fast-math flags at once — `where comm(a, b), omp, repro` on a fold
kernel emits serial with a `// [omp] requested but emitted serial: fold
kernel is `where repro`` marker; and (d) BLAS/LAPACK/cuBLAS routing declined
while the body emits (BLAS differs in the last ULP; an eigenbasis is not even
unique). Refusals: on a lambda (the annotation cannot travel into textual
inlining — name the kernel), on a `static function` (compile-time evaluation
has no emitted body), and combined with `cuda` (device contraction/rounding
cannot discharge the demand). A main-local emission (function forced into
`main()` as a `std::function`) keeps the routing veto but cannot carry the
attribute; the emitted text says so rather than staying silent.

## 17. Equivariance and ML (shipped module)

Module doc: [features/equivariant-nn.md](features/equivariant-nn.md), canonical.
Part II (the equivariant ML library) is landed; Part I's original core-language
hook — a value-level `with equiv(G, ρ)` annotation checked per-expression by the
unifier, with `EquivIdx<n, G, ρ>` carrying the rep data — was **superseded on
2026-07-28** and is archival only (module doc status note + §1–4). The adopted
factoring splits it in two: rep DATA lives in index types and unifies like any
other type (`IrrepsIdx<spec>`, `PgIrrepsIdx<G, spec>`), while the rep CLAIM is a
deduced-then-pinned *signature* attribute (`where ml.equiv(G)`), the same
lattice-plus-pins shape `comm`/`anticomm` use. `ml-spec §N` citations below point
into the retired ml-spec plan; its surviving material is in the module doc.

The whole surface is import-gated — `import ml as ml`, then `ml.<name>(...)`;
bare names are unbound and `from ml import ...` is rejected. Checking is
compile-time only, zero runtime cost.

| Feature | Status | Notes / sources |
|---------|--------|-----------------|
| Representations and irreps (parity, multiplicity) | Partly Done | shipped spelling is a `let static` list of `(l, parity, mult)` triples plus the spec-computing statics, not the `2*L1o + 3*L2e` sugar of ml-spec §2 |
| `IrrepsIdx<spec>` (block-structured primitive index type; flat-dense, spec-keyed nominal identity + nominative aliases; block-navigation statics `irreps_len/l/parity/mult/dim/offset`) | Core (v7) | ml-spec §3; module doc §6/§11b (the DepIdx equation is the semantic reading — DepIdx iteration codegen is NOT landed); corpus `index-types/111–119`, `ml-ops/005–008` |
| Clebsch-Gordan machinery: real-basis coupling tables, path validation, CG lookup | Partly Done | tables are compiler-native (`WignerTables.fs`, pinned against the `ml/` oracle) and baked into elaborated ops; the ml-spec §4 user-facing `CGPath`/`CGIdx` dependent records are not built (module doc §11b F1 resolution) |
| Equivariant ops: `tensor_product` (paths, weights, block indices) / `y_to` / `linear` / `gated` / `linear_rows` / `gated_rows` / `scalars` / `norms` (compile-time elaboration, static specs, real-basis CG tables) | Core (v7) | module doc §11b; corpus `ml-ops/` (24 files) + `ml-e2e/002`; elaborated ops stamp `IrrepsIdx` on their signatures and `grad()` differentiates through them |
| Spherical harmonics (`ml.y_to`) | Partly Done | real solid harmonics, lmax ≤ 2 (explicit polynomials); ml-spec §6's recurrence for lmax > 2 pending |
| Certificate disciplines: `where ml.equiv(G)` (O(3), SO(3), and the point groups), `where ml.galilean(u, …)`, `where ml.perm_equiv` | Core | signature attributes, not expression annotations; checked by `MLEquiv.fs` / `MLGalilean.fs` at the `MLElaborate` pass-1/pass-2 seam, with a typecheck-resident second opinion in `DeduceRep.fs`. A pin is a *polymorphism license*, the equivariance analogue of `comm`. Generator-based (Lie-algebra) discharge accepts hand-written bodies that composition-only checking rejected. Corpus `ml-equiv/` (106 files) |
| Certificate deduction: speculative `where ml.equiv` / `ml.galilean` suggestions | Core | BL4011 / BL4014 warning channels plus structured `deduced[]` entries (`kind` "equiv" \| "galilean") in `ide check --json`; propose-don't-export — always warnings, and deliberately no `--strict-pins` arm, since a certificate owns no storage decision |
| Diagnostics | Core | BL4007 no equivariant map exists (Schur-zero hom-space, and tensor-product outputs no CG path reaches); BL4008 / BL4009 / BL4012 equivariance / galilean / permutation-equivariance discipline violations; BL4011 / BL4014 certificate suggestions; BL9004 internal deduction disagreement (typed walker contradicts the seam checker — an ICE that stops the build, never the user's fault) |
| Synthesis, O(3): `ml.derive_linear` (degree 1), `ml.derive_tp` (uncompacted bilinear, output spec = full CG decomposition), `ml.derive_sym_tp` / `ml.derive_alt_tp` (S₂-compacted self-products), `ml.derive_poly` (degree K ≤ 4 homogeneous, K = 1 degenerating to `derive_linear` bit for bit) | Core | each emits the COMPLETE Schur basis of the admissible hypothesis space as ordinary Blade source — the parameter count is a theorem, and the certificate holds by construction, so a pin needs no proof search. This is the primary user surface, in place of hand-derivation. Corpus `ml-equiv/` |
| `ml.sym_lift` (degree-K monomial lift) and the conversions `tensor_to_irreps` / `sym_to_irreps` / `irreps_to_sym` | Core | rep-INTRODUCTION and rep-ESCAPE forms; deliberately left uncertified (a stamp would be a false axiom) — `sym_lift`'s output is a plain `Idx<cells>` monomial space whose O(3) action is `ml.sym_spec(SPEC, K)`, not an irreps space |
| Synthesis, Sₙ index-action: `ml.derive_perm_linear` (complete basis of Hom_Sₙ(ℝ^{N^K}, ℝ^{N^L}) = orbit indicators = partitions of the K+L axis positions into ≤ N blocks), `ml.derive_perm_bias` (K = 0, the invariant-constant space), `ml.perm_matmul` | Core | the `ml.perm_equiv` discipline; flat row-major node-power buffers, L = 0 the invariant readout. `perm_matmul` is PPGN's engine — the one bilinear shipped by name rather than by synthesis, since the Sₙ bilinears at that arity have no orbit-indicator basis (`derive_perm_tp` deferred). Corpus `ml-equiv/` |
| Synthesis, point groups: `ml.derive_pg_linear` + `PgIrrepsIdx<GROUP, spec>` (`C4`, `D4` shipped) | Core | the complete ℝ-Schur basis over a frozen character table, Frobenius–Schur aware (real irreps degenerate to `derive_linear`'s loop nest verbatim, for ulp agreement). Certificates do not transfer between groups. `ml.restrict` (O(3) ↓ point group) is deferred; corpus `ml-equiv/045–052`, `094`, `104` |
| Static sizing builtins | Core | `sh_spec`, `total_dim`, `tp_spec`, `hom_dim`, `sym_spec`, `alt_spec`, `tp_weight_dim`, `tp_full_weight_dim`, `linear_weight_dim`, `sym_tp_weight_dim`, `alt_tp_weight_dim`, `poly_weight_dim`, `perm_weight_dim`, `perm_bias_dim`, `irreps_*`, and the point-group set `pg_total_dim` / `pg_hom_dim` / `pg_irreps_len` / `pg_irreps_dim` / `pg_irreps_mult` / `pg_irreps_fs` / `pg_irreps_offset` / `pg_restrict` (returns a spec, not an int). Value position only (`let static SOUT = ml.tp_spec(S1, S2)`); the annotation then names the static |
| Norm activation | Near-term | ml-spec §8.2; `ml.norms` (invariant norm readout) is shipped and is a different op |
| Message passing: `scatter` / `gather` | Near-term | ml-spec §9 (expressible today as loops — `ml-e2e/00*` do; dedicated ops pending) |
| Reynolds applications: symmetric message passing, higher-order interactions, antisymmetric applications | Near-term | ml-spec §14; the CG exchange-symmetry compaction shipped as `derive_sym_tp` / `derive_alt_tp` (measured 30–42%, not the 2–4× originally claimed — module doc §9) |
| Automatic differentiation (`ad.grad` reverse mode, v1 subset; `ad.jvp` forward mode, strict superset incl. overwrites/recurrences/product folds/if-else/units/combinators through C7 pipelines) | Core | AST-level source transforms, one pass; module doc §11 has both ABIs + subsets; composition `ad.jvp(ad.grad(f))` = HVP, `ad.jvp(ad.jvp(f))` = second-order; corpus `ad/` + `ad-jvp/` + `ad-jvp-comb/` + `ml-e2e/`; remaining work in [features/equivariant-nn.md](features/equivariant-nn.md) §11 |

## 18. Graphs and trees (planned module)

Design drafts in ext §2.3–2.4; module doc: [features/graphs-trees.md](features/graphs-trees.md).

| Feature | Status |
|---------|--------|
| Tree structures (arrays as fixed-depth trees; path indexing) | Planned |
| Graph types via trace indices | Planned |
| Symmetric trees (commutative children) | Planned (speculative end) |

