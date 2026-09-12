# Blade: Quickstart Part 2: Advanced Features

> Supersedes `rewrite/QUIKSTARTp2.md` (whose section stubs are completed
> here). Assumes [quickstart-1.md](quickstart-1.md).

## Virtual Arrays

Consider two 3D arrays `A` and `B`. For some function `func`, we can iterate
over the arrays in an outer-product pattern:

```F#
function func = ...
let loop = object_for(func)
let result = loop <@> (A, B)
result |> compute
```

This happens seamlessly — but what's happening to the indices? A `method_for`
or `object_for` loop constructs the iteration space and emits the exact
indices into the kernel, so `A` and `B` are indexed correctly at kernel
scope. Usually we don't care what the indices are, just that they are used
correctly.

Sometimes, though, the indices are useful *inside* the kernel. Blade doesn't
allow raw emission of indices; instead we use an array object that has the
shape of an array but no data — slicing it fully just echoes the indices used
to reach it. Since there's no data, it costs nothing at runtime: it simply
becomes part of the loop structure.

This is a **virtual array**. The simplest is `range`:

```F#
range<Idx<M>, Idx<N>>   // emits all index tuples of its index space
```

```F#
function func1(a: T^0, b: T^0) = a * b
function func2(i: Nat, j: Nat, a: T^2, b: T^2) = a(i, j) * b(i, j)
let loop1 = object_for(func1)
let loop2 = object_for(func2)
let result1 = loop1 <@> (A, B)
let result2 = loop2 <@> (range<Idx<M>, Idx<N>>, A, B)
(result1 |> compute) == (result2 |> compute)
```
```
True
```

The second pipeline recreates how for-loops work in most languages — not the
efficient way here, but it shows the mechanism: virtual arrays map positions
in the index space to indices emitted into the kernel. Swapping in a different
virtual array changes the traversal. `halo<I, [offsets]>` is the stencil
transformer: it walks the *interior* of `I`, and instead of a bare index the
kernel receives a window `w`, where `w(o)` is the index at signed offset `o`
from the center — so neighbor reads are spelled through the window:

```F#
let A: Array<Float64 like Idx<5>> = [1.0, 2.0, 4.0, 7.0, 11.0]
let d = method_for(halo<Idx<5>, [-1, 0, 1]>) <@> lambda(w) -> A(w(1)) - A(w(-1)) |> compute
```
```
d = [3, 5, 7]
```

Three central differences from five cells: the `[-1, 0, 1]` window shrinks the
traversal to the interior, so no edge clamping ever reaches the kernel.

Other built-in virtual arrays: `reverse<I>` (reverse order). Because index values carry their
source index type as a unit (`i : Nat<LatIdx>`), a kernel can only index a
captured array with indices from the *right* index space — using a `LatIdx`
value on a `LonIdx` array is a compile error, even at equal extents.

When no named index type is in play, the **anonymous range** `m..n`
(half-open) is the lightweight spelling, and a plain dense range is an
ordinary rank-1 array value: it lifts elementwise, folds, and materializes
when bound. Index generation that is just arithmetic needs no loop at all:

```F#
let axis = -2.5 + 0.01 * Float64(0..450)   // a 450-point coordinate axis
let total = reduce(0..100, (+))             // 4950
let ids = 0..8                              // materialized iota, [0..7]
```

Reach for `method_for(range<...>) <@> lambda` when the kernel is a real
block, gathers, or composes with other combinators — not for a body that is
merely affine in the index.

## For Loops

`method_for` and `object_for` can be unwieldy, so there's shorthand:

```F#
let loop1 = for (A, B)
let result1 = loop1 <@> func
let loop2 = for func
let result2 = loop2 <@> (A, B)
```

Let-bound `for` loops are sugar for `method_for`/`object_for`. Virtual arrays
join via `in`:

```F#
let loop = for (A, B) in range<Idx<M>, Idx<N>>
let result = loop <@> lambda(i, j, a, b) -> ...
```

The `in` clause takes virtual arrays only (`range<I>`, not a bare `Idx<N>`),
and both orientations work — arrays left / kernel right, or kernel left /
arrays right. Poly-packs work too:

```F#
for args in SymIdx<arity(args), N> where comm(args)
<@> lambda(is, xs) -> f(is, xs)
```

## Zero Functions and Zero Array Tuples

Blade's combinator algebra has two "zeros", and both are useful.

**The zero array tuple `()`** is the empty argument *pack* — a loop over no
arrays. (It is not a `Tuple<N>` value: `Tuple<N>` annotations require a
literal `N >= 2`, and `()` is unit, not a tuple — see
[formalism.md](formalism.md) §2.8, "Tuples and argument packs".) It is the
identity for joining loops, and the natural base case of arity-polymorphic
recursion:

```F#
method_for() <@> moment        // ≡ pure 1     (identity element)
method_for(A) <@> moment       // arity 1
method_for(A, A) <@> moment    // covariance
```

**The zero function `zero`** is a kernel that produces zeros while keeping
the iteration structure — S-dimensions from the arrays, no T-dimensions:

```F#
M <@> zero                     // array of zeros shaped by M's S-dims
object_for(zero) <@> (A, A)    // symmetric matrix of zeros
```

In arity-polymorphic recursion, `zero` resolves to the identity element of
the surrounding operation — `1` in a product context, `0` in a sum context:

```F#
function comoment_prod(a: Poly<T^1>)
where comm(a) -> T^1 = {
    match arity with
    | 0 -> zero                       // resolves to 1 (multiplicative context)
    | _ -> let head :: tail = a
           (head - mean(head)) * comoment_prod(tail)
}
```

There is also `guard`, which turns a condition into structure-preserving
control flow:

```F#
guard(p, c)      // c if p is true; zeros of c's shape otherwise
c1 <|> c2        // choice: c1 if non-zero, else c2
```

These satisfy clean algebraic laws (they form a MonadPlus with `zero`), so
conditional pipelines compose predictably. One law that does NOT hold: right
distribution of `<|>` through `>>=` — don't rely on it (this is a checked
fact, not a style note).

## Parallelism

Parallelism in Blade is declared per-function, in the `where` clause:

```F#
function covariance(a: T^1, b: T^1)
where comm(a, b), omp(a: 1)
= (a - mean(a)) * (b - mean(b))
```

`omp(a: 1)` means: **up to** 1 level of the S-dimension loops coming from
argument `a` may carry OpenMP threads. Arrays are bound in order, so their
S-dimension loops nest in order — the first argument's loops are outermost and
are the natural parallelization target.

### The clause licenses EXTERNAL loops

An `omp` clause on a signature is about the S-dims that argument **contributes
to a loop built around the function** — the caller's co-iteration when the
function is used in kernel position:

```F#
function cov(a: Float64, b: Float64) where omp(a: 1) = a * b
let m = object_for(cov) <@> (A, B) |> compute   // <- the licensed nest
```

It says nothing about a loop the **body itself** generates. Those loops belong
to the kernel of the apply that builds them, and are licensed by a clause on
*that* kernel:

```F#
function lsdft(s: T^1, t: T<time>^1, omegas: T<angular_frequency>^1)
where omp(s: 1) = {                    // licenses a CALLER co-iterating series
    ...
    omegas <@> lambda(w) where omp(w: 1) -> {   // licenses THIS loop
        ...scalar work over the captured arrays...
    } |> compute
}
```

Writing `where omp(omegas: 1)` on `lsdft` instead would license nothing:
`omegas` is iterated by a loop `lsdft` builds internally, and that loop reads
its licence from its own kernel. The compiler warns (BL4001) when a licensed
parameter is an operand of an apply inside the body whose kernel carries no
clause of its own, because the emitted C++ for "asked and got serial" is
byte-identical to "never asked".

A `Tuple<N>` parameter may carry a parallel clause: under one-level structural
matching the tuple is **one schema node**, so `omp(p: n)` licenses the first
`n` of the levels that node contributes (`omp(p: 1)` on a `Tuple<2>` threads
one level, not two). `comm`/`anticomm` on a tuple parameter stay refused — they
address parameters by position, and the expansion into row parameters
renumbers those.

The depth is a **licence, not a demand**. It caps the compiler's choice rather
than replacing it: the emitted strategy is still picked from the loop structure
(collapse for rectangular levels, `schedule(dynamic)` when triangular work sits
below), and the licence bounds how far that choice may reach. Three consequences
worth knowing:

- The count is **per argument**. Above, `a` is `T^1` and owns exactly one loop
  level, so `omp(a: 2)` still licenses only one — there is no second level *of
  `a`* to license. To thread both loops of the `(a, b)` nest, license both
  arguments: `omp(a: 1, b: 1)`, which permits `collapse(2)`.
- Licensing fewer levels than the structure could use is honoured. `omp(a: 1)`
  on a collapsible two-level nest emits a plain `parallel for` over `a`'s level,
  never `collapse(2)` — collapsing would thread a dimension of `b`, which
  granted nothing.
- Licensing an argument whose loops are **not** outermost moves the pragma
  inward: `omp(b: 1)` parallelizes `b`'s level and leaves `a`'s outer loop
  serial, opening a team per outer iteration.

A rank-2 argument does own two levels, so `omp(a: 2)` on `a: T^2` licenses both
and permits collapsing them.

Two things fall out of the design:

1. **Parallelism is part of the loop level type.** Two loops fuse only if
   their levels match — extent, symmetry state, AND parallelism. You cannot
   accidentally fuse a parallel loop with a serial one.
2. **Backends are substitutable.** `cuda` requests GPU codegen for the
   kernel; the same S/T structure lowers to CUDA kernels (including
   triangular/simplicial iteration spaces). Other backends (e.g. OpenACC)
   slot into the same clause position.

Because iteration structure is explicit and symmetric spaces are typed,
parallel triangular iteration needs no manual index gymnastics — the loop
object already knows the canonical space it covers.

## Loop Combinators

Blade offers combinators beyond `<@>` for building performant pipelines.

From functional programming:

* `|>` — pipe: pass the left value as the last argument of the right function.
* `>>` — compose: two sequential functions become one.

For loop fusion and composition:

* `<&>` — fuse loop nests where possible (automatic common-prefix fusion),
  run both computations.
* `<&!>` — force complete fusion; error if impossible. Requires both
  computations to come from the *same* `method_for` loop.
* `<*>` — join loops: `method_for(A) <*> method_for(B) == method_for(A, B)`.
  Folding `<*>` over a runtime list of arrays builds loops of runtime arity.
* `>>@` — compose kernels already wrapped by `object_for`:
  `oloop1 >>@ oloop2`.
* `@>>` — compose at `method_for` call sites:
  `(mloop <@> f1) @>> (mloop <@> f2)`.

The two composition operators are two sides of one identity —
compose-then-apply equals apply-then-compose:

```F#
(object_for(f) >>@ object_for(g)) <@> A
== (method_for(A) <@> f) @>> (method_for(A) <@> g)
```

While Blade guarantees performance for a single kernel and array tuple, these
combinators build larger pipelines that Blade can't always reason about
globally; it's up to you to compose them well.

### Complex loop combinators

Combinators are themselves binary functions — so loop constructors lift them
too:

```F#
let pipeline = object_for(>>)      // build kernel pipelines incrementally
let kernel = pipeline(normalize)(log_transform)(clip(0,1))(scale(255))
method_for(A) <@> kernel

let fuse_all = object_for(<&>)     // fold fusion over a list of computations
let first_success = object_for(<|>)
```

Kernels become data — constructed, inspected, transformed — until applied to
arrays.

## Array Combinators

Array-level combinators operate on (lazy) array expressions; `|> compute`
materializes with cache-optimal layout:

* `zip(A, B, ...)` — pair up arrays over their shared leading dimensions. By
  default the kernel still takes one flat parameter per array
  (`lambda(a, b) -> ...`); write a `Tuple<2>` (or written-tuple) parameter to
  take the co-iterated pair as one value instead (`lambda(p: Tuple<2>) ->
  p[0] + p[1]`) — see [formalism.md](formalism.md) §2.8. This is what
  elementwise `+` desugars through, in the flat form.
* `stack(A, B, ...)` — new leftmost dimension selecting among the arrays.
* `transpose(A, perm)` — hard transpose (real data movement on materialize);
  the price of re-ordering a curried index chain.
* `subset` / `split` / `join` / `reverse` / `shift` — range extraction,
  splitting, concatenation, reversal, shifting (with boundary modes — the
  building blocks of stencils).
* `align` / `stencil` — bundle shifted copies so the kernel receives N
  separate neighbor arguments with boundary handling declared once.
* `decompact(A, axis)` — expand a symmetric/antisymmetric compact axis to
  dense storage (sign-correct for antisymmetric axes). Useful at the boundary
  with external tools that expect dense data.
* `A <|:> B` — fallback: read `A` where allocated, else `B` (user-managed
  sparsity).

Relational operations (`mask`, `compound`, `group_by`, `sort`, `unique`,
`reduce`, ...) are covered in [features/sql.md](features/sql.md) — one-liner:

```F#
// SELECT temp FROM temps WHERE temp > 25 ORDER BY temp DESC
sort(compound(temps, mask(temps, lambda(t) -> t > 25.0)), lambda(t) -> -t)
```

## Arity Polymorphism Semantics

Part 1 §10 showed the mechanics; here is the semantic model.

**Identity groups.** At a call site, *neighboring identical* arguments form
identity groups: `(A, A, B)` groups as `[(A, A), (B)]`; `(A, B, A)` is three
singleton groups. `comm` licenses symmetry only within a group.

**Output deduction.** For `kernel(a: Poly<T^k>) -> T^m`:

1. Each input's last k dimensions are consumed by the kernel (T-dims); they
   must be compatible across inputs.
2. Each input's remaining dimensions are S-dims.
3. A group of size g with `comm` contributes `SymIdx<g, ·>` over its
   *compound* S-space — the g whole index tuples are interchangeable, jointly.
   Without `comm`, or at g = 1, contributions are dense.
4. Group contributions concatenate, in order (no broadcasting).
5. The kernel's `T^m` supplies trailing output dims.

**Scope variables.** Inside a poly kernel: `arity` (pack size), `nth`
(recursion depth), `args[k]` (structural access — `[]`, not `()`),
`let (head, tail) = args` (left-associative destructuring; excess names bind
`()`).

**Speedups.** Each comm-ed identity group of size g contributes g!; distinct
groups multiply. One group over a multi-dimensional array gets its factorial
over the compound space — not per dimension (see Part 1 §6 for why).

## Reynolds Operators

Commutativity + identity is one road to symmetric output. The Reynolds
operator is the other: it *makes* symmetry rather than detecting it.

```F#
let g = lambda(x, y) -> x / y            // NOT commutative

let symmetrized = method_for(A, A) <@> reynolds(g)   // g(x,y) + g(y,x)
let antisym = method_for(A, A) <@> reynolds(g, Antisymmetric)   // g(x,y) - g(y,x)
```

`reynolds(g)` sums `g` over permutations of its *value arguments* (with
signs, in the antisymmetric case), making the wrapped kernel commutative by
construction. What that buys follows the same law as `comm`: with the **same
array** in the wrapped positions you get symmetric storage and triangular
iteration (equal permutation terms are deduplicated, so a commutative `g`
costs one term); with **distinct arrays** you get a commutative kernel but a
dense, non-index-symmetric output — Reynolds does not substitute for array
identity. (Machine-checked: the H ∩ Stab license is exact.)

Antisymmetric Reynolds zeroes diagonals automatically and negates on
transposes — determinant-like structures fall out:

```F#
let wedge = lambda(a, b, c) -> a(0) * b(1) * c(2)
method_for(A, B, C) <@> reynolds(wedge, Antisymmetric)
// alternating sum over all 6 permutations, signed
```

Partial symmetrization takes positions: `reynolds(g, positions=[0, 1])`.

## Equivariance Annotations

Where index types handle *discrete* symmetry (permutations — storage and
iteration), equivariance annotations handle *continuous* symmetry (rotations
and friends — pure type checking, zero runtime cost):

```F#
let v: Array<Float like Idx<3>> with equiv(SO<3>, vector)
let e: Float with invariant(SO<3>)

v + v        // OK: same representation
norm(v)      // invariant — safe to compare, order, print
v(0)         // Error: extracting a component breaks SO(3) equivariance
```

Inference flows through expressions (differences of vectors are vectors,
norms are invariants, invariant × vector is a vector...), and mistakes —
adding a vector to a pseudovector, declaring the wrong output representation,
storing a symmetric representation in antisymmetric storage — are compile
errors with domain-language messages.

The two systems compose: a stress tensor is
`Array<Float like SymIdx<2, 3>> with equiv(SO<3>, L2_even)` — triangular
storage from the index type, transformation checking from the annotation.
The full equivariant ML stack (irreps, tensor products, spherical harmonics,
message passing) is the subject of
[features/equivariant-nn.md](features/equivariant-nn.md).

## Metaprogramming with Static Functions

`static` marks compile-time computation. Static values and functions can
appear in *type positions* — types are computed, not just written:

```F#
let static n = 100
static function triangle(k) = k * (k + 1) / 2

type PackedSym = Array<Float like Idx<triangle(n)>>   // Idx<5050>
```

Rules of the game:

- `static function` may capture only `static` values; it runs at
  compile time when its arguments are static.
- No totality proofs, no proof assistant — just the restriction that static
  computation depends only on static inputs (that keeps type checking
  decidable).
- Type providers push this further: `NetCDFProvider<"era5.nc">` reads file
  *metadata* at compile time and mints index types and typed array
  declarations for the file's actual shape. The structure is static; the
  data is runtime.

```F#
let static spec = [(0, 0, 16), (1, 1, 8), (2, 0, 4)]  // (l, parity, mult) — static data...
type Features = IrrepsIdx<spec>                       // ...drives a block-structured index type
let h: Array<Float like Features> = ...               // extent = 16·1 + 8·3 + 4·5 = 60
```

(Landed in v7: `IrrepsIdx` is a flat-dense primitive index type whose
identity is the spec itself — two 60-cell arrays with different specs are
distinct types. See the equivariant-NN module doc §6.)

This is the same machinery that makes arity polymorphism typable: the type
level can *count and compute*, so output types can depend on how many arrays
you passed — which is where we came in.

---

Next steps: worked end-to-end programs in [examples.md](examples.md); the
full semantics in [formalism.md](formalism.md); what's proved, in
[proofs.md](proofs.md).
