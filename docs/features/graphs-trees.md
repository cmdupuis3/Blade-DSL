# Blade Feature Module: Graphs and Trees

Status: **design refreshed (2026-08-25); implemented through P5 (2026-08-26).**
`TreeIdx<shape>` declares, constructs from a flat preorder literal, serves
STATIC whole-path reads `T((c0, c1, …))` and their composition
`T((p))((q))`, and crosses function boundaries — all in both lanes, alongside
`extents`, printing and `reduce`. The derived dense axes `LeafIdx<S>` /
`NodeIdx<S>` are plain `Idx` records, and `leaves(T)` retypes a tree's pool onto
its leaf axis at zero cost, which is the sanctioned route for every bulk form
(`method_for(leaves(T)) <@> f`). Still refused, each for a stated reason:
`method_for` / `object_for` over a tree-slotted operand, `range<>` slots,
non-literal or wildcard coordinates, paths that stop at an internal node
(including a partial path BOUND to a name — escaping subtree views are cut),
reads of a tree slot combined with other slots, and a tree argument reaching a
parameter that declares no tree slot or a different one — see the plan doc's §6. This revision rewrites the v10-era sketch
into current Blade notation and current Blade mechanism. The settled theory is
kept in substance (§1); what changed is the *surface*, and in four places the
surface change forced a substantive correction — each is marked
**[correction]** and argued, not asserted.

Implementation plan: [plans/plan-graphs-trees.md](../plans/plan-graphs-trees.md).

Source material: the deleted `rewrite/blade_extensions_v10.md` §2.3–2.4
(recover with `git show 3bedf83^:rewrite/blade_extensions_v10.md`) and the
previous revision of this file, both of which this supersedes. Companion
reading: `docs/formalism.md` §3 (index types), §7.3 (virtual arrays), §7.5
(recursive arrays), §9 (currying), §10.4 (MonadPlus), §11–12 (symmetry,
triangular storage).

---

## 0. What this revision changes

| v10 sketch | This revision | Why |
|---|---|---|
| `T[(0,)]` path indexing | `T(0)`, `T((0, 1))` | `[]` is pack access only; `()` is application/indexing (formalism §3.2) |
| `Shape = Node(...) \| Leaf` as prose | `let static` shape value + `TreeIdx<shape>` | Matches the `IrrepsIdx<spec>` precedent exactly — no new declaration form (and no `List<T>`, which does not exist) |
| `Trace<N>` as an index type | **deleted as a type**; the visited set is a value | **[correction 1]**, §4.1 |
| `DAGIdx` as an index type | **deleted**; `where acyclic(g)` on the consuming function | **[correction 2]**, §4.4 |
| `for i in Trace<G> take k { ... }` | `let rec` over `Idx<k>` — `take k` *is* the recursion extent | **[correction 3]**, §5 |
| `on_collapse` / `partition_on_collapse` primitives | `guard` / `<\|>` / `group_by`, all of which exist | §5.3 |
| Symmetric trees = "sorted path", storage open | Uniform case *is* `SymIdx<r,n>`; sibling symmetry buys **iteration, not storage** | **[correction 4]**, §6 |
| Open Q3 "tree × array hybrids" | Free, in v1 | §3.6 |
| Open Q4 "AD through structure" | Answered: values differentiate, structure does not | §7.4 |

## 1. Trees as generalized arrays (unchanged theory)

An array is a tree with (1) uniform path depth — the rank — and (2) uniform
branching per level — the extents. Trees relax both:

| Structure | Depth | Branching | Index type | Static? | Needs a prior slot? |
|---|---|---|---|---|---|
| Vector | 1 | n | `Idx<n>` | yes | no |
| Tensor | r | n₁ × … × nᵣ | `Idx<n₁>, …, Idx<nᵣ>` | yes | no |
| Ragged | 2 (fixed) | variable per outer row | `Idx<n>, RaggedIdx<lens>` | **no** (runtime) | **yes** (BL3999) |
| Dependent | r (fixed) | formula per outer index | `DepIdx<I, f>` | yes | n/a |
| **Tree** | **variable per path** | **variable per node** | **`TreeIdx<shape>`** | **yes (v1)** | **no** |

An array index is a fixed-length tuple; a tree index is a **path** — a
variable-length sequence of child selections.

The common abstraction, restored from the v10 source because it is the actual
thesis of this module:

- all of these are functions from an index domain to values;
- all support dimensional currying — partial indexing yields a view;
- all can carry symmetry (commutative indices / commutative children);
- all are stored flat, through a bijection fixed by the index type.

### 1.1 The delta relative to `RaggedIdx`

`RaggedIdx` already gives variable branching at *fixed* depth. `TreeIdx` adds
variable *depth*, and inverts two of `RaggedIdx`'s properties:

- **Staticness.** `IndexTypeValidator.isKnownStatic` classifies `RaggedIdx`,
  `CompoundIdx` and `SparseIdx` as runtime and `DepIdx` as static-iff-its-parts
  are (`src/IndexTypeValidator.fs`). `TreeIdx` v1 sits with `DepIdx`: the whole
  shape is a `let static` value, so cardinality, every subtree size and every
  node arity are compile-time constants. The runtime lane is `DynTreeIdx` (§7.2).
- **Self-containment.** `RaggedIdx` cannot be the first index slot
  (`IxKErrorRaggedNoPrior`, BL3999) because its extent is a *function of the
  outer iteration position* — there is nothing to look the lengths up with.
  `TreeIdx` carries its whole shape in the type, so it is a legal first (and
  only) slot.

The two are not unified, and deliberately: a rank-2
`Array<T like Idx<n>, RaggedIdx<lens>>` *is* a depth-2 tree, but index types
have nominal identity (formalism §3.3) and there are **no implicit conversions
between index types** (formalism §3.7). The bridge is an explicit transform, in
the `flip`/`rename`/`subset` family:

```blade
let lens: Array<Int64 like Idx<3>> = [3, 2, 1]
let r: Array<Float64 like Idx<3>, RaggedIdx<lens>> = [[1.0, 2.0, 3.0], [4.0, 5.0], [6.0]]

let t = retree(r)     // Array<Float64 like TreeIdx<depth2(lens)>>
let b = flatten(t)    // back, when the tree happens to be uniform-depth
```

### 1.2 Flat storage with a precomputed bijection (unchanged)

Trees are stored contiguously in **depth-first preorder** with a precomputed
bijection — the same contract every Blade index type satisfies (formalism §3.2:
Domain, Cardinality, Storage bijection, Enumeration):

```
TreeIdx<shape> = {
    forward  : Path        → Offset            // Σ skipped-subtree sizes + local offsets
    backward : Offset      → Path
    subtree  : PartialPath → (Offset, SubShape)
}
```

Path-to-offset is O(|path|) arithmetic over precomputed subtree sizes — one
memory access, no pointer chasing. Against pointer trees (table restored from
the v10 source):

| Operation | Pointer tree | Flat tree with bijection |
|---|---|---|
| Access path of length k | O(k) pointer chases | O(k) arithmetic + 1 access |
| Cache behavior | k cache misses (random) | 1 cache miss (predictable) |
| Memory overhead | 2–3 pointers per node | subtree-size table |
| Insertion | O(1) at position | O(n) rebuild |

Flat trees with a bijection are the right trade for static or rarely-modified
structures with frequent access — the scientific-data case, which is Blade's.

Concretely the descriptor is a **superset of the existing `Ragged<T>` CSR
descriptor** (pool + offsets), adding two tables:

```
pool : T[card]        values, preorder
off  : Nat[nodes+1]   node → first-cell offset          (the CSR half, reused)
deg  : Nat[nodes]     node → child count
size : Nat[nodes]     node → subtree cell count         (the new table)
```

**The preorder invariant that pays for everything downstream:** a node's entire
subtree occupies a *contiguous* block beginning at the node. Subtree views are
therefore pointer+length, exactly like a peeled `RaggedRow<T>`
(`tests/corpus/index-types/023`), and reverse preorder visits every descendant
strictly before its ancestor — which is what makes the bottom-up fold a plain
`let rec` (§5.5) rather than a new construct.

One implementation gate the v10 sketch never stated: the C++ runtime contract
(`src/cpp/index_types.h`) keys every index type on `std::array<size_t, NDIM>`
with `NDIM` a compile-time constant. A variable-length path does not inhabit
that contract; the plan's P0 settles the representation (single tuple slot, the
`SparseIdx` precedent) before anything else is built.

---

## 2. (a) Declaring a tree shape

### 2.1 No new declaration form

The precedent is settled and it is `IrrepsIdx<spec>`
(`tests/corpus/index-types/111`–`114`):

```blade
let static spec_h = [(0, 0, 2), (1, 1, 2), (2, 0, 1)]
let x: Array<Float like IrrepsIdx<spec_h>> = [...]
```

A `let static` value, referenced from a type-level `< >` slot, its payload
baked into a parameterized index tag (`__irreps:<name>:<payload>`,
`IxKIrreps`). `SparseIdx<keys>` and `CompoundIdx<mask>` follow the same
pattern. A tree shape is spelled the same way and needs **no `Unit`-like
top-level form and no new `type` syntax**:

```blade
let static crystal = [[leaf, leaf], [leaf, leaf, leaf]]
type CrystalIdx = TreeIdx<crystal>

let T: Array<Float64 like CrystalIdx> = [1.0, 2.0, 3.0, 4.0, 5.0]
// valid paths: (0,0) (0,1) (1,0) (1,1) (1,2)      cardinality 5
```

**The shape grammar is an ordinary nested array literal over one new nullary
constant, `leaf`.** A nest of shapes is a shape; `leaf` is the base case. No
new literal syntax, no new lexer state — the elaborator only has to recognise
`leaf` and read the nesting, which it already does for ragged literals.

Nesting is arbitrary and non-uniform:

```blade
let static skew = [ leaf,
                    [ leaf, [leaf, leaf] ],
                    [ [leaf], leaf, leaf ] ]
// paths: (0) (1,0) (1,1,0) (1,1,1) (2,0,0) (2,1) (2,2)   cardinality 7, depth 3
```

Note `(0)` and `(2,1)` are leaves at different depths. That is the whole point.

### 2.2 Where the compile-time-ness comes from

From `let static`, and from nowhere else. `let static` is compile-time and
immutable everywhere; `StaticEval` folds it; `IrrepsIdx` already depends on
exactly this. Consequences, all inherited rather than invented:

- A shape built by a `static function` is legal (statics may even mutually
  recurse), so generated shapes — complete *k*-ary trees, H-trees, octrees,
  a Huffman tree from static weights — are in scope:

  ```blade
  static function complete(k: Int64, d: Int64) =
      if d == 0 then leaf else replicate_shape(k, complete(k, d - 1))

  let static oct = complete(8, 3)          // 512 leaves, 585 nodes
  type OctIdx = TreeIdx<oct>
  ```

- A shape that is *not* statically evaluable is refused, with the same
  reasoning `RaggedIdx` refuses a runtime `lens` (BL4018: a lens computed at
  run time can be neither honoured nor compared). **BL4021 — invalid tree
  shape**: not statically evaluable, or malformed; the non-static half steers
  to `DynTreeIdx` (§7.2). One code, two mistakes, told apart by the message.
- The `StaticEval` stack budget applies; a shape is a static value like any
  other, and a very deeply *generated* shape hits the same cliff. Depth, not
  cardinality, is the binding constraint. Measured at P2: the shape's own
  DEPTH costs nothing at lowering — decoding is a fold and the table build is
  three iterative passes, so a literal depth-99 chain (`[1, 1, …, 1, 0]`)
  lowers with no cliff at all. The budget binds on the *expression* that
  produces the shape, i.e. on a recursive `static function`, not on the tree.

### 2.3 The canonical internal form: the preorder degree sequence

The nest is the *surface*. The elaborator lowers it to a **preorder degree
sequence** — the child count of each node in depth-first order:

```
crystal = [[leaf, leaf], [leaf, leaf, leaf]]   ⇒   deg = [2, 2, 0, 0, 3, 0, 0, 0]
```

This is the only form the type carries, and it is sufficient: `size`, `off`,
cardinality, depth, the leaf set and every subtree shape are derivable from it
in one linear pass. It is a rank-1 `Int64` array, so it hashes — index identity
is the **hash of the degree sequence**, which is `CompoundIdx`'s whole-mask
hash discipline (O(1) type equality) applied to a smaller object.

A second, explicit construction route exists for machine-generated shapes, in
the call-shaped special-form family (formalism §15.7):

```blade
let static big = degrees([2, 2, 0, 0, 3, 0, 0, 0])   // same shape as `crystal`
type BigIdx = TreeIdx<big>
```

`degrees(d)` validates that `d` is a well-formed preorder sequence (Σdeg =
n−1, and the prefix-sum walk closes exactly at n) and is a construction error
otherwise — the `SparseIdx` "duplicate keys are a construction error"
discipline.

### 2.4 Identity, aliasing and tags

`type CrystalIdx = TreeIdx<crystal>` registers a **nominative alias**, exactly
as `type Feat = IrrepsIdx<spec2>` does (`tests/corpus/index-types/112`). Two
shapes with equal degree sequences are the same *structural* type (duck typing,
formalism §3.3); a named alias adds nominal identity. Distinct trees over the
same shape that must not be confused take a tag, as staggered grids do:
`TreeIdx<crystal, Mesh>`.

Proposed kind `IxKTree`, parameterized tag `__tree:<name>:<payload>` — the
`IxKIrreps` discipline verbatim (`Tag` carries something the alias name would
destroy).

---

## 3. (b) Path indexing by curried application

### 3.1 One slot, variable currying depth

**A `TreeIdx<shape>` is ONE index slot.** Its domain is the path set. This is
the `SparseIdx` shape ("one slot whose domain is k-tuples") with k allowed to
vary, and it is the choice that keeps everything else in the language working:
`extents`, `reduce`, `method_for`, kernel `irank`, S-dimension counting and the
`<*>` product all count *slots*, and a slot whose rank contribution varied per
cell would break `TypeCheck.expandedRows` (which widens a multi-rank slot into
a fixed r params, formalism §7.2).

So: rank is a *number* for arrays and a *shape* for trees, and the reconciling
statement is that a tree array's rank is 1 while its **currying depth is
path-dependent**.

### 3.2 What `T(c)` returns

Currying is partial-path application, returning a view — offset plus
sub-shape (§1.2's `subtree`). The residual of a tree shape is a tree shape,
which is the `SparseIdx` residual rule ("the residual of a key set is a key
set") in a second family:

```blade
let static crystal = [[leaf, leaf], [leaf, leaf, leaf]]
type CrystalIdx = TreeIdx<crystal>
let T: Array<Float64 like CrystalIdx> = [1.0, 2.0, 3.0, 4.0, 5.0]

let sub  = T(1)          // Array<Float64 like TreeIdx<[leaf, leaf, leaf]>>   — 3 cells
let cell = T(1)(2)       // Float64                                          — a LEAF
let same = T(1, 2)       // identical:  T(i, j) ≡ T(i)(j)   (formalism §9)
```

`T(1)` is a contiguous pointer with a reduced *shape* type — not a strided view
(formalism §9, "Vs slicing"). Because the subtree is contiguous in preorder,
the view is pointer + length + sub-shape, structurally the same object as a
peeled `RaggedRow<T>`.

**Typing rules.** `s ▷ c` denotes the c-th child subshape of `s`; `s = leaf`
denotes the leaf shape.

```
Δ ⊢ T : Array<V like TreeIdx<s>>   Δ ⊢ c : Nat<TreeIdx<s>>   c static   c < arity(s)
──────────────────────────────────────────────────────────────────────── (Tree-Curry-Static)
Δ ⊢ T(c) : Array<V like TreeIdx<s ▷ c>>          if s ▷ c ≠ leaf
Δ ⊢ T(c) : V                                     if s ▷ c = leaf

Δ ⊢ T : Array<V like TreeIdx<s>>   Δ ⊢ c : Nat<TreeIdx<s>>   uniform_children(s)
────────────────────────────────────────────────────────────────── (Tree-Curry-Dyn)
Δ ⊢ T(c) : Array<V like TreeIdx<s ▷ 0>>          bound erased to BoundedIdx<0, arity(s)>

Δ ⊢ T : Array<V like TreeIdx<s>>   Δ ⊢ p : Path<s>   leaf_path(s, p)
────────────────────────────────────────────────────────────────── (Tree-Path)
Δ ⊢ T(p) : V                                     O(1): forward(p)
```

### 3.3 The whole-path form, and why `[]` still is not indexing

A `Path<s>` is a **pack** — a variable-length tuple of child selectors. Packs
are accessed with `[]` and applied with `()`, and nothing about that changes:

```blade
let p: Path<CrystalIdx> = (1, 2)
let v = T(p)             // ()  — application: one O(1) offset lookup
let c0 = p[0]            // []  — pack access: the first child selector, 1
```

So the v10 spelling `T[(0,)]` was wrong twice over and both readings now exist
separately and correctly: `T((0,))` applies a one-element path, `p[0]` reads a
path component.

Whole-path and curried forms are definitionally equal —
`T(p) ≡ T(p[0])(p[1])…(p[|p|−1])` — and differ only in cost: one hashed/computed
offset versus |p| table steps. The compiler picks; there is no user-facing
choice, per "the fastest way is the only way". (Whether both spellings ride the
same tabulated dispatch in v1, or the curried form defers, is the plan's P0
decision.)

### 3.4 Leaf versus internal, and the refusal that keeps it decidable

When the path is static, leaf-ness is decided at compile time by
(Tree-Curry-Static) and there is no runtime tag. When it is not — a dynamic
path into a non-uniform shape — the result is *either* a value *or* a subtree,
which is a sum the type system will not silently introduce.

**v1 refuses it**: proposed **BL4019 — tree read of indeterminate depth**. The
refusal is the feature, and it names the three ways out, all of which are real:

1. Read the dense leaf axis instead (§3.6) — almost always what was meant.
2. Restrict to a uniform-depth region, where (Tree-Curry-Dyn) applies.
3. Bind the node and `match` on it, making both arms explicit.

### 3.5 Bounds when siblings have different child counts

Three tiers, each already precedented, none new:

| Child value | Check | Precedent |
|---|---|---|
| static literal on a static shape | compile time; `T(1)(5)` on a 3-child node is an index-type violation | BL4003 |
| iteration-emitted | **safe by construction** — `range<ChildIdx<s, p>>` emits exactly the valid children, unit-tagged (formalism §3.10) | `range<I>` |
| computed / dynamic | `BoundedIdx<0, arity(p)>`, erased to a runtime bound; the bound is a table load `deg[node]` | `RaggedIdx`'s `lens[__g]` peel |

An out-of-arity dynamic child is a runtime error of the same class as a
mask-false `CompoundIdx` read — reuse **BL8003** (out-of-domain read), per the
house discipline of joining an existing code family before minting a new one —
and the `<|:>` array fallback is available at that seam exactly as it is for
compound reads.

The one real cost this inherits from `RaggedIdx` is that the bound is
non-affine, so a tree axis does not `collapse(k)` — the same reason
`src/CodeGenCuda.fs` refuses collapse over ragged rows. That constrains the
*node* axis only; the derived dense axes below are affine and lose nothing.

### 3.6 Derived dense axes — the implementability unlock

Path addressing is for structure. Bulk numerics run on the flat pool, and the
type system should say so. Three derived index types, all ordinary `Idx<n>`:

```blade
type Leaves = LeafIdx<crystal>       // Idx<5>  — leaves, preorder
type Nodes  = NodeIdx<crystal>       // Idx<8>  — all nodes, preorder
type Kids   = ChildIdx<crystal, p>   // Idx<deg(p)> — one node's children
```

and their virtual arrays (formalism §7.3), which erase completely:

```blade
range<LeafIdx<crystal>>      preorder<crystal>      postorder<crystal>
```

Because these are plain `Idx`, **every existing optimization applies unchanged**
— vectorization, `omp`, BLAS routing, triangular storage where a symmetric
group sits on top. A tree in Blade is a flat array with an addressing scheme,
and only the addressing is new.

**Implemented at P5** — `LeafIdx<S>` and `NodeIdx<S>` land as written, lowering
to plain dense records (`Tag = None`, `IxKPlain`), so `LeafIdx<crystal>` really
does unify with a bare `Idx<5>`. `range<LeafIdx<S>>` needs no new code and
discharges the iteration half on its own. Three qualifications, all measured:

- **`leaves(T)` is the retype**, and the doc above did not name it. A tree
  binding is already the flat pool, so `leaves(T)` returns the *same* node with
  a new type — zero copy, zero emitted code — and it is the visible point where
  a program opts out of path addressing into array addressing. It is a
  shadowable plain-call intrinsic, not a keyword, so `leaves` stays an ordinary
  name a program may bind.
- **`ChildIdx<S, p>` is deferred**, not shipped: its extent is `deg(node(p))`,
  so the former needs a *path in type-argument position*, which is new type
  grammar rather than a new record. Not free, unlike its two siblings.
- **`preorder<S>` / `postorder<S>` are deferred**, and only the second is real
  work: `range<LeafIdx<S>>` and `range<NodeIdx<S>>` already cover iteration,
  while `postorder` is a genuine PERMUTATION rather than a range and needs a
  virtual-array kind with its own element mapping.

This also makes **tree × array hybrids free**, closing v10's open question 3
without new theory: a tree slot composes with ordinary slots like any other.

```blade
let field: Array<Float64 like CrystalIdx, Idx<3>>     // a 3-vector per tree cell
let block: Array<Float64 like CrystalIdx, SymIdx<2, 6>>  // a symmetric matrix per cell
let v = field(1, 2)          // Array<Float64 like Idx<3>> — trailing slot survives
```

### 3.7 Printing

A tree array is rank 1, so under the current print regime (rank 2 nested,
everything else flat) it prints its flat preorder sequence — consistent with
ragged rank-1 rows and with ranks ≥ 3. A structure-revealing `show_tree`
display form is deferred; v1 does not special-case printing, because a print
regime carve-out is a corpus-wide pin change for cosmetics.

---

## 4. (d) Graphs: adjacency as indexing, without `Trace` as a type

The v10 source's own summary survives the refresh intact, and is worth keeping
verbatim:

> "Graphs aren't data structures you build and then traverse. They're types
> with iteration semantics built in. … The algorithm is the type. The code is
> just `method_for`."

What changes is *where* the iteration semantics live: in the index types and
licenses that already exist, not in a new `Trace` index type.

### 4.1 [correction 1] `Trace<N>` is not an index type

The v10 motivation was real: cyclic structure makes the naive type infinite
(`T^I^I^…`), so `Trace<N>` was to accumulate visited addresses in the *type*
and collapse on revisit, stopping the regress.

**The regress does not arise in current Blade, so nothing needs to stop it.**
Formalism §3.10 already tags index values with their index type: iteration
over `Node` yields `i : Nat<Node>`, and an array whose *element* type is
`Nat<Node>` and whose *index* type is `Node` is closed under application:

```blade
type Node = Idx<64>
let succ: Array<Nat<Node> like Node>

succ(v)        : Nat<Node>
succ(succ(v))  : Nat<Node>          // types, at every depth, with no new type
```

`succ : Node → Node` composes with itself forever at one fixed type. The
infinite family `T^I^I^…` was an artifact of typing the *path*; Blade types
the *address*, and addresses are closed. Bounds safety comes along free — the
element is unit-tagged `Nat<Node>`, so it is accepted only by `Node`-indexed
arrays, which is formalism §3.10's guarantee applied to a graph.

What `Trace` was also doing — recording *which* nodes were seen — is a set that
grows with the data, i.e. **a value**. Putting it in the type would make the
type depend on runtime values, which is the one thing Blade's index types are
built to avoid ("bounds are values, not type parameters", formalism §3.1). So:

> **The visited set is data.** `Trace<N>` survives as the *name of a traversal
> result*, defined over ordinary arrays — never as an index type, and never as
> a typing event.

### 4.2 What a graph value's concrete type is

Adjacency is the indexing structure, not a side table — that claim is kept, and
it is now literally true because the adjacency array *is* a function
`Node → Node`:

```blade
type Node = Idx<64>
type Slot = Idx<4>
struct Edge { to: Nat<Node>, weight: Float64 }

// functional graph — exactly one successor
let succ: Array<Nat<Node> like Node>

// fixed out-degree K
let adj:  Array<Nat<Node> like Node, Slot>

// ragged out-degree — the natural spelling, and RaggedIdx already exists
let deg:  Array<Int64 like Node>
let adjr: Array<Nat<Node> like Node, RaggedIdx<deg>>

// weighted edges — a struct element, or a parallel array
let wadj: Array<Edge like Node, RaggedIdx<deg>>

// undirected: the edge SET is symmetric, so it is a symmetric index type
let w: Array<Float64 like SymIdx<2, 64>>       // edge(a,b) = edge(b,a), triangular storage
```

The v10 table `Idx<N>^Trace<N>`, `Idx<N>^Idx<K>^Trace<N>`, … maps onto these
line for line, with `Trace<N>` replaced by the ordinary node index and the
element type carrying the tag. `SymTrace<N>` is just `SymIdx<2, N>` over the
edge weights, which already has triangular storage and the `comm` license.

### 4.3 What a traversal expression's type is

A traversal produces arrays over the *step* axis, not over a trace type:

```blade
type Step = Idx<32>

let rec trail: Array<Nat<Node> like Step>            // the walk
let rec seen:  Array<Int64 like Step, Node>          // frontier per step
let stop:      Nat<Step>                             // collapse ordinal
```

A stdlib bundle keeps the `Trace` *name* at the value level (sketch — Blade has
no parameterized type aliases today, so v1 returns these as separate bindings
or a tuple; the named bundle is v2 surface):

```blade
struct WalkTrace {                    // one instantiation per (Node, Step) pair in v1
    trail: Array<Nat<Node> like Step>,   // addresses visited, in order
    seen:  Array<Int64 like Node>,       // the visited set — a value
    stop:  Nat<Step>                     // where collapse happened; = extents(trail) if never
}
```

Every question v10 answered with a collapse *event* is now a read on this
value, with no new machinery: reachability is `seen(target) == 1`; cycle
detection is `stop < extents(trail)`; shortest path is a `min` over
first-visit depths; connected components is `group_by` on the collapse target.

### 4.4 [correction 2] The hierarchy is a hierarchy of licenses

v10 stacked four index types — `Idx < TreeIdx < DAGIdx < Trace` — with
"algorithms written against the general type run on the specific ones". The
intent is right and worth keeping. The mechanism is wrong: `DAGIdx` and `Trace`
would be index types that do not determine storage, which is what an index type
*is* (formalism §3.9, "symmetry lives in the index type" — because it fixes
storage, iteration and access). Acyclicity fixes none of the three.

Acyclicity is exactly what `where` clauses are for: a claim about a value that
licenses an optimization. It belongs beside `comm`, `anticomm`, `omp`, `cuda`:

```blade
function tsort(adj: Array<Nat<Node> like Node, RaggedIdx<deg>>)
    where acyclic(adj) -> Array<Nat<Node> like Node> = { ... }
```

| Structure | Declared as | Licenses |
|---|---|---|
| `Idx<n>` | index type | affine addressing, `collapse(k)`, BLAS, vectorization |
| `TreeIdx<s>` | index type | O(depth) addressing, contiguous subtree views, structural fold with **no visited set** |
| DAG | value + `where acyclic(g)` | memoized fold (each node once); no visited set for *termination*, only for *sharing* |
| general graph | value, no clause | bounded walks only; visited set required |

This dissolves v10's open question 6 (DAG join semantics) rather than deferring
it: a join is just a node reached twice, and under `acyclic` the fold memoizes
instead of collapsing.

Unlike `comm`, `acyclic` is **checkable in O(V+E)**, so it is not a bare user
promise: static adjacency is verified at compile time, runtime adjacency gets a
construction-time check (a new runtime diagnostic — BL8010/BL8011 are already
occupied by interpreter panic codes, so the next registered slot, **BL8012**).
A license that can be checked should be checked.

The generality claim survives in its useful form: an algorithm written against
the general adjacency type runs on a tree's `parent`/`children` arrays too, and
the `where` clause is what makes the tree version faster.

---

## 5. (c) Traversal: what replaces the non-deterministic `for`

The v10 sketch was:

```
for i in Idx<n> { f(i) }           // bounded
for i in Trace<G> take k { f(i) }  // bounded by take
for i in Trace<G> { f(i) }         // Stream<T> or compile error
```

All three lines are now unwritable — the imperative `for` is removed (BL1003),
and `for` survives only as surface syntax over the loop constructors
(formalism §7.4). The replacement is not a new form. It is the observation
that:

> **`take k` is the recursion extent of a `let rec`.**

`let rec` already provides bounded sequential structure with a static extent,
implicit-zero history, guaranteed constant-stack compilation, and the ability
to fold the trajectory in the same sweep (formalism §7.5). A bounded walk is a
recurrence on the walker's state; that is the whole mapping.

### 5.1 Bounded walk — `take k` as an axis

```blade
type Node   = Idx<64>
type Walker = Idx<1024>
type Step   = Idx<32>                 // `take 32` IS this type

let deg:  Array<Int64 like Node>
let adjr: Array<Nat<Node> like Node, RaggedIdx<deg>>
let start: Array<Nat<Node> like Walker>
let draws: Array<Float64 like Step, Walker>          // pre-drawn uniforms

function hop(v: Nat<Node>, u: Float64) -> Nat<Node> =
    adjr(v, Int64(floor(u * Float64(deg(v)))))

let rec pos: Array<Nat<Node> like Step, Walker> =
    match pos with
    | zero        -> zero
    | zero :: s   -> zero :: start
    | prefix :: n -> prefix :: (method_for(prefix(n - 1), draws(n)) <@> hop |> compute)
```

(Probed 2026-08-26: the `let rec` element gate currently refuses Nat-tagged
elements — BL3999, Float/Int/Complex only — so the v1 spelling stores untagged
`Int64` walk state and casts at the read sites; lifting that gate is a P7
decision. See plan §10, probe G5.)

1024 walkers, 32 steps: one serial sweep over `Step` with a fully parallel map
inside it. Sequential structure is `let rec`, parallel structure is a loop
object, and the two nest in the one place where a walk actually has both. No
new construct, and the resulting code is a single pre-allocated buffer with no
recursion frames (TRMC, formalism §7.5).

### 5.2 Message passing — the second motivating case

Layers are the take:

```blade
type Layer = Idx<3>
type Feat  = Idx<16>

function gather(h: Array<Float64 like Node, Feat>, v: Nat<Node>) -> Array<Float64 like Feat> =
    reduce(method_for(adjr(v)) <@> lambda(w) -> h(w), (+))     // peel v's ragged neighbour row

let rec H: Array<Float64 like Layer, Node, Feat> =
    match H with
    | zero        -> zero
    | zero :: s   -> zero :: X0
    | prefix :: n -> prefix :: (method_for(range<Node>) <@>
                                lambda(v) -> mlp(n, gather(prefix(n - 1), v)) |> compute)
```

Cycles need no special handling: a message-passing round reads the whole
previous layer, so revisiting a node is *the point*, not a hazard. The v10
claim that GNNs need `Trace` for "natural cycle handling" is met by the layer
recurrence alone.

### 5.3 Collapse events are `guard`, and they already exist

v10 wanted `on_collapse` and `partition_on_collapse` as primitives. Both are
expressible in the MonadPlus layer that is already specified and implemented
(formalism §10.4): `guard(p, c)` yields `c` if `p` else **zeros of c's shape**;
`guard(p, guard(q, c)) ≡ guard(p && q, c)`; `c₁ <|> c₂` takes the first
non-zero. And `let rec`'s implicit-zero history is *the same zero*.

So collapse is: carry a `live` flag on the recursion axis, `guard` on it, and
everything after collapse is zero — which is exactly "collapse stops the
regress, signals termination, forgets the path" implemented as data flow.

```blade
// visited(n): per-walker indicator that the node reached at step n was seen before,
// derived from the trail/seen arrays of §4.3
let rec live: Array<Int64 like Step, Walker> =
    match live with
    | zero        -> zero
    | zero :: s   -> zero :: ones
    | prefix :: n -> prefix :: (method_for(prefix(n - 1), visited(n)) <@>
                                lambda(l, seen) -> l * (1 - seen) |> compute)

let collapsed_at = reduce(live, (+))            // steps taken before revisit
let ever_cycled  = reduce(live, (*)) == 0       // "did collapse occur?"
```

| v10 primitive | Now |
|---|---|
| `on_collapse` | `guard(live, c)` — post-collapse steps are zeros |
| "did collapse occur?" | `reduce(live, (*)) == 0` |
| shortest path / `track(min_depth)` | `reduce(first_visit, min)` over the step axis |
| `partition_on_collapse` | `group_by` on the collapse target (formalism §15.7, already implemented) |
| topological sort | `postorder<s>` collect, `reverse` |

No new combinator. That is the strongest single result in this revision: the
whole collapse vocabulary was a restatement of MonadPlus plus `group_by`.

### 5.4 Unbounded walks — refused, with one proposed construct

`Stream<T>` is refused in v1, and the refusal has a one-line justification in
Blade's own terms: *every Blade array type is a function from a finite address
domain; a value with no cardinality has no storage bijection, therefore no
index type, therefore no array type.* An unbounded walk is not a value this
language has.

The two things users actually want are both bounded:

1. **`take k`** — §5.1. Convergence becomes a *reported* fact (`stop`), not a
   control-flow fact. This covers random walks, MCMC, and fixed-layer GNNs.
2. **Iterate-to-fixpoint with a cap** — worth a construct, because the derived
   version always runs all k steps.

```
Δ ⊢ s₀ : τ    Δ ⊢ f : τ → τ    Δ ⊢ p : τ → Bool    k static, k : ℕ
──────────────────────────────────────────────────────────────────── (Fix-Bounded)
Δ ⊢ fix(s₀, f, until = p, max = k) : Comp[τ × Nat<Idx<k + 1>>]
```

```blade
let (h, iters) = fix(X0, layer, until = converged, max = 50) |> compute
```

Three properties make this admissible rather than a hole in the fence:
`max` is **mandatory and static**, so termination is still by construction;
the result carries the stopping ordinal, so convergence is observable; and the
whole thing is **derivable** — a `let rec` over `Idx<k>` with `guard(p)`
plus a final-slice read computes the same value. The *only* thing the primitive
buys is early exit from the sweep, which is a genuine performance argument and
the honest justification for making it a primitive rather than a stdlib
`static function`. Hence: ship the derived version in v1, promote it in v2
when a measurement says the wasted steps matter (§7.2).

### 5.5 The bottom-up fold is `let rec`, not a new combinator

This is the payoff of the preorder invariant (§1.2). In preorder a node's
descendants occupy a contiguous block *after* it, so **reverse preorder visits
every child strictly before its parent**. A structural bottom-up fold is
therefore a `let rec` over the reversed node axis:

```blade
type Nodes = NodeIdx<crystal>

let rec up: Array<Float64 like Nodes> =
    match up with
    | zero -> zero
    | prefix :: n -> prefix ::
        combine(own(n), reduce(method_for(range<ChildIdx<crystal, n>>) <@>
                               lambda(c) -> prefix(child_pos(n, c)), (+)))
```

And **implicit-zero history is exactly the leaf base case**: a leaf has no
children, the child loop is empty, and formalism §7.5's zero supplies the
identity with no `if` and no explicit base arm — the same way an AB3 integrator
states its startup transient in its weights instead of guarding the call site.

`postorder<crystal>` is offered as a virtual array so the ordinal arithmetic
above is written once in the stdlib rather than at every call site. Top-down
(parent-before-child) folds are the same thing over `preorder<crystal>`.

---

## 6. (e) Symmetric trees

### 6.1 [correction 4] The uniform case is `SymIdx` and is already done

v10: "a symmetric tree has commutative children: `T[(0,1)] == T[(1,0)]`;
canonical form = sorted path; storage reduces to canonical orderings."

That claim is true **exactly when the tree is uniform** — same depth
everywhere, same branching per level. Then `TreeIdx<uniform(r, n)> ≅ Idx<n>ʳ`,
paths are r-tuples over one child space, sorting a path is meaningful, and the
resulting type is `SymIdx<r, n>`: triangular/simplicial packed storage,
cardinality C(n+r−1, r), canonicalizing access, r! iteration savings — all
implemented, all proved (formalism §12, proofs.md §Binomial).

```blade
// A uniform symmetric tree is not a new type. It is this:
let A: Array<Float64 like SymIdx<3, 8>>
```

For a **non-uniform** tree the claim is false, and this is the substantive
correction: `T(1, 2)` means "child 1 of the root, then child 2 of *that node*",
while `T(2, 1)` means "child 2 of the root, then child 1 of *that* node". Those
are different cells unless both levels range over the same child space — i.e.
unless the tree is uniform. Sorting a path in a non-uniform tree is not a
canonicalization; it is a different address.

(The multi-level analog for uniform nests — nested symmetric/antisymmetric
groups with mixed characters — is also already implemented, as
`OrbIdx<[(r,s),...], n>` iterated wreath classes.)

### 6.2 Sibling symmetry: the real non-uniform notion

The meaningful non-uniform symmetry is **sibling exchange**: a node whose `a`
child subtrees are shape-isomorphic and value-interchangeable.

```blade
let static mol = [ sym[leaf, leaf, leaf],      // three interchangeable H
                   leaf ]                       // the O
```

`sym[...]` marks a node's children as commutative; it is shape metadata,
carried alongside the degree sequence in the tag payload (one extra bit per
node), and it is inert on shapes where every child subshape is not identical
(a construction error, since the claim would be meaningless).

**What it does *not* buy is storage.** Canonicalizing `SymIdx<2,n>` works
because `(i,j) ↦ (min, max)` orders *indices*, and indices are ordered by
construction. Ordering `a` interchangeable *subtrees* requires a total order on
subtree **values**, which is data, not addresses. A storage geometry that
depended on data would break the index-type contract (a bijection fixed at type
level) and would be invalidated by any write.

> **The tree analog of triangular storage does not exist for sibling symmetry,
> and this is a theorem-shaped fact, not a gap: canonicalization needs an index
> order, and sibling exchange only offers a data order.** Storage savings return
> exactly in the uniform case, where the type is already `SymIdx`.

This closes v10's open question 2 with an answer.

### 6.3 What the `comm` license does mean for tree kernels

Sibling symmetry buys **iteration**, on the existing license surface with no
parallel mechanism:

```blade
function pair_energy(a: Array<Float64 like Feat>, b: Array<Float64 like Feat>)
    where comm(a, b) -> Float64 = ...

// over the sibling pairs of a symmetric node: triangular, a(a+1)/2 not a²
let e = method_for(siblings<mol, p>, siblings<mol, p>) <@> pair_energy
```

Semantics are formalism §11's, unchanged and un-carved-out:

- Triangular iteration is licensed **only when the same array occupies both
  commuting positions** — sibling slots of one node do; two different nodes'
  sibling sets do not, and that refusal is the proved one.
- A commutative + associative fold at a symmetric node additionally licenses
  reassociation, so `where omp(node: d)` parallelises across independent
  subtrees. Independent subtrees are the natural parallel unit of a tree, and
  the preorder-contiguity invariant means each one is a contiguous slab — so the
  parallel decomposition is also the cache decomposition. Depth-capped like
  every other `omp` clause.
- `reynolds(g)` manufactures a sibling-commutative kernel by symmetrization
  when one cannot be annotated, exactly as elsewhere.

### 6.4 Where storage compression actually comes from

For trees, the compression analogous to triangular packing is **subtree
sharing**: store structurally-identical subtrees once. That converts the tree
into a DAG, which is §4.4's territory — so the two halves of this document meet
here, and "symmetric-tree storage theory" resolves into "hash-consing", a known
technique with a known cost model. Deferred (§7.3), but no longer mysterious.

---

## 7. (f) v1 / v2 / deferred

### 7.1 v1 — implementable on today's machinery

Nothing here needs new theory; every item names the existing mechanism it
extends.

| # | Item | Rides on |
|---|---|---|
| 1 | `TreeIdx<shape>` static shapes; `leaf` + nest surface; `degrees(...)` route | `IrrepsIdx<spec>` (`let static` + parameterized tag `IxKIrreps`) |
| 2 | `IxKTree` kind, `__tree:<name>:<payload>`, degree-sequence hash identity | `IxKIrreps` tag discipline; `CompoundIdx` whole-mask hash |
| 3 | Flat preorder storage: pool + `off` + `deg` + `size` | `Ragged<T>` CSR descriptor (a superset; `off` is reused as-is) |
| 4 | Path currying `T(c)`, contiguous subtree views | dimensional currying (formalism §9); `RaggedRow<T>` peel |
| 5 | Whole-path reads `T(p)`, `Path<s>` as a pack | `SparseIdx` one-slot tuple application |
| 6 | Static-path leaf/internal resolution; BL4019 on indeterminate depth | BL4003 index-type violation |
| 7 | Dynamic child bound via `BoundedIdx<0, arity>`; BL8003 at runtime | `RaggedIdx` `lens[__g]` runtime peel |
| 8 | Derived dense axes `LeafIdx`/`NodeIdx`/`ChildIdx` + `preorder`/`postorder` virtual arrays | `range<I>`, `reverse<I>` (formalism §7.3) |
| 9 | `reduce` over a tree (flat axis); structural fold as `let rec` over `postorder` | formalism §7.5, incl. implicit-zero as the leaf base case |
| 10 | Tree × array hybrids (`Array<T like TreeIdx<s>, Idx<m>>`) | slot composition; free **in the type — measured at P3+P4: the annotation is accepted and allocates as an ordinary rank-2 dense pool, but the READ refuses, because the path fold rewrites the whole subscript to one literal and a residual trailing coordinate has nowhere to go in that rewrite. The residual-view mechanism is P5's.** |
| 11 | Adjacency graphs as `Array<Nat<N> like N, …>`; bounded walks; `guard`-based collapse | formalism §3.10 unit tagging; §10.4 MonadPlus |
| 12 | `where acyclic(g)` license, checked not promised; BL8012 | `where comm(...)` surface |
| 13 | `fix(...)` as a stdlib `static function` over `let rec` + `guard` | formalism §7.5 |
| 14 | `retree` / `flatten` transforms between ragged and depth-2 tree | formalism §3.7 index transforms |

**Prototyping route before `IxKTree` exists.** The formalism specifies an
`unsafe indextype` escape hatch: a user-supplied
`canonical : indices → Option<canonical>` (`None` = implicit zero) plus a
`transform` for non-canonical access. A tree's `forward` bijection fits that
signature, so the addressing scheme (§1.2) can be validated — and a corpus
written against it — before a native kind is added. Note it is *spec-level*:
it appears in `docs/formalism.md` and in no `.fs` or `.blade` file, so
implementing the hatch is itself work, and it is only worth doing if more than
one custom index type is queued behind it.

**Gates to clear before committing to v1** (each is a check, not an
assumption):

- **G1** — `Array<Nat<I> like J>`: index values as array *elements*. This is
  load-bearing for §4 (it is what makes `succ(succ(v))` type) and is *not*
  demonstrated anywhere in the corpus; `src/IR.fs:209` mentions the shape in a
  comment only. Verify, and note that index-tag *arithmetic* is separately
  forbidden by design (BL4003/BL4010) — the requirement here is storage and
  round-tripping, not arithmetic.
- **G2** — struct/tuple elements under `let rec`. Walk state wants it; the
  `structs/` corpus has struct arrays (006, 009, 011) but the recursive-array
  corpus is scalar/`Float` only. The §5.1 spelling avoids it by using parallel
  recursive arrays; confirm that stays sufficient.
- **G3** — the diagnostic codes proposed here (BL4019, BL4021, BL8012, and the
  `sym[...]` construction error) each cost five touch points including the
  hand-authored `diagnostics.json`, guarded only by the full-suite Surface
  block. Budget for it — and prefer joining an existing code family (the
  BL4003 / BL8003 route Sparse and Orb took) wherever one fits.
- **G4** — non-affine tree bounds refuse `collapse(k)` on the node axis, as
  ragged rows already do. Confirm the derived dense axes recover it, which is
  the whole reason §3.6 exists.

### 7.2 v2 — modest new work, clear shape

- **`DynTreeIdx`** — runtime shapes, the `RaggedIdx` lane of the family. The
  consumption half is already dynamic (a CSR descriptor read at runtime); what
  is missing is a *producer*, which is precisely the finding of
  `plan-llvm-runtime-shapes.md` §0 for ragged. Same fix, same order:
  dynamic-first.
- **`fix` as a primitive** — promote from stdlib when a measurement shows the
  wasted steps of the derived form matter.
- **Sibling symmetry** — `sym[...]` payload bit, construction-time
  normalization, and the `comm` iteration license (§6.3). No storage change.
- **Memoized DAG fold** under `where acyclic` — each node evaluated once,
  sharing via the visited array.
- **`Path<s>` as a first-class value** — pack access `p[k]`, path literals,
  and the refusals (path arithmetic, cross-shape paths). Plus the named
  `WalkTrace` bundle (§4.3), pending parameterized aliases or a per-use
  monomorphic struct.
- **Structural writes** — `mut` subtree parameters. Element writes alias the
  caller (array-only, per the `mut` rules), and a subtree view is an array, so
  this should fall out; needs a corpus.
- **`show_tree`** display form.

### 7.3 Deferred — genuinely open

1. **`Stream<T>` and unbounded walks.** Blocked on a value with no cardinality
   (§5.4). Would need a termination-policy theory Blade does not have.
2. **Subtree sharing / hash-consing.** The real storage win (§6.4). Needs a
   cost model for the identity table and a rebuild story.
3. **Dynamic trees** — insertion/deletion against flat storage; buffer growth
   and O(n) rebuild policy.
4. **Distributed trees** — does the product-simplex decomposition generalize?
   Preorder contiguity makes subtree slabs the obvious unit, which is
   encouraging but not an argument.

### 7.4 Answered, and removed from "open"

- **AD through tree/graph structure** (v10 open Q4). Split it and both halves
  resolve: gradients **with respect to values** are already solved, because
  storage is a flat dense axis and every existing AD rule applies to it
  unchanged. Gradients **with respect to structure** do not exist — a degree
  sequence is not in a differentiable space, and there is no perturbation of "a
  node has 3 children" — so the compiler should *refuse* a `grad` whose active
  variable is a shape, rather than silently returning zeros. That refusal is the
  correct answer, not a limitation.
- **Tree × array hybrids** (v10 open Q3) — free in v1, §3.6.
- **DAGIdx join semantics** (v10 open Q6) — dissolved, §4.4.
- **Symmetric-tree storage theory** (v10 open Q2) — answered negatively for
  sibling symmetry, §6.2; the uniform case was always `SymIdx` (and the
  uniform *nested* case is `OrbIdx`).
- **`Stream<T>` termination policies** (v10 open Q7) — narrowed to §7.3 item 1
  and given a bounded substitute with a typing rule, §5.4.

---

## 8. Applications, restated in current notation

- **Graph neural networks** — §5.2. Layer recurrence over
  `Array<Nat<Node> like Node, RaggedIdx<deg>>`; connects to
  [equivariant-nn.md](equivariant-nn.md) §10's worked scatter/gather example,
  where the gather is the ragged peel.
- **Hierarchical scientific data** — nested grids, AMR patches, octrees. The
  motivating case, and the one v1 fully serves: static shape, frequent access,
  rare structural change.
- **Dependency resolution** — `Array<Nat<Pkg> like Pkg, RaggedIdx<ndeps>>`
  with `where acyclic`; a circular dependency is the license check failing
  (BL8012) rather than a collapse event, which is a better error.
- **Random walks / MCMC** — §5.1; batched over walkers, bounded by the step
  axis.
- **State machines and event loops** — bounded `let rec` over steps; a stuck
  state is a fixed point of the step function, observable as `fix`'s returned
  ordinal.
