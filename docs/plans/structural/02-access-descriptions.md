# 02 — Share access descriptions across demand, adjoints, and ownership

Status: DESIGN 2026-09-06 — nothing built. Elaborates item 2 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md).
Source snapshot: master at `e7abf38` (working tree clean under `src/`, `tests/`).
Every `file:line` below was read at that snapshot; every emitted-C++ claim comes
from `Blade.exe emit`/`run` in a private temp directory. Claims marked
**(inferred)** were not executed.

Companions: [plan-distributed-memory.md](../plan-distributed-memory.md) (§4.3 derives
ghost depth from the same halo tag this document names),
[plan-ad-combinators.md](../plan-ad-combinators.md) §2.9 (the standing REV verdict on
`halo`, which §3.3 here supersedes without changing its Tier-1 conclusion),
[plan-fortran-killer-2.md](../plan-fortran-killer-2.md) §3 (shared legality facts).

## 0. What this document claims

One compiler-internal record — *which input ordinals an output ordinal's kernel
reads* — already exists in Blade for the fixed-halo case: it is the
`"__halowin|d:<inner>|<o1,o2,..>"` slot tag minted at `src/TypeLower.fs:1601`.
What does **not** exist is one *reader* of that record. Nine sites re-derive
its arithmetic and four of them re-scan the kernel body for the `w(o)` read
shape with their own pattern matchers (§1.4). None of them checks that the
offsets read are the offsets declared, and that gap is a verified silent
out-of-bounds read (§1.5).

The proposal is deliberately small: (i) parse the tag once into a `HaloAccess`
record and give it a forward image (demand) and a transpose (preimage); (ii)
one shared kernel-body scan for window reads; (iii) two consumers in the first
gate — the forward demand behind the existing extent guards/carousel plus a
streamed-source ring, and a race-free backward **gather** for the halo
cotangent that reverse-mode AD refuses today (BL5500) — built as a cost-only,
escapable rewrite of the scatter route, and **bitwise identical** to it by
construction (§3.3.4). Compact (SymIdx/AntisymIdx/HermitianIdx) reconstruction
is specified (§2.5) as the record's storage image, not built: reverse mode
refuses `range<SymIdx>` outright today (`src/GradNormalize.fs:221`,
`tests/corpus/ad-jvp-comb/104`), so there is no consumer to serve yet.

Recommendation: **GO** for the first gate (§6).

## 1. Verified current state

### 1.1 How a halo window is typed, lowered, stored and emitted

**Surface and typing.** `halo<I, [o..]>` is a parse-time type-position form
(`src/ParserTypes.fs:181-190`, deliberately *not* admitted by `parseIndexType`:
`Array<T like halo<..>>` is illegal because a halo is a traversal transformer,
not storage) and an expression form (`src/ParserGrammar.fs:849-859`,
`src/Ast.fs:465`). Both reach `haloSlotsOf` (`src/TypeLower.fs:1560-1636`)
through the `ExprRange` arm (`src/TypeCheckInfer.fs:1318-1327`) or the
`ExprHalo` arm (`:1437-1439`). For a dense inner index it produces ONE slot
per axis:

- `lo = min 0 (min offs)`, `hi = max 0 (max offs)`, `shrink = hi - lo`; the slot
  extent is `N - shrink` (`:1587-1594`) — the reach always includes the implicit
  center 0, which is why lag sets like `[-2, -4]` are legal
  (`tests/corpus/loops/078`);
- `Tag = "__halowin|d:<innerName>|<o1,o2,..>"`, `IxKind = IxKPlain`
  (`:1600-1602`). A compound inner keeps `IxKCompound` and tags `"__halowin|c:|.."`
  with an unshrunk extent, the shrink riding to the loop bound (`:1569-1582`).
- Offsets must be a compile-time int array; anything else refuses at
  `:1603-1636`.

The window parameter `w` is typed `IRTIdxTagged (Int64, IRefNamed "__halowin|..")`
(`src/TypeLower.fs:1270-1276`, `elemTypeForIterationIndex`), so it erases to
`int64_t` in C++. Three tag helpers re-derive facts from the string:
`(|HaloWinTag|_|)`, `haloStartOffsetOfTag` (= `max 0 (-(min 0 (min offs)))`) and
`haloShrinkOfTag` (`src/Types.fs:272-304`).

**Lowering.** A window read `w(o)` is `TExprApp` whose head type carries the tag
(`src/Lowering.fs:441-466`). Dense: `IRBinOp (IRElementwise, IRAdd, w, o)` — a
plain add, no check that `o` is in the declared set (the comment at `:445-448`
says "BndShrink guarantees in-bounds"; that is a *precondition on `o`*, nowhere
enforced — §1.5). Compound: `IRHaloUnhash (w, o)` with `o` required literal,
else `failwith` (`:464` — an internal crash, not a diagnostic).

**Storage / loop structure.** The slot becomes a `VirtualRange (Some start)`
element binding whose start offset is re-read from the tag
(`src/IRStorage.fs:906-915`); `SlotTag` carries the payload to the peel
(`:41-45`). A compound-inner halo folds its shrink into `StrictOffset`
(`:1105-1116`). The output axis drops the tag and becomes the plain dense
interior (`src/IRLoopStructure.fs:818-830`).

**Emission.** The peel binds the true center, `int64_t w = (__i0 + 1L);`
(`src/CodeGenLoopNest.fs:92-115`; interpreter twin `src/Interp/Loops.fs:1691-1697`).
`planHaloCarousel` (`src/CodeGenLoopNest.fs:1237-1363`) hoists dense window
reads into a rotating ring when the innermost level's sole element is a dense
halo: it scans the kernel body for `IRIndex (IRVar aid, [..; IRBinOp (IRAdd, IRVar wid, lit)])`
(`:1293-1313`), groups by array + rendered prefix, requires span ≤ 8 and ≥ 2
distinct offsets, then emits warm-up loads and one load per step. It bails on
Reynolds, parallel levels, MPI slabs, streamed sources and dynamic starts
(`:1241-1258`). `flatShapeSignature` refuses `__halowin` tags because the reads
are offset from the loop index (`:2086-2108`). MPI: `classifyMpiShape`'s
`allReal` fails for any `VirtualRange`, so every stencil is MPI-ineligible
(`src/CodeGenCuda.fs:1731-1734, 1754`). Streamed sources: `genElementBindingStreamed`
refuses virtual bindings outright (`src/CodeGenLoopNest.fs:407-408`,
"virtual/compound binding shapes are not stream-eligible").

**The two extent guards.** TypeCheck's `haloExtentClash`
(`src/TypeCheckInfer.fs:8647-8708`, inside `buildApplyInfo`) recovers the
declared extent as `shrunk + haloShrinkOfTag` and walks the typed body for
`TExprIndex`/array-headed `TExprApp` sites whose index argument is
`TExprApp (f, _)` with a `"__halowin|d:"`-tagged `f`; literal-vs-literal
disagreement is `HaloExtentMismatch` → BL3016 (`src/TypeEnv.fs:731, 937`).
Codegen's runtime twin (`src/CodeGenCuda.fs:2765-2830`) redoes the same over IR
with its own `haloTagOfIdx` on `IRBinOp (IRAdd, IRVar/IRParam tagged, _)` and
emits one `if (X.extents[d] != N) { .. blade_rt::panic("BL8009", ..) }` before
the nest; the interpreter twin is `src/Interp/Loops.fs:840-885`. Corpus:
`loops/161` (BL3016), `162/163` (BL8009 aborts).

### 1.2 Emitted shape of a stencil (verified)

`tests/corpus/loops/072_halo_central_difference.blade`
(`method_for(halo<Idx<5>, [-1, 0, 1]>) <@> lambda(w) -> A(w(1)) - A(w(-1))`),
emitted to a private directory, run → `d = [3, 5, 7]`. The nest (lines 186-203
of the emitted file):

```cpp
if ((int64_t)(A.extents[0]) != 5LL) { ... blade_rt::panic("BL8009", "halo extent mismatch", nullptr, 0); }
Array<double, 1> d = { allocate<...>(d_extents), d_extents };            // extent 3
// halo carousel: A window [-1..1] -- ring of 4, head = __i0, one write/step
std::array __car_d_0{ A[(size_t)0L], A[(size_t)1L], A[(size_t)2L], A[(size_t)2L] };
for (size_t __i0 = 0; __i0 < 3; __i0++) {
    int64_t w = (__i0 + 1L);
    d[__i0] = (__car_d_0[(__i0 + 2UL) & 3UL] - __car_d_0[(__i0 + 0UL) & 3UL]);
    __car_d_0[(__i0 + 3UL) & 3UL] = A[(size_t)(w + 2L)];
}
```

Note the tail load on the last iteration: `w = 3`, `A[(size_t)(3 + 2)] = A[5]` on
a 5-cell pool — the carousel prefetches the *next* step's newest cell and never
guards `__i0 + 1 < M`. The value is discarded, but the read is one element past
the allocation on every dense carousel nest (the formula
`(M-1+start) + 1 + maxOff` is always exactly `N`). Verified by reading the
emitted text; not observed to fault.

### 1.3 Reverse-mode AD today

**Halo maps are refused.** The eager-map lowering that gives reverse mode its
map capability (`src/GradNormalize.fs:12-24, 160-310`) classifies operands at
`:210-250`; its `ExprHalo` arm (`:245-246`) errors: *"reverse mode cannot lower
a `halo` stencil map (v1): the transposed stencil's adjoint needs a zero-Pad
boundary the surface has no spelling for -- use `ad.jvp`"*. Verified: a
`grad(f)` over the §1.2 stencil fails `blade check` with `error[BL5500]` and
that message. `plan-ad-combinators.md:482-495` records the same verdict
("Tier-1 scatter-add ... REV needs a `halo` boundary parameter").

**A data-dependent read's adjoint is a scatter with accumulation.** `adjointOf`'s
array-read arm (`src/GradSweeps.fs:157-160`) emits
`accum (__g_a(idxs)) cot` = `NAssign (t, add t cot)` (`:136-137`).
`tests/corpus/ad/012_gather_foreign_key_grad.blade` pins the multiplicity
(`dr = [2, 4, 2]` for `srcs = [0, 1, 1, 2]`); its emitted C++ (verified run,
`rv = 160`) is the serial scatter

```cpp
for (int64_t __k25 = 0L; __k25 < 4L; __k25++) {
    __v16[__k25] = (w[srcs[__k25]] * 2.0);
    ...
    __g_w[srcs[__k25]] = (__g_w[srcs[__k25]] + (__v26 * 2.0));
}
```

No inverse map is built; there is no gather form anywhere in the AD lane.

**The lane's loop model.** Statements are `NStmt = NLet | NAssign | NFor`
(`src/GradCommon.fs:170-173`). A lowered map is a construction loop
`let mut m = zeros; for __mi in 0..n { m(__mi) = BODY[p := read] }`
(`src/GradNormalize.fs:300-309`, capped at 65536 cells, `:88, :270`). Its
adjoint is the NFor replay arm (`src/GradSweeps.fs:490-523`): replay the body,
declare loop-local cotangents, run the body's adjoints reversed; an array-cell
overwrite becomes "save cotangent, zero it, flow into rhs" (`:472-489`).
`toStmts` renders NFor back to `StmtForIn` (`:1092-1101`), which TypeCheck
accepts for synthesized code (`src/TypeCheckInfer.fs:11407`; user-written
`for` is BL1003), lowers to `IRForRange`, and is emitted by
`genForRangeBinding` (`src/CodeGenBinding.fs:277-278, 3863`) as a plain
`for (int64_t ..)` with **no pragma**; the interpreter evaluates it at
`src/Interp/Core.fs:609`. The `ad` category is in the interpreter differential
gate (`tests/InterpDiff.fs:179`). Emitted branches are allowed in adjoint code:
the guard arm emits `ExprIf (c, cot, 0.0)` (`src/GradSweeps.fs:226-227, 279-281`).

**The C6 reindexing family is the template.** `adjointOfInit`
(`src/GradSweeps.fs:271-420`) flows a combinator-built local's cotangent back
through the form's *transposed reindexing* (`transpose` = swap, `stack` = peel,
`join` = offset shift, `guard` = same gate), reading the cotangent through a
composed `cotAt`. It dispatches only on combinator forms (`:398-420`); a map
apply is not one of them.

**JVP handles halos by capture-read.** `tangentOfMapCore` passes a halo
operand through untouched (`src/GradSweeps.fs:829`, `:990-996`), refuses a
rank-carrying parameter over it (`:909-913`; `tests/corpus/ad-jvp-comb/074`),
and re-derives the shrunk extent from surface literals in `staticExtentOf`
(`src/GradExpand.fs:1334-1345`). Verified emission of `ad-jvp-comb/014`
(`v = 6, t = 0`): the tangent kernel is
`__lambda_40(int64_t w, Array<double,1>& __t_a) { return (__t_a[(w + 1L)] - __t_a[(w + -(1L))]); }`
with its own carousel over `__t_a` and its own BL8009 guard on `__t_a`.

### 1.4 Nine re-derivations, four body scanners

| # | Site | Re-derives | Own body scan |
|---|---|---|---|
| 1 | `TypeLower.haloSlotsOf` `src/TypeLower.fs:1586-1594` | lo/hi/shrink from the offset list | — |
| 2 | `Types.haloStartOffsetOfTag` / `haloShrinkOfTag` `src/Types.fs:292-304` | start/shrink from the tag | — |
| 3 | `TypeCheckInfer.haloExtentClash` `:8647-8708` | declared = shrunk + shrink | typed `TExprApp(f tagged, _)` |
| 4 | `CodeGenCuda` BL8009 guard `:2779-2830` | same, in IR | `haloTagOfIdx` on `IRBinOp(IRAdd, tagged, _)` |
| 5 | `CodeGenLoopNest.planHaloCarousel` `:1254-1313` | start from `VirtualRange` | `offOf`/`scan` on `IRIndex(.., IRAdd(IRVar wid, lit))` |
| 6 | `Interp/Loops.fs` BL8009 twin `:847-885` | same as 4 | `haloTagOfIdx` twin |
| 7 | `GradExpand.staticExtentOf` `:1334-1345` | shrink from surface literals | — |
| 8 | `IRStorage` element binding `:906-915` | start from the tag | — |
| 9 | `IRStorage` compound `StrictOffset` `:1105-1116` | shrink from the tag | — |

The distributed-memory plan would add a tenth (`ghost depth` from the same two
helpers, `plan-distributed-memory.md` §4.3 step 1).

### 1.5 A verified soundness hole: reads outside the declared set

Probe (private dir), `blade check` → OK, `blade run` → completes:

```blade
type H = Idx<5>
let a: Array<Float like H> = [1.0, 2.0, 4.0, 7.0, 11.0]
let d = method_for(halo<H, [-1, 0, 1]>) <@> lambda(w) -> a(w(2)) - a(w(-1)) |> compute
// printed: d = [6, 9, -4]
```

`d[2] = a(5) - a(2) = <garbage read as 0> - 4`. The declared set is `[-1, 0, 1]`;
`w(2)` is outside it, the interior was shrunk for reach 1, and the read at
ordinal 5 is one past the pool. The emitted carousel makes it worse: warm-up
loads `a[0..3]`, and the tail loads `a[(size_t)(w + 3L)]`, i.e. up to `a[6]`.
Nothing in TypeCheck (`haloExtentClash` compares extents only), Lowering (`:441-466`)
or codegen compares the read offset against the declared set. The interpreter's
`readCell` panics BL8003 on an out-of-range coordinate (comment at
`src/Interp/ArrayOps.fs:565`), so the two lanes **(inferred)** diverge on this
program — a latent differential red that no corpus test exercises.

A second, related hole: the carousel's unconditional tail prefetch (§1.2)
over-reads by one on *valid* programs. Both are the same missing fact — the
per-step demand of the access — and both close when a consumer reads the
description instead of assuming it (§3.2.1).

Scope note for the fix: `tests/corpus/loops/080_halo_delay_embedding.blade`
reads `S(w(0 - i))` with an offset *computed* from a sibling `range<Idx<3>>`
slot (in reach: `[-2, 0] ⊆ [-2, 1]`). A literal-only check keeps 080 green; a
computed offset needs an interval proof or a runtime guard (§3.1.3).

### 1.6 Compact (SymIdx / AntisymIdx / HermitianIdx) reads today

The reconstruction of a logical value from a canonical pool is implemented three
times, consistently:

- C++: `canon_fold<R>` (sort, swap parity, strict-diagonal zero flag),
  `canon_left_justify<R>`, `canon_transform` with `ReadTransform ∈ {Identity,
  NegateOnSwap, ConjugateOnSwap}` (`src/cpp/nested_array_utilities.hpp:685-741`);
  pair helpers `antisym_canonical` (returns 0 on the diagonal: "value is zero")
  and `hermitian_canonical` (`:641-650`).
- Codegen: `renderIndexExpr`'s `lazyCompactRead` emits fold → zero-guard →
  left-justify → fetch → transform chain, and only for *random* access — iteration
  reads are canonical by construction and keep the raw subscript
  (`src/CodeGenExpr.fs:495-640`).
- Interpreter: `canonFold` / `canonLeftJustify` / `applyReadTransform` /
  `readCompact` (`src/Interp/ArrayOps.fs:266-301, 501-565`).

None of these is an *access description*: they answer "where is A(i,j) stored
and with what sign", not "which pool cells does a kernel touch and how often".
Reverse AD has no compact consumer (`src/GradNormalize.fs:221`; the JVP C5
symmetric fast path runs over `range<SymIdx>` canonical cells,
`src/GradSweeps.fs:742-752`).

### 1.7 Provider windows and streams today

`ProviderReadSpec.Window : (int64 * int64) option` (`src/IR.fs:1369-1372`) is a
**user-written** sub-simplex `[lo, hi)` on a *packed* variable
(`alias.read_window(var, lo, hi)`, `src/TypeCheckInfer.fs:241-280`: requires
`lead.Symmetry <> SymNone && lead.Rank >= 2` and literal bounds; recovered at
`src/Lowering.fs:1820-1847`; emitted via `PackedReadOpts.Window`,
`src/providers/ProviderRegistry.fs:26-34`, `src/CodeGenBinding.fs:1383-1435`;
zarr skips tiles disjoint from the window at `src/providers/ZarrProvider.fs:1597-1603`
and extracts the sub-simplex at `:1760-1810`). No dense window exists and no
window is ever *derived* from a consumer.

`alias.stream(var)` (`Streamed`, `src/IR.fs:1373-1377`) is the one place where
an input is not materialized: the nest reads one trailing-axis fiber per site
at the S/T boundary (`genElementBindingStreamed`, `src/CodeGenLoopNest.fs:330-408`;
zarr `genStreamOpen`/`genStreamFiber`, `src/providers/ZarrProvider.fs:1857-1948`,
one `seek+read` per t-chunk). Zarr is pure `std::fstream` C++ with no link
dependency (`:5`), uncompressed only (`:8-9`), and its harness is hermetic
(`tests/ZarrTests.fs:5, 557-595`, stores generated on the fly). A halo over a
streamed source is refused today (`:407-408`).

### 1.8 What the distributed-memory plan already assumes

`plan-distributed-memory.md` §4.3 derives ghost depth `(-min offs, max offs)`
from `haloShrinkOfTag`/`haloStartOffsetOfTag`, neighbour ownership in closed
form, and an interior/boundary split — all functions of the same tag. Its §7.10
refusal ("a read `A(e)` on a distributed axis where `e` is not the iteration
index or a `halo` window offset") is precisely "the access is not in the closed
family". That plan is P2 there and unbuilt; this document gives it the record
it would read (§3.5), nothing more.

## 2. The abstraction

### 2.1 The closed family

```fsharp
/// Which INPUT ordinals one OUTPUT ordinal's kernel may read, per operand axis.
type Access =
    | AccIdentity                       // out i reads in i               (elementwise, zip)
    | AccPermute of perm: int list      // out (i..) reads in (perm i..)  (transpose)
    | AccHalo of HaloAccess             // out i reads in { i + Start + o : o ∈ Reach }
    | AccGather of GatherMap            // out i reads in g(i), g a runtime int array

and HaloAccess = {
    Inner: string          // wrapped index alias ("" when anonymous)   -- from the tag
    Offsets: int list      // the DECLARED set, as written              -- from the tag
    Start: int64           // max 0 (-(min 0 (min Offsets)))            -- = haloStartOffsetOfTag
    Shrink: int64          // (-(min 0 (min Offsets))) + max 0 (max Offsets) -- = haloShrinkOfTag
    IsCompound: bool       // "c:" tag: ordinals walk PRESENT cells
}
/// Reach = Offsets ∪ {0}: the center is always readable (TypeLower.fs:1585-1586).

and GatherMap = { Index: Expr (* the int array *); InExtent: int64 option }
```

`InExtent N` and `OutExtent M = N - Shrink` are properties of the *site* (the
operand's extent and the slot's), not of the access, and travel with it as a
pair `(Access, N)`.

The family is closed under exactly the operations §2.2 lists; anything else
(computed offsets `w(0 - i)`, data-dependent subscripts, `reverse<I>`,
`blocked<I,K>`) is **`AccUnknown`** — a legitimate access with no description.
Consumers must treat `AccUnknown` as "decline", never as "reads nothing"
(memo: "An unknown preimage must decline this schedule; it must never be
treated as empty").

### 2.2 Composition

For `g ∘ f` (f produces the array g consumes) on one axis:

| f \ g | Identity | Permute q | Halo h₂ | Gather |
|---|---|---|---|---|
| Identity | Identity | Permute q | Halo h₂ | Gather |
| Permute p | Permute p | Permute (q∘p) | Halo h₂ on axis p⁻¹ | Unknown (v1) |
| Halo h₁ | Halo h₁ | Halo h₁ on permuted axis | Halo with `Offsets = { a + b : a ∈ Reach₁, b ∈ Reach₂ }`, `Start = Start₁ + Start₂`, `Shrink = Shrink₁ + Shrink₂` | Unknown (v1) |
| Gather | Gather | Unknown (v1) | Unknown (v1) | Unknown (v1) |

The halo∘halo rule is the overlapped-tiling identity; `>>@` composition of
stencil kernels does not exist in the surface today, so this row is *specified*
for the revision-propagation and fusion consumers and has no v1 client.
Gather composes only with identity in v1 (composition of index arrays is a
runtime gather of a gather; nothing decides it statically).

### 2.3 Transpose (preimage)

The transpose answers: which OUTPUT ordinals read INPUT ordinal `j`, and through
which offset.

- `AccIdentity`ᵀ = `AccIdentity`; `AccPermute p`ᵀ = `AccPermute p⁻¹`.
- `AccHalo h`ᵀ over `(N, M)`:
  `contributors h M j = [ for o in Reach -> (o, j - Start - o) ] |> filter (fun (_, i) -> 0 ≤ i < M)`.
  It is again a fixed-offset access, with offsets `{ -Start - o }` over the
  input domain `[0, N)`, whose output domain is *partial*: some `j` have fewer
  than `|Reach|` contributors (the shrunk boundary), and every `j ∈ [0, N)` has
  at least one when `Reach` spans the shrink (always true: `0 ∈ Reach`).
- `AccGather g`ᵀ requires the inverse CSR of `g` (row pointers over `[0, N)`,
  entries the `i` with `g(i) = j`, built in O(E) at runtime, duplicates kept in
  ascending `i`). It is **unknown until built**; a consumer that cannot afford
  to build it declines to the scatter form, which is what the lane emits today
  (§1.3). Not in the first gate.
- `AccUnknown`ᵀ = `AccUnknown` → decline.

### 2.4 Boundary semantics

Forward: the only boundary Blade has is **Shrink** (`src/TypeLower.fs:1587-1594`,
`plan-ad-combinators.md:485-486`, `plan-distributed-memory.md` §4.3.5 proposes
`periodic`/`clamp`/`reflect` "serially first"). The record therefore carries no
boundary field in v1; adding one later is a field, not a redesign.

Transpose of Shrink is **ZeroOutside**: an input ordinal's contribution from a
would-be output ordinal outside `[0, M)` is exactly zero — no value exists
there. This is the "zero-Pad boundary" `GradNormalize.fs:246` says the surface
cannot spell. It does not need a surface spelling: the AD lane emits statements,
and a guarded term `if 0 <= i && i < M then t else 0.0` is admitted adjoint
code (§1.3). It is the array-side twin of the recursive array's implicit zero
history (`docs/formalism.md:954-962`).

If forward boundaries are added later: Clampᵀ has variable contributor
multiplicity at the edges (the clamped cell is read by every clamped ordinal);
Periodicᵀ is modular. Both are still fixed-offset preimages; both must be
specified before any consumer accepts them. A consumer meeting an unspecified
boundary declines.

### 2.5 The canonical-storage reconstruction record

An access over a compact pool composes with the pool's *reconstruction*, which
is exactly what `canon_fold`/`canon_left_justify`/`canon_transform` compute
(§1.6), named:

```fsharp
type Reconstruction = {
    Storage: int64 list   // canon_left_justify (sorted tuple, strict)
    Parity: int           // swap parity of the sort (0 / 1)
    Sign: int             // -1 iff Antisymmetric && Parity = 1   (NegateOnSwap), else +1
    Conj: bool            // Hermitian && Parity = 1                (ConjugateOnSwap)
    Zero: bool            // strict group with a repeated coordinate: no pool cell, value 0
}
```

Forward image (what the runtime already does): `value = transform(pool[Storage])`.
Reverse action (what no consumer does yet): for a logical read with cotangent
`c`, `Zero → nothing`; otherwise `__g_pool[Storage] += Sign * (Conj ? conj c : c)`.
Consequences the record makes explicit, as the memo requires:

- A dense logical computation that reads both `A(i,j)` and `A(j,i)` touches one
  off-diagonal pool cell **twice**; its cotangent receives **both** accumulations.
  Nothing multiplies by `2` or by `r!`; multiplicity is the *access* (dense
  enumeration vs `range<SymIdx>` canonical enumeration), and the reconstruction
  carries only the per-read sign/conjugation.
- Diagonal cells of a symmetric pool have orbit size 1; antisymmetric diagonals
  are `Zero` and contribute nothing. Both fall out of accumulation, not formulas.
- `Conj` needs a declared real/complex differentiation convention. AD refuses
  complex parameters today (`tests/corpus/ad/018_complex_param_rejected.blade`),
  so the field is carried and unused; the convention is a prerequisite of the
  first complex consumer, not of this record.

Not built in the first gate: reverse mode has no compact operand path to hang it
on (§1.6). The JVP C5 path is the natural first *forward* consumer (it already
iterates canonical cells) and would need nothing from the record beyond
`Sign`/`Conj` for a future antisymmetric tangent.

## 3. Design

### 3.1 Where the description is computed and carried

**3.1.1 One parse.** Add to `src/Types.fs` beside the tag helpers (`:272-304`):

```fsharp
type HaloAccess = { Inner: string; Offsets: int list; Start: int64; Shrink: int64; IsCompound: bool }
let haloAccessOfTag (tag: string) : HaloAccess option      // via (|HaloWinTag|_|)
let haloReach (h: HaloAccess) : int list                    // Offsets ∪ {0}, sorted
/// Forward demand of an output tile [lo, hi): the input window [lo', hi').
let haloDemand (h: HaloAccess) (lo: int64, hi: int64) : int64 * int64 =
    (lo + h.Start + int64 (List.min (haloReach h)), hi - 1L + h.Start + int64 (List.max (haloReach h)) + 1L)
/// Transpose: the (offset, output ordinal) pairs that read input ordinal j.
let haloContributors (h: HaloAccess) (outExtent: int64) (j: int64) : (int * int64) list
```

`haloStartOffsetOfTag`/`haloShrinkOfTag` become one-line projections of
`haloAccessOfTag` (keeping their call sites compiling). The tag stays the
carrier: it already survives every `{ inner with .. }` rewrite and is what the
distributed plan's `Chunked` field argument assumed (`plan-distributed-memory.md`
§2.2). A parallel `Access` field on `IRIndexTypeG` (`src/Types.fs:908-918`) was
considered and rejected for this gate: ~92 construction sites for a fact one
string already encodes.

**3.1.2 One body scan.** Add `IRAccess.fs` (new file; `Blade.fsproj` has
`EnableDefaultItems=false`, insert after `IRStorage.fs` in the IR group) with

```fsharp
/// Every dense window read in a kernel body, by NODE reference (the SubstMap contract):
/// (node, array id, prefix indices, static offset). Non-literal offsets are reported
/// separately so callers can decline rather than skip them.
type WindowRead = { Node: IRExpr; ArrayId: IRId; Prefix: IRExpr list; Offset: int }
let windowReadsOf (wid: IRId) (body: IRExpr) : WindowRead list * (* computed *) IRExpr list
```

lifted verbatim from `planHaloCarousel`'s `scan`/`offOf`
(`src/CodeGenLoopNest.fs:1293-1313`) and made the *only* IR-level matcher of the
`IRIndex (.., IRBinOp (IRAdd, IRVar wid, lit))` shape. Consumers: the carousel,
the BL8009 guard, the interpreter twin (through the same function — it is IR,
not C++), and the streamed ring.

**3.1.3 The refusal the record enables.** In `haloExtentClash`'s `checkSite`
(`src/TypeCheckInfer.fs:8677-8695`), when the index argument is
`TExprApp (f tagged, [lit])` (literal or negated literal, the two shapes
`src/Lowering.fs:457-460` already recognizes) and `lit ∉ haloReach h`: new
`TypeError.HaloOffsetOutsideWindow (offset, declared offsets, targetName)` →
**BL4019** "halo window read outside the declared offset set" (first free code
after BL4018; the five touch points are `src/Unify.fs`, `src/TypeEnv.fs` ×2,
`src/Diagnostics.fs`, `protocol/surface.json` + `protocol/data/diagnostics.json`,
gated by `blade test surface`). Message: *"the window over `<inner>` declares
offsets [-1, 0, 1] (reach [-1, 1]); `w(2)` reads outside it — the interior was
shrunk for reach 1, so this read runs past `a`'s allocation. Add 2 to the
offset set, or read within it."* Computed offsets (`loops/080`) are **not**
refused in v1; they stay fail-open exactly as today and are named in the
BL4019 knowledge-base entry as the remaining case. P2 closes them with an
interval bound from the sibling slots' literal extents, or a per-read runtime
panic under the BL8009 family — the choice is deferred because no program in
the repo needs either yet.

### 3.2 Consumer (a): forward demand

**3.2.1 Existing clients, re-homed (default lane, regression-pinned).**
Both extent guards compute `haloDemand h (0, M)` and require it to equal
`(0, N)`; that is one call each replacing the `shrunk + shrink` arithmetic at
`TypeCheckInfer.fs:8655-8657`, `CodeGenCuda.fs:2784-2786`, `Interp/Loops.fs:852-854`,
and their three body scanners become `windowReadsOf`. The carousel computes its
ring from `haloDemand h (i, i+1)` and its per-step delta as
`haloDemand h (i+1, i+2) \ haloDemand h (i, i+1)` — a single cell,
`i + 1 + Start + maxOff`, **present only when `i + 1 < M`** — which fixes the
one-past-the-end prefetch (§1.2) by emitting the tail load under
`if (__i0 + 1 < M)`. Values are unchanged (the carousel is a rendering
substitution, `CodeGenLoopNest.fs:1227-1229`); `loops/072-080, 158-164` pin them,
and the `// halo carousel:` census line count pins that the transform still
fires (§4.1).

**3.2.2 New client: the streamed-source ring (zarr lane).** Today a halo over
`alias.stream(var)` is refused (`CodeGenLoopNest.fs:407-408`). With the record
it is the carousel with the load replaced by a fiber read:

```blade
let S = z.stream(s.vars.T)                      // Array<Float like Site, Time>, never materialized
let d = method_for(halo<Site, [-1, 0, 1]>) <@> lambda(w) ->
            mean(S(w(1)) - S(w(-1))) |> compute // fiber reads through the window
```

Emitted shape (by analogy with `genStreamFiber`'s per-fiber block,
`ZarrProvider.fs:1892-1948`, and the carousel's ring):

```cpp
// Stream T from zarr store ... (chunked fiber reads at the S/T boundary)
size_t S_fiber_ext[1] = { T };  double* S_fseg = new double[ctT];
// halo ring over streamed S: window [-1..1] -- ring of 4 fibers, head = __i0, one fiber read/step
double* __ring_d_0[4] = { new double[T], new double[T], new double[T], new double[T] };
<fiber read site 0 -> __ring_d_0[0]>  <site 1 -> [1]>  <site 2 -> [2]>          // haloDemand h (0,1)
for (size_t __i0 = 0; __i0 < M; __i0++) {
    int64_t w = (__i0 + 1L);
    Array<double,1> S_wm1 = { __ring_d_0[(__i0 + 0UL) & 3UL], S_fiber_ext };   // S(w(-1))
    Array<double,1> S_wp1 = { __ring_d_0[(__i0 + 2UL) & 3UL], S_fiber_ext };   // S(w(1))
    d[__i0] = __lambda_k(S_wm1, S_wp1);
    if (__i0 + 1 < M) { <fiber read site (w + 2) -> __ring_d_0[(__i0 + 3UL) & 3UL]> }
}
```

The `if` is not optional here: an unguarded tail would request site `N`, which
is a missing chunk key — `exit(1)` under a null `fill_value`
(`ZarrProvider.fs:1925-1931`). This is the memo's "less I/O and temporary
storage" benefit in its smallest real form: `reach + 1` fibers live instead of
`N`, each fiber read once. Seams: `genElementBindingStreamed` grows a
`VirtualRange`-with-halo arm that binds nothing per element and instead
registers the window; `genLoopNestStreamed` (`:1365`) consults `windowReadsOf`
for the captured stream's reads (today the streamed map is keyed by element
`ArrayName`, `:1742-1748`; window reads are *captures*, so the ring must be
keyed by the capture's IR id). Kernel bodies with a bare `S` capture (not
through `w(o)`) decline with the existing "not stream-eligible" message.

**3.2.3 Interface for tiles that do not exist yet.** `haloDemand` is the
function a dense `ProviderReadSpec.Window` derivation would call once a tile
request exists (a rank's chunk run, `plan-distributed-memory.md` P3; a fold
chunk, `src/IRStorage.fs:95-100`). No such site exists in serial Blade, so no
dense window is *derived* in this gate — stated so the gate is not read as
promising it.

### 3.3 Consumer (b): the backward gather for the halo cotangent

**3.3.1 Two routes, one record.** Both are built from the same
`HaloAccess` and the same substitution; route G is a cost-only rewrite of route
S under `Optimize.fs`'s charter (`src/Optimize.fs:6-20`: cost only, twin-safe,
escapable) and lives behind `BLADE_AD_HALO_GATHER=0|off` (read per call).

*Shared:* `GradCommon.substWindowReads fname (wname) (h: HaloAccess) (idx: Expr) body`
rewrites `ExprApp (ExprVar w, [lit o])` → `add idx (iLit (h.Start + o))` after
checking `o ∈ haloReach h`, and **declines** (`Error`) on a bare `w`, a
non-literal offset, or `w` under a binder — the same contract as `substParam`
(`src/GradCommon.fs:559-580`: never half-substitute). A surface-level
`haloAccessOfSurface ctx innerTy offsetsExpr` builds the record from literal
offsets (mirroring `staticExtentOf`'s arm, `GradExpand.fs:1334-1345`) and `N`
from `literalIdxExtent`; anything else declines to the standing BL5500 message.

*Route S (scatter, the reference).* The eager-map lowering's `ExprHalo` arm
(`GradNormalize.fs:245-246`) returns a `MapSlot { Axes = [N - Shrink]; Readers = [] }`
and the kernel body is rewritten with `substWindowReads` against the loop index
`__mi` before the existing `substParamMany` (`:296-299`). The construction loop
`for __mi in 0..M { m(__mi) = a(__mi + s + 1) - a(__mi + s - 1) }` then takes
the existing NFor replay, producing the scatter

```
for __mi in 0..M { let __c = __g_m(__mi); __g_m(__mi) = 0.0;
                   __g_a(__mi + 2) += __c;  __g_a(__mi + 0) -= __c }
```

Nothing new is differentiated; this is exactly the move `098_reverse_map_range_construction`
documents for `range<I>`. It handles computed offsets too (they are just index
arithmetic), which is why G declines to S rather than to an error.

*Route G (gather).* A new arm in `adjointOfInit`'s dispatch
(`src/GradSweeps.fs:398-420`) for `MapApplyWith resolveLoop` whose sole operand
is `ExprHalo` and whose kernel is a plain expression-bodied lambda with a rank-0
parameter and no `where` clause (the eager lowering's own admissibility,
`GradNormalize.fs:186-200`). It leaves the **primal map untouched** (so `f__grad`'s
forward pass keeps the loop object and its carousel — better than S, which
densifies it) and emits, for each differentiable captured array `a` and each
offset `o` actually read, in **descending `o`**:

```
for __j in 0..N {
    if 0 <= __j - (s + o) && __j - (s + o) < M then
        __g_a(__j) = __g_a(__j) + T_o[ i := __j - (s + o), __c := __g_m(__j - (s + o)) ]
}
```

where `T_o` is the addend of the scatter statement targeting `__g_a(i + s + o)`,
obtained by running `adjointOf rc BODY' (v "__c")` on the substituted body and
reading each `accum`'s second operand (`accum` is `NAssign (t, add t cot)`,
`:136-137`, so the shape is the lane's own). A target that is not
`__g_a(add (v i) (iLit c))` — impossible after `substWindowReads` unless the body
reads `a` at some other index, e.g. `a(0)` — declines to S. `hoistReduces`
preludes inside the body decline in v1.

**3.3.2 Why this is the transposed description and not a re-derivation.** The
loop bounds `[0, N)`, the guard `0 ≤ j - Start - o < M`, and the term set are
`haloContributors h M j` written out; the only thing taken from the scatter is
the *coefficient* `T_o`, obtained the way every C6 arm obtains its coefficient —
by re-indexing an adjoint expression through the form's transpose
(`:271-300`, "flow cotAt dims init accumulates the cotangent back into the
operands' buffers by the form's TRANSPOSED reindexing"). Nothing pattern-matches
user-written index arithmetic.

**3.3.3 Race-freedom.** Each `__j` is written by exactly one iteration; the
loop body reads only `__g_m` and primal arrays. The serial `IRForRange` emitted
in v1 is therefore *parallelizable by construction*; parallel emission is P2
(an "independent iterations" mark on `IRForRange` consumed by
`genForRangeBinding`, `src/CodeGenBinding.fs:3863`, under the existing OMP gate)
and changes no value (§3.3.4), so the OmpTests differential is its whole gate.

**3.3.4 Bitwise identity with the scatter reference.** In route S, cell `j`
receives its contributions in increasing loop order `i = j - s - o`, i.e. in
**decreasing `o`**; two different offsets never target the same cell within one
iteration. Route G adds the same terms to the same cell in the same order, each
as its own `+=` with an identical addend expression (`T_o` evaluated at the same
`i` with the same `__g_m(i)`), starting from the same caller-supplied buffer
contents (`f__grad`'s `__g_a: mut` accumulation ABI, `src/Grad.fs:16-22`).
Hence `G == S` **bitwise**, including at the boundaries, for every kernel the
arm admits — the property §4.2 pins. (It would *not* hold if the gather summed
the terms first and added once; the design forbids that spelling.)

**3.3.5 Plumbing the arm needs.** `staticDimsOf` (`src/GradExpand.fs:1376-1400`)
gains the halo-map shape `[N - Shrink]` — allowed by its own rule only because
the flow now exists — so `Grad.synthesize`'s `dimsEnv`/`localDecls`
(`src/Grad.fs:135-150`) can declare `__g_m`. The AD-able-subset walker
(`src/GradExpand.fs:480-540`) admits the sole-halo map in `grad` mode as it
does in `jvp` mode (operands contribute no taint; the kernel body is walked with
`inKernel = true`). `analyze`'s taint then marks `m` differentiable through the
body's capture of `a`.

### 3.4 Interpreter twin requirements

- Route S/G output is surface Blade in the NFor subset the interpreter already
  runs (`IRForRange` `src/Interp/Core.fs:609`, `IRIf`, array-cell assignment);
  `ad` is in the interp gate (`tests/InterpDiff.fs:179`). No interpreter change.
  The guard must be a real branch in both lanes (ternary in C++, taken-branch
  evaluation in the interpreter), never a multiply-by-zero, because the guarded
  read `__g_m(j - s - o)` is out of range when the guard is false.
- BL4019 is TypeCheck-level: both lanes refuse identically.
- The extent-guard refactor keeps `Interp/Loops.fs:840-885` as the twin of the
  codegen guard by calling the same `haloDemand`/`windowReadsOf` — the twins
  become one function with two callers rather than two mirrored functions.
- The streamed ring has no interpreter twin: streamed reads are backend-only
  (`src/Interp/Core.fs:869`), so the interp gate skips such programs as it does
  every stream consumer; the differential is against the `.read` spelling of
  the same program in the zarr lane (§4.1).

### 3.5 Later consumers, as interfaces only

```fsharp
/// Ownership (plan-distributed-memory §4.3): ghost strips a chunk run [lo,hi) needs.
let ghostStrips (h: HaloAccess) (lo, hi) = let (lo', hi') = haloDemand h (lo, hi) in ((lo', lo), (hi, hi'))
/// Revision propagation (memo item 4): output tiles touched by a changed input chunk [lo,hi).
let affectedOutputs (h: HaloAccess) (outExtent: int64) (lo, hi) =
    (max 0L (lo - h.Start - int64 (List.max (haloReach h))), min outExtent (hi - h.Start - int64 (List.min (haloReach h))))
/// Compact accesses (§2.5): a logical read's storage image; consumers accumulate Sign * cot into Storage unless Zero.
let reconstruct (sym: SymmetryClass) (coords: int64 list) : Reconstruction   // = canonFold + canonLeftJustify, named
```

None of these has a caller in this gate. `affectedOutputs` is `haloContributors`
over an interval; `ghostStrips` is `haloDemand` minus the tile; `reconstruct` is
`Interp/ArrayOps.canonFold` returned as a record. The point of writing them here
is only to show that the three later consumers read the *same two functions*
the first gate builds — the memo's thesis in its checkable form.

## 4. First gate

### 4.1 Programs and pins

Corpus (default lane; pins are exact because inputs are dyadic):

1. `tests/corpus/ad/025_halo_stencil_grad.blade` — the §1.2 stencil,
   `f(a) = reduce(method_for(halo<H,[-1,0,1]>) <@> lambda(w) -> a(w(1)) - a(w(-1)) |> compute, (+))`,
   `a = [0,1,2,3,4]`: `EXPECT: v = 6`, `EXPECT: da = [-1, -1, 0, 1, 1]`
   (∂/∂a_j of Σ_{i<3} (a_{i+2} − a_i)). Also `ad.jvp(f)(a, t)` with
   `t = [1,0,0,0,1]`: `t_out = 0 = ⟨da, t⟩` — the JVP leg travels a completely
   different route (capture-read, §1.3), so the equality is a differential, not
   a tautology (the pattern `098` uses).
2. `tests/corpus/ad/026_halo_stencil_grad_nonlinear.blade` — kernel
   `a(w(1)) * a(w(-1))`, `a = [1,2,4,8,16]`: `v = 84`, `da = [4, 8, 17, 2, 4]`
   (cell 2 receives two contributions, `a_0 + a_4`).
3. `tests/corpus/ad/027_halo_lagset_grad.blade` — offsets `[-2, -4]` (no 0,
   `Start = 4`, `M = N - 4`), `a = 1..8`, kernel `a(w(0)) * a(w(-2))`:
   `v = 122` (matches `loops/078`'s `s_lag2`),
   `da = [0, 0, 5, 6, 10, 12, 5, 6]` — cells 0 and 1 have *no* contributors,
   the boundary case the ZeroOutside transpose exists for.
4. `tests/corpus/loops/165_halo_offset_outside_window_rejects.blade` — the
   §1.5 probe, `// ERROR: BL4019`; a compound-inner sibling with `w(2)` on
   `halo<CompoundIdx<..>, [-1,0,1]>` pins the same code (today that shape reaches
   Lowering's `failwith` at `:464` only for non-literals; literals are silently
   accepted).
5. `tests/corpus/ad/028_halo_computed_offset_grad_scatter.blade` — the `loops/080`
   shape (`S(w(0 - i))`) differentiated: G declines, S handles it; pins the
   values and, in the harness, that no gather loop was emitted.

Harness (`tests/AccessTests.fs`, pure lowering + codegen in the
`FlatPathTests.fs` style, `tests/FlatPathTests.fs:31-36`; registered in
`Blade.fsproj`, `tests/RunAll.fs:493-496`, and `src/CliSelfTests.fs`'s key map as
`access`):

- Shape pins on `f__grad`'s emitted C++ for programs 1-3: gate on → exactly one
  assignment target `__g_a[__j..]` per cotangent array inside a loop bounded by
  `N`, no `__g_a[(__mi + ..)]` target; gate off → the scatter targets present,
  no `__j` loop. Emission-shape pins are the only way to see this decision;
  the corpus cannot (`FlatPathTests.fs:4-10` makes the same argument).
- Differential (needs g++, skips without): compile and run each program with
  `BLADE_AD_HALO_GATHER` on and off; compare printed gradients **byte-for-byte**
  (§3.3.4 is the claim; a tolerance would hide a broken ordering). Extents 5, 8
  and one non-power-of-two larger case (127) per the benchmark discipline.
- Regression: `loops/072-080, 158-164` unchanged; count of `// halo carousel:`
  lines per program equal before/after the refactor; emitted tail load now
  guarded (`if (__i0 + 1 <`) — and `blade run --memcheck` on `loops/072` clean.
- Zarr lane (`blade test zarr`, hermetic): a store written on the fly, the
  §3.2.2 program spelled with `.stream` and with `.read`, outputs byte-equal;
  emitted text contains one `// halo ring over streamed` census line and
  exactly `reach + 1` fiber buffers.

### 4.2 Comparison against direct evaluation and the scatter reference

Direct: hand derivations above (all dyadic) and the JVP dot-product identity
`⟨ad.grad(f)(a), t⟩ = ad.jvp(f)(a, t)` for two seeds per program, one seed
supported only on boundary cells. Scatter reference: route S under the gate,
byte-for-byte. Boundaries: program 3's leading zeros, program 1's `±1` edge
cells, and a 2-cell array where `M = 0` (the map is empty; both routes must
emit no accumulation and `da = 0`).

### 4.3 STOP condition

Stop, and record the failure in this file's Status line, if either holds
after routes S and G and the carousel/guard refactor are written:

- the gather arm needs any fact about the halo that `HaloAccess` + `windowReadsOf`
  do not supply and the carousel/guard/ring do not also consume — a second,
  unrelated analysis at one consumer is the memo's named failure of the shared
  abstraction; or
- G and S disagree bitwise on any admitted kernel — the transposed-order
  argument of §3.3.4 is then wrong, and a gather that changes bits is not a
  cost-only rewrite and does not belong under an escapable gate.

Neither the streamed ring (P1c) nor parallel emission (P2) can rescue a failed
core gate and neither should start before it passes.

## 5. Size, risk, files

Effort (one engineer, sequential): P1a record + scan + BL4019 + guard/carousel
re-homing, 2-3 days; P1b routes S and G + gate + corpus + `AccessTests`, 4-6
days; P1c streamed ring + zarr lane, 3-5 days. Total ~2.5 weeks. P2 (parallel
gather, computed-offset proof) and everything in §3.5 are unsized.

Risks, ordered by likelihood:

1. **Substitution declines.** `substWindowReads` refuses `w` under binders and
   non-literal offsets; kernels with block bodies are already refused by
   `asKernelLambda` (`GradNormalize.fs:190-192`). Programs like `lswosa.blade:143`
   (block-bodied halo kernel) stay refused with the standing message. Acceptable
   for v1; say so in the BL5500 text.
2. **`staticDimsOf` discipline.** Its doc comment forbids adding a shape without
   its flow (`GradExpand.fs:1380-1385`); the arm must land in the same change as
   the flow or `localDecls` will size a buffer the sweep never fills.
3. **Streamed-ring keying.** The streamed emitter is element-keyed; window reads
   are captures. Getting the capture's IR id from `windowReadsOf` into the ring
   is the one genuinely new seam in P1c; if it needs a body scan the carousel
   does not, that is a §4.3 stop for P1c specifically, not for the core gate.
4. **Diagnostic churn.** BL4019 is five touch points and a `blade test surface`
   gate that category runs do not exercise; run it before the full suite.
5. **False comfort from the gate-off route.** Route S alone already "supports"
   halo in reverse mode. Do not land S without G: S densifies the primal and is
   the scatter the memo wants replaced, so landing it alone leaves the shared
   abstraction untested.

Files to touch, in order:

1. `src/Types.fs` — `HaloAccess`, `haloAccessOfTag`, `haloReach`, `haloDemand`,
   `haloContributors`; re-express the two existing helpers.
2. `src/IRAccess.fs` (new) + `Blade.fsproj` entry after `IRStorage.fs` —
   `windowReadsOf`.
3. `src/Unify.fs`, `src/TypeEnv.fs` (×2), `src/Diagnostics.fs`,
   `protocol/surface.json`, `protocol/data/diagnostics.json` — BL4019.
4. `src/TypeCheckInfer.fs:8647-8708` — offset-set check in `checkSite`;
   `haloDemand` for the extent obligation.
5. `src/CodeGenCuda.fs:2779-2830`, `src/CodeGenLoopNest.fs:1237-1363`
   (guarded tail), `src/Interp/Loops.fs:840-885` — consume 1 and 2.
6. `src/GradCommon.fs` — `substWindowReads`, `haloAccessOfSurface`;
   `src/GradNormalize.fs:245` — route S arm; `src/GradExpand.fs:1376-1400,
   480-540` — dims + walker; `src/GradSweeps.fs:398-420` — route G arm;
   `src/Grad.fs` — nothing beyond what `dimsEnv` already does, verify.
7. `tests/corpus/ad/025-028`, `tests/corpus/loops/165`, `tests/AccessTests.fs`,
   `Blade.fsproj`, `tests/RunAll.fs`, `src/CliSelfTests.fs`.
8. (P1c) `src/CodeGenLoopNest.fs:330-408, 1365-1760`, `tests/ZarrTests.fs`.
9. `docs/plans/README.md` — index row; `docs/features.md` — a `halo` row noting
   reverse-mode support and BL4019; `plan-ad-combinators.md` §2.9 — REV line
   updated to point here.

## 6. Recommendation

**GO** for the first gate (P1a + P1b), with P1c following on the same record
once the gate passes; **DEFER** compact reconstruction, ownership and revision
propagation to their own plans, which should consume `haloDemand`/`haloContributors`
rather than the tag helpers.

The case rests on verified facts, not projected speedups: the description
already exists as a string with nine readers and no owner; one of the readers'
missing checks is a silent out-of-bounds read reachable from a five-line program
(§1.5); reverse-mode AD refuses the construct outright and the fix is an arm in
an existing reindexing family whose output is provably bit-identical to the
scatter it replaces (§3.3.4), so the gate can be pinned byte-for-byte rather
than to a tolerance. The one speculative element — that the *same* record
serves the streamed ring — is isolated in P1c behind its own stop condition.
What this document does not claim: any wall-clock improvement, any dense
provider window (no tile request exists to derive it from), any compact or
distributed consumer beyond the interface signatures in §3.5.
