# 07 — Segmented domains: chunks and files as a structural grouping of one axis

Status: BUILT 2026-09-06 on `feat/segmented-domains` (decisions D1-D10 resolved by the
user; D6 confirmed: a fold over a segmented axis is the plain flat fold at the API,
xarray parity, and the COMPILER chooses how it runs -- over a streamed rank-1 variable
it walks the store block by block in storage order, bitwise the materialized fold;
recorded for `blade plan` as `segment-streaming`, with the group_by and stencil
choices beside it). Also built: the STENCIL over segments (§2.3): a halo map over a
streamed `Chunked` axis runs one segment at a time, reading each run plus the ghost
cells its reach demands (the halo indices say which cells to materialize, as the user
framed it); and `segments(a)` off a value's declared `Chunked` slots, so an array
whose slots are both `Chunked` states its 2-D tiling without repeating the aliases. Landed, in five commits: the surface type in all three forms,
the structural groupings `segments(A)` and `files(A)` with no CSR, `ungroup` in its
grouped and row forms, static `segments`, zarr's inherited chunk edges, the deletion of
`blocked<I, K>`, PER-SEGMENT STREAMED READS (`group_by(A |> z.stream, segments(X))`
over a rank-1 zarr variable reads one run at a time into the ragged pool; no
whole-array buffer, §3.4), and the TILE GROUPING `segments(C0, C1)` over a rank-2
array in slot order with its `ungroup` (§4.1b; the reversed slot order is refused);
corpus `segments/` (13) and zarr-lane sections 10c/10d/10e (sql.md §7c is the
user-facing summary). Two deviations from the design below, both
recorded rather than hidden: (1) the segmentation is carried BESIDE the alias
(`TypeEnv.Segmentations`), not as a field on the index record -- nothing in this arc
changes storage layout, so the record needs no field yet, and the alias adopting the
inner record is what makes `Chunked<I, K>` unify as `I`; the field lands with the
storage level (distributed P0) when layout depends on it. (2) The two-level axis is
exposed one level at a time (`files(T)` = the file runs, `segments(T)` = the innermost
per-file chunk runs, with each file's own edge) rather than as the nested
`[file_outer; chunk_outer(file); member]` shape of §2.7, because a slot-aware nested
`group_by` is not built. NOT built: streaming of rank >= 2 variables and of netcdf/icechunk stores
(the hooks exist; zarr implements rank 1); a stencil streaming more than one source;
the NESTED `[file_outer; chunk_outer(file); member]` shape of §2.7 (the two levels are
exposed one at a time) and the 2-D mosaic-of-stores declaration, which is where the
cross-slot refusal of §4.0 would first have content (with two single-slot
segmentations it holds by construction, and a tile grouping over a file-tiled axis
refuses with that reason); a COST model for the streaming choice (today it is by
consumer shape, recorded but not costed); string-label indexing of the `files` outer
axis; netcdf/icechunk chunk-edge readers; static `segments` over provider axes.
Found in passing: an elementwise map over any `group_by` result is refused by the
ragged-map emitter (pre-existing BL7004), which is what blocks the per-segment
elementwise idiom as a nested map. Reframes item 4 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md)
(revision reuse) as a property of chunked domains in general, and reconciles it with
[plan-distributed-memory.md](../plan-distributed-memory.md) §2 (the `Chunked<I, K>` type
surface, which this design adopts and extends rather than replaces). Source snapshot:
master at `8076f29`. Every `file:line` below was read at that snapshot. Claims marked
**(inferred)** were not executed.

Companions: [02-access-descriptions.md](02-access-descriptions.md) (`HaloAccess`, whose
`haloDemand` is the per-segment ghost window here), [04-revision-reuse.md](04-revision-reuse.md)
(whose tile keys become segment keys), `docs/features/sql.md` §7–§8 (the grouping
machinery this design reuses), `src/providers/StreamingIONotes.md` (the non-materialized
read that per-segment iteration generalizes).

## 0. The claim in one paragraph

A chunked dimension is a `group_by` of its axis whose keys are given structurally — by
the chunk grid, or by which file a position came from — rather than by values. The
existing grouping machinery already has the right shape for it: an outer per-group slot,
a ragged member slot, a name-keyed partition, an enum-keyed case with static reverse
lookup, and a `group_bucket` accessor for the row-to-group map. What it lacks is a
*fourth* dispatch case where the partition is a segment table and no CSR is materialized;
an *inverse* (`ungroup`) that reassembles the flat axis with its identity intact, which
`join` today destroys; a provider path that reads one segment at a time; and the rule
that a stencil crossing a segment boundary reads the neighbouring segment's edge, which
`HaloAccess` already computes. The outer axis of a file-segmented dimension is an
`EnumIdx` over the file labels. Coordinate variables are not consulted at all: to Blade
they are ordinary arrays in the provider, segmented like every other, and a declared
file partition is taken as declared. Out-of-core
computation is then simply *not calling `ungroup`*: a per-segment map or a licensed
per-segment fold never holds the flat array. Files and chunks are *unrelated* partitions that can
both act on one axis: a file-segmented dimension whose files are themselves chunked is
the file grouping first and the chunk grouping second, on the same slot, with the chunk
table a function of the file. Multi-dimensional chunking is a sequence of such steps,
one per (slot, level), in the one grammatical order — slot by slot, files then chunks
within each slot — and a store set whose chunk table on one axis depends on another
axis's file is refused, never reordered.

## 1. Verified current state

### 1.1 What a chunked provider dimension becomes

- A zarr dimension is minted as one plain `Idx<N>` from `(name, length)` in
  `zarrDimToNamedIndexType` (`src/providers/ZarrProvider.fs:1061`, `Tag = None`). The
  chunk grid is parsed (`:694-709`; regular grids only, `:702`) and then **erased from the
  type**: nothing downstream of the module builder can see it.
- Chunk structure reappears only in codegen. `genReadVar` (`:1487`) assembles the whole
  variable into `<v>_flat` from every chunk file (`genAssembleFlat`) and then scatters it
  cell by cell into the array. The distributed plan calls this out (§2.4a) as the
  per-cell scatter that a block copy would replace.
- Icechunk stamps a provenance tag `__icaxis|<dim>@<repo>` on the minted axis (the memo's
  P3 correction) and bakes per-chunk `(file, offset)` tables; the packed pool axes have
  their own `__icpool|` tags. Chunk identity there is content-addressed (04 §1.1).
- **No coordinate values are read at metadata time** by any provider. The constraint
  the user names is the current behaviour; the change is to stop erasing the chunk
  structure between the module builder and codegen.

### 1.2 The grouping machinery

- `group_keys(k)` over `Array<K like I>` types as `IRTGroupKeys (outerIdx, sourceIdx,
  enumValues)` (`src/TypeCheckInfer.fs:3956`), dispatching on the key array's element
  annotation: positional `Idx<N>` (static bucket count), `EnumIdx` (static reverse lookup,
  `enumValues` carried), or unannotated (dynamic discovery by hashing, first-occurrence
  order) — `docs/features/sql.md` §7, the arm at `:3931-3955`.
- `group_by(vals, gk)` yields a rank-2 arrow `[outer; member]` where the member slot is
  `IxKGroupMember` with extent `IRParam "__groupsz"` (`:3902`): ragged, runtime-sized.
  Codegen reads the partition as CSR locals (`gk__offsets`, `gk__perm`) suffixed off the
  **binding name**, which is why any indirection is BL3017 (sql.md §7) and why
  co-iteration of two grouped arrays requires the *same* binding (`:11724`).
- `group_bucket(gk)` is the row-to-bucket map over the source axis (sql.md §7a); it is
  the CSR's inverse and the one form of the partition that crosses function boundaries.
- Negative keys drop rows (sql.md §7): `WHERE` fused into `GROUP BY`.
- `group_by` acts on the **leading** slot of the values array only; grouping a grouped
  array's member slot has no arm (no such form in `inferGroupBy`, `:4055ff`).

### 1.3 The combinators that touch axis identity

- `inferJoin` (`:3739`) sums extents along the joined axis and mints a **fresh anonymous
  axis, `Tag = None`** (`:3782-3787`). A join therefore forgets that its parts were pieces
  of one axis. This is the seam segmentation must change: reassembling the segments of
  `I` must yield `I`.
- `stack`/`sequence` add a leading `__seq` axis (`IxKSeq`, the replicate arm at `:4090`);
  that is the chunk-grid-times-within-chunk factorization when the parts are equal-sized.
- `subset` and `align` from the formalism (§3.7): `subset` is not implemented; `align`
  exists (`ExprAlign`, `:1744`) as a join on common indices. A provenance-carrying subset
  is exactly a segment (§2.3).
- `blocked<I, K>` types as a virtual arrow over `I` (`:1427-1431`) but has no parser arm;
  the distributed plan §2.5 deletes it. This design agrees (§2.6): the loop-side spelling
  of a segmentation is the segment table itself.

### 1.4 The non-materialized reads that exist

- `alias.stream` (StreamingIONotes v1): a fiber read per site inside the consuming nest,
  no whole-array buffer; dense variables, one trailing axis, no fusion leaves. Zarr fibers
  are one seek+read per chunk file covering the site. This is per-*site* laziness; the
  design needs per-*segment* laziness, which is coarser and simpler (a segment is a set
  of whole chunks).
- `|> read` (`ExprRead`, `src/Ast.fs:476`) forces a deferred provider read of a whole
  variable. The deferral is per variable, not per region.
- The distributed plan's P3 ("reads touch only owned chunks") is per-segment laziness
  under another name, with ownership as the selector. Here the selector is the iteration.

### 1.5 The distributed plan's type surface (adopted)

`Chunked<I, K>` "is still `I`": a `Chunk` **field** on the index record, not a kind and
not a tag, so every plain-axis walker keeps working and the halo tag stays free
(plan-distributed-memory §2.2; the rejection of `IxKChunked` and of a tag payload is
argued there and not repeated). `ChunkEdge = EdgeLit | EdgeFromStore`. Unification
refuses mismatched edges and never rechunks (§2.3). Providers inherit the edge from
store metadata at compile time (§2.4b). This design keeps all of it and generalizes the
edge to a segment table (§2.1).

## 2. The abstraction

### 2.1 A segmentation is a partition of one axis into contiguous runs

```
Segmentation over I (extent N):
  boundaries  b_0 = 0 < b_1 < ... < b_G = N          (G segments, run g = [b_g, b_{g+1}))
  labels      l_0 .. l_{G-1}                          (optional; files, chunk ordinals)
  origin      Regular of edge K  |  Files of store list  |  Declared of boundaries
```

`Regular K` is the zarr/netcdf/icechunk chunk grid: `b_g = g*K`, `G = ceil(N/K)`, the
last run possibly short. `Files` is the multi-store case: `b_g` are the cumulative store
extents, `l_g` the store labels. `Declared` is user-written boundaries (rare; kept for
completeness and for tests). The IR carrier is the distributed plan's `Chunk` field with
`ChunkEdge` widened to this record; regular grids stay the compact `EdgeLit` form.

Two invariants the record makes structural: the runs are **pairwise disjoint** and they
**cover** `[0, N)`. Neither is a property of coordinate values.

**One axis can carry more than one level.** Files and chunks are unrelated partitions:
a store's chunk grid says nothing about how a dataset was split into stores, and a
file-segmented dimension whose stores are chunked carries *both*. The field therefore
holds a **sequence of levels**, outermost first, each level a partition of the previous
level's runs:

```
level 1 (files):   b1_0 = 0 < ... < b1_F = N,   labels l_f
level 2 (chunks):  for each file f, its own grid   b2_{f,0} = b1_f < ... < b1_{f+1}
```

A level's table may be indexed by the outer levels' ordinals — level 2's chunk edge is a
per-file fact and two files may be chunked differently — which is what makes the chunk
level a *dependent* index of the file level (§2.7) rather than a product. A plain
chunked store is the one-level case; a plain multi-file dataset with unchunked files is
also one level; a chunked multi-file dataset is two. Nothing below assumes one.

### 2.2 Segmentation as a `GroupKeys<I>`

`segments(I)` (spelling: decision D2) is a `group_keys` whose partition is the segment
table: `outerIdx` is `Idx<G>` for a regular grid or `EnumIdx<[l_0, ..]>` for files (the
enum case of `IRTGroupKeys`, `enumValues` carrying the labels), `sourceIdx` is `I`, and —
the new fourth case — **no CSR is built**: the permutation is the identity and the
offsets are the boundaries, both compile-time. `group_bucket(seg)` is `i -> g` by
binary search on `b` (or `i / K`), also without a table. Every property sql.md §7
states for the enum case holds: static reverse lookup, static group count, empty groups
impossible (cover), no negative keys (a position is always in exactly one run).

`group_by(A, seg)` then types as today's `[outer; member]` arrow with one refinement: the
member extent is **static per group** (`b_{g+1} - b_g`) rather than `IRParam "__groupsz"`.
That is the "closed" ragged form of the formalism (§3.6: lengths visible), not the opaque
one, so a short tail chunk is typeable without a runtime size.

### 2.3 Segments as provenance-carrying subsets, and the inverse

A group `group_by(A, seg)(g)` is a view of `A` over `[b_g, b_{g+1})`. Its member axis is
*not* a fresh `Idx<n_g>`: it is `I` restricted to a run, carrying `(I, b_g)` — the
`subset` of the formalism with its offset remembered. Two consequences:

- `ungroup(G)` (decision D4), the inverse of `group_by` over a segmentation, yields an
  array over **`I` itself** — the identity-restoring join. Today's `join` keeps its
  fresh-axis semantics for parts that are not runs of one axis; over runs of `I` it must
  return `I` or be refused, never mint a look-alike. This is the one change to an existing
  combinator's typing (§3.3).
- A stencil over `I` applied inside a segment reads `haloDemand h (b_g, b_{g+1})`
  (02 §2.2), which extends past the run by the reach on each side. Those cells belong to
  segments `g-1` and `g+1`. The segmented view therefore needs a **ghost extension**: the
  member axis of group `g` under a halo of reach `h` is `[b_g - lo, b_{g+1} + hi)` clipped
  to `[0, N)`, and the values there are the neighbours' edge cells. This is the
  distributed plan's §4.3 ghost exchange with no MPI in it — the serial half that plan
  says should land first.

### 2.4 Two kinds of sharing, and where value-disjointness lives

- **Within one store**, the partition is the chunk grid: structural, from metadata, and
  disjoint by construction. Nothing to check.
- **Across stores** (files tiling one logical axis — yearly files, regional tiles), the
  compiler establishes extents and the order of concatenation from metadata, and that is
  all it establishes. Coordinate variables are **not consulted** (decision D9): to Blade
  a coordinate is one more array in the provider, segmented on the same axis as the data
  and read, grouped and ungrouped by the same rules. Whether the declared file order
  matches the coordinate values is the user's statement about their data, exactly as the
  contents of any array are; the type says "these files are declared to tile `Time`" and
  nothing in the compiler or the runtime second-guesses it. This keeps the provider
  contract uniform — the partition is a property of the axis, not of any variable over
  it — and it is what makes the multi-file form provider-agnostic (§3.1).

The outer axis of a file-segmented dimension is the `EnumIdx` over the file labels
(§2.2). `group_by(A, seg)("era5_2021")` is then a static reverse lookup to one store's
variable, and reading it is one provider read of that store — the lazily created store
sharing the index type, with no metadata-time coordinate read.

### 2.5 Folds over a segmented axis

`reduce(A, (+))` over a segmented `A` has two evaluation orders: the flat one (ungroup,
then fold — materializes) and the per-segment one (fold each run, combine the `G`
partials — never materializes). The second **reassociates**, exactly as the OMP fold
split does, and is admitted on the same terms: a builtin operator or a `where comm(...)`
kernel (`FoldOmpNeedsLicense`, `src/TypeEnv.fs:811`). A licensed per-segment fold is
*bit-reproducible at a fixed segmentation* (the tree is the segment table, which is
static) — the same argument the distributed plan makes for Allgather-of-partials
(§4.2), and a stronger property than any runtime-chunked library offers. An unlicensed
kernel keeps the flat order and therefore materializes; that is a refusal of the
out-of-core path with a steer, not a silent slow path (decision D6).

### 2.6 Iteration is the segment table

The chunk driver loop of the distributed plan (§4.0, `ChunkPlan`) is the loop-side form
of §2.2: an outer level over `g` with the axis level's bounds rewritten to
`[b_g, b_{g+1})`. `blocked<I, K>` was that idea without a table and without a value-side
twin; it is deleted (distributed plan §2.5), and `method_for(group_by(A, seg))` is its
replacement. One table, both sides.

### 2.7 Files and chunks on one axis: two groupings, files first

With two levels on `I`, `segments(I)` is two `group_keys` applied in sequence on the
**same slot**: the file grouping first, then, inside each file's member run, that file's
chunk grouping. Typing:

```
group_by(A, files(I))            : [file_outer : EnumIdx<labels>;  member : run of I]
group_by(.., chunks(I))          : [file_outer; chunk_outer(file) ; member : run of I]
```

`chunk_outer(file)` is a **dependent** index: its extent (the file's chunk count) is a
static function of the file ordinal, so it is the closed ragged form again — a
`Dependencies`-carrying slot, the machinery `IxKDepInner` already provides — not a
product `Idx<F> x Idx<C>`, which would be wrong the moment two files have different chunk
counts. The member run under it is `I` restricted to one chunk of one file, still
carrying its offset in `I` (§2.3).

What this buys, and what it costs:

- **Reads happen at the innermost level.** The read unit is a chunk of a file; the file
  level only selects the store. A consumer iterating files but not chunks reads a file's
  chunks in sequence into the file's member buffer (still never the whole axis).
- **`ungroup` is applied per level**, innermost first, and each application restores the
  level above (chunks of a file reassemble to the file's run; files reassemble to `I`).
  `group_bucket` composes the same way: position -> (file, chunk).
- **Both levels are disjoint by construction**: chunks from the store's grid, files
  from the declaration. No level carries a runtime check of coordinate values (§2.4).
- **Halo ghost strips cross both kinds of boundary** the same way — a chunk boundary
  inside a file reads the neighbouring chunk, a file boundary reads the neighbouring
  file's edge chunk — because both are runs of `I` and `haloDemand` is stated on `I`.
- **The cost** is that regrouping a member slot is no longer an extension for the
  multi-dimensional case only (§4.1); it is required for the one-dimensional two-level
  case, so the slot-aware `group_by` is v1 work, not a possibility. §4.3's exclusion is
  narrowed accordingly: what stays out is a *value-driven* inner partition; a structural,
  static-per-outer-group inner partition is exactly this.

## 3. Design

### 3.1 Where the segmentation is computed and carried

- **Provider path.** `zarrDimToNamedIndexType` (and its netcdf and icechunk twins)
  attach `Chunk = Some (Regular K)` when the store's chunk grid gives one edge for the
  dimension across every variable that uses it; a dimension chunked differently by two
  variables refuses exactly as the conflicting-extent check does today
  (`ZarrProvider.fs:1131-1137`, distributed plan §2.4b). Opt-in, not default (§2.4c
  there): the module builder records the segmentation, and a program that never spells
  `Chunked<...>`/`segments(...)` sees the flat `Idx<N>` it sees today.
- **Multi-store path.** The type itself is the declaration (decisions D1, D5):
  `Chunked<I, [[s1, f1], [s2, f2], ...]>` lists the stores tiling `I` in order, each
  with its own chunking (`f_k` a literal edge, or `store` to inherit the file's grid, or
  absent for an unchunked file). The builder mints one `I` with the two-level table, one
  `EnumIdx` over the labels (D3), and a per-label variable table. This is
  **provider-agnostic**: a provider contributes only "what is this store's extent and
  chunk grid on this dimension" (zarr, netcdf and icechunk all answer from metadata), and
  `segments`, `group_by`, `ungroup` and the reads behave identically over any mix. A
  single zarr store is already a directory of chunk files, which is the one-level case
  with no new provider API; a dataset spread over several stores of any provider is the
  two-level case through this type. Every variable present in every listed store with
  `I` leading (v1 restriction) becomes a segmented variable; a variable missing from one
  store refuses at the declaration.
- **`segments(I)` is statically evaluable (D2).** Store paths are compile-time literals,
  so the segment table is a static value: `segments(I)` may appear where `let static`
  values do, and `StaticEval` gets a `ProviderRoots`-backed arm for it (the field is
  already threaded through `StaticEnv`, `TypeCheckInfer.fs:4077`). A program can therefore
  branch on the segment count at compile time.
- **Carrier.** The `Chunk` field (distributed plan §2.2), widened. The `__icaxis|`
  provenance tag is unchanged and still what identity is compared on; the segmentation is
  orthogonal to it, as it is to `Symmetry` and `Dependencies`.

### 3.2 The grouping case

`inferGroupKeys` gains the fourth arm: the argument is a segmented index type (or the
`segments(I)` form), `outerIdx` is `Idx<G>` or the label `EnumIdx`, `enumValues` the
labels, `sourceIdx` the axis. The typed node records `Structural`, and codegen emits no
CSR: `gk__offsets` is a `constexpr` table (or `g*K`), `gk__perm` is absent, and every
ragged peel that reads `gk__perm[...]` takes an identity branch. The BL3017 name-keying
is unchanged (the partition is still recovered from the binding name), which keeps the
same-binding co-iteration rule (`:11724`) working unchanged: two variables segmented by
the same declaration share the name.

### 3.3 The inverse; `join` is untouched

`ungroup(G)` where `G : [outer; member-of-I]` with the outer axis a segmentation of `I`
types as `Array<T like I, ...>`; applied to a two-level result it restores one level per
application (§2.7). It is the only materializing operation of the design. `join` keeps
its fresh-axis semantics unchanged (decision D4): the two are orthogonal — `join`
concatenates parts into a *new* axis, `ungroup` reassembles runs of an *existing* one —
and a program that `join`s the segments of `I` gets today's anonymous axis, as it does
today. No arm is added to `inferJoin`.

### 3.4 Per-segment reads

The provider's segment reader is `genAssembleFlat` restricted to the chunk files of one
run — for a regular grid, the chunks `[b_g/K, b_{g+1}/K)`, which is a contiguous block
copy (distributed plan §2.4a, P0.5); for files, one store's variable. The binding for a
segmented variable emits no whole-array buffer; a `group_by(A, seg)` consumer reads
segment `g` at the outer loop level, in the slot `genElementBindingStreamed` uses for
fibers today (StreamingIONotes). `ungroup` and any flat consumer read all segments into
one buffer — today's path, unchanged. Ghost cells (§2.3) are read as the neighbouring
runs' edge strips, `haloDemand`-sized, from the neighbouring segment's chunk files.

### 3.5 What prints

A segmented variable at top level prints as today (every top-level binding prints;
04 §1.8 records this as a constraint, not a choice): the print is a flat consumer and
materializes. This is the same fact 04 respects, and the reason out-of-core programs
put their segmented work inside functions or behind a `let` whose value is the fold.

### 3.6 Interpreter twin

`Interp/Loops.fs` gains the structural-grouping arm (identity permutation, static
offsets) and the `ungroup` arm; the provider reads in the interpreter already go
through the same `ChunkSource` seam as codegen, so per-segment reads are a bound
restriction there, not a new reader. The interp-diff gate is the correctness oracle, as
for every other arc.

### 3.7 Relation to revision reuse (04)

04 keys tile reuse on icechunk chunk content refs and defers zarr and netcdf for lack of
content identity. With segments, the reuse unit is the segment: icechunk answers "which
segments changed" from manifests, zarr from chunk-file identity (mtime and length, or a
hash on request), netcdf mostly cannot below the file. The elementwise-first gate of 04
becomes "which segments of the output need recomputing", and the halo case is the
ghost-extended segment. 04 should be re-based on this document once §4 passes.

## 4. Multi-dimensional grouping

This section is the possibilities the user asked for. It is not a commitment beyond
§4.1; the rest are recorded so the v1 choice is made knowing what it forecloses.

### 4.0 Grouping steps, dependencies, and the fixed order

A segmented variable is a **sequence of grouping steps**, each `(slot, level)`: `file_lat`,
`chunk_lat`, `file_lon`, `chunk_lon` for a chunked 2-D mosaic. Each step's table may be
indexed by the ordinals of earlier steps — its **dependency set**, known from metadata:

| step | table indexed by | dependency set |
|---|---|---|
| `file_d` | nothing | empty |
| `chunk_d`, every file has the same edge on `d` | nothing | empty |
| `chunk_d`, the edge on `d` varies with the file on `d` only | `file_d` | `{file_d}` |
| `chunk_d`, the edge on `d` varies with the file on other axes too | `file_d, file_e, ..` | `{file_d, file_e, ..}` |

The order is **fixed** (decisions D8, D10): slots in slot order, and within a slot the
file step then the chunk step — `file_lat, chunk_lat, file_lon, chunk_lon`. It is the
grammatical order because every step of one slot sits together, so the outer × member
shape of one axis is never interleaved with another axis's. One rule then decides
admissibility:

**A step's dependency set must lie within its own slot.** `file_d` depends on nothing;
`chunk_d` may depend on `file_d` (each file its own edge on `d`) and on nothing else. A
store set whose chunk table on `d` varies with another axis's file — tile `(i, j)`
chunked on `lat` with an edge that changes with `j` — cannot be placed in the fixed
order without `chunk_lat` seeing `file_lon` first, and is **refused at module build**
(BL-code to be assigned, naming the axis and the offending tiles) rather than reordered.
Reordering would mix slot behaviour, which is exactly what the fixed order exists to
prevent; a user who has such a mosaic rechunks it or declares it as two datasets.

Commutation is then a corollary rather than a check: steps of different slots have
dependency sets confined to their own slots, so which cells form a group does not depend
on the slot order — the partition is the same in any order, and the fixed order is a
presentation choice, not a semantic one. Program 5 pins this as a harness fact.

### 4.1 A sequence of single-slot segmentations (the chunk-grid case)

A rank-2 variable chunked `(K_lat, K_lon)` carries `Chunk = Some (Regular K_lat)` on the
`Lat` slot and `Regular K_lon` on `Lon`. That is already per-slot in the field encoding
— multi-dimensional chunking costs the carrier nothing. On the grouping side it is
`group_by(group_by(A, seg_lat), seg_lon)`: the second grouping acts on a *member* slot,
which `group_by` cannot do today (§1.2) and which §2.7 already requires. Two ways to
admit it:

- **(a) Nested grouping by slot.** Extend `group_by` with a slot argument (or make the
  structural case slot-aware): the result is `[outer_lat; outer_lon; member_lat;
  member_lon]` — outer axes first, then members. Iteration is the chunk grid outermost,
  which is the layout a block copy wants.
- **(b) A product segmentation.** `segments(Lat, Lon)` as one `GroupKeys` over the
  compound source `(Lat, Lon)` with outer `Idx<G_lat> × Idx<G_lon>`; `group_by` stays
  leading-slot but the leading slot is the compound. This is the multi-key form of
  `group_keys` (sql.md §7, "the compound key is always dynamic") made static, and it
  reuses the joint-level fusion that already exists for multi-dimensional identity groups
  (`IR.fuseJointSLevels`, formalism §11).

**The commutation corollary** of §4.0: for a plain grid, (a) in either slot order and
(b) give the same partition, so the fixed slot order costs nothing semantically. With
files in play the admissibility rule guarantees the same, or refuses.

Recommendation: (a) for v1, because it needs no new source-space form and its outer-first
result order is what §3.4's block copy and §2.6's chunk driver iterate; (b) as the
spelling of a *fused* grid when the two chunk loops should be one compound level (a
later cost decision, not a semantic one).

### 4.2 File tiling in two dimensions

Regional tiles (a 2-D mosaic of stores) are a product of two file segmentations, each an
`EnumIdx` over labels, with the per-label variable table indexed by the label *pair*.
Everything in §2.4 applies per axis, and the two label enums are independent. What
does not generalize: a mosaic whose tiles are not a product (irregular
tilings) is not a sequence of single-slot segmentations and is out of scope — it would
be a `SparseIdx`-keyed table of stores, a different design.

### 4.3 Value-driven hierarchical grouping (deferred, recorded)

`group_by(group_by(A, gk1), gk2)` where `gk2` is a *value* key over the member axis of
the first result — group stations by region, then within region by month — needs a
per-group inner partition discovered from data (one CSR per outer group, or a two-level
CSR). It is a real feature (nested `GROUP BY` with `ROLLUP`-shaped aggregates) and it is
*not* this design. The structural two-level case of §2.7 has the same *shape* (an inner
partition per outer group) and the slot-aware `group_by` of §4.1(a) is the syntactic hook
both share; the difference is that §2.7's inner tables are static per outer group, so
the dependent chunk index is the closed ragged form, while a value-driven inner partition
needs the opaque `RaggedIdx` form and a runtime CSR per group. Deferred on that line.

### 4.4 Mixed value and structural keys

Grouping a segmented axis by a *value* key (`group_by(A, group_keys(region))` where `A`
is file-segmented on `Station`) is legal today's-semantics: the value grouping is over
the flat source axis and forces `ungroup` first (a materialization). A future
"segment-local" variant — bucket within each segment, never crossing files — is the
composition of §4.1(a) with a value key and commutes only when the key does not depend
on the segment. Noted, not designed.

## 5. First gate

### 5.1 Programs and pins

1. **Regular grid, elementwise.** A zarr store with a 1-D variable chunked at 64 over
   `Idx<200>` (ragged tail 8): `method_for(group_by(A, segments(I))) <@> lambda(g) -> ...`
   computing a per-segment sum; `ungroup` of a per-segment elementwise map equals the
   flat map. Pins: values; emission shows no `gk__perm`, three segment reads, and — for
   the elementwise program — **no `A_flat`** buffer.
2. **Licensed per-segment fold.** `reduce(A, (+))` over the segmented `A` with the
   builtin operator: value equals the flat fold to the last bit at this size (both
   trees are pinned, not compared to a tolerance: a builtin `+` over `G = 4` partials of
   fixed content is deterministic). The same program with an unlicensed lambda kernel
   refuses with the steer (D6).
3. **Files as an `EnumIdx`.** Two stores tiling `Time` (extents 12 and 12):
   `group_by(T, segments(Time))("y2021")` reads one store; `ungroup` equals a hand-built
   `join` cell for cell while typing over `Time` rather than a fresh axis (the two spellings
   pinned side by side, D4). The `time` coordinate variable is itself a segmented variable
   and ungroups by the same rule — pinned to make the "coordinates are just arrays" contract
   (D9) visible. `segments(Time)` in a `let static` evaluates to two (D2).
4. **Stencil across a boundary.** `halo<I, [-1, 0, 1]>` over the segmented `A`, computed
   per segment with ghost cells: byte-identical to the flat stencil at every cell,
   including the two cells beside each boundary. This is the `haloDemand` consumer.
5. **Two dimensions.** A `(64, 64)`-chunked rank-2 variable over `(Idx<130>, Idx<70>)`,
   grouped by both slots in both orders: identical partitions (the commutation check as a
   harness fact), identical values, block-copy reads.
6. **Reverse AD** through program 1's per-segment map: the adjoint is per-segment too
   (the map is an ordinary rank-0 map over the outer × member space) — no new AD rule,
   pinned to make sure none was needed.
7. **Files and chunks on one axis.** Two stores tiling `Time` (extents 30 and 20), the
   first chunked at 8 and the second at 16 (four and two chunks — different counts, so
   the chunk level is dependent, not a product): `group_by` twice on the same slot, a
   per-chunk fold combined per file then across files equals the flat fold (licensed
   `+`, bitwise); `ungroup` twice restores `Time`; the reads are per chunk; a stencil
   across the file boundary reads the second file's first chunk as ghost.
8. **A 2-D mosaic, admissible and not.** Four tiles over `(Lat, Lon)`, all chunked
   `(16, 16)`: the shape is `file_lat, chunk_lat, file_lon, chunk_lon` and the partition is
   the same under either slot order (§4.0's corollary as a harness fact). The same mosaic
   with the `(0, 1)` tile chunked `(8, 16)`: `chunk_lat` would depend on `file_lon`, and the
   declaration is **refused** at module build with the new code naming `Lat` and tile
   `(0, 1)` — a `(rejects)` corpus test, so the refusal can never quietly become a reorder.

### 5.2 Assertions

- The interp-diff gate over all of 5.1.
- Emission pins for the absence of the flat buffer in 1, 2 and 4, and for the segment
  read count.
- ASan on 4 (the ghost strips are the one place an off-by-one can hide; 02's lag-set
  bug was found exactly this way).
- The existing `sql-group-by`, zarr, netcdf and icechunk lanes unchanged: the structural
  case is additive to `inferGroupKeys`, and no program without the new spellings
  changes meaning.

### 5.3 STOP conditions

- A consumer needs a fact about the partition that the segment table does not carry
  (the memo's "second analysis at one consumer" failure) — in particular if the ghost
  extension of §2.3 needs anything `haloDemand` does not give.
- The per-segment licensed fold and the flat fold disagree bitwise on program 2 — then
  §2.5's reproducibility claim is wrong and the out-of-core fold does not belong under
  a license.
- Program 5's two slot orders give different partitions — then §4.0's corollary is
  wrong and the fixed order is semantic, which would have to be documented as such.
- Program 7's dependent chunk index needs anything the `Dependencies` machinery does not
  already carry for `IxKDepInner` — then the two-level table is a new index kind, not a
  use of an existing one, and the cost estimate in §6 is wrong.

## 6. Size, risk, files

| piece | touch points | effort |
|---|---|---|
| Segment table on the `Chunk` field; provider attachment (zarr, netcdf, icechunk) | `Types.fs` (field, per distributed plan §2.2, ~92 record sites), `ZarrProvider.fs:1061,1131`, netcdf/icechunk twins, `Unify.fs:747` | 1 wk |
| Structural `group_keys` case, static-member `group_by`, `group_bucket` without a table | `TypeCheckInfer.fs:3914-3960,4055`, codegen ragged peel (identity-perm branch), `Interp/Loops.fs` | 1 wk |
| `ungroup` (per level; `join` untouched) | `Ast.fs`, parser, `TypeCheckInfer.fs`, lowering, both back ends | 3-4 d |
| Per-segment reads; block copy; ghost strips | `ZarrProvider.fs:1487`, `CodeGenBinding.fs:1469`, `StreamingIONotes` slot | 1 wk |
| `Chunked<I, [[s, f], ...]>` type form; label `EnumIdx`; provider-agnostic multi-store table; static `segments` | `ParserTypes.fs`, `TypeLower.fs`, module builder, `StaticEval.fs` (`ProviderRoots` arm) | 1 wk |
| The cross-slot dependency refusal (§4.0) | module builder, `Diagnostics.fs` (+ protocol json) | 2 d |
| Delete `blocked<I, K>` (D7): `Ast.fs:453`, `IR.fs:193`, the three IR traversal arms, the AD refusal arm, `formalism.md:785` | as listed in plan-distributed-memory §2.5 | 1 d |
| Licensed per-segment fold | `TypeCheckSupport.fs:376` region (the OMP license), fold emitter | 3 d |
| Slot-aware `group_by` (§2.7, §4.1a); the dependent chunk level; the step-order derivation and commutation check (§4.0) | `TypeCheckInfer.fs:4055`, `Dependencies`/`IxKDepInner` walkers, ragged peel emitter | 1.5 wk |
| Gate (5.1-5.2), fixtures, docs | `tests/`, `docs/features/sql.md` §7, features.md | 4 d |

Risks, in order: (1) the ~92-site record widening is mechanical but touches every phase
(the distributed plan accepts the same cost and this design shares it — do it once); (2)
the ragged-peel emitter has grown around `gk__perm` and an identity branch may not be a
local change; (3) with no coordinate check, a mis-ordered file list is a silent data error, by
design — the documentation must say plainly that the declaration order *is* the axis
order, and the `EnumIdx` labels in the printed shape are the reader's only cue; (4) identity questions multiply (a segment of
`A` versus a segment of `B`, a segment aliased onto a whole axis) and must reuse the
icechunk mint-table and tag discipline — the alias-laundering review is the checklist.

## 7. Decisions (resolved by the user 2026-09-06, except D6; nothing here is built)

| # | decision | resolution |
|---|---|---|
| D1 | axis spelling | `Chunked` for both levels: `Chunked<I, [[s1, f1], [s2, f2], ...]>` — the index is keyed on the files *and* on each file's chunking (`f_k`: a literal edge, `store` to inherit, or absent) |
| D2 | grouping accessor | `segments(I)`, **statically evaluable** (files are static) |
| D3 | file labels | an `EnumIdx`: a limited set of string-valued states |
| D4 | inverse | add `ungroup`; `join` unchanged — the behaviours stay orthogonal |
| D5 | multi-store declaration | no provider API: a store's own metadata gives its chunk level; the multi-store level is the D1 type and is **provider-agnostic** (`ungroup` etc. behave the same over any provider) |
| D6 | unlicensed folds over a segmented axis | resolved: no eager fold over a segmented dim is ever reassociated -- a flat `reduce` on a `Chunked` axis is the plain flat fold (the alias IS the axis), and a per-segment fold is only spelled through `group_by(a, segments(A))` plus an explicit combine; the deferred-pipeline recognition at `compute` (checking the data was ungrouped) is noted as the place a stronger rule would live |
| D7 | `blocked<I, K>` | remove, or fold into `Chunked` where relevant; it was the theoretical version of `Chunked` |
| D8 | multi-dimensional order | slot order |
| D9 | coordinate variables | **not consulted at all** — they are ordinary arrays in the provider |
| D10 | step order | interleaved (files then chunks within each slot) is the grammatical order; anything else mixes slot behaviour — so it is fixed, and a store set that cannot take it is refused (§4.0) |

The original questions, kept for the record:

- **D1 — the axis spelling.** Adopt `Chunked<I, K>` / `Chunked<I, store>` from the
  distributed plan as-is, and add the multi-store form as `Chunked<I, [s1, s2, ...]>`?
  Or a distinct name (`Segmented<I, ...>`) for the file case, keeping `Chunked` for grids?
  (Resolved: one name, with the per-file chunking carried in each list entry.)
- **D2 — the grouping accessor.** `segments(I)` as a new intrinsic returning
  `GroupKeys<I>`; or `group_keys(I)` overloaded on a type argument; or implicit (a
  `Chunked` axis *is* groupable and `group_by(A, Lat)` names the axis). Recommendation:
  `segments(I)`, explicit and greppable, name-keyed like every `group_keys`.
- **D3 — file labels.** The store's basename without extension, the declaration's own
  labels (`[y2020: "era5_2020.zarr", ...]`), or positional `Idx<G>` with labels as an
  attribute. Recommendation: declared labels, positional fallback when omitted.
- **D4 — the inverse.** `ungroup(G)` as a new intrinsic, or `join` alone with the
  identity-restoring arm. Recommendation: both — `ungroup` as the readable spelling,
  `join` over runs as the same operation so existing code does not silently mint a
  look-alike axis.
- **D5 — the multi-store declaration form.** A provider-level list (`zarr.tiled([...],
  dim = "time")` desugared like `repo.checkout`), or a type-level list under D1.
  Recommendation: provider-level, because the per-label variable table is a provider
  fact and `ProviderDesugar` already owns marker-erasing rewrites.
- **D6 — unlicensed folds over a segmented axis.** Refuse the out-of-core path with a
  steer (recommended: consistent with the OMP rule and "the fastest way is the only
  way"), or silently take the flat order and materialize.
- **D7 — `blocked<I, K>`.** Delete now (recommended; it has no parser arm) or reserve
  with a steer to `Chunked`.
- **D8 — multi-dimensional order.** Outer axes in slot order (recommended, §4.1a) or in
  grouping order; with the commutation check either is sound, and the choice is what a
  printed shape looks like.
- **D9 — the coordinate obligation's scope.** Check at the first materializing or
  cross-segment use only (recommended: pay-per-use, honest about when it ran), or
  eagerly at the first read of any segmented variable.
- **D10 — step order.** Derived from the dependency sets (interleaved when the data
  is separable, a files-first fallback otherwise), or fixed with a refusal. (Resolved:
  fixed and interleaved — the fallback was rejected because it mixes slot behaviour;
  §4.0 now refuses instead.)

## 8. Recommendation

**GO**, ahead of 04 and as the serial half of the distributed plan's P0/P0.5, in the
order of §6 with §5 as the gate, with the two-level (files then chunks on one axis) case
in v1 rather than deferred: it is the reason the slot-aware `group_by` is required at
all, and a design that handled files or chunks but not both on one axis would be wrong
for every chunked multi-file dataset, which is the common case. The case rests on reuse: the grouping machinery already
types the outer-times-ragged-member shape, the enum case already gives static labelled
reverse lookup, the distributed plan already chose the carrier, `HaloAccess` already
computes the ghost window, and the streamed-read slot already exists in the nest
emitter. The new semantics are small and stated: a structural partition needs no CSR;
reassembling runs of an axis yields that axis; a per-segment fold is licensed exactly
like a parallel one; and the file order across stores is a declaration that the compiler
takes as stated. What this document does not claim:
any wall-clock number, any MPI content, any irregular tiling, the value-driven
hierarchical `group_by` of §4.3, or any check of coordinate values (D9: none exists, by
decision).
