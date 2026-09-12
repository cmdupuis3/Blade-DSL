# Distributed memory — chunk-wise decomposition as a storage level

Status: DESIGN 2026-08-30 — nothing built. Written against the Arc 4 audit
(`plan-fortran-killer.md` §4), whose findings are re-verified below, and against
the language author's directive: *"Distributed memory should definitely be part
of the storage story. Decomposition should be done chunk-wise like with xarray,
and distributed memory should only contain the data that maps to the iteration
chunk."*

The directive refines the audit's `Dist<Lat, blocked>` sketch in one decisive
way. The audit proposed a **slab** per rank; the directive asks for a **chunk
grid** on the axis, ranks owning sets of chunks, and memory holding only the
chunks local iteration touches. Slabs then become the degenerate case (one chunk
run per rank), and the chunk grid is the object that unifies three things Blade
already has separately: the store's on-disk layout, the loop's blocking, and the
rank's ownership.

## 0. The one-sentence thesis, and the invariant that carries it

A chunk grid is **a storage attribute of an axis, not a new index space**.
Distribution is **a placement of that grid onto ranks**, and:

> **Ownership is derived from iteration, never declared.** A rank allocates
> exactly the chunks its schedule visits, plus the ghost closure of any `halo`
> on that axis. Both are compile-time functions of `(rank, size)`.

Everything else in this document is consequence. Where a rank's owned set is
*not* a compile-time function of `(rank, size)` — a data-dependent subscript, a
non-local read, two schedules disagreeing — the compiler refuses (§7). That is
the same move `LatIdx`-vs-`LonIdx` makes for bounds safety, applied to locality:
neither coarrays, UPC, nor Chapel type-check locality, and `a[p]` in a coarray
language proves nothing to the compiler.

## 1. What exists today (re-verified)

**The whole emitted MPI vocabulary is `Init`/`Init_thread`, `Comm_rank`,
`Comm_size`, `Allgatherv`, `Abort`, `Finalize`.** Confirmed by census across
`src/CodeGen.fs:2631-2690`, `src/CodeGenCuda.fs:984-1172, 3131-3214`,
`src/CodeGenFusion.fs:858-912`, `src/providers/ZarrProvider.fs:1589-1708`.

- **Storage is fully replicated.** Every rank allocates every array
  (`src/CodeGenCuda.fs:3118-3129` runs unconditionally); a `where mpi` kernel
  slices only its outermost loop level (`MpiSlab`, `src/IRStorage.fs:135`,
  consumed at `src/CodeGenLoopNest.fs:1645-1649`) into balanced slabs — `q = n/P`,
  first `n%P` ranks take one extra (`src/CodeGenCuda.fs:3164-3174`) — and then
  `MPI_Allgatherv(MPI_IN_PLACE, …)` over the whole pool restores the full array
  on every rank (`:3182-3214`, inner-block product at `:3194-3199`). FLOPs scale;
  memory does not.
- **`classifyMpiShape` (`src/CodeGenCuda.fs:1729-1760`) refuses**: folds
  (`:1742-1743`, "fold accumulation reorders floating-point reduction"), scalar
  outputs (`:1746-1747`, also short-circuited earlier at `:2940-2941`),
  compound/sparse domains (`:1748-1749`, `:1753`), **virtual sources including
  `halo<>`** (`:1754`), fused joint levels (`:1758`), element types without a
  native MPI datatype (`:1759`, table at `src/CodeGenState.fs:843-851`).
  Ineligibility is loud — `mpiError` at `src/CodeGenCuda.fs:2948-2950` — which is
  right, and is the discipline this plan extends rather than replaces.
- **The canonical distributed-HPC pattern is exactly the refused shape.**
  `halo<I, [-1,0,1]>` lowers to a *virtual* shrunk slot
  (`src/TypeLower.fs:1559-1601`), so `allReal` fails at
  `src/CodeGenCuda.fs:1731-1734, 1754`. Stencils are MPI-ineligible today.
- **Chunk machinery already exists, in three places**:
  1. Zarr's chunk grid is metadata (`ZarrProvider.fs:284`, parsed `:597-603`;
     `gridDims`/`gridCoords` `:887-898`) and the dense C++ reader already walks
     chunk coordinates and scatters into a dense pool (`:1391-1445`).
     `PackedReadOpts.Distribute` (`providers/ProviderRegistry.fs:26-33`, set at
     `CodeGenBinding.fs:1415-1419`) gives rank-scoped *chunk I/O* for the
     simplex-blocks layout: each rank skips blocks outside its cell range
     (`ZarrProvider.fs:1606-1628`) and reads only its own chunk files
     (`:1630-1643`) — then throws the scaling away with an Allgatherv
     (`:1696-1708`). The read side is already 80% of P3.
  2. `src/SimplexBlocksCore.fs` — combinadic block math for compact domains:
     `blockCount :92`, `tileCount`/`tileWidth` `:94-98`, `blockSequence :139`,
     `divisorTileEdgeIn :285`, `autoTileEdge :302` (**returns `None` — "do not
     block", the measured verdict**).
  3. `.stream` fiber reads (`src/providers/StreamingIONotes.md`), whose §P6
     already sketches MPI × streaming with ownership = output cell ranges.
- **`blocked<I, K>` is dead surface.** `ExprBlocked` (`src/Ast.fs:453`),
  `TExprBlocked` (`src/TypedAst.fs:202`, typed `src/TypeCheckInfer.fs:1370-1374`),
  `IRBlocked` (`src/IR.fs:193`) all exist; `docs/formalism.md:785` specifies it;
  **there is no parser arm** (`tests/Test_Loops.fs:37-38` says so in as many
  words), reverse-mode AD refuses it (`src/GradNormalize.fs:247-248`), and it is
  a *virtual array* (an iteration source), not a storage attribute. It is not the
  construct this plan needs and its name is in the way.
- **`Dist<…>` is taken** by the PPL moment tower (`src/Ast.fs:201`,
  `src/ParserTypes.fs:291-308`, `src/TypeLower.fs:506`). The audit's sketch
  spelling is unavailable.
- **Tests**: 21 differential cases, not 22 (`tests/MpiTests.fs:329-349`), each run
  serial-vs-MPI at `-n {1,2,4}` with outputs compared byte-for-byte
  (`:88-141`); the header states the doctrine plainly: *"the SPMD invariant makes
  rank count unobservable"* (`:5-6`). `--mpi N` exists only on `run`
  (`src/Cli.fs:98-109`) — `emit` cannot show decomposed C++.

## 2. Type surface

### 2.1 Two orthogonal concepts, one spelling

```blade
type Lat  = Idx<1440>
type CLat = Chunked<Lat, 64>          // chunk grid, edge 64. LOCAL. (P0)
type DLat = Chunked<Lat, 64, ranks>   // + chunks placed on MPI ranks   (P1)
type SLat = Chunked<Lat, store>       // edge inherited from the store  (P0.5)
```

`Chunked` / `ranks` / `store` are free identifiers today (grepped: no hits in
`src/`). PascalCase, in type position, parsed beside `SymIdx` in
`src/ParserTypes.fs`.

**Chunking** (arg 2) says *how this axis's cells are grouped for storage and
scheduling*. **Placement** (arg 3, optional) says *which grouping unit lands on
which rank*. They are separate fields in the IR and separately refusable; they
share one surface because the second is meaningless without the first.

`Chunked<Lat, 64>` **is still `Lat`**: same extent, same provenance, same
subscripts, same `Nat<Lat>` element tags, same `A(i)` bounds safety. That is why
it is deliberately **not** named `ChunkIdx` and deliberately **does not** add an
`IxKind` (§2.2).

### 2.2 Where it lives in the IR: a field, not a kind, not a tag

Add one field to `IRIndexTypeG<'Ext>` (`src/Types.fs:908-918`):

```fsharp
Chunk: ChunkSpec option        // None everywhere today
// ChunkSpec = { Edge: ChunkEdge; Place: Placement }
// ChunkEdge = EdgeLit of int64 | EdgeFromStore of path:string * var:string
// Placement = PlaceLocal | PlaceRanks          // PlaceRanks(policy) later
```

Three candidate encodings were considered; the field wins on one decisive
argument.

- **New `IxKind` (`IxKChunked`)** — rejected. Every kind-dispatching walker
  refuses non-`IxKPlain` axes on sight (`flatShapeSignature`
  `src/CodeGenLoopNest.fs:2096`; `classifyMpiShape` `src/CodeGenCuda.fs:1748-1753`;
  the whole `IIndexTypeBehavior` family `src/IRStorage.fs:539-664`), so a chunked
  `Idx` would have to re-implement plain-axis behaviour everywhere, and could
  never compose with `IxKCompound`/`IxKRagged`, which are precisely the shapes we
  eventually want chunked.
- **Tag payload** (`"__chunk|64|ranks|Lat"`, following `__halowin|`
  `src/Types.fs:265-304` and `__irreps:` `:182-194`) — rejected on a composition
  argument: **P2 needs `halo<Chunked<I,K>, [-1,0,1]>`, and in that composition
  the Tag is already fully occupied by the halo payload** (`src/TypeLower.fs:1600`).
  Two payloads in one string field means inventing a composition grammar for
  tags, which is a worse problem than the one being solved.
- **The field** — chosen. It is orthogonal exactly as `Symmetry`, `Kind`, and
  `Dependencies` already are; it survives every `{ inner with Tag = … }` rewrite
  the halo/irreps machinery performs; and its blast radius (≈92 full-record
  construction sites, `grep -c "Dependencies = " src/`) is *mechanical and
  compiler-enforced* — a missed site is an F# build error, never silent wrong
  storage. That failure direction is the whole argument.

Guardrails, because a field is easy to ignore: `validateIndexType`
(`src/IRStorage.fs:683`) grows a chunk arm rejecting every kind/symmetry
combination not licensed by the current phase, and `classifyOutputStorage`
(`:701`) becomes the single allocation choke point (§3.1) so no emitter can
allocate a chunked array without having seen the spec.

### 2.3 Unification: refuse, never rechunk

One arm in `indexPairIncompatible` (`src/Unify.fs:747` — the *shared* predicate,
so ArrayElem and dist-axis matching cannot drift):

| left | right | verdict |
|---|---|---|
| `Chunk = None` | `Chunk = None` | compatible (today, unchanged) |
| `Chunk = None` (abstract `T^r` / parameter position) | `Some _` | compatible — the `SymNone` wildcard precedent (`src/Unify.fs:744`) |
| `Some {Edge=64}` | `Some {Edge=64}` | compatible |
| `Some {Edge=64}` | `Some {Edge=128}` | **INCOMPATIBLE** — BL3016-family, message names both edges and both declaration sites |
| `Some {Place=PlaceLocal}` | `Some {Place=PlaceRanks}` | **INCOMPATIBLE** |
| concrete `Chunk = None` | concrete `Some _` | **INCOMPATIBLE** |

**No implicit rechunk, ever.** dask silently rechunks and pays an all-to-all;
that is precisely the "same computation, two spellings, 3x apart" class the audit
made its sharpest meta-finding (`plan-fortran-killer.md` §0). An explicit
`rechunk<K>(A)` operator (an all-to-all, marked and costed) is P4.

Prerequisite risk, stated plainly: the concrete-vs-concrete refusal can still be
laundered through a `let`-ascription on a provider read — **audit bug B2**
(`src/TypeCheckInfer.fs:220`, `src/Unify.fs:744`). Chunking rides on top of a
known-open soundness hole; B2 should land first or in the same change.

### 2.4 Providers: the file's layout *is* the decomposition

This is the xarray-parity move, and the piece that makes P0 worth landing before
any MPI exists. Three grades, all supported by store metadata
(`ZarrProvider.fs:284`):

- **(a) Declared-and-checked** (P0.5). The user writes
  `Array<Float64 like Chunked<Lat, 64>, Lon>` and the provider validates 64
  against `meta.Chunks`, refusing on mismatch — the same discipline as the
  packed-layout validation at `IcechunkProvider.fs:2750`. A read then becomes a
  per-chunk **block copy** instead of the cell-by-cell scatter at
  `ZarrProvider.fs:1429-1445`.
- **(b) Inherited** (P0.5, the headline). `Chunked<Lat, store>` reads the edge at
  compile time — store paths are compile-time literals, so the metadata is
  available. `zarrDimToNamedIndexType` (called from
  `zarrStoreToModuleWith :1146`) attaches the edge to the store's named index
  type. A dim chunked *differently* by two arrays in one store refuses exactly
  like the existing conflicting-extent check at `:1131-1137`.
- **(c) Whole-store default** — **rejected for v1**. Making every zarr dim
  chunked by default would silently change the storage of every existing zarr
  program. Opt-in spelling only.

`icechunk` inherits (a) and (b) for free: its baked chunk tables
(`plan-icechunk-provider.md` §7) are indexed by flattened grid coordinate, which
is the same object as the ownership grid.

### 2.5 What dies at the surface

`blocked<I, K>` (`src/Ast.fs:453`, `src/IR.fs:193`, `docs/formalism.md:785`) is
deleted in the same change, or explicitly reserved with a BL1003-style steer
pointing at `Chunked`. It is unreachable (no parser arm), it means something
different (an iteration-order transformer, not a storage attribute), and leaving
two block-shaped names in the language is the exact confusion this plan exists to
remove. Deleting it also removes an AD refusal arm (`src/GradNormalize.fs:247`)
and three IR traversal arms (`src/IRMono.fs:1770`, `src/IRLift.fs:891`,
`src/Interp/Loops.fs:1486`).

## 3. Storage model

### 3.1 What `Array<T like Chunked<Lat,64,ranks>, Lon>` allocates

**Local (P0), placement `PlaceLocal`** — one contiguous **chunk-major pool**:
chunk `c` occupies `[c * 64 * Lon, (c+1) * 64 * Lon)`, ragged last chunk carrying
`tileWidth n 64 c` rows (`SimplexBlocksCore.fs:98`, reused verbatim). Cell
`(i, j)` is at `chunkBase(i / 64) + (i % 64) * Lon + j`. Total cardinality is
**unchanged** — a chunk-major pool is a *permutation* of the row-major pool, not
a different size. This is the "S1 brick-major deduced layout" from
`plan-simplex-blocked-compute.md` §5, and it is the layout Zarr already writes.

**Distributed (P1), placement `PlaceRanks`** — only owned chunks are allocated:

```
size_t nchunk        = ceil(n / 64);                     // compile-time
size_t clo = ((size_t)rank * nchunk) / size;             // balanced chunk run
size_t chi = ((size_t)rank + 1) * nchunk / size;
Chunked<double,1> A = { pool(cells over [clo,chi)), extents, clo, chi };
```

plus (P2 only, when a `halo` reaches this axis) a ghost pad of the derived depth
on each interior side. **No chunk map array is needed in v1**: ownership is a
contiguous run, so `owner(c) = c * size / nchunk` inverts in closed form. A
sparse `int32 map[nchunk]` is P4 (LPT ownership, §8.6).

The **v1 simplification that makes this tractable**: one schedule per distributed
axis, module-wide. Every array over `DLat` has the same owned set, so no
whole-program union analysis is needed to size an allocation. Kernel-specific
schedules, and therefore per-array owned sets computed as a union over uses, are
P4.

**Slabs are the degenerate case, provably.** Set `edge = ceil(n / P)`, so
`nchunk = P`, so each rank owns exactly one chunk. The only divergence from
today's math (`CodeGenCuda.fs:3164-3174`) is remainder distribution: today's
`q + (rank < rem)` spreads the remainder one row at a time; the chunk form puts
all of it in the last chunk. The chunk form is the correct one *because it keeps
chunk boundaries aligned with the store's*.

### 3.2 How the existing storage walkers see it

| walker | today | with a chunk spec |
|---|---|---|
| `classifyOutputStorage` (`IRStorage.fs:701-769`) | wreath / antisym / symmetric / dense | new **first** arm: any `ix.Chunk.IsSome` → `AllocChunked of ChunkPlan`, or `AllocUnsupported` for the refused combinations (§7). Being first mirrors the wreath short-circuit at `:716` |
| `AllocSpec` (`:432-454`) | 5 cases | `+ AllocChunked` |
| `allocRoutineFor` (`:464-484`) | placement-class dispatch | untouched — a chunk plan cannot be named from a bare `PlacementClass` (same honest refusal the wreath arm makes at `:474-478`) |
| `bufferGroupCardinality` `:243` / `deviceBufferCardinality` `:305` | global cell count | **split**: `globalCardinality` (unchanged, what `extents()` reports) and `localCardinality` (what `allocate` sizes). Every current caller keeps the global one |
| `flatShapeSignature` (`CodeGenLoopNest.fs:2090-2108`) | `None` for anything non-plain | **P0: `None` when any axis is chunked** (the flat path would walk a chunk-major pool as row-major — silently wrong). **P0b: add the chunk grid to `FlatGroup`** — signature equality then also proves *chunk-grid agreement*, and since chunk-major is a permutation shared by both operands, the flat loop is byte-identical to today's. This is why elementwise needs no communication (§4.1) — true at the *pool* level, not just the index level |
| `flatCellCount` `:2118` / `flatNestMatchesSignature` `:2137` | global | local under `PlaceRanks`; the `expected` level list gains the chunk driver |
| `LoopNestCodeGen.MpiSlab` (`IRStorage.fs:135`) | `bool` | **replaced** by `ChunkPlan option` (§4.0); `MpiSlab = true` is the one-chunk-per-rank plan |
| `planHaloCarousel` (`CodeGenLoopNest.fs:1239-1241`) | bails on `MpiSlab` | keeps bailing on any chunk plan in P0-P1; P2 re-enables it *within* a chunk (the carousel is a within-row rotation and chunk-interior rows are unchanged) |
| compact/symmetric pools (`buildSymmVec :332`, `AllocSymmetric`) | — | **refused in v1** (§7) |

### 3.3 Refused in v1 (storage)

Chunking on a compact group (`SymIdx`/`AntisymIdx`/`Hermitian`/`OrbIdx`);
chunking on `IxKCompound`/`IxKSparse`/`IxKRagged*`/`IxKGroup*`/`IxKDep*`/
`IxKIrreps`/`IxKPgIrreps`; more than one chunked axis per array; a *distributed*
axis that is not the leading axis; two distributed axes (2-D decomposition); a
distributed axis in an array that also carries a compact group. All BL7003 with
the reason named. Full list with phases in §7.

## 4. Iteration and communication derivation

### 4.0 The `ChunkPlan` record

Beside `FoldChunkPlan` (`IRStorage.fs:95-100`), on `LoopNestCodeGen`, replacing
the `MpiSlab: bool` field (`:135`):

```fsharp
type ChunkPlan = {
    Axis: int              // which loop level carries the grid
    Edge: int64            // compile-time
    Count: int64           // ceil(extent / edge), compile-time
    Place: Placement       // PlaceLocal | PlaceRanks
    Ghost: int64           // derived halo depth on this axis; 0 when none
}
```

The nest gains one level: the **chunk driver** outside the axis's level, with the
axis level's bounds rewritten to `[chunkLo(c), chunkHi(c))`. That is the same
substitution shape as today's slab rewrite (`CodeGenLoopNest.fs:1645-1649`) and
the fold-chunk rewrite (`:1654-1655`) — a third case in the same `header` match,
not a new emitter.

### 4.1 Elementwise / zip — **no communication, by construction**

Two operands over the same distributed axis have the same chunk grid (refusal,
§2.3) and the same placement, hence the same owned set (§3.1 v1 simplification),
hence identical local pools cell-for-cell. The nest runs over owned chunks and
writes owned chunks. Nothing crosses. Formally: alignment is a *type* property,
so no runtime alignment check is emitted and no `rechunk` all-to-all can hide in
an expression. This is the property dask cannot state.

### 4.2 Reductions — the fold refusal dies

`classifyMpiShape`'s `MpiIneligible "fold accumulation reorders floating-point
reduction"` (`CodeGenCuda.fs:1742-1743`) becomes a *licence check*, exactly as
the audit's item #2 proposed:

- Licence: `foldReorderLicensed` (`src/CodeGenExprSupport.fs:1139` — declared
  `comm`, non-empty `CommGroups`, or a recognised commutative-associative
  builtin body via `foldKernelBuiltinOp :1118`). This is *already* the licence
  gating cross-thread fold chunking (`CodeGenBinding.fs:2483, 2863`) and the
  `BLADE_FP_REASSOC` lane (`:2661`). No new annotation; unlicensed folds keep
  BL4016.
- Shape: chunk-local fold into a rank partial (the existing `FoldWrapper` /
  `FoldChunkPlan` machinery, one nesting level out), then a cross-rank combine.

**Combine form — a deliberate divergence from the obvious.** Default is
**`MPI_Allgather` of the P partials + local combine in ascending rank order**,
not `MPI_Allreduce`. MPI does not guarantee the association order of `Allreduce`
across implementations or topologies, so `MPI_Allreduce` cannot make the claim
Blade wants to make and its harnesses pin:

> **A distributed Blade fold is bit-reproducible for a fixed rank count, on any
> MPI implementation.** `co_sum` cannot say this; `MPI_Allreduce` cannot say
> this.

The extra cost is one `O(P)` scalar pass, unmeasurable against the collective
itself. `MPI_Allreduce` with a native op stays reachable as a measurement arm
(env-gated, read per-call per the `Build.fs` convention), never as the default —
"the fastest way is the only way" is subordinate to "the printed answer does not
depend on the machine".

Scalar-output kernels (`CodeGenCuda.fs:2940-2941`) become eligible on the same
licence. Element types stay gated on `mpiDatatypeOf` (`CodeGenState.fs:843-851`).

### 4.3 Halo stencils — the ghost-cell declaration

`halo<Chunked<Lat, 64, ranks>, [-1, 0, 1]>`. The offset list already computes the
depth: `haloShrinkOfTag` (`src/Types.fs:300`) is `(-min offs) + (max offs)`, and
`haloStartOffsetOfTag` (`:292`) is the leading pad. Derivation:

1. **Ghost depth** = `max(0, -min offs)` low, `max(0, max offs)` high. Allocation
   pads each owned chunk run by that much (`ChunkPlan.Ghost`).
2. **Neighbours** are compile-time: chunk `c`'s low neighbour is `c-1`, owner
   `((c-1) * size) / nchunk`. No neighbour discovery, no topology query.
3. **Exchange**: `MPI_Isend`/`MPI_Irecv` of the edge strips (contiguous in a
   chunk-major pool — `Lon` × depth cells, no derived datatype needed for a
   leading-axis halo), posted before the nest.
4. **Interior/boundary split**: the interior nest (rows `[lo+depth, hi-depth)`)
   runs while the exchange is in flight; `MPI_Waitall`; then the boundary rows.
   The split is a second `ChunkPlan`-driven bound rewrite in the same `header`
   match.
5. **Boundary policies.** Today `halo` only *shrinks* to the interior
   (`TypeLower.fs:1589-1592`). The reframing that makes distribution honest:
   **shrink is the `open` policy, and ghost exchange is what makes an interior
   chunk boundary not a domain boundary.** `periodic` / `clamp` / `reflect` must
   be added — and they are worth adding **serially first**, in P0, where they
   have no distributed content and are independently valuable.
   (`halo<I, [-1,0,1], periodic>` or a `where boundary(periodic)` clause; §8.)

Also required: `classifyMpiShape`'s `allReal` gate (`CodeGenCuda.fs:1731-1734,
1754`) learns that a halo slot over a chunked axis is *decomposable*, which is
the one place a virtual source becomes MPI-eligible.

### 4.4 Gather / materialise-to-all

An explicit `A |> gather` produces a plain (unchunked, replicated) array. Its
implementation **is today's Allgatherv** (`CodeGenCuda.fs:3182-3214`), moved from
"after every kernel, always" to "here, because you asked". Marked
`// [dist] gather: <n> cells, <P>-way` in the omp-marker style.

Print and provider write are the two sites that need a whole array on one rank
(`CodeGen.fs:2668`, `CodeGenFusion.fs:1170`). Proposal: they emit an **implicit
gather with a marker plus a BL4010 pin** suggesting the explicit `|> gather` —
not a hard refusal (a distributed hello-world must not be unwritable), not
silent (the collective is `O(N)` and must be visible). `--strict-pins` promotes
it, consistent with `src/Diagnostics.fs:280`.

## 5. What dies, and how the 21 tests survive

**Nothing dies on day one.** The replicate-everything model and the chunked model
coexist, keyed on the *type*:

- `where mpi` over **plain** axes → today's slab + Allgatherv, byte-for-byte.
  All 21 cases in `tests/MpiTests.fs:329-349` declare plain axes, so all 21 keep
  passing unmodified at `-n {1,2,4}`. Zero migration cost. This is not a
  concession; it is the property that makes the change landable.
- `where mpi` over a **`PlaceRanks`** axis → the new path.
- Mixing them in one array is BL7003.

Deprecation, once P1–P3 cover dense + folds + halo + reads: the replicated path
gains a **BL4010 pin** — *"this `where mpi` kernel replicates every array;
declare `Chunked<Lat, K, ranks>` to scale memory"* — then, one release later,
becomes an error. Name the milestone; do not schedule the removal until the
chunked path has run something real.

**The test harness gets a stronger oracle, not just more cases.** Today's
differential compares mpi-gate-on against mpi-gate-off (`MpiTests.fs:88-141`).
For distributed cases the oracle is *the same source with `ranks` dropped* —
`Chunked<Lat, 64>` versus `Chunked<Lat, 64, ranks>`. That isolates the
distribution from the chunking: chunk-order effects appear in the P0 differential
(chunked vs plain), placement effects in the P1 differential. A failure lands in
exactly one of the two.

Also fix, in P0, the two things that make this work inspectable at all
(`plan-fortran-killer.md` §8 already lists both): `--mpi` on `emit`/`compile`
(`src/Cli.fs:98-109` currently gates it to `run` only), and a `where mpi` row in
`docs/features.md` §16 stating what it does.

## 6. Phases

| phase | deliverable | touch points | gate | effort |
|---|---|---|---|---|
| **P0** | `Chunked<I,K>` local: type surface, `Chunk` field, unify arm, `AllocChunked` chunk-major pool, chunk-driver loop level, `flatShapeSignature` chunk discriminator, interpreter twin. **No MPI at all.** Plus: serial `halo` boundary policies; `emit --mpi`; features.md row; delete `blocked<I,K>` | `Types.fs:908`; `ParserTypes.fs`; `TypeLower.fs`; `Unify.fs:747`; `IRStorage.fs:432,683,701,243,305`; `CodeGenLoopNest.fs:1645,2090,2118,2137`; `Interp/Loops.fs`; ~92 record sites | differential: chunked vs unchunked printed output **byte-identical**, all extents incl. ragged last chunk; `blade test interp` green; **perf gate is I/O, not compute** (see below) | 4–5 wk |
| **P0.5** | Provider-inherited chunking: `Chunked<Lat, store>`; declared-edge validation; per-chunk **block-copy** reads | `ZarrProvider.fs:284,887,1131,1146,1391-1445`; `IcechunkProvider.fs:2706`; `ProviderRegistry.fs:53` | zarr/icechunk lanes green; read time on a real chunked store vs today's scatter | 1 wk |
| **P1** | `ranks` placement: owned-chunk allocation, chunk driver over owned chunks, `\|> gather`, print-gather + BL4010, **distributed folds** (Allgather partials + ordered combine) on `foldReorderLicensed` | `IRStorage.fs:135` (`MpiSlab`→`ChunkPlan`); `CodeGenCuda.fs:1729-1760,3131-3214`; `CodeGenExprSupport.fs:1139`; `CodeGenBinding.fs:2861`; `CodeGen.fs:2668` | 21 legacy cases unchanged; new cases at `-n {1,2,4}` against the placement-dropped oracle; **peak RSS per rank falls ~1/P** — the actual claim | 3–4 wk |
| **P2** | Ghost exchange for `halo` over a distributed axis: derived depth, neighbour Isend/Irecv, interior/boundary overlap; `allReal` gate learns halo-over-chunked | `CodeGenCuda.fs:1731,1754`; `TypeLower.fs:1559-1601`; `CodeGenLoopNest.fs:1239,1645`; `Types.fs:292-304` | 2-D Jacobi / advection at `-n {1,2,4}` byte-identical to serial; ghost-depth > chunk-edge refuses | 3–4 wk |
| **P2.5** | Distributed **non-leading** axis when the leading axis is a `let rec` recursion axis — the time-stepping shape P1/P2 exclude | `IRStorage.fs:701`; `CodeGenLoopNest.fs`; `TypeCheckInfer.fs` (rec-array arm) | RK4 / Jacobi time-stepper distributed over space | 2 wk |
| **P3** | Reads touch only owned chunks: `PackedReadOpts` → `ReadOpts { Owned; Window }`; ownership `continue` in the dense chunk loop; **Allgatherv dropped** | `ProviderRegistry.fs:26`; `ZarrProvider.fs:1391-1445,1589-1708`; `CodeGenBinding.fs:1415` | a distributed program materialises a global array **nowhere** — RSS×P ≈ single-node RSS | 2 wk |
| **P4** | Compact-domain ownership (path-order quadtree units, §8.6); LPT chunk→rank map; explicit `rechunk<K>`; per-array owned-set union; 2-D decomposition | `SimplexBlocksCore.fs:139,285`; `ZarrProvider.fs:1567,1606` | demand-driven | — |

**P0's gate is deliberately not compute performance.** The simplex plan's
equivalent bet — S0 iteration blocking over a packed pool — was measured and
**lost** (`plan-simplex-blocked-compute.md` §9: 1.07x *slower* at n = 6007;
`autoTileEdge` returns `None` to this day, `SimplexBlocksCore.fs:302-317`).
Justifying P0 on local speed walks into a known-failed gate. P0's payoffs are (i)
the type surface and walkers, proved by a differential rather than a stopwatch,
and (ii) **provider I/O**, where chunk-major-in-memory turns a per-cell scatter
into a per-chunk `memcpy` — exactly what the icechunk plan already predicted
("the store layout is then the in-memory layout, and chunk I/O is a straight
block copy", `plan-simplex-blocked-compute.md` §5 S1, `plan-icechunk-provider.md`
§7).

**On reusing `SimplexBlocksCore` for the dense grid: mostly don't.** A dense
chunk grid needs integer division, not combinadics; the actual dense math is
`gridDims` (`ZarrProvider.fs:887`). What P0 legitimately reuses is
`tileCount`/`tileWidth` (`SimplexBlocksCore.fs:94-98`, ragged-edge handling) and
the edge **policy** (`divisorTileEdgeIn :285` — prefer a divisor of `n` near the
target so no chunk row is ragged). The simplex plan's §6 MPI row is the standing
instruction **not** to unify the compute and ownership decompositions: brick
coarsening produces unequal units, fine for a shared-memory work queue and wrong
as per-rank ownership. Ownership units must be equal-cost; that is the quadtree,
and it is P4.

## 7. Refusals

All BL7003 ("MPI backend limit", `src/Diagnostics.fs:354`) unless noted; each
names the construct and the phase that would lift it.

**Type / storage**

1. `Chunked<SymIdx<r,N>, K>` and every compact sibling (Antisym, Hermitian,
   OrbIdx) — v1; P4.
2. `Chunked<CompoundIdx…>`, `SparseIdx`, `RaggedIdx*`, `GroupOuter/Member`,
   `DepIdx*`, `IrrepsIdx`, `PgIrrepsIdx` — no static grid, or the grid is not
   the storage unit.
3. More than one chunked axis in one array — v1.
4. A distributed axis that is not the leading axis — v1; P2.5 lifts it for the
   `let rec` case only.
5. Two distributed axes (2-D decomposition) — P4.
6. Chunk edge ≤ 0, or ≥ extent (a single chunk is a plain axis — say so, don't
   silently accept).
7. Mismatched chunk edges between two operands — **BL3016-family**, names both
   edges and both declarations. No implicit rechunk.
8. Concrete unchunked array unified against a concrete chunked one — same code.
9. `transpose` / `decompact` across a chunked axis — v1.

**Iteration / communication**

10. A read `A(e)` on a distributed axis where `e` is not the iteration index or a
    `halo` window offset — *the* refusal, the reason locality is in the type. The
    message must say which chunk the subscript would need and which rank owns it.
11. `let rec` whose recursion axis is distributed — the recursion is a sequential
    dependence along exactly that axis. Permanent, not phased.
12. `where mpi` over a distributed axis whose kernel fold carries no reorder
    licence — **BL4016**, unchanged, the existing message.
13. Ghost depth exceeding the chunk edge (a stencil reaching past the immediate
    neighbour) — P2; lifting needs multi-neighbour exchange.
14. An array over a `PlaceRanks` axis reaching a replicating (`MpiSlab`) kernel —
    the two models must not silently mix.
15. `where mpi, cuda(...)` over a distributed axis — v1; the CUDA lane is
    structurally capped (`plan-fortran-killer.md` §9) and the offload story is
    `plan-mlir-backend.md`'s.
16. `extents(A)` returning anything but the **global** extent — not a refusal but
    a non-feature: there is no surface for local extent (§8.5).

**Marked, not refused**

17. Implicit gather at print / provider write: emitted with a
    `// [dist] gather` marker and a BL4010 pin.

## 8. Risks and open questions

**8.1 Printing.** `main`'s print block is rank-0-guarded (`CodeGen.fs:2668`)
because replication makes rank 0's copy complete. Under distribution it is not.
The proposal (implicit gather + marker + BL4010) trades a little silence for
usability; the alternative (hard refusal, forcing `|> gather`) is more honest and
makes every example noisier. **Open**: which, and whether `print` of a
distributed array should print *per-rank chunk ranges* under `--verbose`.

**8.2 The interpreter twin.** `src/Interp/` has no MPI concept at all — `where
mpi` is already inert there, so `PlaceRanks` degenerates to size = 1 for free.
Chunking is the harder half: the local chunk-major *pool order* differs, and the
interpreter's printers are pool-order-sensitive in places
(`Interp/ArrayOps.fs:1766, 1954`). Values are unchanged, so the twin is
"materialise logically, ignore the chunk spec" — but the licensed **chunk-blocked
fold** reassociates, and the interpreter must keep the serial canonical order.
That is the standing licensed-path-is-not-the-tested-path policy
(`plan-simplex-blocked-compute.md` §7), and it means the distributed fold's
correctness rests on the MPI differential, not on `test interp`.

**8.3 `let rec`.** Refused when the recursion axis is distributed (§7.11) — but
that is the *leading* axis, and v1 also refuses distributing non-leading axes
(§7.4). So v1 excludes time-stepping entirely, which is the flagship HPC shape.
This is the sharpest limitation in the plan and P2.5 exists solely to remove it.
It should move earlier if a real program shows up first.

**8.4 Ragged / grouped axes.** No static chunk grid, so refused. The interesting
case is `RaggedIdx` with *inline* lengths, where a prefix-sum gives runtime chunk
boundaries — but then ownership is not a compile-time function of `(rank, size)`
and the whole invariant (§0) collapses. Correct answer is probably "chunk the
outer axis, never the ragged one". Open.

**8.5 `extents()` global vs local.** **Global, always.** Local extent gets no
surface. Reason: the SPMD invariance property the harness pins — *"rank count is
unobservable"* (`MpiTests.fs:5-6`) — is what makes `-n {1,2,4}` a differential at
all, and a rank-dependent `extents()` destroys it. A program that wants a local
extent is hand-authoring decomposition, which is the thing the language exists to
prevent. `blade run --verbose` may print the map; it is not a value.

**8.6 Load balance on compact domains (P4).** Chunk cost varies once the domain
is a simplex: a diagonal tile-multiset block holds `C(w+m-1, m)` cells, an
off-diagonal one holds `w^r` (`SimplexBlocksCore.fs:106-112`). The prior art is
the `schedule(dynamic)` reasoning at `CodeGenLoopNest.fs:551-579` — but that
analysis is about a **work queue** (makespan ≈ ideal + last chunk started, so
ascending order is already LPT for shrinking triangular bounds). MPI ownership is
a **static partition**, so the analog is not dynamic scheduling but **greedy LPT
over compile-time chunk costs** — every cost is a `blockCellCount` call at
compile time, so the partition is computed by the compiler and baked. That is
something OpenMP approximates at runtime and MPI cannot express at all. The
ownership *unit* must stay the equal-size quadtree (`pathMultisets`/`pathRows`,
`ZarrProvider.fs:1567`), never the brick coarsening — the simplex plan's §6 is
explicit that the two schemes share leaf structure and nothing more.

**8.7 Determinism across rank counts.** Fixed-`P` reproducibility is achievable
and claimed (§4.2). Cross-`P` bit-identity for a floating-point fold is not, and
must not be implied anywhere. The existing MPI differential compares outputs
across `-n {1,2,4}` (`MpiTests.fs:124-141`); distributed-fold cases therefore
need **integer-valued f64 corpora or invariant checks**, never a float tolerance
— the same discipline `plan-llvm-backend.md` §6 already applies to licensed
lanes.

**8.8 The `Chunk` field's silent-propagation hazard.** ~92 construction sites get
`Chunk = None` (compiler-enforced), but the ~80 `{ inner with … }` rewrites
*propagate* the field, which is usually right and occasionally wrong (a
projection that drops an axis's storage meaning while keeping its record). The
existing precedent is the `IxKind`/`Tag` agreement validator
(`src/Types.fs:120-125`); the mitigation is symmetric: a `validateIndexType` arm
(`IRStorage.fs:683`) plus an `IRValidate` module-wide check that no array
reaching allocation carries a chunk spec the allocator did not consume.

**8.9 Bug B2 is a prerequisite.** The whole refusal table in §2.3 is defeatable
through a `let`-ascription that launders a provider read's true shape
(`TypeCheckInfer.fs:220`, `Unify.fs:744`). Land B2 first.

**8.10 Open syntax questions.** Boundary policy spelling: third argument to
`halo` (`halo<I, [-1,0,1], periodic>`) versus a `where boundary(periodic)`
clause. Placement policy spelling: `ranks` versus `ranks(block)` /
`ranks(cyclic)` / `ranks(lpt)` — v1 needs only the bare form, but the grammar
should admit the argument now rather than break later. `gather`: a keyword, a
builtin, or a stdlib function.

## 9. Cross-references

- `plan-fortran-killer.md` §4 is the audit this refines; its Arc 4 items #2
  (distributed reductions on the existing licence) and #3 (`halo` as the
  ghost-cell declaration) land here as P1 and P2. Its item #4 (`<&>` as an `omp
  task` boundary) is independent — do not entangle it.
- `plan-simplex-blocked-compute.md` §5 (S1 brick-major layout) is the local half
  of P0's storage; its §6 MPI row is the standing instruction **not** to unify
  the compute and ownership decompositions; its §9 P1 outcome is why P0's gate is
  I/O and not FLOPs.
- `plan-icechunk-provider.md` §7 (baked chunk tables, `ChunkSource` /
  `ChunkFetchEmitter`) is the seam P0.5 and P3 extend; its correction (a) — that
  `genAssemblePackedBlocks` does **not** route through the shared core — is the
  reason compact-domain ownership is P4 and not P3.
- `src/providers/StreamingIONotes.md` §P6 already specifies MPI × streaming with
  ownership = output cell ranges; P3 should supersede it, not duplicate it.
- `plan-mlir-backend.md` owns the offload story; `CodeGenCuda.fs` is not to be
  extended (`plan-fortran-killer.md` §9).
