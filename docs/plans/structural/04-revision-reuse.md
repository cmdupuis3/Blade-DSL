# 04 -- Revision reuse: cache pure output tiles across Icechunk snapshots

Status: **v1 BUILT 2026-09-07 (feat/revision-reuse)** as LEADING-AXIS tiles --
the section-4.4 re-scope (a tile = one chunk of the leading axis, all trailing
axes, so the fixture gives 1-of-2 rather than 1-of-4). What landed: the planner
`src/CodeGenTiles.fs` (admission = the fusion pass's lifted predicates
`IRMono.tilePlainInfo` / `tilePlainKernel` / `tilePureBody`, capture-free
kernels, whole-variable dense icechunk inputs sharing the output's shape and
chunk grid; every tile's key = SHA-256 of the kernel's printed IR + operand
order + output type, the output geometry and tile bounds, and per input the
canonical repo path, node id, `zarr.json` hash and `chunkIdentityText` of the
tile's chunks); the tile run loop through the MPI-slab / segment-run
outer-level substitution (`CodeGenCuda.genApplyCombinator`, `tileLoopLines`
in CodeGenState) with `blade_tiles::probe/load/store`
(`src/cpp/blade_tilecache.hpp`, C stdio, header-validated files under
`<dir>/<key[0:2]>/<key>-<toolchain>.tile`); read avoidance by a probe hoisted
to the input's read (`tileProbeLines`) and a need-masked two-phase assembly
(`ZarrProvider.genAssembleFlatPhased`, `IcechunkProvider.genReadVarPhased`:
phase 1 reads the unhit tiles' chunks, phase 2 after the tiled binding reads
the remainder for the print pass) -- hoisted only when nothing between the
read and the binding observes the input; `-DBLADE_TOOLCHAIN_ID` from Build.fs
(compiler, flags, `-march=native` selection, runtime headers) keys the file
name so no hashing happens at run time; eviction (the exe cache's count cap)
in the compiler. Gate: `BLADE_TILE_CACHE` (unset = OFF; the exe cache's
grammar otherwise), census under `BLADE_TILE_CACHE_VERBOSE`. Pins:
`tests/IcechunkTests.fs` section 22 (one-changed-chunk fixture: 5 chunk
files; E@s1 cold 2/2 computed, warm 2/2 hit; E@s2 recomputes 1 of 2 tiles and
its compute phase reads 2 of 4 chunks; cold = warm = interpreter stdout; the
untiled emission carries no tile code).

**Scale run (4.3 item 4), 2026-09-07** -- 4000x4000 float64 chunked 500x500
(64 chunks, 8 leading-axis tiles), one changed chunk, exe cache off. Run in
two passes, because the first pass measured the wrong thing:

*Pass 1, printing everything.* Flat at ~31 s in every configuration (off 29.8
/ 35.4, cold 31.7, warm 31.7, one-changed 31.0) -- the CLI lane prints every
top-level binding, and 231 MB of formatted stdout (`A` and `F` in full) dwarfs
the 128 MB read and the 16M-cell map. Risk 2, measured: the gate could not be
read at all through the printing.

*Pass 2, after `--print` landed* (section 3.5's follow-up, now built:
`blade run --print total`). The 31 s becomes 2.9 s, of which 2.6 s is the
COMPILE -- a new snapshot always recompiles (1.3), so end-to-end is compile-
bound at this size and the honest comparison is the executable's own time,
medians of 9 interleaved samples:

| program (16M cells) | warm, tiled | untiled | ratio |
|---|---|---|---|
| `x * 2.0 + 1.0` | 211 ms | 259 ms | 1.23x |
| `exp(sin x) + log(1 + x^2) + sqrt(x + 3)` | 211 ms | 522 ms | **2.47x** |

and on the second snapshot (one chunk of 64 changed) the census is exactly the
design's: `computed 1/8, hit 7/8`, `read 8/64 (compute 8, remainder 0)`.

**The 2x gate is met -- on a task whose kernel is worth caching**, and the
cheap map says why the qualifier is not a hedge: a tile holds the OUTPUT of
one input chunk, so for an elementwise map the cache is the same size as the
input it replaces and a warm run trades input bytes for tile bytes, keeping
only the compute. The saving is therefore the RECOMPUTE, and it shows exactly
when recompute costs more than re-reading it. A second finding from pass 2:
read avoidance was not real until `--print` existed -- the remainder read
(phase 2) fetched every chunk the compute phase skipped, because the print
pass observes `A`. Codegen now emits phase 2 only for an input something
actually observes (a later binding, a function body, or the print selection);
an unobserved input releases its buffers and never fetches those chunks, which
is what turns `read 64/64 (compute 0, remainder 64)` into `read 0/64`.

NOT done: per-level tile bounds (the 1-of-4 geometry), the halo consumer,
captured scalars in the key (declined). Originally: DESIGN (2026-09-06).
Elaborates item 4 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md)
("Make versioned scientific computation reuse unchanged work"). Baseline:
working tree at `bd019dc`; every `file:line` below was read at that state, and
the two emission experiments in section 1.7 were run with the installed
`bin/Release/net10.0/Blade.exe` from a private scratch directory.

Companions: [plan-icechunk-provider.md](../plan-icechunk-provider.md) (the
provider whose content identities this design keys on),
[plan-fortran-killer-2.md](../plan-fortran-killer-2.md) section 3 (shared
purity/effect facts) and section 7 (input manifests, run records, "do not add a
second cache"), [plan-distributed-memory.md](../plan-distributed-memory.md)
section 2 (`Chunked<>` as a field on the index record -- the eventual
first-class carrier of a chunk grid), and
[StreamingIONotes.md](../../../src/providers/StreamingIONotes.md) (tile-blocked
iteration aligned to store chunks, P4).

Reading key: **verified** = read in source at the cited line or observed in an
emission; **inferred** = a design conclusion or an unverified belief, marked as
such.

---

## 0. The claim in one paragraph

An Icechunk checkout is resolved at compile time to an immutable snapshot, and
every chunk of every variable the program reads is baked into the executable as
a `(chunk id, offset, length)` table. So the compiler already knows, per chunk,
a content identity that is stable across commits that did not touch that chunk.
For a top-level binding that is a *pure, index-local* traversal (an elementwise
map or a fixed-offset halo) over such reads, the compiler can therefore emit the
traversal one *tile* at a time -- tile = one chunk of the input grid,
transported through the access -- and give every tile a key that is a hash of
(the tile's emitted C++ + toolchain identity, the content identities of exactly
the input chunks it reads, the output/tile geometry, any captured scalars). A
local on-disk store keyed that way lets a run against a later snapshot skip both
the recomputation *and the chunk reads* of every tile whose dependencies did not
move, while producing byte-identical stdout, because printing consumes the
materialized value and never the cache. Everything else -- reductions, mutation,
effects, non-Icechunk providers -- is refused into the ordinary emission path,
not approximated.

---

## 1. Verified current state

### 1.1 Chunk identity in the Icechunk provider

- A repo's only mutable file is `$ROOT/repo`; `snapshots/`, `manifests/`,
  `chunks/` are immutable (`src/providers/IcechunkProvider.fs:5-12`). The
  compiler resolves `ref -> snapshot` once per compilation and *pins* it:
  `RepoPinTable` records the first `(mtime ticks, length, sha256/8)` stamp of
  the repo file per canonical repo path, `AsyncLocal` per compilation
  (`:993-1028`; the content-hash rationale -- a branch reset keeps the file
  length -- is at `:959-990`).
- `buildChunkTable` unions an array's manifests into one row-major
  `ChunkLoc[]` over the chunk grid (`:1561-1592`). `ChunkLoc` is `Fill`
  (no manifest covers the coordinate), `Inline bytes`, or
  `Native { ChunkId; Offset; Length }`. `ResolvedArray` carries `Root` (the
  path *as written*, so the baked path resolves against the exe's cwd),
  `SnapshotId`, `Node`, `Meta` and `Table` (`:1598-1614`), memoized per
  (canonical key, as-written key, variable, pinned stamp) in `arrayMemo`
  (`:1653-1660`).
- Content identities are already *rendered* for the axis-identity machinery:
  `chunkLocText` prints `-` / `i:<sha256 of inline bytes>` /
  `c:<chunkId>+<offset>+<length>` (`:1789-1794`), and `varFingerprint` folds a
  whole variable to `node=<8-byte node id>;user=<sha256 zarr.json>;chunks=<sha256
  of "grid|refs">` (`:1825-1841`). Both are `private`; `coordFingerprint`
  (`:1843-1852`) uses them only for coordinate arrays, and the result lands in
  `AxisIdentity.CoordFP` (`:1088-1105`). The axis *tag* that reaches types is
  `__icaxis|<dim>@<repoDirName>:<16 hex of sha256(canonical repo path)>[#n]`
  (`taggedIdentity`, `:1974-1991`; `axisTag`, `:1993`) -- it identifies the
  *repo and dim*, not the data.
- The provider's own doc on `varFingerprint` (`:1806-1812`) states the failure
  direction: a manifest rewritten to new ids with identical bytes compares
  *unequal* (false negative, refuses sound arithmetic), never a false accept.
  The same asymmetry governs this design (section 2.3).
- **Fixture writer ids are content-derived**: `IcechunkWrite.planChunks` names a
  native chunk file `digest seed ("chunk/" + arrayName) bytes` (12 bytes)
  (`src/providers/IcechunkWrite.fs:557-600`; the `ChunkPlan` doc at `:547-555`
  says "content-addressed"). The real format's id minting is *not* stated in the
  vendored notes (`src/providers/icechunk-format/README.md` has no such line;
  `schema/common.fbs:24` only defines `ObjectId12`). **Inferred:** the reference
  writer mints random ids; what *is* format-guaranteed is that an unchanged
  chunk keeps its manifest ref (immutability), so `same ref => same bytes` holds
  everywhere and `same bytes => same ref` holds only when the writer did not
  rewrite the chunk. This design relies on the first direction only.
- `ProviderSpec.Fingerprint` for icechunk is the *snapshot id* (`fingerprint`,
  `:2388-2400`); `VersionStamp` is the pinned repo mtime (`versionStamp`,
  `:2354-2361`). `GenStreamOpen`/`GenStreamFiber` are `None`; `GenWriteVar`
  refuses (writes are commits) (`:2770-2804`).

### 1.2 How a provider read reaches the binary

- `ProviderReadSpec` = `{ Provider; FilePath; VarName; VarType; MaskName;
  MaskType; Window (packed sub-simplex only); Streamed }`
  (`src/IR.fs:1358-1378`), keyed by the receiving binding's `IRId` in
  `IRModule.ProviderReads` (`:1428-1430`).
- Lowering recognizes reads **only at module top level**: the `TDeclLet` arms
  for `tryCompoundRead` / `tryPlainRead` / `tryStreamRead` / `tryWindowRead`
  (`src/Lowering.fs:2030-2094`; matchers at `:1715-1856`) bind a unit
  placeholder and record the spec. In expression position `TExprRead e`
  lowers to `e` itself (`:714-718`), i.e. a read nested inside a function body
  is not a provider materialization.
- Codegen dispatches the binding to `genProviderReadBinding`
  (`src/CodeGenBinding.fs:29-30`, body `:1337-1470`): streamed arm, wreath arm,
  packed arm, then the dense arm `pspec.GenReadVar` (`:1462-1470`). Icechunk's
  `genReadVar` (`IcechunkProvider.fs:2655-2690`) calls the shared
  `ZarrProvider.CppZarr.genAssembleFlatVia` with `icechunkChunkFetch`
  (`:2487-2640`), which bakes `static const long long <v>_icoff[n]`,
  `<v>_icpath`, `<v>_icfile`, and inline byte blocks, with `Present = "<v>_icoff[i] >= 0"`.
- `genAssembleFlatVia` (`src/providers/ZarrProvider.fs:1393-1478`) emits nested
  `for` loops over **every** chunk-grid coordinate, `if (Present) { Read;
  copy into <v>_flat } else { fill or exit }`, into a full-size flat buffer;
  `genReadVar` then copies `_flat` into a nested `Array<T, r>`. There is no
  chunk-subset or lazy path.
- The interpreter twin `materializeProviderRead` (`src/Interp/Run.fs:340-420`)
  reads the whole variable through `ReadVarData` (with the BL8012 shape twin);
  streamed/compound/windowed/packed arms raise `InterpUnsupported`.

### 1.3 What Build.fs's cache identity covers

- The executable cache (`src/Build.fs:513-560`) keys one g++ invocation by
  SHA-256 over `"blade-exe-cache-v1"`, `gppIdentity` (resolved g++ path + first
  `--version` line, `:566`), the normalized command line, `nativeTargetIdentity`
  when `-march=native` (what native *selected*, hashed, `:599-634`),
  `runtimeHeaderDigest` (the shipped runtime headers, `:636-650`),
  `linkedDllStamp`, and the whole `.cpp` text (`exeCacheKey`, `:672-693`).
  Location `%LOCALAPPDATA%\Blade\exe-cache`, gate `BLADE_EXE_CACHE`
  (unset/on/off/absolute path, read per call, `:536-548`), verbose
  `[cache] hit|store <hash8>` via `BLADE_EXE_CACHE_VERBOSE` (`:556-559`),
  eviction caps 8192 entries / 6 GiB pruned oldest-first on store
  (`:698-726`), atomic temp-then-`File.Move` store (`:751-770`).
- Consequence for this design: **a new snapshot always changes the `.cpp`**
  (the baked `_icoff`/`_icpath` tables and the snapshot id in a comment) and so
  always misses the exe cache and recompiles. The tile store therefore cannot
  live inside the exe cache and cannot be keyed by executable; it must be
  content-keyed and consulted by the running program.
- `Build.fs` compiles **after** `CodeGen.fs` (`Blade.fsproj:375` vs `:417`), so
  codegen cannot call `gppIdentity`/`nativeTargetIdentity` directly.
- `runExecutable` runs the exe with `WorkingDirectory = exe directory` and
  appends stderr to the captured text as `"\n[stderr]: ..."` (`:1237-1262`).

### 1.4 What `SessionMemo` caches and invalidates

`src/Interp/Run.fs:33-91`: a map **binding name -> (IRType, Value)** for the
interpreter lanes (REPL/notebook), plus `MutationFree`, `FrameEmitters`,
`Frames`. Adoption (`:478-512`) skips a binding's initializer when the name is
present and `memoTypeAgrees` (`:129-134`, `canonTypeKey`-based, occurrence-id
independent) holds; only `memoizableValue` values are published (`:145-155`:
scalars, dense `VArray`, tuples/structs of those; no deferred/closure values).
Correctness rests entirely on the **caller's prefix rule** (the session
snippets must start with the snippets that produced the memo). It is a
whole-value, per-session, in-process cache with no notion of input identity or
partial invalidation -- exactly as the memo says. Nothing in it is reusable
here except the instrument idea (`lastMemoAdopted`/`lastMemoEvaluated`,
`:100-101`: "a memo hit and a recompute print the same value, and wall clock is
not a pin").

### 1.5 Output tiling in codegen: none

- The apply nest is emitted whole. `genLoopNest` (`src/CodeGenLoopNest.fs:2031`)
  -> `genLoopNestStreamed` (`:1365`); each `LoopIndexBinding` carries `Extent`,
  `BoundDependencies`, `StrictOffset`, `FusedRank` (`src/IRStorage.fs:49-80`),
  and the header renderer bounds every level at `[0, extent)` except two
  outer-level substitutions: the MPI slab `[__blade_mpi_lo_<out>,
  __blade_mpi_hi_<out>)` and the fold chunk `[__rlo, __rhi)`
  (`CodeGenLoopNest.fs:1642-1658`). That is the only bounded-iteration precedent,
  and it exists for the **outermost level only**.
- Index-free elementwise nests collapse to one flat loop over the contiguous
  pool (`__fk` over `__fp_cells`, `:2035-2066`); the soundness note records that
  the output pool is a fresh allocation distinct from every input
  (`src/EmitCpp.fs:48-55`).
- No chunk-aligned iteration exists anywhere in `CodeGen*`/`Lowering`/`IRStorage`
  (grep for `tile|chunk-aligned|per-chunk` finds only the unroll-and-jam
  comments in `CodeGenExpr.fs:2670-2740`). `StreamingIONotes.md` P4 proposes
  tile-blocked *pair* iteration aligned to store chunks, unbuilt.
  `plan-distributed-memory.md` section 2.2 proposes `Chunk: ChunkSpec option`
  on `IRIndexTypeG` (record at `src/Types.fs:908-918`), with a store-inherited
  edge (P0.5) -- also unbuilt.

### 1.6 Existing purity analyses (neither is a universal proof)

- Fusion's `pureBody` (`src/IRMono.fs:1090-1108`): declines `IRDisplayEmit`,
  `IRAssign`, any `IRApp` whose head is not a resolvable non-static callable,
  recursing through callees; module-local. Its host/inner gates `plainInfo`
  (`:1116-1133`) and `plainKernel` (`:1135-1139`) are the closest existing
  description of "a plain dense elementwise nest with a scalar kernel".
- `IRPrint.exprAttrs.IsPure` (`src/IRPrint.fs:480-597`): only `IRDisplayEmit`
  is impure; the header says nothing consumes it and that it is not a complete
  mutation/exception model. **Not sufficient** for this design.
- `plan-fortran-killer-2.md` section 3 proposes conservative effect summaries
  (reads, writes/aliases, I/O, possible failure, unknown calls, FP contract),
  with "unknown is not pure" and the distinction between *repeatable* and
  *movable* evaluation; step 1 landed, steps 3-4 are open.
- IR effect nodes that exist: `IRDisplayEmit` (`src/IR.fs:137`), `IRAssign`
  (`:275`), `IRForRange` (`:276`); provider writes (`ProviderWrites`); random
  fills (`RandomFillSpec`, `:1380-1400`: `FillModulus` is nondeterministic,
  `RandGen` is keyed-deterministic). Runtime aborts are BL8001-BL8012
  (`src/Diagnostics.fs:381-432`).

### 1.7 Two emissions, verified (private scratch dir, `blade emit`)

Program E (elementwise), against the committed
`examples/data/station_temps.icechunk` (`temp` is 24x10x12 chunked `[8,10,12]`
= 3 chunks along `time`; `examples/tools/StationSpec.fs:80-86`):

```blade
import icechunk as ic
let repo = ic.load("examples/data/station_temps.icechunk")
let ck = repo.checkout("main")
let A = ck.vars.temp |> ic.read
function warm(x: T^0) -> T^0 = x * 1.8 + 32.0
let F = method_for(A) <@> warm |> compute
let total = reduce(F, (+), axes = 3)
```

Emitted C++ (line numbers in the emission): baked `A_icoff[3]`, `A_icpath[3]`,
`A_icfile[3]` (212-222); grid loops `A_c0 < 3` / `A_c1 < 1` / `A_c2 < 1` opening
one `std::ifstream` per chunk **unconditionally** and scattering into
`A_flat[2880]` (226-262); flat -> nested copy into `Array<double,3> A`
(266-271); `F` computed in flat-pool mode, `__fp_F[__fk] = warm(__fp_A[__fk])`
over `2880UL` cells (283-288); the print pass at program end prints **both `A`
and `F` in full** and `total` (313-335). `blade run` printed
`total = 202320` and `F = [68, 67.82, 67.64, ...]` under `setprecision(15)`.

Program H (radius-1 halo along the chunked axis):

```blade
type TimeIdx = ck.index.time
type LatIdx = ck.index.lat
type LonIdx = ck.index.lon
let dt = method_for(range<halo<TimeIdx, [-1, 0, 1]>, LatIdx, LonIdx>)
         <@> lambda(wt, la, lo) -> A(wt(1), la, lo) - A(wt(-1), la, lo) |> compute
```

Emitted: `dt_extents = { 22, 10, 12 }` (interior shrink), nested loops with
`int64_t wt = (__i0 + 1L)` (the start offset from `haloStartOffsetOfTag`,
`src/Types.fs:292-295`), body `A[(wt + 1L)][la][lo] - A[(wt + -(1L))][la][lo]`,
a row hoist `__orow_dt = dt[__i0][__i1]`, and a `BL8009` halo-extent guard
before the nest. So the *access* of a halo nest is fully determined by the
`__halowin|d:<inner>|<offsets>` tag (`Types.fs:265-304`) plus the plain slot
positions -- no analysis beyond the tag is needed for the closed family in
section 2.

### 1.8 Facts that constrain the design

1. **Every top-level binding prints.** `genPrintStatements`
   (`src/CodeGen.fs:2299-2336`) prints every binding except an unforced
   deferred one and a *streamed* provider read; the interpreter twin
   (`src/Interp/Print.fs:350-368`) is the same, with an `only` filter that
   exists solely for the REPL lane (`Run.fs:618` `printOnly`). The CLI has no
   print-selection flag (grep of `src/Cli.fs`). Hence in the compiled CLI lane
   the input array `A` is itself an observed value: its chunks are *demanded by
   the print*, and skipping their reads changes stdout. Section 3.5 handles this.
2. **The nest cannot be re-bounded below the outer level today** (1.5).
3. **The read happens at the binding's position**, before any consumer
   (`CodeGenBinding.fs:29-30`; the interpreter mirrors the position,
   `Run.fs:513-528`). Deciding which chunks a later nest needs therefore
   requires hoisting the cache probes ahead of the read.
4. **A snapshot change is a recompile** (1.3): the tile store must survive
   executables, and its key must not mention the executable.

---

## 2. The dependency model

### 2.1 Tile key -- exact definition

A *tiled binding* is a top-level `let F = <apply> |> compute` admitted by 2.4.
Its output grid is partitioned into tiles `t` (2.5). The key of tile `t` is

```
key(F, t) = SHA256( material(F, t) || captures(F) )
```

`material(F, t)` is a UTF-8 string fully known at compile time and baked as a
literal; `captures(F)` is the little-endian byte image of the tile task's
captured scalars in parameter order, appended at run time (empty for a
capture-free task, in which case the whole key is a compile-time literal).
`material` is the newline-joined list:

| term | content | why |
|---|---|---|
| `blade-tile-v1` | schema version | invalidate everything on a format change |
| `toolchain=<id>` | the value of a `-DBLADE_TOOLCHAIN_ID="..."` define that `Build.fs` adds to every tile-enabled compile: `gppIdentity` + normalized flags (minus the define itself) + `nativeTargetIdentity` (when `-march=native`) + `runtimeHeaderDigest` -- the exe-cache terms of `Build.fs:672-693` other than `cppText` and the DLL stamp | same bits require the same compiler, flags (`-O`, `-march`, `-ffp-contract`, `-fopenmp`), CPU selection and runtime headers; `Build.fs` compiles after codegen (1.3), so the define is the seam |
| `task=<sha256>` | the emitted C++ text of the tile task: the nest body template with tile bounds as parameters, plus every kernel function it calls transitively (e.g. `warm` and `__lambda_27` in 1.7), after alpha-renaming compiler-generated identifiers (`__lambda_N`, `__kN_M`, `__iN`, `__fzN`) in order of first appearance | the emitted text is what g++ compiles; it already reflects fusion, casts, FMA contraction sites, `omp` pragmas (BLADE_OMP_THREADS reaches it), and BLADE_FP_REASSOC-licensed fold shapes; renaming removes SSA-id drift so an unrelated added cell does not miss every tile |
| `out=<elem>;<extents>;<canonical tags>;<tile lo/hi per axis>;t=<tile coords>` | output element type, literal extents, index tags via `canonTypeKey`-style occurrence-independent rendering, this tile's cell box | the same task over a different geometry or tiling is a different tile |
| `in[k]=<canonical repo path>;node=<node id>;user=<sha256 zarr.json>;access=<identity \| halo:<axis>:<offsets>>;chunks=<chunkLocText for each chunk in the tile's read region, grid order>` | one line per input array position `k` | this is the content dependency: dtype/fill/shape via `zarr.json`, the stable node id, and the *per-chunk* refs of exactly the chunks read -- `varFingerprint`'s whole-variable `chunks=` hash would invalidate every tile on any change |

Value: the tile's cells row-major within the tile box, preceded by a 32-byte
header `{ magic "BLTL", version, elem size, rank, extents... }` used to reject
a corrupt/mismatched entry as a miss.

### 2.2 What "pure" must mean here

The tile task must be **repeatable, region-local, and store-after-complete**:

- *Repeatable*: same inputs and same text yield the same bits. For a
  per-cell nest with no reduction this follows from determinism of the emitted
  arithmetic under the pinned toolchain terms; OpenMP thread count does not
  enter a per-cell result, so `OMP_NUM_THREADS` is deliberately **not** in the
  key. It *would* have to be the moment a fold is admitted (the memo's
  reduction-tree caveat) -- that is why folds are excluded (2.4).
- *Region-local*: every cell of the tile depends only on the input cells in the
  tile's read region (2.5) and the captured scalars. Guaranteed by the closed
  access family, and by the fact that the output pool is fresh
  (`EmitCpp.fs:48-55`) so no read observes a write.
- *Store-after-complete*: a tile is written to the store only after its nest
  finished. A kernel that aborts (BL8006 bounds, BL8008 domain, ...) on some
  cell never stores that tile; a warm run recomputes it with the same inputs and
  aborts identically. Aborts are therefore preserved without any totality proof
  -- this is the *repeatable, not movable* distinction of
  `plan-fortran-killer-2.md` section 3 (a pure expression that may fail cannot
  be hoisted past a conditional): tiles are never moved past anything, only
  skipped when their identical evaluation has already completed once.

Relation to the section-3 effect summaries: the tile predicate is a **second
consumer** of the same facts fusion needs. v1 should lift `pureBody` +
`plainKernel` out of `fuseElementwiseChainsModule` into a shared
`IRMono.kernelIsPlainPure` rather than write a third copy; when the summaries
land, both consumers switch. Purity is never inferred from the absence of
prints: `IRDisplayEmit`, `IRAssign`, `mut` array parameters
(`IRCallable` mut positions), static callees, unresolved heads, and CUDA/MPI
kernels each decline explicitly.

### 2.3 Why coordinate identity alone is insufficient

The axis identity that already exists (`AxisIdentity.CoordFP`,
`IcechunkProvider.fs:1088-1105`) fingerprints the *coordinate variable*
(`lat`, `lon`, `time`). The committed demo store is the counterexample: `s_raw`
and `s_corrected` share every coordinate identity (a DATA-ONLY commit,
`StationSpec.fs:28-36`), so `ck1.vars.temp - ck2.vars.temp` typechecks, while
`temp` differs in all three chunks (the bias band `lo >= 8` spans every time
chunk). Two arrays with the same axes have different values; the key must carry
the *data* variable's per-chunk refs, which is what the `in[k]` line does.

Directionality (1.1): equal refs imply equal bytes (immutability); a writer
that rewrites identical bytes under a new id causes a miss, never a wrong hit.
The canonical repo path is in the key on purpose: it forbids cross-repo hits
whose only evidence would be an id equality the format does not promise
globally (cost: moving a repo cold-starts its tiles).

### 2.4 Exclusions and how each is detected (all at compile time, in codegen)

| excluded | detection site | behaviour |
|---|---|---|
| kernel effects: `IRDisplayEmit`, `IRAssign`, static/unknown callee | `pureBody` shape, `IRMono.fs:1090-1108` | not tiled; ordinary emission |
| `mut` array params, `comm`/`anticomm`/`omp`-fold metadata, CUDA, MPI, arity-poly kernels | `plainKernel`, `IRMono.fs:1135-1139` | not tiled |
| reductions and fold nests (`IRReduce`, `IRReduceCompute`, `FoldChunk`), Reynolds, T-dims, rank-raising kernels, triangular/symcom levels | `plainInfo`, `IRMono.fs:1116-1133`; `KernelOutputRank = 0` | not tiled (v1 keeps the prescribed serial fold untouched) |
| captured arrays (any capture that is not a primitive scalar) | `LoopNestCodeGen.Captures` element types | not tiled |
| inputs that are not whole-variable dense icechunk reads: `Streamed`, `MaskName`, `Window`, packed/wreath symmetry, `Provider <> "icechunk"` | `ProviderReadSpec` fields, `IR.fs:1358-1378`; `IRIndexType.Symmetry` | not tiled (Zarr/CSV/NetCDF files are mutable -- no compile-time content identity; the fold cache's `VersionStamp` is an mtime, not content) |
| inputs that are `rand.*`/`fill_random` bindings, literals, or computed arrays | input binding not in `IRModule.ProviderReads` | not tiled in v1 (a later arc can key a *computed* input by the tile keys that produced it) |
| non-identity, non-halo access (gathers, transposes, `w(o)` with non-literal `o`, subscripts that are not a loop index) | kernel-body subscript walk (section 3.2) | not tiled |
| output not a plain dense literal-extent array | `classifyOutputStorage` | not tiled |
| mutable external chunk references | virtual chunk refs are refused at manifest decode (`IcechunkProvider.fs:490-520`, `:781-807`); `Native` refs point inside the immutable repo | cannot reach a baked table |
| a repo that changed *between compile and run* (GC'd snapshot) | already a loud exit in the reader (`Icechunk error: chunk file ... cannot be opened`, emission line 238) | unchanged |
| `--mpi` programs | `mpiProgramOn ()` | tiling off for the whole program |

---

## 3. Design

### 3.1 Where the cache lives

A local on-disk store beside the exe cache: `%LOCALAPPDATA%\Blade\tile-cache\<key[0:2]>\<key>.tile`,
gated by `BLADE_TILE_CACHE` with the exe cache's exact grammar
(`Build.fs:529-548`: unset/`1`/`on` -> default dir; `0`/`off` -> disabled;
absolute path -> that directory; anything else -> disabled) **except that unset
means OFF for v1** -- the mechanism is opt-in until the gate passes. The
variable is read twice: at compile time by the compiler (decides whether to emit
tile phases at all) and at run time by the generated program (locates the
directory; if unset at run time the program computes everything and stores
nothing). Eviction mirrors `evictExeCache` (`Build.fs:704-726`) and runs in the
**compiler** before emitting a tile-enabled program, so no eviction code is
needed in C++. Stores are temp-file-then-rename (`std::filesystem::rename`,
which replaces on Windows -- **verify in the gate**); two processes storing the
same key write identical bytes, so a race is benign. Not the provider store:
writing into an Icechunk repo is a commit (`IcechunkProvider.fs:2760-2768`), and
the cache is a machine-local, discardable artifact.

Verbose census on stderr under `BLADE_TILE_CACHE_VERBOSE` (default silent, so
`InterpDiff`'s stderr comparison is untouched):
`[tiles] F: computed k/n, hit m/n; [chunks] A: read r/R (compute c, remainder d)`.

### 3.2 How the compiler recognizes a tile task

At `genModule`, before emitting bindings, a pre-pass over
`modul.Bindings` builds a `TilePlan` per admitted binding
`b.Value = IRCompute (IRApplyCombinator info)` (`IR.fs:94,114`):

1. `info` passes the lifted `plainInfo`; `kernelOf info` passes the lifted
   `plainKernel && pureBody` (2.2); no array captures; not MPI.
2. Access classification of the kernel body, a closed family:
   - **identity**: `info.Loop` is `IRMethodFor` over real arrays (possibly a
     co-iteration `zip`), each an `IRVar` whose id is in `ProviderReads` with a
     dense unmasked unwindowed unstreamed icechunk spec; the body reads each
     array only through its kernel parameter (this is the flat-pool-eligible
     shape, `CodeGenLoopNest.fs:2035-2066`).
   - **halo**: `info.Loop` is `IRMethodFor` over one `IRRange` whose slots are
     plain named index types or `__halowin|d:` slots (`HaloWinTag`,
     `Types.fs:275-290`; compound-inner `c:` slots decline); every array read in
     the body is `A(s_1, ..., s_r)` with `A` a provider-read binding as above and
     each `s_j` either the plain loop index of slot `j` or a window read `w(o)`
     of the halo slot `j` with literal `o` (the `IRHaloUnhash`/offset shapes at
     `Lowering.fs:443-464`). The access on axis `j` is then the offset set `O_j`
     (identity where the slot is plain).
3. All inputs share one chunk grid on the axes the nest iterates
   (`resolveArray` gives `Meta.Chunks`; a disagreement declines).

Anything else leaves the binding on today's path, byte for byte.

### 3.3 Tile geometry and halo propagation

Tile grid = the input chunk grid transported through the access. For a plain
axis, output coordinate `i` reads input `i`; tile `t` on that axis is
`[t*c, min((t+1)*c, n))`. For a halo axis with offsets `O` (min `o_lo <= 0`,
max `o_hi >= 0`), output interior coordinate `i` has center `i + (-o_lo)`
(`haloStartOffsetOfTag`) and reads centers `+ O`; tile `t` is the set of
interior coordinates whose *center* lies in chunk `t`, and its read region on
that axis is `[t*c + o_lo, (t+1)*c - 1 + o_hi]` clipped to `[0, n)`, i.e. the
chunk set `floor((t*c + o_lo)/c) .. floor(((t+1)*c - 1 + o_hi)/c)` -- for
radius 1 and `c >= 1`, chunks `t-1, t, t+1` where they exist. This is the
*forward demand* reading of item 2's access description; when that shared
description exists, this function consumes it instead of the tag (section 6).

The dirty set of a snapshot follows without any diff against the previous
snapshot: a tile is recomputed iff its key is absent from the store, and the key
already contains exactly the chunk refs of its read region.

### 3.4 What the generated program does (compiled lane)

Per tiled binding `F` with tiles `t = 0..NT-1` over input `A`:

```
// (i) probe phase -- emitted immediately before A's provider read
static const char* F_tkey_prefix[NT] = { ... };   // material(F,t); literal keys when capture-free
bool F_hit[NT]; bool A_need[NC] = {false};
for t: F_hit[t] = blade_tiles::probe(F_key(t));   // key = sha256(prefix || capture bytes)
for t: if (!F_hit[t]) for c in readChunks(t): A_need[c] = true;

// (ii) A's assembly, phase 1 -- the existing genAssembleFlatVia loop with the
//      Present test widened to `A_need[idx] && A_icoff[idx] >= 0`; unread
//      chunks leave their region of A_flat at the fill value, and A_done[c]
//      records what was read

// (iii) F's binding position
allocate F (unchanged);
for t:
    if (F_hit[t]) blade_tiles::load(F_key(t), F, tileBox(t))      // row-wise memcpy into F[i0][i1] rows
    else { <the nest, every level bounded to tileBox(t)>; blade_tiles::store(F_key(t), F, tileBox(t)) }

// (iv) A's assembly, phase 2 -- emitted before the first NON-tiled consumer of
//      A, or before the print pass if there is none: the same loop over
//      `!A_done[idx]`
```

Codegen changes this requires:

- **Per-level tile bounds.** `LoopNestCodeGen` gains `TileBounds: (string *
  string) list option`; the header renderer at `CodeGenLoopNest.fs:1642-1660`
  substitutes `for (i = lo_d; i < hi_d; ...)` at *every* level when set, the
  same substitution shape the MPI slab uses at the outer level. The flat-pool
  collapse is declined for tiled nests in v1 (a tile is contiguous in the pool
  only when it spans all trailing axes); the carousel and row-hoist paths are
  unaffected because they key off the loop variable, not its bounds
  (**inferred from the emission at 1.7; must be confirmed by the gate's
  byte-diff of the untiled path**).
- **Halo start offset per tile.** `wt = __i0 + start` already adds the slot's
  start offset; with tile bounds the loop variable runs over the tile's interior
  sub-range, so nothing else changes.
- **Need-masked assembly.** `ZarrProvider.CppZarr.genAssembleFlatVia` gains an
  optional need/done pair of C++ names; the Icechunk emitter passes them, Zarr's
  callers pass `None` and emit byte-identically (the P0 seam discipline of
  `plan-icechunk-provider.md` section 7).
- **Phase placement.** `genModule` emits (i) before the earliest provider read
  any tiled binding consumes, and (iv) before the earliest binding that consumes
  `A` and is not itself a tiled binding, else before `genPrintStatements`. If a
  captured scalar of `F` is bound *after* that provider read, `F` is still tiled
  but `A_need` is all-true (no read avoidance) -- the rule is stated in the
  census line.
- **Runtime header** `src/cpp/blade_tilecache.hpp`: SHA-256 (public-domain
  implementation), `probe/load/store`, header validation, counters, the env
  gate. Added to `runtimeHeaderNames` (`CodeGenLoopNest.fs`, list starting at
  `runtimeHeaderNames`) so `runtimeHeaderDigest` covers it.
- **Toolchain define.** `Build.fs` appends `-DBLADE_TOOLCHAIN_ID="<sha256 of
  the terms in 2.1>"` when the program was emitted tile-enabled (a sniff for
  `#include "blade_tilecache.hpp"`, the same style as the `blade_linalg.hpp`
  sniff at `:839-841`). The define enters `normalizedArgs` and hence the exe
  cache key -- harmless, it is a function of terms already in the key.

### 3.5 Granularity and the read-avoidance rule

v1 tile = one input chunk (all axes). Two savings are separately accounted:

- *Recompute avoidance* -- always available for an admitted binding.
- *Read avoidance* -- available only for chunks demanded by no observer: a
  chunk is skipped in phase 1 iff every dirty tile's read region excludes it;
  it is read in phase 2 iff some non-tiled consumer or the print demands `A`.
  Because the CLI prints every top-level binding (1.8), a printed `A` is read
  in full by the end of the run **in every case**; the saving in the CLI lane is
  then that the compute phase touched fewer chunks (which is what the gate
  counts), not fewer total bytes. Total-bytes savings appear where `A` is not
  printed: the interpreter lanes have `printOnly` today; a compiled-lane
  `--print <names>` mirroring it is a one-flag follow-up, deliberately outside
  v1. This design does **not** change what prints.

### 3.6 Correctness guarantees

- **Identical cold/warm stdout** (modulo the `completed in` timing line, which
  `InterpDiff` already drops, `tests/InterpDiff.fs:212-221`): tile *values* are
  cached, never text; the print pass reads the same materialized `F`
  (`CodeGen.fs:2299-2336`), so `setprecision(15)` formatting and array order are
  untouched. Values are bit-identical by 2.2. `A` is complete by print time by
  construction of phase 2.
- **Identical exit codes and aborts**: store-after-complete (2.2); the `BL8009`
  halo guard and the reader's own loud exits are emitted unchanged.
- **Never a wrong hit**: every emission-relevant input is in the key (2.1);
  the residual hazard is a SHA-256 collision or a corrupted store file with a
  valid header, both accepted as negligible for a discardable local cache.
- **A cache directory shared by unrelated programs is safe**: keys carry the
  task text, so two programs hit only when they would compute the same bytes.

### 3.7 Interpreter twin

The interpreter ignores the tile store entirely: it computes every binding and
reads every chunk (`materializeProviderRead`, `Run.fs:340-420`), which is the
**specification** the compiled lane must match. No interpreter change is
needed for v1, and the differential gates become the correctness oracle: run
the compiled program twice against the same store (cold, then warm) and diff
both against `Run.runProgram`. The `SessionMemo` is orthogonal (in-process,
name-keyed, interpreter-only) and stays as is.

---

## 4. First gate

### 4.1 Fixture

Extend `tests/IcechunkTests.fs`'s `wxSpec` (`:773-790`: `temp` 5x4 chunked 3x2,
a 2x2 chunk grid) with a one-changed-chunk variant `wxOneChunkSpec`: `s1` =
`tempV1`; `s2` = `tempV1` with only the four cells of chunk `(1,1)` (rows 3-4,
columns 2-3) changed (`+100.0`). Because the writer's chunk ids are
content-derived (`IcechunkWrite.fs:557-600`), `s2`'s manifest carries three refs
identical to `s1`'s and one new chunk id; the test pins that first
(`chunks/` holds 5 files, not 8 as in the existing all-changed fixture at
`:830`). Write it with `IW.writeRepoAt [ repo; e2eDir/repo ]` exactly as
the read-e2e block does (`:1335-1337`), and point `BLADE_TILE_CACHE` at a
**private absolute scratch directory** created by the test (the harness
compiles tests in parallel; hit counts must not see another test's store).

### 4.2 Programs and predictions

Program E (elementwise), compiled against `checkout("v1.0", ic.tag)` (= `s1`)
and against `checkout("main")` (= `s2`):

```blade
let A = ck.vars.temp |> ic.read
let F = method_for(A) <@> lambda(x) -> x * 2.0 + 1.0 |> compute
let total = reduce(F, (+), axes = 2)
```

| run | store state | tiles computed / hit | chunks read in compute phase | remainder (print) |
|---|---|---|---|---|
| E@s1 cold | empty | 4 / 0 | 4 | 0 |
| E@s1 warm | after cold | 0 / 4 | 0 | 4 |
| E@s2 cold-for-s2 | after E@s1 | **1 / 3** | **1** | 3 |
| E@s2 again | full | 0 / 4 | 0 | 4 |

Program H (radius-1 halo along `lat`, the chunked axis with `c = 3`):

```blade
type LatIdx = ck.index.lat
type LonIdx = ck.index.lon
let dt = method_for(range<halo<LatIdx, [-1, 0, 1]>, LonIdx>)
         <@> lambda(w, j) -> A(w(1), j) - A(w(-1), j) |> compute
```

Interior extent 3 (centers rows 1-3). Tiles by center chunk: lat-tile 0 =
centers {1, 2} (read rows 0-3), lat-tile 1 = center {3} (read rows 2-4). The
changed chunk `(1,1)` (rows 3-4, cols 2-3) intersects both lat-tiles' read
regions on column-chunk 1, so H@s2 recomputes **2 of 4** tiles and its compute
phase reads chunks `(0,1)` and `(1,1)`: **2 of 4**; tiles `(0,0)`, `(1,0)` hit.
Any other count is a bug in the propagation.

### 4.3 Assertions (harness shape)

Follow the read-e2e block (`IcechunkTests.fs:1305-1417`): `lower src` ->
`CodeGen.genSelfContainedProgramFromIR` -> `deployRuntimeHeaders` ->
`compileCpp` -> `runExecutable`, under the `baselineFailed`/`isSkipError` skip
discipline. Per program and snapshot:

1. stdout of the cold and warm runs are byte-identical after dropping the
   timing line; both equal `Blade.Interp.Run.runProgram`'s stdout for the same
   lowered program (the interpreter reads the fixture from the compiler cwd --
   the two-copy staging already handles it).
2. The `[tiles]`/`[chunks]` census (verbose on for the test only) matches the
   tables above exactly; also assert the store directory's file count.
3. The **untiled** emission is unchanged: emit E with `BLADE_TILE_CACHE` off and
   assert the C++ is byte-identical to the pre-change emission (the P0
   discipline); run `blade test icechunk`, `zarr`, `loops` (halo corpus),
   `interp loops` green.
4. A scale run outside the harness (not a pin): `4000x4000` float64 chunked
   `500x500` (64 chunks), one changed chunk, elementwise; report cold vs warm
   wall clock **including the recompile** (a new snapshot always recompiles,
   1.3), medians of interleaved runs, non-power-of-two extents.

### 4.4 STOP conditions

- Any stdout difference cold vs warm, or vs the interpreter -> stop; the
  mechanism is unsound as built.
- Counts differ from 4.2 -> stop until the propagation is understood; do not
  widen the family to "make the numbers work".
- Per-level tile bounds cannot be added at the header renderer alone (the
  carousel / row-hoist / flat paths need changes) -> stop and re-scope v1 to
  leading-axis slabs (outer-level substitution only, the MPI-slab shape), which
  gives 1-of-2 instead of 1-of-4 on the fixture.
- The scale run's warm end-to-end time (with recompile) is not at least 2x the
  cold time -> DEFER: the mechanism does not pay at the scale it targets while
  compile dominates.
- Read avoidance requires anything beyond the need/done masks and phase
  placement of 3.4 (e.g. changing print semantics) -> ship recompute avoidance
  alone and report chunk counts as an instrument.

---

## 5. Size, risk, files

Estimate: ~1,200-1,600 lines F#/C++ plus ~250 lines of tests; two to three
weeks for one engineer who has not touched `CodeGenLoopNest.fs` before, one to
two for one who has. The risk concentrates in one place.

Risks, in order:

1. **`CodeGenLoopNest.fs` is the hot, most-coupled surface** (CLAUDE.md: the
   CodeGen chain; `plan-icechunk-provider.md` P0 had to verify nine emissions
   byte-identical). Mitigation: the bounds override is a no-op when
   `TileBounds = None`; the gate's byte-diff of the untiled path is mandatory.
2. **The CLI prints every top-level binding**, so total I/O does not fall in
   the CLI lane (3.5). Mitigation: count compute-phase chunk reads; a `--print`
   flag is a separate, small decision.
3. **Spurious misses**: id drift in the task text (mitigated by alpha-renaming),
   writers that mint new ids for identical bytes, moved repos, `-march=native`
   on another CPU. All safe; all cost only cold time.
4. **Disk growth**: one file per tile per snapshot; eviction mirrors the exe
   cache's caps. Long paths on Windows: the two-level directory keeps names
   short.
5. **Who benefits today**: only the compiled CLI lane. Notebooks run the
   interpreter (the REPL memo lane), which does not consult the store. The
   render fast path (`ide-serve render`) is exe-cached, not tile-cached. State
   this plainly in the feature census.
6. **Probe cost** scales with tile count (one `stat` each); for the 100,000-chunk
   baked-table cap that is a visible pause. Not a v1 concern at 64 tiles; note
   it in the census line.

Files to touch, in implementation order (none under `src/` are touched by this
document):

1. `src/cpp/blade_tilecache.hpp` (new): SHA-256, `probe/load/store`, header,
   env gate, counters. Add to `runtimeHeaderNames` in `src/CodeGenLoopNest.fs`.
2. `src/IRMono.fs`: lift `pureBody`/`plainInfo`/`plainKernel`
   (`:1090-1139`) into shared, non-local predicates; the fusion pass keeps
   calling them (no behaviour change).
3. `src/providers/ZarrProvider.fs`: optional need/done masks on
   `genAssembleFlatVia` (`:1393-1478`); `None` emits byte-identically.
4. `src/providers/IcechunkProvider.fs`: expose a per-chunk identity text for
   the tile key (today `chunkLocText`/`varFingerprint` are `private`,
   `:1789-1841`); expose `Meta.Chunks`/grid for a resolved variable to codegen
   (a small public `tileGridOf : path -> var -> ...`), and the mask-aware
   `genReadVar` variant.
5. `src/IRStorage.fs` / `src/CodeGenState.fs`: `TileBounds` on
   `LoopNestCodeGen`; a `TilePlans: Map<IRId, TilePlan>` on the codegen context
   (beside `ProviderReads`, `CodeGenState.fs:68-76`).
6. `src/CodeGenLoopNest.fs`: header renderer substitution at `:1642-1660`;
   decline the flat-pool collapse when `TileBounds` is set.
7. `src/CodeGenBinding.fs`: the tiled arm ahead of the ordinary
   `IRCompute (IRApplyCombinator _)` emission; probe/phase emission hooks.
8. `src/CodeGen.fs`: the pre-pass building `TilePlan`s in `genModule`, phase
   placement (before the first consumed read; before the first non-tiled
   consumer or `genPrintStatements`), `#include "blade_tilecache.hpp"`.
9. `src/Build.fs`: `-DBLADE_TOOLCHAIN_ID=...` on tile-enabled compiles; the
   tile-cache directory/eviction twins of `exeCacheDir`/`evictExeCache`
   (`:536-548`, `:704-726`), gated `BLADE_TILE_CACHE` (unset = off).
10. `tests/IcechunkTests.fs`: `wxOneChunkSpec`; the section-4 block; the
    untiled byte-identity check.
11. `docs/features.md` row; `docs/plans/README.md` index row for this file;
    this file's Status line.

No new `.fs` file is required, so no `Blade.fsproj` entry; if the tile-plan
code grows past a few hundred lines, put it in `src/CodeGenTiles.fs` compiled
between `CodeGenLoopNest.fs` and `CodeGenBinding.fs` (`Blade.fsproj:371-374`).

---

## 6. Recommendation

**GO -- narrowly, as a bounded experiment, and sequenced with item 2.**

The ingredients the memo names are all real and already shaped for this
(1.1-1.3): a compile-time-pinned immutable snapshot, per-chunk content refs
rendered as text, a content-addressed cache with an identity discipline to copy,
a halo access fully determined by a tag, and a differential interpreter that is
the correctness oracle for free. The design needs no new type-system surface,
no new syntax, no provider write path, and no change to what prints. The
recompute-avoidance half is low-risk; the read-avoidance half is bounded by a
fact (every top-level binding prints) that the design respects rather than
fights.

Two conditions on the GO:

1. Build the **elementwise** case first, with the store, identity, phases and
   the untiled byte-identity check. Do the **halo** case as the *second
   consumer* of item 2's fixed-halo access description if that experiment is
   under way; if it is not, derive the offsets from `HaloWinTag` as in 3.3 and
   record that the two derivations must merge -- item 2's own stop condition
   ("duplicate analyses at both consumers") applies to this pair.
2. Treat the scale run (4.3 item 4) as a real gate, not a demo: a new snapshot
   always recompiles, and if warm-with-recompile is not clearly faster at a
   field size where I/O and compute matter, DEFER the arc rather than tune it.

DEFERRED by this design, deliberately: reduction-tree reuse and invertible
statistics (they change floating-point results unless the tree is preserved --
a separate numerical contract), computed inputs (keying a nest by the tile keys
of the nest that produced its input), non-Icechunk providers (no content
identity without hashing bytes at run time), a compiled-lane `--print` flag,
`Chunked<>` on the index record as the first-class carrier of the tile grid
(`plan-distributed-memory.md` section 2.2 -- when it lands, 3.3 reads the grid
from the type instead of from `ProviderReads`), and any distribution.
