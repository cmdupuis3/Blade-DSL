# Icechunk provider — versioned stores, the checkout factory, and axis identity across checkouts

**Status (2026-08-28, rev 3): ACTIVE — P0–P3 plus the demo notebook LANDED on
`feat/icechunk-provider`; P4 (packed / orbit / `read_window` / MPI parity) is
the one open phase.** Rev 2 folded in design review: unit-marker
disambiguation replaces string-prefixed refspecs at the surface (§3), the four
per-phase checkout arms collapse into one raw-AST desugar (§4), and §5's
metadata fingerprint is stated for all variables, not just axes. Rev 3 records
that the two questions rev 2 left open are closed: the FlatBuffers/zstd
dependency choice (§6.2) is **DECIDED — Option B**, vendored `flatc` output
plus ZstdSharp.Port, and the fixture strategy it gated (§10) is the on-the-fly
`IcechunkWrite` repos that section now describes. Both residuals once recorded
in the §5 P3 outcome are since closed: `SplitReason` prints in divergence
refusals (2026-08-29) and the type-alias laundering route refuses (2026-08-28).
The remaining pinned hole is the function-boundary laxity, pre-existing and
language-wide (§5).
Format facts below are against Icechunk spec 2.1 / icechunk-python 2.1.2
(2026-07-29); code citations are against the working tree at 76ac59a and will drift.

## 1. Why

Icechunk (https://icechunk.io) is a transactional storage engine over Zarr: a repo
holds git-like branches, tags, and immutable snapshots of a Zarr hierarchy, with
ACID commits. Scientific stores that today reach Blade as bare Zarr directories
increasingly live inside Icechunk repos, and the interesting new questions are
version-shaped: *read the store as of tag `v2024`; difference two commits of the
same field; pin a compiled program to an exact snapshot.*

Two properties make Icechunk an unusually good fit for Blade's provider model:

- **Everything but one file is immutable.** Snapshots, manifests, and chunk files
  never change after write; the only mutable object in a repo is the root `repo`
  file that maps refs to snapshots. Blade providers already resolve store metadata
  at compile time; with Icechunk the compiler can resolve *everything* — ref →
  snapshot → manifests → per-chunk byte ranges — at compile time and bake the
  result, because the resolved snapshot cannot change under the emitted binary.
  The generated C++ contains no Icechunk logic at all: no FlatBuffers, no zstd,
  just `std::ifstream` + offset reads, preserving the Zarr provider's
  `LinkNeeds = "none (pure std C++17)"` property exactly.
- **Version identity is first-class.** The provider's `Fingerprint` becomes the
  resolved snapshot ID (the semantically right provenance token — no sha256 sweep)
  and `VersionStamp` becomes the mtime of the single mutable `repo` file (exact
  and O(1), versus the Zarr provider's max-mtime walk over every file).

The payoff feature is snapshot differencing as a *language-level* idiom: two
checkouts of one repo whose shared axes have not diverged produce arrays that
unify, so `ck2.vars.temp - ck1.vars.temp` compiles to a fused loop over provably
identical index spaces — and refuses, loudly, when the grids genuinely diverged.
§5 is the theory of when that unification is licensed.

## 2. Format facts this design leans on

From the spec (https://icechunk.io/en/latest/reference/spec-v2-1/) and the
FlatBuffers schemas at `icechunk-format/flatbuffers/{common,repo,snapshot,manifest,transaction_log}.fbs`
in https://github.com/earth-mover/icechunk. Facts marked *(impl)* are confirmed
from the reference Rust implementation but not stated in spec prose.

- **Layout** (identical on local FS and object store): `$ROOT/repo` (entry point,
  only mutable file), `snapshots/`, `manifests/`, `chunks/`, `transactions/`,
  `overwritten/` (repo-file backups). IDs are Crockford base32, uppercase, no
  padding: 12-byte object IDs → 20-char names, 8-byte node IDs → 13 chars.
- **Metadata file framing**: a 39-byte header — 12 bytes magic `"ICE🧊CHUNK"`,
  24 bytes implementation name (space-padded), 1 byte spec version (`1` or `2`;
  2.0 vs 2.1 is *not* distinguished in the byte — 2.1 only adds one optional
  table field), 1 byte file type (Snapshot=1, Manifest=2, TransactionLog=4,
  RepoInfo=6; the enum also defines Attributes=3 and Chunk=5 but no current
  writer stamps them), 1 byte compression (0=none, 1=zstd) — then the payload:
  a (usually zstd-compressed) FlatBuffer.
- **The `repo` file** (root table `Repo`): `branches: [Ref]` and `tags: [Ref]`
  — **separate namespaces**; a branch `x` and tag `x` can coexist *(impl)* —
  each `Ref = {name, snapshot_index}` indexing into `snapshots: [SnapshotInfo]`
  (which carries `parent_offset`, timestamps, messages);
  `deleted_tags: [string]` tombstones (a deleted tag name must never be
  recreated); `status: RepoStatus` with availability Online=0 / ReadOnly=1 /
  Offline=2. Updates are optimistic-concurrency conditional writes with the old
  version backed up under `overwritten/`.
- **Snapshots**: `NodeSnapshot` entries sorted by path. `id: ObjectId8` is
  spec-stable — *"Stable across snapshots — a node keeps its id for its
  lifetime."* Moves/renames preserve the id (`moved_nodes` in transaction logs
  carries the same id across paths); delete-then-recreate mints a fresh random
  id *(impl)*. `user_data` is **the array's `zarr.json` content verbatim, as
  UTF-8 JSON bytes** — shape, dtype, codecs, `dimension_names`, and `attributes`
  (so the `blade` packed-layout attribute rides along unchanged). Dimension
  names are additionally stored structurally in `ArrayNodeData`. An array's
  chunks may be spread over multiple manifests: `manifests: [ManifestRef]`,
  each with one `ChunkIndexRange {from, to}` per dimension; ranges must not
  overlap — each chunk coordinate is covered by at most one manifest.
- **Manifests**: `ArrayManifest` entries sorted by node id; within one,
  `refs: [ChunkRef]` sorted lexicographically by `index: [uint32]` (plain
  coordinate vectors, no delta coding). A `ChunkRef` is exactly one of:
  **inline** (`inline: [uint8]`, post-codec bytes, ≤ the configurable
  512-byte default threshold), **native** (`chunk_id` + `offset` + `length`
  into `$ROOT/chunks/{chunk_id}`), or **virtual** (`location` or
  zstd-dict-`compressed_location` + offset/length + optional checksum, pointing
  at an external file/URL). A coordinate with no ref reads as the array's fill
  value (Zarr semantics; spec-silent).
- **Chunk files are raw**: native chunk data is written headerless and
  uncompressed-by-Icechunk — the bytes are exactly what the Zarr codec pipeline
  produced. The reference writer emits one chunk per file (offset effectively 0),
  but the schema permits packing; readers must honor offset/length.
- **Transaction logs are prunable** (2.1 `pruned_ancestor_tx_logs`): a reader
  must not depend on full tx history existing. This decides §5's mechanism —
  pairwise chunk-ref comparison, never ancestry walks.
- **No C API.** Rust crate + PyO3 + JS/WASM bindings only. Blade parses the
  format itself; §11 records the rejected alternatives.

## 3. Surface: `load` returns a repo handle; `checkout` is the factory

```blade
import icechunk as ic

let repo = ic.load("data/weather.icechunk")     // repo handle: no vars, no dims
let ck1  = repo.checkout("main")                // bare: name unique across namespaces
let ck2  = repo.checkout("v1.0", ic.tag)        // marker: the tag namespace
let ck3  = repo.checkout("main", ic.branch)     // marker: the branch namespace
let ck4  = repo.checkout("1CECHNKREP0F1RSTCMT0", ic.snapshot)

let temp_now  = ck1.vars.temp |> ic.read
let temp_then = ck2.vars.temp |> ic.read
let drift = temp_now - temp_then                // unifies iff temp's axes are
                                                // unchanged between ck1 and ck2 (§5)
```

- `ic.load(path)` opens the repo, parses the `repo` file, and binds a **repo
  handle**: an empty provider module (no `dims`/`vars` structs — already a
  first-class outcome of `registerProviderModule`, TypeEnv.fs:1010–1055, whose
  `moduleFields` simply come out `[]`). The handle erases to unit at lowering
  like every load binding (Lowering.fs:1662–1669). Its nominal type does not
  satisfy `providerAliasName` (registry lookup on the module name fails), so no
  alias verb can mis-fire against it. Because the repo file is parsed at `load`,
  a bad path, an Offline repo, or a spec-1 repo fails **at the load site**, not
  at first checkout.
- `repo.checkout("<ref>")` is the module-creating site — the analogue of what
  `z.load(path)` is for Zarr. The argument is a string **literal** (same
  restriction as `load`'s path, and for the same reason: metadata resolution is
  a compile-time act). Both bindings are required in v1 — `ic.load(p).checkout(r)`
  chaining is a possible later sugar, not in scope.
- **Ref resolution and the unit markers.** Branches and tags are separate
  namespaces, so bare names can collide. The provider declares three units at
  module level — `Unit branch`, `Unit tag`, `Unit snapshot` — and exposes one
  unit-carrying marker constant of each (`ic.branch`, `ic.tag`, `ic.snapshot`).
  `checkout`'s optional second argument is such a marker, and **dispatch is on
  the argument's unit**: one method name serves all three namespaces,
  disambiguated by type rather than by string prefixes — the same move arity
  polymorphism makes, with units as the discriminant. (Named call arguments
  are reduce-only today — ParserGrammar.fs:743–749 — so the marker is a plain
  positional argument; no grammar change.) The bare one-argument form resolves
  the name across branches ∪ tags ∪ syntactically-plausible snapshot ids
  (20 chars, Crockford alphabet) and demands uniqueness: zero hits is a loud
  error **listing the repo's actual branches and tags** (we hold them parsed);
  two hits is a loud refusal naming both, with the marker spellings as the
  remedy. A name in `deleted_tags` names its tombstone. Refusals are features:
  no silent precedence order between namespaces. String-prefixed refspecs do
  not exist at the surface; `branch:`/`tag:`/`snap:` survive only inside the
  canonical key (§3.1).

  **Surface outcome (2026-08-24): the desugar erases the marker,** so v1
  ships this syntax with no unit machinery at all — `repo.checkout("v1.0",
  ic.tag)` is rewritten whole, and `ic.tag` never reaches typecheck (writing
  `let t = ic.tag` fails as the ordinary unknown-member refusal, which is
  correct). Making the markers genuine unit-carrying values (usable outside
  checkout position, hoverable) costs two touch points — a units slot on
  `ProviderSpec` plus registration in the provider-import arm
  (TypeCheckInfer.fs:12329–12337) — recorded as v-next. The
  tempting alternative, a stdlib `icechunk.blade` carrying the `Unit` decls,
  is a dead end twice over: `ModuleResolve.isBuiltinModule` deliberately steps
  over registered provider names, and a module-exports hit would *shadow* the
  provider alias, breaking `ic.load` recognition entirely.

### 3.1 The canonical key

The entire provider contract — `LoadAsModule`, `ReadVarData`, `GenReadVar`,
the fold cache, `Fingerprint`, `VersionStamp` — threads a single `path: string`
(ProviderRegistry.fs:38–93). Rather than widen that signature everywhere,
checkout **desugars to a canonical key**: `"<repoPath>@<kind>:<name>"`, e.g.
`data/weather.icechunk@branch:main` — `<kind>` read off the marker's unit, or
`?` for the bare form (the provider resolves `?` by cross-namespace uniqueness
at `LoadAsModule` time). That string is what enters every
path-keyed carrier (`ProviderPaths`, `ProviderRoots`, the fold/axis caches,
`ProviderReadSpec.FilePath`), and `IcechunkProvider` parses it back internally.
Consequence, stated rather than hidden: `ic.load("data/weather.icechunk@branch:main")`
is *reachable* as the desugared spelling — checkout is sugar over one mechanism,
not a second mechanism. The documented surface is the factory; the key form is
internal, appears in provenance/diagnostic strings, and is not promised stable.

Ref resolution rides a **pinned per-compilation stamp**, not a live mtime. The
first touch of a repo records its `repo`-file mtime in an AsyncLocal pin
(`RepoPinTable`), and every memoized read for the rest of the compilation keys
on *that* stamp — so typecheck, static folds, lowering, and codegen all see the
*same* snapshot even if a writer commits mid-compilation, which a live
`(repoPath, refspec, repo-file mtime)` key would not have given (a later phase
would key on the new mtime and silently re-read). The pin is cleared by
`resetAxisMint` / `resetCaches`, and the axis mint table is cleared with it on
purpose: identities decided from reads under one stamp must not outlive it.
This closes a TOCTOU the Zarr provider structurally has (it re-`load`s the store
directory in each phase).

## 4. Wiring: one desugar; everything downstream already exists

`checkout` is the first surface verb whose receiver is a **derived binding**
rather than the imported alias. The rev-1 design taught the new shape to every
phase individually — a twin arm in typecheck's load recognition
(TypeCheckInfer.fs:11773), in Lowering's `tryInvokeProvider`
(Lowering.fs:1652–1673), in StaticEval's raw-AST `providerRoots` scan
(StaticEval.fs:946–953), and in two Ide walkers — because the binding→path
association is carried by three independently-built maps. Review called that
over-engineered, and it is: none of it generalizes to providers that exist,
and it plants Icechunk-specific shape knowledge in four base-compiler phases.

Rev 2: a **single raw-AST desugar pass**, run once before both typecheck and
StaticEval's scan (they consume the same `Decl` list). The pass walks the
module's decls, records `repoBinding → (alias, path)` from load-shaped lets
whose alias resolves to the icechunk provider, and rewrites

```
let ck = repo.checkout("v1.0", ic.tag)
    ⇒  let ck = ic.load("data/weather.icechunk@tag:v1.0")
```

preserving the original span on the rewritten node, so every diagnostic still
points at the checkout text. The marker argument is read syntactically at the
rewrite (a field access on the provider alias naming one of the declared ref
units); the bare form encodes `@?:`. A `.checkout` on anything that is not a
recorded icechunk load binding is left unrewritten and fails as the ordinary
missing-member error it is. The pass is a no-op for programs with no icechunk
loads.

After the rewrite, every downstream phase — typecheck, lowering, StaticEval,
Ide hovers, the interpreter, codegen — sees the load shape it already handles:
**zero new arms anywhere**, and the three existing binding→path carriers work
on the canonical key as a plain path string. Codegen and the interpreter were
never in question: they dispatch off IRId-keyed `ProviderReads`/`ProviderWrites`
side maps whose specs carry `FilePath` baked in (CodeGenBinding.fs:1444,
Interp/Run.fs:422–425); EmitLlvm.fs:4047–4048 refuses provider I/O wholesale,
so Icechunk inherits C++-backend-only like every provider.

The repo handle needs no new machinery either: `ic.load(path)` with a bare
path binds an **empty provider module** (`registerProviderModule` with no
dims/vars structs is already a first-class outcome, TypeEnv.fs:1010–1055) and
erases to unit at lowering like every load binding (Lowering.fs:1662–1669);
its nominal type does not satisfy `providerAliasName`, so alias verbs cannot
mis-fire against it. The desugar stays deliberately concrete to icechunk — no
generic "derived-binding verb" registry slot until a second provider actually
wants one.

**Desugar outcome (2026-08-24): LANDED as designed** — `src/ProviderDesugar.fs`
(`expand` for the diagnosing pipeline entry, `desugarOrIdentity` for the
others), wired at three sites: the typeCheck pipeline ahead of `Unfold.expand`,
the raw decl list at the top of `lowerTypedProgram` (the single funnel every
lowering caller passes through — which is where StaticEval's `providerRoots`
scan gets fed), and the IDE entry-buffer check. All three raw-AST load-shape
matchers in the repo receive desugared ASTs; malformed checkouts on a recorded
repo surface as BL3007 at the checkout call's span, and the rewrite fires only
at a top-level binding's RHS — the only position any binding→path carrier
recognizes a load in. Known edges, accepted: tests that call `resolveStatics`
directly on their own parse must desugar first, and hovers inside multi-file
*member* buffers see undesugared checkouts (compile behavior is unaffected —
typeCheck desugars the whole module set).

**Tier-2 review follow-ups (2026-08-29) — desugar, IDE payload, lifecycle.**
Four items from the adversarial pass, all landed as call-site or guard changes
with no new files:

1. *Marker alias.* The marker arm demanded the marker come off the same alias
   whose `load` produced the handle (`markerAlias = alias`). With
   `import icechunk as ic` **and** `import icechunk as ice` in one module,
   `repo.checkout("v1", ice.tag)` fell through to the not-a-marker refusal and
   told the user their own import was not a ref marker. The guard now tests
   membership in `icechunkAliases`, the set the pass already collects. A marker
   off a non-icechunk alias (`z.tag`) still refuses — pinned in both
   directions.
2. *Checkout position.* The rewrite serves exactly one position (a module-level
   binding's RHS), because that is the only one the three binding→path carriers
   look at. A checkout anywhere else — a function body, a block, an argument of
   another call — was left standing and died downstream as a missing-member
   error about a `checkout` field the provider never had: the "fails obliquely"
   risk, now closed. A **detection-only** second walk over the same module,
   after the rewrite, reports those as BL3007 at the checkout's own span with a
   positional body ("a checkout must be a module-level binding: the store's
   metadata is resolved at compile time"). It never rewrites — a load
   synthesized in a function body would be recognized by nobody, which is the
   reason the rewrite declines the position in the first place. It tracks
   shadowing (params, block-locals, match arms, lambda params, `function`
   names), skips the blessed position so a wrong-*shaped* checkout still
   reports exactly once, and reports every violation in one run.
3. *Mint-table lifecycle.* `resetAxisMint` + `IdeStores.reset` ran on the IDE
   lanes only. Two more funnels now run the same pair: `Cli.dispatchInner`
   (once per CLI invocation, ahead of every verb — one invocation of
   `check`/`emit`/`compile`/`run` is one compilation, so it can never land
   between a program's typecheck and its lowering), and
   `Interp.Repl.lowerSessionDiag`, which is the single entry both the REPL and
   the notebook use. The REPL placement was checked against what that function
   actually does: it re-parses and re-typechecks the **whole accumulated
   session** on every submission (SSA IRIds are freshly minted each pass, so
   nothing incremental survives a cell boundary), so the reset precedes the one
   `typeCheck` call that mints the submission's identities, and lowering happens
   further down the same function. Without it a long-lived process accumulated
   identities for axes whose session text had been edited away, and since an
   axis is named by its position in the mint table's identity list, a stale
   entry renamed a live axis (`lat#2` for no reason in the source). Cost,
   accepted: the interpreter's session memo keys on `cachedTy = b.Type`, so
   icechunk-typed bindings re-mint a fresh index-type id each cell and miss the
   memo; they recompute against the provider's own read memo rather than
   re-reading the store.
4. *`providerWrite` in the check payload.* `providerRead` was carrying both
   directions: a `let saved = c.write("out.csv", obs)` binding got the
   provenance of the array it **persisted**, so every client rendering that
   field as "reads store.vars.data" said so about a binding that reads nothing.
   Writes now travel in their own optional `providerWrite` field (same shape,
   documented in `protocol/types/check.d.ts`); the two never appear together.
   The write chase behind it is also scoped per module now — a single
   program-wide name-keyed accumulator let a `let obs` in module A supply the
   provenance for a write in module B. The returned maps stay name-keyed
   because `joinBindings` pairs them against typed entries that carry a scope
   but no module identity.

## 5. Axis identity across checkouts — the theory

### 5.1 What the type system does today

Every provider load mints **fresh** index-type identities:
`zarrDimToNamedIndexType` takes `builder.FreshId()` per dim
(ZarrProvider.fs:1022–1032), the per-store `dimMap` shares that identity across
one store's arrays (so `A(lat, lon)` and `lat` co-index within a load —
pinned by tests/ZarrTests.fs:220–224), and `unify` compares index slots by
`Id`+`Tag` **only** — never name, never extent (Unify.fs:990–992). Two loads in
one program — even the same store twice — are nominal strangers and refuse to
mix. There is no accidental-unification hazard to defend against; sharing must
be *built*, and the hook already exists: `zarrStoreToModule`'s `externalDimMap:
Map<string, IRIndexType> option` parameter (ZarrProvider.fs:1063, 1090–1098),
currently always `None`.

### 5.2 When two checkouts present *the same axis*

An index type is an axis's identity. Two checkouts of one repo share the index
type for dim `d` **iff the axis is unchanged between their snapshots**:

1. **same dim name** `d`, and
2. **same extent**, and
3. **same coordinate-variable content**, when a coordinate variable exists
   (the xarray convention: an array named `d` indexed by `d`). A dim with no
   coordinate variable has no data to compare; (1)+(2) is all the identity
   there is.

Condition (3) is what the user-facing sentence "lat/lon/time are the same
between checkouts because they don't have diffs" means, and Icechunk makes it
decidable **from metadata alone, with no data reads**:

- the coordinate variable's **node id** must match (spec-stable for the node's
  lifetime; a delete-and-recreate — a genuinely new axis — breaks identity
  because the id changes);
- its **`user_data` JSON** must match bytewise (dtype, fill, codecs, shape —
  metadata edits are axis changes); and
- its **chunk-ref table** must match: the sorted `ChunkRef` list — coordinate
  vectors plus, per ref, the inline bytes or `(chunk_id, offset, length)` or
  fill-absence — compared structurally. Chunk files and manifests are
  immutable, so equal refs ⇒ byte-identical content. Commits that never
  touched the coordinate array keep pointing at the same chunk ids, so the
  common case (data commits on a fixed grid) compares equal instantly.

**Rejected mechanism — transaction-log walking** (diff the commit range and
check the node never appears in `updated_arrays`/`updated_chunks`): tx logs are
prunable under 2.1 expiration, ancestry walks cost O(history), and two refs
need not have a simple ancestor relationship. Pairwise ref-table comparison is
direct, O(coord-chunks), and prune-proof.

**Failure direction is safe.** If expiration/compaction rewrites a manifest to
new chunk ids with identical bytes, the ref tables differ and the axes do *not*
share — a false negative: the program refuses arithmetic that would have been
sound. It never falsely accepts. (Tightening: hash actual coordinate bytes at
compile time — coordinate arrays are small and often folded anyway. Polish,
not v1.)

### 5.3 Mechanism

A per-compilation, repo-scoped **axis mint table** (AsyncLocal, reset like
`IdeStores.reset`):

```
(canonical repoPath, key) → [ { Extent; CoordFP; IndexType; Refs; SplitReason } ]
                                                         // CoordFP from §5.2
```

The repo path in the key is **canonicalized** (`canonicalRepoPath`: absolute,
normalized separators, case-folded where the filesystem is), so two spellings of
one directory (`data/wx` and `./data/wx`) are one axis universe rather than two;
the as-written spelling still rides everywhere it is user-visible (baked chunk
paths, diagnostics). `key` is a dim name for a dense axis and `__pool:<var>` for
a packed variable's pool (§5.3's P3 residual, below). The value is a *list* of
identities, newest first, not a single record — see the P3 outcome.

The first checkout that surfaces an axis mints its `IRIndexType` (fresh id, as
today) and records it. Each later checkout computes the axis's
`(Extent, CoordFP)`; on equality it passes the recorded `IRIndexType` in via
`externalDimMap`, so its arrays are built over the **same identity** — `unify`
then succeeds by the ordinary `Id` rule, no unifier changes at all. On
inequality it mints fresh (and records *why* the identity split). Notes:

- `registerProviderModule` qualifies index-type names per binding
  (`"{name}.index.{d}"`, TypeEnv.fs:1041), so two checkouts still get distinct
  *source-level* names over one shared `Id` — the flat `TypeDefs` map stays
  collision-free, and hovers stay per-binding. Unification is by `Id`, so the
  distinct qualified names cost nothing.
- Rank-≥2 variables share automatically when **all** their axes share; the
  element array's own data is irrelevant to its *type* — which is the point:
  `temp` may differ wildly between checkouts while `(lat, lon, time)` agree,
  and that is exactly the differencing case.
- Two checkouts of the *same* ref (or a branch and the tag pointing at the same
  snapshot) share trivially — every axis compares equal.
- **The fingerprint generalizes past axes.** `(node id, user_data bytes,
  chunk-ref table)` decides content-equality between two checkouts for *any*
  variable, not only coordinates — the axis rule is just its type-level use.
  The same test later licenses value-level reuse: reading an unchanged element
  array once and aliasing the buffer across checkouts, and sharing fold-cache
  entries between checkout keys whose fingerprints match (§11).
- Different repos never share: `repoPath` is in the key. Cross-provider sharing
  (an Icechunk checkout vs a bare-Zarr load of "the same" data) stays out;
  there is no identity to anchor it.
- **Divergence diagnostics**: when arithmetic later fails to unify two
  checkout-minted axes of one `(repoPath, dimName)`, the recorded split reason
  lets the error say *"axis 'lat' diverged between checkouts 'main' and
  'v1.0': coordinate data differs (extent 180 = 180)"* rather than a bare
  nominal mismatch. v1 may ship with the generic mismatch; the enriched
  message (likely its own BL code — five touch points per
  `adding-a-BL-diagnostic-code`) is a fast follow.

**P3 outcome (2026-08-25): LANDED — with one material correction to this
section.** The §5.3 claim "unify then succeeds by the ordinary Id rule, no
unifier changes at all" was half wrong: Unify.fs:990's Id rule is the
`IRTArrow` *slot* arm, and the `ArrayElem` arm compares rank, tags, and
symmetry — never Ids. Measured before the fix: two *different repos'* `temp`
arrays subtracted clean. The real carrier of axis provenance at arithmetic
seams is the index **Tag** (the co-iteration predicates behind BL3999), so
each minted identity also stamps a synthetic
`__icaxis|<dim>@<repo-dirname>:<16 hex of the CANONICAL path's sha256>[#n]` tag
(the `#n` split ordinal appears from the second identity on, so two identities
of one dim never print alike). The digest hashes the *canonical* path, matching
the mint-table key, and keeps 16 hex rather than 4: an axis tag is a *license* —
co-iteration reads agreement off the tag alone — so a collision would silently
permit cross-repo arithmetic, and 2⁻¹⁶ per pair was not a rate to accept for a
failure in that direction. The `__` prefix is load-bearing — it keeps BL4003
integer-indexing warnings, `elemTypeForIterationIndex` and `Ide.indexNamesOf`
reading the tag as the synthetic thing it looks like, while the co-iteration
predicates (which exempt nothing) still refuse diverged axes. Residual, stated
plainly: a plain-`unify` function boundary still accepts a diverged axis;
arithmetic refuses. *(Superseded in part — see* Alias laundering *below: the
prefix no longer buys an exemption at `unify`, which was the fourth seam listed
here and the one that made an ascription launder. The other three are
unchanged, and the boundary residual survives in its anonymous spelling.)*
Mechanics that differed
from the sketch, all recorded in the implementation: `externalDimMap` is
all-or-nothing and suppresses `IRTDIndexType` defs, so `checkoutToModule`
runs `zarrStoreToModule` twice (pass 1 discovers the dim universe, pass 2
rebuilds over resolved identities, re-attaching the defs); minted ids come
from a reserved `0x30000000` range so the three independent `IRBuilder`s
that build provider modules cannot collide them; and a mint entry holds a
LIST of identities, so `A → diverged B → A` re-shares A rather than minting
a third. `SplitReason` ("extent 6 -> 5", "coordinate content differs", …) is
recorded and test-pinned but nothing prints it yet — the divergence
diagnostic stays open. Lane: icechunk 471/0 (2026-08-29 tier-2 validation pass, includes new §21).

**P3 scope, stated honestly (2026-08-28, from adversarial review).** Two
things the outcome above does not cover. (1) The **type-alias route still
launders a diverged axis**: ascribing both checkouts' arrays to the same
`type L = ck.index.lat` alias re-tags them with the ALIAS name, so two
identities that the arithmetic seam would have refused co-iterate through the
ascription — the same shape as the plain-`unify` residual named above, one
level closer to the user. **CLOSED 2026-08-28**; mechanism in *Alias
laundering* below. (2) **Packed
pool axes were untagged at review**: a `blade`-attributed compact array's pool
axis carried no `__icaxis|` tag at all, so cross-checkout and cross-repo
packed arithmetic was silently ACCEPTED rather than refused — the one direction
§5.3 promises never to fail in. **CLOSED 2026-08-28**; mechanism below.

**Alias laundering (2026-08-28): CLOSED — and one false refusal closed with
it.** Reproduced first, at both ranks and across repos: ascribing two diverged
checkouts' arrays to one alias typechecked, and so did a hand-written
`type Lat = Idx<5>` of the right extent. TWO independent seams were letting it
through, and fixing either alone would have left the other standing.

- **The alias never carried the identity.** `registerTypeDecl`'s index arm
  rebuilt the record by re-lowering the referenced SURFACE body, and for
  `<binding>.index.<dim>` that body is a synthesized `TyIdx <extent>` —
  `registerProviderModule` stores the real record beside it because there is no
  surface syntax for "the axis this store minted". So the alias came out an
  anonymous `Idx<n>` wearing the alias name, with the `__icaxis|` tag already
  gone before unify was asked anything. The alias now **adopts the provider
  record**, tag included: a fifth carve-out beside the irreps, wreath and
  multi-rank ones, and the same trade they make — the alias names the axis for
  readability and mints no distinct identity, which for a store axis is not a
  cost (an identity minted from a Blade alias could not answer to the store
  that owns the axis).
- **`unify` exempted the tag.** `indexPairIncompatible` treats every `__` tag as
  a structural sentinel that never gates. Provider tags are `__`-prefixed for
  one narrow reason — the four seams that read a Tag as a user-facing NAME must
  leave them alone — and are otherwise identities, not sentinels. A
  `gatesNominally` predicate now splits the two: user names gate, the provider
  family (`Types.isProviderAxisTag`: `__icaxis|` plus both pool spellings)
  gates, every other `__` tag keeps the exemption.

Consequences, all pinned in IcechunkTests §18. A diverged axis now refuses
through an alias at rank 1 and rank 2, within a repo and across repos, and a
user-declared index type cannot adopt a store axis either — the rule the
language already applies between two user-named types, which a store-minted
name now simply joins. The refusal lands as **BL3001** at the ascription
(`unify`) and **BL3999** where co-iteration reaches it first; no new code was
minted. `Tag = None` stays permissive on both sides, so the escape hatch
BL3999's own message advertises — drop the annotation if two axes really are
one — is intact, and it is the remaining way to assert that.

The **false refusal** the same change fixes: inside ONE checkout, an aliased
array beside the raw one carried `Lat` against `__icaxis|lat@…` and earned a
BL3999 that the identical Zarr program never sees (a Zarr axis is untagged, so
`Some name` vs `None` is permissive). Same axis, same tag, co-iterates —
icechunk is no longer stricter than Zarr for a sound program.

**Still open, and measured rather than assumed:** the plain-`unify` FUNCTION
BOUNDARY residual named above survives when the parameter's index type is
spelled anonymously. Its cause is not provider-specific — an argument position
accepts a differently-named index type of equal extent for two ordinary
`type A = Idx<5>` / `type B = Idx<5>` declarations too, while the identical
`let` ascription refuses. That is the pre-existing unify-strictness split
between the ascription and direct-application seams; axis provenance rides it
rather than causing it, and closing it is a language-wide change to how every
named index type is matched at an argument position. Both halves are pinned
(§18 (g) and (h)) so the day it tightens, the pins say so. Lane: icechunk
471/0, index-types 254/0 (2026-08-28 alias-laundering pass; icechunk count
reconfirmed 2026-08-29, now including the tier-2 hardening §21 additions).

**Packed-pool identity (2026-08-28): CLOSED, with sharing.** A pool axis is not
a store dimension — its extent is a derived cardinality — so `sharedDims`
deliberately drops it and `externalDimMap`, the channel the dense identities
ride, structurally cannot reach it; the pool record came out of
`zarrStoreToModule` with `Tag = None` and a fresh id, and *both* refusal
predicates are permissive on `None` (`indexNamesCoIterable`'s `| _ -> true`,
`indexPairIncompatible` falling through to its symmetry arm). The fix threads a
**second, optional hook** the same way — `zarrStoreToModuleWith`'s `poolAxis:
(varName -> mintedRecord -> resolvedRecord) option`, absent by default, so the
four-argument `zarrStoreToModule` every plain-Zarr call site uses emits exactly
the module and types it always did (pinned: a plain Zarr packed store's pool
record is still `Tag = None`, `IxKPlain`, `SymSymmetric`). Details that were
decisions rather than transcription:

- **The pool gets the whole §5 story, not just refusal.** It routes through the
  same mint table under `__pool:<var>`, with the *variable's own*
  `varFingerprint` (node id, `user_data` bytes, chunk-ref table) standing in for
  the coordinate it does not have — the §5.3 bullet "the fingerprint generalizes
  past axes", cashed. So an UNCHANGED packed variable shares ONE identity across
  two checkouts of a repo (measured: one entry, both refs), a rewritten one
  splits with `SplitReason = "packed variable content differs"`, and two repos
  never meet.
- **Two tag spellings, because the Tag doubles as the KIND sentinel.**
  `__icpool|<var>@<repo>:<digest>[#n]` for a depth-1 simplex pool (`IxKPlain`),
  `__icpoolorb|…` for an iterated-wreath pool (`IxKOrbit`, whose record would
  otherwise carry `__orbidx`). `ixKindOfTag` maps the orbit spelling back, which
  is what `IRValidate`'s Tag/IxKind agreement check requires; the simplex
  spelling falls through to `IxKPlain`.
- **One predicate change.** `indexNamesCoIterable` compares two pool tags
  *whatever the IxKind says* — its existing rule is restricted to `IxKPlain`
  because a Tag is normally a kind sentinel, and a wreath pool would have taken
  the permissive non-plain arm. A pool tag against anything else (a plain Zarr
  store's untagged pool, a locally declared `SymIdx<2,4>` array) stays
  permissive, the same anonymous-side rule the dense tags follow.
- **Both hooks are supplied on pass 2 only**, so each identity resolves exactly
  once per `checkoutToModule`; pass 1 exists to read back the dim universe and
  the record shapes. The early-return condition moved with it: a store of
  *nothing but* packed variables has no shared dims at all, and used to return
  pass 1's untagged module.
- **The `__`-prefix exemptions were verified per site, not by analogy**:
  `checkArrayIndexTags` and its one-bracket twin (BL4003), the same predicate in
  `elemTypeForIterationIndex`, `unify`'s `isSyntheticTag`, and
  `Ide.indexNamesOf` all key on `StartsWith "__"` and therefore treat a pool tag
  exactly as they treat `Tag = None` today. `tryAxisTagName` deliberately does
  **not** decode a pool tag: `ppIndexTypeIn` prints a nominal name *instead of*
  the whole compact spelling, so decoding would turn `SymIdx<2, 4>` into a bare
  `cov` in hovers. Pool tags stay invisible in printed types (they surface only
  inside the BL3999 refusal text, exactly as dense ones do).
- **Same residual as the dense case, unchanged**: a plain-`unify` function
  boundary still accepts a diverged pool, because the tag is `__`-synthetic;
  arithmetic — where a difference is actually written — refuses.
- Not reachable end-to-end: elementwise arithmetic over a *wreath* pool is
  refused today on its own grounds (a wreath pool has no traversal nest outside
  its own loop), so the orbit spelling is pinned at the tag/kind level rather
  than through a program. The cross-repo wreath case does refuse on the axes,
  because co-iteration is checked first.

## 6. Compile-time metadata reading

### 6.1 Reader pipeline (pure F#)

`load` / checkout resolution reads, in order: header (39 bytes; refuse spec
byte 1 by name, accept 2; accept compression 0 and 1) → `repo` FlatBuffer
(status gate: Online/ReadOnly proceed, Offline refuses; refs; snapshot list) →
snapshot for the resolved ref → per-array: parse `user_data` **with the
existing `parseArrayMetaV3`** (ZarrProvider.fs:677) — the JSON is verbatim
`zarr.json`, so every v1 gate is inherited for free: uncompressed single
`bytes` codec, little-endian, numeric dtypes, regular chunk grid, and the
`blade` packed/orbit layout attribute — then cross-check the snapshot's
structural shape/dimension-names against the JSON (loud on disagreement) →
manifests for the arrays actually used: union the `ManifestRef`s by their
non-overlapping `ChunkIndexRange`s into one chunk table per array:

```
ChunkLoc = Fill | Inline of byte[] | Native of {File: string; Offset: int64; Length: int64}
```

Virtual refs are refused by name at parse. Native lengths are validated against
the chunk's expected byte size (chunk grids are regular and edge chunks
full-size under the raw `bytes` codec — same contract as Zarr v1). Hierarchy:
root-level arrays only (path `/name`, name a valid identifier), mirroring the
Zarr provider's one-level rule; deeper groups refuse by name.

### 6.2 Dependencies — DECIDED (Option B; was the gate on P1)

Blade.fsproj has **zero** PackageReferences today; this provider forces the
first, and the choice couples to the fixture strategy (§10):

| Need | Option A (lean) | Option B (robust) |
|---|---|---|
| zstd | ZstdSharp.Port — managed-only, keeps "no native library" | same; unavoidable either way (real writers compress metadata) |
| FlatBuffers | hand-rolled reader (~200 lines: root offset, vtables, tables, vectors, strings, structs) pinned to the ~8 tables read, field ids transcribed from the `.fbs` files, loud on unknown spec byte | Google.FlatBuffers NuGet + vendored `flatc` output from `icechunk-format/flatbuffers/*.fbs` |

A reads-only and matches the repo's self-contained style; B additionally buys a
FlatBuffers **writer**, which makes hermetic on-the-fly fixtures (the `ZarrWrite`
discipline) cheap and later enables writes-as-commits (§11). FlatBuffers'
forward-compatibility (unknown fields ignored) protects both options equally;
the spec-version byte is the loud gate.

**DECIDED (2026-08-25): Option B** — ZstdSharp.Port for zstd, Google.FlatBuffers
runtime + vendored `flatc`-generated accessors from icechunk's own
`icechunk-format/flatbuffers/*.fbs`, pinned to the icechunk release the spec
facts were taken from. One mechanical consequence: `flatc` emits C#, not F#, so
the generated code (and the .fbs files beside it, with a README recording the
pinned source commit and the exact flatc invocation) lives in a small
class-library csproj that Blade.fsproj takes a ProjectReference to — the first
project reference and the first NuGet packages in the build. `blade test
icechunk` gains golden-bytes tests so a runtime/codegen version bump that
changes wire behavior fails loudly rather than drifting.

## 7. Runtime C++: the baked chunk table

The refactor that pays for everything (P0): parametrize the Zarr provider's
shared chunk-assembly core over its **chunk source**.

- F# side: `readArrayData` / `readPackedPool` take a `coords -> byte[] option`
  fetcher instead of hard-coding key-file reads. Zarr's fetcher is today's
  file-per-key; Icechunk's serves inline bytes or offset/length file reads.
  Everything above the fetcher — fill handling, edge intersection, packed pool
  assembly, wreath pools, windows — is shared and cannot drift.
- Codegen side: `genAssembleFlat` (ZarrProvider.fs:1268) takes a chunk-fetch
  *emitter*. Zarr's emits the chunk-key-string + `ifstream` open it emits
  today, byte-for-byte. Icechunk's emits compile-time-baked static tables
  indexed by flattened grid coordinate — per chunk a relative file path,
  offset, and length; inline chunks as byte-array literals (≤512 B each);
  a sentinel for fill — and a single loop of open/seek/read/copy. Missing
  chunk files (a GC'd pinned snapshot) die loudly via the existing
  `zExit` discipline, never silent zeros.

The generated program stays pure `<fstream>`/`<filesystem>` C++17 —
`LinkNeeds = "none"`. Dense, packed (sym/antisym), orbit, `read_window`, and
the MPI-distributed packed read all ride the shared core unchanged.

**P0 outcome (2026-08-24): LANDED, with two scope corrections.** The seam
landed as `ChunkSource` (F#: `Label` + `Fetch`, post-codec bytes, `None` =
fill) and `ChunkFetchEmitter` (codegen: `Prologue`/`Locate`/`Present`/`Read`/
`Ident`), with `genAssembleFlatVia` public for the Icechunk instance; nine
emit programs (dense v2/v3, packed flat, orbit, window flat, window blocks,
stream, null-fill, int dtype) verified byte-identical pre/post. Corrections to
the prose above: (a) the **packed-blocks** layout has its own assembler
(`genAssemblePackedBlocks`) with differently-shaped per-block I/O that does
NOT route through the shared core — P4 must either give it a second
`ChunkFetch` wiring or merge the assemblers; (b) the fill-branch *body* stays
in the shared core (the emitter owns detection + acquisition only), and
`genStreamOpen`/`genStreamFiber` still bake their own partial-read I/O,
consistent with streams being `None` in §8.

## 8. ProviderSpec assembly

Registered as `"icechunk"` in `ProviderStatics.install` (ProviderStatics.fs:161);
new file `src/providers/IcechunkProvider.fs` compiled **after** `ZarrProvider.fs`
(it reuses the parsers, SimplexBlocks, and the shared core) and before
`ProviderStatics.fs` — hand-placed `<Compile>` entry, `EnableDefaultItems=false`.

| Slot | v1 | Note |
|---|---|---|
| `LoadAsModule` | ✓ | bare path → repo-handle module (empty); canonical key → full dims/vars module |
| `ReadVarData` | ✓ | fold path; packed variables steer to `ic.read` like Zarr (ZarrProvider.fs:2202–2212) |
| `GenReadVar` / `GenReadPacked` / `ReadWreathPool` | ✓ | via the shared chunk-source core (§7) |
| `GenReadCompoundVar` | None | loud, as Zarr |
| `GenWriteVar` | refuses loudly | a write is a commit: new chunks + manifest + snapshot + conditional `repo` swap — its own arc (§11) |
| `GenStreamOpen` / `GenStreamFiber` | None | deferred; the baked table makes fiber reads easy later |
| `VarDimNames` | ✓ | from the snapshot |
| `Fingerprint` | resolved **snapshot ID** | exact provenance: `folded temp from weather.icechunk@branch:main = snapshot 1CECH…` |
| `VersionStamp` | mtime ticks of `$ROOT/repo` | the only mutable file; every existing (path, var, stamp)-keyed cache works unchanged. Polish: `tag:`/`snap:` keys are immutable and could skip invalidation |
| `LinkNeeds` | `"none (pure std C++17)"` | |

## 9. v1 scope — loud, specific refusals

In: local-filesystem repos; spec byte 2 (covers 2.0/2.1); branch/tag/snapshot
checkouts; dense + packed + orbit reads, `read_window`, static folds; interp
parity (free via `ReadVarData`); axis sharing per §5.

Refused **by name**: object-store URLs (`s3://…` — the runtime is `fstream`);
spec byte 1; virtual chunk refs; repo status Offline; deleted-tag names;
ambiguous bare refs (remedy: the unit markers); nested groups; writes; `.stream`;
`load_compound`; non-literal checkout arguments. Compressed/transformed Zarr
codecs, big-endian, non-numeric dtypes: inherited verbatim from
`parseArrayMetaV3`'s gates.

## 10. Testing

- `tests/IcechunkTests.fs`, entry `runIcechunkTests () : int`, local
  `check`/counters per the ZarrTests pattern; `printHeader`/`printFooter` from
  `TestHarness`; e2e compile+run behind `Build.compileCpp` with the
  `isSkipError`-SKIP and `baselineFailed` disciplines (tests/ZarrTests.fs:50–61).
- Dispatch arm `| [ "icechunk" ] -> …` immediately after `csv`
  (src/CliSelfTests.fs:2234–2237). Store-backed provider lanes are
  **standalone-only** — nothing in `tests/RunAll.fs` calls them — so a green
  default suite says nothing about this lane. Registering the verb is
  therefore only half the wiring: **the lane also has to exist in
  `.github/workflows/ci.yml`**, both as a row in the `$lanes` table and as a
  member of `$groups.providers` and the `workflow_dispatch` options list, or
  nothing ever runs it. Done (2026-08-28): `icechunk` (timeout 45) sits beside
  netcdf/zarr/csv/hybrid.
- `provider-desugar` is the exception and is wired the other way. It is a pure
  in-process AST rewrite with no store, no registry and no g++ behind it, and
  `TypeCheck.typeCheck` runs `ProviderDesugar.expand` on **every** program, so
  it has no business being opt-in: `tests/RunAll.fs` yields
  `ProviderDesugarTests.runProviderDesugarBlock ()` into the default suite's
  grand total (2026-08-28), with the standalone verb kept as a thin wrapper
  and a cheap CI lane (timeout 15) so a pull request sees it without waiting
  on the nightly-only `cpp` lane.
- **Fixtures**: generated on the fly, `ZarrWrite`-style — which requires
  FlatBuffers *writing*, and was why §6.2 gated P1. Both halves shipped:
  golden byte fixtures hand-assembled in F# for the parser unit tests (a
  minimal FlatBuffer is constructible by hand), and full hermetic repos from
  `IcechunkWrite` now that the §6.2 decision has landed.
  `tests/fixtures/icechunk_repos/` gets a README **and a .gitignore from day
  one** — the zarr_stores precedent states nothing is committed but 161
  generated files are in fact tracked; don't repeat that.
- One-time (not CI) cross-validation of `IcechunkWrite` fixtures against
  icechunk-python 2.1.x, oracle-style. **DONE (2026-08-28)** — and it did not
  come back clean; see the outcome below.

**Oracle cross-check outcome (2026-08-28).** Done as a committed GOLDEN
FIXTURE rather than a one-off script, because a check that runs once and is
then deleted cannot fail again. `tests/fixtures/icechunk_repos/golden_py/` is a
20-file, ~6.8 KB spec-2 repo written by **icechunk 2.1.2 / zarr 3.3.0** (two
commits on `main`, tag `v1.0`, uncompressed `bytes` chunk codec, native chunk
files, `dimension_names` on every array); `make_golden_py.py` beside it is its
provenance and is **run by hand, never by the build or the suite** — the
no-Python rule is about the build and test loop, not about how a committed
byte-fixture was obtained. `tests/IcechunkTests.fs` §19 reads it. The fixture
is a CAPTURE, not a reproducible build (icechunk mints snapshot ids randomly),
so §19 pins no object ids — it reaches both commits through the refs and pins
the array values.

It found exactly one defect, and validated everything else:

- **DEFECT (open): `IcechunkProvider.decompress` cannot read a frame the
  reference writes.** The reference compresses metadata with a STREAMING zstd
  encoder, whose frame header descriptor is `0x00` — `Frame_Content_Size` flag
  0, `Single_Segment` flag 0, i.e. *no recorded decompressed size*.
  `IcechunkWrite` uses ZstdSharp's one-shot `Wrap`, whose descriptor is `0x60`
  and carries the exact size. `decompress` already anticipates the unsized case
  and has a grow-and-retry branch for it — but that branch is DEAD, because
  `ZstdSharp.Decompressor.GetDecompressedSize` does not return the
  `CONTENTSIZE_UNKNOWN` sentinel for such a frame; it returns the frame's
  **window size** (131072). The `sized` test therefore passes on a number that
  is not a content size, the reader allocates 131072 bytes, `Unwrap` writes the
  true 1552, and the `written <> out.Length` guard refuses the file. Every
  metadata file in a real repo hits this, so the reader currently cannot open
  ANY repo written by icechunk itself. Candidate fixes: read the frame header
  descriptor directly and route flag-0 frames to the existing grow-and-retry
  path; or use the span overload `Decompressor.Unwrap(ReadOnlySpan<byte>)`,
  which sizes itself correctly for both frame shapes (verified against both
  fixtures).
- **Everything else the two hand-transcribed halves believe is CORRECT**,
  confirmed against reference bytes: the 12 magic bytes, the 39-byte header
  layout, the 24-byte space-padded implementation field, the file-type bytes
  (RepoInfo 6 / Snapshot 1 / Manifest 2), the spec byte, the Crockford base32
  file-name alphabet and width, the FlatBuffer repo/snapshot/manifest reading,
  `shape_v2`, structural vs JSON `dimension_names` agreement, root-group node
  handling, and native chunk placement by id + offset + length. Values read
  exactly. Verified by running §19 against a copy of the fixture whose metadata
  was rewritten with compression byte 0: **45 of 46 assertions pass**, the
  remaining one being the check that correctly notices the copy was doctored.
- **§5.2's premise is now observed in the reference, not only in our own
  writer**: across a data-only commit icechunk keeps each node's id and REUSES
  the untouched coordinate arrays' manifests byte-for-byte, minting a fresh
  manifest only for the array that was rewritten.
- Three things the reference does that no Blade fixture does, all handled:
  a third `"Repository initialized"` snapshot (`1CECHNKREP0F1RSTCMT0`),
  populated `transactions/` and `overwritten/` directories, and a
  pretty-printed `zarr.json` with a different key order and a
  `storage_transformers` field.
- Axis-sharing tests (P3): same-ref checkouts unify; data-only commits keep
  axes shared (arithmetic across checkouts compiles and runs); a coordinate
  rewrite splits identity (rejects); an extent change splits identity; a
  branch/tag name collision refuses with the qualified remedy.

## 11. Later arcs and rejected alternatives

Later: **writes-as-commits** (`GenWriteVar` or an F#-side post-run commit: new
chunk files + manifest + snapshot + conditional `repo` swap; note the local-FS
conditional-put mechanism in the reference `object_store` backend is
unconfirmed — pin it down before building); **virtual chunk refs** (a natural
bridge to NetcdfProvider — a virtual ref *is* an offset/length into an external
file); object-store repos; `.stream`; checkout chaining; buffer dedup when two
checkouts' chunk tables coincide (read once, alias); repo-handle introspection
(branch/tag enumeration as compile-time values).

Rejected: **FFI to the Rust crate** (no C API exists; a bespoke cdylib shim
adds a native dependency to compile *and* run); **shelling out to
icechunk-python** (non-hermetic compile); **"export to plain Zarr first"**
(works today, loses versioning, provenance, and the differencing idiom);
**tx-log diffing for axis identity** (§5.2).

## 12. Phasing

| phase | deliverable | gate |
|---|---|---|
| P0 | ChunkSource seam in ZarrProvider: F# fetcher param + codegen fetch-emitter param; zero behavior change | **LANDED** — byte-identical across nine emit programs; `blade test zarr` confirmation rides the next runner pass. Packed-blocks assembler intentionally NOT shared (see §7 outcome; moves to P4) |
| P1 | §6.2 decision made; format reader: header, zstd, FlatBuffers repo/snapshot/manifest models; refusal gates (spec-1, Offline, virtual, deleted tags) | **LANDED** — Option B deps + vendored accessors; decode/decompress real; every refusal live incl. Offline at the bare load site; `IcechunkWrite` fixture repos round-trip (`icechunk` lane 471/0, 2026-08-29 validation pass). The icechunk-python cross-validation is **DONE (2026-08-28)** and found one real defect, **FIXED the same day**: `decompress` sized its output from the frame header and so refused the unsized streaming zstd frames the reference writes; it now sizes from the decode itself (§10's outcome block), and the golden fixture reads clean (46/0). Everything the reader believes about the format is confirmed against reference bytes |
| P2 | Provider + surface: spec registration, repo-handle load, the checkout desugar pass, ref units + marker constants, canonical key, memoized resolution; dense reads, folds, `blade test icechunk` lane | **LANDED** — desugar + skeleton + real reads/folds/e2e (compile+run through `repo.checkout`, bare and marker forms; baked chunk tables; provenance prints snapshot IDs); lanes (2026-08-29 tier-2 validation pass, includes the desugar §9/§10 position tests and the icechunk §21 hardening tests): provider-desugar 72/0, icechunk 471/0, zarr 273/0, netcdf 171/0, csv 57/0, hybrid 32/0 (1 skipped, mpi+cuda env skip), full suite 5180/0 (1 skipped, unrelated GR-render env skip). `read_window` joined packed-blocks in P4 (its sub-simplex extraction sits above the shared core) |
| P3 | Axis mint table + `externalDimMap` wiring + divergence recording | **LANDED** — sharing tests pass (same-ref, data-only commit shares + compiles+runs, coord rewrite refuses, regrid refuses, coordinate-less dims share, cross-repo never shares, resets clear); the mechanism is Tag-based, see the §5 P3 outcome. Packed POOL axes now carry the same identity (§5.3 "Packed-pool identity", 2026-08-28): unchanged pools share across checkouts, rewritten ones split, cross-repo refuses. Both former residuals closed: `SplitReason` prints in the refusal clause (2026-08-29) and the alias-laundering route refuses (§5 "Alias laundering", 2026-08-28); the remaining pinned hole is the pre-existing, language-wide function-boundary laxity |
| P4 | Packed / orbit / `read_window` / MPI-distributed parity through the shared core | **OPEN** (nothing built yet). Gate when it starts: Zarr's packed/window test shapes mirrored under Icechunk, green. The packed-pool axis tagging this row used to carry landed early, with P3 (§5.3, 2026-08-28) — the reads it guards are still P4's |

Landing P2 adds the `| Icechunk | … |` row to `docs/features.md` §15 and updates
this doc's Status header plus the README index row — never the filename.

## 13. Demo notebook (post-P3 deliverable)

An `examples/` notebook demonstrating the provider end to end, written once P3
lands (cells 6–8 are *about* axis identity). Shape, following the
`lseof.bladenb` conventions (markdown narration between code cells; the
notebook lane re-runs the whole session per cell, so keep provider loads
memoization-friendly — they are, per §3.1 — and arrays small enough that
`let static` folds stay under the fold ceiling):

- **Data**: `examples/data/station_temps.icechunk`, a small synthetic
  station-temperature grid generated by `examples/tools/make_station_icechunk.fsx`
  (F# script `#r`-ing the built Blade.dll and driving `IcechunkWrite` — the
  no-Python-tooling rule). History designed for the story: tag `v1.0` = raw
  data; `main` = one bias-correction commit later (coordinates untouched, node
  ids stable); branch `regrid` = lat axis rewritten at a different extent;
  plus a branch and tag sharing one name, for the ambiguity cell.
- **Cells**: (1) markdown: what Icechunk is, what Blade does differently —
  compile-time snapshot pinning, provenance, refusals; (2) `import icechunk
  as ic` + `ic.load` → the repo handle; (3) `repo.checkout("main")`, read,
  extents; (4) a `let static` fold — the `[provenance] … snapshot 1CECH…`
  line is the point, narrate it; (5) `repo.checkout("v1.0", ic.tag)` — the
  unit-marker form; (6) **the headline**: `main_ck.vars.temp - v1.vars.temp`
  compiles *because* the lat/lon/time axes have no diff between the
  checkouts — narrate the §5 metadata rule; (7) a snapshot-id checkout
  (`ic.snapshot`) — bit-exact reproducibility; (8) intentional-error cell:
  the `regrid` checkout's diff refuses (diverged axis) — refusals are
  features, quote the message; (9) intentional-error cell: the ambiguous
  bare name refuses and names the marker remedy; (10) close: what's pinned
  where, and the versioned-store idioms this unlocks. Intentional-error
  cells are established notebook practice here (the quickstart notebook
  does the same).
- **Verification**: cells concatenate into a flat `.blade` (they are
  top-level decls), so `blade run` over the concatenation is the cheap gate;
  a true notebook-lane pass through the REPL tooling is the full gate.

**Notebook outcome (2026-08-25): WRITTEN and verified** —
`examples/station_temps.bladenb` + `examples/tools/make_station_icechunk.fsx`
+ the committed deterministic repo `examples/data/station_temps.icechunk`
(snapshot ids stable: raw `9ZDMT4JXK0A92JRXYXRG` — **re-derived 2026-08-28**,
when snapshot ids became content-derived; the old `TFWGY8Y38WKVN9HJX450` was a
hash of the spec author's *name* for the commit, which is exactly why the
notebook's "a snapshot id can never move" claim needed the writer fixed under
it rather than the sentence softened). The passing-cell
concatenation compiles and runs: `mean_drift = -0.5` exactly, both
same-snapshot diffs exactly 0; the regrid cell refuses BL3999 and the
ambiguity cell refuses with the marker remedy. Two facts the
verification surfaced: provider resolution failures at `blade check` fell
back silently (lowering owned those diagnostics — so the ambiguity
cell errored under `emit`/`run`/the notebook lane, not bare `check`), and
`blade run` executes the compiled exe under a different cwd than the caller's
(the fixture-mirroring trick in the e2e tests exists for this; the notebook
lane interprets and is unaffected).

> **Superseded 2026-08-28 — the check-phase silence was a defect, not a
> design.** The first of those two facts is no longer true, and calling it
> "by design" was the mistake. `TypeCheck.checkDecl` already resolves the
> store at the binding site (it has to — the dims' and variables' types come
> out of the metadata), and its `try` parked only a dead NATIVE library as
> BL2007 while a bare `| _ ->` swallowed the rest. So the ENTIRE refusal set
> that is this provider's product claim — typo'd and ambiguous refs, a
> missing or corrupt repo, the spec byte, `Offline`, deleted-tag tombstones,
> virtual chunk refs, nested groups, every verifier and offset refusal —
> produced no diagnostic under `blade check` and none in the editor, arriving
> only once `emit`/`run` re-opened the store at lowering. A refusal a user
> cannot see at the phase they run is not a refusal.
>
> It now surfaces as **BL2008 ("provider cannot resolve the store")**, parked
> at the load/checkout site's own span with the provider's own words intact.
> The mechanism is a typed exception, `Types.ProviderResolutionError`, raised
> by `IcechunkProvider.loadAsModule` (the one funnel every resolution path
> leaves through, `IcechunkDecodeError` folded in) and caught by name in
> `checkDecl`. **Additive:** zarr/netcdf/csv raise no such type, so the
> catch-all still swallows theirs and their missing-store diagnostics stay at
> lowering exactly as before. The ambiguity cell therefore now refuses at
> typecheck rather than as BL6002 at lowering, with the same message; the
> notebook's own prose says "refuses at compile time" and needed no change.
> Pinned in `IcechunkTests` section 20 (check-phase codes for a typo'd ref, an
> ambiguous bare ref and a missing repo, plus the zarr non-regression and a
> resolvable-checkout positive control).

> **Same pass, the other half — what the refusals SAY (2026-08-28).** Four
> defects, all in message text, none changing a verdict.
>
> - The rank-1 co-iteration refusal printed the raw provenance tag
>   (`'__icaxis|lat@ic_launder:9f3a1c2b4d5e6f70#2'`) where the store says
>   `lat`; the pool refusals printed `'__icpool|cov@…'`. Both now decode
>   through a hook in `Types` (`registerProviderAxisTagDecoder`, installed by
>   `ProviderStatics.install`) so `TypeLower`/`TypeEnv` need no provider
>   dependency, and an unregistered decoder reproduces the old text exactly. A
>   dense axis decodes to its dim name; a packed pool — which is not a
>   dimension — to `pool(cov)` / `orbit_pool(w)`. `tryAxisTagName` stays
>   dense-only for the IDE, where the string is rendered as an index-type name.
> - The rank≥2 refusals ("identical index shapes", "same axis tags and
>   extents") named no operand, record or axis. Both now append which record
>   first disagreed, its axis on each side, and the extents when those differ,
>   through one shared `TypeLower.indexShapeClashDetail`.
> - `AxisIdentity.SplitReason` had been recorded at every mint since §5.3 and
>   printed by nothing — so a refusal could say two axes disagree but never
>   what about the STORE made them. A second hook recovers it by tag, and
>   `Types.providerSplitClause` appends one clause: *(these are two identities
>   of axis 'lat': coordinate content differs)*.
> - The laundering ascription rendered both sides identically (`expected
>   Array<Float64 like Idx<5>, Idx<4>>, got Array<Float64 like Idx<5>,
>   Idx<4>>`) because the type printer reads no Tag. Fixed at the
>   `formatTypeError` site with one appended `note:` line rather than in the
>   printer — many corpus categories pin `Idx<n>` in error text — and scoped to
>   provider tags so nothing outside this feature can see it.
>
> One live bug fell out of making the tag user-facing: `irTypeUnknownAxisPath`
> classified any dotted Tag as an unresolved `store.index.<dim>` path. A
> provider tag's repo-label half is a DIRECTORY NAME, and `<name>.icechunk` is
> the conventional spelling — so `type L = ck.index.lat` was refused on every
> conventionally-named repo, printing the raw tag while doing it. Every fixture
> in the suite is named without a dot, which is why nothing caught it; section
> 20 now writes `ic_dotted.icechunk` and pins the accept.

**Notebook lane: PASSES** (`blade repl`, the interpreter — so provider reads,
checkouts and axis identity all work without codegen): `mean_drift = -0.5`,
and both refusal cells fire, the ambiguity one included (it surfaced there
before it surfaced at `check`, because the REPL lowers; since BL2008 both
phases report it). One rule the lane imposes and the
notebook now states: **a cell's working directory is the notebook's own
directory**, so the store path is `data/station_temps.icechunk`, not the
repo-root-relative spelling a `blade run` from the root would want. Also
worth knowing when a notebook "can't find the provider": the VS Code
extension resolves its binary through `blade.compilerPath` first, and a
stale pin (e.g. a pre-net10 `bin/Release/net7.0/Blade.exe`) fails as
`BL2004 module 'icechunk' not found` — an old binary, not a broken import.
The vendored `node_modules/@blade-lang/ide-protocol/resolveCompiler.js`
carries pre-net10 `net7.0` candidates too; the repo's own
`protocol/resolveCompiler.js` is already correct, so that copy is stale
vendoring for the ide-protocol branch to re-sync.

Two display facts the first real notebook run surfaced, both fixed rather than
explained away. (1) A checkout's arrays printed as `Array<Float64 like Idx<24>,
Idx<10>, Idx<12>>`: the type printer's nominal-name map reads `IRIndexType.Tag`
and drops `__`-prefixed tags as synthetic, which is exactly what P3's axis tag
is — so the store's own dim names sat inside the tag, unreadable.
`IcechunkProvider.tryAxisTagName` now decodes the name (and the split ordinal)
back out for `Ide.indexNamesOf`, giving `Array<Float64 like Idx<time>, Idx<lat>,
Idx<lon>>` — the same `Idx<Name>` house form a user-declared alias prints in,
and a diverged identity reads as `Idx<lat#2>` rather than colliding on sight.
Display only; zarr and netcdf are untouched. (2) `let static` over a provider
array materializes as a structural TUPLE, since `StaticValue` has no array
carrier (`ProviderStatics.shapeValue` nests `SVTuple`s) — by design, not a
provider bug, and identical under zarr. Indexing one is not a supported static
form, so the notebook folds a two-cell `lat_bounds` pair and says what the
tuple is.

The icechunk-python oracle cross-check is **DONE (2026-08-28)** — see §10's
outcome block for the fixture and the defect it found
(`IcechunkProvider.decompress` cannot read the reference's unsized streaming
zstd frames). §19 currently reports 7 passes — the whole header cross-check —
and one loud failure at the load; everything downstream is verified to pass
once the frame handling is corrected.

## 14. Risks

- **First NuGet dependencies** in Blade.fsproj (zstd at minimum). Managed-only
  packages preserve the no-native-library property, but the build acquires a
  restore step it never had; worth a deliberate call, not a drive-by.
- **Schema drift**: FlatBuffers forward-compat absorbs added fields; the spec
  version byte is the cliff, and it refuses loudly. A spec-3 repo is a new
  reader, by design.
- **Axis-sharing false negatives** after manifest compaction/expiration
  (§5.2): sound but potentially surprising; the divergence diagnostic must say
  *why* so the user isn't debugging a phantom.
- **Desugar placement**: the pass must run before *every* consumer of the raw
  `Decl` list — typecheck, StaticEval's `providerRoots` scan, and the Ide
  walkers. A consumer that grabs the AST upstream of the pass sees undesugared
  checkouts and fails obliquely (StaticEval's miss is silent: "not foldable").
  The P2 gate includes a fold test *and* a runtime test *and* an IDE-hover
  check for this reason. Span fidelity on the rewritten node is part of the
  gate: diagnostics must point at the checkout text, not the synthesized key.
- **Baked snapshots vs GC**: a compiled binary pinned to an expired snapshot
  dies at first read (loudly, missing file). Acceptable — and the loud path
  must stay loud through the shared emitter refactor.
- The **local-FS conditional-write mechanism** for the `repo` file is
  unconfirmed (read path doesn't care; the write arc does). Recorded here so
  the writes arc starts from the open question, not a guess.

## 15. Tier-2 hardening — outcome (2026-08-29)

An adversarial re-read of the landed provider, taken as a backlog of small
items. Everything below is implemented in `src/providers/IcechunkProvider.fs`
unless named otherwise, and each item observable from outside the module is
pinned in `tests/IcechunkTests.fs` §21.

**Caps that only counted.** `maxBakedChunks` bounded the number of baked table
*entries* and said nothing about their size. An inline chunk emits as ~5
characters of C++ hex per byte, so one 64 MB inline chunk was a single table
entry and ~320 MB of generated source — a g++ that never returns, reported as
neither a refusal nor an error. There is now a second, independent cap on a
variable's **total inline payload** (4 MiB), whose refusal names the store and
the variable and points at the *store-side* remedy (re-chunk so the chunks are
written natively). `maxBakedChunks` came down 1_000_000 → 100_000 in the same
breath: a million baked entries was never a size anyone wanted to compile.

**The chunk grid, in int64.** Three sites computed the grid as
`gridDims … |> List.map int` and multiplied in `int`, so a chunk-grid product
past `Int32.MaxValue` either truncated — a table of the wrong length, with the
manifest scattered into it at wrapped indices — or threw a bare
`ArgumentException` out of `Array.create` inside `resolveArray`. The product is
now folded in int64 with saturation (the honest product can overflow int64 too)
and capped *before* anything narrows, by one shared `gridLens` helper the fold
path, the chunk source and the emitter all call. At the metadata gate the
refusal reaches `blade check` through the provider's resolution funnel.

**Rank 0.** `parseArrayMetaV3` accepts `shape: []`, and every icechunk gate
below it is written over at least one dimension, so a scalar array slipped
through all of them and died in the dense emitter's flat-index expression
(`idxVars.[0]` on an empty loop nest). Refused by name at `arrayMetaOfNode`,
with the remedy (store it as a length-1 rank-1 array, or keep it in
attributes). The Zarr provider was deliberately **not** changed: its dense read
already refuses rank 0 cleanly in `genAssembleFlatVia`, and its *writer* emits
rank-0 arrays on purpose, so a metadata-parse refusal there would break a
round-trip the zarr lane owns.

**The change stamp.** The snapshot pin fixed *phase* consistency; the stamp
itself was still `(mtime ticks, byte length)` of `$ROOT/repo`, and the one
rewrite this provider must notice defeats both halves. A branch **reset** swaps
a fixed-width `snapshot_index` inside a FlatBuffer, so the file keeps its length
exactly; Windows' ~15.6 ms timestamp granularity lets that reset share an mtime
with the write before it. Since the stamp is the read memos' key, an undetected
rewrite is not a stale read but a *split* one. The stamp now carries the first 8
bytes of SHA-256 over the repo file — a few hundred bytes to a few kilobytes,
read once per repo per compilation because the pin is taken once. §21
reproduces the scenario literally (write, capture mtime, reset the branch,
force the mtime back) and drives it through `resetAxisMint`, the per-request
reset the IDE daemon does, which drops the pin and keeps the memos: the only
path on which the stamp is load-bearing.

Audited alongside it: `snapshotMemo` and `manifestMemo` carried the mutable
repo stamp in their keys and no longer do. Both files are content-addressed and
immutable, so `(canonical repo path, object id)` already *is* their content
identity, and a mutable stamp in the key could only ever force a re-decode of
bytes known not to have changed.

**Split reasons, told against the closest prior.** The identities of one axis
are a set this compilation has met in checkout order, not a chain, so "the
previous identity" was an accident of which checkouts a program names.
A (extent 10, coordinate X) → B (extent 8) → C (extent 10, coordinate Y)
recorded C's reason against B and printed "extent 8 -> 10" — true of B, and
silent about the pairing a user actually hits (C against A, whose real story is
a coordinate divergence). `closestPrior` now prefers an identity of the same
extent and otherwise falls back to the oldest. This text is *printed*, at every
co-iteration refusal, through `trySplitReasonOfTag`.

**Refusals that were quietly unsound.** The dense axis extent fell back to
`-1L` on a non-literal `IRIndexType.Extent`; an axis identity is
`(extent, coordinate fingerprint)`, and the fingerprint is `None` for every
dimension without a coordinate variable, so a shared sentinel extent would have
made *all* symbolic axes one identity — a licence to co-iterate arrays with
nothing to do with each other. It fails loudly instead. `AxisMintTable.freshId`
counts from 0x30000000 and never rewinds, so its reserved range is exhaustible,
and 0x40000000 upward belongs to CodeGen; walking past that boundary would hand
a provider axis an id CodeGen also mints, and equal ids are exactly what makes
two index types unify. It is now a tripwire with a message, not a wrap. Both
are unreachable by construction and untestable in-lane; the reasoning is in the
comments.

**Diagnostics.** "no branch named X — branches in this repo: …" printed the
entire ref namespace; one branch per experiment (or per CI run, or per user) is
an ordinary repo, and the listing is a hint, not an inventory. Capped at ten
per namespace with "and N more". The compressed-store refusal — inherited from
`ZarrProvider`'s codec gate, and the first wall a real store hits, since
zarr-python and icechunk-python both compress by default — pointed at "the
ZarrCodec extension point", which is a seam inside this compiler: a thing to
implement, not a thing to do. Both arms (v2 `compressor`, v3 `codecs`) now name
the codec found *and* the writer-side remedy, keeping the compiler seam as a
trailing note. No test pinned the old wording, in either lane.

**Emitter tidying.** `cppPathLit` escaped backslashes in a string `normPath`
had just stripped of them — dead, and reading as a safety it was not. And the
per-chunk path table baked one full path string per chunk even when
`PackNativeChunks` had put every chunk of a variable in a single file: distinct
paths now go in their own table behind an index, and the one-file case
collapses to a single baked pointer.

**A regeneration guard for the committed store.**
`examples/data/station_temps.icechunk` is both committed (the notebook reads it
and pins numbers computed from it) and generated
(`examples/tools/make_station_icechunk.fsx`), and nothing connected the two: a
generator edit nobody ran, or a store edit nobody generated, left a fixture and
a recipe that disagree — surfacing eventually as notebook pins that are quietly
wrong. The `RepoSpec` moved into `examples/tools/StationSpec.fs`, which the
script `#load`s and `Blade.fsproj` compiles (one `<Compile>` entry, after
`IcechunkWrite.fs`); §21 rebuilds it into a scratch directory and compares —
file-name set equality across the whole store, plus byte equality of `repo` and
every snapshot. Chunks and manifests are content-addressed, so their *names*
are already the content claim.

**Test-harness hygiene.** The "repo missing at run time" e2e block was
`if File.Exists builtExe then …` with no `else`: on a machine without g++ the
whole assertion vanished from the output — no PASS, no FAIL, no SKIP, nothing
to notice was missing. It prints a SKIP now, in the file's `baselineFailed`
style.
