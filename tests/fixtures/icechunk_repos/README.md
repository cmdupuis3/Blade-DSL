# icechunk_repos/

Generated Icechunk fixture repos for `blade test icechunk` (see
`tests/IcechunkTests.fs`).

Every repo in here **except `golden_py/`** is written on the fly by
`Blade.IcechunkWrite.writeRepo` (`src/providers/IcechunkWrite.fs`) at the start
of the relevant test section — pure .NET file writes through the vendored
FlatBuffers object API plus ZstdSharp, with no external tool and no
icechunk-python in the loop. None of those is committed or hand-maintained, and
each is safe to delete: the next `blade test icechunk` run recreates exactly
what it needs.

**Nothing is tracked but this README, the `.gitignore` beside it, and the
`golden_py/` exception described below**, and the `.gitignore` is what enforces
that. The sibling `zarr_stores/` directory carries the same promise in prose but
has 161 generated files tracked anyway, because it never got a `.gitignore`;
this one had both from day one so the mistake does not repeat. If `git status`
ever shows a *generated* repo directory here as untracked-but-not-ignored, the
`.gitignore` is what to fix — not the test.

`writeRepo` **replaces** the root it is given (an existing directory is deleted
first), so a fixture never inherits chunk or manifest files from an earlier
spec — which matters here more than it does for Zarr, because Icechunk object
files are content-addressed and several tests count or diff them.

Two copies of each fixture exist during a test run, the same split as
`zarr_stores/`:

- `tests/fixtures/icechunk_repos/<repo>` (this directory) — resolved at the
  **compiler's** cwd, for compile-time ref resolution, metadata loads and
  static folds.
- `generated_cpp_tests/tests/fixtures/icechunk_repos/<repo>` — the mirror
  resolved at the **test executable's** cwd, for the chunk reads the compiled
  program performs at run time.

The same relative path string (`tests/fixtures/icechunk_repos/<repo>`) is baked
into the blade sources so it resolves correctly from either working directory.

## What a fixture repo looks like

```
<repo>/repo                  the RepoInfo file: refs -> snapshots (the ONLY
                             mutable object in an Icechunk repo)
<repo>/snapshots/<id>        one Snapshot per commit
<repo>/manifests/<id>        chunk tables
<repo>/chunks/<id>           RAW, headerless chunk payloads
<repo>/transactions/         created empty — tx logs are PRUNABLE, so no read
<repo>/overwritten/          path may depend on either directory existing
```

`<id>` is Crockford base32, uppercase, no padding: 20 characters for the
12-byte object ids that name snapshots, manifests and chunks. Metadata files
carry the 39-byte Icechunk header; chunk files carry no header at all.

Ids are deterministic — a truncated SHA-256, never `System.Random` — and, with
one deliberate exception, each hashes the object's own CONTENT:

- a **chunk** id hashes the chunk's bytes (plus the array name);
- a **manifest** id hashes the canonical text of the chunk-ref table it holds;
- a **snapshot** id hashes the snapshot's own serialized FlatBuffer — built once
  with a zeroed id, hashed, then rebuilt with the resulting id stamped in. Every
  other byte of the file is therefore covered: node ids and paths, each array's
  verbatim `zarr.json`, shapes, dimension names, the manifest ids and sizes it
  points at, the commit message and the timestamp;
- a **node** id is the exception: it hashes the array's NAME, because a node id
  is the array's identity *across* snapshots and so must survive a rewrite of
  the data it names. That is the spec's own stability rule, and the anchor of
  `docs/plans/plan-icechunk-provider.md` §5.2.

The seed is mixed into all four, so two fixture specs never collide.

Three consequences the tests lean on: a repo written twice from the same spec is
byte-identical; an array left untouched between two snapshots reuses its
manifest and chunk files exactly, which is what makes the axis-identity scenario
of §5 expressible as a fixture; and a snapshot id is a claim about DATA, so a
pinned `@snapshot:<id>` key cannot come to name a different dataset when a
fixture is edited.

None of that applies to `golden_py/`, which is not ours to derive.

## `golden_py/` — the one committed repo, and the only one Blade did not write

Everything above describes fixtures written by `IcechunkWrite` and read back by
`IcechunkProvider`. Those two are deliberately independent statements of the
format — the magic bytes, the 39-byte header layout and the Crockford encoder
are each spelled out twice, once per side — so a round trip through both is a
real cross-check of the two halves *against each other*.

What it is **not** is a check against the format. Both halves were transcribed
from the same spec by the same author, so a header belief that is wrong but
CONSISTENT passes every test in sections 1–18. `golden_py/` is the outside
voice: a small repo written by the reference implementation, committed as bytes,
so that section 19 reads a repo no Blade code produced.

### Provenance

| | |
|---|---|
| Written by | `icechunk` **2.1.2** (PyPI wheel `icechunk-2.1.2-cp312-abi3-win_amd64`) |
| Zarr layer | `zarr` **3.3.0**, `numcodecs` 0.16.5, `numpy` 2.5.2 |
| Python | 3.14.7 |
| Generator | `make_golden_py.py`, committed beside this file |
| Captured | 2026-08-28 |
| Size | 20 files, ~6.8 KB |

The `icechunk` pin is load-bearing: `supported_spec_versions()` must offer spec
**v2**, the only on-disk spec version the reader accepts (it refuses spec 1 at
the header, before any payload is touched). 2.1.2 writes v2 by default, and the
generator asks for it explicitly.

`make_golden_py.py` is **run by hand and never by the build or the suite**. The
repo's no-Python-in-the-loop rule is about the build and test loop; nothing
invokes this script from `dotnet build`, `blade test` or CI. It is committed as
provenance — so the bytes can be audited and re-derived — exactly as
`examples/tools/make_station_icechunk.fsx` sits beside the store it writes.

### It is a CAPTURE, not a reproducible build

Unlike every other repo here, `golden_py/` cannot be regenerated byte-for-byte:
icechunk mints snapshot ids randomly, and the `overwritten/` backup file names
embed a counter. Re-running the generator produces a different-but-equivalent
repo. Section 19 therefore pins **no object ids** — it reaches every snapshot
through `main` and the `v1.0` tag, and pins the array values, which the
generator chose to be checkable by eye.

### What the reader faces here that it never faces elsewhere

Worth knowing before debugging a failure in section 19; all of these are the
reference's choices, not mistakes:

- **The zstd frames record no content size.** The reference compresses with a
  streaming encoder: the frame header descriptor is `0x00` (`Frame_Content_Size`
  flag 0, `Single_Segment` flag 0). `IcechunkWrite` uses `ZstdSharp`'s one-shot
  `Wrap`, whose header is `0x60` and carries the exact size.
- **`zarr.json` is pretty-printed**, multi-line and indented, with the keys in a
  different order, an always-present `"attributes": {}`, and a
  `"storage_transformers": []` field `IcechunkWrite` does not write. The reader
  parses with `System.Text.Json`, so none of this is meant to matter — but it
  had never been demonstrated.
- **There is a third snapshot**, `1CECHNKREP0F1RSTCMT0` (`"Repository
  initialized"`), which the reference mints when the repo is created. That
  well-known id is the same constant sections 2–3 use as a base32 example.
- **`transactions/` and `overwritten/` are populated.** Every Blade fixture
  leaves both empty. A reader that depended on either being empty — or on
  `overwritten/` file names looking like object ids — would break here.
- **The implementation-name field reads `ic-2.1.2`**, not `blade-fixtures`.
- Chunks are native files (`inline_chunk_threshold_bytes=0`), one per chunk at
  offset 0, so the chunk-id + offset + length path is the one under test.
- Chunk codecs are `bytes` little-endian and nothing else
  (`compressors=None, filters=None`), because the reader refuses a compressed
  chunk codec. Icechunk's *metadata*-level zstd is a different layer and is left
  on.
