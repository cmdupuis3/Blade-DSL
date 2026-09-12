# Vendored Icechunk format schemas + generated accessors

`schema/` holds the FlatBuffers schemas copied verbatim from the Icechunk
reference implementation; `generated/` holds the `flatc`-generated C# compiled
by `Blade.IcechunkFormat.csproj` (C# because flatc emits no F#). Everything in
this directory is a build input pinned to one upstream release — never edit
`generated/` by hand, and treat a version bump of either the schemas or the
FlatBuffers toolchain as a wire-behavior change gated by the golden-bytes
tests in `tests/IcechunkTests.fs`.

Provenance:

- Source: https://github.com/earth-mover/icechunk, tag `v2.1.2`
  (commit `d4b5b04324`), files `icechunk-format/flatbuffers/*.fbs`
  (spec 2.1, https://icechunk.io/en/latest/reference/spec-v2-1/).
- Generator: `flatc` 25.2.10 (matching the `Google.FlatBuffers` 25.2.10
  runtime the csproj references — keep these two versions equal).
- Invocation, from this directory:

```bash
flatc --csharp --gen-object-api -o generated -I schema \
    schema/common.fbs schema/repo.fbs schema/snapshot.fbs \
    schema/manifest.fbs schema/transaction_log.fbs
```

(`all.fbs` is an include aggregator only; the object API is what
`IcechunkWrite` builds fixture repos with.)

To upgrade: bump the tag here, re-download `schema/`, re-run the invocation
with the flatc release matching the new runtime, rebuild, and let
`blade test icechunk` judge the result — the design doc is
`docs/plans/plan-icechunk-provider.md` (§6.2 records this decision).
