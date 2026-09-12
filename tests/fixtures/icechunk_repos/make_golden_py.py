#!/usr/bin/env python3
"""Regenerate golden_py/ -- the ONE Icechunk repo in this tree written by the
REFERENCE implementation instead of by Blade's own fixture writer.

    RUN BY HAND, NEVER BY THE BUILD OR THE TEST SUITE.

Blade's no-Python-in-the-loop rule is about the build and test loop: nothing
here is invoked by `dotnet build`, `blade test`, or CI. This script exists as
PROVENANCE -- committed beside its output so the bytes in `golden_py/` can be
audited and reproduced, exactly as `examples/tools/make_station_icechunk.fsx`
sits beside the store it writes. The output is committed; the script is not
wired to anything.

WHY IT EXISTS. `Blade.IcechunkWrite` (the writer) and
`Blade.IcechunkProvider` (the reader) are independent statements of the
Icechunk format -- magic bytes, header layout and the Crockford encoder are
each written out twice -- which makes a round trip through both a real
cross-check of the two Blade halves against each other. What it is NOT is a
check against the format. Both halves were hand-transcribed from the same
spec by the same author, so a header belief that is wrong but CONSISTENT
passes every test in `tests/IcechunkTests.fs` sections 1-17. This repo is the
outside voice: section 18 reads bytes no Blade code wrote.

Setup (Python 3.11+; a venv anywhere outside the repo):

    python -m venv .venv-icechunk
    .venv-icechunk/Scripts/pip install "icechunk==2.1.2" "zarr==3.3.0"
    .venv-icechunk/Scripts/python tests/fixtures/icechunk_repos/make_golden_py.py

The icechunk pin matters: `icechunk.supported_spec_versions()` must offer
spec **v2**, which is the only on-disk spec version the Blade reader accepts
(it refuses spec 1 at the header, before any payload is touched). 2.1.2
writes v2 by default.

WHAT IT WRITES, and why each choice is load-bearing for the reader:

  * TWO snapshots on `main`, the second a DATA-ONLY change (`temp` rewritten,
    the `lat`/`lon` coordinate arrays untouched) -- the shape the axis
    identity rules of plan section 5.2 are about.
  * ONE tag, `v1.0`, on the first snapshot: the reader resolves branches and
    tags out of separate namespaces in the repo file.
  * float64 everywhere with `dimension_names` on every array. The reader
    cross-checks the names in the zarr.json against the ones icechunk records
    structurally in `ArrayNodeData.dimension_names`, so an array carrying
    both is the interesting case.
  * `compressors=None, filters=None` -- the CHUNK codec pipeline is the
    single little-endian `bytes` codec and nothing else. This is not
    decoration: the Blade reader REFUSES a compressed chunk codec, so a
    default-compressed array would make this fixture unreadable for reasons
    that have nothing to do with the format. Icechunk's own METADATA-level
    zstd (the compression byte in the 39-byte header) is untouched and
    expected -- that is a different layer, and the reader decompresses it.
  * `inline_chunk_threshold_bytes=0` -- every chunk gets its own file in
    `chunks/` rather than riding inline in the manifest, which is what puts
    the reader's native-chunk path (object id + offset + length) under test.
  * Tiny extents: 4x5 and 5x5, KB-scale in total.

THE VALUES ARE CHOSEN, NOT ARBITRARY. `tests/IcechunkTests.fs` section 18
pins them as literals, so they are written here to be checkable by eye:

    lat  = [10, 20, 30, 40]                       sum   100.0
    lon  = [100, 101, 102, 103, 104]              sum   510.0
    temp(i, j) = 100*i + j        (snapshot 1)    sum  3040.0
    temp(i, j) = 100*i + j + 1    (snapshot 2)    sum  3060.0

    temp_v1(0, 0) =   0.0     temp_main(0, 0) =   1.0
    temp_v1(2, 3) = 203.0     temp_main(2, 3) = 204.0
    temp_v1(3, 4) = 304.0     temp_main(3, 4) = 305.0

The +1 offset over 20 cells is why the two sums differ by exactly 20: a test
that reads the wrong snapshot cannot land on the right total by accident.
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path

import icechunk
import numpy as np
import zarr

# --- the pinned values (see the module docstring) ---------------------------

LAT = np.array([10.0, 20.0, 30.0, 40.0], dtype="float64")
LON = np.array([100.0, 101.0, 102.0, 103.0, 104.0], dtype="float64")

# temp(i, j) = 100*i + j, and the second commit adds 1.0 to every cell.
TEMP_V1 = np.array([[100.0 * i + j for j in range(5)] for i in range(4)], dtype="float64")
TEMP_V2 = TEMP_V1 + 1.0


def build(root: Path) -> None:
    if root.exists():
        shutil.rmtree(root)
    root.mkdir(parents=True)

    storage = icechunk.local_filesystem_storage(str(root))
    config = icechunk.RepositoryConfig.default()
    # Native chunk files, not inline manifest bytes: see the docstring.
    config.inline_chunk_threshold_bytes = 0
    repo = icechunk.Repository.create(
        storage,
        config=config,
        spec_version=icechunk.SpecVersion.v2,
    )

    # --- snapshot 1: the coordinate arrays plus the raw field ---------------
    session = repo.writable_session("main")
    group = zarr.open_group(store=session.store, mode="a")

    lat = group.create_array(
        "lat", shape=LAT.shape, chunks=LAT.shape, dtype="float64",
        dimension_names=("lat",), compressors=None, filters=None,
    )
    lat[:] = LAT

    lon = group.create_array(
        "lon", shape=LON.shape, chunks=LON.shape, dtype="float64",
        dimension_names=("lon",), compressors=None, filters=None,
    )
    lon[:] = LON

    # Two chunks along lat, so the manifest carries more than one ChunkRef and
    # the reader has to place them by index rather than by luck.
    temp = group.create_array(
        "temp", shape=TEMP_V1.shape, chunks=(2, 5), dtype="float64",
        dimension_names=("lat", "lon"), compressors=None, filters=None,
    )
    temp[:, :] = TEMP_V1

    snap1 = session.commit("raw field")
    repo.create_tag("v1.0", snap1)

    # --- snapshot 2: DATA ONLY -- lat and lon are not touched ---------------
    session = repo.writable_session("main")
    group = zarr.open_group(store=session.store, mode="a")
    group["temp"][:, :] = TEMP_V2
    snap2 = session.commit("corrected field: +1.0 everywhere")

    print(f"repo:     {root}")
    print(f"v1.0      snapshot {snap1}")
    print(f"main      snapshot {snap2}")
    print(f"sum(v1.0.temp)  = {TEMP_V1.sum()}")
    print(f"sum(main.temp)  = {TEMP_V2.sum()}")
    print(f"sum(lat) = {LAT.sum()}   sum(lon) = {LON.sum()}")


if __name__ == "__main__":
    out = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent / "golden_py"
    build(out.resolve())
