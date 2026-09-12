// StationSpec.fs -- THE spec for the committed Icechunk demo store
// (examples/data/station_temps.icechunk), as a value.
//
// WHY THIS IS A MODULE AND NOT A SCRIPT.
//
// The store is COMMITTED: examples/station_temps.bladenb reads it, the
// notebook pins values computed from it, and IcechunkTests drives it. It is
// also GENERATED, by examples/tools/make_station_icechunk.fsx. Nothing
// connected the two: an edit to the generator that nobody ran, or an edit to
// the store that nobody generated, left the repository holding a fixture and a
// recipe that no longer agree -- and the disagreement would surface as a
// notebook whose pinned numbers are wrong, which is the least legible place it
// could surface.
//
// So the spec lives here, in ONE value that both consumers read:
//
//   * make_station_icechunk.fsx `#load`s this file and writes `spec` to disk;
//   * IcechunkTests section 21 rebuilds `spec` into a scratch directory and
//     compares it against the committed store, file for file and byte for
//     byte.
//
// The comparison is only a real regeneration guard because `IcechunkWrite`'s
// ids are CONTENT-derived and everything below is analytic (no RNG, no clock):
// the same spec writes the same bytes in any directory, process or machine.
//
// THE HISTORY, authored for the notebook's story:
//
//   tag  v1.0    -> s_raw        raw ingest: a +1.5 K warm bias in the
//                                eastern third (lon index >= 8)
//   main         -> s_corrected  the bias-correction commit. DATA-ONLY:
//                                the coordinate arrays are byte-identical,
//                                so their manifest and chunk files are
//                                REUSED (content-derived ids) -- which is
//                                what licenses cross-checkout arithmetic
//                                in the notebook (plan section 5).
//   regrid       -> s_regrid     lat re-gridded at twice the resolution:
//                                a genuinely different axis, so mixing it
//                                with main REFUSES.
//   release      -> s_corrected  a branch AND a tag of the same name (the
//   release(tag) -> s_raw        bare-name ambiguity refusal fixture).
//
// The bias band is 4 of 12 longitudes at exactly +1.5, so
// mean(main - v1.0) = -0.5 exactly -- the notebook pins it.
module Blade.Examples.StationSpec

open System
open Blade.IcechunkWrite

let nT, nLat, nLon = 24, 10, 12
let nLatFine = 20

// Coordinate values. time in hours; lat/lon in degrees.
let timeHours = [| for t in 0 .. nT - 1 -> float t |]
let latCoarse = [| for la in 0 .. nLat - 1 -> 30.0 + 0.5 * float la |]
let latFine   = [| for la in 0 .. nLatFine - 1 -> 30.0 + 0.25 * float la |]
let lonVals   = [| for lo in 0 .. nLon - 1 -> 100.0 + 0.5 * float lo |]

/// The "true" field: a lat gradient, a weak lon tilt, a diurnal cycle.
let baseTemp (latDeg: float) (lonIdx: int) (t: int) =
    20.0 + 0.8 * (latDeg - 30.0) - 0.1 * float lonIdx
         + 2.0 * sin (2.0 * Math.PI * float t / float nT)

/// Row-major (time, lat, lon) cells over a given latitude axis.
let field (lats: float[]) (bias: bool) : float[] =
    [| for t in 0 .. nT - 1 do
         for la in 0 .. lats.Length - 1 do
           for lo in 0 .. nLon - 1 ->
             baseTemp lats.[la] lo t + (if bias && lo >= 8 then 1.5 else 0.0) |]

/// The domain's latitude span as a CF-style bounds pair. Two cells, so a
/// compile-time fold of it stays legible: `let static` materializes a provider
/// array as a structural TUPLE (StaticValue has no array carrier), which is
/// informative at 2 elements and noise at 24.
let latBounds (lats: float[]) =
    mkArray "lat_bounds" ["bnds"] [2L] [2L] (IceF64 [| lats.[0]; lats.[lats.Length - 1] |])

let coord name (vals: float[]) (dim: string) =
    mkArray name [dim] [int64 vals.Length] [int64 vals.Length] (IceF64 vals)

let tempArray (lats: float[]) (bias: bool) =
    // One chunk per 8 time steps: 3 native chunks (well over the 512-byte
    // inline threshold), exercising the baked-table read path.
    mkArray "temp" ["time"; "lat"; "lon"]
            [int64 nT; int64 lats.Length; int64 nLon]
            [8L; int64 lats.Length; int64 nLon]
            (IceF64 (field lats bias))

let coarseCoords = [
    coord "time_hours" timeHours "time"
    coord "lat" latCoarse "lat"
    coord "lon" lonVals "lon"
    latBounds latCoarse
]

/// The whole store, as a value. `writeRepo <root> spec` reproduces the
/// committed directory byte for byte.
let spec =
    { emptyRepo with
        Implementation = "blade-examples"
        Seed = 42
        Snapshots = [
            mkSnapshot "s_raw"       (tempArray latCoarse true  :: coarseCoords)
            mkSnapshot "s_corrected" (tempArray latCoarse false :: coarseCoords)
            mkSnapshot "s_regrid"    (tempArray latFine   false ::
                                      [ coord "time_hours" timeHours "time"
                                        coord "lat" latFine "lat"
                                        coord "lon" lonVals "lon"
                                        latBounds latFine ])
        ]
        Branches = [ "main", "s_corrected"; "regrid", "s_regrid"; "release", "s_corrected" ]
        Tags = [ "v1.0", "s_raw"; "release", "s_raw" ]
    }

/// The committed store's path, relative to the repository root. Both the
/// generator (which resolves it against its own source directory) and the
/// regeneration guard (which searches upward from the working directory) need
/// the same spelling.
let committedStoreRelPath = "examples/data/station_temps.icechunk"
