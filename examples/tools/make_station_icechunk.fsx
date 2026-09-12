// make_station_icechunk.fsx — deterministic fixture generator for the
// Icechunk demo notebook (examples/station_temps.bladenb).
//
// Writes the small versioned station-temperature repo at
// examples/data/station_temps.icechunk, using the repo's own fixture writer
// (Blade.IcechunkWrite, src/providers/IcechunkWrite.fs) end to end.
//
// THE SPEC ITSELF LIVES IN StationSpec.fs, beside this script, because it has
// a second consumer: IcechunkTests section 21 rebuilds the same value into a
// scratch directory and compares it against the committed store, so a
// generator edit that was never run (or a store edit that was never
// generated) fails a test instead of silently rotting the notebook's pinned
// numbers. This script is now just "write that value here, and print the ids".
//
// Everything in the spec is analytic (no RNG) and IcechunkWrite's ids are
// content-derived, so the committed repo is byte-stable across regenerations.
//
// Run from anywhere, AFTER `dotnet build Blade.fsproj -c Release`:
//   dotnet fsi examples/tools/make_station_icechunk.fsx
// (Regenerates the committed repo in place; idempotent. Prints the snapshot
// ids the notebook's pinned-checkout cell bakes in.)

#r "../../bin/Release/net10.0/Google.FlatBuffers.dll"
#r "../../bin/Release/net10.0/ZstdSharp.dll"
#r "../../bin/Release/net10.0/Blade.IcechunkFormat.dll"
#load "../../src/providers/IcechunkWrite.fs"
#load "StationSpec.fs"

open System.IO
open Blade.IcechunkWrite

let spec = Blade.Examples.StationSpec.spec

let root =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "data", "station_temps.icechunk")
    |> Path.GetFullPath

writeRepo root spec

printfn "repo:    %s" root
for s in spec.Snapshots do
    printfn "%-12s snapshot %s" s.Name (snapshotId spec s.Name)
printfn "refs:    main + regrid + release (branches); v1.0 + release (tags)"
printfn "check:   mean(main.temp - v1.0.temp) = -0.5 exactly"
