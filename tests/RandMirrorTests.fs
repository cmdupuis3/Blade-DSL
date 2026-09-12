/// The interpreter's RNG mirror against published known-answer vectors.
///
/// The `_at` families (plan-fortran-killer-2 section 5) address every sample
/// through Philox4x32-10. Its F# twin (src/Interp/RandMirror.fs) is pinned
/// here against Random123's `kat_vectors` file -- the three philox4x32
/// 10-round lines -- and the interpreter differential over
/// tests/corpus/rand/019-022 ties the C++ header's copy to this twin. That is
/// the chain: published vectors -> F# generator -> (byte-identical corpus
/// output) -> C++ generator. The mirror-level identity properties of the
/// address (chunk / order / subrange invariance, stream and key separation)
/// are checked here too, on the generator alone, so a regression names the
/// layer it is in.
module Blade.Tests.RandMirrorTests

open Blade.Tests.TestHarness
open Blade.Interp

/// Random123 tests/kat_vectors, the `philox4x32 10` lines: counter (4 words),
/// key (2 words), expected output (4 words).
let private katVectors : ((uint32 * uint32 * uint32 * uint32) * (uint32 * uint32) * (uint32 * uint32 * uint32 * uint32)) list =
    [ (0x00000000u, 0x00000000u, 0x00000000u, 0x00000000u), (0x00000000u, 0x00000000u),
      (0x6627e8d5u, 0xe169c58du, 0xbc57ac4cu, 0x9b00dbd8u)
      (0xffffffffu, 0xffffffffu, 0xffffffffu, 0xffffffffu), (0xffffffffu, 0xffffffffu),
      (0x408f276du, 0x41c83b0eu, 0xa20bc7c6u, 0x6d5451fdu)
      (0x243f6a88u, 0x85a308d3u, 0x13198a2eu, 0x03707344u), (0xa4093822u, 0x299f31d0u),
      (0xd16cfe09u, 0x94fdccebu, 0x5001e420u, 0x24126ea1u) ]

let private hex4 (a: uint32, b: uint32, c: uint32, d: uint32) = sprintf "%08x %08x %08x %08x" a b c d

let private katCase (i: int) (ctr, key, expected) =
    let name = $"philox4x32_10_kat_{i}"
    let got = RandMirror.philox4x32_10 ctr key
    if got = expected then
        resultLine Pass name (hex4 got)
        true
    else
        resultLine Fail name ($"expected {hex4 expected}, got {hex4 got}")
        false

/// Sample `offset + i` drawn as part of one fill equals the same sample drawn
/// in another fill -- for a data-dependent transform, so the per-sample
/// draw counter is what is being tested.
let private chunkInvariance () =
    let name = "philox_gamma_at_chunk_invariance"
    let whole = RandMirror.drawsAt "gamma_at" 99L 3L 0L [ 2.5; 1.0 ] 10
    let tail = RandMirror.drawsAt "gamma_at" 99L 3L 6L [ 2.5; 1.0 ] 4
    let sub = RandMirror.drawsAt "gamma_at" 99L 3L 2L [ 2.5; 1.0 ] 5
    let ok =
        Array.forall2 (=) tail whole.[6..9] && Array.forall2 (=) sub whole.[2..6]
    if ok then (resultLine Pass name "tail and subrange fills are bit-identical to the whole"; true)
    else (resultLine Fail name "a chunked gamma_at fill differs from the whole fill"; false)

let private separation () =
    let name = "philox_uniform_at_stream_key_separation"
    let a = RandMirror.drawsAt "uniform_at" 12345L 7L 0L [] 12
    let b = RandMirror.drawsAt "uniform_at" 12345L 8L 0L [] 12
    let c = RandMirror.drawsAt "uniform_at" 12346L 7L 0L [] 12
    let again = RandMirror.drawsAt "uniform_at" 12345L 7L 0L [] 12
    let inRange = a |> Array.forall (fun u -> u >= 0.0 && u < 1.0)
    let ok = a = again && Array.forall2 (<>) a b && Array.forall2 (<>) a c && inRange
    if ok then (resultLine Pass name "same address repeats; other stream / other key differ; draws in [0, 1)"; true)
    else (resultLine Fail name "address separation failed"; false)

let runRandMirrorTests () =
    printHeader "Blade-DSL: Rand Mirror (Philox known answers)"
    let results =
        (katVectors |> List.mapi katCase) @ [ chunkInvariance (); separation () ]
    let passed = results |> List.filter id |> List.length
    let failed = results.Length - passed
    printFooter "Rand Mirror" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Rand Mirror"; Passed = passed; Failed = failed; Skipped = 0
      FailedNames = if failed = 0 then [] else ["see above"] }
