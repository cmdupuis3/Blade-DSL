// Icechunk provider tests. Fully hermetic: no real Icechunk repo is committed
// and none is needed. Sections 1-8 are PURE -- assertions about functions whose
// inputs are bytes, strings or synthetic domain values, plus a handful of
// hand-assembled 39-byte headers in a temp directory, which is enough to
// exercise every refusal that fires before a payload is even looked at.
//
// Sections 9-11 add the other half: REAL spec-2 repos, written on the fly by
// `Blade.IcechunkWrite` (src/providers/IcechunkWrite.fs) into
// tests/fixtures/icechunk_repos/ -- the ZarrWrite discipline, applied to a
// versioned store. Section 9 pins the WRITER without the provider in the loop
// (layout, byte-level headers, base32 file-name shapes, id determinism and
// reuse); section 10 drives the PROVIDER'S public surface against those repos
// (ref resolution, checkout modules, dense reads, every named refusal);
// section 11 compiles and runs a Blade program that reads one, behind the
// `Build.compileCpp` / `isSkipError` skip discipline ZarrTests uses.
//
// The reader and the writer state the format INDEPENDENTLY -- IcechunkWrite
// re-derives the magic bytes, the header layout and the Crockford encoder
// rather than calling the provider's -- so a round-trip through both is a real
// cross-check and not a tautology.
module Blade.Tests.IcechunkTests

open System
open System.IO
open Blade
open Blade.IR
open Blade.Types
open Blade.Lowering
open Blade.Build
open Blade.IcechunkProvider
open Blade.Tests.TestHarness

/// The fixture writer, always qualified: it deliberately shares NAMES with the
/// provider (`base32Encode`, `magicBytes`, `headerSize`, `repoFilePath`), and
/// the point of these tests is that the two agree -- which they cannot do if
/// an `open` silently picks one.
module IW = Blade.IcechunkWrite

let runIcechunkTests () =
    printHeader "Icechunk Provider Tests"
    let mutable passed = 0
    let mutable failed = 0

    let check (name: string) (condition: bool) (detail: string) =
        if condition then
            printfn "  PASS: %s" name
            passed <- passed + 1
        else
            printfn "  FAIL: %s — %s" name detail
            failed <- failed + 1

    let isError (r: Result<'a, string>) (needle: string) =
        match r with
        | Error e -> e.Contains needle
        | Ok _ -> false

    let errorText (r: Result<'a, string>) =
        match r with
        | Error e -> e
        | Ok _ -> "<Ok>"

    /// The message an exception-throwing entry point produced ("<no
    /// exception>" when it did not throw), so failures print what happened.
    let caught (f: unit -> 'a) : string =
        try
            f () |> ignore
            "<no exception>"
        with ex -> ex.Message

    // "Check the environment FIRST." Probed once, here, so the e2e section
    // can tell a genuine missing-toolchain SKIP from a real failure.
    let icCaps = Blade.Build.capabilities.Value

    /// Verdict for a failing BASELINE build (the reference compile+run every
    /// later assertion in a block depends on). A missing toolchain is a SKIP;
    /// a lowering error, a compile failure or a nonzero exit is a FAILURE.
    /// Printing SKIP unconditionally here would silently DELETE the
    /// assertions that ride on the baseline -- the block would stay green
    /// with nothing tested (the trap ZarrTests documents at its own top).
    let baselineFailed (what: string) (e: string) =
        if not icCaps.HasGpp then printfn "  SKIP %s: g++ not found (%s)" what e
        elif isSkipError e then printfn "  SKIP %s: %s" what e
        else check ($"{what}: baseline builds and runs") false e

    // Fixture repos live under tests/fixtures/icechunk_repos/ (see the README
    // there). The SAME relative string resolves at the compiler cwd
    // (compile-time ref resolution and metadata loads) and, mirrored under
    // generated_cpp_tests, at the exe cwd (runtime chunk reads).
    let fixRepo (name: string) = "tests/fixtures/icechunk_repos/" + name

    // ---------------------------------------------------------------
    // 1. The 39-byte metadata header (golden bytes)
    // ---------------------------------------------------------------
    printfn "\n--- metadata header ---"

    // The magic spelled out BYTE BY BYTE: UTF-8 of "ICE" + U+1F9CA + "CHUNK".
    // Written as bytes on purpose -- the point of the test is that the
    // parser's 12-byte magic is these bytes, not that some string literal
    // happens to have length 12 in whatever encoding the source was saved in.
    let goldenMagic =
        [| 0x49uy; 0x43uy; 0x45uy                     // I C E
           0xF0uy; 0x9Fuy; 0xA7uy; 0x8Auy             // U+1F9CA
           0x43uy; 0x48uy; 0x55uy; 0x4Euy; 0x4Buy |]  // C H U N K

    check "header: magic is 12 bytes" (goldenMagic.Length = 12) $"{goldenMagic.Length}"
    check "header: provider magic matches the golden bytes"
        (magicBytes = goldenMagic) (magicBytes |> Array.map (sprintf "%02x") |> String.concat " ")
    check "header: framing is 39 bytes" (headerSize = 39) $"{headerSize}"

    /// Build a 39-byte header: 12 magic + 24 space-padded impl name + spec + type + compression.
    let mkHeader (impl: string) (specByte: byte) (typeByte: byte) (compByte: byte) : byte[] =
        let implField = Array.create 24 0x20uy
        let implBytes = Text.Encoding.UTF8.GetBytes impl
        Array.blit implBytes 0 implField 0 (min implBytes.Length 24)
        Array.concat [ goldenMagic; implField; [| specByte; typeByte; compByte |] ]

    check "header: constructed header is exactly 39 bytes"
        ((mkHeader "icechunk-rust" 2uy 6uy 1uy).Length = 39) ""

    (match parseHeader "test" (mkHeader "icechunk-rust 1.2.3" 2uy 6uy 1uy) with
     | Ok h ->
         check "header: implementation name unpads" (h.Implementation = "icechunk-rust 1.2.3") h.Implementation
         check "header: spec version 2 accepted" (h.SpecVersion = 2) $"{h.SpecVersion}"
         check "header: file type 6 = RepoInfo" (h.FileType = FtRepoInfo) (fileTypeName h.FileType)
         check "header: compression 1 = zstd" (h.Compression = CompZstd) (compressionName h.Compression)
     | Error e -> check "header: a well-formed spec-2 header parses" false e)

    (match parseHeader "test" (mkHeader "x" 2uy 1uy 0uy) with
     | Ok h ->
         check "header: file type 1 = Snapshot" (h.FileType = FtSnapshot) (fileTypeName h.FileType)
         check "header: compression 0 = none" (h.Compression = CompNone) (compressionName h.Compression)
     | Error e -> check "header: snapshot header parses" false e)

    check "header: file type 2 = Manifest"
        (match parseHeader "test" (mkHeader "x" 2uy 2uy 0uy) with
         | Ok h -> h.FileType = FtManifest
         | Error _ -> false) ""
    check "header: file type 4 = TransactionLog"
        (match parseHeader "test" (mkHeader "x" 2uy 4uy 0uy) with
         | Ok h -> h.FileType = FtTransactionLog
         | Error _ -> false) ""
    // Unstamped-but-defined enum members are TOLERATED (they parse, and the
    // diagnostic names them) rather than refused at the header.
    check "header: file type 3 = Attributes (tolerated)"
        (match parseHeader "test" (mkHeader "x" 2uy 3uy 0uy) with
         | Ok h -> h.FileType = FtAttributes
         | Error _ -> false) ""
    check "header: file type 5 = Chunk (tolerated)"
        (match parseHeader "test" (mkHeader "x" 2uy 5uy 0uy) with
         | Ok h -> h.FileType = FtChunk
         | Error _ -> false) ""
    check "header: an unknown file type parses and is NAMED, not swallowed"
        (match parseHeader "test" (mkHeader "x" 2uy 99uy 0uy) with
         | Ok h -> h.FileType = FtUnknown 99 && (fileTypeName h.FileType).Contains "99"
         | Error _ -> false) ""

    // Spec version 1: refused BY NAME, citing the version this reader wants.
    (let r = parseHeader "repo file 'r/repo'" (mkHeader "icechunk-rust" 1uy 6uy 0uy)
     check "header: spec version 1 refused by name"
         (isError r "spec version 1" && isError r "spec version 2") (errorText r))
    (let r = parseHeader "test" (mkHeader "icechunk-rust" 3uy 6uy 0uy)
     check "header: an unknown spec version refuses as unknown"
         (isError r "unknown Icechunk spec version 3") (errorText r))

    // Compression: 0 and 1 known, anything else refused (never assumed raw).
    (let r = parseHeader "test" (mkHeader "x" 2uy 6uy 7uy)
     check "header: unknown compression byte refused"
         (isError r "unknown compression byte 7") (errorText r))

    (let r = parseHeader "test" (Array.sub (mkHeader "x" 2uy 6uy 0uy) 0 38)
     check "header: a 38-byte file is refused as too short"
         (isError r "39-byte") (errorText r))
    (let r = parseHeader "test" [||]
     check "header: an empty file is refused as too short" (isError r "39-byte") (errorText r))

    (let bad = mkHeader "x" 2uy 6uy 0uy
     bad.[3] <- 0x00uy
     let r = parseHeader "test" bad
     check "header: wrong magic refuses as 'not an Icechunk metadata file'"
         (isError r "not an Icechunk metadata file") (errorText r))

    // The WRITER's header is the same 39 bytes, derived independently.
    (let written = IW.makeHeader "blade-fixtures" 2uy IW.ftRepoInfo IW.compZstd
     check "header: the fixture writer's header is byte-identical to the twin"
         (written = mkHeader "blade-fixtures" 2uy 6uy 1uy)
         (written |> Array.map (sprintf "%02x") |> String.concat " ")
     check "header: the writer's magic equals the provider's"
         (IW.magicBytes = magicBytes) ""
     check "header: the writer's framing size equals the provider's"
         (IW.headerSize = headerSize) $"{IW.headerSize}"
     match parseHeader "written" written with
     | Ok h ->
         check "header: the provider parses what the writer wrote"
             (h.Implementation = "blade-fixtures" && h.SpecVersion = 2
              && h.FileType = FtRepoInfo && h.Compression = CompZstd) $"%A{h}"
     | Error e -> check "header: the provider parses what the writer wrote" false e)
    check "header: the writer refuses an over-long implementation name"
        ((caught (fun () -> IW.makeHeader (String.replicate 25 "x") 2uy 6uy 0uy)).Contains "header field")
        (caught (fun () -> IW.makeHeader (String.replicate 25 "x") 2uy 6uy 0uy))

    // ---------------------------------------------------------------
    // 2. Crockford base32 (object ids -> file names)
    // ---------------------------------------------------------------
    printfn "\n--- Crockford base32 ---"

    check "base32: alphabet is 32 characters" (base32Alphabet.Length = 32) base32Alphabet
    check "base32: alphabet omits I, L, O and U"
        (["I"; "L"; "O"; "U"] |> List.forall (fun c -> not (base32Alphabet.Contains c))) base32Alphabet

    // The spec's own example id.
    (let bytes =
        [| 0x0buy; 0x1cuy; 0xc8uy; 0xd6uy; 0x78uy; 0x75uy; 0x80uy; 0xf0uy; 0xe3uy; 0x3auy; 0x65uy; 0x34uy |]
     let s = base32Encode bytes
     check "base32: spec example encodes to 1CECHNKREP0F1RSTCMT0" (s = "1CECHNKREP0F1RSTCMT0") s
     check "base32: the fixture writer's encoder agrees on the spec example"
         (IW.base32Encode bytes = s) (IW.base32Encode bytes))

    let zeros12 = base32Encode (Array.zeroCreate 12)
    let zeros8 = base32Encode (Array.zeroCreate 8)
    check "base32: a 12-byte object id is 20 characters" (zeros12.Length = objectIdChars) zeros12
    check "base32: an 8-byte node id is 13 characters" (zeros8.Length = nodeIdChars) zeros8
    check "base32: all-zero bytes encode to all zeros (right-padded, no '=')"
        (zeros12 = String.replicate 20 "0") zeros12
    // 64 one-bits: 12 full groups of 11111, then 1111 right-padded to 11110.
    check "base32: 8 x 0xFF encodes to ZZZZZZZZZZZZY (zero-bit right padding)"
        (base32Encode (Array.create 8 0xFFuy) = "ZZZZZZZZZZZZY") (base32Encode (Array.create 8 0xFFuy))
    check "base32: empty input encodes to the empty string" (base32Encode [||] = "") (base32Encode [||])

    // The two encoders are independent statements of the same rule; a
    // disagreement anywhere in the byte space would make every fixture file
    // name unreadable by the provider, so sweep rather than spot-check.
    check "base32: writer and provider encoders agree over 256 12-byte ids"
        ([ 0 .. 255 ] |> List.forall (fun i ->
            let b = Array.init 12 (fun j -> byte ((i * 31 + j * 7) % 256))
            IW.base32Encode b = base32Encode b)) ""

    check "base32: a 20-char canonical id is object-id shaped"
        (isObjectIdForm "1CECHNKREP0F1RSTCMT0") ""
    check "base32: 19 characters is NOT object-id shaped" (not (isObjectIdForm "1CECHNKREP0F1RSTCMT")) ""
    check "base32: a name containing 'I' is NOT object-id shaped (I is not in the alphabet)"
        (not (isObjectIdForm "1CECHNKREPIF1RSTCMT0")) ""
    check "base32: lowercase is NOT object-id shaped (ids are canonical uppercase)"
        (not (isObjectIdForm "1cechnkrep0f1rstcmt0")) ""
    check "base32: a plain branch name is not mistaken for a snapshot id"
        (not (isObjectIdForm "main")) ""

    // ---------------------------------------------------------------
    // 3. The canonical key (plan §3.1)
    // ---------------------------------------------------------------
    printfn "\n--- canonical key ---"

    let roundTrips (k: string) =
        match parseKey k with
        | Ok parsed -> formatKey parsed = k
        | Error _ -> false

    (match parseKey "data/weather.icechunk" with
     | Ok k ->
         check "key: a bare path is a repo-handle load" (k.Ref = None && k.RepoPath = "data/weather.icechunk") $"%A{k}"
     | Error e -> check "key: bare path parses" false e)

    (match parseKey "data/weather.icechunk@branch:main" with
     | Ok k ->
         check "key: branch refspec parses"
             (k.RepoPath = "data/weather.icechunk" && k.Ref = Some (RefBranch, "main")) $"%A{k}"
     | Error e -> check "key: branch key parses" false e)

    (match parseKey "r@tag:v1.0" with
     | Ok k -> check "key: tag refspec parses" (k.Ref = Some (RefTag, "v1.0")) $"%A{k}"
     | Error e -> check "key: tag key parses" false e)
    (match parseKey "r@snapshot:1CECHNKREP0F1RSTCMT0" with
     | Ok k ->
         check "key: snapshot refspec parses"
             (k.Ref = Some (RefSnapshot, "1CECHNKREP0F1RSTCMT0")) $"%A{k}"
     | Error e -> check "key: snapshot key parses" false e)
    (match parseKey "r@?:main" with
     | Ok k -> check "key: '?' is the bare checkout form" (k.Ref = Some (RefBare, "main")) $"%A{k}"
     | Error e -> check "key: bare-form key parses" false e)

    // A Windows path's drive colon must not be mistaken for the kind:name colon.
    (match parseKey "C:\\data\\w.icechunk@branch:main" with
     | Ok k ->
         check "key: a Windows drive path survives parsing"
             (k.RepoPath = "C:\\data\\w.icechunk" && k.Ref = Some (RefBranch, "main")) $"%A{k}"
     | Error e -> check "key: Windows path key parses" false e)
    (match parseKey "C:\\data\\w.icechunk" with
     | Ok k -> check "key: a bare Windows path is a repo handle" (k.Ref = None) $"%A{k}"
     | Error e -> check "key: bare Windows path parses" false e)

    check "key: round-trips (bare)" (roundTrips "data/weather.icechunk") ""
    check "key: round-trips (branch)" (roundTrips "data/weather.icechunk@branch:main") ""
    check "key: round-trips (tag)" (roundTrips "data/weather.icechunk@tag:v1.0") ""
    check "key: round-trips (snapshot)" (roundTrips "r@snapshot:1CECHNKREP0F1RSTCMT0") ""
    check "key: round-trips (bare form '?')" (roundTrips "r@?:main") ""
    check "key: kind tokens are branch/tag/snapshot/?"
        (List.map kindToken [RefBranch; RefTag; RefSnapshot; RefBare] = ["branch"; "tag"; "snapshot"; "?"]) ""

    // The desugar and the provider must spell the key the same way, or a
    // checkout resolves to a path the provider parses differently.
    check "key: ProviderDesugar.canonicalKey is what parseKey accepts"
        (roundTrips (Blade.ProviderDesugar.canonicalKey "data/w.icechunk" "tag" "v1.0")
         && Blade.ProviderDesugar.canonicalKey "data/w.icechunk" "tag" "v1.0" = "data/w.icechunk@tag:v1.0")
        (Blade.ProviderDesugar.canonicalKey "data/w.icechunk" "tag" "v1.0")
    check "key: the desugar's bare-form kind token is the provider's '?'"
        (roundTrips (Blade.ProviderDesugar.canonicalKey "r" "?" "main")) ""

    // The last '@' separates a refspec ONLY when what follows is
    // "<known-kind>:<name>". Anything else is an ordinary character of a repo
    // PATH -- because paths really do contain '@' -- so these two parse as bare
    // paths rather than as malformed keys. (They used to be loud errors, which
    // is what made `C:/Users/o@corp/data/w.icechunk` unloadable.)
    (match parseKey "repo@main" with
     | Ok k ->
         check "key: an '@' with no \"<kind>:\" after it is part of the PATH"
             (k.RepoPath = "repo@main" && k.Ref = None) $"%A{k}"
     | Error e -> check "key: an '@' with no \"<kind>:\" after it is part of the PATH" false e)
    (match parseKey "repo@bogus:x" with
     | Ok k ->
         check "key: an unknown kind token leaves the '@' in the path"
             (k.RepoPath = "repo@bogus:x" && k.Ref = None) $"%A{k}"
     | Error e -> check "key: an unknown kind token leaves the '@' in the path" false e)
    // A KNOWN kind token, though, is a refspec attempt: completing it wrongly
    // stays loud, so `repo.checkout("")` cannot degrade into a repo-handle load.
    (let r = parseKey "repo@branch:"
     check "key: an empty ref name is refused loudly" (isError r "ref name after ':' is empty") (errorText r))
    (let r = parseKey "@branch:main"
     check "key: an empty repo path is refused loudly" (isError r "repo path before '@' is empty") (errorText r))
    (let r = parseKey "   "
     check "key: an empty key is refused loudly" (isError r "empty store path") (errorText r))

    // '@' IN A REPO PATH. A corporate Windows profile directory, an
    // email-named share, a credentialed URL: all of them carry an '@' that is
    // not a refspec separator, and all of them used to be refused as
    // "malformed store key" -- the path never reached the repo reader at all.
    (match parseKey "C:\\Users\\o@corp\\data\\w.icechunk" with
     | Ok k ->
         check "key: an '@' inside a directory name is a bare repo path"
             (k.RepoPath = "C:\\Users\\o@corp\\data\\w.icechunk" && k.Ref = None) $"%A{k}"
     | Error e -> check "key: an '@' inside a directory name is a bare repo path" false e)
    // ... and the SAME path still takes a refspec: the split is at the LAST
    // '@', and only that one is examined.
    (match parseKey "C:\\Users\\o@corp\\data\\w.icechunk@branch:main" with
     | Ok k ->
         check "key: an '@'-carrying path still checks a ref out"
             (k.RepoPath = "C:\\Users\\o@corp\\data\\w.icechunk"
              && k.Ref = Some (RefBranch, "main")) $"%A{k}"
     | Error e -> check "key: an '@'-carrying path still checks a ref out" false e)
    check "key: an '@'-carrying checkout key round-trips"
        (roundTrips "C:\\Users\\o@corp\\data\\w.icechunk@branch:main") ""
    check "key: an '@'-carrying bare path round-trips"
        (roundTrips "C:\\Users\\o@corp\\data\\w.icechunk") ""
    check "key: hasRefSuffix separates the two"
        (hasRefSuffix "data/w.icechunk@branch:main"
         && not (hasRefSuffix "C:\\Users\\o@corp\\data\\w.icechunk")
         && not (hasRefSuffix "repo@main")
         && not (hasRefSuffix "data/w.icechunk")) ""

    (let r = checkLocalPath "s3://bucket/weather.icechunk"
     check "key: object-store URLs are refused BY NAME"
         (isError r "object-store URLs" && isError r "s3://") (errorText r))
    check "key: a local path passes the object-store gate"
        (match checkLocalPath "data/weather.icechunk" with Ok () -> true | Error _ -> false) ""
    // A credentialed URL has an '@' too. It must reach the object-store
    // refusal, which says what is actually wrong, instead of dying earlier as
    // a "malformed store key" about a refspec the user never wrote.
    (match parseKey "https://user@host/repo.icechunk" with
     | Ok k ->
         let r = checkLocalPath k.RepoPath
         check "key: a credentialed URL reaches the object-store refusal"
             (k.Ref = None && isError r "object-store URLs" && isError r "https://") (errorText r)
     | Error e -> check "key: a credentialed URL reaches the object-store refusal" false e)

    // ---------------------------------------------------------------
    // 4. Ref resolution (pure, over synthetic repo files)
    // ---------------------------------------------------------------
    printfn "\n--- ref resolution ---"

    let snapOf (b: byte) : SnapshotInfo =
        { Id = Array.create 12 b; ParentOffset = 0; FlushedAtMillis = 0L; Message = "" }
    let snap1 = snapOf 0x11uy
    let snap2 = snapOf 0x22uy

    let info : RepoInfo = {
        Branches = [("main", 0); ("dev", 1)]
        Tags = [("v1.0", 0)]
        DeletedTags = ["old"]
        Snapshots = [snap1; snap2]
        Status = StatusOnline
    }

    check "resolve: branch by marker"
        (resolveRef info RefBranch "main" = Ok snap1.Id) (errorText (resolveRef info RefBranch "main"))
    check "resolve: the second branch resolves to the second snapshot"
        (resolveRef info RefBranch "dev" = Ok snap2.Id) (errorText (resolveRef info RefBranch "dev"))
    check "resolve: tag by marker"
        (resolveRef info RefTag "v1.0" = Ok snap1.Id) (errorText (resolveRef info RefTag "v1.0"))
    check "resolve: bare name unique across namespaces"
        (resolveRef info RefBare "main" = Ok snap1.Id) (errorText (resolveRef info RefBare "main"))
    check "resolve: a bare tag name resolves too (no namespace precedence needed when unique)"
        (resolveRef info RefBare "v1.0" = Ok snap1.Id) (errorText (resolveRef info RefBare "v1.0"))

    // A bare name shaped like an object id is matched against snapshot ids.
    (let sid = base32Encode snap2.Id
     check "resolve: a bare object-id-shaped name resolves as a snapshot"
         (resolveRef info RefBare sid = Ok snap2.Id) (errorText (resolveRef info RefBare sid))
     check "resolve: the same id resolves under the snapshot marker"
         (resolveRef info RefSnapshot sid = Ok snap2.Id) (errorText (resolveRef info RefSnapshot sid)))

    (let r = resolveRef info RefSnapshot "main"
     check "resolve: a non-id name under ic.snapshot is refused by shape"
         (isError r "is not a snapshot id" && isError r "20 Crockford") (errorText r))
    (let r = resolveRef info RefSnapshot (String.replicate 20 "0")
     check "resolve: an id-shaped name that no snapshot carries is refused"
         (isError r "no snapshot") (errorText r))

    // Zero hits: the refusal LISTS the repo's actual branches and tags.
    (let r = resolveRef info RefBare "nope"
     check "resolve: zero hits lists the repo's branches and tags"
         (isError r "no branch, tag or snapshot named 'nope'"
          && isError r "main" && isError r "dev" && isError r "v1.0") (errorText r))
    (let r = resolveRef info RefBranch "nope"
     check "resolve: a missing branch lists the branches"
         (isError r "no branch named 'nope'" && isError r "main, dev") (errorText r))
    (let r = resolveRef info RefTag "nope"
     check "resolve: a missing tag lists the tags"
         (isError r "no tag named 'nope'" && isError r "v1.0") (errorText r))

    // Two hits: name BOTH namespaces, and point at the markers.
    (let ambiguous = { info with Branches = [("shared", 0)]; Tags = [("shared", 1)] }
     let r = resolveRef ambiguous RefBare "shared"
     check "resolve: an ambiguous bare ref refuses, naming both namespaces"
         (isError r "ambiguous" && isError r "branch" && isError r "tag") (errorText r)
     check "resolve: the ambiguity refusal offers the ic.branch / ic.tag markers"
         (isError r "ic.branch" && isError r "ic.tag") (errorText r)
     check "resolve: the marker form disambiguates a colliding name"
         (resolveRef ambiguous RefBranch "shared" = Ok snap1.Id
          && resolveRef ambiguous RefTag "shared" = Ok snap2.Id) "")

    // Deleted tags are tombstones, in both the bare and marker forms.
    (let r = resolveRef info RefBare "old"
     check "resolve: a deleted tag name names its tombstone (bare)"
         (isError r "DELETED TAG" && isError r "deleted_tags") (errorText r))
    (let r = resolveRef info RefTag "old"
     check "resolve: a deleted tag name names its tombstone (ic.tag)"
         (isError r "DELETED TAG") (errorText r))

    // Repo status: Offline refuses; ReadOnly and Online read.
    (let offline = { info with Status = StatusOffline }
     let r = resolveRef offline RefBranch "main"
     check "resolve: an Offline repo refuses every ref"
         (isError r "Offline") (errorText r)
     check "resolve: Offline refuses the bare form too"
         (isError (resolveRef offline RefBare "main") "Offline") "")
    (let ro = { info with Status = StatusReadOnly }
     check "resolve: a ReadOnly repo resolves normally"
         (resolveRef ro RefBranch "main" = Ok snap1.Id) (errorText (resolveRef ro RefBranch "main"))
     check "status names round-trip"
         (List.map statusName [StatusOnline; StatusReadOnly; StatusOffline] = ["Online"; "ReadOnly"; "Offline"]) "")

    // A ref pointing outside the snapshot list is a corrupt repo file, not a miss.
    (let broken = { info with Branches = [("bad", 7)] }
     let r = resolveRef broken RefBranch "bad"
     check "resolve: an out-of-range snapshot index is refused as inconsistent"
         (isError r "inconsistent" && isError r "index 7") (errorText r))

    // ---------------------------------------------------------------
    // 5. The payload codec: zstd + FlatBuffers
    // ---------------------------------------------------------------
    // STALE-PENDING-P1: this section used to pin `decodePayload` and
    // `decompress` as honest STUBS by their `pendingPayloadDecode` /
    // `pendingZstd` messages -- the §6.2 dependency decision was open, and
    // those pins were what P1 would change. §6.2 is DECIDED (ZstdSharp.Port +
    // vendored flatc accessors), so the pins are inverted: what used to be
    // "this seam refuses by name" is now "this seam round-trips", and the
    // only thing still refused is a payload that really is malformed.
    printfn "\n--- payload codec (zstd + FlatBuffers) ---"

    check "codec: compression 0 is the identity (payload passes through unchanged)"
        (decompressPayload CompNone [| 7uy; 8uy; 9uy |] = Ok [| 7uy; 8uy; 9uy |])
        (errorText (decompressPayload CompNone [| 7uy; 8uy; 9uy |]))

    // Round-trip through the WRITER's compressor and the PROVIDER's
    // decompressor: two sides of the §6.2 decision, meeting in the middle.
    (let plain = Array.init 4096 (fun i -> byte ((i * 37) % 251))
     let squeezed = IW.compressZstd plain
     check "codec: zstd compression actually compresses a repetitive payload"
         (squeezed.Length > 0 && squeezed.Length < plain.Length) $"{squeezed.Length} vs {plain.Length}"
     check "codec: the provider decompresses what the writer compressed"
         (decompress squeezed = Ok plain) (errorText (decompress squeezed))
     check "codec: compression 1 routes through the same zstd path"
         (decompressPayload CompZstd squeezed = Ok plain)
         (errorText (decompressPayload CompZstd squeezed)))
    (let empty : byte[] = [||]
     check "codec: an empty payload survives the zstd round trip"
         (decompress (IW.compressZstd empty) = Ok empty)
         (errorText (decompress (IW.compressZstd empty))))

    // Not a zstd frame: a LOUD refusal, never a silent pass-through of the
    // compressed bytes (which would hand the FlatBuffers decoder garbage and
    // fail somewhere far away from the cause).
    check "codec: non-zstd bytes are refused, not passed through"
        (match decompress [| 1uy; 2uy; 3uy; 4uy |] with
         | Error _ -> true
         | Ok bs -> bs <> [| 1uy; 2uy; 3uy; 4uy |]) "compressed bytes passed through unchanged"

    // A FlatBuffer that is not one: refused, and never as a successful decode
    // of an empty table. The refusal must come from the VERIFIER -- a walk of
    // vtables, offset targets, vector extents and required fields BEFORE any
    // accessor dereferences the buffer -- and not from whatever exception an
    // accessor happens to raise while reading an arbitrary offset. Pinning the
    // verifier's own wording is what makes that pass un-deletable: remove the
    // verify call and these go red, even though the decode still fails.
    let decodeErr (ft: FileType) (bs: byte[]) =
        match decodePayload ft bs with
        | Error e -> e
        | Ok _ -> "<decoded>"
    check "codec: garbage payload bytes are refused by the VERIFIER, before any field is read"
        ((decodeErr FtRepoInfo [| 1uy; 2uy; 3uy; 4uy |]).Contains "not a valid Repo FlatBuffer")
        (decodeErr FtRepoInfo [| 1uy; 2uy; 3uy; 4uy |])
    check "codec: a truncated payload is refused as an invalid Snapshot FlatBuffer"
        ((decodeErr FtSnapshot [| 0uy |]).Contains "not a valid Snapshot FlatBuffer")
        (decodeErr FtSnapshot [| 0uy |])
    check "codec: an empty payload is refused as an invalid Manifest FlatBuffer"
        ((decodeErr FtManifest [||]).Contains "not a valid Manifest FlatBuffer")
        (decodeErr FtManifest [||])

    // ---------------------------------------------------------------
    // 6. Node paths and chunk tables (pure)
    // ---------------------------------------------------------------
    printfn "\n--- nodes and chunk tables ---"

    check "node: a root-level array path yields its variable name"
        (nodeVarName "/temp" = Ok "temp") (errorText (nodeVarName "/temp"))
    (let r = nodeVarName "/group/temp"
     check "node: a nested group refuses BY NAME"
         (isError r "NESTED GROUP" && isError r "root-level arrays only") (errorText r))
    (let r = nodeVarName "/"
     check "node: the root group is not an array" (isError r "root group") (errorText r))
    (let r = nodeVarName "temp"
     check "node: a relative path is refused" (isError r "is not absolute") (errorText r))
    (let r = nodeVarName "/2temp"
     check "node: a non-identifier array name is refused"
         (isError r "not a valid Blade identifier") (errorText r))

    // The chunk table unions manifests into row-major grid order; a
    // coordinate no manifest covers reads as fill.
    let v3json = """{"zarr_format": 3, "node_type": "array", "shape": [6, 4], "data_type": "float32",
                     "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [3, 4]}},
                     "fill_value": 0, "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                     "dimension_names": ["t", "p"]}"""
    (match ZarrProvider.parseArrayMetaV3 "temp" "/repo" v3json with
     | Error e -> check "chunks: the v3 user_data JSON parses with the Zarr parser" false e
     | Ok meta ->
         check "chunks: the v3 user_data JSON parses with the Zarr parser" true ""
         let native = Native { ChunkId = [| 0xAAuy; 0xAAuy; 0xAAuy |]; Offset = 0L; Length = 48L }
         let m1 = { NodeId = Array.zeroCreate 8; Refs = [ { Index = [1L; 0L]; Loc = native } ] }
         (match buildChunkTable meta [m1] with
          | Ok table ->
              check "chunks: the table has one entry per chunk-grid cell" (table.Length = 2) $"{table.Length}"
              check "chunks: an uncovered coordinate reads as Fill" (table.[0] = Fill) (sprintf "%A" table.[0])
              check "chunks: a covered coordinate carries its native ref" (table.[1] = native) (sprintf "%A" table.[1])
          | Error e -> check "chunks: a single-manifest table builds" false e)
         // Overlap is a refusal: §2 requires non-overlapping ChunkIndexRanges.
         let m2 = { NodeId = Array.zeroCreate 8; Refs = [ { Index = [1L; 0L]; Loc = Inline [| 1uy |] } ] }
         (let r = buildChunkTable meta [m1; m2]
          check "chunks: overlapping manifests are refused"
              (isError r "OVERLAP") (errorText r))
         (let r = buildChunkTable meta [ { NodeId = Array.zeroCreate 8; Refs = [ { Index = [9L; 0L]; Loc = Fill } ] } ]
          check "chunks: a coordinate outside the grid is refused"
              (isError r "outside the chunk grid") (errorText r))
         (let r = buildChunkTable meta [ { NodeId = Array.zeroCreate 8; Refs = [ { Index = [0L]; Loc = Fill } ] } ]
          check "chunks: a wrong-rank chunk index is refused"
              (isError r "is rank 1" && isError r "rank 2") (errorText r)))

    check "chunks: virtual refs have a NAMED refusal (no ChunkLoc case exists for them)"
        ((virtualChunkRefused "temp" "s3://other/f.nc").Contains "VIRTUAL chunk refs are not supported")
        (virtualChunkRefused "temp" "s3://other/f.nc")

    // A manifest's chunk offset is a uint64 on the wire and an int64 in here,
    // so an offset at or past 2^63 arrives NEGATIVE. The baked table declares
    // presence as `v_icoff[i] >= 0` with -1 as the FILL sentinel, so baking one
    // verbatim would make a corrupt offset read as fill -- silently, printing
    // zeros where real data was asked for. 0xFFFF...FF is the sharp case: it
    // lands exactly ON the sentinel. Driven at the emitter, because the fixture
    // writer computes offsets from the file it is writing and cannot produce
    // one this large.
    (match ZarrProvider.parseArrayMetaV3 "temp" "" v3json with
     | Error e -> check "codegen: a negative baked offset is refused" false e
     | Ok meta ->
         let node : NodeMeta =
             { Id = Array.create 8 0x01uy; Path = "/temp"; Kind = NodeArray
               UserDataJson = v3json; Shape = []; DimensionNames = None; ManifestRefs = [] }
         let resolved (off: int64) : ResolvedArray =
             { Root = "repo"; Ref = (RefBranch, "main"); SnapshotId = Array.create 12 0x02uy
               VarName = "temp"; Node = node; Meta = meta
               Table = [| Fill
                          Native { ChunkId = Array.create 12 0xABuy; Offset = off; Length = 48L } |] }
         // 48 = a padded chunk of this array (3 x 4 float32), so the LENGTH
         // check cannot be what fires.
         let emit (off: int64) = caught (fun () -> CppIcechunk.icechunkChunkFetch (resolved off) "v" 48)
         check "codegen: an offset that wrapped to -1 (the fill sentinel) is refused"
             ((emit (-1L)).Contains "corrupt manifest: chunk offset out of range") (emit (-1L))
         check "codegen: an offset of exactly 2^63 is refused"
             ((emit Int64.MinValue).Contains "chunk offset out of range") (emit Int64.MinValue)
         check "codegen: a legal offset still emits (the guard does not over-fire)"
             ((emit 4096L) = "<no exception>") (emit 4096L))

    // ---------------------------------------------------------------
    // 7. Repo-file gates (hand-assembled headers -- no real repo)
    // ---------------------------------------------------------------
    printfn "\n--- repo file gates ---"

    let tmp = Path.Combine(Path.GetTempPath(), "blade_ic_" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory tmp |> ignore

    /// A repo directory whose `repo` file carries `header` and a nonsense payload.
    let fakeRepo (name: string) (header: byte[]) : string =
        let root = Path.Combine(tmp, name)
        Directory.CreateDirectory root |> ignore
        File.WriteAllBytes(Path.Combine(root, "repo"), Array.append header [| 0uy; 1uy; 2uy; 3uy |])
        root

    let junkRepo = fakeRepo "junk.icechunk" (mkHeader "icechunk-rust 1.2.3" 2uy 6uy 0uy)
    let junkZstdRepo = fakeRepo "junkzstd.icechunk" (mkHeader "icechunk-rust 1.2.3" 2uy 6uy 1uy)
    let spec1Repo = fakeRepo "spec1.icechunk" (mkHeader "icechunk-rust 0.1.0" 1uy 6uy 0uy)
    let wrongTypeRepo = fakeRepo "wrongtype.icechunk" (mkHeader "icechunk-rust" 2uy 1uy 0uy)
    let badCompRepo = fakeRepo "badcomp.icechunk" (mkHeader "icechunk-rust" 2uy 6uy 9uy)
    let emptyDir = Path.Combine(tmp, "empty.icechunk")
    Directory.CreateDirectory emptyDir |> ignore

    check "repo: a well-formed repo header validates (header level, no payload)"
        (match validateRepoFile junkRepo with Ok _ -> true | Error _ -> false)
        (errorText (validateRepoFile junkRepo))
    (let r = validateRepoFile (Path.Combine(tmp, "nope.icechunk"))
     check "repo: a missing repo directory refuses by name" (isError r "does not exist") (errorText r))
    (let r = validateRepoFile emptyDir
     check "repo: a directory with no 'repo' file is not an Icechunk repo"
         (isError r "is not an Icechunk repo") (errorText r))
    (let r = validateRepoFile spec1Repo
     check "repo: a spec-1 repo file refuses at the load site" (isError r "spec version 1") (errorText r))
    (let r = validateRepoFile wrongTypeRepo
     check "repo: a repo file stamped Snapshot refuses as the wrong file type"
         (isError r "expected RepoInfo") (errorText r))
    (let r = validateRepoFile badCompRepo
     check "repo: an unknown compression byte refuses at the repo file"
         (isError r "unknown compression byte 9") (errorText r))
    (let r = validateRepoFile "s3://bucket/repo.icechunk"
     check "repo: an object-store URL refuses before any file IO"
         (isError r "object-store URLs") (errorText r))

    // STALE-PENDING-P1: these three used to assert that `load` "reaches the
    // payload seam" -- i.e. that the pending §6.2 message was the error a
    // structurally fine repo produced. With the decoder live, the SAME
    // fixtures (a valid header over four junk bytes) must now fail as what
    // they are: an undecodable payload. What is pinned is that the failure is
    // loud and is NOT a pending-dependency message.
    (let r = load junkRepo
     check "load: a valid header over a junk payload fails as a DECODE error"
         (match r with Error e -> not (e.Contains "pending") | Ok _ -> false) (errorText r))
    (let r = load junkZstdRepo
     check "load: a junk zstd payload fails as a DECOMPRESSION error, not a stub"
         (match r with Error e -> not (e.Contains "pending") | Ok _ -> false) (errorText r))
    (let r = load (junkRepo + "@branch:main")
     check "load: a canonical key over a junk payload fails the same way"
         (match r with Error e -> not (e.Contains "pending") | Ok _ -> false) (errorText r))
    // NOTE: an UNKNOWN kind token (e.g. "@bogus:main") is deliberately NOT a
    // malformed key -- refSuffixOf (IcechunkProvider.fs) treats it as literal
    // repo-PATH text (see "key: 'repo@bogus:x' is not a refspec" below and the
    // '@'-in-a-directory-name coverage in ProviderDesugarTests.fs). A malformed
    // key is a RECOGNIZED kind with an empty repo path or empty ref name.
    (let r = load (junkRepo + "@branch:")
     check "load: a malformed key refuses before any file IO"
         (isError r "the ref name after ':' is empty") (errorText r))

    // STALE-PENDING-P1: `loadAsModule` on a checkout key used to be pinned by
    // the pending payload message; the live behavior it must have is section
    // 10's (a real checkout builds a dims/vars module). What survives here is
    // the pre-payload half: a spec-1 repo and a malformed key still die at
    // the load site with their own names.
    (let msg = caught (fun () -> loadAsModule (IRBuilder()) "wx" spec1Repo)
     check "module: a spec-1 repo fails at the load site, by name"
         (msg.Contains "spec version 1") msg)
    (let msg = caught (fun () -> loadAsModule (IRBuilder()) "ck" (junkRepo + "@branch:"))
     check "module: a malformed key fails loudly" (msg.Contains "malformed store key") msg)
    (let msg = caught (fun () -> loadAsModule (IRBuilder()) "ck" (junkRepo + "@tag:v1.0"))
     check "module: a checkout of an undecodable repo fails loudly, not with a stub message"
         (msg <> "<no exception>" && not (msg.Contains "pending")) msg)

    // Version stamp: mtime of the ONE mutable file, refspec-independent.
    check "stamp: versionStamp reads the repo file's mtime" (versionStamp junkRepo > 0L) $"{versionStamp junkRepo}"
    check "stamp: a refspec does not change the stamp (same repo file)"
        (versionStamp junkRepo = versionStamp (junkRepo + "@branch:main")) ""
    check "stamp: an unreadable repo stamps 0 rather than throwing"
        (versionStamp (Path.Combine(tmp, "nope.icechunk")) = 0L) ""

    check "fingerprint: 64 hex characters" ((fingerprint junkRepo).Length = 64) (fingerprint junkRepo)
    check "fingerprint: stable across calls" (fingerprint junkRepo = fingerprint junkRepo) ""
    check "fingerprint: distinguishes two checkouts of one repo"
        (fingerprint (junkRepo + "@branch:main") <> fingerprint (junkRepo + "@tag:v1.0")) ""
    check "fingerprint: never throws on a missing repo"
        ((fingerprint (Path.Combine(tmp, "nope.icechunk"))).Length = 64) ""

    // ---------------------------------------------------------------
    // 8. The ProviderSpec (registration surface + refusals)
    // ---------------------------------------------------------------
    printfn "\n--- provider spec ---"

    check "spec: registers under the surface module name 'icechunk'" (spec.Name = "icechunk") spec.Name
    check "spec: LinkNeeds stays 'none (pure std C++17)'"
        (spec.LinkNeeds = "none (pure std C++17)") spec.LinkNeeds
    check "spec: includes are std-only" (spec.Includes () |> List.contains "#include <fstream>") ""
    check "spec: load_compound is refused (GenReadCompoundVar = None)" spec.GenReadCompoundVar.IsNone ""
    check "spec: .stream is refused (GenStreamOpen/Fiber = None)"
        (spec.GenStreamOpen.IsNone && spec.GenStreamFiber.IsNone) ""
    check "spec: packed and wreath capabilities are declared (§8)"
        (spec.GenReadPacked.IsSome && spec.ReadWreathPool.IsSome) ""

    let arrType =
        let b = IRBuilder()
        { ElemType = IRTScalar ETFloat64
          IndexTypes = [ { Id = b.FreshId(); Rank = 1; Extent = IRLit (IRLitInt 4L); Symmetry = SymNone
                           Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] } ]
          IsVirtual = false; Identity = Some (AIDVariable "A") }

    (let msg = caught (fun () -> spec.GenWriteVar junkRepo "temp" "temp" arrType ["t"])
     check "spec: a write refuses loudly, citing writes-as-commits"
         (msg.Contains "COMMIT" && msg.Contains "conditional swap") msg)
    (let msg = caught (fun () -> spec.GenReadVar (Path.Combine(tmp, "nope.icechunk")) "temp" "temp" arrType)
     check "spec: a read of a missing repo names the REPO, not some inner seam"
         (msg.Contains "does not exist") msg)

    // STALE-PENDING-P1: these two used to pin "stops at the payload seam".
    // The live contract is that an UNDECODABLE repo still refuses loudly (and
    // real reads are section 10's business).
    (let msg = caught (fun () -> spec.GenReadVar junkRepo "temp" "temp" arrType)
     check "spec: a read of an undecodable repo refuses loudly, without a stub message"
         (msg <> "<no exception>" && not (msg.Contains "pending")) msg)
    (let r = spec.ReadVarData junkRepo "temp"
     check "spec: the compile-time fold refuses an undecodable repo loudly"
         (match r with Error e -> not (e.Contains "pending") | Ok _ -> false) (errorText r))

    (let r = spec.ReadVarData (Path.Combine(tmp, "nope.icechunk")) "temp"
     check "spec: the compile-time fold reports the missing repo"
         (isError r "does not exist") (errorText r))
    check "spec: VarDimNames is None-safe on an unreadable store"
        ((spec.VarDimNames (Path.Combine(tmp, "nope.icechunk")) "temp").IsNone) ""

    (try Directory.Delete(tmp, true) with _ -> ())

    // ---------------------------------------------------------------
    // 9. The fixture writer, on its own terms (no provider in the loop)
    // ---------------------------------------------------------------
    // Everything here is decidable from the DIRECTORY: which files exist,
    // what their names look like, what their first 39 bytes say, and how the
    // file set changes when the spec changes. The provider is deliberately
    // not consulted -- if both sides were wrong in the same way, section 10
    // would still pass and this section would not.
    printfn "\n--- fixture writer ---"

    // 5x4 chunked 3x2: a 2x2 chunk grid with edge overhang on BOTH axes
    // (rows 3-4 against a 3-row chunk, column 3 against a 2-column chunk).
    let tempV1 = Array.init 20 (fun i -> float (i + 1))            // 1 .. 20, sum 210
    let tempV2 = Array.init 20 (fun i -> float (i + 1) * 10.0)     // 10 .. 200, sum 2100
    let latData = [| 0.0; 1.0; 2.0; 3.0; 4.0 |]
    let lonData = [| 10.0; 11.0; 12.0; 13.0 |]

    /// The workhorse fixture: two commits over a fixed grid. `lat` and `lon`
    /// are byte-identical in both snapshots (small enough to ride INLINE),
    /// `temp` changes (and is forced NATIVE) -- which is precisely the
    /// "coordinate arrays untouched, data array rewritten" shape plan §5
    /// needs, and it falls out of content-derived ids rather than being
    /// staged by hand.
    let wxSpec (compress: bool) (snapshots: string list) : IW.RepoSpec =
        let coords = [
            IW.mkArray "lat" ["lat"] [5L] [5L] (IW.IceF64 latData)
            IW.mkArray "lon" ["lon"] [4L] [4L] (IW.IceF64 lonData) ]
        let tempOf (d: float[]) =
            { IW.mkArray "temp" ["lat"; "lon"] [5L; 4L] [3L; 2L] (IW.IceF64 d) with
                InlineThreshold = 0 }
        let snapOfName (n: string) =
            match n with
            | "s1" -> IW.mkSnapshot "s1" (coords @ [ tempOf tempV1 ])
            | "s2" -> IW.mkSnapshot "s2" (coords @ [ tempOf tempV2 ])
            | other -> failwithf "unknown fixture snapshot '%s'" other
        { IW.emptyRepo with
            Compress = compress
            Seed = 7
            Snapshots = snapshots |> List.map snapOfName
            Branches = (if List.contains "s2" snapshots then [ ("main", "s2") ] else [ ("main", "s1") ])
            Tags = [ ("v1.0", "s1") ]
            DeletedTags = [ "old" ] }

    let wxFull = wxSpec true ["s1"; "s2"]
    let wxRoot = fixRepo "ic_wx"
    let wxPlainRoot = fixRepo "ic_wx_plain"      // same repo, compression byte 0
    let wxOneRoot = fixRepo "ic_wx_one"          // s1 only: the id-reuse control

    let filesIn (dir: string) : string list =
        if Directory.Exists dir then
            Directory.GetFiles dir
            |> Array.map (fun (f: string) -> Path.GetFileName f)
            |> Array.sort
            |> List.ofArray
        else []

    (try
        IW.writeRepo wxRoot wxFull
        IW.writeRepo wxPlainRoot (wxSpec false ["s1"; "s2"])
        IW.writeRepo wxOneRoot (wxSpec true ["s1"])

        check "writer: the repo file exists" (File.Exists (Path.Combine(wxRoot, "repo"))) wxRoot
        check "writer: snapshots/ manifests/ chunks/ all exist"
            ([ "snapshots"; "manifests"; "chunks" ]
             |> List.forall (fun d -> Directory.Exists (Path.Combine(wxRoot, d)))) ""
        check "writer: transactions/ and overwritten/ exist but are empty (tx logs are PRUNABLE)"
            (Directory.Exists (Path.Combine(wxRoot, "transactions"))
             && List.isEmpty (filesIn (Path.Combine(wxRoot, "transactions")))) ""

        let snapFiles = filesIn (Path.Combine(wxRoot, "snapshots"))
        let manFiles = filesIn (Path.Combine(wxRoot, "manifests"))
        let chunkFiles = filesIn (Path.Combine(wxRoot, "chunks"))

        check "writer: two commits produce two snapshot files" (snapFiles.Length = 2) $"%A{snapFiles}"
        // lat and lon are identical in both commits, so their manifests are
        // written ONCE; temp differs, so it gets one manifest per commit.
        check "writer: manifests = lat + lon + temp@s1 + temp@s2 = 4 (coords shared)"
            (manFiles.Length = 4) $"%A{manFiles}"
        // lat (40 B) and lon (32 B) are under the 512-byte inline threshold and
        // have NO chunk files at all; temp's four 48-byte chunks are native, per commit.
        check "writer: chunks = 4 native temp chunks per commit = 8 (coords are inline)"
            (chunkFiles.Length = 8) $"%A{chunkFiles}"
        check "writer: every native chunk file is FULL SIZE (3*2 float64 = 48 B, edges padded)"
            (chunkFiles |> List.forall (fun f ->
                FileInfo(Path.Combine(wxRoot, "chunks", f)).Length = 48L))
            (chunkFiles |> List.map (fun f -> string (FileInfo(Path.Combine(wxRoot, "chunks", f)).Length))
                        |> String.concat ", ")

        check "writer: every object file name is 20 canonical Crockford characters"
            (snapFiles @ manFiles @ chunkFiles |> List.forall isObjectIdForm)
            (String.concat ", " (snapFiles @ manFiles @ chunkFiles))
        check "writer: the snapshot files are named by the ids `snapshotId` predicts"
            (List.sort [ IW.snapshotId wxFull "s1"; IW.snapshotId wxFull "s2" ] = snapFiles)
            $"%A{snapFiles}"

        // Header bytes, read straight off disk -- no provider parser involved.
        let head (f: string) = Array.sub (File.ReadAllBytes f) 0 39
        let repoHead = head (Path.Combine(wxRoot, "repo"))
        check "writer: the repo file's first 12 bytes are the magic"
            (Array.sub repoHead 0 12 = goldenMagic)
            (Array.sub repoHead 0 12 |> Array.map (sprintf "%02x") |> String.concat " ")
        check "writer: bytes 12..35 are the space-padded implementation name"
            (Text.Encoding.UTF8.GetString(repoHead, 12, 24)
              = "blade-fixtures" + String.replicate (24 - 14) " ")
            ("[" + Text.Encoding.UTF8.GetString(repoHead, 12, 24) + "]")
        check "writer: byte 36 is spec version 2" (repoHead.[36] = 2uy) $"{repoHead.[36]}"
        check "writer: byte 37 is file type 6 (RepoInfo)" (repoHead.[37] = 6uy) $"{repoHead.[37]}"
        check "writer: byte 38 is compression 1 (zstd) by default"
            (repoHead.[38] = 1uy) $"{repoHead.[38]}"
        check "writer: Compress = false stamps compression byte 0"
            ((head (Path.Combine(wxPlainRoot, "repo"))).[38] = 0uy) ""
        check "writer: snapshot files are stamped file type 1"
            (snapFiles |> List.forall (fun f -> (head (Path.Combine(wxRoot, "snapshots", f))).[37] = 1uy)) ""
        check "writer: manifest files are stamped file type 2"
            (manFiles |> List.forall (fun f -> (head (Path.Combine(wxRoot, "manifests", f))).[37] = 2uy)) ""
        check "writer: chunk files are RAW -- no 39-byte header, no magic"
            (chunkFiles |> List.forall (fun f ->
                let b = File.ReadAllBytes (Path.Combine(wxRoot, "chunks", f))
                b.Length = 48 && Array.sub b 0 3 <> [| 0x49uy; 0x43uy; 0x45uy |])) ""

        // Determinism: the same spec, written twice, is the same repo.
        let twinRoot = fixRepo "ic_wx_twin"
        IW.writeRepo twinRoot wxFull
        check "writer: the same spec written twice yields identical `repo` bytes"
            (File.ReadAllBytes (Path.Combine(twinRoot, "repo"))
              = File.ReadAllBytes (Path.Combine(wxRoot, "repo"))) ""
        check "writer: the same spec written twice yields the same object file names"
            (filesIn (Path.Combine(twinRoot, "chunks")) = chunkFiles
             && filesIn (Path.Combine(twinRoot, "manifests")) = manFiles) ""
        // Compression is a framing choice, not an identity one.
        check "writer: compression changes the file BYTES, not the object ids"
            (filesIn (Path.Combine(wxPlainRoot, "chunks")) = chunkFiles
             && filesIn (Path.Combine(wxPlainRoot, "manifests")) = manFiles
             && File.ReadAllBytes (Path.Combine(wxPlainRoot, "repo"))
                <> File.ReadAllBytes (Path.Combine(wxRoot, "repo"))) ""
        (try Directory.Delete(twinRoot, true) with _ -> ())

        // ID STABILITY -- the anchor of plan §5.2.
        check "writer: a node id depends on the array NAME, not on the snapshot"
            (IW.nodeId wxFull "temp" = IW.nodeId (wxSpec true ["s1"]) "temp"
             && IW.nodeId wxFull "lat" <> IW.nodeId wxFull "temp") (IW.nodeId wxFull "temp")
        check "writer: a node id is 13 Crockford characters"
            ((IW.nodeId wxFull "temp").Length = nodeIdChars
             && (IW.nodeId wxFull "temp") |> Seq.forall (fun c -> base32Alphabet.IndexOf c >= 0))
            (IW.nodeId wxFull "temp")
        check "writer: a different seed shares no ids"
            (IW.nodeId wxFull "temp" <> IW.nodeId { wxFull with Seed = 8 } "temp") ""

        // The one-snapshot control: every manifest and chunk file the s1-only
        // repo needs is REUSED verbatim by the two-snapshot repo. That is what
        // "the coordinate arrays did not change" means on disk.
        let oneMan = filesIn (Path.Combine(wxOneRoot, "manifests"))
        let oneChunk = filesIn (Path.Combine(wxOneRoot, "chunks"))
        check "writer: an s1-only repo has 3 manifests and 4 chunks"
            (oneMan.Length = 3 && oneChunk.Length = 4) $"%A{oneMan} / %A{oneChunk}"
        check "writer: adding a commit REUSES every untouched array's manifest and chunk files"
            (oneMan |> List.forall (fun f -> List.contains f manFiles)
             && oneChunk |> List.forall (fun f -> List.contains f chunkFiles))
            $"one=%A{oneMan @ oneChunk}"
        check "writer: the second commit adds exactly temp's new manifest and 4 new chunks"
            (manFiles.Length - oneMan.Length = 1 && chunkFiles.Length - oneChunk.Length = 4) ""
     with ex -> check "writer: fixture repos build" false ex.Message)

    // Placement policies, each in its own array of one repo, so a single
    // read loop in section 10 covers all four ChunkRef shapes.
    let polRoot = fixRepo "ic_policies"
    let polData (bias: float) = Array.init 20 (fun i -> bias + float (i + 1))
    let polSpec : IW.RepoSpec =
        let baseArr name d =
            IW.mkArray name ["r"; "c"] [5L; 4L] [3L; 2L] (IW.IceF64 d)
        { IW.emptyRepo with
            Seed = 11
            Snapshots = [
                IW.mkSnapshot "only" [
                    // Under the 512-byte default threshold: rides in the manifest.
                    baseArr "inl" (polData 0.0)
                    // Threshold 0: one chunk file per chunk, offset 0.
                    { baseArr "nat" (polData 100.0) with InlineThreshold = 0 }
                    // One file for the whole array: the refs carry NONZERO offsets.
                    { baseArr "pk" (polData 200.0) with InlineThreshold = 0; PackNativeChunks = true }
                    // Two manifests over disjoint ChunkIndexRanges on axis 0.
                    { baseArr "sp" (polData 300.0) with InlineThreshold = 0; SplitManifests = true }
                    // Chunk (1,1) has NO ref at all: it must read as fill_value.
                    { baseArr "fl" (polData 400.0) with
                        Fill = IW.IceFillFloat(-1.0)
                        OmitChunks = [ [1L; 1L] ] } ] ]
            Branches = [ ("main", "only") ] }

    (try
        IW.writeRepo polRoot polSpec
        let polChunks = filesIn (Path.Combine(polRoot, "chunks"))
        let polMans = filesIn (Path.Combine(polRoot, "manifests"))
        // nat: 4 files. pk: 1 file. sp: 4 files. inl and fl: inline, 0 files.
        check "writer: placement policies produce 4 + 1 + 4 = 9 chunk files"
            (polChunks.Length = 9) $"%A{polChunks}"
        check "writer: the packed array's single chunk file holds all four chunks (4*48 B)"
            (polChunks |> List.exists (fun f ->
                FileInfo(Path.Combine(polRoot, "chunks", f)).Length = 192L)) ""
        // inl(1) + nat(1) + pk(1) + sp(2) + fl(1) = 6
        check "writer: SplitManifests gives its array TWO manifests (6 in total)"
            (polMans.Length = 6) $"%A{polMans}"
     with ex -> check "writer: placement-policy fixture builds" false ex.Message)

    // Repos whose whole point is a refusal.
    let ambRoot = fixRepo "ic_ambiguous"
    let offlineRoot = fixRepo "ic_offline"
    let roRoot = fixRepo "ic_readonly"
    let spec1Root = fixRepo "ic_spec1"
    let virtRoot = fixRepo "ic_virtual"
    let packedRoot = fixRepo "ic_packed"

    /// A one-array, one-commit repo -- the base every refusal fixture varies.
    let tinySpec (seed: int) (arrays: IW.ArraySpec list) (branches: (string * string) list) : IW.RepoSpec =
        { IW.emptyRepo with
            Seed = seed
            Snapshots = [ IW.mkSnapshot "only" arrays ]
            Branches = branches }

    let tinyArr = IW.mkArray "a" ["r"] [4L] [2L] (IW.IceF64 [| 1.0; 2.0; 3.0; 4.0 |])

    (try
        // A branch AND a tag both named "x": the ambiguity the unit markers exist for.
        IW.writeRepo ambRoot
            { tinySpec 21 [ tinyArr ] [ ("x", "only") ] with Tags = [ ("x", "only") ] }
        IW.writeRepo offlineRoot
            { tinySpec 22 [ tinyArr ] [ ("main", "only") ] with
                Availability = generated.RepoAvailability.Offline
                AvailabilityReason = Some "fixture: marked unavailable" }
        IW.writeRepo roRoot
            { tinySpec 23 [ tinyArr ] [ ("main", "only") ] with
                Availability = generated.RepoAvailability.ReadOnly }
        IW.writeRepo spec1Root { tinySpec 24 [ tinyArr ] [ ("main", "only") ] with SpecByte = 1uy }
        IW.writeRepo virtRoot
            (tinySpec 25
                [ { IW.mkArray "v" ["r"] [4L] [2L] (IW.IceF64 [| 1.0; 2.0; 3.0; 4.0 |]) with
                      InlineThreshold = 0
                      VirtualChunk = Some ([ 1L ], "s3://elsewhere/bucket/chunk.bin") } ]
                [ ("main", "only") ])
        // A depth-1 SymIdx<2,4> pool: cardinality C(5,2) = 10, one flat axis.
        IW.writeRepo packedRoot
            (tinySpec 26
                [ { IW.mkArray "tri" [] [10L] [10L] (IW.IceF64 (Array.init 10 (fun i -> float i))) with
                      AttributesJson =
                        Some "\"blade\": {\"spec_version\": 1, \"layout\": \"packed\", \"order\": \"ascending-lex\", \"index_types\": [{\"kind\": \"sym\", \"rank\": 2, \"extent\": 4}], \"decomposition\": {\"scheme\": \"flat-ranges\"}}" } ]
                [ ("main", "only") ])
        check "writer: the refusal fixtures build" true ""
        check "writer: a SpecByte=1 fixture really carries spec byte 1"
            ((Array.sub (File.ReadAllBytes (Path.Combine(spec1Root, "repo"))) 0 39).[36] = 1uy) ""
        check "writer: the virtual-ref array writes NO chunk file for the virtual coordinate"
            (filesIn (Path.Combine(virtRoot, "chunks")) |> List.length = 1) ""
        // The writer rejects nonsense before it can produce a half-written repo.
        check "writer: a branch pointing at an undefined snapshot is refused"
            ((caught (fun () -> IW.writeRepo (fixRepo "ic_bad") (tinySpec 27 [ tinyArr ] [ ("main", "nope") ])))
                .Contains "which the spec does not define") ""
        check "writer: a data/shape mismatch is refused"
            ((caught (fun () ->
                IW.writeRepo (fixRepo "ic_bad")
                    (tinySpec 28 [ IW.mkArray "a" ["r"] [4L] [2L] (IW.IceF64 [| 1.0 |]) ] [ ("main", "only") ])))
                .Contains "cells but shape") ""
     with ex -> check "writer: refusal fixtures build" false ex.Message)

    // ---------------------------------------------------------------
    // 10. The provider, against REAL repos (the P1 contract)
    // ---------------------------------------------------------------
    // Everything below drives IcechunkProvider's PUBLIC surface over the
    // repos section 9 wrote. These are the behaviors the payload decoder has
    // to deliver -- they are written from the plan and the schemas, not from
    // any particular implementation of them.
    printfn "\n--- provider over real repos ---"

    let wxFullSpec = wxSpec true ["s1"; "s2"]
    let mainKey = wxRoot + "@branch:main"      // -> s2 (tempV2)
    let tagKey = wxRoot + "@tag:v1.0"          // -> s1 (tempV1)
    let bareKey = wxRoot + "@?:main"

    (try
        match load wxRoot with
        | Error e -> check "real: a bare path loads as a repo handle" false e
        | Ok (LoadedCheckout _) ->
            check "real: a bare path loads as a repo handle" false "got a checkout"
        | Ok (LoadedRepo handle) ->
            check "real: a bare path loads as a repo handle" true ""
            check "real: the repo file's branches come back"
                (handle.Info.Branches |> List.map fst = ["main"]) $"%A{handle.Info.Branches}"
            check "real: the repo file's tags come back"
                (handle.Info.Tags |> List.map fst = ["v1.0"]) $"%A{handle.Info.Tags}"
            check "real: deleted_tags tombstones come back"
                (handle.Info.DeletedTags = ["old"]) $"%A{handle.Info.DeletedTags}"
            check "real: both snapshots are listed" (handle.Info.Snapshots.Length = 2) $"{handle.Info.Snapshots.Length}"
            check "real: the status is Online" (handle.Info.Status = StatusOnline) (statusName handle.Info.Status)
            check "real: the header names the fixture writer"
                (handle.Header.Implementation = "blade-fixtures" && handle.Header.SpecVersion = 2)
                handle.Header.Implementation
            // Ref resolution against the REAL repo file, cross-checked
            // against the id the writer says it minted.
            check "real: branch 'main' resolves to snapshot s2"
                (resolveRef handle.Info RefBranch "main" = Ok (IW.snapshotIdBytes wxFullSpec "s2"))
                (errorText (resolveRef handle.Info RefBranch "main"))
            check "real: tag 'v1.0' resolves to snapshot s1"
                (resolveRef handle.Info RefTag "v1.0" = Ok (IW.snapshotIdBytes wxFullSpec "s1"))
                (errorText (resolveRef handle.Info RefTag "v1.0"))
            check "real: the bare form resolves 'main' uniquely"
                (resolveRef handle.Info RefBare "main" = Ok (IW.snapshotIdBytes wxFullSpec "s2")) ""
            check "real: a snapshot id resolves under ic.snapshot"
                (resolveRef handle.Info RefSnapshot (IW.snapshotId wxFullSpec "s1")
                  = Ok (IW.snapshotIdBytes wxFullSpec "s1"))
                (errorText (resolveRef handle.Info RefSnapshot (IW.snapshotId wxFullSpec "s1")))
            check "real: a deleted tag names its tombstone"
                (isError (resolveRef handle.Info RefBare "old") "DELETED TAG")
                (errorText (resolveRef handle.Info RefBare "old"))
     with ex -> check "real: bare repo-handle load" false ex.Message)

    (try
        match load mainKey with
        | Error e -> check "real: a branch key loads as a checkout" false e
        | Ok (LoadedRepo _) -> check "real: a branch key loads as a checkout" false "got a repo handle"
        | Ok (LoadedCheckout ck) ->
            check "real: a branch key loads as a checkout" true ""
            check "real: the checkout carries the resolved snapshot id"
                (ck.SnapshotId = IW.snapshotIdBytes wxFullSpec "s2") (base32Encode ck.SnapshotId)
            check "real: the checkout's root-level arrays are lat, lon, temp"
                (List.sort (arrayNames ck) = ["lat"; "lon"; "temp"]) $"%A{arrayNames ck}"
            // The snapshot really does carry a root GROUP node -- the decoder
            // has to read the NodeData union's tag, not assume everything is
            // an array -- and that node is NOT offered as a variable.
            check "real: the root '/' node decodes as a GROUP and is not a variable"
                ((ck.Snapshot.Nodes |> List.exists (fun n -> n.Path = "/" && n.Kind = NodeGroup))
                 && (arrayNames ck).Length = 3)
                (sprintf "%A" (ck.Snapshot.Nodes |> List.map (fun n -> (n.Path, n.Kind))))
            (match findArray ck "temp" with
             | Error e -> check "real: temp's zarr.json parses with the Zarr v3 parser" false e
             | Ok (node, meta) ->
                 check "real: temp's zarr.json parses with the Zarr v3 parser" true ""
                 check "real: shape and chunks survive the round trip"
                     (meta.Shape = [5L; 4L] && meta.Chunks = [3L; 2L]) $"%A{meta.Shape} / %A{meta.Chunks}"
                 check "real: dimension names survive the round trip"
                     (meta.DimNames = Some ["lat"; "lon"]) $"%A{meta.DimNames}"
                 check "real: the element type is float64" (meta.Dtype.Elem = ETFloat64) meta.Dtype.Code
                 check "real: the node id is the one the writer minted"
                     (node.Id = IW.nodeIdBytes wxFullSpec "temp") (base32Encode node.Id)
                 check "real: the structural dimension names agree with the JSON"
                     (node.DimensionNames = Some ["lat"; "lon"]) $"%A{node.DimensionNames}"
                 check "real: the array points at exactly one manifest"
                     (node.ManifestRefs.Length = 1) $"{node.ManifestRefs.Length}")
            // Node ids are STABLE across snapshots, and the CHUNK-REF TABLE is
            // what decides whether two checkouts present the same axis
            // (plan §5.2): unchanged coordinate arrays keep their node id,
            // their user_data bytes AND their manifest ids, while the array
            // that was rewritten points somewhere else. All three are
            // decidable from metadata alone -- no chunk is read here.
            (match load tagKey with
             | Ok (LoadedCheckout ck1) ->
                 let nodeOf (c: CheckoutHandle) (n: string) =
                     match findArray c n with
                     | Ok (node, _) -> Some node
                     | Error _ -> None
                 let idOf c n = nodeOf c n |> Option.map (fun x -> base32Encode x.Id)
                 let jsonOf c n = nodeOf c n |> Option.map (fun x -> x.UserDataJson)
                 let manOf c n =
                     nodeOf c n |> Option.map (fun x -> x.ManifestRefs |> List.map (fst >> base32Encode))
                 check "real: an array keeps its node id across snapshots"
                     (idOf ck1 "temp" = idOf ck "temp" && idOf ck1 "lat" = idOf ck "lat")
                     (sprintf "%A vs %A" (idOf ck1 "temp") (idOf ck "temp"))
                 check "real: two different arrays have different node ids"
                     (idOf ck "lat" <> idOf ck "temp" && (idOf ck "lat").IsSome) ""
                 check "real: an untouched coordinate array keeps its user_data bytewise"
                     (jsonOf ck1 "lat" = jsonOf ck "lat" && (jsonOf ck "lat").IsSome) ""
                 check "real: an untouched coordinate array keeps its manifest ids"
                     (manOf ck1 "lat" = manOf ck "lat" && (manOf ck "lat").IsSome)
                     (sprintf "%A vs %A" (manOf ck1 "lat") (manOf ck "lat"))
                 check "real: the REWRITTEN array's manifest id DIFFERS between snapshots"
                     (manOf ck1 "temp" <> manOf ck "temp" && (manOf ck "temp").IsSome)
                     (sprintf "%A vs %A" (manOf ck1 "temp") (manOf ck "temp"))
             | Ok (LoadedRepo _) -> check "real: the tag key loads as a checkout" false "got a repo handle"
             | Error e -> check "real: the tag key loads as a checkout" false e)
            // A name that is not there names what IS there.
            (let r = findArray ck "nope"
             check "real: a missing variable lists the checkout's root-level arrays"
                 (isError r "not found" && isError r "temp") (errorText r))
     with ex -> check "real: branch checkout" false ex.Message)

    // The checkout module: dims and vars structs, built through the Zarr
    // provider's own module builder over verbatim zarr.json.
    (try
        let m = loadAsModule (IRBuilder()) "ck" mainKey
        let indexDefs = m.Types |> List.choose (function IRTDIndexType (n, it) -> Some (n, it) | _ -> None)
        let dimsFields =
            m.Types |> List.tryPick (function IRTDStruct ("ck__dims", fs) -> Some fs | _ -> None)
            |> Option.defaultValue []
        let varsFields =
            m.Types |> List.tryPick (function IRTDStruct ("ck__vars", fs) -> Some fs | _ -> None)
            |> Option.defaultValue []
        check "module: a checkout key builds named index types lat and lon"
            (indexDefs |> List.map fst |> List.sort = ["lat"; "lon"]) $"%A{List.map fst indexDefs}"
        check "module: dims carries lat and lon"
            (dimsFields |> List.map fst |> List.sort = ["lat"; "lon"]) $"%A{List.map fst dimsFields}"
        check "module: vars carries temp only (lat/lon are coordinate arrays)"
            (varsFields |> List.map fst = ["temp"]) $"%A{List.map fst varsFields}"
        let tempIds =
            varsFields |> List.tryPick (fun (n, t) ->
                if n <> "temp" then None else
                match t with
                | ArrayElem at -> Some (at.IndexTypes |> List.map (_.Id))
                | _ -> None)
        let latId = indexDefs |> List.tryPick (fun (n, it) -> if n = "lat" then Some it.Id else None)
        let sharesLat =
            match tempIds, latId with
            | Some (t0 :: _), Some l -> t0 = l
            | _ -> false
        check "module: temp's first index IS the shared lat index type (same Id)"
            sharesLat (sprintf "temp ids %A, lat id %A" tempIds latId)
     with ex -> check "module: checkout builds a dims/vars module" false ex.Message)

    (try
        let m = loadAsModule (IRBuilder()) "wx" wxRoot
        check "module: a bare path binds an EMPTY repo-handle module (no dims, no vars)"
            (m.Name = "wx" && m.Types.IsEmpty && m.Bindings.IsEmpty) $"%A{m.Types}"
     with ex -> check "module: a bare path binds an empty repo-handle module" false ex.Message)

    // A branch and the snapshot id it points at name the SAME snapshot, so
    // they must build the same module shape.
    (try
        let snapKey = wxRoot + "@snapshot:" + IW.snapshotId wxFullSpec "s2"
        let byId = loadAsModule (IRBuilder()) "ck" snapKey
        let byBranch = loadAsModule (IRBuilder()) "ck" mainKey
        check "module: a snapshot-id key builds the same module as the branch pointing at it"
            (List.length byId.Types = List.length byBranch.Types
             && List.length byId.Types > 0)
            (sprintf "%d vs %d types" (List.length byId.Types) (List.length byBranch.Types))
     with ex -> check "module: a snapshot-id key builds the same module as its branch" false ex.Message)

    // Dense reads: the whole point. Both compression modes, all four
    // ChunkRef shapes, and an edge-chunk grid throughout.
    printfn "\n--- provider dense reads ---"

    let readsAs (key: string) (varName: string) (expected: float[]) (dims: int list) (label: string) =
        match spec.ReadVarData key varName with
        | Error e -> check ($"read: {label}") false e
        | Ok data ->
            check ($"read: {label} -- dim lengths") (data.DimLengths = dims) (sprintf "%A" data.DimLengths)
            match data.Payload with
            | Blade.ProviderRegistry.PFloats got ->
                check ($"read: {label} -- values")
                    (got.Length = expected.Length
                     && Array.forall2 (fun (a: float) (b: float) -> abs (a - b) <= 1e-12 * max 1.0 (abs b)) got expected)
                    (if got.Length <> expected.Length then $"{got.Length} vs {expected.Length} values"
                     else sprintf "%A" (Array.truncate 8 got))
            | Blade.ProviderRegistry.PInts _ ->
                check ($"read: {label} -- values") false "payload came back as int64, not float"

    (try
        readsAs mainKey "temp" tempV2 [5; 4] "branch main -> s2 (native chunks, zstd)"
        readsAs tagKey "temp" tempV1 [5; 4] "tag v1.0 -> s1 (native chunks, zstd)"
        readsAs bareKey "temp" tempV2 [5; 4] "bare '?' form resolves to the branch"
        readsAs (wxRoot + "@snapshot:" + IW.snapshotId wxFullSpec "s1") "temp" tempV1 [5; 4]
                "snapshot id -> s1"
        readsAs mainKey "lat" latData [5] "coordinate array (INLINE chunk)"
        readsAs mainKey "lon" lonData [4] "second coordinate array (INLINE chunk)"
        readsAs (wxPlainRoot + "@branch:main") "temp" tempV2 [5; 4]
                "the same repo with compression byte 0"
     with ex -> check "read: dense reads over the workhorse fixture" false ex.Message)

    (try
        readsAs (polRoot + "@?:main") "inl" (polData 0.0) [5; 4] "policy inl -- every chunk INLINE"
        readsAs (polRoot + "@?:main") "nat" (polData 100.0) [5; 4] "policy nat -- one file per chunk"
        readsAs (polRoot + "@?:main") "pk" (polData 200.0) [5; 4]
                "policy pk -- packed file, NONZERO ChunkRef offsets"
        readsAs (polRoot + "@?:main") "sp" (polData 300.0) [5; 4]
                "policy sp -- two manifests over disjoint ChunkIndexRanges"
        // Chunk (1,1) of a 5x4/3x2 grid covers rows [3,5) x cols [2,4):
        // flat indices 14, 15, 18, 19. With no ref, those read as fill (-1).
        let flExpect =
            let a = Array.copy (polData 400.0)
            for i in [14; 15; 18; 19] do a.[i] <- -1.0
            a
        readsAs (polRoot + "@?:main") "fl" flExpect [5; 4]
                "policy fl -- an ABSENT chunk reads as fill_value"
     with ex -> check "read: placement-policy reads" false ex.Message)

    // Named refusals, over repos that really are what they claim.
    printfn "\n--- provider refusals over real repos ---"

    (try
        (let r = load (ambRoot + "@?:x")
         check "refuse: a bare ref naming BOTH a branch and a tag is ambiguous"
             (isError r "ambiguous" && isError r "ic.branch" && isError r "ic.tag") (errorText r))
        check "refuse: the markers disambiguate the colliding name"
            (match load (ambRoot + "@branch:x"), load (ambRoot + "@tag:x") with
             | Ok _, Ok _ -> true
             | _ -> false)
            (errorText (load (ambRoot + "@branch:x")) + " | " + errorText (load (ambRoot + "@tag:x")))

        (let r = load (offlineRoot + "@branch:main")
         check "refuse: an Offline repo refuses a checkout by name" (isError r "Offline") (errorText r))
        // Plan §3: the repo file is parsed AT LOAD, so an Offline repo fails
        // at the load site rather than at the first checkout.
        (let msg = caught (fun () -> loadAsModule (IRBuilder()) "wx" offlineRoot)
         check "refuse: an Offline repo fails at the bare LOAD site, not at first checkout"
             (msg.Contains "Offline") msg)
        check "refuse: a ReadOnly repo reads normally (only Offline refuses)"
            (match load (roRoot + "@branch:main") with Ok _ -> true | Error _ -> false)
            (errorText (load (roRoot + "@branch:main")))

        (let r = load spec1Root
         check "refuse: a real spec-1 repo refuses by name at the load site"
             (isError r "spec version 1") (errorText r))

        (let r = spec.ReadVarData (virtRoot + "@?:main") "v"
         check "refuse: a VIRTUAL chunk ref is refused by name"
             (isError r "VIRTUAL chunk refs are not supported") (errorText r))
        (let msg = caught (fun () -> spec.GenReadVar (virtRoot + "@?:main") "v" "v" arrType)
         check "refuse: the emitted reader refuses a virtual ref too"
             (msg.Contains "VIRTUAL chunk refs are not supported") msg)

        // Packed pools do not FOLD (StaticValue has no packed carrier), so the
        // fold path steers to `ic.read` -- the same steering the Zarr provider
        // applies, word for word.
        (let r = spec.ReadVarData (packedRoot + "@?:main") "tri"
         check "refuse: a packed pool steers the compile-time fold to ic.read"
             (isError r "packed (blade: layout=packed) pool layout"
              && isError r "do not fold at compile time") (errorText r))
        (let r = spec.ReadVarData (wxRoot + "@branch:main") "nope"
         check "refuse: a missing variable in a real checkout lists what IS there"
             (isError r "not found" && isError r "temp") (errorText r))
        (let r = spec.ReadVarData wxRoot "temp"
         check "refuse: reading through a bare REPO HANDLE says to check a ref out first"
             (isError r "REPO HANDLE" && isError r "checkout") (errorText r))
        // Wreath capability: present, and it refuses anything that is not an
        // orbit head rather than mistyping the pool.
        (match spec.ReadWreathPool with
         | Some readPool ->
             let r = readPool (packedRoot + "@?:main") "tri"
             check "refuse: a depth-1 packed array is not an orbit pool"
                 (isError r "depth-1 packed" || isError r "not an orbit") (errorText r)
             let r2 = readPool (wxRoot + "@branch:main") "temp"
             check "refuse: a dense array is not an orbit pool"
                 (isError r2 "not an orbit") (errorText r2)
         | None -> check "refuse: the wreath capability is declared" false "ReadWreathPool = None")
     with ex -> check "refuse: named refusals over real repos" false ex.Message)

    // VarDimNames, VersionStamp and Fingerprint over a real repo.
    (try
        let tempDims = spec.VarDimNames mainKey "temp"
        check "provenance: VarDimNames reads the snapshot's structural names"
            (tempDims = Some ["lat"; "lon"]) (sprintf "%A" tempDims)
        check "provenance: VarDimNames is None-safe on a repo handle"
            ((spec.VarDimNames wxRoot "temp").IsNone) ""
        check "provenance: the version stamp is the ONE mutable file's mtime"
            (spec.VersionStamp wxRoot > 0L && spec.VersionStamp wxRoot = spec.VersionStamp mainKey) ""
        check "provenance: the fingerprint separates two checkouts of one repo"
            (spec.Fingerprint mainKey <> spec.Fingerprint tagKey
             && (spec.Fingerprint mainKey).Length > 0) ""
     with ex -> check "provenance over a real repo" false ex.Message)

    // ---------------------------------------------------------------
    // 11. Runtime read e2e (g++; fixture repos generated on the fly)
    // ---------------------------------------------------------------
    // The full arc: `ic.load` -> `repo.checkout` -> `ic.read` -> a reduce,
    // compiled to C++ and run. Two programs, because the two checkout forms
    // take different paths through the desugar: the BARE form (resolved
    // across namespaces) and the MARKER form (`ic.tag`).
    printfn "\n--- read e2e: ic.load / repo.checkout / ic.read ---"

    let e2eDir = "./generated_cpp_tests"
    if not (Directory.Exists e2eDir) then Directory.CreateDirectory e2eDir |> ignore

    /// `name = <number>` on its own line, InvariantCulture.
    let printedScalar (name: string) (out: string) : float option =
        out.Split('\n')
        |> Array.tryPick (fun l ->
            let l = l.Trim()
            let prefix = name + " = "
            if l.StartsWith prefix then
                match Double.TryParse(l.Substring prefix.Length,
                                      Globalization.NumberStyles.Float,
                                      Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some v
                | _ -> None
            else None)

    (try
        // The same relative path has to resolve at BOTH working directories:
        // the compiler's (compile-time ref resolution + chunk-table baking)
        // and the executable's (runtime chunk reads). Same split as
        // ZarrTests' store mirroring.
        let e2eRepo = fixRepo "ic_e2e"
        let e2eSpec = wxSpec true ["s1"; "s2"]
        IW.writeRepoAt [ e2eRepo; Path.Combine(e2eDir, e2eRepo) ] e2eSpec

        // (label, source, expected `total`, expected checkout)
        //   main -> s2 -> tempV2, sum 2100      v1.0 -> s1 -> tempV1, sum 210
        let programs =
            [ ("bare", "repo.checkout(\"main\")", 2100.0)
              ("marker", "repo.checkout(\"v1.0\", ic.tag)", 210.0) ]

        for (label, checkoutExpr, expectedTotal) in programs do
            let src =
                sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = %s
let A = ck.vars.temp |> ic.read
let total = reduce(A, (+), axes = 2)
"""
                        e2eRepo checkoutExpr
            match lower src with
            | Error e -> check ($"e2e {label}: lowers") false e
            | Ok ir ->
                check ($"e2e {label}: ProviderReads spec (provider=icechunk, var=temp)")
                    (ir.Modules.[0].ProviderReads
                     |> Map.exists (fun _ s -> s.Provider = "icechunk" && s.VarName = "temp"))
                    ""
                let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir ($"icechunk_read_e2e_{label}")
                // The whole point of resolving at compile time: no Icechunk
                // logic reaches the binary -- no FlatBuffers, no zstd, just
                // std::ifstream over baked offsets.
                check ($"e2e {label}: emits fstream reads and NO icechunk/zstd/flatbuffers dependency")
                    (cppCode.Contains "std::ifstream"
                     && not (cppCode.Contains "zstd") && not (cppCode.Contains "flatbuffers")
                     && not (cppCode.Contains "netcdf.h")) ""
                CodeGen.deployRuntimeHeaders e2eDir
                let cppFile = Path.Combine(e2eDir, $"icechunk_read_e2e_{label}.cpp")
                File.WriteAllText(cppFile, cppCode)
                (match compileCpp cppFile e2eDir with
                 | Ok exePath ->
                     check ($"e2e {label}: compiles (pure std C++ -- no link flags)") true ""
                     (match runExecutable exePath with
                      | Ok (0, runOut) ->
                          check ($"e2e {label}: runs (exit 0)") true ""
                          check ($"e2e {label}: total = sum over the checked-out snapshot = {expectedTotal}")
                              (match printedScalar "total" runOut with
                               | Some v -> abs (v - expectedTotal) <= 1e-9
                               | None -> false)
                              runOut
                      | Ok (code, runOut) ->
                          check ($"e2e {label}: runs (exit 0)") false ($"exit {code}: {runOut}")
                      | Error e -> check ($"e2e {label}: runs (exit 0)") false e)
                 | Error e -> baselineFailed ($"icechunk read e2e {label}") e)

        // A repo that is gone at RUN time must die loudly, not read zeros:
        // the compiled binary holds baked paths into a directory that a GC
        // or an expiration can remove out from under it.
        (let missingDir = Path.Combine(Path.GetTempPath(), "blade_ic_missing_" + Guid.NewGuid().ToString("N"))
         Directory.CreateDirectory missingDir |> ignore
         try
            let exeName = "icechunk_read_e2e_bare" + (if OperatingSystem.IsWindows() then ".exe" else "")
            let builtExe = Path.Combine(e2eDir, exeName)
            if File.Exists builtExe then
                let exeCopy = Path.Combine(missingDir, exeName)
                File.Copy(builtExe, exeCopy, true)
                match runExecutable exeCopy with
                | Ok (code, missOut) ->
                    let headOfOut = missOut.Substring(0, min 200 missOut.Length)
                    check "e2e: a repo missing at run time fails loudly (nonzero exit)"
                        (code <> 0) ($"exit {code}: " + headOfOut)
                | Error e -> check "e2e: a repo missing at run time fails loudly" false e
            else
                // No exe means the baseline compile above SKIPped (no g++) or
                // failed -- either way `baselineFailed` has already said so.
                // Without this arm the whole assertion VANISHED from the
                // output on a toolchain-less machine: no PASS, no FAIL, no
                // SKIP, nothing to notice was missing. Same failure mode the
                // file's header warns about for baselines, one level down.
                printfn "  SKIP e2e: a repo missing at run time fails loudly: '%s' was not built" exeName
         finally
            try Directory.Delete(missingDir, true) with _ -> ())
     with ex -> check "e2e: icechunk read" false ex.Message)

    // ---------------------------------------------------------------
    // 12. Axis identity across checkouts (plan §5)
    // ---------------------------------------------------------------
    // Two checkouts of one repo share the index type for a dimension IFF the
    // AXIS is unchanged between their snapshots: same name, same extent, and
    // -- when a coordinate variable named after the dim exists -- the same
    // coordinate content, decided from METADATA ALONE (node id, user_data
    // bytes, chunk-ref table). No chunk is read to answer the question.
    //
    // The observable consequence is the point, and it is asserted at BOTH
    // levels: the index types two modules carry have the same `Id` (which is
    // what `unify` compares), and a source-level program that subtracts one
    // checkout's variable from another's typechecks -- or, when the axis
    // diverged, refuses.
    printfn "\n--- axis identity across checkouts ---"

    /// `dim -> index-type Id` for a checkout module. Ids are what unification
    /// compares, so two checkouts SHARE an axis exactly when these agree.
    let axisIdsOf (m: IRModule) : Map<string, int> =
        m.Types
        |> List.choose (function IRTDIndexType (n, it) -> Some (n, it.Id) | _ -> None)
        |> Map.ofList

    /// The index-type Ids of one variable of a checkout module, in order --
    /// the array type's OWN slots, not the module's type defs.
    let varIdsOf (m: IRModule) (binding: string) (varName: string) : int list option =
        m.Types
        |> List.tryPick (function IRTDStruct (n, fs) when n = binding + "__vars" -> Some fs | _ -> None)
        |> Option.defaultValue []
        |> List.tryPick (fun (n, t) ->
            if n <> varName then None else
            match t with
            | ArrayElem at -> Some (at.IndexTypes |> List.map (_.Id))
            | _ -> None)

    let axisModule (binding: string) (key: string) : IRModule =
        loadAsModule (IRBuilder()) binding key

    /// The refusal a diverged axis earns: the ordinary NAMED-AXIS refusal, in
    /// whichever of its two wordings applies -- the rank->=2 product rule
    /// ("same axis tags and extents") for a 2-D variable, BL3999's rank-1
    /// co-iteration message for a 1-D one. Both are the same fact: two index
    /// types with different names are not one index space.
    ///
    /// AND NO RAW PROVENANCE TAG, anywhere in the text. An axis tag is an
    /// internal identity ("__icaxis|lat@ic_launder:9f3a1c2b4d5e6f70#2") and the
    /// rank-1 message used to print it verbatim where the store says `lat`;
    /// it now decodes through `Types.displayTagName`. Pinned HERE, on the
    /// predicate every axis-refusal assertion in this file already runs
    /// through, so the leak cannot come back through any of them -- section 20
    /// pins the decoded NAMES for the two shapes whose fixtures it knows.
    let refusesOnAxes (r: Result<'a, string>) : bool =
        match r with
        | Error e ->
            (e.Contains "same axis tags and extents" || e.Contains "DIFFERENT index types")
            && not (e.Contains axisTagPrefix)
            && not (e.Contains providerPoolTagPrefix)
            && not (e.Contains providerOrbPoolTagPrefix)
        | Ok _ -> false

    /// A cross-checkout differencing program over one repo: the §5 headline.
    /// `ck2.vars.temp - ck1.vars.temp` typechecks only if the two checkouts'
    /// axes unify.
    let crossCheckoutSrc (root: string) =
        sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
let A = ck1.vars.temp |> ic.read
let B = ck2.vars.temp |> ic.read
let d = B - A
let total = reduce(d, (+), axes = 2)
"""
                root

    // Every axis fixture is its own repo root, so one section's identities can
    // never answer another's -- `repoPath` is half the mint-table key.
    let axisCoordRoot = fixRepo "ic_axis_coord"      // coordinate REWRITTEN, same extent
    let axisRegridRoot = fixRepo "ic_axis_regrid"    // coordinate REGRIDDED, new extent
    let axisNoCoordRoot = fixRepo "ic_axis_nocoord"  // dims with NO coordinate variable
    let axisTwinRoot = fixRepo "ic_axis_twin"        // a byte-identical SECOND repo

    let latShifted = [| 100.0; 101.0; 102.0; 103.0; 104.0 |]
    let lat6 = [| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 |]
    let temp6x4 = Array.init 24 (fun i -> float (i + 1))

    /// Same grid, same `temp`, `lat` rewritten between the two commits. The
    /// coordinate array's node id is unchanged (ids are derived from the NAME),
    /// so what has to notice the difference is the chunk-ref table.
    let coordRewriteSpec : IW.RepoSpec =
        let lon = IW.mkArray "lon" ["lon"] [4L] [4L] (IW.IceF64 lonData)
        let temp = { IW.mkArray "temp" ["lat"; "lon"] [5L; 4L] [3L; 2L] (IW.IceF64 tempV1) with
                       InlineThreshold = 0 }
        let lat (d: float[]) = IW.mkArray "lat" ["lat"] [5L] [5L] (IW.IceF64 d)
        { IW.emptyRepo with
            Seed = 11
            Snapshots = [ IW.mkSnapshot "s1" [ lat latData; lon; temp ]
                          IW.mkSnapshot "s2" [ lat latShifted; lon; temp ] ]
            Branches = [ ("main", "s2") ]
            Tags = [ ("v1.0", "s1") ] }

    /// A regrid: `lat` (and therefore `temp`) changes EXTENT between commits.
    let regridSpec : IW.RepoSpec =
        let lon = IW.mkArray "lon" ["lon"] [4L] [4L] (IW.IceF64 lonData)
        { IW.emptyRepo with
            Seed = 12
            Snapshots =
                [ IW.mkSnapshot "s1"
                    [ IW.mkArray "lat" ["lat"] [5L] [5L] (IW.IceF64 latData); lon
                      { IW.mkArray "temp" ["lat"; "lon"] [5L; 4L] [3L; 2L] (IW.IceF64 tempV1) with
                          InlineThreshold = 0 } ]
                  IW.mkSnapshot "s2"
                    [ IW.mkArray "lat" ["lat"] [6L] [6L] (IW.IceF64 lat6); lon
                      { IW.mkArray "temp" ["lat"; "lon"] [6L; 4L] [3L; 2L] (IW.IceF64 temp6x4) with
                          InlineThreshold = 0 } ] ]
            Branches = [ ("main", "s2") ]
            Tags = [ ("v1.0", "s1") ] }

    /// `temp` over named dimensions with NO coordinate arrays: §5.2 condition
    /// (3) is vacuous, so name + extent IS the identity.
    let noCoordSpec : IW.RepoSpec =
        let temp (d: float[]) =
            { IW.mkArray "temp" ["row"; "col"] [5L; 4L] [3L; 2L] (IW.IceF64 d) with
                InlineThreshold = 0 }
        { IW.emptyRepo with
            Seed = 13
            Snapshots = [ IW.mkSnapshot "s1" [ temp tempV1 ]; IW.mkSnapshot "s2" [ temp tempV2 ] ]
            Branches = [ ("main", "s2") ]
            Tags = [ ("v1.0", "s1") ] }

    (try
        IW.writeRepo axisCoordRoot coordRewriteSpec
        IW.writeRepo axisRegridRoot regridSpec
        IW.writeRepo axisNoCoordRoot noCoordSpec
        IW.writeRepo axisTwinRoot wxFull
        // The fixtures were just (re)written at paths earlier sections may
        // already have read; drop the mtime-keyed memos AND the identities
        // decided from them before asking anything.
        resetCaches ()

        // (a) The same ref, checked out twice. Nothing changed because nothing
        // could have: every axis compares equal to itself.
        let sameA = axisModule "ck1" mainKey
        let sameB = axisModule "ck2" mainKey
        check "axis: the same ref checked out twice shares every axis identity"
            (axisIdsOf sameA = axisIdsOf sameB && (axisIdsOf sameA).Count = 2)
            (sprintf "%A vs %A" (axisIdsOf sameA) (axisIdsOf sameB))
        check "axis: two checkouts of one ref give `temp` the same index slots"
            (varIdsOf sameA "ck1" "temp" = varIdsOf sameB "ck2" "temp"
             && (varIdsOf sameA "ck1" "temp").IsSome)
            (sprintf "%A vs %A" (varIdsOf sameA "ck1" "temp") (varIdsOf sameB "ck2" "temp"))

        // (b) THE HEADLINE. `main` and `v1.0` differ only in `temp`'s cells --
        // the coordinate arrays are untouched, so their node ids, user_data and
        // manifest ids are identical and the axes are the SAME axes.
        let mainM = axisModule "ck2" mainKey
        let tagM = axisModule "ck1" tagKey
        check "axis: a data-only commit leaves lat and lon SHARED across checkouts"
            (axisIdsOf mainM = axisIdsOf tagM && (axisIdsOf mainM).Count = 2)
            (sprintf "%A vs %A" (axisIdsOf mainM) (axisIdsOf tagM))
        check "axis: the differencing pair's `temp` arrays carry the same index slots"
            (varIdsOf mainM "ck2" "temp" = varIdsOf tagM "ck1" "temp"
             && (varIdsOf mainM "ck2" "temp") = Some [ (axisIdsOf mainM).["lat"]; (axisIdsOf mainM).["lon"] ])
            (sprintf "%A vs %A" (varIdsOf mainM "ck2" "temp") (varIdsOf tagM "ck1" "temp"))
        check "axis: a shared axis records ONE identity, presented by both refs"
            (match axisIdentities wxRoot "lat" with
             | [ one ] -> List.sort one.Refs = ["branch:main"; "tag:v1.0"] && one.CoordFP.IsSome
             | _ -> false)
            (sprintf "%A" (axisIdentities wxRoot "lat" |> List.map (fun i -> (i.Refs, i.SplitReason))))
        check "axis: the shared identity is a NAMED axis, and both checkouts use that name"
            (match axisIdentities wxRoot "lat" with
             | [ one ] ->
                 (defaultArg one.IndexType.Tag "").StartsWith (axisTagPrefix + "lat@")
                 && one.IndexType.Id >= axisIdBase
             | _ -> false)
            (sprintf "%A" (axisIdentities wxRoot "lat" |> List.map (fun i -> (i.IndexType.Tag, i.IndexType.Id))))

        // ... and the consequence that matters: the program compiles.
        (match lower (crossCheckoutSrc wxRoot) with
         | Ok _ -> check "axis: `ck2.vars.temp - ck1.vars.temp` TYPECHECKS across a data-only commit" true ""
         | Error e ->
             check "axis: `ck2.vars.temp - ck1.vars.temp` TYPECHECKS across a data-only commit" false e)

        // Naming an axis must not make ORDINARY use of a checkout noisy. The
        // tag is `__`-prefixed exactly so the three seams that read a tag as a
        // user-facing NAME stay quiet: no BL4003 "indexed with untagged
        // integer" on a plain subscript, and an alias through
        // `<binding>.index.<dim>` (which re-tags with the ALIAS name) still
        // ascribes. Both regressed when the tag was a plain name.
        (let quiet =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
let A = ck.vars.temp |> ic.read
type Lat = ck.index.lat
type Lon = ck.index.lon
let cell = A(2, 1)
let B: Array<Float64 like Lat, Lon> = A
let total = reduce(B, (+), axes = 2)
"""
                    wxRoot
         let (r, warns) = Blade.Lowering.lowerCaptured quiet
         check "axis: a named axis does not make ordinary subscripting noisy (no BL4003)"
             ((match r with Ok _ -> true | Error _ -> false)
              && not (warns |> List.exists (fun (d: Blade.Diagnostics.Diagnostic) -> d.Code = "BL4003")))
             (match r with
              | Error e -> e
              | Ok _ -> warns |> List.map (fun d -> d.Code + ": " + d.Message) |> String.concat "; "))

        // (c) The coordinate variable itself was rewritten (same extent, same
        // node id): a different axis, and arithmetic across it refuses.
        let coordMain = axisModule "ck2" (axisCoordRoot + "@branch:main")
        let coordTag = axisModule "ck1" (axisCoordRoot + "@tag:v1.0")
        check "axis: a REWRITTEN coordinate splits the identity (lat differs)"
            ((axisIdsOf coordMain).["lat"] <> (axisIdsOf coordTag).["lat"])
            (sprintf "%A vs %A" (axisIdsOf coordMain) (axisIdsOf coordTag))
        check "axis: the split is PER-AXIS -- untouched lon still shares"
            ((axisIdsOf coordMain).["lon"] = (axisIdsOf coordTag).["lon"])
            (sprintf "%A vs %A" (axisIdsOf coordMain) (axisIdsOf coordTag))
        check "axis: the split records WHY (coordinate content, not extent)"
            (match axisIdentities axisCoordRoot "lat" with
             | newest :: _ :: [] -> newest.SplitReason = Some "coordinate content differs"
             | _ -> false)
            (sprintf "%A" (axisIdentities axisCoordRoot "lat" |> List.map (fun i -> i.SplitReason)))
        // The refusal is the ordinary named-axis one (BL3999): the two
        // identities carry DIFFERENT axis names, and a named index type does
        // not co-iterate with a differently-named one merely by matching
        // extent. Ids alone would not do this -- `unify`'s ArrayElem arm never
        // compares them.
        check "axis: the split identities carry different axis NAMES"
            (match axisIdentities axisCoordRoot "lat" with
             | newest :: older :: [] ->
                 newest.IndexType.Tag <> older.IndexType.Tag
                 && (defaultArg newest.IndexType.Tag "").StartsWith (axisTagPrefix + "lat@")
             | _ -> false)
            (sprintf "%A" (axisIdentities axisCoordRoot "lat" |> List.map (fun i -> i.IndexType.Tag)))
        (let r = lower (crossCheckoutSrc axisCoordRoot)
         check "axis: differencing across a REWRITTEN coordinate is REFUSED"
             (refusesOnAxes r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked, so the two checkouts' lat axes co-iterated"))

        // (d) A regrid: the extent moved, which is a split before any
        // coordinate content is even compared.
        let regridMain = axisModule "ck2" (axisRegridRoot + "@branch:main")
        let regridTag = axisModule "ck1" (axisRegridRoot + "@tag:v1.0")
        check "axis: a REGRID (extent 5 -> 6) splits the identity"
            ((axisIdsOf regridMain).["lat"] <> (axisIdsOf regridTag).["lat"]
             && (axisIdsOf regridMain).["lon"] = (axisIdsOf regridTag).["lon"])
            (sprintf "%A vs %A" (axisIdsOf regridMain) (axisIdsOf regridTag))
        // Both extents in the reason, in the order the two checkouts were
        // loaded -- `main` (6) is read above before `v1.0` (5).
        check "axis: the regrid split names the EXTENT"
            (match axisIdentities axisRegridRoot "lat" with
             | newest :: _ :: [] -> newest.SplitReason = Some "extent 6 -> 5"
             | _ -> false)
            (sprintf "%A" (axisIdentities axisRegridRoot "lat" |> List.map (fun i -> i.SplitReason)))
        (let r = lower (crossCheckoutSrc axisRegridRoot)
         check "axis: differencing across a REGRID is REFUSED"
             (match r with Error _ -> true | Ok _ -> false)
             "the program typechecked, so two differently-sized lat axes unified")

        // (e) No coordinate variable: nothing to compare, so name + extent is
        // the whole identity and a data-only commit still shares.
        let ncMain = axisModule "ck2" (axisNoCoordRoot + "@branch:main")
        let ncTag = axisModule "ck1" (axisNoCoordRoot + "@tag:v1.0")
        check "axis: dims with NO coordinate variable share on name + extent"
            (axisIdsOf ncMain = axisIdsOf ncTag && (axisIdsOf ncMain).Count = 2)
            (sprintf "%A vs %A" (axisIdsOf ncMain) (axisIdsOf ncTag))
        check "axis: a coordinate-less axis records no fingerprint at all"
            (match axisIdentities axisNoCoordRoot "row" with
             | [ one ] -> one.CoordFP.IsNone && one.Extent = 5L
             | _ -> false)
            (sprintf "%A" (axisIdentities axisNoCoordRoot "row" |> List.map (fun i -> (i.CoordFP, i.Extent))))
        (match lower (crossCheckoutSrc axisNoCoordRoot) with
         | Ok _ -> check "axis: differencing across a coordinate-less grid TYPECHECKS" true ""
         | Error e -> check "axis: differencing across a coordinate-less grid TYPECHECKS" false e)

        // (f) A byte-identical SECOND repo. Same dim names, same extents, same
        // coordinate bytes -- and no sharing, because there is no identity
        // between two repos to anchor one.
        let twinM = axisModule "ck3" (axisTwinRoot + "@branch:main")
        check "axis: two DIFFERENT repos never share, however identical"
            ((axisIdsOf twinM).["lat"] <> (axisIdsOf mainM).["lat"]
             && (axisIdsOf twinM).["lon"] <> (axisIdsOf mainM).["lon"]
             && (axisIdsOf twinM).Count = 2)
            (sprintf "%A vs %A" (axisIdsOf twinM) (axisIdsOf mainM))
        check "axis: the twin repo's axes are a separate mint-table entry"
            ((axisIdentities axisTwinRoot "lat").Length = 1
             && (axisIdentities wxRoot "lat").Length = 1)
            (sprintf "twin %d, wx %d"
                 (axisIdentities axisTwinRoot "lat").Length (axisIdentities wxRoot "lat").Length)
        (let crossRepo =
            sprintf """
import icechunk as ic

let r1 = ic.load("%s")
let r2 = ic.load("%s")
let ck1 = r1.checkout("main")
let ck2 = r2.checkout("main")
let A = ck1.vars.temp |> ic.read
let B = ck2.vars.temp |> ic.read
let d = B - A
let total = reduce(d, (+), axes = 2)
"""
                    wxRoot axisTwinRoot
         let r = lower crossRepo
         check "axis: differencing ACROSS REPOS is refused, identical bytes and all"
             (refusesOnAxes r)
             (match r with Error e -> e | Ok _ -> "the program typechecked across two repos"))

        // (g) The table is per COMPILATION. Both resets clear it, and the next
        // compilation mints identities of its own -- ids from a dead table can
        // never come back and be mistaken for live ones.
        let beforeReset = (axisIdsOf (axisModule "ck" mainKey)).["lat"]
        resetAxisMint ()
        check "axis: resetAxisMint clears the table" (axisIdentities wxRoot "lat" = []) ""
        let afterReset = (axisIdsOf (axisModule "ck" mainKey)).["lat"]
        check "axis: the next compilation mints a FRESH identity (no leak)"
            (afterReset <> beforeReset) $"{beforeReset} vs {afterReset}"
        resetCaches ()
        check "axis: resetCaches clears the mint table too" (axisIdentities wxRoot "lat" = []) ""
        let afterCaches = (axisIdsOf (axisModule "ck" mainKey)).["lat"]
        check "axis: and that one is fresh as well"
            (afterCaches <> afterReset && afterCaches <> beforeReset)
            $"{beforeReset}, {afterReset}, {afterCaches}"
     with ex -> check "axis: identity across checkouts" false ex.Message)

    // The differencing program, compiled and RUN: sharing is not a
    // typechecker-only property. tempV2 sums to 2100 and tempV1 to 210, so the
    // difference sums to 1890 -- computed cell by cell over axes both
    // checkouts agree on.
    (try
        let axisE2eRoot = fixRepo "ic_axis_e2e"
        IW.writeRepoAt [ axisE2eRoot; Path.Combine(e2eDir, axisE2eRoot) ] wxFull
        resetCaches ()
        match lower (crossCheckoutSrc axisE2eRoot) with
        | Error e -> check "axis e2e: the cross-checkout difference lowers" false e
        | Ok ir ->
            check "axis e2e: both checkouts' reads reach ProviderReads"
                ((ir.Modules.[0].ProviderReads
                  |> Map.filter (fun _ s -> s.Provider = "icechunk" && s.VarName = "temp")
                  |> Map.count) = 2)
                (sprintf "%d icechunk temp reads" (ir.Modules.[0].ProviderReads |> Map.count))
            let (cppCode, _) = CodeGen.genSelfContainedProgramFromIR ir "icechunk_axis_diff"
            CodeGen.deployRuntimeHeaders e2eDir
            let cppFile = Path.Combine(e2eDir, "icechunk_axis_diff.cpp")
            File.WriteAllText(cppFile, cppCode)
            (match compileCpp cppFile e2eDir with
             | Ok exePath ->
                 (match runExecutable exePath with
                  | Ok (0, runOut) ->
                      check "axis e2e: total = sum(tempV2 - tempV1) = 1890"
                          (match printedScalar "total" runOut with
                           | Some v -> abs (v - 1890.0) <= 1e-9
                           | None -> false)
                          runOut
                  | Ok (code, runOut) -> check "axis e2e: runs (exit 0)" false ($"exit {code}: {runOut}")
                  | Error e -> check "axis e2e: runs (exit 0)" false e)
             | Error e -> baselineFailed "icechunk axis-difference e2e" e)
     with ex -> check "axis e2e: cross-checkout difference" false ex.Message)

    // ---------------------------------------------------------------
    // 13. The snapshot pin, and repo-path identity
    // ---------------------------------------------------------------
    printfn "\n--- snapshot pin and path identity ---"

    // §3.1 claims typecheck, static folds, lowering and codegen all see ONE
    // snapshot even when a writer commits mid-compilation. A memo keyed on a
    // stamp that is RE-STATTED per call does not deliver that: the commit is
    // simply a memo MISS, and the later phases then resolve the newer snapshot
    // while typecheck's types still describe the older one -- each phase
    // internally consistent, the compilation as a whole not. The stamp is
    // therefore taken ONCE per repo per compilation and pinned, and this drives
    // the hazard exactly as it would arise: resolve, commit underneath, resolve
    // again.
    (try
        let pinRoot = fixRepo "ic_pin"
        let cells (k: int) = Array.init 12 (fun i -> float (k * 100 + i))
        let temp (d: float[]) =
            { IW.mkArray "temp" ["row"; "col"] [4L; 3L] [2L; 3L] (IW.IceF64 d) with
                InlineThreshold = 0 }
        /// The same two snapshots throughout; only where `main` POINTS moves,
        /// so the difference between the two repo states is one commit.
        let spec (mainAt: string) : IW.RepoSpec =
            { IW.emptyRepo with
                Seed = 71
                Snapshots =
                    [ IW.mkSnapshot "s1" [ temp (cells 1) ]; IW.mkSnapshot "s2" [ temp (cells 2) ] ]
                Branches = [ ("main", mainAt) ] }
        IW.writeRepo pinRoot (spec "s1")
        resetCaches ()

        let pinKey = pinRoot + "@branch:main"
        let resolvedSnapshot () =
            match load pinKey with
            | Ok (LoadedCheckout ck) -> base32Encode ck.SnapshotId
            | Ok (LoadedRepo _) -> "<repo handle>"
            | Error e -> "<error: " + e + ">"

        let firstResolve = resolvedSnapshot ()
        check "pin: the first resolution names a snapshot"
            (firstResolve.Length = objectIdChars) firstResolve
        check "pin: the first touch of a repo records a pinned stamp"
            ((RepoPinTable.tryPinned (canonicalRepoPath pinRoot)).IsSome) ""

        // The writer commits: `main` now names s2, and the repo file's mtime
        // moves forward -- which is exactly what a re-statted stamp notices,
        // and exactly what must NOT change an answer already given.
        IW.writeRepo pinRoot (spec "s2")
        File.SetLastWriteTimeUtc(repoFilePath pinRoot, DateTime.UtcNow.AddMinutes 10.0)

        let duringCompilation = resolvedSnapshot ()
        check "pin: a commit MID-COMPILATION does not move the resolved snapshot"
            (duringCompilation = firstResolve) $"{firstResolve} -> {duringCompilation}"

        // ... and the next compilation is not stuck on it: the pin dies with
        // the memos that were keyed on it.
        resetCaches ()
        check "pin: resetCaches drops the pin"
            ((RepoPinTable.tryPinned (canonicalRepoPath pinRoot)).IsNone) ""
        let afterReset = resolvedSnapshot ()
        check "pin: the NEXT compilation resolves the new commit"
            (afterReset <> firstResolve && afterReset.Length = objectIdChars)
            $"{firstResolve} -> {afterReset}"
     with ex -> check "pin: mid-compilation commit" false ex.Message)

    // Two spellings of ONE directory are ONE repo. Identity -- the read memos,
    // the axis mint table, the axis-tag digest -- keys on the canonical path,
    // so `ic.load("d/wx")` and `ic.load("./d/wx")` share axes; before this they
    // minted two universes and differencing across them refused, over the same
    // bytes of the same repo. Only IDENTITY canonicalizes: what gets baked into
    // C++ and printed in diagnostics is still the spelling the source wrote.
    (try
        let spellRoot = fixRepo "ic_spelling"
        IW.writeRepo spellRoot wxFull
        resetCaches ()
        let plainKey = spellRoot + "@branch:main"
        let dottedKey = "./" + spellRoot + "@branch:main"
        let plainM = axisModule "ck1" plainKey
        let dottedM = axisModule "ck2" dottedKey
        check "path: two spellings of one repo share every axis identity"
            (axisIdsOf plainM = axisIdsOf dottedM && (axisIdsOf plainM).Count = 2)
            (sprintf "%A vs %A" (axisIdsOf plainM) (axisIdsOf dottedM))
        check "path: and it is ONE mint-table entry, reachable under both spellings"
            ((axisIdentities spellRoot "lat").Length = 1
             && axisIdentities ("./" + spellRoot) "lat" = axisIdentities spellRoot "lat")
            (sprintf "%d identities" (axisIdentities spellRoot "lat").Length)
        check "path: canonicalization folds the two spellings together"
            (canonicalRepoPath spellRoot = canonicalRepoPath ("./" + spellRoot)
             && canonicalRepoPath spellRoot <> canonicalRepoPath (fixRepo "ic_wx"))
            (canonicalRepoPath spellRoot + " vs " + canonicalRepoPath ("./" + spellRoot))
        // The tag's discriminating half is 16 hex of the CANONICAL path's
        // digest, not 4: a 4-hex collision is 2^-16 per pair and would hand two
        // different repos one axis tag -- and an axis tag is a LICENSE, since
        // co-iteration reads axis agreement off the tag alone.
        check "path: the axis tag carries a 16-hex repo digest"
            (match axisIdentities spellRoot "lat" with
             | [ one ] ->
                 let tag = defaultArg one.IndexType.Tag ""
                 match tag.LastIndexOf ':' with
                 | i when i > 0 ->
                     let digest = tag.Substring(i + 1)
                     digest.Length = 16
                     && digest |> Seq.forall (fun c -> Char.IsDigit c || (c >= 'a' && c <= 'f'))
                 | _ -> false
             | _ -> false)
            (sprintf "%A" (axisIdentities spellRoot "lat" |> List.map (fun i -> i.IndexType.Tag)))
     with ex -> check "path: two spellings of one repo" false ex.Message)

    // ---------------------------------------------------------------
    // 14. Interpreter parity (the back end the notebook actually runs on)
    // ---------------------------------------------------------------
    // The sections above drive the COMPILED half exclusively: `lower`, then
    // genSelfContainedProgramFromIR, then g++. `blade repl` and the notebook
    // lane walk `Blade.Interp` instead, and the interpreter and codegen are
    // differential twins -- so the shipped demo had no automated evidence at
    // all that its own back end can read a checkout. Two programs: the plain
    // arc against a hand-computed value, and the §5 cross-checkout headline
    // against the compiled binary's stdout, byte for byte.
    printfn "\n--- interpreter parity ---"

    /// Normalized stdout (CRLF -> LF, timing lines out, trimmed) -- the
    /// InterpDiff comparison, mirrored from ZarrTests' `normOut`.
    let icNormOut (s: string) =
        s.Replace("\r\n", "\n").Split('\n')
        |> Array.filter (fun l -> not (l.Contains "completed in") && not (l.Contains "input allocation took"))
        |> Array.map (_.TrimEnd())
        |> String.concat "\n" |> fun t -> t.Trim()

    /// The interpreter's stdout for a source, or the reason it produced none.
    let icInterpStdout (label: string) (src: string) : Result<string, string> =
        match lower src with
        | Error e -> Error $"lower: {e}"
        | Ok ir ->
            let r = Blade.Interp.Run.runProgram ir label Blade.Interp.Value.defaultLimits
            if r.ExitCode = Blade.Interp.Run.ExitOk then Ok r.Stdout
            else Error $"interp exit {r.ExitCode}: {r.Stderr.Trim()}"

    /// The compiled binary's stdout for the same source (the reference half of
    /// the differential). The same emit -> deploy -> compile -> run chain the
    /// read-e2e section spells out inline.
    let icCompiledStdout (label: string) (src: string) : Result<string, string> =
        match lower src with
        | Error e -> Error $"lower: {e}"
        | Ok ir ->
            let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir label
            CodeGen.deployRuntimeHeaders e2eDir
            let cppFile = Path.Combine(e2eDir, label + ".cpp")
            File.WriteAllText(cppFile, cpp)
            match compileCpp cppFile e2eDir with
            | Error e -> Error e
            | Ok exePath ->
                match runExecutable exePath with
                | Ok (0, out) -> Ok out
                | Ok (code, out) -> Error $"exit {code}: {out}"
                | Error e -> Error e

    (try
        // Its OWN repo root, written to BOTH working directories: the
        // interpreter reads chunks at the compiler cwd, the compiled binary at
        // the exe cwd. A fresh root also keeps this section's mint entries and
        // mtime-keyed memos from answering (or being answered by) any other
        // section's.
        let interpRoot = fixRepo "ic_interp"
        IW.writeRepoAt [ interpRoot; Path.Combine(e2eDir, interpRoot) ] wxFull
        resetCaches ()

        // (i) The plain arc: load -> checkout -> read -> reduce. `main` is s2,
        // whose temp is tempV2 (10 .. 200), summing to 2100 -- the same
        // hand-computed number the read-e2e block pins on the compiled side.
        let interpBasicSrc =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
let A = ck.vars.temp |> ic.read
let total = reduce(A, (+), axes = 2)
"""
                    interpRoot
        (match icInterpStdout "icechunk_interp_basic" interpBasicSrc with
         | Error e -> check "interp: a checkout read runs on the interpreter" false e
         | Ok out ->
             check "interp: a checkout read runs on the interpreter" true ""
             check "interp: total = sum over the checked-out snapshot = 2100"
                 (match printedScalar "total" out with
                  | Some v -> abs (v - 2100.0) <= 1e-9
                  | None -> false)
                 out)

        // (ii) The §5 headline on BOTH back ends. tempV2 sums to 2100 and
        // tempV1 to 210, so the difference sums to 1890 -- and the two back
        // ends have to say so identically, which is a claim a single-value
        // assertion on one of them cannot make.
        let interpDiffSrc = crossCheckoutSrc interpRoot
        (match icInterpStdout "icechunk_interp_diff" interpDiffSrc with
         | Error e -> check "interp: the cross-checkout difference runs on the interpreter" false e
         | Ok interpOut ->
             check "interp: the cross-checkout difference runs on the interpreter" true ""
             check "interp: cross-checkout total = sum(tempV2 - tempV1) = 1890"
                 (match printedScalar "total" interpOut with
                  | Some v -> abs (v - 1890.0) <= 1e-9
                  | None -> false)
                 interpOut
             // The compiled half is the reference. A missing toolchain SKIPS
             // this one comparison only -- the value assertions above stand on
             // their own, so nothing is silently deleted by the skip.
             (match icCompiledStdout "icechunk_interp_diff_cpp" interpDiffSrc with
              | Error e -> baselineFailed "icechunk interpreter/compiled differential" e
              | Ok compiledOut ->
                  check "interp: interpreter stdout == compiled stdout (both back ends agree)"
                      (icNormOut interpOut = icNormOut compiledOut)
                      $"interp:\n{icNormOut interpOut}\ncompiled:\n{icNormOut compiledOut}"))
     with ex -> check "interp: icechunk parity" false ex.Message)

    // ---------------------------------------------------------------
    // 15. The IDE payload (plan §13's P2 gate, and axis dim names)
    // ---------------------------------------------------------------
    // `ide check` is what the editor and the notebook client read, and it is
    // the one surface where a checkout has to describe itself: which store the
    // read came from, and what the array's axes are CALLED. The axis tag is
    // `__`-prefixed on purpose (IcechunkProvider.axisTag), which is exactly
    // why the type printer dropped it and rendered `Idx<5>, Idx<4>` where the
    // store says lat and lon; `Ide.indexNamesOf` decodes it back.
    printfn "\n--- ide payload ---"

    // The registry install the ide path assumes (CliSelfTests does the same
    // before its own provider-payload block). Idempotent.
    Blade.ProviderStatics.install ()

    /// A named binding's rendered `type` from an `ide check` payload.
    let bindingTypeOf (json: string) (name: string) : string option =
        use doc = Text.Json.JsonDocument.Parse json
        match doc.RootElement.TryGetProperty "bindings" with
        | true, bs ->
            bs.EnumerateArray()
            |> Seq.tryPick (fun b ->
                match b.TryGetProperty "name" with
                | true, n when n.GetString() = name ->
                    (match b.TryGetProperty "type" with
                     | true, t -> Some (t.GetString())
                     | _ -> None)
                | _ -> None)
        | _ -> None

    (try
        let ideRoot = fixRepo "ic_ide"
        IW.writeRepo ideRoot wxFull
        resetCaches ()
        let ideSrc =
            "import icechunk as ic\n"
            + sprintf "let repo = ic.load(\"%s\")\n" (ideRoot.Replace('\\', '/'))
            + "let ck = repo.checkout(\"main\")\n"
            + "let A = ck.vars.temp |> ic.read\n"
        let (ideJson, ideCode) = Blade.Ide.ideCheckSource "icechunk_ide.blade" ideSrc
        let ideHead = if ideJson.Length > 800 then ideJson.Substring(0, 800) else ideJson
        check "ide: the checkout program checks clean" (ideCode = 0) ideHead
        // providers[] describes the store the DESUGARED program loads -- the
        // checkout binding, since `repo.checkout("main")` IS an `ic.load` by
        // the time Ide.collectProviderStores walks the tree.
        check "ide: providers[] names the icechunk checkout store"
            (ideJson.Contains "\"store\":\"ck\",\"alias\":\"ic\",\"provider\":\"icechunk\"") ideHead
        // The read binding's provenance, the same {store, member} shape the
        // csv and zarr payload blocks pin.
        check "ide: the read binding's providerRead = {store ck, member vars.temp}"
            (ideJson.Contains "\"providerRead\":{\"store\":\"ck\",\"member\":\"vars.temp\"}") ideHead
        // Pinned on the NAMES only: the digest inside the tag is an
        // implementation detail (its width has already changed once) and
        // nothing here depends on it.
        let aType = bindingTypeOf ideJson "A"
        check "ide: the checkout array's type renders the store's own dim names"
            (aType = Some "Array<Float64 like Idx<lat>, Idx<lon>>") $"%A{aType}"
        check "ide: no raw axis tag leaks into the payload"
            (not (ideJson.Contains axisTagPrefix)) ideHead
     with ex -> check "ide: icechunk payload" false ex.Message)

    // The decoder itself, on synthetic input -- the half `ideCheckSource`
    // cannot isolate. Names and the PREFIX only: the repo label's digest is
    // deliberately not part of any claim here, so it stays free to change
    // width or canonicalization without touching a single assertion.
    let sharedTag = axisTagPrefix + "lat@ic_wx:0f3a"
    let splitTag = axisTagPrefix + "lat@ic_wx:0f3a#2"
    let longTag = axisTagPrefix + "time@ic_wx:0123456789abcdef"
    check "axis tag: a shared axis decodes to its bare dim name"
        (tryAxisTagName sharedTag = Some "lat") $"%A{tryAxisTagName sharedTag}"
    check "axis tag: a SPLIT axis keeps its ordinal (two identities must not print alike)"
        (tryAxisTagName splitTag = Some "lat#2") $"%A{tryAxisTagName splitTag}"
    check "axis tag: a longer digest decodes the same (nothing here reads the digest)"
        (tryAxisTagName longTag = Some "time") $"%A{tryAxisTagName longTag}"
    check "axis tag: a FOREIGN `__` tag is not an axis tag"
        ((tryAxisTagName "__orbidx|x").IsNone && (tryAxisTagName "__raggedidx").IsNone) ""
    check "axis tag: a plain user-written tag is not an axis tag"
        ((tryAxisTagName "lat").IsNone) ""
    check "axis tag: malformed input decodes to None, never to a partial name"
        ([ ""; axisTagPrefix; axisTagPrefix + "lat"; axisTagPrefix + "@ic_wx:0f3a" ]
         |> List.forall (fun t -> (tryAxisTagName t).IsNone)) ""

    // And the LIVE tag: whatever the mint stamped on a real checkout's axis
    // has to decode back to the dim name the store uses. Synthetic inputs
    // alone would keep passing if the minted SHAPE drifted.
    (try
        let tagRoot = fixRepo "ic_ide_tag"
        IW.writeRepo tagRoot wxFull
        resetCaches ()
        axisModule "ck" (tagRoot + "@branch:main") |> ignore
        check "axis tag: the tag a real checkout mints decodes to the store's dim name"
            (match axisIdentities tagRoot "lat" with
             | [ one ] -> tryAxisTagName (defaultArg one.IndexType.Tag "") = Some "lat"
             | _ -> false)
            (sprintf "%A" (axisIdentities tagRoot "lat" |> List.map (fun i -> i.IndexType.Tag)))
     with ex -> check "axis tag: live decode" false ex.Message)

    // ---------------------------------------------------------------
    // 16. The committed demo notebook (examples/station_temps.bladenb)
    // ---------------------------------------------------------------
    // The notebook is a shipped artifact reading a COMMITTED store, and it had
    // no automated evidence anywhere: a provider change could rot it silently
    // and only a human opening the file would find out. Cheap by construction
    // -- the cells are split, assembled and LOWERED, never compiled and never
    // run.
    //
    // LOWERING, not `check`, and the reason has changed. It used to be that
    // `check` proved LESS: provider resolution failures were swallowed there
    // and lowering was the only level at which the store had to answer. Since
    // BL2008 (section 20) that is no longer true -- the ambiguous-ref cell
    // refuses at typecheck now, with the same words. Lowering stays because it
    // is strictly the wider gate: it re-opens the store AND runs everything
    // after the checker, so a cell that breaks in either half is caught here.
    printfn "\n--- demo notebook ---"

    /// The nearest ancestor of the current directory containing `rel`. The
    /// scratch dir is cwd-relative and running two suites from private working
    /// directories is supported, so the repo root is FOUND, not assumed --
    /// and a genuine absence is a SKIP, not a red.
    let rec findUpFrom (dir: DirectoryInfo option) (rel: string) : string option =
        match dir with
        | None -> None
        | Some d ->
            let cand = Path.Combine(d.FullName, rel)
            if File.Exists cand || Directory.Exists cand then Some cand
            else findUpFrom (Option.ofObj d.Parent) rel

    let hereDir = Some (DirectoryInfo (Directory.GetCurrentDirectory()))
    let nbPathOpt = findUpFrom hereDir (Path.Combine("examples", "station_temps.bladenb"))
    let nbStoreOpt = findUpFrom hereDir (Path.Combine("examples", "data", "station_temps.icechunk"))
    (match nbPathOpt, nbStoreOpt with
     | None, _ ->
         printfn "  SKIP notebook: examples/station_temps.bladenb not found above the working directory"
     | _, None ->
         printfn "  SKIP notebook: the committed store examples/data/station_temps.icechunk is not present"
     | Some nbPath, Some nbStore ->
         try
            // The store this block reads is a different repo path from every
            // fixture above, so nothing can answer across -- but the axis mint
            // is per COMPILATION, and the three `lower` calls below want to
            // start from an empty one (the regrid cell's refusal is a SPLIT
            // against the identity the success cells mint).
            resetCaches ()
            // The .bladenb cell grammar, read off the file itself: a line whose
            // trimmed text opens with `// %%` starts a cell, and `[markdown]`
            // on that line makes the cell prose. Everything after it, up to the
            // next such line, is the cell body.
            let cells =
                let acc = ResizeArray<bool * ResizeArray<string>>()
                for line in File.ReadAllLines nbPath do
                    let t = line.TrimStart()
                    if t.StartsWith "// %%" then acc.Add((t.Contains "[markdown]", ResizeArray<string>()))
                    elif acc.Count > 0 then (snd acc.[acc.Count - 1]).Add line
                acc
                |> Seq.map (fun (isMd, body) -> (isMd, String.concat "\n" (List.ofSeq body)))
                |> List.ofSeq
            let codeCells =
                cells |> List.filter (fun (isMd, body) -> not isMd && body.Trim() <> "") |> List.map snd

            // The notebook's store path is relative to the NOTEBOOK's own
            // directory (a cell evaluates there). Rewriting it to the absolute
            // path just located is the same move the e2e section makes with its
            // mirrored fixtures, and it makes this block cwd-independent.
            let atRoot (s: string) =
                s.Replace("\"data/station_temps.icechunk\"",
                          "\"" + nbStore.Replace('\\', '/') + "\"")

            // The two cells the narration says fail: the regrid difference and
            // the ambiguous bare `release`. Identified by their TEXT, so the
            // block cannot silently re-point at a different cell when the
            // notebook is edited.
            let isRefusal (c: string) =
                c.Contains "checkout(\"regrid\")"
                || (c.Contains "checkout(\"release\")" && not (c.Contains "ic.tag"))
            let refusals = codeCells |> List.indexed |> List.filter (fun (_, c) -> isRefusal c)

            check "notebook: the cell split finds the code cells and exactly two refusals"
                (codeCells.Length >= 7 && refusals.Length = 2)
                (sprintf "%d code cells, %d refusals" codeCells.Length refusals.Length)
            check "notebook: a code cell loads the committed store by the documented relative path"
                (codeCells |> List.exists (fun c -> c.Contains "ic.load(\"data/station_temps.icechunk\")"))
                (match codeCells with c :: _ -> c | [] -> "<no code cells>")

            // (a) Every SUCCESS cell, concatenated in order -- which is what
            // the notebook lane itself assembles per evaluation.
            let successSrc =
                codeCells |> List.filter (isRefusal >> not) |> String.concat "\n" |> atRoot
            (match lower successSrc with
             | Ok _ -> check "notebook: the success cells lower against the committed store" true ""
             | Error e -> check "notebook: the success cells lower against the committed store" false e)

            // (b) Each refusal cell, on the prefix of success cells that
            // precedes it -- the state a reader has when they reach it.
            for (idx, cell) in refusals do
                let prefix =
                    codeCells |> List.take idx |> List.filter (isRefusal >> not) |> String.concat "\n"
                let src = atRoot (prefix + "\n" + cell)
                let label = if cell.Contains "regrid" then "regrid" else "ambiguous ref"
                match lower src with
                | Ok _ ->
                    check $"notebook: the '{label}' cell REFUSES" false
                        "the cell lowered, so the notebook's narrated failure no longer happens"
                | Error e ->
                    check $"notebook: the '{label}' cell REFUSES" true ""
                    if label = "regrid" then
                        // The narration promises an index-type mismatch naming
                        // the two axes, not some unrelated error.
                        check "notebook: the regrid cell refuses ON THE AXES"
                            (refusesOnAxes (Error e : Result<unit, string>)) e
                    else
                        // ... and the ambiguous one names both namespaces and
                        // the markers that resolve them.
                        check "notebook: the ambiguous-ref cell names both namespaces and the markers"
                            (e.Contains "ambiguous" && e.Contains "ic.branch" && e.Contains "ic.tag") e
         with ex -> check "notebook: examples/station_temps.bladenb" false ex.Message)

    // ---------------------------------------------------------------
    // 17. PACKED-POOL axis identity (plan §5.3's remaining residual)
    // ---------------------------------------------------------------
    // Section 12 gave every DENSE dimension a repo-scoped identity, which is
    // what makes cross-repo and diverged-axis arithmetic refuse. A PACKED
    // variable's pool axis was not covered by it and could not be: a pool is
    // not a store dimension (its extent is a derived cardinality), so
    // `zarrStoreToModule`'s `sharedDims` deliberately drops it and the
    // `externalDimMap` channel the dense identities ride never reaches it. It
    // was minted `Tag = None`, and BOTH refusal predicates are permissive on
    // None (`indexNamesCoIterable`'s `| _ -> true`; `indexPairIncompatible`
    // falling through to its symmetry arm), so `ca.vars.cov - cb.vars.cov`
    // across two DIFFERENT repos silently typechecked -- the pre-P3 defect,
    // still live for every packed variable.
    //
    // The pool now rides a SECOND hook (`zarrStoreToModuleWith`'s `poolAxis`)
    // into the same mint table, keyed on `__pool:<var>` and fingerprinted over
    // the packed variable's OWN metadata. So it gets the whole §5 story, not
    // just the refusal half: an UNCHANGED packed variable SHARES one identity
    // across two checkouts of a repo exactly as an unchanged dim does.
    printfn "\n--- packed-pool axis identity ---"

    let poolSharedRoot = fixRepo "ic_pool_shared"     // cov UNCHANGED across the two commits
    let poolChangedRoot = fixRepo "ic_pool_changed"   // cov REWRITTEN between them
    let poolTwinRoot = fixRepo "ic_pool_twin"         // a byte-identical SECOND repo

    /// A depth-1 SymIdx<2,4> pool: cardinality C(5,2) = 10, one flat axis, no
    /// trailing dense dims -- the shape whose whole index list is the pool, and
    /// therefore the one the untagged pool made unsound.
    let poolAttrs =
        Some "\"blade\": {\"spec_version\": 1, \"layout\": \"packed\", \"order\": \"ascending-lex\", \"index_types\": [{\"kind\": \"sym\", \"rank\": 2, \"extent\": 4}], \"decomposition\": {\"scheme\": \"flat-ranges\"}}"
    /// An iterated-wreath pool (spec_version 2 'orbit'): levels [(2,+),(2,+)]
    /// at base extent 3, cardinality 21. Elementwise arithmetic over a wreath
    /// operand is refused on its own grounds today (the pool has no traversal
    /// nest outside its own loop), so this fixture exists to pin the TAG -- the
    /// wreath spelling, and its `IxKind` agreement -- not an arithmetic result.
    let orbAttrs =
        Some "\"blade\": {\"spec_version\": 2, \"layout\": \"packed\", \"order\": \"ascending-lex\", \"index_types\": [{\"kind\": \"orbit\", \"levels\": [[2, \"+\"], [2, \"+\"]], \"extent\": 3}]}"

    let covArr (d: float[]) =
        { IW.mkArray "cov" [] [10L] [10L] (IW.IceF64 d) with AttributesJson = poolAttrs }
    let orbArr (d: float[]) =
        { IW.mkArray "w" [] [21L] [21L] (IW.IceF64 d) with AttributesJson = orbAttrs }
    let covV1 = Array.init 10 (fun i -> float (i + 1))
    let covV2 = Array.init 10 (fun i -> float (i + 1) * 2.0)
    let orbV1 = Array.init 21 (fun i -> float (i + 1))
    // A DENSE companion over a named dim with no coordinate array: it is what
    // makes the "cov changed, the dense dim did not" split observable, and it
    // proves the pool's mint-table entry does not collide with a dim's.
    let obsArr (d: float[]) = IW.mkArray "obs" ["r"] [4L] [2L] (IW.IceF64 d)
    let obsV1 = [| 1.0; 2.0; 3.0; 4.0 |]
    let obsV2 = [| 5.0; 6.0; 7.0; 8.0 |]

    /// `cov` and `w` identical in both commits (the same `ArraySpec` value, so
    /// the manifests are shared verbatim); only `obs` moves. The data-only
    /// commit of §5.2, one flavour down.
    let poolSharedSpec (seed: int) : IW.RepoSpec =
        { IW.emptyRepo with
            Seed = seed
            Snapshots = [ IW.mkSnapshot "s1" [ covArr covV1; orbArr orbV1; obsArr obsV1 ]
                          IW.mkSnapshot "s2" [ covArr covV1; orbArr orbV1; obsArr obsV2 ] ]
            Branches = [ ("main", "s2") ]
            Tags = [ ("v1.0", "s1") ] }

    /// The packed variable ITSELF was rewritten: same layout, same cardinality,
    /// different cells. Nothing in the type says so -- only the fingerprint.
    let poolChangedSpec : IW.RepoSpec =
        { IW.emptyRepo with
            Seed = 52
            Snapshots = [ IW.mkSnapshot "s1" [ covArr covV1; obsArr obsV1 ]
                          IW.mkSnapshot "s2" [ covArr covV2; obsArr obsV1 ] ]
            Branches = [ ("main", "s2") ]
            Tags = [ ("v1.0", "s1") ] }

    /// Two checkouts of ONE repo, differencing a PACKED variable.
    let poolCrossCheckoutSrc (root: string) =
        sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
let A = ck1.vars.cov |> ic.read
let B = ck2.vars.cov |> ic.read
let d = B - A
"""
                (root.Replace('\\', '/'))

    (try
        IW.writeRepo poolSharedRoot (poolSharedSpec 51)
        IW.writeRepo poolChangedRoot poolChangedSpec
        // The SAME spec at a different path: identical bytes, and still not the
        // same pool -- there is no identity between two repos to anchor one.
        IW.writeRepo poolTwinRoot (poolSharedSpec 51)
        resetCaches ()

        // (a) THE POSITIVE CONTROL, first: a packed variable used with ITSELF
        // inside one checkout still works. A tag that refused here would be a
        // regression dressed up as soundness.
        (let selfSrc =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
let A = ck.vars.cov |> ic.read
let d = A - A
let e = A * 2.0
let cell = A(1, 2)
"""
                    (poolSharedRoot.Replace('\\', '/'))
         let (r, warns) = Blade.Lowering.lowerCaptured selfSrc
         check "pool: same-checkout packed self-arithmetic still TYPECHECKS"
             (match r with Ok _ -> true | Error _ -> false)
             (match r with Error e -> e | Ok _ -> "")
         // The pool tag is `__`-prefixed for the same three seams the dense
         // axis tag is (`checkArrayIndexTags`, `elemTypeForIterationIndex`,
         // `Ide.indexNamesOf`), so a plain packed subscript stays quiet.
         check "pool: a tagged pool does not make packed subscripting noisy (no BL4003)"
             (not (warns |> List.exists (fun (d: Blade.Diagnostics.Diagnostic) -> d.Code = "BL4003")))
             (warns |> List.map (fun d -> d.Code + ": " + d.Message) |> String.concat "; "))

        // (b) THE HOLE, closed. Two DIFFERENT repos, byte-identical stores,
        // same layout, same cardinality -- and no shared pool. Before the pool
        // carried a tag this program typechecked.
        (let crossRepoSrc =
            sprintf """
import icechunk as ic

let r1 = ic.load("%s")
let r2 = ic.load("%s")
let ck1 = r1.checkout("main")
let ck2 = r2.checkout("main")
let A = ck1.vars.cov |> ic.read
let B = ck2.vars.cov |> ic.read
let d = B - A
"""
                    (poolSharedRoot.Replace('\\', '/')) (poolTwinRoot.Replace('\\', '/'))
         let r = lower crossRepoSrc
         check "pool: differencing a PACKED variable ACROSS REPOS is REFUSED"
             (refusesOnAxes r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked, so two repos' pools co-iterated"))

        // (c) One repo, a commit that rewrote the packed variable: a diverged
        // pool, refused exactly as a diverged dense axis is.
        (let r = lower (poolCrossCheckoutSrc poolChangedRoot)
         check "pool: differencing across a REWRITTEN packed variable is REFUSED"
             (refusesOnAxes r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked, so a rewritten pool co-iterated with its older self"))
        check "pool: the rewrite records a SPLIT, and says what moved"
            (match poolIdentities poolChangedRoot "cov" with
             | newest :: _ :: [] -> newest.SplitReason = Some "packed variable content differs"
             | _ -> false)
            (sprintf "%A" (poolIdentities poolChangedRoot "cov" |> List.map (fun i -> i.SplitReason)))
        check "pool: the split is PER-VARIABLE -- the untouched dense dim still shares"
            ((axisIdentities poolChangedRoot "r").Length = 1)
            (sprintf "%A" (axisIdentities poolChangedRoot "r" |> List.map (fun i -> i.IndexType.Tag)))

        // (d) THE SHARING HALF. An UNCHANGED packed variable is one identity
        // across both checkouts -- the §5.2 rule, with the variable's own
        // content standing in for a coordinate it does not have.
        (match lower (poolCrossCheckoutSrc poolSharedRoot) with
         | Ok _ ->
             check "pool: differencing an UNCHANGED packed variable across checkouts TYPECHECKS" true ""
         | Error e ->
             check "pool: differencing an UNCHANGED packed variable across checkouts TYPECHECKS" false e)
        // ONE identity, and BOTH refs presented it. The ref spellings are left
        // out of the claim: these programs write bare `checkout("main")`, whose
        // refText is the unresolved-marker form, and the point here is the
        // count -- one identity, two presenters -- not how a ref was spelled.
        check "pool: an unchanged pool records ONE identity, presented by both refs"
            (match poolIdentities poolSharedRoot "cov" with
             | [ one ] ->
                 one.Refs.Length = 2 && List.contains "tag:v1.0" one.Refs && one.SplitReason.IsNone
             | _ -> false)
            (sprintf "%A" (poolIdentities poolSharedRoot "cov" |> List.map (fun i -> (i.Refs, i.SplitReason))))

        // (e) The tag SHAPE. Two spellings, because the Tag doubles as the
        // record's kind sentinel: a depth-1 simplex pool is IxKPlain and takes
        // `__icpool|`, an iterated-wreath pool is IxKOrbit (it would otherwise
        // carry "__orbidx") and takes `__icpoolorb|`. `ixKindOfTag` maps each
        // back, which is what IRValidate's Tag/IxKind agreement requires.
        check "pool: a simplex pool is tagged `__icpool|<var>@<repo>:<digest>`"
            (match poolIdentities poolSharedRoot "cov" with
             | [ one ] ->
                 (defaultArg one.IndexType.Tag "").StartsWith (providerPoolTagPrefix + "cov@")
                 && one.IndexType.Id >= axisIdBase
             | _ -> false)
            (sprintf "%A" (poolIdentities poolSharedRoot "cov" |> List.map (fun i -> i.IndexType.Tag)))
        check "pool: a WREATH pool takes the orbit spelling, and its kind agrees"
            (match poolIdentities poolSharedRoot "w" with
             | [ one ] ->
                 (defaultArg one.IndexType.Tag "").StartsWith (providerOrbPoolTagPrefix + "w@")
                 && one.IndexType.IxKind = IxKOrbit
                 && ixKindOfTag one.IndexType.Tag = one.IndexType.IxKind
             | _ -> false)
            (sprintf "%A" (poolIdentities poolSharedRoot "w"
                           |> List.map (fun i -> (i.IndexType.Tag, i.IndexType.IxKind))))
        check "pool: both spellings are recognised as pool provenance, and `__orbidx` is not"
            (isProviderPoolTag providerPoolTagPrefix
             && isProviderPoolTag providerOrbPoolTagPrefix
             && not (isProviderPoolTag "__orbidx")
             && not (isProviderPoolTag axisTagPrefix)) ""
        // A pool tag is provenance, not a display name: `tryAxisTagName` leaves
        // it alone ON PURPOSE, so `Ide.indexNamesOf` never picks it up and
        // `ppIndexTypeIn` keeps printing the class (`SymIdx<2, 4>`) rather than
        // substituting a bare name for the whole compact spelling.
        check "pool: a pool tag is not an AXIS tag, and decodes to no display name"
            ((tryAxisTagName (providerPoolTagPrefix + "cov@ic_pool:0f3a")).IsNone
             && (tryAxisTagName (providerOrbPoolTagPrefix + "w@ic_pool:0f3a")).IsNone) ""

        // (f) The pool's mint-table entry is its own: a variable named after a
        // dimension must not answer for that dimension, and vice versa.
        check "pool: a pool identity lives under its own key, beside the dims"
            ((axisIdentities poolSharedRoot "cov").IsEmpty
             && (poolIdentities poolSharedRoot "r").IsEmpty
             && (axisIdentities poolSharedRoot "r").Length = 1
             && (poolIdentities poolSharedRoot "cov").Length = 1) ""
        check "pool: two DIFFERENT repos mint separate pool identities"
            ((poolIdentities poolTwinRoot "cov").Length = 1
             && (poolIdentities poolSharedRoot "cov").Length = 1
             && (poolIdentities poolTwinRoot "cov" |> List.map (fun i -> i.IndexType.Id))
                <> (poolIdentities poolSharedRoot "cov" |> List.map (fun i -> i.IndexType.Id)))
            (sprintf "%A vs %A"
                 (poolIdentities poolTwinRoot "cov" |> List.map (fun i -> i.IndexType.Tag))
                 (poolIdentities poolSharedRoot "cov" |> List.map (fun i -> i.IndexType.Tag)))
     with ex -> check "pool: packed-pool axis identity" false ex.Message)

    // ZARR DID NOT MOVE. The pool hook is an OPTION threaded exactly the way
    // `externalDimMap` is, and the four-argument `zarrStoreToModule` -- every
    // plain Zarr call site -- passes it absent. A plain store's pool record must
    // still come out with `Tag = None`, or the zarr lane's packed tests are
    // asserting a type this change invented.
    (let zarrPacked : Blade.ZarrProvider.ZarrStore =
        { Path = "/tmp/zpool"; Version = 3
          Arrays =
            [ { Name = "tri"; ArrayDir = "/tmp/zpool/tri"; Shape = [10L]; Chunks = [10L]
                Dtype = { Code = "f8"; Elem = ETFloat64; ByteSize = 8; IsFloat = true }
                DimNames = None; FillValue = Blade.ZarrProvider.FillFloat 0.0
                Codec = Blade.ZarrProvider.CodecIdentity
                Blade = Some { Group = { Sym = SymSymmetric; Rank = 2; Extent = 4L; Levels = [] }
                               DenseDims = []; Blocks = None }
                Version = 3; ChunkKeySep = "/"; ChunkKeyPrefix = "c" } ] }
     let zm = Blade.ZarrProvider.zarrStoreToModule (IRBuilder()) "z" zarrPacked None
     let poolRec =
        zm.Types |> List.tryPick (function
            | IRTDStruct ("z__vars", fs) ->
                fs |> List.tryPick (fun (n, t) ->
                    match n, t with
                    | "tri", ArrayElem at -> List.tryHead at.IndexTypes
                    | _ -> None)
            | _ -> None)
     check "pool: a PLAIN Zarr store's pool axis is still untagged (the hook defaults absent)"
         (match poolRec with
          | Some ix -> ix.Tag = None && ix.IxKind = IxKPlain && ix.Symmetry = SymSymmetric
          | None -> false)
         (sprintf "%A" poolRec))

    // ---------------------------------------------------------------
    // 18. Axis provenance survives a TYPE ALIAS (plan §5)
    // ---------------------------------------------------------------
    // Sections 12 and 17 make a diverged axis refuse where the difference is
    // WRITTEN -- `ck2.vars.temp - ck1.vars.temp`. That refusal was reachable
    // around: an ANNOTATION re-tagged the operands before the arithmetic
    // ever saw them.
    //
    //     type Lat = ck1.index.lat
    //     let A: Array<Float64 like Lat, Lon> = ck1.vars.temp |> ic.read
    //     let B: Array<Float64 like Lat, Lon> = ck2.vars.temp |> ic.read
    //     let d = B - A                       // typechecked. It should not.
    //
    // TWO independent seams let that through, and both are pinned below.
    //
    //   (1) `registerTypeDecl`'s alias arm rebuilt the record by re-lowering
    //       the referenced SURFACE body, and for `<binding>.index.<dim>` that
    //       body is a synthesized `TyIdx <extent>` (`registerProviderModule`
    //       stores the real record beside it because there IS no surface
    //       syntax for "the axis this store minted"). So the alias came out an
    //       anonymous `Idx<5>` re-tagged with the alias name, and the
    //       `__icaxis|` identity was gone before unify was asked anything.
    //       Now the alias ADOPTS the provider record, tag included -- the
    //       fifth carve-out beside the irreps/wreath/multi-rank ones.
    //
    //   (2) `unify`'s `indexPairIncompatible` exempted every `__` tag as
    //       "synthetic, structural, never gates". Provider tags are `__`
    //       ONLY so the four seams that read a Tag as a user-facing name leave
    //       them alone; they are IDENTITIES, and `gatesNominally` now says so.
    //       Without this the ascription still passed even with (1) fixed --
    //       it would simply have relabelled the operands one layer later.
    //
    // The same pair FIXES a refusal that should never have fired: inside ONE
    // checkout, an aliased array beside the raw one carried 'Lat' against
    // '__icaxis|lat@...' and earned a BL3999 that the identical Zarr program
    // (untagged axes) never sees. Same axis, same tag, co-iterates.
    printfn "\n--- axis provenance through a type alias ---"

    // Section 12's discipline: this section's fixtures are its own repo roots,
    // so no earlier section's minted identities can answer for them.
    let launderRoot = fixRepo "ic_launder"          // lat rewritten, SAME extent
    let launderTwinRoot = fixRepo "ic_launder_twin" // a byte-identical SECOND repo

    /// The refusal a laundered axis earns. Which seam fires first depends on
    /// the shape: an ASCRIPTION refuses in `unify` (BL3001 "Type mismatch"),
    /// a bare difference refuses in co-iteration (BL3999). Both are the same
    /// fact and either one is a pass here -- what is pinned is that the
    /// program does not compile, and that it failed on the index types.
    let refusesOnAxesOrAscription (r: Result<'a, string>) : bool =
        match r with
        | Error e ->
            refusesOnAxes (Error e : Result<unit, string>) || e.Contains "Type mismatch"
        | Ok _ -> false

    /// Two checkouts' rank-2 `temp`, both ascribed to ONE axis alias spelled
    /// through ck1. The alias is the whole trick: without it this is section
    /// 12's `crossCheckoutSrc`, which already refused.
    let launderRank2Src (root: string) =
        sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
type Lat = ck1.index.lat
type Lon = ck1.index.lon
let A: Array<Float64 like Lat, Lon> = ck1.vars.temp |> ic.read
let B: Array<Float64 like Lat, Lon> = ck2.vars.temp |> ic.read
let d = B - A
let total = reduce(d, (+), axes = 2)
"""
                root

    (try
        IW.writeRepo launderRoot coordRewriteSpec
        IW.writeRepo launderTwinRoot coordRewriteSpec
        resetCaches ()

        // (a) THE HOLE, closed, at rank 2.
        (let r = lower (launderRank2Src launderRoot)
         check "launder: ascribing two DIVERGED checkouts to ONE axis alias is REFUSED"
             (refusesOnAxesOrAscription r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked, so an annotation relabelled two diverged axes into one"))

        // (b) ... and at rank 1, over the coordinate array itself. Rank 1 has
        // its own agreement predicate (`indexNamesCoIterable`, not the rank->=2
        // `indexRecordsAgree` product rule), so it is a separate obligation and
        // not a corollary of (a).
        (let src =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
type Lat = ck1.index.lat
let a: Array<Float64 like Lat> = ck1.dims.lat |> ic.read
let b: Array<Float64 like Lat> = ck2.dims.lat |> ic.read
let d = b - a
let total = reduce(d, (+))
"""
                    launderRoot
         let r = lower src
         check "launder: the rank-1 (coordinate) shape is refused too"
             (refusesOnAxesOrAscription r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked at rank 1"))

        // (c) Two DIFFERENT repos, byte-identical stores, one alias. The
        // cross-repo refusal section 12 pins, walked around the same way.
        (let src =
            sprintf """
import icechunk as ic

let r1 = ic.load("%s")
let r2 = ic.load("%s")
let ck1 = r1.checkout("main")
let ck2 = r2.checkout("main")
type Lat = ck1.index.lat
type Lon = ck1.index.lon
let A: Array<Float64 like Lat, Lon> = ck1.vars.temp |> ic.read
let B: Array<Float64 like Lat, Lon> = ck2.vars.temp |> ic.read
let d = B - A
"""
                    launderRoot launderTwinRoot
         let r = lower src
         check "launder: two REPOS' arrays through one alias are REFUSED"
             (refusesOnAxesOrAscription r)
             (match r with
              | Error e -> e
              | Ok _ -> "the program typechecked across two repos"))

        // (d) The alias need not come from the store at all -- a USER-DECLARED
        // index type of the right extent was the shortest laundering route,
        // and it is now refused at the FIRST ascription. This is the rule the
        // language already applies between two user-named types (`type A =
        // Idx<5>` does not adopt a `B`-tagged array); a store-minted axis is a
        // name too, so it joins that rule rather than sitting outside it. The
        // ways to name a checkout axis are `ck.index.<dim>` -- pinned in (f) --
        // or, if two axes really ARE one, to drop the annotation.
        (let src =
            sprintf """
import icechunk as ic

type Lat = Idx<5>
type Lon = Idx<4>

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
let A: Array<Float64 like Lat, Lon> = ck1.vars.temp |> ic.read
let B: Array<Float64 like Lat, Lon> = ck2.vars.temp |> ic.read
let d = B - A
"""
                    launderRoot
         let r = lower src
         check "launder: a USER-DECLARED index type cannot adopt a store axis either"
             (refusesOnAxesOrAscription r)
             (match r with
              | Error e -> e
              | Ok _ -> "a hand-written Idx<5> adopted two diverged store axes"))

        // (e) THE FALSE REFUSAL, fixed. One checkout, one axis: the aliased
        // array and the raw one are the same axis and must co-iterate. This is
        // the shape that made icechunk STRICTER than Zarr for a sound program
        // -- a Zarr axis is untagged, so `Some "Lat"` vs `None` was permissive
        // there while `Some "Lat"` vs `Some "__icaxis|lat@..."` refused here.
        (let src =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
type Lat = ck.index.lat
let lats = ck.dims.lat |> ic.read
let w: Array<Float64 like Lat> = lats
let z = w * lats
let total = reduce(z, (+))
"""
                    launderRoot
         let r = lower src
         check "alias: an aliased axis co-iterates with the RAW one in the same checkout"
             (match r with Ok _ -> true | Error _ -> false)
             (match r with
              | Error e -> e
              | Ok _ -> ""))

        // (f) POSITIVE CONTROL, restated so this section stands alone: the
        // rank-2 same-checkout ascription section 12 pins is still accepted,
        // and still quiet. Same checkout means one identity, so the alias
        // carries the tag the array carries.
        (let src =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
let A = ck.vars.temp |> ic.read
type Lat = ck.index.lat
type Lon = ck.index.lon
let cell = A(2, 1)
let B: Array<Float64 like Lat, Lon> = A
let total = reduce(B, (+), axes = 2)
"""
                    launderRoot
         let (r, warns) = Blade.Lowering.lowerCaptured src
         check "alias: the same-checkout rank-2 ascription still typechecks, still quiet"
             ((match r with Ok _ -> true | Error _ -> false)
              && not (warns |> List.exists (fun (d: Blade.Diagnostics.Diagnostic) -> d.Code = "BL4003")))
             (match r with
              | Error e -> e
              | Ok _ -> warns |> List.map (fun d -> d.Code + ": " + d.Message) |> String.concat "; "))

        // (g) KNOWN HOLE, pinned rather than fixed: a plain-`unify` FUNCTION
        // BOUNDARY still accepts a diverged axis. The plan's P3 outcome names
        // this residual already; what (h) adds is the measurement that it is
        // NOT a provider defect. Closing it means
        // changing how EVERY named index type is matched at an argument
        // position, which is a language-wide change to the direct-application
        // seam, not a fix to axis provenance. If this ever starts REFUSING,
        // that is good news: delete this pin and keep the one in (h).
        (let src =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
type Lat = ck1.index.lat
type Lon = ck1.index.lon

function total_of(x: Array<Float64 like Lat, Lon>) -> Float64 = reduce(x, (+), axes = 2)

let t = total_of(ck2.vars.temp |> ic.read)
"""
                    launderRoot
         let r = lower src
         check "hole: a function BOUNDARY still accepts a diverged axis (known, pinned)"
             (match r with Ok _ -> true | Error _ -> false)
             (match r with Error e -> "now refuses -- see the comment: " + e | Ok _ -> ""))

        // (h) ... and the same laxity with NO provider in the program at all.
        // This is the evidence for (g)'s claim: an argument position accepts a
        // differently-named index type of equal extent, while the identical
        // LET ascription refuses. Two seams, two answers -- the pre-existing
        // unify-strictness split, which axis provenance rides along with
        // rather than causes.
        (let boundarySrc = """
type A = Idx<5>
type B = Idx<5>

function total_of(x: Array<Float64 like A>) -> Float64 = reduce(x, (+))

let b: Array<Float64 like B> = [1.0, 2.0, 3.0, 4.0, 5.0]
let r = total_of(b)
"""
         let ascribeSrc = """
type A = Idx<5>
type B = Idx<5>

let b: Array<Float64 like B> = [1.0, 2.0, 3.0, 4.0, 5.0]
let a: Array<Float64 like A> = b
"""
         check "hole: the boundary laxity is NOT provider-specific (two plain named axes do it too)"
             ((match lower boundarySrc with Ok _ -> true | Error _ -> false)
              && (match lower ascribeSrc with Error _ -> true | Ok _ -> false))
             (sprintf "boundary %A / ascription %A"
                  (match lower boundarySrc with Ok _ -> "Ok" | Error e -> e)
                  (match lower ascribeSrc with Ok _ -> "Ok" | Error e -> e)))
     with ex -> check "launder: axis provenance through a type alias" false ex.Message)

    // The two predicates, driven directly. The source programs above pin the
    // CONSEQUENCE; these pin the RULE, including the arms that must stay
    // permissive -- a tightening here is the failure mode with real blast
    // radius, since `indexPairIncompatible` is every array unification in the
    // language.
    (let mkIx (tag: string option) (extent: int64) : IRIndexType =
        { Id = 0; Rank = 1; Extent = IRLit (IRLitInt extent); Symmetry = SymNone
          Tag = tag; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
     let icA = Some (axisTagPrefix + "lat@ic_a:0f3a1c2b4d5e6f70")
     let icB = Some (axisTagPrefix + "lat@ic_a:0f3a1c2b4d5e6f70#2")
     let incompat a b = Blade.Unify.indexPairIncompatible (mkIx a 5L) (mkIx b 5L)
     check "predicate: two DIFFERENT provider axis tags are incompatible at unify"
         (incompat icA icB) ""
     check "predicate: the SAME provider axis tag is compatible"
         (not (incompat icA icA)) ""
     check "predicate: a provider tag against a USER name is incompatible"
         (incompat icA (Some "Lat") && incompat (Some "Lat") icA) ""
     // The escape hatch BL3999's own message advertises, and the one route by
     // which a user can still assert that two diverged axes are one axis.
     check "predicate: a provider tag against an UNTAGGED record stays compatible"
         (not (incompat icA None) && not (incompat None icA)) ""
     // Other `__` tags are KIND SENTINELS, not identities. Gating them would
     // refuse sound code across the whole language, so they keep the exemption.
     check "predicate: other `__` sentinels keep the synthetic exemption"
         (not (incompat icA (Some "__orbidx"))
          && not (incompat (Some "__orbidx") (Some "__sparseidx"))) ""
     check "predicate: two user names still gate (unchanged)"
         (incompat (Some "Lat") (Some "Lon")) ""
     // The family predicate itself: all three provider spellings, and nothing
     // that merely starts with `__`.
     check "predicate: isProviderAxisTag covers the dense axis and both pool spellings"
         (isProviderAxisTag (axisTagPrefix + "lat@r:00")
          && isProviderAxisTag (providerPoolTagPrefix + "cov@r:00")
          && isProviderAxisTag (providerOrbPoolTagPrefix + "w@r:00")
          && not (isProviderAxisTag "__orbidx")
          && not (isProviderAxisTag "__sparseidx")
          && not (isProviderAxisTag "Lat")) ""
     // One spelling, one place: `IcechunkProvider.axisTagPrefix` re-exports
     // `Types.providerAxisTagPrefix` so the minting site and the two typecheck
     // predicates cannot drift onto different literals.
     check "predicate: the provider re-exports the shared axis-tag prefix"
         (axisTagPrefix = providerAxisTagPrefix && axisTagPrefix = "__icaxis|") axisTagPrefix
     // Rank 1's own predicate, previously refusing an alias of the SAME axis.
     // Same tag co-iterates, different tags do not, untagged is permissive
     // on either side.
     let coIter a b = Blade.TypeLower.indexNamesCoIterable (mkIx a 5L) (mkIx b 5L)
     check "predicate: rank-1 co-iteration agrees on one provider tag and refuses two"
         (coIter icA icA && not (coIter icA icB) && coIter icA None && coIter None icA) "")

    // ---------------------------------------------------------------
    // 19. The GOLDEN repo -- bytes icechunk-python wrote, not bytes we did
    // ---------------------------------------------------------------
    // Every section above reads a repo `IcechunkWrite` produced. That is a real
    // cross-check of Blade's two halves against EACH OTHER -- the writer and the
    // reader spell out the magic bytes, the 39-byte header layout and the
    // Crockford encoder independently -- but it is not a check against the
    // FORMAT. Both halves were transcribed from the same spec by the same
    // author, so a header belief that is wrong but CONSISTENT passes all of
    // sections 1-18 and fails on the first real repo anyone points at Blade.
    //
    // `tests/fixtures/icechunk_repos/golden_py/` closes that. It is the one
    // repo in the tree that is COMMITTED rather than generated, because Blade
    // cannot generate it: icechunk-python 2.1.2 wrote it (spec v2, local FS,
    // two commits on `main`, tag `v1.0`, uncompressed `bytes` chunk codec,
    // native chunk files). `make_golden_py.py` beside it is its provenance and
    // is run BY HAND -- nothing in the build or this suite invokes Python.
    //
    // NO OBJECT ID IS PINNED HERE. icechunk mints snapshot ids randomly, so the
    // fixture is a capture, not a reproducible build; every snapshot is reached
    // through `main` or the `v1.0` tag instead. What IS pinned is the array
    // values, chosen in the generator to be checkable by eye (see its
    // docstring, and the transcription below).
    printfn "\n--- golden repo (icechunk-python 2.1.2) ---"

    let goldenOpt =
        findUpFrom hereDir (Path.Combine("tests", "fixtures", "icechunk_repos", "golden_py"))
    (match goldenOpt with
     | None ->
         printfn "  SKIP golden: tests/fixtures/icechunk_repos/golden_py is not present"
     | Some golden ->
     try
        // The generator's constants, transcribed from make_golden_py.py. They
        // are literals there and literals here ON PURPOSE: if the script is
        // edited and the fixture is not regenerated (or the other way round),
        // the two disagree and this section says so.
        //     lat  = [10, 20, 30, 40]                      sum  100.0
        //     lon  = [100, 101, 102, 103, 104]             sum  510.0
        //     temp(i, j) = 100*i + j       (tag v1.0)      sum 3040.0
        //     temp(i, j) = 100*i + j + 1   (branch main)   sum 3060.0
        let gLat = [| 10.0; 20.0; 30.0; 40.0 |]
        let gLon = [| 100.0; 101.0; 102.0; 103.0; 104.0 |]
        let gTempV1 = Array.init 20 (fun k -> 100.0 * float (k / 5) + float (k % 5))
        let gTempMain = gTempV1 |> Array.map ((+) 1.0)

        // (a) THE HEADER, WITH NO DECODER IN THE LOOP -------------------------
        // The narrowest and most valuable claim in this section: the 39 bytes
        // the reference stamps on a metadata file are the 39 bytes BOTH Blade
        // halves believe in. Section 1 pins writer-constants against
        // reader-constants; this pins that agreed pair against the reference.
        // It reads raw bytes, so it stands whatever the payload decoder does.
        let firstFileIn (sub: string) =
            Directory.GetFiles (Path.Combine(golden, sub)) |> Array.sort |> Array.last
        let headerOf (path: string) = Array.sub (File.ReadAllBytes path) 0 IW.headerSize
        let repoHdr = headerOf (Path.Combine(golden, "repo"))
        let snapHdr = headerOf (firstFileIn "snapshots")
        let manHdr = headerOf (firstFileIn "manifests")

        check "golden: the reference's magic bytes are the ones BOTH Blade halves write and read"
            (Array.sub repoHdr 0 IW.magicBytes.Length = IW.magicBytes
             && Array.sub repoHdr 0 magicBytes.Length = magicBytes)
            (Array.sub repoHdr 0 12 |> Array.map (sprintf "%02x") |> String.concat " ")
        check "golden: the implementation-name field is 24 space-padded bytes"
            (IW.implNameSize = implNameSize
             && Text.Encoding.UTF8.GetString(repoHdr, 12, implNameSize).TrimEnd(' ') = "ic-2.1.2")
            (Text.Encoding.UTF8.GetString(repoHdr, 12, 24))
        // The FILE-TYPE bytes. Blade's are hand-transcribed enum values that
        // nothing outside Blade had ever confirmed.
        check "golden: the file-type bytes are RepoInfo=6, Snapshot=1, Manifest=2"
            (repoHdr.[37] = IW.ftRepoInfo && snapHdr.[37] = IW.ftSnapshot
             && manHdr.[37] = IW.ftManifest)
            (sprintf "repo %d, snapshot %d, manifest %d" repoHdr.[37] snapHdr.[37] manHdr.[37])
        check "golden: the spec byte is 2, the version this reader accepts"
            (repoHdr.[36] = 2uy && snapHdr.[36] = 2uy && manHdr.[36] = 2uy)
            (sprintf "%d / %d / %d" repoHdr.[36] snapHdr.[36] manHdr.[36])
        check "golden: metadata is zstd-framed (compression byte 1), as real writers do"
            (repoHdr.[38] = IW.compZstd) (sprintf "%d" repoHdr.[38])
        // ... and the reader's own header parser, over reference bytes.
        (match parseHeader "golden repo file" (File.ReadAllBytes (Path.Combine(golden, "repo"))) with
         | Error e -> check "golden: the reader's parseHeader accepts the reference header" false e
         | Ok h ->
             check "golden: the reader's parseHeader accepts the reference header"
                 (h.SpecVersion = 2 && h.FileType = FtRepoInfo && h.Compression = CompZstd
                  && h.Implementation = "ic-2.1.2")
                 (sprintf "%s spec %d %A %A" h.Implementation h.SpecVersion h.FileType h.Compression))
        // File NAMES: 20-character Crockford base32, same alphabet as ours.
        check "golden: object files are named in the same 20-char Crockford base32"
            (Directory.GetFiles (Path.Combine(golden, "snapshots"))
             |> Array.forall (fun p ->
                 let n = Path.GetFileName p
                 n.Length = 20 && n |> Seq.forall (fun c -> IW.base32Alphabet.IndexOf c >= 0)))
            (Directory.GetFiles (Path.Combine(golden, "snapshots"))
             |> Array.map Path.GetFileName |> Array.sort |> sprintf "%A")

        // (b) THE READ-THROUGH ------------------------------------------------
        // Everything from here needs the payload decoder. It is guarded on the
        // load so that ONE root cause reports ONCE: a decode refusal here is a
        // single loud failure plus skipped downstream assertions, not thirty
        // reds all restating the same byte.
        match load golden with
        | Ok (LoadedCheckout _) ->
            check "golden: the reference repo LOADS as a repo handle" false
                "a bare path returned a checkout"
        | Error e ->
            check "golden: the reference repo LOADS as a repo handle" false e
            printfn "  SKIP golden (b): the remaining %s assertions need a decoded payload" "read-through"
        | Ok (LoadedRepo h) ->
            check "golden: the reference repo LOADS as a repo handle" true ""
            check "golden: the header names the REFERENCE writer, not ours"
                (h.Header.Implementation = "ic-2.1.2" && h.Header.SpecVersion = 2)
                h.Header.Implementation
            check "golden: the refs enumerate -- branch 'main' and tag 'v1.0', separate namespaces"
                (List.map fst h.Info.Branches = ["main"] && List.map fst h.Info.Tags = ["v1.0"]
                 && h.Info.DeletedTags = [])
                (sprintf "branches %A tags %A" (List.map fst h.Info.Branches) (List.map fst h.Info.Tags))
            // THREE, not two: the reference mints an initial "Repository
            // initialized" commit when the repo is created, and its id is the
            // well-known 1CECHNKREP0F1RSTCMT0 that sections 2-3 use as their
            // base32 example. A reader that assumed a repo's snapshots are
            // exactly its user's commits would be wrong here.
            check "golden: all three snapshots are listed, the init commit included"
                (h.Info.Snapshots.Length = 3
                 && h.Info.Snapshots |> List.exists (fun s -> base32Encode s.Id = "1CECHNKREP0F1RSTCMT0"))
                (sprintf "%A" (h.Info.Snapshots |> List.map (fun s -> (base32Encode s.Id, s.Message))))
            check "golden: the status decodes as Online" (h.Info.Status = StatusOnline)
                (statusName h.Info.Status)
            let mainId = resolveRef h.Info RefBranch "main"
            let tagId = resolveRef h.Info RefTag "v1.0"
            check "golden: both refs resolve, to DIFFERENT snapshots"
                (match mainId, tagId with
                 | Ok a, Ok b -> a <> b
                 | _ -> false)
                (sprintf "main %s / v1.0 %s" (errorText mainId) (errorText tagId))
            check "golden: the bare form resolves 'main' uniquely (no branch/tag collision here)"
                (resolveRef h.Info RefBare "main" = mainId) (errorText (resolveRef h.Info RefBare "main"))

            // --- the two checkouts ------------------------------------------
            let mainKeyG = golden + "@branch:main"
            let tagKeyG = golden + "@tag:v1.0"
            let checkoutOf (key: string) =
                match load key with
                | Ok (LoadedCheckout ck) -> Some ck
                | _ -> None
            (match checkoutOf mainKeyG, checkoutOf tagKeyG with
             | Some ckMain, Some ckV1 ->
                 check "golden: both refs check out" true ""
                 check "golden: the checkout's root-level arrays are lat, lon, temp"
                     (List.sort (arrayNames ckMain) = ["lat"; "lon"; "temp"])
                     (sprintf "%A" (arrayNames ckMain))
                 check "golden: the reference writes a root GROUP node, and it is not a variable"
                     (ckMain.Snapshot.Nodes |> List.exists (fun n -> n.Path = "/" && n.Kind = NodeGroup))
                     (sprintf "%A" (ckMain.Snapshot.Nodes |> List.map (fun n -> (n.Path, n.Kind))))

                 // Array METADATA, against what the generator asked zarr for.
                 // The reference's zarr.json is pretty-printed, orders its keys
                 // differently from ours and carries a `storage_transformers`
                 // field IcechunkWrite never writes -- none of which should
                 // matter to a real JSON parser, and this is where that stops
                 // being an assumption.
                 let metaCheck (label: string) (v: string) (shape: int64 list)
                               (chunks: int64 list) (dims: string list) =
                     match findArray ckMain v with
                     | Error e -> check $"golden: {label} metadata decodes" false e
                     | Ok (node, meta) ->
                         check $"golden: {label} metadata decodes" true ""
                         check $"golden: {label} shape and chunk grid"
                             (meta.Shape = shape && meta.Chunks = chunks)
                             (sprintf "%A / %A" meta.Shape meta.Chunks)
                         check $"golden: {label} is float64"
                             (meta.Dtype.Elem = ETFloat64) meta.Dtype.Code
                         // Both spellings, and they must agree: the JSON's
                         // `dimension_names` and the structural copy icechunk
                         // records in ArrayNodeData.
                         check $"golden: {label} dimension names agree in the JSON and the structure"
                             (meta.DimNames = Some dims && node.DimensionNames = Some dims)
                             (sprintf "json %A / struct %A" meta.DimNames node.DimensionNames)
                 metaCheck "temp" "temp" [4L; 5L] [2L; 5L] ["lat"; "lon"]
                 metaCheck "lat" "lat" [4L] [4L] ["lat"]
                 metaCheck "lon" "lon" [5L] [5L] ["lon"]

                 // THE §5.2 PREMISE, OBSERVED IN THE REFERENCE. Blade's axis
                 // identity rests on two claims about how a real writer behaves
                 // across a data-only commit: node ids stay put, and an
                 // untouched array's manifest is REUSED rather than rewritten.
                 // Until this fixture existed, both were only ever demonstrated
                 // by the fixture writer that was built to demonstrate them.
                 let nodeOf (c: CheckoutHandle) (n: string) =
                     match findArray c n with Ok (node, _) -> Some node | Error _ -> None
                 let idOf c n = nodeOf c n |> Option.map (fun x -> base32Encode x.Id)
                 let manOf c n =
                     nodeOf c n |> Option.map (fun x -> x.ManifestRefs |> List.map (fst >> base32Encode))
                 check "golden: the reference keeps a node's id across snapshots (the spec's own rule)"
                     ([ "lat"; "lon"; "temp" ] |> List.forall (fun n -> idOf ckMain n = idOf ckV1 n))
                     (sprintf "%A vs %A"
                          ([ "lat"; "lon"; "temp" ] |> List.map (idOf ckMain))
                          ([ "lat"; "lon"; "temp" ] |> List.map (idOf ckV1)))
                 check "golden: a data-only commit REUSES the untouched arrays' manifests"
                     (manOf ckMain "lat" = manOf ckV1 "lat" && manOf ckMain "lon" = manOf ckV1 "lon")
                     (sprintf "lat %A vs %A" (manOf ckMain "lat") (manOf ckV1 "lat"))
                 check "golden: ... and mints a fresh manifest for the array that was rewritten"
                     (manOf ckMain "temp" <> manOf ckV1 "temp" && (manOf ckMain "temp").IsSome)
                     (sprintf "temp %A vs %A" (manOf ckMain "temp") (manOf ckV1 "temp"))
             | _ ->
                 check "golden: both refs check out" false
                     "one of @branch:main / @tag:v1.0 did not load as a checkout")

            // --- the VALUES -------------------------------------------------
            // The whole point: numbers that came off disk through a chunk table
            // no Blade code wrote. Chunks are native files here (the generator
            // sets inline_chunk_threshold_bytes=0), so this exercises the
            // chunk-id + offset + length path end to end.
            let readsGolden (key: string) (v: string) (expected: float[]) (dims: int list)
                            (total: float) (label: string) =
                match spec.ReadVarData key v with
                | Error e -> check $"golden read: {label}" false e
                | Ok data ->
                    check $"golden read: {label} -- dim lengths" (data.DimLengths = dims)
                        (sprintf "%A" data.DimLengths)
                    match data.Payload with
                    | Blade.ProviderRegistry.PFloats got ->
                        check $"golden read: {label} -- every cell"
                            (got.Length = expected.Length
                             && Array.forall2 (fun (a: float) (b: float) -> a = b) got expected)
                            (if got.Length <> expected.Length then $"{got.Length} vs {expected.Length} values"
                             else sprintf "%A" (Array.truncate 8 got))
                        // The sum is pinned SEPARATELY and as a round literal:
                        // it is the number a human can check against the
                        // generator's docstring without reading an array dump.
                        check $"golden read: {label} -- whole-array sum is {total}"
                            (Array.sum got = total) (sprintf "%f" (Array.sum got))
                    | Blade.ProviderRegistry.PInts _ ->
                        check $"golden read: {label}" false "payload came back as int64, not float"
            readsGolden mainKeyG "temp" gTempMain [4; 5] 3060.0 "branch main -> the corrected field"
            readsGolden tagKeyG "temp" gTempV1 [4; 5] 3040.0 "tag v1.0 -> the raw field"
            readsGolden mainKeyG "lat" gLat [4] 100.0 "the lat coordinate array"
            readsGolden mainKeyG "lon" gLon [5] 510.0 "the lon coordinate array"
            // Three spot cells, spelled out, so a failure says WHICH cell moved
            // rather than only that a sum did. Flat indices 0, 13 and 19 are
            // temp(0,0), temp(2,3) and temp(3,4).
            (match spec.ReadVarData tagKeyG "temp", spec.ReadVarData mainKeyG "temp" with
             | Ok a, Ok b ->
                 (match a.Payload, b.Payload with
                  | Blade.ProviderRegistry.PFloats v1, Blade.ProviderRegistry.PFloats mn ->
                      check "golden read: spot cells -- v1.0 (0,0)=0, (2,3)=203, (3,4)=304"
                          (v1.[0] = 0.0 && v1.[13] = 203.0 && v1.[19] = 304.0)
                          (sprintf "%g / %g / %g" v1.[0] v1.[13] v1.[19])
                      check "golden read: spot cells -- main is v1.0 plus exactly 1.0 everywhere"
                          (mn.[0] = 1.0 && mn.[13] = 204.0 && mn.[19] = 305.0
                           && Array.forall2 (fun (x: float) (y: float) -> y - x = 1.0) v1 mn)
                          (sprintf "%g / %g / %g" mn.[0] mn.[13] mn.[19])
                  | _ -> check "golden read: spot cells" false "a payload came back as int64")
             | a, b -> check "golden read: spot cells" false (errorText a + " | " + errorText b))
            // The compile-time dim-name lookup, over reference bytes.
            check "golden: VarDimNames reports the reference's dimension names"
                (spec.VarDimNames mainKeyG "temp" = Some ["lat"; "lon"])
                (sprintf "%A" (spec.VarDimNames mainKeyG "temp"))
     with ex -> check "golden: the committed icechunk-python repo" false ex.Message)

    // ---------------------------------------------------------------
    // 20. Refusals a USER can act on: at `check`, and BY NAME
    // ---------------------------------------------------------------
    // Two fixes here, both about what a refusal SAYS and WHEN -- not about
    // which programs are refused. Every verdict below was already the
    // verdict; none of this section changes an accept into a reject.
    //
    //   (1) NOTHING reached `blade check`. TypeCheck resolves the store at the
    //       binding site (it must: the dims' and variables' types come from the
    //       metadata), but its catch parked only a dead NATIVE LIBRARY as
    //       BL2007 and swallowed everything else -- so a typo'd ref, an
    //       ambiguous bare ref, a missing or corrupt repo, a spec byte, an
    //       Offline status, a deleted-tag tombstone, virtual chunk refs, nested
    //       groups and every verifier/offset refusal produced NO diagnostic
    //       under `check` and none in the editor, appearing only once
    //       `emit`/`run` re-opened the store at lowering. That refusal set is
    //       this provider's product claim, so hiding all of it from the phase
    //       people actually run was the feature's largest UX hole. It is now
    //       BL2008, at the load/checkout site's own span.
    //
    //   (2) The refusals that DID arrive named nothing a user recognises. The
    //       rank-1 co-iteration message printed the raw provenance tag; the
    //       rank->=2 one named no axis at all; the mint table's recorded
    //       SplitReason -- the one fact saying WHY two checkouts' axes diverged
    //       -- was printed by nothing; and the laundering-shaped BL3001
    //       rendered both sides identically ("expected Array<Float64 like
    //       Idx<5>, Idx<4>>, got Array<Float64 like Idx<5>, Idx<4>>"), which
    //       reads as a compiler bug rather than a diagnosis.
    printfn "\n--- check-time refusals, named axes ---"

    /// The in-process `ide check` seam section 15 drives: typecheck only, no
    /// lowering, no spawn -- which is exactly the phase under test here. Using
    /// `lower` instead would prove nothing, since lowering re-opens the store
    /// and has always reported these.
    let checkPayload (name: string) (src: string) : string * int =
        Blade.Ide.ideCheckSource name src

    (try
        // Section 18's repos, reused rather than re-minted: `coordRewriteSpec`
        // is exactly the fixture these messages need (lat rewritten, SAME
        // extent, so the two identities are indistinguishable by extent and the
        // recorded reason is the only explanation there is). Written only if a
        // reordering ever leaves this section standing alone.
        if not (Directory.Exists launderRoot) then IW.writeRepo launderRoot coordRewriteSpec
        let root = launderRoot.Replace('\\', '/')

        /// One check-phase refusal: the code, at the phase, with the provider's
        /// own words intact. `resetCaches` per case for section 12's reason --
        /// a compilation must not inherit another's identities or memos.
        let refusesAtCheck (label: string) (src: string) (needles: string list) =
            resetCaches ()
            let (json, code) = checkPayload "ic_check_refusal.blade" src
            let head = if json.Length > 900 then json.Substring(0, 900) else json
            check $"check: a {label} refuses AT TYPECHECK, as BL2008"
                (code <> 0 && json.Contains "\"code\":\"BL2008\"") head
            check $"check: ... and keeps the provider's own words ({label})"
                (needles |> List.forall (fun (n: string) -> json.Contains n)) head

        // (a) A typo'd ref. The provider lists what the repo DOES have, which
        // is the whole value of surfacing this at check rather than at run.
        refusesAtCheck "typo'd ref"
            (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("mian")
let A = ck.vars.temp |> ic.read
"""
                     root)
            [ "no branch, tag or snapshot named"; "mian"; "main" ]

        // (b) An ambiguous BARE ref -- section 12's `ic_ambiguous` fixture,
        // where "x" is a branch and a tag.
        refusesAtCheck "bare ref naming both a branch and a tag"
            (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("x")
"""
                     (ambRoot.Replace('\\', '/')))
            [ "ambiguous"; "ic.branch"; "ic.tag" ]

        // (c) A repo that is not there at all -- the plainest case, and the one
        // a reader hits first when a relative path is resolved from the wrong
        // working directory.
        refusesAtCheck "missing repo"
            (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
"""
                     (root + "_definitely_absent"))
            [ "does not exist" ]

        // (d) ADDITIVE, and this is the pin that says so: zarr raises no
        // store-resolution type, so its missing store still falls through to
        // the historical opaque-type fallback and lowering keeps ownership of
        // the diagnostic. A change that made BL2008 fire for every provider
        // would be a behaviour change to three features, not one.
        (let (zjson, _) =
            checkPayload "zarr_absent.blade"
                "import zarr as z\n\nlet s = z.load(\"tests/fixtures/zarr_stores/definitely_absent\")\nlet a = 1\n"
         check "check: a MISSING ZARR store still defers silently (other providers untouched)"
             (not (zjson.Contains "BL2008"))
             (if zjson.Length > 400 then zjson.Substring(0, 400) else zjson))

        // (e) The POSITIVE control: surfacing refusals must not make a GOOD
        // store refuse. Section 15 checks a clean program too; this one runs
        // against the same fixture the refusals above use, so a fixture fault
        // cannot masquerade as the feature working.
        (resetCaches ()
         let (json, code) =
            checkPayload "ic_check_ok.blade"
                (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
let A = ck.vars.temp |> ic.read
"""
                         root)
         check "check: a RESOLVABLE checkout still checks clean" (code = 0)
             (if json.Length > 600 then json.Substring(0, 600) else json))
     with ex -> check "check: store-resolution refusals reach `blade check`" false ex.Message)

    // The MESSAGES. Same fixture, same verdicts as section 12/18 -- what is
    // pinned here is the text, which is the half no other assertion reads.
    (try
        if not (Directory.Exists launderRoot) then IW.writeRepo launderRoot coordRewriteSpec
        let root = launderRoot.Replace('\\', '/')

        /// A refusal's text, with the mint table cleared first so the two
        /// checkouts below mint their identities from scratch.
        let refusalText (src: string) : string =
            resetCaches ()
            errorText (lower src)

        // (a) RANK 1, over the coordinate array itself. Two identities of one
        // dim, decoded ('lat' and 'lat#2'), plus the recorded reason -- which
        // is the only thing distinguishing two axes of equal extent, and which
        // nothing printed until now.
        (let e =
            refusalText
                (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck1 = repo.checkout("v1.0", ic.tag)
let ck2 = repo.checkout("main")
let a = ck1.dims.lat |> ic.read
let b = ck2.dims.lat |> ic.read
let d = b - a
let total = reduce(d, (+))
"""
                         root)
         check "message: the rank-1 refusal names both identities by their DECODED names"
             (e.Contains "'lat'" && e.Contains "'lat#2'") e
         check "message: the rank-1 refusal leaks no raw provenance tag"
             (not (e.Contains axisTagPrefix)) e
         check "message: the rank-1 refusal says WHY they diverged (the recorded SplitReason)"
             (e.Contains "two identities of axis 'lat'" && e.Contains "coordinate content differs") e)

        // (b) RANK 2, the §5 headline program. The bare sentence named no
        // operand, no record and no axis; a five-axis store would have left the
        // reader to guess which dimension moved.
        (let e = refusalText (crossCheckoutSrc launderRoot)
         check "message: the rank->=2 refusal names the FIRST disagreeing index record"
             (e.Contains "they first differ at index record") e
         check "message: ... and names that record's axis on each side"
             (e.Contains "'lat'" && e.Contains "'lat#2'") e
         check "message: the rank->=2 refusal leaks no raw provenance tag"
             (not (e.Contains axisTagPrefix)) e)

        // (c) The IDENTICAL-RENDER mismatch. The type printer reads no Tag, so
        // two provider identities of one axis print alike and BL3001 said
        // "expected X, got X". One appended line, so every ERROR-CONTAINS pin
        // on the sentence above it still matches.
        (let e = refusalText (launderRank2Src launderRoot)
         // Pinned in two halves so the note's assertion can never pass
         // VACUOUSLY. If the ascription seam stops being the one that fires
         // first, this line goes red and says so, rather than the note silently
         // becoming untested.
         check "message: the laundering ascription is refused by UNIFY (the identical-render shape)"
             (e.Contains "Type mismatch"
              && e.Contains "expected Array<Float64 like Idx<5>, Idx<4>>, got Array<Float64 like Idx<5>, Idx<4>>")
             e
         check "message: ... and that identically-RENDERING mismatch now names the two identities"
             (e.Contains "note: the index types differ by identity"
              && e.Contains "'lat'" && e.Contains "'lat#2'"
              && e.Contains "coordinate content differs")
             e
         check "message: the laundering ascription still refuses (section 18's verdict, unchanged)"
             (refusesOnAxesOrAscription (Error e : Result<unit, string>)) e)
     with ex -> check "message: named axes in co-iteration refusals" false ex.Message)

    // The decoder's hardened cases. Section 15 pins the SHAPES the mint
    // produces today; these pin the shapes it produces for inputs nobody had
    // tried, now that the decoded name is user-facing in every refusal above.
    // Both separators are legal INSIDE the field to their left, so both are
    // read from the RIGHT.
    let decodesTo (label: string) (payload: string) (expected: string option) =
        let tag = axisTagPrefix + payload
        check label (tryAxisTagName tag = expected) (sprintf "%s -> %A" tag (tryAxisTagName tag))

    decodesTo "axis tag: a dim genuinely named `a@b` keeps its `@` (the LAST one separates the repo)"
        "a@b@ic_wx:0f3a" (Some "a@b")
    decodesTo "axis tag: a `#` inside the repo label is not a split ordinal"
        "lat@we#ird:0f3a" (Some "lat")
    decodesTo "axis tag: ... and a REAL ordinal on such a repo still decodes"
        "lat@we#ird:0f3a#2" (Some "lat#2")
    decodesTo "axis tag: a trailing `#` with nothing after it is not an ordinal"
        "lat@ic_wx:0f3a#" (Some "lat")
    decodesTo "axis tag: a trailing `#word` is not an ordinal either (digits only)"
        "lat@ic_wx:0f3a#beta" (Some "lat")
    decodesTo "axis tag: the conventional `<name>.icechunk` repo directory decodes normally"
        "lat@wx.icechunk:0f3a" (Some "lat")

    // The DIAGNOSTIC decoder is wider than the IDE one: a packed pool has no
    // index-type spelling (`tryAxisTagName` returns None, pinned in section 15
    // and unchanged), but a refusal message still has to call it something
    // other than forty characters of internal identity -- which is what the
    // two cross-repo/rewritten POOL refusals printed until now.
    let poolDecodesTo (label: string) (tag: string) (expected: string option) =
        check label (tryProviderTagName tag = expected)
            (sprintf "%s -> %A" tag (tryProviderTagName tag))

    poolDecodesTo "pool tag: a simplex pool decodes to a spelling no dim name can collide with"
        (providerPoolTagPrefix + "cov@ic_pool:0f3a") (Some "pool(cov)")
    poolDecodesTo "pool tag: ... and a SPLIT pool keeps its ordinal"
        (providerPoolTagPrefix + "cov@ic_pool:0f3a#2") (Some "pool(cov)#2")
    poolDecodesTo "pool tag: an iterated-wreath pool decodes to its own spelling"
        (providerOrbPoolTagPrefix + "w@ic_pool:0f3a") (Some "orbit_pool(w)")
    poolDecodesTo "pool tag: a DENSE axis still decodes to its bare dim name here too"
        (axisTagPrefix + "lat@ic_wx:0f3a") (Some "lat")
    poolDecodesTo "pool tag: a foreign `__` sentinel is decoded by neither"
        "__orbidx" None

    // ... and the LIVE consequence of that last one. A resolved provider axis
    // carries a `__icaxis|` tag whose repo-label half is a DIRECTORY NAME, and
    // `<name>.icechunk` is the conventional spelling -- so the tag contains a
    // dot for entirely ordinary reasons. TypeCheck's unresolved-qualified-path
    // heuristic ("a dotted Tag can only be an unregistered name") read that as
    // a typo'd dimension and refused `type L = ck.index.lat` on every
    // conventionally-named repo, printing the raw tag while doing it. Every
    // fixture above is named WITHOUT a dot, which is why nothing caught it.
    (try
        let dotRoot = fixRepo "ic_dotted.icechunk"
        IW.writeRepo dotRoot coordRewriteSpec
        resetCaches ()
        let r =
            lower
                (sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = repo.checkout("main")
type Lat = ck.index.lat
let lats = ck.dims.lat |> ic.read
let w: Array<Float64 like Lat> = lats
let total = reduce(w, (+))
"""
                         (dotRoot.Replace('\\', '/')))
        check "dotted repo: `ck.index.lat` resolves when the repo DIRECTORY contains a dot"
            (match r with Ok _ -> true | Error _ -> false) (errorText r)
     with ex -> check "dotted repo: `ck.index.lat`" false ex.Message)

    // ---------------------------------------------------------------
    // 21. Tier-2 hardening
    // ---------------------------------------------------------------
    // Each item pinned at the seam it moved: a cap that only counted entries
    // and never their size, a change stamp a same-tick same-length rewrite
    // walked straight past, a chunk grid that narrowed to `int` before it was
    // bounded, a split story told against the wrong prior, unbounded ref
    // listings in diagnostics -- and a regeneration guard for the one fixture
    // in this repository that is BOTH committed and generated.
    printfn "\n--- tier-2 hardening ---"

    // -- (a) The INLINE-BYTES cap ------------------------------------------
    // `maxBakedChunks` bounds how many table ENTRIES the emitter writes and
    // says nothing about how big they are. An inline chunk bakes as a
    // `static const unsigned char[]` of `0x..` literals -- about five
    // characters of C++ per byte -- so ONE 64 MB inline chunk is one table
    // entry and ~320 MB of generated source, reported as neither a refusal nor
    // an error but as a g++ that never returns. Driven at the emitter over a
    // synthetic ResolvedArray, exactly like the negative-offset guard in
    // section 6: the fixture writer inlines only chunks under its own
    // threshold and cannot produce one this large.
    let oneChunkJson (cells: int) =
        sprintf """{"zarr_format": 3, "node_type": "array", "shape": [%d], "data_type": "float64",
                    "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [%d]}},
                    "fill_value": 0, "codecs": [{"name": "bytes", "configuration": {"endian": "little"}}],
                    "dimension_names": ["x"]}""" cells cells

    check "inline cap: the chunk-COUNT cap came down to 100_000 alongside it"
        (maxBakedChunks = 100_000) $"{maxBakedChunks}"

    /// Emit one all-inline variable of `cells` float64s in a single chunk, and
    /// report what the emitter said (or "<no exception>").
    let emitInline (cells: int) : string =
        let json = oneChunkJson cells
        match ZarrProvider.parseArrayMetaV3 "big" "" json with
        | Error e -> "metadata did not parse: " + e
        | Ok meta ->
            let node : NodeMeta =
                { Id = Array.create 8 0x07uy; Path = "/big"; Kind = NodeArray
                  UserDataJson = json; Shape = []; DimensionNames = None; ManifestRefs = [] }
            let ra : ResolvedArray =
                { Root = "wx.icechunk"; Ref = (RefBranch, "main"); SnapshotId = Array.create 12 0x03uy
                  VarName = "big"; Node = node; Meta = meta
                  Table = [| Inline (Array.zeroCreate (cells * 8)) |] }
            caught (fun () -> CppIcechunk.icechunkChunkFetch ra "v" (cells * 8))

    // 655360 float64 = 5 MiB in ONE chunk: one table entry, past the 4 MiB cap.
    (let msg = emitInline 655360
     check "inline cap: 5 MiB of inline chunk data in a single chunk is refused"
         (msg.Contains "INLINE chunk data" && msg.Contains "inline cap") msg
     check "inline cap: ... and the refusal names the variable and the store"
         (msg.Contains "'big'" && msg.Contains "wx.icechunk") msg
     check "inline cap: ... and gives a remedy in the STORE, not in the compiler"
         (msg.Contains "Re-chunk" && msg.Contains "natively") msg)
    check "inline cap: an ordinary small inline chunk still emits (the cap does not over-fire)"
        (emitInline 8 = "<no exception>") (emitInline 8)

    // -- (b) rank-0 and the int64 chunk-grid product -----------------------
    // Both are gates at `arrayMetaOfNode`, the seam every checkout reads every
    // node through, so a store that cannot be read says so at CHECK rather
    // than at the first index expression that has no index to build.
    let mkNode (path: string) (json: string) (dims: int64 list) : NodeMeta =
        { Id = Array.create 8 0x09uy; Path = path; Kind = NodeArray
          UserDataJson = json
          Shape = dims |> List.map (fun n -> { ArrayLength = n; NumChunks = None })
          DimensionNames = None; ManifestRefs = [] }

    let rank0Json = """{"zarr_format": 3, "node_type": "array", "shape": [], "data_type": "float64",
                        "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": []}},
                        "fill_value": 0, "codecs": [{"name": "bytes"}]}"""
    check "rank-0: the Zarr v3 parser itself accepts a scalar array (so something downstream must refuse it)"
        (match ZarrProvider.parseArrayMetaV3 "scalar" "" rank0Json with
         | Ok m -> m.Shape.IsEmpty
         | Error _ -> false)
        (errorText (ZarrProvider.parseArrayMetaV3 "scalar" "" rank0Json))
    (let r = arrayMetaOfNode "scalar" (mkNode "/scalar" rank0Json [])
     check "rank-0: a rank-0 array is refused BY NAME at the icechunk metadata gate"
         (isError r "rank-0 arrays are not supported by the icechunk provider") (errorText r)
     check "rank-0: ... and the refusal says what to do with the value instead"
         (isError r "length-1 rank-1 array") (errorText r))

    // 3_000_000 x 3_000_000 chunks = 9e12, well past Int32.MaxValue: the old
    // `gridDims ... |> List.map int` truncated the product into a table of the
    // wrong length, or threw a bare ArgumentException out of `Array.create`.
    let hugeGridJson =
        """{"zarr_format": 3, "node_type": "array", "shape": [3000000, 3000000], "data_type": "float64",
            "chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [1, 1]}},
            "fill_value": 0, "codecs": [{"name": "bytes"}], "dimension_names": ["a", "b"]}"""
    (let r = arrayMetaOfNode "huge" (mkNode "/huge" hugeGridJson [3000000L; 3000000L])
     check "grid cap: a chunk grid past Int32 range is refused at the metadata gate"
         (isError r "chunk grid" && isError r "re-chunk") (errorText r))
    (match ZarrProvider.parseArrayMetaV3 "huge" "" hugeGridJson with
     | Error e -> check "grid cap: the huge-grid metadata parses" false e
     | Ok meta ->
         let r = caught (fun () -> buildChunkTable meta [] |> ignore)
         check "grid cap: buildChunkTable REFUSES rather than throwing out of Array.create"
             (r = "<no exception>") r
         check "grid cap: ... and the refusal names the array and the grid"
             (isError (buildChunkTable meta []) "chunk grid") (errorText (buildChunkTable meta [])))
    // The cap does not over-fire: section 6's 2-chunk grid still builds.
    (match ZarrProvider.parseArrayMetaV3 "temp" "/repo" v3json with
     | Error e -> check "grid cap: an ordinary grid still builds" false e
     | Ok meta ->
         check "grid cap: an ordinary 2-chunk grid still builds a table"
             (match buildChunkTable meta [] with Ok t -> t.Length = 2 | Error _ -> false)
             (errorText (buildChunkTable meta [])))

    // -- (c) The change stamp's content hash --------------------------------
    // mtime + byte length LOOKS like a complete stamp for `$ROOT/repo` and is
    // not, for the one rewrite that matters: a BRANCH RESET swaps a
    // fixed-width `snapshot_index` inside the FlatBuffer, so the file keeps
    // its length exactly, and Windows' ~15.6 ms timestamp granularity lets a
    // reset share an mtime with the write before it. The stamp is the read
    // memos' key, so an undetected rewrite is not a stale read but a SPLIT
    // one: whichever phase asks next keeps getting the old snapshot.
    //
    // Reproduced literally -- write, capture mtime, reset the branch, force
    // the mtime back -- and driven through `resetAxisMint`, the per-request
    // reset the IDE daemon does, which drops the pin and KEEPS the memos. That
    // is the only path on which the stamp is load-bearing.
    (try
        let stampRoot = fixRepo "ic_stamp"
        let stampKey = stampRoot + "@branch:main"
        let latN (vals: float[]) = IW.mkArray "lat" ["lat"] [int64 vals.Length] [int64 vals.Length] (IW.IceF64 vals)
        let stampBase : IW.RepoSpec =
            { IW.emptyRepo with
                Seed = 31
                // UNCOMPRESSED, so "same length" is a property of the FORMAT
                // and not of zstd's luck: two payloads differing in one
                // fixed-width field compress to different lengths, which would
                // hand this test a discriminator the real hazard does not
                // have. Both reader paths are live (section 7), so an
                // uncompressed repo is an ordinary one.
                Compress = false
                Snapshots = [ IW.mkSnapshot "s1" [ latN [| 1.0; 2.0; 3.0; 4.0 |] ]
                              IW.mkSnapshot "s2" [ latN [| 5.0; 6.0; 7.0; 8.0 |] ]
                              IW.mkSnapshot "s3" [ latN [| 9.0; 10.0; 11.0; 12.0 |] ] ]
                Branches = [ ("main", "s1") ] }
        // `Repo.snapshots` is sorted by id BYTES and `Ref.snapshot_index`
        // indexes into that order; FlatBuffers OMIT a field at its default, so
        // an index of 0 would serialize SHORTER than a nonzero one and the
        // "same length" premise would be the test's own doing. Both targets
        // are therefore picked away from position 0. Crockford base32 is
        // order-preserving, so sorting the spellings sorts the ids.
        let sorted = [ "s1"; "s2"; "s3" ] |> List.sortBy (IW.snapshotId stampBase)
        let firstTarget = sorted.[1]
        let secondTarget = sorted.[2]
        let specOf (t: string) = { stampBase with Branches = [ ("main", t) ] }
        let snapNow () =
            match load stampKey with
            | Ok (LoadedCheckout ck) -> base32Encode ck.SnapshotId
            | Ok (LoadedRepo _) -> "<repo handle>"
            | Error e -> "ERR: " + e

        IW.writeRepo stampRoot (specOf firstTarget)
        resetCaches ()
        let before = snapNow ()
        check "stamp: the first checkout resolves the branch's first target"
            (before = IW.snapshotId stampBase firstTarget)
            (sprintf "%s, expected %s" before (IW.snapshotId stampBase firstTarget))

        let stampFile = repoFilePath stampRoot
        let mtime = File.GetLastWriteTimeUtc stampFile
        let lenBefore = (FileInfo stampFile).Length
        IW.writeRepo stampRoot (specOf secondTarget)
        let lenAfter = (FileInfo stampFile).Length
        File.SetLastWriteTimeUtc(stampFile, mtime)
        check "stamp: a branch RESET rewrites `repo` at EXACTLY the same byte length"
            (lenAfter = lenBefore) (sprintf "%d -> %d bytes" lenBefore lenAfter)
        check "stamp: ... and the mtime is forced back, so (ticks, length) is unchanged"
            (File.GetLastWriteTimeUtc stampFile = mtime)
            (sprintf "%A vs %A" (File.GetLastWriteTimeUtc stampFile) mtime)

        // The per-request reset: pin dropped, memos kept. Under a (ticks,
        // length) stamp the key is identical and `load` answers from the memo
        // with the OLD snapshot.
        resetAxisMint ()
        let after = snapNow ()
        check "stamp: the content hash still invalidates the read memo across the reset"
            (after = IW.snapshotId stampBase secondTarget && after <> before)
            (sprintf "before %s, after %s, expected %s"
                 before after (IW.snapshotId stampBase secondTarget))
        resetCaches ()
     with ex -> check "stamp: a same-mtime same-length branch reset" false ex.Message)

    // -- (d) `splitReason` against the CLOSEST prior ------------------------
    // The identities of one axis are a SET this compilation has met, in
    // checkout order -- not a chain -- so "the previous one" is an accident of
    // which checkouts a program names. A (extent 10, coord X) -> B (extent 8)
    // -> C (extent 10, coord Y) recorded C's reason against B and printed
    // "extent 8 -> 10", which is true of B and says nothing about the pairing
    // a user actually hits: C against A, whose real story is a coordinate
    // divergence. This text is PRINTED at every co-iteration refusal
    // (`trySplitReasonOfTag`, section 20), so it is a claim.
    (try
        let splitRoot = fixRepo "ic_split_prior"
        let latN (vals: float[]) = IW.mkArray "lat" ["lat"] [int64 vals.Length] [int64 vals.Length] (IW.IceF64 vals)
        let lat10a = Array.init 10 (fun i -> float i)
        let lat8 = Array.init 8 (fun i -> float i)
        let lat10b = Array.init 10 (fun i -> 100.0 + float i)
        IW.writeRepo splitRoot
            { IW.emptyRepo with
                Seed = 33
                Snapshots = [ IW.mkSnapshot "sa" [ latN lat10a ]
                              IW.mkSnapshot "sb" [ latN lat8 ]
                              IW.mkSnapshot "sc" [ latN lat10b ] ]
                Branches = [ ("a", "sa"); ("b", "sb"); ("c", "sc") ] }
        resetCaches ()
        axisModule "ka" (splitRoot + "@branch:a") |> ignore
        axisModule "kb" (splitRoot + "@branch:b") |> ignore
        axisModule "kc" (splitRoot + "@branch:c") |> ignore
        let ids = axisIdentities splitRoot "lat"
        let story = sprintf "%A" (ids |> List.map (fun i -> (i.Extent, i.SplitReason)))
        check "split prior: three checkouts of one dim mint three identities" (ids.Length = 3) story
        check "split prior: the newest reason is told against the SAME-EXTENT prior, not the newest one"
            (match ids with
             | newest :: _ -> newest.SplitReason = Some "coordinate content differs"
             | [] -> false)
            story
        check "split prior: ... and the middle identity still reports its own extent change"
            (match ids with
             | _ :: middle :: _ -> middle.SplitReason = Some "extent 10 -> 8"
             | _ -> false)
            story
     with ex -> check "split prior: closest-prior split reason" false ex.Message)

    // The other half of the rule: with NO same-extent prior, the reason is told
    // against the OLDEST identity -- the axis as this compilation first met it
    // -- rather than against whichever checkout happened to come last.
    (try
        let oldestRoot = fixRepo "ic_split_oldest"
        let latN (vals: float[]) = IW.mkArray "lat" ["lat"] [int64 vals.Length] [int64 vals.Length] (IW.IceF64 vals)
        IW.writeRepo oldestRoot
            { IW.emptyRepo with
                Seed = 34
                Snapshots = [ IW.mkSnapshot "sa" [ latN (Array.init 10 float) ]
                              IW.mkSnapshot "sb" [ latN (Array.init 8 float) ]
                              IW.mkSnapshot "sd" [ latN (Array.init 12 float) ] ]
                Branches = [ ("a", "sa"); ("b", "sb"); ("d", "sd") ] }
        resetCaches ()
        axisModule "oa" (oldestRoot + "@branch:a") |> ignore
        axisModule "ob" (oldestRoot + "@branch:b") |> ignore
        axisModule "od" (oldestRoot + "@branch:d") |> ignore
        let ids = axisIdentities oldestRoot "lat"
        check "split prior: no same-extent prior falls back to the OLDEST identity (10 -> 12, not 8 -> 12)"
            (match ids with
             | newest :: _ -> newest.SplitReason = Some "extent 10 -> 12"
             | [] -> false)
            (sprintf "%A" (ids |> List.map (fun i -> (i.Extent, i.SplitReason))))
     with ex -> check "split prior: oldest-identity fallback" false ex.Message)

    // -- (e) Bounded ref listings ------------------------------------------
    // "no branch named X -- branches in this repo: ..." printed the WHOLE ref
    // namespace. One branch per experiment (or per CI run, or per user) is an
    // ordinary repo, and the listing is a hint, not an inventory -- it reaches
    // a terminal, an editor popup and every pinned message.
    let manyInfo : RepoInfo =
        { Branches = [ for i in 1 .. 15 -> (sprintf "b%02d" i, 0) ]
          Tags = [ for i in 1 .. 12 -> (sprintf "t%02d" i, 0) ]
          DeletedTags = []
          Snapshots = [ snap1 ]
          Status = StatusOnline }
    (let e = errorText (resolveRef manyInfo RefBranch "nope")
     check "ref listing: a not-found branch lists at most ten names"
         (e.Contains "b01" && e.Contains "b10" && not (e.Contains "b11")) e
     check "ref listing: ... and says how many it did not list"
         (e.Contains "and 5 more") e)
    (let e = errorText (resolveRef manyInfo RefTag "nope")
     check "ref listing: the tag listing is bounded by the same rule"
         (e.Contains "t10" && not (e.Contains "t11") && e.Contains "and 2 more") e)
    (let e = errorText (resolveRef manyInfo RefBare "nope")
     check "ref listing: the bare-name refusal bounds BOTH namespaces independently"
         (e.Contains "and 5 more" && e.Contains "and 2 more") e)
    (let e = errorText (resolveRef info RefBranch "nope")
     check "ref listing: a small repo still lists every branch, with no truncation note"
         (e.Contains "main" && e.Contains "dev" && not (e.Contains "more")) e)

    // -- (f) Deduped chunk-file paths --------------------------------------
    // A repo written with packed native chunks puts EVERY chunk of a variable
    // in one file at different offsets; the per-chunk path table then baked
    // that one absolute path N times. One distinct path collapses to a single
    // pointer; several go in their own table behind an index.
    (match ZarrProvider.parseArrayMetaV3 "temp" "" v3json with
     | Error e -> check "path dedupe: the synthetic metadata parses" false e
     | Ok meta ->
         let node : NodeMeta =
             { Id = Array.create 8 0x11uy; Path = "/temp"; Kind = NodeArray
               UserDataJson = v3json; Shape = []; DimensionNames = None; ManifestRefs = [] }
         let withTable (t: ChunkLoc[]) : ResolvedArray =
             { Root = "wx.icechunk"; Ref = (RefBranch, "main"); SnapshotId = Array.create 12 0x04uy
               VarName = "temp"; Node = node; Meta = meta; Table = t }
         let nat (id: byte) (off: int64) = Native { ChunkId = Array.create 12 id; Offset = off; Length = 48L }
         let prologueOf (t: ChunkLoc[]) =
             String.concat "\n" (CppIcechunk.icechunkChunkFetch (withTable t) "v" 48).Prologue
         (let p = prologueOf [| nat 0xABuy 0L; nat 0xABuy 48L |]
          check "path dedupe: two chunks packed in ONE file bake a single path pointer"
              (p.Contains "static const char* const v_icpath =" && not (p.Contains "v_icfile")) p)
         (let p = prologueOf [| nat 0xABuy 0L; nat 0xCDuy 0L |]
          check "path dedupe: two DISTINCT files bake a 2-entry path table plus an index"
              (p.Contains "static const char* const v_icpath[2]" && p.Contains "static const int v_icfile[2]") p)
         (let p = prologueOf [| Fill; Fill |]
          check "path dedupe: an all-fill variable bakes no path table at all"
              (not (p.Contains "v_icpath") && not (p.Contains "v_icfile")) p))

    // -- (g) The committed demo store vs its generator ----------------------
    // examples/data/station_temps.icechunk is COMMITTED (the notebook reads it
    // and pins numbers computed from it) and GENERATED
    // (examples/tools/make_station_icechunk.fsx). Nothing connected the two: a
    // generator edit nobody ran, or a store edit nobody generated, left a
    // fixture and a recipe that disagree -- surfacing, eventually, as notebook
    // pins that are quietly wrong.
    //
    // The spec now lives in ONE value (examples/tools/StationSpec.fs) that the
    // script `#load`s and this test compiles against, so the comparison is
    // real: IcechunkWrite's ids are content-derived and the spec is analytic,
    // so the same value writes the same bytes in any directory.
    (match findUpFrom hereDir (Path.Combine("examples", "data", "station_temps.icechunk")) with
     | None ->
         printfn "  SKIP regen: the committed store examples/data/station_temps.icechunk is not present"
     | Some committed ->
         let scratch = Path.Combine(Path.GetTempPath(), "blade_ic_regen_" + Guid.NewGuid().ToString("N"))
         try
            try
                IW.writeRepoAt [ scratch ] Blade.Examples.StationSpec.spec
                /// Every FILE under a root, as repo-relative forward-slash names.
                /// Chunk and manifest files are content-addressed, so their NAMES
                /// are already a content claim -- which is why the name-set half
                /// covers them and the byte half does not need to.
                let namesUnder (root: string) =
                    Directory.GetFiles(root, "*", SearchOption.AllDirectories)
                    |> Array.map (fun f -> (Path.GetRelativePath(root, f)).Replace('\\', '/'))
                    |> Set.ofArray
                let committedNames = namesUnder committed
                let rebuiltNames = namesUnder scratch
                check "regen: the committed store holds EXACTLY the files the spec writes"
                    (committedNames = rebuiltNames)
                    (sprintf "only committed: %A; only rebuilt: %A"
                         (Set.difference committedNames rebuiltNames)
                         (Set.difference rebuiltNames committedNames))
                let sameBytes (name: string) =
                    let a = Path.Combine(committed, name.Replace('/', Path.DirectorySeparatorChar))
                    let b = Path.Combine(scratch, name.Replace('/', Path.DirectorySeparatorChar))
                    File.Exists a && File.Exists b && File.ReadAllBytes a = File.ReadAllBytes b
                check "regen: the mutable `repo` file is byte-identical to the spec's"
                    (sameBytes "repo") "examples/data/station_temps.icechunk/repo differs from StationSpec.spec"
                let snapNames = rebuiltNames |> Set.filter (fun n -> n.StartsWith "snapshots/")
                check "regen: every snapshot is byte-identical to the spec's"
                    (not (Set.isEmpty snapNames) && snapNames |> Set.forall sameBytes)
                    (sprintf "differing: %A" (snapNames |> Set.filter (sameBytes >> not)))
            with ex -> check "regen: the committed store matches StationSpec.spec" false ex.Message
         finally
            try Directory.Delete(scratch, true) with _ -> ())

    // ---------------------------------------------------------------
    // 22. Revision reuse: the tile store across snapshots (structural/04)
    // ---------------------------------------------------------------
    // A pure elementwise map over a checkout's variable runs one leading-axis
    // tile at a time; each tile's key carries the content identities of the
    // chunks it reads, so a run against a later snapshot recomputes only the
    // tiles whose chunks moved -- and, with the probe hoisted before the
    // read, the compute phase reads only those chunks. Values, never text:
    // cold and warm stdout are identical and equal the interpreter's. The
    // fixture is wxSpec with ONE changed chunk (rows 3-4, columns 2-3, +100),
    // so s2's manifest shares three chunk ids with s1's and mints one.
    // Leading-axis tiles over the 3x2-chunked 5x4 grid: two lat tiles of two
    // chunks each; the changed chunk sits in lat tile 1.
    printfn "\n--- revision reuse: tiles across snapshots ---"
    (try
        let tileRoot = fixRepo "ic_tiles"
        let tempV2One =
            tempV1 |> Array.mapi (fun i v -> (if i / 4 >= 3 && i % 4 >= 2 then v + 100.0 else v))
        let oneSpec =
            let coords = [
                IW.mkArray "lat" ["lat"] [5L] [5L] (IW.IceF64 latData)
                IW.mkArray "lon" ["lon"] [4L] [4L] (IW.IceF64 lonData) ]
            let tempOf (d: float[]) =
                { IW.mkArray "temp" ["lat"; "lon"] [5L; 4L] [3L; 2L] (IW.IceF64 d) with InlineThreshold = 0 }
            { IW.emptyRepo with
                Compress = true
                Seed = 7
                Snapshots = [ IW.mkSnapshot "s1" (coords @ [ tempOf tempV1 ]); IW.mkSnapshot "s2" (coords @ [ tempOf tempV2One ]) ]
                Branches = [ ("main", "s2") ]
                Tags = [ ("v1.0", "s1") ] }
        IW.writeRepoAt [ tileRoot; Path.Combine(e2eDir, tileRoot) ] oneSpec
        resetCaches ()
        check "tiles: one changed chunk mints ONE new chunk file (5, not 8)"
            ((filesIn (Path.Combine(tileRoot, "chunks"))).Length = 5)
            (sprintf "%d chunk files" (filesIn (Path.Combine(tileRoot, "chunks"))).Length)

        let srcOf (checkoutExpr: string) =
            sprintf """
import icechunk as ic

let repo = ic.load("%s")
let ck = %s
let A = ck.vars.temp |> ic.read
let F = method_for(A) <@> lambda(x) -> x * 2.0 + 1.0 |> compute
let total = reduce(F, (+), axes = 2)
"""
                    tileRoot checkoutExpr
        let srcS1 = srcOf "repo.checkout(\"v1.0\", ic.tag)"
        let srcS2 = srcOf "repo.checkout(\"main\")"

        // Off: no tile code reaches the emission at all.
        (match lower srcS1 with
         | Error e -> check "tiles: E lowers" false e
         | Ok ir ->
             let (cppOff, _) = CodeGen.genSelfContainedProgramFromIR ir "ic_tiles_off"
             check "tiles: with BLADE_TILE_CACHE unset the emission carries no tile code"
                 (not (cppOff.Contains "blade_tiles") && not (cppOff.Contains "blade_tilecache.hpp")) "")

        // On: a private absolute store for this section alone.
        let store = Path.GetFullPath(Path.Combine(e2eDir, "tile_cache_" + Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory store |> ignore
        let priorStore = Environment.GetEnvironmentVariable "BLADE_TILE_CACHE"
        let priorVerbose = Environment.GetEnvironmentVariable "BLADE_TILE_CACHE_VERBOSE"
        Environment.SetEnvironmentVariable("BLADE_TILE_CACHE", store)
        Environment.SetEnvironmentVariable("BLADE_TILE_CACHE_VERBOSE", "1")
        try
            let buildTiled (label: string) (src: string) : Result<string * string, string> =
                match lower src with
                | Error e -> Error $"lower: {e}"
                | Ok ir ->
                    let (cpp, _) = CodeGen.genSelfContainedProgramFromIR ir label
                    CodeGen.deployRuntimeHeaders e2eDir
                    let cppFile = Path.Combine(e2eDir, label + ".cpp")
                    File.WriteAllText(cppFile, cpp)
                    match compileCpp cppFile e2eDir with
                    | Error e -> Error e
                    | Ok exe -> Ok (exe, cpp)
            let runTiled (exe: string) : Result<string, string> =
                match runExecutable exe with
                | Ok (0, out) -> Ok out
                | Ok (code, out) -> Error $"exit {code}: {out}"
                | Error e -> Error e
            let census (out: string) (needle: string) = out.Contains needle
            let stdoutOnly (out: string) =
                let i = out.IndexOf "\n[stderr]:"
                icNormOut (if i >= 0 then out.Substring(0, i) else out)
            match buildTiled "ic_tiles_s1" srcS1 with
            | Error e -> baselineFailed "tiles s1" e
            | Ok (exeS1, cppS1) ->
                check "tiles: the emission tiles F over 2 leading-axis tiles with the probe hoisted to A's read"
                    (cppS1.Contains "F__tlo[3]" && cppS1.Contains "A__need[" && cppS1.Contains "blade_tiles::probe(F__tkeys")
                    ""
                match runTiled exeS1, runTiled exeS1 with
                | Ok cold, Ok warm ->
                    check "tiles: E@s1 cold computes every tile and reads every chunk in the compute phase"
                        (census cold "[tiles] F: computed 2/2, hit 0/2" && census cold "[chunks] A: read 4/4 (compute 4, remainder 0)") cold
                    check "tiles: E@s1 warm hits every tile and reads every chunk in the remainder"
                        (census warm "[tiles] F: computed 0/2, hit 2/2" && census warm "[chunks] A: read 4/4 (compute 0, remainder 4)") warm
                    check "tiles: cold and warm stdout are byte-identical" (stdoutOnly cold = stdoutOnly warm) ""
                    (match icInterpStdout "ic_tiles_s1_interp" srcS1 with
                     | Ok interp -> check "tiles: E@s1 stdout equals the interpreter's" (stdoutOnly cold = icNormOut interp) (stdoutOnly cold + "\n---\n" + icNormOut interp)
                     | Error e -> check "tiles: E@s1 interpreter run" false e)
                    check "tiles: the store holds one file per tile" ((Directory.GetFiles(store, "*.tile", SearchOption.AllDirectories)).Length = 2) ""
                | Ok _, Error e | Error e, _ -> check "tiles: E@s1 runs" false e
                match buildTiled "ic_tiles_s2" srcS2 with
                | Error e -> baselineFailed "tiles s2" e
                | Ok (exeS2, _) ->
                    match runTiled exeS2, runTiled exeS2 with
                    | Ok cold2, Ok warm2 ->
                        check "tiles: E@s2 after E@s1 recomputes ONLY the tile holding the changed chunk (1 of 2) and reads only its 2 chunks in the compute phase"
                            (census cold2 "[tiles] F: computed 1/2, hit 1/2" && census cold2 "[chunks] A: read 4/4 (compute 2, remainder 2)") cold2
                        check "tiles: E@s2 again hits every tile"
                            (census warm2 "[tiles] F: computed 0/2, hit 2/2") warm2
                        check "tiles: E@s2 cold and warm stdout are byte-identical" (stdoutOnly cold2 = stdoutOnly warm2) ""
                        (match icInterpStdout "ic_tiles_s2_interp" srcS2 with
                         | Ok interp -> check "tiles: E@s2 stdout equals the interpreter's" (stdoutOnly cold2 = icNormOut interp) (stdoutOnly cold2 + "\n---\n" + icNormOut interp)
                         | Error e -> check "tiles: E@s2 interpreter run" false e)
                        check "tiles: the store holds s1's two tiles plus s2's one changed tile"
                            ((Directory.GetFiles(store, "*.tile", SearchOption.AllDirectories)).Length = 3) ""
                        check "tiles: s2's total reflects the changed chunk (values, not a stale hit)"
                            (match printedScalar "total" cold2 with Some v -> abs (v - (2.0 * 210.0 + 20.0 + 2.0 * 400.0)) <= 1e-9 | None -> false) cold2
                    | Ok _, Error e | Error e, _ -> check "tiles: E@s2 runs" false e

                // WHERE READ AVOIDANCE BECOMES REAL. The remainder read exists
                // for the consumers that read the input whole -- and the CLI
                // prints every top-level binding, so `A` always had one. With
                // `--print total` selecting the array away, nothing observes
                // `A` but the tiled nest, and the chunks its HIT tiles would
                // have needed are never fetched at all (structural/04, 3.5;
                // the scale run's second pass). The emission says so too: no
                // remainder read is generated.
                let priorPrint = Environment.GetEnvironmentVariable "BLADE_PRINT"
                Environment.SetEnvironmentVariable("BLADE_PRINT", "total")
                try
                    match buildTiled "ic_tiles_unobs" srcS1 with
                    | Error e -> baselineFailed "tiles unobserved input" e
                    | Ok (exeU, cppU) ->
                        check "tiles: with the input selected away, the emission carries no remainder read"
                            (cppU.Contains "has no reader but the tiled nest above" && not (cppU.Contains "for the consumers that read it whole")) ""
                        match runTiled exeU with
                        | Ok warmU ->
                            check "tiles: an unobserved input never fetches the chunks its hit tiles would have needed"
                                (census warmU "[tiles] F: computed 0/2, hit 2/2" && census warmU "[chunks] A: read 0/4 (compute 0, remainder 0)") warmU
                            check "tiles: the selection prints only `total`, and its value is unchanged"
                                (match printedScalar "total" warmU with Some v -> abs (v - (2.0 * 210.0 + 20.0)) <= 1e-9 | None -> false) warmU
                        | Error e -> check "tiles: unobserved-input run" false e
                finally
                    Environment.SetEnvironmentVariable("BLADE_PRINT", priorPrint)
        finally
            Environment.SetEnvironmentVariable("BLADE_TILE_CACHE", priorStore)
            Environment.SetEnvironmentVariable("BLADE_TILE_CACHE_VERBOSE", priorVerbose)
            try Directory.Delete(store, true) with _ -> ()
     with ex -> check "tiles: revision reuse" false ex.Message)

    // ---------------------------------------------------------------
    // Summary
    // ---------------------------------------------------------------
    printFooter "Icechunk Provider" [$"{passed} passed"; $"{failed} failed"]
    if failed > 0 then 1 else 0
