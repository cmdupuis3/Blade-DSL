// Blade Icechunk Store Provider: compile-time metadata extraction from
// Icechunk repos (spec version 2, covering formats 2.0 and 2.1). An Icechunk
// repo is a DIRECTORY of immutable files plus ONE mutable entry point:
//
//     $ROOT/repo            the only mutable file: refs -> snapshots
//     $ROOT/snapshots/      immutable snapshots (the Zarr hierarchy)
//     $ROOT/manifests/      immutable chunk tables
//     $ROOT/chunks/         immutable chunk payloads (raw, headerless)
//     $ROOT/transactions/   transaction logs (PRUNABLE -- never depended on)
//     $ROOT/overwritten/    repo-file backups
//
// Everything but `repo` is immutable, so the compiler resolves ref -> snapshot
// -> manifests -> per-chunk byte ranges ONCE, at compile time, and bakes the
// result: the generated C++ contains no Icechunk logic at all (no FlatBuffers,
// no zstd -- just std::ifstream + offset reads), keeping LinkNeeds = "none".
// Design doc: docs/plans/plan-icechunk-provider.md.
//
// v1 scope (loud, specific refusals outside it -- §9 of the plan):
//   - LOCAL FILESYSTEM repos only. Object-store URLs (s3://, gs://, ...) are
//     refused BY NAME: the runtime is fstream, not an object-store client.
//   - Spec version byte 2 only. Byte 1 is refused by name; anything else is
//     refused as an unknown spec (a spec-3 repo is a new reader, by design).
//   - Virtual chunk refs, repo status Offline, deleted-tag names, ambiguous
//     bare refs, nested groups, writes, .stream and load_compound: each
//     refused by name, never silently degraded.
//   - Array metadata is verbatim `zarr.json` (§2), parsed with the ZARR
//     provider's own `parseArrayMetaV3`, so every Zarr v1 gate (uncompressed
//     single `bytes` codec, little-endian, numeric dtypes, regular chunk
//     grid, the `blade` packed/orbit layout attribute) is inherited verbatim.
//
// THE PAYLOAD DECODE (plan §6.2, DECIDED: option B). Metadata payloads are
// (usually zstd-compressed) FlatBuffers. zstd is ZstdSharp.Port, a MANAGED-only
// port -- no native library, so the compiler keeps its "pure .NET at compile
// time" property; the FlatBuffers accessors are the flatc-generated C# vendored
// under providers/icechunk-format (namespace `generated`), pinned to icechunk
// v2.1.2's own schemas. Both are COMPILE-TIME-ONLY dependencies: neither
// reaches the emitted program, because every ref, manifest and chunk byte
// range is resolved here and baked as static tables (see CppIcechunk below).
//
// CONSISTENCY. Ref resolution (and everything under it) is memoized per
// (canonical key, repo-file stamp) -- plan §3.1 -- and the stamp is PINNED:
// `RepoPinTable` stats `$ROOT/repo` once per repo per compilation and every
// later consumer reads that recorded value rather than re-statting. So
// typecheck, static folds, lowering and codegen all see the SAME snapshot even
// if a writer commits mid-compilation; without the pin the commit would merely
// be a memo MISS, and the later phases would resolve the newer snapshot while
// typecheck's types still described the older one. That closes a TOCTOU the
// Zarr provider structurally has, since it re-walks its store directory in
// every phase. The pin lives as long as the compilation: `resetAxisMint` /
// `resetCaches` drop it (Ide.fs and IdeServe.fs already call the former per
// request).
module Blade.IcechunkProvider

open System
open System.Collections.Concurrent
open System.IO
open Blade.IR
open Blade.Types

// ---------------------------------------------------------------------------
// The canonical key (plan §3.1)
// ---------------------------------------------------------------------------
//
// The whole provider contract threads ONE `path: string`. Rather than widen
// that signature, `repo.checkout(...)` desugars to a canonical key
//
//     "<repoPath>@<kind>:<name>"      e.g. data/weather.icechunk@branch:main
//
// with <kind> one of branch | tag | snapshot | ? ('?' = the bare form, whose
// namespace the provider resolves by cross-namespace uniqueness). A key with
// no '@' is a bare REPO HANDLE load. That string is what enters every
// path-keyed carrier (ProviderPaths, ProviderRoots, the fold/axis caches,
// ProviderReadSpec.FilePath); this module parses it back.

/// Which namespace a checkout names. `RefBare` is the one-argument
/// `checkout("x")` form: resolved across branches, tags and snapshot ids
/// with a uniqueness demand, never a precedence order.
type RefKind =
    | RefBranch
    | RefTag
    | RefSnapshot
    | RefBare

/// A parsed canonical key: the repo root, plus the refspec when one is present.
type RepoKey = {
    RepoPath: string
    /// None = a bare repo-handle load (no '@' in the key).
    Ref: (RefKind * string) option
}

/// The `<kind>` token as it appears in a canonical key.
let kindToken (k: RefKind) : string =
    match k with
    | RefBranch -> "branch"
    | RefTag -> "tag"
    | RefSnapshot -> "snapshot"
    | RefBare -> "?"

/// The surface marker constant that names this namespace (plan §3).
let kindMarker (k: RefKind) : string =
    match k with
    | RefBranch -> "ic.branch"
    | RefTag -> "ic.tag"
    | RefSnapshot -> "ic.snapshot"
    | RefBare -> "(bare)"

let private kindOfToken (tok: string) : RefKind option =
    match tok with
    | "branch" -> Some RefBranch
    | "tag" -> Some RefTag
    | "snapshot" -> Some RefSnapshot
    | "?" -> Some RefBare
    | _ -> None

/// Split a key at its refspec, when it HAS one: the last '@' is a refspec
/// separator only if what follows is `<known-kind>:...`. Everything else --
/// no '@' at all, an '@' with no ':' after it, an unknown kind token -- means
/// the '@' is an ordinary character of the repo PATH, because paths really do
/// contain one: a Windows profile directory (`C:/Users/o@corp/data`), an
/// email-named share, a credentialed URL (`https://user@host/repo`, which then
/// reaches `checkLocalPath`'s object-store refusal instead of dying here as a
/// "malformed key"). The desugar always writes `@<kind>:<name>`, so keys it
/// produces still split, and re-desugaring one is still a no-op.
///
/// MIRRORED, deliberately, by `Blade.ProviderDesugar.isCheckoutKey`
/// (ProviderDesugar.fs): that pass compiles far earlier than this module --
/// its consumers are TypeCheck, Lowering and Ide -- so it cannot call this.
/// The two predicates must agree; change both together.
let private refSuffixOf (key: string) : (string * RefKind * string) option =
    let at = key.LastIndexOf '@'
    if at < 0 then None
    else
        let refspec = key.Substring(at + 1)
        match refspec.IndexOf ':' with
        | colon when colon > 0 ->
            match kindOfToken (refspec.Substring(0, colon)) with
            | Some kind -> Some (key.Substring(0, at), kind, refspec.Substring(colon + 1))
            | None -> None
        | _ -> None

/// Does this key carry a refspec (as opposed to being a bare repo path whose
/// text happens to contain '@')? The public face of `refSuffixOf`.
let hasRefSuffix (key: string) : bool = (refSuffixOf key).IsSome

/// Parse a canonical key. A key with no `@<known-kind>:` suffix is a bare repo
/// path; one that has the suffix must complete it -- an empty repo path or an
/// empty ref name is a loud error, never a silently-mistaken path.
let parseKey (key: string) : Result<RepoKey, string> =
    if String.IsNullOrWhiteSpace key then
        Error "icechunk: empty store path (expected a repo directory, optionally suffixed \"@<kind>:<name>\")"
    else
        match refSuffixOf key with
        | None -> Ok { RepoPath = key; Ref = None }
        | Some (repoPath, kind, name) ->
            if repoPath = "" then
                Error $"icechunk: malformed store key '{key}' -- the repo path before '@' is empty (expected \"<repoPath>@<kind>:<name>\")"
            elif name = "" then
                Error $"icechunk: malformed store key '{key}' -- the ref name after ':' is empty"
            else Ok { RepoPath = repoPath; Ref = Some (kind, name) }

/// Render a canonical key (the inverse of `parseKey` on well-formed input).
let formatKey (key: RepoKey) : string =
    match key.Ref with
    | None -> key.RepoPath
    | Some (kind, name) -> $"{key.RepoPath}@{kindToken kind}:{name}"

/// Object-store schemes refused by name: v1 emits pure std::ifstream C++,
/// with no object-store client at compile time OR run time.
let private urlSchemes =
    [ "s3://"; "s3a://"; "gs://"; "gcs://"; "az://"; "abfs://"; "abfss://"; "r2://"; "http://"; "https://" ]

/// Gate a repo path to the local filesystem (§9). LIVE today.
let checkLocalPath (repoPath: string) : Result<unit, string> =
    match urlSchemes |> List.tryFind (fun s -> repoPath.StartsWith(s, StringComparison.OrdinalIgnoreCase)) with
    | Some s ->
        Error $"icechunk repo '{repoPath}': object-store URLs ('{s}') are not supported -- v1 reads LOCAL FILESYSTEM repos only, because the generated program reads chunks with std::ifstream and links no object-store client; mirror the repo to local disk first"
    | None -> Ok ()

/// The IDENTITY form of a repo path: one directory, however it is spelled, is
/// ONE repo. `Path.GetFullPath` resolves a relative spelling against the
/// process working directory -- which the IDE daemon changes PER REQUEST
/// (IdeServe.fs), so without this the same relative path from two projects is
/// one memo key -- and folds `.` / `..` / duplicated separators; on Windows the
/// result case-folds too, because the filesystem does.
///
/// KEYING ONLY, and deliberately not a rewrite of `RepoKey.RepoPath`. Nothing
/// user-facing is rebuilt from this: the chunk paths baked into generated C++
/// keep the spelling the SOURCE wrote (a relative path stays relative, so the
/// emitted program resolves it against its OWN working directory -- netcdf/zarr
/// parity, and what keeps an exe relocatable), and every diagnostic quotes the
/// user's own text. This is what the read memos, the axis mint table and the
/// axis-tag digest key on, and nothing else.
let canonicalRepoPath (repoPath: string) : string =
    let full = try Path.GetFullPath repoPath with _ -> repoPath
    let trimmed = full.TrimEnd([| '/'; '\\' |])
    let trimmed = if trimmed = "" then full else trimmed
    if OperatingSystem.IsWindows() then trimmed.ToLowerInvariant() else trimmed

/// The identity form of a whole canonical key: the repo path canonicalized,
/// the refspec left exactly as parsed (ref names are case-sensitive, and they
/// name nothing on disk).
let private canonicalKeyOf (key: RepoKey) : string =
    match key.Ref with
    | None -> canonicalRepoPath key.RepoPath
    | Some (kind, name) -> $"{canonicalRepoPath key.RepoPath}@{kindToken kind}:{name}"

// ---------------------------------------------------------------------------
// Crockford base32 (object ids -> file names)
// ---------------------------------------------------------------------------
//
// Ids are Crockford base32, UPPERCASE, no padding, zero bits padding the last
// group on the right: a 12-byte object id (snapshots, manifests, chunks) is
// 20 characters, an 8-byte node id is 13. The alphabet omits I, L, O and U.

/// Crockford's encoding alphabet (index = 5-bit value).
let base32Alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

/// Characters in a 12-byte object id (snapshot / manifest / chunk names).
let objectIdChars = 20

/// Characters in an 8-byte node id.
let nodeIdChars = 13

/// Crockford base32 of a byte string: MSB-first, five bits per character,
/// the final partial group right-padded with zero bits. No '=' padding.
let base32Encode (bytes: byte[]) : string =
    let sb = Text.StringBuilder()
    let mutable acc = 0
    let mutable bits = 0
    for b in bytes do
        acc <- (acc <<< 8) ||| int b
        bits <- bits + 8
        while bits >= 5 do
            bits <- bits - 5
            sb.Append(base32Alphabet.[(acc >>> bits) &&& 31]) |> ignore
    if bits > 0 then
        sb.Append(base32Alphabet.[(acc <<< (5 - bits)) &&& 31]) |> ignore
    sb.ToString()

/// Is `s` shaped like an object id (20 canonical Crockford characters)? The
/// bare-ref resolver uses this to decide whether a name is even a PLAUSIBLE
/// snapshot id before comparing it against the repo's snapshot list.
let isObjectIdForm (s: string) : bool =
    s.Length = objectIdChars && s |> Seq.forall (fun c -> base32Alphabet.IndexOf c >= 0)

// ---------------------------------------------------------------------------
// Repo layout
// ---------------------------------------------------------------------------

/// The one mutable file in a repo: $ROOT/repo.
let repoFilePath (root: string) : string = Path.Combine(root, "repo")

/// $ROOT/snapshots/<id>, the immutable snapshot named by a 12-byte object id.
let snapshotPath (root: string) (id: byte[]) : string =
    Path.Combine(root, "snapshots", base32Encode id)

/// $ROOT/manifests/<id>.
let manifestPath (root: string) (id: byte[]) : string =
    Path.Combine(root, "manifests", base32Encode id)

/// $ROOT/chunks/<id> -- raw, headerless chunk bytes exactly as the Zarr codec
/// pipeline produced them (§2); readers honor the ref's offset and length.
let chunkPath (root: string) (id: byte[]) : string =
    Path.Combine(root, "chunks", base32Encode id)

// ---------------------------------------------------------------------------
// The 39-byte metadata file header (plan §2)
// ---------------------------------------------------------------------------
//
//   [0 .. 11]   magic, 12 bytes: UTF-8 of "ICE" + U+1F9CA (ice cube) + "CHUNK"
//   [12 .. 35]  implementation name, 24 bytes, space-padded
//   [36]        spec version   (1 or 2; 2.0 and 2.1 share byte 2)
//   [37]        file type      (Snapshot 1, Manifest 2, Attributes 3,
//                               TransactionLog 4, Chunk 5, RepoInfo 6)
//   [38]        compression    (0 none, 1 zstd)
//
// then the payload: a (usually zstd-compressed) FlatBuffer.

/// Bytes of the header, before any payload.
let headerSize = 39

/// The magic, spelled as BYTES rather than as a source string literal so no
/// source-encoding accident can change it: UTF-8 for "ICE" + U+1F9CA + "CHUNK".
let magicBytes : byte[] =
    [| 0x49uy; 0x43uy; 0x45uy                      // I C E
       0xF0uy; 0x9Fuy; 0xA7uy; 0x8Auy              // U+1F9CA, 4-byte UTF-8
       0x43uy; 0x48uy; 0x55uy; 0x4Euy; 0x4Buy |]   // C H U N K

/// Bytes of the space-padded implementation-name field.
let implNameSize = 24

/// The spec version this reader accepts. Formats 2.0 and 2.1 are NOT
/// distinguished by the byte (2.1 only adds one optional table field).
let supportedSpecVersion = 2

/// Which kind of metadata file this is. `FtUnknown` is tolerated at parse so
/// the diagnostic can NAME the byte instead of dying on it; consumers that
/// require a particular kind check for it explicitly.
type FileType =
    | FtSnapshot
    | FtManifest
    | FtAttributes
    | FtTransactionLog
    | FtChunk
    | FtRepoInfo
    | FtUnknown of int

/// Human-readable file type, for diagnostics.
let fileTypeName (t: FileType) : string =
    match t with
    | FtSnapshot -> "Snapshot"
    | FtManifest -> "Manifest"
    | FtAttributes -> "Attributes"
    | FtTransactionLog -> "TransactionLog"
    | FtChunk -> "Chunk"
    | FtRepoInfo -> "RepoInfo"
    | FtUnknown b -> $"unknown file type {b}"

let private fileTypeOfByte (b: byte) : FileType =
    match int b with
    | 1 -> FtSnapshot
    | 2 -> FtManifest
    | 3 -> FtAttributes
    | 4 -> FtTransactionLog
    | 5 -> FtChunk
    | 6 -> FtRepoInfo
    | n -> FtUnknown n

/// Payload compression. Unknown bytes are REFUSED (never assumed to be raw:
/// handing a compressed FlatBuffer to the decoder would fail obscurely).
type Compression =
    | CompNone
    | CompZstd

let compressionName (c: Compression) : string =
    match c with
    | CompNone -> "none"
    | CompZstd -> "zstd"

/// A parsed 39-byte metadata header.
type FileHeader = {
    /// The 24-byte implementation-name field with its space padding removed.
    Implementation: string
    /// Spec version byte (always `supportedSpecVersion` once parsed).
    SpecVersion: int
    FileType: FileType
    Compression: Compression
}

let private hexOf (bytes: byte[]) : string =
    bytes |> Array.map (sprintf "%02x") |> String.concat " "

/// Parse the 39-byte header. Pure: byte array in, Result out. `where_` names
/// the file for the diagnostic ("repo file 'data/w.icechunk/repo'").
///
/// Refusals, all LIVE today: short file, wrong magic, spec version 1 (by
/// name), unknown spec version, unknown compression byte.
let parseHeader (where_: string) (bytes: byte[]) : Result<FileHeader, string> =
    if isNull (box bytes) || bytes.Length < headerSize then
        let n = if isNull (box bytes) then 0 else bytes.Length
        Error $"{where_}: {n} bytes is shorter than the {headerSize}-byte Icechunk metadata header -- this is not an Icechunk metadata file"
    else
        let magic = Array.sub bytes 0 magicBytes.Length
        if magic <> magicBytes then
            Error $"{where_}: leading bytes are {hexOf magic}, not the Icechunk magic {hexOf magicBytes} (UTF-8 \"ICE\" + U+1F9CA + \"CHUNK\") -- this is not an Icechunk metadata file"
        else
            let implName =
                Text.Encoding.UTF8.GetString(bytes, magicBytes.Length, implNameSize).TrimEnd(' ')
            let specVersion = int bytes.[magicBytes.Length + implNameSize]
            let fileType = fileTypeOfByte bytes.[magicBytes.Length + implNameSize + 1]
            let compByte = int bytes.[magicBytes.Length + implNameSize + 2]
            if specVersion = 1 then
                Error $"{where_}: Icechunk spec version 1 is not supported -- this reader accepts spec version {supportedSpecVersion} only (which covers formats 2.0 and 2.1); rewrite the repo with an icechunk release that emits spec 2"
            elif specVersion <> supportedSpecVersion then
                Error $"{where_}: unknown Icechunk spec version {specVersion} -- this reader accepts spec version {supportedSpecVersion} only (formats 2.0 and 2.1); a newer spec byte means a newer reader, by design"
            else
                match compByte with
                | 0 | 1 ->
                    Ok { Implementation = implName
                         SpecVersion = specVersion
                         FileType = fileType
                         Compression = (if compByte = 0 then CompNone else CompZstd) }
                | n ->
                    Error $"{where_}: unknown compression byte {n} -- Icechunk defines 0 (none) and 1 (zstd)"

// ---------------------------------------------------------------------------
// Domain model (mirrors the spec §2 tables; filled by the payload decoder)
// ---------------------------------------------------------------------------

/// Repo availability, from the repo file's RepoStatus. Online and ReadOnly
/// both read; Offline refuses (§9).
type RepoStatus =
    | StatusOnline
    | StatusReadOnly
    | StatusOffline

let statusName (s: RepoStatus) : string =
    match s with
    | StatusOnline -> "Online"
    | StatusReadOnly -> "ReadOnly"
    | StatusOffline -> "Offline"

/// One entry of the repo file's `snapshots: [SnapshotInfo]` list.
type SnapshotInfo = {
    /// 12-byte object id; `base32Encode` gives the snapshots/ file name.
    Id: byte[]
    /// The schema's `parent_offset`: the parent's ABSOLUTE index in this same
    /// list ("offset from the start of the list, not from this entry"), with
    /// -1 meaning no parent -- the initial snapshot. The READ path never walks
    /// ancestry (§5.2 rejects tx-log/ancestry mechanisms outright); this is
    /// carried for provenance only.
    ParentOffset: int
    /// Commit time, Unix MILLISECONDS. The wire field is microseconds
    /// (`flushed_at`, "non-leap microseconds since Jan 1, 1970 UTC"); the
    /// decoder divides.
    FlushedAtMillis: int64
    /// Commit message.
    Message: string
}

/// The repo file (`Repo` root table): the only mutable state in a repo.
/// Branches and tags are SEPARATE namespaces -- a branch `x` and a tag `x`
/// can coexist, which is why bare refs demand uniqueness rather than picking
/// a winner.
type RepoInfo = {
    /// (name, index into `Snapshots`).
    Branches: (string * int) list
    /// (name, index into `Snapshots`).
    Tags: (string * int) list
    /// Tombstones: a deleted tag name must never be recreated, so a name
    /// listed here names its tombstone and nothing else.
    DeletedTags: string list
    Snapshots: SnapshotInfo list
    Status: RepoStatus
}

/// A node is either a group or an array; v1 reads ROOT-LEVEL arrays only.
type NodeKind =
    | NodeGroup
    | NodeArray

/// One dimension as the SNAPSHOT records it structurally (ArrayNodeData's
/// `shape_v2`, or the V1 `shape` when a repo still carries one). Cross-checked
/// against the node's own `zarr.json` -- the two must agree (§6.1).
type DimShape = {
    /// Total elements along this dimension.
    ArrayLength: int64
    /// Chunks along this dimension. `None` when the record came from the V1
    /// `DimensionShape`, whose `chunk_length` the schema itself marks
    /// possibly-inaccurate ("the authoritative chunk size comes from the Zarr
    /// metadata in user_data"), so there is nothing trustworthy to compare.
    NumChunks: int64 option
}

/// One `NodeSnapshot` entry of a snapshot (the entries are sorted by path).
type NodeMeta = {
    /// 8-byte node id, STABLE across snapshots for the node's lifetime --
    /// the anchor of §5.2's axis-identity test. Delete-then-recreate mints a
    /// fresh id, which is exactly why the test is sound.
    Id: byte[]
    /// Hierarchy path, e.g. "/temp". The root group is "/".
    Path: string
    Kind: NodeKind
    /// The array's `zarr.json` VERBATIM, as UTF-8 JSON text: shape, dtype,
    /// codecs, dimension_names, attributes (so the `blade` packed-layout
    /// attribute rides along unchanged). Parsed with the Zarr provider's own
    /// `parseArrayMetaV3`, inheriting every v1 gate.
    UserDataJson: string
    /// Structural shape from ArrayNodeData; empty for a group.
    Shape: DimShape list
    /// Dimension names as stored STRUCTURALLY in ArrayNodeData -- cross-checked
    /// against the JSON's `dimension_names` (loud on disagreement, §6.1).
    /// None when the snapshot leaves any of them unnamed.
    DimensionNames: string list option
    /// Per manifest holding some of this array's chunks: the manifest's
    /// 12-byte object id and, per dimension, the half-open chunk-index range
    /// [from, to) it covers. Ranges across manifests must not overlap -- each
    /// chunk coordinate is covered by at most one manifest (§2).
    ManifestRefs: (byte[] * (int64 * int64) list) list
}

/// One resolved snapshot: its id plus its nodes, sorted by path.
type Snapshot = {
    Id: byte[]
    Nodes: NodeMeta list
}

/// Where one chunk's bytes live (plan §6.1). VIRTUAL refs -- an external
/// file/URL with offset/length -- have no case here on purpose: they are
/// refused BY NAME at parse (`virtualChunkRefused`), because honoring one
/// would mean the emitted C++ reads a file outside the repo.
type ChunkLoc =
    | Fill
    | Inline of byte[]
    | Native of NativeChunk

/// A native chunk ref: a byte range of the file `$ROOT/chunks/<ChunkId>`. The
/// reference writer emits one chunk per file (offset 0), but the schema
/// permits packing, so readers must honor offset and length.
///
/// The 12-byte id rather than a path, so the decoders stay PURE -- a decoded
/// manifest says the same thing wherever the repo happens to sit, and unit
/// tests need no repo on disk. `nativeChunkFile` joins it to a root.
and NativeChunk = {
    ChunkId: byte[]
    Offset: int64
    Length: int64
}

/// The file a native chunk ref names, under a repo root.
let nativeChunkFile (root: string) (nc: NativeChunk) : string =
    chunkPath root nc.ChunkId

/// The named refusal for a virtual chunk ref (§9). The decoder calls this
/// rather than inventing a ChunkLoc case, so the refusal has one wording.
/// At decode time the owner is known only by its (spec-stable) NODE ID, which
/// is what `arrayName` carries.
let virtualChunkRefused (arrayName: string) (location: string) : string =
    $"icechunk array '{arrayName}': VIRTUAL chunk refs are not supported in v1 (this chunk points at '{location}', a file outside the repo) -- the emitted reader only opens files under the repo's chunks/ directory; rewrite the virtual refs into native chunks, or read the referenced store directly"

/// One array's chunk table inside a manifest (`ArrayManifest`); entries are
/// sorted lexicographically by `Index`.
type ArrayManifest = {
    /// The 8-byte node id this table belongs to.
    NodeId: byte[]
    Refs: ChunkRef list
}

/// One `ChunkRef`: a plain chunk-grid coordinate vector plus its location.
and ChunkRef = {
    Index: int64 list
    Loc: ChunkLoc
}

/// What a decoded metadata payload turned out to be. One case per file type
/// the read path consumes; transaction logs are decoded by nothing today
/// (they are PRUNABLE under 2.1, so no read path may depend on them).
type Payload =
    | PRepoInfo of RepoInfo
    | PSnapshot of Snapshot
    | PManifest of ArrayManifest list
    | PTransactionLog

let payloadKindName (p: Payload) : string =
    match p with
    | PRepoInfo _ -> "RepoInfo"
    | PSnapshot _ -> "Snapshot"
    | PManifest _ -> "Manifest"
    | PTransactionLog -> "TransactionLog"

/// A chunk-grid coordinate, for diagnostics: "[0, 3]".
let private coordText (c: int64 list) : string =
    "[" + (c |> List.map string |> String.concat ", ") + "]"

// ---------------------------------------------------------------------------
// Payload decode: zstd (ZstdSharp.Port) + FlatBuffers (vendored accessors)
// ---------------------------------------------------------------------------

/// A decode refusal whose message is ALREADY FINAL. It travels out of the
/// decoders unwrapped, so a named refusal (a virtual chunk ref, an unknown
/// union arm, a violated exactly-one rule) is never buried under a generic
/// "malformed FlatBuffer".
exception IcechunkDecodeError of string

let private iceErr (message: string) = raise (IcechunkDecodeError message)

/// Ceiling on a decompressed metadata payload. Metadata is small even for big
/// stores (a manifest for a million chunks is tens of MB); the cap exists so a
/// corrupt frame header cannot ask for the address space.
let maxPayloadBytes = 1 <<< 30

// zstd's two sentinel returns from a frame header: the frame recorded no
// content size, or the header could not be read at all.
let private zstdContentSizeUnknown = UInt64.MaxValue - 1UL
let private zstdContentSizeError = UInt64.MaxValue

/// zstd decompression of a metadata payload (header compression byte 1),
/// through the managed ZstdSharp port. Every failure names the BYTE COUNTS:
/// "the payload did not decompress" without them is unactionable.
///
/// THE OUTPUT IS SIZED BY THE DECODE, NEVER BY THE FRAME HEADER. The reference
/// writer (icechunk-python 2.1.2) emits metadata through a STREAMING zstd
/// encoder -- frame descriptor 0x00, no `Frame_Content_Size` field -- and for
/// such a frame `GetDecompressedSize` does not answer "unknown", it answers the
/// codec's WINDOW SIZE (131072); trusting that as the real length is wrong for
/// every reference-written file. `Unwrap(ReadOnlySpan, maxDecompressedSize)`
/// sizes itself from the decode instead, so one call is correct whether or not
/// the frame pledges a size. The header's declared size is still read, but only
/// to refuse an absurd one by name before any allocation happens; the cap rides
/// into `Unwrap` too, so a frame lying in the other direction cannot claim the
/// address space either.
let decompress (bytes: byte[]) : Result<byte[], string> =
    if isNull (box bytes) then
        Error "icechunk: zstd decompression was handed a null payload"
    elif bytes.Length = 0 then
        Error $"icechunk: the file carries a {headerSize}-byte header and NO payload -- a zstd frame is never zero bytes"
    else
    try
        use dec = new ZstdSharp.Decompressor()
        let hinted = ZstdSharp.Decompressor.GetDecompressedSize(bytes, 0, bytes.Length)
        let sized = hinted <> zstdContentSizeUnknown && hinted <> zstdContentSizeError
        if sized && hinted > uint64 maxPayloadBytes then
            Error $"icechunk: the zstd frame in a {bytes.Length}-byte payload declares {hinted} decompressed bytes, past this reader's {maxPayloadBytes}-byte metadata cap"
        else
            Ok ((dec.Unwrap(ReadOnlySpan<byte>(bytes), maxPayloadBytes)).ToArray())
    with ex ->
        Error $"icechunk: zstd decompression of a {bytes.Length}-byte payload failed: {ex.Message}"

/// Post-header bytes -> plaintext payload bytes, per the header's compression
/// byte. Identity for compression 0; `decompress` for compression 1.
let decompressPayload (compression: Compression) (bytes: byte[]) : Result<byte[], string> =
    match compression with
    | CompNone -> Ok bytes
    | CompZstd -> decompress bytes

/// The 12 bytes of a required `ObjectId12` (snapshot / manifest / chunk id).
let private oid12 (id: Nullable<generated.ObjectId12>) (what: string) : byte[] =
    if not id.HasValue then
        iceErr $"icechunk: {what} is missing its 12-byte object id (a required FlatBuffers field)"
    else
        let v = id.Value
        Array.init 12 (fun j -> v.Bytes j)

/// The 8 bytes of a required `ObjectId8` (node id).
let private oid8 (id: Nullable<generated.ObjectId8>) (what: string) : byte[] =
    if not id.HasValue then
        iceErr $"icechunk: {what} is missing its 8-byte node id (a required FlatBuffers field)"
    else
        let v = id.Value
        Array.init 8 (fun j -> v.Bytes j)

/// A payload's FlatBuffers root table, VERIFIED first. The generated
/// `*Verify.Verify` walkers check the whole buffer -- vtable bounds, offset
/// targets, vector extents, required fields, union arms -- before any accessor
/// dereferences it. Without that pass, a corrupt or foreign file reaches
/// `GetRootAs*`, which simply reads whatever offset the first four bytes name;
/// the failure then surfaces (if at all) far from its cause, or not at all.
/// A refusal here is an `iceErr`, so it comes out of `decodePayload` as an
/// ordinary Error like every other decode refusal.
///
/// NOT through the generated one-call helpers (`Repo.VerifyRepo` and friends).
/// flatc emits those as `VerifyBuffer("", false, ...)`, and Google.FlatBuffers'
/// `Verifier` treats a non-null identifier of any length but 4 as an error --
/// it THROWS `ArgumentException: file identifier must be length 4`. The
/// icechunk schemas declare no `file_identifier` (the 39-byte Icechunk header
/// in front of the payload is what names the file type), so those helpers
/// cannot succeed on any input, valid or not. Passing `null` is what "this
/// buffer has no identifier" is spelled as, and it verifies the same table.
///
/// The verifier's stock budget (1,000,000 tables, depth 64) sits at
/// `CppIcechunk.maxBakedChunks`: a manifest large enough to exhaust it is one
/// the codegen already refuses to bake.
let private verifiedRoot (kind: string)
                         (verifyTable: Google.FlatBuffers.Verifier -> uint32 -> bool)
                         (get: Google.FlatBuffers.ByteBuffer -> 'a)
                         (bytes: byte[]) : 'a =
    let ok =
        try
            let v = Google.FlatBuffers.Verifier(Google.FlatBuffers.ByteBuffer(bytes))
            v.VerifyBuffer(null, false, (fun vf pos -> verifyTable vf pos))
        with _ -> false
    if not ok then
        iceErr $"icechunk: the {bytes.Length}-byte payload is not a valid {kind} FlatBuffer -- the FlatBuffers verifier rejected it (truncated, corrupt, or a file of some other format), so none of its fields may be read"
    get (Google.FlatBuffers.ByteBuffer(bytes))

/// `Repo` root table -> RepoInfo. Branches and tags stay SEPARATE lists (they
/// are separate namespaces), deleted tags come across as tombstones, and the
/// availability enum is mapped by name rather than by a numeric fallthrough.
let private decodeRepoInfo (bytes: byte[]) : RepoInfo =
    let r =
        verifiedRoot "Repo"
            (fun v pos -> generated.RepoVerify.Verify(v, pos))
            (fun bb -> generated.Repo.GetRootAsRepo bb)
            bytes
    // The table repeats the header's spec byte. 0 means the field was left at
    // its flatbuffers default (absent); a genuine disagreement means the repo
    // file contradicts its own header.
    if r.SpecVersion <> 0uy && int r.SpecVersion <> supportedSpecVersion then
        iceErr $"icechunk repo file: the Repo table records spec version {int r.SpecVersion}, but the file header records {supportedSpecVersion} -- the repo file contradicts itself"
    if not r.Status.HasValue then
        iceErr "icechunk repo file: no RepoStatus table (a required field) -- availability is unknown, and this reader will not guess Online"
    let status =
        match r.Status.Value.Availability with
        | generated.RepoAvailability.Online -> StatusOnline
        | generated.RepoAvailability.ReadOnly -> StatusReadOnly
        | generated.RepoAvailability.Offline -> StatusOffline
        | other ->
            iceErr $"icechunk repo file: unknown RepoAvailability {int other} -- this reader knows Online (0), ReadOnly (1) and Offline (2)"
    let refList (len: int) (get: int -> Nullable<generated.Ref>) (what: string) =
        [ for i in 0 .. len - 1 ->
            let rf = get i
            if not rf.HasValue then iceErr $"icechunk repo file: {what} entry {i} is absent"
            elif isNull rf.Value.Name then iceErr $"icechunk repo file: {what} entry {i} has no name (a required field)"
            else (rf.Value.Name, int rf.Value.SnapshotIndex) ]
    { Branches = refList r.BranchesLength (fun i -> r.Branches(i)) "branches"
      Tags = refList r.TagsLength (fun i -> r.Tags(i)) "tags"
      DeletedTags =
        [ for i in 0 .. r.DeletedTagsLength - 1 ->
            let t = r.DeletedTags(i)
            if isNull t then iceErr $"icechunk repo file: deleted_tags entry {i} is null" else t ]
      Snapshots =
        [ for i in 0 .. r.SnapshotsLength - 1 ->
            let s = r.Snapshots(i)
            if not s.HasValue then iceErr $"icechunk repo file: snapshots entry {i} is absent"
            else
                let sv = s.Value
                { Id = oid12 sv.Id $"repo file snapshot entry {i}"
                  ParentOffset = sv.ParentOffset
                  FlushedAtMillis = int64 (sv.FlushedAt / 1000UL)
                  Message = (if isNull sv.Message then "" else sv.Message) } ]
      Status = status }

/// `Snapshot` root table -> Snapshot. `user_data` is decoded as UTF-8 and kept
/// VERBATIM (it is the node's zarr.json); the structural shape and dimension
/// names come across so `arrayMetaOfNode` can cross-check them against it.
let private decodeSnapshot (bytes: byte[]) : Snapshot =
    let s =
        verifiedRoot "Snapshot"
            (fun v pos -> generated.SnapshotVerify.Verify(v, pos))
            (fun bb -> generated.Snapshot.GetRootAsSnapshot bb)
            bytes
    let nodes =
        [ for i in 0 .. s.NodesLength - 1 ->
            let nOpt = s.Nodes(i)
            if not nOpt.HasValue then iceErr $"icechunk snapshot: node entry {i} is absent"
            else
            let ns = nOpt.Value
            let path =
                if isNull ns.Path then iceErr $"icechunk snapshot: node entry {i} has no path (a required field)"
                else ns.Path
            let ud = ns.GetUserDataArray()
            if isNull ud then
                iceErr $"icechunk snapshot node '{path}': no user_data (a required field) -- user_data IS the node's zarr.json, so there is no metadata to read"
            let json = Text.Encoding.UTF8.GetString ud
            let nodeId = oid8 ns.Id $"snapshot node '{path}'"
            match ns.NodeDataType with
            | generated.NodeData.Group ->
                { Id = nodeId; Path = path; Kind = NodeGroup; UserDataJson = json
                  Shape = []; DimensionNames = None; ManifestRefs = [] }
            | generated.NodeData.Array ->
                let a = ns.NodeDataAsArray()
                let shape =
                    if a.ShapeV2Length > 0 then
                        [ for d in 0 .. a.ShapeV2Length - 1 ->
                            let ds = a.ShapeV2(d)
                            if not ds.HasValue then iceErr $"icechunk snapshot node '{path}': shape_v2 entry {d} is absent"
                            else { ArrayLength = int64 ds.Value.ArrayLength; NumChunks = Some (int64 ds.Value.NumChunks) } ]
                    elif a.ShapeLength > 0 then
                        [ for d in 0 .. a.ShapeLength - 1 ->
                            let ds = a.Shape(d)
                            if not ds.HasValue then iceErr $"icechunk snapshot node '{path}': shape entry {d} is absent"
                            else { ArrayLength = int64 ds.Value.ArrayLength; NumChunks = None } ]
                    else []
                let dimNames =
                    if a.DimensionNamesLength = 0 then None
                    else
                        let ns_ =
                            [ for d in 0 .. a.DimensionNamesLength - 1 ->
                                let dn = a.DimensionNames(d)
                                if dn.HasValue && not (isNull dn.Value.Name) then dn.Value.Name else "" ]
                        if ns_ |> List.exists String.IsNullOrEmpty then None else Some ns_
                let manifests =
                    [ for m in 0 .. a.ManifestsLength - 1 ->
                        let mrOpt = a.Manifests(m)
                        if not mrOpt.HasValue then iceErr $"icechunk snapshot node '{path}': manifest ref {m} is absent"
                        else
                            let mr = mrOpt.Value
                            let mid = oid12 mr.ObjectId $"manifest ref {m} of snapshot node '{path}'"
                            let extents =
                                [ for e in 0 .. mr.ExtentsLength - 1 ->
                                    let rg = mr.Extents(e)
                                    if not rg.HasValue then iceErr $"icechunk snapshot node '{path}': manifest ref {m} has no extent for dimension {e}"
                                    else (int64 rg.Value.From, int64 rg.Value.To) ]
                            (mid, extents) ]
                { Id = nodeId; Path = path; Kind = NodeArray; UserDataJson = json
                  Shape = shape; DimensionNames = dimNames; ManifestRefs = manifests }
            | other ->
                iceErr $"icechunk snapshot node '{path}': the NodeData union arm is {int other}, which is neither Array (1) nor Group (2) -- this snapshot was written by a format this reader does not know" ]
    { Id = oid12 s.Id "snapshot"; Nodes = nodes }

/// One `ChunkRef` -> ChunkLoc, enforcing the schema's EXACTLY-ONE rule across
/// the three ref forms. Presence is read from the FlatBuffers field offsets
/// (an absent vector is null, an absent struct has no value), never from a
/// length -- an empty inline vector is "present and empty", which is a
/// different fact from "not inline".
let private decodeChunkRef (owner: string) (cr: generated.ChunkRef) : ChunkRef =
    let index = [ for j in 0 .. cr.IndexLength - 1 -> int64 (cr.Index(j)) ]
    let inlineBytes = cr.GetInlineArray()
    let hasInline = not (isNull inlineBytes)
    let chunkId = cr.ChunkId
    let hasNative = chunkId.HasValue
    let location = cr.Location
    let hasLocation = not (isNull location)
    let hasCompressedLocation = not (isNull (cr.GetCompressedLocationArray()))
    let hasVirtual = hasLocation || hasCompressedLocation
    let forms = (if hasInline then 1 else 0) + (if hasNative then 1 else 0) + (if hasVirtual then 1 else 0)
    if forms <> 1 then
        let named =
            [ (if hasInline then Some "inline" else None)
              (if hasNative then Some "chunk_id" else None)
              (if hasLocation then Some "location" else None)
              (if hasCompressedLocation then Some "compressed_location" else None) ]
            |> List.choose id
        let listing = if List.isEmpty named then "none of them" else String.concat " AND " named
        iceErr $"icechunk manifest for node '{owner}': chunk ref {coordText index} sets {listing} -- the schema requires EXACTLY ONE of inline / chunk_id / location to be present. (A writer using the flatbuffers OBJECT api must null the unused fields explicitly: ChunkRefT initializes chunk_id to a zero object id, and that zero id lands on the wire.)"
    elif hasVirtual then
        iceErr (virtualChunkRefused owner (if hasLocation then location else "<zstd-dictionary-compressed location>"))
    elif hasInline then
        { Index = index; Loc = Inline inlineBytes }
    else
        let cid = oid12 chunkId $"native chunk ref {coordText index} of node '{owner}'"
        { Index = index
          Loc = Native { ChunkId = cid; Offset = int64 cr.Offset; Length = int64 cr.Length } }

/// `Manifest` root table -> the per-array chunk tables it holds.
let private decodeManifest (bytes: byte[]) : ArrayManifest list =
    let m =
        verifiedRoot "Manifest"
            (fun v pos -> generated.ManifestVerify.Verify(v, pos))
            (fun bb -> generated.Manifest.GetRootAsManifest bb)
            bytes
    [ for i in 0 .. m.ArraysLength - 1 ->
        let amOpt = m.Arrays(i)
        if not amOpt.HasValue then iceErr $"icechunk manifest: array entry {i} is absent"
        else
        let am = amOpt.Value
        let nodeId = oid8 am.NodeId $"manifest array entry {i}"
        let owner = base32Encode nodeId
        { NodeId = nodeId
          Refs =
            [ for j in 0 .. am.RefsLength - 1 ->
                let crOpt = am.Refs(j)
                if not crOpt.HasValue then iceErr $"icechunk manifest for node '{owner}': chunk ref entry {j} is absent"
                else decodeChunkRef owner crOpt.Value ] } ]

/// Post-header, post-decompression bytes -> domain values. The bytes are a
/// FlatBuffer whose root table is chosen by the file type. PURE: no repo path
/// enters here, so a decoded value says the same thing wherever the repo sits.
/// Transaction logs decode to nothing on purpose: they are PRUNABLE under 2.1,
/// so no read path may depend on their contents.
let decodePayload (fileType: FileType) (bytes: byte[]) : Result<Payload, string> =
    if isNull (box bytes) then
        Error $"icechunk: a null {fileTypeName fileType} payload"
    else
    try
        match fileType with
        | FtRepoInfo -> Ok (PRepoInfo (decodeRepoInfo bytes))
        | FtSnapshot -> Ok (PSnapshot (decodeSnapshot bytes))
        | FtManifest -> Ok (PManifest (decodeManifest bytes))
        | FtTransactionLog -> Ok PTransactionLog
        | FtAttributes | FtChunk | FtUnknown _ ->
            Error $"icechunk: {fileTypeName fileType} payloads have no reader -- this reader decodes RepoInfo, Snapshot and Manifest files only"
    with
    | IcechunkDecodeError m -> Error m
    | ex ->
        Error $"icechunk: the {fileTypeName fileType} payload ({bytes.Length} bytes) is not a readable FlatBuffer: {ex.Message}"

// ---------------------------------------------------------------------------
// Ref resolution (pure: RepoInfo in, snapshot id out)
// ---------------------------------------------------------------------------

/// How many refs a "no such branch/tag" diagnostic lists before it stops. A
/// repo with thousands of branches (one per experiment, per CI run, per user)
/// is ordinary, and the listing is a HINT -- "did you mean one of these" --
/// not an inventory. Unbounded, one typo printed the whole ref namespace into
/// a terminal, an editor's diagnostic popup and every pinned message.
let private refListCap = 10

let private refNames (rs: (string * int) list) : string =
    if List.isEmpty rs then "(none)"
    else
        let shown = rs |> List.truncate refListCap |> List.map fst |> String.concat ", "
        if rs.Length > refListCap then $"{shown}, and {rs.Length - refListCap} more"
        else shown

let private tombstoneMessage (name: string) : string =
    $"icechunk: '{name}' is a DELETED TAG -- it is listed in the repo's deleted_tags tombstones, and a deleted tag name is never recreated, so it names nothing; pick another ref"

/// Offline refuses; Online and ReadOnly proceed (§9).
let private statusGate (info: RepoInfo) : Result<unit, string> =
    match info.Status with
    | StatusOffline ->
        Error "icechunk: repo status is Offline -- the repo is marked unavailable, so no ref resolves from it (Online and ReadOnly repos both read normally)"
    | StatusOnline | StatusReadOnly -> Ok ()

let private snapshotIdAt (info: RepoInfo) (what: string) (name: string) (idx: int) : Result<byte[], string> =
    if idx < 0 || idx >= info.Snapshots.Length then
        Error $"icechunk repo file is inconsistent: {what} '{name}' points at snapshot index {idx}, but the repo lists {info.Snapshots.Length} snapshots"
    else
        let s = List.item idx info.Snapshots
        Ok s.Id

/// Resolve a refspec against a parsed repo file. Pure and total: the only
/// inputs are the parsed `repo` payload and the refspec, so every refusal
/// here is unit-testable without a repo on disk.
///
/// `RefBare` searches branches, tags and (when the name is object-id shaped)
/// snapshot ids, and demands UNIQUENESS: zero hits lists the repo's actual
/// branches and tags, two hits names both namespaces and points at the
/// `ic.branch` / `ic.tag` / `ic.snapshot` markers. There is deliberately no
/// precedence order between namespaces.
let resolveRef (info: RepoInfo) (kind: RefKind) (name: string) : Result<byte[], string> =
    statusGate info
    |> Result.bind (fun () ->
        let branchHit = info.Branches |> List.tryFind (fun (n, _) -> n = name)
        let tagHit = info.Tags |> List.tryFind (fun (n, _) -> n = name)
        let snapHit =
            if isObjectIdForm name then
                info.Snapshots |> List.tryFind (fun s -> base32Encode s.Id = name)
            else None
        let deleted = info.DeletedTags |> List.contains name
        match kind with
        | RefBranch ->
            match branchHit with
            | Some (_, idx) -> snapshotIdAt info "branch" name idx
            | None ->
                Error $"icechunk: no branch named '{name}' -- branches in this repo: {refNames info.Branches}"
        | RefTag ->
            match tagHit with
            | Some (_, idx) -> snapshotIdAt info "tag" name idx
            | None when deleted -> Error (tombstoneMessage name)
            | None ->
                Error $"icechunk: no tag named '{name}' -- tags in this repo: {refNames info.Tags}"
        | RefSnapshot ->
            match snapHit with
            | Some s -> Ok s.Id
            | None when not (isObjectIdForm name) ->
                Error $"icechunk: '{name}' is not a snapshot id -- snapshot ids are exactly {objectIdChars} Crockford base32 characters (0-9 and A-Z without I, L, O, U), e.g. 1CECHNKREP0F1RSTCMT0"
            | None ->
                Error $"icechunk: no snapshot '{name}' in this repo -- the repo lists {info.Snapshots.Length} snapshots"
        | RefBare ->
            let hits =
                [ (match branchHit with
                   | Some (_, idx) -> Some ("branch", snapshotIdAt info "branch" name idx)
                   | None -> None)
                  (match tagHit with
                   | Some (_, idx) -> Some ("tag", snapshotIdAt info "tag" name idx)
                   | None -> None)
                  (match snapHit with
                   | Some s -> Some ("snapshot", Ok s.Id)
                   | None -> None) ]
                |> List.choose id
            match hits with
            | [ (_, resolved) ] -> resolved
            | [] when deleted -> Error (tombstoneMessage name)
            | [] ->
                Error $"icechunk: no branch, tag or snapshot named '{name}' -- branches: {refNames info.Branches}; tags: {refNames info.Tags}"
            | many ->
                let kinds = many |> List.map fst
                let kindsText = kinds |> String.concat " AND a "
                let markers = kinds |> List.map (fun k -> $"ic.{k}") |> String.concat " / "
                let firstKind = List.head kinds
                Error $"icechunk: '{name}' is ambiguous -- it names a {kindsText} in this repo, and branches, tags and snapshots are separate namespaces with no precedence order between them. Name the namespace with a marker ({markers}): e.g. checkout(\"{name}\", ic.{firstKind}), or the canonical key form '@{firstKind}:{name}'")

// ---------------------------------------------------------------------------
// Memoized resolution (plan §3.1)
// ---------------------------------------------------------------------------

/// One repo's change stamp: mtime ticks, byte length, AND a content hash of
/// `$ROOT/repo`. The repo file is the ONLY mutable object in a repo, so one
/// stamp of it is a complete change stamp for the whole store.
///
/// WHY THE CONTENT HASH IS NOT BELT-AND-BRACES. mtime + length looks like a
/// complete discriminator and is not, for the one rewrite this provider must
/// notice: a BRANCH RESET. Moving a branch swaps a `snapshot_index` inside a
/// fixed-width FlatBuffer field, so the new repo file has EXACTLY the length
/// of the old one; and Windows' filesystem timestamp granularity is ~15.6 ms,
/// so a reset that lands in the same tick as the write before it carries
/// exactly the same mtime too. The stamp is the memo key AND the pin, so an
/// undetected rewrite is not a stale read but a SPLIT one: a later phase would
/// keep answering from the old snapshot's decode.
///
/// The file is tiny (a few hundred bytes to a few kilobytes -- refs, snapshot
/// ids and a status block, nothing per-chunk), and it is read once per repo
/// per compilation because the pin is taken once (`RepoPinTable`), so hashing
/// it costs one small read that the very next step (`readRepoHandle`) makes
/// anyway. First 8 bytes of SHA-256, as an int64: 2^-64 per pair against an
/// accidental collision, and nothing here is adversarial.
let private statRepo (repoPath: string) : int64 * int64 * int64 =
    try
        let f = repoFilePath repoPath
        if File.Exists f then
            let fi = FileInfo f
            let content =
                try
                    use sha = System.Security.Cryptography.SHA256.Create()
                    BitConverter.ToInt64(sha.ComputeHash(File.ReadAllBytes f), 0)
                with _ -> 0L
            (fi.LastWriteTimeUtc.Ticks, fi.Length, content)
        else (0L, 0L, 0L)
    with _ -> (0L, 0L, 0L)

/// THE SNAPSHOT PIN (plan §3.1's consistency point, made real).
///
/// Memoizing on a stamp that is RE-STATTED per call is not a pin: a writer that
/// commits mid-compilation moves the mtime, every later lookup misses, and
/// lowering and codegen then resolve a DIFFERENT snapshot than typecheck did --
/// silently, because each phase's answer is internally consistent. The stamp
/// therefore has to be TAKEN ONCE per repo per compilation and remembered.
///
/// That is this table: canonical repo path -> the stamp first observed for it.
/// Every stamp consumer (`repoStamp` for `VersionStamp` and ProviderStatics'
/// fold cache, `repoMemoStamp` for the read memos) reads it, so all of them
/// pin to the same instant. Per-compilation via `AsyncLocal`, exactly like
/// `AxisMintTable` -- the harness compiles programs in parallel, each in its
/// own async context -- and cleared by the same `resetAxisMint` / `resetCaches`
/// the mint table is, since a pin outliving its compilation would answer for a
/// repo that has since moved on.
module RepoPinTable =
    open System.Threading

    let private table = new AsyncLocal<Map<string, int64 * int64 * int64>>()
    let private current () = match box table.Value with null -> Map.empty | _ -> table.Value

    let reset () = table.Value <- Map.empty

    /// The pinned stamp for a repo, taking (and recording) it on first touch.
    let pin (canonical: string) (stat: unit -> int64 * int64 * int64) : int64 * int64 * int64 =
        let cur = current ()
        match Map.tryFind canonical cur with
        | Some s -> s
        | None ->
            let s = stat ()
            table.Value <- Map.add canonical s cur
            s

    /// What this compilation has pinned, for tests and diagnostics.
    let tryPinned (canonical: string) : (int64 * int64 * int64) option = Map.tryFind canonical (current ())

/// The memo key's stamp: the PINNED (mtime ticks, byte length, content hash)
/// of `$ROOT/repo` -- see `RepoPinTable` and `statRepo`. Test code that
/// regenerates a fixture repo in place must call `resetCaches`, which drops
/// the pin along with the memos.
let private repoMemoStamp (repoPath: string) : int64 * int64 * int64 =
    let canonical = canonicalRepoPath repoPath
    RepoPinTable.pin canonical (fun () -> statRepo repoPath)

/// mtime ticks of `$ROOT/repo`, from the SAME pin the read memos use, so
/// `VersionStamp` (and through it ProviderStatics' fold cache, :122) cannot
/// name a different instant than the resolution it stamps (§8).
let private repoStamp (repoPath: string) : int64 =
    let (ticks, _, _) = repoMemoStamp repoPath
    ticks

/// Bound on each memo, so a long-lived process (the REPL, the test harness)
/// cannot grow one without limit. Correctness never depends on a hit: a miss
/// just re-reads the same immutable files.
let private memoCap = 512

let private memoize (d: ConcurrentDictionary<'k, 'v>) (k: 'k) (f: unit -> 'v) : 'v =
    if d.Count > memoCap then d.Clear()
    d.GetOrAdd(k, Func<'k, 'v>(fun _ -> f ()))

/// Registered clearers, so `resetCaches` does not have to name every memo (and
/// cannot silently miss one added later).
let private memoClearers = ResizeArray<unit -> unit>()

let private newMemo<'k, 'v when 'k: equality> () : ConcurrentDictionary<'k, 'v> =
    let d = ConcurrentDictionary<'k, 'v>()
    memoClearers.Add(fun () -> d.Clear())
    d

// ---------------------------------------------------------------------------
// The axis mint table (plan §5.3)
// ---------------------------------------------------------------------------
//
// Two checkouts of one repo share the index type for dimension `d` IFF the
// axis is UNCHANGED between their snapshots (§5.2): same dim name, same
// extent, and -- when a coordinate variable named after the dim exists -- the
// same coordinate CONTENT, decided from metadata alone (`coordFingerprint`
// below). An unchanged axis hands the recorded `IRIndexType` to
// `zarrStoreToModule`'s `externalDimMap`, so both checkouts' arrays are built
// over ONE identity and `unify` succeeds by its ordinary Id rule -- no
// unifier arm learns that checkouts exist. A diverged axis mints a fresh
// identity, and cross-checkout arithmetic refuses as an index-type mismatch.
//
// The table is per COMPILATION (AsyncLocal, mirroring
// `ProviderRegistry.IdeStores`) and repo-scoped: the repo path is in the key,
// so two repos never share. The key is the CANONICAL path
// (`canonicalRepoPath`), not the source's spelling, so two spellings of one
// directory are one repo -- `ck1.vars.temp - ck2.vars.temp` must not refuse
// because one `ic.load` wrote "data/wx" and the other "./data/wx". The
// as-written spelling still rides everywhere it is user-visible (baked chunk
// paths, diagnostics); only identity is canonicalized.

/// One identity an axis has carried in this compilation.
type AxisIdentity = {
    Extent: int64
    /// `None` when the checkout has no coordinate variable named after the
    /// dim: there is no data to compare, so name + extent IS the identity
    /// (§5.2 condition 3 is vacuous).
    CoordFP: string option
    /// What actually makes two checkouts' arrays unify: `unify` compares index
    /// slots by Id + Tag (Unify.fs:990-992), so this record -- Id and all -- is
    /// what every matching checkout passes in through `externalDimMap`.
    IndexType: IRIndexType
    /// Canonical refspecs that presented this identity ("branch:main"), first
    /// seen first.
    Refs: string list
    /// Why this identity differs from the one it SPLIT FROM. `None` for the
    /// first identity an axis had. Recorded for §5.3's divergence diagnostic
    /// ("axis 'lat' diverged between checkouts ..."), which v1 does not emit
    /// yet -- the split still refuses, with the generic mismatch.
    SplitReason: string option
}

/// Every identity one `(repoPath, dimName)` has carried, NEWEST FIRST. A
/// one-element list is the shared case; a longer one is the split history.
/// Lookup scans the WHOLE list rather than just the head, so a program that
/// checks out A, then a diverged B, then A again gives A back its original
/// identity instead of minting a third.
type AxisMint = { Identities: AxisIdentity list }

/// Reserved id floor for axis-mint index types. These ids are spliced into
/// modules built by OTHER `IRBuilder`s -- typecheck's, lowering's, and the
/// throwaway one `ProviderStatics.storeAxes` builds per axis query -- and
/// every one of those counts up from 0, so an id captured from one of them
/// could collide with an unrelated index type minted by another and make two
/// strangers unify. A reserved range no builder ever reaches removes the
/// question. Codegen already reserves 0x40000000 upward for the same reason
/// (CodeGen.fs:2701) and synthetic sentinels take the negative range (IR.fs);
/// this sits between the builders and codegen.
let axisIdBase = 0x30000000

/// Per-compilation storage for the mint table. AsyncLocal for the same reason
/// `IdeStores` is: the test harness compiles programs in parallel, each in its
/// own async context.
module AxisMintTable =
    open System.Threading

    let private table = new AsyncLocal<Map<string * string, AxisMint>>()
    let private current () = match box table.Value with null -> Map.empty | _ -> table.Value

    /// Fresh compilation: wired everywhere `IdeStores.reset` is, and folded
    /// into `resetCaches`.
    let reset () = table.Value <- Map.empty

    /// Keyed on the CANONICAL repo path (see `canonicalRepoPath`), so the same
    /// directory under two spellings is one axis universe. Callers pass the
    /// path as the source wrote it; canonicalization happens here, once, so no
    /// caller can forget it.
    let tryFind (repoPath: string) (dimName: string) : AxisMint option =
        Map.tryFind (canonicalRepoPath repoPath, dimName) (current ())

    let put (repoPath: string) (dimName: string) (mint: AxisMint) : unit =
        table.Value <- Map.add (canonicalRepoPath repoPath, dimName) mint (current ())

    /// Every identity this compilation has minted, in no particular order --
    /// the REVERSE direction (`tag -> the identity carrying it`) that the
    /// diagnostics decoder needs, since a refusal has the two Tags and not the
    /// (repo, dim) keys they came from. A linear scan on purpose: the table
    /// holds one entry per axis per repo per compilation, and this runs only
    /// while a refusal message is being rendered.
    let allIdentities () : AxisIdentity list =
        current () |> Map.toList |> List.collect (fun (_, m) -> m.Identities)

    let private idLock = obj ()
    let mutable private nextId = axisIdBase

    /// A fresh reserved-range index-type id. Process-global and monotonic ON
    /// PURPOSE: `reset` drops the table but never rewinds this, so an id from
    /// an earlier compilation can never be minted again and mistaken for a
    /// live identity.
    ///
    /// THE CEILING. Monotonic-and-never-rewound is what makes the range
    /// EXHAUSTIBLE: this counter climbs from `axisIdBase` (0x30000000) for the
    /// life of the process, and 0x40000000 upward belongs to CodeGen
    /// (CodeGen.fs:2701). Walking past that boundary would hand a provider axis
    /// an id CodeGen also mints, and equal ids are exactly what makes two index
    /// types unify -- so the failure would be two unrelated axes silently
    /// agreeing, not a crash. 0x10000000 ids is ~268 million axes; a daemon
    /// would have to mint one every millisecond for eight days to get there, so
    /// this is a tripwire on an impossible path, and it says so rather than
    /// letting the range wrap into someone else's.
    let freshId () : int =
        lock idLock (fun () ->
            let id = nextId
            if id >= 0x40000000 then
                failwith "icechunk axis mint exhausted its reserved id range (0x30000000..0x3fffffff): the next id would collide with CodeGen's reserved range and make two unrelated index types unify. This is a long-lived-process leak, not a program error -- restart the compiler/daemon."
            nextId <- nextId + 1
            id)

/// Drop the axis mint table (§5.3) AND the snapshot pin. A compilation must not
/// inherit another one's axis identities -- and in a process that never exits
/// (the REPL, the IDE daemon, the test harness) both tables would otherwise
/// grow without bound. The two go together on purpose: an inherited pin would
/// hold the next compilation to a stamp taken before it started, and the mint
/// table's identities were decided from reads keyed on exactly that stamp. The
/// pin rides along here rather than in a function of its own so that every
/// existing "fresh compilation" call site (Ide.fs, IdeServe.fs) clears it
/// without knowing it exists.
let resetAxisMint () : unit =
    AxisMintTable.reset ()
    RepoPinTable.reset ()

/// Drop every memoized read. Compilation never needs this -- the pinned stamp
/// handles it -- but a test that regenerates a fixture repo AT THE SAME PATH
/// does. The snapshot pin and the axis mint table go with them: the pin would
/// otherwise hold the stamp of the repo that was there BEFORE the regeneration,
/// and the mint table's identities were decided from reads under it.
let resetCaches () : unit =
    for clear in memoClearers do clear ()
    resetAxisMint ()

// ---------------------------------------------------------------------------
// Reading metadata files
// ---------------------------------------------------------------------------

/// The first `headerSize` bytes of a file (short reads tolerated: a truncated
/// file must reach `parseHeader`, which names the truncation).
let private readHeaderBytes (file: string) : byte[] =
    use fs = File.OpenRead file
    let buf = Array.zeroCreate<byte> headerSize
    let mutable off = 0
    let mutable n = 1
    while off < headerSize && n > 0 do
        n <- fs.Read(buf, off, headerSize - off)
        off <- off + n
    Array.sub buf 0 off

/// Header-only validation of an existing metadata file. Everything decidable
/// from 39 bytes: magic, spec version, file type, compression byte.
let private validateFileHeader (where_: string) (file: string) (expected: FileType) : Result<FileHeader, string> =
    try
        parseHeader where_ (readHeaderBytes file)
        |> Result.bind (fun h ->
            if h.FileType <> expected then
                Error $"{where_}: file type is {fileTypeName h.FileType}, expected {fileTypeName expected} -- '{file}' is not the file it is being read as"
            else Ok h)
    with ex ->
        Error $"{where_}: cannot read '{file}': {ex.Message}"

/// Pre-payload validation of a repo root: the path is local, the directory
/// exists, `$ROOT/repo` exists, and its 39-byte header is a spec-2 RepoInfo
/// header with a known compression byte. This is what a bare `ic.load(path)`
/// runs before it binds the repo handle.
let validateRepoFile (root: string) : Result<FileHeader, string> =
    checkLocalPath root
    |> Result.bind (fun () ->
        let file = repoFilePath root
        if not (Directory.Exists root) then
            Error $"icechunk repo '{root}' does not exist (an Icechunk repo is a DIRECTORY holding 'repo' plus snapshots/, manifests/ and chunks/)"
        elif not (File.Exists file) then
            Error $"'{root}' is not an Icechunk repo: there is no 'repo' file at '{file}' (the repo file is the entry point and the only mutable object in a repo)"
        else
            validateFileHeader $"icechunk repo file '{file}'" file FtRepoInfo)

/// Read a metadata file whole: header, file-type check, decompression, and the
/// plaintext payload bytes out.
let private readMetadataFile (where_: string) (file: string) (expected: FileType) : Result<FileHeader * byte[], string> =
    let raw =
        try Ok (File.ReadAllBytes file)
        with ex -> Error $"{where_}: cannot read '{file}': {ex.Message}"
    raw
    |> Result.bind (fun bytes ->
        parseHeader where_ bytes
        |> Result.bind (fun h ->
            if h.FileType <> expected then
                Error $"{where_}: file type is {fileTypeName h.FileType}, expected {fileTypeName expected} -- '{file}' is not the file it is being read as"
            else
                decompressPayload h.Compression (Array.sub bytes headerSize (bytes.Length - headerSize))
                |> Result.map (fun payload -> (h, payload))))

// ---------------------------------------------------------------------------
// Handles: a repo, and a checkout of one snapshot of it
// ---------------------------------------------------------------------------

/// A parsed repo root. `ic.load(path)` with a bare path binds one of these
/// (as an EMPTY provider module: no dims, no vars -- checkout is the factory).
type RepoHandle = {
    Root: string
    Header: FileHeader
    Info: RepoInfo
}

/// One resolved checkout: the snapshot a refspec named, plus its nodes. This
/// is what `LoadAsModule` turns into a dims/vars module.
type CheckoutHandle = {
    Repo: RepoHandle
    Ref: RefKind * string
    SnapshotId: byte[]
    Snapshot: Snapshot
}

/// What a canonical key loaded to: a bare repo handle, or a checkout.
type Loaded =
    | LoadedRepo of RepoHandle
    | LoadedCheckout of CheckoutHandle

/// Read and decode `$ROOT/repo`.
let readRepoHandle (root: string) : Result<RepoHandle, string> =
    checkLocalPath root
    |> Result.bind (fun () -> validateRepoFile root |> Result.map ignore)
    |> Result.bind (fun () ->
        let file = repoFilePath root
        readMetadataFile $"icechunk repo file '{file}'" file FtRepoInfo)
    |> Result.bind (fun (header, payload) ->
        decodePayload header.FileType payload
        |> Result.bind (fun decoded ->
            match decoded with
            | PRepoInfo info -> Ok { Root = root; Header = header; Info = info }
            | other ->
                Error $"icechunk repo file '{repoFilePath root}' decoded as a {payloadKindName other} table, not a RepoInfo"))

// Snapshots and manifests decode to values that hold NO path (only ids and
// byte ranges), so their memos key on the CANONICAL repo path alone: two
// spellings of one repo share the decode, and -- the part that is not an
// optimization -- the same relative path under two working directories (the
// IDE daemon chdirs per request, IdeServe.fs) can no longer collide.
//
// AND ON NO REPO STAMP. Both of these files are CONTENT-ADDRESSED and
// immutable: `$ROOT/snapshots/<id>` and `$ROOT/manifests/<id>` are named by a
// hash of the bytes they hold, so the (repo path, id) pair already IS the
// content identity and a mutable-repo-file stamp in the key can only ever
// force a re-decode of bytes that are known not to have changed. The stamp
// stays in `loadMemo` and `arrayMemo`, whose answers DO depend on the mutable
// repo file (ref resolution picks the snapshot; the array memo is keyed
// through it). `resetCaches` still clears these, which is what a test that
// regenerates a fixture repo at the same path relies on.
let private snapshotMemo = newMemo<string * string, Result<Snapshot, string>> ()
let private manifestMemo = newMemo<string * string, Result<ArrayManifest list, string>> ()

/// Read and decode `$ROOT/snapshots/<id>`. Snapshots are immutable, so the
/// memo only ever re-reads after the repo file itself changed.
let readSnapshot (root: string) (snapshotId: byte[]) : Result<Snapshot, string> =
    memoize snapshotMemo (canonicalRepoPath root, base32Encode snapshotId) (fun () ->
        let file = snapshotPath root snapshotId
        let where_ = $"icechunk snapshot '{base32Encode snapshotId}'"
        if not (File.Exists file) then
            Error $"{where_}: '{file}' is missing -- the snapshot a ref names must exist; an expired/garbage-collected snapshot cannot be read"
        else
            readMetadataFile where_ file FtSnapshot
            |> Result.bind (fun (header, payload) ->
                decodePayload header.FileType payload
                |> Result.bind (fun decoded ->
                    match decoded with
                    | PSnapshot snap -> Ok snap
                    | other -> Error $"{where_} decoded as a {payloadKindName other} table, not a Snapshot")))

/// Read and decode `$ROOT/manifests/<id>`. One manifest commonly covers
/// several arrays, so this memo is what keeps a multi-variable program from
/// re-decompressing the same file per variable.
let readManifest (root: string) (manifestId: byte[]) : Result<ArrayManifest list, string> =
    memoize manifestMemo (canonicalRepoPath root, base32Encode manifestId) (fun () ->
        let file = manifestPath root manifestId
        let where_ = $"icechunk manifest '{base32Encode manifestId}'"
        if not (File.Exists file) then
            Error $"{where_}: '{file}' is missing -- manifests are immutable and must outlive every snapshot referencing them"
        else
            readMetadataFile where_ file FtManifest
            |> Result.bind (fun (header, payload) ->
                decodePayload header.FileType payload
                |> Result.bind (fun decoded ->
                    match decoded with
                    | PManifest arrays -> Ok arrays
                    | other -> Error $"{where_} decoded as a {payloadKindName other} table, not a Manifest")))

// Keyed on BOTH the canonical key and the key as written. The canonical half
// is the identity: without it the same relative path under two working
// directories is one entry (the IDE daemon chdirs per request), and the stamp
// is not a discriminator -- two different repos can share an mtime and a size.
// The as-written half stays because the VALUE carries the path: a
// `RepoHandle.Root` is the spelling the source used, and it reaches the chunk
// paths baked into generated C++, which must stay relative when the source was
// relative. Two spellings of one repo therefore keep separate entries here (a
// re-decode, not a wrong answer) while sharing one AXIS universe, which is the
// only place the split was ever observable.
let private loadMemo = newMemo<string * string * (int64 * int64 * int64), Result<Loaded, string>> ()

let private loadUncached (key: RepoKey) : Result<Loaded, string> =
    readRepoHandle key.RepoPath
    |> Result.bind (fun repo ->
        match key.Ref with
        // A bare handle carries no ref to resolve, so `resolveRef`'s own
        // status gate never runs for it -- gate here instead, so an Offline
        // repo refuses at ic.load itself rather than only at first checkout.
        | None -> statusGate repo.Info |> Result.map (fun () -> LoadedRepo repo)
        | Some (kind, name) ->
            resolveRef repo.Info kind name
            |> Result.mapError (fun e -> $"{e} (repo '{key.RepoPath}')")
            |> Result.bind (fun snapshotId ->
                readSnapshot key.RepoPath snapshotId
                |> Result.map (fun snap ->
                    LoadedCheckout {
                        Repo = repo
                        Ref = (kind, name)
                        SnapshotId = snapshotId
                        Snapshot = snap })))

/// Open a canonical key: parse it, read the repo file, and (when the key
/// carries a refspec) resolve the ref and read its snapshot.
///
/// MEMOIZED per (canonical key, key as written, PINNED repo-file stamp) --
/// plan §3.1's consistency point (see `RepoPinTable`): typecheck, static
/// folds, lowering and codegen all resolve through here and see the same
/// snapshot even when a writer commits mid-compilation.
let load (path: string) : Result<Loaded, string> =
    match parseKey path with
    | Error e -> Error e
    | Ok key ->
        memoize loadMemo (canonicalKeyOf key, path, repoMemoStamp key.RepoPath)
            (fun () -> loadUncached key)

// ---------------------------------------------------------------------------
// Arrays inside a checkout
// ---------------------------------------------------------------------------

let private isValidIdent (s: string) =
    s <> ""
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_')

/// The Blade variable name of a node path. v1 mirrors the Zarr provider's
/// one-level rule: ROOT-LEVEL arrays only, path "/name" with `name` a valid
/// identifier. Deeper groups refuse BY NAME (§6.1). Pure.
let nodeVarName (path: string) : Result<string, string> =
    if path = "" || path.[0] <> '/' then
        Error $"icechunk node path '{path}' is not absolute (paths start at the root group, '/')"
    elif path = "/" then
        Error "icechunk node path '/' is the root group, not an array"
    else
        let rest = path.Substring 1
        if rest.Contains "/" then
            Error $"icechunk array '{path}' lives in a NESTED GROUP -- v1 reads root-level arrays only (paths of the form '/name'); flatten the hierarchy, or read the group as its own store"
        elif not (isValidIdent rest) then
            Error $"icechunk array '{path}': '{rest}' is not a valid Blade identifier"
        else Ok rest

/// Root-level array names in a checkout, in snapshot order.
let arrayNames (ck: CheckoutHandle) : string list =
    ck.Snapshot.Nodes
    |> List.filter (fun n -> n.Kind = NodeArray)
    |> List.choose (fun n -> match nodeVarName n.Path with Ok nm -> Some nm | Error _ -> None)

/// Cap on a variable's CHUNK-GRID SIZE, shared by every path that turns the
/// grid into an array index -- the fold path's chunk table, the axis
/// fingerprint, and the emitter's baked tables. Past it the baked table stops
/// being a good idea (compile time, object size) and the answer is a
/// differently chunked store, not a bigger literal.
///
/// It is also the OVERFLOW guard, which is why it is a shared constant and not
/// a number in the emitter. Every one of those sites computed the grid as
/// `gridDims ... |> List.map int` and multiplied in `int`, so a chunk grid
/// whose product exceeds Int32.MaxValue either TRUNCATED -- a table of the
/// wrong length, silently, with the manifest scattered into it at wrapped
/// indices -- or threw a bare `ArgumentException` out of `Array.create`,
/// which is not a refusal anybody can act on. The product is now computed in
/// int64 and capped BEFORE anything narrows to `int`.
///
/// Lowered from 1_000_000 with the inline-bytes cap (`CppIcechunk`): a million
/// baked entries was never a size anyone wanted to compile, and the two caps
/// together are what bound the generated source.
let maxBakedChunks = 100_000

/// The chunk-grid extents as `int`s, or a named refusal. See `maxBakedChunks`.
///
/// The product is folded with SATURATION rather than computed and then tested:
/// `gridDims` returns ceil-divisions of int64 extents, so the honest product
/// can overflow int64 itself, and nothing above the cap is interesting enough
/// to be worth representing. Every intermediate stays at or below `cap + 1`,
/// and `cap` is small enough that `acc * d` inside the guard cannot overflow.
let gridLens (where_: string) (shape: int64 list) (chunks: int64 list) : Result<int list, string> =
    let dims = ZarrProvider.gridDims shape chunks
    let cap = int64 maxBakedChunks
    let total =
        dims |> List.fold (fun acc d ->
            if acc = 0L || d <= 0L then 0L
            elif acc > cap || d > cap then cap + 1L
            else min (acc * d) (cap + 1L)) 1L
    if total > cap then
        let gridText = dims |> List.map string |> String.concat " x "
        Error $"{where_}: its chunk grid is {gridText}, more than the {maxBakedChunks} chunks this provider resolves into a baked chunk table -- re-chunk the array with larger chunks, or read a subset of it"
    else Ok (dims |> List.map int)

/// Parse one node's `zarr.json` user data with the ZARR provider's v3 parser
/// -- the JSON is verbatim `zarr.json` (§2), so every Zarr v1 gate applies
/// unchanged -- and then CROSS-CHECK the snapshot's own structural record
/// against it (§6.1): rank, per-dimension extent, chunk count, dimension
/// names. A snapshot that disagrees with the metadata it carries is a
/// corrupt repo, and this is the only place that can notice.
///
/// `arrayDir` is empty on purpose: chunks live under $ROOT/chunks by object
/// id, never beside the metadata, and this provider never takes the chunk-key
/// path, so there is no directory to name.
/// (Public so its gates are unit-testable against a hand-built `NodeMeta`, the
/// way `buildChunkTable` and `resolveRef` are: this is the metadata seam every
/// checkout reads every node through, so its refusals want testing WITHOUT a
/// repo that can express them on disk.)
let arrayMetaOfNode (varName: string) (node: NodeMeta) : Result<ZarrProvider.ZarrArrayMeta, string> =
    ZarrProvider.parseArrayMetaV3 varName "" node.UserDataJson
    |> Result.mapError (fun e -> $"icechunk array '{node.Path}': {e}")
    |> Result.bind (fun meta ->
        let where_ = $"icechunk array '{node.Path}'"
        // RANK 0 FIRST, because every gate below it is written over at least
        // one dimension and a rank-0 array slips through all of them: the
        // rank cross-check compares 0 to 0, the per-dimension zip is empty,
        // the chunk grid is the single scalar chunk. What it does NOT survive
        // is anything downstream -- the module builder has no dimension to
        // build an index type from, and the dense emitter's flat index opens
        // with the FIRST loop variable of a loop nest that has none. Refused
        // by name here, where the reason is still legible.
        if meta.Shape.IsEmpty then
            Error $"{where_}: rank-0 arrays are not supported by the icechunk provider -- a Blade array needs at least one index type, and a scalar has none. Store the value as a length-1 rank-1 array, or keep it in the group's attributes"
        elif node.Shape.Length <> meta.Shape.Length then
            Error $"{where_}: the snapshot records {node.Shape.Length} structural dimension(s), but the zarr.json it carries declares rank {meta.Shape.Length} -- the snapshot disagrees with its own metadata"
        else
        // The chunk-grid cap, at the metadata gate: this is the CHECK-phase
        // seam (`checkoutToModule` reads every node through here), so a store
        // whose grid cannot be baked says so before a manifest is decoded.
        match gridLens where_ meta.Shape meta.Chunks with
        | Error e -> Error e
        | Ok _ ->
            let grid = ZarrProvider.gridDims meta.Shape meta.Chunks
            let dimErr =
                List.zip node.Shape (List.zip meta.Shape grid)
                |> List.mapi (fun i (ds, (ext, nch)) ->
                    if ds.ArrayLength <> ext then
                        Some $"{where_}: the snapshot records extent {ds.ArrayLength} for dimension {i}, but its zarr.json declares {ext}"
                    else
                        match ds.NumChunks with
                        | Some k when k <> nch ->
                            Some $"{where_}: the snapshot records {k} chunk(s) along dimension {i}, but its zarr.json's shape/chunk_shape gives {nch}"
                        | _ -> None)
                |> List.tryPick id
            match dimErr with
            | Some e -> Error e
            | None ->
                match node.DimensionNames, meta.DimNames with
                | Some sn, Some jn when sn <> jn ->
                    Error $"""{where_}: the snapshot names its dimensions {String.concat ", " sn}, but its zarr.json names them {String.concat ", " jn}"""
                | Some sn, None ->
                    Error $"""{where_}: the snapshot names its dimensions {String.concat ", " sn}, but the zarr.json it carries has no dimension_names -- the module would be built over synthesized axes while the snapshot names real ones"""
                | _ -> Ok meta)

/// Find a variable in a checkout and parse (and cross-check) its metadata. A
/// name that exists only inside a nested group is refused BY NAME rather than
/// reported as missing.
let findArray (ck: CheckoutHandle) (varName: string) : Result<NodeMeta * ZarrProvider.ZarrArrayMeta, string> =
    let wanted = "/" + varName
    match ck.Snapshot.Nodes |> List.tryFind (fun n -> n.Path = wanted && n.Kind = NodeArray) with
    | Some node ->
        arrayMetaOfNode varName node |> Result.map (fun meta -> (node, meta))
    | None ->
        match ck.Snapshot.Nodes |> List.tryFind (fun n -> n.Path.EndsWith("/" + varName)) with
        | Some nested ->
            Error (match nodeVarName nested.Path with
                   | Error e -> e
                   | Ok _ -> $"icechunk array '{nested.Path}' is not a root-level array")
        | None ->
            let names = arrayNames ck
            let listing = if List.isEmpty names then "(none)" else String.concat ", " names
            Error $"variable '{varName}' not found in icechunk snapshot {base32Encode ck.SnapshotId} -- root-level arrays: {listing}"

/// Union an array's manifests into ONE chunk table in row-major chunk-grid
/// order: entry `i` locates the chunk at `gridCoords`[i] (§6.1). A coordinate
/// no manifest covers reads as the array's fill value, per Zarr semantics.
///
/// Loud on every way the manifests can disagree with the metadata: a
/// wrong-rank index vector, a coordinate outside the grid, or two manifests
/// covering the same chunk (§2 requires non-overlapping ChunkIndexRanges).
let buildChunkTable (meta: ZarrProvider.ZarrArrayMeta) (manifests: ArrayManifest list) : Result<ChunkLoc[], string> =
    // `Array.create total` is the site the int64 grid product protects (see
    // `gridLens`): a truncated product allocates a table of the wrong length
    // and a negative one throws out of here uncaught.
    match gridLens $"icechunk array '{meta.Name}'" meta.Shape meta.Chunks with
    | Error e -> Error e
    | Ok lens ->
    let strides = ZarrProvider.rowMajorStrides lens
    let total = lens |> List.fold (*) 1
    let rank = lens.Length
    let table = Array.create total Fill
    let filled = Array.create total false
    let mutable err : string option = None
    for m in manifests do
        for r in m.Refs do
            if err.IsNone then
                if r.Index.Length <> rank then
                    err <- Some $"icechunk manifest for '{meta.Name}': chunk index {coordText r.Index} is rank {r.Index.Length}, but the array's chunk grid is rank {rank}"
                elif List.exists2 (fun (c: int64) (n: int) -> c < 0L || c >= int64 n) r.Index lens then
                    err <- Some $"icechunk manifest for '{meta.Name}': chunk index {coordText r.Index} is outside the chunk grid {coordText (lens |> List.map int64)}"
                else
                    let flat = List.fold2 (fun acc (c: int64) (s: int) -> acc + int c * s) 0 r.Index strides
                    if filled.[flat] then
                        err <- Some $"icechunk manifests for '{meta.Name}' OVERLAP: chunk {coordText r.Index} is covered by more than one manifest (§2 requires each chunk coordinate to be covered at most once)"
                    else
                        filled.[flat] <- true
                        table.[flat] <- r.Loc
    match err with
    | Some e -> Error e
    | None -> Ok table

// ---------------------------------------------------------------------------
// The resolved array: metadata + baked chunk table
// ---------------------------------------------------------------------------

/// One array of one checkout, fully resolved at compile time: its metadata
/// plus the chunk table (one `ChunkLoc` per chunk-grid coordinate, row-major).
/// The F# fold path and the C++ emitter read EXACTLY this, so they cannot
/// disagree about where a chunk lives.
type ResolvedArray = {
    /// The repo path AS GIVEN -- a relative path stays relative, which is what
    /// makes a baked path resolve against the emitted program's working
    /// directory (netcdf/zarr parity), not the compiler's.
    Root: string
    Ref: RefKind * string
    SnapshotId: byte[]
    VarName: string
    Node: NodeMeta
    Meta: ZarrProvider.ZarrArrayMeta
    /// Row-major over the chunk grid; `Fill` where no manifest covers a coordinate.
    Table: ChunkLoc[]
}

let private repoHandleRefusal (path: string) : string =
    $"'{path}' is an icechunk REPO HANDLE, not a checkout -- a repo handle has no variables; check a ref out first (`repo.checkout(\"main\")`, whose canonical key is '{path}@branch:main')"

/// Read every manifest an array's node points at, taking only ITS chunk table
/// out of each. Loud when a manifest ref's extents are the wrong rank, when a
/// named manifest holds no table for this node, or when a manifest holds a
/// chunk outside the extents it declares (§2: the extents ARE the coverage
/// claim the non-overlap invariant is built on).
let private collectManifests (root: string) (node: NodeMeta) (meta: ZarrProvider.ZarrArrayMeta) : Result<ArrayManifest list, string> =
    let rank = meta.Shape.Length
    let rec go acc refs =
        match refs with
        | [] -> Ok (List.rev acc)
        | ((mid: byte[]), (extents: (int64 * int64) list)) :: rest ->
            let mname = base32Encode mid
            if extents.Length <> rank then
                Error $"icechunk array '{node.Path}': manifest '{mname}' declares {extents.Length} chunk-index range(s) for a rank-{rank} array"
            else
                match readManifest root mid with
                | Error e -> Error e
                | Ok arrays ->
                    match arrays |> List.tryFind (fun am -> am.NodeId = node.Id) with
                    | None ->
                        Error $"icechunk array '{node.Path}': manifest '{mname}' holds no chunk table for node {base32Encode node.Id} -- the snapshot points at a manifest that does not cover this array"
                    | Some am ->
                        let outside =
                            am.Refs |> List.tryFind (fun r ->
                                r.Index.Length <> rank
                                || List.exists2 (fun (c: int64) ((lo, hi): int64 * int64) -> c < lo || c >= hi) r.Index extents)
                        match outside with
                        | Some r ->
                            let ext = extents |> List.map (fun (a, b) -> $"[{a}, {b})") |> String.concat " x "
                            Error $"icechunk array '{node.Path}': manifest '{mname}' declares extents {ext} but holds chunk {coordText r.Index}, outside them"
                        | None -> go (am :: acc) rest
    go [] node.ManifestRefs

/// Keyed like `loadMemo`, and for the same two reasons: the canonical key is
/// the identity, the as-written key is kept because `ResolvedArray.Root` is
/// what the emitter bakes.
let private arrayMemo = newMemo<string * string * string * (int64 * int64 * int64), Result<ResolvedArray, string>> ()

/// Resolve one variable of one checkout all the way to its chunk table.
/// Memoized per (canonical key, key as written, variable, PINNED repo-file
/// stamp) alongside `load`, so the typecheck, fold, lowering and codegen passes
/// that each ask for the same variable pay the manifest decode ONCE and see one
/// answer -- one answer even across a mid-compilation commit, since the stamp
/// is pinned rather than re-statted.
let resolveArray (path: string) (varName: string) : Result<ResolvedArray, string> =
    match parseKey path with
    | Error e -> Error e
    | Ok key ->
        memoize arrayMemo (canonicalKeyOf key, path, varName, repoMemoStamp key.RepoPath) (fun () ->
            match load path with
            | Error e -> Error e
            | Ok (LoadedRepo _) -> Error (repoHandleRefusal path)
            | Ok (LoadedCheckout ck) ->
                findArray ck varName
                |> Result.bind (fun (node, meta) ->
                    collectManifests ck.Repo.Root node meta
                    |> Result.bind (fun manifests ->
                        buildChunkTable meta manifests
                        |> Result.map (fun table ->
                            { Root = ck.Repo.Root
                              Ref = ck.Ref
                              SnapshotId = ck.SnapshotId
                              VarName = varName
                              Node = node
                              Meta = meta
                              Table = table }))))

// ---------------------------------------------------------------------------
// Compile-time payload read (the static fold), through the shared core
// ---------------------------------------------------------------------------

/// One native chunk's bytes: a byte range of an immutable file under
/// $ROOT/chunks. Failures throw, because `readArrayDataFrom` turns an
/// exception into an Error with this message -- never into silent zeros.
let private readNativeChunk (ra: ResolvedArray) (coords: int64 list) (nc: NativeChunk) : byte[] =
    let file = nativeChunkFile ra.Root nc
    if nc.Length < 0L || nc.Length > int64 maxPayloadBytes then
        failwith $"icechunk array '{ra.VarName}': chunk {coordText coords} declares a {nc.Length}-byte range, which is not a readable chunk length"
    if not (File.Exists file) then
        failwith $"icechunk array '{ra.VarName}': chunk file '{file}' for chunk {coordText coords} is missing -- chunk files are immutable, so a missing one means the snapshot was expired or garbage-collected"
    use fs = File.OpenRead file
    if nc.Offset < 0L || nc.Offset + nc.Length > fs.Length then
        failwith $"icechunk array '{ra.VarName}': chunk {coordText coords} claims bytes [{nc.Offset}, {nc.Offset + nc.Length}) of '{file}', which holds {fs.Length} bytes"
    fs.Seek(nc.Offset, SeekOrigin.Begin) |> ignore
    let buf = Array.zeroCreate<byte> (int nc.Length)
    let mutable off = 0
    let mutable n = 1
    while off < buf.Length && n > 0 do
        n <- fs.Read(buf, off, buf.Length - off)
        off <- off + n
    if off <> buf.Length then
        failwith $"icechunk array '{ra.VarName}': chunk {coordText coords} read {off} of {nc.Length} bytes from '{file}'"
    buf

/// The icechunk `ChunkSource` (plan §7): the baked table answers every
/// coordinate -- inline bytes come straight out of the manifest, a native
/// chunk is a byte range of a file under $ROOT/chunks, and a coordinate no
/// manifest covers is ABSENT, which the shared core turns into fill. Nothing
/// above this seam is icechunk-specific, so fill handling, edge intersection,
/// packed-pool reassembly and wreath pools cannot drift from Zarr's.
let private icechunkChunkSource (ra: ResolvedArray) : ZarrProvider.ChunkSource =
    // Already guaranteed by `buildChunkTable` for anything that came through
    // `resolveArray`; restated because a `ResolvedArray` is a plain record and
    // this is the other place the grid narrows to `int`.
    let lens =
        match gridLens $"icechunk array '{ra.VarName}'" ra.Meta.Shape ra.Meta.Chunks with
        | Ok l -> l
        | Error e -> failwith e
    let strides = ZarrProvider.rowMajorStrides lens
    let flatOf (coords: int64 list) =
        List.fold2 (fun acc (c: int64) (s: int) -> acc + int c * s) 0 coords strides
    { Label = coordText
      Fetch = fun coords ->
        match ra.Table.[flatOf coords] with
        | Fill -> None
        | Inline bytes -> Some (ZarrProvider.decodeChunk ra.Meta.Codec bytes)
        | Native nc -> Some (ZarrProvider.decodeChunk ra.Meta.Codec (readNativeChunk ra coords nc)) }

let private adaptVarData (d: ZarrProvider.ZarrVarData) : Blade.ProviderRegistry.ProviderVarData =
    { DimLengths = d.DimLengths
      Payload =
        match d.Payload with
        | ZarrProvider.ZFloats xs -> Blade.ProviderRegistry.PFloats xs
        | ZarrProvider.ZInts xs -> Blade.ProviderRegistry.PInts xs }

/// Whole-variable read for `let static A = ic.read(ck.vars.A)`. Structured
/// through the real gates -- key parse, repo file, ref resolution, snapshot,
/// node lookup, zarr.json parse and cross-check, manifests, chunk table --
/// and then through the SHARED assembly core over the icechunk chunk source.
let readVarData (path: string) (varName: string) : Result<Blade.ProviderRegistry.ProviderVarData, string> =
    match load path with
    | Error e -> Error e
    | Ok (LoadedRepo _) -> Error (repoHandleRefusal path)
    | Ok (LoadedCheckout ck) ->
        findArray ck varName
        |> Result.bind (fun (_, meta) ->
            // Same steering the Zarr provider applies: StaticValue has no
            // packed carrier, so a pool would fold to a WRONG dense shape.
            if meta.Blade.IsSome then
                Error $"variable '{varName}' has a packed (blade: layout=packed) pool layout -- triangular and orbit (iterated-wreath) variables do not fold at compile time; bind with a plain `let ... |> <alias>.read`"
            else
                resolveArray path varName
                |> Result.bind (fun ra ->
                    ZarrProvider.readArrayDataFrom (icechunkChunkSource ra) ra.Meta
                    |> Result.map adaptVarData))

/// Wreath (OrbIdx depth >= 2) canonical pool read. Presence of this function
/// in the spec is the provider's wreath CAPABILITY flag at every seam; the
/// pool itself rides the shared chunk-source core.
let readWreathPool (path: string) (varName: string) : Result<Blade.ProviderRegistry.ProviderVarData, string> =
    match load path with
    | Error e -> Error e
    | Ok (LoadedRepo _) ->
        Error $"'{path}' is an icechunk repo handle, not a checkout -- check a ref out first"
    | Ok (LoadedCheckout ck) ->
        findArray ck varName
        |> Result.bind (fun (_, meta) ->
            match meta.Blade with
            | Some l when l.Group.Sym = SymWreath ->
                resolveArray path varName
                |> Result.bind (fun ra ->
                    ZarrProvider.readPackedPoolFrom (icechunkChunkSource ra) ra.Meta
                    |> Result.map adaptVarData)
            | Some _ -> Error $"variable '{varName}' has a depth-1 packed (sym/antisym) layout, not an orbit head"
            | None -> Error $"variable '{varName}' is an ordinary dense array, not an orbit (iterated-wreath) pool")

// ---------------------------------------------------------------------------
// Axis identity: the coordinate fingerprint (plan §5.2)
// ---------------------------------------------------------------------------

let private sha256Hex (bytes: byte[]) : string =
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash bytes |> Array.map (sprintf "%02x") |> String.concat ""

/// One chunk location, canonically. Inline bytes hash; a native ref is its
/// content-addressed chunk id plus the byte range it claims; a coordinate no
/// manifest covers is the fill ABSENCE, which is a fact about the axis exactly
/// as much as a present chunk is.
let private chunkLocText (loc: ChunkLoc) : string =
    match loc with
    | Fill -> "-"
    | Inline b -> "i:" + sha256Hex b
    | Native nc -> $"c:{base32Encode nc.ChunkId}+{nc.Offset}+{nc.Length}"

/// The §5.2 fingerprint of dimension `dimName`'s coordinate variable in this
/// checkout, or `None` when the checkout has no coordinate variable named
/// after the dim (then name + extent is the whole identity).
///
/// METADATA ONLY -- the coordinate array's stable node id, its `user_data`
/// bytes, and its chunk-ref table -- so comparing two axes never reads a
/// chunk. Chunk files and manifests are immutable, so equal refs imply
/// byte-identical content, and a commit that never touched the coordinate
/// array keeps pointing at the same chunk ids: the common case (data commits
/// on a fixed grid) compares equal instantly. The failure direction is a false
/// NEGATIVE -- a compaction that rewrites a manifest to new ids with identical
/// bytes refuses arithmetic that would have been sound -- never a false accept.
///
/// The three components stay NAMED rather than collapsing into one hash, so
/// the divergence diagnostic can eventually say WHICH of them moved.
///
/// Reading the coordinate array's manifests is the only file access this adds
/// over a P2 checkout load, and `resolveArray` memoizes it alongside every
/// other read of the same checkout.
///
/// The three METADATA components of one variable's content identity, named
/// rather than collapsed into one hash so the divergence diagnostic can
/// eventually say WHICH of them moved. Shared by the coordinate fingerprint
/// (below) and the packed-pool fingerprint (§5.3's residual, closed by
/// `resolvePool`): a pool axis's identity is its own variable's content, since
/// a pool is not a dimension and has no coordinate of its own.
let private varFingerprint (key: string) (ck: CheckoutHandle) (varName: string) : string =
    match resolveArray key varName with
    | Error _ ->
        // The variable is there but its manifests would not read. Equal to
        // ITSELF (the same snapshot re-checked out shares) and to nothing else:
        // the snapshot id is what makes it so.
        $"unreadable@{base32Encode ck.SnapshotId}"
    | Ok ra ->
        let grid =
            ZarrProvider.gridDims ra.Meta.Shape ra.Meta.Chunks
            |> List.map string |> String.concat "x"
        let refs = ra.Table |> Array.map chunkLocText |> String.concat ","
        let chunksHash = sha256Hex (Text.Encoding.UTF8.GetBytes $"{grid}|{refs}")
        let userHash = sha256Hex (Text.Encoding.UTF8.GetBytes ra.Node.UserDataJson)
        $"node={base32Encode ra.Node.Id};user={userHash};chunks={chunksHash}"

let private coordFingerprint (key: string) (ck: CheckoutHandle)
                             (arrays: ZarrProvider.ZarrArrayMeta list)
                             (dimName: string) : string option =
    // The Zarr module builder's own coordinate rule (`isCoordinateArr`): a
    // dense rank-1 array named after its single, NAMED dimension. An array
    // whose dimension name is synthesized (`lat_dim0`) is not a coordinate of
    // `lat` -- and is not in the dims struct either.
    let isCoord (a: ZarrProvider.ZarrArrayMeta) =
        a.Name = dimName && a.Blade.IsNone && a.Shape.Length = 1 && a.DimNames = Some [dimName]
    if not (arrays |> List.exists isCoord) then None
    else Some (varFingerprint key ck dimName)

/// Prefix of the axis tag. `__`-prefixed on purpose -- see `axisTag`.
///
/// The literal itself lives in `Types.providerAxisTagPrefix`, beside the two
/// pool prefixes and the `isProviderAxisTag` family predicate: the typecheck
/// seams that must read this tag NOMINATIVELY compile before this file does,
/// and one spelling in one place is what keeps them from drifting apart.
let axisTagPrefix = providerAxisTagPrefix

/// The DIM NAME inside an axis tag, for display only:
/// `__icaxis|lat@ic_wx:9f3a1c...`
/// -> `lat`, and `...#2` -> `lat#2`. The tag is `__`-prefixed so every seam that
/// reads a tag as a user-written NAME leaves it alone (see `axisTag`), and the
/// type printer's nominal-name map drops it for exactly that reason -- so a
/// checkout array printed as `Array<Float64 like Idx<24>, Idx<10>, Idx<12>>`
/// while the store's own dim names sat inside the tag. This hands the printer
/// back the one part of the tag that IS a name. The split ordinal rides along on
/// purpose: two identities of one dim that no longer unify must not print
/// identically.
///
/// PARSED FROM THE RIGHT, both separators, because BOTH are legal inside the
/// fields to their left and neither is validated out. `@` separates the dim
/// name from the repo label, and the repo label is the one field minted HERE
/// (`taggedIdentity`), so the LAST `@` is ours and any earlier one belongs to a
/// dim genuinely named `a@b`. `#` marks the split ordinal, and the repo label
/// is a directory name as written -- `wx#2/` is a perfectly ordinary directory
/// -- so a `#` counts only in the trailing position and only ahead of digits.
/// Reading either from the left decoded `lat` out of `a@b@wx:...` and `lat#ird`
/// out of a repo called `we#ird`, in a string this function now hands straight
/// to the user in refusal messages.
///
/// The parse itself is `splitIdentityTag`, shared with the pool decoder below,
/// because all three tag spellings are one shape under three prefixes.
let private splitIdentityTag (prefix: string) (tag: string) : (string * string) option =
    if not (tag.StartsWith prefix) then None
    else
        let rest = tag.Substring prefix.Length
        let (body, ordinal) =
            match rest.LastIndexOf '#' with
            | h when h > 0 && h < rest.Length - 1
                     && rest.Substring(h + 1) |> Seq.forall Char.IsDigit ->
                (rest.Substring(0, h), rest.Substring h)
            | _ -> (rest, "")
        match body.LastIndexOf '@' with
        | i when i > 0 -> Some (body.Substring(0, i), ordinal)
        | _ -> None

let tryAxisTagName (tag: string) : string option =
    splitIdentityTag axisTagPrefix tag |> Option.map (fun (name, ordinal) -> name + ordinal)

/// The display name of ANY provider provenance tag -- what a refusal message
/// prints where the raw tag used to appear. Registered into Types' decoder hook
/// by `ProviderStatics.install`; every refusal site reads it through there.
///
/// Deliberately WIDER than `tryAxisTagName`, which stays dense-only. That one
/// feeds `Ide.indexNamesOf`, where the string is rendered as an index-type NAME
/// inside `Array<Float64 like Idx<lat>>` -- and a packed pool has no such
/// spelling, which is exactly what section 15's `IsNone` pins. A DIAGNOSTIC has
/// no such constraint: there the only failure mode is printing forty characters
/// of internal identity at a user, which is what
/// `'__icpool|cov@ic_pool_twin:ce36e95a9686e348'` did in the cross-repo pool
/// refusal.
///
/// A pool is not a dimension -- its cells ARE the variable -- so it decodes to
/// `pool(cov)` / `orbit_pool(w)` rather than to a bare name a dim could
/// collide with. The split ordinal rides along for the same reason it does on a
/// dense axis: two identities that no longer unify must not print identically.
let tryProviderTagName (tag: string) : string option =
    let wrap (kind: string) (prefix: string) =
        splitIdentityTag prefix tag |> Option.map (fun (v, ordinal) -> $"{kind}({v}){ordinal}")
    match tryAxisTagName tag with
    | Some n -> Some n
    | None ->
        match wrap "orbit_pool" providerOrbPoolTagPrefix with
        | Some n -> Some n
        | None -> wrap "pool" providerPoolTagPrefix

/// The identity an axis carries in the TYPE, beyond its Id. Shape:
///
///     __icaxis|lat@ic_wx:9f3a1c...  the axis as this repo first presented it
///     __icaxis|lat@ic_wx:9f3a1c...#2  the SECOND identity that (repo, dim) took on
///
/// The repo label is the directory name as written, plus 16 hex of the
/// CANONICAL path's digest, so two repos that happen to share a directory name
/// are still distinct axes -- §5.3's "different repos never share", stated
/// where the type system can read it -- while two spellings of ONE repo are one
/// axis.
///
/// WHY A TAG AND NOT JUST THE ID. §5.3 expects the shared `IRIndexType.Id` to
/// carry the whole story ("unify then succeeds by the ordinary Id rule"). It
/// carries the SHARING half and none of the refusal half: `unify`'s ArrayElem
/// arm compares rank, tags and symmetry and NEVER Ids (Unify.fs:887-911 --
/// Unify.fs:990's Id rule is the IRTArrow SLOT arm, which arrays do not take),
/// and co-iteration decides axis agreement from `Tag` alone
/// (`indexNamesCoIterable` at rank 1, `indexRecordsAgree` for the rank->=2
/// product). Untagged provider axes are "some axis of this extent", so two of
/// them co-iterate freely -- measurably: before this, subtracting a variable of
/// one repo from the same variable of a DIFFERENT repo typechecked. The tag is
/// what makes a diverged axis refuse.
///
/// WHY `__`. A tag is also a user-facing NAME, and three seams key on that:
/// `checkArrayIndexTags` skips `__` slots, `elemTypeForIterationIndex` hands
/// `Nat<tag>` to iteration params only for non-`__` tags, and
/// `Ide.indexNamesOf` builds its display map from non-`__` tags (it makes its
/// own exception here, decoding the dim name back out with `tryAxisTagName`).
/// A plain name (`lat@ic_wx:9f3a`) therefore turns every ordinary `A(2, 1)`
/// into a BL4003 "indexed with untagged integer" warning advising a cast to a
/// name the user never wrote. `__` keeps all three quiet while leaving the two
/// co-iteration predicates -- which have no `__` exemption -- free to refuse.
///
/// A FOURTH seam used to key on `__` and no longer does: `unify`'s
/// `indexPairIncompatible` exempted synthetic tags outright, which let an
/// ASCRIPTION relabel a diverged axis and walk around the arithmetic refusal
/// entirely (plan §5, *Alias laundering*). It now asks `gatesNominally`, and
/// the provider family (`Types.isProviderAxisTag`) gates like a user name --
/// provenance is an identity, not a kind sentinel. Other `__` tags keep the
/// exemption. Residual: a function BOUNDARY still accepts a diverged axis when
/// the parameter's index type is spelled ANONYMOUSLY, which is not a provider
/// fact -- an argument position accepts a differently-named index type of equal
/// extent for two plain `Idx<5>` aliases too (IcechunkTests §18 (g), (h)).
let private taggedIdentity (prefix: string) (repoPath: string) (dimName: string) (ordinal: int) : string =
    let baseName =
        // The directory name AS WRITTEN: this half of the tag is a display
        // name, and it is printed back at the user in every mismatch.
        let trimmed = repoPath.TrimEnd([| '/'; '\\' |])
        let n = try Path.GetFileName trimmed with _ -> ""
        if String.IsNullOrEmpty n then "repo" else n
    // The digest is the DISCRIMINATING half, so it hashes the canonical path
    // (two spellings of one repo are one axis universe, matching the mint
    // table's key) and keeps 16 hex characters, not 4. Four hex is 2^-16 per
    // pair: a collision would hand two DIFFERENT repos the same axis tag, and
    // an axis tag is a LICENSE -- co-iteration reads agreement off the tag
    // alone -- so the failure direction is silently permitting cross-repo
    // arithmetic, not refusing sound arithmetic.
    let digest = (sha256Hex (Text.Encoding.UTF8.GetBytes (canonicalRepoPath repoPath))).Substring(0, 16)
    let suffix = if ordinal <= 1 then "" else $"#{ordinal}"
    $"{prefix}{dimName}@{baseName}:{digest}{suffix}"

/// A DENSE dimension's identity tag (the shape documented above).
let private axisTag (repoPath: string) (dimName: string) (ordinal: int) : string =
    taggedIdentity axisTagPrefix repoPath dimName ordinal

/// A PACKED variable's POOL-axis identity tag -- the same shape over the
/// variable's name instead of a dim name, under one of the two pool prefixes
/// (`Types.providerPoolTagPrefix` / `providerOrbPoolTagPrefix`).
///
/// WHY THE POOL NEEDED ONE. A pool axis is not a store dimension: its extent is
/// a derived cardinality (C(n+r-1, r) and friends), which is why
/// `zarrStoreToModule`'s `sharedDims` deliberately drops it and why
/// `externalDimMap` -- the channel P3's dense identities ride -- can never reach
/// it. It was therefore minted `Tag = None`, and the refusal machinery is
/// PERMISSIVE on None in both directions (`indexNamesCoIterable`'s `| _ -> true`,
/// `indexPairIncompatible` falling through to its symmetry arm), so
/// `ca.vars.cov - cb.vars.cov` across two DIFFERENT repos typechecked -- the
/// exact pre-P3 defect, surviving for every packed variable.
///
/// WHY TWO PREFIXES. The Tag doubles as the record's KIND sentinel and a pool
/// comes in two kinds (depth-1 simplex: `IxKPlain`; iterated wreath: `IxKOrbit`,
/// whose record would otherwise carry "__orbidx"). `ixKindOfTag` maps the
/// wreath spelling back to `IxKOrbit` and lets the depth-1 spelling fall through
/// to `IxKPlain`, which is what `IRValidate`'s Tag/IxKind agreement check needs.
/// `indexNamesCoIterable` recognises the family (`isProviderPoolTag`) and
/// compares pool tags whatever the kind says, so the wreath pool refuses too --
/// its `IxKOrbit` would otherwise take that predicate's permissive non-plain arm.
let private poolTag (repoPath: string) (varName: string) (isWreath: bool) (ordinal: int) : string =
    let prefix = if isWreath then providerOrbPoolTagPrefix else providerPoolTagPrefix
    taggedIdentity prefix repoPath varName ordinal

/// The mint-table key for a packed variable's pool. `AxisMintTable` is keyed on
/// `(canonical repo path, dim name)`; a pool is not a dim, so it takes a key no
/// dim name can collide with -- dim names are validated Blade identifiers
/// (`isValidIdent`) and this one carries a ':'.
let private poolMintKey (varName: string) : string = $"__pool:{varName}"

/// Why one identity of an axis differs from an older one, for the split
/// history (and, later, for the divergence diagnostic). The two nouns name what
/// the fingerprint was taken OVER -- the dim's coordinate variable for a dense
/// axis, the packed variable itself for a pool -- so the recorded reason reads
/// truthfully for both flavours.
let private splitReason (carrier: string) (content: string)
                        (older: AxisIdentity) (extent: int64) (fp: string option) : string =
    if older.Extent <> extent then $"extent {older.Extent} -> {extent}"
    else
        match older.CoordFP, fp with
        | None, Some _ -> $"a {carrier} appeared"
        | Some _, None -> $"the {carrier} was removed"
        | _ -> $"{content} content differs"

/// WHICH prior identity a new one's `splitReason` is told against.
///
/// Not the newest. The identities of one axis are a SET this compilation has
/// met, in checkout order, not a chain -- so "the previous one" is an accident
/// of which checkouts a program happens to name and in which order. Checkouts
/// A (extent 10, coordinate X), B (extent 8) and C (extent 10, coordinate Y)
/// recorded C's reason against B and printed "extent 8 -> 10", which is true of
/// B and says nothing about why C does not co-iterate with A -- the pairing a
/// user actually hit, and the one whose real story is a coordinate divergence.
///
/// So: prefer an identity of the SAME EXTENT (the newest of them), because that
/// is the pairing whose difference is the interesting one and the one a plain
/// extent sentence cannot explain; otherwise the OLDEST, the axis as this
/// compilation first met it. This text is printed at the user by
/// `trySplitReasonOfTag` at every co-iteration refusal, so it is a claim, not
/// bookkeeping.
let private closestPrior (extent: int64) (identities: AxisIdentity list) : AxisIdentity =
    match identities |> List.filter (fun i -> i.Extent = extent) with
    | sameExtent :: _ -> sameExtent
    | [] -> List.last identities

/// Look one axis up in the mint table and return the index type this
/// checkout's arrays must be built over: the RECORDED identity when the axis
/// is unchanged (§5.2), a fresh one otherwise. Records what it decided either
/// way.
///
/// `fresh` is the record `zarrStoreToModule` just minted for this dim;
/// everything but its Id and Tag is kept, so the shape of a provider axis type
/// is stated in exactly one place (ZarrProvider's `zarrDimToNamedIndexType`).
/// The Id comes from the reserved range (`axisIdBase`) and the Tag from
/// `mkTag`, which is the half that actually refuses.
///
/// `mintKey` is the table key (a dim name for a dense axis, `poolMintKey` for a
/// packed pool) and `mkTag ordinal` the tag builder for that flavour. Both
/// flavours share this body ON PURPOSE: the split history, the reserved-range
/// Id, the refs bookkeeping and the "an axis this compilation already saw
/// UNCHANGED comes back with the identity it had" rule are the same facts about
/// a pool as about a dimension.
let private resolveIdentity (mintKey: string) (mkTag: int -> string)
                            (carrier: string) (content: string)
                            (repoPath: string) (refText: string)
                            (extent: int64) (fp: string option) (fresh: IRIndexType) : IRIndexType =
    let matches (i: AxisIdentity) = i.Extent = extent && i.CoordFP = fp
    let born (reason: string option) (ordinal: int) = {
        Extent = extent
        CoordFP = fp
        IndexType = { fresh with
                        Id = AxisMintTable.freshId ()
                        Tag = Some (mkTag ordinal) }
        Refs = [refText]
        SplitReason = reason
    }
    match AxisMintTable.tryFind repoPath mintKey with
    | Some mint ->
        match mint.Identities |> List.tryFind matches with
        | Some hit ->
            // Unchanged axis: the SAME identity -- same Id, same name -- so the
            // two modules' arrays are over one index space and co-iterate.
            if not (List.contains refText hit.Refs) then
                let updated = { hit with Refs = hit.Refs @ [refText] }
                AxisMintTable.put repoPath mintKey
                    { Identities = mint.Identities |> List.map (fun i -> if matches i then updated else i) }
            hit.IndexType
        | None ->
            let split =
                born (Some (splitReason carrier content
                                        (closestPrior extent mint.Identities) extent fp))
                     (mint.Identities.Length + 1)
            AxisMintTable.put repoPath mintKey { Identities = split :: mint.Identities }
            split.IndexType
    | None ->
        let first = born None 1
        AxisMintTable.put repoPath mintKey { Identities = [first] }
        first.IndexType

let private resolveAxis (repoPath: string) (refText: string) (dimName: string)
                        (extent: int64) (fp: string option) (fresh: IRIndexType) : IRIndexType =
    resolveIdentity dimName (axisTag repoPath dimName) "coordinate variable" "coordinate"
                    repoPath refText extent fp fresh

/// The POOL twin of `resolveAxis` (§5.3's "packed pool axes untagged" residual).
/// Same table, same sharing rule, one flavour down: the identity's content half
/// is the packed variable's OWN fingerprint (`varFingerprint` -- node id,
/// user_data bytes, chunk-ref table), because a pool has no coordinate variable
/// and its cells ARE the variable. So an UNCHANGED packed variable shares one
/// pool identity across two checkouts of a repo exactly as an unchanged dim
/// does, a packed variable a commit rewrote splits, and two repos never meet
/// (the repo path is half the key AND inside the tag's digest).
///
/// `extent` is the pool's BASE extent (`n` in `SymIdx<r, n>`), which is what
/// both record shapes carry -- flat in `IRLit` for a simplex head, one level
/// down inside the `IROrbitClass` marker for a wreath. It is belt-and-braces
/// beside the fingerprint, which already covers the layout: `user_data` is
/// verbatim `zarr.json` and the blade layout block lives in it.
let private resolvePool (repoPath: string) (refText: string) (varName: string)
                        (fp: string) (fresh: IRIndexType) : IRIndexType =
    let extent =
        match orbitBaseExtent fresh with
        | IRLit (IRLitInt v) -> v
        | _ -> -1L
    let isWreath = (fresh.Symmetry = SymWreath)
    // The carrier noun can never fire for a pool (`fp` is always `Some`, since
    // a pool's cells ARE the variable and there is nothing for the fingerprint
    // to be absent for); it is supplied for completeness, not for a live path.
    resolveIdentity (poolMintKey varName) (poolTag repoPath varName isWreath)
                    "packed variable" "packed variable"
                    repoPath refText extent (Some fp) fresh

/// The identities one `(repoPath, dimName)` has carried in this compilation,
/// newest first -- empty when the axis has not been seen. The divergence
/// diagnostic and the axis-sharing tests read the table through here.
let axisIdentities (repoPath: string) (dimName: string) : AxisIdentity list =
    match AxisMintTable.tryFind repoPath dimName with
    | Some mint -> mint.Identities
    | None -> []

/// The same, for a PACKED variable's pool axis -- the key `poolMintKey` builds.
/// One identity means the pool is shared by every checkout that has presented
/// it; two means a commit changed the variable and the two no longer co-iterate.
let poolIdentities (repoPath: string) (varName: string) : AxisIdentity list =
    axisIdentities repoPath (poolMintKey varName)

/// WHY the identity carrying `tag` differs from the one it split from
/// ("coordinate content differs", "extent 5 -> 10", "the coordinate variable
/// was removed"), or `None` for a first identity, an unknown tag, or a tag from
/// some other compilation. Registered into `Types.registerProviderAxisSplitReason`
/// by `ProviderStatics.install`; the refusal sites read it through there.
///
/// `SplitReason` has been recorded at every mint since section 5.3 landed and
/// was printed by nothing, which is why a diverged-checkout refusal could say
/// two axes disagree and never say what about the STORE made them disagree.
let trySplitReasonOfTag (tag: string) : string option =
    AxisMintTable.allIdentities ()
    |> List.tryPick (fun i -> if i.IndexType.Tag = Some tag then i.SplitReason else None)

// ---------------------------------------------------------------------------
// Mapping to Blade IR modules
// ---------------------------------------------------------------------------

/// The repo-handle module: NO dims and NO vars structs. This is a first-class
/// outcome of `registerProviderModule` (its `moduleFields` simply come out
/// []), and it is what makes `ic.load(path)` a handle rather than a store:
/// the binding carries no fields, so no alias verb can mis-fire against it,
/// and `checkout` is the only way to reach data.
let emptyRepoModule (moduleName: string) : IRModule = {
    Name = moduleName
    Types = []
    Functions = []
    Bindings = []
    StaticFunctionUsage = Map.empty
    ProviderReads = Map.empty
    ProviderWrites = Map.empty
    RandomInits = Map.empty
    CompoundInits = Map.empty
    SparseInits = Map.empty
    MutableArrayLets = Set.empty
    DerivedFuncOrigins = Map.empty
}

/// The dims/vars module for a resolved checkout. Node user data is verbatim
/// `zarr.json`, so a checkout presents as a synthetic v3 ZarrStore and the
/// module itself is built by the Zarr provider's own `zarrStoreToModule` --
/// index-type minting, the dims/vars struct shapes, coordinate-array
/// detection and packed-layout typing are inherited, not re-derived.
///
/// Axis identity (§5.3) rides `externalDimMap`: every dimension is resolved
/// against the repo-scoped mint table first, so an axis this compilation has
/// already seen UNCHANGED in another checkout of the same repo comes back with
/// the identity it had there, and a diverged one gets a fresh identity that
/// refuses to unify with it.
///
/// A PACKED variable's pool axis is not a dimension and cannot ride
/// `externalDimMap` (see `poolTag`); it rides the second hook,
/// `zarrStoreToModuleWith`'s `poolAxis`, against the same table under a
/// `poolMintKey`. Both hooks are supplied on PASS 2 only, so each identity is
/// resolved exactly once per call -- pass 1 exists to read back the dim
/// universe and the record shapes, and its throwaway records are never minted
/// into the table.
///
/// Hierarchy (§9): root-level arrays only. Nested arrays are simply not
/// fields (the Zarr provider's one-level rule, where a subgroup directory is
/// never scanned); a root-level array whose name is not a Blade identifier
/// refuses loudly, because it would have to become a struct field.
let checkoutToModule (builder: IRBuilder) (moduleName: string) (ck: CheckoutHandle) : IRModule =
    let key = formatKey { RepoPath = ck.Repo.Root; Ref = Some ck.Ref }
    let refText = $"{kindToken (fst ck.Ref)}:{snd ck.Ref}"
    let arrays =
        ck.Snapshot.Nodes
        |> List.filter (fun n -> n.Kind = NodeArray)
        |> List.choose (fun n ->
            let rest = if n.Path.StartsWith "/" then n.Path.Substring 1 else n.Path
            if rest = "" || rest.Contains "/" then None
            else
                match nodeVarName n.Path with
                | Ok name -> Some (name, n)
                | Error e -> failwith e)
        |> List.sortBy fst
        |> List.map (fun (name, n) ->
            match arrayMetaOfNode name n with
            | Ok meta -> meta
            | Error e -> failwith e)
    let store : ZarrProvider.ZarrStore = { Path = key; Version = 3; Arrays = arrays }
    // Pass 1 builds the module the way a lone checkout always has. Its
    // index-type defs ARE the dimension universe -- names, extents, and the
    // exact record shape a provider axis carries -- which is why this reads
    // them back instead of re-deriving the Zarr builder's `sharedDims` rule
    // (a packed array's pool dimension is deliberately not in it).
    let minted = ZarrProvider.zarrStoreToModule builder moduleName store None
    let mintedDims =
        minted.Types |> List.choose (function IRTDIndexType (n, it) -> Some (n, it) | _ -> None)
    // A store of nothing but packed variables has NO shared dims at all (a pool
    // dim never joins the universe), so the dim list alone cannot decide whether
    // pass 2 is needed -- and that store is exactly the one whose pool axes were
    // the surviving hole. Either kind of identity to resolve earns pass 2.
    let hasPacked = arrays |> List.exists (fun a -> a.Blade.IsSome)
    if List.isEmpty mintedDims && not hasPacked then minted
    else
        let dims =
            mintedDims |> List.map (fun (dimName, fresh) ->
                // NOT a fallback. A dense axis's identity is (extent, coordinate
                // fingerprint), and the fingerprint is `None` for every dim
                // without a coordinate variable -- so a sentinel extent shared by
                // every symbolic axis would make all of them ONE identity, which
                // is a LICENSE to co-iterate arrays that have nothing to do with
                // each other. Unreachable by construction (a provider dim is
                // minted from a store extent, which is a literal), and it says so
                // loudly rather than defaulting into the unsound direction.
                let extent =
                    match fresh.Extent with
                    | IRLit (IRLitInt v) -> v
                    | other ->
                        failwith $"icechunk: internal -- dimension '{dimName}' of '{key}' was minted with a non-literal extent ({other}); a provider axis identity cannot be decided without one"
                let fp = coordFingerprint key ck arrays dimName
                (dimName, resolveAxis ck.Repo.Root refText dimName extent fp fresh))
        // The pool hook. `fresh` is the record ZarrProvider just minted for this
        // variable's pool (simplex or wreath); everything but its Id and Tag
        // survives, so the pool record's SHAPE is still stated in exactly one
        // place. Called once per packed variable, on pass 2 only.
        let poolAxis (varName: string) (fresh: IRIndexType) : IRIndexType =
            resolvePool ck.Repo.Root refText varName (varFingerprint key ck varName) fresh
        // Pass 2 rebuilds over the resolved identities. `externalDimMap` is
        // ALL-OR-NOTHING at the Zarr end: a supplied map REPLACES the dim map
        // entirely and suppresses the index-type defs, so it has to cover every
        // dimension, and the defs are re-attached here in pass 1's order. The
        // module that comes out is shaped exactly as it was before P3 --
        // `registerProviderModule`'s `<binding>.index.<dim>` registration,
        // ProviderStatics' axis-extent read and the IDE hovers all read those
        // defs back.
        let m =
            ZarrProvider.zarrStoreToModuleWith builder moduleName store
                (Some (Map.ofList dims)) (Some poolAxis)
        { m with Types = (dims |> List.map IRTDIndexType) @ m.Types }

/// Provider contract entry point. A BARE path binds the repo handle (an empty
/// module) after header-level validation; a canonical key with a refspec
/// resolves the ref and builds the full dims/vars module.
///
/// THE RESOLUTION FUNNEL. Every way this provider can decline to produce a
/// module leaves through here, and every one of them now leaves as a
/// `Types.ProviderResolutionError` rather than a bare `failwith`: the key does
/// not parse, the repo is missing or is not spec 2 or is Offline, the ref is
/// typo'd or ambiguous or a deleted-tag tombstone, and -- through the
/// `IcechunkDecodeError` arm -- every structural refusal `checkoutToModule`
/// raises under it (virtual chunk refs, nested groups, the verifier's and the
/// offset checks' named rejections).
///
/// The point is the CHECK phase. `TypeCheck.checkDecl` calls this at the
/// binding site and used to swallow anything that was not a dead native
/// library, falling back to opaque types; the refusal then appeared only when
/// lowering re-opened the store under `emit`/`run`, so `blade check` and the
/// editor reported nothing at all for the entire refusal set that is this
/// provider's product claim. A NAMED exception is what lets that arm re-raise
/// this as a spanned BL2008 while leaving the other providers' fallback alone.
/// `.Message` is preserved verbatim, so the lowering-phase text every existing
/// pin matches is unchanged for the paths that still reach lowering.
let loadAsModule (builder: IRBuilder) (moduleName: string) (path: string) : IRModule =
    let refuse (message: string) : 'a = raise (ProviderResolutionError message)
    try
        match parseKey path with
        | Error e -> refuse e
        | Ok key ->
            match key.Ref with
            | None ->
                // Route through `load`, not a bare `validateRepoFile`, so a
                // structurally fine but Offline repo still refuses here (plan
                // §3: the repo file is parsed, and its status gated, AT LOAD).
                match load path with
                | Error e -> refuse e
                | Ok (LoadedRepo _) -> emptyRepoModule moduleName
                | Ok (LoadedCheckout _) ->
                    refuse $"icechunk: internal -- canonical key '{path}' carries no refspec but resolved to a checkout"
            | Some _ ->
                match load path with
                | Ok (LoadedCheckout ck) -> checkoutToModule builder moduleName ck
                | Ok (LoadedRepo _) ->
                    refuse $"icechunk: internal -- canonical key '{path}' carries a refspec but resolved to a bare repo handle"
                | Error e -> refuse $"icechunk checkout '{path}': {e}"
    with
    // The named decode refusals `checkoutToModule` raises are resolution
    // failures too -- the module does not exist because the snapshot says
    // something this reader refuses -- so they join the family rather than
    // escaping as a bare exception the checker cannot classify.
    | IcechunkDecodeError m -> raise (ProviderResolutionError m)

// ---------------------------------------------------------------------------
// Fingerprint / version stamp
// ---------------------------------------------------------------------------

/// Fold-memoization stamp: mtime ticks of `$ROOT/repo`. Exact: the repo file
/// is the ONLY mutable object in a repo, so one O(1) stat replaces the Zarr
/// provider's max-mtime walk over every file. (Polish, not v1: `tag:` and
/// `snapshot:` keys are immutable and could skip invalidation entirely.)
let versionStamp (path: string) : int64 =
    try
        match parseKey path with
        | Ok key -> repoStamp key.RepoPath
        | Error _ -> 0L
    with _ -> 0L

/// The fallback provenance token for a path that names no snapshot: a bare
/// repo handle, a missing repo, an unresolvable ref. sha256 over the canonical
/// key plus the mutable repo file's bytes, so the same key against the same
/// repo state yields the same token. Never throws.
let private placeholderFingerprint (path: string) : string =
    use sha = System.Security.Cryptography.SHA256.Create()
    let keyBytes = Text.Encoding.UTF8.GetBytes(path + "\n")
    sha.TransformBlock(keyBytes, 0, keyBytes.Length, null, 0) |> ignore
    let repoBytes =
        try
            match parseKey path with
            | Ok key ->
                let file = repoFilePath key.RepoPath
                if File.Exists file then File.ReadAllBytes file else [||]
            | Error _ -> [||]
        with _ -> [||]
    if repoBytes.Length > 0 then
        sha.TransformBlock(repoBytes, 0, repoBytes.Length, null, 0) |> ignore
    sha.TransformFinalBlock([||], 0, 0) |> ignore
    sha.Hash |> Array.map (sprintf "%02x") |> String.concat ""

/// Fold-provenance token: the RESOLVED SNAPSHOT ID, prefixed with the refspec
/// that named it -- "branch:main@1CECHNKREP0F1RSTCMT0" (§8). This is the
/// semantically right provenance and it costs one already-memoized resolve,
/// with no sha256 sweep over the store: the snapshot is immutable, so the id
/// IS the content identity of everything the fold could have read. Never
/// throws; a path that names no snapshot falls back to the sha256 placeholder.
let fingerprint (path: string) : string =
    try
        match load path with
        | Ok (LoadedCheckout ck) ->
            // The bare snapshot id: provenance is CONTENT identity, so two
            // refs at the same snapshot share it -- and the refspec already
            // rides in the canonical key printed beside it, so repeating it
            // here doubled the ref in every [provenance] line.
            base32Encode ck.SnapshotId
        | _ -> placeholderFingerprint path
    with _ -> placeholderFingerprint path

// ---------------------------------------------------------------------------
// C++ code generation (pure std C++17 -- no Icechunk logic in the binary)
// ---------------------------------------------------------------------------

module CppIcechunk =

    /// Required includes. The emitted reader opens files by baked path and
    /// reads baked byte ranges -- the same std-only surface the Zarr provider
    /// needs, which is what keeps LinkNeeds = "none".
    let genIncludes () : string list =
        [ "#include <fstream>"
          "#include <filesystem>"
          "#include <cstdint>"
          "#include <string>"
          "#include <limits>" ]

    let private elemCppOf (t: IRType) : string =
        match t with
        | IRTScalar ETFloat32 -> "float"
        | IRTScalar ETFloat64 -> "double"
        | IRTScalar ETInt32 -> "int"
        | IRTScalar ETInt64 -> "long long"
        | _ -> "double"

    let private normPath (p: string) : string = p.Replace('\\', '/')

    /// A C++ string literal for a baked path: separators normalized (the
    /// emitted program is not necessarily built on the compiling host), then
    /// escaped.
    ///
    /// Only the quote is escaped: `normPath` has just replaced every `\` with
    /// `/`, so a backslash-escaping pass over its result can never match.
    let private cppPathLit (p: string) : string =
        "\"" + (normPath p).Replace("\"", "\\\"") + "\""

    /// Emission cap on the TOTAL INLINE PAYLOAD of one variable.
    ///
    /// The chunk-count cap (`maxBakedChunks`) bounds how many table ENTRIES
    /// are emitted and says nothing about how big they are. An inline chunk is
    /// baked as a `static const unsigned char[]` of `0x..` literals -- about
    /// five characters of C++ per byte -- so a single 64 MB inline chunk is one
    /// table entry and ~320 MB of generated source, which is a hung g++ and a
    /// disk full, reported as neither. Icechunk inlines only SMALL chunks by
    /// convention, so 4 MB (about 20 MB of hex) is far above anything a
    /// conventionally written store produces and far below anything that hurts.
    let private maxInlineBytes = 4L * 1024L * 1024L

    /// `static const T name[n] = { ... };`, wrapped so a big table is still a
    /// readable diff. A trailing comma before `}` is well-formed C++.
    let private bakedTable (decl: string) (perLine: int) (items: string list) : string list =
        [ decl + " {" ]
        @ (items |> List.chunkBySize perLine |> List.map (fun g -> "    " + String.concat ", " g + ","))
        @ [ "};" ]

    /// Icechunk's chunk fetch: a compile-time-BAKED table indexed by the
    /// flattened chunk-grid coordinate. The generated program contains no
    /// icechunk logic -- no FlatBuffers, no zstd, no ref resolution -- because
    /// the resolved snapshot is immutable, so its chunk table is a constant.
    ///
    /// Per emitted variable `v`, over a grid of N chunks (tables always
    /// declared with at least one entry, since a zero-length array is
    /// ill-formed C++):
    ///
    ///     static const long long v_icoff[N]            byte offset; -1 marks FILL
    ///     static const char* const v_icpath            the ONE chunk file, when every
    ///                                                  native chunk shares it
    ///     static const char* const v_icpath[K]         the K DISTINCT chunk files, plus
    ///     static const int v_icfile[N]                 a per-chunk index into them
    ///                                                  -- either shape, only if some chunk
    ///                                                  is native
    ///     static const unsigned char v_icb<k>[L]       one array per INLINE chunk, in
    ///                                                  ascending flat-index order
    ///     static const unsigned char* const v_icinl[N] inline pointer or nullptr
    ///                                                  -- emitted only if some chunk is inline
    ///
    /// There is deliberately no per-chunk LENGTH table: every present chunk's
    /// declared length is checked against the padded chunk size at compile time
    /// below, so the length is one baked literal, not N table entries.
    ///
    /// `Present` is `v_icoff[v_cidx] >= 0`, so fill is a table lookup rather
    /// than a failed file open; the shared core still owns the grid loops,
    /// edge intersection, the fill branch body and the flat scatter.
    ///
    /// Every chunk's declared byte length is validated against the padded
    /// chunk size HERE, at compile time -- the runtime check that survives is
    /// for a file that changed under the binary (a GC'd pinned snapshot),
    /// which dies loudly, never as silent zeros.
    let icechunkChunkFetch (ra: ResolvedArray) : ZarrProvider.CppZarr.ChunkFetchEmitter =
        fun v chunkBytes ->
            let meta = ra.Meta
            let varName = meta.Name
            let rank = meta.Shape.Length
            let lens =
                match gridLens $"icechunk codegen: variable '{varName}'" meta.Shape meta.Chunks with
                | Ok l -> l
                | Error e -> failwith e
            let strides = ZarrProvider.rowMajorStrides lens
            let n = ra.Table.Length
            if n > maxBakedChunks then
                failwith $"icechunk codegen: variable '{varName}' has {n} chunks, past the {maxBakedChunks}-entry baked-table cap -- store it with larger chunks"
            // The OTHER dimension of the same budget: entry count above, total
            // baked bytes here. See `maxInlineBytes`.
            let inlineTotal =
                ra.Table |> Array.sumBy (function Inline b -> int64 b.Length | _ -> 0L)
            if inlineTotal > maxInlineBytes then
                failwith $"icechunk codegen: variable '{varName}' of repo '{normPath ra.Root}' bakes {inlineTotal} bytes of INLINE chunk data, past the {maxInlineBytes}-byte inline cap -- inline chunks emit as ~5 characters of C++ hex per byte, so this variable alone would be roughly {inlineTotal * 5L / 1_048_576L} MB of generated source. Re-chunk the array so its chunks are written as native chunk files (icechunk inlines only small chunks), or store the variable natively"
            ra.Table
            |> Array.iteri (fun i loc ->
                match loc with
                | Fill -> ()
                // A manifest's offset is a uint64 on the wire and an int64 in
                // here, so an offset at or past 2^63 arrives NEGATIVE. It would
                // then be baked verbatim into `v_icoff`, where the emitted
                // reader's presence test is `v_icoff[i] >= 0` and -1 is the FILL
                // sentinel: a corrupt offset would read as fill, silently, and
                // the program would print zeros for real data. The fold path
                // already refuses this (`readNativeChunk` checks
                // offset + length against the file); this is the codegen twin.
                | Native nc when nc.Offset < 0L ->
                    failwith $"icechunk codegen: variable '{varName}': corrupt manifest: chunk offset out of range at flat index {i} -- the manifest declares a byte offset at or past 2^63, which is not a position in any file (and would bake as the table's fill sentinel)"
                | Inline bytes when bytes.Length <> chunkBytes ->
                    failwith $"icechunk codegen: variable '{varName}': the inline chunk at flat index {i} holds {bytes.Length} bytes, but a padded chunk of this array is {chunkBytes} -- a compressed or corrupt store?"
                | Native nc when nc.Length <> int64 chunkBytes ->
                    failwith $"icechunk codegen: variable '{varName}': the chunk at flat index {i} declares a {nc.Length}-byte range, but a padded chunk of this array is {chunkBytes} bytes -- a compressed or corrupt store?"
                | _ -> ())

            let tn = max 1 n
            let entry (f: ChunkLoc -> string) (dflt: string) =
                [ for i in 0 .. tn - 1 -> if i < n then f ra.Table.[i] else dflt ]

            let anyNative = ra.Table |> Array.exists (function Native _ -> true | _ -> false)
            let anyInline = ra.Table |> Array.exists (function Inline _ -> true | _ -> false)

            // One byte array per inline chunk, named by its ORDINAL among the
            // inline chunks (ascending flat index), plus the flat -> ordinal map.
            let inlineOrdinal = Collections.Generic.Dictionary<int, int>()
            let inlineBlocks =
                [ for i in 0 .. n - 1 do
                    match ra.Table.[i] with
                    | Inline bytes ->
                        let k = inlineOrdinal.Count
                        inlineOrdinal.[i] <- k
                        yield!
                            bakedTable
                                $"static const unsigned char {v}_icb{k}[{bytes.Length}] ="
                                16
                                (bytes |> Array.map (sprintf "0x%02x") |> Array.toList)
                    | _ -> () ]

            let offTable =
                bakedTable $"static const long long {v}_icoff[{tn}] =" 12
                    (entry (function
                            | Fill -> "-1"
                            | Inline _ -> "0"
                            | Native nc -> string nc.Offset) "-1")
            // DEDUPED chunk-file paths. A repo written with packed native
            // chunks (`PackNativeChunks`) puts EVERY chunk of a variable in one
            // file at different offsets, and the per-chunk path table then
            // baked that one absolute path N times -- N string literals and N
            // pointers for one distinct value. The distinct paths go in their
            // own table and the per-chunk table holds indices; the one-file
            // case drops the per-chunk table entirely and bakes a single
            // pointer that the reader names directly.
            let pathOf (loc: ChunkLoc) =
                match loc with
                | Native nc -> Some (nativeChunkFile ra.Root nc)
                | _ -> None
            let distinctPaths =
                ra.Table |> Array.toList |> List.choose pathOf |> List.distinct
            let pathIndex =
                distinctPaths |> List.mapi (fun k p -> (p, k)) |> Map.ofList
            let fileTable =
                match distinctPaths with
                | [] -> []
                | [ only ] -> [ $"static const char* const {v}_icpath = {cppPathLit only};" ]
                | many ->
                    bakedTable $"static const char* const {v}_icpath[{many.Length}] =" 4
                        (many |> List.map cppPathLit)
                    // Index 0 for a chunk that has no file: it is never
                    // dereferenced (the read is guarded by `Present`, and by
                    // the inline pointer in the mixed case), and an in-range
                    // index cannot become an out-of-bounds read if some future
                    // caller evaluates it anyway.
                    @ bakedTable $"static const int {v}_icfile[{tn}] =" 12
                        (entry (fun loc ->
                            match pathOf loc with
                            | Some p -> string (Map.find p pathIndex)
                            | None -> "0") "0")
            let inlineTable =
                if not anyInline then []
                else
                    bakedTable $"static const unsigned char* const {v}_icinl[{tn}] =" 8
                        ([ for i in 0 .. tn - 1 ->
                            match inlineOrdinal.TryGetValue i with
                            | true, k -> $"{v}_icb{k}"
                            | _ -> "nullptr" ])

            let (refKind, refName) = ra.Ref
            let idx = $"{v}_cidx"
            let fileExpr =
                match distinctPaths with
                | [] -> "\"\""                    // no native chunk: never emitted
                | [ _ ] -> $"{v}_icpath"
                | _ -> $"{v}_icpath[{v}_icfile[{idx}]]"
            let offExpr = $"{v}_icoff[{idx}]"
            let inlExpr = $"{v}_icinl[{idx}]"

            let inlineRead (ind: string) =
                [ ind + $"{{ const unsigned char* {v}_isrc = {inlExpr};"
                  ind + $"  char* {v}_idst = (char*){v}_cbuf;"
                  ind + $"  for (long long {v}_ib = 0; {v}_ib < {chunkBytes}LL; {v}_ib++) {v}_idst[{v}_ib] = (char){v}_isrc[{v}_ib]; }}" ]
            let nativeRead (ind: string) =
                [ ind + $"std::ifstream {v}_cf({fileExpr}, std::ios::binary);"
                  ind + $"if (!{v}_cf) {{ std::cerr << \"Icechunk error: chunk file '\" << {fileExpr} << \"' of '{varName}' cannot be opened -- an expired or garbage-collected snapshot?\" << std::endl; std::exit(1); }}"
                  ind + $"{v}_cf.seekg((std::streamoff){offExpr});"
                  ind + $"{v}_cf.read((char*){v}_cbuf, {chunkBytes});"
                  ind + $"if ({v}_cf.gcount() != (std::streamsize){chunkBytes}) {{ std::cerr << \"Icechunk error: chunk file '\" << {fileExpr} << \"' of '{varName}' is short: expected {chunkBytes} bytes at offset \" << {offExpr} << std::endl; std::exit(1); }}" ]

            let readLines (ind: string) =
                match anyInline, anyNative with
                | true, true ->
                    [ ind + $"if ({inlExpr} != nullptr) {{" ]
                    @ inlineRead (ind + "    ")
                    @ [ ind + "} else {" ]
                    @ nativeRead (ind + "    ")
                    @ [ ind + "}" ]
                | true, false -> inlineRead ind
                | false, true -> nativeRead ind
                | false, false ->
                    [ ind + $"// every chunk of '{varName}' is fill in this snapshot: nothing to read" ]

            let flatExpr =
                if rank = 0 then "0"
                else [ for d in 0 .. rank - 1 -> $"{v}_c{d} * {strides.[d]}" ] |> String.concat " + "

            let identExpr =
                let parts = [ for d in 0 .. rank - 1 -> $"std::to_string({v}_c{d})" ]
                if List.isEmpty parts then "std::string(\"[]\")"
                else "std::string(\"[\") + " + String.concat " + std::string(\", \") + " parts + " + std::string(\"]\")"

            { Prologue =
                [ $"// Read {varName} from icechunk repo {normPath ra.Root} ({kindToken refKind}:{refName}, snapshot {base32Encode ra.SnapshotId})"
                  $"// {n} chunk(s), baked at compile time: the snapshot is immutable, so this table is a constant." ]
                @ offTable @ fileTable @ inlineBlocks @ inlineTable
              Locate = fun ind -> [ ind + $"size_t {idx} = {flatExpr};" ]
              Present = $"{offExpr} >= 0"
              Read = readLines
              Ident = identExpr }

    /// Resolve a variable for emission, or die with the reason at the read site.
    let private resolveOrFail (what: string) (path: string) (varName: string) : ResolvedArray =
        match resolveArray path varName with
        | Ok ra -> ra
        | Error e -> failwith $"icechunk {what} of variable '{varName}' from '{path}': {e}"

    /// Dense reader: the shared assembly core over the baked chunk table into
    /// `<v>_flat`, then the same materialization CppNetcdf/CppZarr do (nested
    /// Array via allocate<>, flat->nested copy, buffers released).
    let genReadVar (path: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let ra = resolveOrFail "read" path varName
        if ra.Meta.Blade.IsSome then
            failwith $"icechunk codegen: variable '{varName}' is blade-packed; the dense reader cannot materialize it (this indicates a typing inconsistency)"
        // Rank 0 is refused at the metadata gate (`arrayMetaOfNode`), so this
        // is a restatement, not a live path -- but the loop nest below opens
        // with `idxVars.[0]`, which on an empty list is an IndexOutOfRange
        // escaping codegen rather than anything a user can read.
        if ra.Meta.Shape.IsEmpty then
            failwith $"icechunk codegen: variable '{varName}' is rank 0 -- rank-0 arrays are not supported by the icechunk provider (this indicates a typing inconsistency)"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let assemble = ZarrProvider.CppZarr.genAssembleFlatVia (icechunkChunkFetch ra) ra.Meta v elemCpp
        let shape = ra.Meta.Shape |> List.map int
        let rank = shape.Length
        let extentDecls = shape |> List.mapi (fun i n -> $"size_t {v}_extent_{i} = {n};")
        let extentNames = shape |> List.mapi (fun i _ -> $"{v}_extent_{i}")
        let idxVars = [ for i in 0 .. rank - 1 -> $"{v}_i{i}" ]
        let openLoops =
            idxVars |> List.mapi (fun d iv ->
                let ind = String.replicate d "    "
                $"{ind}for (size_t {iv} = 0; {iv} < {extentNames.[d]}; {iv}++) {{")
        let nestedSub = idxVars |> List.map (sprintf "[%s]") |> String.concat ""
        let flatIdx =
            let mutable acc = idxVars.[0]
            for i in 1 .. rank - 1 do
                acc <- $"({acc}) * {extentNames.[i]} + {idxVars.[i]}"
            acc
        let bodyInd = String.replicate rank "    "
        let materialize =
            extentDecls
            @ [ $"""size_t {v}_extents[] = {{ {(String.concat ", " extentNames)} }};"""
                $"Array<{elemCpp}, {rank}> {v} = {{ allocate<typename promote<{elemCpp}, {rank}>::type, nullptr>({v}_extents), {v}_extents }};" ]
            @ openLoops
            @ [ $"{bodyInd}{v}{nestedSub} = {v}_flat[{flatIdx}];" ]
            @ [ for d in rank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
            @ [ $"delete[] {v}_flat;" ]
        assemble @ materialize

    /// Packed (SymIdx/AntisymIdx) and orbit (OrbIdx) reader. The store's pool
    /// IS the in-memory representation, so assembly is the ordinary flat chunk
    /// walk through the shared core; the codegen intercept owns allocation and
    /// copy, so this emits `<v>_flat` and nothing else.
    ///
    /// Two shapes refuse loudly rather than half-work: the 'packed-blocks'
    /// layout, whose per-block assembler is NOT routed through the shared core
    /// (plan §7, P0 outcome (a)), and `read_window`, whose sub-simplex
    /// extraction sits above the core. Both are phase P4.
    let genReadPacked (path: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (opts: Blade.ProviderRegistry.PackedReadOpts) : string list =
        let ra = resolveOrFail "packed read" path varName
        let meta = ra.Meta
        match meta.Blade with
        | None ->
            failwith $"icechunk codegen: variable '{varName}' has no blade packed layout but was typed packed (this indicates a typing inconsistency)"
        | Some layout when layout.Blocks.IsSome ->
            failwith $"icechunk codegen: variable '{varName}' is stored with the 'packed-blocks' layout, whose per-block chunk I/O does NOT route through the shared chunk-source core (docs/plans/plan-icechunk-provider.md §7, P0 outcome (a)) -- merging the two assemblers is phase P4; store the variable with layout 'packed' to read it from an icechunk repo today"
        | Some _ when opts.Window.IsSome ->
            failwith $"icechunk codegen: variable '{varName}': read_window needs the window-extraction emitter that sits ABOVE the shared chunk-source core, which the icechunk reader does not reach yet (docs/plans/plan-icechunk-provider.md §12, phase P4) -- read the whole pool and take the sub-simplex in Blade"
        | Some layout when layout.Group.Sym = SymWreath ->
            // Everything that could go wrong is a MISMATCH between the declared
            // class and the stored one, checked here rather than trusted.
            let g = layout.Group
            (match arrType.IndexTypes with
             | [ lead ] when lead.Symmetry = SymWreath ->
                 let declLevels = Blade.IR.orbitLevelsOf lead
                 let declExtent =
                     match Blade.IR.orbitBaseExtent lead with
                     | IRLit (IRLitInt n) -> n
                     | _ -> -1L
                 if declLevels <> g.Levels || declExtent <> g.Extent then
                     failwithf "icechunk codegen: variable '%s': declared OrbIdx<%s, %d> does not match the store's orbit head OrbIdx<%s, %d>"
                               varName (Blade.IR.ppOrbitLevels declLevels) declExtent
                               (Blade.IR.ppOrbitLevels g.Levels) g.Extent
             | _ ->
                 failwith $"icechunk codegen: variable '{varName}': the store declares an orbit (iterated-wreath) head, so the variable must type as a SOLE OrbIdx group")
            if opts.Distribute then
                failwith $"icechunk codegen: variable '{varName}': the MPI-distributed read is not defined for an OrbIdx (iterated-wreath) pool (spec_version 2 is the flat single-pool layout only)"
            ZarrProvider.CppZarr.genAssembleFlatVia (icechunkChunkFetch ra) meta cppVarName (elemCppOf arrType.ElemType)
        | Some layout ->
            let g = layout.Group
            (match arrType.IndexTypes with
             | lead :: rest ->
                 let leadOk =
                     lead.Symmetry = g.Sym && lead.Rank = g.Rank
                     && (match lead.Extent with IRLit (IRLitInt n) -> n = g.Extent | _ -> false)
                 let restExtents =
                     rest |> List.map (fun ix ->
                         match ix.Extent with IRLit (IRLitInt n) -> n | _ -> -1L)
                 let restOk =
                     (rest |> List.forall (fun ix -> ix.Symmetry = SymNone && ix.Rank = 1))
                     && restExtents = layout.DenseDims
                 if not (leadOk && restOk) then
                     failwithf "icechunk codegen: variable '%s': declared packed type does not match the store's blade layout (group %A rank %d expected lead extent %d, dense %A)"
                         varName g.Sym g.Rank g.Extent layout.DenseDims
             | [] -> failwith $"icechunk codegen: variable '{varName}': packed read with no index types")
            // opts.Distribute is ignored for the flat pool layout, exactly as
            // the Zarr provider does: only the blocks assembler is rank-scoped.
            ZarrProvider.CppZarr.genAssembleFlatVia (icechunkChunkFetch ra) meta cppVarName (elemCppOf arrType.ElemType)

    /// Writes are refused BY NAME: an Icechunk write is a COMMIT, not an
    /// in-place store write (§8, §11).
    let genWriteVar (path: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (dimNames: string list) : string list =
        ignore cppVarName
        ignore arrType
        ignore dimNames
        failwith $"icechunk write of '{varName}' to '{path}' is refused: writing to an Icechunk repo is a COMMIT -- new chunk files, a new manifest, a new snapshot, and an optimistic-concurrency conditional swap of the mutable 'repo' file -- not an in-place store write. Writes-as-commits are their own arc (docs/plans/plan-icechunk-provider.md §11); write a plain Zarr store instead and ingest it with icechunk-python"

// ---------------------------------------------------------------------------
// Provider registration record (plan §8)
// ---------------------------------------------------------------------------

/// The icechunk ProviderSpec (surface module name "icechunk").
let spec : Blade.ProviderRegistry.ProviderSpec = {
    Name = "icechunk"
    LoadAsModule = loadAsModule
    ReadVarData = readVarData
    GenReadVar = CppIcechunk.genReadVar
    // Presence of these two is the provider's packed/wreath CAPABILITY
    // declaration at every codegen and interpreter seam (§8 lists both as v1
    // via the shared chunk-source core).
    GenReadPacked = Some CppIcechunk.genReadPacked
    ReadWreathPool = Some readWreathPool
    GenReadCompoundVar = None  // load_compound: refused loudly, as Zarr
    GenWriteVar = CppIcechunk.genWriteVar
    GenStreamOpen = None       // `.stream`: deferred (the baked table makes fiber reads easy later)
    GenStreamFiber = None
    GenStreamRowsOpen = None
    GenStreamRows = None
    StreamRowsBlock = None
    GenStreamWindow = None
    Includes = CppIcechunk.genIncludes
    VarDimNames = fun path varName ->
        // Must not throw on an unreadable store: writers fall back to
        // synthesized dim<i> names when this yields None. The zarr.json is
        // authoritative (it is what builds the module); the snapshot's
        // structural names stand in when the JSON carries none.
        try
            match load path with
            | Ok (LoadedCheckout ck) ->
                match findArray ck varName with
                | Ok (node, meta) ->
                    match meta.DimNames with
                    | Some ns -> Some ns
                    | None -> node.DimensionNames
                | Error _ -> None
            | _ -> None
        with _ -> None
    Fingerprint = fingerprint
    VersionStamp = versionStamp
    LinkNeeds = "none (pure std C++17)"
}
