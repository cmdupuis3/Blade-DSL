// Blade Icechunk fixture WRITER: programmatic, deterministic, hermetic
// spec-2 repos on local disk, for `blade test icechunk`.
//
// This is the ZarrWrite discipline (ZarrProvider.fs, module ZarrWrite) applied
// to a versioned store: pure .NET file writes, no external tool, no committed
// binary fixture, and every byte reproducible from a seed. What ZarrWrite is
// to `tests/fixtures/zarr_stores/`, this module is to
// `tests/fixtures/icechunk_repos/` -- see that directory's README.
//
// WHAT IT WRITES (plan section 2, docs/plans/plan-icechunk-provider.md):
//
//     $ROOT/repo               the RepoInfo file (refs -> snapshots), the ONLY
//                              mutable object in a repo
//     $ROOT/snapshots/<id>     one Snapshot per commit
//     $ROOT/manifests/<id>     chunk tables
//     $ROOT/chunks/<id>        RAW, headerless chunk payloads
//     $ROOT/transactions/      created empty (tx logs are PRUNABLE; no read
//     $ROOT/overwritten/       path may depend on either directory existing)
//
// Every metadata file carries the 39-byte header -- 12 magic bytes, a 24-byte
// space-padded implementation name, then the spec / file-type / compression
// bytes -- followed by a FlatBuffer built through the flatc-generated OBJECT
// API (`generated.RepoT`, `generated.SnapshotT`, `generated.ManifestT`; see
// src/providers/icechunk-format/). Chunk files carry NO header: their bytes
// are exactly what the Zarr codec pipeline produced, which for the single
// little-endian `bytes` codec this provider accepts means raw cells.
//
// THREE INVARIANTS THE SPEC REQUIRES AND THIS WRITER HONORS (a reader is
// allowed to binary-search these vectors, so an unsorted fixture is not a
// "slightly wrong" fixture, it is an undefined one):
//
//   * `Repo.tags`, `Repo.branches` and `Repo.deleted_tags` sorted by name in
//     ascending UTF-8 BYTE order; `Repo.snapshots` sorted by id bytes (and
//     `Ref.snapshot_index` indexes into that SORTED list).
//   * `Snapshot.nodes` sorted by path bytes; `Snapshot.manifest_files_v2`
//     sorted by id bytes.
//   * `Manifest.arrays` sorted by node-id bytes; within one `ArrayManifest`,
//     `refs` sorted in ascending lexicographic order of `ChunkRef.index`.
//
// DETERMINISM AND ID STABILITY. No `System.Random` anywhere: every id is a
// truncated SHA-256 over (seed, role, payload), which buys the property the
// axis-identity work of plan section 5 needs --
//
//   * a NODE id is derived from the array's NAME alone, so an array keeps its
//     id across every snapshot of a repo (the spec's own "stable across
//     snapshots" rule). This one is deliberately NOT content-derived: a node
//     id is the thing that says "same array, later commit", so it has to
//     survive a rewrite of the data it names.
//   * a MANIFEST id is derived from the array name plus the canonical text of
//     its chunk-ref table, and a CHUNK id from the array name plus the chunk's
//     own bytes -- so a snapshot that leaves an array untouched REUSES that
//     array's manifest and chunk files byte-for-byte, while an array whose
//     data changed mints fresh ones. "Coordinate arrays unchanged, data array
//     rewritten" therefore falls out of the spec rather than being staged by
//     hand.
//   * a SNAPSHOT id is derived from the snapshot's own SERIALIZED CONTENT --
//     built once with a zeroed id, hashed, then rebuilt with the resulting id
//     stamped in (see `snapshotIdBytes`) -- rather than from the spec's
//     snapshot NAME, so two commits are never mistaken for one just because a
//     fixture reused a name like "s1".
//
// This module deliberately does NOT reference `Blade.IcechunkProvider`: the
// base32 encoder, the magic bytes and the header layout are written out again
// here, so the reader and the writer are independent statements of the format
// that the tests can play off against each other. A byte-level twin is the
// point, not duplication to be factored away.
module Blade.IcechunkWrite

open System
open System.IO
open System.Text

// ---------------------------------------------------------------------------
// The metadata file header (39 bytes)
// ---------------------------------------------------------------------------

/// The 12-byte magic, spelled as BYTES so no source-encoding accident can
/// change it: UTF-8 for "ICE" + U+1F9CA (ice cube) + "CHUNK".
let magicBytes : byte[] =
    [| 0x49uy; 0x43uy; 0x45uy                      // I C E
       0xF0uy; 0x9Fuy; 0xA7uy; 0x8Auy              // U+1F9CA, 4-byte UTF-8
       0x43uy; 0x48uy; 0x55uy; 0x4Euy; 0x4Buy |]   // C H U N K

/// Bytes of the space-padded implementation-name field.
let implNameSize = 24

/// Total header bytes: 12 magic + 24 impl name + spec + file type + compression.
let headerSize = 39

/// File-type bytes. Attributes (3) and Chunk (5) exist in the enum but no
/// writer stamps them, so they are not offered here.
let ftSnapshot = 1uy
let ftManifest = 2uy
let ftTransactionLog = 4uy
let ftRepoInfo = 6uy

/// Compression bytes.
let compNone = 0uy
let compZstd = 1uy

/// Build a 39-byte metadata header. The implementation name is padded with
/// ASCII spaces (0x20) to exactly 24 bytes; a longer name is a hard error
/// rather than a silent truncation, because truncating it would produce a
/// header that parses and lies.
let makeHeader (implementation: string) (specByte: byte) (fileType: byte) (compression: byte) : byte[] =
    let implBytes = Encoding.UTF8.GetBytes implementation
    if implBytes.Length > implNameSize then
        failwithf "IcechunkWrite: implementation name '%s' is %d UTF-8 bytes, but the header field is %d"
                  implementation implBytes.Length implNameSize
    let implField = Array.create implNameSize 0x20uy
    Array.blit implBytes 0 implField 0 implBytes.Length
    Array.concat [ magicBytes; implField; [| specByte; fileType; compression |] ]

// ---------------------------------------------------------------------------
// Crockford base32 (object ids -> file names)
// ---------------------------------------------------------------------------
//
// A byte-level TWIN of the provider's encoder, on purpose (see the module
// header): uppercase, no padding, five bits per character MSB-first, the final
// partial group right-padded with zero bits. 12-byte object ids -> 20
// characters, 8-byte node ids -> 13.

let base32Alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

let base32Encode (bytes: byte[]) : string =
    let sb = StringBuilder()
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

// ---------------------------------------------------------------------------
// Ordering helpers (the spec's sort rules are byte-order rules)
// ---------------------------------------------------------------------------

/// Ascending UTF-8 BYTE order. Not `String.CompareOrdinal`: that compares
/// UTF-16 code units, which disagrees with UTF-8 byte order above the BMP.
/// Test fixtures are ASCII, so this never bites -- it is here so the fixture
/// writer states the spec's rule rather than an approximation of it.
let utf8Compare (a: string) (b: string) : int =
    let xa = Encoding.UTF8.GetBytes a
    let xb = Encoding.UTF8.GetBytes b
    let n = min xa.Length xb.Length
    let mutable i = 0
    let mutable r = 0
    while r = 0 && i < n do
        r <- compare xa.[i] xb.[i]
        i <- i + 1
    if r <> 0 then r else compare xa.Length xb.Length

/// Ascending byte order over two ids of equal length.
let byteCompare (a: byte[]) (b: byte[]) : int =
    let n = min a.Length b.Length
    let mutable i = 0
    let mutable r = 0
    while r = 0 && i < n do
        r <- compare a.[i] b.[i]
        i <- i + 1
    if r <> 0 then r else compare a.Length b.Length

// ---------------------------------------------------------------------------
// Deterministic ids
// ---------------------------------------------------------------------------

/// Truncated SHA-256 over (seed, role, payload). Deterministic by
/// construction: the same spec written twice -- in two directories, in two
/// processes, on two machines -- yields the same ids, and two different seeds
/// share none.
let private digest (seed: int) (role: string) (payload: byte[]) (n: int) : byte[] =
    use sha = System.Security.Cryptography.SHA256.Create()
    let prefix = Encoding.UTF8.GetBytes(sprintf "blade-icechunk-fixture/%d/%s/" seed role)
    Array.sub (sha.ComputeHash(Array.append prefix payload)) 0 n

let private idFromText (seed: int) (role: string) (text: string) (n: int) : byte[] =
    digest seed role (Encoding.UTF8.GetBytes text) n

// ---------------------------------------------------------------------------
// The fixture spec
// ---------------------------------------------------------------------------

/// A fixture array's cells, row-major. The element type decides the zarr
/// `data_type` and the raw little-endian cell width.
type IceData =
    | IceF32 of float32[]
    | IceF64 of float[]
    | IceI32 of int32[]
    | IceI64 of int64[]

/// A fixture array's `fill_value`. Zarr v3 requires one, so there is no
/// "absent" case: a chunk coordinate with no ChunkRef reads as THIS value.
type IceFill =
    | IceFillFloat of float
    | IceFillInt of int64

/// One root-level array in one snapshot.
///
/// `Shape`/`Chunks`/`Data`/`Fill`/`DimNames` describe an ordinary Zarr v3
/// array; the remaining fields are the per-chunk PLACEMENT POLICY, which is
/// the part of the format that is Icechunk's rather than Zarr's.
type ArraySpec = {
    /// Becomes node path "/{Name}" and the Blade variable name.
    Name: string
    Shape: int64 list
    /// Regular chunk grid. Edge chunks are written FULL SIZE, padded with the
    /// fill value -- the same contract the Zarr provider's raw `bytes` codec
    /// path assumes.
    Chunks: int64 list
    Fill: IceFill
    /// `dimension_names`, written BOTH in the zarr.json user data and
    /// structurally in `ArrayNodeData.dimension_names` (the reader
    /// cross-checks the two, so they are always written in agreement).
    DimNames: string list option
    /// Row-major cells; length must equal the product of `Shape`.
    Data: IceData
    /// Chunk-grid coordinates to leave WITHOUT a ChunkRef. A coordinate no
    /// manifest covers reads as `Fill` -- the absence IS the fill encoding,
    /// so this is how a fixture exercises the fill path.
    OmitChunks: int64 list list
    /// Chunks whose payload is at most this many bytes ride INLINE in the
    /// manifest instead of getting their own file. Icechunk's own default is
    /// 512; `0` forces every present chunk native.
    InlineThreshold: int
    /// Pack every native chunk of this array into ONE `chunks/` file at
    /// increasing offsets. The reference writer emits one chunk per file
    /// (offset 0), but the schema permits packing and readers MUST honor
    /// offset/length -- this is the fixture that proves they do.
    PackNativeChunks: bool
    /// Split this array's chunk table across TWO manifests, cut along the
    /// leading chunk-grid axis, so the reader has to union non-overlapping
    /// `ChunkIndexRange`s. Ignored when the leading axis has one chunk.
    SplitManifests: bool
    /// Emit exactly one VIRTUAL ChunkRef (a `location` pointing outside the
    /// repo) at this chunk coordinate. No chunk file is written for it: the
    /// provider must refuse the ref BY NAME long before anything opens it.
    VirtualChunk: (int64 list * string) option
    /// Verbatim body of the zarr.json `attributes` object, e.g.
    /// `"\"blade\": {...}"` for a packed layout. `None` writes no attributes.
    AttributesJson: string option
}

/// One commit. `Arrays` is the WHOLE hierarchy of that snapshot (plus the
/// implicit root group), not a delta: two snapshots that list the same array
/// with the same data automatically share its manifest and chunk files,
/// because both are content-derived.
type SnapshotSpec = {
    /// Names this snapshot inside the spec (branches and tags point at it by
    /// this name) and seeds its id. Never written into the repo.
    Name: string
    Message: string
    /// Non-leap microseconds since the Unix epoch.
    FlushedAtMicros: uint64
    Arrays: ArraySpec list
}

/// A whole repo. `writeRepo` turns one of these into a directory.
type RepoSpec = {
    /// The 24-byte space-padded implementation-name header field.
    Implementation: string
    /// The spec-version header byte. 2 is the only version the reader
    /// accepts; writing 1 produces a real spec-1-headed repo whose refusal
    /// fires at the header, before any payload is touched.
    SpecByte: byte
    /// zstd-compress every metadata payload (compression byte 1) or write it
    /// raw (byte 0). Real writers compress, so `true` is the default -- but
    /// both reader paths are live, so fixtures come in pairs.
    Compress: bool
    /// Repo availability. Offline refuses every ref (plan section 9);
    /// Online and ReadOnly both read.
    Availability: generated.RepoAvailability
    /// Reason text stamped beside a non-Online availability.
    AvailabilityReason: string option
    /// The snapshots, in COMMIT order. That order only decides each
    /// snapshot's `parent_offset` (each one's parent is its predecessor here,
    /// the first has none); the repo file's own `snapshots` vector is sorted
    /// by id, as the spec requires.
    Snapshots: SnapshotSpec list
    /// branch name -> `SnapshotSpec.Name`.
    Branches: (string * string) list
    /// tag name -> `SnapshotSpec.Name`. Tags and branches are SEPARATE
    /// namespaces, so the same name may appear in both -- which is exactly
    /// the fixture the bare-ref ambiguity refusal needs.
    Tags: (string * string) list
    /// Tombstones. A deleted tag name must never resolve, and must never be
    /// recreated as a tag.
    DeletedTags: string list
    /// Seeds every derived id.
    Seed: int
}

/// Sensible spec-2 defaults: a compressed, Online, empty repo. Fixtures are
/// built by record-copying this.
let emptyRepo : RepoSpec = {
    Implementation = "blade-fixtures"
    SpecByte = 2uy
    Compress = true
    Availability = generated.RepoAvailability.Online
    AvailabilityReason = None
    Snapshots = []
    Branches = []
    Tags = []
    DeletedTags = []
    Seed = 1
}

/// An ordinary dense fixture array: every chunk present, inline under
/// Icechunk's own 512-byte default threshold, one manifest, nothing virtual.
let mkArray (name: string) (dimNames: string list) (shape: int64 list)
            (chunks: int64 list) (data: IceData) : ArraySpec = {
    Name = name
    Shape = shape
    Chunks = chunks
    Fill = IceFillFloat 0.0
    DimNames = (if List.isEmpty dimNames then None else Some dimNames)
    Data = data
    OmitChunks = []
    InlineThreshold = 512
    PackNativeChunks = false
    SplitManifests = false
    VirtualChunk = None
    AttributesJson = None
}

/// A commit of `arrays` with a generated message and a fixed timestamp (a
/// wall-clock time would break byte-reproducibility for no benefit).
let mkSnapshot (name: string) (arrays: ArraySpec list) : SnapshotSpec = {
    Name = name
    Message = $"fixture commit {name}"
    FlushedAtMicros = 1700000000000000UL
    Arrays = arrays
}

// ---------------------------------------------------------------------------
// Layout, grids and chunk bytes
// ---------------------------------------------------------------------------

let repoFilePath (root: string) = Path.Combine(root, "repo")
let snapshotsDir (root: string) = Path.Combine(root, "snapshots")
let manifestsDir (root: string) = Path.Combine(root, "manifests")
let chunksDir (root: string) = Path.Combine(root, "chunks")

let private ceilDiv (a: int64) (b: int64) = (a + b - 1L) / b

/// Chunks along each dimension.
let gridDims (shape: int64 list) (chunks: int64 list) : int64 list =
    List.map2 ceilDiv shape chunks

/// Every chunk-grid coordinate, row-major (last axis fastest).
let gridCoords (dims: int64 list) : int64 list list =
    let rec go (ds: int64 list) : int64 list list =
        match ds with
        | [] -> [ [] ]
        | d :: rest ->
            let tails = go rest
            [ for i in 0L .. d - 1L do
                for t in tails do
                    yield i :: t ]
    go dims

let private rowMajorStrides (dims: int list) : int[] =
    let arr = List.toArray dims
    let out = Array.zeroCreate arr.Length
    let mutable acc = 1
    for i in arr.Length - 1 .. -1 .. 0 do
        out.[i] <- acc
        acc <- acc * arr.[i]
    out

let private dataTypeName (d: IceData) : string =
    match d with
    | IceF32 _ -> "float32"
    | IceF64 _ -> "float64"
    | IceI32 _ -> "int32"
    | IceI64 _ -> "int64"

let private elemSize (d: IceData) : int =
    match d with
    | IceF32 _ -> 4
    | IceF64 _ -> 8
    | IceI32 _ -> 4
    | IceI64 _ -> 8

let private dataLength (d: IceData) : int =
    match d with
    | IceF32 a -> a.Length
    | IceF64 a -> a.Length
    | IceI32 a -> a.Length
    | IceI64 a -> a.Length

// Little-endian by construction: BitConverter is little-endian on every
// platform Blade builds for, and the Zarr codec gate this fixture feeds
// accepts little-endian only.
let private cellBytes (d: IceData) (i: int) : byte[] =
    match d with
    | IceF32 a -> BitConverter.GetBytes a.[i]
    | IceF64 a -> BitConverter.GetBytes a.[i]
    | IceI32 a -> BitConverter.GetBytes a.[i]
    | IceI64 a -> BitConverter.GetBytes a.[i]

let private fillBytes (d: IceData) (f: IceFill) : byte[] =
    match d, f with
    | IceF32 _, IceFillFloat v -> BitConverter.GetBytes (float32 v)
    | IceF32 _, IceFillInt n -> BitConverter.GetBytes (float32 n)
    | IceF64 _, IceFillFloat v -> BitConverter.GetBytes v
    | IceF64 _, IceFillInt n -> BitConverter.GetBytes (float n)
    | IceI32 _, IceFillInt n -> BitConverter.GetBytes (int32 n)
    | IceI32 _, IceFillFloat v -> BitConverter.GetBytes (int32 v)
    | IceI64 _, IceFillInt n -> BitConverter.GetBytes n
    | IceI64 _, IceFillFloat v -> BitConverter.GetBytes (int64 v)

let private fillJson (f: IceFill) : string =
    match f with
    | IceFillFloat v when Double.IsNaN v -> "\"NaN\""
    | IceFillFloat v when Double.IsPositiveInfinity v -> "\"Infinity\""
    | IceFillFloat v when Double.IsNegativeInfinity v -> "\"-Infinity\""
    | IceFillFloat v -> v.ToString("R", Globalization.CultureInfo.InvariantCulture)
    | IceFillInt n -> string n

/// One chunk's post-codec bytes: the in-bounds region copied out of `Data`,
/// any edge overhang padded with the fill value. Chunks are always FULL SIZE
/// (product of `Chunks` cells), which is the contract the reader's
/// length validation checks against.
let chunkBytes (a: ArraySpec) (coords: int64 list) : byte[] =
    let shape = a.Shape |> List.map int
    let chunks = a.Chunks |> List.map int
    let rank = shape.Length
    let bs = elemSize a.Data
    let cells = chunks |> List.fold (*) 1
    let gStr = rowMajorStrides shape
    let cStr = rowMajorStrides chunks
    let coordsArr = coords |> List.map int |> List.toArray
    let shapeArr = List.toArray shape
    let chunksArr = List.toArray chunks
    let out = Array.zeroCreate<byte> (cells * bs)
    let pad = fillBytes a.Data a.Fill
    for c in 0 .. cells - 1 do
        Array.blit pad 0 out (c * bs) bs
    let rec go (d: int) (gBase: int) (cBase: int) =
        if d = rank then
            Array.blit (cellBytes a.Data gBase) 0 out (cBase * bs) bs
        else
            let basePos = coordsArr.[d] * chunksArr.[d]
            let lim = min chunksArr.[d] (shapeArr.[d] - basePos)
            for l in 0 .. lim - 1 do
                go (d + 1) (gBase + (basePos + l) * gStr.[d]) (cBase + l * cStr.[d])
    if cells > 0 && (shape |> List.fold (*) 1) > 0 then go 0 0 0
    out

/// The array's `zarr.json`, VERBATIM as it lands in `NodeSnapshot.user_data`.
/// Shaped exactly like `ZarrWrite.writeStoreV3`'s node metadata, so the
/// provider's inherited `parseArrayMetaV3` gates apply unchanged: single
/// little-endian `bytes` codec, regular chunk grid, numeric dtype.
///
/// `chunk_key_encoding` is inert for Icechunk (chunks are addressed by object
/// id, never by key) but is written anyway -- the JSON is a real zarr.json,
/// not an Icechunk-flavored subset of one.
let arrayJson (a: ArraySpec) : string =
    let shapeJson = a.Shape |> List.map string |> String.concat ", "
    let chunksJson = a.Chunks |> List.map string |> String.concat ", "
    let dimsJson =
        match a.DimNames with
        | Some ds -> sprintf ", \"dimension_names\": [%s]" (ds |> List.map (sprintf "\"%s\"") |> String.concat ", ")
        | None -> ""
    let attrsJson =
        match a.AttributesJson with
        | Some body -> sprintf ", \"attributes\": {%s}" body
        | None -> ""
    sprintf "{\"zarr_format\": 3, \"node_type\": \"array\", \"shape\": [%s], \"data_type\": \"%s\", \"chunk_grid\": {\"name\": \"regular\", \"configuration\": {\"chunk_shape\": [%s]}}, \"chunk_key_encoding\": {\"name\": \"default\", \"configuration\": {\"separator\": \"/\"}}, \"fill_value\": %s, \"codecs\": [{\"name\": \"bytes\", \"configuration\": {\"endian\": \"little\"}}]%s%s}"
        shapeJson (dataTypeName a.Data) chunksJson (fillJson a.Fill) dimsJson attrsJson

/// The root group's `zarr.json`.
let rootGroupJson = "{\"zarr_format\": 3, \"node_type\": \"group\"}"

// ---------------------------------------------------------------------------
// Derived ids (public: tests build `@snapshot:<id>` keys from them)
// ---------------------------------------------------------------------------

/// An array's 8-byte node id. Derived from the NAME alone, so the node keeps
/// its id across every snapshot of the repo -- the spec's own stability rule,
/// and the anchor of plan section 5.2's axis-identity test.
let nodeIdBytes (spec: RepoSpec) (arrayName: string) : byte[] =
    idFromText spec.Seed "node" arrayName 8

/// The base32 spelling of `nodeIdBytes` (13 characters).
let nodeId (spec: RepoSpec) (arrayName: string) : string =
    base32Encode (nodeIdBytes spec arrayName)

// SNAPSHOT ids are further down, at `snapshotIdBytes`: they hash the
// snapshot's serialized bytes, so they cannot be defined until the machinery
// that produces those bytes has been.

// ---------------------------------------------------------------------------
// zstd
// ---------------------------------------------------------------------------

/// Compression level for fixture payloads. Level is a size/speed knob only --
/// any level round-trips -- so a low one keeps fixture writing cheap.
let zstdLevel = 3

/// zstd frame around a metadata payload (ZstdSharp.Port, managed-only: no
/// native library joins the build).
let compressZstd (payload: byte[]) : byte[] =
    use compressor = new ZstdSharp.Compressor(zstdLevel)
    compressor.Wrap(ReadOnlySpan<byte>(payload)).ToArray()

/// Header + (optionally compressed) payload: a complete metadata file.
let frameFile (spec: RepoSpec) (fileType: byte) (payload: byte[]) : byte[] =
    let body = if spec.Compress then compressZstd payload else payload
    let comp = if spec.Compress then compZstd else compNone
    Array.append (makeHeader spec.Implementation spec.SpecByte fileType comp) body

// ---------------------------------------------------------------------------
// Building one array's chunk refs
// ---------------------------------------------------------------------------

/// Where one chunk ended up. Mirrors the three ChunkRef shapes; `RefAbsent`
/// is the fourth outcome the schema encodes by saying nothing at all.
type private Placement =
    | RefAbsent
    | RefInline of byte[]
    | RefNative of byte[] * int64 * int64      // chunk id, offset, length
    | RefVirtual of string * int64 * int64     // location, offset, length

/// Canonical one-line rendering of a placed ref. Only used as MANIFEST ID
/// INPUT: two chunk tables render identically exactly when they are the same
/// table, which is what makes an untouched array reuse its manifest file.
let private refText (coords: int64 list) (p: Placement) : string =
    let coordText = coords |> List.map string |> String.concat ","
    match p with
    | RefAbsent -> $"[{coordText}]=absent"
    | RefInline bs -> $"[{coordText}]=inline:" + (bs |> Array.map (sprintf "%02x") |> String.concat "")
    | RefNative (id, off, len) ->
        $"[{coordText}]=native:{base32Encode id}:{off}:{len}"
    | RefVirtual (loc, off, len) -> $"[{coordText}]=virtual:{loc}:{off}:{len}"

/// Where every chunk of one array goes, and the chunk FILES that implies.
///
/// PURE -- nothing is written here. The snapshot that names these files is
/// itself named by a hash of its own serialized bytes, so the whole plan has
/// to be computable before a single byte lands on disk.
type private ChunkPlan = {
    /// Placements in row-major grid order, paired with their coordinates.
    Placements: (int64 list * Placement) list
    /// `chunks/` files as (base32 file name, bytes). Chunk files are
    /// content-addressed, so the same name always carries the same bytes and
    /// a repeated entry (two snapshots, two arrays) is a no-op on write.
    Files: (string * byte[]) list
}

/// Plan every chunk of one array. See `ChunkPlan`.
let private planChunks (spec: RepoSpec) (a: ArraySpec) : ChunkPlan =
    let dims = gridDims a.Shape a.Chunks
    let omit =
        a.OmitChunks |> List.map (List.map string >> String.concat ",") |> Set.ofList
    let virtualAt =
        a.VirtualChunk |> Option.map (fun (c, loc) -> ((c |> List.map string |> String.concat ","), loc))
    let coords = gridCoords dims
    let key (c: int64 list) = c |> List.map string |> String.concat ","

    // Native chunks first, so a packed array can be laid out in one file
    // before any ref is built.
    let isPresent (c: int64 list) = not (Set.contains (key c) omit)
    let isVirtual (c: int64 list) =
        match virtualAt with
        | Some (vk, _) -> vk = key c
        | None -> false
    let bytesOf (c: int64 list) = chunkBytes a c
    let goesInline (c: int64 list) =
        a.InlineThreshold > 0 && (bytesOf c).Length <= a.InlineThreshold

    let nativeCoords =
        coords |> List.filter (fun c -> isPresent c && not (isVirtual c) && not (goesInline c))

    // Packed: one file per array holding every native chunk end to end, so
    // the refs carry NONZERO offsets. Unpacked: one file per chunk at
    // offset 0, which is what the reference writer emits.
    let (packedTable : Map<string, byte[] * int64 * int64>), (chunkFiles : (string * byte[]) list) =
        if a.PackNativeChunks && not (List.isEmpty nativeCoords) then
            let blobs = nativeCoords |> List.map bytesOf
            let all = Array.concat blobs
            let fileId = digest spec.Seed ("chunkpack/" + a.Name) all 12
            let mutable off = 0L
            let mutable acc = Map.empty
            for (c, b) in List.zip nativeCoords blobs do
                acc <- Map.add (key c) (fileId, off, int64 b.Length) acc
                off <- off + int64 b.Length
            (acc, [ (base32Encode fileId, all) ])
        else
            let mutable acc = Map.empty
            let mutable fs = []
            for c in nativeCoords do
                let b = bytesOf c
                let fileId = digest spec.Seed ("chunk/" + a.Name) b 12
                fs <- (base32Encode fileId, b) :: fs
                acc <- Map.add (key c) (fileId, 0L, int64 b.Length) acc
            (acc, List.rev fs)

    let placements =
        coords
        |> List.map (fun c ->
            let p =
                if not (isPresent c) then RefAbsent
                elif isVirtual c then
                    let loc = match virtualAt with Some (_, l) -> l | None -> ""
                    RefVirtual (loc, 0L, int64 (bytesOf c).Length)
                elif goesInline c then RefInline (bytesOf c)
                else
                    match Map.tryFind (key c) packedTable with
                    | Some (id, off, len) -> RefNative (id, off, len)
                    | None -> failwithf "IcechunkWrite '%s': chunk %A was neither placed nor absent" a.Name c
            (c, p))
    { Placements = placements; Files = chunkFiles }

/// A `ChunkRefT` for one placed chunk. The object API's default constructor
/// pre-populates `ChunkId` with a ZEROED ObjectId12, so an inline or virtual
/// ref must NULL it explicitly -- otherwise the buffer carries an all-zero
/// chunk_id beside the inline bytes and violates the schema's
/// exactly-one-of rule while still parsing.
let private chunkRefT (coords: int64 list) (p: Placement) : generated.ChunkRefT option =
    let baseRef () =
        let r = generated.ChunkRefT()
        r.Index <- ResizeArray<uint32>(coords |> List.map uint32)
        r.ChunkId <- null
        r
    match p with
    | RefAbsent -> None
    | RefInline bs ->
        let r = baseRef ()
        r.Inline <- ResizeArray<byte>(bs)
        Some r
    | RefNative (id, off, len) ->
        let r = baseRef ()
        let oid = generated.ObjectId12T()
        oid.Bytes <- Array.copy id
        r.ChunkId <- oid
        r.Offset <- uint64 off
        r.Length <- uint64 len
        Some r
    | RefVirtual (loc, off, len) ->
        let r = baseRef ()
        r.Location <- loc
        r.Offset <- uint64 off
        r.Length <- uint64 len
        Some r

// ---------------------------------------------------------------------------
// writeRepo
// ---------------------------------------------------------------------------

/// One manifest a snapshot points at, as it is being assembled.
type private PendingManifest = {
    Id: byte[]
    /// One `ChunkIndexRange {from, to}` per dimension, half-open.
    Extents: (uint32 * uint32) list
    Refs: (int64 list * generated.ChunkRefT) list
}

let private validateArray (a: ArraySpec) =
    if a.Shape.Length <> a.Chunks.Length then
        failwithf "IcechunkWrite '%s': shape rank %d != chunk rank %d" a.Name a.Shape.Length a.Chunks.Length
    if a.Chunks |> List.exists (fun c -> c <= 0L) then
        failwithf "IcechunkWrite '%s': non-positive chunk extent %A" a.Name a.Chunks
    let expected = a.Shape |> List.fold (*) 1L
    if int64 (dataLength a.Data) <> expected then
        failwithf "IcechunkWrite '%s': Data has %d cells but shape %A needs %d"
                  a.Name (dataLength a.Data) a.Shape expected
    match a.DimNames with
    | Some ds when ds.Length <> a.Shape.Length ->
        failwithf "IcechunkWrite '%s': %d dimension names for a rank-%d array" a.Name ds.Length a.Shape.Length
    | _ -> ()

/// Plan one array's manifests: one, or two when `SplitManifests` asks and the
/// leading axis admits a cut. Manifest ids are derived from the canonical text
/// of the refs they hold, so an array untouched between two snapshots plans
/// the SAME id -- and `writeManifestFile` then finds the file already there
/// and leaves it alone.
let private buildManifests (spec: RepoSpec) (a: ArraySpec)
                           (placed: (int64 list * Placement) list) : PendingManifest list =
    let dims = gridDims a.Shape a.Chunks
    let leading = match dims with | d :: _ -> d | [] -> 1L
    let cut = if a.SplitManifests && leading > 1L then leading / 2L else leading
    // Two half-open bands along axis 0: [0, cut) and [cut, leading). When
    // `cut = leading` the second band is empty and is dropped.
    let bands =
        if cut >= leading then [ (0u, uint32 leading) ]
        else [ (0u, uint32 cut); (uint32 cut, uint32 leading) ]
    bands
    |> List.mapi (fun bandIx (lo, hi) ->
        let inBand (c: int64 list) =
            match c with
            | first :: _ -> uint32 first >= lo && uint32 first < hi
            | [] -> true
        let refs =
            placed
            |> List.filter (fun (c, _) -> inBand c)
            |> List.choose (fun (c, p) -> chunkRefT c p |> Option.map (fun r -> (c, p, r)))
            // Ascending lexicographic order of the index vector. `placed` is
            // already row-major, which IS that order, but the sort states the
            // invariant instead of relying on it.
            |> List.sortWith (fun (ca, _, _) (cb, _, _) -> compare ca cb)
        if List.isEmpty refs then None
        else
            let extents =
                dims
                |> List.mapi (fun d n -> if d = 0 then (lo, hi) else (0u, uint32 n))
            let idInput =
                refs |> List.map (fun (c, p, _) -> refText c p) |> String.concat "\n"
            let manifestId =
                idFromText spec.Seed (sprintf "manifest/%s/%d" a.Name bandIx) idInput 12
            Some { Id = manifestId
                   Extents = extents
                   Refs = refs |> List.map (fun (c, _, r) -> (c, r)) })
    |> List.choose id

/// Serialize one manifest file. `Manifest.arrays` must be sorted by node id,
/// so a manifest holding several arrays is built only after every array's refs
/// are known. This writer keeps ONE array per manifest (the split above is a
/// split of one array's grid), so each file carries a single-entry `arrays`
/// vector -- legal, and it is what makes per-array id reuse across snapshots
/// exact.
///
/// PURE, like `planChunks`: the snapshot that points at this file records its
/// SIZE, so the bytes have to exist before the snapshot id that will name the
/// directory they live in.
let private manifestFileBytes (spec: RepoSpec) (arrayNodeId: byte[])
                              (m: PendingManifest) : byte[] =
    let am = generated.ArrayManifestT()
    let nid = generated.ObjectId8T()
    nid.Bytes <- Array.copy arrayNodeId
    am.NodeId <- nid
    am.Refs <- ResizeArray<generated.ChunkRefT>(m.Refs |> List.map snd)
    let mt = generated.ManifestT()
    let mid = generated.ObjectId12T()
    mid.Bytes <- Array.copy m.Id
    mt.Id <- mid
    mt.Arrays <- ResizeArray<generated.ArrayManifestT>([ am ])
    // No dictionary-compressed locations are written, so the algorithm field
    // says "none". (The schema's own default is 1; stating 0 describes what
    // is actually in the file.)
    mt.CompressionAlgorithm <- 0uy
    frameFile spec ftManifest (mt.SerializeToBinary())

// ---------------------------------------------------------------------------
// Planning a snapshot (pure), and the content-derived snapshot id
// ---------------------------------------------------------------------------

/// Everything one snapshot implies, computed WITHOUT touching the disk.
///
/// The plan has to be pure because the snapshot's ID IS A HASH OF IT: the
/// bytes exist before the name they are filed under does.
type private SnapshotPlan = {
    /// The chunk and manifest files this snapshot needs, as
    /// (subdirectory under the repo root, file name, bytes). Every one is
    /// content-addressed, so a repeated entry carries identical bytes.
    Files: (string * string * byte[]) list
    /// Builds the FlatBuffer object with the given 12 bytes stamped into
    /// `Snapshot.id`. EVERY OTHER FIELD IS ID-INDEPENDENT -- which is what
    /// makes hashing the zero-id form a hash of the snapshot's real content.
    Build: byte[] -> generated.SnapshotT
}

/// Plan one snapshot: its nodes, its manifests, and the files both imply.
let private planSnapshot (spec: RepoSpec) (s: SnapshotSpec) : SnapshotPlan =
    let nodes = ResizeArray<generated.NodeSnapshotT>()
    let manifestFiles = ResizeArray<generated.ManifestFileInfoV2T>()
    let files = ResizeArray<string * string * byte[]>()

    // The root group. Sorted first by path bytes ("/" < "/anything").
    let rootNode = generated.NodeSnapshotT()
    let rootId = generated.ObjectId8T()
    rootId.Bytes <- Array.copy (idFromText spec.Seed "node" "/" 8)
    rootNode.Id <- rootId
    rootNode.Path <- "/"
    rootNode.UserData <- ResizeArray<byte>(Encoding.UTF8.GetBytes rootGroupJson)
    rootNode.NodeData <- generated.NodeDataUnion.FromGroup(generated.GroupNodeDataT())
    nodes.Add rootNode

    for a in s.Arrays |> List.sortWith (fun x y -> utf8Compare ("/" + x.Name) ("/" + y.Name)) do
        let nid = nodeIdBytes spec a.Name
        let plan = planChunks spec a
        for (name, bytes) in plan.Files do
            files.Add ("chunks", name, bytes)
        let manifests = buildManifests spec a plan.Placements
        let manifestRefs = ResizeArray<generated.ManifestRefT>()
        for m in manifests do
            let mBytes = manifestFileBytes spec nid m
            files.Add ("manifests", base32Encode m.Id, mBytes)
            let mr = generated.ManifestRefT()
            let oid = generated.ObjectId12T()
            oid.Bytes <- Array.copy m.Id
            mr.ObjectId <- oid
            mr.Extents <-
                ResizeArray<generated.ChunkIndexRangeT>(
                    m.Extents |> List.map (fun (lo, hi) ->
                        let r = generated.ChunkIndexRangeT()
                        r.From <- lo
                        r.To <- hi
                        r))
            manifestRefs.Add mr
            let info = generated.ManifestFileInfoV2T()
            let iid = generated.ObjectId12T()
            iid.Bytes <- Array.copy m.Id
            info.Id <- iid
            info.SizeBytes <- uint64 mBytes.Length
            info.NumChunkRefs <- uint32 m.Refs.Length
            manifestFiles.Add info

        let arrNode = generated.ArrayNodeDataT()
        // V1's `shape` MUST be empty in a V2 snapshot; `shape_v2` carries
        // (array length, chunk COUNT) -- note that second field is a
        // count, not a chunk length.
        arrNode.Shape <- ResizeArray<generated.DimensionShapeT>()
        arrNode.ShapeV2 <-
            ResizeArray<generated.DimensionShapeV2T>(
                List.map2 (fun (len: int64) (nchunks: int64) ->
                    let d = generated.DimensionShapeV2T()
                    d.ArrayLength <- uint64 len
                    d.NumChunks <- uint32 nchunks
                    d) a.Shape (gridDims a.Shape a.Chunks))
        arrNode.DimensionNames <-
            match a.DimNames with
            | Some ds ->
                ResizeArray<generated.DimensionNameT>(
                    ds |> List.map (fun n ->
                        let d = generated.DimensionNameT()
                        d.Name <- n
                        d))
            | None -> null
        arrNode.Manifests <- manifestRefs

        let node = generated.NodeSnapshotT()
        let nidT = generated.ObjectId8T()
        nidT.Bytes <- Array.copy nid
        node.Id <- nidT
        node.Path <- "/" + a.Name
        node.UserData <- ResizeArray<byte>(Encoding.UTF8.GetBytes (arrayJson a))
        node.NodeData <- generated.NodeDataUnion.FromArray(arrNode)
        nodes.Add node

    // Sorted ONCE: `Build` may run twice (zero-id probe, then the real one)
    // and both passes must lay the vector out identically.
    let sortedManifestFiles =
        manifestFiles |> Seq.sortWith (fun x y -> byteCompare x.Id.Bytes y.Id.Bytes) |> List.ofSeq
    let nodeList = List.ofSeq nodes

    let build (idBytes: byte[]) =
        let snap = generated.SnapshotT()
        let sid = generated.ObjectId12T()
        sid.Bytes <- Array.copy idBytes
        snap.Id <- sid
        // V2 snapshots carry NO parent id (parent tracking moved to the repo
        // file). The object API's default constructor supplies a zeroed one,
        // so it has to be nulled explicitly.
        snap.ParentId <- null
        snap.Nodes <- ResizeArray<generated.NodeSnapshotT>(nodeList)
        snap.FlushedAt <- s.FlushedAtMicros
        snap.Message <- s.Message
        snap.Metadata <- ResizeArray<generated.MetadataItemT>()
        // Required, and MUST be empty in V2 (superseded by manifest_files_v2).
        snap.ManifestFiles <- ResizeArray<generated.ManifestFileInfoT>()
        snap.ManifestFilesV2 <- ResizeArray<generated.ManifestFileInfoV2T>(sortedManifestFiles)
        snap

    { Files = List.ofSeq files; Build = build }

/// The 12 zero bytes a snapshot wears while it is being hashed. Never reaches
/// disk: `writeRepo` always re-builds with the derived id before writing.
let private placeholderSnapshotId : byte[] = Array.zeroCreate 12

/// Hash a planned snapshot into its id. See `snapshotIdBytes`.
let private idOfPlan (spec: RepoSpec) (plan: SnapshotPlan) : byte[] =
    digest spec.Seed "snapshot" ((plan.Build placeholderSnapshotId).SerializeToBinary()) 12

/// A snapshot's 12-byte object id: a truncated SHA-256 over the snapshot's own
/// SERIALIZED CONTENT.
///
/// TWO PASSES, because a snapshot's id lives inside the snapshot. The object is
/// built once with a ZEROED id and serialized; that payload is hashed; the
/// object is then built again with the resulting id stamped in, and THAT is
/// what lands in `snapshots/`. The hashed form is the id-independent portion of
/// the file, and every other byte of it is covered -- node ids and paths, the
/// verbatim `zarr.json` of each array, shapes, dimension names, the manifest
/// ids and sizes each array points at (and manifest ids are themselves derived
/// from the chunk table, whose chunk ids are derived from the chunk bytes), the
/// commit message and the timestamp. Change any of them and the id moves;
/// change none of them and it does not.
///
/// This closes the loop the rest of the module already had: chunk and manifest
/// ids were content-derived from the start, and a snapshot id was not -- it
/// hashed the spec author's NAME for the snapshot, so "s1" named whatever "s1"
/// currently held and the same twenty characters could name two different
/// datasets across two commits of this repository. A pinned `@snapshot:<id>`
/// key claims bit-exact reproducibility; that claim is only true of an id that
/// is a function of the bits.
///
/// Contrast `nodeIdBytes`, which stays NAME-derived on purpose: a node id is
/// identity ACROSS snapshots, so it has to survive a rewrite of its own data.
///
/// Still a pure function of the `RepoSpec` -- nothing is read from disk, and
/// the same spec yields the same id in any directory, process or machine.
let snapshotIdBytes (spec: RepoSpec) (snapshotName: string) : byte[] =
    match spec.Snapshots |> List.tryFind (fun s -> s.Name = snapshotName) with
    | Some s -> idOfPlan spec (planSnapshot spec s)
    | None ->
        failwithf "IcechunkWrite: no snapshot named '%s' in this spec (have: %A)"
                  snapshotName (spec.Snapshots |> List.map (fun s -> s.Name))

/// The base32 spelling of `snapshotIdBytes` (20 characters) -- the
/// `snapshots/` file name, and the string a `@snapshot:<id>` key carries.
let snapshotId (spec: RepoSpec) (snapshotName: string) : string =
    base32Encode (snapshotIdBytes spec snapshotName)

// ---------------------------------------------------------------------------
// writeRepo
// ---------------------------------------------------------------------------

/// Write a complete Icechunk repo under `root`.
///
/// The directory is REPLACED: an existing `root` is deleted first, so a
/// fixture never inherits chunk or manifest files from an earlier spec (which
/// would silently break any test that counts or diffs them).
let writeRepo (root: string) (spec: RepoSpec) : unit =
    // --- validate the spec before touching the disk -------------------------
    let snapNames = spec.Snapshots |> List.map (fun s -> s.Name)
    if (snapNames |> List.distinct |> List.length) <> snapNames.Length then
        failwithf "IcechunkWrite: duplicate snapshot names in %A" snapNames
    for (kind, refs) in [ ("branch", spec.Branches); ("tag", spec.Tags) ] do
        for (refName, target) in refs do
            if not (List.contains target snapNames) then
                failwithf "IcechunkWrite: %s '%s' points at snapshot '%s', which the spec does not define (have: %A)"
                          kind refName target snapNames
    for s in spec.Snapshots do
        let names = s.Arrays |> List.map (fun a -> a.Name)
        if (names |> List.distinct |> List.length) <> names.Length then
            failwithf "IcechunkWrite: snapshot '%s' names an array twice: %A" s.Name names
        for a in s.Arrays do validateArray a

    // --- layout -------------------------------------------------------------
    if Directory.Exists root then Directory.Delete(root, true)
    Directory.CreateDirectory root |> ignore
    Directory.CreateDirectory (snapshotsDir root) |> ignore
    Directory.CreateDirectory (manifestsDir root) |> ignore
    Directory.CreateDirectory (chunksDir root) |> ignore
    // Present but empty: transaction logs are PRUNABLE, and `overwritten/`
    // holds repo-file backups a fixture never makes. A reader that needs
    // either to exist is a reader with a bug.
    Directory.CreateDirectory (Path.Combine(root, "transactions")) |> ignore
    Directory.CreateDirectory (Path.Combine(root, "overwritten")) |> ignore

    // --- snapshots ----------------------------------------------------------
    // PLAN EVERY SNAPSHOT FIRST. Snapshot ids are content-derived, so a
    // snapshot's bytes have to exist before the name it is filed under does --
    // and the ids are then computed ONCE here rather than re-derived at each of
    // the four places below that needs one (the file name, the `Snapshot.id`
    // field, the sort, and the repo file's `SnapshotInfo`).
    let plans =
        spec.Snapshots
        |> List.map (fun s ->
            let plan = planSnapshot spec s
            (s, plan, idOfPlan spec plan))
    let idBytesOf (name: string) =
        plans |> List.pick (fun (s, _, id) -> if s.Name = name then Some id else None)

    // Content-derived ids mean two snapshots that serialize identically ARE
    // one snapshot. `mkSnapshot` puts the spec name in the commit message, so
    // this cannot fire for a spec built the usual way -- but a hand-built spec
    // that repeats a whole commit would otherwise write ONE file while the
    // repo file claims two, which is a repo no reader can make sense of.
    let planIds = plans |> List.map (fun (_, _, id) -> base32Encode id)
    if (planIds |> List.distinct |> List.length) <> planIds.Length then
        failwithf "IcechunkWrite: two snapshots serialize identically and so share the content-derived id (%A for %A). Snapshots are told apart by their CONTENT -- message and timestamp included -- not by their spec names."
                  planIds (spec.Snapshots |> List.map (fun s -> s.Name))

    for (_, plan, idBytes) in plans do
        // Chunk and manifest files are content-addressed: the same name always
        // carries the same bytes, so writing only what is missing is not an
        // optimization but the statement that two snapshots SHARE a file.
        for (sub, name, bytes) in plan.Files do
            let path = Path.Combine(root, sub, name)
            if not (File.Exists path) then File.WriteAllBytes(path, bytes)
        let file = Path.Combine(snapshotsDir root, base32Encode idBytes)
        File.WriteAllBytes(file, frameFile spec ftSnapshot ((plan.Build idBytes).SerializeToBinary()))

    // --- the repo file ------------------------------------------------------
    // `Repo.snapshots` is sorted by id bytes, and `Ref.snapshot_index` indexes
    // into THAT order -- so the index map is built after the sort, never from
    // the spec's commit order.
    let commitOrder = spec.Snapshots |> List.map (fun s -> s.Name)
    let sortedNames =
        commitOrder
        |> List.sortWith (fun x y -> byteCompare (idBytesOf x) (idBytesOf y))
    let indexOf (name: string) = List.findIndex ((=) name) sortedNames

    let snapshotInfos =
        sortedNames
        |> List.map (fun name ->
            let s = spec.Snapshots |> List.find (fun s -> s.Name = name)
            let info = generated.SnapshotInfoT()
            let sid = generated.ObjectId12T()
            sid.Bytes <- Array.copy (idBytesOf name)
            info.Id <- sid
            // Each snapshot's parent is its predecessor in COMMIT order; the
            // offset is that parent's position in the SORTED list. -1 for the
            // initial commit. (No read path walks this -- plan section 5.2
            // rejects ancestry mechanisms outright -- but a repo whose
            // parent links are nonsense is not a repo.)
            info.ParentOffset <-
                match List.findIndex ((=) name) commitOrder with
                | 0 -> -1
                | i -> indexOf (List.item (i - 1) commitOrder)
            info.FlushedAt <- s.FlushedAtMicros
            info.Message <- s.Message
            info)

    let mkRefs (pairs: (string * string) list) =
        pairs
        |> List.sortWith (fun (a, _) (b, _) -> utf8Compare a b)
        |> List.map (fun (name, target) ->
            let r = generated.RefT()
            r.Name <- name
            r.SnapshotIndex <- uint32 (indexOf target)
            r)

    let status = generated.RepoStatusT()
    status.Availability <- spec.Availability
    status.SetAt <- 1700000000000000UL
    status.LimitedAvailabilityReason <-
        match spec.AvailabilityReason with
        | Some r -> r
        | None -> null

    let repo = generated.RepoT()
    repo.SpecVersion <- spec.SpecByte
    repo.Tags <- ResizeArray<generated.RefT>(mkRefs spec.Tags)
    repo.Branches <- ResizeArray<generated.RefT>(mkRefs spec.Branches)
    repo.DeletedTags <- ResizeArray<string>(spec.DeletedTags |> List.sortWith utf8Compare)
    repo.Snapshots <- ResizeArray<generated.SnapshotInfoT>(snapshotInfos)
    repo.Status <- status
    // Required by the schema; an empty ops log is a legal one.
    repo.LatestUpdates <- ResizeArray<generated.UpdateT>()
    File.WriteAllBytes(repoFilePath root, frameFile spec ftRepoInfo (repo.SerializeToBinary()))

/// Write the SAME repo into two roots. Fixtures need exactly this: one copy
/// resolved at the compiler's cwd (compile-time metadata and static folds) and
/// one mirrored beside the test executable (runtime chunk reads), because a
/// single relative path string has to work from both working directories.
let writeRepoAt (roots: string list) (spec: RepoSpec) : unit =
    for r in roots do writeRepo r spec
