// Blade-DSL Zarr Store Provider: compile-time metadata extraction from Zarr
// stores (v2 and v3). A Zarr store is a DIRECTORY: JSON metadata files plus
// one raw binary file per chunk. Metadata and compile-time data reads are
// pure .NET (System.Text.Json + File IO, no native library); runtime data
// I/O is generated C++ using only std::fstream / std::filesystem (no link-time dependency, unlike NetcdfProvider's libnetcdf).
//
// v1 scope (loud, specific rejections outside it):
//   - UNCOMPRESSED chunks only: v2 `compressor: null` / v3 a single `bytes`
//     codec. gzip/blosc/zstd/sharding/transpose are rejected BY NAME at
//     parse; the ZarrCodec seam below is where future codecs slot in.
//   - Little-endian on-disk data ('<' or '|' v2 dtypes, v3 bytes codec
//     endian "little"); big-endian is rejected.
//   - C (row-major) order; v2 order "F" is rejected (v3 F-order arrives as
//     a transpose codec, already rejected).
//   - Numeric dtypes only: f4/f8 and the integer codings (collapsed to
//     ETInt64, mirroring NetcdfProvider.ncTypeToElemType).
//
// Missing chunk files read as fill_value; a missing chunk with a null
// fill_value (v2) is a loud error, never silent zeros. Edge chunks are stored
// FULL-SIZE (padded) -- readers copy only the intersection with the array bounds.
module Blade.ZarrProvider

open System
open System.IO
open System.Text.Json
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types

// Metadata model

/// Normalized dtype: Code is the v2-style suffix without the byte-order char ("f8", "i4", "u2", ...); v3 names normalize onto the same codes.
type ZarrDtype = {
    Code: string
    Elem: ElemType
    ByteSize: int
    IsFloat: bool
}

type ZarrFill =
    | FillFloat of float
    | FillInt of int64
    /// v2 `fill_value: null` -- legal until a chunk is actually missing, then a loud error.
    | FillNone

/// The codec seam. v1 supports identity only; future compressed codecs (gzip, blosc, ...) add arms here, decoding in `decodeChunk` and emitting decompression calls in CppZarr.
type ZarrCodec =
    | CodecIdentity

/// Triangular-decomposed layout (the `blade` attribute): the physical
/// array's LEADING dimension is a packed pool in canonical ascending-lex
/// order. Two head kinds:
///   * spec_version 1 -- a SIMPLEX pool: C(n+r-1, r) cells for "sym", C(n,
///     r) for "antisym" (== linearized_storage's linearize order). Trailing
///     dims are ordinary dense axes; chunking the pool dimension yields
///     contiguous flat-cell ranges (one decomposition block = one chunk).
///   * spec_version 2 -- an ORBIT (iterated-wreath) pool: `Sym = SymWreath`,
///     `Levels` non-empty. No trailing dense dims.
/// See providers/ZarrTriangularSpec.md.
type PackedGroup = {
    Sym: SymmetryClass
    /// Depth-1: the group's arity r. Wreath: the RAW AXIS COUNT prod(r_i), matching `mkWreathIndexRecord`'s Rank field.
    Rank: int
    /// The base extent n (the wreath fold's base modulus).
    Extent: int64
    /// The iterated-wreath level list [(r1,s1), ...], OUTERMOST-LAST, `true` = '+'.
    /// Non-empty exactly when `Sym = SymWreath`, `[]` for a simplex head -- the same carrier convention `IR.orbitLevelsOf` uses.
    Levels: (int * bool) list
}

/// Block ordering for the simplex-blocks decomposition: ascending-lex over tile multisets (the combinadic rank), or the recursive-halving DFS ("mixed-radix path") order that keeps subtrees contiguous.
type BlockOrder =
    | OrderLex
    | OrderPath

/// simplex-blocks decomposition parameters: the pool is stored as padded block rows [blockCount, tile^rank] instead of one flat pool. See providers/ZarrSimplexBlocksPlan.md.
type SimplexBlocksInfo = {
    /// Tile edge B over the index interval [0, extent).
    Tile: int64
    /// T = ceil(extent / Tile).
    Grid: int64
    Order: BlockOrder
}

type BladeLayout = {
    /// v1: exactly one packed group, and it is the leading dimension.
    Group: PackedGroup
    /// Trailing dense extents (must match the physical shape's tail).
    DenseDims: int64 list
    /// None: layout "packed" (flat pool, decomposition "flat-ranges"); Some: layout "packed-blocks" (simplex-blocks rows).
    Blocks: SimplexBlocksInfo option
}

/// C(m, k) -- cardinality arithmetic for pool validation.
let binom (m: int64) (k: int) : int64 =
    if k < 0 || m < int64 k then 0L
    else
        let mutable num = 1L
        let mutable den = 1L
        for i in 0 .. k - 1 do
            num <- num * (m - int64 i)
            den <- den * int64 (i + 1)
        num / den

/// Packed pool cardinality of a group, with the failure surfaced: multiset
/// (sym) or strict (antisym) combinations at depth 1, and the iterated fold
/// over the level list for a wreath head. The wreath arm goes through
/// `Blade.OrbRank.cellCountChecked`, the SAME overflow-checked fold
/// `IR.bufferGroupCardinality`/`IRStorage.classifyOutputStorage` size the in-memory
/// pool with, so the two never independently disagree about a pool length.
let packedCardinalityChecked (g: PackedGroup) : Result<int64, string> =
    match g.Sym with
    | SymSymmetric -> Ok (binom (g.Extent + int64 g.Rank - 1L) g.Rank)
    | SymAntisymmetric -> Ok (binom g.Extent g.Rank)
    | SymWreath ->
        Blade.OrbRank.cellCountChecked (Blade.IR.orbRankLevels g.Levels) g.Extent
    | s ->
        Error (sprintf "packed group symmetry %A has no pool cardinality" s)

/// Unchecked cardinality for the depth-1 (simplex) call sites (packed-blocks
/// math and window extraction, neither reachable by a wreath head). A failure
/// answers 0 rather than a guess: a zero-sized pool fails visibly, a wrong-sized one does not.
let packedCardinality (g: PackedGroup) : int64 =
    match packedCardinalityChecked g with
    | Ok c -> c
    | Error _ -> 0L

// Simplex-blocks math (decomposition scheme "simplex-blocks"): the block
// grid of a rank-r simplex with T tiles IS SymIdx<r, T>: blocks are tile
// MULTISETS in both the sym and antisym cases (a repeated tile holds the
// strict cells inside it in the antisym case). AntisymIdx per-tile factors
// are C(w, m) not C(w+m-1, m), and a tile of width w with multiplicity
// m > w contributes an EMPTY block -- every repeated-tile block is empty when Tile = 1.
module SimplexBlocks =

    /// Combinadic rank of a canonical coordinate tuple (ascending-lex order; sorted, strictly increasing when strict). Mirrors linearized_storage::{symmetric|antisymmetric}::linearize.
    let rankOfCoords (strict: bool) (n: int64) (coords: int64[]) : int64 =
        let r = coords.Length
        // Completions with m positions remaining, next value >= v (+1 strict).
        let completions (v: int64) (m: int) : int64 =
            if strict then binom (n - v - 1L) m
            else binom ((n - v) + int64 m - 1L) m
        let mutable rank = 0L
        let mutable lo = 0L
        for k in 0 .. r - 1 do
            let mutable v = lo
            while v < coords.[k] do
                rank <- rank + completions v (r - k - 1)
                v <- v + 1L
            lo <- coords.[k] + (if strict then 1L else 0L)
        rank

    /// Inverse of rankOfCoords.
    let unrankToCoords (strict: bool) (n: int64) (r: int) (rank: int64) : int64[] =
        let completions (v: int64) (m: int) : int64 =
            if strict then binom (n - v - 1L) m
            else binom ((n - v) + int64 m - 1L) m
        let coords = Array.zeroCreate r
        let mutable rest = rank
        let mutable lo = 0L
        for k in 0 .. r - 1 do
            let mutable v = lo
            let mutable c = completions v (r - k - 1)
            while rest >= c do
                rest <- rest - c
                v <- v + 1L
                c <- completions v (r - k - 1)
            coords.[k] <- v
            lo <- v + (if strict then 1L else 0L)
        coords

    /// Number of blocks: tile multisets of size r over T tiles (both symmetries -- blocks are always multisets).
    let blockCount (r: int) (T: int64) : int64 = binom (T + int64 r - 1L) r

    /// Width of tile t over [0, n) with edge B (the last tile may be ragged).
    let tileWidth (n: int64) (B: int64) (t: int64) : int64 = min B (n - t * B)

    /// Cells in a block (tile multiset, ascending): group tiles by value; a tile of width w with multiplicity m contributes C(w+m-1, m) (sym) or C(w, m) (antisym -- ZERO when m > w: the empty diagonal blocks).
    let blockCellCount (strict: bool) (n: int64) (B: int64) (tiles: int64[]) : int64 =
        tiles
        |> Array.countBy id
        |> Array.fold (fun acc (t, m) ->
            let w = tileWidth n B t
            let f = if strict then binom w m else binom (w + int64 m - 1L) m
            acc * f) 1L

    /// The format's fixed row width: B^r bounds every block's cell count (each per-tile factor is <= w^m <= B^m).
    let maxBlockCells (r: int) (B: int64) : int64 =
        let mutable acc = 1L
        for _ in 1 .. r do acc <- acc * B
        acc

    /// A block's cells in absolute ascending-lex order (the within-block canonical order both the writer and the emitted C++ use): branch-free bounds i_k in [max(tile_k*B, i_{k-1}+strict), min((tile_k+1)*B, n)).
    let enumBlockCells (strict: bool) (n: int64) (B: int64) (tiles: int64[]) : seq<int64[]> =
        let r = tiles.Length
        let rec go (k: int) (prev: int64) (acc: int64 list) : seq<int64[]> =
            seq {
                if k = r then
                    yield (List.rev acc |> Array.ofList)
                else
                    let tileLo = tiles.[k] * B
                    let lo =
                        if k = 0 then tileLo
                        else max tileLo (prev + (if strict then 1L else 0L))
                    let hi = min ((tiles.[k] + 1L) * B) n
                    for i in lo .. hi - 1L do
                        yield! go (k + 1) i (i :: acc)
            }
        go 0 -1L []

    /// Tile multisets in recursive-halving DFS ("mixed-radix path") order:
    /// split [a, b) at the midpoint; children ordered all-low first (j = r
    /// down to 0 tiles in the low half). Requires a power-of-two grid.
    let rec private pathMultisets (a: int64) (b: int64) (r: int) : seq<int64 list> =
        seq {
            if r = 0 then yield []
            elif b - a = 1L then yield List.replicate r a
            else
                let m = a + (b - a) / 2L
                for j in r .. -1 .. 0 do
                    for low in pathMultisets a m j do
                        for high in pathMultisets m b (r - j) do
                            yield low @ high
        }

    let isPowerOfTwo (x: int64) = x > 0L && (x &&& (x - 1L)) = 0L

    /// Physical row of each block under "path" order: pathRows.[lexBlockRank] = row index in the store.
    let pathRows (r: int) (T: int64) : int64[] =
        if not (isPowerOfTwo T) then
            failwith $"simplex-blocks path order requires a power-of-two grid (got {T})"
        let rows = Array.zeroCreate (int (blockCount r T))
        pathMultisets 0L T r
        |> Seq.iteri (fun i ms ->
            rows.[int (rankOfCoords false T (Array.ofList ms))] <- int64 i)
        rows

    /// (physical padded cell count, canonical pool cell count): the padding overhead report for a configuration.
    let paddingReport (strict: bool) (n: int64) (B: int64) (r: int) : int64 * int64 =
        let T = (n + B - 1L) / B
        let phys = blockCount r T * maxBlockCells r B
        let pool =
            if strict then binom n r else binom (n + int64 r - 1L) r
        (phys, pool)

    /// physCellIdx -> poolCellIdx map (cells only, trailing dims excluded);
    /// -1 marks padding. physCellIdx = physicalRow * B^r + localOffset
    /// (ascending-lex within the block). Shared by the F# writer (inverted) and reader so the two sides cannot drift.
    let blocksCellMap (strict: bool) (n: int64) (B: int64) (r: int) (order: BlockOrder) : int[] =
        let T = (n + B - 1L) / B
        let nBlocks = blockCount r T
        let rowW = maxBlockCells r B
        let rowOf =
            match order with
            | OrderLex -> fun (b: int64) -> b
            | OrderPath ->
                let rows = pathRows r T
                fun (b: int64) -> rows.[int b]
        let map = Array.create (int (nBlocks * rowW)) -1
        for b in 0L .. nBlocks - 1L do
            let tiles = unrankToCoords false T r b
            let row = rowOf b
            let mutable local = 0L
            for cell in enumBlockCells strict n B tiles do
                let pool = rankOfCoords strict n cell
                map.[int (row * rowW + local)] <- int pool
                local <- local + 1L
        map

let decodeChunk (codec: ZarrCodec) (raw: byte[]) : byte[] =
    match codec with
    | CodecIdentity -> raw

type ZarrArrayMeta = {
    Name: string
    /// Absolute path of the array directory (metadata + chunk files).
    ArrayDir: string
    Shape: int64 list
    Chunks: int64 list
    Dtype: ZarrDtype
    /// xarray `_ARRAY_DIMENSIONS` (v2 .zattrs) / `dimension_names` (v3).
    DimNames: string list option
    FillValue: ZarrFill
    Codec: ZarrCodec
    /// Triangular-decomposed layout from the `blade` attribute; None for ordinary dense arrays.
    Blade: BladeLayout option
    /// 2 | 3
    Version: int
    /// Chunk-key separator ("." v2 default, "/" v3 default).
    ChunkKeySep: string
    /// Chunk-key prefix ("" for v2 and v3 "v2" encoding; "c" for v3 default).
    ChunkKeyPrefix: string
}

type ZarrStore = {
    Path: string
    Version: int
    Arrays: ZarrArrayMeta list
}

/// A variable's payload read at compile time: dimension extents plus the row-major flat buffer (mirrors NetcdfProvider.NcVarData).
type ZarrVarData = {
    DimLengths: int list
    Payload: ZarrPayload
}
and ZarrPayload =
    | ZFloats of float[]
    | ZInts of int64[]

// Dtype tables

/// v2-style normalized code -> dtype record (the integer collapse to ETInt64 mirrors ncTypeToElemType).
let private dtypeOfCode (code: string) : Result<ZarrDtype, string> =
    match code with
    | "f4" -> Ok { Code = "f4"; Elem = ETFloat32; ByteSize = 4; IsFloat = true }
    | "f8" -> Ok { Code = "f8"; Elem = ETFloat64; ByteSize = 8; IsFloat = true }
    | "i1" -> Ok { Code = "i1"; Elem = ETInt64; ByteSize = 1; IsFloat = false }
    | "i2" -> Ok { Code = "i2"; Elem = ETInt64; ByteSize = 2; IsFloat = false }
    | "i4" -> Ok { Code = "i4"; Elem = ETInt64; ByteSize = 4; IsFloat = false }
    | "i8" -> Ok { Code = "i8"; Elem = ETInt64; ByteSize = 8; IsFloat = false }
    | "u1" -> Ok { Code = "u1"; Elem = ETInt64; ByteSize = 1; IsFloat = false }
    | "u2" -> Ok { Code = "u2"; Elem = ETInt64; ByteSize = 2; IsFloat = false }
    | "u4" -> Ok { Code = "u4"; Elem = ETInt64; ByteSize = 4; IsFloat = false }
    | "u8" -> Ok { Code = "u8"; Elem = ETInt64; ByteSize = 8; IsFloat = false }
    | other -> Error $"unsupported dtype '{other}' (numeric f4/f8/i*/u* only -- bool, complex, datetime and string dtypes are not supported)"

/// v2 dtype string ("<f8", "|i1", ">f4"): byte-order char + code.
let zarrDtypeV2 (dtype: string) : Result<ZarrDtype, string> =
    if String.IsNullOrEmpty dtype || dtype.Length < 2 then
        Error $"malformed v2 dtype '{dtype}'"
    else
        match dtype.[0] with
        | '<' | '|' -> dtypeOfCode (dtype.Substring 1)
        | '>' -> Error $"big-endian dtype '{dtype}' is not supported (little-endian stores only)"
        | _ -> Error $"malformed v2 dtype '{dtype}' (expected a byte-order prefix '<', '|' or '>')"

/// v3 data_type name ("float64", "int32", ...).
let zarrDtypeV3 (name: string) : Result<ZarrDtype, string> =
    match name with
    | "float32" -> dtypeOfCode "f4"
    | "float64" -> dtypeOfCode "f8"
    | "int8" -> dtypeOfCode "i1"
    | "int16" -> dtypeOfCode "i2"
    | "int32" -> dtypeOfCode "i4"
    | "int64" -> dtypeOfCode "i8"
    | "uint8" -> dtypeOfCode "u1"
    | "uint16" -> dtypeOfCode "u2"
    | "uint32" -> dtypeOfCode "u4"
    | "uint64" -> dtypeOfCode "u8"
    | other -> Error $"unsupported data_type '{other}' (numeric float32/float64/int*/uint* only)"

// JSON parsing helpers

let private tryProp (el: JsonElement) (name: string) : JsonElement option =
    match el.TryGetProperty name with
    | true, v -> Some v
    | _ -> None

let private jsonInt64List (el: JsonElement) : int64 list =
    el.EnumerateArray() |> Seq.map (fun e -> e.GetInt64()) |> List.ofSeq

let private jsonStringList (el: JsonElement) : string list =
    el.EnumerateArray() |> Seq.map (fun e -> e.GetString()) |> List.ofSeq

/// fill_value: number, "NaN"/"Infinity"/"-Infinity" strings, or null.
let private parseFill (where_: string) (isFloat: bool) (el: JsonElement option) : Result<ZarrFill, string> =
    match el with
    | None -> Ok FillNone
    | Some e ->
        match e.ValueKind with
        | JsonValueKind.Null -> Ok FillNone
        | JsonValueKind.Number ->
            if isFloat then Ok (FillFloat (e.GetDouble()))
            else
                match e.TryGetInt64() with
                | true, n -> Ok (FillInt n)
                | _ -> Ok (FillInt (int64 (e.GetDouble())))
        | JsonValueKind.String ->
            (match e.GetString(), isFloat with
             | "NaN", true -> Ok (FillFloat nan)
             | "Infinity", true -> Ok (FillFloat infinity)
             | "-Infinity", true -> Ok (FillFloat (-infinity))
             | s, _ -> Error $"{where_}: unsupported fill_value '{s}'")
        | _ -> Error (sprintf "%s: unsupported fill_value kind %A" where_ e.ValueKind)

/// Parse + validate the `blade` layout attribute (spec_version 1 or 2)
/// against the physical shape. `attrs` is the attributes object (v2
/// .zattrs root / v3 `attributes`); an absent "blade" key is an ordinary
/// dense array. spec_version 2 adds ONE thing: the `"orbit"` head, an
/// iterated-wreath (`OrbIdx` depth >= 2) pool -- rejected BY NAME under
/// spec_version 1, since v1's kind set is closed and an old reader must not silently reinterpret a v2 store.
let parseBladeLayout (where_: string) (shape: int64 list) (attrs: JsonElement option) : Result<BladeLayout option, string> =
    match attrs |> Option.bind (fun a -> tryProp a "blade") with
    | None -> Ok None
    | Some b ->
        let strOf name dflt =
            tryProp b name
            |> Option.map (fun v -> if v.ValueKind = JsonValueKind.String then v.GetString() else "")
            |> Option.defaultValue dflt
        let specVersion =
            tryProp b "spec_version"
            |> Option.map (fun v -> if v.ValueKind = JsonValueKind.Number then v.GetInt32() else -1)
            |> Option.defaultValue -1
        let layoutStr = strOf "layout" ""
        if specVersion <> 1 && specVersion <> 2 then
            Error $"{where_}: blade.spec_version {specVersion} is not supported (this reader implements spec_version 1 and 2)"
        elif layoutStr <> "packed" && layoutStr <> "packed-blocks" then
            Error $"{where_}: blade.layout '{layoutStr}' is not supported ('packed' or 'packed-blocks')"
        elif strOf "order" "ascending-lex" <> "ascending-lex" then
            Error ($"""{where_}: blade.order '{(strOf "order" "")}' is not supported ('ascending-lex' only -- the pinned linearized_storage order)""")
        else
        match tryProp b "index_types" with
        | Some its when its.ValueKind = JsonValueKind.Array && its.GetArrayLength() >= 1 ->
            let entries = its.EnumerateArray() |> List.ofSeq
            let parseEntry (i: int) (e: JsonElement) : Result<Choice<PackedGroup, int64>, string> =
                let kind =
                    tryProp e "kind"
                    |> Option.map (fun v -> if v.ValueKind = JsonValueKind.String then v.GetString() else "")
                    |> Option.defaultValue ""
                let extent =
                    tryProp e "extent"
                    |> Option.map (fun v -> if v.ValueKind = JsonValueKind.Number then v.GetInt64() else -1L)
                    |> Option.defaultValue -1L
                match kind with
                | "dense" ->
                    if extent > 0L then Ok (Choice2Of2 extent)
                    else Error $"{where_}: blade.index_types[{i}]: dense entry needs a positive extent"
                | "sym" | "antisym" ->
                    let rank =
                        tryProp e "rank"
                        |> Option.map (fun v -> if v.ValueKind = JsonValueKind.Number then v.GetInt32() else -1)
                        |> Option.defaultValue -1
                    if rank < 2 then Error $"{where_}: blade.index_types[{i}]: packed group needs rank >= 2"
                    elif extent <= 0L then Error $"{where_}: blade.index_types[{i}]: packed group needs a positive extent"
                    else
                        let sym = if kind = "sym" then SymSymmetric else SymAntisymmetric
                        Ok (Choice1Of2 { Sym = sym; Rank = rank; Extent = extent; Levels = [] })
                | "orbit" when specVersion < 2 ->
                    // The v1 door, shut by name: v1's kind set is closed, so an
                    // orbit head reaching a v1 reader must be a loud refusal, never
                    // a reinterpretation as some simplex group with matching cardinality.
                    Error ($"{where_}: blade.index_types[{i}]: kind 'orbit' (an iterated-wreath OrbIdx class) is a spec_version 2 head, but this store declares spec_version {specVersion} -- see providers/ZarrTriangularSpec.md")
                | "orbit" ->
                    // levels: [[r, "+"|"-"], ...], OUTERMOST-LAST -- a reader
                    // must not reverse this direction.
                    let parseLevel (j: int) (le: JsonElement) : Result<int * bool, string> =
                        if le.ValueKind <> JsonValueKind.Array || le.GetArrayLength() <> 2 then
                            Error $"{where_}: blade.index_types[{i}].levels[{j}]: each level is a two-element array [rank, \"+\"|\"-\"]"
                        else
                            let items = le.EnumerateArray() |> Array.ofSeq
                            let r = if items.[0].ValueKind = JsonValueKind.Number then items.[0].GetInt32() else -1
                            let s = if items.[1].ValueKind = JsonValueKind.String then items.[1].GetString() else ""
                            if r < 2 then
                                // A rank-1 level is the trivial group and normalizes
                                // away (OrbRank.normalizeLevels), so admitting it
                                // would put TWO spellings of one class on disk.
                                Error $"{where_}: blade.index_types[{i}].levels[{j}]: level rank {r} is not >= 2 (a rank-1 level is the trivial group and must be omitted, so that one class has one spelling on disk)"
                            elif s <> "+" && s <> "-" then
                                Error $"{where_}: blade.index_types[{i}].levels[{j}]: sign '{s}' is not \"+\" or \"-\""
                            else Ok (r, (s = "+"))
                    match tryProp e "levels" with
                    | Some ls when ls.ValueKind = JsonValueKind.Array ->
                        let rec collectLevels j (acc: (int * bool) list) =
                            let items = ls.EnumerateArray() |> Array.ofSeq
                            if j >= items.Length then Ok (List.rev acc)
                            else
                                match parseLevel j items.[j] with
                                | Ok lv -> collectLevels (j + 1) (lv :: acc)
                                | Error err -> Error err
                        (match collectLevels 0 [] with
                         | Error err -> Error err
                         | Ok levels ->
                             if List.length levels < 2 then
                                 // The one-spelling rule: OrbIdx<[(r,+)],n> IS SymIdx<r,n>
                                 // and OrbIdx<[(r,-)],n> IS AntisymIdx<r,n>, so a depth-1
                                 // orbit head would be a second spelling of a class that already has one.
                                 Error ($"{where_}: blade.index_types[{i}]: an 'orbit' head needs depth >= 2 (got {(List.length levels)} level(s)). A depth-1 class is written as kind 'sym' / 'antisym' -- OrbIdx<[(r,+)],n> IS SymIdx<r,n> and OrbIdx<[(r,-)],n> IS AntisymIdx<r,n>, and one class gets one spelling on disk")
                             elif extent <= 0L then
                                 Error $"{where_}: blade.index_types[{i}]: orbit head needs a positive extent (the class's base extent n)"
                             else
                                 let axes = levels |> List.fold (fun a (r, _) -> a * r) 1
                                 Ok (Choice1Of2 { Sym = SymWreath; Rank = axes; Extent = extent; Levels = levels }))
                    | _ ->
                        Error $"{where_}: blade.index_types[{i}]: orbit head is missing its 'levels' array"
                | "herm" ->
                    Error $"{where_}: blade.index_types[{i}]: kind 'herm' is reserved (constraint-coupled cells) and not supported"
                | other ->
                    Error ($"""{where_}: blade.index_types[{i}]: unknown kind '{other}' (sym | antisym | dense{(if specVersion >= 2 then " | orbit" else "")})""")
            let rec collect i acc =
                if i >= entries.Length then Ok (List.rev acc)
                else
                    match parseEntry i entries.[i] with
                    | Ok c -> collect (i + 1) (c :: acc)
                    | Error e -> Error e
            match collect 0 [] with
            | Error e -> Error e
            | Ok parsed ->
                match parsed with
                | Choice1Of2 group :: rest ->
                    let isOrbit = (group.Sym = SymWreath)
                    let headDesc =
                        if isOrbit then $"orbit {Blade.IR.ppOrbitLevels group.Levels}"
                        elif group.Sym = SymSymmetric then "sym"
                        else "antisym"
                    if rest |> List.exists (function Choice1Of2 _ -> true | _ -> false) then
                        Error $"{where_}: blade layout supports exactly ONE packed group (and it must be leading)"
                    elif isOrbit && layoutStr <> "packed" then
                        // 'packed-blocks' decomposes a SIMPLEX by tile multisets (the
                        // block grid of a rank-r simplex is itself SymIdx<r,T>); a
                        // wreath pool's rows shrink per LEVEL, with no tile multiset to decompose by.
                        Error $"{where_}: blade.layout 'packed-blocks' is not defined for an orbit (iterated-wreath) head -- spec_version 2 is the flat single-pool layout only"
                    elif isOrbit && not (List.isEmpty rest) then
                        // The in-memory side has no trailing dims for a wreath array at all, so this is a refusal, not a mis-shape.
                        Error ($"{where_}: blade orbit head with {(List.length rest)} trailing dense dim(s) is not yet supported -- a wreath array's in-memory representation is a SOLE flat pool (no index group composes with it), so writers never emit trailing dims beside an orbit head")
                    else
                    match packedCardinalityChecked group with
                    | Error detail ->
                        Error ($"{where_}: blade packed group ({headDesc}, extent {group.Extent}): cardinality cannot be computed -- {detail}")
                    | Ok card ->
                        let denseDims = rest |> List.map (function Choice2Of2 d -> d | _ -> 0L)
                        if layoutStr = "packed" then
                            match shape with
                            | pool :: tail when pool = card && tail = denseDims ->
                                Ok (Some { Group = group; DenseDims = denseDims; Blocks = None })
                            | pool :: _ when pool <> card ->
                                Error ($"{where_}: blade packed group ({headDesc}, rank {group.Rank}, extent {group.Extent}) has cardinality {card} but the pool dimension is {(List.head shape)} -- a corrupt or mislabeled store")
                            | _ ->
                                Error (sprintf "%s: blade dense dims %A do not match the physical trailing shape %A" where_ denseDims (List.tail shape))
                        else
                            // packed-blocks: shape = [blockCount, tile^rank] @ dense,
                            // parameters from the decomposition object.
                            match tryProp b "decomposition" with
                            | None ->
                                Error $"{where_}: blade.layout 'packed-blocks' requires a decomposition object (scheme 'simplex-blocks')"
                            | Some d ->
                                let dStr name dflt =
                                    tryProp d name
                                    |> Option.map (fun v -> if v.ValueKind = JsonValueKind.String then v.GetString() else "")
                                    |> Option.defaultValue dflt
                                let dInt name =
                                    tryProp d name
                                    |> Option.map (fun v -> if v.ValueKind = JsonValueKind.Number then v.GetInt64() else -1L)
                                let scheme = dStr "scheme" ""
                                if scheme <> "simplex-blocks" then
                                    Error $"{where_}: blade decomposition scheme '{scheme}' is not supported for packed-blocks ('simplex-blocks' only)"
                                else
                                match dInt "tile", dInt "grid" with
                                | Some tile, Some grid when tile > 0L && grid > 0L ->
                                    let expectGrid = (group.Extent + tile - 1L) / tile
                                    let orderStr = dStr "block_order" "ascending-lex"
                                    let orderRes =
                                        match orderStr with
                                        | "ascending-lex" -> Ok OrderLex
                                        | "path" ->
                                            if SimplexBlocks.isPowerOfTwo grid then
                                                match dInt "depth" with
                                                | Some dep when (1L <<< int dep) <> grid ->
                                                    Error $"{where_}: blade decomposition depth {dep} does not match grid {grid} (expected log2)"
                                                | _ -> Ok OrderPath
                                            else Error $"{where_}: blade block_order 'path' requires a power-of-two grid (got {grid})"
                                        | other -> Error $"{where_}: blade block_order '{other}' is not supported ('ascending-lex' or 'path')"
                                    if grid <> expectGrid then
                                        Error $"{where_}: blade decomposition grid {grid} does not match ceil(extent {group.Extent} / tile {tile}) = {expectGrid}"
                                    else
                                    match orderRes with
                                    | Error e -> Error e
                                    | Ok order ->
                                        let nBlocks = SimplexBlocks.blockCount group.Rank grid
                                        let rowW = SimplexBlocks.maxBlockCells group.Rank tile
                                        match shape with
                                        | b0 :: b1 :: tail when b0 = nBlocks && b1 = rowW && tail = denseDims ->
                                            Ok (Some { Group = group
                                                       DenseDims = denseDims
                                                       Blocks = Some { Tile = tile; Grid = grid; Order = order } })
                                        | _ ->
                                            Error (sprintf "%s: packed-blocks physical shape %A does not match [blockCount %d, tile^rank %d] @ dense %A"
                                                       where_ shape nBlocks rowW denseDims)
                                | _ ->
                                    Error $"{where_}: blade decomposition needs positive integer tile and grid"
                | _ ->
                    Error ($"""{where_}: blade layout's FIRST index_types entry must be the packed group (sym/antisym{(if specVersion >= 2 then "/orbit" else "")})""")
        | _ -> Error $"{where_}: blade layout is missing index_types"

// Array metadata parsers (pure: JSON text in, Result out -- unit-testable)

/// Parse a v2 array's `.zarray` (+ optional `.zattrs` for _ARRAY_DIMENSIONS).
let parseArrayMetaV2 (name: string) (arrayDir: string) (zarrayJson: string) (zattrsJson: string option) : Result<ZarrArrayMeta, string> =
    try
        use doc = JsonDocument.Parse zarrayJson
        let root = doc.RootElement
        let where_ = $"array '{name}'"
        match tryProp root "shape", tryProp root "chunks", tryProp root "dtype" with
        | Some shapeEl, Some chunksEl, Some dtypeEl ->
            let shape = jsonInt64List shapeEl
            let chunks = jsonInt64List chunksEl
            if shape.Length <> chunks.Length then
                Error $"{where_}: shape rank {shape.Length} != chunks rank {chunks.Length}"
            elif chunks |> List.exists (fun c -> c <= 0L) then
                Error $"{where_}: non-positive chunk extent"
            else
            // Compression gate (the v1 uncompressed contract).
            let compressorErr =
                match tryProp root "compressor" with
                | None | Some _ when (match tryProp root "compressor" with
                                      | Some c -> c.ValueKind = JsonValueKind.Null
                                      | None -> true) -> None
                | Some c ->
                    let cid =
                        match tryProp c "id" with
                        | Some idEl when idEl.ValueKind = JsonValueKind.String -> idEl.GetString()
                        | _ -> "<unknown>"
                    // NAME THE CODEC AND THE USER-SIDE REMEDY. zarr-python and
                    // icechunk-python both compress BY DEFAULT, so this is the
                    // first wall a real store hits, and "see the ZarrCodec
                    // extension point" pointed at a seam inside this compiler
                    // -- a thing to implement, not a thing to do. The action is
                    // in the writer.
                    Some $"{where_} uses compressor '{cid}' -- compressed Zarr stores are not supported (uncompressed only). Rewrite the array with compression off (zarr-python v2: `compressor=None, filters=None`; v3: `compressors=None, filters=None`), or convert the store once with a copy that does. The compiler-side seam for adding a codec is ZarrProvider's ZarrCodec type"
                | None -> None
            match compressorErr with
            | Some e -> Error e
            | None ->
            let filtersErr =
                match tryProp root "filters" with
                | None -> None
                | Some f when f.ValueKind = JsonValueKind.Null -> None
                | Some f when f.ValueKind = JsonValueKind.Array && f.GetArrayLength() = 0 -> None
                | Some _ -> Some $"{where_} uses filters -- not supported (uncompressed, unfiltered only)"
            match filtersErr with
            | Some e -> Error e
            | None ->
            let orderErr =
                match tryProp root "order" with
                | Some o when o.ValueKind = JsonValueKind.String && o.GetString() = "C" -> None
                | Some o when o.ValueKind = JsonValueKind.String -> Some $"{where_} has order '{o.GetString()}' -- only C (row-major) order is supported"
                | _ -> None  // missing order: tolerate, C assumed
            match orderErr with
            | Some e -> Error e
            | None ->
            match zarrDtypeV2 (dtypeEl.GetString()) with
            | Error e -> Error $"{where_}: {e}"
            | Ok dt ->
            match parseFill where_ dt.IsFloat (tryProp root "fill_value") with
            | Error e -> Error e
            | Ok fill ->
            let sep =
                match tryProp root "dimension_separator" with
                | Some s when s.ValueKind = JsonValueKind.String -> s.GetString()
                | _ -> "."
            if sep <> "." && sep <> "/" then
                Error $"{where_}: unsupported dimension_separator '{sep}'"
            else
            // .zattrs carries both the xarray dim names and the blade layout.
            let (dimNames, bladeRes) =
                match zattrsJson with
                | None -> (None, Ok None)
                | Some txt ->
                    try
                        use adoc = JsonDocument.Parse txt
                        let root = adoc.RootElement
                        let dn =
                            match tryProp root "_ARRAY_DIMENSIONS" with
                            | Some arr when arr.ValueKind = JsonValueKind.Array -> Some (jsonStringList arr)
                            | _ -> None
                        (dn, parseBladeLayout where_ shape (Some root))
                    with _ -> (None, Ok None)
            match bladeRes with
            | Error e -> Error e
            | Ok blade ->
            Ok { Name = name; ArrayDir = arrayDir; Shape = shape; Chunks = chunks
                 Dtype = dt; DimNames = dimNames; FillValue = fill; Codec = CodecIdentity
                 Blade = blade; Version = 2; ChunkKeySep = sep; ChunkKeyPrefix = "" }
        | _ -> Error $"array '{name}': .zarray is missing shape/chunks/dtype"
    with ex ->
        Error $"array '{name}': malformed .zarray JSON: {ex.Message}"

/// Parse a v3 array's `zarr.json`.
let parseArrayMetaV3 (name: string) (arrayDir: string) (zarrJson: string) : Result<ZarrArrayMeta, string> =
    try
        use doc = JsonDocument.Parse zarrJson
        let root = doc.RootElement
        let where_ = $"array '{name}'"
        let nodeType =
            match tryProp root "node_type" with
            | Some nt when nt.ValueKind = JsonValueKind.String -> nt.GetString()
            | _ -> ""
        if nodeType <> "array" then Error $"{where_}: zarr.json node_type is '{nodeType}', expected 'array'"
        else
        match tryProp root "shape", tryProp root "data_type", tryProp root "chunk_grid" with
        | Some shapeEl, Some dtypeEl, Some gridEl ->
            let shape = jsonInt64List shapeEl
            let gridName =
                match tryProp gridEl "name" with
                | Some n when n.ValueKind = JsonValueKind.String -> n.GetString()
                | _ -> ""
            if gridName <> "regular" then
                Error $"{where_}: chunk_grid '{gridName}' is not supported (regular only)"
            else
            let chunks =
                tryProp gridEl "configuration"
                |> Option.bind (fun c -> tryProp c "chunk_shape")
                |> Option.map jsonInt64List
            match chunks with
            | None -> Error $"{where_}: chunk_grid.configuration.chunk_shape missing"
            | Some chunks when chunks.Length <> shape.Length ->
                Error $"{where_}: shape rank {shape.Length} != chunk_shape rank {chunks.Length}"
            | Some chunks when chunks |> List.exists (fun c -> c <= 0L) ->
                Error $"{where_}: non-positive chunk extent"
            | Some chunks ->
            // Codec gate: exactly one `bytes` codec, little-endian.
            let codecErr =
                match tryProp root "codecs" with
                | None -> None  // lenient: absent codecs = raw bytes
                | Some cs when cs.ValueKind = JsonValueKind.Array ->
                    let arr = cs.EnumerateArray() |> List.ofSeq
                    let names =
                        arr |> List.map (fun c ->
                            match tryProp c "name" with
                            | Some n when n.ValueKind = JsonValueKind.String -> n.GetString()
                            | _ -> "<unknown>")
                    match names with
                    | [] -> None
                    | ["bytes"] ->
                        let endian =
                            tryProp arr.[0] "configuration"
                            |> Option.bind (fun cfg -> tryProp cfg "endian")
                            |> Option.map (fun e -> e.GetString())
                            |> Option.defaultValue "little"
                        if endian = "little" then None
                        else Some $"{where_}: big-endian bytes codec is not supported (little-endian stores only)"
                    | _ ->
                        let bad = names |> List.filter (fun n -> n <> "bytes")
                        // Same remedy as the v2 `compressor` arm above, and for
                        // the same reason: zarr-python v3 and icechunk-python
                        // write `[bytes, zstd]` (or blosc) unless told not to,
                        // so this fires on the first real store anyone points
                        // at Blade -- and it has to say what to DO.
                        Some (sprintf "%s uses codec(s) %s -- compressed/transformed Zarr stores are not supported (a single little-endian 'bytes' codec only). Rewrite the array with compression off (zarr-python v3: `compressors=None, filters=None`; icechunk: the same, on the array you commit), or convert the store once with a copy that does. The compiler-side seam for adding a codec is ZarrProvider's ZarrCodec type"
                                  where_ (bad @ (if List.isEmpty bad then names else []) |> List.map (sprintf "'%s'") |> String.concat ", "))
                | Some _ -> Some $"{where_}: malformed codecs"
            match codecErr with
            | Some e -> Error e
            | None ->
            match zarrDtypeV3 (dtypeEl.GetString()) with
            | Error e -> Error $"{where_}: {e}"
            | Ok dt ->
            match parseFill where_ dt.IsFloat (tryProp root "fill_value") with
            | Error e -> Error e
            | Ok fill ->
            let (prefix, sep) =
                match tryProp root "chunk_key_encoding" with
                | None -> ("c", "/")
                | Some cke ->
                    let ename =
                        match tryProp cke "name" with
                        | Some n when n.ValueKind = JsonValueKind.String -> n.GetString()
                        | _ -> "default"
                    let csep dflt =
                        tryProp cke "configuration"
                        |> Option.bind (fun c -> tryProp c "separator")
                        |> Option.map (fun s -> s.GetString())
                        |> Option.defaultValue dflt
                    match ename with
                    | "default" -> ("c", csep "/")
                    | "v2" -> ("", csep ".")
                    | other -> (other, "!")  // marker checked below
            if sep = "!" then
                Error $"{where_}: unsupported chunk_key_encoding '{prefix}'"
            elif sep <> "." && sep <> "/" then
                Error $"{where_}: unsupported chunk-key separator '{sep}'"
            else
            let dimNames =
                match tryProp root "dimension_names" with
                | Some dn when dn.ValueKind = JsonValueKind.Array ->
                    let names = dn.EnumerateArray() |> Seq.map (fun e -> if e.ValueKind = JsonValueKind.String then e.GetString() else "") |> List.ofSeq
                    if names |> List.exists String.IsNullOrEmpty then None else Some names
                | _ -> None
            match parseBladeLayout where_ shape (tryProp root "attributes") with
            | Error e -> Error e
            | Ok blade ->
            Ok { Name = name; ArrayDir = arrayDir; Shape = shape; Chunks = chunks
                 Dtype = dt; DimNames = dimNames; FillValue = fill; Codec = CodecIdentity
                 Blade = blade; Version = 3; ChunkKeySep = sep; ChunkKeyPrefix = prefix }
        | _ -> Error $"array '{name}': zarr.json is missing shape/data_type/chunk_grid"
    with ex ->
        Error $"array '{name}': malformed zarr.json: {ex.Message}"

// Store discovery (the multi-file walk)

let private isValidIdent (s: string) =
    s.Length > 0
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun ch -> Char.IsLetterOrDigit ch || ch = '_')

let private readTextOpt (path: string) : string option =
    if File.Exists path then Some (File.ReadAllText path) else None

let private loadArrayV2 (name: string) (arrayDir: string) : ZarrArrayMeta =
    let zarray = File.ReadAllText (Path.Combine(arrayDir, ".zarray"))
    let zattrs = readTextOpt (Path.Combine(arrayDir, ".zattrs"))
    match parseArrayMetaV2 name arrayDir zarray zattrs with
    | Ok m -> m
    | Error e -> failwith $"Zarr store: {e}"

let private loadArrayV3 (name: string) (arrayDir: string) : ZarrArrayMeta =
    let zj = File.ReadAllText (Path.Combine(arrayDir, "zarr.json"))
    match parseArrayMetaV3 name arrayDir zj with
    | Ok m -> m
    | Error e -> failwith $"Zarr store: {e}"

/// Load all metadata from a Zarr store directory. Recognizes, in order: v3
/// (`zarr.json` group or single array), v2 group (`.zgroup` + array
/// subdirectories), v2 single array (`.zarray`). Array names must be valid Blade identifiers; one level of nesting only.
let load (path: string) : ZarrStore =
    let full = Path.GetFullPath path
    let checkName (n: string) =
        if not (isValidIdent n) then
            failwith $"Zarr store '{path}': array name '{n}' is not a valid identifier (it becomes a struct field)"
    let leafName () =
        let n = Path.GetFileName (full.TrimEnd [| '/' ; '\\' |])
        let n = if n.EndsWith ".zarr" then n.Substring(0, n.Length - 5) else n
        if isValidIdent n then n else "data"
    let v3Path = Path.Combine(full, "zarr.json")
    if File.Exists v3Path then
        use doc = JsonDocument.Parse (File.ReadAllText v3Path)
        let nodeType =
            match doc.RootElement.TryGetProperty "node_type" with
            | true, nt when nt.ValueKind = JsonValueKind.String -> nt.GetString()
            | _ -> ""
        match nodeType with
        | "array" ->
            let name = leafName ()
            { Path = full; Version = 3; Arrays = [ loadArrayV3 name full ] }
        | "group" ->
            let arrays =
                Directory.GetDirectories full
                |> Array.filter (fun d -> File.Exists (Path.Combine(d, "zarr.json")))
                |> Array.choose (fun d ->
                    use adoc = JsonDocument.Parse (File.ReadAllText (Path.Combine(d, "zarr.json")))
                    match adoc.RootElement.TryGetProperty "node_type" with
                    | true, nt when nt.ValueKind = JsonValueKind.String && nt.GetString() = "array" ->
                        let name = Path.GetFileName d
                        checkName name
                        Some (loadArrayV3 name d)
                    | _ -> None)
                |> Array.sortBy _.Name
                |> List.ofArray
            { Path = full; Version = 3; Arrays = arrays }
        | other -> failwith $"Zarr store '{path}': zarr.json node_type '{other}' is neither 'group' nor 'array'"
    elif File.Exists (Path.Combine(full, ".zgroup")) then
        let arrays =
            Directory.GetDirectories full
            |> Array.filter (fun d -> File.Exists (Path.Combine(d, ".zarray")))
            |> Array.map (fun d ->
                let name = Path.GetFileName d
                checkName name
                loadArrayV2 name d)
            |> Array.sortBy _.Name
            |> List.ofArray
        { Path = full; Version = 2; Arrays = arrays }
    elif File.Exists (Path.Combine(full, ".zarray")) then
        let name = leafName ()
        { Path = full; Version = 2; Arrays = [ loadArrayV2 name full ] }
    else
        failwith $"'{path}' is not a Zarr store (no zarr.json, .zgroup, or .zarray found)"

let tryFindArray (store: ZarrStore) (varName: string) : ZarrArrayMeta option =
    store.Arrays |> List.tryFind (fun a -> a.Name = varName)

// Chunk-grid math

/// Chunk key for a grid coordinate: v2 "i.j" (rank-0: "0"); v3 default "c/i/j" (rank-0: "c"); v3 "v2"-encoding like v2.
let chunkKey (meta: ZarrArrayMeta) (coords: int64 list) : string =
    let joined = coords |> List.map string |> String.concat meta.ChunkKeySep
    match meta.ChunkKeyPrefix, coords with
    | "", [] -> "0"
    | "", _ -> joined
    | p, [] -> p
    | p, _ -> p + meta.ChunkKeySep + joined

/// Per-dimension chunk-grid extents (ceil-div; rank-0 -> []).
let gridDims (shape: int64 list) (chunks: int64 list) : int64 list =
    List.map2 (fun s c -> (s + c - 1L) / c) shape chunks

/// All grid coordinates, row-major (rank-0 -> [[]], the single scalar chunk).
let gridCoords (shape: int64 list) (chunks: int64 list) : int64 list list =
    let rec cartesian dims =
        match dims with
        | [] -> [ [] ]
        | d :: rest ->
            [ for i in 0L .. d - 1L do
                for tail in cartesian rest -> i :: tail ]
    cartesian (gridDims shape chunks)

/// Row-major strides for a shape (innermost stride 1).
let rowMajorStrides (lens: int list) : int list =
    let rec go = function
        | [] -> []
        | [_] -> [1]
        | _ :: rest ->
            let tail = go rest
            (List.head tail * List.head rest) :: tail
    go lens

// Compile-time data read (chunk assembly for the static fold + tests)

let private decodeFloatCell (code: string) (b: byte[]) (off: int) : float =
    match code with
    | "f8" -> BitConverter.ToDouble(b, off)
    | "f4" -> float (BitConverter.ToSingle(b, off))
    | c -> failwith $"decodeFloatCell: not a float code '{c}'"

let private decodeIntCell (code: string) (b: byte[]) (off: int) : int64 =
    match code with
    | "i8" -> BitConverter.ToInt64(b, off)
    | "i4" -> int64 (BitConverter.ToInt32(b, off))
    | "i2" -> int64 (BitConverter.ToInt16(b, off))
    | "i1" -> int64 (sbyte b.[off])
    | "u1" -> int64 b.[off]
    | "u2" -> int64 (BitConverter.ToUInt16(b, off))
    | "u4" -> int64 (BitConverter.ToUInt32(b, off))
    | "u8" -> int64 (BitConverter.ToUInt64(b, off))
    | c -> failwith $"decodeIntCell: not an integer code '{c}'"

/// The chunk-source seam. Everything ABOVE it -- the padded-chunk size check,
/// fill handling, edge intersection, the row-major scatter, packed-pool
/// reassembly -- is shared; below it sits the one store-shaped act of turning
/// a chunk-grid coordinate into bytes. `Fetch` answers None for a coordinate
/// the source does not store (Zarr: no chunk file), which is what becomes fill
/// semantics above; `Label` names the chunk in a diagnostic. A second source
/// (an Icechunk manifest's inline bytes / (file, offset, length) table) plugs
/// in here and inherits the assembly core verbatim.
type ChunkSource = {
    Label: int64 list -> string
    Fetch: int64 list -> byte[] option
}

/// Zarr's chunk source: one file per chunk key under the array directory, through the codec seam.
let zarrChunkSource (meta: ZarrArrayMeta) : ChunkSource =
    { Label = chunkKey meta
      Fetch = fun coords ->
        let file = Path.Combine(meta.ArrayDir, (chunkKey meta coords).Replace('/', Path.DirectorySeparatorChar))
        if File.Exists file then Some (decodeChunk meta.Codec (File.ReadAllBytes file))
        else None }

/// Read an array's full payload by assembling its chunks out of `src`. Absent chunks fill with fill_value (loud error when null); chunk bytes must be exactly full-chunk-sized (edge chunks are stored padded).
let readArrayDataFrom (src: ChunkSource) (meta: ZarrArrayMeta) : Result<ZarrVarData, string> =
    try
        let shape = meta.Shape |> List.map int
        let chunks = meta.Chunks |> List.map int
        let rank = shape.Length
        let total = shape |> List.fold (*) 1
        let chunkCount = chunks |> List.fold (*) 1
        let bs = meta.Dtype.ByteSize
        let gStr = rowMajorStrides shape
        let cStr = rowMajorStrides chunks
        let shapeArr = List.toArray shape
        let chunksArr = List.toArray chunks
        let gStrArr = List.toArray gStr
        let cStrArr = List.toArray cStr
        let outF = if meta.Dtype.IsFloat then Array.zeroCreate<float> (max total 1) else [||]
        let outI = if meta.Dtype.IsFloat then [||] else Array.zeroCreate<int64> (max total 1)
        for coords in gridCoords meta.Shape meta.Chunks do
            let coordsArr = coords |> List.map int |> List.toArray
            let chunkBytes =
                match src.Fetch coords with
                | Some raw ->
                    if raw.Length <> chunkCount * bs then
                        failwith $"chunk '{src.Label coords}' of array '{meta.Name}' is {raw.Length} bytes, expected {(chunkCount * bs)} -- a compressed or corrupt store?"
                    Some raw
                | None ->
                    match meta.FillValue with
                    | FillNone ->
                        failwith $"chunk '{src.Label coords}' of array '{meta.Name}' is missing and fill_value is null -- refusing to invent data"
                    | _ -> None
            // Copy the chunk's intersection with the array bounds (edge
            // chunks are stored full-size; the overhang is ignored).
            let rec copy (d: int) (gBase: int) (cBase: int) =
                if d = rank then
                    match chunkBytes with
                    | Some raw ->
                        if meta.Dtype.IsFloat then outF.[gBase] <- decodeFloatCell meta.Dtype.Code raw (cBase * bs)
                        else outI.[gBase] <- decodeIntCell meta.Dtype.Code raw (cBase * bs)
                    | None ->
                        (match meta.FillValue with
                         | FillFloat f when meta.Dtype.IsFloat -> outF.[gBase] <- f
                         | FillInt n when not meta.Dtype.IsFloat -> outI.[gBase] <- n
                         | FillFloat f -> outI.[gBase] <- int64 f
                         | FillInt n -> outF.[gBase] <- float n
                         | FillNone -> ())
                else
                    let basePos = coordsArr.[d] * chunksArr.[d]
                    let lim = min chunksArr.[d] (shapeArr.[d] - basePos)
                    for l in 0 .. lim - 1 do
                        copy (d + 1) (gBase + (basePos + l) * gStrArr.[d]) (cBase + l * cStrArr.[d])
            if total > 0 then copy 0 0 0
        Ok { DimLengths = shape
             Payload = if meta.Dtype.IsFloat then ZFloats outF else ZInts outI }
    with ex ->
        Error ex.Message

/// Read an array's full payload out of its Zarr store directory.
let readArrayData (meta: ZarrArrayMeta) : Result<ZarrVarData, string> =
    readArrayDataFrom (zarrChunkSource meta) meta

/// Read a variable's full payload at compile time (provider contract).
let readVarData (path: string) (varName: string) : Result<ZarrVarData, string> =
    try
        let store = load path
        match tryFindArray store varName with
        | None ->
            Error ($"""variable '{varName}' not found in Zarr store '{path}' (arrays: {(store.Arrays |> List.map _.Name |> String.concat ", ")})""")
        | Some meta -> readArrayData meta
    with ex ->
        Error ex.Message

/// Canonical pool of a packed variable regardless of physical layout:
/// "packed" reads the pool directly; "packed-blocks" reassembles it from the
/// padded block rows via the shared cell map. Ground truth for tests and the
/// differential gate between the two layouts -- and, like the dense read, blind to where the chunks came from.
let readPackedPoolFrom (src: ChunkSource) (meta: ZarrArrayMeta) : Result<ZarrVarData, string> =
    match meta.Blade with
    | None -> Error $"variable '{meta.Name}' has no blade packed layout"
    | Some layout ->
        match layout.Blocks with
        | None -> readArrayDataFrom src meta
        | Some info ->
            match readArrayDataFrom src meta with
            | Error e -> Error e
            | Ok phys ->
                let g = layout.Group
                let strict = (g.Sym = SymAntisymmetric)
                let card = int (packedCardinality g)
                let trail = layout.DenseDims |> List.fold (fun a d -> a * int d) 1
                let cellMap = SimplexBlocks.blocksCellMap strict g.Extent info.Tile g.Rank info.Order
                let dims = card :: (layout.DenseDims |> List.map int)
                let remap (src: 'a[]) (zero: 'a) : 'a[] =
                    let out = Array.create (max (card * trail) 1) zero
                    for p in 0 .. cellMap.Length - 1 do
                        let pool = cellMap.[p]
                        if pool >= 0 then
                            for t in 0 .. trail - 1 do
                                out.[pool * trail + t] <- src.[p * trail + t]
                    out
                match phys.Payload with
                | ZFloats xs -> Ok { DimLengths = dims; Payload = ZFloats (remap xs 0.0) }
                | ZInts xs -> Ok { DimLengths = dims; Payload = ZInts (remap xs 0L) }

/// Canonical pool of a packed variable stored as a Zarr directory.
let readPackedPool (meta: ZarrArrayMeta) : Result<ZarrVarData, string> =
    readPackedPoolFrom (zarrChunkSource meta) meta

// Mapping to Blade IR types (mirrors NetcdfProvider.ncFileToModule)

/// Build a named IRIndexType for a dimension (name, extent).
let zarrDimToNamedIndexType (builder: IRBuilder) (name: string, length: int64) : string * IRIndexType =
    let idx = {
        Id = builder.FreshId()
        Rank = 1
        Extent = IRLit (IRLitInt length)
        Symmetry = SymNone
        Tag = None; IxKind = IxKPlain
        Kind = SDimension
        Dependencies = []
    }
    (name, idx)

/// Resolved dimension names for an array: DimNames when present (count must match rank), synthesized "<var>_dim<i>" otherwise.
let private resolvedDimNames (a: ZarrArrayMeta) : string list =
    match a.DimNames with
    | Some ns when ns.Length = a.Shape.Length -> ns
    | Some ns ->
        failwith $"Zarr array '{a.Name}': {ns.Length} dimension names for rank {a.Shape.Length}"
    | None -> a.Shape |> List.mapi (fun i _ -> $"{a.Name}_dim{i}")

/// The chunk edge of every dimension the store chunks UNIFORMLY across its
/// dense variables (docs/plans/structural/07 §3.1): what `Chunked<s.index.d,
/// store>` inherits. A dimension two variables chunk differently is absent
/// (the declaration then refuses with a steer rather than guessing), as is a
/// blade-packed variable's pool dimension.
let dimChunkEdges (storePath: string) : Map<string, int64> =
    let store = load storePath
    let seen = System.Collections.Generic.Dictionary<string, int64 option>()
    for a in store.Arrays do
        if a.Blade.IsNone && a.Chunks.Length = a.Shape.Length then
            for (dn, edge) in List.zip (resolvedDimNames a) a.Chunks do
                match seen.TryGetValue dn with
                | true, Some prev when prev <> edge -> seen.[dn] <- None
                | true, _ -> ()
                | _ -> seen.[dn] <- Some edge
    seen |> Seq.choose (fun kv -> kv.Value |> Option.map (fun e -> (kv.Key, e))) |> Map.ofSeq

/// Converts a ZarrStore into an IRModule using structs for dims/vars, the
/// same shape ncFileToModule produces:
///
///   type x = Idx<20>          (named index types, one per dimension)
///   struct sample__dims = { x: Array<Float64, Idx<x>> }   (coordinate arrays)
///   struct sample__vars = { A: Array<Float32, Idx<x>, ...> }
///
/// The structs are namespaced by `moduleName` rather than the bare
/// "dims"/"vars": registerProviderModule keeps one flat TypeDefs map, so
/// literal names would let a second load silently overwrite the first;
/// `fieldFor` resolves the suffix.
///
/// Coordinate arrays (1-D, named after their dimension) go in dims; unlike
/// NetCDF (which hardcodes Int64 coordinates), a Zarr coordinate array's
/// ACTUAL element type is used when it exists. Dimension names come from
/// xarray's _ARRAY_DIMENSIONS (v2) / dimension_names (v3); unnamed arrays
/// get synthesized dimensions. Same-named dimensions must agree on extent.
///
/// `poolAxis` is the PACKED-POOL twin of `externalDimMap` (icechunk plan §5.3).
/// A packed variable's pool dimension is deliberately NOT in `sharedDims` -- its
/// extent is a derived cardinality, not a store dimension -- so `externalDimMap`
/// can never reach it, and the pool record below is minted fresh and untagged.
/// A caller that has provenance for the pool (the icechunk provider, which
/// knows the repo and the variable's content fingerprint) supplies a hook
/// `varName -> mintedRecord -> resolvedRecord`; `None` (every plain Zarr call
/// site) leaves the record exactly as it was, so a Zarr store's emitted module
/// and types are byte-for-byte what they have always been.
let zarrStoreToModuleWith
    (builder: IRBuilder)
    (moduleName: string)
    (store: ZarrStore)
    (externalDimMap: Map<string, IRIndexType> option)
    (poolAxis: (string -> IRIndexType -> IRIndexType) option)
    : IRModule =

    // Dimension universe (first-seen order), extent-consistent. A blade-packed
    // array's POOL dimension is not shareable (its extent is a derived
    // cardinality, typed per-var below), so only trailing dense dims join it.
    let sharedDims (a: ZarrArrayMeta) : (string * int64) list =
        let all = List.zip (resolvedDimNames a) a.Shape
        match a.Blade with
        | Some l when l.Blocks.IsSome -> all |> List.skip 2  // [blockCount, tile^rank] physical dims
        | Some _ -> List.tail all                            // [cardinality] pool dim
        | None -> all
    let dimOrder = ResizeArray<string>()
    let dimExtents = System.Collections.Generic.Dictionary<string, int64>()
    for a in store.Arrays do
        for (dn, ext) in sharedDims a do
            if not (isValidIdent dn) then
                failwith $"Zarr array '{a.Name}': dimension name '{dn}' is not a valid identifier"
            match dimExtents.TryGetValue dn with
            | true, prev when prev <> ext ->
                failwith $"Zarr store '{store.Path}': dimension '{dn}' has conflicting extents {prev} and {ext} across arrays"
            | true, _ -> ()
            | _ ->
                dimExtents.[dn] <- ext
                dimOrder.Add dn

    // Named index types.
    let (indexTypeDefs, dimMap) =
        match externalDimMap with
        | Some dm -> ([], dm)
        | None ->
            let pairs =
                dimOrder |> List.ofSeq
                |> List.map (fun dn -> zarrDimToNamedIndexType builder (dn, dimExtents.[dn]))
            let typeDefs = pairs |> List.map (fun (name, idx) -> IRTDIndexType(name, idx))
            (typeDefs, Map.ofList pairs)

    let isCoordinateArr (a: ZarrArrayMeta) =
        a.Blade.IsNone && a.Shape.Length = 1 && resolvedDimNames a = [a.Name]

    let coordElem (dn: string) : ElemType =
        store.Arrays
        |> List.tryFind (fun a -> a.Name = dn && isCoordinateArr a)
        |> Option.map _.Dtype.Elem
        |> Option.defaultValue ETInt64

    // dims struct: one coordinate array per dimension.
    let dimsFields =
        dimOrder |> List.ofSeq |> List.map (fun dn ->
            let idx = dimMap.[dn]
            let arrType = mkArrayArrow [idx] (IRTScalar (coordElem dn)) (Some (AIDVariable dn))
            (dn, arrType))
    let dimsStruct = IRTDStruct($"{moduleName}__dims", dimsFields)

    // vars struct: data arrays (coordinate arrays excluded). A blade-packed
    // array types with its packed group as the LEADING index type, the exact
    // record shape source-level SymIdx/AntisymIdx lowering produces.
    let varsFields =
        store.Arrays
        |> List.filter (not << isCoordinateArr)
        |> List.map (fun a ->
            let trailingIdx =
                sharedDims a
                |> List.map (fun (dn, _) ->
                    match Map.tryFind dn dimMap with
                    | Some idx -> idx
                    | None -> failwith $"Zarr array '{a.Name}': dimension '{dn}' not found in module dim map")
            let indexTypes =
                match a.Blade with
                | Some layout ->
                    let g = layout.Group
                    let packedIdx =
                        if g.Sym = SymWreath then
                            // Routed through the SAME normalization + constructor
                            // (IR.orbitNormalForm -> IR.mkWreathIndexRecord) the
                            // surface type and deduction use, so a loaded and a
                            // deduced class are the SAME record. The other normal
                            // forms below are backstops (depth <= 1 was refused at parse).
                            match orbitNormalForm g.Levels with
                            | OrbNfWreath ls ->
                                mkWreathIndexRecord (builder.FreshId()) ls (IRLit (IRLitInt g.Extent))
                            | _ ->
                                failwith $"Zarr array '{a.Name}': orbit head {(ppOrbitLevels g.Levels)} normalizes to depth <= 1, which parseBladeLayout rejects -- a depth-1 class is stored as kind 'sym'/'antisym'"
                        else
                            { Id = builder.FreshId()
                              Rank = g.Rank
                              Extent = IRLit (IRLitInt g.Extent)
                              Symmetry = g.Sym
                              Tag = None; IxKind = IxKPlain
                              Kind = SDimension
                              Dependencies = [] }
                    // The pool's PROVENANCE hook. Absent for a plain Zarr store
                    // (the record stays exactly as minted above); the icechunk
                    // provider uses it to stamp a repo-scoped identity so two
                    // repos' pools do not co-iterate.
                    let packedIdx =
                        match poolAxis with
                        | Some resolve -> resolve a.Name packedIdx
                        | None -> packedIdx
                    packedIdx :: trailingIdx
                | None -> trailingIdx
            let arrType = {
                ElemType = IRTScalar a.Dtype.Elem
                IndexTypes = indexTypes
                IsVirtual = false
                Identity = Some (AIDVariable a.Name)
            }
            (a.Name, mkArrayLike arrType))
    let varsStruct = IRTDStruct($"{moduleName}__vars", varsFields)

    {
        Name = moduleName
        Types = indexTypeDefs @ [dimsStruct; varsStruct]
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

/// The historical four-argument spelling: no pool-provenance hook, which is
/// what every plain Zarr call site wants. Kept as the name rather than adding a
/// default argument so no existing caller changes and no emitted Zarr module or
/// type moves.
let zarrStoreToModule
    (builder: IRBuilder)
    (moduleName: string)
    (store: ZarrStore)
    (externalDimMap: Map<string, IRIndexType> option)
    : IRModule =
    zarrStoreToModuleWith builder moduleName store externalDimMap None

/// Convenience: load a store and produce a module in one step (contract).
let loadAsModule (builder: IRBuilder) (moduleName: string) (path: string) : IRModule =
    let store = load path
    zarrStoreToModule builder moduleName store None

// Store fingerprint / version stamp (multi-file provenance)

/// Every file under the store, sorted by relative path (deterministic).
let private storeFiles (root: string) : (string * string) list =
    if Directory.Exists root then
        Directory.GetFiles(root, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> (Path.GetRelativePath(root, f).Replace('\\', '/'), f))
        |> Array.sortBy fst
        |> List.ofArray
    else []

/// SHA256 over (relative path, contents) of every file in the store -- metadata AND chunks, so provenance pins the actual payload.
let storeFingerprint (root: string) : string =
    use sha = System.Security.Cryptography.SHA256.Create()
    for (rel, file) in storeFiles root do
        let relBytes = Text.Encoding.UTF8.GetBytes(rel + "\n")
        sha.TransformBlock(relBytes, 0, relBytes.Length, null, 0) |> ignore
        let content = File.ReadAllBytes file
        sha.TransformBlock(content, 0, content.Length, null, 0) |> ignore
    sha.TransformFinalBlock([||], 0, 0) |> ignore
    sha.Hash |> Array.map (sprintf "%02x") |> String.concat ""

/// Max mtime over the store's files (fold-memo change stamp).
let storeVersionStamp (root: string) : int64 =
    try
        storeFiles root
        |> List.map (fun (_, f) -> File.GetLastWriteTimeUtc(f).Ticks)
        |> function [] -> 0L | ts -> List.max ts
    with _ -> 0L

// C++ code generation (pure std C++17: <fstream>, <filesystem>)

module CppZarr =

    let private elemCppOf (t: IRType) : string =
        match t with
        | IRTScalar ETFloat32 -> "float"
        | IRTScalar ETFloat64 -> "double"
        | IRTScalar ETInt32 -> "int"
        | IRTScalar ETInt64 -> "long long"
        | _ -> "double"

    /// On-disk C++ type per normalized dtype code.
    let private diskCppOf (code: string) : string =
        match code with
        | "f8" -> "double"
        | "f4" -> "float"
        | "i8" -> "int64_t"
        | "i4" -> "int32_t"
        | "i2" -> "int16_t"
        | "i1" -> "int8_t"
        | "u1" -> "uint8_t"
        | "u2" -> "uint16_t"
        | "u4" -> "uint32_t"
        | "u8" -> "uint64_t"
        | c -> failwith $"diskCppOf: unknown dtype code '{c}'"

    /// v2 dtype string for a Blade elem type (the write format).
    let private v2DtypeOf (t: IRType) : string =
        match t with
        | IRTScalar ETFloat32 -> "<f4"
        | IRTScalar ETFloat64 -> "<f8"
        | IRTScalar ETInt32 -> "<i4"
        | IRTScalar ETInt64 -> "<i8"
        | _ -> "<f8"

    let private normPath (p: string) : string = p.Replace('\\', '/')

    let private fmtF (f: float) : string =
        if Double.IsNaN f then "std::numeric_limits<double>::quiet_NaN()"
        elif Double.IsPositiveInfinity f then "std::numeric_limits<double>::infinity()"
        elif Double.IsNegativeInfinity f then "(-std::numeric_limits<double>::infinity())"
        else f.ToString("R", Globalization.CultureInfo.InvariantCulture)

    /// A loud runtime failure (the ncChecked discipline: never exit 0 with an uninitialized buffer).
    let private zExit (message: string) : string =
        $"{{ std::cerr << \"Zarr error: {message}\" << std::endl; std::exit(1); }}"

    /// The codegen twin of `ChunkSource`: how the EMITTED C++ gets one chunk's
    /// bytes into `<v>_cbuf`. `Prologue` is emitted once, ahead of the buffers
    /// (Zarr's store-existence probe; a baked chunk table for a source that has
    /// one); `Locate`, `Present`, and `Read` shape the per-chunk acquisition
    /// inside the grid loops, whose counters `<v>_c0 .. <v>_c{rank-1}` ARE the
    /// chunk coordinate; `Ident` is the C++ expression naming the chunk in a
    /// diagnostic. Each line-producing field is indented by its caller. The core
    /// keeps everything else -- grid loops, edge intersection, the fill branch,
    /// the flat scatter -- so the two sources cannot drift.
    type ChunkFetch = {
        Prologue: string list
        Locate: string -> string list
        Present: string
        Read: string -> string list
        Ident: string
    }

    /// A chunk fetch instantiated for one emitted variable: the C++ name prefix and the exact byte size of a (padded) chunk.
    type ChunkFetchEmitter = string -> int -> ChunkFetch

    /// Zarr's chunk fetch: the chunk-key string built from the grid counters, opened as one file under the array directory.
    let zarrChunkFetch (storePath: string) (store: ZarrStore) (meta: ZarrArrayMeta) : ChunkFetchEmitter =
        fun v chunkBytes ->
            let varName = meta.Name
            let rank = meta.Shape.Length
            // Bake the path AS GIVEN (netcdf parity): a relative store path
            // resolves against the executable's working directory at runtime,
            // not against wherever the compiler happened to run.
            let arrayDir =
                let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
                normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
            let metaFile = if meta.Version = 3 then "zarr.json" else ".zarray"
            // Chunk key expression from the loop counters, e.g.
            // std::to_string(c0) + "." + std::to_string(c1)  /  "c" "/" ...
            let keyExpr =
                let coordParts = [ for d in 0 .. rank - 1 -> $"std::to_string({v}_c{d})" ]
                let sepLit = $"\"{meta.ChunkKeySep}\""
                let joined = String.concat $" + {sepLit} + " coordParts
                if meta.ChunkKeyPrefix = "" then joined
                else $"std::string(\"{meta.ChunkKeyPrefix}\") + {sepLit} + {joined}"
            { Prologue =
                [ $"// Read {varName} from zarr store {normPath storePath} (v{meta.Version}, uncompressed)"
                  sprintf "{ std::ifstream %s_zm(\"%s/%s\"); if (!%s_zm) %s }"
                      v arrayDir metaFile v
                      (zExit $"array '{varName}' not found in store '{normPath storePath}' (missing {metaFile})") ]
              Locate = fun ind ->
                [ ind + $"std::string {v}_key = {keyExpr};"
                  ind + $"std::ifstream {v}_cf(std::string(\"{arrayDir}/\") + {v}_key, std::ios::binary);" ]
              Present = $"{v}_cf"
              Read = fun ind ->
                [ ind + $"{v}_cf.read((char*){v}_cbuf, {chunkBytes});"
                  ind + $"if ({v}_cf.gcount() != (std::streamsize){chunkBytes}) {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{varName}' is short (expected {chunkBytes} bytes) -- a compressed or corrupt store?\" << std::endl; std::exit(1); }}" ]
              Ident = $"{v}_key" }

    /// The chunk-assembly core shared by the dense and packed readers: emits
    /// C++ assembling the (physical, dense) on-disk array into a flat
    /// row-major buffer `<cppVarName>_flat` of type `elemCpp`, one chunk at a
    /// time out of `fetch`. All metadata is baked at compile time -- the
    /// generated program parses no JSON. Absent chunks fill with fill_value
    /// (or fail loudly when null). Caller owns (and must delete[]) `<cppVarName>_flat`.
    let genAssembleFlatVia (fetch: ChunkFetchEmitter) (meta: ZarrArrayMeta) (cppVarName: string) (elemCpp: string) : string list =
        let v = cppVarName
        let varName = meta.Name
        let rank = meta.Shape.Length
        if rank = 0 then
            failwith $"Zarr codegen: rank-0 variable '{varName}' has no runtime read (bind it with `let static` instead)"
        let diskCpp = diskCppOf meta.Dtype.Code
        let shape = meta.Shape |> List.map int
        let chunks = meta.Chunks |> List.map int
        let grid = gridDims meta.Shape meta.Chunks |> List.map int
        let gStr = rowMajorStrides shape
        let cStr = rowMajorStrides chunks
        let total = shape |> List.fold (*) 1
        let chunkCount = chunks |> List.fold (*) 1
        let chunkBytes = chunkCount * meta.Dtype.ByteSize
        let src = fetch v chunkBytes

        let fillDecl =
            match meta.FillValue with
            | FillFloat f -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fmtF f};" ]
            | FillInt n -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){n}LL;" ]
            | FillNone -> []

        let header =
            src.Prologue
            @ [ $"{elemCpp}* {v}_flat = new {elemCpp}[{total}];"
                $"{diskCpp}* {v}_cbuf = new {diskCpp}[{chunkCount}];" ]
            @ fillDecl

        // Grid loops.
        let gridLoops =
            [ for d in 0 .. rank - 1 ->
                let ind = String.replicate d "    "
                $"{ind}for (size_t {v}_c{d} = 0; {v}_c{d} < {grid.[d]}; {v}_c{d}++) {{" ]
        let gInd = String.replicate rank "    "

        // In-bounds limits per dim (edge chunks are stored padded; copy the
        // intersection only).
        let limDecls =
            [ for d in 0 .. rank - 1 do
                yield $"{gInd}size_t {v}_lim{d} = {shape.[d]} - {v}_c{d} * {chunks.[d]};"
                yield $"{gInd}if ({v}_lim{d} > {chunks.[d]}) {v}_lim{d} = {chunks.[d]};" ]

        // Copy loops (shared shape by both branches): global index from
        // (chunkCoord*chunk + local) with row-major strides.
        let copyLoops (assign: string) =
            [ for d in 0 .. rank - 1 ->
                let ind = gInd + String.replicate (d + 1) "    "
                $"{ind}for (size_t {v}_l{d} = 0; {v}_l{d} < {v}_lim{d}; {v}_l{d}++) {{" ]
            @ [ gInd + String.replicate (rank + 1) "    " + assign ]
            @ [ for d in rank - 1 .. -1 .. 0 -> gInd + String.replicate (d + 1) "    " + "}" ]
        let gIdx =
            [ for d in 0 .. rank - 1 -> $"({v}_c{d} * {chunks.[d]} + {v}_l{d}) * {gStr.[d]}" ]
            |> String.concat " + "
        let cIdx =
            [ for d in 0 .. rank - 1 -> $"{v}_l{d} * {cStr.[d]}" ]
            |> String.concat " + "

        let presentBranch =
            [ gInd + $"if ({src.Present}) {{" ]
            @ src.Read (gInd + "    ")
            @ (copyLoops $"{v}_flat[{gIdx}] = ({elemCpp}){v}_cbuf[{cIdx}];")
        let missingBranch =
            match meta.FillValue with
            | FillNone ->
                [ gInd + "} else {"
                  gInd + $"    std::cerr << \"Zarr error: chunk '\" << {src.Ident} << \"' of '{varName}' is missing and fill_value is null\" << std::endl; std::exit(1);"
                  gInd + "}" ]
            | _ ->
                [ gInd + "} else {" ]
                @ (copyLoops $"{v}_flat[{gIdx}] = {v}_fillv;" |> List.map (fun s -> "    " + s))
                @ [ gInd + "}" ]

        let chunkBody =
            limDecls
            @ src.Locate gInd
            @ presentBranch
            @ missingBranch

        let gridClose = [ for d in rank - 1 .. -1 .. 0 -> String.replicate d "    " + "}" ]

        header
        @ gridLoops
        @ chunkBody
        @ gridClose
        @ [ $"delete[] {v}_cbuf;" ]

    /// The assembly core over a Zarr store directory (its file-per-chunk-key fetch).
    let private genAssembleFlat (storePath: string) (store: ZarrStore) (meta: ZarrArrayMeta) (cppVarName: string) (elemCpp: string) : string list =
        genAssembleFlatVia (zarrChunkFetch storePath store meta) meta cppVarName elemCpp

    /// Generates C++ to read a DENSE variable: chunk assembly into
    /// `<v>_flat`, then the same materialization as CppNetcdf.genReadVar
    /// (nested Array via allocate<>, flat->nested copy, buffers released).
    let genReadVar (storePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        if meta.Blade.IsSome then
            failwith $"Zarr codegen: variable '{varName}' is blade-packed; the dense reader cannot materialize it (this indicates a typing inconsistency)"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let shape = meta.Shape |> List.map int
        let rank = shape.Length
        let assemble = genAssembleFlat storePath store meta v elemCpp

        let extentDecls =
            shape |> List.mapi (fun i n -> $"size_t {v}_extent_{i} = {n};")
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

    /// Blocks-layout pool assembly with PER-BLOCK chunk I/O: for each block
    /// (tile multiset), optionally skip it (window / MPI ownership), read ITS
    /// chunk file only, and scatter its cells into the canonical pool
    /// (branch-free intra-block bounds, pool index via linearize). Antisym
    /// blocks with a repeated narrow tile iterate zero cells (empty-diagonal-block).
    /// distribute: rank-scoped I/O, each rank handling only blocks whose pool
    /// range intersects its balanced flat-cell range, then MPI_Allgatherv
    /// restores the full pool on all ranks. windowSkip: blocks with any tile interval disjoint from [wlo, whi) are skipped entirely.
    let private genAssemblePackedBlocks
            (storePath: string) (store: ZarrStore) (meta: ZarrArrayMeta)
            (layout: BladeLayout) (info: SimplexBlocksInfo)
            (v: string) (elemCpp: string)
            (distribute: bool) (windowSkip: (int64 * int64) option) : string list =
        let g = layout.Group
        let strict = (g.Sym = SymAntisymmetric)
        let sInc = if strict then 1 else 0
        let r = g.Rank
        let n = g.Extent
        let T = info.Grid
        let B = info.Tile
        let nBlocks = SimplexBlocks.blockCount r T
        let rowW = SimplexBlocks.maxBlockCells r B
        let card = packedCardinality g
        let trailExts = layout.DenseDims
        let trail = trailExts |> List.fold (fun a d -> a * d) 1L
        let nsName = if strict then "antisymmetric" else "symmetric"
        let diskCpp = diskCppOf meta.Dtype.Code
        let chunkCells = rowW * trail
        let chunkBytes = chunkCells * int64 meta.Dtype.ByteSize
        let arrayDir =
            let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
            normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
        let metaFile = if meta.Version = 3 then "zarr.json" else ".zarray"
        let tableDecl =
            match info.Order with
            | OrderLex -> []
            | OrderPath ->
                if nBlocks > 1_000_000L then
                    failwith $"Zarr codegen: variable '{meta.Name}': path-order block table would need {nBlocks} entries -- beyond the emission cap"
                let rows = SimplexBlocks.pathRows r T
                [ $"""static const size_t {v}_sbrow[{rows.Length}] = {{ {(rows |> Array.map string |> String.concat ", ")} }};""" ]
        let rowExpr =
            match info.Order with
            | OrderLex -> $"{v}_sb"
            | OrderPath -> $"{v}_sbrow[{v}_sb]"
        // Runtime chunk key for physical coords (row, 0, 0...): the block
        // row is the only varying chunk coordinate.
        let keyExpr =
            let sep = meta.ChunkKeySep
            let zeros = List.replicate (1 + trailExts.Length) "0" |> String.concat sep
            let core = $"std::to_string({v}_row) + \"{sep}{zeros}\""
            if meta.ChunkKeyPrefix = "" then core
            else $"std::string(\"{meta.ChunkKeyPrefix}{sep}\") + {core}"

        let fillDecl =
            match meta.FillValue with
            | FillFloat f -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fmtF f};" ]
            | FillInt fi -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fi}LL;" ]
            | FillNone -> []

        // Distribution: balanced flat-cell range [dlo, dhi) per rank (same
        // q/rem split as the MPI compute decomposition).
        let distDecls =
            if not distribute then [] else
            [ $"// zarr mpi: distributed simplex-blocks read of '{meta.Name}' (rank-scoped chunk I/O + Allgatherv)"
              $"size_t {v}_dq = {card}UL / (size_t)__blade_mpi_size;"
              sprintf "size_t %s_dr = %dUL %% (size_t)__blade_mpi_size;" v card
              $"size_t {v}_dlo = (size_t)__blade_mpi_rank * {v}_dq + ((size_t)__blade_mpi_rank < {v}_dr ? (size_t)__blade_mpi_rank : {v}_dr);"
              $"size_t {v}_dhi = {v}_dlo + {v}_dq + ((size_t)__blade_mpi_rank < {v}_dr ? 1 : 0);" ]

        // Window skip: any tile interval disjoint from [wlo, whi) means no
        // block cell can be fully inside the window.
        let windowSkipLines =
            match windowSkip with
            | None -> []
            | Some (wlo, whi) ->
                [ for k in 0 .. r - 1 ->
                    $"    if ({v}_sbt[{k}] * {B} >= {whi}UL || ({v}_sbt[{k}] + 1) * {B} <= {wlo}UL) continue;" ]

        // Ownership skip (distribute): exact block pool range via greedy
        // first/last cells (within-block enumeration is pool-monotone).
        let ownershipSkipLines =
            if not distribute then [] else
            let fwd =
                [ for k in 0 .. r - 1 do
                    if k = 0 then
                        yield $"    {{ size_t lo = {v}_sbt[0] * {B}; size_t hi = ({v}_sbt[0] + 1) * {B}; if (hi > {n}UL) hi = {n}UL; {v}_fc[0] = lo; if (lo >= hi) {v}_emptyb = true; }}"
                    else
                        yield $"    {{ size_t lo = {v}_sbt[{k}] * {B}; size_t p = {v}_fc[{k - 1}] + {sInc}; if (p > lo) lo = p; size_t hi = ({v}_sbt[{k}] + 1) * {B}; if (hi > {n}UL) hi = {n}UL; {v}_fc[{k}] = lo; if (lo >= hi) {v}_emptyb = true; }}" ]
            let bwd =
                [ for k in r - 2 .. -1 .. 0 ->
                    if strict then
                        $"    {{ size_t cap; if ({v}_lc[{k + 1}] < 1) {{ {v}_emptyb = true; cap = 0; }} else cap = {v}_lc[{k + 1}] - 1; size_t hi = ({v}_sbt[{k}] + 1) * {B}; if (hi > {n}UL) hi = {n}UL; size_t c = hi - 1; if (cap < c) c = cap; {v}_lc[{k}] = c; if (c < {v}_sbt[{k}] * {B}) {v}_emptyb = true; }}"
                    else
                        $"    {{ size_t cap = {v}_lc[{k + 1}]; size_t hi = ({v}_sbt[{k}] + 1) * {B}; if (hi > {n}UL) hi = {n}UL; size_t c = hi - 1; if (cap < c) c = cap; {v}_lc[{k}] = c; if (c < {v}_sbt[{k}] * {B}) {v}_emptyb = true; }}" ]
            [ $"    std::array<size_t, {r}> {v}_fc; std::array<size_t, {r}> {v}_lc;"
              $"    bool {v}_emptyb = false;" ]
            @ fwd
            @ [ $"    {{ size_t hi = ({v}_sbt[{r - 1}] + 1) * {B}; if (hi > {n}UL) hi = {n}UL; {v}_lc[{r - 1}] = hi - 1; }}" ]
            @ bwd
            @ [ $"    if ({v}_emptyb) continue;"
                $"    if (linearized_storage::{nsName}::linearize<{r}>({v}_lc, {n}UL) < {v}_dlo || linearized_storage::{nsName}::linearize<{r}>({v}_fc, {n}UL) >= {v}_dhi) continue;" ]

        // Per-block chunk read (missing chunk -> fill_value / loud on null).
        let chunkRead =
            [ $"    size_t {v}_row = {rowExpr};"
              $"    std::string {v}_key = {keyExpr};"
              $"    std::ifstream {v}_cf(std::string(\"{arrayDir}/\") + {v}_key, std::ios::binary);"
              $"    bool {v}_have = (bool){v}_cf;" ]
            @ (match meta.FillValue with
               | FillNone ->
                   [ $"    if (!{v}_have) {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{meta.Name}' is missing and fill_value is null\" << std::endl; std::exit(1); }}" ]
               | _ -> [])
            @ [ $"    if ({v}_have) {{"
                $"        {v}_cf.read((char*){v}_cbuf, {chunkBytes});"
                $"        if ({v}_cf.gcount() != (std::streamsize){chunkBytes}) {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{meta.Name}' is short (expected {chunkBytes} bytes) -- a compressed or corrupt store?\" << std::endl; std::exit(1); }}"
                "    }" ]

        // Branch-free intra-block bounds per level.
        let cellLoops =
            [ for k in 0 .. r - 1 do
                let ind = String.replicate (k + 1) "    "
                yield $"{ind}size_t {v}_lo{k} = {v}_sbt[{k}] * {B};"
                if k > 0 then
                    yield $"{ind}{{ size_t {v}_p = {v}_i{(k - 1)} + {sInc}; if ({v}_p > {v}_lo{k}) {v}_lo{k} = {v}_p; }}"
                yield $"{ind}size_t {v}_hi{k} = ({v}_sbt[{k}] + 1) * {B}; if ({v}_hi{k} > {n}) {v}_hi{k} = {n};"
                yield $"{ind}for (size_t {v}_i{k} = {v}_lo{k}; {v}_i{k} < {v}_hi{k}; {v}_i{k}++) {{" ]
        let bodyInd = String.replicate (r + 1) "    "
        let coordsInit = [ for k in 0 .. r - 1 -> $"{v}_i{k}" ] |> String.concat ", "
        let trailVars = trailExts |> List.mapi (fun i _ -> $"{v}_sbt{i}")
        let guardInd = if distribute then "    " else ""
        let trailOpen =
            trailVars |> List.mapi (fun i tv ->
                $"""{(bodyInd + guardInd + String.replicate i "    ")}for (size_t {tv} = 0; {tv} < {trailExts.[i]}; {tv}++) {{""")
        let trailClose =
            [ for i in trailVars.Length - 1 .. -1 .. 0 -> bodyInd + guardInd + String.replicate i "    " + "}" ]
        let trailIdx =
            if trailVars.IsEmpty then "0"
            else
                let mutable acc = trailVars.[0]
                for i in 1 .. trailVars.Length - 1 do
                    acc <- $"({acc}) * {trailExts.[i]} + {trailVars.[i]}"
                acc
        let valueExpr =
            match meta.FillValue with
            | FillNone -> $"({elemCpp}){v}_cbuf[{v}_local * {trail} + {trailIdx}]"
            | _ -> $"({v}_have ? ({elemCpp}){v}_cbuf[{v}_local * {trail} + {trailIdx}] : {v}_fillv)"
        let assign =
            $"""{(bodyInd + guardInd + String.replicate trailVars.Length "    ")}{v}_flat[{v}_pool * {trail} + {trailIdx}] = {valueExpr};"""
        let cellBody =
            [ bodyInd + $"std::array<size_t, {r}> {v}_sbc = {{ {coordsInit} }};"
              bodyInd + $"size_t {v}_pool = linearized_storage::{nsName}::linearize<{r}>({v}_sbc, {n}UL);" ]
            @ (if distribute then [ bodyInd + $"if ({v}_pool >= {v}_dlo && {v}_pool < {v}_dhi) {{" ] else [])
            @ trailOpen
            @ [ assign ]
            @ trailClose
            @ (if distribute then [ bodyInd + "}" ] else [])
            @ [ bodyInd + $"{v}_local++;" ]
        let cellClose = [ for k in r - 1 .. -1 .. 0 -> String.replicate (k + 1) "    " + "}" ]

        // Post-loop restoration under distribution: contiguous cell-range
        // Allgatherv (counts in ELEMENTS = cells x trailing block).
        let mpiDtype =
            match elemCpp with
            | "double" -> "MPI_DOUBLE"
            | "float" -> "MPI_FLOAT"
            | "long long" -> "MPI_LONG_LONG"
            | "int" -> "MPI_INT"
            | other -> failwith $"Zarr codegen: variable '{meta.Name}': no MPI datatype for element type '{other}'"
        let gather =
            if not distribute then [] else
            [ $"if (__blade_mpi_size > 1) {{ // zarr mpi: restore full pool of '{meta.Name}' on all ranks"
              $"    if ({card * trail}ULL > 2147483647ULL) {{ MPI_Abort(MPI_COMM_WORLD, 13); }}"
              $"    int* {v}_cnt = new int[__blade_mpi_size]; int* {v}_dsp = new int[__blade_mpi_size];"
              "    for (int __r = 0; __r < __blade_mpi_size; __r++) {"
              $"        size_t __lo = (size_t)__r * {v}_dq + ((size_t)__r < {v}_dr ? (size_t)__r : {v}_dr);"
              $"        size_t __hi = __lo + {v}_dq + ((size_t)__r < {v}_dr ? 1 : 0);"
              $"        {v}_cnt[__r] = (int)((__hi - __lo) * {trail}); {v}_dsp[__r] = (int)(__lo * {trail});"
              "    }"
              $"    MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, {v}_flat, {v}_cnt, {v}_dsp, {mpiDtype}, MPI_COMM_WORLD);"
              $"    delete[] {v}_cnt; delete[] {v}_dsp;"
              "}" ]

        [ $"// Read {meta.Name} from zarr store {normPath storePath} (simplex-blocks: {nBlocks} blocks, grid {T}, tile {B})"
          $"{{ std::ifstream {v}_zm(\"{arrayDir}/{metaFile}\"); if (!{v}_zm) {{ std::cerr << \"Zarr error: array '{meta.Name}' not found in store '{(normPath storePath)}' (missing {metaFile})\" << std::endl; std::exit(1); }} }}" ]
        @ tableDecl
        @ fillDecl
        @ distDecls
        @ [ $"{elemCpp}* {v}_flat = new {elemCpp}[{card * trail}];"
            $"{diskCpp}* {v}_cbuf = new {diskCpp}[{chunkCells}];"
            $"for (size_t {v}_sb = 0; {v}_sb < {nBlocks}; {v}_sb++) {{"
            $"    auto {v}_sbt = linearized_storage::symmetric::unlinearize<{r}>({v}_sb, {T}UL);" ]
        @ windowSkipLines
        @ ownershipSkipLines
        @ chunkRead
        @ [ $"    size_t {v}_local = 0;" ]
        @ cellLoops
        @ cellBody
        @ cellClose
        @ [ "}"
            $"delete[] {v}_cbuf;" ]
        @ gather

    /// Generates C++ assembling a blade-packed variable's canonical flat pool
    /// into `<cppVarName>_flat` (the codegen intercept performs allocation,
    /// copy, and release). Validates the declared Blade index types against
    /// the store's layout, then dispatches on physical layout: flat pool
    /// ("packed") or simplex-blocks rows ("packed-blocks"). opts.Distribute
    /// emits the MPI rank-scoped read (blocks only); opts.Window emits the sub-simplex extraction.
    let genReadPacked (storePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (opts: Blade.ProviderRegistry.PackedReadOpts) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        match meta.Blade with
        | None ->
            failwith $"Zarr codegen: variable '{varName}' has no blade packed layout but was typed packed (this indicates a typing inconsistency)"
        | Some layout when layout.Group.Sym = SymWreath ->
            // The store's pool IS the in-memory representation, so assembly is
            // the ordinary flat chunk walk and downstream "materialization" is
            // a straight copy. Everything that could go wrong is a MISMATCH
            // between the declared class and the stored one, checked here rather than trusted.
            let g = layout.Group
            (match arrType.IndexTypes with
             | [ lead ] when lead.Symmetry = SymWreath ->
                 let declLevels = Blade.IR.orbitLevelsOf lead
                 let declExtent =
                     match Blade.IR.orbitBaseExtent lead with
                     | IRLit (IRLitInt n) -> n
                     | _ -> -1L
                 if declLevels <> g.Levels || declExtent <> g.Extent then
                     failwithf "Zarr codegen: variable '%s': declared OrbIdx<%s, %d> does not match the store's orbit head OrbIdx<%s, %d>"
                               varName (Blade.IR.ppOrbitLevels declLevels) declExtent
                               (Blade.IR.ppOrbitLevels g.Levels) g.Extent
             | _ ->
                 failwith $"Zarr codegen: variable '{varName}': the store declares an orbit (iterated-wreath) head, so the variable must type as a SOLE OrbIdx group")
            if opts.Window.IsSome then
                failwith $"Zarr codegen: variable '{varName}': z.read_window over an OrbIdx (iterated-wreath) pool is not supported -- a wreath class has no translated sub-class to window into"
            if opts.Distribute then
                failwith $"Zarr codegen: variable '{varName}': the MPI-distributed read is not defined for an OrbIdx (iterated-wreath) pool (spec_version 2 is the flat single-pool layout only)"
            if layout.Blocks.IsSome then
                failwith $"Zarr codegen: variable '{varName}': 'packed-blocks' is not defined for an orbit head"
            genAssembleFlat storePath store meta cppVarName (elemCppOf arrType.ElemType)
        | Some layout ->
            let g = layout.Group
            let expectedLead =
                match opts.Window with
                | None -> g.Extent
                | Some (lo, hi) ->
                    if lo < 0L || lo >= hi || hi > g.Extent then
                        failwith $"Zarr codegen: variable '{varName}': window [{lo}, {hi}) is outside the packed extent {g.Extent}"
                    hi - lo
            (match arrType.IndexTypes with
             | lead :: rest ->
                 let leadOk =
                     lead.Symmetry = g.Sym && lead.Rank = g.Rank
                     && (match lead.Extent with IRLit (IRLitInt n) -> n = expectedLead | _ -> false)
                 let restExtents =
                     rest |> List.map (fun ix ->
                         match ix.Extent with IRLit (IRLitInt n) -> n | _ -> -1L)
                 let restOk =
                     (rest |> List.forall (fun ix -> ix.Symmetry = SymNone && ix.Rank = 1))
                     && restExtents = layout.DenseDims
                 if not (leadOk && restOk) then
                     failwithf "Zarr codegen: variable '%s': declared packed type does not match the store's blade layout (group %A rank %d expected lead extent %d, dense %A)"
                         varName g.Sym g.Rank expectedLead layout.DenseDims
             | [] -> failwith $"Zarr codegen: variable '{varName}': packed read with no index types")
            let elemCpp = elemCppOf arrType.ElemType
            match opts.Window with
            | None ->
                (match layout.Blocks with
                 | None -> genAssembleFlat storePath store meta cppVarName elemCpp
                 | Some info -> genAssemblePackedBlocks storePath store meta layout info cppVarName elemCpp opts.Distribute None)
            | Some (wlo, whi) ->
                // Assemble the SOURCE pool (blocks stores skip chunks whose tiles
                // miss the window), then extract the translated sub-simplex:
                // window cells enumerate ascending-lex, so the destination index is a running counter.
                let v = cppVarName
                let srcV = v + "_ws"
                let assemble =
                    match layout.Blocks with
                    | None -> genAssembleFlat storePath store meta srcV elemCpp
                    | Some info -> genAssemblePackedBlocks storePath store meta layout info srcV elemCpp false (Some (wlo, whi))
                let strict = (g.Sym = SymAntisymmetric)
                let sInc = if strict then 1 else 0
                let r = g.Rank
                let n = g.Extent
                let w = whi - wlo
                let wcard = packedCardinality { g with Extent = w }
                let trailExts = layout.DenseDims
                let trail = trailExts |> List.fold (fun a d -> a * d) 1L
                let nsName = if strict then "antisymmetric" else "symmetric"
                let wLoops =
                    [ for k in 0 .. r - 1 ->
                        let ind = String.replicate (k + 1) "    "
                        let lo = if k = 0 then $"{wlo}UL" else $"{v}_w{k - 1} + {sInc}"
                        $"{ind}for (size_t {v}_w{k} = {lo}; {v}_w{k} < {whi}UL; {v}_w{k}++) {{" ]
                let bodyInd = String.replicate (r + 1) "    "
                let coordsInit = [ for k in 0 .. r - 1 -> $"{v}_w{k}" ] |> String.concat ", "
                let trailVars = trailExts |> List.mapi (fun i _ -> $"{v}_wt{i}")
                let trailOpen =
                    trailVars |> List.mapi (fun i tv ->
                        $"""{(bodyInd + String.replicate i "    ")}for (size_t {tv} = 0; {tv} < {trailExts.[i]}; {tv}++) {{""")
                let trailClose =
                    [ for i in trailVars.Length - 1 .. -1 .. 0 -> bodyInd + String.replicate i "    " + "}" ]
                let trailIdx =
                    if trailVars.IsEmpty then "0"
                    else
                        let mutable acc = trailVars.[0]
                        for i in 1 .. trailVars.Length - 1 do
                            acc <- $"({acc}) * {trailExts.[i]} + {trailVars.[i]}"
                        acc
                let wClose = [ for k in r - 1 .. -1 .. 0 -> String.replicate (k + 1) "    " + "}" ]
                assemble
                @ [ $"// window [{wlo}, {whi}) extraction: translated {nsName} sub-simplex ({wcard} cells)"
                    $"{elemCpp}* {v}_flat = new {elemCpp}[{wcard * trail}];"
                    $"size_t {v}_wdst = 0;" ]
                @ wLoops
                @ [ bodyInd + $"std::array<size_t, {r}> {v}_wc = {{ {coordsInit} }};"
                    bodyInd + $"size_t {v}_wsrc = linearized_storage::{nsName}::linearize<{r}>({v}_wc, {n}UL);" ]
                @ trailOpen
                @ [ bodyInd + String.replicate trailVars.Length "    "
                    + $"{v}_flat[{v}_wdst * {trail} + {trailIdx}] = {srcV}_flat[{v}_wsrc * {trail} + {trailIdx}];" ]
                @ trailClose
                @ [ bodyInd + $"{v}_wdst++;" ]
                @ wClose
                @ [ $"delete[] {srcV}_flat;" ]

    /// STREAMED fiber reads, hoisted prologue: metadata existence check, the fiber buffer, a per-t-chunk segment buffer, and the fill value. Dense variables only (site dims dense, fiber = the LAST axis).
    let genStreamOpen (storePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        if meta.Blade.IsSome then
            failwith $"Zarr stream of '{varName}': packed variables are not streamable (bind with .read)"
        if arrType.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone || ix.Rank <> 1) then
            failwith $"Zarr stream of '{varName}': dense variables only"
        if meta.Shape.Length < 2 then
            failwith $"Zarr stream of '{varName}': needs at least one site dim plus the trailing fiber axis (rank >= 2)"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let diskCpp = diskCppOf meta.Dtype.Code
        let fiberLen = List.last meta.Shape
        let ctT = List.last meta.Chunks
        let arrayDir =
            let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
            normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
        let metaFile = if meta.Version = 3 then "zarr.json" else ".zarray"
        let fillDecl =
            match meta.FillValue with
            | FillFloat f -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fmtF f};" ]
            | FillInt fi -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fi}LL;" ]
            | FillNone -> []
        [ $"// Stream {varName} from zarr store {normPath storePath} (chunked fiber reads at the S/T boundary)"
          $"{{ std::ifstream {v}_zm(\"{arrayDir}/{metaFile}\"); if (!{v}_zm) {{ std::cerr << \"Zarr error: array '{varName}' not found in store '{(normPath storePath)}' (missing {metaFile})\" << std::endl; std::exit(1); }} }}"
          $"size_t {v}_fiber_ext[1] = {{ {fiberLen} }};"
          $"{diskCpp}* {v}_fseg = new {diskCpp}[{ctT}];" ]
        @ fillDecl

    /// PER-SEGMENT streaming of a RANK-1 dense variable (docs/plans/structural/07
    /// §3.4): the prologue probes the metadata, allocates one chunk's scratch
    /// buffer and the fill value. Nothing of the variable itself is read here.
    let genStreamRowsOpen (storePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        if meta.Blade.IsSome then
            failwith $"Zarr stream of '{varName}': packed variables are not streamable (bind with .read)"
        if meta.Shape.Length <> 1 then
            failwith $"Zarr per-segment stream of '{varName}': rank-1 dense variables only (a rank-{meta.Shape.Length} variable streams by fiber)"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let diskCpp = diskCppOf meta.Dtype.Code
        let k = List.head meta.Chunks
        let arrayDir =
            let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
            normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
        let metaFile = if meta.Version = 3 then "zarr.json" else ".zarray"
        let fillDecl =
            match meta.FillValue with
            | FillFloat f -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fmtF f};" ]
            | FillInt fi -> [ $"{elemCpp} {v}_fillv = ({elemCpp}){fi}LL;" ]
            | FillNone -> []
        [ $"// Stream {varName} from zarr store {normPath storePath} per SEGMENT (rank-1 chunk-range reads; no whole-array buffer)"
          $"{{ std::ifstream {v}_zm(\"{arrayDir}/{metaFile}\"); if (!{v}_zm) {{ std::cerr << \"Zarr error: array '{varName}' not found in store '{(normPath storePath)}' (missing {metaFile})\" << std::endl; std::exit(1); }} }}"
          $"{diskCpp}* {v}_rseg = new {diskCpp}[{k}];" ]
        @ fillDecl

    /// The run [lo, hi) of a rank-1 dense variable read into `destBuf[0 ..
    /// hi-lo)`: one file open per chunk the run intersects, one seek+read of
    /// the intersection, fill (or a loud failure under a null fill_value) for
    /// a missing chunk. `lo`/`hi` are C++ expressions, so the group loop can
    /// drive them from the grouping's offsets.
    let genStreamRows (storePath: string) (varName: string) (cppVarName: string) (destBuf: string) (lo: string) (hi: string) (arrType: IRArrayType) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        if meta.Shape.Length <> 1 then
            failwith $"Zarr per-segment stream of '{varName}': rank-1 dense variables only"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let bs = meta.Dtype.ByteSize
        let n = List.head meta.Shape
        let k = List.head meta.Chunks
        let arrayDir =
            let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
            normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
        let sep = meta.ChunkKeySep
        let keyExpr =
            if meta.ChunkKeyPrefix = "" then $"std::to_string({v}_rc)"
            else $"std::string(\"{meta.ChunkKeyPrefix}{sep}\") + std::to_string({v}_rc)"
        let missingBranch =
            match meta.FillValue with
            | FillNone ->
                [ $"    }} else {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{varName}' is missing and fill_value is null\" << std::endl; std::exit(1); }}" ]
            | _ ->
                [ "    } else {"
                  $"        for (size_t {v}_q = 0; {v}_q < {v}_rlen; {v}_q++) ({destBuf})[{v}_rs - ({lo}) + {v}_q] = {v}_fillv;"
                  "    }" ]
        [ $"for (size_t {v}_rc = (size_t)({lo}) / {k}; {v}_rc * {k} < (size_t)({hi}); {v}_rc++) {{"
          $"    size_t {v}_rs = {v}_rc * {k}; if ({v}_rs < (size_t)({lo})) {v}_rs = (size_t)({lo});"
          $"    size_t {v}_re = ({v}_rc + 1) * {k}; if ({v}_re > (size_t)({hi})) {v}_re = (size_t)({hi}); if ({v}_re > {n}) {v}_re = {n};"
          $"    size_t {v}_rlen = {v}_re - {v}_rs;"
          $"    std::string {v}_key = {keyExpr};"
          $"    std::ifstream {v}_cf(std::string(\"{arrayDir}/\") + {v}_key, std::ios::binary);"
          $"    if ({v}_cf) {{"
          $"        {v}_cf.seekg((std::streamoff)(({v}_rs - {v}_rc * {k}) * {bs}));"
          $"        {v}_cf.read((char*){v}_rseg, {v}_rlen * {bs});"
          $"        if ({v}_cf.gcount() != (std::streamsize)({v}_rlen * {bs})) {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{varName}' is short -- a compressed or corrupt store?\" << std::endl; std::exit(1); }}"
          $"        for (size_t {v}_q = 0; {v}_q < {v}_rlen; {v}_q++) ({destBuf})[{v}_rs - ({lo}) + {v}_q] = ({elemCpp}){v}_rseg[{v}_q];" ]
        @ missingBranch
        @ [ "}" ]

    /// STREAMED fiber reads, in-nest: assemble one trailing-axis fiber at the
    /// given site coordinates, one seek+read per t-chunk (contiguous WITHIN a
    /// chunk since the fiber axis is innermost). Missing chunks fill (or fail loudly under a null fill_value).
    let genStreamFiber (storePath: string) (varName: string) (cppVarName: string) (destBuf: string) (siteExprs: string list) (arrType: IRArrayType) : string list =
        let store = load storePath
        let meta =
            match tryFindArray store varName with
            | Some m -> m
            | None -> failwith $"Zarr codegen: variable '{varName}' not found in store '{storePath}'"
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let bs = meta.Dtype.ByteSize
        let shape = meta.Shape
        let chunks = meta.Chunks
        let d = shape.Length - 1
        if siteExprs.Length <> d then
            failwith $"Zarr stream of '{varName}': {siteExprs.Length} site coordinates for {d} site dims"
        let fiberLen = List.last shape
        let ctT = List.last chunks
        let gridT = (fiberLen + ctT - 1L) / ctT
        let arrayDir =
            let rel = Path.GetRelativePath(store.Path, meta.ArrayDir)
            normPath (if rel = "." then storePath else Path.Combine(storePath, rel))
        // Chunk key from site-chunk coords + the t-chunk counter.
        let sep = meta.ChunkKeySep
        let coordParts =
            [ for k in 0 .. d - 1 -> $"std::to_string((size_t)({siteExprs.[k]}) / {chunks.[k]})" ]
            @ [ $"std::to_string({v}_tc)" ]
        let joined = String.concat $" + \"{sep}\" + " coordParts
        let keyExpr =
            if meta.ChunkKeyPrefix = "" then joined
            else $"std::string(\"{meta.ChunkKeyPrefix}{sep}\") + {joined}"
        // Within-chunk fiber start (elements): Horner over chunk-local site
        // coords with chunk-internal strides, times the t-chunk extent.
        let offExpr =
            let mutable acc = sprintf "((size_t)(%s) %% %d)" siteExprs.[0] chunks.[0]
            for k in 1 .. d - 1 do
                acc <- sprintf "(%s * %d + (size_t)(%s) %% %d)" acc chunks.[k] siteExprs.[k] chunks.[k]
            $"{acc} * {ctT}"
        let missingBranch =
            match meta.FillValue with
            | FillNone ->
                [ $"    }} else {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{varName}' is missing and fill_value is null\" << std::endl; std::exit(1); }}" ]
            | _ ->
                [ "    } else {"
                  $"        for (size_t {v}_q = 0; {v}_q < {v}_len; {v}_q++) {destBuf}[{v}_tc * {ctT} + {v}_q] = {v}_fillv;"
                  "    }" ]
        [ $"for (size_t {v}_tc = 0; {v}_tc < {gridT}; {v}_tc++) {{"
          $"    std::string {v}_key = {keyExpr};"
          $"    size_t {v}_len = {fiberLen} - {v}_tc * {ctT}; if ({v}_len > {ctT}) {v}_len = {ctT};"
          $"    std::ifstream {v}_cf(std::string(\"{arrayDir}/\") + {v}_key, std::ios::binary);"
          $"    if ({v}_cf) {{"
          $"        {v}_cf.seekg((std::streamoff)(({offExpr}) * {bs}));"
          $"        {v}_cf.read((char*){v}_fseg, {v}_len * {bs});"
          $"        if ({v}_cf.gcount() != (std::streamsize)({v}_len * {bs})) {{ std::cerr << \"Zarr error: chunk '\" << {v}_key << \"' of '{varName}' is short -- a compressed or corrupt store?\" << std::endl; std::exit(1); }}"
          $"        for (size_t {v}_q = 0; {v}_q < {v}_len; {v}_q++) {destBuf}[{v}_tc * {ctT} + {v}_q] = ({elemCpp}){v}_fseg[{v}_q];" ]
        @ missingBranch
        @ [ "}" ]

    /// Generates C++ to write a variable as an uncompressed Zarr v2 array
    /// (one chunk = the whole array): flat "."-separated chunk keys, readable
    /// by every zarr-python/xarray. Multiple writes into one store root
    /// accumulate arrays (.zgroup is idempotent); re-writing overwrites. The caller provides `<cppVarName>_flat` (row-major).
    let genWriteVar (storePath: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (dimNames: string list) : string list =
        let v = cppVarName
        // A packed (SymIdx/AntisymIdx) leading group writes as its POOL:
        // on-disk shape = [cardinality] @ trailing dense extents. The flat
        // buffer the intercept hands over is already in canonical pool order.
        let packedLead =
            match arrType.IndexTypes with
            | lead :: _ when lead.Symmetry <> SymNone && lead.Rank >= 2 -> Some lead
            | _ -> None
        let litExtent (context: string) (e: IRExpr) =
            match e with
            | IRLit (IRLitInt n) -> n
            | _ -> failwith $"Zarr write of '{varName}' requires literal extents ({context})"
        let (shape, bladeAttrJson) =
            match packedLead with
            | Some lead when lead.Symmetry = SymWreath ->
                // The array IS the flat pool, so the on-disk shape is [cardinality]
                // and the caller's buffer is copied verbatim (no trailing dims: a wreath group never composes with another index group).
                if arrType.IndexTypes.Length <> 1 then
                    failwith $"Zarr write of '{varName}': an OrbIdx (iterated-wreath) group is stored as a SOLE flat pool; {arrType.IndexTypes.Length} index groups were declared"
                let levels = Blade.IR.orbitLevelsOf lead
                if List.isEmpty levels then
                    failwith $"Zarr write of '{varName}': the wreath record carries no level list (its Extent must be the IROrbitClass marker)"
                let n = litExtent "orbit base extent" (Blade.IR.orbitBaseExtent lead)
                let group = { Sym = SymWreath; Rank = lead.Rank; Extent = n; Levels = levels }
                let card =
                    match packedCardinalityChecked group with
                    | Ok c -> c
                    | Error detail ->
                        failwith $"Zarr write of '{varName}': OrbIdx{(Blade.IR.ppOrbitLevels levels)} at extent {n} -- {detail}"
                let levelsJson =
                    levels
                    |> List.map (fun (r, plus) -> sprintf "[%d, \\\"%s\\\"]" r (if plus then "+" else "-"))
                    |> String.concat ", "
                let attr =
                    $", \\\"blade\\\": {{\\\"spec_version\\\": 2, \\\"layout\\\": \\\"packed\\\", \\\"order\\\": \\\"ascending-lex\\\", \\\"index_types\\\": [{{\\\"kind\\\": \\\"orbit\\\", \\\"levels\\\": [{levelsJson}], \\\"extent\\\": {n}}}], \\\"decomposition\\\": {{\\\"scheme\\\": \\\"flat-ranges\\\"}}}}"
                ([card], attr)
            | Some lead ->
                (match lead.Symmetry with
                 | SymSymmetric | SymAntisymmetric -> ()
                 | s -> failwithf "Zarr write of '%s': %A packed groups are not supported" varName s)
                let trailing =
                    arrType.IndexTypes |> List.tail |> List.map (fun ix ->
                        if ix.Symmetry <> SymNone || ix.Rank <> 1 then
                            failwith $"Zarr write of '{varName}': only one leading packed group plus dense trailing dims is supported"
                        litExtent "trailing dim" ix.Extent)
                let group = { Sym = lead.Symmetry; Rank = lead.Rank; Extent = litExtent "packed extent" lead.Extent; Levels = [] }
                let card = packedCardinality group
                let kindStr = if group.Sym = SymSymmetric then "sym" else "antisym"
                let idxEntries =
                    $"{{\\\"kind\\\": \\\"{kindStr}\\\", \\\"rank\\\": {group.Rank}, \\\"extent\\\": {group.Extent}}}"
                    :: (trailing |> List.map (sprintf "{\\\"kind\\\": \\\"dense\\\", \\\"extent\\\": %d}"))
                // spec_version stays 1 for a depth-1 head: version 2 admits
                // sym/antisym unchanged, so stamping 2 here would only make
                // today's stores unreadable by a v1 reader for no gain.
                let attr =
                    sprintf ", \\\"blade\\\": {\\\"spec_version\\\": 1, \\\"layout\\\": \\\"packed\\\", \\\"order\\\": \\\"ascending-lex\\\", \\\"index_types\\\": [%s], \\\"decomposition\\\": {\\\"scheme\\\": \\\"flat-ranges\\\"}}"
                        (String.concat ", " idxEntries)
                (card :: trailing, attr)
            | None ->
                let componentExtents =
                    arrType.IndexTypes |> List.collect (fun idx -> List.replicate idx.Rank idx.Extent)
                (componentExtents |> List.map (litExtent "dim"), "")
        let rank = shape.Length
        let elemCpp = elemCppOf arrType.ElemType
        let dtype = v2DtypeOf arrType.ElemType
        let total = shape |> List.fold (fun a b -> a * b) 1L
        let root = normPath storePath
        let arrayDir = $"{root}/{varName}"
        let shapeJson = shape |> List.map string |> String.concat ", "
        let dims =
            if dimNames.Length >= rank then List.truncate rank dimNames
            else [ for i in 0 .. rank - 1 -> $"dim{i}" ]
        // Escaped for splicing inside the C++ string literal (like the
        // .zarray body below): the emitted code must print {"..."} JSON.
        let dimsJson = dims |> List.map (fun d -> $"\\\"{d}\\\"") |> String.concat ", "
        let zarrayJson =
            $"{{\\\"zarr_format\\\": 2, \\\"shape\\\": [{shapeJson}], \\\"chunks\\\": [{shapeJson}], \\\"dtype\\\": \\\"{dtype}\\\", \\\"compressor\\\": null, \\\"fill_value\\\": 0, \\\"order\\\": \\\"C\\\", \\\"filters\\\": null}}"
        let zattrsJson = $"{{\\\"_ARRAY_DIMENSIONS\\\": [{dimsJson}]{bladeAttrJson}}}"
        let chunkKey0 =
            if rank = 0 then "0"
            else List.replicate rank "0" |> String.concat "."
        let wCheck (stream: string) (context: string) =
            $"if (!{stream}.good()) {zExit context}"
        [
            $"// Write {varName} to zarr store {root} (v2, uncompressed, single chunk)"
            sprintf "{ std::error_code %s_ec; std::filesystem::create_directories(\"%s\", %s_ec); if (%s_ec) %s }"
                v arrayDir v v (zExit $"cannot create store directory '{arrayDir}'")
            sprintf "{ std::ofstream %s_zg(\"%s/.zgroup\", std::ios::trunc); %s_zg << \"{\\\"zarr_format\\\": 2}\"; %s }"
                v root v (wCheck $"{v}_zg" $"cannot write '{root}/.zgroup'")
            sprintf "{ std::ofstream %s_za(\"%s/.zarray\", std::ios::trunc); %s_za << \"%s\"; %s }"
                v arrayDir v zarrayJson (wCheck $"{v}_za" $"cannot write '{arrayDir}/.zarray'")
            sprintf "{ std::ofstream %s_zt(\"%s/.zattrs\", std::ios::trunc); %s_zt << \"%s\"; %s }"
                v arrayDir v zattrsJson (wCheck $"{v}_zt" $"cannot write '{arrayDir}/.zattrs'")
            sprintf "{ std::ofstream %s_ch(\"%s/%s\", std::ios::binary | std::ios::trunc); %s_ch.write((const char*)%s_flat, sizeof(%s) * %d); %s }"
                v arrayDir chunkKey0 v v elemCpp total
                (wCheck $"{v}_ch" $"cannot write chunk '{arrayDir}/{chunkKey0}'")
        ]

    /// Required C++ includes for Zarr I/O (std only -- no link flags).
    let genIncludes () : string list =
        [ "#include <fstream>"
          "#include <filesystem>"
          "#include <cstdint>"
          "#include <string>"
          "#include <limits>" ]

// F#-side store writer (fixtures and programmatic store creation)

module ZarrWrite =

    type WritePayload =
        | WF32 of float32[]
        | WF64 of float[]
        | WI32 of int32[]
        | WI64 of int64[]

    type WriteVar = {
        Name: string
        /// None -> no _ARRAY_DIMENSIONS / dimension_names written.
        DimNames: string list option
        Shape: int64 list
        Chunks: int64 list
        FillValue: ZarrFill
        /// Row-major, length = product Shape.
        Data: WritePayload
        /// Chunk grid coordinates to leave UNWRITTEN (fill_value tests).
        OmitChunks: int64 list list
        /// Triangular-decomposed layout: Shape must be [cardinality] @ DenseDims and Data already in canonical ascending-lex pool order. Writes the `blade` attribute.
        Blade: BladeLayout option
    }

    let private payloadInfo (p: WritePayload) : string * string * int =
        // (v2 dtype, v3 data_type, byte size)
        match p with
        | WF32 _ -> ("<f4", "float32", 4)
        | WF64 _ -> ("<f8", "float64", 8)
        | WI32 _ -> ("<i4", "int32", 4)
        | WI64 _ -> ("<i8", "int64", 8)

    let private cellBytes (p: WritePayload) (idx: int) : byte[] =
        match p with
        | WF32 xs -> BitConverter.GetBytes xs.[idx]
        | WF64 xs -> BitConverter.GetBytes xs.[idx]
        | WI32 xs -> BitConverter.GetBytes xs.[idx]
        | WI64 xs -> BitConverter.GetBytes xs.[idx]

    let private fillBytes (p: WritePayload) (fill: ZarrFill) : byte[] =
        match p, fill with
        | WF32 _, FillFloat f -> BitConverter.GetBytes (float32 f)
        | WF32 _, FillInt n -> BitConverter.GetBytes (float32 n)
        | WF64 _, FillFloat f -> BitConverter.GetBytes f
        | WF64 _, FillInt n -> BitConverter.GetBytes (float n)
        | WI32 _, FillInt n -> BitConverter.GetBytes (int32 n)
        | WI32 _, FillFloat f -> BitConverter.GetBytes (int32 f)
        | WI64 _, FillInt n -> BitConverter.GetBytes n
        | WI64 _, FillFloat f -> BitConverter.GetBytes (int64 f)
        | _, FillNone -> Array.zeroCreate 8  // edge padding when no fill declared
        |> fun bs -> bs

    let private jsonNum (fill: ZarrFill) : string =
        match fill with
        | FillFloat f when Double.IsNaN f -> "\"NaN\""
        | FillFloat f when Double.IsPositiveInfinity f -> "\"Infinity\""
        | FillFloat f when Double.IsNegativeInfinity f -> "\"-Infinity\""
        | FillFloat f -> f.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | FillInt n -> string n
        | FillNone -> "null"

    /// The `blade` attribute value for a packed layout (plain JSON -- the F#
    /// writer emits real files, no C++ escaping). spec_version 2 is stamped
    /// ONLY for an orbit head, so a depth-1 store stays byte-identical to what a v1 reader wrote/reads.
    let private bladeAttrValue (layout: BladeLayout) : string =
        let isOrbit = (layout.Group.Sym = SymWreath)
        let headEntry =
            if isOrbit then
                let levelsJson =
                    layout.Group.Levels
                    |> List.map (fun (r, plus) -> sprintf "[%d, \"%s\"]" r (if plus then "+" else "-"))
                    |> String.concat ", "
                $"{{\"kind\": \"orbit\", \"levels\": [{levelsJson}], \"extent\": {layout.Group.Extent}}}"
            else
                let kindStr = if layout.Group.Sym = SymSymmetric then "sym" else "antisym"
                $"{{\"kind\": \"{kindStr}\", \"rank\": {layout.Group.Rank}, \"extent\": {layout.Group.Extent}}}"
        let idxEntries =
            headEntry :: (layout.DenseDims |> List.map (sprintf "{\"kind\": \"dense\", \"extent\": %d}"))
        let (layoutStr, decompJson) =
            match layout.Blocks with
            | None -> ("packed", "{\"scheme\": \"flat-ranges\"}")
            | Some i ->
                let orderStr = match i.Order with OrderLex -> "ascending-lex" | OrderPath -> "path"
                ("packed-blocks",
                 $"{{\"scheme\": \"simplex-blocks\", \"tile\": {i.Tile}, \"grid\": {i.Grid}, \"block_order\": \"{orderStr}\"}}")
        sprintf "{\"spec_version\": %d, \"layout\": \"%s\", \"order\": \"ascending-lex\", \"index_types\": [%s], \"decomposition\": %s}"
            (if isOrbit then 2 else 1) layoutStr (String.concat ", " idxEntries) decompJson

    /// Normalize a blocks-layout WriteVar to its physical form: the caller
    /// supplies the LOGICAL view (Shape = [cardinality] @ dense, Data = the
    /// canonical pool); this permutes/pads into block rows ([blockCount, tile^rank] @ dense). Ordinary vars pass through unchanged.
    let private toPhysical (var: WriteVar) : WriteVar =
        match var.Blade with
        | Some layout when layout.Blocks.IsSome ->
            let info = layout.Blocks.Value
            let g = layout.Group
            let strict = (g.Sym = SymAntisymmetric)
            let card = packedCardinality g
            let trail = layout.DenseDims |> List.fold (fun a d -> a * int d) 1
            let logicalShape = card :: layout.DenseDims
            if var.Shape <> logicalShape then
                failwithf "ZarrWrite '%s': blocks-layout vars take the LOGICAL shape [cardinality] @ dense = %A (got %A); Data is the canonical pool"
                    var.Name logicalShape var.Shape
            let cellMap = SimplexBlocks.blocksCellMap strict g.Extent info.Tile g.Rank info.Order
            let physCells = cellMap.Length
            let nBlocks = SimplexBlocks.blockCount g.Rank info.Grid
            let rowW = SimplexBlocks.maxBlockCells g.Rank info.Tile
            let inline remap (src: 'a[]) (pad: 'a) : 'a[] =
                let out = Array.create (physCells * trail) pad
                for p in 0 .. physCells - 1 do
                    let pool = cellMap.[p]
                    if pool >= 0 then
                        for t in 0 .. trail - 1 do
                            out.[p * trail + t] <- src.[pool * trail + t]
                out
            let padAsFloat =
                match var.FillValue with
                | FillFloat f -> f
                | FillInt n -> float n
                | FillNone -> 0.0
            let data' =
                match var.Data with
                | WF64 xs -> WF64 (remap xs padAsFloat)
                | WF32 xs -> WF32 (remap xs (float32 padAsFloat))
                | WI64 xs -> WI64 (remap xs (int64 padAsFloat))
                | WI32 xs -> WI32 (remap xs (int32 padAsFloat))
            let physRank = 2 + layout.DenseDims.Length
            { var with
                Shape = nBlocks :: rowW :: layout.DenseDims
                Chunks = 1L :: rowW :: layout.DenseDims
                Data = data'
                DimNames = (match var.DimNames with
                            | Some ns when ns.Length = physRank -> Some ns
                            | _ -> None)
                OmitChunks = [] }
        | _ -> var

    /// Build one chunk's bytes: the in-bounds region from Data, edge overhang padded with the fill value (zeros under FillNone).
    let private chunkBytesFor (var: WriteVar) (coords: int64 list) : byte[] =
        let shape = var.Shape |> List.map int
        let chunks = var.Chunks |> List.map int
        let rank = shape.Length
        let (_, _, bs) = payloadInfo var.Data
        let chunkCount = chunks |> List.fold (*) 1
        let gStr = rowMajorStrides shape
        let cStr = rowMajorStrides chunks
        let coordsArr = coords |> List.map int |> List.toArray
        let shapeArr = List.toArray shape
        let chunksArr = List.toArray chunks
        let gStrArr = List.toArray gStr
        let cStrArr = List.toArray cStr
        let out = Array.zeroCreate<byte> (chunkCount * bs)
        let pad = fillBytes var.Data var.FillValue
        // Pre-fill everything with padding, then overwrite in-bounds cells.
        for c in 0 .. chunkCount - 1 do
            Array.blit pad 0 out (c * bs) bs
        let rec go (d: int) (gBase: int) (cBase: int) =
            if d = rank then
                Array.blit (cellBytes var.Data gBase) 0 out (cBase * bs) bs
            else
                let basePos = coordsArr.[d] * chunksArr.[d]
                let lim = min chunksArr.[d] (shapeArr.[d] - basePos)
                for l in 0 .. lim - 1 do
                    go (d + 1) (gBase + (basePos + l) * gStrArr.[d]) (cBase + l * cStrArr.[d])
        if chunkCount > 0 && (shape |> List.fold (*) 1) > 0 then go 0 0 0
        out

    let private writeChunks (arrayDir: string) (var: WriteVar) (sep: string) (prefix: string) : unit =
        let omit = var.OmitChunks |> List.map (List.map string >> String.concat ",") |> Set.ofList
        for coords in gridCoords var.Shape var.Chunks do
            let skip = Set.contains (coords |> List.map string |> String.concat ",") omit
            if not skip then
                let joined = coords |> List.map string |> String.concat sep
                let key =
                    match prefix, coords with
                    | "", [] -> "0"
                    | "", _ -> joined
                    | p, [] -> p
                    | p, _ -> p + sep + joined
                let file = Path.Combine(arrayDir, key.Replace('/', Path.DirectorySeparatorChar))
                Directory.CreateDirectory (Path.GetDirectoryName file) |> ignore
                File.WriteAllBytes(file, chunkBytesFor var coords)

    /// Write a Zarr v2 group store ("." chunk keys, .zgroup/.zarray/.zattrs).
    let writeStoreV2 (root: string) (vars: WriteVar list) : unit =
        Directory.CreateDirectory root |> ignore
        File.WriteAllText(Path.Combine(root, ".zgroup"), "{\"zarr_format\": 2}")
        for var in vars |> List.map toPhysical do
            let (dtype, _, _) = payloadInfo var.Data
            let arrayDir = Path.Combine(root, var.Name)
            Directory.CreateDirectory arrayDir |> ignore
            let shapeJson = var.Shape |> List.map string |> String.concat ", "
            let chunksJson = var.Chunks |> List.map string |> String.concat ", "
            File.WriteAllText(
                Path.Combine(arrayDir, ".zarray"),
                $"{{\"zarr_format\": 2, \"shape\": [{shapeJson}], \"chunks\": [{chunksJson}], \"dtype\": \"{dtype}\", \"compressor\": null, \"fill_value\": {(jsonNum var.FillValue)}, \"order\": \"C\", \"filters\": null}}")
            let attrParts =
                [ match var.DimNames with
                  | Some dims ->
                      yield sprintf "\"_ARRAY_DIMENSIONS\": [%s]" (dims |> List.map (sprintf "\"%s\"") |> String.concat ", ")
                  | None -> ()
                  match var.Blade with
                  | Some layout -> yield $"\"blade\": {bladeAttrValue layout}"
                  | None -> () ]
            if not attrParts.IsEmpty then
                File.WriteAllText(
                    Path.Combine(arrayDir, ".zattrs"),
                    $"""{{{(String.concat ", " attrParts)}}}""")
            writeChunks arrayDir var "." ""

    /// Write a Zarr v3 group store (zarr.json nodes, "c/"-prefixed keys).
    let writeStoreV3 (root: string) (vars: WriteVar list) : unit =
        Directory.CreateDirectory root |> ignore
        File.WriteAllText(Path.Combine(root, "zarr.json"), "{\"zarr_format\": 3, \"node_type\": \"group\"}")
        for var in vars |> List.map toPhysical do
            let (_, dataType, _) = payloadInfo var.Data
            let arrayDir = Path.Combine(root, var.Name)
            Directory.CreateDirectory arrayDir |> ignore
            let shapeJson = var.Shape |> List.map string |> String.concat ", "
            let chunksJson = var.Chunks |> List.map string |> String.concat ", "
            let fillJson =
                match var.FillValue with
                | FillNone -> "0"  // v3 requires a fill_value
                | f -> jsonNum f
            let dimsJson =
                match var.DimNames with
                | Some dims ->
                    sprintf ", \"dimension_names\": [%s]" (dims |> List.map (sprintf "\"%s\"") |> String.concat ", ")
                | None -> ""
            let attrsJson =
                match var.Blade with
                | Some layout -> $", \"attributes\": {{\"blade\": {bladeAttrValue layout}}}"
                | None -> ""
            File.WriteAllText(
                Path.Combine(arrayDir, "zarr.json"),
                $"{{\"zarr_format\": 3, \"node_type\": \"array\", \"shape\": [{shapeJson}], \"data_type\": \"{dataType}\", \"chunk_grid\": {{\"name\": \"regular\", \"configuration\": {{\"chunk_shape\": [{chunksJson}]}}}}, \"chunk_key_encoding\": {{\"name\": \"default\", \"configuration\": {{\"separator\": \"/\"}}}}, \"fill_value\": {fillJson}, \"codecs\": [{{\"name\": \"bytes\", \"configuration\": {{\"endian\": \"little\"}}}}]{dimsJson}{attrsJson}}}")
            writeChunks arrayDir var "/" "c"

// Provider registration record

let private adaptVarData (d: ZarrVarData) : Blade.ProviderRegistry.ProviderVarData =
    { DimLengths = d.DimLengths
      Payload =
        match d.Payload with
        | ZFloats xs -> Blade.ProviderRegistry.PFloats xs
        | ZInts xs -> Blade.ProviderRegistry.PInts xs }

/// The zarr ProviderSpec (surface module name "zarr").
let spec : Blade.ProviderRegistry.ProviderSpec = {
    Name = "zarr"
    LoadAsModule = loadAsModule
    ReadVarData = fun path varName ->
        // Packed (blade-layout) variables do not fold: StaticValue has no
        // packed carrier (SVTuple nesting assumes dense row-major), so the
        // pool would fold to a WRONG dense shape. Loud steering instead.
        try
            let store = load path
            match tryFindArray store varName with
            | Some m when m.Blade.IsSome ->
                Error $"variable '{varName}' has a packed (blade: layout=packed) pool layout -- triangular and orbit (iterated-wreath) variables do not fold at compile time; bind with a plain `let ... |> <alias>.read`"
            | _ -> readVarData path varName |> Result.map adaptVarData
        with ex -> Error ex.Message
    GenReadVar = CppZarr.genReadVar
    GenReadPacked = Some CppZarr.genReadPacked
    // Presence is the wreath capability flag at every seam; the function
    // itself is the F#-side pool reader the interpreter materializes from.
    // Refuses anything that is not an orbit head, so a depth-1 packed variable gets a named error, not a pool typed as the wrong class.
    ReadWreathPool = Some (fun path varName ->
        try
            let store = load path
            match tryFindArray store varName with
            | None ->
                Error $"variable '{varName}' not found in Zarr store '{path}'"
            | Some m ->
                match m.Blade with
                | Some l when l.Group.Sym = SymWreath -> readPackedPool m |> Result.map adaptVarData
                | Some _ ->
                    Error $"variable '{varName}' has a depth-1 packed (sym/antisym) layout, not an orbit head"
                | None ->
                    Error $"variable '{varName}' is an ordinary dense array, not an orbit (iterated-wreath) pool"
        with ex -> Error ex.Message)
    GenReadCompoundVar = None  // load_compound: rejected loudly in v1
    GenWriteVar = CppZarr.genWriteVar
    GenStreamOpen = Some CppZarr.genStreamOpen
    GenStreamFiber = Some CppZarr.genStreamFiber
    GenStreamRowsOpen = Some CppZarr.genStreamRowsOpen
    GenStreamRows = Some CppZarr.genStreamRows
    StreamRowsBlock = Some (fun path varName ->
        match tryFindArray (load path) varName with
        | Some m when not m.Chunks.IsEmpty -> List.head m.Chunks
        | _ -> 4096L)
    Includes = CppZarr.genIncludes
    VarDimNames = fun path varName ->
        try
            load path |> fun s -> tryFindArray s varName |> Option.bind _.DimNames
        with _ -> None
    Fingerprint = storeFingerprint
    VersionStamp = storeVersionStamp
    LinkNeeds = "none (pure std C++17)"
}
