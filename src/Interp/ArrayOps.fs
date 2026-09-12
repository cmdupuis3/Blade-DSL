// Blade tree-walking interpreter: dense array storage, access, virtual arrays,
// eager array ops, and byte-parity array printing (Milestone M2-alpha).
//
// Owns the value-space array machinery Interp/Loops.fs and the Core.fs /
// Print.fs arms build on: allocate/reshape stores, read/write/peel cells,
// produce virtual-array element values, fold reductions, run the eager
// set/reshape ops, and emit the same stdout the compiled C++ produces for a
// top-level array binding (CodeGen.genPrintArrayFlat / genPrintArraySymAware),
// byte-for-byte.
//
// SCOPE: M2-alpha = dense arrays (rank-1 flat / rank>=2 nested rows).
// Symmetric-compact, ragged, and compound storage are later milestones;
// unimplemented entry points raise `ArrayOpUnsupported`. canonFold is fully
// implemented (needed by the M2.5 symmetry read/write it precedes).
//
// Deviations from m2-design.md 1: (1) no InterpState parameter -- Core.fs
// (which defines it) compiles AFTER this file, so state-dependent work comes
// in as a closure (`fold`/`pred`/`key`) the caller builds; only InterpPanic
// (Value.fs) is needed directly. (2) own `ArrayOpUnsupported` exception,
// since Core.InterpUnsupported/Print.PrintUnsupported compile later; Run.fs
// catches it and maps it to ExitUnsupported (125) like the other SKIP
// categories. (3) VirtualKind lives here, not in the IR.
//
// Compiled after Interp/RandMirror.fs, before Interp/Core.fs. References
// Value.fs, CppFormat.fs, Numerics.fs, and the concrete IR/Types.
module Blade.Interp.ArrayOps

open System.Text
open System.Collections.Generic
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Interp.Value
open Blade.Interp.CppFormat

module N = Blade.Interp.Numerics

// Faults

/// Raised for an array storage class / print form not yet interpreted
/// (symmetric-compact, ragged, compound, rank-5+ nests, ...). The driver maps
/// this to the gate's ExitUnsupported (see CONTRACT NOTE (2)); it is the M2
/// analog of Core.InterpUnsupported for the array layer.
exception ArrayOpUnsupported of string

// Value <-> store-cell coercions: writing a kernel result / literal element
// into a typed unboxed store may need widening (Int64 into a Float64 output,
// etc.), as C++ does at the assignment. Same rules as Core.fs's private
// toI64/toF64 and Numerics' asF64/asI64.

let private toF64v (v: Value) : float =
    match v with
    | VFloat f -> f
    | VFloat32 f -> float f
    | VInt n -> float n
    | VInt32 n -> float n
    | VBool b -> if b then 1.0 else 0.0
    | VChar c -> float (int c)
    | VComplex (r, _) -> r
    | _ -> nan

let private toI64v (v: Value) : int64 =
    match v with
    | VInt n -> n
    | VInt32 n -> int64 n
    | VFloat f -> int64 f
    | VFloat32 f -> int64 (float f)
    | VBool b -> if b then 1L else 0L
    | VChar c -> int64 (int c)
    | _ -> 0L

let private toComplexv (v: Value) : float * float =
    match v with
    | VComplex (r, i) -> (r, i)
    | other -> (toF64v other, 0.0)

let private toBoolv (v: Value) : bool =
    match v with
    | VBool b -> b
    | VInt n -> n <> 0L
    | VInt32 n -> n <> 0
    | _ -> false

// Element-type projection

/// Project the primitive ElemType out of an element IRType, seeing through unit
/// annotations and nominal index-tag wrappers (Nat<I> = IRTIdxTagged(IRTScalar
/// ETInt64, _)). None for non-scalar element types (struct / func / nested array).
/// Mirrors Print.elemThrough.
let rec elemThrough (ty: IRType) : ElemType option =
    match ty with
    | IRTScalar et -> Some et
    | IRTUnitAnnotated (inner, _) -> elemThrough inner
    | IRTIdxTagged (inner, _) -> elemThrough inner
    | _ -> None

// 1 Storage: allocate / reshape flat backing stores.
//
// STORAGE MODEL (m2-design 7): a dense BladeArray is a rank-1 flat unboxed
// store, or a rank>=2 SNested tree whose leaves are flat rows (row-major). A
// peel (peelDim) shares -- does not copy -- the parent's SNested row, so
// mutation through the peel is visible in the parent (the C++
// `{data[i], extents+1}` view). Narrow types widen per Value.fs: Float32->
// SFloat, Int32->SInt, Complex64->SComplex; String/struct/tuple/func -> SObj.

let private storeLen (s: Store) : int =
    match s with
    | SFloat a -> a.Length
    | SInt a -> a.Length
    | SComplex a -> a.Length
    | SBool a -> a.Length
    | SObj a -> a.Length
    | SNested r -> r.Length
    | SRagged (r, _, _) -> r.Length

/// Copy a contiguous slice out of a FLAT store (used to partition a flat backing
/// into rows when reshaping). Nested/ragged stores are never sliced this way.
let private sliceStore (s: Store) (start: int) (len: int) : Store =
    match s with
    | SFloat a -> SFloat (Array.sub a start len)
    | SInt a -> SInt (Array.sub a start len)
    | SComplex a -> SComplex (Array.sub a start len)
    | SBool a -> SBool (Array.sub a start len)
    | SObj a -> SObj (Array.sub a start len)
    | SNested _ | SRagged _ -> raise (ArrayOpUnsupported "sliceStore: expected a flat store")

let rec private deepCopyStore (s: Store) : Store =
    match s with
    | SFloat a -> SFloat (Array.copy a)
    | SInt a -> SInt (Array.copy a)
    | SComplex a -> SComplex (Array.copy a)
    | SBool a -> SBool (Array.copy a)
    | SObj a -> SObj (Array.copy a)
    | SNested rows -> SNested (rows |> Array.map deepCopyStore)
    | SRagged (rows, lens, offs) -> SRagged (rows |> Array.map deepCopyStore, Array.copy lens, Array.copy offs)

/// Allocate a zeroed FLAT store of `n` cells for an element type. Widens narrow
/// element types (Value.fs gap); non-scalar elements get an SObj of VUnit
/// placeholders (each overwritten by writeCell during materialization).
let storeOfElemType (elemTy: IRType) (n: int) : Store =
    match elemThrough elemTy with
    | Some (ETFloat64 | ETFloat32) -> SFloat (Array.zeroCreate n)
    | Some (ETInt64 | ETInt32) -> SInt (Array.zeroCreate n)
    | Some (ETComplex64 | ETComplex128) -> SComplex (Array.zeroCreate n)
    | Some ETBool -> SBool (Array.zeroCreate n)
    | Some ETString -> SObj (Array.create n (VString ""))
    | Some ETUnit -> SObj (Array.create n VUnit)
    | _ -> SObj (Array.create n VUnit)

/// Pack a row-major flat array of leaf Values into an unboxed store for the
/// given element type (mirrors storeOfElemType's widening).
let storeOfValues (elemTy: IRType) (vs: Value[]) : Store =
    match elemThrough elemTy with
    | Some (ETFloat64 | ETFloat32) -> SFloat (vs |> Array.map toF64v)
    | Some (ETInt64 | ETInt32) -> SInt (vs |> Array.map toI64v)
    | Some (ETComplex64 | ETComplex128) ->
        SComplex (vs |> Array.map (fun v -> let (r, i) = toComplexv v in struct (r, i)))
    | Some ETBool -> SBool (vs |> Array.map toBoolv)
    | _ -> SObj (Array.copy vs)

/// Reshape a FLAT row-major store (length = product of extents) into the nested
/// row structure for `extents.[dim..]`. Rank-1 (innermost) returns the flat leaf.
let rec private reshapeFlat (extents: int64[]) (dim: int) (flat: Store) : Store =
    if dim >= extents.Length - 1 then
        flat
    else
        let outer = int extents.[dim]
        let innerLen =
            [ for d in dim + 1 .. extents.Length - 1 -> int extents.[d] ]
            |> List.fold (*) 1
        SNested (Array.init outer (fun r -> reshapeFlat extents (dim + 1) (sliceStore flat (r * innerLen) innerLen)))

/// Wrap a FLAT row-major store as a rank-N dense BladeArray: rank<=1 keeps the
/// flat store; rank>=2 reshapes into SNested rows (m2-design 1).
let mkDenseArray (elemTy: IRType) (indexTypes: IRIndexType list) (extents: int64[]) (flat: Store) : BladeArray =
    let data = if extents.Length <= 1 then flat else reshapeFlat extents 0 flat
    { ElemType = elemTy; IndexTypes = indexTypes; Extents = extents; Data = data }

/// Convenience allocator: a zeroed dense BladeArray of the given shape (the nest
/// output-allocation path fills it via writeCell). Not in the (1) contract;
/// added so Loops.fs can allocate without hand-building the flat store.
let allocDense (elemTy: IRType) (indexTypes: IRIndexType list) (extents: int64[]) : BladeArray =
    let total = extents |> Array.fold (fun acc e -> acc * int e) 1
    mkDenseArray elemTy indexTypes extents (storeOfElemType elemTy total)

// 2 Access: read / write / peel + shape accessors.

/// Rank (number of dense loop levels) of an array.
let rank (arr: BladeArray) : int = arr.Extents.Length

/// Extent of a dimension (0 for an out-of-range dim).
let extent (arr: BladeArray) (dim: int) : int64 =
    if dim >= 0 && dim < arr.Extents.Length then arr.Extents.[dim] else 0L

/// Read the scalar leaf at a FULL coordinate path (absolute dense row-major
/// coords). For partial (sub-array) reads use peelDim / indexArray instead.
let readCell (arr: BladeArray) (coords: int64 list) : Value =
    let rec go (store: Store) (cs: int64 list) : Value =
        match cs, store with
        | [ i ], SFloat a -> VFloat a.[int i]
        | [ i ], SInt a -> VInt a.[int i]
        | [ i ], SComplex a -> let struct (r, im) = a.[int i] in VComplex (r, im)
        | [ i ], SBool a -> VBool a.[int i]
        | [ i ], SObj a -> a.[int i]
        | i :: rest, SNested rows -> go rows.[int i] rest
        | i :: rest, SRagged (rows, _, _) -> go rows.[int i] rest
        | _ -> raise (InterpPanic ("BL8003", "array read: coordinate/shape mismatch", None, 0))
    go arr.Data coords

/// Write `v` (coerced to the store's cell type) at a FULL coordinate path.
/// Materialization only -- grows nothing; the cell must already exist.
let writeCell (arr: BladeArray) (coords: int64 list) (v: Value) : unit =
    let rec go (store: Store) (cs: int64 list) : unit =
        match cs, store with
        | [ i ], SFloat a -> a.[int i] <- toF64v v
        | [ i ], SInt a -> a.[int i] <- toI64v v
        | [ i ], SComplex a -> let (r, im) = toComplexv v in a.[int i] <- struct (r, im)
        | [ i ], SBool a -> a.[int i] <- toBoolv v
        | [ i ], SObj a -> a.[int i] <- v
        | i :: rest, SNested rows -> go rows.[int i] rest
        | i :: rest, SRagged (rows, _, _) -> go rows.[int i] rest
        | _ -> raise (InterpPanic ("BL8003", "array write: coordinate/shape mismatch", None, 0))
    go arr.Data coords

/// Peel ONE (outermost) dimension at index i. rank-1 (or rank-0) yields the
/// scalar leaf; rank>=2 yields a sub-array VIEW whose Store ALIASES the parent's
/// SNested row (mutation through the peel is visible in the parent -- the C++
/// `{ data[i], extents+1 }` view, m2-design 7).
let peelDim (arr: BladeArray) (i: int64) : Value =
    if arr.Extents.Length <= 1 then
        readCell arr [ i ]
    else
        let childIdx = match arr.IndexTypes with _ :: t -> t | [] -> []
        let childExt = arr.Extents.[1..]
        match arr.Data with
        | SNested rows ->
            VArray { ElemType = arr.ElemType; IndexTypes = childIdx; Extents = childExt; Data = rows.[int i] }
        | SRagged (rows, lens, _) ->
            // A peeled ragged row: its length is per-row (lens[i]), not the
            // parent's placeholder inner extent. The row becomes an ordinary
            // flat rank-1 array (its own leaf store) so every downstream
            // dense op (index, extents(row), reduce, print) works unchanged.
            let rlen = if int i < lens.Length then lens.[int i] else int64 (storeLen rows.[int i])
            VArray { ElemType = arr.ElemType; IndexTypes = childIdx; Extents = [| rlen |]; Data = rows.[int i] }
        | _ ->
            // A rank>=2 array whose backing is unexpectedly flat: fall back to a
            // scalar read (keeps the interpreter total; should not occur for
            // arrays built through mkDenseArray).
            readCell arr [ i ]

/// Dimensional curry: `arr[idx]` -- peel the first dim, yielding a sub-array
/// (rank>=2) or scalar (rank-1). Identical to peelDim (IRCurry, m2-design 6).
let curryArray (arr: BladeArray) (i: int64) : Value = peelDim arr i

// 1 Symmetry-aware canonicalization (canonFold), used by the M2.5
// compact-symmetric read/write it precedes. Returns the sorted (left-
// justified) storage coordinates, the swap parity (0 even / 1 odd; drives
// negate/conjugate on odd), and whether the tuple hits a strict diagonal
// (antisymmetric repeated index: element is zero).

let private sortWithParity (coords: int64[]) : int64[] * int =
    let a = Array.copy coords
    let mutable swaps = 0
    for i in 1 .. a.Length - 1 do
        let mutable j = i
        while j > 0 && a.[j - 1] > a.[j] do
            let t = a.[j - 1] in a.[j - 1] <- a.[j]
            a.[j] <- t
            swaps <- swaps + 1
            j <- j - 1
    (a, swaps % 2)

/// Canonicalize an index tuple over a compact symmetry group.
///   SymNone      -> (coords, 0, false)  (no canonicalization)
///   SymSymmetric -> (sorted, parity, false)
///   SymHermitian -> (sorted, parity, false)  (parity drives conjugate-on-swap)
///   SymAntisym   -> (sorted, parity, isZero) (isZero when any index repeats)
///   SymWreath    -> REFUSED: canonicalization is a fold of per-level sorts,
///                  not one flat sort (that would map distinct orbits onto
///                  the same tuple with a meaningless parity), and the
///                  reference (OrbRank.canonOrb) needs the level list this
///                  signature lacks -- refuse loudly rather than read a
///                  plausible tuple from the wrong cell.
let canonFold (sym: SymmetryClass) (coords: int64[]) : int64[] * int * bool =
    match sym with
    | SymNone -> (Array.copy coords, 0, false)
    | SymWreath ->
        failwith (Blade.IR.orbitStorageUnsupported "compact read (canonFold)" [])
    | SymSymmetric
    | SymHermitian ->
        let sorted, parity = sortWithParity coords
        (sorted, parity, false)
    | SymAntisymmetric ->
        let hasRepeat = (coords |> Array.distinct).Length <> coords.Length
        let sorted, parity = sortWithParity coords
        (sorted, parity, hasRepeat)

/// Left-justify a sorted index tuple to compact storage coords (mirrors
/// nested_array_utilities::canon_left_justify, cpp:564): c[0]=p[0];
/// c[k] = p[k] - p[k-1] - (strict ? 1 : 0). `strict` is true for an
/// antisymmetric group (each successive row one shorter: the dropped diagonal).
let canonLeftJustify (sorted: int64[]) (strict: bool) : int64[] =
    let r = sorted.Length
    let c = Array.zeroCreate r
    if r > 0 then c.[0] <- sorted.[0]
    for k in 1 .. r - 1 do
        c.[k] <- sorted.[k] - sorted.[k - 1] - (if strict then 1L else 0L)
    c

// 1b Compact (symmetric / antisymmetric / Hermitian) output allocation.
//
// A compact BladeArray keeps the logical Extents (so reads/prints know the
// true shape) plus the output IndexTypes; only Data is a left-justified
// nested skeleton whose rows shrink within each symmetry group, mirroring
// nested_array_utilities::allocate/build_skeleton (cpp:185-244): at flattened
// depth d inside a group (symmVec[d-1]=symmVec[d]) a row holds
// extents[d]-lastIndex cells, lastIndex threading the parent index (+strict
// seed for antisym). The nest writes at the raw left-justified loop coords
// (interpretNest's storage coords), so writeCell navigates it directly, and
// the sym printer reads the same raw coords (canonical by construction).

/// symmVec/strictVec come from IR.buildSymmVecWithStrict on the OUTPUT type:
///   symmVec[d]   = storage group number at flattened dim d (adjacent-equal =
///                  one shrinking group),  strictVec[d] = 1 if antisymmetric.
let allocCompact (elemTy: IRType) (idxTys: IRIndexType list) (extents: int64[])
                 (symmVec: int[]) (strictVec: int[]) : BladeArray =
    let rank = extents.Length
    let rec build (depth: int) (lastIndex: int64) : Store =
        let inGroupWithPrev = depth > 0 && symmVec.[depth - 1] = symmVec.[depth]
        let n =
            let raw = if inGroupWithPrev then extents.[depth] - lastIndex else extents.[depth]
            if raw < 0L then 0L else raw
        if depth >= rank - 1 then
            storeOfElemType elemTy (int n)
        else
            let nextInGroup = symmVec.[depth] = symmVec.[depth + 1]
            let strictOff = int64 strictVec.[depth]
            SNested (Array.init (int n) (fun i ->
                let childLast =
                    if nextInGroup then
                        if inGroupWithPrev then int64 i + lastIndex + strictOff
                        else int64 i + strictOff
                    else 0L
                build (depth + 1) childLast))
    let data =
        if rank = 0 then storeOfElemType elemTy 1
        elif rank = 1 then storeOfElemType elemTy (int extents.[0])
        else build 0 0L
    { ElemType = elemTy; IndexTypes = idxTys; Extents = extents; Data = data }

// OrbIdx (iterated-wreath) pools (docs/plan-orbit-index-types.md 4, 9 step 4).
//
// INVARIANT: a wreath array is a flat pool of exactly
// `OrbRank.cellCountChecked levels n` cells in `visitStream` order (== the
// C++ `orb_visit` order == ascending-lex canonical). Deliberately not the
// shrinking-row SNested skeleton `allocCompact` builds: a wreath's rows
// shrink per level, so no single simplex describes them, and a skeleton
// shaped like one would put every cell at a plausible-but-wrong offset.
//
// The record keeps its honest raw-axis Extents (prod(ri) copies of n) so the
// logical shape reads true; only Data is flat. Any path that would navigate
// `Extents` as a nested store is refused (forEachStorageCell / emitSymAware);
// indexArray/readCompact go through the flat read (2) instead.

/// Zero value for a scalar element type (implicit-zero of a strict-diagonal
/// antisymmetric access, and of a wreath zero-set tuple; mirrors `return T()`
/// in the lazy compact reader). Defined here rather than beside the other
/// compact-read helpers below because the wreath read needs it first.
let zeroOfElemTy (elemTy: IRType) : Value =
    match elemThrough elemTy with
    // WIDTH-EXACT ZEROS. A fold seeded with a double zero stays double: the
    // binop promotion rule takes the wider operand, so `gram` over Float32
    // operands accumulated every step in Float64 and rounded once at the
    // store, while the compiled loop rounds after every product and every
    // add (`float __gacc += float * float`). On cancellation-sensitive data
    // (16777216 + 1 - 16777216) the two answered 1 and 0.
    | Some ETFloat64 -> VFloat 0.0
    | Some ETFloat32 -> VFloat32 0.0f
    | Some ETInt64 -> VInt 0L
    | Some ETInt32 -> VInt32 0
    | Some (ETComplex64 | ETComplex128) -> VComplex (0.0, 0.0)
    | Some ETBool -> VBool false
    | _ -> VFloat 0.0

/// Round a value to the WIDTH of an element type: Float32 to a `VFloat32`,
/// Complex64 to a complex with single-precision parts (the interpreter has
/// no single-precision complex value, so the parts are rounded through
/// float32 and carried as doubles); everything else is returned as is.
/// The stores are double-backed and `readCell` yields doubles, so a fold
/// that must match a compiled `float` accumulation narrows every operand
/// and every partial result through this (gramArray, matmulArray).
let narrowToElem (elemTy: IRType) : Value -> Value =
    match elemThrough elemTy with
    | Some ETFloat32 ->
        (fun v ->
            match v with
            | VFloat32 _ -> v
            | _ -> VFloat32 (float32 (toF64v v)))
    | Some ETComplex64 ->
        (fun v ->
            let (r, im) = toComplexv v
            VComplex (float (float32 r), float (float32 im)))
    | _ -> id

/// True iff any slot of this record list is a depth >= 2 OrbIdx class.
let hasWreath (idxTys: IRIndexType list) : bool =
    idxTys |> List.exists (fun ix -> ix.Symmetry = SymWreath)

/// Allocate a zeroed wreath pool: `cellCountChecked` cells, flat, in stream
/// order. The count comes from the same checked fold `IRStorage.classifyOutputStorage`
/// sized the compiled pool with, so the two backends cannot allocate
/// differently (7.2: the failure to guard is a silent wraparound).
let allocWreath (elemTy: IRType) (idxTys: IRIndexType list)
                (levels: (int * bool) list) (n: int64) : BladeArray =
    let cells =
        match Blade.OrbRank.cellCountChecked (Blade.IR.orbRankLevels levels) n with
        | Ok c -> c
        | Error detail ->
            raise (ArrayOpUnsupported
                     ($"OrbIdx{(Blade.IR.ppOrbitLevels levels)} at extent {n}: cell count -- {detail}"))
    let axes = levels |> List.fold (fun a (r, _) -> a * r) 1
    { ElemType = elemTy
      IndexTypes = idxTys
      Extents = Array.create axes n
      Data = storeOfElemType elemTy (int cells) }

/// Number of stored cells of a wreath pool (its flat store length).
let wreathCellCount (arr: BladeArray) : int = storeLen arr.Data

/// Read the pool cell at a canonical tuple, by its `orbRank` position -- the
/// read the traversal nest performs on a wreath input, where the tuple comes
/// straight out of `visitStream` so it is canonical by construction (character
/// +1). Deliberately does not fold: a canonical tuple is a fixed point, so
/// folding would be dead work and would hide a stream/rank disagreement. The
/// mirrored read is `wreathReadAny` below.
let wreathReadCanonical (arr: BladeArray) (levels: (int * bool) list) (n: int64)
                        (tuple: int list) : Value =
    match Blade.OrbRank.orbRank (Blade.IR.orbRankLevels levels) (int n) tuple with
    | Error detail ->
        raise (ArrayOpUnsupported
                 (sprintf "OrbIdx%s canonical read at (%s): %s"
                          (Blade.IR.ppOrbitLevels levels)
                          (tuple |> List.map string |> String.concat ",") detail))
    | Ok r -> readCell arr [ r ]

/// docs/plan-orbidx-decompaction.md 2's read at an arbitrary raw tuple:
///
///     dense[t] = 0                                if canon(t) is zero-set
///              = chi(t) * pool[orbRank(canon(t))]  otherwise
///
/// The interpreter twin of the emitted `orb_read<T, Levels...>`. Does not
/// re-derive the semantics: `OrbRank.orbReadPlan` (built on the same core
/// `OrbRank.orbRead`, pinned by `blade test orbrank`) answers which cell and
/// which character; the only thing added here is the cell type (`Value`
/// negate instead of int64).
///
/// Two failure modes stay apart: zero-set -> the element type's zero (a `-`
/// level genuinely stores nothing there); out-of-domain (bad digit/rank/pool
/// size/class) -> BL8003, so an off-by-one never aliases onto a structural
/// zero. Out-of-domain is unreachable for a typechecker-admitted tuple except
/// an out-of-range coordinate (the same unchecked hazard readCell has), and
/// this path diagnoses it instead of reading a neighbouring cell.
let wreathReadAny (arr: BladeArray) (levels: (int * bool) list) (n: int64)
                  (tuple: int list) : Value =
    match Blade.OrbRank.orbReadPlan "OrbIdx read" (Blade.IR.orbRankLevels levels)
                                    (int n) (storeLen arr.Data) tuple with
    | Error detail ->
        raise (InterpPanic ("BL8003",
                            $"""OrbIdx{(Blade.IR.ppOrbitLevels levels)} read at ({(tuple |> List.map string |> String.concat ",")}): {detail}""",
                            None, 0))
    | Ok Blade.OrbRank.OrbZeroCell -> zeroOfElemTy arr.ElemType
    | Ok (Blade.OrbRank.OrbPoolCell (i, chi)) ->
        let v = readCell arr [ int64 i ]
        if chi < 0 then N.evalUnaryOp IRNeg v else v

/// Write the pool cell at stream position `k` (the traversal nest's own
/// counter). Position-addressed rather than tuple-addressed precisely because
/// the nest already knows the position: `visitStream` yields cells in order, so
/// the writer bumps a counter exactly as the C++ visitor's `linear_index` does.
let wreathWriteAt (arr: BladeArray) (k: int64) (v: Value) : unit =
    writeCell arr [ k ] v

// 2 General indexing (IRIndex, plain dense) + poly-index.

let private hasSymmetry (idxTys: IRIndexType list) : bool =
    idxTys
    |> List.exists (fun idx ->
        idx.Symmetry = SymSymmetric
        || idx.Symmetry = SymAntisymmetric
        || idx.Symmetry = SymHermitian)

/// Apply a compact read transform at the given swap parity (mirrors
/// nested_array_utilities::canon_transform + ReadTransform, cpp:573):
///   Symmetric  -> Identity;   Antisymmetric -> NegateOnSwap;
///   Hermitian  -> ConjugateOnSwap (identity on reals -- conj_scalar).
let private applyReadTransform (sym: SymmetryClass) (parity: int) (v: Value) : Value =
    if parity = 0 then v
    else
        match sym with
        | SymAntisymmetric -> N.evalUnaryOp IRNeg v
        | SymHermitian -> N.evalUnaryOp IRConj v
        | _ -> v

/// Canonical (compact) random read: given a FULL logical coordinate list covering
/// every dimension, fold each compact group (canon_fold: sort + swap parity +
/// strict-diagonal zero-guard), left-justify to storage coords, read the raw
/// stored cell, and chain the per-group read transforms. Mirrors
/// CodeGen.renderIndexExpr's lazyCompactRead (CodeGen.fs:1463-1538) +
/// nested_array_utilities. Plain (SymNone) slots pass their index through.
let readCompact (arr: BladeArray) (logicalCoords: int64 list) : Value =
    // A sole wreath slot is the (2) read, delegated whole -- the per-slot fold
    // below cannot express it (a wreath pool is flat, not a nest of
    // left-justified rows). Routing it here is what makes `decompactArray`
    // work over a wreath source unchanged. A wreath COMBINED with other
    // slots still refuses: the mixed case has no pool layout at all.
    match arr.IndexTypes with
    | [ ix ] when ix.Symmetry = SymWreath ->
        let n = if arr.Extents.Length >= 1 then arr.Extents.[0] else 0L
        // int64 -> int is the one narrowing here. Range-check BEFORE
        // truncating: 2^32 + 1 truncates to 1, which the storage gate would
        // then accept as a valid coordinate -- the silent aliased read this
        // check prevents.
        (match logicalCoords |> List.tryFind (fun c -> c < 0L || c >= n) with
         | Some bad ->
             raise (InterpPanic ("BL8003",
                                 $"OrbIdx{(Blade.IR.ppOrbitLevels (Blade.IR.orbitLevelsOf ix))} read: coordinate {bad} outside [0,{n})",
                                 None, 0))
         | None ->
             wreathReadAny arr (Blade.IR.orbitLevelsOf ix) n
                           (logicalCoords |> List.map int))
    | _ ->
    arr.IndexTypes |> List.iter (fun ix ->
        if ix.Symmetry = SymWreath then
            failwith (Blade.IR.orbitStorageUnsupported "compact read of a wreath group combined with \
other index slots (readCompact)"
                                                       (Blade.IR.orbitLevelsOf ix)))
    let coords = Array.ofList logicalCoords
    let storage = ResizeArray<int64>()
    let mutable transforms = []          // (parity, sym) per compact group, slot order
    let mutable isZero = false
    let mutable cursor = 0
    for ix in arr.IndexTypes do
        let a = max 1 ix.Rank
        let these = Array.sub coords cursor a
        cursor <- cursor + a
        if ix.Symmetry <> SymNone && a >= 2 then
            let (sorted, parity, z) = canonFold ix.Symmetry these
            if z then isZero <- true
            let strict = ix.Symmetry = SymAntisymmetric
            for c in canonLeftJustify sorted strict do storage.Add c
            transforms <- transforms @ [ (parity, ix.Symmetry) ]
        else
            for c in these do storage.Add c
    if isZero then zeroOfElemTy arr.ElemType
    else
        let raw = readCell arr (List.ofSeq storage)
        transforms |> List.fold (fun v (p, sym) -> applyReadTransform sym p v) raw

/// Plain dense random read through an index list: chained peels (row-major).
/// A full index list yields a scalar; a partial list yields a sub-array view.
/// Out-of-range indices panic BL8003 (matching blade_rt on the abort probes).
/// A compact (symmetric/antisym/Hermitian) array with a FULL index list routes
/// to the canonical reader (readCompact); a partial (sub-array) compact read is
/// still gated (M3+).
let indexArray (arr: BladeArray) (indices: Value list) : Value =
    // A wreath pool's store is flat and `hasSymmetry` is false for it, so
    // without this arm the dense peel below would walk `Extents` as if
    // nested and read a plausible cell from the wrong place. Route the
    // full-arity form to (2) (canonOrb fold + character + rank), the twin of
    // the emitted `orb_read`; a partial list has no answer (rows shrink per
    // level, so no residual class describes a fibre) -- the typechecker
    // refuses it before here, this is the backstop.
    if hasWreath arr.IndexTypes then
        let ix = arr.IndexTypes |> List.find (fun ix -> ix.Symmetry = SymWreath)
        let totalRank = arr.IndexTypes |> List.sumBy (fun ix -> max 1 ix.Rank)
        if indices.Length = totalRank then
            readCompact arr (indices |> List.map toI64v)
        else
            raise (ArrayOpUnsupported
                     (sprintf "index: OrbIdx%s spans %d raw axes and takes exactly that many flat \
subscripts; a partial (sub-array) read of a wreath pool has no residual class"
                              (Blade.IR.ppOrbitLevels (Blade.IR.orbitLevelsOf ix)) totalRank))
    elif hasSymmetry arr.IndexTypes then
        let totalRank = arr.IndexTypes |> List.sumBy (fun ix -> max 1 ix.Rank)
        if indices.Length = totalRank then
            readCompact arr (indices |> List.map toI64v)
        else
            raise (ArrayOpUnsupported "index: partial (sub-array) read of a compact symmetric array (M3+)")
    else
    let rec go (cur: Value) (idxs: Value list) : Value =
        match idxs with
        | [] -> cur
        | iv :: rest ->
            match cur with
            | VArray a ->
                let i = toI64v iv
                if i < 0L || a.Extents.Length < 1 || i >= a.Extents.[0] then
                    raise (InterpPanic ("BL8003", "array index out of bounds", None, 0))
                go (peelDim a i) rest
            | _ -> raise (InterpPanic ("BL8003", "indexing a non-array value", None, 0))
    go (VArray arr) indices

/// Poly-pack indexing (IRPolyIndex): a tuple pack -> get<i>; an array pack -> peel.
let polyIndex (pack: Value) (i: int64) : Value =
    match pack with
    | VTuple els when i >= 0L && int i < els.Length -> els.[int i]
    | VArray a -> peelDim a i
    | _ -> raise (ArrayOpUnsupported "IRPolyIndex: pack is neither a tuple nor an array")

// 3 Virtual arrays.

/// Value-space descriptor for a no-store virtual source (m2-design 0.10, 3).
type VirtualKind =
    /// range<I> (+offset): element at loop index i is `i + offset` (offset 0
    /// for plain `0..N`). int64 throughout so `i - 1` at i=0 is -1, not an
    /// unsigned wrap (CodeGen.fs:2958-2964).
    | VRange of offset: int64
    /// reverse<I>: element at loop index i is `extent - 1 - i`.
    | VReverse
    /// blocked<I>: iteration is blocked but the element value is still the
    /// flat index i. Best-effort: no compiled-binary pin yet (blocked is in
    /// the mpi domain-decomposition slice, outside the dense M2 corpus).
    | VBlocked of blockSize: int64

/// The element value a virtual source produces at loop index i. Always
/// Int64-typed, matching the C++ int64_t kernel-param binding.
let virtualElem (vk: VirtualKind) (extent: int64) (i: int64) : Value =
    match vk with
    | VRange off -> VInt (i + off)
    | VReverse -> VInt (extent - 1L - i)
    | VBlocked _ -> VInt i

// 4 Array-literal construction (IRArrayLit, incl. nested rank>=2).
//
// Core.fs's IRArrayLit arm evaluates the literal's top-level element exprs to
// Values, then calls here. Rank-1: scalar leaves packed into a flat store.
// Rank>=2: each element is a VArray row (covers "rows of computed arrays"
// too, CodeGen.fs:4254); outer extent = element count, inner extents from the
// first row's shape (rectangular). Ragged / DepIdx literals are M2.7.

let arrayLitFromValues (arrType: IRArrayType) (elems: Value list) : BladeArray =
    let elemTy = arrType.ElemType
    let idxTys = arrType.IndexTypes
    let isRagged =
        Blade.CodeGen.isRaggedArrayType arrType || Blade.CodeGen.isDepIdxArrayType arrType
    match elems with
    | (VArray _) :: _ when isRagged ->
        // Ragged / DepIdx literal (heterogeneous per-row lengths). CSR layout
        // mirroring CodeGen.genArrayLiteral's Ragged<T> emission (CodeGen.fs:
        // 4485-4522): each row kept as its own leaf store; `lens` is the
        // actual per-row length (the rectangular assumption -- inner extent
        // from the first row -- caused r(2,3) to read past a short row,
        // BL8003); `offsets` is the exclusive prefix-sum, length nRows+1.
        // rank = 2; logical Extents' inner slot is the max row length only
        // (every per-row bound is actually served from `lens` by peelDim).
        let rows =
            elems
            |> List.map (function
                | VArray a -> a.Data
                | v -> raise (ArrayOpUnsupported (sprintf "ragged literal: non-array row (%A)" v)))
            |> Array.ofList
        let lens =
            elems
            |> List.map (function
                | VArray a -> (if a.Extents.Length >= 1 then a.Extents.[0] else 0L)
                | _ -> 0L)
            |> Array.ofList
        let offsets = Array.zeroCreate (rows.Length + 1)
        for r in 0 .. rows.Length - 1 do offsets.[r + 1] <- offsets.[r] + lens.[r]
        let innerExtent = if lens.Length = 0 then 0L else Array.max lens
        { ElemType = elemTy
          IndexTypes = idxTys
          Extents = [| int64 rows.Length; innerExtent |]
          Data = SRagged (rows, lens, offsets) }
    | (VArray _) :: _ when
            idxTys |> List.exists (fun ix ->
                ix.Rank >= 2 &&
                (match ix.Symmetry with
                 | SymSymmetric | SymAntisymmetric | SymHermitian -> true
                 | SymNone | SymWreath -> false)) ->
        // Compact literal (TypeCheck.checkCompactArrayLit; CodeGen's compact
        // genArrayLiteral branch): rows are the left-justified simplex, so
        // they shrink and the first is not the axis shape -- extents come
        // from the index records instead (the rectangular arm's first-row
        // approach is off by one for a strict/antisym group).
        let rows =
            elems
            |> List.map (function
                | VArray a -> a.Data
                | v -> raise (ArrayOpUnsupported (sprintf "compact literal: non-array row (%A)" v)))
            |> Array.ofList
        let extents =
            idxTys
            |> List.collect (fun ix ->
                let e =
                    match ix.Extent with
                    | IRLit (IRLitInt n) -> n
                    | _ -> raise (ArrayOpUnsupported "compact literal: non-static extent")
                List.replicate (max 1 ix.Rank) e)
            |> Array.ofList
        { ElemType = elemTy; IndexTypes = idxTys; Extents = extents; Data = SNested rows }
    | (VArray first) :: _ ->
        // rank>=2: each element is a row; nest the rows' stores (shared -- the
        // rows are freshly-evaluated and owned by this literal).
        let rows =
            elems
            |> List.map (function
                | VArray a -> a.Data
                | v -> raise (ArrayOpUnsupported (sprintf "array literal: mixed row/scalar elements (%A)" v)))
            |> Array.ofList
        let extents = Array.append [| int64 rows.Length |] first.Extents
        { ElemType = elemTy; IndexTypes = idxTys; Extents = extents; Data = SNested rows }
    | _ ->
        // rank-1 (or empty): scalar leaves packed flat.
        let vs = Array.ofList elems
        { ElemType = elemTy
          IndexTypes = idxTys
          Extents = [| int64 vs.Length |]
          Data = storeOfValues elemTy vs }

/// IRReplicate(count, body): a rank-added array of `count` copies of body.
/// Rows are deep-copied so the copies don't alias. (replicate/001.)
let replicateArray (count: int64) (body: Value) : Value =
    let c = int count
    match body with
    | VArray a ->
        let rows = Array.init c (fun _ -> deepCopyStore a.Data)
        VArray
            { ElemType = a.ElemType
              IndexTypes = a.IndexTypes
              Extents = Array.append [| count |] a.Extents
              Data = SNested rows }
    | scalar ->
        match N.scalarElem scalar with
        | Some et ->
            let elemTy = IRTScalar et
            VArray
                { ElemType = elemTy
                  IndexTypes = []
                  Extents = [| count |]
                  Data = storeOfValues elemTy (Array.create c scalar) }
        | None -> raise (ArrayOpUnsupported "replicate: body is neither a scalar nor an array")

/// `A <|:> B` dense-left allocated-fallback (fallback_copy<T,N>, nested_array_
/// utilities.hpp:106; genFallbackMaterialize dense arm, CodeGen.fs:9410): the
/// per-curry-level rule `dst[i..] = a ? a[i..] : b[i..]` keyed on A's
/// allocation. Every Blade array is fully allocated and the interpreter has
/// no partial-depth null notion, so the mask is all-present: result is a
/// fresh copy of A (`A <|:> B = A`, the distinguisher from value-keyed `<|>`,
/// which replaces zeros with B). B is unused.
let fallbackDense (a: BladeArray) : BladeArray = copyBladeArray a

// 5 Reductions.

/// Row-major flat leaves of an array (rank-1: its elements).
let private flatLeaves (arr: BladeArray) : Value[] =
    let out = ResizeArray<Value>()
    let rec loop (dim: int) (acc: int64 list) =
        if dim = arr.Extents.Length then out.Add(readCell arr (List.rev acc))
        else
            let e = arr.Extents.[dim]
            let mutable i = 0L
            while i < e do
                loop (dim + 1) (i :: acc)
                i <- i + 1L
    loop 0 []
    out.ToArray()

/// reduce(arr, fold[, init]) over a materialized array (genReduceBinding
/// parity, m2-design 5). Without init: seed = arr[0], fold i=1..n-1; empty
/// panics BL8003 "reduce: empty array, no reduction possible" (matches
/// blade_rt). With init: seed = init, fold all i from 0; empty result is init.
let reduceArray (arr: BladeArray) (fold: Value -> Value -> Value) (init: Value option) : Value =
    let leaves = flatLeaves arr
    match init with
    | Some seed ->
        let mutable acc = seed
        for v in leaves do
            acc <- fold acc v
        acc
    | None ->
        if leaves.Length = 0 then
            raise (InterpPanic ("BL8003", "reduce: empty array, no reduction possible", None, 0))
        let mutable acc = leaves.[0]
        for k in 1 .. leaves.Length - 1 do
            acc <- fold acc leaves.[k]
        acc

/// prodsum(x1..xk) = sum_t prod_l xl[t] over rank-1 equal-extent arrays; seed
/// 0; empty extent = 0 (IRProdSum, m2-design 0.8). Uses Numerics for
/// bit-exact promotion; the seed's type follows the first arg's element type.
let prodSum (args: BladeArray list) : Value =
    match args with
    | [] -> VInt 0L
    | first :: _ ->
        let n = if first.Extents.Length >= 1 then int first.Extents.[0] else 0
        let zero =
            match elemThrough first.ElemType with
            | Some (ETFloat64 | ETFloat32) -> VFloat 0.0
            | Some (ETComplex64 | ETComplex128) -> VComplex (0.0, 0.0)
            | _ -> VInt 0L
        let mutable sum = zero
        for t in 0 .. n - 1 do
            let mutable prod : Value option = None
            for a in args do
                let v = readCell a [ int64 t ]
                prod <-
                    match prod with
                    | None -> Some v
                    | Some p -> Some(N.evalBinOp IRMul p v)
            match prod with
            | Some p -> sum <- N.evalBinOp IRAdd sum p
            | None -> ()
        sum

// 6 Eager set / reshape ops (dense rank-1 unless noted). These mostly serve
// the SQL-ish categories outside the M2 loop-object corpus; first-occurrence
// order (unique/intersect/union) and a stable sort are pinned to CodeGen's
// semantics. Higher-rank forms beyond transpose are later.

let private cmpValues (a: Value) (b: Value) : int =
    match a, b with
    | VInt x, VInt y -> compare x y
    | VInt32 x, VInt32 y -> compare x y
    | VBool x, VBool y -> compare x y
    | VString x, VString y -> System.String.CompareOrdinal(x, y)
    | _ -> compare (toF64v a) (toF64v b)

let private eqValues (a: Value) (b: Value) : bool = cmpValues a b = 0

let private elems1 (arr: BladeArray) : Value list =
    if arr.Extents.Length <> 1 then
        raise (ArrayOpUnsupported "eager op: only rank-1 arrays are supported")
    [ for i in 0L .. arr.Extents.[0] - 1L -> readCell arr [ i ] ]

let private mkRank1 (elemTy: IRType) (idxTys: IRIndexType list) (vs: Value list) : BladeArray =
    let a = Array.ofList vs
    { ElemType = elemTy
      IndexTypes = idxTys
      Extents = [| int64 a.Length |]
      Data = storeOfValues elemTy a }

/// mask(arr, pred): keep elements where pred(v) is truthy (first-occurrence
/// order preserved). This is the OLD filtering `mask`, not what CodeGen emits
/// anymore (that's the Bool presence array below, `maskPresence`); kept only
/// so nothing that still references it breaks. The IR arm routes to
/// `maskPresence`, not here.
let maskArray (arr: BladeArray) (pred: Value -> Value) : BladeArray =
    let kept = elems1 arr |> List.filter (fun v -> toBoolv (pred v))
    mkRank1 arr.ElemType arr.IndexTypes kept

/// mask(arr, pred): current semantics -- a rank-1 Bool presence array over
/// arr's own index space, `m[i] = pred(arr[i])`. One pass, no compaction, no
/// reorder, no value copy: compaction belongs to `compound(A, m)`, iteration
/// to `range<CompoundIdx<m>>`. Byte-verified against `materializeMaskForm`
/// (CodeGen.fs:2245): `extents(m)` is the SOURCE extent, not a filtered
/// cardinality; rank-1 only (CodeGen emits `#error` for rank>1). Carries the
/// source's IndexTypes (so `compound(A, m)` sees the shared index space);
/// element type Bool. `pred` is the caller's kernel closure.
let maskPresence (arr: BladeArray) (pred: Value -> Value) : BladeArray =
    if arr.Extents.Length <> 1 then
        raise (ArrayOpUnsupported "mask over a rank>1 array (rank-1 only; mirrors CodeGen's #error)")
    let n = arr.Extents.[0]
    let bools = Array.init (int n) (fun i -> toBoolv (pred (readCell arr [ int64 i ])))
    { ElemType = IRTScalar ETBool
      IndexTypes = arr.IndexTypes
      Extents = [| n |]
      Data = SBool bools }

/// unique(arr): dedup, first-occurrence order.
let uniqueArray (arr: BladeArray) : BladeArray =
    let seen = ResizeArray<Value>()
    for v in elems1 arr do
        if not (seen |> Seq.exists (eqValues v)) then seen.Add v
    mkRank1 arr.ElemType arr.IndexTypes (List.ofSeq seen)

/// sort(arr, key): STABLE ascending sort by key(v) (List.sortWith is stable).
let sortArray (arr: BladeArray) (key: Value -> Value) : BladeArray =
    let sorted = elems1 arr |> List.sortWith (fun a b -> cmpValues (key a) (key b))
    mkRank1 arr.ElemType arr.IndexTypes sorted

/// intersect(a, b): a's elements that also appear in b, first-occurrence, deduped.
let intersectArray (a: BladeArray) (b: BladeArray) : BladeArray =
    let bvals = elems1 b
    let out = ResizeArray<Value>()
    for v in elems1 a do
        if (bvals |> List.exists (eqValues v)) && not (out |> Seq.exists (eqValues v)) then
            out.Add v
    mkRank1 a.ElemType a.IndexTypes (List.ofSeq out)

/// union(a, b): a's elements then b's not-already-present, first-occurrence.
let unionArray (a: BladeArray) (b: BladeArray) : BladeArray =
    let out = ResizeArray<Value>()
    let add v = if not (out |> Seq.exists (eqValues v)) then out.Add v
    for v in elems1 a do add v
    for v in elems1 b do add v
    mkRank1 a.ElemType a.IndexTypes (List.ofSeq out)

/// transpose(arr, d1, d2): swap two dimensions (any rank; new dense array).
let transposeArray (arr: BladeArray) (d1: int) (d2: int) : BladeArray =
    let r = arr.Extents.Length
    if d1 < 0 || d2 < 0 || d1 >= r || d2 >= r then
        raise (ArrayOpUnsupported "transpose: dimension index out of range")
    let newExtents = Array.copy arr.Extents
    let t = newExtents.[d1] in newExtents.[d1] <- newExtents.[d2]
    newExtents.[d2] <- t
    let out = allocDense arr.ElemType arr.IndexTypes newExtents
    let rec loop (dim: int) (acc: int64 list) =
        if dim = r then
            let src = readCell arr (List.rev acc)
            let dst = List.rev acc |> List.toArray
            let tmp = dst.[d1] in dst.[d1] <- dst.[d2]
            dst.[d2] <- tmp
            writeCell out (List.ofArray dst) src
        else
            let mutable i = 0L
            while i < arr.Extents.[dim] do
                loop (dim + 1) (i :: acc)
                i <- i + 1L
    loop 0 []
    out

/// stack(A1..An): fresh LEADING axis of extent n over n same-shaped arrays,
/// so `stack(A,B,C)(k)` selects array k (formalism 2.6). Rank r -> r+1.
/// Mirrors CodeGen.materializeStackForm: a fresh dense pool plus a per-source
/// element copy (never aliasing). Output IndexTypes reuse the child's (the
/// extra leading Idx<n> is not reflected there, matching forceSequence).
let stackArrays (arrs: BladeArray list) : BladeArray =
    match arrs with
    | [] -> raise (ArrayOpUnsupported "stack: no operands")
    | first :: _ ->
        let srcRank = first.Extents.Length
        let outExtents = Array.append [| int64 arrs.Length |] first.Extents
        let out = allocDense first.ElemType first.IndexTypes outExtents
        arrs |> List.iteri (fun k src ->
            let rec loop (dim: int) (acc: int64 list) =
                if dim = srcRank then
                    let coords = List.rev acc
                    writeCell out (int64 k :: coords) (readCell src coords)
                else
                    let mutable i = 0L
                    while i < src.Extents.[dim] do
                        loop (dim + 1) (i :: acc)
                        i <- i + 1L
            loop 0 [])
        out

/// join(A1..An, d): concatenate along dimension d (formalism 2.6) -- rank is
/// preserved, extents[d] is the sum of the operands' extents[d], every other
/// axis agrees. Mirrors CodeGen.materializeJoinForm's running-offset copy.
let joinArrays (arrs: BladeArray list) (dim: int) : BladeArray =
    match arrs with
    | [] -> raise (ArrayOpUnsupported "join: no operands")
    | first :: _ ->
        let r = first.Extents.Length
        if dim < 0 || dim >= r then
            raise (ArrayOpUnsupported "join: dimension index out of range")
        let outExtents = Array.copy first.Extents
        outExtents.[dim] <- arrs |> List.sumBy (fun a -> a.Extents.[dim])
        let out = allocDense first.ElemType first.IndexTypes outExtents
        let mutable offset = 0L
        for src in arrs do
            let rec loop (dim2: int) (acc: int64 list) =
                if dim2 = r then
                    let coords = List.rev acc
                    let dst = coords |> List.mapi (fun d i -> if d = dim then i + offset else i)
                    writeCell out dst (readCell src coords)
                else
                    let mutable i = 0L
                    while i < src.Extents.[dim2] do
                        loop (dim2 + 1) (i :: acc)
                        i <- i + 1L
            loop 0 []
            offset <- offset + src.Extents.[dim]
        out

// 6.5 Symmetry producers: decompact / gram / negate / conjugate. Eager
// producers over compact/dense storage. Each mirrors a CodeGen
// materialize*Form emitter (CodeGen.fs: 2477 decompact, 2806 negate/conjugate,
// 2924 gram), byte-verified against the compiled binary.

/// Apply `f` to every scalar leaf of a store, producing a fresh store of the
/// SAME shape (preserving the SNested/SRagged skeleton). Backs the whole-array
/// negate/conjugate contiguous-pool transforms.
let rec private mapStoreLeaves (f: Value -> Value) (s: Store) : Store =
    match s with
    | SFloat a -> SFloat (a |> Array.map (fun x -> toF64v (f (VFloat x))))
    | SInt a -> SInt (a |> Array.map (fun x -> toI64v (f (VInt x))))
    | SComplex a ->
        SComplex (a |> Array.map (fun (struct (r, i)) ->
            let (nr, ni) = toComplexv (f (VComplex (r, i))) in struct (nr, ni)))
    | SBool a -> SBool (a |> Array.map (fun x -> toBoolv (f (VBool x))))
    | SObj a -> SObj (a |> Array.map f)
    | SNested rows -> SNested (rows |> Array.map (mapStoreLeaves f))
    | SRagged (rows, lens, offs) -> SRagged (rows |> Array.map (mapStoreLeaves f), Array.copy lens, Array.copy offs)

/// Whole-array elementwise negate/conjugate (IRArrayNegate / IRArrayConjugate,
/// CodeGen.fs:2806). Type- AND storage-shape-PRESERVING: negate_pool /
/// conjugate_pool run a flat transform over the contiguous pool, so the result
/// carries the source's exact IndexTypes/Extents/skeleton with every stored
/// cell transformed. Antisym intra-group transpose reaches negate; Hermitian
/// adjoint reaches conjugate; conj on a real element is the identity.
let negateConjugateArray (conj: bool) (src: BladeArray) : BladeArray =
    let f = if conj then N.evalUnaryOp IRConj else N.evalUnaryOp IRNeg
    { src with Data = mapStoreLeaves f src.Data }

/// Enumerate every STORED cell of a compact/dense storage shape (`idxTys` +
/// `extents`), invoking `visit storageCoords logicalCoords`. Mirrors
/// emitSymAware's left-justified storage walk exactly -- per-dim bound
/// `extents[d] - sum(prior group storage coords) - (#prior)*strictConst` --
/// reconstructing the logical tuple via canon_left_justify's inverse
/// (p_k = p_{k-1} + s_k + strict; plain dims: storage == logical). Used by
/// decompact to walk its output and read the source at each logical coordinate.
let private forEachStorageCell (idxTys: IRIndexType list) (extents: int64[])
                               (visit: int64 list -> int64 list -> unit) : unit =
    // Per-flattened-dim descriptor: (dimIdx, priorGroupDims, strictConst).
    let dims = ResizeArray<int * int list * int>()
    let mutable dimIdx = 0
    for ix in idxTys do
        let a = max 1 ix.Rank
        // A wreath group's stored cells are NOT a shrinking-row simplex (rows
        // shrink per LEVEL, not per the triangle this bound describes) --
        // walking it that way would visit the wrong cell set, silently.
        if ix.Symmetry = SymWreath then
            failwith (Blade.IR.orbitStorageUnsupported "storage-cell walk (forEachStorageCell)"
                                                       (Blade.IR.orbitLevelsOf ix))
        let isSym =
            ix.Symmetry = SymSymmetric || ix.Symmetry = SymAntisymmetric || ix.Symmetry = SymHermitian
        let strictConst = if ix.Symmetry = SymAntisymmetric then 1 else 0
        let groupStart = dimIdx
        for comp in 0 .. a - 1 do
            let priorDims = if isSym && comp > 0 then [ groupStart .. groupStart + comp - 1 ] else []
            dims.Add(dimIdx, priorDims, strictConst)
            dimIdx <- dimIdx + 1
    let rank = dims.Count
    let storage : int64[] = Array.zeroCreate rank
    let logical : int64[] = Array.zeroCreate rank
    let rec loop (d: int) =
        if d = rank then visit (List.ofArray storage) (List.ofArray logical)
        else
            let (dIdx, priorDims, strictConst) = dims.[d]
            let subStore = priorDims |> List.sumBy (fun pd -> storage.[pd])
            let bound = extents.[dIdx] - subStore - int64 (List.length priorDims * strictConst)
            let mutable i = 0L
            while i < bound do
                storage.[d] <- i
                logical.[d] <-
                    match priorDims with
                    | [] -> i
                    | _ -> logical.[d - 1] + i + int64 strictConst
                loop (d + 1)
                i <- i + 1L
    if rank > 0 then loop 0

/// decompact(src, d): binary group fission (materializeDecompactForm,
/// CodeGen.fs:2477). Fission only re-groups storage -- the logical tensor is
/// unchanged -- so every output canonical cell equals the source read at that
/// same logical coordinate. Allocates the fission-shaped output (from its
/// carried type `outType`), enumerates its stored cells, and fills each from
/// `readCompact src logicalCoords`, whose canon_fold (sort + antisym sign +
/// strict-diagonal zero + Hermitian conj) reproduces the C++ scatter's baked
/// full-tuple sign. One algorithm covers all four C++ shapes (symmetric
/// gather, antisym/Hermitian r2 dense, antisym r>=3 per-group-strict
/// residual) and chained decompaction (readCompact folds a mixed-compact
/// intermediate correctly).
let decompactArray (src: BladeArray) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let outIdxTys = outArr.IndexTypes
        let outElem = outArr.ElemType
        let totalRank = outIdxTys |> List.sumBy (fun ix -> max 1 ix.Rank)
        let n = if src.Extents.Length >= 1 then src.Extents.[0] else 0L
        let extents = Array.create totalRank n
        let (osym, ostrict) = buildSymmVecWithStrict outType
        let out =
            if hasRealSymmetry osym then
                allocCompact outElem outIdxTys extents (Array.ofList osym) (Array.ofList ostrict)
            else
                allocDense outElem outIdxTys extents
        forEachStorageCell outIdxTys extents (fun storageCoords logicalCoords ->
            writeCell out storageCoords (readCompact src logicalCoords))
        out
    | _ -> raise (ArrayOpUnsupported "decompact: output type is not an array")

/// gram(left, right) = left * right^H:  R[i][j] = sum_k left[i][k]*conj(right[j][k])
/// (materializeGramForm, CodeGen.fs:2924). Two modes, driven by the carried
/// output type: same-array -> square m x m upper-triangle Sym/Hermitian
/// compact (jr = j - i; lower triangle recovered lazily on read); distinct ->
/// dense m x p full scatter. conj is std::conj on complex / identity on real.
/// The k-fold accumulates ascending (matching C++ `acc += ...`) for byte-parity.
let gramArray (left: BladeArray) (right: BladeArray) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let outElem = outArr.ElemType
        let m = if left.Extents.Length >= 1 then left.Extents.[0] else 0L
        let nn = if left.Extents.Length >= 2 then left.Extents.[1] else 0L
        let p = if right.Extents.Length >= 1 then right.Extents.[0] else 0L
        // Contracted-dim guard (BL8011), the twin of materializeGramForm's:
        // the k-fold runs to A's trailing extent and reads B's row at the
        // same k, so B's trailing extent must be equal. The checker refuses
        // literal disagreements; this covers the extents it cannot see.
        let rn = if right.Extents.Length >= 2 then right.Extents.[1] else 0L
        if rn <> nn then
            raise (InterpPanic ("BL8011", "co-iteration extent mismatch", None, 0))
        let zero = zeroOfElemTy outElem
        // WIDTH-EXACT FOLD. The store is double-backed (`SFloat of float[]`)
        // and readCell hands back a double whatever the array's element
        // type, so a Float32 contraction accumulated every step in Float64
        // and rounded once at the store -- 1 where the compiled `float
        // __gacc += float * float` (a rounding after every product and every
        // add) answers 0 on 16777216 + 1 - 16777216. Every read, product and
        // partial sum is narrowed to the element width first.
        let narrow = narrowToElem outElem
        let dot (i: int64) (j: int64) : Value =
            let mutable acc = zero
            for k in 0L .. nn - 1L do
                let lv = narrow (readCell left [ i; k ])
                let rv = narrow (N.evalUnaryOp IRConj (readCell right [ j; k ]))
                acc <- narrow (N.evalBinOp IRAdd acc (narrow (N.evalBinOp IRMul lv rv)))
            acc
        let (osym, ostrict) = buildSymmVecWithStrict outType
        if hasRealSymmetry osym then
            // same-array: compact Sym/Hermitian m x m, upper triangle (j = i + jr).
            let extents = [| m; m |]
            let out = allocCompact outElem outArr.IndexTypes extents (Array.ofList osym) (Array.ofList ostrict)
            for i in 0L .. m - 1L do
                for jr in 0L .. m - i - 1L do
                    writeCell out [ i; jr ] (dot i (i + jr))
            out
        else
            // distinct: dense m x p, full scatter.
            let extents = [| m; p |]
            let out = allocDense outElem outArr.IndexTypes extents
            for i in 0L .. m - 1L do
                for j in 0L .. p - 1L do
                    writeCell out [ i; j ] (dot i j)
            out
    | _ -> raise (ArrayOpUnsupported "gram: output type is not an array")

/// gram_apply(a, b, x) = a * (b^H * x):  y[i] = sum_k a[i][k] * t[k] with
/// t[k] = sum_j conj(b[j][k]) * x[j] (materializeGramApplyForm's twin). Two
/// ascending width-exact folds from the element zero, one accumulator per
/// cell: t's fold runs j ascending per k, y's runs k ascending per i --
/// exactly the C++ loops' per-cell order, so the lanes agree bit for bit.
/// BL8011 twins of the emitter's guards on the two runtime extents.
let gramApplyArray (a: BladeArray) (b: BladeArray) (x: BladeArray) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let outElem = outArr.ElemType
        let m = if a.Extents.Length >= 1 then a.Extents.[0] else 0L
        let n = if a.Extents.Length >= 2 then a.Extents.[1] else 0L
        let p = if b.Extents.Length >= 1 then b.Extents.[0] else 0L
        let bn = if b.Extents.Length >= 2 then b.Extents.[1] else 0L
        let xp = if x.Extents.Length >= 1 then x.Extents.[0] else 0L
        // The two guards carry the emitter's wording so an ABORT pin reads the
        // same in both lanes.
        if bn <> n then
            raise (InterpPanic ("BL8011", "co-iteration extent mismatch -- gram_apply(A, B, x): A's and B's trailing axes must be equal", None, 0))
        if xp <> p then
            raise (InterpPanic ("BL8011", "co-iteration extent mismatch -- gram_apply(A, B, x): x must have B's leading extent", None, 0))
        let zero = zeroOfElemTy outElem
        let narrow = narrowToElem outElem
        let t =
            Array.init (int n) (fun k ->
                let mutable acc = zero
                for j in 0L .. p - 1L do
                    let bv = narrow (N.evalUnaryOp IRConj (readCell b [ j; int64 k ]))
                    let xv = narrow (readCell x [ j ])
                    acc <- narrow (N.evalBinOp IRAdd acc (narrow (N.evalBinOp IRMul bv xv)))
                acc)
        let out = allocDense outElem outArr.IndexTypes [| m |]
        for i in 0L .. m - 1L do
            let mutable acc = zero
            for k in 0L .. n - 1L do
                let av = narrow (readCell a [ i; k ])
                acc <- narrow (N.evalBinOp IRAdd acc (narrow (N.evalBinOp IRMul av t.[int k])))
            writeCell out [ i ] acc
        out
    | _ -> raise (ArrayOpUnsupported "gram_apply: output type is not an array")

/// matmul(left, right) = left * right:  R[i][j] = sum_t left[i][t]*right[t][j]
/// (materializeMatmulForm; C++ side is one `blade_linalg::blade_matmul` call).
/// Always dense m x n -- A*A is not symmetric, so no same-array claim like
/// gram's. The t-fold accumulates ascending from the element zero, matching
/// both the shim's native fallback and the synthesized triple loop it
/// replaced -- the agreement tests/InterpDiff.fs checks.
let matmulArray (left: BladeArray) (right: BladeArray) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let outElem = outArr.ElemType
        let m = if left.Extents.Length >= 1 then left.Extents.[0] else 0L
        let kk = if left.Extents.Length >= 2 then left.Extents.[1] else 0L
        let n = if right.Extents.Length >= 2 then right.Extents.[1] else 0L
        let zero = zeroOfElemTy outElem
        // Width-exact, as gramArray's fold (see narrowToElem).
        let narrow = narrowToElem outElem
        let extents = [| m; n |]
        let out = allocDense outElem outArr.IndexTypes extents
        for i in 0L .. m - 1L do
            for j in 0L .. n - 1L do
                let mutable acc = zero
                for t in 0L .. kk - 1L do
                    let lv = narrow (readCell left [ i; t ])
                    let rv = narrow (readCell right [ t; j ])
                    acc <- narrow (N.evalBinOp IRAdd acc (narrow (N.evalBinOp IRMul lv rv)))
                writeCell out [ i; j ] acc
        out
    | _ -> raise (ArrayOpUnsupported "matmul: output type is not an array")

/// solve(A, b) -> x with A.x = b: the general dense linear solve by
/// partial-pivoted LU (`materializeSolveForm`; the C++ side is the same loop
/// nest, or one `blade_lapack::blade_solve` call under the LAPACK gate).
///
/// THE OPERATION-FOR-OPERATION TWIN of the emitted native arm, and unlike
/// `eighArrays` this one IS a byte-identity claim: `blade test interp math`
/// compares this function's printed output against the compiled program's,
/// digit for digit. The two texts are written to be diffable by eye --
///
///   * one row-major working copy `lu` of A, so A itself is never factorized
///     in place and `solve(A, b)` twice over one A answers twice the same;
///   * x starts as a copy of b, and the forward substitution is FUSED INTO the
///     elimination (each multiplier is applied to the right-hand side the
///     moment it is formed), so no permutation vector is replayed later;
///   * every update spelled `a - f * b`, never a compound assignment, so the
///     two implementations' arithmetic reads as the same sequence of
///     roundings. .NET never fuses a multiply-add on its own, and the C++ side
///     is compiled with `-ffp-contract=off` by the differential harnesses, so
///     the two agree exactly rather than nearly.
///
/// THE PIVOT RULE: scan column k downward from row k and keep the FIRST row
/// attaining the maximum |value| -- a STRICT `>`, so a later equal magnitude
/// never displaces an earlier one. That single character is the whole tie-break
/// and it matches both the emitted C++ and LAPACK's own `idamax`.
///
/// SINGULARITY is an EXACT `= 0.0` test on the chosen pivot, never an epsilon:
/// an epsilon would be a tunable this function and the C++ arm would have to
/// agree on across a difference of two roundings, which is precisely the class
/// of disagreement the design removes. The panic is BL8007 with
/// `CodeGen.solveSingularMessage`, spelled here as a literal because the
/// compiler's CodeGen module is not referenced from the interpreter -- the two
/// copies are kept in step by the corpus abort pin that reads both.
let solveArray (matrix: BladeArray) (rhs: BladeArray) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let n = if matrix.Extents.Length >= 1 then int matrix.Extents.[0] else 0
        // Working copy + right-hand side, in the compiled arm's own order: the
        // whole row of `lu`, then that row's `x` cell.
        let lu = Array.zeroCreate<float> (n * n)
        let x = Array.zeroCreate<float> n
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                lu.[i * n + j] <- toF64v (indexArray matrix [VInt (int64 i); VInt (int64 j)])
            x.[i] <- toF64v (indexArray rhs [VInt (int64 i)])
        for k in 0 .. n - 1 do
            // Partial pivot: first maximal |value|, strict >.
            let mutable p = k
            let mutable big = abs lu.[k * n + k]
            for i in k + 1 .. n - 1 do
                let m = abs lu.[i * n + k]
                if m > big then
                    big <- m
                    p <- i
            if lu.[p * n + k] = 0.0 then
                raise (InterpPanic ("BL8007",
                                    "solve(A, b): the matrix is SINGULAR -- LU factorization found an exactly-zero pivot",
                                    None, 0))
            if p <> k then
                for j in 0 .. n - 1 do
                    let t = lu.[k * n + j]
                    lu.[k * n + j] <- lu.[p * n + j]
                    lu.[p * n + j] <- t
                let xt = x.[k] in x.[k] <- x.[p]; x.[p] <- xt
            for i in k + 1 .. n - 1 do
                let f = lu.[i * n + k] / lu.[k * n + k]
                lu.[i * n + k] <- f
                for j in k + 1 .. n - 1 do
                    lu.[i * n + j] <- lu.[i * n + j] - f * lu.[k * n + j]
                x.[i] <- x.[i] - f * x.[k]
        // Back substitution, k descending.
        for kk in n .. -1 .. 1 do
            let k = kk - 1
            let mutable s = x.[k]
            for j in k + 1 .. n - 1 do
                s <- s - lu.[k * n + j] * x.[j]
            x.[k] <- s / lu.[k * n + k]
        let out = allocDense outArr.ElemType outArr.IndexTypes [| int64 n |]
        for i in 0 .. n - 1 do writeCell out [ int64 i ] (VFloat x.[i])
        out
    | _ -> raise (ArrayOpUnsupported "solve: output type is not an array")

/// lu(A) -> (LU, piv): `solveArray`'s elimination, kept -- the same working
/// copy, the same strict-`>` pivot rule, the same exact-zero singularity
/// test; the multipliers stay below the diagonal, piv[k] is the row swapped
/// with k at step k (0-based). Twin of materializeLuForm's native arm.
let luArrays (matrix: BladeArray) (outType: IRType) : BladeArray * BladeArray =
    match outType with
    | IRTTuple [ ArrayElem luArr; ArrayElem pivArr ] ->
        let n = if matrix.Extents.Length >= 1 then int matrix.Extents.[0] else 0
        let lu = Array.zeroCreate<float> (n * n)
        let piv = Array.zeroCreate<int64> n
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                lu.[i * n + j] <- toF64v (indexArray matrix [VInt (int64 i); VInt (int64 j)])
        for k in 0 .. n - 1 do
            let mutable p = k
            let mutable big = abs lu.[k * n + k]
            for i in k + 1 .. n - 1 do
                let m = abs lu.[i * n + k]
                if m > big then
                    big <- m
                    p <- i
            if lu.[p * n + k] = 0.0 then
                raise (InterpPanic ("BL8007",
                                    "lu(A): the matrix is SINGULAR -- LU factorization found an exactly-zero pivot",
                                    None, 0))
            piv.[k] <- int64 p
            if p <> k then
                for j in 0 .. n - 1 do
                    let t = lu.[k * n + j]
                    lu.[k * n + j] <- lu.[p * n + j]
                    lu.[p * n + j] <- t
            for i in k + 1 .. n - 1 do
                let f = lu.[i * n + k] / lu.[k * n + k]
                lu.[i * n + k] <- f
                for j in k + 1 .. n - 1 do
                    lu.[i * n + j] <- lu.[i * n + j] - f * lu.[k * n + j]
        let luOut = allocDense luArr.ElemType luArr.IndexTypes [| int64 n; int64 n |]
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do writeCell luOut [ int64 i; int64 j ] (VFloat lu.[i * n + j])
        let pivOut = allocDense pivArr.ElemType pivArr.IndexTypes [| int64 n |]
        for k in 0 .. n - 1 do writeCell pivOut [ int64 k ] (VInt piv.[k])
        (luOut, pivOut)
    | _ -> raise (ArrayOpUnsupported "lu: output type is not a (LU, piv) tuple")

/// lu_solve[_t](LU, piv, b): the stored factors applied, twin of
/// materializeLuSolveForm's two native arms (plain: pivots, L, U in the
/// solve form's fused order, bitwise `solveArray`; transposed: U^T forward,
/// L^T backward, pivots undone in reverse step order).
let luSolveArray (lu: BladeArray) (piv: BladeArray) (rhs: BladeArray) (transposed: bool) (outType: IRType) : BladeArray =
    match outType with
    | ArrayElem outArr ->
        let n = if lu.Extents.Length >= 1 then int lu.Extents.[0] else 0
        let l (i: int) (j: int) = toF64v (indexArray lu [VInt (int64 i); VInt (int64 j)])
        let pv (k: int) =
            match indexArray piv [VInt (int64 k)] with
            | VInt p -> int p
            | VInt32 p -> int p
            | VFloat p -> int p
            | _ -> k
        let x = Array.init n (fun i -> toF64v (indexArray rhs [VInt (int64 i)]))
        if not transposed then
            for k in 0 .. n - 1 do
                let p = pv k
                if p <> k then
                    let t = x.[k] in x.[k] <- x.[p]; x.[p] <- t
                for i in k + 1 .. n - 1 do
                    x.[i] <- x.[i] - l i k * x.[k]
            for kk in n .. -1 .. 1 do
                let k = kk - 1
                let mutable s = x.[k]
                for j in k + 1 .. n - 1 do
                    s <- s - l k j * x.[j]
                x.[k] <- s / l k k
        else
            let y = Array.zeroCreate<float> n
            for k in 0 .. n - 1 do
                let mutable s = x.[k]
                for j in 0 .. k - 1 do
                    s <- s - l j k * y.[j]
                y.[k] <- s / l k k
            for kk in n .. -1 .. 1 do
                let k = kk - 1
                let mutable s = y.[k]
                for j in k + 1 .. n - 1 do
                    s <- s - l j k * y.[j]
                y.[k] <- s
            for kk in n .. -1 .. 1 do
                let k = kk - 1
                let p = pv k
                if p <> k then
                    let t = y.[k] in y.[k] <- y.[p]; y.[p] <- t
            for i in 0 .. n - 1 do x.[i] <- y.[i]
        let out = allocDense outArr.ElemType outArr.IndexTypes [| int64 n |]
        for i in 0 .. n - 1 do writeCell out [ int64 i ] (VFloat x.[i])
        out
    | _ -> raise (ArrayOpUnsupported "lu_solve: output type is not an array")

/// eigh(S) -> (Q, LAM): symmetric eigendecomposition by cyclic two-sided
/// Jacobi. Q's columns are the eigenvectors, LAM is descending, each Q column
/// sign-fixed so the first row attaining the maximum |entry| is positive --
/// the conventions `MathDecls.eighDecl` / `blade_lapack`'s `emit_values_desc`
/// / `emit_vectors_desc` document and reproduce.
///
/// A DELIBERATE COPY of `BladeMath.Jacobi.eigh` (oracles/math/Jacobi.fs), not a
/// call into it: BladeMath is a separate fsproj the compiler never
/// references, so it can serve as the value oracle for the generated code.
/// Kept in step by `blade test diff-oracle math`.
///
/// `IREigh` only exists when LAPACK was available at elaboration time; gate
/// off, `math.eigh` expands to synthesized Jacobi source the interpreter
/// walks as ordinary code, so this function runs only in a gate-ON run --
/// NEVER use it for byte-identity (`interp`/`diff-oracle` run gate-off,
/// since an eigensolver's output is not unique). It is not a differential
/// twin of the LAPACK route: the two agree on eigenvalues and the two
/// normalised freedoms, not bit for bit, and not on the degenerate-subspace basis.
///
/// The operand is read through `indexArray`, routing a compact rank-2 group
/// to the canonical reader. Complex is declined by name (no oracle behind
/// it); the compiled `?heev` / `?hpev` route covers that case.
let eighArrays (operand: BladeArray) (outType: IRType) : BladeArray * BladeArray =
    let (qTy, lamTy) =
        match outType with
        | IRTTuple [ qT; lamT ] ->
            (match qT, lamT with
             | ArrayElem qa, ArrayElem la -> (qa, la)
             | _ -> raise (ArrayOpUnsupported "eigh: result type is not a pair of arrays"))
        | _ -> raise (ArrayOpUnsupported "eigh: result type is not a 2-tuple")
    (match elemThrough operand.ElemType with
     | Some (ETComplex64 | ETComplex128) ->
         raise (ArrayOpUnsupported "eigh: the interpreter implements the REAL symmetric case only; a Hermitian (complex) eigendecomposition is available from the compiled LAPACK route (?heev / ?hpev) and is deliberately not re-implemented here without an oracle behind it")
     | _ -> ())
    let n = if operand.Extents.Length >= 1 then int operand.Extents.[0] else 0
    // Working copy of the symmetric input + the eigenvector accumulator,
    // identical to the oracle's `aw` / `q` (and to eighDecl's `aw` / `qm`).
    let aw = Array2D.init n n (fun i j -> toF64v (indexArray operand [VInt (int64 i); VInt (int64 j)]))
    let qm = Array2D.init n n (fun i j -> if i = j then 1.0 else 0.0)
    // `MathDecls.defaultSweeps` (10), duplicated here for the same
    // oracle-independence reason the algorithm is. The intrinsic only ever
    // means the DEFAULT schedule: the planned elaborator rule keeps the
    // synthesized Jacobi path whenever an explicit SWEEPS argument is given,
    // because a stated sweep budget is a request for that algorithm and LAPACK
    // has no analogue of it. If that rule ever changes, this constant is the
    // other half of the change.
    let sweeps = 10
    for _sweep in 1 .. sweeps do
        for p in 0 .. n - 2 do
            for r in p + 1 .. n - 1 do
                let apq = aw.[p, r]
                let app = aw.[p, p]
                let aqq = aw.[r, r]
                let conv = abs apq <= 1.0e-15 * sqrt (abs app * abs aqq + 1.0e-300)
                let theta = (aqq - app) / (if conv then 1.0 else 2.0 * apq)
                let tt = (if theta >= 0.0 then 1.0 else -1.0) / (abs theta + sqrt (1.0 + theta * theta))
                let cs = if conv then 1.0 else 1.0 / sqrt (1.0 + tt * tt)
                let sn = if conv then 0.0 else cs * tt
                // AW <- AW.R (columns p, r), then AW <- R^T.AW (rows p, r).
                for i in 0 .. n - 1 do
                    let tp = aw.[i, p]
                    let tq = aw.[i, r]
                    aw.[i, p] <- cs * tp - sn * tq
                    aw.[i, r] <- sn * tp + cs * tq
                for i in 0 .. n - 1 do
                    let tp = aw.[p, i]
                    let tq = aw.[r, i]
                    aw.[p, i] <- cs * tp - sn * tq
                    aw.[r, i] <- sn * tp + cs * tq
                // Accumulate the column rotation into Q.
                for i in 0 .. n - 1 do
                    let tp = qm.[i, p]
                    let tq = qm.[i, r]
                    qm.[i, p] <- cs * tp - sn * tq
                    qm.[i, r] <- sn * tp + cs * tq
    let lam = Array.init n (fun j -> aw.[j, j])
    // Selection sort descending (ties keep original order) + Q column swaps.
    for kk in 0 .. n - 1 do
        let mutable best = kk
        for j in kk + 1 .. n - 1 do
            if lam.[j] > lam.[best] then best <- j
        let tl = lam.[kk] in lam.[kk] <- lam.[best]; lam.[best] <- tl
        for i in 0 .. n - 1 do
            let tq = qm.[i, kk] in qm.[i, kk] <- qm.[i, best]; qm.[i, best] <- tq
    // Sign fix: first row attaining max |entry| per column made positive.
    for j in 0 .. n - 1 do
        let mutable bigv = 0.0
        let mutable best = 0
        for i in 0 .. n - 1 do
            let mag = abs qm.[i, j]
            if mag > bigv then
                best <- i
                bigv <- mag
        let flip = if qm.[best, j] < 0.0 then -1.0 else 1.0
        for i in 0 .. n - 1 do qm.[i, j] <- qm.[i, j] * flip
    let qOut = allocDense qTy.ElemType qTy.IndexTypes [| int64 n; int64 n |]
    let lamOut = allocDense lamTy.ElemType lamTy.IndexTypes [| int64 n |]
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do writeCell qOut [ int64 i; int64 j ] (VFloat qm.[i, j])
    for j in 0 .. n - 1 do writeCell lamOut [ int64 j ] (VFloat lam.[j])
    (qOut, lamOut)

// 7 Compound (masked product space, formalism 4.5): construction + reads.
// The value-space twin of runtime `Compound<T,RANK>` + `compound_index_t`
// (cpp/nested_array_types.hpp:133, index_types.h:235): the rank<->tuple
// bijection over a masked product space plus a compact backing buffer
// holding only the present cells (each followed by its trailing block).
// Every read/reduce mirrors a C++ helper byte-for-byte (4.7): full scalar
// C(i,j) -> data[linearize(coords)*trail + t]; trailing row -> the trailing
// block; reduce/sort/... -> walk .data. (Partial/wildcard reads are
// SparseIdx's job: 7b's sparsePartial.)

/// Flatten a rank-N Bool mask array to row-major bits -- the presence vector a
/// compound_index_t enumerates (pool_base flatten, genCompoundIndexFromMask).
let maskToBits (arr: BladeArray) : bool[] = flatLeaves arr |> Array.map toBoolv

/// Row-major (lex) flat offset of a tuple over the masked grid -- mirrors
/// compound_index_t::mask_offset (index_types.h:283): off = off*extents[d]+t[d].
let compoundMaskOffset (leadExtents: int64[]) (tuple: int64[]) : int64 =
    let mutable off = 0L
    for d in 0 .. leadExtents.Length - 1 do off <- off * leadExtents.[d] + tuple.[d]
    off

/// Build the rank<->tuple bijection from flat mask bits over the masked grid,
/// scanning the product space in row-major LEX order and appending each
/// mask-valid tuple (compound_index_t::enumerate, index_types.h:288-300). Returns
/// (rank_to_tuple, mask_offset -> rank map, cardinality).
let buildCompoundIndex (leadExtents: int64[]) (maskBits: bool[]) : int64[][] * Dictionary<int64, int> * int64 =
    let table = ResizeArray<int64[]>()
    let rankOf = Dictionary<int64, int>()
    let rank = leadExtents.Length
    let idx = Array.zeroCreate rank
    let rec enumerate (depth: int) =
        if depth = rank then
            let off = compoundMaskOffset leadExtents idx
            if maskBits.[int off] then
                rankOf.[off] <- table.Count
                table.Add(Array.copy idx)
        else
            let e = leadExtents.[depth]
            let mutable v = 0L
            while v < e do
                idx.[depth] <- v
                enumerate (depth + 1)
                v <- v + 1L
    (if rank > 0 then enumerate 0)
    (table.ToArray(), rankOf, int64 table.Count)

/// Read compact-buffer cell `i` as a Value (mirrors the stored element type).
let private compactCell (data: Store) (i: int) : Value =
    match data with
    | SFloat a -> VFloat a.[i]
    | SInt a -> VInt a.[i]
    | SComplex a -> let struct (r, im) = a.[i] in VComplex (r, im)
    | SBool a -> VBool a.[i]
    | SObj a -> a.[i]
    | _ -> raise (InterpPanic ("BL8003", "compound read: unexpected backing store", None, 0))

/// linearize(tuple) -> compact rank via the reverse map (compound_index_t::
/// linearize = tuple_to_rank.at). A tuple that is not present is a program
/// error (C++ .at() throws); the corpus only ever reads present cells.
let compoundLinearize (cv: CompoundValue) (coords: int64[]) : int =
    match cv.RankOf.TryGetValue(compoundMaskOffset cv.LeadExtents coords) with
    | true, r -> r
    | _ -> raise (InterpPanic ("BL8003", "compound read: coordinate not present in mask", None, 0))

/// Build a Compound VALUE from a dense array + a bool mask array (the compound()
/// / load_compound constructor). `arrType` is the compound view type (its
/// IxKCompound slot carries LeadRank). The mask covers the LEADING `leadRank`
/// dims of `dense`; remaining dense dims fold into the trailing stride. Scatter
/// each present leading cell's trailing block into a compact buffer, in the
/// index's lex rank order (genCompoundInitBinding, CodeGen.fs:8581-8640).
let buildCompound (arrType: IRArrayType) (dense: BladeArray) (mask: BladeArray) : CompoundValue =
    let leadRank =
        arrType.IndexTypes
        |> List.tryFind (fun ix -> ix.IxKind = IxKCompound)
        |> Option.map _.Rank
        |> Option.defaultValue (max 1 mask.Extents.Length)
    let leadExtents = mask.Extents
    let maskBits = flatLeaves mask |> Array.map toBoolv
    let (table, rankOf, card) = buildCompoundIndex leadExtents maskBits
    let trail =
        [ leadRank .. dense.Extents.Length - 1 ]
        |> List.fold (fun acc d -> acc * dense.Extents.[d]) 1L
    let denseVals = flatLeaves dense
    let itrail = int trail
    let compact = Array.create (int card * itrail) VUnit
    for r in 0 .. int card - 1 do
        let off = int (compoundMaskOffset leadExtents table.[r])
        for t in 0 .. itrail - 1 do
            compact.[r * itrail + t] <- denseVals.[off * itrail + t]
    { ElemType = arrType.ElemType
      IndexTypes = arrType.IndexTypes
      LeadRank = leadRank
      LeadExtents = leadExtents
      Mask = maskBits
      Table = table
      RankOf = rankOf
      Cardinality = card
      TrailingStride = trail
      Data = storeOfValues arrType.ElemType compact }

/// The trailing (regular) index types of a compound (everything after the
/// compound head slot); used to shape a trailing-row / residual sub-view.
let private trailingIndexTypes (cv: CompoundValue) : IRIndexType list =
    match cv.IndexTypes with _ :: t -> t | [] -> []

/// Full-tuple SCALAR read: `data[linearize(coords)*trailing_stride + trailOffset]`
/// (Compound::operator(), nested_array_types.hpp:145).
let compoundFullScalar (cv: CompoundValue) (coords: int64[]) (trailOffset: int64) : Value =
    let r = compoundLinearize cv coords
    compactCell cv.Data (r * int cv.TrailingStride + int trailOffset)

/// Trailing-ROW sub-view for a resolved lead tuple (Compound::row): the
/// contiguous span of `trailing_stride` cells at data + linearize(coords)*trail.
/// A dense rank-1 array over the (single) trailing dim.
let compoundRow (cv: CompoundValue) (coords: int64[]) : Value =
    let r = compoundLinearize cv coords
    let itrail = int cv.TrailingStride
    let vs = Array.init itrail (fun t -> compactCell cv.Data (r * itrail + t))
    VArray
        { ElemType = cv.ElemType
          IndexTypes = trailingIndexTypes cv
          Extents = [| cv.TrailingStride |]
          Data = storeOfValues cv.ElemType vs }

// There is no compoundPartial: partial/wildcard reads are a SparseIdx
// feature (sparsePartial in 7b below). A CompoundPartial classification on a
// compound head is an internal invariant break, backstopped at the read
// sites in Loops.fs / CodeGen's compoundRead.

/// The compact present values of a compound as a plain rank-1 dense array
/// (cardinality*trailing_stride cells, buffer order) -- the operand form the
/// eager ops (sort/reduce/set-op) consume, matching CodeGen's compound-operand
/// path which walks `.data` (4.1, genReduceBinding reduceBound 1936).
let compoundToDense (cv: CompoundValue) : BladeArray =
    let n = int cv.Cardinality * int cv.TrailingStride
    let vs = Array.init n (fun i -> compactCell cv.Data i)
    { ElemType = cv.ElemType
      IndexTypes = []
      Extents = [| int64 n |]
      Data = storeOfValues cv.ElemType vs }

/// reduce over a compound's present cells (init required for the always-emitted
/// empty guard; without init, empty panics -- matching genReduceBinding).
let compoundReduce (cv: CompoundValue) (fold: Value -> Value -> Value) (init: Value option) : Value =
    reduceArray (compoundToDense cv) fold init

/// `S <|:> D` compound-left allocated fallback: a dense array shaped like D,
/// in which each of S's present leading cells overwrites its trailing block
/// onto a copy of D (absent leading cells keep D: the SQL sparse-overlay
/// regime, genFallbackMaterialize compound-left arm, CodeGen.fs:9398-9449).
/// Single trailing dim only (the compiler-wide compound gate).
let fallbackCompoundLeft (cvS: CompoundValue) (d: BladeArray) : BladeArray =
    let result = copyBladeArray d
    let itrail = int cvS.TrailingStride
    for r in 0 .. int cvS.Cardinality - 1 do
        let lead = Array.toList cvS.Table.[r]
        for tr in 0 .. itrail - 1 do
            let coords = if itrail = 1 then lead else lead @ [ int64 tr ]
            writeCell result coords (compactCell cvS.Data (r * itrail + tr))
    result

// 7b Sparse (explicit key enumeration, formalism 3.5): construction + reads.
// The value-space twin of runtime `Sparse<T,RANK>` + `sparse_index_t`
// (cpp/nested_array_types.hpp, index_types.h): compound (7) minus the grid.
// Keys stay in given order (iteration order == key order), the reverse map
// keys structurally on the tuple (TupleKeyComparer -- no grid offset
// exists), and every partial read is a gather over the entry list.

/// Build the rank<->tuple bijection from an explicit key list in GIVEN order.
/// Duplicate keys panic (sparse_index_t's ctor throws -- the bijection would
/// be ill-defined); InterpDiff parity requires the same failure here.
let buildSparseIndex (keys: int64[][]) : Dictionary<int64[], int> * int64 =
    let rankOf = Dictionary<int64[], int>(TupleKeyComparer())
    keys |> Array.iteri (fun r key ->
        if rankOf.ContainsKey key then
            raise (InterpPanic ("BL8005", "SparseIdx: duplicate key tuple", None, 0))
        rankOf.[key] <- r)
    (rankOf, int64 keys.Length)

/// Build a Sparse VALUE from a values array + an explicit key list (the
/// sparse() constructor). `arrType`'s IxKSparse slot carries the key tuple
/// arity. The values' LEADING dimension is the key axis (one cell per key, in
/// key order); remaining dims fold into the trailing stride, mirroring
/// buildCompound's split. No scatter: the flattened values pool IS the
/// compact buffer's key-major layout, so this is a straight copy.
let buildSparse (arrType: IRArrayType) (values: BladeArray) (keys: int64[][])
                (rankOf: Dictionary<int64[], int>) (card: int64) : SparseValue =
    let leadRank =
        arrType.IndexTypes
        |> List.tryFind (fun ix -> ix.IxKind = IxKSparse)
        |> Option.map _.Rank
        |> Option.defaultValue (if keys.Length > 0 then keys.[0].Length else 1)
    if values.Extents.Length < 1 || values.Extents.[0] <> card then
        raise (InterpPanic ("BL8001", "sparse(values, keys): values length does not match key count", None, 0))
    let trail =
        [ 1 .. values.Extents.Length - 1 ]
        |> List.fold (fun acc d -> acc * values.Extents.[d]) 1L
    let valueCells = flatLeaves values
    { ElemType = arrType.ElemType
      IndexTypes = arrType.IndexTypes
      LeadRank = leadRank
      Keys = keys
      RankOf = rankOf
      Cardinality = card
      TrailingStride = trail
      Data = storeOfValues arrType.ElemType (Array.sub valueCells 0 (int card * int trail)) }

/// linearize(tuple) -> rank via the structural hash (sparse_index_t::linearize
/// = tuple_to_rank.at). A missing key is a program error (C++ .at() throws).
let sparseLinearize (sv: SparseValue) (coords: int64[]) : int =
    match sv.RankOf.TryGetValue coords with
    | true, r -> r
    | _ -> raise (InterpPanic ("BL8003", "sparse read: key tuple not present in key set", None, 0))

let private sparseTrailingIndexTypes (sv: SparseValue) : IRIndexType list =
    match sv.IndexTypes with _ :: t -> t | [] -> []

/// Full-key SCALAR read (Sparse::operator()).
let sparseFullScalar (sv: SparseValue) (coords: int64[]) (trailOffset: int64) : Value =
    let r = sparseLinearize sv coords
    compactCell sv.Data (r * int sv.TrailingStride + int trailOffset)

/// Trailing-ROW sub-view for a resolved key (Sparse::row).
let sparseRow (sv: SparseValue) (coords: int64[]) : Value =
    let r = sparseLinearize sv coords
    let itrail = int sv.TrailingStride
    let vs = Array.init itrail (fun t -> compactCell sv.Data (r * itrail + t))
    VArray
        { ElemType = sv.ElemType
          IndexTypes = sparseTrailingIndexTypes sv
          Extents = [| sv.TrailingStride |]
          Data = storeOfValues sv.ElemType vs }

/// Partial (residual) sparse indexing: always a gather -- one pass over Keys in
/// key order keeping the entries whose pinned axes match. Residual keys are the
/// matches' free-axis coordinates (automatically distinct, parent key order).
///   residual rank 1  -> dense Array<T,1> (trail 1) or Array<T,2> (trail>1)
///   residual rank>=2 -> residual Sparse (its own key table, key order)
let sparsePartial (sv: SparseValue) (pinned: (int * int64) list) (freePos: int list) : Value =
    let rr = List.length freePos
    let itrail = int sv.TrailingStride
    let freeArr = Array.ofList freePos
    let matches =
        [| for r in 0 .. int sv.Cardinality - 1 do
             let key = sv.Keys.[r]
             if pinned |> List.forall (fun (pos, v) -> key.[pos] = v) then
                 yield (r, freeArr |> Array.map (fun p -> key.[p])) |]
    if rr = 1 then
        if itrail = 1 then
            let vs = matches |> Array.map (fun (r, _) -> compactCell sv.Data r)
            VArray
                { ElemType = sv.ElemType
                  IndexTypes = sparseTrailingIndexTypes sv
                  Extents = [| int64 vs.Length |]
                  Data = storeOfValues sv.ElemType vs }
        else
            // rank-2 dense {matches, trailing extent}: each match's whole
            // trailing block (make_sparse_gather_dense_trail).
            let rows =
                matches
                |> Array.map (fun (r, _) ->
                    let vs = Array.init itrail (fun t -> compactCell sv.Data (r * itrail + t))
                    storeOfValues sv.ElemType vs)
            VArray
                { ElemType = sv.ElemType
                  IndexTypes = []
                  Extents = [| int64 rows.Length; sv.TrailingStride |]
                  Data = SNested rows }
    else
        // Residual SPARSE: sub-key table in parent key order, gathered blocks.
        let subKeys = matches |> Array.map snd
        let (subRankOf, subCard) = buildSparseIndex subKeys
        let compact = Array.create (int subCard * itrail) VUnit
        matches |> Array.iteri (fun i (r, _) ->
            for t in 0 .. itrail - 1 do
                compact.[i * itrail + t] <- compactCell sv.Data (r * itrail + t))
        VSparse
            { ElemType = sv.ElemType
              IndexTypes = sv.IndexTypes
              LeadRank = rr
              Keys = subKeys
              RankOf = subRankOf
              Cardinality = subCard
              TrailingStride = sv.TrailingStride
              Data = storeOfValues sv.ElemType compact }

/// The compact values of a sparse as a plain rank-1 dense array (buffer/key
/// order) -- the operand form the eager ops consume, mirroring compoundToDense.
let sparseToDense (sv: SparseValue) : BladeArray =
    let n = int sv.Cardinality * int sv.TrailingStride
    let vs = Array.init n (fun i -> compactCell sv.Data i)
    { ElemType = sv.ElemType
      IndexTypes = []
      Extents = [| int64 n |]
      Data = storeOfValues sv.ElemType vs }

/// reduce over a sparse's cells (key order; init drives the empty guard).
let sparseReduce (sv: SparseValue) (fold: Value -> Value -> Value) (init: Value option) : Value =
    reduceArray (sparseToDense sv) fold init

// 8 group_keys / group_by (CSR grouping): build + read. group_keys builds a
// CSR structure (offsets + group-contiguous member perm); group_by gathers
// each group's values into a ragged array. Bucket order is the subtle part
// (4.2/4.8): first-appearance (dynamic / multi-key), numeric-value
// (positional Idx<N>), or enum-list-position (EnumIdx). CodeGen stores no
// keys array -- the perm recovers everything (genGroupKeysBinding, CodeGen.fs:7511).

/// The three group-key bucketing regimes, dispatched on the group_keys binding's
/// IRTGroupKeys type (single key) or key arity (>1 = dynamic tuple-hash).
///   GKDynamic      -- Case 3 / multi-key: bucket = first-appearance ordinal.
///   GKPositional n -- Case 1 (Idx<N> keys): bucket = the integer key value.
///   GKEnum values  -- Case 2 (EnumIdx): bucket = the key's position in `values`.
type GroupKeyCase =
    | GKDynamic
    | GKPositional of ngroups: int
    | GKEnum of values: Value[]

/// An injective-enough string key for first-appearance dedup (mirrors the C++
/// unordered_map keyed by the value / tuple; ints value-equal, strings ordinal).
let private valueDedupKey (v: Value) : string =
    match v with
    | VInt n -> "i" + string n
    | VInt32 n -> "i" + string (int64 n)
    | VString s -> "s" + s
    | VBool b -> "b" + (if b then "1" else "0")
    | VFloat f -> "f" + f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    | VFloat32 f -> "f" + (float f).ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    | VChar c -> "c" + string (int c)
    | _ -> "?"

/// NEGATIVE KEY = "this row belongs to no group" (CodeGen.negativeKeyDrop,
/// CodeGen.fs:14409): a key function is allowed to do selection, so a row whose
/// key is negative is dropped from the grouping entirely rather than forming a
/// group of its own. Only NUMERIC keys can be negative -- std::string has no
/// `< 0`, so codegen emits no guard for string (or bool/char) keys and neither
/// does this.
let private isNegativeKey (v: Value) : bool =
    match v with
    | VInt n -> n < 0L
    | VInt32 n -> n < 0
    | VFloat f -> f < 0.0
    | VFloat32 f -> f < 0.0f
    | _ -> false

/// Build the CSR grouping (offsets length ngroups+1, member perm in group-
/// contiguous input order). Bucket order per the regime; counts/offsets/perm
/// exactly mirror genGroupKeysBinding (CodeGen.fs:7595-7607).
let buildGroupKeys (keyArrays: BladeArray list) (gkCase: GroupKeyCase) : GroupKeysValue =
    let n = if List.isEmpty keyArrays then 0 else int keyArrays.[0].Extents.[0]
    let buckets = Array.zeroCreate n
    // Negative-key drop, mirroring the `continue` guard codegen puts at the top
    // of EVERY pass that walks the key array (negativeKeyDrop, CodeGen.fs:14409):
    // a dropped row is invisible to discovery (so it never opens a bucket of its
    // own), to counts, and to the permutation. A multi-key row drops when ANY
    // numeric component is negative. GKEnum is exempt -- its admissible values
    // are declared up front, so a negative entry there is a value the user asked
    // for, not a sentinel. `perm` still spans the full input length and is simply
    // under-filled; reads are bounded by `offsets`, as in the emitted C++.
    let dropped = Array.zeroCreate<bool> n
    match gkCase with
    | GKEnum _ -> ()
    | GKPositional _ | GKDynamic ->
        for i in 0 .. n - 1 do
            dropped.[i] <- keyArrays |> List.exists (fun a -> isNegativeKey (readCell a [ int64 i ]))
    let ngroups =
        match gkCase with
        | GKPositional ng ->
            for i in 0 .. n - 1 do
                if not dropped.[i] then buckets.[i] <- int (toI64v (readCell keyArrays.[0] [ int64 i ]))
            ng
        | GKEnum values ->
            for i in 0 .. n - 1 do
                let v = readCell keyArrays.[0] [ int64 i ]
                buckets.[i] <- (match values |> Array.tryFindIndex (eqValues v) with Some p -> p | None -> 0)
            values.Length
        | GKDynamic ->
            let lookup = Dictionary<string, int>()
            let mutable ng = 0
            for i in 0 .. n - 1 do
                if not dropped.[i] then
                    let key =
                        keyArrays |> List.map (fun a -> valueDedupKey (readCell a [ int64 i ])) |> String.concat ""
                    match lookup.TryGetValue key with
                    | true, b -> buckets.[i] <- b
                    | _ -> lookup.[key] <- ng; buckets.[i] <- ng; ng <- ng + 1
            ng
    let counts = Array.zeroCreate (max 1 ngroups)
    for i in 0 .. n - 1 do
        if not dropped.[i] then counts.[buckets.[i]] <- counts.[buckets.[i]] + 1
    let offsets = Array.zeroCreate (ngroups + 1)
    for g in 0 .. ngroups - 1 do offsets.[g + 1] <- offsets.[g] + int64 counts.[g]
    let fill = Array.zeroCreate (max 1 ngroups)
    let perm = Array.zeroCreate n
    for i in 0 .. n - 1 do
        if not dropped.[i] then
            let g = buckets.[i]
            perm.[int offsets.[g] + fill.[g]] <- int64 i
            fill.[g] <- fill.[g] + 1
    { Offsets = offsets; Members = perm; Coords = None }

/// group_bucket(gk): invert the CSR pair into a dense row -> bucket map over the
/// source index space (genGroupBucketBinding). Rows the permutation never names
/// are exactly the rows a negative key dropped, so the -1 prefill IS the drop
/// marker -- no key rescan, and no dependence on the bucketing regime.
///
/// The source length is `Members.Length`: buildGroupKeys sizes `perm` at the full
/// input length and merely under-fills it when rows drop, which is the same
/// invariant the emitted C++ carries in `<gk>__nsrc`.
let buildGroupBucket (idxTys: IRIndexType list) (gk: GroupKeysValue) : BladeArray =
    let n = gk.Members.Length
    let bucket = Array.create n -1L
    for g in 0 .. gk.Offsets.Length - 2 do
        for p in int gk.Offsets.[g] .. int gk.Offsets.[g + 1] - 1 do
            bucket.[int gk.Members.[p]] <- int64 g
    { ElemType = IRTScalar ETInt64
      IndexTypes = idxTys
      Extents = [| int64 n |]
      Data = SInt bucket }

/// extents(gk): per-group sizes from the CSR row pointers, no gather
/// (genGroupSizesBinding). The same arithmetic the ragged peel reads each row's
/// length from, so this and an extents-only peel agree by construction.
let buildGroupSizes (idxTys: IRIndexType list) (gk: GroupKeysValue) : BladeArray =
    let ngroups = gk.Offsets.Length - 1
    { ElemType = IRTScalar ETInt64
      IndexTypes = idxTys
      Extents = [| int64 ngroups |]
      Data = SInt (Array.init ngroups (fun g -> gk.Offsets.[g + 1] - gk.Offsets.[g])) }

/// group_by(vals, gk): gather each group's values (`vals[perm[offsets[g]+k]]`,
/// input order) into a ragged rank-2 array (genGroupByBinding, CodeGen.fs:8767).
/// Extents = [ngroups; 0] -- the inner is ragged, print-bound 0 (auto-print -> []).
let buildGroupBy (idxTys: IRIndexType list) (gk: GroupKeysValue) (vals: BladeArray) : BladeArray =
    let ngroups = gk.Offsets.Length - 1
    let rows =
        Array.init ngroups (fun g ->
            let lo = int gk.Offsets.[g]
            let hi = int gk.Offsets.[g + 1]
            let vs =
                match gk.Coords with
                | Some coords -> Array.init (hi - lo) (fun k -> readCell vals coords.[lo + k])
                | None -> Array.init (hi - lo) (fun k -> readCell vals [ gk.Members.[lo + k] ])
            storeOfValues vals.ElemType vs)
    let lens = Array.init ngroups (fun g -> gk.Offsets.[g + 1] - gk.Offsets.[g])
    { ElemType = vals.ElemType
      IndexTypes = idxTys
      Extents = [| int64 ngroups; 0L |]
      Data = SRagged (rows, lens, gk.Offsets) }

// PRINT: byte-parity array binding printer (mirrors CodeGen genPrintStatements,
// CodeGen.fs:9889): genPrintArrayFlat (rank 2 nested, every other rank flat)
// or genPrintArraySymAware (symmetric; placeholder past rank 8). Sym-aware,
// ragged, and non-scalar-element paths raise ArrayOpUnsupported (gate SKIPs).
// Called by Print.printBindings in place of its PrintUnsupported raise,
// appending to the same StringBuilder (no timing line -- printBindings emits
// that once).
//
// Flat format (genPrintArrayFlat, verified against the compiled binary):
//   rank 2   :  name = [[a, b], [c, d]]\n      (nested, round-trips as source)
//   other    :  name = [c0, c1, c2, ...]\n     (row-major, ", " between cells,
//                                              EVERY rank >= 1 except 2)
//   rank 0   :  name = <rank-0>\n
// (The rank-4 grid and rank-5+ `<rank-N array>` dense formats are retired:
// rank-3 already printed flat, and neither of those two emitted a line the
// EXPECT harness could parse as a value.)
// Each cell renders as `cout << name[...]` would for the element's C++ static
// type (formatFloat15 for Float64, etc).

let private isPrintableScalarEt (et: ElemType) : bool =
    match et with
    | ETFloat64 | ETFloat32 | ETInt64 | ETInt32 | ETBool | ETComplex64 | ETComplex128 | ETString -> true
    | ETUnit -> false

/// Render one array cell exactly as `cout << name[...]` for its element type,
/// coercing the stored Value to that static type (mirrors Print.formatScalar).
let private formatCell (et: ElemType) (v: Value) : string =
    match et with
    | ETFloat64 ->
        match v with
        | VFloat f -> formatFloat15 f
        | VFloat32 f -> formatFloat15 (float f)
        | VInt n -> formatFloat15 (float n)
        | VInt32 n -> formatFloat15 (float n)
        | VComplex (r, _) -> formatFloat15 r
        | _ -> formatFloat15 nan
    | ETFloat32 ->
        match v with
        | VFloat32 f -> formatFloat32 f
        | VFloat f -> formatFloat32 (float32 f)
        | VInt n -> formatFloat32 (float32 n)
        | VInt32 n -> formatFloat32 (float32 n)
        | _ -> formatFloat32 (float32 nan)
    | ETInt64 ->
        match v with
        | VInt n -> formatInt64 n
        | VInt32 n -> formatInt64 (int64 n)
        | VBool b -> formatInt64 (if b then 1L else 0L)
        | VFloat f -> formatInt64 (int64 f)
        | _ -> formatInt64 0L
    | ETInt32 ->
        match v with
        | VInt32 n -> formatInt32 n
        | VInt n -> formatInt32 (int32 n)
        | VChar c -> formatInt32 (int32 c)
        | _ -> formatInt32 0
    | ETBool ->
        match v with
        | VBool b -> formatBool b
        | VInt n -> formatBool (n <> 0L)
        | _ -> formatBool false
    | ETComplex128 | ETComplex64 ->
        match v with
        | VComplex (r, im) -> formatComplex r im
        | VFloat f -> formatComplex f 0.0
        | VFloat32 f -> formatComplex (float f) 0.0
        | VInt n -> formatComplex (float n) 0.0
        | VInt32 n -> formatComplex (float n) 0.0
        | _ -> formatComplex 0.0 0.0
    | ETString ->
        match v with
        | VString s -> formatString s
        | _ -> ""
    | ETUnit -> ""

/// Iterate every coordinate of a dense array in row-major order.
let private forEachCoordRowMajor (extents: int64[]) (f: int64 list -> unit) : unit =
    let n = extents.Length
    let rec loop (dim: int) (acc: int64 list) =
        if dim = n then f (List.rev acc)
        else
            let e = extents.[dim]
            let mutable i = 0L
            while i < e do
                loop (dim + 1) (i :: acc)
                i <- i + 1L
    loop 0 []

/// Rank 2: `name = [[a, b], [c, d]]` -- the twin of CodeGen.genPrintNested2, and
/// byte-identical to it. `innerBound` takes the outer coordinate because a
/// compact group's row shrinks with it (`extents[1] - i`, minus one more when
/// the group is strict); a dense pair ignores its argument.
let private emitNested2 (sb: StringBuilder) (name: string) (arr: BladeArray) (et: ElemType)
                        (outerBound: int64) (innerBound: int64 -> int64) : unit =
    sb.Append(name).Append(" = [") |> ignore
    let mutable i = 0L
    while i < outerBound do
        if i > 0L then sb.Append(", ") |> ignore
        sb.Append("[") |> ignore
        let mutable first = true
        let mutable j = 0L
        while j < innerBound i do
            if not first then sb.Append(", ") |> ignore
            first <- false
            sb.Append(formatCell et (readCell arr [ i; j ])) |> ignore
            j <- j + 1L
        sb.Append("]") |> ignore
        i <- i + 1L
    sb.Append("]").Append('\n') |> ignore

/// Every rank except 2: `name = [c0, c1, ...]` row-major, ", "-separated, `]`,
/// newline. Rank 2 goes through emitNested2 -- see its twin's note on why only
/// that rank nests. The twin of CodeGen.genPrintArrayFlat, byte-identical for
/// any rank (the coord walk is rank-generic).
let private emitFlat (sb: StringBuilder) (name: string) (arr: BladeArray) (et: ElemType) : unit =
    if arr.Extents.Length = 2 then
        emitNested2 sb name arr et arr.Extents.[0] (fun _ -> arr.Extents.[1])
    else
    sb.Append(name).Append(" = [") |> ignore
    let mutable first = true
    forEachCoordRowMajor arr.Extents (fun coords ->
        if not first then sb.Append(", ") |> ignore
        first <- false
        sb.Append(formatCell et (readCell arr coords)) |> ignore)
    sb.Append("]").Append('\n') |> ignore

/// Symmetric-aware print: mirrors CodeGen.genPrintArraySymAware (CodeGen.fs:9791)
/// exactly. Iterates the compact (triangular/strict-triangular) index space in
/// left-justified storage coordinates -- bound at group component a is
/// `extents[d] - sum(prior group vars) - a*strictConst` (strictConst 1 for
/// antisym, 0 for sym/Hermitian) -- reading each raw stored cell (canonical by
/// construction, no fold needed). Framing matches the flat printer.
let private emitSymAware (sb: StringBuilder) (name: string) (arr: BladeArray) (et: ElemType) : unit =
    // Per-dimension descriptor in flattened order: (dimIdx, priorGroupDims, strictConst).
    let dims = ResizeArray<int * int list * int>()
    let mutable dimIdx = 0
    for ix in arr.IndexTypes do
        let a = max 1 ix.Rank
        // The interpreter print path must byte-match the compiled one; neither
        // has a wreath walk, and a triangular walk here would print a cell set
        // that no other path agrees with.
        if ix.Symmetry = SymWreath then
            failwith (Blade.IR.orbitStorageUnsupported "compact print (interp emitSymAware)"
                                                       (Blade.IR.orbitLevelsOf ix))
        let isSym =
            ix.Symmetry = SymSymmetric || ix.Symmetry = SymAntisymmetric || ix.Symmetry = SymHermitian
        let strictConst = if ix.Symmetry = SymAntisymmetric then 1 else 0
        let groupStart = dimIdx
        for comp in 0 .. a - 1 do
            let priorDims = if isSym && comp > 0 then [ groupStart .. groupStart + comp - 1 ] else []
            dims.Add(dimIdx, priorDims, strictConst)
            dimIdx <- dimIdx + 1
    let rank = dims.Count
    // Bound at one dimension: extent minus the prior group coords minus the
    // strict constant -- the twin of genPrintArraySymAware's `boundAt`.
    let boundAt (d: int) (coords: int64[]) =
        let (dIdx, priorDims, strictConst) = dims.[d]
        let sub = (priorDims |> List.sumBy (fun pd -> coords.[pd])) + int64 (List.length priorDims * strictConst)
        arr.Extents.[dIdx] - sub
    if rank < 1 || rank > 8 then
        sb.Append(name).Append(" = <rank-").Append(string rank).Append(" array>").Append('\n') |> ignore
    elif rank = 2 then
        emitNested2 sb name arr et (boundAt 0 [| 0L; 0L |]) (fun i -> boundAt 1 [| i; 0L |])
    else
        sb.Append(name).Append(" = [") |> ignore
        let coords : int64[] = Array.zeroCreate rank
        let mutable first = true
        let rec loop (d: int) =
            if d = rank then
                if not first then sb.Append(", ") |> ignore
                first <- false
                sb.Append(formatCell et (readCell arr (List.ofArray coords))) |> ignore
            else
                let (dIdx, priorDims, strictConst) = dims.[d]
                let sub = (priorDims |> List.sumBy (fun pd -> coords.[pd])) + int64 (List.length priorDims * strictConst)
                let bound = arr.Extents.[dIdx] - sub
                let mutable i = 0L
                while i < bound do
                    coords.[d] <- i
                    loop (d + 1)
                    i <- i + 1L
        loop 0
        sb.Append("]").Append('\n') |> ignore

/// Flatten a ragged/nested store's leaves in row-major order into a Value list.
/// Used by the ragged auto-print, which streams the flat backing buffer.
let rec private raggedFlatValues (s: Store) : Value list =
    match s with
    | SFloat a -> a |> Array.toList |> List.map VFloat
    | SInt a -> a |> Array.toList |> List.map VInt
    | SComplex a -> a |> Array.toList |> List.map (fun (struct (r, im)) -> VComplex (r, im))
    | SBool a -> a |> Array.toList |> List.map VBool
    | SObj a -> a |> Array.toList
    | SNested rows -> rows |> Array.toList |> List.collect raggedFlatValues
    | SRagged (rows, _, _) -> rows |> Array.toList |> List.collect raggedFlatValues

/// Print a top-level ARRAY binding to `sb`, byte-matching the compiled binary.
/// Dense scalar-element arrays (flat 1-3, grid 4, placeholder) via the flat
/// emitters; symmetric/antisym/Hermitian (rank 2-8) via emitSymAware; ragged
/// literals stream their flat backing buffer; non-scalar element arrays raise
/// ArrayOpUnsupported (LATER; gate SKIPs).
let printArrayBinding (b: IRBinding) (arr: BladeArray) (sb: StringBuilder) : unit =
    match stripUnits b.Type with
    | ArrayElem arrType ->
        match elemThrough arrType.ElemType with
        | Some et when isPrintableScalarEt et ->
            match arr.Data with
            // A group_by result is SRagged too, but its auto-print is the dense
            // flat print over Extents=[ngroups; 0] (inner extent 0 emits no
            // cells) -- route it to the flat emitter, not the backing pool.
            | SRagged (rows, lens, _) when (match b.Value with IRGroupBy _ -> false | _ -> true) ->
                // A ragged / DepIdx literal prints its rows nested, like every
                // other rank-2 array (`lens[i]` bounds each row): the row
                // boundary is the one thing the flat pool cannot show.
                sb.Append(b.Name).Append(" = [") |> ignore
                rows
                |> Array.iteri (fun i row ->
                    if i > 0 then sb.Append(", ") |> ignore
                    sb.Append("[") |> ignore
                    // `lens` is the row bound the compiled printer walks; a row
                    // store can hold more than its length (a peel or a provider
                    // read backs rows with a shared buffer).
                    raggedFlatValues row
                    |> List.truncate (int lens.[i])
                    |> List.iteri (fun j v ->
                        if j > 0 then sb.Append(", ") |> ignore
                        sb.Append(formatCell et v) |> ignore)
                    sb.Append("]") |> ignore)
                sb.Append("]").Append('\n') |> ignore
            | _ ->
                let rank = arr.Extents.Length
                // An OrbIdx (iterated-wreath) binding prints its POOL CELLS in
                // storage (visitStream/orb_cell_count) order, with the framing
                // every other array printer uses. Checked FIRST: a wreath
                // record reads as "compact" below, but its store is flat and
                // neither emitSymAware nor the dense emitters describe it.
                if hasWreath arrType.IndexTypes then
                    sb.Append(b.Name).Append(" = [") |> ignore
                    let cells = wreathCellCount arr
                    for k in 0 .. cells - 1 do
                        if k > 0 then sb.Append(", ") |> ignore
                        sb.Append(formatCell et (readCell arr [ int64 k ])) |> ignore
                    sb.Append("]").Append('\n') |> ignore
                // Every symmetric rank routes to emitSymAware, whose internal
                // guard prints the `<rank-N array>` placeholder past rank 8;
                // emitFlat dense-walks EVERY rank and would misread compact
                // storage. Same routing as CodeGen genPrintStatements.
                elif hasSymmetry arrType.IndexTypes && rank >= 2 then
                    emitSymAware sb b.Name arr et
                elif rank < 1 then
                    sb.Append(b.Name).Append(" = <rank-0>").Append('\n') |> ignore
                else
                    emitFlat sb b.Name arr et
        | _ -> raise (ArrayOpUnsupported $"print: array '{b.Name}' of non-scalar element type")
    | _ -> raise (ArrayOpUnsupported $"print: binding '{b.Name}' is not an array type")
