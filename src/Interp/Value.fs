// Blade tree-walking interpreter: value domain (Milestone 1 foundation).
//
// The interpreter walks the typed IR (Blade.IR) and produces printed output
// that must byte-match the compiled C++ binaries (the differential gate). This
// file owns the runtime value universe the walker manipulates; Numerics.fs owns
// the bit-exact scalar arithmetic over it. Nothing here evaluates -- these are
// data definitions plus the environment/limit/panic scaffolding the evaluator
// plugs into.
//
// Compiled inside Blade.fsproj after IR.fs and CodeGen.fs, so it freely
// references the project's concrete IR types:
//   - IRType        = IRTypeG<IRExpr>     (IR.fs:385)   -- carried by arrays/closures
//   - IRIndexType   = IRIndexTypeG<IRExpr>(IR.fs:386)   -- array dimension structure
//   - IRExpr        (IR.fs:46)            -- suspended-computation payload
//   - IRCallable    (IR.fs:239)           -- closure provenance (Params/Body/Captures)
//   - IRId = int    (Types.fs:10)         -- binding identity, the env key
//   - ElemType      (Types.fs:285)        -- scalar element discriminator
module Blade.Interp.Value

open System.Collections.Generic
open Blade.Types
open Blade.IR

// The value universe. Scalar spelling mirrors ElemType 1:1 so a Value
// round-trips its declared type without a side table:
//   VInt      <-> ETInt64        VInt32    <-> ETInt32
//   VFloat    <-> ETFloat64      VFloat32  <-> ETFloat32
//   VComplex  <-> ETComplex128   VBool     <-> ETBool
//   VString   <-> ETString       VUnit     <-> ETUnit
//   VChar                        -- char literals lower to ETInt32
//                                  (TypeCheck.fs:1095); VChar is kept for
//                                  surface fidelity/printing only.
//
// Known gap: ETComplex64 (std::complex<float>) has no distinct value case --
// VComplex carries double components only (Complex128 is the first-class
// type); a width-tagged complex is deferred to the milestone that needs it.
[<CustomEquality; NoComparison>]
type Value =
    | VInt of int64
    | VInt32 of int32
    | VFloat of float
    | VFloat32 of float32
    | VComplex of re: float * im: float
    | VBool of bool
    | VString of string
    | VChar of char
    | VUnit
    | VTuple of Value[]
    | VStruct of typeName: string * fields: (string * Value)[]
    | VVariant of typeName: string * tag: int * payload: Value option
    | VArray of BladeArray
    // A callable value: the reusable IRCallable plus the captured free-variable
    // cells (by binding id -- IRCallable.Captures carries the CaptureInfo.Id
    // keys), captured by ValueRef so mutation through the closure is visible
    // to the defining scope, matching CodeGen's `T&`-by-reference capture
    // semantics for lifted lambdas (IR.fs:280 CaptureInfo).
    | VClosure of callable: IRCallable * captures: Map<IRId, ValueRef>
    | VLoopObj of LoopObjValue
    // A suspended computation (Blade's deferred-array / IRCompute discipline):
    // the unevaluated expression plus the environment it must be forced in.
    | VDeferred of expr: IRExpr * env: Env
    | VGroupKeys of GroupKeysValue
    | VCompound of CompoundValue
    | VSparse of SparseValue

    // Value embeds IRExpr / IRCallable / Env (a Dictionary) -- deep structural
    // equality would be surprising and costly, and comparison is undefined for
    // the env. Physical equality is the right identity here; the evaluator
    // compares scalars by destructuring through Numerics, never `=` on Value.
    override this.Equals(o: obj) = System.Object.ReferenceEquals(this, o)
    override this.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(this)

/// A mutable value cell. The unit of binding: env maps a binding id to a cell,
/// closures capture cells, and IRAssign writes through a cell. Identity (not
/// contents) is its equality, so aliasing is observable.
and [<ReferenceEquality>] ValueRef = { mutable V: Value }

// Arrays and their backing stores.
//
// A BladeArray is the runtime image of a Blade Array<Elem like I, J, ...>. It
// keeps the full IR element type and index types so the evaluator can reproduce
// CodeGen's shape decisions (symmetry-compact placement, ragged rows, index
// tags) and print with the exact formatting the C++ runtime uses. `Extents` is
// the per-dimension logical size (row-major); `Data` is the flat backing pool.
and BladeArray = {
    ElemType: IRType
    IndexTypes: IRIndexType list
    Extents: int64[]
    Data: Store
}

// The flat pool. Primitive stores are unboxed for the common Float64 / Int64 /
// Complex128 / Bool cases (the vast majority of Blade arrays); SObj carries
// structs, tuples, strings, nested arrays, or any non-primitive element. SNested
// models regular multi-level nesting (array-of-arrays with uniform sub-shape);
// SRagged is the CSR-style per-row layout shared by RaggedIdx / DepIdx / group
// members (CodeGen's Ragged<T>: flat backing + per-row lens + prefix offsets,
// CodeGen.fs ~4237). Known gap: no SFloat32 / SInt32 / SComplex64 stores --
// narrow element types widen into SFloat / SInt / SComplex or fall to SObj
// (scalar Float32/Int32 ARE exact in Numerics; this is only their array form).
and Store =
    | SFloat of float[]
    | SInt of int64[]
    | SComplex of struct(float * float)[]
    | SBool of bool[]
    | SObj of Value[]
    | SNested of Store[]
    | SRagged of rows: Store[] * lens: int64[] * offsets: int64[]

// Placeholder value shapes (fleshed out in later milestones).

/// A reified loop object: `method_for(...)` / `object_for(f)` and their
/// compositions, awaiting the complementary operand before `<@>` forces a nest.
/// Records the provenance IRExpr and the environment captured at
/// reification, which is enough to force it later.
and LoopObjValue = {
    /// Provenance: IRMethodFor / IRObjectFor / IRComposeObj / IRComposeMeth.
    Provenance: IRExpr
    /// Environment captured when the loop object was constructed.
    Captured: Env
}

/// group_keys result: the CSR grouping structure (IRTGroupKeys, Types.fs:420,
/// maps a sourceIdx into groups indexed by an outerIdx). `Offsets` is the CSR
/// row-pointer (length nGroups+1, exclusive prefix-sum of the per-group counts);
/// `Members` are the source positions in group-contiguous order (the CSR "perm",
/// genGroupKeysBinding's `<name>__perm`, CodeGen.fs:7603-7606). A group g's
/// members are `Members.[Offsets.[g] .. Offsets.[g+1]-1]`, each an index into the
/// original value array; its key can be recovered as `keys.[Members.[Offsets.[g]]]`
/// (the compiler stores no keys array, 4.2). ReferenceEquality: never compared
/// structurally.
and [<ReferenceEquality>] GroupKeysValue = {
    Offsets: int64[]
    Members: int64[]
    /// A GRID grouping (segments(C0, C1)): the coordinates of each member
    /// position, since the value is rank 2 and `Members` (flat row-major
    /// indices) cannot address it. None for every other grouping.
    Coords: int64 list[] option
}

/// A compound (masked product-space) index value: formalism 4.5 CompoundIdx
/// (IRCompoundMask, IR.fs:164) / runtime `Compound<T,RANK>` + `compound_index_t`
/// (cpp/nested_array_types.hpp:133, index_types.h:235). Bundles the full
/// rank<->tuple bijection with a compact backing buffer of the present cells,
/// so full/partial/trailing reads and reduce reproduce the C++ helpers byte-for-byte.
///
///   ElemType       - the stored element type (drives read coercion / print).
///   IndexTypes     - the full index-type list (compound head slot + any
///                    trailing regular dims); carried for downstream typing.
///   LeadRank       - k, the number of masked (compound) leading dims (= mask
///                    rank). A rank-1 compound is the filtered-set case.
///   LeadExtents    - extents of the k masked dims (row-major); mask_offset base.
///   Mask           - flat mask bits over the masked grid (length = product
///                    LeadExtents, row-major); the allocation record + submask
///                    source for partial (residual) reads.
///   Table          - rank_to_tuple: present coordinate tuples in row-major lex
///                    order, one per valid cell (each length LeadRank).
///   RankOf         - linearize: mask_offset(tuple) -> compact rank (O(1) reverse
///                    of Table; mask_offset is a unique per-grid-cell key).
///   Cardinality    - popcount(Mask) = |Table| (runtime, not closed form).
///   TrailingStride - product of the trailing regular extents; 1 when the mask
///                    covers all dims (the all-dims compound).
///   Data           - compact backing buffer, length Cardinality*TrailingStride,
///                    present cells in Table order, each followed by its trailing
///                    block (data[r*trailing_stride + t]).
/// ReferenceEquality: runtime value, never structurally compared (and the
/// RankOf Dictionary has no structural comparison anyway).
and [<ReferenceEquality>] CompoundValue = {
    ElemType: IRType
    IndexTypes: IRIndexType list
    LeadRank: int
    LeadExtents: int64[]
    Mask: bool[]
    Table: int64[][]
    RankOf: Dictionary<int64, int>
    Cardinality: int64
    TrailingStride: int64
    Data: Store
}

/// A sparse (explicit key enumeration) index value: formalism 3.5 SparseIdx
/// (IRSparseKeys) / runtime `Sparse<T,RANK>` + `sparse_index_t`. The compound
/// twin minus the grid: there are no per-axis extents and no mask, so the
/// reverse map keys structurally on the tuple itself (a grid offset does not
/// exist), and partial reads scan `Keys` in key order rather than a mask.
///
///   Keys           - rank_to_tuple in given order (never sorted); iteration
///                    order is this order.
///   RankOf         - linearize: tuple -> rank, via TupleKeyComparer (int64[]
///                    has no structural hashing in .NET).
///   Other fields   - as CompoundValue (compact Data in key order, each cell
///                    followed by its trailing block).
and [<ReferenceEquality>] SparseValue = {
    ElemType: IRType
    IndexTypes: IRIndexType list
    LeadRank: int
    Keys: int64[][]
    RankOf: Dictionary<int64[], int>
    Cardinality: int64
    TrailingStride: int64
    Data: Store
}

// Environment.
//
// Scoping model: lowered Blade IR is SSA-ish -- every let / parameter / loop
// variable gets a globally-unique IRId (IRLet's id, IRParam.VarId, IRForRange's
// varId, IRVar's id). So the primary binding structure is a flat mutable
// Dictionary<IRId, ValueRef>, direct-address, O(1), and a loop variable is a
// single reused cell mutated per iteration (matching the C++ loop counter).
//
// A `Parent` link is retained for the two places uniqueness is not enough:
// (1) function application binds this call's parameters in a FRESH child
// scope so recursion doesn't clobber the caller's frame; (2) lexical
// shadowing across independently-lowered fragments. Lookup walks Vars, then
// Parent. Closures capture their free cells explicitly (VClosure.captures),
// so they don't rely on the parent chain surviving.
and [<ReferenceEquality>] Env = {
    Vars: Dictionary<IRId, ValueRef>
    Parent: Env option
}

/// Structural equality/hash for int64[] key tuples (SparseValue.RankOf).
/// .NET arrays hash by reference; a Dictionary keyed on raw int64[] would
/// never find anything. Order-sensitive combine mirrors array_hasher; only
/// self-consistency matters here.
type TupleKeyComparer() =
    interface System.Collections.Generic.IEqualityComparer<int64[]> with
        member _.Equals(a, b) =
            a.Length = b.Length && Array.forall2 (=) a b
        member _.GetHashCode(a) =
            let mutable h = 17
            for v in a do
                h <- h * 31 + int (v ^^^ (v >>> 32))
            h

// Environment helpers.

/// A fresh empty root environment.
let envNew () : Env = { Vars = Dictionary<IRId, ValueRef>(); Parent = None }

/// A fresh child scope over `parent` (used per function call / nested block).
let envChild (parent: Env) : Env = { Vars = Dictionary<IRId, ValueRef>(); Parent = Some parent }

/// Look up a binding cell, walking the parent chain. None if unbound.
let rec envTryFind (env: Env) (id: IRId) : ValueRef option =
    match env.Vars.TryGetValue id with
    | true, cell -> Some cell
    | _ -> match env.Parent with
           | Some p -> envTryFind p id
           | None -> None

/// Bind (or rebind, in this scope) `id` to a fresh cell holding `v`, returning
/// the cell. Rebinding in the same scope replaces the cell (lexical shadowing).
let envBind (env: Env) (id: IRId) (v: Value) : ValueRef =
    let cell = { V = v }
    env.Vars.[id] <- cell
    cell

/// Bind an existing cell (closure capture / by-reference parameter passing).
let envBindRef (env: Env) (id: IRId) (cell: ValueRef) : unit =
    env.Vars.[id] <- cell

// Store / array deep copy.

/// Deep-copy a backing store. Element Values themselves are copied by
/// reference (SObj) -- they are treated immutably by the evaluator (mutation
/// goes through cells, never in-place on a Value), exactly as the C++ pool
/// copy copies struct elements by value.
let rec copyStore (s: Store) : Store =
    match s with
    | SFloat a -> SFloat (Array.copy a)
    | SInt a -> SInt (Array.copy a)
    | SComplex a -> SComplex (Array.copy a)
    | SBool a -> SBool (Array.copy a)
    | SObj a -> SObj (Array.copy a)
    | SNested a -> SNested (a |> Array.map copyStore)
    | SRagged (rows, lens, offsets) ->
        SRagged (rows |> Array.map copyStore, Array.copy lens, Array.copy offsets)

/// Deep-copy a BladeArray (fresh extents + fresh backing pool). The
/// differential twin of CodeGen's materializeArrayCopyForm (fresh allocate<>
/// + std::copy_n), backing the copy semantics of assignable array bindings
/// (`let mut a = Z`; IRModule.MutableArrayLets).
let copyBladeArray (ba: BladeArray) : BladeArray =
    { ba with Extents = Array.copy ba.Extents; Data = copyStore ba.Data }

// Interpreter faults and resource limits.

/// A runtime fault raised by the interpreter, carrying the same shape as the
/// C++ runtime's blade_rt::panic (error code + message + source provenance).
/// `code` is a BLxxxx diagnostic code (see the diagnostics registry); `file` is
/// None for synthesized/spanless checks (the C++ side then panics with a null
/// file / line 0, per IRConstraintCheck, IR.fs:207).
exception InterpPanic of code: string * msg: string * file: string option * line: int

/// Resource ceilings that bound a single interpreter run so a divergent or
/// pathological program fails loudly instead of hanging the differential gate.
/// Generous by default -- real corpus programs are far under these.
///   MaxSteps - evaluator step budget (one per reduction).
///   MaxDepth - recursion / call-stack depth.
///   MaxCells - total array cells allocated across the run.
type InterpLimits = {
    MaxSteps: int64
    MaxDepth: int
    MaxCells: int64
}

/// Default ceilings (billion steps / cells, 100k deep) -- high enough that no
/// legitimate corpus program trips them, low enough to catch runaways.
let defaultLimits : InterpLimits =
    { MaxSteps = 1_000_000_000L
      MaxDepth = 100_000
      MaxCells = 1_000_000_000L }
