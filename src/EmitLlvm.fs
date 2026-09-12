// The direct LLVM back end (`BLADE_LLVM` lane): scalar programs plus DENSE
// STATIC-EXTENT arrays.
//
// docs/plans/plan-llvm-backend.md section 5 fixes the shape this implements:
// textual `.ll` written by F# and handed to `clang file.ll shim.o -O3` in ONE
// step (no opt/llc orchestration), opaque pointers only, and -- the decision
// that keeps this file small -- SSA BY ALLOCA. Every binding, parameter and
// match-arm variable gets an `alloca` plus loads and stores; clang's own -O3
// runs mem2reg/SROA and rebuilds real SSA from it. That is how clang itself
// emits C, and it deletes the dominance-frontier problem from the emitter, so
// there is no phi node anywhere below.
//
// REFUSAL IS WHOLE-PROGRAM. `tryEmitProgram` returns `Error <reason>` naming
// the first unsupported construct and emits NOTHING; the caller prints one
// notice and compiles the same program through the C++ lane instead. A
// half-emitted module is never produced, so the lane can only be all-correct
// or absent -- which is what makes it safe to develop against a byte-pinned
// corpus.
//
// THE BOUNDARY is Int64/Float64/Bool/String scalars, and arrays over
// `Idx<n>` axes whose extents are compile-time constants. Every array is a
// flat row-major pool (no Iliffe skeleton, no triangular packing) with the
// extents baked into its GEPs, and every UNFORCED computation -- a
// combinator, `A + B`, `range<I>` -- is a producer closure that never
// acquires storage until `|> compute` asks for it. Refused by name, each
// with its own wording: symmetric/antisymmetric (compact) storage, runtime
// extents, ragged/compound/sparse/orbit index types, providers, cuda/mpi,
// complex, `let rec` (IRForRange), structs and display frames.
//
// NUMERIC POLICY (plan section 6): the DEFAULT emission carries no fast-math
// flag at all. Default LLVM IR contracts nothing and reassociates nothing, so
// the emitted arithmetic is order-preserving by construction -- the favorable
// inversion over the g++ lane, which needs `-ffp-contract=off` pinned to get
// there. Two per-instruction opt-ins exist, both off unless asked: `reassoc
// nsz` on a fold whose kernel carries the SAME license the C++ lane's K-lane
// forms spend (`foldReorderLicensed` and `BLADE_FP_REASSOC` together), and
// `contract` under `BLADE_FP_CONTRACT`. See the fact-layer section below.
//
// The printers this emits call into `src/cpp/blade_llvm_shim.c`, whose output
// bytes are pinned to the C++ lane's `cout << setprecision(15) << boolalpha`.
module Blade.EmitLlvm

open System
open System.Collections.Generic
open System.Text
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage

/// Raised anywhere in the walk to abandon the WHOLE program. Carries the
/// one-line reason `tryEmitProgram` hands back.
exception LlvmRefusal of string

let private refuse (msg: string) : 'a = raise (LlvmRefusal msg)

/// The DU case name of an IR node, for refusals that have nothing better to
/// say than "no arm for this". Reflection is fine: it only runs on the
/// refusal path, which ends the emission.
let private caseName (e: IRExpr) : string =
    let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(e, typeof<IRExpr>)
    case.Name

// ---------------------------------------------------------------------------
// The scalar universe
// ---------------------------------------------------------------------------

/// Every value v1 can hold. Deliberately NOT ElemType: this is the set the
/// emitter has a lowering for, and keeping it separate is what makes an
/// unsupported width (Int32/Float32/complex) a refusal instead of a silent
/// re-typing.
type private Sc =
    | ScI64
    | ScF64
    | ScBool
    | ScStr
    | ScVoid

let private llTy = function
    | ScI64 -> "i64"
    | ScF64 -> "double"
    | ScBool -> "i1"
    | ScStr -> "ptr"
    | ScVoid -> "void"

/// Project a Blade type onto the scalar universe. `IRTIdxTagged` (nominal
/// index tags) and `IRTUnitAnnotated` (physical units) are TRANSPARENT here
/// for the same reason they are at C++ codegen: both erase to their inner
/// type, so a `Float<mps>` binding is an ordinary double.
let rec private scalarTyOf (t: IRType) : Sc option =
    match t with
    | IRTScalar ETInt64 -> Some ScI64
    | IRTScalar ETFloat64 -> Some ScF64
    | IRTScalar ETBool -> Some ScBool
    | IRTScalar ETString -> Some ScStr
    | IRTScalar ETUnit | IRTUnit -> Some ScVoid
    | IRTIdxTagged (inner, _) -> scalarTyOf inner
    | IRTUnitAnnotated (inner, _) -> scalarTyOf inner
    | _ -> None

let private requireScalar (what: string) (t: IRType) : Sc =
    match scalarTyOf t with
    | Some sc -> sc
    | None ->
        refuse ($"{what} has type {(Blade.IRPrint.ppIRType t)} -- the llvm lane handles Int64/Float64/Bool/String scalars only")

// ---------------------------------------------------------------------------
// The fact layer (plan section 5)
//
// What the front end PROVED, serialized where stock -O3 passes read it:
// function attributes, parameter attributes, and per-instruction fast-math
// flags. Three rules govern everything below.
//
//   1. A FACT IS EMITTED ONLY WHERE IT IS PROVED. The red team's calibration
//      (plan section 8, risk 1: Rust shipped mutable-noalias in 2017,
//      regressed immediately, and it is still unstable six years on) is that
//      an alias fact asserted one notch beyond its proof is a miscompile, not
//      a slow program. So `noalias` appears on exactly one thing -- the
//      allocator's FRESH return pointer -- and never on a read operand, and
//      `readonly` is gated on a whole-module scan for element writes rather
//      than on a per-parameter guess.
//
//   2. FACTS ATTACH AT INSTRUCTION BIRTH, never by a later pass. Each builder
//      below takes its attribute text as a field, so there is no "decorate the
//      module afterwards" step that could decorate the wrong instruction.
//
//   3. ONE KILL SWITCH, so a miscompile bisects to a fact class in one run.
//      `BLADE_LLVM_FACTS=off` turns all of it off and returns the lane to the
//      shape that passed its differential before this layer existed;
//      `BLADE_LLVM_FACTS=fmf:off` (or `fnattrs:off`, `paramattrs:off`) turns
//      off one class.
//
// The record `ModuleFacts` is the seed of the backend-neutral `BackendFacts`
// the plan hands to a future EmitMlir: computed ONCE from the IR, consumed by
// serialization only. It lives here until a second backend needs it.
// ---------------------------------------------------------------------------

/// The three fact classes, as the kill switch spells them.
let private factFnAttrs = "fnattrs"
let private factParamAttrs = "paramattrs"
let private factFmf = "fmf"

/// Is this fact class licensed to emit? A FUNCTION, never a module-level
/// `let`, for the reason `Build.llvmEnabled` and `CodeGenState.fpReassocEnabled`
/// are: a harness that pins the variable mid-process must be honored by the
/// next emission.
///
/// `BLADE_LLVM_FACTS` unset (or set to anything without an `off` token) means
/// EVERY class emits -- the facts are the point of the lane. A bare `off`/`0`
/// disables all three; `<class>:off` disables one. An unrecognized token is
/// ignored rather than fatal: this is a bisection knob, and a typo that
/// silently disabled everything would make a bisection lie.
let factEnabled (cls: string) : bool =
    match System.Environment.GetEnvironmentVariable "BLADE_LLVM_FACTS" with
    | null | "" -> true
    | v ->
        let toks =
            v.Split([| ','; ';'; ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun t -> t.Trim().ToLowerInvariant())
        if toks |> Array.exists (fun t -> t = "off" || t = "0" || t = "none") then false
        else
            let target = cls.ToLowerInvariant()
            not (toks |> Array.exists (fun t -> t = target + ":off" || t = "-" + target))

/// Does `BLADE_FP_CONTRACT` license contraction (fmul+fadd -> fma)?
///
/// THE POLARITY IS DELIBERATELY OPPOSITE TO THE g++ LANE, and this is the one
/// favorable inversion the plan calls out (section 5). g++ contracts by
/// DEFAULT and needs `-ffp-contract=off` pinned to reach byte identity with
/// the interpreter; LLVM IR contracts only where the `contract` flag is
/// present, so the default emission is already order- and value-preserving and
/// `BLADE_FP_CONTRACT` becomes a per-instruction OPT-IN. Unset therefore means
/// OFF here while it means `fast` there -- `Build.llvmOptFlags` correspondingly
/// passes no `-ffp-contract` to clang, because the flag would not reach IR that
/// no C front end produced.
let private contractLicensed () : bool =
    if not (factEnabled factFmf) then false
    else
        match System.Environment.GetEnvironmentVariable "BLADE_FP_CONTRACT" with
        | null | "" -> false
        | v ->
            match v.Trim().ToLowerInvariant() with
            | "fast" | "on" | "1" -> true
            | _ -> false

// Attribute-group indices. Groups rather than inline attribute lists because
// every `define` in a Blade module carries the SAME set: one line at the
// bottom of the module beats the same twelve words on every function, and it
// is what clang itself emits.
//
// The split between 0 and 1 is the honest one. `willreturn`/`mustprogress`
// assert TERMINATION, which this lane can prove for a module whose loops are
// all statically counted (they all are -- runtime extents are refused) and
// whose call graph has no cycle. A single self-recursive function anywhere
// costs every function in the module those two attributes, because a
// non-recursive caller of a possibly-non-terminating callee does not itself
// terminate. `norecurse` stays per-function: mutual recursion is rejected by
// the front end (BL4006/BL2001 under define-before-use), so "does not call
// itself directly" IS "does not recurse".
let private grpFnTerminating = 0      // no recursion anywhere in the module
let private grpFnNoRecurse = 1        // this function does not self-call, but some function does
let private grpFnRecursive = 2        // this function self-calls
let private grpExternReturns = 3      // the shim's printers and libm: effects, but always returns
let private grpShimAlloc = 4          // the pool allocator
let private grpShimPanic = 5          // the panic path
let private grpShimFree = 6           // the pool deallocator (obviously not `nofree`)

let private attrGroupText (anyFrees: bool) (g: int) : string =
    // Blade has no exceptions (nounwind: deletes all Windows SEH lowering)
    // and no atomics or threads in this lane (nosync). `nofree` holds for the
    // whole module ONLY while no scope-exit free was emitted (`Ctx.AnyFrees`):
    // module-scope pools live to process exit as before, but a scoped body
    // that frees its temps makes every function's blanket claim false, so the
    // groups drop it together. (Groups are shared across functions by design;
    // per-function precision would need per-function attribute lists.)
    let drop (s: string) = if anyFrees then s.Replace("nofree ", "") else s
    if g = grpFnTerminating then drop "mustprogress nofree norecurse nosync nounwind willreturn"
    elif g = grpFnNoRecurse then drop "nofree norecurse nosync nounwind"
    elif g = grpFnRecursive then drop "nofree nosync nounwind"
    // The shim's printers do I/O and libm may set errno, so neither gets a
    // `memory(...)` claim -- only the two properties both really have. Their
    // `nofree` is a claim about the SYMBOL, not the module, and stays true.
    elif g = grpExternReturns then "nofree nounwind willreturn"
    // NOT `willreturn`: the allocator's out-of-memory arm calls blade_panic,
    // which exits the process instead of returning.
    elif g = grpShimAlloc then "nofree nounwind"
    elif g = grpShimPanic then "cold noreturn nounwind"
    elif g = grpShimFree then "nounwind willreturn"
    else ""

/// The facts a whole module carries, computed once before any instruction is
/// built. Every field is a PROOF OBLIGATION discharged by a scan, not an
/// assumption: the comments name what would have to be true for the fact to be
/// wrong.
type private ModuleFacts =
    { /// Callable ids whose body calls themselves. Over-approximating this set
      /// is safe (it only withholds attributes); under-approximating it would
      /// assert `norecurse` about a recursive function.
      SelfRecursive: HashSet<IRId>
      /// True when `SelfRecursive` is non-empty -- see the group 0/1 note.
      AnyRecursion: bool
      /// True when NO element write exists anywhere in the module, which makes
      /// every array parameter `readonly`. The scan is whole-module rather
      /// than per-function because a write can be one call deep: a callee's
      /// `a(i) = v` lands in the pool ITS CALLER passed down, so a caller
      /// cannot be read-only unless nothing downstream writes either. Kernels
      /// are covered because they are callables in the same table.
      ArraysReadOnly: bool
      /// Ids whose `IRVar` occurs AT MOST ONCE across the whole module
      /// (bindings, function bodies, callable bodies). A sole-read deferred
      /// binding may be CONSUMED -- its producer run inside the one
      /// consumer -- instead of forced into a pool, because the slot-upgrade
      /// contract `materializeExpr` protects (later readers share storage)
      /// is vacuous with no later reader. The census over-approximates
      /// safely in one direction: an id NOT in the set forces, which is
      /// exactly today's behavior. Callee references and kernel slots are
      /// also spelled `IRVar`, so callable ids land in the counts too --
      /// harmless, since only array-operand reads ever consult this set.
      ReadOnce: HashSet<IRId>
      /// Array-returning functions whose return is a pool the CALLER owns --
      /// THIS LANE's own fixpoint, mirroring the C++ lane's
      /// `computeFreshReturnFacts` but WITHOUT its interior-view arm: the C++
      /// return arm copies a returned slice into a fresh pool, while this
      /// lane returns the view's GEP itself, so a view return is never
      /// caller-owned here (and the fixpoint must drop the arm TRANSITIVELY,
      /// or `f() = g()` over a view-returning `g` would still classify
      /// fresh). Absent means NotFresh: unproven is leaked, never freed --
      /// the same one-sided rule the C++ facts state.
      FreshReturns: HashSet<IRId> }

let private computeModuleFacts (m: IRModule) (callables: Map<IRId, IRCallable>) : ModuleFacts =
    // The callables table maps SEVERAL ids to one record: `let f = lambda(..)`
    // adds an alias key beside the callable's own id. Iterate DISTINCT
    // records, or every body behind an alias is scanned twice -- harmless for
    // the monotone facts below, fatal for the read census (a doubled count is
    // never sole-read).
    let distinctCallables =
        callables |> Map.toSeq |> Seq.map snd |> Seq.distinctBy (_.Id) |> Seq.toList
    // RECURSION IS A CYCLE, NOT A SELF-CALL. This used to ask only "does
    // cl's body call cl", which is blind to every cycle of length > 1 --
    // and those are reachable: a NESTED function sees the enclosing name
    // (TypeCheckInfer registers a function before checking its body), so
    //     function f(n) = { function h(m) = ... f(m - 1) ...; h(n) }
    // is a genuine f -> h -> f cycle that the front end accepts (only
    // SIBLING mutual recursion is rejected, BL4006/BL2001 under
    // define-before-use). Asserting `norecurse` there is a miscompile
    // license, not a missed optimization, and `willreturn`/`mustprogress`
    // would additionally claim a recursion terminates when nothing proved
    // it. Inlined kernels need no special case: the callable is in this
    // graph whether the emitter inlines its body or calls it.
    //
    // Edges first, then "can a node reach itself" by transitive closure.
    // Modules hold tens of callables, so the naive fixpoint is free.
    let calleesOf = Dictionary<IRId, HashSet<IRId>>()
    for cl in distinctCallables do
        let outs = HashSet<IRId>()
        iterIRExpr
            (fun n ->
                match n with
                // A call spells its callee as `IRVar` of a table id, which may
                // be an ALIAS entry whose id differs from the callable's own
                // -- so the edge lands on the RESOLVED callable's id.
                | IRApp (IRVar (fid, _), _, _) ->
                    (match Map.tryFind fid callables with
                     | Some target -> outs.Add target.Id |> ignore
                     | None -> ())
                | _ -> ())
            cl.Body
        calleesOf.[cl.Id] <- outs
    // Transitive closure of the edge relation, iterated to a fixpoint.
    let reach = Dictionary<IRId, HashSet<IRId>>()
    for KeyValue (id, outs) in calleesOf do reach.[id] <- HashSet<IRId>(outs)
    let mutable changed = true
    while changed do
        changed <- false
        for KeyValue (id, seen) in reach |> Seq.map (fun kv -> kv) |> Seq.toList do
            let additions = HashSet<IRId>()
            for t in seen do
                match reach.TryGetValue t with
                | true, more -> for x in more do if not (seen.Contains x) then additions.Add x |> ignore
                | _ -> ()
            if additions.Count > 0 then
                for x in additions do seen.Add x |> ignore
                changed <- true
    let selfRec = HashSet<IRId>()
    for KeyValue (id, seen) in reach do
        if seen.Contains id then selfRec.Add id |> ignore
    let mutable writes = false
    let scanWrites (e: IRExpr) =
        iterIRExpr
            (fun n ->
                match n with
                // An element write through a (possibly `mut`) array.
                | IRAssign (IRIndex _, _) -> writes <- true
                // A whole-variable assignment rebinds a slot; it stores into an
                // alloca or re-points an array binding, and touches no pool.
                | IRAssign (IRVar _, _) -> ()
                // Any other assignment target is a shape this emitter has no
                // arm for. It will refuse later; until then, assume it writes.
                | IRAssign _ -> writes <- true
                | _ -> ())
            e
    for b in m.Bindings do scanWrites b.Value
    for f in m.Functions do scanWrites f.Body
    for cl in distinctCallables do scanWrites cl.Body
    // The read census for `ReadOnce`: every `IRVar` occurrence, everywhere
    // the writes scan looks. One traversal set, deliberately shared, so a
    // reader of either fact audits the same coverage.
    let reads = Dictionary<IRId, int>()
    let countReads (e: IRExpr) =
        iterIRExpr
            (fun n ->
                match n with
                | IRVar (vid, _) ->
                    reads.[vid] <- (match reads.TryGetValue vid with | true, k -> k | _ -> 0) + 1
                | _ -> ())
            e
    // The census roots are the EMISSION roots -- module bindings plus the
    // callables table -- and deliberately NOT `m.Functions`: the table IS
    // built from `m.Functions` (IRPrint.buildCallablesTable maps each one to
    // an entry), so listing both scans every function body twice and nothing
    // is ever sole-read. Distinct-by-record above closes the same hole for
    // alias keys.
    for b in m.Bindings do countReads b.Value
    for cl in distinctCallables do countReads cl.Body
    let readOnce = HashSet<IRId>()
    for KeyValue (vid, k) in reads do
        if k <= 1 then readOnce.Add vid |> ignore
    // The fresh-return fixpoint (see the field's note). The two positive arms
    // are exactly the shapes whose emission provably allocates INSIDE the
    // body: a lifted materializer tail, or a sole-use binding of one; a
    // returned call chains through the fact.
    let freshReturns =
        let isLifted (e: IRExpr) =
            match e with
            | IRCompute (IRApplyCombinator _) | IRArrayLit _ -> true
            | _ -> false
        let unrolled =
            m.Functions
            |> List.filter (fun f -> match f.RetType with ArrayElem _ -> true | _ -> false)
            |> List.map (fun f -> (f.Id, Blade.CodeGenLoopNest.deepUnrollBody f.Body))
        let acc = HashSet<IRId>()
        let rec fix () =
            let mutable changed = false
            for (fid, (lets, retExpr)) in unrolled do
                if not (acc.Contains fid) then
                    let fresh =
                        if isLifted retExpr then true
                        else
                            match retExpr with
                            | IRVar (rid, _) ->
                                let boundToLifted =
                                    lets |> List.exists (fun (id, v) -> id = rid && isLifted v)
                                let otherUses =
                                    lets |> List.exists (fun (id, v) ->
                                        id <> rid && Set.contains rid (collectVarRefsIR v))
                                boundToLifted && not otherUses
                            | IRApp (f, _, _) ->
                                match resolveCallable f with
                                | Some cl -> acc.Contains cl.Id
                                | None -> false
                            | _ -> false
                    if fresh then
                        acc.Add fid |> ignore
                        changed <- true
            if changed then fix ()
        fix ()
        acc
    { SelfRecursive = selfRec
      AnyRecursion = selfRec.Count > 0
      ArraysReadOnly = not writes
      ReadOnce = readOnce
      FreshReturns = freshReturns }

// ---------------------------------------------------------------------------
// Typed builders
//
// Every instruction shape with more than two slots is a named-field record
// (the EmitCpp.fs discipline): a positional sprintf with five holes is how an
// operand and a type end up transposed in a way no reader catches. Each
// builder renders exactly one `.ll` line.
// ---------------------------------------------------------------------------

/// An emitted value: the textual operand (an SSA register, or a literal --
/// `.ll` takes constants wherever it takes registers) plus its scalar type.
type private Val = { Reg: string; Ty: Sc }

type private BinInstr =
    { Dest: string
      Opcode: string
      /// Fast-math flags, ALREADY carrying a leading space when non-empty
      /// (`" reassoc nsz"`). A required field rather than an optional one on
      /// purpose: every arithmetic site in this file then states, in its own
      /// text, whether it is licensed to reassociate -- which is the whole
      /// discipline the numeric policy rests on. Integer sites pass "".
      Flags: string
      Ty: Sc
      Lhs: string
      Rhs: string }

type private CmpInstr =
    { Dest: string; Kind: string    // "icmp" | "fcmp"
      Pred: string; Ty: Sc; Lhs: string; Rhs: string }

type private ConvInstr =
    { Dest: string; Opcode: string; From: Sc; Value: string; To: Sc }

type private CallInstr =
    { Dest: string option; RetTy: Sc; Callee: string; Args: (Sc * string) list }

type private BrInstr =
    { Cond: string; IfTrue: string; IfFalse: string }

let private renderBin (i: BinInstr) =
    $"{i.Dest} = {i.Opcode}{i.Flags} {(llTy i.Ty)} {i.Lhs}, {i.Rhs}"

let private renderCmp (i: CmpInstr) =
    $"{i.Dest} = {i.Kind} {i.Pred} {(llTy i.Ty)} {i.Lhs}, {i.Rhs}"

let private renderConv (i: ConvInstr) =
    $"{i.Dest} = {i.Opcode} {(llTy i.From)} {i.Value} to {(llTy i.To)}"

let private renderCall (i: CallInstr) =
    let args = i.Args |> List.map (fun (t, v) -> $"{(llTy t)} {v}") |> String.concat ", "
    match i.Dest with
    | Some d -> $"{d} = call {(llTy i.RetTy)} {i.Callee}({args})"
    | None -> $"call {(llTy i.RetTy)} {i.Callee}({args})"

let private renderBr (i: BrInstr) =
    sprintf "br i1 %s, label %%%s, label %%%s" i.Cond i.IfTrue i.IfFalse

// ---------------------------------------------------------------------------
// Array shapes: dense axes and packed simplex groups
//
// An array's shape is a list of GROUPS, not a list of extents, because that is
// the granularity storage is decided at. A plain `Idx<n>` axis is its own
// group and stores n cells; a `SymIdx<r,n>` / `AntisymIdx<r,n>` group spans r
// AXES but stores only the canonical simplex -- C(n+r-1, r) or C(n, r) cells.
//
// TWO COORDINATE SYSTEMS, and every function below says which it takes.
//   * CANONICAL coordinates are the absolute ones: i <= j (or i < j).
//   * STORAGE coordinates are left-justified into each shrinking row:
//     (i, p) with p in [0, n - i - strict), canonical j = i + p + strict.
// The emitter works in STORAGE coordinates everywhere except the one place a
// user writes an index (`IRIndex`), which converts. That is not a taste
// decision: it is the convention the C++ lane already emits and the corpus
// already pins (loops/170-175 -- "the kernel receives PACKED STORAGE
// COORDINATES, the raw triangular loop counters"), so agreeing with it is how
// the two lanes' kernels see the same numbers.
// ---------------------------------------------------------------------------

/// One axis group of an array's shape. v1 admits compact groups of RANK 2
/// only; a rank-3+ simplex needs the multi-level left-justification the
/// storage plan calls prisms, and refuses by name at the construction sites.
type private Grp =
    | GDense of int64
    | GSym of int * int64
    | GAnti of int * int64
    /// A RAGGED PAIR: ONE group spanning TWO axes -- the static outer row
    /// axis and the per-row inner axis -- whose row base is a TABLE LOOKUP
    /// (offsets[i]) instead of a closed form. The static/dynamic split the
    /// IR already makes (IxKRaggedInline literals bake their lens; a ragged
    /// function parameter's lens arrive at run time) lives in the TABLE, so
    /// addressing and loop shapes are one code path for both lanes.
    | GRagged of rows: int64 * table: RaggedTable
    /// RANK 1 with an OPERAND extent: the residual of peeling one row off a
    /// ragged group. The operand is a literal for a static-lane row peeled at
    /// a literal index, a register otherwise -- either way `emitCountedLoopTo`
    /// takes it as-is.
    | GDynDense of len: string

/// Where a ragged group's offsets live. rows+1 entries; offsets[rows] is the
/// total cell count. The STATIC lane carries the values (folded straight into
/// addressing at literal indices) plus a `private unnamed_addr constant`
/// global for loop-variable indices -- `constant` proves no store can touch
/// it, which lets consecutive rows share one load. The DYNAMIC lane holds a
/// runtime `ptr` (a ragged parameter's sidecar argument) with IDENTICAL
/// addressing -- the two lanes differ in one operand token.
and private RaggedTable =
    | RtStatic of offsets: int64[] * tableSym: string
    | RtDynamic of tablePtr: string

let private grpRank = function GDense _ | GDynDense _ -> 1 | GSym (r, _) -> r | GAnti (r, _) -> r | GRagged _ -> 2
let private grpExtent =
    function
    | GDense n -> n | GSym (_, n) -> n | GAnti (_, n) -> n
    | GRagged _ -> refuse "a ragged index group where a single static extent is required"
    | GDynDense _ -> refuse "a runtime-extent axis where a static extent is required"
/// Antisymmetric groups are STRICT (i < j) and store no diagonal.
let private grpStrict (g: Grp) = g.IsGAnti
let private grpCompact = function GSym _ | GAnti _ -> true | _ -> false
let private isRaggedGrp (g: Grp) = g.IsGRagged
let private hasRagged (gs: Grp list) = gs |> List.exists isRaggedGrp

/// How many AXES a group spans, as extents (a rank-2 group spans two).
let private grpAxes (g: Grp) : int64 list = List.replicate (grpRank g) (grpExtent g)

let private axisExtents (gs: Grp list) : int64 list = gs |> List.collect grpAxes

/// Stored cells in one group -- the cardinality the pool is allocated at.
let private grpCells (g: Grp) : int64 =
    match g with
    | GDense n -> n
    | GSym (r, n) -> Blade.SimplexBlocksCore.binom (n + int64 r - 1L) r
    | GAnti (r, n) -> Blade.SimplexBlocksCore.binom n r
    // offsets[rows] IS the total cell count -- the static lane's one number.
    | GRagged (rows, RtStatic (offsets, _)) -> offsets.[int rows]
    | GRagged (_, RtDynamic _) -> refuse "a dynamic ragged shape where a static cell count is required"
    | GDynDense _ -> refuse "a runtime-extent axis where a static cell count is required"

let private shapeCells (gs: Grp list) : int64 = gs |> List.fold (fun acc g -> acc * grpCells g) 1L

let private isDenseShape (gs: Grp list) : bool = gs |> List.forall (grpCompact >> not)

let private denseGroups (extents: int64 list) : Grp list = extents |> List.map GDense

/// The shape that is EXACTLY one compact simplex, at any rank: `(r, n,
/// strict)`. Whole-array schedules (the flat pool walk, the blocked
/// decomposition) are defined on this shape and no other, so the predicate
/// lives in one place.
let private soleSimplexR (gs: Grp list) : (int * int64 * bool) option =
    match gs with
    | [ GSym (r, n) ] -> Some (r, n, false)
    | [ GAnti (r, n) ] -> Some (r, n, true)
    | _ -> None

// ---------------------------------------------------------------------------
// Emitter state
// ---------------------------------------------------------------------------

/// A STATIC-EXTENT array value: a single flat pool (no Iliffe skeleton),
/// with every extent a compile-time constant baked into the GEPs and bounds.
/// The pool is row-major over the shape's groups, and a compact group
/// contributes its canonical simplex in ascending-lex order -- byte-for-byte
/// the order `linearized_storage`'s `linearize` and the C++ lane's pointer
/// skeleton put cells in.
///
/// `Src` is the one interesting field. A materialized array is a `ptr`; a
/// DEFERRED one (`method_for(...) <@> f`, `range<I>`, `A + B` before
/// `|> compute`) is a PRODUCER -- a function from index registers to the
/// element's value -- and is never given storage at all. That is the same
/// laziness the C++ lane spells with `DeferredComputations`, but expressed
/// as an F# closure instead of an IR map, so `|> compute` is literally
/// "run the producer into a pool" and `reduce` over an unforced computation
/// is "run the producer inside the fold" with no array in between.
type private ArrVal =
    { Elem: Sc
      /// Outermost group first.
      Groups: Grp list
      Src: ArrSrc
      /// The REUSE HINT (plan-simplex-blocked-compute.md section 0, third
      /// block): bytes of the largest operand a kernel parameter binds as a
      /// ROW VIEW, 0 when every bind is a scalar. Computed once at
      /// construction by the producer that knows (applyToArr); everything
      /// else says 0. Consulted only by brickTileEdge's BrickAuto branch --
      /// a plain map's cells read scalars, so maps carry 0 and never brick
      /// by default, which is the second-block verdict.
      RowOpBytes: int64 }
    /// Per-AXIS extents, outermost first: a rank-2 compact group contributes
    /// its extent twice. Length = the array's rank, which is what every
    /// consumer that counts index slots means by rank.
    member this.Extents = axisExtents this.Groups
    /// The number of index SLOTS, computed without touching extents -- safe
    /// on ragged shapes, whose extent list has no static answer.
    member this.Rank = this.Groups |> List.sumBy grpRank

and private ArrSrc =
    | APool of string
    /// STORAGE coordinates (one i64 Val per axis) -> element value.
    | AVirt of (Ctx -> Val list -> Val)
    /// FLAT CELL ORDINAL -> element value: the cell-congruent producer.
    ///
    /// This is the flat-elementwise path the blocked-simplex plan tells us not
    /// to touch (section 6: "elementwise maps over sym arrays -- flat-pool
    /// traversal, already optimal, ZERO coordinate math; do not touch this
    /// path"). An elementwise map whose operands all carry the SAME shape as
    /// its output needs no coordinates at all: cell k of the result is the
    /// kernel of cell k of each operand, whatever the shape means. Keeping it
    /// a separate producer flavour is what makes `S * 3.0` over a packed
    /// triangle ONE counted loop over the pool rather than a triangular nest
    /// with row-base arithmetic per cell.
    | AVirtFlat of (Ctx -> Val -> Val)

/// What a Blade value expression produced. Not the same axis as `IRType`: a
/// fused reduction join is one loop with k accumulators and answers a FLAT
/// PACK of registers, and an unforced computation answers a producer -- both
/// decided by the expression, not by the type the checker stamped on it.
and private ValKind =
    | VScalar of Val
    | VArray of ArrVal
    /// A flat PACK: `(L <@> f) <&> (L <@> g)` is two arrays, a reduction join
    /// is k accumulators, a destructured tuple is its components. Flat because
    /// `IRTupleProj` indexes flat (docs: Tuples Design C).
    | VTuple of ValKind list
    /// A binding that names something with no runtime representation at all:
    /// a loop object (`method_for(...)`, `object_for(...)`) or an alias for a
    /// lifted callable (`let f = lambda(x) -> ...`). No storage, no code, no
    /// printed line -- and the C++ lane emits a comment for exactly these.
    | VOpaque

/// One argument handed to an inlined kernel. A kernel parameter is a scalar
/// element at a fully-peeled level and a ROW VIEW where the nest stopped
/// short of the array's rank (`reduce(row, ...)` kernels), so the two shapes
/// travel together.
and private KArg =
    | KScalar of Val
    | KArray of ArrVal

/// One open pool-tracking scope. `At` is the `c.Body` index where the scope
/// opened: the scope-entry null resets are INSERTED there at pop time, so
/// every EXECUTION of the scope starts its tracking slots at null -- an
/// alloca does not zero, and a pool allocated on only one branch of the body
/// must not leave last execution's (already freed) pointer for the exit
/// frees. `Pools` pairs each pool register with its tracking slot; the frees
/// load the slot rather than naming the register, so a conditionally
/// allocated pool needs no dominance argument. `KnownIds` snapshots which
/// array bindings existed when the scope opened: a pool that later UPGRADES
/// such a binding (`materializeExpr`, whole-array rebinding) outlives the
/// scope and is kept by nulling its slot at the escape site.
and private PoolScope =
    { At: int
      Pools: ResizeArray<string * string>
      KnownIds: HashSet<IRId> }

and private Ctx =
    { /// Module-scope constants (string literals for print labels and panics).
      Globals: ResizeArray<string>
      /// Open pool-tracking scopes, innermost last (a function body, or one
      /// IRForRange trip). While any scope is open, `allocPool` records each
      /// pool into the innermost one, and the scope's exit frees what was
      /// recorded -- the arena model's repair for allocations in REPEATED
      /// contexts (section 0.7/0.8 of the plan). Empty at module scope, where
      /// a pool is allocated once per program and the arena is already sound.
      PoolScopes: ResizeArray<PoolScope>
      /// True once any `blade_free` was emitted: the module-tail attribute
      /// groups drop their `nofree` claim, which stops being true the moment
      /// one scope frees.
      mutable AnyFrees: bool
      StringPool: Dictionary<string, string>
      /// `declare` lines, deduplicated and sorted so emission is deterministic.
      Externs: SortedSet<string>
      /// Finished function definitions, in emission order.
      Funcs: ResizeArray<string list>
      /// Every callable the module offers, including let-alias entries.
      Callables: Map<IRId, IRCallable>
      /// Callable id -> LLVM symbol, populated at FIRST REFERENCE so a
      /// self-recursive body can call the symbol it is still inside of.
      Emitted: Dictionary<IRId, string>
      Pending: Queue<IRCallable>
      /// Registers and labels are numbered from one process-wide counter, so
      /// no two labels can collide even across function boundaries.
      mutable Counter: int
      /// Per-function: IR variable id -> (alloca register, type).
      mutable Slots: Dictionary<IRId, string * Sc>
      /// Per-function: IR variable id -> array value. Separate from `Slots`
      /// because an array binding has no single scalar register: it is a
      /// pointer plus a shape, or a closure.
      mutable ArrSlots: Dictionary<IRId, ArrVal>
      /// Per-function: parameter NAME -> slot, for the `IRParam` spelling of a
      /// parameter reference (function bodies use `IRVar` of the param's
      /// VarId, kernel-shaped bodies use `IRParam`; both reach the same slot).
      mutable NameSlots: Dictionary<string, string * Sc>
      mutable NameArrSlots: Dictionary<string, ArrVal>
      /// Loop-object bindings (`let L = method_for(A, B)`). They hold no
      /// storage and print nothing; the table exists so a reference to one
      /// can be diagnosed as a loop object instead of an unbound variable.
      LoopObjs: HashSet<IRId>
      /// Bindings whose value is a flat tuple of scalars (fusion-join folds,
      /// tuple-returning kernels). `IRTupleProj` reads these.
      mutable TupleSlots: Dictionary<IRId, ValKind list>
      /// Deferred bindings a consumer MATERIALIZED under their own name.
      /// Mirrors `CodeGen.forcedDeferredIdsCell`: such a binding is a real
      /// array by program end and therefore prints, while one that stayed
      /// deferred through the whole program prints nothing.
      Forced: HashSet<IRId>
      /// Top-level binding id -> its value expression. Read only to propagate
      /// forcing: materializing a deferred binding materializes the deferred
      /// bindings ITS value names too (the C++ lane's `forceDeferredArrayInput`
      /// recursion), and each of those then prints.
      mutable BindingValues: Map<IRId, IRExpr>
      /// Callables currently being inlined (capturing callees), so a
      /// recursive one refuses instead of expanding forever.
      Inlining: HashSet<IRId>
      /// ENTRY-BLOCK allocas, spliced ahead of the body at assembly time. An
      /// `alloca` inside a loop body would allocate once PER ITERATION (stack
      /// is reclaimed at function return, not at block exit), so a nest over
      /// a real extent would run the stack out; hoisting also keeps every
      /// slot in the entry block, which is where mem2reg promotes them.
      mutable Allocas: ResizeArray<string>
      mutable Body: ResizeArray<string>
      /// What the front end proved about this module, computed before the
      /// first instruction (see the fact-layer note above).
      Facts: ModuleFacts
      /// Open ONLY while emitting a licensed fold's combine. Float arithmetic
      /// born inside this scope carries `reassoc nsz`; everything else stays
      /// order-preserving. Scoped rather than global because the same kernel
      /// text appears in map position too, where no license exists.
      mutable FoldFmf: bool
      /// Attribute groups actually referenced, so the module's tail lists the
      /// ones it uses and no others.
      UsedAttrGroups: SortedSet<int> }

let private newCtx (callables: Map<IRId, IRCallable>) (facts: ModuleFacts) =
    { Globals = ResizeArray()
      PoolScopes = ResizeArray()
      AnyFrees = false
      StringPool = Dictionary()
      Externs = SortedSet(StringComparer.Ordinal)
      Funcs = ResizeArray()
      Callables = callables
      Emitted = Dictionary()
      Pending = Queue()
      Counter = 0
      Facts = facts
      FoldFmf = false
      UsedAttrGroups = SortedSet()
      Slots = Dictionary()
      ArrSlots = Dictionary()
      NameSlots = Dictionary()
      NameArrSlots = Dictionary()
      LoopObjs = HashSet()
      TupleSlots = Dictionary()
      Forced = HashSet()
      BindingValues = Map.empty
      Inlining = HashSet()
      Allocas = ResizeArray()
      Body = ResizeArray() }

let private nextN (c: Ctx) = c.Counter <- c.Counter + 1; c.Counter
let private freshReg (c: Ctx) = sprintf "%%r%d" (nextN c)
let private freshLbl (c: Ctx) (tag: string) = $"{tag}{nextN c}"
let private ln (c: Ctx) (s: string) = c.Body.Add("  " + s)
let private lbl (c: Ctx) (l: string) = c.Body.Add(l + ":")
let private need (c: Ctx) (decl: string) = c.Externs.Add decl |> ignore

/// Claim an attribute group and render its `#N` reference (empty when function
/// attributes are switched off).
let private attrRef (c: Ctx) (g: int) : string =
    if not (factEnabled factFnAttrs) then ""
    else
        c.UsedAttrGroups.Add g |> ignore
        $" #{g}"

/// The uniform parameter decoration (` noundef`), or nothing when parameter
/// attributes are off.
let private paramAttr () : string =
    if factEnabled factParamAttrs then " noundef" else ""

// ---------------------------------------------------------------------------
// Fast-math flags
// ---------------------------------------------------------------------------

/// The flag text one arithmetic instruction is born with, in LLVM's canonical
/// order, with the leading space `BinInstr.Flags` expects.
///
/// TWO INDEPENDENT LICENSES, and neither is a blanket:
///   * `reassoc nsz` -- only inside a fold whose kernel passed
///     `foldReorderLicensed` while `BLADE_FP_REASSOC` is on. `nsz` rides with
///     `reassoc` because a reassociated sum's intermediate zeros are exactly
///     what the reordering may resign, and LLVM's reduction vectorizer wants
///     both before it will split the chain into lanes.
///   * `contract` -- `BLADE_FP_CONTRACT` (opt-in here; see `contractLicensed`).
/// Integer instructions get nothing: `nsw`/`nuw` would be a WRAPAROUND claim
/// this lane has not proved (Blade's Int64 arithmetic wraps like C++'s
/// `int64_t`, and no overflow analysis exists to say otherwise).
let private fmfFor (c: Ctx) (ty: Sc) : string =
    if ty <> ScF64 || not (factEnabled factFmf) then ""
    else
        let parts =
            [ if c.FoldFmf then
                  yield "reassoc"
                  yield "nsz"
              if contractLicensed () then yield "contract" ]
        if List.isEmpty parts then "" else " " + String.concat " " parts

/// May a fold through this kernel be reassociated? The SAME two-part gate the
/// C++ lane spends on its K-lane forms (`CodeGenBinding.fs`): the declared or
/// derived license, and the environment knob. Sharing the predicate is what
/// keeps the two lanes' numeric policy one policy -- an answer that reassociates
/// in one lane and not the other would make the differential gate report
/// drift that is really a policy split.
let private foldFmfLicensed (cl: IRCallable) : bool =
    Blade.CodeGenState.fpReassocEnabled () && Blade.CodeGenExprSupport.foldReorderLicensed cl

/// May FMF DECORATE the kernel application? Only when that application IS the
/// combining instruction -- a recognized builtin op, where `applyKernel` emits
/// exactly one `fadd`/`fmul`. A lambda or named kernel INLINES its body inside
/// the `withFoldFmf` scope, so `reassoc nsz` would land on ALL of the body's
/// arithmetic -- e.g. the `fmul` in `a + b * b` -- and the fold license covers
/// reordering whole kernel APPLICATIONS across iterations, never the interior
/// of one application. Such kernels still fold correctly; they just carry no
/// flags (the C++ lane's K-lane forms reassociate structurally for the same
/// reason).
let private foldFmfDecorable (cl: IRCallable) : bool =
    foldFmfLicensed cl && (Blade.CodeGenExprSupport.foldKernelBuiltinOp cl).IsSome

/// Emit `f` with the licensed-fold flag scope open. Wrapped around the KERNEL
/// APPLICATION only, never around the element read that feeds it: on a
/// deferred operand that read runs the map kernel, whose arithmetic carries no
/// license at all.
let private withFoldFmf (c: Ctx) (licensed: bool) (f: unit -> 'a) : 'a =
    if not licensed then f ()
    else
        let saved = c.FoldFmf
        c.FoldFmf <- true
        try f () finally c.FoldFmf <- saved

// ---------------------------------------------------------------------------
// The shim's C ABI
// ---------------------------------------------------------------------------

/// One shim symbol's declaration shape.
type private ShimFn =
    { Ret: string
      /// Return-value attributes. `noalias align 64` on the allocator is the
      /// ONLY `noalias` this lane emits: that pointer is freshly allocated, so
      /// it provably aliases nothing live, and `blade_alloc_cells` aligns to 64
      /// bytes by construction. A read operand never gets one -- the measured
      /// payoff there was 0-3% (plan section 1) against a miscompile risk the
      /// red team priced at "Rust needed six years".
      RetAttrs: string
      /// Parameter TYPES; `noundef` is added uniformly (every argument this
      /// emitter passes is a real value -- there is no undef/poison channel).
      Args: string list
      Group: int }

/// Every symbol the shim exports, in ONE table.
///
/// Not a literal at each call site, for a reason that already bites: a
/// `declare` line is deduplicated by EXACT TEXT, and two sites needing
/// `blade_out_str` (`outStr` and `outVal`) that spelled its attributes
/// differently would emit the symbol twice and clang would reject the module.
/// One table also means the fact layer decorates each symbol in exactly one
/// place.
let private shimTable : Map<string, ShimFn> =
    let io ret args = { Ret = ret; RetAttrs = ""; Args = args; Group = grpExternReturns }
    Map.ofList
        [ "blade_now", io "double" []
          "blade_panic", { Ret = "void"; RetAttrs = ""; Args = [ "ptr" ]; Group = grpShimPanic }
          "blade_alloc_cells",
          { Ret = "ptr"; RetAttrs = "noalias align 64"; Args = [ "i64"; "i64" ]; Group = grpShimAlloc }
          "blade_free", { Ret = "void"; RetAttrs = ""; Args = [ "ptr" ]; Group = grpShimFree }
          "blade_out_str", io "void" [ "ptr" ]
          "blade_out_i64", io "void" [ "i64" ]
          "blade_out_f64", io "void" [ "double" ]
          "blade_out_bool", io "void" [ "i32" ]
          "blade_print_i64", io "void" [ "ptr"; "i64" ]
          "blade_print_f64", io "void" [ "ptr"; "double" ]
          "blade_print_bool", io "void" [ "ptr"; "i32" ]
          "blade_print_str", io "void" [ "ptr"; "ptr" ]
          "blade_print_completed", io "void" [ "ptr"; "double" ] ]

/// Declare one shim symbol, decorated with whatever fact classes are on.
let private needShim (c: Ctx) (name: string) : unit =
    let f =
        match Map.tryFind name shimTable with
        | Some f -> f
        | None -> refuse ($"the shim symbol '{name}', which has no declaration in shimTable")
    let args = f.Args |> List.map (fun t -> t + paramAttr ()) |> String.concat ", "
    let ret =
        if factEnabled factParamAttrs && f.RetAttrs <> "" then f.RetAttrs + " " + f.Ret else f.Ret
    need c ($"declare {ret} @{name}({args}){(attrRef c f.Group)}")

/// Reserve an entry-block slot. Every `alloca` in this file goes through
/// here; see `Ctx.Allocas` for why none may be emitted in place.
let private allocaOf (c: Ctx) (llType: string) : string =
    let reg = freshReg c
    c.Allocas.Add($"  {reg} = alloca {llType}")
    reg

// ---------------------------------------------------------------------------
// Pool-tracking scopes (the arena model's edge, plan section 0.7/0.8 item 3)
// ---------------------------------------------------------------------------

/// Open a tracking scope at the CURRENT body position. Everything `allocPool`
/// hands out while this scope is innermost gets a tracking slot; the matching
/// `popPoolScope` frees what the slots still hold.
let private pushPoolScope (c: Ctx) : unit =
    c.PoolScopes.Add { At = c.Body.Count
                       Pools = ResizeArray()
                       KnownIds = HashSet(c.ArrSlots.Keys) }

/// A tracked pool ESCAPES: null its tracking slot wherever it is tracked, so
/// no scope's exit frees reclaim it. Emitting a store (rather than removing
/// the record) is what makes a CONDITIONAL escape correct -- the null lands
/// only on executions that actually took the escaping path.
let private keepPool (c: Ctx) (poolReg: string) : unit =
    for scope in c.PoolScopes do
        for (p, slot) in scope.Pools do
            if p = poolReg then ln c ($"store ptr null, ptr {slot}")

/// Close the innermost scope. With `emitFrees`, each tracking slot is loaded
/// and freed -- a null slot (branch never taken, or pool kept) frees
/// nothing. `keepReg` spares its own entry TEXTUALLY: a tracking slot only
/// ever holds its own allocation or null, and two live allocations cannot
/// share an address, so no runtime compare is needed. The scope-entry null
/// resets are inserted where the scope opened. Without `emitFrees` the
/// records are simply dropped -- today's arena.
let private popPoolScope (c: Ctx) (keepReg: string option) (emitFrees: bool) : unit =
    let scope = c.PoolScopes.[c.PoolScopes.Count - 1]
    c.PoolScopes.RemoveAt(c.PoolScopes.Count - 1)
    let freed = scope.Pools |> Seq.filter (fun (p, _) -> Some p <> keepReg) |> Seq.toList
    if emitFrees && not (List.isEmpty freed) then
        c.AnyFrees <- true
        needShim c "blade_free"
        let resets =
            freed |> List.map (fun (_, slot) -> $"  store ptr null, ptr {slot}")
        c.Body.InsertRange(scope.At, resets)
        for (_, slot) in freed do
            let p = freshReg c
            ln c ($"{p} = load ptr, ptr {slot}")
            ln c ($"call void @blade_free(ptr {p})")

/// A double as its EXACT bit pattern. `.ll` accepts `double 0x<16 hex>`, and
/// that spelling is the only one immune to the decimal round-trip question
/// the C++ lane had to solve separately (floatToCppLiteral's "R" format).
let private f64Const (x: float) =
    sprintf "0x%016X" (uint64 (BitConverter.DoubleToInt64Bits x))

/// Intern a string as a private constant and return its symbol. `ptr` is the
/// symbol itself under opaque pointers -- no getelementptr needed.
let private stringGlobal (c: Ctx) (s: string) : string =
    match c.StringPool.TryGetValue s with
    | true, g -> g
    | _ ->
        let bytes = Encoding.UTF8.GetBytes s
        let name = $"@.blade.str.{c.StringPool.Count}"
        let sb = StringBuilder()
        for b in bytes do
            // `\` and `"` would end the token; anything outside printable
            // ASCII goes out as \XX so the file stays byte-exact.
            if b >= 0x20uy && b < 0x7Fuy && b <> 0x22uy && b <> 0x5Cuy then sb.Append(char b) |> ignore
            else sb.AppendFormat("\\{0:X2}", b) |> ignore
        sb.Append("\\00") |> ignore
        c.Globals.Add($"{name} = private unnamed_addr constant [{bytes.Length + 1} x i8] c\"{(sb.ToString())}\"")
        c.StringPool.[s] <- name
        name

/// LLVM identifiers take [A-Za-z0-9_$.-]; Blade names are already tame but a
/// module-qualified or generated name is not guaranteed to be.
let private sanitizeSym (s: string) =
    let sb = StringBuilder()
    for ch in s do
        if Char.IsLetterOrDigit ch || ch = '_' then sb.Append ch |> ignore
        else sb.Append '_' |> ignore
    let out = sb.ToString()
    if out = "" then "fn" else out

// ---------------------------------------------------------------------------
// Conversions and promotion
//
// These mirror C++'s implicit conversions at the SAME seams the C++ emitter
// relies on them (assignment to a declared type, argument passing, return),
// because that is what makes the two lanes agree on `let n: Int64 = 2 ^ 3`
// (pow returns double; the binding truncates).
// ---------------------------------------------------------------------------

let private coerce (c: Ctx) (target: Sc) (v: Val) : Val =
    if v.Ty = target then v
    else
        let dest = freshReg c
        let conv opcode =
            ln c (renderConv { Dest = dest; Opcode = opcode; From = v.Ty; Value = v.Reg; To = target })
            { Reg = dest; Ty = target }
        match v.Ty, target with
        | ScI64, ScF64 -> conv "sitofp"
        | ScBool, ScI64 -> conv "zext"
        | ScBool, ScF64 -> conv "uitofp"
        // C++ narrows double -> integral by truncation toward zero, which is
        // exactly fptosi.
        | ScF64, ScI64 -> conv "fptosi"
        | ScI64, ScBool ->
            ln c (renderCmp { Dest = dest; Kind = "icmp"; Pred = "ne"; Ty = ScI64; Lhs = v.Reg; Rhs = "0" })
            { Reg = dest; Ty = ScBool }
        | ScF64, ScBool ->
            // `(bool)x` in C++ is `x != 0`, and NaN converts to true -- `une`
            // (unordered-or-not-equal) is the predicate that says so.
            ln c (renderCmp { Dest = dest; Kind = "fcmp"; Pred = "une"; Ty = ScF64; Lhs = v.Reg; Rhs = f64Const 0.0 })
            { Reg = dest; Ty = ScBool }
        | _ ->
            refuse ($"no conversion from {(llTy v.Ty)} to {(llTy target)}")

/// The binary-operand common type. Mirrors `IR.promoteElemType` restricted to
/// the widths v1 carries.
let private promote (a: Sc) (b: Sc) : Sc =
    match a, b with
    | ScStr, _ | _, ScStr -> refuse "an operator applied to String operands"
    | ScVoid, _ | _, ScVoid -> refuse "an operator applied to a unit operand"
    | ScF64, _ | _, ScF64 -> ScF64
    | ScI64, _ | _, ScI64 -> ScI64
    | _ -> ScBool

// ---------------------------------------------------------------------------
// Math intrinsics
// ---------------------------------------------------------------------------

/// The libm-backed unary intrinsics. `GradCommon.mathIntrinsics` is the
/// language's list; lgamma/digamma are deliberately ABSENT here because they
/// are Blade's OWN hand-rolled series (blade_runtime.hpp), kept bit-identical
/// with the interpreter -- routing them to libm would silently change results,
/// so the lane refuses the program instead.
let private libmUnary : Map<string, string> =
    [ "exp", "exp"; "log", "log"; "log10", "log10"; "sqrt", "sqrt"
      "sin", "sin"; "cos", "cos"; "tan", "tan"
      "sinh", "sinh"; "cosh", "cosh"; "tanh", "tanh"
      "asin", "asin"; "acos", "acos"; "atan", "atan"
      "floor", "floor"; "ceil", "ceil"
      // `std::abs` on a double IS fabs; typeOf pins the result Float64.
      "abs", "fabs" ]
    |> Map.ofList

// ---------------------------------------------------------------------------
// Static-extent arrays: dense pools and packed simplex pools
//
// STORAGE MODEL (plan section 5, "flat pool + baked extents"): one contiguous
// pool per array, addressed by constant-folded GEPs. No Iliffe skeleton --
// the pointer-of-pointers the C++ lane builds is a way to spell `arr[i][j]`
// in C++, not a layout, and the pool order it indexes is exactly the order
// below.
//
// A COMPACT (symmetric / antisymmetric) group stores only its canonical
// simplex, in the same ascending-lex order: row i of a rank-2 group occupies
// `n - i - strict` contiguous cells starting at `rowBase(i)`. That is why the
// C++ lane's printer walks `arr[i][j]` with the shrinking bound `extents[1] -
// i` and why this lane's printer must too -- the two are pinned against each
// other by every corpus EXPECT over a symmetric array.
//
// Still refused by name: Hermitian (constraint-coupled cells), wreath/OrbIdx
// (a wreath pool has no tile multiset), and compact groups of rank 3+ (the
// prisms of plan section 3.2).
// ---------------------------------------------------------------------------

/// The in-pool spelling of an element. Bools narrow to `i8` because `i1` has
/// no settled memory layout to share with the C shim, and a Blade `Bool`
/// array must be readable by the printer as a C `int`.
let private poolTy = function
    | ScI64 -> "i64"
    | ScF64 -> "double"
    | ScBool -> "i8"
    | ScStr -> "ptr"
    | ScVoid -> refuse "an array of unit elements"

let private poolElemBytes = function
    | ScBool -> 1L
    | _ -> 8L

/// Does a rank-r simplex of extent n keep the emitted offset arithmetic inside
/// i64? `emitBinomConst` lays down the FALLING FACTORIAL and divides at the
/// end, so the intermediate -- at most (n+r)^r -- overflows well before the
/// binomial itself would. Checked in floating point because the whole point is
/// to answer without overflowing. Rank 2 admits n up to ~3e9, rank 3 ~2e6,
/// rank 5 ~6000: comfortably past any extent whose pool would fit in memory,
/// so this refuses only genuinely absurd shapes -- and it refuses them by
/// declining the GROUP, which makes it a clean whole-program fallback to the
/// C++ lane rather than a miscompile.
let private simplexFitsI64 (r: int) (n: int64) : bool =
    let mutable acc = 1.0
    for _ in 1 .. r do acc <- acc * float (n + int64 r)
    acc < 9.0e18

/// Project ONE index record onto a shape group, or None when this lane has no
/// storage for it.
let private groupOfIndexType (ix: IRIndexType) : Grp option =
    if ix.IxKind <> IxKPlain || not (List.isEmpty ix.Dependencies) then None
    else
        match Blade.IRPrint.tryEvalIntIR ix.Extent with
        | None -> None
        | Some n ->
            match ix.Symmetry, ix.Rank with
            | SymNone, 1 -> Some (GDense n)
            // ANY rank: the offset closed form, the serial nest and the
            // canonicalizing read are all rank-parametric, so rank 3+ needs no
            // arm of its own -- only the arithmetic-range check above.
            | SymSymmetric, r when r >= 2 && simplexFitsI64 r n -> Some (GSym (r, n))
            | SymAntisymmetric, r when r >= 2 && simplexFitsI64 r n -> Some (GAnti (r, n))
            // Hermitian, wreath, and out-of-range simplices land here.
            | _ -> None

/// Project an IRType onto the static-extent array universe. Everything this
/// returns None for is a refusal at the use site, WITH ITS OWN WORDING:
/// "not an array shape" is never a useful diagnostic on its own.
let private arrayShapeOf (t: IRType) : (Sc * Grp list) option =
    match Blade.IR.stripUnits t with
    | ArrayElem arr when not (List.isEmpty arr.IndexTypes) ->
        let elemOk = scalarTyOf arr.ElemType
        let groups = arr.IndexTypes |> List.map groupOfIndexType
        match elemOk with
        | Some e when e <> ScVoid && groups |> List.forall Option.isSome ->
            Some (e, groups |> List.map Option.get)
        | _ -> None
    | _ -> None

/// The refusal every array-shaped site shares, so one unsupported index type
/// reads the same wherever it is met.
let private requireArray (what: string) (t: IRType) : Sc * Grp list =
    match arrayShapeOf t with
    | Some s -> s
    | None ->
        refuse ($"{what} has type {(Blade.IRPrint.ppIRType t)} -- the llvm lane handles arrays over static Idx<n> axes and rank-2 Sym/Antisym groups only")

/// The RAGGED-PAIR array type: a static plain outer axis over a ragged-family
/// (or DepIdx) inner. Deliberately NOT folded into arrayShapeOf: every
/// existing dense/compact site keeps refusing ragged by name, and only the
/// sites that grew a ragged story consult this. Answers (element, rows).
let private raggedArrShape (t: IRType) : (Sc * int64) option =
    match Blade.IR.stripUnits t with
    | ArrayElem arr ->
        (match arr.IndexTypes with
         | [ outer; inner ]
             when (outer.IxKind = IxKPlain || outer.IxKind = IxKDepOuter)
                  && outer.Symmetry = SymNone && outer.Rank = 1
                  && (isRaggedFamilyKind inner.IxKind || inner.IxKind = IxKDepInner) ->
             (match scalarTyOf arr.ElemType, Blade.IRPrint.tryEvalIntIR outer.Extent with
              | Some e, Some n when e <> ScVoid -> Some (e, n)
              | _ -> None)
         | _ -> None)
    | _ -> None

/// `for (i = 0; i < n; i++)`, with the counter in an entry-block slot so
/// mem2reg turns it back into the induction variable it is. `body` may emit
/// its own control flow: the increment lands in whatever block is current
/// when it returns, and terminates it.
let private emitCountedLoopTo (c: Ctx) (bound: string) (body: string -> unit) : unit =
    let iv = allocaOf c "i64"
    ln c ($"store i64 0, ptr {iv}")
    let lCond = freshLbl c "loop.cond"
    let lBody = freshLbl c "loop.body"
    let lEnd = freshLbl c "loop.end"
    ln c (sprintf "br label %%%s" lCond)
    lbl c lCond
    let i = freshReg c
    ln c ($"{i} = load i64, ptr {iv}")
    let t = freshReg c
    ln c (renderCmp { Dest = t; Kind = "icmp"; Pred = "slt"; Ty = ScI64; Lhs = i; Rhs = bound })
    ln c (renderBr { Cond = t; IfTrue = lBody; IfFalse = lEnd })
    lbl c lBody
    body i
    let i2 = freshReg c
    ln c ($"{i2} = load i64, ptr {iv}")
    let i3 = freshReg c
    ln c (renderBin { Dest = i3; Opcode = "add"; Flags = ""; Ty = ScI64; Lhs = i2; Rhs = "1" })
    ln c ($"store i64 {i3}, ptr {iv}")
    ln c (sprintf "br label %%%s" lCond)
    lbl c lEnd

/// The literal-trip-count form. Every rectangular loop in this file is one of
/// these, which is what "static extents only" buys: the trip count is a
/// constant the vectorizer reads straight off the compare.
let private emitCountedLoop (c: Ctx) (n: int64) (body: string -> unit) : unit =
    emitCountedLoopTo c (string n) body

/// A row-major nest over `extents`, innermost last, handing the body one
/// index register per level.
let private emitNest (c: Ctx) (extents: int64 list) (body: Val list -> unit) : unit =
    let rec go acc rest =
        match rest with
        | [] -> body (List.rev acc)
        | n :: tl -> emitCountedLoop c n (fun i -> go ({ Reg = i; Ty = ScI64 } :: acc) tl)
    go [] extents

let private i64Bin (c: Ctx) (opcode: string) (lhs: string) (rhs: string) : string =
    let d = freshReg c
    ln c (renderBin { Dest = d; Opcode = opcode; Flags = ""; Ty = ScI64; Lhs = lhs; Rhs = rhs })
    d

/// `a + b` with the literal-zero identity folded at EMISSION time. The tile
/// arithmetic below is full of `+ 0` (the serial triangle's row origin, a
/// symmetric group's strict offset), and emitting them would bury the shape of
/// the loop under instructions clang deletes anyway -- a .ll nobody can read
/// is a .ll nobody checks.
let private i64Add (c: Ctx) (a: string) (b: string) : string =
    if a = "0" then b elif b = "0" then a else i64Bin c "add" a b

/// The first pool cell of storage row `i` in a rank-2 compact group of extent
/// `n`: `i * (2n + 1 - 2*strict - i) / 2`, which is
/// `SimplexBlocksCore.rowBase2` emitted. Closed form rather than a running
/// counter on purpose -- it makes every row's addressing independent of every
/// other's, which is what lets a brick's rows vectorize and (later)
/// parallelize. One of the two factors is always even, so the halving is
/// exact.
let private emitRowBase2 (c: Ctx) (n: int64) (strict: bool) (i: string) : string =
    let k = 2L * n + 1L - (if strict then 2L else 0L)
    let t = i64Bin c "sub" (string k) i
    let m = i64Bin c "mul" i t
    i64Bin c "sdiv" m "2"

/// An operand that is a decimal literal, or None. Constant folding at
/// emission is not cosmetic here: the rank-r offset arithmetic below is full
/// of binomials of a CONSTANT (`n` minus a literal lower bound), and folding
/// them turns a chain of six instructions into one immediate.
let private tryLitI64 (s: string) : int64 option =
    match System.Int64.TryParse(s, System.Globalization.NumberStyles.AllowLeadingSign,
                                System.Globalization.CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

/// Emit a STATIC ragged group's offsets table as a module constant and answer
/// its symbol. `constant`, never `global`: LLVM then knows no store anywhere
/// can touch it, so consecutive rows share one load (the previous row's hi IS
/// this row's lo) and a small outer loop can fold the loads away entirely.
let private raggedTableGlobal (c: Ctx) (offsets: int64[]) : string =
    let name = $"@.blade.roff.{nextN c}"
    let body = offsets |> Array.map (sprintf "i64 %d") |> String.concat ", "
    c.Globals.Add($"{name} = private unnamed_addr constant [{offsets.Length} x i64] [{body}], align 64")
    name

/// offsets[i] as an operand: folded to a literal when the table and the index
/// are both compile-time; one gep + load otherwise -- the SAME two lines for
/// the static global and the dynamic pointer.
let private raggedOffAt (c: Ctx) (table: RaggedTable) (i: string) : string =
    match table, tryLitI64 i with
    | RtStatic (offsets, _), Some k when k >= 0L && k < int64 offsets.Length ->
        string offsets.[int k]
    | _ ->
        let sym = match table with RtStatic (_, s) -> s | RtDynamic p -> p
        let g = freshReg c
        ln c ($"{g} = getelementptr inbounds i64, ptr {sym}, i64 {i}")
        let v = freshReg c
        ln c ($"{v} = load i64, ptr {g}")
        v

/// The length of row i: offsets[i+1] - offsets[i], folded when it can be.
let private raggedLenAt (c: Ctx) (table: RaggedTable) (i: string) : string =
    match table, tryLitI64 i with
    | RtStatic (offsets, _), Some k when k >= 0L && k + 1L < int64 offsets.Length ->
        string (offsets.[int k + 1] - offsets.[int k])
    | _ ->
        let ip1 = i64Bin c "add" i "1"
        let hi = raggedOffAt c table ip1
        let lo = raggedOffAt c table i
        i64Bin c "sub" hi lo

/// `C(x, m)` for a RUNTIME x and a compile-time m: the falling factorial
/// x(x-1)...(x-m+1) over the constant m!. Exact in integers -- a product of m
/// consecutive integers is always divisible by m! -- so this needs no rounding
/// care. Folds outright when x is a literal.
///
/// The callers only ever evaluate this where `x >= m` holds by construction
/// (see `emitPrefixTerm`), which is what lets the plain falling factorial
/// stand in for `SimplexBlocksCore.binom`'s clamped-to-zero branch.
let private emitBinomConst (c: Ctx) (x: string) (m: int) : string =
    if m <= 0 then "1"
    elif m = 1 then x
    else
        match tryLitI64 x with
        | Some v -> string (Blade.SimplexBlocksCore.binom v m)
        | None ->
            let mutable acc = x
            for j in 1 .. m - 1 do
                acc <- i64Bin c "mul" acc (i64Bin c "sub" x (string j))
            let mutable f = 1L
            for j in 2 .. m do f <- f * int64 j
            i64Bin c "sdiv" acc (string f)

/// `SimplexBlocksCore.prefixTerm` emitted: the rank contributed by advancing
/// coordinate k of a rank-r group from its canonical lower bound `lo` by the
/// STORAGE offset `s` (so the absolute coordinate is `lo + s`).
///
///     strict:     C(n-lo,   m+1) - C(n-lo-s,   m+1)
///     symmetric:  C(n-lo+m, m+1) - C(n-lo-s+m, m+1)     (m = r-k-1)
///
/// TWO PROPERTIES CARRY THE WHOLE DESIGN. At the last level (k = r-1, m = 0)
/// the degree is 1 and the term is exactly `s` -- no arithmetic at all, so the
/// innermost run is affine in its loop counter and therefore contiguous in the
/// pool at EVERY rank. And every term is invariant under the levels inside it,
/// so a nest hoists each one to its own level and pays O(1) per cell.
///
/// `x >= m+1` holds at both evaluation points for any canonical tuple (the
/// canonical bound leaves at least `m` coordinates' worth of room above
/// `i_k`), which is why `emitBinomConst`'s unclamped falling factorial is
/// sound here.
let private emitPrefixTerm (c: Ctx) (strict: bool) (n: int64) (r: int) (k: int)
                           (lo: string) (s: string) : string =
    let m = r - k - 1
    if m = 0 then s
    else
        let d = if strict then 0L else int64 m
        let hi =
            match tryLitI64 lo with
            | Some l -> string (n + d - l)
            | None -> i64Bin c "sub" (string (n + d)) lo
        let lowEnd = i64Bin c "sub" hi s
        i64Bin c "sub" (emitBinomConst c hi (m + 1)) (emitBinomConst c lowEnd (m + 1))

/// The offset of a cell inside ONE group, from that group's STORAGE
/// coordinates. A dense axis is its own coordinate (no instruction at all); a
/// compact group of rank r sums `emitPrefixTerm` over its levels, threading
/// the canonical lower bound (`lo_0 = 0`, `lo_k = i_{k-1} + strict`).
///
/// At rank 2 this is `rowBase(i) + p` with the row base spelled as
/// `C(n,2) - C(n-i,2)` instead of `i*(2n-i-1)/2` -- the same value (the
/// property pins assert it) by the one formula that also serves ranks 3+.
let private grpOffset (c: Ctx) (g: Grp) (idxs: Val list) : string =
    match g, idxs with
    | GDense _, [ i ] -> i.Reg
    | GDense _, _ -> refuse "a dense axis indexed by more than one coordinate"
    | GDynDense _, [ i ] -> i.Reg
    | GDynDense _, _ -> refuse "a runtime-extent axis indexed by more than one coordinate"
    // A ragged cell is offsets[row] + element: one table operand and one add,
    // folded outright when the row is a literal against a static table.
    | GRagged (_, table), [ i; j ] -> i64Add c (raggedOffAt c table i.Reg) j.Reg
    | GRagged _, _ -> refuse "a ragged group addressed by other than (row, element) coordinates"
    | _ ->
        let r = grpRank g
        let n = grpExtent g
        let strict = grpStrict g
        if List.length idxs <> r then
            refuse ($"a rank-{r} compact index group addressed by {List.length idxs} coordinates")
        let sInc = if strict then 1L else 0L
        let mutable lo = "0"
        let mutable acc = "0"
        idxs |> List.iteri (fun k (sk: Val) ->
            acc <- i64Add c acc (emitPrefixTerm c strict n r k lo sk.Reg)
            // The next level's canonical floor: i_k + strict = lo + s_k + strict.
            lo <- i64Add c (i64Add c lo sk.Reg) (string sInc))
        acc

/// Pool offset from STORAGE coordinates, one per axis. The Horner chain over
/// groups: `((off0 * cells1 + off1) * cells2 + off2) ...`. For an all-dense
/// shape every group offset is the bare index and the chain is exactly the
/// row-major `((i0*E1 + i1)*E2 + i2)` it always was -- byte-identical
/// emission, which is what keeps the dense goldens pinned across this change.
let private storageOffset (c: Ctx) (groups: Grp list) (idxs: Val list) : string =
    let rec split (gs: Grp list) (xs: Val list) =
        match gs with
        | [] -> []
        | g :: tl ->
            let r = grpRank g
            (g, xs |> List.truncate r) :: split tl (xs |> List.skip (min r xs.Length))
    match idxs with
    | [] -> "0"
    | _ ->
        let parts = split groups idxs
        match parts with
        | [] -> "0"
        | (g0, x0) :: rest ->
            let mutable acc = grpOffset c g0 x0
            for (g, xs) in rest do
                let off = grpOffset c g xs
                let m = i64Bin c "mul" acc (string (grpCells g))
                acc <- i64Bin c "add" m off
            acc

let private gepCell (c: Ctx) (elem: Sc) (basePtr: string) (off: string) : string =
    let p = freshReg c
    ln c ($"{p} = getelementptr inbounds {(poolTy elem)}, ptr {basePtr}, i64 {off}")
    p

let private loadCell (c: Ctx) (elem: Sc) (ptr: string) : Val =
    let raw = freshReg c
    ln c ($"{raw} = load {(poolTy elem)}, ptr {ptr}")
    match elem with
    | ScBool ->
        let b = freshReg c
        ln c ($"{b} = icmp ne i8 {raw}, 0")
        { Reg = b; Ty = ScBool }
    | _ -> { Reg = raw; Ty = elem }

let private storeCell (c: Ctx) (elem: Sc) (ptr: string) (v: Val) : unit =
    match elem with
    | ScBool ->
        let w = freshReg c
        ln c ($"{w} = zext i1 {v.Reg} to i8")
        ln c ($"store i8 {w}, ptr {ptr}")
    | _ -> ln c ($"store {(poolTy elem)} {v.Reg}, ptr {ptr}")

/// An UNINITIALIZED pool of `n` cells -- `n` an OPERAND, so a runtime-sized
/// ragged pool and a literal-sized dense one are the same call.
///
/// THE INVARIANT EVERY CALLER DISCHARGES: the pool's every cell is written
/// before any cell is read. Each call site is a complete fill by
/// construction -- `materialize`'s four nests each cover their whole shape,
/// `copyArr` copies cell-for-cell, and the two literal fills store exactly
/// `total` cells behind a count guard. Zeros the LANGUAGE promises (a
/// recursive array's unbuilt prefix, `guard`'s else arm) are explicit stores
/// in the IR both lanes share, so they hold here without allocator help --
/// the C++ lane's pools are uninitialized `new T[]` for the same reason. The
/// allocator used to memset anyway, which cost one full write pass per pool
/// inside the timed region (the llvm lane's "13-20% IR-shape headroom" of
/// plan-simplex-blocked-compute.md section 0b was almost entirely this).
let private allocPool (c: Ctx) (elem: Sc) (n: string) : string =
    needShim c "blade_alloc_cells"
    let p = freshReg c
    ln c (renderCall { Dest = Some p; RetTy = ScStr; Callee = "@blade_alloc_cells"
                       Args = [ ScI64, n; ScI64, string (poolElemBytes elem) ] })
    // Inside a tracking scope (a function body, an IRForRange trip), record
    // the pool so the scope's exit can free it -- through a slot, so a pool
    // allocated on one branch frees without a dominance argument.
    if c.PoolScopes.Count > 0 then
        let slot = allocaOf c "ptr"
        ln c ($"store ptr {p}, ptr {slot}")
        c.PoolScopes.[c.PoolScopes.Count - 1].Pools.Add(p, slot)
    p

/// Read one cell, at STORAGE coordinates. A pool read is a GEP + load; a
/// producer read RUNS the deferred kernel at this index, which is what makes
/// an unforced computation cost nothing until somebody asks for a value.
let private readCell (c: Ctx) (a: ArrVal) (idxs: Val list) : Val =
    match a.Src with
    | APool p -> loadCell c a.Elem (gepCell c a.Elem p (storageOffset c a.Groups idxs))
    | AVirt f -> f c idxs
    | AVirtFlat f -> f c { Reg = storageOffset c a.Groups idxs; Ty = ScI64 }

/// Read cell number `k` of the pool, ignoring shape entirely. The
/// flat-elementwise path: legal exactly when the caller knows the ordinal
/// means the same cell in this array as in the one it is co-iterating.
let private readFlat (c: Ctx) (a: ArrVal) (k: Val) : Val =
    match a.Src with
    | APool p -> loadCell c a.Elem (gepCell c a.Elem p k.Reg)
    | AVirtFlat f -> f c k
    | AVirt _ -> refuse "a flat read of a coordinate-bearing producer"

/// Can this value answer a flat cell-ordinal read without materializing?
let private hasFlatRead (a: ArrVal) : bool =
    match a.Src with
    | APool _ | AVirtFlat _ -> true
    | AVirt _ -> false

/// Total stored cells of a shape, as an OPERAND: a literal for every static
/// shape (including a static-table ragged pair, whose offsets[rows] folds),
/// one load for a dynamic-table ragged pair.
let private shapeCellsOp (c: Ctx) (gs: Grp list) : string =
    match gs with
    | [ GRagged (rows, table) ] -> raggedOffAt c table (string rows)
    | _ -> string (shapeCells gs)

/// The single extent of a rank-1 shape, as an OPERAND: a literal for a dense
/// axis, the carried length operand for a peeled ragged row. Loops take it
/// through `emitCountedLoopTo`, which is what makes the two the same shape.
let private soleExtentOp (a: ArrVal) : string =
    match a.Groups with
    | [ GDense n ] -> string n
    | [ GDynDense len ] -> len
    | _ -> refuse "a rank-1 operand with no single extent"

/// Read at ABSOLUTE coordinates -- the ones a user writes in `S(2)(1)`. For a
/// dense shape this IS `readCell` (identical emission, so the dense goldens do
/// not move). For a compact group it is the canonicalizing read the C++ lane
/// spells `canon_fold` + `ReadTransform`: sort the pair, left-justify it into
/// storage coordinates, then apply the class's character --
///   * symmetric: none, the mirror cell holds the same value;
///   * antisymmetric: NEGATE when the pair was swapped, and answer ZERO on
///     the diagonal, which is not stored at all.
/// The diagonal of a strict group is NOT stored, so a diagonal read must
/// never compute the diagonal's own address: at i = j = n-1 the last row of
/// a strict triangle is EMPTY and `rowBase(n-1) + 0` is one cell past the
/// pool (invisible to value checks because the value is discarded; exposed
/// to the allocator whenever the 64-byte rounding leaves no slack --
/// smallest extent n = 16, where C(16,2)*8 = 960 is a multiple of 64). The
/// guard is BRANCHLESS: the storage coordinates are redirected to cell 0 on
/// the diagonal -- always inside the allocation -- and the fetched value is
/// discarded by select, so the read stays a straight-line select chain the
/// loop vectorizer can if-convert.
///
/// With several strict groups the character composes: the value is zero when
/// ANY group sits on its diagonal, and the sign flips once per SWAPPED group
/// (an XOR, not the last group's flag).
let private canonRead (c: Ctx) (a: ArrVal) (idxs: Val list) : Val =
    if not (a.Groups |> List.exists grpCompact) then readCell c a idxs
    else
    let mutable rest = idxs
    let mutable diags : string list = []
    let mutable swaps : string list = []
    let storage =
        [ for g in a.Groups do
            let r = grpRank g
            let mine = rest |> List.truncate r
            rest <- rest |> List.skip r
            match g, mine with
            | GDense _, [ i ] -> yield i
            | _ when grpCompact g && List.length mine = r ->
                // CANONICALIZE BY SORTING NETWORK, at any rank. A bubble pass
                // over r coordinates is r(r-1)/2 compare-exchanges, each a
                // compare and two selects -- straight-line, no branch, and at
                // r = 2 it is exactly the single exchange this used to be.
                //
                // The ANTISYMMETRIC CHARACTER falls out of the same network:
                // the permutation's sign is the PARITY of the exchanges it
                // performed (a transposition is odd), so xor-ing the swap
                // flags gives the sign for any rank -- the rank-2 "negate when
                // swapped" rule generalized, with no per-rank table.
                let arr = Array.ofList (mine |> List.map (fun (v: Val) -> v.Reg))
                let mutable swapFlags = []
                for pass in 0 .. r - 2 do
                    for q in 0 .. r - 2 - pass do
                        let x = arr.[q]
                        let y = arr.[q + 1]
                        let gt = freshReg c
                        ln c (renderCmp { Dest = gt; Kind = "icmp"; Pred = "sgt"; Ty = ScI64; Lhs = x; Rhs = y })
                        let lo = freshReg c
                        ln c ($"{lo} = select i1 {gt}, i64 {y}, i64 {x}")
                        let hi = freshReg c
                        ln c ($"{hi} = select i1 {gt}, i64 {x}, i64 {y}")
                        arr.[q] <- lo
                        arr.[q + 1] <- hi
                        swapFlags <- gt :: swapFlags
                if grpStrict g then
                    // Sorted, so a repeat is ADJACENT: r-1 equality tests
                    // decide "not stored" (the strict diagonal, at any rank --
                    // for r >= 3 that is every tuple with any two equal
                    // coordinates, not merely i = j).
                    for q in 0 .. r - 2 do
                        let eq = freshReg c
                        ln c (renderCmp { Dest = eq; Kind = "icmp"; Pred = "eq"; Ty = ScI64; Lhs = arr.[q]; Rhs = arr.[q + 1] })
                        diags <- eq :: diags
                    swaps <- swapFlags @ swaps
                // Sorted ABSOLUTE coordinates -> STORAGE coordinates:
                // s_0 = i_0, s_k = i_k - i_{k-1} - strict. On a strict repeat
                // some s_k is negative, which is a dead select arm: the whole
                // tuple is REDIRECTED to cell 0 below before any address is
                // computed.
                let sInc = if grpStrict g then 1L else 0L
                for q in 0 .. r - 1 do
                    if q = 0 then yield { Reg = arr.[0]; Ty = ScI64 }
                    else
                        let d = i64Bin c "sub" arr.[q] arr.[q - 1]
                        yield { Reg = (if sInc = 0L then d else i64Bin c "sub" d (string sInc)); Ty = ScI64 }
            | _ -> refuse "an absolute read of an index group this lane cannot canonicalize" ]
    match diags with
    | [] -> readCell c a storage
    | _ ->
        let fold1 (op: string) (regs: string list) =
            regs |> List.reduce (fun x y ->
                let d = freshReg c
                ln c ($"{d} = {op} i1 {x}, {y}")
                d)
        let eqAny = fold1 "or" diags
        let swXor = fold1 "xor" swaps
        let ety = a.Elem
        let zero =
            match ety with
            | ScF64 -> f64Const 0.0
            | ScI64 -> "0"
            | _ -> refuse "an antisymmetric array of non-numeric elements"
        // BRANCHLESS guarded read. On a diagonal the strict groups' rows may
        // be EMPTY (i = n-1), so the unredirected offset lands one past the
        // pool -- but instead of branching around the load, REDIRECT every
        // storage coordinate to cell 0, which is always inside the allocation
        // (the shim's minimum pool is one 64-byte line, so even a zero-cell
        // antisym pool has a readable cell 0), and discard the fetched value
        // by select. A branch here defeated the loop vectorizer on every
        // antisym mirror-read loop (CantVectorizeInstruction, 2026-08-18
        // census); the select chain if-converts, and the sym mirror fold that
        // uses only selects vectorizes under the reassoc license.
        let redirected =
            storage |> List.map (fun v ->
                let r = freshReg c
                ln c ($"{r} = select i1 {eqAny}, i64 0, i64 {v.Reg}")
                { Reg = r; Ty = ScI64 })
        let v = coerce c ety (readCell c a redirected)
        let negated =
            match ety with
            | ScF64 ->
                let d = freshReg c
                ln c ($"{d} = fneg double {v.Reg}")
                d
            | _ -> i64Bin c "sub" "0" v.Reg
        let signed = freshReg c
        ln c ($"{signed} = select i1 {swXor}, {(llTy ety)} {negated}, {(llTy ety)} {v.Reg}")
        let final = freshReg c
        ln c ($"{final} = select i1 {eqAny}, {(llTy ety)} {zero}, {(llTy ety)} {signed}")
        { Reg = final; Ty = ety }

/// Split a shape after `k` AXES, or refuse. A peel may only land on a GROUP
/// boundary: half of a compact group is not an array -- row i of a packed
/// triangle has extent `n - i`, which is not a static shape at all.
let private splitGroupsAt (k: int) (groups: Grp list) : Grp list * Grp list =
    let rec go acc taken rest =
        if taken = k then (List.rev acc, rest)
        elif taken > k then refuse "a peel that lands inside a compact index group"
        else
            match rest with
            | [] -> refuse "a peel of more axes than the array has"
            | g :: tl -> go (g :: acc) (taken + grpRank g) tl
    go [] 0 groups

/// Peel `k` leading indices, yielding the rank-(r-k) view they name. On a
/// pool this is one GEP -- rows of a row-major pool ARE contiguous sub-pools,
/// which is the whole reason the skeleton is not emitted. On a producer it is
/// index-list concatenation, with no materialization anywhere.
let private rowView (c: Ctx) (a: ArrVal) (idxs: Val list) : ArrVal =
    // Peeling ONE index off a ragged pair yields the row: a rank-1 view whose
    // extent is an OPERAND (offsets[i+1] - offsets[i]) rather than a static
    // number. On a pool that is one GEP, exactly like the dense case; on the
    // cell-congruent producer it is an ordinal rebase. The generic path below
    // must never see this shape -- half of a ragged group is not a static
    // shape, and splitGroupsAt says so.
    match a.Groups, idxs with
    | [ GRagged (_, table) ], [ i ] ->
        let off = raggedOffAt c table i.Reg
        let len = raggedLenAt c table i.Reg
        (match a.Src with
         | APool p -> { a with Groups = [ GDynDense len ]; Src = APool (gepCell c a.Elem p off) }
         | AVirtFlat f ->
             { a with Groups = [ GDynDense len ]
                      Src = AVirt (fun c2 tail ->
                                match tail with
                                | [ j ] -> f c2 { Reg = i64Add c2 off j.Reg; Ty = ScI64 }
                                | _ -> refuse "a ragged row read with more than one coordinate") }
         | AVirt _ -> refuse "a row view of a coordinate-bearing ragged producer")
    | _ ->
    let k = List.length idxs
    let (lead, rest) = splitGroupsAt k a.Groups
    match a.Src with
    | APool p ->
        // Offset in CELLS of the sub-pool: the leading indices addressed
        // against the full shape, times the size of what remains.
        let leadOff = storageOffset c lead idxs
        let block = shapeCells rest
        let off =
            if block = 1L then leadOff
            else
                let m = freshReg c
                ln c (renderBin { Dest = m; Opcode = "mul"; Flags = ""; Ty = ScI64; Lhs = leadOff; Rhs = string block })
                m
        { a with Groups = rest; Src = APool (gepCell c a.Elem p off) }
    | AVirt f -> { a with Groups = rest; Src = AVirt (fun c2 tail -> f c2 (idxs @ tail)) }
    | AVirtFlat f ->
        let flat = { a with Src = AVirt (fun c2 all -> f c2 { Reg = storageOffset c2 a.Groups all; Ty = ScI64 }) }
        { flat with Groups = rest; Src = AVirt (fun c2 tail ->
                        match flat.Src with
                        | AVirt g -> g c2 (idxs @ tail)
                        | _ -> refuse "a peeled flat producer") }

// ---------------------------------------------------------------------------
// The blocked-simplex nest (docs/plans/plan-simplex-blocked-compute.md)
//
// The canonical rank-2 domain {i <= j < n} (or {i < j < n}) is enumerated in
// one of two shapes, and both write EXACTLY the same set of cells:
//
//   SERIAL -- the plain triangle, cells in pool order. Row i runs
//   `n - i - strict` iterations: a trip count that changes every row, which
//   is what the vectorizer and any static schedule dislike.
//
//   BRICKED -- the simplex covered by BLOCKS of the B-wide tile grid. A block
//   whose two tiles differ is a DENSE B x B rectangle: i in tile t1 and j in
//   tile t2 > t1 gives i < j for free, so the canonicality constraint is
//   discharged by the block structure and costs no instruction. Only the T
//   on-diagonal blocks stay triangular, and they hold 1/T of the cells. Both
//   inner loops then have LITERAL trip counts, and the packed addressing
//   inside a brick is affine in the column (row base + j - i - strict, then
//   +1 per cell), which is the whole point.
//
// Block order is ascending-lex over the tile multiset -- (0,0), (0,1), ...,
// (0,T-1), (1,1), ... -- which is `SimplexBlocksCore.blockSequence`. Order is
// free for a map (distinct cells, independent writes) and is the DETERMINISM
// GUARANTEE for a fold, whose per-brick partials combine in exactly it.
// ---------------------------------------------------------------------------

/// `BLADE_LLVM_BRICKS`, read per call (never a module-level `let`, per the
/// repo's environment discipline). MEASUREMENT USE ONLY, exactly as plan
/// section 8.3 licenses: `off`/`0` forces the serial triangle, a number pins
/// the tile edge B, anything else defers to the derived policy.
type private BrickKnob =
    | BrickAuto
    | BrickOff
    | BrickFixed of int64

let private brickKnob () : BrickKnob =
    match System.Environment.GetEnvironmentVariable "BLADE_LLVM_BRICKS" with
    | null | "" -> BrickAuto
    | v ->
        match v.Trim().ToLowerInvariant() with
        | "off" | "0" | "none" -> BrickOff
        | "on" | "auto" | "1" -> BrickAuto
        | s ->
            match System.Int64.TryParse s with
            | true, b when b >= 1L -> BrickFixed b
            | _ -> BrickAuto

/// Bricks pay only when the traversal re-streams a row-operand working set
/// that cannot stay cached. Smallest measured winning set: 9.2 MB (1.38x
/// reassoc / 1.05x strict); 18.4 MB won 2.76x / 1.11x
/// (plan-simplex-blocked-compute.md section 0, third block). The threshold
/// sits just under the smallest measured win; below it the serial triangle
/// keeps the proven-fastest default. The principled form is "operand bytes
/// exceed the outermost cache"; 8 MiB is the measured stand-in until a cache
/// probe exists.
let [<Literal>] reuseThresholdBytes = 8388608L

/// The tile edge for a domain of extent `n`, or None for "run the serial
/// triangle". `licensed` is the fold-reordering licence: a MAP passes true
/// unconditionally (its cells are distinct and its writes independent -- plan
/// section 7, "brick order free"), a FOLD passes `foldReorderLicensed`,
/// because grouping the fold by brick reassociates it. `rowOpBytes` is the
/// construction-time reuse hint (ArrVal.RowOpBytes): every plain map carries
/// 0 and so never bricks by default -- the second-block verdict -- while a
/// row-operand producer at or above the threshold takes the divisor-preferred
/// reuse edge.
///
/// PUBLIC because it IS the gate. An unlicensed fold must never brick, and
/// that is a property of this function alone -- so `blade test llvm blocks`
/// asserts it here, by name and in both directions, rather than inferring it
/// from emitted text.
let brickTileEdge (licensed: bool) (rowOpBytes: int64) (n: int64) : int64 option =
    if not licensed then None
    else
        match brickKnob () with
        | BrickOff -> None
        | BrickFixed b -> if Blade.SimplexBlocksCore.tileCount n b >= 2L then Some b else None
        | BrickAuto ->
            if rowOpBytes >= reuseThresholdBytes then Blade.SimplexBlocksCore.reuseTileEdge n
            else Blade.SimplexBlocksCore.autoTileEdge n

/// One block of the decomposition, laid down as loops. `rowLo`/`colLo` are
/// operands (a tile loop's variable, or a literal for the peeled last tile);
/// `rows`/`cols` are LITERAL widths, which is what makes the trip counts
/// constants. `body` receives (i, p, offset) in STORAGE coordinates.
let private emitBrick (c: Ctx) (n: int64) (strict: bool)
                      (rowLo: string) (rows: int64) (colLo: string option) (cols: int64)
                      (body: Val -> Val -> Val -> unit) : unit =
    let s = if strict then 1L else 0L
    emitCountedLoop c rows (fun a ->
        let i = i64Add c rowLo a
        let rb = emitRowBase2 c n strict i
        match colLo with
        | None ->
            // ON-DIAGONAL block: columns start at the row itself, so the row
            // is a shrinking triangle of `cols - a - strict` cells and the
            // storage coordinate starts at 0. Serial by design (plan section
            // 8.2): these blocks are small, and covering them densely would
            // waste half of each and create write hazards on canonical cells.
            let len = i64Bin c "sub" (string (cols - s)) a
            emitCountedLoopTo c len (fun p ->
                body { Reg = i; Ty = ScI64 } { Reg = p; Ty = ScI64 }
                     { Reg = i64Add c rb p; Ty = ScI64 })
        | Some cl ->
            // OFF-DIAGONAL block: a dense rectangle, `cols` literal
            // iterations, every cell canonical by construction. The storage
            // coordinate and the pool offset are both affine in the column
            // counter, so each is one `add` past a per-row base.
            let pBase = i64Bin c "sub" cl (i64Add c i (string s))
            let oBase = i64Add c rb pBase
            emitCountedLoop c cols (fun b ->
                body { Reg = i; Ty = ScI64 }
                     { Reg = i64Add c pBase b; Ty = ScI64 }
                     { Reg = i64Add c oBase b; Ty = ScI64 }))

/// Enumerate a rank-2 compact group's canonical cells. `onBlock` wraps each
/// block's loops (the fold's per-brick partial hangs off it; a map passes
/// `fun emit -> emit ()`); `body` gets (i, p, pool offset).
let private emitSimplex2 (c: Ctx) (n: int64) (strict: bool) (tile: int64 option)
                         (onBlock: (unit -> unit) -> unit)
                         (body: Val -> Val -> Val -> unit) : unit =
    match tile with
    | None ->
        // The serial triangle IS one on-diagonal block covering everything.
        onBlock (fun () -> emitBrick c n strict "0" n None n body)
    | Some b ->
        let T = Blade.SimplexBlocksCore.tileCount n b
        let last = T - 1L
        let wLast = Blade.SimplexBlocksCore.tileWidth n b last
        // Row tiles 0 .. T-2 are FULL (width b); the last is peeled so every
        // emitted trip count stays a literal.
        emitCountedLoop c last (fun t1 ->
            let rowLo = i64Bin c "mul" t1 (string b)
            onBlock (fun () -> emitBrick c n strict rowLo b None b body)
            // Column tiles t1+1 .. T-2, also full.
            let cnt = i64Bin c "sub" (string (last - 1L)) t1
            emitCountedLoopTo c cnt (fun k ->
                let t2 = i64Bin c "add" (i64Bin c "add" t1 "1") k
                let colLo = i64Bin c "mul" t2 (string b)
                onBlock (fun () -> emitBrick c n strict rowLo b (Some colLo) b body))
            // ... then the ragged last column tile.
            onBlock (fun () -> emitBrick c n strict rowLo b (Some (string (last * b))) wLast body))
        // ... and finally the last diagonal block.
        onBlock (fun () -> emitBrick c n strict (string (last * b)) wLast None wLast body)

/// THE ARBITRARY-RANK SERIAL SIMPLEX: every canonical cell of a rank-r compact
/// group, once, in ascending-lex (= pool) order.
///
/// r nested counted loops, level k running its absolute coordinate from its
/// canonical floor (`0`, then `i_{k-1} + strict`) over the room left above it.
/// Nothing here is rank-2 shaped, and nothing is special-cased per rank: the
/// only rank-dependent quantities are the trip-count slack and the degree of
/// `emitPrefixTerm`'s polynomial, both derived from r.
///
/// THE COST ARGUMENT, which is why this is not merely "a triangular nest".
/// The pool offset is threaded down the nest as a running base: level k adds
/// its own term, which is invariant under every level inside it, so it is
/// emitted ONCE per iteration of level k rather than once per cell. The last
/// level's term degenerates to its own loop counter (`emitPrefixTerm`, m = 0),
/// so the innermost loop adds one `add` and walks the pool contiguously. Total
/// addressing cost is O(1) per cell at any rank, with no combinadic
/// arithmetic, no per-cell canonicality test, and no division except by the
/// compile-time factorials in the hoisted terms.
///
/// The trip count carries the strict slack (`n - lo - (r-1-k)` when strict):
/// the tuples it drops are exactly those with no room for the coordinates
/// still to place, so the nest never opens an inner loop it knows is empty.
let private emitSimplexSerialR (c: Ctx) (r: int) (n: int64) (strict: bool)
                               (body: Val list -> Val -> unit) : unit =
    let sInc = if strict then 1L else 0L
    let rec go (k: int) (lo: string) (baseOff: string) (accS: Val list) =
        if k = r then body (List.rev accS) { Reg = baseOff; Ty = ScI64 }
        else
            let room = n - (if strict then int64 (r - 1 - k) else 0L)
            let trips =
                match tryLitI64 lo with
                | Some l -> string (room - l)
                | None -> i64Bin c "sub" (string room) lo
            emitCountedLoopTo c trips (fun t ->
                // The loop counter IS the storage coordinate: it counts from
                // the canonical floor, which is what a packed coordinate means.
                let term = emitPrefixTerm c strict n r k lo t
                go (k + 1)
                   (i64Add c (i64Add c lo t) (string sInc))
                   (i64Add c baseOff term)
                   ({ Reg = t; Ty = ScI64 } :: accS))
    go 0 "0" "0" []

/// Enumerate a compact group's canonical cells at ANY rank, handing `body` the
/// group's STORAGE coordinates (one per level) and the pool offset.
///
/// `tile` selects the schedule: `None` is the serial simplex above (one block
/// covering the domain, so `onBlock` wraps it once); `Some B` is the blocked
/// decomposition, which is currently defined for rank 2 only -- the rank-r
/// block enumeration exists in `SimplexBlocksCore` (`blockSequence`,
/// `isDenseBrick`) but its emitter does not, and a fold that silently ran
/// serial when it asked to be blocked would corrupt the one thing the blocked
/// arm is for (a reassociated-but-deterministic combine order). So it refuses
/// by name instead.
let private emitSimplexR (c: Ctx) (r: int) (n: int64) (strict: bool) (tile: int64 option)
                         (onBlock: (unit -> unit) -> unit)
                         (body: Val list -> Val -> unit) : unit =
    match tile, r with
    | None, _ -> onBlock (fun () -> emitSimplexSerialR c r n strict body)
    | Some b, 2 -> emitSimplex2 c n strict (Some b) onBlock (fun i p off -> body [ i; p ] off)
    | Some _, _ ->
        refuse ($"a BLOCKED rank-{r} simplex (the blocked schedule is rank-2 only; the serial rank-{r} nest is supported)")

/// Enumerate a whole shape's STORAGE coordinates, innermost group last,
/// handing the body the coordinates and the pool offset. Dense groups are
/// plain counted loops; a compact group is its serial triangle. Bricking is
/// NOT applied here: it is a whole-array strategy, chosen by the caller for
/// the shapes where it is defined (a sole rank-2 simplex).
let private emitShapeNest (c: Ctx) (groups: Grp list) (body: Val list -> Val -> unit) : unit =
    let rec go (acc: Val list) (rest: Grp list) =
        match rest with
        | [] ->
            let idxs = List.rev acc
            body idxs { Reg = storageOffset c groups idxs; Ty = ScI64 }
        | GDense n :: tl -> emitCountedLoop c n (fun i -> go ({ Reg = i; Ty = ScI64 } :: acc) tl)
        | g :: tl when grpCompact g ->
            emitSimplexR c (grpRank g) (grpExtent g) (grpStrict g) None (fun emit -> emit ())
                (fun coords _ -> go (List.fold (fun a x -> x :: a) acc coords) tl)
        | _ -> refuse "an index group the llvm lane cannot iterate"
    go [] groups

// ---------------------------------------------------------------------------
// Expression emission
// ---------------------------------------------------------------------------

/// Emit `e` and return its value, coerced to the type `IR.typeOf` reports for
/// the node. The coercion at the boundary is what keeps every consumer honest:
/// a node's emitted register always has the type the IR says it has, so no arm
/// below has to second-guess its children.
let rec private emitExpr (c: Ctx) (e: IRExpr) : Val =
    let v = emitRaw c e
    match scalarTyOf (typeOf e) with
    | Some t when t <> ScVoid && v.Ty <> ScVoid -> coerce c t v
    | _ -> v

and private emitRaw (c: Ctx) (e: IRExpr) : Val =
    match e with
    | IRLit (IRLitInt n) -> { Reg = string n; Ty = ScI64 }
    | IRLit (IRLitFloat f) -> { Reg = f64Const f; Ty = ScF64 }
    // The LLVM lane has no ScF32 (IRTScalar ETFloat32 maps to no scalar and
    // the gate declines Float32 programs), so a Float32 literal that somehow
    // arrives is widened -- value-preserving, and unreachable until an f32
    // scalar type exists here.
    | IRLit (IRLitFloat32 f) -> { Reg = f64Const (float f); Ty = ScF64 }
    | IRLit (IRLitBool b) -> { Reg = (if b then "true" else "false"); Ty = ScBool }
    | IRLit (IRLitString s) -> { Reg = stringGlobal c s; Ty = ScStr }
    | IRLit IRLitUnit -> { Reg = ""; Ty = ScVoid }

    | IRVar (id, _) ->
        (match c.Slots.TryGetValue id with
         | true, (slot, sc) -> loadSlot c slot sc
         | _ ->
             if c.Callables.ContainsKey id then
                 refuse ($"function '{(c.Callables.[id]).Name}' used as a value -- the llvm lane emits direct calls only")
             else refuse ($"reference to an unbound variable (ir id {id})"))

    | IRParam (name, _, _) ->
        (match c.NameSlots.TryGetValue name with
         | true, (slot, sc) -> loadSlot c slot sc
         | _ -> refuse ($"reference to parameter '{name}' outside a function body"))

    | IRBinOp (mode, op, l, r) ->
        if mode = IROuter then refuse "an outer-product binary operator ([+], [*])"
        emitBinOp c op l r

    | IRUnaryOp (op, x) -> emitUnary c op x

    // fma(a, b, c) -> llvm.fma.f64: the fused op is the semantics, so it is
    // emitted as the intrinsic (not fmul+fadd with `contract`), and it
    // carries NO fast-math flags -- correctly rounded on every lane.
    | IRFma (a, b, cc) ->
        need c "declare double @llvm.fma.f64(double, double, double)"
        let va = coerce c ScF64 (emitExpr c a)
        let vb = coerce c ScF64 (emitExpr c b)
        let vc = coerce c ScF64 (emitExpr c cc)
        let dest = freshReg c
        ln c (renderCall { Dest = Some dest; RetTy = ScF64; Callee = "@llvm.fma.f64"
                           Args = [ ScF64, va.Reg; ScF64, vb.Reg; ScF64, vc.Reg ] })
        { Reg = dest; Ty = ScF64 }

    | IRIf (cond, tb, fb) ->
        let resTy = requireScalar "an if-expression" (typeOf e)
        let cv = coerce c ScBool (emitExpr c cond)
        let slot = allocSlot c resTy
        let lThen = freshLbl c "if.then"
        let lElse = freshLbl c "if.else"
        let lEnd = freshLbl c "if.end"
        ln c (renderBr { Cond = cv.Reg; IfTrue = lThen; IfFalse = lElse })
        lbl c lThen
        storeSlot c slot resTy (emitExpr c tb)
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lElse
        storeSlot c slot resTy (emitExpr c fb)
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lEnd
        (match slot with
         | Some s -> loadSlot c s resTy
         | None -> { Reg = ""; Ty = ScVoid })

    | IRMatch (scrutinee, cases) -> emitMatch c e scrutinee cases

    | IRLet (id, value, body) ->
        bindLet c id value
        emitExpr c body

    | IRSequence exprs ->
        let mutable last = { Reg = ""; Ty = ScVoid }
        for x in exprs do last <- emitExpr c x
        last

    | IRAssign (target, value) ->
        (match target with
         // An element write through a `mut` array parameter. It aliases the
         // caller by construction: the parameter IS the caller's pool.
         | IRIndex (arrExpr, idxExprs, _) ->
             let a = materializeExpr c arrExpr
             let idxs = idxExprs |> List.map (fun ix -> coerce c ScI64 (emitExpr c ix))
             if List.length idxs <> a.Rank then
                 refuse "an element write through a partial index"
             // A write into COMPACT storage is refused, not canonicalized: a
             // store through (j, i) would silently land on the canonical cell
             // (i, j) and, for an antisymmetric group, would have to negate
             // the value to mean what the source said. That is a semantic
             // decision the surface has never had to make, so this lane will
             // not invent one.
             if a.Groups |> List.exists grpCompact then
                 refuse "an element write into compact (symmetric/antisymmetric) storage"
             (match a.Src with
              | APool p ->
                  let v = coerce c a.Elem (emitExpr c value)
                  storeCell c a.Elem (gepCell c a.Elem p (storageOffset c a.Groups idxs)) v
                  { Reg = ""; Ty = ScVoid }
              | _ -> refuse "an element write into a computed (non-stored) array")
         | IRVar (id, _) ->
             (match c.Slots.TryGetValue id with
              | true, (slot, sc) ->
                  let v = coerce c sc (emitExpr c value)
                  ln c ($"store {(llTy sc)} {v.Reg}, ptr {slot}")
                  { Reg = ""; Ty = ScVoid }
              | _ ->
                  if c.ArrSlots.ContainsKey id then
                      // Whole-array rebinding: the slot re-points, which is
                      // what the C++ lane's wrapper assignment does too. The
                      // rebound target may predate an open tracking scope, in
                      // which case the new pool escapes it (same rule as
                      // materializeExpr's upgrade).
                      let a = emitArr c value
                      bindArray c id "" a
                      (match a.Src with
                       | APool p when c.PoolScopes |> Seq.exists (fun s -> s.KnownIds.Contains id) ->
                           keepPool c p
                       | _ -> ())
                      { Reg = ""; Ty = ScVoid }
                  else refuse "assignment to a variable with no storage in this scope")
         | _ -> refuse "assignment to a non-variable target")

    | IRApp (func, args, _) -> emitCall c func args

    // THE ONE ORDERED ITERATION CONSTRUCT the IR has, and the only place a
    // program's own sequencing survives into the back end. Two shapes lower
    // through it, both statement-shaped (unit-valued; everything they do they
    // do through stores): `let rec` -- with the front end's prefix reads
    // already turned into hoisted lag bindings and clamped, implicitly-zero
    // guarded reads -- and the rank-k dense fold `reduce(A, op, axes = k)`.
    | IRForRange (vid, lo, hi, body) -> emitForRange c vid lo hi body

    // The rec-array `while` guard's early exit (and the budget abort that
    // rides with it as IRConstraintCheck) would need a conditional-branch
    // loop shape this emitter's single-block for-range does not have yet.
    // Refuse LOUDLY: a silently dropped break would run the full budget and
    // overwrite the frozen slices.
    | IRBreakIf _ ->
        refuse "a `while`-guarded recursive array (early exit / IRBreakIf) -- not supported by the llvm lane yet"

    // ---- array-derived scalars -------------------------------------------
    // A read CONSUMES its base where it can: one cell of a producer costs one
    // kernel application, and materializing a whole pool to fetch it was the
    // same shape the reduce/prodsum fixes removed. The two exceptions are the
    // same two (printing and sharing demand): a base with a SECOND reader
    // materializes so the readers share one pool, and a MODULE BINDING is
    // marked forced so it still prints (the C++ lane's
    // `forceDeferredPositionalReads` notes it for exactly this, corpus
    // loops/117, /192) without being stored here, where a pool allocated
    // inside a nest would not dominate the print that reads it afterwards.
    | IRIndex (arrExpr, idxExprs, _) ->
        let a =
            match arrExpr with
            | IRVar (id, _) when not (c.Facts.ReadOnce.Contains id) -> materializeExpr c arrExpr
            | IRVar (id, _) when c.BindingValues.ContainsKey id ->
                markForced c id
                emitArr c arrExpr
            | _ -> emitArr c arrExpr
        let idxs = idxExprs |> List.map (fun ix -> coerce c ScI64 (emitExpr c ix))
        if List.length idxs <> a.Rank then
            refuse ($"a partial index ({(List.length idxs)} of {a.Rank}) in value position")
        // THE ONE PLACE ABSOLUTE COORDINATES ENTER. Everything else in this
        // file works in storage coordinates; a subscript the user wrote is
        // absolute and may name a mirror cell, so it canonicalizes here.
        canonRead c a idxs

    | IRExtent (arrExpr, dim) ->
        // extents() of a STATIC shape is a constant. The one runtime answer is
        // a peeled ragged row, whose length is the operand its view carries.
        (match arrayShapeOf (typeOf arrExpr) with
         | Some (_, groups) ->
             let extents = axisExtents groups
             if dim < 0 || dim >= List.length extents then
                 refuse ($"extents(_, {dim}) outside the array's rank")
             { Reg = string extents.[dim]; Ty = ScI64 }
         | None ->
             let a = emitArr c arrExpr
             (match a.Groups, dim with
              | [ GDynDense len ], 0 -> { Reg = len; Ty = ScI64 }
              | groups, d ->
                  let extents = axisExtents groups
                  if d < 0 || d >= List.length extents then
                      refuse ($"extents(_, {d}) outside the array's rank")
                  { Reg = string extents.[d]; Ty = ScI64 }))

    | IRRank arrExpr ->
        (match arrayShapeOf (typeOf arrExpr) with
         | Some (_, groups) -> { Reg = string (groups |> List.sumBy grpRank); Ty = ScI64 }
         | None ->
             let a = emitArr c arrExpr
             { Reg = string a.Rank; Ty = ScI64 })

    | IRReduce (arrExpr, kernelExpr, initExpr) -> emitReduce c arrExpr kernelExpr initExpr

    | IRReduceCompute (compExpr, kernelExpr, initExpr) ->
        (match emitReduceCompute c compExpr kernelExpr initExpr with
         | [ v ] -> v
         | vs -> refuse ($"a fused reduction producing {List.length vs} accumulators in scalar position"))

    | IRTupleProj (parent, idx, _) ->
        (match tupleComponent c parent idx with
         | VScalar v -> v
         | _ -> refuse ($"tuple projection [{idx}] of a non-scalar component in value position"))

    | IRPure inner -> emitExpr c inner
    | IRCompute inner -> emitExpr c inner
    | IRBind (comp, cont) -> emitExpr c (bindContinuationBody c comp cont)

    // `a <|> b`: the left operand is evaluated EXACTLY ONCE and kept if it is
    // non-zero, else the right. The C++ lane spells it as a ternary over a
    // bound temporary; branches give the same one-evaluation guarantee here
    // and keep the right side unevaluated when the left wins.
    | IRChoice (a, b) ->
        let resTy = requireScalar "a choice expression" (typeOf e)
        let l = coerce c resTy (emitExpr c a)
        let nz = coerce c ScBool l
        let slot = allocSlot c resTy
        let lKeep = freshLbl c "choice.keep"
        let lElse = freshLbl c "choice.else"
        let lEnd = freshLbl c "choice.end"
        ln c (renderBr { Cond = nz.Reg; IfTrue = lKeep; IfFalse = lElse })
        lbl c lKeep
        storeSlot c slot resTy l
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lElse
        storeSlot c slot resTy (emitExpr c b)
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lEnd
        (match slot with Some s -> loadSlot c s resTy | None -> { Reg = ""; Ty = ScVoid })

    // `guard(p, body)` = `p ? body : <type-appropriate zero>`.
    | IRGuard (cond, body) ->
        let resTy = requireScalar "a guard expression" (typeOf e)
        let cv = coerce c ScBool (emitExpr c cond)
        let slot = allocSlot c resTy
        let lThen = freshLbl c "guard.then"
        let lElse = freshLbl c "guard.else"
        let lEnd = freshLbl c "guard.end"
        ln c (renderBr { Cond = cv.Reg; IfTrue = lThen; IfFalse = lElse })
        lbl c lThen
        storeSlot c slot resTy (emitExpr c body)
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lElse
        storeSlot c slot resTy { Reg = (match resTy with ScF64 -> f64Const 0.0 | ScBool -> "false" | _ -> "0"); Ty = resTy }
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lEnd
        (match slot with Some s -> loadSlot c s resTy | None -> { Reg = ""; Ty = ScVoid })

    | IRProdSum args -> emitProdSum c args

    | other ->
        refuse ($"the IR node {(caseName other)} in value position -- no arm in the llvm lane")

/// `for k in lo..hi { ... }` -- a SEQUENTIAL counted loop, laid down by the
/// same `emitCountedLoopTo` every other loop in this file uses, with the body
/// emitted ONCE into the loop's own block.
///
/// NO FAST-MATH, licensed or not, and nothing to license: a recursive array is
/// ordered by construction (step n reads the prefix step n-1 wrote), and the
/// rank-k dense fold this also carries is the front end's own accumulation
/// nest, which folds in declared row-major order with one accumulator. The
/// arithmetic inside the body is born under whatever scope already applies --
/// `FoldFmf` is not opened here.
///
/// BOTH BOUNDS ARE EVALUATED ONCE, ahead of the loop. That is the
/// interpreter's contract verbatim (`Interp/Core.fs`: "IRForRange evaluates
/// both bounds once, then runs a single reused int64"), and it is the only
/// shape that keeps a bound expression's own instructions out of the trip
/// count. The C++ lane's `for (int64_t __k = lo; __k < hi; __k++)` agrees on
/// every bound a Blade program can currently present -- the desugars build
/// literals, or an extent that `StaticEval` already folded to one.
///
/// THE LOOP VARIABLE IS THE USER'S `Int64` and gets its own slot rather than
/// riding the counter register: the body reads it as an ordinary `IRVar`, and
/// the implicit-zero guards the recursive-array desugar emits compare it (and
/// its lags) against zero, so a counter and a loop variable are not
/// interchangeable the moment `lo` is not zero.
///
/// A NEGATIVE TRIP COUNT IS ZERO ITERATIONS -- `0 slt <negative>` is false at
/// the first test -- which is what `hi <= lo` means in both other lanes.
and private emitForRange (c: Ctx) (vid: IRId) (lo: IRExpr) (hi: IRExpr) (body: IRExpr) : Val =
    let loV = coerce c ScI64 (emitExpr c lo)
    let hiV = coerce c ScI64 (emitExpr c hi)
    // The literal-zero start folds away here exactly as `i64Add` folds its
    // identity: `for n in 0..N` is then the bare counted loop and the loop
    // variable IS the counter, with no arithmetic standing between them.
    let trips = if loV.Reg = "0" then hiV.Reg else i64Bin c "sub" hiV.Reg loV.Reg
    let slot = ensureSlot c vid ScI64
    forceLoopBodyProducers c body
    let before = arrSlotSnapshot c
    emitCountedLoopTo c trips (fun k ->
        ln c ($"store i64 {(i64Add c loV.Reg k)}, ptr {slot}")
        // Each trip is a tracking scope: a step's materializations (a rank-2
        // `let rec`'s per-step slice, a copy-on-alias) are dead when the trip
        // ends, and freeing them here is what keeps a long trajectory's
        // resident footprint at one step's temps instead of all of them.
        // Escapes (a pool upgrading a binding that predates the trip) null
        // their slot via `keepPool` and survive.
        pushPoolScope c
        emitExpr c body |> ignore
        popPoolScope c None true)
    requireArrSlotsStable c before
    { Reg = ""; Ty = ScVoid }

/// Materialize, BEFORE the loop opens, every deferred array binding the body
/// will read BY NAME.
///
/// This is `genForRangeBinding`'s own first act -- `forceDeferredPositionalReads`
/// (CodeGenBinding.fs), which the C++ lane runs for the obvious reason that a
/// producer materialized inside the body would re-run once per trip. Here it is
/// not an optimization at all but the difference between valid and invalid IR:
/// the pool would additionally be a register the code AFTER the loop does not
/// dominate, and the binding -- now forced, and therefore printed -- is read
/// exactly there.
///
/// WHICH READS COUNT AS BY-NAME is decided by `collectDeferredPositionalReads`,
/// the C++ lane's own collector, CALLED rather than re-walked. Not a
/// convenience: forcing one binding more than the other lane forces adds a
/// printed line, and the differential would report the extra line as drift; a
/// second copy of that walk would drift the moment either lane grew a node.
/// The probe context it takes is read for its deferred KEY SET only, so the
/// values are placeholders.
and private forceLoopBodyProducers (c: Ctx) (body: IRExpr) : unit =
    let deferred =
        [ for KeyValue (id, a) in c.ArrSlots do
            match a.Src with
            | APool _ -> ()
            | AVirt _ | AVirtFlat _ -> yield (id, IRLit IRLitUnit) ]
        |> Map.ofList
    if not (Map.isEmpty deferred) then
        let probe = { Blade.CodeGenState.emptyContext () with DeferredComputations = deferred }
        for id in Blade.CodeGenFusion.collectDeferredPositionalReads probe body do
            match c.ArrSlots.TryGetValue id with
            | true, a ->
                c.ArrSlots.[id] <- materialize c a
                markForced c id
            | _ -> ()

/// The array bindings a loop body may not REBIND, checked by snapshot.
///
/// Every array in this emitter is a register -- a pool pointer, or a producer
/// closure over one -- and a register born inside a loop body does not
/// dominate the code after the loop. So a binding that already existed when
/// the loop opened and holds something DIFFERENT when it closes is a dominance
/// violation waiting for its next reader: `let mut acc = A(0)` re-pointed
/// once per step is the leading-axis fold's shape, and the honest fix is a
/// pointer slot for array bindings (the same treatment scalars already get),
/// not a patch at this seam. Until that exists the program is refused by name
/// and the C++ lane takes it.
///
/// REFERENCE IDENTITY IS THE EXACT TEST, and it is exact rather than
/// conservative: `materialize` of an already-stored array returns the very
/// object it was handed, so a binding merely read (or forced, which re-stores
/// the same instance) is still the same instance, while every rebinding builds
/// a new record. Checking AFTER the body is emitted costs nothing -- a refusal
/// discards the whole module, so the instructions already written are never
/// seen.
and private arrSlotSnapshot (c: Ctx) : (IRId * ArrVal) list =
    [ for KeyValue (id, a) in c.ArrSlots -> (id, a) ]

and private requireArrSlotsStable (c: Ctx) (before: (IRId * ArrVal) list) : unit =
    for (id, a) in before do
        match c.ArrSlots.TryGetValue id with
        | true, now when obj.ReferenceEquals(now, a) -> ()
        | _ ->
            refuse ($"an array binding (ir id {id}) rebound inside a for-range loop -- its storage would not outlive the loop body")

/// `prodsum(x1..xk)` = `sum_t prod_l x_l(t)` over rank-1 operands of equal
/// extent: ONE loop, one accumulator, ascending -- the shape the moment
/// formers' fiber kernels land on, and the same association the C++ IIFE uses.
and private emitProdSum (c: Ctx) (args: IRExpr list) : Val =
    if List.isEmpty args then refuse "prodsum with no operands"
    // Operands are CONSUMED, on the same three-way rule `emitReduce` uses --
    // the loop below reads every operand's cell exactly once, so running a
    // producer costs precisely what filling a pool from it would, minus the
    // pool. This matters because prodsum's home is the moment-formers' fiber
    // kernels, i.e. inside a kernel body, where forcing meant one pool and
    // one fill pass PER OUTPUT CELL -- the same shape as the reduce leak.
    //
    // The two exceptions are the ones printing and sharing demand:
    //   * a name with a SECOND reader must materialize, so the readers share
    //     one pool rather than re-running the producer each time;
    //   * a MODULE BINDING must be marked forced so it still prints (the C++
    //     lane notes prodsum operands for exactly this, corpus loops/117),
    //     but must NOT be materialized here -- this prodsum may sit inside a
    //     loop nest, and a pool allocated there would not dominate the print
    //     that reads it afterwards.
    let ops =
        args
        |> List.map (fun a ->
            match a with
            | IRVar (id, _) when not (c.Facts.ReadOnce.Contains id) -> materializeExpr c a
            | IRVar (id, _) when c.BindingValues.ContainsKey id ->
                markForced c id
                emitArr c a
            | _ -> emitArr c a)
    let head = List.head ops
    if head.Rank <> 1 then refuse "prodsum over a rank-2+ operand"
    let ty = if head.Elem = ScBool then ScI64 else head.Elem
    let acc = allocaOf c (llTy ty)
    // The OUTER accumulation is `+`, which is licensed by construction: the
    // fold kernel is not a user kernel at all here, it is the builtin the
    // C++ lane's own prodsum IIFE reassociates under the same knob
    // (`CodeGenExpr.fs`'s `if not (fpReassocEnabled ()) then serialForm`).
    // The per-element PRODUCT chain gets no such flag -- reordering the
    // factors of one element is a different reassociation nobody licensed.
    let accFlags =
        withFoldFmf c (Blade.CodeGenState.fpReassocEnabled ()) (fun () -> fmfFor c ty)
    ln c ($"""store {(llTy ty)} {(match ty with ScF64 -> f64Const 0.0 | _ -> "0")}, ptr {acc}""")
    emitCountedLoopTo c (soleExtentOp head) (fun t ->
        let iv = { Reg = t; Ty = ScI64 }
        let mutable prod = coerce c ty (readCell c head [ iv ])
        for o in List.tail ops do
            let v = coerce c ty (readCell c o [ iv ])
            let d = freshReg c
            ln c (renderBin { Dest = d; Opcode = (if ty = ScF64 then "fmul" else "mul"); Flags = fmfFor c ty; Ty = ty; Lhs = prod.Reg; Rhs = v.Reg })
            prod <- { Reg = d; Ty = ty }
        let cur = loadSlot c acc ty
        let s = freshReg c
        ln c (renderBin { Dest = s; Opcode = (if ty = ScF64 then "fadd" else "add"); Flags = accFlags; Ty = ty; Lhs = cur.Reg; Rhs = prod.Reg })
        ln c ($"store {(llTy ty)} {s}, ptr {acc}"))
    loadSlot c acc ty

/// `c >>= k`: bind the continuation's parameter to the computation and answer
/// its BODY, so the caller emits it in whatever position the bind sits in.
and private bindContinuationBody (c: Ctx) (comp: IRExpr) (cont: IRExpr) : IRExpr =
    let cl =
        match resolveCallable cont with
        | Some cl when cl.Params.Length = 1 -> cl
        | Some cl -> refuse ($"a bind continuation '{cl.Name}' of {cl.Params.Length} parameters")
        | None -> refuse "a bind whose continuation does not resolve to a callable"
    let p = List.head cl.Params
    (match classifyValue c comp with
     | VArray a -> bindKernelParam c p (KArray a)
     | VScalar v -> bindKernelParam c p (KScalar v)
     | _ -> refuse "a bind over a pack-valued computation")
    cl.Body

and private tupleComponent (c: Ctx) (parent: IRExpr) (idx: int) : ValKind =
    let parts = emitTupleParts c parent
    if idx < 0 || idx >= List.length parts then
        refuse ($"tuple projection [{idx}] outside a pack of {List.length parts}")
    parts.[idx]

/// The per-AXIS extents of an array-valued EXPRESSION -- what `extents()` and
/// `rank()` report. Prefers the declared type (free) and falls back to
/// emitting the array (not free, but correct for a shape the type erased).
and private arrayShapeOfExpr (c: Ctx) (e: IRExpr) : Sc * int64 list =
    match arrayShapeOf (typeOf e) with
    | Some (elem, groups) -> (elem, axisExtents groups)
    | None ->
        let a = emitArr c e
        (a.Elem, a.Extents)

/// A result slot, or None for a unit-valued construct that stores nothing.
and private allocSlot (c: Ctx) (ty: Sc) : string option =
    if ty = ScVoid then None
    else Some (allocaOf c (llTy ty))

and private storeSlot (c: Ctx) (slot: string option) (ty: Sc) (v: Val) : unit =
    match slot with
    | Some s ->
        let v = coerce c ty v
        ln c ($"store {(llTy ty)} {v.Reg}, ptr {s}")
    | None -> ()

and private loadSlot (c: Ctx) (slot: string) (ty: Sc) : Val =
    let dest = freshReg c
    ln c ($"{dest} = load {(llTy ty)}, ptr {slot}")
    { Reg = dest; Ty = ty }

and private emitBinOp (c: Ctx) (op: IRBinOp) (l: IRExpr) (r: IRExpr) : Val =
    match op with
    // `&&` / `||` SHORT-CIRCUIT, and that is not a micro-optimization here:
    // a plain `and i1` would evaluate a right operand C++ never runs, and the
    // guarded-divisor idiom (`d != 0 && n / d > 1`) turns an unreached sdiv
    // into immediate UB.
    | IRAnd | IROr -> emitShortCircuit c op l r
    | IRCaret ->
        let a = coerce c ScF64 (emitExpr c l)
        let b = coerce c ScF64 (emitExpr c r)
        callLibm2 c "pow" a b
    | IRMath2 "log_base" ->
        // No libm log_base; the quotient IS the definition, matching
        // CodeGenExprSupport.renderMath2.
        let a = callLibm1 c "log" (coerce c ScF64 (emitExpr c l))
        let b = callLibm1 c "log" (coerce c ScF64 (emitExpr c r))
        let dest = freshReg c
        ln c (renderBin { Dest = dest; Opcode = "fdiv"; Flags = fmfFor c ScF64; Ty = ScF64; Lhs = a.Reg; Rhs = b.Reg })
        { Reg = dest; Ty = ScF64 }
    | IRMath2 "atan2" ->
        let a = coerce c ScF64 (emitExpr c l)
        let b = coerce c ScF64 (emitExpr c r)
        callLibm2 c "atan2" a b
    | IRMath2 name -> refuse ($"the binary math intrinsic '{name}'")
    | _ ->
        let a0 = emitExpr c l
        let b0 = emitExpr c r
        let common = promote a0.Ty b0.Ty
        let a = coerce c common a0
        let b = coerce c common b0
        let dest = freshReg c
        match op with
        | IREq | IRNeq | IRLt | IRLe | IRGt | IRGe ->
            let kind = if common = ScF64 then "fcmp" else "icmp"
            let pred =
                match common, op with
                // ORDERED comparisons except `!=`: C's `a != b` is `!(a == b)`,
                // which is true when either operand is NaN -- `une`, not `one`.
                | ScF64, IREq -> "oeq" | ScF64, IRNeq -> "une"
                | ScF64, IRLt -> "olt" | ScF64, IRLe -> "ole"
                | ScF64, IRGt -> "ogt" | ScF64, IRGe -> "oge"
                | _, IREq -> "eq" | _, IRNeq -> "ne"
                | _, IRLt -> "slt" | _, IRLe -> "sle"
                | _, IRGt -> "sgt" | _, IRGe -> "sge"
                | _ -> refuse "an unrecognized comparison"
            ln c (renderCmp { Dest = dest; Kind = kind; Pred = pred; Ty = common; Lhs = a.Reg; Rhs = b.Reg })
            { Reg = dest; Ty = ScBool }
        | _ ->
            let opcode =
                match common, op with
                | ScF64, IRAdd -> "fadd" | ScF64, IRSub -> "fsub"
                | ScF64, IRMul -> "fmul" | ScF64, IRDiv -> "fdiv"
                | ScF64, IRMod -> refuse "`%` applied to floating-point operands"
                | ScI64, IRAdd -> "add" | ScI64, IRSub -> "sub"
                | ScI64, IRMul -> "mul"
                // C++ integer division and `%` truncate toward zero: sdiv/srem.
                | ScI64, IRDiv -> "sdiv" | ScI64, IRMod -> "srem"
                | _ -> refuse ($"arithmetic on {(llTy common)} operands")
            ln c (renderBin { Dest = dest; Opcode = opcode; Flags = fmfFor c common; Ty = common; Lhs = a.Reg; Rhs = b.Reg })
            { Reg = dest; Ty = common }

and private emitShortCircuit (c: Ctx) (op: IRBinOp) (l: IRExpr) (r: IRExpr) : Val =
    let slot = allocaOf c "i1"
    let a = coerce c ScBool (emitExpr c l)
    ln c ($"store i1 {a.Reg}, ptr {slot}")
    let lRhs = freshLbl c "sc.rhs"
    let lEnd = freshLbl c "sc.end"
    // `&&` evaluates the right side only when the left held; `||` only when it
    // did not.
    if op = IRAnd then ln c (renderBr { Cond = a.Reg; IfTrue = lRhs; IfFalse = lEnd })
    else ln c (renderBr { Cond = a.Reg; IfTrue = lEnd; IfFalse = lRhs })
    lbl c lRhs
    let b = coerce c ScBool (emitExpr c r)
    ln c ($"store i1 {b.Reg}, ptr {slot}")
    ln c (sprintf "br label %%%s" lEnd)
    lbl c lEnd
    loadSlot c slot ScBool

and private emitUnary (c: Ctx) (op: IRUnaryOp) (x: IRExpr) : Val =
    match op with
    | IRNeg ->
        let v = emitExpr c x
        let dest = freshReg c
        (match v.Ty with
         | ScF64 ->
             ln c ($"{dest} = fneg{(fmfFor c ScF64)} double {v.Reg}")
             { Reg = dest; Ty = ScF64 }
         | ScI64 ->
             ln c (renderBin { Dest = dest; Opcode = "sub"; Flags = ""; Ty = ScI64; Lhs = "0"; Rhs = v.Reg })
             { Reg = dest; Ty = ScI64 }
         | other -> refuse ($"negation of a {(llTy other)} value"))
    | IRNot ->
        let v = coerce c ScBool (emitExpr c x)
        let dest = freshReg c
        ln c (renderBin { Dest = dest; Opcode = "xor"; Flags = ""; Ty = ScBool; Lhs = v.Reg; Rhs = "true" })
        { Reg = dest; Ty = ScBool }
    // On a REAL operand conj and real are the identity and imag is zero,
    // exactly as <complex>'s arithmetic overloads define them. A complex
    // operand never reaches here: its type refuses first.
    | IRConj | IRReal -> emitExpr c x
    | IRImag -> { Reg = f64Const 0.0; Ty = ScF64 }
    | IRArg -> refuse "arg() (complex phase)"
    | IRMath name ->
        (match Map.tryFind name libmUnary with
         | Some fn -> callLibm1 c fn (coerce c ScF64 (emitExpr c x))
         | None when name = "lgamma" || name = "digamma" ->
             refuse ($"{name} -- Blade's own series lives in blade_runtime.hpp and has no libm twin")
         | None -> refuse ($"the math intrinsic '{name}'"))
    // Explicit numeric cast: the lane's own scalar coercion (sitofp/fptosi)
    // covers the Float64/Int64 targets; the widths this lane has no scalar
    // for (Float32/Int32/complex) refuse like every other Float32/complex
    // program does.
    | IRCast ETFloat64 -> coerce c ScF64 (emitExpr c x)
    | IRCast ETInt64 -> coerce c ScI64 (emitExpr c x)
    | IRCast et -> refuse ($"a numeric cast to {(Blade.Types.castNameOf et)}")

and private callLibm1 (c: Ctx) (fn: string) (arg: Val) : Val =
    need c ($"declare double @{fn}(double{(paramAttr ())}){(attrRef c grpExternReturns)}")
    let dest = freshReg c
    ln c (renderCall { Dest = Some dest; RetTy = ScF64; Callee = "@" + fn; Args = [ ScF64, arg.Reg ] })
    { Reg = dest; Ty = ScF64 }

and private callLibm2 (c: Ctx) (fn: string) (a: Val) (b: Val) : Val =
    need c ($"declare double @{fn}(double{(paramAttr ())}, double{(paramAttr ())}){(attrRef c grpExternReturns)}")
    let dest = freshReg c
    ln c (renderCall { Dest = Some dest; RetTy = ScF64; Callee = "@" + fn; Args = [ ScF64, a.Reg; ScF64, b.Reg ] })
    { Reg = dest; Ty = ScF64 }

and private emitMatch (c: Ctx) (whole: IRExpr) (scrutinee: IRExpr) (cases: IRMatchCase list) : Val =
    if List.isEmpty cases then refuse "a match with no arms"
    let resTy = requireScalar "a match expression" (typeOf whole)
    let sv = emitExpr c scrutinee
    let slot = allocSlot c resTy
    let lEnd = freshLbl c "match.end"
    let lFail = freshLbl c "match.fail"
    let rec arms (cs: IRMatchCase list) =
        match cs with
        | [] -> ln c (sprintf "br label %%%s" lFail)
        | case :: rest ->
            let lNext = freshLbl c "match.next"
            // Pattern test. A wildcard/variable arm has none -- it always
            // matches -- so `lNext` for that arm is simply unreachable, which
            // is well-formed as long as it still gets a terminator (it does,
            // from the arm that follows or from the empty-case arm above).
            (match case.Pattern with
             | IRPatWild -> ()
             | IRPatVar vid ->
                 let slotV = allocaOf c (llTy sv.Ty)
                 ln c ($"store {(llTy sv.Ty)} {sv.Reg}, ptr {slotV}")
                 c.Slots.[vid] <- (slotV, sv.Ty)
             | IRPatLit lit ->
                 let lv = coerce c sv.Ty (emitExpr c (IRLit lit))
                 if sv.Ty = ScStr then refuse "a String literal pattern"
                 let t = freshReg c
                 let kind = if sv.Ty = ScF64 then "fcmp" else "icmp"
                 let pred = if sv.Ty = ScF64 then "oeq" else "eq"
                 ln c (renderCmp { Dest = t; Kind = kind; Pred = pred; Ty = sv.Ty; Lhs = sv.Reg; Rhs = lv.Reg })
                 let lBody = freshLbl c "match.body"
                 ln c (renderBr { Cond = t; IfTrue = lBody; IfFalse = lNext })
                 lbl c lBody
             | other ->
                 refuse (sprintf "the match pattern %A -- the llvm lane handles wildcard, variable and literal patterns" other))
            // A failed guard falls through to the next arm, and on the LAST
            // arm that lands in the panic block -- the same non-exhaustive
            // behavior renderMatchExpr gives the C++ lane.
            (match case.Guard with
             | Some g ->
                 let gv = coerce c ScBool (emitExpr c g)
                 let lGuarded = freshLbl c "match.guarded"
                 ln c (renderBr { Cond = gv.Reg; IfTrue = lGuarded; IfFalse = lNext })
                 lbl c lGuarded
             | None -> ())
            storeSlot c slot resTy (emitExpr c case.Body)
            ln c (sprintf "br label %%%s" lEnd)
            lbl c lNext
            arms rest
    arms cases
    lbl c lFail
    needShim c "blade_panic"
    let msg = stringGlobal c "error[BL8002]: Blade: non-exhaustive match"
    ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_panic"; Args = [ ScStr, msg ] })
    ln c "unreachable"
    lbl c lEnd
    match slot with
    | Some s -> loadSlot c s resTy
    | None -> { Reg = ""; Ty = ScVoid }

and private emitCall (c: Ctx) (func: IRExpr) (args: IRExpr list) : Val =
    let callable =
        match func with
        | IRVar (id, _) ->
            (match Map.tryFind id c.Callables with
             | Some cl -> cl
             | None -> refuse "a call to a function the module does not define (indirect or higher-order)")
        | _ -> refuse "an indirect / higher-order call"
    if callable.IsArityPoly then refuse ($"a call to the arity-polymorphic function '{callable.Name}'")
    if callable.IsCudaKernel || callable.IsMpiParallel then
        refuse ($"a call to '{callable.Name}', which carries a cuda/mpi strategy")
    // An `omp` clause is a LICENCE to parallelize, not a demand: emitting the
    // serial body answers the same values. (Licensed reassociation is a
    // separate question and stays off -- see the numeric policy above.)
    //
    // A CAPTURING callee has no closure ABI in this lane, so it is INLINED
    // instead: capture references are ordinary IR ids and resolve through the
    // caller's own slot tables. Recursion would not terminate that way, so a
    // re-entered inline refuses rather than looping.
    if not (List.isEmpty callable.Captures) then
        if not (c.Inlining.Add callable.Id) then
            refuse ($"a recursive call to '{callable.Name}', which captures enclosing bindings")
        try
            let kargs =
                List.zip callable.Params args
                |> List.map (fun (p, a) ->
                    match arrayShapeOf p.Type with
                    | Some _ -> KArray (emitArr c a)
                    | None when (raggedArrShape p.Type).IsSome -> KArray (emitArr c a)
                    | None -> KScalar (emitExpr c a))
            applyKernel c callable kargs
        finally c.Inlining.Remove callable.Id |> ignore
    else
    if List.length callable.Params <> List.length args then
        refuse ($"a partial application of '{callable.Name}' ({(List.length args)} of {(List.length callable.Params)} arguments)")
    let sym = ensureFunction c callable
    // An array argument crosses as its POOL POINTER, which is also what makes
    // `mut` work: the callee's element writes land in the caller's storage
    // because there is only one pool. Materializing here is therefore not an
    // optimization -- a producer has no address for the callee to write to.
    let argVals =
        List.zip callable.Params args
        |> List.collect (fun (p, a) ->
            match arrayShapeOf p.Type with
            | Some (elem, groups) ->
                let av = materializeExpr c a
                if av.Elem <> elem || av.Groups <> groups then
                    refuse ($"argument shape disagrees with parameter '{p.Name}' of '{callable.Name}'")
                (match av.Src with
                 | APool ptr -> [ (ScStr, ptr) ]
                 | _ -> refuse "an unmaterialized array argument")
            | None ->
                match raggedArrShape p.Type with
                | Some (elem, rows) ->
                    // A ragged pair crosses as TWO arguments: the pool and its
                    // offsets table. A static table's global symbol IS a ptr
                    // operand; a forwarded parameter's table is a register.
                    let av = materializeExpr c a
                    (match av.Groups, av.Src with
                     | [ GRagged (r2, table) ], APool ptr when av.Elem = elem && r2 = rows ->
                         let tp = match table with RtStatic (_, sym) -> sym | RtDynamic reg -> reg
                         [ (ScStr, ptr); (ScStr, tp) ]
                     | _ ->
                         refuse ($"argument shape disagrees with ragged parameter '{p.Name}' of '{callable.Name}'"))
                | None ->
                    let sc = requireScalar ($"parameter '{p.Name}' of '{callable.Name}'") p.Type
                    let v = coerce c sc (emitExpr c a)
                    [ (sc, v.Reg) ])
    match arrayShapeOf callable.RetType with
    | Some _ ->
        let dest = freshReg c
        ln c (renderCall { Dest = Some dest; RetTy = ScStr; Callee = sym; Args = argVals })
        // A FreshPool callee's return is a pool THIS frame owns: track it
        // like a local allocation, so the enclosing scope's exit frees it.
        // This was the arena model's last unbounded edge -- an array-returning
        // function called per cell (or per `let rec` step) leaked its result
        // once per call. NotFresh stays untracked: unproven is leaked, never
        // freed.
        if c.PoolScopes.Count > 0 && c.Facts.FreshReturns.Contains callable.Id then
            let slot = allocaOf c "ptr"
            ln c ($"store ptr {dest}, ptr {slot}")
            c.PoolScopes.[c.PoolScopes.Count - 1].Pools.Add(dest, slot)
        { Reg = dest; Ty = ScStr }
    | None ->
    let retTy = requireScalar ($"the return type of '{callable.Name}'") callable.RetType
    if retTy = ScVoid then
        ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = sym; Args = argVals })
        { Reg = ""; Ty = ScVoid }
    else
        let dest = freshReg c
        ln c (renderCall { Dest = Some dest; RetTy = retTy; Callee = sym; Args = argVals })
        { Reg = dest; Ty = retTy }

/// Claim a symbol for a callable and queue its body. The symbol is recorded
/// BEFORE the body is emitted, which is exactly what makes self-recursion work
/// and what makes a re-entrant reference cheap.
and private ensureFunction (c: Ctx) (cl: IRCallable) : string =
    match c.Emitted.TryGetValue cl.Id with
    | true, sym -> sym
    | _ ->
        let sym = $"@bl_{(sanitizeSym cl.Name)}_{cl.Id}"
        c.Emitted.[cl.Id] <- sym
        c.Pending.Enqueue cl
        sym

// ---------------------------------------------------------------------------
// Binding a name to a value
// ---------------------------------------------------------------------------

/// One slot per IR id, reused across every store to it. Reuse is what makes a
/// kernel parameter legal inside a loop: the slot is allocated once in the
/// entry block and rewritten each iteration.
and private ensureSlot (c: Ctx) (varId: IRId) (ty: Sc) : string =
    match c.Slots.TryGetValue varId with
    | true, (slot, existing) when existing = ty -> slot
    | _ ->
        let slot = allocaOf c (llTy ty)
        c.Slots.[varId] <- (slot, ty)
        slot

and private bindScalar (c: Ctx) (varId: IRId) (name: string) (v: Val) : unit =
    let slot = ensureSlot c varId v.Ty
    ln c ($"store {(llTy v.Ty)} {v.Reg}, ptr {slot}")
    if name <> "" then c.NameSlots.[name] <- (slot, v.Ty)

and private bindArray (c: Ctx) (varId: IRId) (name: string) (a: ArrVal) : unit =
    c.ArrSlots.[varId] <- a
    if name <> "" then c.NameArrSlots.[name] <- a

/// Bind a NAME to a value: the one entry point for both `IRLet` and a
/// top-level binding, so the two cannot drift on the copy rule below.
///
/// COPY-ON-ALIAS: an assignable binding initialized from an EXISTING stored
/// array deep-copies its pool. Every Blade binding except `static` is
/// assignable, so `let b = Z` followed by `b(0) = ...` must not reach into
/// Z's storage -- the C++ lane learned this the hard way (the mut-aliasing
/// bug pinned by loops/081). A deferred producer is exempt: it has no storage
/// to share, and copying it would materialize -- and therefore PRINT -- a
/// computation the program never forced.
and private bindNamed (c: Ctx) (id: IRId) (name: string) (declTy: IRType) (value: IRExpr) : unit =
    match classifyValue c value with
    | VArray a ->
        let stored =
            match value, a.Src with
            // Only a RENAME copies. A sub-view binding (`let row = M(0)`) is a
            // VIEW in the C++ lane too -- copying it would break writes made
            // through a `mut` array's peeled row.
            | (IRVar _ | IRParam _), APool _ -> copyArr c a
            | _ -> a
        bindArray c id name stored
    | VTuple vs -> c.TupleSlots.[id] <- vs
    | VScalar v ->
        if v.Ty <> ScVoid then
            let target = match scalarTyOf declTy with Some t when t <> ScVoid -> t | _ -> v.Ty
            bindScalar c id name (coerce c target v)
    | VOpaque -> c.LoopObjs.Add id |> ignore

and private bindLet (c: Ctx) (id: IRId) (value: IRExpr) : unit =
    bindNamed c id "" (typeOf value) value

and private copyArr (c: Ctx) (a: ArrVal) : ArrVal =
    match a.Src with
    | AVirt _ | AVirtFlat _ -> a
    | APool src ->
        let total = shapeCellsOp c a.Groups
        let dst = allocPool c a.Elem total
        emitCountedLoopTo c total (fun i ->
            let v = loadCell c a.Elem (gepCell c a.Elem src i)
            storeCell c a.Elem (gepCell c a.Elem dst i) v)
        { a with Src = APool dst }

/// What a value expression produced. The split exists because an LLVM value is
/// one register, a Blade value may be a pool, a producer or a flat pack -- and
/// the difference is decided by the expression, not the type (a fused
/// reduction join is `IRTTuple`-typed only after the checker ran).
and private classifyValue (c: Ctx) (e: IRExpr) : ValKind =
    match e with
    | IRMethodFor _ | IRObjectFor _ | IRComposeObj _ | IRCompose _ -> VOpaque
    // `let f = lambda(x, y) -> ...`: the lambda already lifted to a module
    // function and the binding is an ALIAS for it (IRPrint's alias table is
    // what every kernel slot resolves through). Nothing to emit.
    | IRVar (id, _) when c.Callables.ContainsKey id && not (c.ArrSlots.ContainsKey id) && not (c.Slots.ContainsKey id) -> VOpaque
    | IRReduceCompute (comp, k, i) ->
        (match emitReduceCompute c comp k i with
         | [ v ] -> VScalar v
         | vs -> VTuple (vs |> List.map VScalar))
    | IRParallel _ | IRFusion _ -> VTuple (emitTupleParts c e)
    | IRCompute (IRParallel _ | IRFusion _ | IRTuple _) -> VTuple (emitTupleParts c e)
    | IRTuple _ -> VTuple (emitTupleParts c e)
    // A BARE range in value position (`let xs = 0..n`, its `|> compute`
    // form, the lift pass's hoisted reduce operand): an ARRAY value, though
    // `typeOf` answers the IntValued tier's Int64 scalar (the element under
    // nest peeling), so the type fall-through below would misroute it to the
    // scalar lane's refusal. emitArr already carries the descriptor;
    // materialize it (rather than leave it AVirt) to mirror the C++ lane's
    // genRangeBinding -- every Blade binding except `static` is assignable,
    // and a write needs a pool. Non-plain/multi-slot ranges still refuse
    // inside emitArr/staticExtentOf, keeping the lane's clean-skip contract.
    | IRRange _ -> VArray (materialize c (emitArr c e))
    | IRCompute (IRRange _ as inner) -> VArray (materialize c (emitArr c inner))
    | _ ->
        match Blade.IR.stripUnits (typeOf e) with
        | ArrayElem _ -> VArray (emitArr c e)
        // A `Computation<T>` over a SCALAR (`pure(42)`) carries no shape: the
        // monad wrapper is erased and the value is an ordinary register.
        | IRTComputation inner when (scalarTyOf inner).IsSome -> VScalar (emitExpr c e)
        | IRTComputation _ -> VArray (emitArr c e)
        | IRTTuple _ -> VTuple (emitTupleParts c e)
        | _ -> VScalar (emitExpr c e)

// ---------------------------------------------------------------------------
// Array-valued expressions
// ---------------------------------------------------------------------------

and private emitArr (c: Ctx) (e: IRExpr) : ArrVal =
    match e with
    | IRVar (id, _) ->
        (match c.ArrSlots.TryGetValue id with
         | true, a -> a
         | _ ->
             if c.LoopObjs.Contains id then
                 refuse "a loop object (method_for/object_for) used where an array is required"
             else refuse ($"reference to an unbound array (ir id {id})"))

    | IRParam (name, _, _) ->
        (match c.NameArrSlots.TryGetValue name with
         | true, a -> a
         | _ -> refuse ($"reference to array parameter '{name}' outside a function body"))

    | IRArrayLit (elems, arrType) -> emitArrayLit c elems arrType

    // `range<I>` / `reverse<I>` as a value in their own right. Inside a
    // combinator they never become one -- the nest peels them as induction
    // values (VirtualKind), which is the whole point of a virtual array.
    | IRRange (ixs, offset) ->
        let extents = ixs |> List.map (fun ix -> staticExtentOf ix "range")
        let off =
            match offset with
            | Some o -> coerce c ScI64 (emitExpr c o)
            | None -> { Reg = "0"; Ty = ScI64 }
        { Elem = ScI64; Groups = denseGroups extents
          RowOpBytes = 0L
          Src = AVirt (fun c2 idxs ->
                    match idxs with
                    | [ i ] -> addI64 c2 i off
                    | _ -> refuse "a multi-slot range read outside a loop nest") }

    | IRVirtualReverse ix ->
        let n = staticExtentOf ix "reverse"
        { Elem = ScI64; Groups = [ GDense n ]
          RowOpBytes = 0L
          Src = AVirt (fun c2 idxs ->
                    match idxs with
                    | [ i ] ->
                        let d = freshReg c2
                        ln c2 (renderBin { Dest = d; Opcode = "sub"; Flags = ""; Ty = ScI64
                                           Lhs = string (n - 1L); Rhs = i.Reg })
                        { Reg = d; Ty = ScI64 }
                    | _ -> refuse "a multi-slot reverse read outside a loop nest") }

    // `|> compute` IS the materialization site, and the only one: everything
    // else in this file stays a producer.
    | IRCompute inner -> materialize c (emitArr c inner)
    | IRPure inner -> emitArr c inner

    | IRApplyCombinator info -> applyToArr c info
    | IRApp (IRObjectFor objInfo, args, retType) -> objectForApply c objInfo args retType
    | IRApp _ -> emitArrCall c e

    | IRIndex (arrExpr, idxExprs, _) ->
        let a = emitArr c arrExpr
        let idxs = idxExprs |> List.map (fun ix -> coerce c ScI64 (emitExpr c ix))
        if List.length idxs >= a.Rank then
            refuse "a full index where an array was expected"
        rowView c a idxs

    | IRTupleProj (parent, idx, _) ->
        (match tupleComponent c parent idx with
         | VArray a -> a
         | _ -> refuse ($"tuple projection [{idx}] of a non-array component in array position"))

    | IRLet (id, value, body) ->
        bindLet c id value
        emitArr c body

    | IRSequence exprs ->
        (match List.rev exprs with
         | last :: earlier ->
             for x in List.rev earlier do emitExpr c x |> ignore
             emitArr c last
         | [] -> refuse "an empty sequence in array position")

    | IRBind (comp, cont) -> emitArr c (bindContinuationBody c comp cont)

    // AN ARRAY-VALUED `if`, which is one construct's signature and not a
    // general facility: the guarded prefix read a recursive array desugars to.
    // `prefix(n - 3)` at n < 3 is formalism 7.5's implicit zero, and
    // TypeCheckInfer's `guardWrap` spells it as a select over a CLAMPED row
    // read -- `if n - 3 >= 0 then <the lag row> else <the zero slice>` --
    // because merely FORMING an out-of-range row view is undefined, so the
    // discarded branch has to stay in bounds too.
    //
    // What the select answers is a POINTER, chosen once, rather than a
    // producer that re-tests the condition in every cell of the copy nest that
    // consumes it. Both branches are stored arrays of one shape here (a row
    // view and a zero literal); a branch that is still a producer materializes
    // INSIDE its own block, so nothing escapes the arm that built it.
    | IRIf (cond, tb, fb) ->
        let cv = coerce c ScBool (emitExpr c cond)
        let slot = allocaOf c "ptr"
        let lThen = freshLbl c "arrif.then"
        let lElse = freshLbl c "arrif.else"
        let lEnd = freshLbl c "arrif.end"
        let poolOf (x: ArrVal) =
            match x.Src with
            | APool p -> p
            | _ -> refuse "an unmaterialized array branch of an if-expression"
        ln c (renderBr { Cond = cv.Reg; IfTrue = lThen; IfFalse = lElse })
        lbl c lThen
        let a = materialize c (emitArr c tb)
        ln c ($"store ptr {(poolOf a)}, ptr {slot}")
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lElse
        let b = materialize c (emitArr c fb)
        if b.Elem <> a.Elem || b.Groups <> a.Groups then
            refuse "an if-expression whose branches are arrays of different static shapes"
        ln c ($"store ptr {(poolOf b)}, ptr {slot}")
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lEnd
        let p = freshReg c
        ln c ($"{p} = load ptr, ptr {slot}")
        { a with Src = APool p }

    // Elementwise `<|>`: keep the left cell when it is non-zero, else the
    // right. Value-keyed, not allocation-keyed -- that distinction is what
    // separates `<|>` from `<|:>` (which stays refused).
    | IRChoice (l, r) ->
        let a = emitArr c l
        let b = emitArr c r
        if a.Groups <> b.Groups then refuse "a choice between arrays of different static shapes"
        { a with Src = AVirt (fun c2 idxs ->
                    let lv = readCell c2 a idxs
                    let nz = coerce c2 ScBool lv
                    let slot = allocSlot c2 a.Elem
                    let lKeep = freshLbl c2 "cchoice.keep"
                    let lElse = freshLbl c2 "cchoice.else"
                    let lEnd = freshLbl c2 "cchoice.end"
                    ln c2 (renderBr { Cond = nz.Reg; IfTrue = lKeep; IfFalse = lElse })
                    lbl c2 lKeep
                    storeSlot c2 slot a.Elem lv
                    ln c2 (sprintf "br label %%%s" lEnd)
                    lbl c2 lElse
                    storeSlot c2 slot a.Elem (readCell c2 b idxs)
                    ln c2 (sprintf "br label %%%s" lEnd)
                    lbl c2 lEnd
                    match slot with
                    | Some s -> loadSlot c2 s a.Elem
                    | None -> refuse "a choice over unit cells") }

    | IRZip _ -> refuse "zip(...) outside a method_for (a bare zip has no materialized shape)"

    | other ->
        refuse ($"the IR node {(caseName other)} in array position -- no arm in the llvm lane")

and private addI64 (c: Ctx) (a: Val) (b: Val) : Val =
    if b.Reg = "0" then a
    else
        let d = freshReg c
        ln c (renderBin { Dest = d; Opcode = "add"; Flags = ""; Ty = ScI64; Lhs = a.Reg; Rhs = b.Reg })
        { Reg = d; Ty = ScI64 }

and private staticExtentOf (ix: IRIndexType) (what: string) : int64 =
    if ix.IxKind <> IxKPlain || ix.Symmetry <> SymNone || ix.Rank <> 1 then
        refuse ($"a {what} over a non-dense index type")
    match Blade.IRPrint.tryEvalIntIR ix.Extent with
    | Some n -> n
    | None -> refuse ($"a {what} over a runtime extent")

/// A RAGGED literal. Row lengths come from the LITERAL'S OWN STRUCTURE,
/// exactly as the C++ lane's computeRaggedRowLengths reads them -- the closed
/// annotation's lens binding is decorative in both lanes today. One flat pool
/// (no row table), one constant offsets global.
and private emitRaggedLit (c: Ctx) (elems: IRExpr list) (elem: Sc) (rows: int64) : ArrVal =
    if int64 (List.length elems) <> rows then
        refuse ($"a ragged literal with {(List.length elems)} rows for a declared outer extent of {rows}")
    let rowElems =
        elems |> List.map (fun e ->
            match e with
            | IRArrayLit (inner, _) -> inner
            | _ -> refuse "a ragged literal whose rows are not row literals")
    let offsets = Array.zeroCreate (List.length rowElems + 1)
    rowElems |> List.iteri (fun i r -> offsets.[i + 1] <- offsets.[i] + int64 (List.length r))
    let total = offsets.[List.length rowElems]
    let sym = raggedTableGlobal c offsets
    let ptr = allocPool c elem (string total)
    rowElems |> List.concat |> List.iteri (fun i x ->
        let v = coerce c elem (emitExpr c x)
        storeCell c elem (gepCell c elem ptr (string i)) v)
    { Elem = elem; Groups = [ GRagged (rows, RtStatic (offsets, sym)) ]
      Src = APool ptr; RowOpBytes = 0L }

/// A literal array. The IR nests one `IRArrayLit` per rank level; the pool is
/// its row-major flattening, which is the declared shape's traversal order.
and private emitArrayLit (c: Ctx) (elems: IRExpr list) (arrType: IRArrayType) : ArrVal =
    match raggedArrShape (Blade.IR.mkArrayLike arrType) with
    | Some (elem, rows) -> emitRaggedLit c elems elem rows
    | None ->
    let (elem, groups) = requireArray "an array literal" (Blade.IR.mkArrayLike arrType)
    let rec flatten (es: IRExpr list) : IRExpr list =
        es |> List.collect (fun x ->
            match x with
            | IRArrayLit (inner, _) -> flatten inner
            | other -> [ other ])
    let flat = flatten elems
    let total = shapeCells groups
    if int64 flat.Length <> total then
        refuse ($"an array literal with {flat.Length} values for a shape of {total} cells")
    let ptr = allocPool c elem (string total)
    flat |> List.iteri (fun i x ->
        let v = coerce c elem (emitExpr c x)
        storeCell c elem (gepCell c elem ptr (string i)) v)
    { Elem = elem; Groups = groups; Src = APool ptr; RowOpBytes = 0L }

/// Run a producer into a fresh pool. The ONLY place an array acquires
/// storage in this lane, and therefore the one place the iteration strategy
/// for a shape is chosen. Four shapes, four strategies:
///
///   * already a pool                -> nothing to do;
///   * FLAT producer (any shape)     -> one counted loop over the pool. This
///     is the elementwise path the blocked-simplex plan tells us to leave
///     alone (section 6): zero coordinate arithmetic, whatever the shape;
///   * one rank-2 compact group      -> the BLOCKED SIMPLEX nest. A map's
///     cells are distinct and its writes independent, so brick order is free
///     and needs no licence (section 7, "Maps: brick order free");
///   * anything else                 -> the shape's plain nest.
and private materialize (c: Ctx) (a: ArrVal) : ArrVal =
    match a.Src with
    | APool _ -> a
    | AVirtFlat f ->
        let total = shapeCellsOp c a.Groups
        let ptr = allocPool c a.Elem total
        emitCountedLoopTo c total (fun k ->
            let v = coerce c a.Elem (f c { Reg = k; Ty = ScI64 })
            storeCell c a.Elem (gepCell c a.Elem ptr k) v)
        { a with Src = APool ptr }
    | AVirt _ when hasRagged a.Groups ->
        // A coordinate producer over the ragged pair: the nest is the ragged
        // walk itself -- outer rows, inner bound off the table, pool offset
        // one add past the row base.
        (match a.Groups with
         | [ GRagged (rows, table) ] ->
             let total = shapeCellsOp c a.Groups
             let ptr = allocPool c a.Elem total
             emitCountedLoop c rows (fun g ->
                 let off = raggedOffAt c table g
                 emitCountedLoopTo c (raggedLenAt c table g) (fun j ->
                     let v = coerce c a.Elem (readCell c a [ { Reg = g; Ty = ScI64 }; { Reg = j; Ty = ScI64 } ])
                     storeCell c a.Elem (gepCell c a.Elem ptr (i64Add c off j)) v))
             { a with Src = APool ptr }
         | _ -> refuse "a coordinate producer over a mixed ragged shape")
    | AVirt _ when isDenseShape a.Groups ->
        let total = shapeCells a.Groups
        let ptr = allocPool c a.Elem (string total)
        emitNest c a.Extents (fun idxs ->
            let v = coerce c a.Elem (readCell c a idxs)
            storeCell c a.Elem (gepCell c a.Elem ptr (storageOffset c a.Groups idxs)) v)
        { a with Src = APool ptr }
    | AVirt _ ->
        let total = shapeCells a.Groups
        let ptr = allocPool c a.Elem (string total)
        (match soleSimplexR a.Groups with
         | Some (r, n, strict) ->
             // A MAP's cells are independent, so the brick order is free (no
             // licence question); the tile edge is the reuse policy's call.
             // Blocking is defined at rank 2 only, so rank 3+ takes `None` and
             // runs the serial simplex rather than refusing a shape it can
             // perfectly well emit.
             let tile = if r = 2 then brickTileEdge true a.RowOpBytes n else None
             emitSimplexR c r n strict tile (fun emit -> emit ())
                 (fun coords off ->
                     let v = coerce c a.Elem (readCell c a coords)
                     storeCell c a.Elem (gepCell c a.Elem ptr off.Reg) v)
         | None ->
             emitShapeNest c a.Groups (fun idxs off ->
                 let v = coerce c a.Elem (readCell c a idxs)
                 storeCell c a.Elem (gepCell c a.Elem ptr off.Reg) v))
        { a with Src = APool ptr }

/// Materialize an array EXPRESSION in place: if it names a binding, the
/// binding's slot is upgraded to the pool, so later reads share the storage
/// instead of re-running the kernel -- and so a `mut` callee writing through
/// the pointer is visible to the caller.
and private materializeExpr (c: Ctx) (e: IRExpr) : ArrVal =
    let a = materialize c (emitArr c e)
    (match e with
     | IRVar (id, _) when c.ArrSlots.ContainsKey id ->
         c.ArrSlots.[id] <- a
         markForced c id
         // The upgraded binding may OUTLIVE an open tracking scope: it
         // escapes any scope it predates (its later reads happen after that
         // scope's exit frees). A binding born inside the scope dies with it,
         // so its pool stays freeable.
         (match a.Src with
          | APool p when c.PoolScopes |> Seq.exists (fun s -> s.KnownIds.Contains id) ->
              keepPool c p
          | _ -> ())
     | _ -> ())
    a

/// Record that binding `id` ends the program as a real array -- AND that every
/// deferred array binding its value names does too.
///
/// The recursion is the whole point, and it is the C++ lane's: materializing
/// `k2 = method_for(llsq, llsq) <@> (+)` runs `forceDeferredArrayInput` over
/// `llsq`, which materializes `llsq` under its own name, which forces `ll` in
/// turn -- so all three join the auto-print list even though the program only
/// ever asks for a scalar out of `k2` (corpus loops, "Deferred Binding Forced
/// On Read"). Marking is enough here: a producer prints by re-running, so the
/// llvm lane needs the PRINTABILITY fact, not a second pool.
///
/// A LOOP BODY IS PRUNED, and that boundary is the C++ lane's `ctx.Indent = 0`
/// guard in `forceDeferredArrayInput`: a producer materialized inside a loop
/// body becomes a per-iteration LOCAL there, block-scoped and deliberately
/// left out of the printable set, so a name this value merely MENTIONS under a
/// loop is no evidence that the binding ends the program materialized. A
/// `let rec`'s whole recursion is one such body, which is how a producer forced
/// per step (`let f = dn |> compute` inside a step block, corpus memfree/007)
/// used to reach this list through the recursion above -- `traj` names it, so
/// forcing `traj` printed `dn`, a line the C++ lane does not emit.
///
/// Pruning loses nothing, because the reads that DO print from a loop body
/// never travel this edge: the C++ lane hoists them out of the loop with
/// `forceDeferredPositionalReads` (at the enclosing indent, hence printable)
/// and `forceLoopBodyProducers` mirrors that hoist by calling this function
/// directly on each one.
and private markForced (c: Ctx) (id: IRId) : unit =
    if c.Forced.Add id then
        match Map.tryFind id c.BindingValues with
        | None -> ()
        | Some v ->
            // `iterIRExpr` with one subtree withheld. Recursion is the shared
            // `ExprShape` fold, exactly as there, so no variant can be skipped
            // by accident -- only the loop body named here.
            let rec walk (e: IRExpr) =
                match e with
                | IRForRange (_, lo, hi, _) -> walk lo; walk hi
                | ExprShape (children, _) ->
                    children |> List.iter walk
                    match e with
                    | IRVar (src, _) when src <> id && c.ArrSlots.ContainsKey src -> markForced c src
                    | _ -> ()
            walk v

// ---------------------------------------------------------------------------
// Kernels and loop nests
// ---------------------------------------------------------------------------

/// Bind one kernel parameter, coercing to its DECLARED type. The declared
/// type is what decides the arithmetic below it: a `range<I>` level hands the
/// kernel an i64 induction value, and a kernel declared over `Float64`
/// converts it exactly where the C++ lane's implicit conversion would.
and private bindKernelParam (c: Ctx) (p: IRParam) (arg: KArg) : unit =
    match arg with
    | KArray a ->
        (match arrayShapeOf p.Type with
         | Some (e, gs) when e = a.Elem && gs = a.Groups -> ()
         | Some _ ->
             refuse ($"kernel parameter '{p.Name}' whose declared shape disagrees with the peeled row")
         | None ->
             // A rank-polymorphic kernel param can be declared T^k and still
             // arrive as a row; accept whatever the peel produced.
             ())
        bindArray c p.VarId p.Name a
    | KScalar v ->
        let sc = match scalarTyOf p.Type with Some s -> s | None -> v.Ty
        bindScalar c p.VarId p.Name (coerce c sc v)

/// Inline a kernel at a call site: bind its parameters, emit its body. Kernels
/// are inlined rather than outlined because they routinely CAPTURE enclosing
/// bindings, and a capture list is a closure ABI this lane deliberately does
/// not have -- a capture reference resolves through the ordinary slot tables
/// instead, since IR ids are program-unique.
and private applyKernel (c: Ctx) (cl: IRCallable) (args: KArg list) : Val =
    if cl.IsCudaKernel || cl.IsMpiParallel then
        refuse ($"the kernel '{cl.Name}', which carries a cuda/mpi strategy")
    if List.length cl.Params <> List.length args then
        refuse ($"a kernel '{cl.Name}' applied to {(List.length args)} of its {(List.length cl.Params)} parameters")
    List.iter2 (bindKernelParam c) cl.Params args
    emitExpr c cl.Body

/// A `method_for(...) <@> kernel` map, as a PRODUCER.
///
/// The loop structure comes from `buildLoopNestCodeGen` -- the same IR-level
/// builder the host, CUDA and fold emitters consume -- so outer-product versus
/// co-iteration, per-array level ownership, virtual-source peeling and kernel
/// parameter assignment are decided in one place for every back end. What this
/// lane refuses is everything the builder describes but the flat pool cannot
/// express: triangular bounds (`BoundDependencies` / `StrictOffset`), fused
/// joint levels, runtime extents.
and private applyToArr (c: Ctx) (info: ApplyInfo) : ArrVal =
    let rk =
        match resolveKernel info.Kernel with
        | Some rk -> rk
        | None -> refuse "an apply-combinator whose kernel does not resolve to a callable"
    // A RAGGED operand or output takes its own peel BEFORE the shared loop
    // builder runs: buildLoopNestCodeGen has no ragged story (the C++ lane
    // routes these through tryRaggedPeel for the same reason), and an
    // exception from a shared layer here would escape tryEmitProgram.
    let typeHasRaggedRow (t: IRType) =
        match Blade.IR.stripUnits t with
        | ArrayElem arr ->
            arr.IndexTypes |> List.exists (fun ix ->
                isRaggedRowKind ix.IxKind || ix.IxKind = IxKGroupOuter)
        | _ -> false
    if info.Arrays |> List.exists (fun a -> typeHasRaggedRow (typeOf a))
       || typeHasRaggedRow info.OutputType then
        applyRaggedPeel c info rk.Callable
    else
    // A WREATH (OrbIdx depth >= 2) pool is refused BEFORE the shared loop
    // builder runs, not after: `buildLoopNestCodeGen` reaches `buildSymmVec`,
    // which `failwith`s on a wreath class rather than returning. A refusal
    // this lane raises itself falls back to the C++ lane; an exception from a
    // shared layer escapes `tryEmitProgram` and takes the harness with it.
    (match Blade.IR.stripUnits info.OutputType with
     | ArrayElem arr when arr.IndexTypes |> List.exists (fun ix -> ix.Symmetry = SymWreath) ->
         refuse "an OrbIdx (iterated-wreath) output pool"
     | _ -> ())
    let cg =
        buildLoopNestCodeGen info
            (info.Arrays |> List.mapi (fun i _ -> $"__a{i}"))
            "__out" (IRBuilder())
    if List.isEmpty cg.Bindings then refuse "a combinator that produced no loop levels"
    for b in cg.Bindings do
        if b.FusedRank.IsSome then refuse "a fused joint loop level (compound index domain)"
    let extents =
        cg.Bindings |> List.map (fun b ->
            match Blade.IRPrint.tryEvalIntIR b.Extent with
            | Some n -> n
            | None -> refuse "a loop level with a runtime extent (the llvm lane bakes extents)")
    // THE OUTPUT'S STORAGE CLASS comes from the type the checker deduced, not
    // from the loop shape: a triangular NEST and a compact POOL are separate
    // facts (two different arrays in commuting positions iterate triangularly
    // and store densely -- the refusal the symmetry corpus pins). A symmetric
    // output type therefore decides the pool; anything without one keeps the
    // dense row-major shape the loop levels describe, which is what makes this
    // change invisible to every dense program.
    let outGroups =
        match Blade.IR.stripUnits info.OutputType with
        | ArrayElem arr when arr.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone) ->
            let (_, groups) =
                requireArray "a combinator whose output storage is compact" info.OutputType
            if axisExtents groups <> extents then
                refuse "a compact output whose group extents disagree with the loop levels"
            groups
        | _ -> denseGroups extents
    // A triangular level is now emitted, not refused -- but ONLY when the
    // output is the compact pool that level is packing into. A triangular nest
    // writing a dense pool would leave the mirror half of it untouched
    // (uninitialized memory printed as values), so it stays a refusal.
    for b in cg.Bindings do
        if (not (List.isEmpty b.BoundDependencies) || b.StrictOffset <> 0)
           && isDenseShape outGroups then
            refuse "a triangular loop level over dense output storage"
    // The produced element type is the KERNEL's return type: a comparison
    // kernel consumes doubles and produces bools, and a broadcast kernel may
    // promote, exactly as genObjectForApplication reads it.
    let elem =
        match scalarTyOf rk.Callable.RetType with
        | Some s when s <> ScVoid -> s
        | _ ->
            match Blade.IR.stripUnits info.OutputType with
            | ArrayElem arr -> requireScalar "a combinator's element" arr.ElemType
            | t -> requireScalar "a combinator's result" t
    // Operand arrays are emitted ONCE, here -- not per cell. Virtual sources
    // (`range`, `reverse`) never become arrays at all: their levels peel as
    // induction values below.
    let operands =
        info.Arrays |> List.map (fun a ->
            match a with
            | IRRange _ | IRVirtualReverse _ -> None
            | _ -> Some (emitArr c a))
    // Level -> which array positions peel there, and with which element
    // descriptor. Outer product gives one position per level; co-iteration
    // gives every position at every level.
    let levelsOf (pos: int) =
        cg.Bindings
        |> List.mapi (fun li b -> (li, b))
        |> List.collect (fun (li, b) ->
            b.Elements |> List.filter (fun el -> el.ArrayPosition = pos) |> List.map (fun el -> (li, el)))
    let positions = cg.Bindings |> List.collect (fun b -> b.Elements |> List.map (_.ArrayPosition)) |> List.distinct
    let paramOf (varId: IRId) =
        cg.KernelParams |> List.tryFind (fun p -> p.VarId = varId)
    // A real array's subscript at a triangular level is the ABSOLUTE
    // coordinate: the loop counter is left-justified into its shrinking row,
    // so the array index is `counter + the prior levels it is measured from +
    // the strict step`. This is `genElementBindingPeel`'s case (1) verbatim
    // (CodeGenLoopNest.fs), and the two lanes must agree cell for cell.
    let absoluteAt (c2: Ctx) (li: int) (idxs: Val list) : Val =
        let b = cg.Bindings.[li]
        let mutable acc = idxs.[li]
        for d in b.BoundDependencies do
            acc <- addI64 c2 acc idxs.[d]
        if b.StrictOffset > 0 then addI64 c2 acc { Reg = string b.StrictOffset; Ty = ScI64 } else acc
    // The REYNOLDS term plan, computed once (it depends only on the kernel's
    // shape, not on the cell). Shared with the C++ lane down to the canonical
    // key -- `ReynoldsCore.reynoldsTermPlan` over `CodeGenLoopNest.canonicalKey`
    // is literally the same enumeration, dedup and ordering, so the two lanes
    // sum the same terms in the same order and their float bits agree.
    let reynoldsPlan =
        if not cg.HasReynolds then None
        else
            let n = List.length cg.KernelParams
            if n < 2 then None
            else
                let permNames (perm: int list) =
                    cg.KernelParams
                    |> List.mapi (fun i p -> (p.VarId, $"__perm{perm.[i]}"))
                    |> List.fold (fun acc (vid, nm) -> Map.add vid nm acc) Map.empty
                Some (Blade.ReynoldsCore.reynoldsTermPlan n cg.IsAntisymmetric
                        (fun perm -> Blade.CodeGenLoopNest.canonicalKey (permNames perm) cg.KernelExpr))
    // THE FLAT-ELEMENTWISE CASE (plan section 6, row 1: "already optimal --
    // do not touch this path"). When every operand is a real array of the
    // OUTPUT'S OWN SHAPE, fully peeled to scalars, with no virtual source and
    // no Reynolds permutation, the kernel sees cell values and nothing else:
    // cell k of the result is the kernel of cell k of each operand, and the
    // whole triangular apparatus -- row bases, shrinking bounds, block
    // decomposition -- is exactly the wasted motion the plan says not to
    // spend. A Reynolds body is excluded by name: it reads PERMUTED
    // coordinates, which is the opposite of index-free.
    let flatEligible =
        not (isDenseShape outGroups)
        && reynoldsPlan.IsNone
        && (operands |> List.forall Option.isSome)
        && (operands |> List.forall (fun o -> (Option.get o).Groups = outGroups))
        && (positions |> List.forall (fun pos ->
                let mine = levelsOf pos
                let a = Option.get (List.item pos operands)
                mine |> List.forall (fun (_, el) -> el.Virtual = RealArray)
                && List.length mine = List.length a.Extents))
    if flatEligible then
        { Elem = elem; Groups = outGroups
          RowOpBytes = 0L
          Src = AVirtFlat (fun c2 k ->
            for pos in positions do
                let a = Option.get (List.item pos operands)
                let mine = levelsOf pos
                match mine with
                | (_, el0) :: _ ->
                    (match paramOf el0.ParamVarId with
                     | Some p -> bindKernelParam c2 p (KScalar (readFlat c2 a k))
                     | None -> refuse "an array level with no kernel parameter")
                | [] -> ()
            coerce c2 elem (emitExpr c2 cg.KernelExpr)) }
    else
    // The reuse hint (plan-simplex-blocked-compute.md section 0, third block):
    // bytes of the largest operand a kernel parameter binds as a ROW VIEW in
    // the closure below. The predicate mirrors the KArray(rowView ...) site's
    // own test -- fewer peeled levels than the operand has extents -- and is
    // evaluated ONCE at construction, never per cell.
    let rowOpBytes =
        positions
        |> List.choose (fun pos ->
            match List.item pos operands with
            | Some a when List.length (levelsOf pos) < List.length a.Extents ->
                Some (shapeCells a.Groups * int64 (poolElemBytes a.Elem))
            | _ -> None)
        |> function [] -> 0L | xs -> List.max xs
    { Elem = elem; Groups = outGroups
      RowOpBytes = rowOpBytes
      Src = AVirt (fun c2 idxs ->
        // Every kernel parameter's argument, in parameter order, so a Reynolds
        // sum can re-bind them under a permutation without re-reading a cell.
        let bound = Dictionary<IRId, KArg>()
        let bind (p: IRParam) (arg: KArg) =
            bound.[p.VarId] <- arg
            bindKernelParam c2 p arg
        for pos in positions do
            let mine = levelsOf pos
            match mine with
            | [] -> ()
            | (_, el0) :: _ ->
                match el0.Virtual with
                // VIRTUAL SOURCES BIND PER SLOT, and the offset is per slot
                // too: `range<halo<Lat, ..>, Lon>` gives the halo slot the
                // window's start offset and the plain slot none. Reading el0's
                // offset for every level of the source shifts the plain axis
                // by the halo's interior shrink -- the 2-D separable stencil
                // then reads one column over, silently.
                | VirtualRange _ | VirtualReverse ->
                    for (li, el) in mine do
                        let v =
                            match el.Virtual with
                            | VirtualRange offset ->
                                let off =
                                    match offset with
                                    | Some o -> Blade.IRPrint.tryEvalIntIR o |> Option.defaultValue 0L
                                    | None -> 0L
                                addI64 c2 idxs.[li] { Reg = string off; Ty = ScI64 }
                            | VirtualReverse ->
                                let d = freshReg c2
                                ln c2 (renderBin { Dest = d; Opcode = "sub"; Flags = ""; Ty = ScI64
                                                   Lhs = string (extents.[li] - 1L); Rhs = idxs.[li].Reg })
                                { Reg = d; Ty = ScI64 }
                            | RealArray -> refuse "a mixed virtual/real source at one array position"
                        match paramOf el.ParamVarId with
                        | Some p -> bind p (KScalar v)
                        | None -> refuse "a virtual level with no kernel parameter"
                | RealArray ->
                    let a =
                        match List.item pos operands with
                        | Some a -> a
                        | None -> refuse "a real-array level over a virtual source"
                    // WHICH COORDINATE SYSTEM this operand wants. A DENSE
                    // array is read at the absolute coordinate; a COMPACT one
                    // is read at its STORAGE coordinate, which is the raw
                    // left-justified loop counter -- adding the dependency
                    // back would walk off the shrinking row. This is the same
                    // split `genElementBindingPeel` makes between its
                    // "original array" and "already-sliced sub-array" cases:
                    // peeling a compact pool row by row leaves the inner
                    // subscript local, and reading the pool in one step (as
                    // this lane does) means never shifting it in the first
                    // place.
                    let rawAxis =
                        [ for g in a.Groups do
                            for k in 0 .. grpRank g - 1 -> (grpCompact g && k > 0) ]
                    let myIdxs =
                        mine |> List.mapi (fun d (li, _) ->
                            if d < rawAxis.Length && rawAxis.[d] then idxs.[li]
                            else absoluteAt c2 li idxs)
                    let arg =
                        if List.length myIdxs = List.length a.Extents then KScalar (readCell c2 a myIdxs)
                        elif List.length myIdxs < List.length a.Extents then KArray (rowView c2 a myIdxs)
                        else refuse "a loop nest that peels more levels than the array has"
                    match paramOf el0.ParamVarId with
                    | Some p -> bind p arg
                    | None -> refuse "an array level with no kernel parameter"
        match reynoldsPlan with
        | None -> coerce c2 elem (emitExpr c2 cg.KernelExpr)
        | Some plan ->
            // sum_perm coeff * kernel(args permuted), left-associated in the
            // plan's first-occurrence order -- and, when the plan is empty,
            // ZERO: antisymmetrizing a symmetric kernel cancels completely,
            // and the C++ lane spells that `0.0` too.
            let args =
                cg.KernelParams
                |> List.map (fun p ->
                    match bound.TryGetValue p.VarId with
                    | true, a -> a
                    | _ -> refuse "a reynolds kernel whose parameters are not all bound by the nest")
            let termOf (perm: int list) =
                List.iteri (fun i (p: IRParam) -> bindKernelParam c2 p args.[perm.[i]]) cg.KernelParams
                coerce c2 elem (emitExpr c2 cg.KernelExpr)
            let scaled (coeff: int) (v: Val) =
                if coeff = 1 then v
                else
                    match elem with
                    | ScF64 ->
                        let d = freshReg c2
                        ln c2 (renderBin { Dest = d; Opcode = "fmul"; Flags = fmfFor c2 ScF64; Ty = ScF64
                                           Lhs = f64Const (float coeff); Rhs = v.Reg })
                        { Reg = d; Ty = ScF64 }
                    | ScI64 -> { Reg = i64Bin c2 "mul" (string coeff) v.Reg; Ty = ScI64 }
                    | _ -> refuse "a reynolds sum over non-numeric elements"
            match plan.Terms with
            | [] -> { Reg = (match elem with ScF64 -> f64Const 0.0 | _ -> "0"); Ty = elem }
            | (c0, p0) :: rest ->
                let mutable acc = scaled c0 (termOf p0)
                for (coeff, perm) in rest do
                    let t = termOf perm
                    // A negative antisymmetric coefficient is a SUBTRACTION,
                    // not an add of a negated term: `a - b`, which is the
                    // expression the C++ lane emits and the same bits.
                    let (opcode, rhs) =
                        if coeff < 0 then
                            ((if elem = ScF64 then "fsub" else "sub"), scaled (abs coeff) t)
                        else ((if elem = ScF64 then "fadd" else "add"), scaled coeff t)
                    let d = freshReg c2
                    ln c2 (renderBin { Dest = d; Opcode = opcode
                                       Flags = (if elem = ScF64 then fmfFor c2 ScF64 else "")
                                       Ty = elem; Lhs = acc.Reg; Rhs = rhs.Reg })
                    acc <- { Reg = d; Ty = elem }
                acc) }

/// The RAGGED PEEL: `method_for(r) <@> kernel` over a ragged pair, dispatched
/// on the OUTPUT type exactly as the C++ lane's tryRaggedPeel dispatches
/// (CodeGenCuda.fs): an output that KEEPS the ragged inner axis is the
/// shape-preserving ELEMENTWISE map (the kernel sees one scalar cell, and the
/// output shares the operand's offsets, so the whole thing is cell-congruent
/// -- the flat path, zero coordinate arithmetic); an output that DROPS it is
/// the CONSUMING form (the kernel sees the row view, answers one scalar per
/// row, and the output is a plain dense vector). Both stay producers;
/// `|> compute` materializes as usual.
and private applyRaggedPeel (c: Ctx) (info: ApplyInfo) (cl: IRCallable) : ArrVal =
    let hasGroupKind (t: IRType) =
        match Blade.IR.stripUnits t with
        | ArrayElem arr ->
            arr.IndexTypes |> List.exists (fun ix ->
                ix.IxKind = IxKGroupOuter || ix.IxKind = IxKGroupMember)
        | _ -> false
    if hasGroupKind info.OutputType
       || (info.Arrays |> List.exists (fun a -> hasGroupKind (typeOf a))) then
        refuse "a group_by result in a combinator (grouped shapes are not in the llvm lane yet)"
    match info.Arrays, cl.Params with
    | [ arrExpr ], [ _ ] ->
        // The operand rule is emitReduce's three-way rule: a multi-read name
        // forces so readers share one pool; a sole-read module binding is
        // consumed but marked forced so it still prints; anything else is
        // consumed where it stands.
        let a =
            match arrExpr with
            | IRVar (id, _) when not (c.Facts.ReadOnce.Contains id) -> materializeExpr c arrExpr
            | IRVar (id, _) when c.BindingValues.ContainsKey id ->
                markForced c id
                emitArr c arrExpr
            | _ -> emitArr c arrExpr
        let rows =
            match a.Groups with
            | [ GRagged (rows, _) ] -> rows
            | _ -> refuse "a ragged peel whose operand is not a sole ragged pair"
        let outputRagged =
            match Blade.IR.stripUnits info.OutputType with
            | ArrayElem arr -> arr.IndexTypes |> List.exists (fun ix -> isRaggedRowKind ix.IxKind)
            | _ -> false
        let elem =
            match scalarTyOf cl.RetType with
            | Some s when s <> ScVoid -> s
            | _ ->
                match Blade.IR.stripUnits info.OutputType with
                | ArrayElem arr -> requireScalar "a ragged combinator's element" arr.ElemType
                | t -> requireScalar "a ragged combinator's result" t
        if outputRagged then
            if not (hasFlatRead a) then
                refuse "an elementwise ragged map over a coordinate-bearing producer"
            { Elem = elem; Groups = a.Groups; RowOpBytes = 0L
              Src = AVirtFlat (fun c2 k ->
                        coerce c2 elem (applyKernel c2 cl [ KScalar (readFlat c2 a k) ])) }
        else
            { Elem = elem; Groups = [ GDense rows ]; RowOpBytes = 0L
              Src = AVirt (fun c2 idxs ->
                        match idxs with
                        | [ g ] -> coerce c2 elem (applyKernel c2 cl [ KArray (rowView c2 a [ g ]) ])
                        | _ -> refuse "a ragged peel read at other than its row coordinate") }
    | arrays, ps ->
        refuse ($"a ragged combinator over {(List.length arrays)} arrays and {(List.length ps)} kernel parameters -- mixing ragged operands with other arrays or multi-parameter kernels is not supported")

/// `object_for(k)` applied directly to arrays -- the shape `A + B`, `A * 2.0`
/// and `A [+] B` lower to (Lowering.lowerTypedBinOp). InputRanks says which:
/// `[0; 0]` co-iterates, `[1; 1]` is the outer product, `[0]` is the
/// one-array broadcast whose scalar operand is already baked into the kernel.
and private objectForApply (c: Ctx) (objInfo: ObjectForInfo) (args: IRExpr list) (retType: IRType) : ArrVal =
    let cl =
        match resolveCallable objInfo.Kernel with
        | Some cl -> cl
        | None -> refuse "an object_for whose kernel does not resolve to a callable"
    let arrays =
        match args with
        | [ IRTuple elems ] -> elems
        | _ -> args
    let ops = arrays |> List.map (emitArr c)
    let elem =
        match scalarTyOf cl.RetType with
        | Some s when s <> ScVoid -> s
        | _ ->
            match Blade.IR.stripUnits retType with
            | ArrayElem arr -> requireScalar "an object_for result element" arr.ElemType
            | t -> requireScalar "an object_for result" t
    // CO-ITERATION OVER ONE SHAPE IS CELL-CONGRUENT: cell k of the result is
    // the kernel of cell k of each operand, whatever the shape means. On a
    // compact pool that is the flat-elementwise path -- one counted loop, zero
    // coordinate arithmetic -- and the blocked-simplex plan is explicit that
    // this path is already optimal and must not be bricked (section 6, first
    // row: "do not touch this path"). A dense shape keeps the coordinate
    // producer it always had, so dense emission does not move.
    let congruentFlat (parts: ArrVal list) =
        parts |> List.forall hasFlatRead
        && (parts |> List.exists (fun p ->
                p.Groups |> List.exists (fun g -> grpCompact g || isRaggedGrp g)))
    match objInfo.InputRanks, ops with
    | [ 0; 0 ], [ a; b ] ->
        if a.Groups <> b.Groups then
            refuse "an elementwise operator over arrays of different static shapes"
        if congruentFlat [ a; b ] then
            { Elem = elem; Groups = a.Groups
              RowOpBytes = 0L
              Src = AVirtFlat (fun c2 k ->
                        coerce c2 elem (applyKernel c2 cl [ KScalar (readFlat c2 a k); KScalar (readFlat c2 b k) ])) }
        else
        { Elem = elem; Groups = a.Groups
          RowOpBytes = 0L
          Src = AVirt (fun c2 idxs ->
                    coerce c2 elem (applyKernel c2 cl [ KScalar (readCell c2 a idxs); KScalar (readCell c2 b idxs) ])) }
    | [ 1; 1 ], [ a; b ] ->
        if (a.Groups @ b.Groups) |> List.exists (fun g -> grpCompact g || isRaggedGrp g) then
            refuse "an outer product over compact or ragged storage"
        let ra = a.Rank
        { Elem = elem; Groups = a.Groups @ b.Groups
          RowOpBytes = 0L
          Src = AVirt (fun c2 idxs ->
                    let ia = idxs |> List.truncate ra
                    let ib = idxs |> List.skip ra
                    coerce c2 elem (applyKernel c2 cl [ KScalar (readCell c2 a ia); KScalar (readCell c2 b ib) ])) }
    | [ 0 ], [ a ] ->
        if congruentFlat [ a ] then
            { Elem = elem; Groups = a.Groups
              RowOpBytes = 0L
              Src = AVirtFlat (fun c2 k ->
                        coerce c2 elem (applyKernel c2 cl [ KScalar (readFlat c2 a k) ])) }
        else
        { Elem = elem; Groups = a.Groups
          RowOpBytes = 0L
          Src = AVirt (fun c2 idxs ->
                    coerce c2 elem (applyKernel c2 cl [ KScalar (readCell c2 a idxs) ])) }
    | ranks, _ ->
        refuse (sprintf "an object_for application with input ranks %A over %d arrays" ranks (List.length ops))

// ---------------------------------------------------------------------------
// Reductions
// ---------------------------------------------------------------------------

/// `reduce(A, op[, init])`. ORDER-PRESERVING and SERIAL: seed from element 0
/// (or `init`), then accumulate ASCENDING, left-associated -- the exact walk
/// `Interp/ArrayOps.reduceArray` performs, which is what the differential gate
/// compares against. No reassociation, licensed or not: FMF arrives with the
/// fact layer, not here.
and private emitReduce (c: Ctx) (arrExpr: IRExpr) (kernelExpr: IRExpr) (initExpr: IRExpr option) : Val =
    // The operand is CONSUMED, not forced. `readCell` runs a producer
    // natively -- the header's contract, "reduce over an unforced computation
    // is run the producer inside the fold with no array in between" -- and
    // the fold below reads each cell exactly once, so an anonymous deferred
    // operand (`reduce(x * y, (+))` in a kernel body: the gram fixture's
    // per-evaluation alloc-fill-refold, ~14.5 GB across one run's cells)
    // folds in ONE pass with no temp at all.
    //
    // A NAMED operand still forces, deliberately: `materializeExpr` upgrades
    // the binding's slot so every LATER read shares the pool, and consuming
    // a named producer lazily here would re-run its kernel once per
    // consumer -- the units/065 double-pass, reintroduced backwards. `IRVar`
    // is the naming boundary, so it is the guard.
    let a =
        match arrExpr with
        // A name read MORE than once must force: `materializeExpr` upgrades
        // the slot so every reader shares one pool. A SOLE-READ name -- the
        // IRLift hoist that gives a kernel-body operand its buffer name,
        // whose only reader is this fold -- has nobody to share with, so it
        // is consumed like the anonymous case (`ModuleFacts.ReadOnce`).
        //
        | IRVar (id, _) when not (c.Facts.ReadOnce.Contains id) -> materializeExpr c arrExpr
        // A SOLE-READ MODULE BINDING: consume it, but still mark it forced.
        // The two things `materializeExpr` bundles come apart here. Storage
        // is not wanted -- this fold may be emitted inside a loop nest, and
        // a pool allocated there does not dominate the print that reads it
        // after ("Instruction does not dominate all uses", which is what
        // materializing unconditionally actually produced). PRINTABILITY is
        // wanted: `emitPrints` shows a deferred binding only if `Forced`
        // holds it, so consuming without the mark silently deletes an output
        // line the C++ lane emits (`c = [2, 4, 6, 8]`, corpus loops/193).
        // `markForced` records printability, NOT storage -- the printer
        // re-runs the producer at module scope, where nothing encloses it.
        | IRVar (id, _) when c.BindingValues.ContainsKey id ->
            markForced c id
            emitArr c arrExpr
        | _ -> emitArr c arrExpr
    let cl =
        match resolveCallable kernelExpr with
        | Some cl when cl.Params.Length = 2 -> cl
        | Some cl -> refuse ($"a fold kernel '{cl.Name}' of {cl.Params.Length} parameters")
        | None -> refuse "a reduce whose kernel does not resolve to a callable"
    let accTy =
        match scalarTyOf cl.RetType with
        | Some s when s <> ScVoid -> s
        | _ -> a.Elem
    if a.Groups |> List.exists grpCompact then emitCompactFold c a cl accTy initExpr else
    if a.Rank <> 1 then
        refuse "reduce over a rank-2+ array (partial folds are not in the llvm lane yet)"
    // The bound is an OPERAND: a literal for a static axis (byte-identical to
    // the counted loop this always was), the carried length for a peeled
    // ragged row. An unseeded fold over an empty row is the same UB it is in
    // the C++ lane; only the statically-provable case refuses.
    let bound = soleExtentOp a
    let acc = allocaOf c (llTy accTy)
    let start =
        match initExpr with
        | Some ini ->
            let v = coerce c accTy (emitExpr c ini)
            ln c ($"store {(llTy accTy)} {v.Reg}, ptr {acc}")
            0L
        | None ->
            (match tryLitI64 bound with
             | Some n when n < 1L -> refuse "reduce over a statically empty array"
             | _ -> ())
            let v0 = coerce c accTy (readCell c a [ { Reg = "0"; Ty = ScI64 } ])
            ln c ($"store {(llTy accTy)} {v0.Reg}, ptr {acc}")
            1L
    let licensed = foldFmfDecorable cl
    let trips =
        if start = 0L then bound
        else
            match tryLitI64 bound with
            | Some n -> string (n - start)
            | None -> i64Bin c "sub" bound (string start)
    emitCountedLoopTo c trips (fun k ->
        let i = addI64 c { Reg = k; Ty = ScI64 } { Reg = string start; Ty = ScI64 }
        let cur = loadSlot c acc accTy
        // The cell read is OUTSIDE the licensed scope on purpose: on a deferred
        // operand it runs the map kernel, whose arithmetic carries no fold
        // license (see `withFoldFmf`).
        let cell = readCell c a [ i ]
        let next =
            withFoldFmf c licensed (fun () ->
                coerce c accTy (applyKernel c cl [ KScalar cur; KScalar cell ]))
        ln c ($"store {(llTy accTy)} {next.Reg}, ptr {acc}"))
    loadSlot c acc accTy

/// A fold over a COMPACT (simplex) domain: every canonical cell, once.
///
/// THE BLOCKED-SIMPLEX FOLD (docs/plans/plan-simplex-blocked-compute.md
/// sections 4a and 7). Two forms, and which one is emitted is a LICENSING
/// question, not a performance one:
///
///   * UNLICENSED -- the ordinary serial canonical walk, cells in pool order,
///     left-associated. Byte-identical to what a flat traversal produces,
///     which is what an unlicensed fold is entitled to.
///   * LICENSED (`foldFmfLicensed`: the kernel is `comm`-declared or is a
///     recognized commutative builtin -- the same structural predicate the
///     C++ lane spends on its K-lane forms and its OMP chunking -- AND the
///     `BLADE_FP_REASSOC` knob is on, because the brick walk reassociates
///     the RESULT) -- the brick decomposition, with ONE PARTIAL PER BRICK
///     combined into the total in ascending-lex block order. Deterministic-but-reassociated: the same
///     program always produces the same bits, and those bits may differ from
///     the serial walk exactly as far as the declared commutativity says they
///     may. Each brick seeds from its OWN first contributed value (the `have`
///     flag), so no identity element is required and an empty brick
///     contributes nothing -- the discipline `FoldChunkPlan` already uses.
///
/// REACHABILITY, stated plainly: no Blade program can currently deliver a
/// compact operand to a fold. `reduce()` over compact symmetric/antisymmetric
/// storage -- and over a computation whose OUTPUT is compact -- is a FRONT-END
/// refusal at five sites in TypeCheckInfer (BL3999: "folding the canonical
/// cells and folding the logical (mirrored) cells differ ... decompact(A, d)
/// first"), because the surface has never decided which of the two folds
/// `reduce` means. So this arm is the back end holding up its end of a
/// contract the front end has not signed: the nest it lays down is the same
/// `emitSimplex2` the compact MAP path runs on every symmetric corpus file,
/// and only the accumulator plumbing here is unexercised.
and private emitCompactFold (c: Ctx) (a: ArrVal) (cl: IRCallable) (accTy: Sc) (initExpr: IRExpr option) : Val =
    let total = shapeCells a.Groups
    if total < 1L then refuse "a fold over a statically empty compact domain"
    let acc = allocaOf c (llTy accTy)
    // The brick decomposition REASSOCIATES THE RESULT (one partial per brick,
    // combined in block order), so it takes the same two-part gate as FMF:
    // the structural license AND the environment knob. Structure alone must
    // never brick -- with `BLADE_FP_REASSOC` unset the serial canonical walk
    // is the only walk, which is what keeps the byte-identity oracles honest
    // the day BL3999's refusal lifts.
    let brickable = foldFmfLicensed cl
    let fmf = foldFmfDecorable cl
    let combine (dst: string) (cur: Val) (cell: Val) =
        let next = withFoldFmf c fmf (fun () -> coerce c accTy (applyKernel c cl [ KScalar cur; KScalar cell ]))
        ln c ($"store {(llTy accTy)} {next.Reg}, ptr {dst}")
    // The seed. Cell 0 in POOL order is also the first cell in BLOCK order
    // (block (0,0) comes first, and its first row is row 0), so an unseeded
    // fold starts from the same value either way -- and every remaining cell
    // is then folded in, once.
    let seeded =
        match initExpr with
        | Some ini ->
            let v = coerce c accTy (emitExpr c ini)
            ln c ($"store {(llTy accTy)} {v.Reg}, ptr {acc}")
            true
        | None -> false
    let haveTotal = allocaOf c "i1"
    ln c ($"""store i1 {(if seeded then "true" else "false")}, ptr {haveTotal}""")
    let feed (dst: string) (haveFlag: string) (cell: Val) =
        let h = freshReg c
        ln c ($"{h} = load i1, ptr {haveFlag}")
        let lHave = freshLbl c "fold.have"
        let lFirst = freshLbl c "fold.first"
        let lEnd = freshLbl c "fold.done"
        ln c (renderBr { Cond = h; IfTrue = lHave; IfFalse = lFirst })
        lbl c lHave
        combine dst (loadSlot c dst accTy) cell
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lFirst
        let v0 = coerce c accTy cell
        ln c ($"store {(llTy accTy)} {v0.Reg}, ptr {dst}")
        ln c ($"store i1 true, ptr {haveFlag}")
        ln c (sprintf "br label %%%s" lEnd)
        lbl c lEnd
    match soleSimplexR a.Groups with
    | None ->
        // A compact group beside other groups: serial over the whole shape.
        emitShapeNest c a.Groups (fun idxs _ -> feed acc haveTotal (readCell c a idxs))
        loadSlot c acc accTy
    | Some (r, n, strict) ->
        // Folds carry no reuse hint: the canonical fold streams the pool once
        // (nothing to re-read), so bricking stays knob+licence-only here --
        // and the row-bin schedule in plan-compact-sym-folds.md section 5.6
        // is the fold's real future, not bricks.
        match (if r = 2 then brickTileEdge brickable 0L n else None) with
        | None ->
            // Serial canonical order. When the operand can answer a flat read
            // (a pool, or a cell-congruent producer) that order IS pool order,
            // so the walk is one counted loop with no coordinate arithmetic --
            // and that shortcut is rank-independent, which is why a rank-3
            // fold over a materialized pool costs exactly what a rank-2 one
            // does per cell.
            if hasFlatRead a then
                emitCountedLoop c total (fun k -> feed acc haveTotal (readFlat c a { Reg = k; Ty = ScI64 }))
            else
                emitSimplexR c r n strict None (fun emit -> emit ())
                    (fun coords _ -> feed acc haveTotal (readCell c a coords))
            loadSlot c acc accTy
        | Some b ->
            let part = allocaOf c (llTy accTy)
            let havePart = allocaOf c "i1"
            emitSimplexR c r n strict (Some b)
                (fun emitBlock ->
                    ln c ($"store i1 false, ptr {havePart}")
                    emitBlock ()
                    // The brick's partial joins the total HERE, so the combine
                    // order is exactly `SimplexBlocksCore.blockSequence`.
                    let hp = freshReg c
                    ln c ($"{hp} = load i1, ptr {havePart}")
                    let lJoin = freshLbl c "brick.join"
                    let lSkip = freshLbl c "brick.skip"
                    ln c (renderBr { Cond = hp; IfTrue = lJoin; IfFalse = lSkip })
                    lbl c lJoin
                    feed acc haveTotal (loadSlot c part accTy)
                    ln c (sprintf "br label %%%s" lSkip)
                    lbl c lSkip)
                (fun coords _ -> feed part havePart (readCell c a coords))
            loadSlot c acc accTy

/// `reduce(<deferred computation>, op, init)` -- the FUSED terminal. One nest,
/// no materialized array, and one accumulator PER FUSION LEAF: an `<&!>` tree
/// answers a flat pack of scalars, which is how "several statistics in one
/// pass" costs one traversal.
and private emitReduceCompute (c: Ctx) (compExpr: IRExpr) (kernelExpr: IRExpr) (initExpr: IRExpr) : Val list =
    let rec leaves e =
        match e with
        | IRFusion (l, r) -> leaves l @ leaves r
        | IRVar (id, _) when c.ArrSlots.ContainsKey id -> [ e ]
        | other -> [ other ]
    let legs = leaves compExpr
    let kernels =
        match kernelExpr with
        | IRTuple ks -> ks
        | k -> List.replicate legs.Length k
    let seeds =
        match initExpr with
        | IRTuple ss -> ss
        | s -> List.replicate legs.Length s
    if kernels.Length <> legs.Length || seeds.Length <> legs.Length then
        refuse "a fused reduction whose kernel/seed pack does not match its leaf count"
    let arrs = legs |> List.map (emitArr c)
    let shape = (List.head arrs).Groups
    for a in arrs do
        if a.Groups <> shape then refuse "a fused reduction over legs of different shapes"
    let plans =
        List.map3 (fun (a: ArrVal) k s ->
            let cl =
                match resolveCallable k with
                | Some cl when cl.Params.Length = 2 -> cl
                | Some cl -> refuse ($"a fold kernel '{cl.Name}' of {cl.Params.Length} parameters")
                | None -> refuse "a fused reduction whose kernel does not resolve to a callable"
            let ty =
                match scalarTyOf cl.RetType with
                | Some t when t <> ScVoid -> t
                | _ -> a.Elem
            let slot = allocaOf c (llTy ty)
            let seed = coerce c ty (emitExpr c s)
            ln c ($"store {(llTy ty)} {seed.Reg}, ptr {slot}")
            // PER LEG: one leg of a join may be a `comm`-declared kernel and
            // its neighbour an arbitrary one, and they share a nest without
            // sharing a license.
            (a, cl, ty, slot, foldFmfDecorable cl)) arrs kernels seeds
    // A COMPACT domain goes through the blocked-simplex fold, one leg at a
    // time: a fused join shares ONE nest by construction, and the bricked nest
    // is a different nest per leg's licence, so mixing them would either
    // license the unlicensed leg or serialize the licensed one. One leg is the
    // shape every reachable program has anyway.
    if shape |> List.exists grpCompact then
        match plans with
        | [ (a, cl, ty, _, _) ] -> [ emitCompactFold c a cl ty (Some (List.head seeds)) ]
        | _ -> refuse "a fused reduction join over compact (symmetric/antisymmetric) storage"
    else
    emitNest c (axisExtents shape) (fun idxs ->
        for (a, cl, ty, slot, licensed) in plans do
            let cur = loadSlot c slot ty
            let cell = readCell c a idxs
            let next =
                withFoldFmf c licensed (fun () ->
                    coerce c ty (applyKernel c cl [ KScalar cur; KScalar cell ]))
            ln c ($"store {(llTy ty)} {next.Reg}, ptr {slot}"))
    plans |> List.map (fun (_, _, ty, slot, _) -> loadSlot c slot ty)

/// Flatten a pack expression into its components. `<&>` (independent) and
/// `<&!>` (fused) both answer a pack of computations, and both associate --
/// `a <&> b <&> c` is three legs, not a nest -- so the walk flattens.
and private emitTupleParts (c: Ctx) (e: IRExpr) : ValKind list =
    match e with
    | IRTuple elems -> elems |> List.collect (emitTupleParts c)
    | IRParallel (l, r, _) -> emitTupleParts c l @ emitTupleParts c r
    // `<&!>` is MANDATORY fusion: legs whose loop extents differ cannot share
    // a nest, and the compiler must say so rather than quietly emit two
    // independent loops. Refusing here hands the program to the C++ lane,
    // which owns the diagnostic (corpus loops/065 pins it as REJECT-AT
    // codegen).
    | IRFusion (l, r) ->
        let parts = emitTupleParts c l @ emitTupleParts c r
        let shapes = parts |> List.choose (function VArray a -> Some a.Extents | _ -> None) |> List.distinct
        if List.length shapes > 1 then
            refuse "a mandatory fusion (<&!>) whose legs iterate different extents"
        parts
    | IRVar (id, _) when c.TupleSlots.ContainsKey id -> c.TupleSlots.[id]
    | IRReduceCompute (comp, k, i) -> emitReduceCompute c comp k i |> List.map VScalar
    // `|> compute` on a pack forces EVERY leg. The fused spelling differs from
    // the independent one only in how many traversals the C++ lane runs, not
    // in what the legs answer, so both land here.
    | IRCompute inner ->
        emitTupleParts c inner
        |> List.map (fun p -> match p with VArray a -> VArray (materialize c a) | other -> other)
    | IRPure inner -> emitTupleParts c inner
    | IRLet (id, v, body) -> bindLet c id v; emitTupleParts c body
    | IRTupleProj (parent, idx, _) ->
        (match tupleComponent c parent idx with
         | VTuple ps -> ps
         | single -> [ single ])
    // TERMINATION: the base case must NOT hand a pack-typed node back to
    // `classifyValue`, whose `IRTTuple` arm calls straight back here -- that
    // pair loops forever on any pack shape without an arm above (a
    // tuple-returning call has no struct-return ABI in this lane).
    | other ->
        match Blade.IR.stripUnits (typeOf other) with
        | IRTTuple _ ->
            refuse ($"the IR node {(caseName other)} in pack position -- the llvm lane builds packs from tuples, <&>/<&!> legs and reduction joins only")
        | _ -> [ classifyValue c other ]

/// A call whose RESULT is an array. The ABI is the same one every array uses:
/// the callee returns the pool pointer, and the caller reads the shape off the
/// declared return type -- no descriptor struct, per plan section 5.
and private emitArrCall (c: Ctx) (e: IRExpr) : ArrVal =
    match e with
    | IRApp (func, args, retType) ->
        let (elem, groups) = requireArray "an array-returning call" retType
        let v = emitCall c func args
        { Elem = elem; Groups = groups; Src = APool v.Reg; RowOpBytes = 0L }
    | _ -> refuse "a non-call in array-call position"

// ---------------------------------------------------------------------------
// Function and program assembly
// ---------------------------------------------------------------------------

/// How one function parameter crosses the ABI.
type private ParamKind =
    | PScalar of Sc
    | PArray of Sc * Grp list
    /// The ragged pair: pool pointer plus its offsets-table pointer, two
    /// arguments -- the "carry the per-row shape across the boundary as a
    /// separate pointer" ABI, spelled with offsets rather than lens because
    /// offsets are what addressing consumes and offsets[rows] is the total.
    | PRagged of Sc * int64

/// `internal` linkage everywhere but `main`: the module is the whole program
/// (Blade has no separate compilation), so nothing needs to be externally
/// visible and every definition stays available for inlining and DCE.
let private emitFunctionBody (c: Ctx) (cl: IRCallable) (sym: string) : string list =
    c.Body <- ResizeArray()
    c.Allocas <- ResizeArray()
    c.Slots <- Dictionary()
    c.ArrSlots <- Dictionary()
    c.NameSlots <- Dictionary()
    c.NameArrSlots <- Dictionary()
    if not (List.isEmpty cl.Captures) then
        refuse ($"the function '{cl.Name}', which captures enclosing bindings")
    if (raggedArrShape cl.RetType).IsSome then
        refuse ($"'{cl.Name}' returns a ragged array (ragged returns are not in the llvm lane yet)")
    // An array parameter arrives as a bare `ptr` with its shape baked from the
    // declared type -- never a descriptor struct (plan section 5). A `mut`
    // parameter needs no separate treatment: the pointer IS the caller's pool,
    // so an element write already aliases.
    let ps =
        cl.Params
        |> List.mapi (fun i p ->
            match arrayShapeOf p.Type with
            | Some (elem, groups) -> (i, p, PArray (elem, groups))
            | None ->
                match raggedArrShape p.Type with
                | Some (elem, rows) -> (i, p, PRagged (elem, rows))
                | None ->
                    let sc = requireScalar ($"parameter '{p.Name}' of '{cl.Name}'") p.Type
                    if sc = ScVoid then refuse ($"a unit parameter on '{cl.Name}'")
                    (i, p, PScalar sc))
    // Parameter facts. `noundef` on everything (no undef/poison channel exists
    // in this emitter); `readonly` on pointers only when a whole-module scan
    // found no element write anywhere -- see `ModuleFacts.ArraysReadOnly`.
    // NOT `noalias`: two array arguments at one call site may be the SAME
    // pool (`f(a, a)` is ordinary Blade, and array identity is what licenses
    // symmetric storage in the first place), so a parameter is exactly where
    // that claim would be false.
    let arrayParamAttrs =
        if not (factEnabled factParamAttrs) then ""
        elif c.Facts.ArraysReadOnly then " noundef readonly"
        else " noundef"
    let signature =
        ps
        |> List.map (fun (i, _, k) ->
            match k with
            | PScalar sc -> sprintf "%s%s %%a%d" (llTy sc) (paramAttr ()) i
            | PArray _ -> sprintf "ptr%s %%a%d" arrayParamAttrs i
            | PRagged _ -> sprintf "ptr%s %%a%d, ptr%s %%a%dr" arrayParamAttrs i arrayParamAttrs i)
        |> String.concat ", "
    // NOT `noalias` on an array RETURN either: a body whose result is one of
    // its own parameters returns that pointer unchanged (`materialize` of an
    // already-stored array is the identity), so the returned pool is not
    // always fresh.
    let fnAttrs =
        attrRef c
            (if c.Facts.SelfRecursive.Contains cl.Id then grpFnRecursive
             elif c.Facts.AnyRecursion then grpFnNoRecurse
             else grpFnTerminating)
    for (i, p, k) in ps do
        match k with
        | PScalar sc ->
            let slot = allocaOf c (llTy sc)
            ln c (sprintf "store %s %%a%d, ptr %s" (llTy sc) i slot)
            c.Slots.[p.VarId] <- (slot, sc)
            c.NameSlots.[p.Name] <- (slot, sc)
        | PArray (elem, groups) ->
            let a = { Elem = elem; Groups = groups; Src = APool (sprintf "%%a%d" i); RowOpBytes = 0L }
            c.ArrSlots.[p.VarId] <- a
            c.NameArrSlots.[p.Name] <- a
        | PRagged (elem, rows) ->
            let a = { Elem = elem
                      Groups = [ GRagged (rows, RtDynamic (sprintf "%%a%dr" i)) ]
                      Src = APool (sprintf "%%a%d" i); RowOpBytes = 0L }
            c.ArrSlots.[p.VarId] <- a
            c.NameArrSlots.[p.Name] <- a
    // A function body is a tracking scope: its temporaries die at `ret`, and
    // a function cannot capture (refused above), so nothing but the returned
    // pool can outlive the frame. The per-cell call was the arena model's
    // worst edge -- every temp a kernel-position callee materialized leaked
    // once per output cell.
    pushPoolScope c
    match arrayShapeOf cl.RetType with
    | Some (elem, groups) ->
        // An array-returning function hands back the pool pointer. The result
        // must be MATERIALIZED before it leaves: a producer's closure captures
        // this frame's slots, which do not survive the return.
        let result = materialize c (emitArr c cl.Body)
        if result.Elem <> elem || result.Groups <> groups then
            refuse ($"the body of '{cl.Name}' produces a shape its return type does not declare")
        (match result.Src with
         | APool p ->
             // Free the frame's temps, sparing the result. What the result IS
             // decides how much can be freed: a pool this frame allocated
             // (spared by pointer compare -- a branch join may alias it to any
             // tracked pool); a parameter (nothing of this frame escapes); or
             // UNKNOWN provenance -- a loaded slot, a row-view GEP whose
             // backing pool must stay live -- where freeing anything would
             // need an escape argument this emitter does not have, so the
             // scope drops its records and keeps today's arena behavior.
             let tracked =
                 c.PoolScopes.[c.PoolScopes.Count - 1].Pools
                 |> Seq.exists (fun (q, _) -> q = p)
             let isParam =
                 p.StartsWith "%a" && p.Length > 2 && p.[2..] |> Seq.forall System.Char.IsDigit
             if tracked then popPoolScope c (Some p) true
             elif isParam then popPoolScope c None true
             else popPoolScope c None false
             ln c ($"ret ptr {p}")
         | _ -> refuse "an unmaterialized array return")
        [ yield $$"""define internal ptr {{sym}}({{signature}}){{fnAttrs}} {"""
          yield "entry:"
          yield! c.Allocas
          yield! c.Body
          yield "}" ]
    | None ->
    let retTy = requireScalar ($"the return type of '{cl.Name}'") cl.RetType
    let result = emitExpr c cl.Body
    if retTy = ScVoid then
        popPoolScope c None true
        ln c "ret void"
    else
        let result = coerce c retTy result
        popPoolScope c None true
        ln c ($"ret {(llTy retTy)} {result.Reg}")
    [ yield $$"""define internal {{(llTy retTy)}} {{sym}}({{signature}}){{fnAttrs}} {"""
      yield "entry:"
      yield! c.Allocas
      yield! c.Body
      yield "}" ]

/// Merge every module into one, exactly as `genSelfContainedProgramFromIR`
/// does before handing the C++ emitter a single module: ids are program-global,
/// so concatenation preserves both declaration order and every metadata key.
let private mergeModules (modules: IRModule list) : IRModule =
    match modules with
    | [ m ] -> m
    | ms ->
        { Name = "merged"
          Types = ms |> List.collect (_.Types)
          Functions = ms |> List.collect (_.Functions)
          Bindings = ms |> List.collect (_.Bindings)
          StaticFunctionUsage = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.StaticFunctionUsage) Map.empty
          ProviderReads = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderReads) Map.empty
          ProviderWrites = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.ProviderWrites) Map.empty
          RandomInits = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.RandomInits) Map.empty
          CompoundInits = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.CompoundInits) Map.empty
          SparseInits = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.SparseInits) Map.empty
          MutableArrayLets = ms |> List.fold (fun acc m -> Set.union acc m.MutableArrayLets) Set.empty
          DerivedFuncOrigins = ms |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m.DerivedFuncOrigins) Map.empty }

/// Module-level metadata the lane has no lowering for. Checked FIRST, before a
/// single instruction is built, so the refusal names the feature rather than
/// whatever expression happens to trip over it.
let private checkModuleScope (m: IRModule) : unit =
    if not (Map.isEmpty m.ProviderReads) then refuse "a provider read (netcdf/zarr/csv/sql)"
    if not (Map.isEmpty m.ProviderWrites) then refuse "a provider write (netcdf/zarr/csv/sql)"
    if not (Map.isEmpty m.RandomInits) then refuse "a random-fill initializer"
    if not (Map.isEmpty m.CompoundInits) then refuse "a compound-index initializer"
    if not (Map.isEmpty m.SparseInits) then refuse "a sparse-index initializer"
    for td in m.Types do
        match td with
        // Index-type and alias declarations are typecheck-time identity; they
        // carry no storage and reach the back end as nothing at all.
        | IRTDAlias _ | IRTDIndexType _ | IRTDEnumIdx _ -> ()
        | IRTDStruct (name, _) -> refuse ($"the struct type '{name}'")
        | IRTDVariant (name, _) -> refuse ($"the variant type '{name}'")

// ---------------------------------------------------------------------------
// Printing
//
// EVERY corpus EXPECT is a byte pin against what the C++ lane prints, so the
// shapes below are transcriptions of `CodeGen.genPrintNested2` /
// `genPrintArrayFlat`, not re-derivations: ONE regime for every rank -- rank 2
// nests (`[[a, b], [c, d]]`, the shape its literal is written in), every other
// rank prints FLAT as one row-major comma-separated run.
// ---------------------------------------------------------------------------

let private outStr (c: Ctx) (s: string) : unit =
    needShim c "blade_out_str"
    ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_out_str"
                       Args = [ ScStr, stringGlobal c s ] })

let private outVal (c: Ctx) (v: Val) : unit =
    match v.Ty with
    | ScI64 ->
        needShim c "blade_out_i64"
        ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_out_i64"; Args = [ ScI64, v.Reg ] })
    | ScF64 ->
        needShim c "blade_out_f64"
        ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_out_f64"; Args = [ ScF64, v.Reg ] })
    | ScStr ->
        needShim c "blade_out_str"
        ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_out_str"; Args = [ ScStr, v.Reg ] })
    | ScBool ->
        // C has no i1 ABI: the flag widens at the call and the shim prints
        // boolalpha's "true"/"false".
        needShim c "blade_out_bool"
        let w = freshReg c
        ln c ($"{w} = zext i1 {v.Reg} to i32")
        ln c ($"call void @blade_out_bool(i32 {w})")
    | ScVoid -> ()

let private emitIfThen (c: Ctx) (cond: string) (thenPart: unit -> unit) : unit =
    let lThen = freshLbl c "when.then"
    let lEnd = freshLbl c "when.end"
    ln c (renderBr { Cond = cond; IfTrue = lThen; IfFalse = lEnd })
    lbl c lThen
    thenPart ()
    ln c (sprintf "br label %%%s" lEnd)
    lbl c lEnd

/// `if (!first) cout << ", "; first = false;` -- the separator discipline both
/// C++ printers use, so a leading element never gets a comma.
let private emitSeparator (c: Ctx) (flag: string) : unit =
    let f = freshReg c
    ln c ($"{f} = load i1, ptr {flag}")
    let nf = freshReg c
    ln c (renderBin { Dest = nf; Opcode = "xor"; Flags = ""; Ty = ScBool; Lhs = f; Rhs = "true" })
    emitIfThen c nf (fun () -> outStr c ", ")
    ln c ($"store i1 false, ptr {flag}")

/// The C++ lane's `genPrintArraySymAware` in LLVM: ONE regime for every rank
/// (rank 2 nests, everything else runs flat), with the per-axis bound of a
/// compact group SHRUNK by the prior axes of its own group -- `extents[1] - i`
/// symmetric, `- i - 1` strict. The inner loop variable is the STORAGE
/// coordinate (the left-justified slot), which is exactly what `arr[i][j]`
/// indexes on the C++ side; that is what makes a printed triangle the same
/// bytes in both lanes, which every symmetric corpus EXPECT depends on.
let private emitArrayPrintGeneric (c: Ctx) (label: string) (a: ArrVal) : unit =
    let rank = a.Rank
    /// The literal bound of axis `d` given the loop registers already open.
    /// Only an axis INSIDE a compact group has a dependent bound.
    let axisPlan =
        [ for g in a.Groups do
            for k in 0 .. grpRank g - 1 do
                yield (grpExtent g, (if grpCompact g then k else 0), grpStrict g) ]
    let boundOf (d: int) (prior: string list) : string =
        let (ext, groupPos, strict) = axisPlan.[d]
        if groupPos = 0 then string ext
        else
            // extent - (the group's prior storage coordinates) - strict*pos.
            let baseVal = ext - (if strict then int64 groupPos else 0L)
            let mutable acc = string baseVal
            for p in prior |> List.skip (d - groupPos) do
                acc <- i64Bin c "sub" acc p
            acc
    outStr c (label + " = [")
    if rank = 2 then
        emitCountedLoop c a.Extents.[0] (fun i ->
            let t = freshReg c
            ln c (renderCmp { Dest = t; Kind = "icmp"; Pred = "ne"; Ty = ScI64; Lhs = i; Rhs = "0" })
            emitIfThen c t (fun () -> outStr c ", ")
            outStr c "["
            let flag = allocaOf c "i1"
            ln c ($"store i1 true, ptr {flag}")
            emitCountedLoopTo c (boundOf 1 [ i ]) (fun j ->
                emitSeparator c flag
                outVal c (readCell c a [ { Reg = i; Ty = ScI64 }; { Reg = j; Ty = ScI64 } ]))
            outStr c "]")
    else
        let flag = allocaOf c "i1"
        ln c ($"store i1 true, ptr {flag}")
        let rec go (d: int) (prior: string list) =
            if d = rank then
                emitSeparator c flag
                outVal c (readCell c a (prior |> List.map (fun r -> { Reg = r; Ty = ScI64 })))
            else
                emitCountedLoopTo c (boundOf d prior) (fun i -> go (d + 1) (prior @ [ i ]))
        go 0 []
    outStr c "]\n"

/// The print dispatcher. Ragged shapes carry operand bounds the generic
/// printer's static axis plan cannot express, so they get their own arms --
/// the SAME bytes the C++ lane's Ragged<T> printer produces: rank 2 nests
/// with the per-row bound, a peeled row runs flat.
let private emitArrayPrint (c: Ctx) (label: string) (a: ArrVal) : unit =
    match a.Groups with
    | [ GRagged (rows, table) ] ->
        outStr c (label + " = [")
        emitCountedLoop c rows (fun i ->
            let t = freshReg c
            ln c (renderCmp { Dest = t; Kind = "icmp"; Pred = "ne"; Ty = ScI64; Lhs = i; Rhs = "0" })
            emitIfThen c t (fun () -> outStr c ", ")
            outStr c "["
            let flag = allocaOf c "i1"
            ln c ($"store i1 true, ptr {flag}")
            emitCountedLoopTo c (raggedLenAt c table i) (fun j ->
                emitSeparator c flag
                outVal c (readCell c a [ { Reg = i; Ty = ScI64 }; { Reg = j; Ty = ScI64 } ]))
            outStr c "]")
        outStr c "]\n"
    | [ GDynDense len ] ->
        outStr c (label + " = [")
        let flag = allocaOf c "i1"
        ln c ($"store i1 true, ptr {flag}")
        emitCountedLoopTo c len (fun j ->
            emitSeparator c flag
            outVal c (readCell c a [ { Reg = j; Ty = ScI64 } ]))
        outStr c "]\n"
    | _ -> emitArrayPrintGeneric c label a

/// Which bindings never print, mirroring `CodeGen.computeDeferredIds`: an
/// unforced computation has no materialized array to echo. A binding a
/// consumer LATER forced is a real array by program end and prints like any
/// eager one -- that is what `Ctx.Forced` records.
let private deferredBindingIds (bindings: IRBinding list) : HashSet<IRId> =
    let ids = HashSet<IRId>()
    let isDeferredExpr (e: IRExpr) =
        match e with
        | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRFunctorMap _
        | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _ | IRBind _ | IRZip _
        | IRSequence _ -> true
        | IRVar (id, _) -> ids.Contains id
        | _ -> false
    for b in bindings do
        let resultIsArray = match Blade.IR.stripUnits b.Type with ArrayElem _ -> true | _ -> false
        let deferred =
            match b.Value with
            | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ -> true
            | IRZip _ | IRComposeObj _ | IRFallback _ -> true
            | IRBind (comp, _) -> resultIsArray || isDeferredExpr comp
            | IRComposeMeth (l, r) -> resultIsArray || isDeferredExpr l || isDeferredExpr r
            | IRFunctorMap (_, inner) -> resultIsArray || isDeferredExpr inner
            | IRChoice (l, r) -> resultIsArray || isDeferredExpr l || isDeferredExpr r
            | IRGuard (_, body) -> resultIsArray || isDeferredExpr body
            | IRSequence elems -> resultIsArray || (elems |> List.exists isDeferredExpr)
            | IRTuple elems -> elems |> List.forall isDeferredExpr
            | IRTupleProj (IRVar (pid, _), _, _) -> ids.Contains pid
            | IRVar (srcId, _) -> ids.Contains srcId
            | _ -> false
        if deferred then ids.Add b.Id |> ignore
    ids

/// `|> compute` of a deferred combinator is a forced materialization and
/// always prints; `|> compute` of anything else prints exactly when the
/// wrapped value would. Unmaterialized loop values never print.
let rec private printableValue (v: IRExpr) : bool =
    match v with
    | IRCompute (IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ | IRVar _ | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeMeth _ | IRBind _ | IRGuard _ | IRSequence _) -> true
    | IRCompute inner -> printableValue inner
    | IRMethodFor _ | IRObjectFor _ -> false
    | _ -> true

/// The auto-print pass, mirroring `CodeGen.genPrintStatements`: top-level
/// bindings echo in DECLARATION order, after the timing line, one
/// `<name> = <value>` per printable binding.
let private emitPrints (c: Ctx) (bindings: IRBinding list) : unit =
    let deferred = deferredBindingIds bindings
    // The TYPES genPrintStatements has an arm for. A loop object, a function
    // alias, a tuple pack and a `Computation<T>` wrapper all fall off the end
    // of its match and print nothing -- `let x = pure(42)` echoes no line.
    let printableType (t: IRType) =
        match Blade.IR.stripUnits t with
        | IRTScalar (ETFloat64 | ETFloat32 | ETInt64 | ETInt32 | ETBool | ETComplex64 | ETComplex128 | ETString) -> true
        | IRTIdxTagged _ | IRTNat _ -> true
        | ArrayElem _ -> true
        | _ -> false
    for b in bindings do
        let visible =
            (not (deferred.Contains b.Id) || c.Forced.Contains b.Id)
            && printableValue b.Value && printableType b.Type
        if visible then
            match c.Slots.TryGetValue b.Id with
            | true, (slot, sc) when sc <> ScVoid ->
                let v = loadSlot c slot sc
                let label = stringGlobal c b.Name
                let callPrint fn argTy argReg =
                    needShim c fn
                    ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@" + fn
                                       Args = [ ScStr, label; argTy, argReg ] })
                match sc with
                | ScI64 -> callPrint "blade_print_i64" ScI64 v.Reg
                | ScF64 -> callPrint "blade_print_f64" ScF64 v.Reg
                | ScStr -> callPrint "blade_print_str" ScStr v.Reg
                | ScBool ->
                    // The shim's parameter is a C `int`: C has no i1 ABI, so the
                    // boolean widens at the call and the shim prints boolalpha.
                    needShim c "blade_print_bool"
                    let w = freshReg c
                    ln c ($"{w} = zext i1 {v.Reg} to i32")
                    ln c ($"call void @blade_print_bool(ptr {label}, i32 {w})")
                | ScVoid -> ()
            | _ ->
                match c.ArrSlots.TryGetValue b.Id with
                | true, a -> emitArrayPrint c b.Name a
                | _ -> ()

/// Emit `main`: bindings in declaration order, the `<name> completed in <t>s`
/// timing line, then the auto-print pass -- the exact statement order
/// `genMainWrapper` assembles for the C++ lane, which is what makes the two
/// lanes' stdout comparable line for line.
let private emitMain (c: Ctx) (m: IRModule) (programName: string) : string list =
    c.Body <- ResizeArray()
    c.Allocas <- ResizeArray()
    c.Slots <- Dictionary()
    c.ArrSlots <- Dictionary()
    c.NameSlots <- Dictionary()
    c.NameArrSlots <- Dictionary()
    needShim c "blade_now"
    needShim c "blade_print_completed"
    c.BindingValues <- (m.Bindings |> List.map (fun b -> (b.Id, b.Value)) |> Map.ofList)
    let t0 = freshReg c
    ln c (renderCall { Dest = Some t0; RetTy = ScF64; Callee = "@blade_now"; Args = [] })
    for b in m.Bindings do
        bindNamed c b.Id b.Name b.Type b.Value
    let t1 = freshReg c
    ln c (renderCall { Dest = Some t1; RetTy = ScF64; Callee = "@blade_now"; Args = [] })
    let elapsed = freshReg c
    ln c (renderBin { Dest = elapsed; Opcode = "fsub"; Flags = ""; Ty = ScF64; Lhs = t1; Rhs = t0 })
    let nameG = stringGlobal c programName
    ln c (renderCall { Dest = None; RetTy = ScVoid; Callee = "@blade_print_completed"
                       Args = [ ScStr, nameG; ScF64, elapsed ] })
    emitPrints c m.Bindings
    ln c "ret i32 0"
    // `main` is the one EXTERNAL definition (everything else is `internal`),
    // and nothing calls it, so `norecurse` holds unconditionally; termination
    // is the module-wide question every other function faces.
    let mainAttrs = attrRef c (if c.Facts.AnyRecursion then grpFnNoRecurse else grpFnTerminating)
    [ yield $$"""define i32 @main(){{mainAttrs}} {"""
      yield "entry:"
      yield! c.Allocas
      yield! c.Body
      yield "}" ]

/// Emit the whole program as textual LLVM IR, or refuse it by name.
///
/// `programName` is the label the timing line carries -- the C++ lane uses the
/// source file's stem, so the router passes the same thing.
let tryEmitProgramNamed (programName: string) (program: IRProgram) : Result<string, string> =
    try
        match program.Modules with
        | [] -> Error "an empty program"
        | modules ->
            let m = mergeModules modules
            checkModuleScope m
            let callables = Blade.IRPrint.buildCallablesTableForModule m
            // INSTALL the callables table for the whole emission, and restore
            // the caller's on the way out. `resolveKernel`, `resolveCallable`
            // and `buildLoopNestCodeGen` all read it out of the AsyncLocal
            // analysis context -- without this the loop builder resolves every
            // kernel to None and silently emits an empty nest.
            let prevCtx = Blade.IR.setCallablesContext callables
            try
            // The fact pass runs BEFORE any instruction: `main`'s own attribute
            // group depends on whether a function emitted much later recurses,
            // and its `define` line is finished the moment its body is.
            // Diagnostics escape hatch, off unless asked: the day the reduce
            // fusion landed was spent guessing at IR shapes this dump would
            // have shown in one look. stderr only; emission is unaffected.
            if System.Environment.GetEnvironmentVariable "BLADE_LLVM_DUMP_IR" = "1" then
                let names = Blade.IRPrint.indexNameMap m
                for KeyValue (cid, cl) in callables do
                    eprintfn "[llvm ir-dump] callable %d (%s):" cid cl.Name
                    eprintfn "%s" (Blade.IRPrint.ppIRExprWithNames names 1 cl.Body)
            let c = newCtx callables (computeModuleFacts m callables)
            // main FIRST: it discovers which functions are actually reachable,
            // and the worklist below emits only those. An unreachable helper
            // (a static function already folded away, say) never has to be
            // supported, which is the difference between refusing a program
            // and refusing a declaration nothing calls.
            let mainLines = emitMain c m programName
            while c.Pending.Count > 0 do
                let cl = c.Pending.Dequeue()
                let sym = c.Emitted.[cl.Id]
                c.Funcs.Add(emitFunctionBody c cl sym)
            let sb = StringBuilder()
            let put (s: string) = sb.Append(s).Append('\n') |> ignore
            put "; Blade -- generated by the BLADE_LLVM lane (src/EmitLlvm.fs)."
            put "; No target triple: clang supplies the host's. Fast-math flags"
            put "; appear ONLY on licensed accumulator chains (BLADE_FP_REASSOC)"
            put "; and, on request, as per-instruction `contract`"
            put "; (BLADE_FP_CONTRACT); default emission is order-preserving."
            put ""
            for g in c.Globals do put g
            if c.Globals.Count > 0 then put ""
            for d in c.Externs do put d
            put ""
            for f in c.Funcs do
                for line in f do put line
                put ""
            for line in mainLines do put line
            // Attribute groups last, as clang writes them, and only the ones
            // something referenced.
            if c.UsedAttrGroups.Count > 0 then
                put ""
                for g in c.UsedAttrGroups do
                    put ($$"""attributes #{{g}} = { {{(attrGroupText c.AnyFrees g)}} }""")
            Ok (sb.ToString())
            finally Blade.IR.restoreAnalysisContext prevCtx
    with LlvmRefusal reason -> Error reason

/// The fixed back-end surface (`IRProgram -> Result<string, string>`).
/// Programs reaching this entry are unnamed; the timing line reads "program".
let tryEmitProgram (program: IRProgram) : Result<string, string> =
    tryEmitProgramNamed "program" program

// ---------------------------------------------------------------------------
// Runtime shim deployment
// ---------------------------------------------------------------------------

/// The C runtime the emitted `.ll` links against. Deployed beside the `.ll`
/// exactly as `deployRuntimeHeaders` deploys the C++ headers -- shipped in
/// src/cpp/, copied into the build output by Blade.fsproj, read from disk at
/// emit time.
let shimFileName = "blade_llvm_shim.c"

let shimSourceText () : string =
    Blade.CodeGenLoopNest.readCppRuntimeHeader shimFileName

/// Write the shim next to a generated `.ll`, skipping the write when the
/// destination already holds byte-identical content (same rule, and the same
/// hand-edit-preserving rationale, as deployRuntimeHeaders).
let deployShim (outputDir: string) : unit =
    let dest = System.IO.Path.Combine(outputDir, shimFileName)
    let text = shimSourceText ()
    let current =
        try (if System.IO.File.Exists dest then Some (System.IO.File.ReadAllText dest) else None)
        with _ -> None
    if current <> Some text then
        System.IO.File.WriteAllText(dest, text)
