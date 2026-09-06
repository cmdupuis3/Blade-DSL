// Blade tree-walking interpreter: scalar-subset IR evaluator (Milestone M0).
//
// The differential twin of CodeGen: walks the typed Blade IR and produces the
// same runtime values the emitted C++ computes, so a later printer can make
// the interpreter's output byte-match the compiled binaries. M0 covers the
// scalar core (literals, arithmetic/comparison/logic, complex scalars,
// let/sequence/assign/for-range/if/match with all pattern forms, tuples,
// structs, variant construction+dispatch, function calls, closures with
// captures, recursion, and runtime constraint guards). Array/loop-object/
// reduction/deferred nodes are M2+ and raise InterpUnsupported carrying the
// offending IR case name.
//
// Every semantic decision here was pinned against how CodeGen emits the same
// node, so the interpreter cannot silently drift:
//   * scalar/complex arithmetic delegates wholesale to Numerics (never
//     reimplemented) -- bit-exact promotion/wraparound/complex rules live there;
//   * `&&`/`||` short-circuit (CodeGen.fs:782); IRIf evaluates only the taken
//     branch (ternary, CodeGen.fs:1202);
//   * IRMatch is first-match with guard fall-through, each case in a fresh
//     scope so a partial-then-failed bind doesn't leak (chained
//     `cond ? body : rest`, CodeGen.fs:1749+);
//   * IRForRange evaluates both bounds once, then runs a single reused int64
//     counter lo to hi-1 (EmitCpp.forLoopFrom:26);
//   * IRConstraintCheck panics with the exact code/message/span the C++
//     blade_rt::panic call carries (CodeGen.fs:7166, cpp/blade_runtime.hpp:29).
//
// Compiled inside Blade.fsproj after Interp/RandMirror.fs. Depends on Value.fs
// (value universe + env/limits/panic scaffolding) and Numerics.fs (scalar
// arithmetic). Consumes the concrete IR (Blade.IR): IRExpr (IR.fs:46),
// IRCallable (IR.fs:239), IRPattern (IR.fs:361), IRModule (IR.fs:1755).
module Blade.Interp.Core

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

module N = Blade.Interp.Numerics

// Interpreter state.

/// Raised for any node/feature deferred past M0 (arrays, loop objects,
/// reductions, deferred computations, ...). Carries the IR case name so the
/// caller can report precisely which construct is not yet interpreted.
exception InterpUnsupported of feature: string

/// Control-flow signal for IRBreakIf (the rec-array `while` guard): thrown by
/// the break-if arm, caught ONLY by the enclosing IRForRange arm, which stops
/// iterating -- the twin of the C++ `break` genForRangeBinding emits. Escaping
/// a loop entirely (an IRBreakIf outside any for-range) is a compiler bug and
/// surfaces as an unhandled exception rather than being silently swallowed.
exception InterpBreak

/// Runtime bridge from the scalar Core evaluator down to the M2 loop/array
/// layer (Interp/Loops.fs). Core must stay free of a *compile-time* dependency
/// on Loops, because Loops calls back into Core.evalExpr for kernel bodies --
/// a cycle the fsproj file order forbids -- so Run.fs installs these two
/// closures at startup and Core reaches them via `InterpState.Hooks`. With
/// `Hooks = None` every routed node raises InterpUnsupported (M0 parity).
/// The shapes are the M2 contract: Interp/Loops.fs's `evalArrayNode`/`force`
/// must match these signatures.
type InterpHooks = {
    /// Evaluate any node Core classifies as loop/array *machinery*: IRCompute
    /// (the force), the reduce family, IRContains -- NOT IRArrayLit (it
    /// materializes eagerly via ArrayOps in evalExpr). The IRExpr is passed
    /// verbatim; Loops owns input forcing, buildLoopNestCodeGen, output
    /// allocation, and the fill/fold.
    EvalArrayNode: InterpState -> Env -> IRExpr -> Value
    /// Force a VDeferred / VLoopObj to a concrete Value, mirroring
    /// genComputeBinding.resolveComputation + dispatch, including the (4)
    /// double-consumer memoization. Called whenever Core needs a concrete
    /// value out of a suspended computation (e.g. a deferred array feeding
    /// IRIndex / IRExtent / IRRank).
    Force: InterpState -> Env -> Value -> Value
}

/// One interpreter run's mutable accounting plus the shared read-only tables.
/// Steps/Depth/Cells are the live budgets, Callables is the id->callable
/// table (built once by makeState, includes let-alias entries so
/// `let f = <lambda>` references resolve), and Err accumulates panic text
/// for the caller to flush to stderr. `Cells` is allocated but not
/// incremented in M0 (no arrays are materialized yet); it is here so the
/// array milestone can charge allocations without changing this record's shape.
and InterpState = {
    Limits: InterpLimits
    mutable Steps: int64
    mutable Depth: int
    mutable Cells: int64
    Callables: Dictionary<IRId, IRCallable>
    Err: System.Text.StringBuilder
    // Shadow call stack (M1): the differential twin of cpp/blade_runtime.hpp's
    // `thread_local Frame stack[64]` + `int depth`. CodeGen emits
    // `BLADE_FRAME("<blade-name>", nullptr, 0)` at every user function/lambda
    // body entry (CodeGen.fs:9342, 9392) -- file/line always nullptr/0, so a
    // frame prints only its name, pushed/popped in evalCall with the same
    // string CodeGen pushes (IRCallable.Name == IRFuncDef.Name). `FrameNames`
    // holds the outermost 64 (header caps storage at `if (depth < 64) store`);
    // `FrameDepth` keeps counting past 64, so the panic-time `min(depth, 64)`
    // slice matches the header byte-for-byte.
    FrameNames: string[]
    mutable FrameDepth: int
    // Module-global scope (M1, capturing-function arc): the root env holding
    // the module's evaluated top-level bindings. Call frames chain to it as
    // their parent -- a function body may reference module-level bindings
    // directly (CodeGen emits such functions as main-local [&]-capturing
    // lambdas), and with globally-unique IRIds a root-parented lookup
    // resolves exactly the binding the C++ capture references. Set by Run.fs
    // after envNew, before any binding is evaluated.
    mutable Global: Env option
    /// Fresh-id source for synthetic callables minted while forcing (e.g.
    /// applyFunctorWrappers registers inline-wrapped kernels into the AsyncLocal
    /// AnalysisContext buildLoopNestCodeGen resolves). Seeded past the module's
    /// id space (mirroring CodeGen.genMainProgram) so a synthetic id can never
    /// hijack a real binding/param id. One builder / run.
    Builder: IRBuilder
    /// The installed loop/array backend (Interp/Loops.fs), or None for a pure
    /// scalar run. Set once by Run.fs immediately after makeState. When None,
    /// every loop/array-machinery node raises InterpUnsupported (M0 parity).
    mutable Hooks: InterpHooks option
    /// Assignable block-level array lets (IRModule.MutableArrayLets). A let
    /// whose id is here and whose initializer references an existing array
    /// deep-copies the store at bind time -- the differential twin of
    /// CodeGen's fresh-alloc + pool-copy path (genVarAliasBinding), so
    /// mutations through the binding never corrupt the source array.
    MutableArrayLets: Set<IRId>
    /// Reference-keyed index from a module-level binding's Value expression
    /// object to its binding id (forced-deferred print parity). A root-cell
    /// VDeferred always carries the binding's own Value node as its payload,
    /// so a reference hit in Loops.force means "the deferred payload of
    /// module binding <id> is being forced" -- the value-space twin of
    /// forceDeferredArrayInput's IRVar arm. Sub-expression VDeferreds miss
    /// the index and are ignored. Built once by makeState over the merged module.
    DeferredBindingIndex: Dictionary<IRExpr, IRId>
    /// Module-level deferred bindings actually FORCED during evaluation.
    /// Print consults this: a deferred binding that ended up materialized
    /// auto-prints (mirroring genPrintStatements' forcedDeferredIdsCell); one
    /// that stayed deferred prints nothing. Populated by Loops.force.
    ForcedDeferred: HashSet<IRId>
}

/// The header's fixed frame-store size (cpp/blade_runtime.hpp: `Frame stack[64]`).
[<Literal>]
let FrameCap = 64

/// Build the interpreter state for a lowered module: the callables table
/// (reusing IRPrint.buildCallablesTableForModule so let-bound lambda aliases resolve
/// exactly as they do at codegen) plus zeroed budgets.
let makeState (modul: IRModule) (limits: InterpLimits) : InterpState =
    let table = buildCallablesTableForModule modul
    let dict = Dictionary<IRId, IRCallable>()
    for kv in table do dict.[kv.Key] <- kv.Value
    let builder = IRBuilder()
    // Keep synthetic-callable ids minted during forcing disjoint from the
    // module's own ids (same floor CodeGen uses -- see CodeGen.genMainProgram's
    // `builder.EnsureAtLeast(0x40000000)`).
    builder.EnsureAtLeast(0x40000000)
    { Limits = limits
      Steps = 0L
      Depth = 0
      Cells = 0L
      Callables = dict
      Err = System.Text.StringBuilder()
      FrameNames = Array.create FrameCap ""
      FrameDepth = 0
      Global = None
      Builder = builder
      Hooks = None
      MutableArrayLets = modul.MutableArrayLets
      DeferredBindingIndex =
        // Reference identity, deliberately: the SAME Value node stored into a
        // root VDeferred must hit; a structurally-equal node elsewhere must not.
        let d = Dictionary<IRExpr, IRId>(HashIdentity.Reference)
        for b in modul.Bindings do d.[b.Value] <- b.Id
        d
      ForcedDeferred = HashSet<IRId>() }

// Shadow call stack push/pop + panic-time capture, mirroring blade_runtime.hpp.
// Push at Blade function-body entry, pop on normal exit. On a panic the frames
// are read (capturedFrames) BEFORE unwinding: evalCall does NOT pop in a
// finally, so an escaping InterpPanic leaves the live call chain intact --
// exactly as the C++ side, where blade_rt::panic reads the stack and exits
// without running the RAII Scope destructors.

/// Push a frame (`Scope` ctor: `if (depth < 64) stack[depth] = ...; ++depth`).
let private pushFrame (st: InterpState) (name: string) : unit =
    if st.FrameDepth < FrameCap then st.FrameNames.[st.FrameDepth] <- name
    st.FrameDepth <- st.FrameDepth + 1

/// Pop a frame (`Scope` dtor: `--depth`).
let private popFrame (st: InterpState) : unit =
    st.FrameDepth <- st.FrameDepth - 1

/// The live shadow-stack frames at panic time, innermost first -- the order
/// blade_rt::panic walks them (`for (i = min(depth,64)-1; i >= 0; --i)`).
let capturedFrames (st: InterpState) : string list =
    let d = min st.FrameDepth FrameCap
    [ for i in (d - 1) .. -1 .. 0 -> st.FrameNames.[i] ]

// Small value coercions (loop bounds, complex components, truthiness).
// Numerics owns the bit-critical arithmetic coercions (private asF64/asI64);
// these are the few extra projections the evaluator itself needs, following
// the same C++ rules (truncate-toward-zero, nonzero-is-true) but never
// touching arithmetic results.

let private toBoolV (v: Value) : bool =
    match v with
    | VBool b -> b
    | VInt n -> n <> 0L
    | VInt32 n -> n <> 0
    | _ -> false

/// C++ `x != 0` over any scalar -- the choice `<|>` pick test (exprToCpp
/// binds the left operand to a temporary first, CodeGen.fs:1589). Distinct
/// from toBoolV, the bool-context projection (floats are never bool contexts,
/// but ARE compared against zero by choice).
let private valueNonZero (v: Value) : bool =
    match v with
    | VBool b -> b
    | VInt n -> n <> 0L
    | VInt32 n -> n <> 0
    | VFloat f -> f <> 0.0
    | VFloat32 f -> f <> 0.0f
    | VComplex (r, i) -> r <> 0.0 || i <> 0.0
    | _ -> false

/// Integer projection for loop bounds / counters -- C++ `(int64_t)x` truncates
/// toward zero on float sources.
let private toI64 (v: Value) : int64 =
    match v with
    | VInt n -> n
    | VInt32 n -> int64 n
    | VFloat f -> int64 f
    | VFloat32 f -> int64 (float f)
    | VBool b -> if b then 1L else 0L
    | VChar c -> int64 (int c)
    | _ -> 0L

/// Float projection for complex-literal components (checkExpr guarantees these
/// are float-typed, so this is a widening, not a lossy cast).
let private toF64 (v: Value) : float =
    match v with
    | VFloat f -> f
    | VFloat32 f -> float f
    | VInt n -> float n
    | VInt32 n -> float n
    | VChar c -> float (int c)
    | VBool b -> if b then 1.0 else 0.0
    | VComplex (r, _) -> r
    | _ -> nan

/// The IRExpr union case name (e.g. "IRZip"), for InterpUnsupported messages.
let private nodeName (e: IRExpr) : string =
    let case, _ =
        Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(e, typeof<IRExpr>)
    case.Name

// Tuple projection (flat vs. structural), mirroring CodeGen.fs:1219.
// IRTupleProj carries isFlat: structural (`std::get<i>`) indexes the immediate
// tuple; flat indexes leaves of the fully-flattened nested tuple. Leaf
// counting matches IR.flattenTupleLeaves exactly (only tuples recurse).

let rec private countLeaves (v: Value) : int =
    match v with
    | VTuple els -> els |> Array.sumBy countLeaves
    | _ -> 1

let private projectStruct (v: Value) (i: int) : Value =
    match v with
    | VTuple els when i >= 0 && i < els.Length -> els.[i]
    | _ -> raise (InterpPanic ("BL8003", "tuple projection index out of range", None, 0))

let rec private projectFlat (v: Value) (flatIdx: int) : Value =
    match v with
    | VTuple els ->
        let mutable offset = 0
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < els.Length do
            let c = countLeaves els.[i]
            if flatIdx < offset + c then
                match els.[i] with
                | VTuple _ -> result <- Some (projectFlat els.[i] (flatIdx - offset))
                | leaf -> result <- Some leaf
            offset <- offset + c
            i <- i + 1
        match result with
        | Some r -> r
        | None -> raise (InterpPanic ("BL8003", "tuple projection index out of range", None, 0))
    | leaf -> leaf   // flat index into a leaf: the value is itself the sole leaf

// Variant construction, mirroring Lowering.fs:164-176 / 654-655. A
// payload-less variant constructor (`North`) lowers to a bare IRParam whose
// type is the sum type (IRTNamed); a payload constructor (`Some x`) lowers to
// IRApp(IRParam("Some", _, FuncElem(_, IRTNamed _)), [x]). The runtime tag is
// `hash constructorName`, the same derivation IRPatVariant stores, so
// construction and dispatch agree (F# `hash` of a string is stable within a
// process). Qualified references (TExprQualified -> IRParam with a dotted
// name) also spell as IRParam, so a name containing '.' is never a constructor.

let private variantResultName (ty: IRType) : string option =
    match ty with
    | IRTNamed tn -> Some tn
    | FuncElem (_, IRTNamed tn) -> Some tn
    | _ -> None

let private isCtorName (name: string) = not (name.Contains ".")

// Pattern literal comparison (IRPatLit): value-level, IEEE/ordinal exact.

let private litMatches (lit: IRLit) (v: Value) : bool =
    match lit, v with
    | IRLitInt n, VInt m -> n = m
    | IRLitInt n, VInt32 m -> n = int64 m
    | IRLitInt n, VChar c -> n = int64 (int c)
    | IRLitFloat f, VFloat g -> f = g
    | IRLitFloat f, VFloat32 g -> f = float g
    | IRLitFloat32 f, VFloat32 g -> f = g
    | IRLitFloat32 f, VFloat g -> float f = g
    | IRLitBool a, VBool b -> a = b
    | IRLitString a, VString b -> System.String.Equals(a, b, System.StringComparison.Ordinal)
    | IRLitUnit, VUnit -> true
    | _ -> false

// M2 defer/force plumbing (formalism 4): classification + backend routing.
// The interpreter keeps CodeGen's DeferredComputations map IMPLICIT in the
// value domain: a deferred binding's cell holds VDeferred(expr, env), a loop
// object's cell holds VLoopObj. `evalBinding` decides which, mirroring
// CodeGen.genBinding / computeDeferredIds exactly so the interpreter's cell
// contents stay in lock-step with Print.

/// True when an OPERAND expression is (or aliases) a deferred computation --
/// the value-space mirror of CodeGen.computeDeferredIds' inner
/// `isDeferred ids e` (a combinator/compose/parallel/... form, or an IRVar
/// whose bound cell already holds a VDeferred). By induction over
/// module-ordered bindings this coincides with `Set.contains id deferredIds`.
let private isDeferredOperand (env: Env) (e: IRExpr) : bool =
    match e with
    | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _
    | IRFunctorMap _ | IRChoice _ | IRFallback _ | IRComposeObj _ | IRComposeMeth _
    | IRBind _ | IRZip _ | IRSequence _ -> true
    | IRVar (id, _) ->
        match envTryFind env id with
        | Some cell -> cell.V.IsVDeferred
        | None -> false
    | _ -> false

/// Whether a top-level binding VALUE defers (its cell holds VDeferred) rather
/// than materializes -- an exact port of CodeGen.computeDeferredIds' per-binding
/// `shouldDefer` (CodeGen.fs:9865-9886), so a binding is VDeferred iff Print
/// would skip it. IRMethodFor/IRObjectFor are NOT deferred here (evalBinding
/// treats them as loop objects -- VLoopObj -- before consulting this).
let private shouldDeferBinding (env: Env) (ty: IRType) (value: IRExpr) : bool =
    // Mirror computeDeferredIds' resultIsArray rule: an array-typed combinator is a
    // deferred computation whatever its operands (materializes only at |> compute);
    // scalar `<|>` / guard stay eager ternaries.
    let resultIsArray = match stripUnits ty with ArrayElem _ -> true | _ -> false
    match value with
    | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _ -> true
    | IRZip _ -> true
    | IRComposeObj _ -> true
    // f >> g is a function VALUE (applied via applyValue, never forced through
    // the Loops backend) -- defer so the eager branch doesn't force it. The
    // binding is function-typed, so Print skips it on both sides regardless.
    | IRCompose _ -> true
    | IRBind (comp, _) -> resultIsArray || isDeferredOperand env comp
    | IRComposeMeth (left, right) -> resultIsArray || isDeferredOperand env left || isDeferredOperand env right
    | IRFunctorMap (_, inner) -> resultIsArray || isDeferredOperand env inner
    | IRChoice (left, right) -> resultIsArray || isDeferredOperand env left || isDeferredOperand env right
    // <|:> always defers at binding level (CodeGen.genFallbackBinding /
    // computeDeferredIds agree) -- its operands are arrays, never scalars.
    | IRFallback _ -> true
    | IRGuard (_, body) ->
        let rec leafIsDeferred e =
            match e with
            | IRGuard (_, inner) -> leafIsDeferred inner
            | _ -> isDeferredOperand env e
        resultIsArray || leafIsDeferred body
    | IRSequence elems -> resultIsArray || (elems |> List.exists (isDeferredOperand env))
    | IRTuple elems -> elems |> List.forall (isDeferredOperand env)
    | IRTupleProj (IRVar (pid, _), _, _) ->
        match envTryFind env pid with
        | Some cell -> cell.V.IsVDeferred
        | None -> false
    | IRVar (srcId, _) ->
        match envTryFind env srcId with
        | Some cell -> cell.V.IsVDeferred
        | None -> false
    | _ -> false

/// Route a node Core classifies as loop/array *machinery* (the force, reductions,
/// nest-consuming reads) to the installed Loops backend. With no backend it is an
/// unimplemented feature -- the same InterpUnsupported skip M0 produced.
let private evalArrayNode (st: InterpState) (env: Env) (expr: IRExpr) : Value =
    match st.Hooks with
    | Some h -> h.EvalArrayNode st env expr
    | None -> raise (InterpUnsupported (nodeName expr))

/// Force a value to concrete form when it is a suspended computation / loop
/// object (delegating to the Loops backend); a concrete value passes through.
/// Used wherever an array-op arm needs a real array out of a deferred input.
let private forceValue (st: InterpState) (env: Env) (v: Value) : Value =
    match v with
    | VDeferred _ | VLoopObj _ ->
        match st.Hooks with
        | Some h -> h.Force st env v
        | None -> raise (InterpUnsupported "force (no loop/array backend installed)")
    | _ -> v

/// Is this array eligible for the assignable-let deep copy? Mirrors the
/// C++ side's dense-only guard in genVarAliasBinding (plain dense or
/// symmetric-compact Array<T,N>): ragged-family / dep-idx / compound index
/// tags -- and any ragged backing store -- keep the historical alias.
let private isDenseCopyableArray (ba: BladeArray) : bool =
    (match ba.Data with SRagged _ -> false | _ -> true)
    && ba.IndexTypes |> List.forall (fun ix ->
        not (isRaggedRowKind ix.IxKind)
        && ix.IxKind <> IxKCompound
        && ix.IxKind <> IxKCompoundDynamic
        && ix.IxKind <> IxKSparse)

/// An EnumIdx admissible value -> the interpreter Value used to compare against a
/// key array's element (Case-2 EnumIdx reverse lookup).
let private enumValueToValue (ev: EnumValue) : Value =
    match ev with
    | EVInt n -> VInt n
    | EVString s -> VString s

/// The IRTGroupKeys type of a function-body `let gk = group_keys(...)`, read
/// back off the body's typed references to the grouping. The value twin of
/// CodeGen.groupKeysTypeInScope, and load-bearing for the same reason: that
/// type picks the bucketing regime, and the regimes answer different questions
/// (dynamic discovery buckets in first-occurrence order over the observed keys;
/// positional bucketing uses the declared Idx<N>, empty groups included). Both
/// lanes must read it from the same place or the diff gate is comparing two
/// different groupings. `None` -> dynamic, matching the C++ fallback.
let private groupKeysTypeInScope (id: IRId) (scope: IRExpr) : IRType option =
    let rec scan (e: IRExpr) : IRType option =
        match e with
        | IRVar (vid, (IRTGroupKeys _ as ty)) when vid = id -> Some ty
        | ExprShape (children, _) -> children |> List.tryPick scan
    scan scope

// The evaluator.

/// Evaluate an IR expression to a runtime value. One step charged per entry;
/// exceeding the step budget raises a loud panic (there is no matching C++
/// output -- a runaway is a diagnostic, not a value).
let rec evalExpr (st: InterpState) (env: Env) (expr: IRExpr) : Value =
    st.Steps <- st.Steps + 1L
    if st.Steps > st.Limits.MaxSteps then
        raise (InterpPanic ("BL8005", "interpreter step budget exceeded", None, 0))
    match expr with
    | IRLit lit ->
        match lit with
        | IRLitInt n -> VInt n            // Blade's default integer is Int64; narrower
        | IRLitFloat f -> VFloat f        // int contexts coerce through arithmetic, not here
        | IRLitFloat32 f -> VFloat32 f    // width reconciled at lowering (lowerLiteralValued)
        | IRLitBool b -> VBool b
        | IRLitString s -> VString s
        | IRLitUnit -> VUnit

    | IRVar (id, _) ->
        // Locals (let / param / loop var / capture) win; otherwise the id names
        // a callable (top-level function or lifted lambda) -- reify it as a
        // closure, capturing the current frame's cells for its free vars.
        match envTryFind env id with
        | Some cell -> cell.V
        | None ->
            match st.Callables.TryGetValue id with
            | true, callable -> makeClosure env callable
            | _ -> raise (InterpPanic ("BL8004", $"unbound variable (id {id})", None, 0))

    | IRParam (name, _, ty) ->
        // Bare IRParam in value position: a nullary variant constructor. (Real
        // function parameters are referenced as IRVar of their VarId, never as
        // this expression form.)
        match variantResultName ty with
        | Some tn when isCtorName name -> VVariant (tn, hash name, None)
        | _ -> raise (InterpUnsupported $"IRParam '{name}' (qualified or partially-applied reference)")

    | IRComplex (re, im) ->
        let r = toF64 (evalExpr st env re)
        let i = toF64 (evalExpr st env im)
        VComplex (r, i)

    // fma(a, b, c): ONE rounding. Math.FusedMultiplyAdd is the hardware fma
    // where the CPU has it and a correctly-rounded software fma otherwise,
    // which is exactly the contract of the std::fma the compiled lane calls
    // -- so this node is bit-identical across lanes under ANY BLADE_FP_CONTRACT
    // (the one arithmetic node for which that is true by construction).
    | IRFma (a, b, c) ->
        let x = toF64 (evalExpr st env a)
        let y = toF64 (evalExpr st env b)
        let z = toF64 (evalExpr st env c)
        VFloat (System.Math.FusedMultiplyAdd (x, y, z))

    // `&&` / `||` short-circuit exactly like the emitted C++ (CodeGen.fs:782):
    // the right operand is not evaluated when the left decides the result.
    | IRBinOp (_, IRAnd, l, r) ->
        if toBoolV (evalExpr st env l) then VBool (toBoolV (evalExpr st env r))
        else VBool false
    | IRBinOp (_, IROr, l, r) ->
        if toBoolV (evalExpr st env l) then VBool true
        else VBool (toBoolV (evalExpr st env r))
    | IRBinOp (_, op, l, r) ->
        // Left-to-right operand evaluation, then Numerics' bit-exact dispatch
        // (promotion / wraparound / complex coercion / string concat). Array
        // operands never reach here: Lowering's lowerArrayBinOpsModule rewrites
        // every array-typed binop into a method_for co-iteration combinator,
        // so this arm only ever sees scalars.
        let lv = evalExpr st env l
        let rv = evalExpr st env r
        N.evalBinOp op lv rv

    | IRUnaryOp (op, e) ->
        N.evalUnaryOp op (evalExpr st env e)

    | IRLet (id, value, body) ->
        // SSA ids are globally unique, so binding into the current frame (rather
        // than a child scope) is safe and matches the flat env model.
        let v =
            match value with
            // `let gk = group_keys(k)` inside a function body: intercepted here
            // for the same reason evalBinding intercepts the module-level form
            // -- the bucketing regime lives in the IRTGroupKeys type, not in the
            // node -- with the type recovered from this let's SCOPE rather than
            // from a binding record it does not have.
            | IRGroupKeys keys ->
                let ty = groupKeysTypeInScope id body |> Option.defaultValue IRTUnit
                buildGroupKeysValue st env keys ty
            | IRSegments (offsets, _) -> segmentsValue offsets
            | _ -> evalExpr st env value
        // Copy semantics for assignable array lets initialized from an
        // existing array (`let mut a = Z` -- st.MutableArrayLets): deep-copy
        // the store so mutations through `a` never corrupt `Z`, mirroring
        // CodeGen's fresh-alloc + pool-copy (genVarAliasBinding). Only an
        // IRVar initializer aliases; ragged stores keep the historical alias
        // (matching the C++ side's dense-only guard).
        let v =
            match value, v with
            | IRVar _, VArray ba when Set.contains id st.MutableArrayLets
                                      && isDenseCopyableArray ba ->
                VArray (copyBladeArray ba)
            | _ -> v
        envBind env id v |> ignore
        evalExpr st env body

    | IRSequence exprs ->
        let mutable result = VUnit
        for e in exprs do result <- evalExpr st env e
        result

    | IRIf (cond, thenBr, elseBr) ->
        if toBoolV (evalExpr st env cond) then evalExpr st env thenBr
        else evalExpr st env elseBr

    | IRMatch (scrutinee, cases) ->
        let sv = evalExpr st env scrutinee
        evalMatch st env sv cases

    | IRTuple exprs ->
        VTuple (exprs |> List.map (evalExpr st env) |> Array.ofList)

    | IRTupleProj (e, i, isFlat) ->
        let v = evalExpr st env e
        if isFlat then projectFlat v i else projectStruct v i

    | IRTupleCons (head, tail) ->
        let h = evalExpr st env head
        match evalExpr st env tail with
        | VTuple els -> VTuple (Array.append [| h |] els)
        | other -> VTuple [| h; other |]

    | IRTupleDecons tuple ->
        evalExpr st env tuple   // decons is a no-op; projection does the work

    | IRStructLit (typeName, fields) ->
        let fs = fields |> List.map (fun (n, e) -> (n, evalExpr st env e)) |> Array.ofList
        VStruct (typeName, fs)

    | IRFieldAccess (obj, field) ->
        match evalExpr st env obj with
        | VStruct (_, fields) ->
            match fields |> Array.tryFind (fun (n, _) -> n = field) with
            | Some (_, v) -> v
            | None -> raise (InterpPanic ("BL8003", $"no field '{field}' on struct", None, 0))
        | _ -> raise (InterpUnsupported "IRFieldAccess on non-struct value")

    // Inline object_for application (`A [op] B` outer product, etc.) is an
    // array-producing binding form, not a callable application -- route it to the
    // Loops backend (materializeObjectForApp) rather than evalApp, whose callee
    // would be a VLoopObj it cannot invoke.
    | IRApp (IRObjectFor _, _, _) ->
        evalArrayNode st env expr

    | IRApp (func, args, _) ->
        evalApp st env func args

    | IRAssign (target, value) ->
        let v = evalExpr st env value
        evalAssign st env target v
        VUnit

    | IRForRange (vid, lo, hi, body) ->
        // Bounds evaluated ONCE before the loop; one reused int64 counter cell
        // from lo to hi-1 (EmitCpp.forLoopFrom emits `for (int64_t k = lo;
        // k < hi; k++)`). The body sees the counter via IRVar(vid).
        let loV = toI64 (evalExpr st env lo)
        let hiV = toI64 (evalExpr st env hi)
        let cell = envBind env vid (VInt loV)
        let mutable i = loV
        // InterpBreak = the C++ `break` genForRangeBinding emits for IRBreakIf
        // (the rec-array while guard): it unwinds to HERE, ending this loop and
        // resuming after it. The handler wraps the whole loop rather than each
        // trip -- one guarded region per loop execution instead of one per
        // iteration -- and being per loop LEVEL it still stops only the
        // innermost enclosing loop when they nest.
        (try
            while i < hiV do
                cell.V <- VInt i
                evalExpr st env body |> ignore
                i <- i + 1L
         with InterpBreak -> ())
        VUnit

    | IRBreakIf cond ->
        if toBoolV (evalExpr st env cond) then raise InterpBreak
        else VUnit

    | IRConstraintCheck (cond, code, message, span) ->
        // `if (!(cond)) blade_rt::panic(code, message, file, line)`
        // (CodeGen.fs:7166). File is nullptr when empty, line 0 when unset --
        // reproduced here so Run.fs can render the identical stderr line.
        if toBoolV (evalExpr st env cond) then VUnit
        else
            let fileOpt = match span.File with Some f when f <> "" -> Some f | _ -> None
            let line = if span.StartLine > 0 then span.StartLine else 0
            raise (InterpPanic (code, message, fileOpt, line))

    | IRRank arr ->
        // Rank is static in CodeGen (from the type). Scalars are rank 0; a
        // concrete array reports its loop-level count. A deferred input is forced
        // first (via the backend) so an unmaterialized array still ranks.
        (match forceValue st env (evalExpr st env arr) with
         | v when (N.scalarElem v).IsSome -> VInt 0L
         | VArray ba -> VInt (int64 ba.Extents.Length)
         | _ -> raise (InterpUnsupported "IRRank"))

    // ---- M2 deferred-computation forms (formalism 4). The suspended (expr, env)
    //      pair IS the deferral, mirroring the DeferredComputations entry
    //      genBinding creates; a downstream IRCompute / combinator forces it via
    //      Hooks.Force. evalBinding intercepts these at binding level too --
    //      this arm covers sub-expression / aliased occurrences. (IRSequence
    //      defers only at binding level, via shouldDeferBinding.)
    | IRApplyCombinator _ | IRComposeApply _ | IRParallel _ | IRFusion _
    | IRFunctorMap _ | IRZip _ | IRComposeObj _ | IRComposeMeth _
    | IRBind _ ->
        VDeferred (expr, env)

    // ---- pure(v): the identity monadic unit -- renders as just its inner value
    //      (exprToCppCore IRPure -> inner, CodeGen.fs:1291; typeOf sees through).
    | IRPure e -> evalExpr st env e

    // ---- f >> g function composition: a VALUE (C++ emits a lambda,
    //      CodeGen.fs:1384-1388). Held suspended; applyValue unwraps it at the
    //      call site as g(f(args)). Never forced through the Loops backend --
    //      shouldDeferBinding lists it so compose bindings skip the eager-branch
    //      force (function-typed bindings print nothing on both sides).
    | IRCompose _ -> VDeferred (expr, env)

    // ---- M2 choice / guard in EXPRESSION position. Over a computation operand
    //      they defer exactly like the combinator forms above. Over scalars
    //      they are the EAGER exprToCpp ternaries (CodeGen.fs:1589/1598):
    //        a <|> b            ->  auto l = a; l != 0 ? l : b   (a evaluated once)
    //        guard(p, body)     ->  (p ? body : zero)   (body NOT evaluated
    //                               when p is false)
    //      Without these arms a scalar guard/choice inside arithmetic reached
    //      Numerics as a VDeferred operand -- BL8010 (guard-combinators/006).
    | IRChoice (left, right) ->
        if isDeferredOperand env left || isDeferredOperand env right then
            VDeferred (expr, env)
        else
            let lv = evalExpr st env left
            if valueNonZero lv then lv else evalExpr st env right
    // <|:> allocated-fallback: array-level only (typecheck rejects scalars);
    // always a deferred computation here, forced (and currently refused) in
    // Loops.forceExpr.
    | IRFallback _ -> VDeferred (expr, env)
    | IRGuard (cond, body) ->
        // Defer iff the guard's LEAF body is a computation -- genGuardBinding's
        // leafIsComputation recursion (CodeGen.fs:9019, nested guards peel).
        let rec leafIsComp e =
            match e with
            | IRGuard (_, inner) -> leafIsComp inner
            | _ -> isDeferredOperand env e
        if leafIsComp body then VDeferred (expr, env)
        elif toBoolV (evalExpr st env cond) then evalExpr st env body
        else
            // The false arm: the renderer's type-appropriate zero, derived from
            // the body's STATIC type (the body is not evaluated), CodeGen.fs:1377.
            match typeOf body with
            | IRTScalar ETBool -> VBool false
            | IRTScalar ETInt64 | IRTScalar ETInt32 -> VInt 0L
            | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> VInt 0L
            | _ -> VFloat 0.0

    // ---- M2 loop objects: method_for / object_for -- provenance the `<@>` apply
    //      reads later (no code, like genBinding's IRMethodFor/IRObjectFor arms).
    | IRMethodFor _ | IRObjectFor _ ->
        VLoopObj { Provenance = expr; Captured = env }

    // ---- M2 the force (IRCompute) + reduce family + membership -> Loops backend
    //      (the nest machinery). None = InterpUnsupported, i.e. M0-parity skip.
    | IRCompute _ | IRReduce _ | IRReduceCompute _ | IRProdSum _ | IRContains _ ->
        evalArrayNode st env expr

    // ---- display.emit: the IR's one EFFECTFUL expression (docs/display-frames.md).
    //      Buffered rather than written, because the interpreter assembles all
    //      of stdout after every binding has evaluated -- Run.execProgram
    //      flushes the buffer ahead of the binding prints, which is exactly
    //      where the compiled binary's std::cout writes land (inside main()'s
    //      body, before the timing line). Same bytes, same order, both lanes.
    | IRDisplayEmit (head, quoted, dataExpr, metaTail, idOpt) ->
        (match evalExpr st env dataExpr, idOpt with
         | VString s, None -> VBool (Blade.Display.Frame.emit head quoted s metaTail)
         // display.emit_id: the frame's meta.id is this second runtime String.
         // A live sink (ide serve) may intercept a STREAM-mime frame here
         // instead of buffering it; with no sink -- every other lane -- the
         // line is buffered exactly like emit's.
         | VString s, Some idExpr ->
             (match evalExpr st env idExpr with
              | VString idText -> VBool (Blade.Display.Frame.emitId head quoted s metaTail idText)
              | _ -> raise (InterpUnsupported "display.emit_id: id did not evaluate to a String"))
         | _ -> raise (InterpUnsupported "display.emit: payload did not evaluate to a String"))

    // ---- display.json_array / display.json_num: JSON text of a numeric
    //      array / scalar. Formatting parity is the contract:
    //      CppFormat.formatFloatShortest is the byte-exact mirror of the C++
    //      helpers' shortest round-trip rendering (blade_display::jsonfloat
    //      in Blade.Display.Frame.cppRuntime), so the differential gate pins
    //      the two lanes together exactly as it does for prints.
    | IRDisplayJson (rank, dataExpr) ->
        (match forceValue st env (evalExpr st env dataExpr) with
         | VArray ba ->
             // Frame.jsonNumber is the non-finite guard -- NaN/+-Inf go out as
             // `null`, everything else as the shortest round-trip rendering.
             // Its C++ mirror is blade_display::jsonval.
             let jsonF (x: float) =
                 Blade.Display.Frame.jsonNumber (Blade.Interp.CppFormat.formatFloatShortest x) x
             let jsonF32 (x: float32) =
                 Blade.Display.Frame.jsonNumber (Blade.Interp.CppFormat.formatFloatShortest (float x)) (float x)
             let elemStr (store: Store) (i: int) : string =
                 match store with
                 | SFloat a -> jsonF a.[i]
                 | SInt a -> string a.[i]
                 | SObj vs ->
                     (match vs.[i] with
                      | VFloat f -> jsonF f
                      | VFloat32 f -> jsonF32 f
                      | VInt n -> string n
                      | VInt32 n -> string n
                      | _ -> raise (InterpUnsupported "display.json_array: non-numeric element"))
                 | _ -> raise (InterpUnsupported "display.json_array: unsupported element store")
             let sb = System.Text.StringBuilder()
             (match rank, ba.Data with
              | 1, store ->
                  let n = int ba.Extents.[0]
                  sb.Append '[' |> ignore
                  for i in 0 .. n - 1 do
                      if i > 0 then sb.Append ',' |> ignore
                      sb.Append(elemStr store i) |> ignore
                  sb.Append ']' |> ignore
              | 2, SNested rows ->
                  let nr = int ba.Extents.[0]
                  let nc = int ba.Extents.[1]
                  sb.Append '[' |> ignore
                  for i in 0 .. nr - 1 do
                      if i > 0 then sb.Append ',' |> ignore
                      sb.Append '[' |> ignore
                      for j in 0 .. nc - 1 do
                          if j > 0 then sb.Append ',' |> ignore
                          sb.Append(elemStr rows.[i] j) |> ignore
                      sb.Append ']' |> ignore
                  sb.Append ']' |> ignore
              | 2, store ->
                  // Flat row-major backing (computed rank-2 results).
                  let nr = int ba.Extents.[0]
                  let nc = int ba.Extents.[1]
                  sb.Append '[' |> ignore
                  for i in 0 .. nr - 1 do
                      if i > 0 then sb.Append ',' |> ignore
                      sb.Append '[' |> ignore
                      for j in 0 .. nc - 1 do
                          if j > 0 then sb.Append ',' |> ignore
                          sb.Append(elemStr store (i * nc + j)) |> ignore
                      sb.Append ']' |> ignore
                  sb.Append ']' |> ignore
              | r, _ -> raise (InterpUnsupported $"display.json_array: unsupported rank {r}"))
             VString (sb.ToString())
         | _ -> raise (InterpUnsupported "display.json_array: operand did not evaluate to an array"))

    | IRDisplayNum dataExpr ->
        (match forceValue st env (evalExpr st env dataExpr) with
         | VFloat f -> VString (Blade.Display.Frame.jsonNumber (Blade.Interp.CppFormat.formatFloatShortest f) f)
         | VFloat32 f -> VString (Blade.Display.Frame.jsonNumber (Blade.Interp.CppFormat.formatFloatShortest (float f)) (float f))
         | VInt n -> VString (string n)
         | VInt32 n -> VString (string n)
         | _ -> raise (InterpUnsupported "display.json_num: operand did not evaluate to a numeric scalar"))

    // ---- display.json_string: a String as a quoted, escaped JSON string.
    //      Frame.jsonString is the shared implementation; the compiled lane
    //      calls blade_display::jsonstr, its generated mirror.
    | IRDisplayStr dataExpr ->
        (match forceValue st env (evalExpr st env dataExpr) with
         | VString s -> VString (Blade.Display.Frame.jsonString s)
         | _ -> raise (InterpUnsupported "display.json_string: operand did not evaluate to a String"))

    // ---- group_by(vals, gk) -> a ragged (CSR) array. Materializes via the Loops
    //      backend (it reads the VGroupKeys from `gk` + gathers the values).
    //      group_keys itself is intercepted at BINDING level (evalBinding) where
    //      the IRTGroupKeys type drives the Case-1/2/3 bucketing.
    | IRGroupBy _ ->
        evalArrayNode st env expr

    // ---- group_keys deliberately has NO arm here. Both of its legal positions
    //      are intercepted one layer up -- evalBinding for a module binding, the
    //      IRLet arm above for a function-body let -- because both need the
    //      IRTGroupKeys type the bare node does not carry. A group_keys reaching
    //      expression position is an indirection BL3017 should already have
    //      refused, and the loud InterpUnsupported is the right answer for it:
    //      guessing a regime here would answer a different question than the
    //      compiled lane, silently.

    // ---- group_bucket(gk) -> a dense rank-1 Int64 array over the source index
    //      space. Same backend route as group_by: it reads the VGroupKeys.
    | IRGroupBucket _ ->
        evalArrayNode st env expr

    // ---- ungroup(G) -> the rows of a segment-grouped array over the source
    //      axis (docs/plans/structural/07 §3.3). Same backend route.
    | IRUngroup _ ->
        evalArrayNode st env expr

    // ---- extents(gk) -> a dense rank-1 Int64 array of per-group sizes.
    | IRGroupSizes _ ->
        evalArrayNode st env expr

    // ---- M2 dense array literal: materialize eagerly via ArrayOps (no nest, no
    //      backend needed). Elements evaluate left-to-right, then pack/nest.
    | IRArrayLit (elements, arrType) ->
        let elemVals = elements |> List.map (evalExpr st env)
        VArray (ArrayOps.arrayLitFromValues arrType elemVals)

    // ---- M2 virtual-array sources (range / reverse / blocked): no standalone
    //      store -- consumed only as nest inputs (ArraySource.SVirtual). The
    //      suspended (expr, env) lets the Loops backend read the IRRange /
    //      IRVirtualReverse descriptor when it wires the nest.
    | IRRange _ | IRVirtualReverse _ ->
        VDeferred (expr, env)

    // ---- M2 indexing / currying / poly-index over a concrete array. Force a
    //      deferred array input first, then delegate to ArrayOps (state-free -- it
    //      takes only the array + evaluated indices).
    | IRIndex (arrExpr, indices, _) ->
        (match forceValue st env (evalExpr st env arrExpr) with
         | VArray ba ->
             let idxVals = indices |> List.map (evalExpr st env)
             ArrayOps.indexArray ba idxVals
         | _ -> evalArrayNode st env expr)   // streamed / non-array source -> backend
    | IRCurry (arrExpr, idxExpr, _) ->
        (match forceValue st env (evalExpr st env arrExpr) with
         | VArray ba -> ArrayOps.curryArray ba (toI64 (evalExpr st env idxExpr))
         | _ -> raise (InterpUnsupported "IRCurry on non-array"))
    | IRPolyIndex (packExpr, idxExpr) ->
        let packV = evalExpr st env packExpr
        let iV = toI64 (evalExpr st env idxExpr)
        (match packV with
         | VTuple _ | VArray _ -> ArrayOps.polyIndex packV iV
         | _ -> raise (InterpUnsupported "IRPolyIndex on non-pack"))
    | IRPolyTail _ ->
        raise (InterpUnsupported "IRPolyTail on non-monomorphized parameter pack")

    // ---- M2 extent: read the concrete array's static shape (formalism 0.9).
    //      A rank-1 compound has no `.extents` -- its sole axis's runtime extent
    //      is the compact index's cardinality (popcount), the `extents(f)`
    //      overload for compounds (genExtentExpr, CodeGen.fs:2127).
    | IRExtent (arrExpr, dim) ->
        (match forceValue st env (evalExpr st env arrExpr) with
         | VArray ba when dim >= 0 && dim < ba.Extents.Length -> VInt ba.Extents.[dim]
         | VCompound cv -> VInt cv.Cardinality
         | VSparse sv -> VInt sv.Cardinality
         | _ -> raise (InterpUnsupported "IRExtent"))

    // ---- Compound-halo window read `w(o)`: the COORDINATE of the present cell at
    //      ordinal (center + o) -- IRHaloUnhash's `w[(o)][0]` over the compound's
    //      rank_to_tuple table (CodeGen.fs:3051). The window value is a VTuple
    //      (coordinate column, center ordinal), bound by materializeCompoundHaloMap.
    | IRHaloUnhash (winExpr, off) ->
        (match forceValue st env (evalExpr st env winExpr) with
         | VTuple [| VArray col; VInt center |] -> ArrayOps.readCell col [ center + off ]
         | _ -> raise (InterpUnsupported "IRHaloUnhash: window is not a compound-halo window"))

    // ---- M4 wave-1 eager set/reshape ops (mask / sort / set-ops / transpose).
    //      MATERIALIZE a fresh array (forcing deferred inputs first; sort/mask
    //      also call a kernel), so route to the Loops backend like IRReduce /
    //      IRContains. Semantics pinned to CodeGen's materialize*Form emitters:
    //      mask = Bool PRESENCE array (NOT filtering), sort = stable ascending by
    //      key, set-ops = unordered_set first-occurrence, transpose = hard swap.
    | IRMask _ | IRSort _ | IRUnique _ | IRIntersect _ | IRUnion _ | IRTranspose _ ->
        evalArrayNode st env expr

    // ---- Rank-changing assembly (formalism 2.6): stack adds a fresh leading
    //      axis over same-shaped operands; join concatenates along a dimension.
    //      Both materialize a fresh dense pool by copying their operands,
    //      mirroring CodeGen's materialize{Stack,Join}Form.
    | IRStack _ | IRJoin _ ->
        evalArrayNode st env expr

    // ---- Symmetry producers: decompact fission, gram A.B^H, whole-array
    //      negate/conjugate. Like the eager set/reshape ops these MATERIALIZE a
    //      fresh array, so route to the Loops backend (mirrors CodeGen's
    //      materialize{Decompact,Gram,Negate/Conjugate}Form emitters).
    | IRDecompact _ | IRGram _ | IRMatmul _ | IRSolve _ | IRArrayNegate _ | IRArrayConjugate _ ->
        evalArrayNode st env expr

    // ---- eigh: same materializing family, but its value is a TUPLE (Q, LAM)
    //      rather than one array, so the Loops backend returns a VTuple here.
    //      Reachable only when the LAPACK gate was on at elaboration; gate
    //      off, `math.eigh` is synthesized Blade source and this node never exists.
    | IREigh _ ->
        evalArrayNode st env expr

    | other ->
        // Anything still uninterpreted (compound / group / provider forms, ...):
        // later milestones.
        raise (InterpUnsupported (nodeName other))

/// Reify a callable id as a closure value, snapshotting the current frame's
/// cells for each declared capture (by CaptureInfo.Id -- the free var's original
/// VarId). Top-level functions have no captures, so this yields an empty map.
and makeClosure (env: Env) (callable: IRCallable) : Value =
    let captures =
        callable.Captures
        |> List.choose (fun c ->
            match envTryFind env c.Id with
            | Some cell -> Some (c.Id, cell)
            | None -> None)
        |> Map.ofList
    VClosure (callable, captures)

/// Apply an IRApp. A variant constructor in function position builds a variant;
/// otherwise the callee evaluates to a closure and we invoke it.
and evalApp (st: InterpState) (env: Env) (func: IRExpr) (args: IRExpr list) : Value =
    match func with
    | IRParam (ctorName, _, ty) when isCtorName ctorName && (variantResultName ty).IsSome ->
        let tn = (variantResultName ty).Value
        let argVals = args |> List.map (evalExpr st env)
        let payload =
            match argVals with
            | [] -> None
            | [v] -> Some v
            | vs -> Some (VTuple (Array.ofList vs))   // multi-arg ctor: payload tuple
        VVariant (tn, hash ctorName, payload)
    | _ ->
        // Callee first, then arguments left-to-right. (C++ leaves argument
        // evaluation order unspecified; M0 operands are pure apart from panics,
        // so this only fixes which panic surfaces first.)
        let fv = evalExpr st env func
        let argVals = args |> List.map (evalExpr st env)
        applyValue st fv argVals

/// Apply an already-evaluated callee value. A composed function (`f >> g`,
/// held as a suspended IRCompose -- see the evalExpr arm) unwraps here:
/// CodeGen renders composition as `[&](auto... a){ return g(f(a...)); }`
/// (CodeGen.fs:1384), so application is g-of-f with f evaluated first; chains
/// (left-assoc IRCompose nests, Lowering.fs:335-341) recurse naturally.
and applyValue (st: InterpState) (fv: Value) (argVals: Value list) : Value =
    match fv with
    | VClosure (callable, captures) -> evalCall st callable captures argVals
    | VDeferred (IRCompose (fEx, gEx), denv) ->
        let inner = applyValue st (evalExpr st denv fEx) argVals
        applyValue st (evalExpr st denv gEx) [inner]
    | _ -> raise (InterpUnsupported "IRApp callee is not a callable")

/// Invoke a callable in a FRESH frame: captured cells bound by reference (so
/// mutation through the closure is visible to the definer), then parameters
/// bound positionally to the evaluated arguments. The frame has no parent --
/// a callable body reaches only its params, its captures, and other callables
/// (via the id->callable table), never the caller's locals.
and evalCall (st: InterpState) (callable: IRCallable) (captures: Map<IRId, ValueRef>) (args: Value list) : Value =
    st.Depth <- st.Depth + 1
    if st.Depth > st.Limits.MaxDepth then
        raise (InterpPanic ("BL8002", $"interpreter call depth budget exceeded ({st.Limits.MaxDepth})", None, 0))
    // Shadow-stack frame at body entry, named by the Blade function -- the SAME
    // string CodeGen pushes via BLADE_FRAME (CodeGen.fs:9342/9392). Pushed AFTER
    // the depth guard (an over-budget call never ran a C++ body, so it pushed no
    // frame) and NOT popped on an exception path: an escaping InterpPanic must
    // leave this frame live so Run.fs's formatPanic can print it, matching
    // blade_rt::panic reading the stack before exit.
    pushFrame st callable.Name
    // Chain the frame to the module-global scope: function bodies may read
    // module-level bindings directly (main-local capturing lambdas in C++).
    // Globally-unique IRIds make this exact -- no accidental shadowing.
    let frame =
        match st.Global with
        | Some g -> envChild g
        | None -> envNew ()
    captures |> Map.iter (fun id cell -> envBindRef frame id cell)
    let ps = callable.Params
    if List.length ps <> List.length args then
        raise (InterpPanic ("BL8002",
                            $"arity mismatch calling '{callable.Name}' (expected {(List.length ps)}, got {(List.length args)})",
                            None, 0))
    List.iter2 (fun (p: IRParam) (a: Value) -> envBind frame p.VarId a |> ignore) ps args
    let result = evalExpr st frame callable.Body
    popFrame st
    st.Depth <- st.Depth - 1
    result

/// First-match with guard fall-through. Each candidate case gets a fresh child
/// scope so that a pattern which partially binds and then fails (or whose guard
/// is false) leaves no bindings visible to later cases.
and evalMatch (st: InterpState) (env: Env) (sv: Value) (cases: IRMatchCase list) : Value =
    match cases with
    // BL8002 to match codegen and the LLVM lane byte-for-byte: all three
    // evaluators panic the SAME code and message on a non-exhaustive match,
    // so the differential gates can pin the event once. (This site used to
    // say BL8006 -- the out-of-bounds family -- and the interp-diff harness
    // caught the drift the day a corpus test pinned the abort.)
    | [] -> raise (InterpPanic ("BL8002", "Blade: non-exhaustive match", None, 0))
    | c :: rest ->
        let caseEnv = envChild env
        if tryMatch caseEnv sv c.Pattern then
            match c.Guard with
            | Some g ->
                if toBoolV (evalExpr st caseEnv g) then evalExpr st caseEnv c.Body
                else evalMatch st env sv rest
            | None -> evalExpr st caseEnv c.Body
        else evalMatch st env sv rest

/// Try to match `scrut` against `pat`, binding variables into `env` as a side
/// effect. Returns whether the pattern matched (bindings are only consumed on a
/// full match, so partial bindings on failure are inert -- the caller discards
/// this scope).
and tryMatch (env: Env) (scrut: Value) (pat: IRPattern) : bool =
    match pat with
    | IRPatWild -> true
    | IRPatVar id ->
        envBind env id scrut |> ignore
        true
    | IRPatLit lit -> litMatches lit scrut
    | IRPatTuple pats ->
        match scrut with
        | VTuple els when els.Length = List.length pats ->
            List.forall2 (fun p v -> tryMatch env v p) pats (List.ofArray els)
        // A struct pattern keeps its field names (IRPatStruct) and is
        // handled below; this arm is what a genuinely POSITIONAL pattern over
        // a struct scrutinee means, and it stays because the compiled lane
        // decodes the same shape the same way (by declaration order).
        | VStruct (_, fields) when fields.Length = List.length pats ->
            List.forall2 (fun p (_, v) -> tryMatch env v p) pats (List.ofArray fields)
        | _ -> false
    | IRPatStruct (_, fieldPats) ->
        // BY NAME, not by position: `Point { y, x }` binds y to the y field
        // however the struct declared its order. The type name is not
        // re-checked -- typecheck already fixed the scrutinee's type, and a
        // struct value carries no sum-type tag to discriminate on.
        match scrut with
        | VStruct (_, fields) ->
            fieldPats |> List.forall (fun (fname, fpat) ->
                match fields |> Array.tryFind (fun (n, _) -> n = fname) with
                | Some (_, v) -> tryMatch env v fpat
                | None -> false)
        | _ -> false
    | IRPatCons (hp, tp) ->
        match scrut with
        | VTuple els when els.Length >= 2 ->
            // Blade has no 1-tuple -- `(x)` IS `x` -- so a pair's remainder is
            // the BARE element, not a one-element tuple. That is the rule
            // Lowering's `subBindingValue` applies to `let h :: t` and the one
            // TypeCheck types the leaf by; handing back `VTuple [|x|]` here
            // made `h :: t` on a pair a type-lie no compiled lane could
            // reproduce. A 1-element scrutinee has no remainder at all, so
            // cons does not match it.
            let tail = if els.Length = 2 then els.[1] else VTuple els.[1..]
            tryMatch env els.[0] hp && tryMatch env tail tp
        | _ -> false
    | IRPatVariant (_, tag, innerOpt, _) ->
        // Dispatch by tag (= hash constructorName), matching construction above.
        match scrut with
        | VVariant (_, vtag, payload) when vtag = tag ->
            match innerOpt, payload with
            | None, _ -> true
            | Some ip, Some pv -> tryMatch env pv ip
            | Some ip, None -> tryMatch env VUnit ip
        | _ -> false

/// Write `v` through an assignment target's cell. Locals write their existing
/// cell (aliasing/closure-visible); a struct field mutates in place.
and evalAssign (st: InterpState) (env: Env) (target: IRExpr) (v: Value) : unit =
    match target with
    | LVVar id ->
        match envTryFind env id with
        | Some cell -> cell.V <- v
        | None -> envBind env id v |> ignore
    | LVField (obj, field) ->
        match evalExpr st env obj with
        | VStruct (_, fields) ->
            match fields |> Array.tryFindIndex (fun (n, _) -> n = field) with
            | Some i -> let (n, _) = fields.[i] in fields.[i] <- (n, v)
            | None -> raise (InterpPanic ("BL8003", $"no field '{field}' on struct", None, 0))
        | _ -> raise (InterpUnsupported "assignment to non-struct field target")
    | LVIndex (arrExpr, indices) ->
        // Array element assignment: force the target array to concrete form, then
        // write through the coordinate path (ArrayOps.writeCell mutates in place --
        // the C++ `arr[i][j]... = v` store).
        (match forceValue st env (evalExpr st env arrExpr) with
         | VArray ba ->
             let coords = indices |> List.map (fun e -> toI64 (evalExpr st env e))
             ArrayOps.writeCell ba coords v
         | _ -> raise (InterpUnsupported "IRAssign to non-array index target"))
    | LVOther _ -> raise (InterpUnsupported "IRAssign to non-lvalue target")

/// Build the CSR VGroupKeys for `group_keys(keys...)`. `ty` picks the bucketing
/// regime -- Case 1 positional (Idx<N>: bucket = key value), Case 2 EnumIdx
/// (bucket = enum-list position), Case 3 dynamic (first-appearance); multi-key:
/// dynamic tuple-hash. Mirrors genGroupKeysBinding's dispatch.
///
/// Only a top-level BINDING carries that type in a record, so evalBinding reads
/// it off `b.Type`; a function-body let has no record and its caller (evalExpr's
/// IRLet arm) recovers the type from the scope instead -- groupKeysTypeInScope,
/// the twin of what genFuncBodyScoped's group_keys arm does. Either way an
/// IRTUnit (nothing found) lands on dynamic discovery, which is also what every
/// un-annotated key array gets.
/// The structural grouping's value: offsets are the run boundaries, the
/// member permutation is the identity over the source extent.
and private segmentsValue (offsets: int64 list) : Value =
    let offs = Array.ofList offsets
    let n = int (Array.last offs)
    VGroupKeys { Offsets = offs; Members = Array.init n int64 }

and private buildGroupKeysValue (st: InterpState) (env: Env) (keys: IRExpr list) (ty: IRType) : Value =
    let keyArrs =
        keys |> List.map (fun k ->
            match forceValue st env (evalExpr st env k) with
            | VArray a -> a
            | _ -> raise (InterpUnsupported "group_keys: key operand is not an array"))
    let gkCase =
        if List.length keys > 1 then ArrayOps.GKDynamic
        else
            match ty with
            | IRTGroupKeys (outerIdx, _, enumValuesOpt) ->
                let ngroupsOpt =
                    match outerIdx.Extent with IRLit (IRLitInt n) -> Some (int n) | _ -> None
                match ngroupsOpt, enumValuesOpt with
                | None, _ -> ArrayOps.GKDynamic
                | Some ng, None -> ArrayOps.GKPositional ng
                | Some ng, Some values -> ArrayOps.GKEnum (values |> List.map enumValueToValue |> Array.ofList)
            | _ -> ArrayOps.GKDynamic
    VGroupKeys (ArrayOps.buildGroupKeys keyArrs gkCase)

// Defer-aware top-level binding driver (formalism 4).

/// Runtime guard for COMPUTED rows in an array-literal binding -- the value-space
/// twin of genArrayLiteral's per-copied-dim extent guard (CodeGen.fs:4383-4386):
///
///   if (src.extents[j] != n) { cerr << "Blade runtime: array literal row
///   [path] of 'name' has extent E in dim j, but the declared type expects n";
///   blade_rt::panic("BL8006", "array literal extent mismatch", nullptr, 0); }
///
/// Walks the literal EXPRESSION structure exactly as codegen's walkLeaves does
/// (row-major; recursing only through nested IRArrayLit children): a leaf at
/// a path SHORTER than the declared rank is a computed row whose remaining
/// dims must equal the DECLARED extents; the first mismatch panics BL8006.
/// Nested-literal rows are shape-checked statically and NOT guarded here. The
/// InterpPanic message carries the cerr line's content so the abort gate's
/// `// ABORT:` substring pins match.
let private checkArrayLitRowExtents (varName: string) (elements: IRExpr list) (arrType: IRArrayType) (arr: BladeArray) : unit =
    // Declared per-dim extents (multi-rank index types expand one entry per
    // rank component); None = non-literal extent, not guarded (codegen renders
    // the guard bound with %d from computeArrayDims' literal ints only).
    let declaredDims =
        arrType.IndexTypes
        |> List.collect (fun ix ->
            List.replicate (max 1 ix.Rank)
                (match ix.Extent with IRLit (IRLitInt n) -> Some n | _ -> None))
    let rank = declaredDims.Length
    let formatPath (path: int list) = path |> List.map (sprintf "[%d]") |> String.concat ""
    let rec walk (path: int list) (e: IRExpr) : unit =
        match e with
        | IRArrayLit (children, _) ->
            children |> List.iteri (fun i c -> walk (path @ [ i ]) c)
        | _ when path.Length < rank ->
            // Computed (array-valued) leaf: check its ACTUAL extents (read from
            // the materialized value at `path`) against the declared remainder.
            let subDims = declaredDims |> List.skip path.Length
            let mutable cur : Value = VArray arr
            for i in path do
                cur <- (match cur with VArray a -> ArrayOps.peelDim a (int64 i) | v -> v)
            (match cur with
             | VArray row ->
                 subDims |> List.iteri (fun j dOpt ->
                     match dOpt with
                     | Some n when j < row.Extents.Length && row.Extents.[j] <> n ->
                         raise (InterpPanic ("BL8006",
                                 $"array literal row {(formatPath path)} of '{varName}' has extent {row.Extents.[j]} in dim {j}, but the declared type expects {n}",
                                 None, 0))
                     | _ -> ())
             | _ -> ())
        | _ -> ()   // full-rank scalar leaf: no row to guard
    walk [] (IRArrayLit (elements, arrType))

/// Evaluate one top-level binding into a runtime value, reproducing
/// CodeGen.genBinding's defer-vs-materialize-vs-loop-object decision so the
/// interpreter's cell contents stay in lock-step with the C++ twin and Print:
///   * `method_for` / `object_for`        -> VLoopObj (pure provenance, no force);
///   * a combinator/compose/parallel/...  -> VDeferred(binding.Value, env), NOT
///     evaluated, exactly the DeferredComputations entry genBinding records;
///   * everything else -> eager `evalExpr` (scalars, structs, tuples, array
///     literals, IRCompute forces, reductions, ...), as in M0.
/// Run.execProgram calls this once per binding, in module order, so
/// shouldDeferBinding sees every earlier binding's cell already populated,
/// coinciding with computeDeferredIds' fold.
let evalBinding (st: InterpState) (env: Env) (b: IRBinding) : Value =
    match b.Value with
    | IRMethodFor _ | IRObjectFor _ ->
        VLoopObj { Provenance = b.Value; Captured = env }
    // group_keys is intercepted HERE (not in the generic eager path) because the
    // Case-1/2/3 bucketing needs the binding's declared IRTGroupKeys type, which
    // the bare IRGroupKeys node does not carry.
    | IRGroupKeys keys ->
        buildGroupKeysValue st env keys b.Type
    // segments(A): the structural grouping -- static offsets, identity
    // members (docs/plans/structural/07 §3.2; genSegmentsBinding's twin).
    | IRSegments (offsets, _) -> segmentsValue offsets
    | v when shouldDeferBinding env b.Type v ->
        VDeferred (b.Value, env)
    | _ ->
        // Force after eager evaluation: evalExpr's unconditional-defer arms
        // (e.g. a scalar `<|>` choice) can hand back VDeferred even when the
        // BINDING is classified eager -- the compiled binary materializes such a
        // binding inline, so the interpreter must too, or a scalar cell ends up
        // holding a suspension that Print (which never forces) chokes on.
        let value = forceValue st env (evalExpr st env b.Value)
        // Computed-row extent guard (genArrayLiteral, CodeGen.fs:4383-4386):
        // an array-literal binding whose row is a COMPUTED array (not a nested
        // literal) is copied against the DECLARED extents in C++, guarded per
        // dim at runtime -- annotation-vs-actual mismatches are not (yet)
        // rejected by unify, so the mismatch must be a loud BL8006 panic here
        // too, not a silently mis-shaped array (func-arrays T12 abort probe).
        (match b.Value, value with
         | IRArrayLit (elements, arrType), VArray arr ->
             let cppName = if b.Name.StartsWith "_(" then $"__tup_{b.Id}" else b.Name
             checkArrayLitRowExtents cppName elements arrType arr
         | _ -> ())
        // Copy semantics for assignable top-level array bindings whose
        // initializer references an existing array (`let U = Z`) -- the
        // differential twin of genVarAliasBinding's mut-copy path (every
        // non-static let is assignable, IRBinding.IsMutable). Ragged stores
        // keep the historical alias, matching the C++ dense-only guard.
        match b.Value, value with
        | IRVar _, VArray ba when (b.IsMutable || Set.contains b.Id st.MutableArrayLets)
                                  && isDenseCopyableArray ba ->
            VArray (copyBladeArray ba)
        | _ -> value

// Public call entry.

/// Invoke a callable with no closure captures -- the entry point Run.fs uses for
/// top-level functions (source-level functions never capture). Lifted lambdas
/// that DO capture are applied through evalExpr's IRApp path, which threads the
/// VClosure's captured cells into evalCall.
let callCallable (st: InterpState) (callable: IRCallable) (args: Value list) : Value =
    evalCall st callable Map.empty args
