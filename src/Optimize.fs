// The semantic-equivalence optimization layer.
//
// This module is the HOME for rewrites that change what a program COSTS
// without changing what it MEANS -- the compiler-side of "the fastest way is
// the only way". Rules of admission, each load-bearing:
//
//   1. COST ONLY, NEVER CONTRACT. A pass may derive an early exit, collapse
//      a loop, or fold a decided branch; it may never add or remove an
//      abort, a licence, or an observable value. (The `while` guard's
//      budget abort is a CONTRACT and therefore a surface spelling, not an
//      optimization -- see recognizeFreezeIdiom below for the boundary.)
//   2. TWIN-SAFE. Both back ends and the interpreter consume the rewritten
//      tree (passes run in Lowering, or pre-lowering where the equivalence
//      is only decidable there), so the differential gates hold by
//      construction; a pass that could not keep them byte-identical does
//      not belong here.
//   3. ESCAPABLE. Every pass carries a per-call environment gate
//      (BLADE_FUSION, BLADE_FREEZE_IDIOM, ...) so any rewrite can be A/B'd
//      against its absence. Gates are functions, never cached module lets
//      -- tests pin and restore them mid-process.
//   4. DECIDABLE AT ITS OWN SEAM. Most passes are IR->IR, but a recognition
//      whose evidence dissolves by IR time (the recursive-array freeze
//      idiom, whose declarative shape only exists on RecArrayDef) runs at
//      the last seam where it is exact. The layer is defined by the charter
//      above, not by a pipeline position.
//
// Implementations that PREDATE the layer live in IRMono for dependency
// reasons -- foldConstIntMatch is shared with the arity specializer (which
// needs it DURING specialization for recursion termination), and the fusion
// pass grew up beside the binop rewrite it runs after. `optimizeModule` is
// the single pipeline entry over them; new passes land in this file.
module Blade.Optimize

open Blade.Types

open Blade.Ast
open Blade.IR
open Blade.IRMono

/// BLADE_FREEZE_IDIOM=0|off disables freeze-idiom recognition (the A/B
/// escape hatch, read per call like every other gate).
let freezeIdiomEnabled () =
    match System.Environment.GetEnvironmentVariable "BLADE_FREEZE_IDIOM" with
    | null -> true
    | v ->
        match v.Trim().ToLowerInvariant() with
        | "0" | "off" | "false" -> false
        | _ -> true

// --- Freeze-idiom recognition (plan-match-statements.md section 5, R7/B) ---
//
// The hand-written convergence idiom on a recursive array's inductive arm:
//
//   | prefix :: n -> prefix :: (if G then STEP else prefix(n - 1))
//
// declares a fixed point: once G is false the slice repeats. When G's only
// per-iteration inputs are reads of prefix(n - 1), falseness is ABSORBING --
// the frozen slice reproduces exactly the inputs the guard just judged
// false, so it stays false by induction -- and the remaining iterations are
// provably copies. Rewriting the definition to the guarded form WITHOUT the
// abort (`Guard = Some G, Slice = STEP`, best-effort) then derives the early
// exit and the freeze epilogue from machinery that already exists, and the
// emitted values are byte-identical to running the budget out: the epilogue
// writes the same repeated slice the else-arm would have written, and every
// skipped guard evaluation is a repeat of one that already completed (G is
// pure surface arithmetic; its inputs no longer change).
//
// This is cost-only by construction, which is the R7 boundary: the `while`
// spelling OPTS INTO the must-converge contract (BL8010 when the budget
// runs out); the recognized idiom keeps if/else's contract -- run to
// budget, freeze if done early, never abort. Same analysis, same break,
// different contract, chosen by spelling.
//
// Soundness demands three shape checks, all CONSERVATIVE (any unrecognized
// node declines recognition rather than guessing):
//   - the else-arm is EXACTLY `prefix(n - 1)` (the whole previous slice --
//     an else that repairs, decays, or reads deeper lags is a live arm, not
//     a freeze);
//   - the guard's prefix reads are all at lag 1, and it references neither
//     the step ordinal outside those reads (a guard varying with `n`
//     independently of the trajectory is NOT absorbing: `n < k` flips on
//     its own) nor the bare prefix family;
//   - every call the guard makes is to a PURE scalar intrinsic (the caller
//     decides which names qualify -- see `calleeAdmissible` below). The
//     skipped guard evaluations are only repeats if evaluating the guard
//     changes nothing: a guard that calls a helper with a `mut` parameter
//     mutates its own input, so its first false answer is not absorbing
//     (plan-fortran-killer-2.md appendix A: `if tick(counter) then ...`
//     froze the trajectory at 1 and dropped six of seven `tick` calls), and
//     a helper that prints or aborts has effects the freeze would drop even
//     when its value would repeat. A user-declared function -- pure or not
//     -- therefore declines today: this seam sees names, not resolved
//     bodies, and an invariant NAME does not make a call repeatable. The
//     P0 follow-up (plan-fortran-killer-2.md section 3) is to keep the
//     candidate and discharge purity against the typed callee, which would
//     re-admit provably pure helpers.

/// `e` is syntactically `<stepVar> - 1`.
let private isStepMinusOne (stepVar: Ident) (e: Expr) : bool =
    match e.Kind with
    | ExprBinOp (Elementwise, OpSub, l, r) ->
        (match l.Kind, r.Kind with
         | ExprVar sv, ExprLit (LitInt 1L) -> sv = stepVar
         | _ -> false)
    | _ -> false

/// `e` is syntactically `prefix(<stepVar> - 1)` -- the whole previous slice.
let private isPrevSliceRead (prefixVar: Ident) (stepVar: Ident) (e: Expr) : bool =
    match e.Kind with
    | ExprApp (h, [arg]) ->
        (match h.Kind with
         | ExprVar pv -> pv = prefixVar && isStepMinusOne stepVar arg
         | _ -> false)
    | _ -> false

/// Guard admissibility: every read of the prefix is at lag 1, the step
/// ordinal appears ONLY inside those lag expressions, the prefix family is
/// never referenced bare, every ordinary call's head satisfies
/// `calleeAdmissible` (a REPEATABLE callee -- an unshadowed scalar intrinsic
/// or a declared function whose effect summary allows a repeat; the caller
/// supplies the judgment because the name and summary tables live in
/// TypeEnv, downstream of this file), and the whole guard is built from the
/// shapes a convergence predicate uses (literals, variables, arithmetic/
/// comparison/boolean operators, unary ops, applications, ascriptions).
/// Anything else -- a lambda, a block, a match -- declines recognition
/// conservatively. Returns None when admissible, else the FIRST reason it is
/// not, which the decision record carries.
let rec private guardInadmissible (calleeAdmissible: string -> string option)
                                  (prefixVar: Ident) (stepVar: Ident) (e: Expr) : string option =
    let check = guardInadmissible calleeAdmissible prefixVar stepVar
    let firstOf (xs: Expr list) = xs |> List.tryPick check
    match e.Kind with
    | ExprLit _ -> None
    | ExprVar v when v = stepVar -> Some "the guard reads the step ordinal outside a lag-1 prefix read (not absorbing: `n < k` flips on its own)"
    | ExprVar v when v = prefixVar -> Some "the guard references the prefix family bare"
    | ExprVar _ -> None
    | ExprBinOp (_, _, l, r) -> firstOf [l; r]
    | ExprUnaryOp (_, x) -> check x
    | ExprTyped (x, _) -> check x
    | ExprApp _ ->
        // Peel the application spine: `prefix(n-1)(j)(k)` is nested
        // ExprApps whose base head is the prefix var and whose FIRST
        // argument list carries the lag.
        let rec spine (f: Expr) (argLists: Expr list list) =
            match f.Kind with
            | ExprApp (h, args) -> spine h (args :: argLists)
            | _ -> f, argLists
        let baseHead, argLists = spine e []
        (match baseHead.Kind with
         | ExprVar pv when pv = prefixVar ->
             (match argLists with
              | (lagArg :: restFirst) :: deeper ->
                  if not (isStepMinusOne stepVar lagArg) then
                      Some "the guard reads the prefix at a lag other than 1"
                  else firstOf (restFirst @ List.concat deeper)
              | _ -> Some "the guard references the prefix family bare")
         | ExprVar fn when fn = stepVar -> Some "the guard applies the step ordinal"
         | ExprVar fn ->
             // An ordinary call. Evaluating a REPEATABLE callee again on the
             // same inputs gives the same value and changes nothing, so the
             // skipped evaluations really are repeats; the arguments carry
             // the lag discipline. Anything else -- a helper that mutates a
             // `mut` argument, prints, reads a file, or calls unknown code --
             // is refused with the caller's reason.
             (match calleeAdmissible fn with
              | Some why -> Some $"the guard calls `{fn}`, which is not repeatable: {why}"
              | None -> firstOf (List.concat argLists))
         | _ -> Some "the guard applies something other than a name")
    | _ -> Some "the guard contains a shape recognition does not read (a lambda, a block, or a match)"

/// Recognize the freeze idiom on an UNGUARDED recursive-array definition and
/// repartition it into the guarded best-effort form. Returns None (leave the
/// definition alone -- it still compiles and still means the same thing) for
/// anything that is not exactly the idiom.
///
/// `calleeAdmissible name` is the caller's judgment of a plain call to
/// `name`: None when it is repeatable (an unshadowed scalar intrinsic, or a
/// declared function whose Blade.Effects summary is repeatable), else the
/// reason it is not. The guard may call nothing else.
///
/// Every CANDIDATE -- an unguarded inductive arm whose slice is an `if` --
/// leaves a record in Blade.Effects.Decisions (rule `freeze-recognition`,
/// v2: v1 admitted callees by name), applied with its discharged
/// obligations or declined with the first reason. A slice that is not an
/// `if` is not a candidate and records nothing.
let recognizeFreezeIdiom (calleeAdmissible: string -> string option) (def: RecArrayDef) : RecArrayDef option =
    match def.Guard with
    | Some _ -> None
    | None ->
        match def.SliceExpr.Kind with
        | ExprIf (g, stepExpr, elseExpr) ->
            let decide (outcome: Blade.Effects.DecisionOutcome) (evidence: string list) =
                Blade.Effects.Decisions.record
                    { Blade.Effects.Rule = "freeze-recognition"; Version = 2
                      Span = def.SliceExpr.Span; Subject = def.Name
                      Outcome = outcome; Evidence = evidence }
            if not (freezeIdiomEnabled ()) then
                decide (Blade.Effects.Declined "disabled by BLADE_FREEZE_IDIOM") []
                None
            elif not (isPrevSliceRead def.PrefixVar def.StepVar elseExpr) then
                decide (Blade.Effects.Declined "the else-arm is not exactly `prefix(n - 1)` (a live arm, not a freeze)") []
                None
            else
                match guardInadmissible calleeAdmissible def.PrefixVar def.StepVar g with
                | Some why ->
                    decide (Blade.Effects.Declined why) []
                    None
                | None ->
                    decide Blade.Effects.Applied
                        [ "else-arm is `prefix(n - 1)`"
                          "guard reads the prefix at lag 1 only and never the step ordinal"
                          "every callee in the guard is repeatable"
                          "contract kept: best-effort freeze, no BL8010 budget abort" ]
                    Some { def with Guard = Some g; SliceExpr = stepExpr }
        | _ -> None

// --- Pipeline entry -------------------------------------------------------

/// The IR-level optimization stage, run per module in Lowering after the
/// monomorphizers and the array-binop rewrite, before inline-form lifting:
/// constant-scrutinee match folding (which also resolves symbolic ranks per
/// specialization), then elementwise-chain fusion. One entry point so the
/// pipeline reads as a stage, and so a new pass has one obvious place to
/// join.
/// The SEGMENT-STREAMING decision (docs/plans/structural/07 §3.4; the
/// principle the user stated: a computation over a streamed, segmented
/// variable is examined as a whole before `compute`, and the compiler --
/// not a task graph -- chooses between reading the store one run at a
/// time and materializing). Today the choice is by consumer SHAPE and is
/// recorded, not costed: a fold walks the store block by block in storage
/// order (bitwise the flat fold), a `group_by` under a structural grouping
/// reads one run per group, a key grouping needs the whole variable. The
/// emission lives in codegen; this pass only says what it will do, so
/// `blade plan` shows it.
/// The streamed variable an expression reads, looking through a lifted
/// kernel: the kernel of an apply is a reference to a callable in the
/// module's function table, and the source it reads is one of that
/// callable's CAPTURES (by the outer binding's id), not a var in the
/// expression itself.
let private streamedReadOf (modul: IRModule) (streamed: Map<Blade.Types.IRId, ProviderReadSpec>) (value: IRExpr) : ProviderReadSpec option =
    let mutable found = None
    iterIRExpr (fun e ->
        match e with
        | IRVar (vid, _) when Map.containsKey vid streamed -> found <- Some streamed.[vid]
        | IRVar (fid, _) ->
            (match modul.Functions |> List.tryFind (fun f -> f.Id = fid) with
             | Some f ->
                 (match f.Captures |> List.tryFind (fun c -> Map.containsKey c.Id streamed) with
                  | Some c -> found <- Some streamed.[c.Id]
                  | None -> ())
             | None -> ())
        | _ -> ()) value
    found

let private recordSegmentStreaming (modul: IRModule) : unit =
    let streamed =
        modul.ProviderReads
        |> Map.filter (fun _ s -> s.Streamed && not s.VarType.IndexTypes.IsEmpty)
    if not (Map.isEmpty streamed) then
        let structural =
            modul.Bindings
            |> List.choose (fun b -> match b.Value with IRSegments _ | IRSegmentsGrid _ -> Some b.Id | _ -> None)
            |> Set.ofList
        // The one number a cost model needs first: what materializing the
        // variable would hold in memory (literal extents only; a symbolic
        // extent says so). The choice itself stays by consumer shape.
        let materializedBytes (s: ProviderReadSpec) : string =
            let elem =
                match stripUnits s.VarType.ElemType with
                | IRTScalar (ETFloat64 | ETInt64) -> Some 8L
                | IRTScalar (ETFloat32 | ETInt32) -> Some 4L
                | IRTScalar ETComplex128 -> Some 16L
                | IRTScalar ETComplex64 -> Some 8L
                | IRTScalar ETBool -> Some 1L
                | _ -> None
            let cells =
                s.VarType.IndexTypes |> List.fold (fun acc ix ->
                    match acc, ix.Extent with
                    | Some a, IRLit (IRLitInt n) -> Some (a * n)
                    | _ -> None) (Some 1L)
            match elem, cells with
            | Some e, Some c -> $"materialized it would hold {e * c} B"
            | _ -> "materialized size not static"
        let decide (subject: string) (outcome: Blade.Effects.DecisionOutcome) (evidence: string list) =
            Blade.Effects.Decisions.record
                { Blade.Effects.Rule = "segment-streaming"; Version = 2
                  Span = Blade.Ast.noSpan; Subject = subject
                  Outcome = outcome; Evidence = evidence }
        for b in modul.Bindings do
            match b.Value with
            | IRReduce (IRVar (vid, _), _, _) when Map.containsKey vid streamed ->
                let s = streamed.[vid]
                decide b.Name Blade.Effects.Applied
                    [ $"fold over the streamed variable '{s.VarName}' walks the store one block at a time, in storage order: the same operation sequence as the flat fold, so the answer is bitwise the materialized one"
                      materializedBytes s ]
            | value when
                    (let mutable halo = false
                     iterIRExpr (fun e ->
                         match e with
                         | IRRange ([ ix ], _) when (match ix.Tag with Some t -> t.StartsWith Blade.Types.haloWinTagPrefix | None -> false) -> halo <- true
                         | _ -> ()) value
                     halo && (streamedReadOf modul streamed value).IsSome) ->
                let s = (streamedReadOf modul streamed value).Value
                decide b.Name Blade.Effects.Applied
                    [ $"the stencil over the streamed variable '{s.VarName}' runs one segment at a time; each run is read with the ghost cells its halo reach demands, nothing else of the variable is ever in memory"
                      materializedBytes s ]
            | value when
                    (let mutable found = false
                     iterIRExpr (fun e ->
                         match e with
                         | IRApplyCombinator info when info.Arrays |> List.exists (function IRVar (vid, _) -> Map.containsKey vid streamed | _ -> false) -> found <- true
                         | _ -> ()) value
                     found) ->
                // every apply in the binding (a zip nested under a reduce
                // included) whose OPERANDS are streamed is an elementwise
                // consumer run one block at a time
                iterIRExpr (fun e ->
                    match e with
                    | IRApplyCombinator info ->
                        (match info.Arrays |> List.tryPick (function IRVar (vid, _) when Map.containsKey vid streamed -> Some streamed.[vid] | _ -> None) with
                         | Some s ->
                             decide b.Name Blade.Effects.Applied
                                 [ $"the elementwise consumer of the streamed variable '{s.VarName}' runs one block of the store's chunk edge at a time (a band of rows above rank 1), each block its own window; nothing else of the variable is ever in memory"
                                   materializedBytes s ]
                         | None -> ())
                    | _ -> ()) value
            | IRGroupBy (IRVar (vid, _), IRVar (gid, _)) when Map.containsKey vid streamed ->
                let s = streamed.[vid]
                if Set.contains gid structural then
                    decide b.Name Blade.Effects.Applied
                        [ $"group_by over the streamed variable '{s.VarName}' under a structural grouping reads one run per group straight into its row; the whole variable is never materialized"
                          materializedBytes s ]
                else
                    decide b.Name (Blade.Effects.Declined "a key grouping needs every cell before any row is known")
                        [ $"'{s.VarName}' is streamed but the grouping is by keys; bind it with .read" ]
            | _ -> ()

/// SCRATCH REUSE ACROSS BARRIERS (plan-fortran-killer-2 section 4, gate 2).
///
/// Elementwise fusion removes the temporaries it can; the ones it cannot --
/// a whole-array dependency between two pointwise stages (`let y = ..; let m
/// = reduce(y, (+)); let z = y - m`) -- each allocate a fresh pool. In one
/// function body, once such a pool is DEAD (nothing after some point reads
/// it) and UNALIASED (no view, alias, capture-for-later, call argument or
/// assignment ever named it), a later fresh pool of the same element type and
/// the same literal extents can take it instead of allocating: the values
/// are untouched (a dead pool's bits are never read again), only the
/// allocation count and the peak live bytes change -- the optimizer's
/// charter. The plan is keyed by let id in `Types.PoolReuseTable`; codegen
/// renders the reuser's declaration as `{ donor.data, extents }`, skips its
/// scope free, and treats an escaping reuser as an escaping donor.
///
/// Admitted narrowly (each guard removes a way to be wrong, not a way to be
/// slow):
///  * both pools are `|> compute`d applies (`IRCompute (IRApplyCombinator)`)
///    of plain dense rank-1-slot storage with LITERAL extents and a scalar
///    element, and neither let is a `let mut` -- the one shape whose
///    declaration is one `arrayAlloc` line with a static extents table;
///  * the donor is never referenced by anything that could keep a handle on
///    its storage: every position that names it must be a fresh-pool form
///    (its cells are read, its storage is not retained) or scalar-typed; a
///    view, an alias let, a tuple, a CALL (a callee may hand back its
///    argument), an assignment, or a loop declines it;
///  * a DEFERRED let (a bare apply left for a join to consume) attributes
///    its reads to its consumers, and a callable's captures are reads the
///    callable performs when it runs, so liveness sees through both;
///  * the reuser does not read the donor (so the flat nest's restrict
///    pointers never alias), and no member of the donor's pool -- the root
///    and every earlier reuser of it -- is read at or after the reuser.
/// The RETURN position (`z * 0.5` as the body's value) is a reuser too: it
/// is the common tail of a pipeline and the largest single allocation saved.
/// Every function with two or more candidate pools leaves a decision (rule
/// `pool-reuse`): applied, with the pairs and the peak live bytes before and
/// after, or declined with the first blocker.
let private poolReuseElemBytes (t: IRType) : int64 option =
    match stripUnits t with
    | IRTScalar (ETFloat64 | ETInt64) -> Some 8L
    | IRTScalar (ETFloat32 | ETInt32) -> Some 4L
    | IRTScalar ETComplex128 -> Some 16L
    | IRTScalar ETComplex64 -> Some 8L
    | IRTScalar ETBool -> Some 1L
    | _ -> None

/// The reusable shape of a dense pool: (bare element type, literal extents).
let private densePoolShape (ty: IRType) : (IRType * int64 list) option =
    match ty with
    | ArrayElem at when not at.IndexTypes.IsEmpty
                        && at.IndexTypes |> List.forall (fun ix -> ix.IxKind = IxKPlain && ix.Symmetry = SymNone && ix.Rank = 1)
                        && (poolReuseElemBytes at.ElemType).IsSome ->
        let exts = at.IndexTypes |> List.map (fun ix -> match ix.Extent with IRLit (IRLitInt n) when n > 0L -> Some n | _ -> None)
        if exts |> List.forall Option.isSome then Some (stripUnits at.ElemType, exts |> List.map Option.get) else None
    | _ -> None

let private isFreshForm (e: IRExpr) : bool =
    let rec go e =
        match e with
        | IRCompute inner -> go inner
        | IRApplyCombinator _ | IRComposeApply _ | IRArrayLit _ -> true
        | IRMask _ | IRSort _ | IRUnique _ | IRIntersect _ | IRUnion _ -> true
        | IRTranspose _ | IRDecompact _ | IRStack _ | IRJoin _ | IRGram _ | IRGramApply _ | IRMatmul _ -> true
        | IREigh _ | IRSolve _ | IRLu _ | IRLuSolve _ | IRArrayNegate _ | IRArrayConjugate _ -> true
        | IRReduce _ | IRReduceCompute _ | IRProdSum _ -> true
        | _ -> false
    go e

let planPoolReuse (modul: IRModule) : unit =
    let funcs = modul.Functions |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    let rec unroll (e: IRExpr) : (IRId * IRExpr) list * IRExpr =
        match e with
        | IRLet (id, v, body) ->
            let (iv, vf) = unroll v
            let (rb, rf) = unroll body
            (match iv with
             | [] -> ((id, v) :: rb, rf)
             | _ -> (iv @ [ (id, vf) ] @ rb, rf))
        | _ -> ([], e)
    let rec collapse (e: IRExpr) = match e with IRCompute (IRCompute _ as i) -> collapse i | e -> e
    let materialized (v: IRExpr) =
        match collapse v with
        | IRCompute (IRApplyCombinator _) -> true
        | _ -> false
    let deferred (v: IRExpr) =
        match v with
        | IRApplyCombinator _ | IRComposeApply _ -> true
        | _ -> false
    let scalarValued (v: IRExpr) =
        match typeOf v with
        | IRTScalar _ | IRTUnit -> true
        | _ -> false
    let decide (subject: string) (outcome: Blade.Effects.DecisionOutcome) (evidence: string list) =
        Blade.Effects.Decisions.record
            { Blade.Effects.Rule = "pool-reuse"; Version = 1
              Span = Blade.Ast.noSpan; Subject = subject
              Outcome = outcome; Evidence = evidence }
    for f in modul.Functions do
        let (lets, ret) = unroll f.Body
        let letValues = Map.ofList lets
        // Reads a position performs, closed over deferred lets (their reads
        // happen at the consumer) and callable captures (the callable reads
        // them when it runs).
        let refsOf (e: IRExpr) : Set<IRId> =
            let mutable acc = Set.empty
            let work = System.Collections.Generic.Stack<IRId>()
            for r in collectVarRefsIR e do work.Push r
            while work.Count > 0 do
                let id = work.Pop()
                if not (Set.contains id acc) then
                    acc <- Set.add id acc
                    (match Map.tryFind id funcs with
                     | Some g -> for c in g.Captures do work.Push c.Id
                     | None -> ())
                    (match Map.tryFind id letValues with
                     | Some dv when deferred dv -> for r in collectVarRefsIR dv do work.Push r
                     | _ -> ())
            acc
        let n = lets.Length
        let positions = (lets |> List.map (fun (id, v) -> (Some id, v))) @ [ (None, ret) ]
        let refsAt = positions |> List.map (fun (_, v) -> refsOf v) |> Array.ofList
        let valueAt = positions |> List.map snd |> Array.ofList
        // Candidate pools: (position, let id option, shape, bytes).
        let candidateAt (k: int) =
            let (idOpt, v) = positions.[k]
            // A body-level `let` is reassignable in its own scope, so the
            // MutableArrayLets side table names every array let; what matters
            // here is whether an ASSIGNMENT ever names the pool (assignedIds).
            let ok =
                (match idOpt with
                    | Some _ -> materialized v
                    | None -> (match collapse v with IRCompute (IRApplyCombinator _) | IRApplyCombinator _ -> true | _ -> false))
            if not ok then None
            else
                match densePoolShape (typeOf (collapse v)) with
                | Some (elem, exts) ->
                    let bytes = (poolReuseElemBytes elem |> Option.defaultValue 0L) * (exts |> List.fold (*) 1L)
                    Some (elem, exts, bytes)
                | None -> None
        let candidates = [ for k in 0 .. n -> (k, candidateAt k) ] |> List.choose (fun (k, c) -> c |> Option.map (fun c -> (k, c)))
        if candidates.Length >= 2 then
            // A donor's storage must never be retained by anyone: every
            // position naming it is a fresh-pool form or scalar-typed, and no
            // assignment anywhere names it.
            let assignedIds =
                let mutable s = Set.empty
                for (_, v) in positions do
                    iterIRExpr (fun e ->
                        match e with
                        | IRAssign (t, rhs) ->
                            (match t with LVVar tid -> s <- Set.add tid s | _ -> ())
                            s <- Set.union s (collectVarRefsIR rhs)
                            (match t with LVVar _ -> () | _ -> s <- Set.union s (collectVarRefsIR t))
                        | _ -> ()) v
                s
            // A call to a declared or lifted callable that names the donor may
            // retain it (hand it back, or alias it into a module-level mut),
            // whatever the call's own type; a fresh-pool form or a call-free
            // scalar only READS it.
            let callsCallable (v: IRExpr) =
                let mutable found = false
                iterIRExpr (fun e ->
                    match e with
                    | IRApp (IRVar (fid, _), _, _) when Map.containsKey fid funcs -> found <- true
                    | _ -> ()) v
                found
            let unaliased (k: int) (a: IRId) =
                not (Set.contains a assignedIds)
                && [ 0 .. n ] |> List.forall (fun p ->
                    p = k
                    || not (Set.contains a refsAt.[p])
                    || (match positions.[p] with
                        | (Some _, pv) when deferred pv -> true      // reads attributed to its consumers
                        | (_, pv) -> not (callsCallable pv) && (isFreshForm pv || scalarValued pv)))
            // Greedy in program order. `pools`: root position -> members
            // (positions) sharing its storage; `owner`: position -> root.
            let pools = System.Collections.Generic.Dictionary<int, int list>()
            let owner = System.Collections.Generic.Dictionary<int, int>()
            let idAt (p: int) = fst positions.[p]
            let deadFrom (members: int list) (j: int) =
                // no member is read at or after position j
                [ j .. n ] |> List.forall (fun p ->
                    members |> List.forall (fun m ->
                        match idAt m with
                        | Some mid -> not (Set.contains mid refsAt.[p])
                        | None -> true))
            let pairs = ResizeArray<int * int>()
            for (k, (elem, exts, _)) in candidates do
                let donor =
                    candidates
                    |> List.filter (fun (r, (relem, rexts, _)) ->
                        r < k && relem = elem && rexts = exts
                        && pools.ContainsKey r
                        && (match idAt r with Some a -> unaliased r a | None -> false)
                        && deadFrom pools.[r] k)
                    |> List.tryHead
                match donor with
                | Some (r, _) ->
                    pools.[r] <- pools.[r] @ [ k ]
                    owner.[k] <- r
                    pairs.Add((k, r))
                | None ->
                    pools.[k] <- [ k ]
                    owner.[k] <- k
            let nameOf (p: int) = match idAt p with Some id -> $"__v{id}" | None -> "the return"
            let bytesOf (p: int) = candidates |> List.tryPick (fun (q, (_, _, b)) -> if q = p then Some b else None) |> Option.defaultValue 0L
            // Peak live pool bytes: each pool live from its first member's
            // definition to its members' last read.
            let lastRead (p: int) =
                match idAt p with
                | None -> n
                | Some id -> [ p .. n ] |> List.filter (fun q -> Set.contains id refsAt.[q]) |> List.fold max p
            let peakOf (groups: (int * int list) list) =
                let intervals = groups |> List.map (fun (root, members) -> (root, members |> List.map lastRead |> List.fold max root, bytesOf root))
                [ 0 .. n ] |> List.map (fun pos -> intervals |> List.sumBy (fun (lo, hi, b) -> if lo <= pos && pos <= hi then b else 0L)) |> List.fold max 0L
            let before = peakOf (candidates |> List.map (fun (k, _) -> (k, [ k ])))
            let after = peakOf (pools |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> List.ofSeq)
            if pairs.Count > 0 then
                for (k, r) in pairs do
                    match idAt k with
                    | Some id -> Blade.Types.PoolReuseTable.record id (idAt r).Value
                    | None -> Blade.Types.PoolReuseTable.recordReturn f.Id (idAt r).Value
                decide f.Name Blade.Effects.Applied
                    ((pairs |> List.ofSeq |> List.map (fun (k, r) -> $"{nameOf k} takes {nameOf r}'s dead pool ({bytesOf k} B)"))
                     @ [ $"pools {candidates.Length} -> {pools.Count}; peak live pool bytes {before} -> {after} (literal extents only)" ])
            else
                decide f.Name (Blade.Effects.Declined "no candidate pool is both dead and unaliased when a pool of the same shape is allocated")
                    [ $"{candidates.Length} candidate pools, peak live pool bytes {before}" ]

/// A kernel reference resolved through the module's own function table
/// (the AsyncLocal CallablesTable is not installed at this point in the
/// pipeline, as the fusion pass notes).
let private resolveCallableIn (funcs: Map<IRId, IRCallable>) (k: IRExpr) : IRCallable option =
    match k with
    | IRVar (fid, _) -> Map.tryFind fid funcs
    | _ -> None

/// LET-LEVEL COMMON-SUBEXPRESSION ELIMINATION over REPEATABLE values -- the
/// P0 consumer plan-fortran-killer-2 section 3 listed ("reuse the facts for
/// ... later CSE") and did not build. Straight-line only: inside one
/// function body, a let whose value is structurally identical to an EARLIER
/// let's value is dropped and every later reference reads the earlier one.
/// Equality of values is equality of results only when the value is
/// REPEATABLE (no assignment, no display, no unknown call -- a called
/// callable's `Blade.Effects` summary is the fact, exactly as the fusion
/// pass reads it) and nothing between the two evaluations changed an input:
/// a body containing any assignment or loop declines wholesale, which is
/// cheap and sound. A MayFail value is fine: the first evaluation already
/// ran, so the second could not newly fail. Deferred lets (a bare apply left
/// for a join) are never touched -- their identity is the join's sharing
/// declaration, and dropping one would change what the join emits. Trivial
/// values (a literal, a variable) are not worth a record. Every body with a
/// hit records `cse` (applied, the pairs); nothing otherwise.
let private cseValueRepeatable (funcs: Map<IRId, IRCallable>) (v: IRExpr) : bool =
    let mutable ok = true
    iterIRExpr (fun e ->
        if ok then
            match e with
            | IRDisplayEmit _ | IRAssign _ | IRForRange _ -> ok <- false
            | IRApp (IRVar (fid, _), _, _) ->
                (match Map.tryFind fid funcs with
                 | Some callee -> if callee.IsStatic || not (Blade.Effects.isRepeatable callee.Effects) then ok <- false
                 | None -> ok <- false)
            | IRApp _ -> ok <- false
            | _ -> ()) v
    ok

let cseModule (modul: IRModule) : IRModule =
    let funcs = modul.Functions |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    // Two references to callables are the SAME value when the callables are
    // structurally identical: same parameter types, same captures, and the
    // same body once each one's own parameter ids are replaced by their
    // positions. Every `(+)` section and every inline lambda is lifted to
    // its own callable, so two identical folds name different ids; this is
    // the identity the comparison needs. `canonId` maps a callable to the
    // first structurally identical one (by id order).
    let canonKey (f: IRCallable) : string =
        let subst = f.Params |> List.mapi (fun i p -> (p.VarId, -(i + 1))) |> Map.ofList
        let body =
            mapIRExpr (fun e ->
                match e with
                | IRVar (id, t) when Map.containsKey id subst -> IRVar (subst.[id], t)
                | _ -> e) f.Body
        sprintf "%A|%A|%A" (f.Params |> List.map (fun p -> p.Type)) (f.Captures |> List.map (fun c -> c.Id)) body
    let canonId : Map<IRId, IRId> =
        let byKey = System.Collections.Generic.Dictionary<string, IRId>()
        modul.Functions
        |> List.sortBy (fun f -> f.Id)
        |> List.map (fun f ->
            let k = canonKey f
            match byKey.TryGetValue k with
            | true, first -> (f.Id, first)
            | _ -> byKey.[k] <- f.Id; (f.Id, f.Id))
        |> Map.ofList
    let canon (e: IRExpr) : IRExpr =
        mapIRExpr (fun n ->
            match n with
            | IRVar (id, t) when Map.containsKey id canonId && canonId.[id] <> id -> IRVar (canonId.[id], t)
            | _ -> n) e
    let rec unroll (e: IRExpr) : (IRId * IRExpr) list * IRExpr =
        match e with
        | IRLet (id, v, body) ->
            let (iv, vf) = unroll v
            let (rb, rf) = unroll body
            (match iv with
             | [] -> ((id, v) :: rb, rf)
             | _ -> (iv @ [ (id, vf) ] @ rb, rf))
        | _ -> ([], e)
    let trivial (v: IRExpr) =
        match v with
        | IRLit _ | IRVar _ | IRParam _ -> true
        | IRApplyCombinator _ | IRComposeApply _ -> true   // deferred: a join's sharing declaration
        | _ -> false
    let hasBarrier (e: IRExpr) =
        let mutable found = false
        iterIRExpr (fun n -> match n with IRAssign _ | IRForRange _ -> found <- true | _ -> ()) e
        found
    let moduleSubst = System.Collections.Generic.Dictionary<IRId, IRId>()
    let rewriteBody (f: IRCallable) : IRCallable =
        let (lets, ret) = unroll f.Body
        if lets.Length < 2 || hasBarrier f.Body then f
        else
            let subst = System.Collections.Generic.Dictionary<IRId, IRId>()
            let applySubst (e: IRExpr) =
                if subst.Count = 0 then e
                else
                    mapIRExpr (fun n ->
                        match n with
                        | IRVar (id, t) when subst.ContainsKey id -> IRVar (subst.[id], t)
                        | _ -> n) e
            let kept = ResizeArray<IRId * IRExpr>()
            let pairs = ResizeArray<IRId * IRId>()
            for (id, v0) in lets do
                let v = applySubst v0
                let dup =
                    if trivial v || not (cseValueRepeatable funcs v) then None
                    else
                        let cv = canon v
                        kept |> Seq.tryFind (fun (_, kv) -> canon kv = cv) |> Option.map fst
                match dup with
                | Some earlier ->
                    subst.[id] <- earlier
                    pairs.Add((id, earlier))
                | None -> kept.Add((id, v))
            if pairs.Count = 0 then f
            else
                let ret' = applySubst ret
                let body' = Seq.foldBack (fun (id, v) acc -> IRLet (id, v, acc)) kept ret'
                // A dropped let may be CAPTURED by a kernel lambda lifted out of
                // this body: that callable's capture list and body name the
                // dropped id, and codegen forwards captures by name, so both
                // are rewritten too (ids are program-global, so this is exact).
                for (j, i) in pairs do moduleSubst.[j] <- i
                Blade.Effects.Decisions.record
                    { Blade.Effects.Rule = "cse"; Version = 1
                      Span = Blade.Ast.noSpan; Subject = f.Name
                      Outcome = Blade.Effects.Applied
                      Evidence = pairs |> Seq.map (fun (j, i) -> $"__v{j} is the same repeatable value as __v{i}: dropped, its reads go to __v{i}") |> List.ofSeq }
                { f with Body = body' }
    let rewritten = modul.Functions |> List.map rewriteBody
    if moduleSubst.Count = 0 then { modul with Functions = rewritten }
    else
        let fix (e: IRExpr) =
            mapIRExpr (fun n ->
                match n with
                | IRVar (id, t) when moduleSubst.ContainsKey id -> IRVar (moduleSubst.[id], t)
                | _ -> n) e
        { modul with
            Functions =
                rewritten |> List.map (fun g ->
                    { g with
                        Body = fix g.Body
                        Captures = g.Captures |> List.map (fun c -> if moduleSubst.ContainsKey c.Id then { c with Id = moduleSubst.[c.Id] } else c) }) }

/// The structural/05 D7 ADVISORY, never a rewrite: the checker/optimizer sees
/// `gram(A, A)` decompacted and applied to a vector by a row `prodsum` -- an
/// N x N matrix formed and read once -- and records that `gram_apply(A, A,
/// v)` declares the same action without the matrix. Recorded as a DECLINED
/// decision ("left as written") so `blade plan` shows it beside the others;
/// the fastest-way P3 advisory channel, when it exists, can lift it.
let private recordGramApplyAdvisory (modul: IRModule) : unit =
    let funcs = modul.Functions |> List.map (fun f -> (f.Id, f)) |> Map.ofList
    let values = modul.Bindings |> List.map (fun b -> (b.Id, b)) |> Map.ofList
    let rec collapse (e: IRExpr) = match e with IRCompute i -> collapse i | e -> e
    for b in modul.Bindings do
        match collapse b.Value with
        | IRApplyCombinator info ->
            (match info.Arrays, resolveCallableIn funcs info.Kernel with
             | [ IRVar (gdId, _) ], Some k ->
                // prodsum's two operands arrive in either order (the checker
                // may canonicalize them): one is the row parameter, the other
                // the vector.
                let rowAndVec (p: IRParam) (body: IRExpr) =
                    match body with
                    | IRProdSum [ IRVar (a, _); IRVar (b, _) ] when a = p.VarId -> Some b
                    | IRProdSum [ IRVar (a, _); IRVar (b, _) ] when b = p.VarId -> Some a
                    | _ -> None
                (match Map.tryFind gdId values, k.Params, k.Params |> List.tryHead |> Option.bind (fun p -> rowAndVec p k.Body) with
                 | Some gd, [ _ ], Some vid ->
                    (match gd.Value with
                     | IRDecompact (IRVar (gId, _), 0) ->
                        (match Map.tryFind gId values with
                         | Some g ->
                            (match g.Value with
                             | IRGram (IRVar (aId, _), IRVar (a2, _), true) when aId = a2 ->
                                let nameOf id = modul.Bindings |> List.tryFind (fun x -> x.Id = id) |> Option.map (fun x -> x.Name) |> Option.defaultValue $"__v{id}"
                                Blade.Effects.Decisions.record
                                    { Blade.Effects.Rule = "gram-apply-advisory"; Version = 1
                                      Span = Blade.Ast.noSpan; Subject = b.Name
                                      Outcome = Blade.Effects.Declined $"left as written (advise, never rewrite): `gram_apply({nameOf aId}, {nameOf aId}, {nameOf vid})` declares the same action without the N x N pool"
                                      Evidence = [ $"`{b.Name}` forms the Gram matrix of `{nameOf aId}` (gram, decompact) and applies it once by a row prodsum against `{nameOf vid}`; `gram_apply({nameOf aId}, {nameOf aId}, {nameOf vid})` declares the same action with two rank-1 temporaries and no N x N pool (docs/plans/structural/05)" ] }
                             | _ -> ())
                         | None -> ())
                     | _ -> ())
                 | _ -> ())
             | _ -> ())
        | _ -> ()

let optimizeModule (builder: IRBuilder) (modul: IRModule) : IRModule =
    recordGramApplyAdvisory modul
    recordSegmentStreaming modul
    modul
    |> foldConstMatchesModule
    |> (fun m -> fuseElementwiseChainsModule m builder)
