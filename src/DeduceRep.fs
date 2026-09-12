/// Typecheck-resident representation-status deduction -- the fourth lattice
/// made typed, sibling of Deduce.fs (parity/sign). MLEquiv remains the
/// CHECKING and EMITTING authority; proposals ride TypedCertProposals,
/// consumed only by tests/Test_RepDifferential.fs until the parity gate
/// (typed recall >= seam recall, zero false proposals). Compile order:
/// after TypedAst/Deduce, before StaticEval; nothing here references TypeEnv.
module Blade.DeduceRep

open Blade.Ast
open Blade.Types
open Blade.IR
open Blade.TypedAst

/// A typed-deduction certificate proposal -- the structured twin of the
/// seam's suggestion strings: who, which group, the rendered signature
/// (seam vocabulary, for meaningful string comparison), dependency closure.
type RepProposal = {
    Owner: string
    /// "O3" | "SO3" | "<g>" -- seam's groupStr vocabulary: a point group
    /// renders as its BARE registry label ("C4"), never "Point C4"
    /// (MLEquiv.groupStr: `| Point n -> n`), matched against groupStr.
    Group: string
    /// Rendered like the seam's sigSummary, for differential comparison.
    Signature: string
    /// Dependency closure (unpinned helpers this proposal rests on), decl order.
    Deps: string list
}

/// Internal channel between the typed walker (producer) and the
/// differential harness (consumer). Not surfaced to users -- BL4011 stays
/// the seam's to emit until the parity gate. AsyncLocal, reset/add/get.
module TypedCertProposals =
    let private slot = new System.Threading.AsyncLocal<(RepProposal * Span) list>()
    let reset () = slot.Value <- []
    let add (p: RepProposal) (span: Span) = slot.Value <- (p, span) :: slot.Value
    let get () : (RepProposal * Span) list =
        match box slot.Value with null -> [] | _ -> List.rev slot.Value

// Status lattice: typed twin of MLEquiv's {Rep, Inv of InvShape, Opaque},
// plus one element the seam doesn't need because it lives in a Result:
//   TRep s  -- transforms via block-spec `s`; the only status with a THEOREM.
//   TInv sh -- held FIXED by the action; `sh` records provable
//              0-dimensionality, load-bearing only for scaling a rep.
//   TOpaque -- nothing established; propagates, never manufactures a claim.
//   TBottom -- not judgeable here: no proposal (MLEquiv's `Error`, dropped).
// PBottom (Deduce.fs): a rule that cannot PROVE its conclusion answers
// TBottom (or TOpaque), never a status.

/// WHICH block-spec family describes a value's transformation law. The two
/// cases never meet: a certificate names one group.
type RepSpecT =
    /// O(3)/SO(3) irreps, (l, parity, mult) triples in spec order -- the
    /// payload `Types.mkIrrepsTag` serializes.
    | TO3Spec of (int * int * int) list
    /// Point-group irreps: group name plus (LABEL, mult) entries -- the
    /// payload `Types.mkPgIrrepsTag` serializes.
    | TPgSpecT of group: string * entries: (string * int) list

/// Shape refinement carried by `TInv`. `rep * c` is equivariant when `c` is
/// SCALAR (commutes with every block); `rep * w` for an invariant ARRAY
/// scales each component independently -- a diagonal matrix with unequal
/// entries does not commute with D^l.
type InvShapeT =
    /// Provably 0-dimensional.
    | TInvScalar
    /// Provably an aggregate; `Some r` when the rank is known.
    | TInvAgg of rank: int option
    /// Shape not established -- treated as non-scalar wherever scalarity is
    /// load-bearing.
    | TInvShapeUnknown

type RepStatusT =
    | TRep of RepSpecT
    | TInv of InvShapeT
    | TOpaque
    | TBottom

/// WHY a walk declined, and where. `TBottom` stays payload-free: it is
/// compared structurally at a dozen sites shared with two other
/// disciplines that must not carry a cause. The reason instead rides
/// BESIDE the lattice, in a first-write-wins slot on `RepCtx`, written only
/// by the site that ORIGINATES a decline -- so the DEEPEST reason survives.
/// ANALYSIS, NOT CHECKING: nothing reads this slot to decide a verdict; it
/// only refines abstention-census reason strings.
type DeclineCause = {
    /// One sentence, in the walker's own vocabulary, not the seam's --
    /// checking stays at the seam and the two must not compare as one text.
    Why: string
    /// The offending sub-expression. `None` for the two rule sites the
    /// generic kit calls without one (`CovAppliedAsCallee`,
    /// `FormerConclusion`); widening `StructRules` to carry a span would
    /// touch a record three disciplines share, not worth the nicety.
    Where: Span option
}

/// The group hypothesis. Index types are GROUP-LESS: the candidate ladder
/// supplies it and the walker carries it.
type GroupT =
    | GO3
    | GSO3
    /// A registered point group, by MLPointSpec registry name (C4, D4).
    | GPoint of string

/// A function's certified-or-deduced rep signature: per-parameter status in
/// declaration order plus the return status, all classified from ZONKED types.
type RepSigT = {
    Owner: string
    Group: GroupT
    Params: (string * RepStatusT) list
    Return: RepStatusT
}

// Rendering -- BYTE-COMPATIBLE with the seam, since the differential
// compares strings.

/// Mirror of MLEquiv.groupStr: a point group renders as its bare registry
/// name (`C4`), not `Point C4`.
let groupStrT (g: GroupT) : string =
    match g with GO3 -> "O3" | GSO3 -> "SO3" | GPoint n -> n

/// Parse a group name as written in an `__ml_equiv` conjunct argument.
let groupOfName (n: string) : GroupT =
    match n with "O3" -> GO3 | "SO3" -> GSO3 | other -> GPoint other

let private specStrT (s: (int * int * int) list) : string =
    s
    |> List.map (fun (l, p, m) -> $"({l}, {p}, {m})")
    |> String.concat ", "
    |> sprintf "[%s]"

let private pgSpecStrT (s: (string * int) list) : string =
    s
    |> List.map (fun (label, m) -> $"(\"{label}\", {m})")
    |> String.concat ", "
    |> sprintf "[%s]"

/// Mirror of MLEquiv.repStr -- the INDEX TYPE the user would have to write.
let repStrT (r: RepSpecT) : string =
    match r with
    | TO3Spec s -> $"IrrepsIdx<{specStrT s}>"
    | TPgSpecT (g, s) -> $"PgIrrepsIdx<{g}, {pgSpecStrT s}>"

/// Mirror of MLEquiv.sigSummary, phrase for phrase -- this string is the
/// differential's comparison key.
let sigSummaryT (sg: RepSigT) : string =
    let one (n, st) =
        match st with
        | TRep r -> $"{n} transforms as {repStrT r}"
        | TInv _ -> $"{n} invariant"
        | _ -> $"{n} unclassifiable"
    let ps =
        if sg.Params.IsEmpty then "(no parameters)"
        else sg.Params |> List.map one |> String.concat ", "
    let ret =
        match sg.Return with
        | TRep r -> repStrT r
        | TInv _ -> "invariant"
        | _ -> "unclassifiable"
    $"{ps} -> {ret}"

// Lattice algebra (mirrors of MLEquiv's binShape / meetShape / joinStatus).

let private isInvT (s: RepStatusT) = s.IsTInv
let private isRepT (s: RepStatusT) = s.IsTRep

/// Shape of an elementwise binary combination (broadcast: scalar op aggregate
/// is the aggregate). Never claims scalar unless BOTH operands are scalar.
let private binShapeT (a: InvShapeT) (b: InvShapeT) : InvShapeT =
    match a, b with
    | TInvScalar, TInvScalar -> TInvScalar
    | TInvAgg r, TInvScalar | TInvScalar, TInvAgg r -> TInvAgg r
    | TInvAgg r1, TInvAgg r2 -> TInvAgg (if r1 = r2 then r1 else None)
    | _ -> TInvShapeUnknown

/// Meet of two shapes reached on different control-flow paths.
let private meetShapeT (a: InvShapeT) (b: InvShapeT) : InvShapeT =
    if a = b then a
    else
        match a, b with
        | TInvAgg _, TInvAgg _ -> TInvAgg None
        | _ -> TInvShapeUnknown

/// Merge two statuses reached on different control-flow paths. `None` = the
/// paths disagree, which the caller turns into TBottom (the seam rejects).
let private joinStatusT (a: RepStatusT) (b: RepStatusT) : RepStatusT option =
    match a, b with
    | TBottom, _ | _, TBottom -> None
    | TRep s1, TRep s2 when s1 = s2 -> Some (TRep s1)
    | TInv x, TInv y -> Some (TInv (meetShapeT x y))
    | TOpaque, TOpaque -> Some TOpaque
    | _ -> None

/// Statuses agree for certification purposes. Invariant SHAPE is deliberately
/// not part of the comparison -- it exists only to decide the scaling rule.
let private statusAgreesT (a: RepStatusT) (b: RepStatusT) : bool =
    (joinStatusT a b) |> Option.isSome

// The TYPED CLASSIFIER -- IRType -> RepStatusT. Typed twin of
// MLEquiv.statusOfType: reads the block-spec tag off `IRIndexTypeG.Tag`
// (`IrrepsTag`/`PgIrrepsTag`, Types.fs) rather than a surface `TypeExpr`,
// so an unannotated parameter classifies exactly like an annotated one.
// TAG FORMATS (Types.mkIrrepsTag / mkPgIrrepsTag):
//   "__irreps:<alias>:<l,p,m|l,p,m|...>"
//   "__pgirreps:<GROUP>:<alias>:<LABEL,mult|LABEL,mult|...>"
// Prefixes are disjoint, so tag equality decides identity for free; the
// alias is dropped as diagnostic sugar.

/// The O(3) spec riding an index slot's tag, or None.
let private irrepsOf (ix: IRIndexType) : (int * int * int) list option =
    match ix.Tag with
    | Some (IrrepsTag (_alias, triples)) -> Some triples
    | _ -> None

/// The point-group (group, spec) riding an index slot's tag, or None.
let private pgIrrepsOf (ix: IRIndexType) : (string * (string * int) list) option =
    match ix.Tag with
    | Some (PgIrrepsTag (group, _alias, entries)) -> Some (group, entries)
    | _ -> None

/// Provable 0-dimensionality of a type. `IRTScalar` IS 0-dimensional, which
/// the surface classifier had to guess from builtin names; the typed
/// lattice reads it directly -- deliberately STRONGER than the seam,
/// soundly: it only turns "unknown shape" into "provably scalar".
let rec private shapeOfType (resolve: IRType -> IRType) (ty: IRType) : InvShapeT =
    match resolve ty with
    | IRTScalar _ -> TInvScalar
    | IRTUnitAnnotated (inner, _) -> shapeOfType resolve inner
    | IRTIdxTagged (inner, _) -> shapeOfType resolve inner
    | ArrayElem arr -> TInvAgg (Some (List.length arr.IndexTypes))
    | _ -> TInvShapeUnknown

/// Classify a ZONKED type under a group hypothesis. The group SELECTS the
/// live index family: under O3/SO3 that is `IrrepsIdx` (a `PgIrrepsIdx`
/// buffer is an ordinary invariant); under `Point g` it is
/// `PgIrrepsIdx<g, _>`. An `IrrepsIdx` slot, or another group's
/// `PgIrrepsIdx`, refuses (TOpaque -- the seam's `Error`); `Error why` IS
/// `TOpaque`, carrying the reason as payload. TOpaque means two things by
/// position: in EXPRESSION position it is an absence that propagates
/// harmlessly (`classifyType` below); in SIGNATURE position it is a
/// REFUSAL that skips the whole function (`classifySignature` reads that
/// reason; no verdict depends on it).
let rec classifyTypeR (g: GroupT) (resolve: IRType -> IRType) (ty: IRType)
    : Result<RepStatusT, string> =
    match resolve ty with
    | IRTScalar _ -> Ok (TInv TInvScalar)
    | IRTUnitAnnotated (inner, _) -> classifyTypeR g resolve inner
    | IRTIdxTagged (inner, _) -> classifyTypeR g resolve inner
    | ArrayElem arr ->
        let idxs = arr.IndexTypes
        let n = List.length idxs
        let irreps = idxs |> List.choose irrepsOf
        let pgs = idxs |> List.choose pgIrrepsOf
        match g with
        | GPoint pg ->
            match irreps, pgs with
            // An O(3) axis under a point-group cert needs `ml.restrict`'s
            // branching rules to say anything: decline rather than guess.
            | _ :: _, _ ->
                Error $"it carries an O(3) IrrepsIdx axis, but the certificate names the point group {pg}; reading an O(3) module as a {pg}-module needs a restriction this checker does not have"
            | [], [ (gn, _) ] when gn <> pg ->
                Error $"its PgIrrepsIdx axis names point group {gn} while the certificate names {pg}, and certificates do not transfer between groups -- this checker knows each registered group's frozen table and no map between two of them"
            | [], [ (_, entries) ] when n = 1 -> Ok (TRep (TPgSpecT (pg, entries)))
            | [], [] -> Ok (TInv (TInvAgg (Some n)))
            | _ ->
                Error $"it is a multi-index array mixing a PgIrrepsIdx axis with {n - List.length pgs} other axis/axes, which is outside the supported fragment"
        | GO3 | GSO3 ->
            match irreps with
            // No irreps axis: a plain (or pg-tagged) buffer is invariant.
            | [] -> Ok (TInv (TInvAgg (Some n)))
            | [ triples ] when n = 1 -> Ok (TRep (TO3Spec triples))
            // Multi-index arrays mixing an irreps axis with others are
            // outside the supported fragment, exactly as at the seam.
            | _ ->
                Error $"it is a {n}-index array carrying {List.length irreps} IrrepsIdx axis/axes; only a single-axis irreps array is supported"
    // Named type (struct/sum): invariant but unestablished shape, never
    // scales a rep (mirrors the seam's `TyNamed` arm).
    | IRTNamed _ -> Ok (TInv TInvShapeUnknown)
    // Everything else is unclassifiable; TOpaque in signature position
    // skips the function, never a claim.
    | _ -> Error "its type is not one the classifier reads (an inference variable, an arity-polymorphic pack, a tuple, a function type, a loop or a dist)"

/// The expression-position reading: `classifyTypeR` with the reason dropped.
let classifyType (g: GroupT) (resolve: IRType -> IRType) (ty: IRType) : RepStatusT =
    match classifyTypeR g resolve ty with
    | Ok s -> s
    | Error _ -> TOpaque

/// Still open at decl close? Used only for skipped-polymorphic accounting --
/// the late/monomorphized tier is not implemented by this module.
let rec private isUnresolvedTy (resolve: IRType -> IRType) (ty: IRType) : bool =
    match resolve ty with
    | IRTInfer _ -> true
    | IRTPoly _ -> true
    | IRTUnitAnnotated (inner, _) -> isUnresolvedTy resolve inner
    | IRTIdxTagged (inner, _) -> isUnresolvedTy resolve inner
    | _ -> false

/// How many functions declined because their signature was still
/// polymorphic at decl close while carrying a rep-classifiable family.
/// Counted, never reported. AsyncLocal, reset beside the channel.
module SkippedPolymorphic =
    let private slot = new System.Threading.AsyncLocal<int>()
    let reset () = slot.Value <- 0
    let bump () = slot.Value <- slot.Value + 1
    let get () : int = slot.Value

// The candidate ladder -- the group lives in the DEDUCTION, not the index type.

let private familiesOfTy (resolve: IRType -> IRType) (ty: IRType) : bool * Set<string> =
    match resolve ty with
    | ArrayElem arr ->
        arr.IndexTypes
        |> List.fold (fun (ir, pgs) ix ->
            match irrepsOf ix, pgIrrepsOf ix with
            | Some _, _ -> (true, pgs)
            | _, Some (gn, _) -> (ir, Set.add gn pgs)
            | _ -> (ir, pgs)) (false, Set.empty)
    | _ -> (false, Set.empty)

/// Candidate groups for a signature, STRONGEST FIRST -- typed mirror of
/// MLEquiv.candidatesFor: any `IrrepsIdx` axis gives O3 then SO3 (mixed
/// signatures land here too, subsumed by O(3)); `PgIrrepsIdx<g, _>` with no
/// `IrrepsIdx` gives `Point g`, only when exactly ONE group is named.
/// DEVIATION, deliberate: the seam re-checks `gn` against
/// `PointSpec.pointGroupNames`, but a zonked tag can only exist if
/// TypeCheck's signature fence already validated it, and PointSpec compiles
/// AFTER this module -- the check here would be unavailable and redundant.
let candidatesFor (resolve: IRType -> IRType) (tys: IRType list) : GroupT list =
    let (ir, pgs) =
        tys
        |> List.fold (fun (a, b) t ->
            let (x, y) = familiesOfTy resolve t
            (a || x, Set.union b y)) (false, Set.empty)
    if ir then [ GO3; GSO3 ]
    else
        match Set.toList pgs with
        | [ gn ] -> [ GPoint gn ]
        | _ -> []

// Interprocedural summary tables

/// Speculative (deduced-this-pass) summaries, their dependency closures,
/// and DECL ORDER, keyed by BINDER IRId rather than name (a shadowing
/// parameter must not borrow a top-level function's law). ANALYSIS ONLY:
/// these never leave this compilation unit; only source-written pins (the
/// CERTIFIED table) license checking.
type RepSpecTable() =
    /// (groupStr, callee binder id) -> speculative signature
    member val Sigs = System.Collections.Generic.Dictionary<string * IRId, RepSigT>()
    /// (groupStr, callee binder id) -> that proposal's own dependency closure
    member val Deps = System.Collections.Generic.Dictionary<string * IRId, string list>()
    /// decl order per group: (groupStr, binder id, name)
    member val Order = System.Collections.Generic.List<string * IRId * string>()

/// The walker's hypothesis environment.
type private RepCtx = {
    Group: GroupT
    Resolve: IRType -> IRType
    /// Pinned (`where ml.equiv(G)`) or elaborator-stamped callee summaries.
    Certified: IRId -> RepSigT option
    /// This pass's speculative summaries under THIS group.
    Speculative: IRId -> RepSigT option
    /// The function being judged. NO SUMMARY PROVES ITSELF: any mention of
    /// this binder, call or value position, declines.
    Self: IRId
    /// Binder ids whose SPECULATIVE summaries this walk actually consumed.
    DepHits: System.Collections.Generic.HashSet<IRId>
    /// CHECKING MODE: false for deduction, true for validating a declared
    /// certificate; otherwise IDENTICAL. Makes the walker refuse a DEFINITE
    /// status at the one rule knowingly more permissive than the seam, so
    /// the divergence is never reported as a compiler bug.
    Checking: bool
    /// The FIRST decline this walk originated (see `DeclineCause`); written
    /// only by `decline` below. Fresh per (function, group) attempt, so
    /// there is no cross-walk bleed.
    Decline: DeclineCause option ref
}

/// Answer `TBottom`, recording WHY if not already recorded. FIRST WRITE
/// WINS: the walk is bottom-up, so the first site to originate a decline is
/// the innermost; arms above it propagate `TBottom` without calling this.
let private decline (ctx: RepCtx) (span: Span) (why: string) : RepStatusT =
    if (ctx.Decline.Value).IsNone then
        ctx.Decline.Value <- Some { Why = why; Where = Some span }
    TBottom

/// `decline` for the two rule sites the generic kit invokes without a span.
let private declineNoSpan (ctx: RepCtx) (why: string) : RepStatusT =
    if (ctx.Decline.Value).IsNone then
        ctx.Decline.Value <- Some { Why = why; Where = None }
    TBottom

// THE TRANSFER TABLE. Bottom-up over TypedExpr under a fixed group
// hypothesis; every rule below carries its own soundness note.
// POST-ELABORATION twin of MLEquiv.judge: no `ml.*` op survives to
// typecheck, so the seam's ml-op arms are replaced by the CALL rule --
// `derive_*`/`tp`/`poly` emitters stamp synthesized functions with
// `where ml.equiv(G)`, landing them in the certified table, consumed as axioms.

/// The COMPONENTWISE-UNIFORM LINEAR fragment (`isElementwiseArith`, used by
/// the former rule below): literals, variable reads, arithmetic on them --
/// required before a per-element walk may conclude `TRep`.
/// Offsets of an O(3) spec whose cells hold FULL invariants under `g`: O3
/// needs (l=0, parity EVEN); SO3 admits any l=0 (pseudoscalars flip only
/// under improper rotation). Block `b` spans `[start_b, start_b+mult_b)`,
/// dim `mult*(2l+1)`, accumulated in spec order (matches MLSpec's
/// `dim`/`blockDim`/`blockStarts`; reimplemented since MLSpec compiles
/// after this module).
/// POINT GROUPS ARE A NAMED DEFERRAL: pg trivial-label data lives in
/// MLPointSpec, out of compile-order reach; a pg spec declines instead.
let private invariantOffsetsT (g: GroupT) (spec: (int * int * int) list) : Set<int> =
    match g with
    | GPoint _ -> Set.empty
    | GO3 | GSO3 ->
        let mutable start = 0
        let acc = System.Collections.Generic.List<int>()
        for (l, p, m) in spec do
            if l = 0 && (g = GSO3 || p = 0) then
                for k in 0 .. m - 1 do acc.Add (start + k)
            start <- start + m * (2 * l + 1)
        Set.ofSeq acc

// The kit instantiation: equiv's answers to the generic walker's questions.
// `DisciplineKit.structuralArm` owns every arm whose soundness argument
// quantifies over ANY action (variables, control flow, binders, static
// selectors, closures, the call rule, the former walk) -- three questions,
// answered by the three records below. Everything else is `ruleArm`,
// whose justification names the action: "block-diagonal LINEAR rep".

/// What the generic walker must be able to do to a `RepStatusT` without
/// knowing what one is. Built ONCE per walk (closes over `ctx.Resolve` and
/// `ctx.Group`), not once per node.
let private repOps (ctx: RepCtx) : DisciplineKit.StatusOps<RepStatusT> = {
    Bottom = TBottom
    Opaque = TOpaque
    FixTop = TInv TInvShapeUnknown
    FixScalar = TInv TInvScalar
    IsCov = isRepT
    IsFix = isInvT
    IsBottom = (fun s -> s = TBottom)
    IsOpaque = (fun s -> s = TOpaque)
    Join = joinStatusT
    // NOT `joinStatusT >> Option.isSome`: right for a control-flow merge but
    // WRONG for a call argument -- unclassifiable proves nothing and must
    // not satisfy a parameter. The seam's `applies` predicate, verbatim.
    ParamMatches =
        (fun pSt aSt ->
            match pSt, aSt with
            | TRep sp, TRep sa -> sp = sa
            | TInv _, TInv _ -> true
            | _ -> false)
    // Invariant, shape read off the node's own resolved type.
    FixOfType = (fun ty -> TInv (shapeOfType ctx.Resolve ty))
    ClassifyTy = (fun ty -> classifyType ctx.Group ctx.Resolve ty)
}

/// The two questions the kit's structural arms must ask the DISCIPLINE:
/// shared shape, discipline-specific verdict.
let private repStructRules (ctx: RepCtx) : DisciplineKit.StructRules<RepStatusT> = {
    // Application over a rep-bound variable is a component read, same
    // verdict as TExprIndex: components of an l>0 block are basis-dependent
    // numbers this discipline refuses. (Galilean is the opposite -- its
    // elements are per-component boost-variant -- hence a rule.)
    CovAppliedAsCallee =
        (fun _ ->
            declineNoSpan ctx
                "a representation-typed value is read at a component offset in application position, and the components of an l > 0 block are basis-dependent numbers")

    // Former conclusion, valid when every step is COMPONENTWISE UNIFORM AND
    // LINEAR: only Rep +/- Rep (addition commutes with block-diagonal D)
    // and scalar*Rep can produce TRep. SECOND GUARD (typed-only): must
    // AGREE WITH THE NODE'S OWN TYPE, rejecting a former that
    // CROSS-ITERATES or whose output spec differs from input.
    FormerConclusion =
        (fun kSt outSt elementwise anyRepSrc ->
            match kSt, outSt with
            | TBottom, _ -> TBottom
            | TRep a, TRep b when a = b && elementwise -> TRep a
            | TInv _, TInv sh -> TInv sh
            | _ ->
                if anyRepSrc then
                    declineNoSpan ctx
                        "a former reads a representation-typed source, but its kernel is not componentwise-uniform linear arithmetic whose conclusion matches the former's own type"
                else TOpaque)
}

/// Project a stored equiv signature into the shape the kit's call rule reads.
let private toCallSig (sg: RepSigT) : DisciplineKit.CallSig<GroupT, RepStatusT> =
    { CHyp = sg.Group; CParams = sg.Params |> List.map snd; CReturn = sg.Return }

let private repWalkCtx (ctx: RepCtx) : DisciplineKit.WalkCtx<GroupT, RepStatusT> = {
    Ops = repOps ctx
    Rules = repStructRules ctx
    Hyp = ctx.Group
    HypEq = (=)
    Certified = (fun id -> ctx.Certified id |> Option.map toCallSig)
    Speculative = (fun id -> ctx.Speculative id |> Option.map toCallSig)
    Self = ctx.Self
    DepHits = ctx.DepHits
    // Sole consumer is the kit's call rule: in CHECKING mode the certified
    // callee's all-invariant fall-through must abstain (TOpaque) rather
    // than answer definitely, since the seam lacks that rule. In DEDUCTION
    // the extra recall is the point.
    Checking = ctx.Checking
}

/// The walker: kit's structural arms first, this discipline's rules second.
/// CURRIED so `repWalkCtx ctx` is built once per walk, not once per node.
/// THE PARTITION IS EXACT AND DISJOINT. `structuralArm` answers `Some` for
/// exactly: TExprVar (both arms), TExprIf, TExprMatch, TExprLet, TExprBlock,
/// TExprSequence, TExprAssign, TExprTupleIndex, TExprField, TExprCompute,
/// TExprLambda, TExprApp, TExprApply. `ruleArm` covers exactly the rest
/// (literals, binary/unary arithmetic, whole-array negate/conjugate,
/// indexing, reduction, aggregate construction, virtual arrays, catch-all);
/// no node kind is handled by both.
let private statusOf (ctx: RepCtx) : Map<IRId, RepStatusT> -> TypedExpr -> RepStatusT =
    let wctx = repWalkCtx ctx
    let rec go (env: Map<IRId, RepStatusT>) (expr: TypedExpr) : RepStatusT =
        match DisciplineKit.structuralArm wctx go env expr with
        | Some s -> s
        | None -> ruleArm env expr

    /// Equiv's OWN rules: arms whose soundness argument names the action;
    /// each would be wrong for at least one of the other two disciplines.
    and ruleArm (env: Map<IRId, RepStatusT>) (expr: TypedExpr) : RepStatusT =
        let j = go env
        /// Shape read off the node's own resolved type -- the typed win
        /// over the seam's syntactic shape guessing.
        let nodeShape () = shapeOfType ctx.Resolve expr.Type
        /// Decline HERE, recording why -- only where this arm ORIGINATES a
        /// decline; a forwarded sub-expression `TBottom` stays plain.
        let dcl (why: string) = decline ctx expr.Span why

        /// Aggregate constructor. SOUNDNESS: packing a rep into a literal
        /// loses its block structure, so a rep element declines; all-invariant
        /// elements make an invariant aggregate. (Galilean is the OPPOSITE.)
        let aggOf (es: TypedExpr list) =
            let sts = es |> List.map j
            if sts |> List.exists ((=) TBottom) then TBottom
            elif sts |> List.exists isRepT then
                dcl "a representation-typed value is packed into a literal aggregate, which loses its block structure -- the aggregate does not transform as the representation"
            elif sts |> List.exists ((=) TOpaque) then TOpaque
            else TInv (TInvAgg None)

        match expr.Kind with

        // -- literals --
        // SOUNDNESS: a constant does not move under any group action.
        | TExprLit _ -> TInv TInvScalar

        // -- arithmetic --
        | TExprBinOp (mode, op, l, r) ->
            let sl = j l
            let sr = j r
            (match sl, sr, op with
             | TBottom, _, _ | _, TBottom, _ -> TBottom
             // SOUNDNESS: outer product cross-iterates to a HIGHER RANK, so
             // it cannot be the rep it was built from.
             | (TRep _, _, _ | _, TRep _, _) when mode <> Elementwise ->
                 dcl "the outer-product form cross-iterates, so its result has a higher rank than either operand and cannot be the representation it was built from"
             // SOUNDNESS (Rep +/- Rep): LINEAR action (D(x+y)=Dx+Dy) requires
             // IDENTICAL specs. Galilean rejects outright (doubles U0).
             | TRep s1, TRep s2, (OpAdd | OpSub) ->
                 if s1 = s2 then TRep s1
                 else
                     dcl $"adding or subtracting two representations with DIFFERENT laws ({repStrT s1} and {repStrT s2}): the sum transforms under neither"
             // Elementwise product of two reps is Clebsch-Gordan's job, not
             // pointwise. (Perm ADMITS this one -- commutes with relabelling.)
             | TRep _, TRep _, _ ->
                 dcl "an elementwise product of two representation-typed values is not equivariant -- that contraction is the Clebsch-Gordan one, not a pointwise multiply"
             // SOUNDNESS (scalar scaling): SCALAR commutes with every block
             // (D(cx)=cD(x)); an invariant ARRAY does not in general, so
             // scalarity must be PROVEN.
             | TRep s, TInv TInvScalar, (OpMul | OpDiv) -> TRep s
             | TInv TInvScalar, TRep s, OpMul -> TRep s
             | (TRep _, TInv _, _) | (TInv _, TRep _, _) ->
                 dcl "only a provably SCALAR invariant may scale a representation-typed value under * or /, because a scalar is the only invariant that commutes with every block of the action"
             | TInv shl, TInv shr, _ ->
                 TInv (if mode = Elementwise then binShapeT shl shr else TInvShapeUnknown)
             // Nothing established either side: nothing claimed for result.
             | TOpaque, _, _ | _, TOpaque, _ -> TOpaque)

        // SOUNDNESS: only a LINEAR unary op transports a rep. Negation is
        // -I, commuting with every D, so `-x` transforms as `x`; everything
        // else applied to a rep is refused. TRAP: TypeCheck rewrites
        // whitelisted math names (e.g. `exp(x)`) into
        // `TExprUnaryOp (OpMath "exp", _)` -- do not pass status through
        // unconditionally, or this would silently CERTIFY a nonlinearity on
        // rep components (gate with ml.gated, or extract invariants with
        // ml.scalars/ml.norms). LOAD-BEARING SPLIT, do not collapse: Perm
        // passes ALL unary ops through (relabelling); galilean rejects even
        // negation (flips U0). Three different functions -- belongs here.
        | TExprUnaryOp (OpNeg, inner) -> j inner
        | TExprUnaryOp (_, inner) ->
            (match j inner with
             | TRep _ ->
                 dcl "a NONLINEAR unary operator is applied to a representation-typed value; only a linear map transports a representation (gate it with ml.gated, or extract invariants with ml.scalars / ml.norms)"
             | s -> s)

        // SOUNDNESS: whole-array negation is -I, which commutes with every D.
        | TExprArrayNegate a -> j a

        // Complex conjugation does NOT commute with a complex rep in general
        // (it conjugates the matrix): decline on a rep, pass through otherwise.
        | TExprArrayConjugate a ->
            (match j a with
             | TRep _ ->
                 dcl "complex conjugation of a representation-typed value conjugates the representation matrix, so it does not commute with the action in general"
             | s -> s)

        // -- reads --
        // SOUNDNESS: `Inv` is HELD FIXED, so a component of an invariant
        // aggregate picked by an invariant selector is invariant; a REP
        // base declines IN GENERAL (basis-dependent components). EXCEPTION:
        // a STATIC offset inside a trivial block -- (l=0, even) under O3, or
        // any l=0 under SO3 (pseudoscalar fixed by every proper rotation) --
        // is acted on by the identity, so that cell is the SAME number in
        // every frame: `TInvScalar`. The offset must be a LITERAL; a
        // computed index could land anywhere and declines. TO3Spec ONLY --
        // see `invariantOffsetsT`: a pg's trivial labels are out of
        // compile-order reach, so no offset is claimed there.
        | TExprIndex (arr, idxs, _) ->
            (match j arr with
             | TBottom -> TBottom
             | TRep (TO3Spec spec) ->
                 (match idxs with
                  | [ i ] ->
                      (match DisciplineKit.staticIntOf i with
                       | Some k when Set.contains k (invariantOffsetsT ctx.Group spec) ->
                           TInv TInvScalar
                       | Some k ->
                           dcl $"raw indexing at offset {k} reads a basis-dependent component of an l > 0 block of {repStrT (TO3Spec spec)}"
                       | None ->
                           dcl "indexing a representation-typed value needs a LITERAL offset this walker can place inside a trivial block; a computed index could land anywhere")
                  | _ ->
                      dcl "only a single-offset read into a representation-typed value is modelled")
             // Point-group reps: see `invariantOffsetsT` -- trivial-LABEL
             // table is out of compile-order reach, so no offset is claimed.
             | TRep _ ->
                 dcl "raw indexing into a point-group representation: which labels are trivial is registry data this module cannot reach in compile order, so no offset is claimed invariant"
             | TOpaque -> TOpaque
             | TInv _ ->
                 if idxs |> List.forall (fun i -> isInvT (j i))
                 then TInv (nodeShape ())
                 else
                     dcl "an invariant aggregate is indexed by a selector that is not itself invariant")

        // -- reduction --
        // SOUNDNESS: a fold over a rep sums BASIS-DEPENDENT COMPONENTS -- not
        // a rotational invariant (the norm is) -- so a rep source declines.
        | TExprReduce (src, _, init) ->
            let ss = j src
            let si = match init with Some i -> j i | None -> TInv TInvScalar
            (match ss, si with
             | TBottom, _ | _, TBottom -> TBottom
             | TRep _, _ | _, TRep _ ->
                 dcl "a reduction folds over the BASIS-DEPENDENT components of a representation; the sum of the components of an l > 0 value is not an invariant (the norm is)"
             | TInv _, TInv _ -> TInv TInvScalar
             | _ -> TOpaque)

        // -- aggregates and virtual arrays --
        | TExprTuple es | TExprStack es | TExprZip es -> aggOf es
        | TExprArrayLit (es, _) -> aggOf es
        | TExprJoin (es, _) -> aggOf es

        // SOUNDNESS: virtual arrays ENUMERATE INDICES, no rep structure.
        // (Perm can't say this unconditionally -- `range<Idx<N>>` IS its
        // node index set -- hence a rule.)
        | TExprRange _ | TExprReverse _ -> TInv (TInvAgg None)
        | TExprDotDot _ -> TInv (TInvAgg None)

        // -- everything else -- nothing established; TOpaque propagates,
        // never manufacturing a Rep claim. The one unmodelled-node-as-callee
        // path is closed by the kit's callee guard.
        | _ -> TOpaque

    go

// Signature classification and the deduction driver.

/// A parameter as the deduction needs it: surface name (for the rendered
/// signature), BINDER id (the walker's env key), and ZONKED type.
type RepParam = { PName: string; PId: IRId; PType: IRType }

/// WHICH signature position refused, and why.
type SigRefusal = {
    /// "parameter 'x'" or "the return type".
    Position: string
    /// `classifyTypeR`'s reason, phrased to follow the position.
    Why: string
}

/// The rendered one-liner, for an abstain reason or a tooling tooltip.
let sigRefusalStr (r: SigRefusal) : string = $"{r.Position} does not classify: {r.Why}"

/// Classify a whole signature under a group hypothesis. `Error` when ANY
/// position is unclassifiable -- the seam's `certSigOf -> Error` path: a
/// proposal the checker would refuse at the signature is worse than none.
/// PARAMETERS BEFORE THE RETURN, first failure wins.
let classifySignature (g: GroupT) (resolve: IRType -> IRType) (owner: string)
                      (parms: RepParam list) (retTy: IRType) : Result<RepSigT, SigRefusal> =
    let ps = parms |> List.map (fun p -> (p.PName, classifyTypeR g resolve p.PType))
    match ps |> List.tryPick (fun (n, r) -> match r with Error w -> Some (n, w) | Ok _ -> None) with
    | Some (n, w) -> Error { Position = $"parameter '{n}'"; Why = w }
    | None ->
        match classifyTypeR g resolve retTy with
        | Error w -> Error { Position = "the return type"; Why = w }
        | Ok r ->
            Ok { Owner = owner
                 Group = g
                 Params = ps |> List.map (fun (n, r) -> (n, (match r with Ok s -> s | Error _ -> TOpaque)))
                 Return = r }

/// THE NON-VACUITY FILTER: a signature with nothing rep-typed proposes
/// nothing -- `equiv(G)` on a scalar helper is vacuously true, noise with
/// a theorem's face on.
let private isVacuous (sg: RepSigT) : bool =
    not ((sg.Params |> List.exists (snd >> isRepT)) || isRepT sg.Return)

/// Record a CERTIFIED signature -- a source-written `where ml.equiv(G)` pin,
/// or an elaborator stamp on a synthesized function; both arrive as the
/// same `__ml_equiv` conjunct. Trust, not proof: a caller always borrows
/// this DECLARED summary (`checkDeclaredRep`'s second opinion never changes
/// it). A pin whose signature does not classify is simply not recorded.
let recordCertified (certified: System.Collections.Generic.Dictionary<IRId, RepSigT>)
                    (resolve: IRType -> IRType) (owner: string) (funcId: IRId)
                    (groupName: string) (parms: RepParam list) (retTy: IRType) : unit =
    let g = groupOfName groupName
    match classifySignature g resolve owner parms retTy with
    | Ok sg -> certified.[funcId] <- sg
    | Error _ -> ()

// Declared-certificate VALIDATION. The typed walker runs a SECOND,
// INDEPENDENT judgment of a theorem the seam already accepted; it has no
// authority to reject what the seam accepted or accept what it rejected.
// Only DISAGREEMENT surfaces, as an INTERNAL COMPILER ERROR (LieGuardFailure
// posture) -- two independent judgments contradicting is a compiler bug,
// not a program bug. ABSTENTION IS THE DEFAULT AT EVERY UNCERTAIN BOUNDARY:
// a false DISAGREE on a legitimately certified body would turn a working
// program into a compiler-bug report. Silence is never disagreement.

/// The outcome of validating one declared certificate.
type CheckVerdict =
    /// The walker derived a status consistent with the declaration.
    | RepConfirm
    /// The walker declined to judge. NOT a disagreement -- keeps
    /// engine-discharged bodies (and other modeling gaps) safe when no
    /// discharger is registered. Carries a reason for the census.
    | RepAbstain of reason: string
    /// The walker derived a DEFINITE status that contradicts the declaration.
    | RepDisagree of detail: string

// ENGINE HOOK SLOT (an external TypedExpr polynomial-extraction discharger)

/// What an external discharger may conclude about a body composition
/// could not judge.
type EngineVerdict =
    /// The body discharges: the declared certificate holds.
    | EngineConfirms
    /// The body provably does NOT satisfy the declared certificate --
    /// since the seam already accepted it, a compiler-bug signal, surfacing
    /// exactly as a composition-derived disagreement does.
    | EngineRefutes of detail: string

/// The registered discharger. `None` means NOT APPLICABLE, leaving the
/// verdict at abstain. Carries `resolve` and the PARAMETER LIST alongside
/// the signature since neither is recoverable from `RepSigT` alone:
/// `sg.Params` holds (name, status) pairs, but binding a rep parameter to
/// a polynomial's variable vector needs the BINDER ID and type
/// (`RepParam`), positionally in `sg.Params` order. The group is not a
/// separate argument -- it rides in `sg.Group`, avoiding a desync risk.
type EngineHook =
    (IRType -> IRType) -> RepParam list -> RepSigT -> TypedExpr -> EngineVerdict option

/// The slot itself (`Blade.Constraints.registerConstraint` shape): a
/// process-wide mutable holding at most one discharger. While empty, every
/// unjudgeable body abstains.
module EngineDischarge =
    let mutable private hook : EngineHook option = None
    let register (h: EngineHook) : unit = hook <- Some h
    let clear () : unit = hook <- None
    let isRegistered () : bool = hook.IsSome
    /// Total by construction: an escape from the discharger reads as "not
    /// applicable" -- EXCEPT `LieDischarge.LieGuardFailure`, a compiler-bug
    /// assert the registered adapter (TypeCheck) catches and converts to
    /// `EngineRefutes` before it reaches this wrapper.
    let tryDischarge (resolve: IRType -> IRType) (parms: RepParam list)
                     (sg: RepSigT) (body: TypedExpr) : EngineVerdict option =
        match hook with
        | None -> None
        | Some h -> (try h resolve parms sg body with _ -> None)

// The disagreement channel and the agreement census.

/// Disagreements found this compilation, as (owner, detail, span). TypeCheck
/// drains this into the compile-error list so a contradiction stops the build
/// loudly. AsyncLocal, reset beside the proposal channel.
module RepCheckDisagreements =
    let private slot = new System.Threading.AsyncLocal<(string * string * Span) list>()
    let reset () = slot.Value <- []
    let add (owner: string) (detail: string) (span: Span) =
        slot.Value <- (owner, detail, span) :: slot.Value
    let get () : (string * string * Span) list =
        match box slot.Value with null -> [] | _ -> List.rev slot.Value

/// Confirm/abstain census for the compilation, so the agreement test block
/// can report the split. Abstain count is the shrinking target for future
/// engine coverage.
module RepCheckCensus =
    /// Elaborator-synthesized decls carry the generated-name prefix; splitting
    /// the census on it makes the abstain number ACTIONABLE: a generated
    /// body is a CG loop nest (what an engine discharger judges), while a
    /// source-written abstention points at a composition-fragment gap.
    let private isGenerated (owner: string) = owner.StartsWith "__ml_"
    let private confirms = new System.Threading.AsyncLocal<int>()
    let private abstains = new System.Threading.AsyncLocal<int>()
    let private genConfirms = new System.Threading.AsyncLocal<int>()
    let private genAbstains = new System.Threading.AsyncLocal<int>()
    let private reasons = new System.Threading.AsyncLocal<string list>()
    let reset () =
        confirms.Value <- 0
        abstains.Value <- 0
        genConfirms.Value <- 0
        genAbstains.Value <- 0
        reasons.Value <- []
    let recordConfirm (owner: string) =
        confirms.Value <- confirms.Value + 1
        if isGenerated owner then genConfirms.Value <- genConfirms.Value + 1
    let recordAbstain (owner: string) (reason: string) =
        abstains.Value <- abstains.Value + 1
        if isGenerated owner then genAbstains.Value <- genAbstains.Value + 1
        reasons.Value <- reason :: (match box reasons.Value with null -> [] | _ -> reasons.Value)
    let confirmed () : int = confirms.Value
    let abstained () : int = abstains.Value
    /// Of `confirmed ()` / `abstained ()`, how many were elaborator-generated.
    let generatedConfirmed () : int = genConfirms.Value
    let generatedAbstained () : int = genAbstains.Value
    /// Abstention reasons, most recent first -- a histogram source, not a
    /// stable ordering.
    let abstainReasons () : string list =
        match box reasons.Value with null -> [] | _ -> reasons.Value

/// Validate ONE declared certificate against the typed walker. The declared
/// group comes from the conjunct; the declared SIGNATURE is this module's
/// own classification of the zonked signature -- the same one
/// `recordCertified` stores, so checking and the table cannot drift apart.
/// SELF-REFERENCE IS ASSUMED, NOT PROVED (correct here, wrong in deduction):
/// validation is an assume-guarantee obligation, mirroring the seam's
/// `judgeFunction`. Deduction must refuse self-reference: there, no
/// theorem has been declared yet.
/// DISAGREEMENT IS DELIBERATELY NARROW: only a definite `TRep` on both sides
/// with DIFFERENT specs is reported. Every other mismatch abstains (reachable
/// through a modeling gap). A spec contradiction is the one shape no
/// divergence can produce: every rule yielding `TRep s` requires a rep
/// input and preserves the spec exactly.
let checkDeclaredRep (certified: System.Collections.Generic.Dictionary<IRId, RepSigT>)
                     (resolve: IRType -> IRType)
                     (owner: string) (funcId: IRId) (groupName: string)
                     (parms: RepParam list) (retTy: IRType)
                     (body: TypedExpr) : CheckVerdict =
    try
        let g = groupOfName groupName
        match classifySignature g resolve owner parms retTy with
        | Error e ->
            RepAbstain $"signature not classifiable by the typed classifier: {sigRefusalStr e}"
        | Ok sg ->
            let ctx = {
                Group = g
                Resolve = resolve
                // Declaring function's OWN cert is visible here (makes the
                // assume-guarantee reading work for a recursive body).
                Certified =
                    (fun id -> match certified.TryGetValue id with | true, s -> Some s | _ -> None)
                // Stands only on pins and axioms, never another proposal.
                Speculative = (fun _ -> None)
                // No binder is "self" here; IRIds are non-negative.
                Self = System.Int32.MinValue
                DepHits = System.Collections.Generic.HashSet<IRId>()
                Checking = true
                Decline = ref None
            }
            let bodySt = statusOf ctx (
                List.zip parms sg.Params
                |> List.fold (fun m (p, (_, st)) -> Map.add p.PId st m) Map.empty) body
            let engineFallback (why: string) =
                match EngineDischarge.tryDischarge resolve parms sg body with
                | Some EngineConfirms -> RepConfirm
                | Some (EngineRefutes d) -> RepDisagree d
                | None -> RepAbstain why
            // A DEFINITE-BUT-MISMATCHED status is reachable through a
            // modeling gap, so these arms consult the engine too, but
            // honour ONLY a discharge, not a stacked refutation.
            let engineUpgradeOnly (why: string) =
                match EngineDischarge.tryDischarge resolve parms sg body with
                | Some EngineConfirms -> RepConfirm
                | Some (EngineRefutes _) | None -> RepAbstain why
            // The decline's CAUSE, for the census; empty for a generic-kit
            // decline (no cause channel; see `DeclineCause`).
            let declineReason () =
                match ctx.Decline.Value with
                | Some c -> $"walker declined: {c.Why}"
                | None -> "walker declined (outside the composition fragment)"
            match bodySt with
            | TBottom -> engineFallback (declineReason ())
            | TOpaque -> engineFallback "nothing established for the body"
            | _ when statusAgreesT bodySt sg.Return -> RepConfirm
            | TRep bs ->
                (match sg.Return with
                 | TRep ds ->
                     RepDisagree (
                         $"the typed walker derives {(repStrT bs)} for the body of '{owner}', but the declared certificate's return transforms as {(repStrT ds)}")
                 // Derived rep vs declared invariant: a modeling gap, abstain.
                 | _ -> engineUpgradeOnly "derived a representation where the declaration is invariant")
            | _ -> engineUpgradeOnly "derived an invariant where the declaration is a representation"
    with _ ->
        // Totality: a second opinion may never turn a compiling program into a crash.
        RepAbstain "validation raised"

/// The early-tier deduction for ONE function, at decl close, from ZONKED
/// signature types. Returns the proposal to publish, or None for silence.
/// Candidate ladder strongest-first; only the STRONGEST passer is proposed:
/// pinning O3 is what the user would actually write, and an SO3 caller of
/// an O3-pinned callee is a body the checker rejects, so recording the
/// weaker one would make the dependency closure dishonest.
let deduceFunctionRep (certified: System.Collections.Generic.Dictionary<IRId, RepSigT>)
                      (spec: RepSpecTable)
                      (resolve: IRType -> IRType)
                      (owner: string) (funcId: IRId)
                      (parms: RepParam list) (retTy: IRType)
                      (body: TypedExpr) : RepProposal option =
    let sigTys = (parms |> List.map (_.PType)) @ [ retTy ]
    let candidates = candidatesFor resolve sigTys
    if candidates.IsEmpty then None
    elif sigTys |> List.exists (isUnresolvedTy resolve) then
        // Still open at decl close: not classifiable, count and stay silent
        // (the ladder ran FIRST, so this excludes every generic non-rep helper).
        SkippedPolymorphic.bump ()
        None
    else
        /// One candidate attempt: hypothesize the group, classify the ZONKED
        /// signature, and walk the body against it.
        let attempt (g: GroupT) : (string * RepSigT * System.Collections.Generic.HashSet<IRId>) option =
            match classifySignature g resolve owner parms retTy with
            | Error _ -> None
            | Ok sg when isVacuous sg -> None
            | Ok sg ->
                let gs = groupStrT g
                let ctx = {
                    Group = g
                    Resolve = resolve
                    Certified =
                        (fun id -> match certified.TryGetValue id with | true, s -> Some s | _ -> None)
                    Speculative =
                        (fun id -> match spec.Sigs.TryGetValue ((gs, id)) with | true, s -> Some s | _ -> None)
                    Self = funcId
                    DepHits = System.Collections.Generic.HashSet<IRId>()
                    // Deduction, not validation: checking-mode flag is off.
                    Checking = false
                    Decline = ref None
                }
                // Parameters enter under their classified statuses -- the theorem's hypothesis.
                let bodyEnv =
                    List.zip parms sg.Params
                    |> List.fold (fun m (p, (_, st)) -> Map.add p.PId st m) Map.empty
                let bodySt = statusOf ctx bodyEnv body
                // Holds iff the body's law AGREES with the return; TBottom never agrees.
                if statusAgreesT bodySt sg.Return then Some (gs, sg, ctx.DepHits)
                else
                    // COMPOSITION FIRST, ENGINE SECOND: consulted only where
                    // composition DECLINED, never to overturn a reached verdict;
                    // sits INSIDE `attempt`, so strongest-first still governs.
                    // A REFUTATION IS JUST A DECLINE HERE -- deduction never
                    // rejects, so "not equivariant" and "nothing to say" are the
                    // same silence; only CHECKING treats it as a bug signal.
                    match EngineDischarge.tryDischarge resolve parms sg body with
                    | Some EngineConfirms -> Some (gs, sg, ctx.DepHits)
                    | Some (EngineRefutes _) | None -> None

        // TOTAL BY CONSTRUCTION: a speculative second opinion may never turn
        // a compiling program into a crash, so any escape reads as "no
        // proposal" rather than a compiler crash.
        let attempt g = try attempt g with _ -> None
        match candidates |> List.tryPick attempt with
        | None -> None
        | Some (gs, sg, hits) ->
            // Dependency closure: SPECULATIVE pins this rests on (direct
            // deps are speculative callees consumed, plus their own deps).
            let orderG =
                spec.Order |> Seq.filter (fun (g2, _, _) -> g2 = gs) |> Seq.toList
            let closure =
                hits
                |> Seq.collect (fun id ->
                    let self =
                        orderG |> List.tryPick (fun (_, i, n) -> if i = id then Some n else None)
                    let theirs =
                        match spec.Deps.TryGetValue ((gs, id)) with
                        | true, ds -> ds
                        | _ -> []
                    (Option.toList self) @ theirs)
                |> Seq.distinct
                |> Set.ofSeq
            let ordered = orderG |> List.map (fun (_, _, n) -> n) |> List.filter (fun n -> closure.Contains n)
            // Record so LATER declarations can rest on it -- single-pass in
            // decl order, no fixpoint (a forward call declines: silence).
            spec.Sigs.[(gs, funcId)] <- sg
            spec.Deps.[(gs, funcId)] <- ordered
            spec.Order.Add ((gs, funcId, owner))
            Some { Owner = owner; Group = gs; Signature = sigSummaryT sg; Deps = ordered }
