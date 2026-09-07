// Expression-level expansion passes that run before the AD sweeps: sort
// carried as permutation data (C7), map/apply views, the AD-able-subset
// walker, capture-avoiding renaming, call/reduce hoisting, recursive-array
// expansion, and static extent/dims inference.
module Blade.GradExpand

open Blade.Ast
open Blade.GradCommon

// ---------------------------------------------------------------------------
// C7: sort, differentiated by carrying the PERMUTATION AS DATA
//
// `sort` is piecewise constant in its input: off the tie set (measure zero)
// a small perturbation of A moves the VALUES but not which original slot
// lands where. So the derivative of `s = sort(A, key)` is not a derivative of
// the sort at all -- it is the derivative of a GATHER through a permutation
// that the transform may treat as constant index data:
//
//     s(i) = A(perm(i))       =>  tangent  ds(i) = dA(perm(i))
//                             =>  adjoint  dA(j) += ds(invperm(j))
//
// Both legs need `perm` as a first-class array, which `sort` does not hand
// back -- so the pre-pass MATERIALIZES it, by sorting the row indices under
// the same key instead of the values:
//
//     let __sx_s = method_for(range<I>) <@> lambda(i: I) -> i |> compute
//     let __sp_s = sort(__sx_s, lambda(i: I) -> A(i))
//     let s      = sort(A, key)                       // primal, unchanged
//
// The reverse leg needs the INVERSE permutation, and gets it from a SECOND
// sort rather than a scatter primitive (the same "carry data, don't invert
// structurally" move the 2.17a grouping route makes):
//
//     let __si_s = sort(__sx_s, lambda(i: I) -> __sp_s(i))
//
// `__sp_s` is shared BY NAME between the primal and the tangent/adjoint legs,
// so the two can never disagree about which permutation was taken -- including
// at ties, where `std::stable_sort` fixes input order and the two legs inherit
// the same convention for free.
//
// The lambda params are ALWAYS annotated: an unannotated index-typed key
// param is miscompiled to double today.
// ---------------------------------------------------------------------------

/// Synthesized-binding prefixes for the sort expansion. Double-underscore
/// (internal, per the style guide) and NOT added to the reserved-name gate:
/// the gate runs on the post-pre-pass statement list, which is exactly where
/// these live. Collisions are caught in `expandSort` against the SURFACE body
/// instead, where a user binding and a re-entered synthesized one can still
/// be told apart.
let internal sortPermName (n: string) = "__sp_" + n
let internal sortInvName (n: string) = "__si_" + n
let internal sortIotaName (n: string) = "__sx_" + n

/// What the sort expansion left behind for the differentiation sweeps.
type internal SortPlan = {
    /// The ORIGINAL index landing at each sorted slot.
    Perm: string
    /// The inverse permutation. Emitted in reverse mode only ("" in jvp,
    /// where nothing reads it).
    InvPerm: string
    /// The sorted array's index type, as WRITTEN (alias identity preserved):
    /// the gather lambdas annotate their params with it.
    IdxTy: TypeExpr
    /// The sorted array's name.
    Src: string
}

let internal lamP (nm: string) (t: TypeExpr) : LambdaParam =
    { Name = nm; Type = Some t; Default = None; NameSpan = noSpan }

/// `method_for(<operand>) <@> lambda(<pv>: <ity>) -> <body> |> compute`.
let internal gatherForm (operand: Expr) (pv: string) (ity: TypeExpr) (body: Expr) : Expr =
    syn (ExprCompute (syn (ExprBinOp (Elementwise, OpApply,
                                      syn (ExprMethodFor [operand]),
                                      syn (ExprLambda ([lamP pv ity], None, body))))))

/// The gather of `src` through permutation `perm`, over index type `ity`.
let internal permGather (perm: string) (ity: TypeExpr) (src: string) : Expr =
    let pv = "__spi"
    gatherForm (v perm) pv ity (syn (ExprApp (v src, [v pv])))

/// `method_for(range<I>) <@> lambda(i: I) -> i |> compute`: the materialized
/// identity index array the expansion sorts in place of the values. Admitted
/// in BOTH modes as constant index plumbing -- it computes no float and reads
/// no array, so admitting it cannot hide a derivative.
let internal (|IndexIota|_|) (e: Expr) =
    match e.Kind with
    | ExprKind.ExprCompute { Kind = ExprKind.ExprBinOp (_, OpApply,
                                        { Kind = ExprKind.ExprMethodFor [{ Kind = ExprKind.ExprRange _ }] },
                                        { Kind = ExprKind.ExprLambda ([p], _, { Kind = ExprKind.ExprVar b }) }) }
            when p.Name = b -> true
    | _ -> false

/// `sort(<iota>, lambda(<p>: I) -> <key>[x := <A>(<p>)])`: the shape both
/// synthesized permutation sorts have. Recognizes an ALREADY-EXPANDED body on a
/// composition round (`ad.jvp(ad.grad(f))` re-runs the pre-pass over a body
/// this pass itself wrote) so the plumbing is reused instead of re-emitted, and
/// it is also how `collectSortPlans` recovers which array a permutation belongs
/// to after inlining has renamed every local.
///
/// The key body is the USER's, composed over the gathered element, so it is NOT
/// in general the bare application `A(p)` -- a descending key reads
/// `0.0 - A(p)`. What identifies the plumbing is that `p` occurs ONLY as the
/// subscript of array reads, and that every such read names the SAME array:
/// that array is the sorted source.
///
/// The `iotas` gate is load-bearing: without it a PRIMAL sort under a
/// one-call key (`sort(A, lambda(x: Float) -> abs(x))`) has the same shape
/// and would be misread as plumbing, losing its plan and its derivative.
/// Only a sort whose operand is a materialized index array is plumbing.
let internal (|SortPermForm|_|) (iotas: Set<string>) (e: Expr) =
    /// The single array `p` is read through, or None if `p` is read any other
    /// way (bare, as a function head, through two different arrays, or not at
    /// all). Structural, and its catch-all answers "cannot tell" -- a form this
    /// misses is simply not recognized as plumbing.
    let keyedArray (p: string) (body: Expr) : string option =
        let hits = System.Collections.Generic.HashSet<string>()
        let mutable bad = false
        let rec go (x: Expr) =
            match x.Kind with
            | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, [{ Kind = ExprKind.ExprVar b }]) when b = p ->
                if a = p then bad <- true else hits.Add a |> ignore
            | ExprKind.ExprVar n -> if n = p then bad <- true
            | ExprKind.ExprLit _ -> ()
            | ExprKind.ExprTyped (i, _) | ExprKind.ExprUnaryOp (_, i)
            | ExprKind.ExprPure i | ExprKind.ExprCompute i -> go i
            | ExprKind.ExprBinOp (_, _, l, r) -> go l; go r
            | ExprKind.ExprApp (f, args) -> go f; List.iter go args
            | ExprKind.ExprIf (c, t, f) -> go c; go t; go f
            | ExprKind.ExprArrayLit es | ExprKind.ExprTuple es -> List.iter go es
            | _ -> bad <- true
        go body
        if bad || hits.Count <> 1 then None else Some (Seq.head hits)
    match e.Kind with
    | ExprKind.ExprSort ({ Kind = ExprKind.ExprVar srcIdx },
                         { Kind = ExprKind.ExprLambda ([p], _, kbody) })
            when Set.contains srcIdx iotas ->
        keyedArray p.Name kbody |> Option.map (fun keyed -> (keyed, p.Type))
    | _ -> None

/// Does `e` mention any name in `names`? Unhandled forms answer TRUE. This
/// gates the sort-KEY refusal, where a missed reference is a wrong derivative
/// (the key would close over differentiable data whose contribution the
/// permutation-as-data rule silently drops), while a spurious hit costs only
/// a refusal the user can work around by hoisting the value out of the key.
let rec internal mentionsAnyOf (names: Set<string>) (e: Expr) : bool =
    let any = List.exists (mentionsAnyOf names)
    match e.Kind with
    | ExprKind.ExprVar n -> Set.contains n names
    | ExprKind.ExprLit _ -> false
    | ExprKind.ExprTyped (inner, _) | ExprKind.ExprUnaryOp (_, inner)
    | ExprKind.ExprPure inner | ExprKind.ExprCompute inner -> mentionsAnyOf names inner
    | ExprKind.ExprBinOp (_, _, l, r) | ExprKind.ExprDotDot (l, r) ->
        mentionsAnyOf names l || mentionsAnyOf names r
    | ExprKind.ExprApp (f, args) -> mentionsAnyOf names f || any args
    | ExprKind.ExprArrayLit es | ExprKind.ExprTuple es | ExprKind.ExprStack es -> any es
    | ExprKind.ExprIf (c, t, f) ->
        mentionsAnyOf names c || mentionsAnyOf names t || mentionsAnyOf names f
    | _ -> true

/// Any `sort(...)` nested inside `e`. Coverage mirrors `hoistReduces` -- the
/// same fragment the pre-pass rewrites -- and shares its structural catch-all:
/// a form this misses is not a silent zero, it just reaches the sweeps'
/// generic refusal instead of the specific message below.
let rec internal containsSort (e: Expr) : bool =
    let any = List.exists containsSort
    match e.Kind with
    | ExprKind.ExprSort _ -> true
    | ExprKind.ExprTyped (inner, _) | ExprKind.ExprUnaryOp (_, inner)
    | ExprKind.ExprPure inner | ExprKind.ExprCompute inner
    | ExprKind.ExprGuard (_, inner) | ExprKind.ExprReplicate (_, inner)
    | ExprKind.ExprTranspose (inner, _, _) -> containsSort inner
    | ExprKind.ExprBinOp (_, _, l, r) | ExprKind.ExprDotDot (l, r)
    | ExprKind.ExprAssign (l, r) -> containsSort l || containsSort r
    | ExprKind.ExprApp (f, args) -> containsSort f || any args
    | ExprKind.ExprArrayLit es | ExprKind.ExprTuple es | ExprKind.ExprStack es
    | ExprKind.ExprSequence es | ExprKind.ExprJoin (es, _) -> any es
    | ExprKind.ExprIf (c, t, f) -> containsSort c || containsSort t || containsSort f
    | ExprKind.ExprReduce (src, _, initOpt, _) ->
        containsSort src || (match initOpt with Some i -> containsSort i | None -> false)
    | ExprKind.ExprMethodFor es -> any es
    | _ -> false

/// Every name a surface statement list binds with a plain `let <name>`,
/// nested loops included. The sort expansion checks its synthesized names
/// against this (plus the parameters) BEFORE emitting them.
let rec internal surfaceBoundNames (ss: Stmt list) : Set<string> =
    ss |> List.fold (fun acc s ->
        match unwrapStmt s with
        | StmtLet { Pattern = { Kind = PatternKind.PatVar n } } -> Set.add n acc
        | StmtForIn (_, _, body) -> Set.union acc (surfaceBoundNames body)
        | _ -> acc) Set.empty

/// Expand a `let <name> = sort(<A>, <key>)` into the permutation plumbing
/// described above, returning the statements to emit BEFORE the (unchanged)
/// primal sort and the plan the sweeps read.
///
/// `preBound` is every name the SURFACE body binds, so a re-entered body
/// (composition) is told from a genuine user collision.
let internal expandSort (fname: string) (ctx: Ctx)
                       (idxTys: Map<string, TypeExpr>) (preBound: Set<string>)
                       (name: string) (operand: Expr) (key: Expr)
    : Result<Stmt list * SortPlan, string> =
    // C2's operand discipline: the sorted side must be a NAMED array whose
    // index type is declared, because the expansion has to spell `range<I>`
    // and annotate the gather lambdas with it.
    match operand.Kind with
    | ExprKind.ExprVar src ->
        (match Map.tryFind src idxTys with
         | None ->
             err fname $"differentiating `sort` needs a named array operand with a declared rank-1 index type; '{src}' has none (annotate the parameter or local as `Array<Float like I>`, or sort a parameter directly)"
         // The expansion SPELLS the index type three times (`range<I>` plus
         // the key and gather lambda annotations), and every syntactic
         // occurrence of an ANONYMOUS `Idx<n>` gets its own nominal identity
         // -- by design, since index provenance is part of the type. Only a
         // named alias gives the occurrences one identity, so require one
         // here rather than let the mismatch surface as a BL3001 pointing at
         // the whole function. (The C2 map rule is unaffected: it spells the
         // index type ONCE.)
         | Some ity when (match ity with TyNamed (_, []) -> false | _ -> true) ->
             err fname $"differentiating `sort` needs the sorted array's index type to be a NAMED alias (v1): '{src}' is declared over an anonymous index type, whose occurrences do not share an identity, so the synthesized permutation would not unify with it. Add `type I = Idx<n>` and declare the array as `Array<Float like I>`"
         | Some ity ->
        // The key must be a lambda of one parameter reading only THAT
        // element. A key closing over ANY other binding in the differentiated
        // scope would make the permutation depend on it, and the
        // permutation-as-data rule drops that dependence -- so refuse rather
        // than answer wrong.
        //
        // `preBound` is every parameter (all ranks, arrays AND scalars) plus
        // every local the surface body binds. The predecessor set here was
        // "rank-1 arrays with a declared index type, plus the operand", which
        // let a key close over a rank-2 array or a plain Float parameter and
        // compile to a silently constant permutation. Over-refusal is the safe
        // direction and costs a hoist; under-refusal is a wrong derivative.
        // MODULE-level constants stay legal (they are not in `preBound`, and
        // they carry no derivative), which is the case worth keeping.
        match key.Kind with
        | ExprKind.ExprLambda ([kp], _, kbody) ->
            let closedOver = Set.remove kp.Name preBound
            if mentionsDeep closedOver kbody then
                err fname $"differentiating `sort` requires a key that reads ONLY the sorted element: the key of '{name}' closes over another binding in scope, so the permutation would depend on data whose contribution the derivative drops (v1). Precompute the key into the sorted array, or sort outside the differentiated function"
            else
            let iotaN = sortIotaName name
            let permN = sortPermName name
            let invN = sortInvName name
            let wantInv = (errMode.Value <> "jvp")
            // Already expanded? A composition round (`ad.jvp(ad.grad(f))`)
            // re-runs this pre-pass over a body this pass itself wrote, so
            // the plumbing is REUSED by name -- which is also what keeps the
            // primal and tangent legs on one permutation.
            let alreadyPerm = Set.contains permN preBound
            let alreadyInv = Set.contains invN preBound
            let collided =
                [iotaN; permN; invN]
                |> List.filter (fun n -> Set.contains n preBound)
            if not (List.isEmpty collided) && not alreadyPerm then
                err fname $"binding '{List.head collided}' collides with a name the sort expansion synthesizes for '{name}'; rename it"
            else
            let pv = "__spk"
            // The permutation is an ARGSORT of the ORIGINAL INDICES under the
            // USER'S key: `sort(iota, lambda(i: I) -> key[x := A(i)])`. The key
            // body has to be COMPOSED here, not dropped -- an argsort of the
            // raw values answers a different question the moment the key is
            // anything but the identity (a descending key `0.0 - x` would come
            // back ascending), and every leg downstream reads this permutation
            // as if it were the primal's own.
            //
            // `substKern` is the substitution because its catch-all DECLINES:
            // it never leaves a live occurrence of the key parameter behind and
            // never crosses a binder it cannot prove safe. A decline is a named
            // refusal, not a half-substituted key.
            match substKern kp.Name (syn (ExprApp (v src, [v pv]))) kbody with
            | None ->
                err fname $"differentiating `sort` cannot rebuild the key of '{name}' over the permutation: a binder or an unsupported form stands between the key parameter '{kp.Name}' and its use, so the composed key cannot be proved capture-free (v1). Simplify the key, or precompute it into the sorted array"
            | Some permKeyBody ->
            // A key that never reads its element sorts by a constant. The
            // permutation would then carry no reference to the sorted array,
            // and `collectSortPlans` -- which recovers the plumbing by SHAPE,
            // because inlining renames locals -- would have nothing to key on.
            if not (mentionsDeep (Set.singleton pv) permKeyBody) then
                err fname $"differentiating `sort` requires a key that reads the sorted element: the key of '{name}' is constant, so the sort is an identity the derivative cannot trace (v1); drop the sort instead"
            else
            let iotaLet =
                if alreadyPerm then []
                else [ StmtLet { Mutability = BindLet; Pattern = synPat (PatVar iotaN); Type = None
                                 Value = gatherForm (syn (ExprRange [ity])) pv ity (v pv) } ]
            let permLet =
                if alreadyPerm then []
                else [ StmtLet { Mutability = BindLet; Pattern = synPat (PatVar permN); Type = None
                                 Value = syn (ExprSort (v iotaN,
                                                        syn (ExprLambda ([lamP pv ity], None, permKeyBody)))) } ]
            let invLet =
                if not wantInv || alreadyInv then []
                else [ StmtLet { Mutability = BindLet; Pattern = synPat (PatVar invN); Type = None
                                 Value = syn (ExprSort (v iotaN,
                                                        syn (ExprLambda ([lamP pv ity], None,
                                                                         syn (ExprApp (v permN, [v pv])))))) } ]
            Ok (iotaLet @ permLet @ invLet,
                { Perm = permN; InvPerm = (if wantInv then invN else ""); IdxTy = ity; Src = src })
        | ExprKind.ExprLambda _ ->
            err fname $"differentiating `sort` requires a single-parameter key lambda (the sorted element); the key of '{name}' takes a different arity"
        | _ ->
            err fname $"differentiating `sort` requires an explicit key lambda `lambda(x: T) -> ...`; the key of '{name}' is not a lambda (v1)")
    | _ ->
        err fname $"differentiating `sort` requires a named array operand (v1); bind the sorted array to a `let` first (in '{name}')"

/// A map application, decomposed. `method_for(ops) <@> k` and
/// `object_for(k) <@> ops` are the SAME loop written two ways, and every
/// consumer here wants the same three things out of either spelling.
type internal MapApplyView = {
    /// The operand list. The object-side spelling carries it on the right,
    /// where a tuple is an operand LIST and anything else is one operand.
    Ops: Expr list
    /// The kernel.
    Kern: Expr
    /// The broadcast mode the application carried (the map rule reads it).
    Mode: BinOpMode
    /// Put a new kernel back in the spelling this was found in, so a rewrite
    /// is minimal. Only the fusion rewrites use it.
    Rebuild: Expr -> Expr
}

/// The decomposition, with a hook for the ONE wrinkle its consumers differ
/// on: a loop side that is a bare NAME (`let L = method_for(A)` then
/// `L <@> k`). `resolve` chases such a name to the value it was bound to;
/// sites with no binding environment pass `noLoopResolve` and decline.
///
/// This is the single definition: four sites (the taint/validation walk, the
/// extent recovery, the grouped-peel matcher, the tangent rule) plus the two
/// pipeline-fusion rewrites used to spell it out inline, and they had drifted
/// on tuple normalization and on what a var loop side means.
let internal (|MapApplyWith|_|) (resolve: string -> Expr option) (e: Expr) : MapApplyView option =
    match e.Kind with
    | ExprKind.ExprBinOp (bm, OpApply, lo, rhs) ->
        let loR =
            match lo.Kind with
            | ExprKind.ExprVar n -> (match resolve n with Some b -> b | None -> lo)
            | _ -> lo
        (match loR.Kind with
         | ExprKind.ExprMethodFor ops ->
             Some { Ops = ops; Kern = rhs; Mode = bm
                    Rebuild = fun k' -> inheritSpan e (ExprBinOp (bm, OpApply, lo, k')) }
         | ExprKind.ExprObjectFor kern ->
             let ops = match rhs.Kind with ExprKind.ExprTuple es -> es | _ -> [rhs]
             Some { Ops = ops; Kern = kern; Mode = bm
                    Rebuild = fun k' ->
                                inheritSpan e (ExprBinOp (bm, OpApply, inheritSpan e (ExprObjectFor k'), rhs)) }
         | _ -> None)
    | _ -> None

/// "No loop-object environment here" -- a var loop side declines.
let internal noLoopResolve : string -> Expr option = fun _ -> None

let internal (|MapApply|_|) (e: Expr) : MapApplyView option = (|MapApplyWith|_|) noLoopResolve e

// ---------------------------------------------------------------------------
// Halo stencil maps (docs/plans/structural/02, sections 3.2-3.3). A
// `method_for(halo<I, [offs]>) <@> lambda(w) -> ...` reads its operands
// through the window, `x(w(k))`; both reverse routes need the same three
// facts about it: its ACCESS record (built here from the surface literals,
// the same record the checker and the runtime guards parse from the slot
// tag), whether a map is a SOLE-halo map, and the rewrite of `w(k)` into
// the interior ordinal plus a static offset.
// ---------------------------------------------------------------------------

/// A literal or negated-literal offset.
let internal literalOffsetOf (e: Expr) : int option =
    match e.Kind with
    | ExprKind.ExprLit (LitInt k) -> Some (int k)
    | ExprKind.ExprUnaryOp (OpNeg, { Kind = ExprKind.ExprLit (LitInt k) }) -> Some (int -k)
    | _ -> None

/// The access record and the inner extent N of a surface `halo<I, [offs]>`
/// operand: literal `Idx<n>` inner, literal offset set. None otherwise (a
/// compound inner or a computed set has no static interior here).
let internal haloAccessOfSurface (ctx: Ctx) (inner: TypeExpr) (offs: Expr) : (Blade.Types.HaloAccess * int) option =
    match resolveTy ctx inner, offs.Kind with
    | TyIdx { Kind = ExprKind.ExprLit (LitInt n) }, ExprKind.ExprArrayLit os when not os.IsEmpty ->
        let lits = os |> List.map literalOffsetOf
        if lits |> List.forall Option.isSome then
            Some (Blade.Types.haloAccessOf "" (lits |> List.map Option.get) false, int n)
        else None
    | _ -> None

/// A map whose ONE loop operand is a halo, looked at through `pure` /
/// `compute` (materialization barriers with no value content): the inner
/// index, the offset expression and the kernel.
let internal soleHaloOfWith (resolve: string -> Expr option) (e: Expr) : (TypeExpr * Expr * Expr) option =
    let rec peel (x: Expr) =
        match x.Kind with
        | ExprKind.ExprCompute i | ExprKind.ExprPure i -> peel i
        | _ -> x
    match peel e with
    | MapApplyWith resolve mv ->
        (match mv.Ops with
         | [ { Kind = ExprKind.ExprHalo (inner, offs) } ] -> Some (inner, offs, mv.Kern)
         | _ -> None)
    | _ -> None

let internal soleHaloOf (e: Expr) : (TypeExpr * Expr * Expr) option = soleHaloOfWith noLoopResolve e

/// The head every subset refusal shares -- what differentiated code DOES
/// support, said once.
let internal subsetHead =
    "differentiable code supports straight-line arithmetic, additive `reduce`, and rank-1 recursive arrays (v1)"

/// The refusal for ONE combinator operator met in differentiated code.
///
/// One message per operator instead of one message listing eleven of them.
/// That list was the right shape while reverse mode refused the whole family
/// uniformly; after the C2-reverse map lowering
/// (`plan-equivariant-nn-notebooks.md` 5.2) the survivors are genuinely
/// different problems with different fixes -- a `<@>` outside `let`
/// initializer position wants a `let`, `<|>` is DISCONTINUOUS and wants
/// `guard`, `<&!>` wants separate folds -- and naming the family named none
/// of them. Each message says what was met and the nearest spelling that
/// works.
let internal combinatorOpMsg (op: BinOp) : string =
    let grad = (errMode.Value <> "jvp")   // grad and vjp share the reverse sweep
    match op with
    | OpApply ->
        if grad then
            $"{subsetHead}; reverse mode differentiates an eager map by lowering it to a construction loop, and it can only expand one that is the WHOLE initializer of a `let` whose loop side resolves to a `method_for`/`object_for` -- bind it first (`let m = method_for(...) <@> <kernel> |> compute`) and read `m` by index"
        else
            $"{subsetHead}; the `<@>` map rule needs a loop side that is a `method_for`/`object_for` (or a name bound to one) -- this application's left operand is neither"
    | OpChoice ->
        $"{subsetHead}; `<|>` (value-keyed choice) is not differentiable in ANY mode and will not become so: it selects the first NON-ZERO cell, so the output JUMPS by the size of the right operand across the switching set -- a discontinuity, not merely a kink, and its failover idiom is engineered to sit on it. Use `guard(p, c)`, whose predicate is explicit and whose false branch is a genuine zero, or a smooth blend. (`<|:>`, the storage-keyed sibling, IS differentiable -- allocation is not a value.)"
    | OpFusion | OpParallel ->
        let nm = if op = OpFusion then "`<&!>` (full fusion)" else "`<&>` (prefix fusion)"
        $"{subsetHead}; {nm} joins several computations into one traversal, and its result is a TUPLE that neither sweep can bind -- write each leg as its own `let` and fold it with its own `reduce`, which computes the same numbers in as many traversals as there are legs"
    | OpArrayProd ->
        $"{subsetHead}; `<*>` concatenates two loops' operand lists -- spell the combined list directly instead (`method_for(A, B) <@> <kernel>`), which is the same loop and is differentiable"
    | OpBind ->
        $"{subsetHead}; `>>=` is a `let` in disguise (materialize, bind, continue) -- write the bind as a `let` and the continuation as ordinary statements"
    | OpFunctor | OpComposeObj | OpComposeMeth | OpCompose ->
        $"{subsetHead}; this pipeline operator survived the fuse-then-differentiate pre-pass, which means fusion DECLINED it -- compose the stages by hand into a single kernel (`lambda(x) -> g(f(x))`), which is the same value by the Compose-Apply identity"
    | OpCons ->
        $"{subsetHead}; `::` builds a recursive-array slice and is differentiable only inside a top-level `let rec` binding in the body"
    | _ ->
        $"{subsetHead}; loop-object combinator operators are not differentiable"

/// Walk an expression, validating it stays inside the differentiable
/// fragment, and call `onVar` for every variable REFERENCE (not index
/// positions, which are int-typed and non-differentiable, but we still
/// visit them for taint bookkeeping of index vars; harmless).
///
/// `inKernel` says the walk is under a map KERNEL body. It gates exactly one
/// arm: `reduce`. In statement position a reduce has always been rewritten
/// away by `hoistReduces` before this walk runs, so meeting one there means
/// the rewrite declined and the refusal is right. A kernel body is an
/// expression the pre-pass never descends into, so a reduce over a
/// rank-carrying kernel parameter is UNLOWERED BY DESIGN and
/// `tangentOfExpr`'s fold rule differentiates it in place.
let rec internal walkExpr (fname: string) (ctx: Ctx) (onVar: string -> unit) (inKernel: bool) (e: Expr) : Result<unit, string> =
    match e with
    | { Kind = ExprKind.ExprLit _ } -> Ok ()
    | { Kind = ExprKind.ExprVar name } -> onVar name; Ok ()
    | { Kind = ExprKind.ExprTyped (inner, _) } -> walkExpr fname ctx onVar inKernel inner
    | { Kind = ExprKind.ExprUnaryOp (OpNeg, inner) } -> walkExpr fname ctx onVar inKernel inner
    | { Kind = ExprKind.ExprUnaryOp (OpNot, inner) } -> walkExpr fname ctx onVar inKernel inner
    | { Kind = ExprKind.ExprUnaryOp _ } -> err fname "unsupported unary operator in differentiated code"
    // C7 plumbing, admitted in BOTH modes (it must precede LinearForm, which
    // would otherwise walk into the `<@>` and hit grad's combinator refusal).
    // The materialized identity index array computes no float and reads no
    // array: nothing to taint, nothing to differentiate.
    | IndexIota -> Ok ()
    // C7: `sort`. The OPERAND is visited (its taint is the sorted result's);
    // the KEY deliberately is NOT. The permutation is piecewise constant in
    // the data, so it carries no derivative -- and a key that closed over
    // differentiable data was already refused by the pre-pass, which is what
    // makes skipping it safe rather than silent. Visiting it would instead
    // MIS-taint the synthesized permutation arrays (whose keys read the
    // differentiable source) as differentiable index data.
    | { Kind = ExprKind.ExprSort (arr, _) } -> walkExpr fname ctx onVar inKernel arr
    // C1/C6: the linear closure. Forward mode admits the whole family;
    // reverse mode admits it too, because every form either has an adjoint
    // arm (adjointOfInit) or is refused THERE with a named message -- so
    // widening this walk cannot produce a silent zero.
    | LinearForm (ops, _) ->
        ops |> iterR (walkExpr fname ctx onVar inKernel)
    // gram is bilinear, not linear -- its own arm (C6 reverse, jvp tangent)
    | { Kind = ExprKind.ExprGram (ga, gb) } ->
        walkExpr fname ctx onVar inKernel ga |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel gb)
    // gram_apply is trilinear -- its own arms (reverse, jvp tangent)
    | { Kind = ExprKind.ExprGramApply (ga, gb, gx) } ->
        walkExpr fname ctx onVar inKernel ga
        |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel gb)
        |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel gx)
    // Grouping data is constant plumbing in BOTH modes: keys are Int/index
    // data, so a `group_keys`/`group_bucket`/`extents` binding carries no
    // derivative and the explicit-bucket gather pattern (2.17a) rides the
    // ordinary data-dependent-read machinery.
    | { Kind = ExprKind.ExprGroupKeys keys } ->
        keys |> iterR (walkExpr fname ctx onVar inKernel)
    | { Kind = ExprKind.ExprGroupBucket g } -> walkExpr fname ctx onVar inKernel g
    | { Kind = ExprKind.ExprExtents a } -> walkExpr fname ctx onVar inKernel a
    // (Their `|> compute` materializations need no arm of their own: `compute`
    // IS a LinearForm, so the arm above already walked through it.)
    // Bare loop objects are LEGAL let initializers in jvp mode (the binding
    // is resolved at its application site); operands are visited for taint.
    | { Kind = ExprKind.ExprMethodFor arrs } when errMode.Value = "jvp" ->
        arrs |> iterR (fun a ->
            match a.Kind with
            | ExprKind.ExprHalo _ | ExprKind.ExprRange _ -> Ok ()
            | _ -> walkExpr fname ctx onVar inKernel a)
    // A let-bound `object_for(kern)` is a deferred iteration and gets no
    // tangent of its own -- but its kernel may CAPTURE differentiable data, and
    // the binding then carries that dependence to every application site (which
    // resolves the loop object by name and re-taints from it). Walking nothing
    // here meant a result whose ONLY route to a differentiable parameter ran
    // through a capture came out with a silent ZERO tangent, or -- worse -- was
    // refused as "does not depend on any differentiable parameter", which was
    // simply untrue. Taint ONLY: the kernel body's validation belongs to the
    // apply site (which walks it properly, with `inKernel` set), and adding a
    // second validation here would refuse bindings that are never applied.
    | { Kind = ExprKind.ExprObjectFor kern } when errMode.Value = "jvp" ->
        let captures (ps: LambdaParam list) (body: Expr) =
            let bound = ps |> List.map _.Name |> Set.ofList
            Set.difference (allVarsDeep body) bound
        (match kern.Kind with
         | ExprKind.ExprLambda (ps, _, kbody)
         | ExprKind.ExprReynolds ({ Kind = ExprKind.ExprLambda (ps, _, kbody) }, _) ->
             captures ps kbody |> Set.iter onVar
         // a named-function or intrinsic kernel: the NAME carries whatever
         // taint it has, exactly as at the apply site
         | ExprKind.ExprVar n -> onVar n
         | other -> allVarsDeep (inheritSpan kern other) |> Set.iter onVar)
        Ok ()
    // C2-C5: the rank-0 map (both spellings). Visit the operand arrays (so
    // taint sees them; virtual halo/range operands have nothing to visit)
    // and the kernel BODY; kernel params are bound by the lambda, and the
    // tangent rule substitutes indexed reads for them.
    // In reverse mode every map has been lowered before this walk runs --
    // except a sole-halo stencil map the gather route KEEPS (route G,
    // GradNormalize.haloGatherPlan), which is admitted here on the same
    // terms as the forward-mode map.
    | { Kind = ExprKind.ExprBinOp (_, OpApply, lo2, kn) } when (errMode.Value = "jvp" || (soleHaloOf e).IsSome)
            && (match lo2.Kind with ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _ | ExprKind.ExprVar _ -> true | _ -> false) ->
        // Both spellings decompose the same way. A VAR loop side does not
        // resolve here (this walk has no loop-binding environment): the name
        // itself carries the taint, and the right side is dispatched on its
        // shape below -- a tuple there is an operand list, not a kernel.
        let arrays, kernE =
            match e with
            | MapApply m -> m.Ops, m.Kern
            | _ -> [], kn
        let walkOperand (a: Expr) =
            match a.Kind with
            | ExprKind.ExprHalo _ | ExprKind.ExprRange _ -> Ok ()
            | _ -> walkExpr fname ctx onVar inKernel a
        // a var-bound loop side carries its binding's taint to the result
        (match lo2.Kind with ExprKind.ExprVar n -> onVar n | _ -> ())
        arrays |> iterR walkOperand
        |> Result.bind (fun () ->
            match kernE.Kind with
            | ExprKind.ExprLambda (_, _, body) -> walkExpr fname ctx onVar true body
            | ExprKind.ExprReynolds ({ Kind = ExprKind.ExprLambda (_, _, body) }, _) -> walkExpr fname ctx onVar true body
            | ExprKind.ExprVar n -> onVar n; Ok ()
            // a var-bound object_for applied to a tuple: the right side is
            // OPERANDS, not a kernel -- walk them for taint
            | ExprKind.ExprTuple es -> es |> iterR walkOperand
            | _ -> err fname kernUnsupportedMsg)
    // Combinator OPERATORS. The syntactic combinator forms (lambda /
    // method_for / ...) are rejected further down, but the operator spellings
    // are ordinary BinOps and would otherwise slip through this walk when both
    // operands are plain names, dying much later in the adjoint with a generic
    // "unsupported expression form" that never says the word combinator.
    // `<|:>` (OpFallback) is deliberately ABSENT: LinearForm admits it above
    // (storage branching is linear in both legs), so listing it here would be
    // both unreachable and a lie in the message.
    | { Kind = ExprKind.ExprBinOp (_, ((OpApply | OpBind | OpParallel | OpFusion | OpArrayProd | OpFunctor | OpChoice | OpComposeObj | OpComposeMeth | OpCompose | OpCons) as op), _, _) } ->
        err fname (combinatorOpMsg op)
    | { Kind = ExprKind.ExprBinOp (_, _, l, r) } ->
        walkExpr fname ctx onVar inKernel l |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel r)
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, args) } ->
        // intrinsic, user call (inlined earlier), or array read -- all
        // fine structurally; recurse into arguments.
        onVar name
        args |> iterR (walkExpr fname ctx onVar inKernel)
    | { Kind = ExprKind.ExprApp _ } -> err fname "only named calls and array reads are supported in differentiated code"
    // A tuple projection (`f[0]`, the elaborated spelling of a solve's factor
    // operand): the tuple's name carries whatever taint it has.
    | { Kind = ExprKind.ExprTupleIndex (t, _) } -> walkExpr fname ctx onVar inKernel t
    | { Kind = ExprKind.ExprArrayLit elems } ->
        elems |> iterR (walkExpr fname ctx onVar inKernel)
    | { Kind = ExprKind.ExprIf (c, t, f) } ->
        // Forward mode admits branches: the tangent is the branch of
        // tangents under the SAME condition (exact off the boundary,
        // subgradient convention at it -- the primal is not differentiable
        // there either). Reverse mode still refuses: the adjoint would
        // need the taken branch recorded.
        if errMode.Value = "jvp" then
            walkExpr fname ctx onVar inKernel c
            |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel t)
            |> Result.bind (fun () -> walkExpr fname ctx onVar inKernel f)
        else err fname "if/else is not supported in differentiated code yet"
    | { Kind = ExprKind.ExprMatch _ } -> err fname "match is not supported in differentiated code"
    | { Kind = ExprKind.ExprBlock _ } -> err fname "nested block expressions are not supported in differentiated code"
    | { Kind = ExprKind.ExprLet _ } -> err fname "expression-level let is not supported in differentiated code"
    | ConstFill _ -> Ok ()   // literal fill: computes nothing, reads nothing
    // C8: a fold inside a KERNEL body stays where it is (see the `inKernel`
    // note on this function). Walk the source and the init for taint; the
    // fold kernel is a section, which binds nothing and reads nothing.
    | { Kind = ExprKind.ExprReduce (src, _, initOpt, _) } when inKernel && errMode.Value = "jvp" ->
        walkExpr fname ctx onVar inKernel src
        |> Result.bind (fun () ->
            match initOpt with
            | Some ie -> walkExpr fname ctx onVar inKernel ie
            | None -> Ok ())
    | { Kind = ExprKind.ExprReduce _ } ->
        err fname "differentiable code supports straight-line arithmetic, additive `reduce`, and rank-1 recursive arrays (v1); this reduce could not be normalized (only `reduce(A, (+)[, init])` over an array variable or inline literal is differentiable)"
    | { Kind = ExprKind.ExprRecArray _ } ->
        err fname "differentiable code supports straight-line arithmetic, additive `reduce`, and rank-1 recursive arrays (v1); a recursive array is differentiable only as a top-level `let rec` binding in the body"
    // A bare loop object or kernel that never reached an APPLICATION the
    // rules could expand. (`compute`/`pure` never arrive here -- they are
    // LinearForms, admitted above -- and are listed only so a future grammar
    // change trips the exhaustiveness check rather than falling through.)
    | { Kind = ExprKind.ExprLambda _ } ->
        err fname (if errMode.Value = "grad"
                   then $"{subsetHead}; this kernel lambda is never applied to a loop object in a position reverse mode can lower, so there is nothing to differentiate it AGAINST -- apply it (`let m = <loop> <@> <kernel> |> compute`) where its result is used"
                   else $"{subsetHead}; a kernel lambda is differentiable only as the kernel of a `<@>` application, not as a value in its own right")
    | { Kind = ExprKind.ExprMethodFor _ } | { Kind = ExprKind.ExprObjectFor _ } ->
        err fname $"{subsetHead}; a bare loop object computes nothing until it is APPLIED -- bind it and apply it (`let L = method_for(...)` then `let m = L <@> <kernel> |> compute`), which is the shape both sweeps expand"
    | { Kind = ExprKind.ExprCompute _ } | { Kind = ExprKind.ExprPure _ } ->
        err fname $"{subsetHead}; loop-object combinators (compute/pure) are not differentiable here"
    | { Kind = ExprKind.ExprTuple _ } -> err fname "tuple values are not supported in differentiated code"
    | { Kind = ExprKind.ExprField _ } -> err fname "struct field access is not supported in differentiated code"
    | _ -> err fname "unsupported expression form in differentiated code"

// Renaming (for inlining)

/// Names BOUND by a pattern (binders only; struct field labels and variant
/// constructor names are not value names).
let rec internal patternBoundNames (p: Pattern) : string list =
    match p.Kind with
    | PatternKind.PatWildcard | PatternKind.PatLit _ -> []
    | PatternKind.PatVar n -> [n]
    | PatternKind.PatTuple ps -> ps |> List.collect patternBoundNames
    | PatternKind.PatCons (a, b) -> patternBoundNames a @ patternBoundNames b
    | PatternKind.PatStruct (_, fields) -> fields |> List.collect (fun (_, sub) -> patternBoundNames sub)
    | PatternKind.PatVariant (_, sub) -> sub |> Option.map patternBoundNames |> Option.defaultValue []
    | PatternKind.PatGuarded (sub, _) -> patternBoundNames sub
    | PatternKind.PatTyped (sub, _) -> patternBoundNames sub

/// Drop `names` from the rename map: an inner binder shadows them.
let internal shadowNames (names: string list) (ren: Map<string, string>) : Map<string, string> =
    names |> List.fold (fun m n -> Map.remove n m) ren

/// Guard expressions embedded in a pattern (PatGuarded), in order.
let rec internal patGuardExprs (p: Pattern) : Expr list =
    match p.Kind with
    | PatternKind.PatWildcard | PatternKind.PatVar _ | PatternKind.PatLit _ -> []
    | PatternKind.PatTuple ps -> ps |> List.collect patGuardExprs
    | PatternKind.PatCons (a, b) -> patGuardExprs a @ patGuardExprs b
    | PatternKind.PatStruct (_, fields) -> fields |> List.collect (fun (_, sub) -> patGuardExprs sub)
    | PatternKind.PatVariant (_, sub) -> sub |> Option.map patGuardExprs |> Option.defaultValue []
    | PatternKind.PatGuarded (sub, g) -> patGuardExprs sub @ [g]
    | PatternKind.PatTyped (sub, _) -> patGuardExprs sub

let internal captureMsg (src: string) (tgt: string) : string =
    $"inlining would rename '{src}' to '{tgt}', which a nested binder of the same name would capture -- rename the local binder"

/// Does `name` occur FREE in `e` -- i.e. would `renameExpr` rewrite at least
/// one reference to it? Exhaustive over ExprKind and SHADOWING-AWARE, arm for
/// arm the same walk `renameExpr` performs, because that is exactly the
/// question `captureCheck` asks. It is not `mentionsDeep`: that one ignores
/// inner binders, and over-reporting here means refusing an inlining that is
/// perfectly safe -- the wrong direction for a refusal.
///
/// Two arms are easy to lose and both matter: `ExprArity p` names a value
/// (`renameExpr` rewrites it), and a recursive array's `Name` is renamed with
/// the OUTER map while its prefix/step vars shadow only the slice. A
/// where-clause is deliberately absent, matching `renameExpr`: its names are
/// kernel-param-local.
let rec internal occursFree (name: string) (e: Expr) : bool =
    let o = occursFree name
    let any = List.exists o
    let opt x = match x with Some i -> o i | None -> false
    /// A binder scope: shadowed binders make the whole scope inert.
    let under (binders: string list) (scope: Expr list) =
        not (List.contains name binders) && any scope
    match e.Kind with
    | ExprKind.ExprVar n -> n = name
    | ExprKind.ExprArity p -> p = name
    | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprQualified _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _
    | ExprKind.ExprRange _ | ExprKind.ExprReverse _ -> false
    | ExprKind.ExprUnaryOp (_, i) -> o i
    | ExprKind.ExprTyped (i, _) | ExprKind.ExprHalo (_, i)
    | ExprKind.ExprPure i | ExprKind.ExprCompute i | ExprKind.ExprRead i
    | ExprKind.ExprRank i | ExprKind.ExprUnique i | ExprKind.ExprGroupBucket i
    | ExprKind.ExprExtents i | ExprKind.ExprStatic i | ExprKind.ExprObjectFor i
    | ExprKind.ExprTranspose (i, _, _) | ExprKind.ExprDecompact (i, _)
    | ExprKind.ExprPartialApp (_, i, _) | ExprKind.ExprReynolds (i, _)
    | ExprKind.ExprField (i, _) -> o i
    | ExprKind.ExprBinOp (_, _, l, r) | ExprKind.ExprTupleIndex (l, r)
    | ExprKind.ExprDotDot (l, r) | ExprKind.ExprGuard (l, r)
    | ExprKind.ExprReplicate (l, r) | ExprKind.ExprMask (l, r)
    | ExprKind.ExprCompound (l, r) | ExprKind.ExprSparse (l, r)
    | ExprKind.ExprIntersect (l, r) | ExprKind.ExprUnion (l, r)
    | ExprKind.ExprContains (l, r) | ExprKind.ExprGroupBy (l, r)
    | ExprKind.ExprSort (l, r) | ExprKind.ExprGram (l, r)
    | ExprKind.ExprAssign (l, r) -> o l || o r
    | ExprKind.ExprGramApply (a, b, x) -> o a || o b || o x
    | ExprKind.ExprApp (f, args) -> o f || any args
    | ExprKind.ExprIf (c, t, f) -> o c || o t || o f
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprMethodFor es
    | ExprKind.ExprZip es | ExprKind.ExprStack es | ExprKind.ExprSequence es
    | ExprKind.ExprGroupKeys es -> any es
    | ExprKind.ExprAlign (es, _) | ExprKind.ExprJoin (es, _) -> any es
    | ExprKind.ExprReduce (a, k, i, ax) -> o a || o k || opt i || opt ax
    | ExprKind.ExprStruct (_, fields, spread) ->
        (fields |> List.exists (fun (_, fe) -> o fe)) || opt spread
    | ExprKind.ExprFor (src, _, k) ->
        (match src with
         | ForArrays (es, inc) -> any es || opt inc
         | ForKernel k2 -> o k2)
        || opt k
    // Binders. Params shadow the defaults as well as the body (renameExpr
    // renames both with the shadowed map).
    | ExprKind.ExprLambda (ps, _, body) ->
        under (ps |> List.map _.Name)
              (body :: (ps |> List.choose _.Default))
    // A `let`'s VALUE is outside its own binder; the guards and body are in.
    | ExprKind.ExprLet (b, body) ->
        o b.Value || under (patternBoundNames b.Pattern) (patGuardExprs b.Pattern @ [body])
    | ExprKind.ExprMatch (scrut, cases) ->
        o scrut
        || (cases |> List.exists (fun c ->
                under (patternBoundNames c.Pattern)
                      (patGuardExprs c.Pattern @ Option.toList c.Guard @ [c.Body])))
    | ExprKind.ExprBlock (stmts, lastOpt) ->
        // Statement binders shadow the REST of the block, sequentially; a
        // `for` variable shadows only its own body.
        let rec goStmts (shadowed: bool) (ss: Stmt list) : bool * bool =
            match ss with
            | [] -> (false, shadowed)
            | s :: rest ->
                let (hit, shadowed') = goStmt shadowed s
                if hit then (true, shadowed')
                else goStmts shadowed' rest
        and goStmt (shadowed: bool) (s: Stmt) : bool * bool =
            match s with
            | StmtSpanned (inner, _) -> goStmt shadowed inner
            | StmtLet b ->
                let hit = not shadowed && o b.Value
                (hit, shadowed || List.contains name (patternBoundNames b.Pattern))
            | StmtAssign (l, _, r) -> ((not shadowed && (o l || o r)), shadowed)
            | StmtExpr inner -> ((not shadowed && o inner), shadowed)
            | StmtForIn (vn, rng, body) ->
                if not shadowed && o rng then (true, shadowed)
                else (fst (goStmts (shadowed || vn = name) body), shadowed)
        let (hit, shadowedEnd) = goStmts false stmts
        hit || (not shadowedEnd && opt lastOpt)
    | ExprKind.ExprRecArray d ->
        // The NAME is the enclosing let's binder and is renamed with the
        // outer map; the prefix/step vars shadow the slice, the seed var its
        // own arm.
        d.Name = name
        || under [d.PrefixVar; d.StepVar] ([d.SliceExpr] @ Option.toList d.Guard)
        || (match d.SeedArm with
            | Some (sv, se) -> under [sv] [se]
            | None -> false)

/// Rename variable references per `ren` (total map application: names not in
/// the map pass through). Exhaustive over ExprKind BY DESIGN: the previous
/// version walked only a small fragment behind a wildcard, which silently
/// skipped if/lambda/sort/stack bodies and left an inlined callee's
/// references pointing at its pre-rename parameters. A new expression form
/// now trips the compiler's exhaustiveness check instead. Nested binders
/// SHADOW the map (top-level callee locals are renamed by renameNStmts,
/// which is the inliner's collision mechanism); capture is refused via
/// captureCheck. Type annotations and where-clauses are left untouched:
/// their names are type names or kernel-param-local (shadowed scope) by
/// construction.
let rec internal renameExpr (ren: Map<string, string>) (e: Expr) : Result<Expr, string> =
    let rn n = Map.tryFind n ren |> Option.defaultValue n
    let re k = inheritSpan e k
    let r = renameExpr ren
    let rlist es = traverseR r es
    let ropt eo =
        match eo with
        | None -> Ok None
        | Some x -> r x |> Result.map Some
    match e.Kind with
    // Leaves: no value names inside (ExprRange/ExprReverse carry only types).
    | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprQualified _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _
    | ExprKind.ExprRange _ | ExprKind.ExprReverse _ -> Ok e
    | ExprKind.ExprVar name -> Ok (re (ExprVar (rn name)))
    | ExprKind.ExprArity p -> Ok (re (ExprArity (rn p)))
    // Structural: rename every child.
    | ExprKind.ExprTyped (inner, t) -> r inner |> Result.map (fun i -> re (ExprTyped (i, t)))
    | ExprKind.ExprUnaryOp (op, inner) -> r inner |> Result.map (fun i -> re (ExprUnaryOp (op, i)))
    | ExprKind.ExprBinOp (m, op, l, rhs) ->
        r l |> Result.bind (fun l' -> r rhs |> Result.map (fun r' -> re (ExprBinOp (m, op, l', r'))))
    | ExprKind.ExprApp (f, args) ->
        r f |> Result.bind (fun f' -> rlist args |> Result.map (fun args' -> re (ExprApp (f', args'))))
    | ExprKind.ExprTupleIndex (t, i) ->
        r t |> Result.bind (fun t' -> r i |> Result.map (fun i' -> re (ExprTupleIndex (t', i'))))
    | ExprKind.ExprField (inner, fld) -> r inner |> Result.map (fun i -> re (ExprField (i, fld)))
    | ExprKind.ExprIf (c, t, f) ->
        r c |> Result.bind (fun c' -> r t |> Result.bind (fun t' -> r f |> Result.map (fun f' -> re (ExprIf (c', t', f')))))
    | ExprKind.ExprTuple es -> rlist es |> Result.map (fun es' -> re (ExprTuple es'))
    | ExprKind.ExprArrayLit es -> rlist es |> Result.map (fun es' -> re (ExprArrayLit es'))
    | ExprKind.ExprMethodFor es -> rlist es |> Result.map (fun es' -> re (ExprMethodFor es'))
    | ExprKind.ExprObjectFor k -> r k |> Result.map (fun k' -> re (ExprObjectFor k'))
    | ExprKind.ExprDotDot (l, h) ->
        r l |> Result.bind (fun l' -> r h |> Result.map (fun h' -> re (ExprDotDot (l', h'))))
    | ExprKind.ExprHalo (t, offs) -> r offs |> Result.map (fun o -> re (ExprHalo (t, o)))
    | ExprKind.ExprZip es -> rlist es |> Result.map (fun es' -> re (ExprZip es'))
    | ExprKind.ExprAlign (es, spec) -> rlist es |> Result.map (fun es' -> re (ExprAlign (es', spec)))
    | ExprKind.ExprStack es -> rlist es |> Result.map (fun es' -> re (ExprStack es'))
    | ExprKind.ExprJoin (es, d) -> rlist es |> Result.map (fun es' -> re (ExprJoin (es', d)))
    | ExprKind.ExprPure inner -> r inner |> Result.map (fun i -> re (ExprPure i))
    | ExprKind.ExprCompute inner -> r inner |> Result.map (fun i -> re (ExprCompute i))
    | ExprKind.ExprRead inner -> r inner |> Result.map (fun i -> re (ExprRead i))
    | ExprKind.ExprGuard (c, b) ->
        r c |> Result.bind (fun c' -> r b |> Result.map (fun b' -> re (ExprGuard (c', b'))))
    | ExprKind.ExprSequence es -> rlist es |> Result.map (fun es' -> re (ExprSequence es'))
    | ExprKind.ExprReplicate (c, b) ->
        r c |> Result.bind (fun c' -> r b |> Result.map (fun b' -> re (ExprReplicate (c', b'))))
    | ExprKind.ExprReynolds (k, anti) -> r k |> Result.map (fun k' -> re (ExprReynolds (k', anti)))
    | ExprKind.ExprRank inner -> r inner |> Result.map (fun i -> re (ExprRank i))
    | ExprKind.ExprMask (a, p) ->
        r a |> Result.bind (fun a' -> r p |> Result.map (fun p' -> re (ExprMask (a', p'))))
    | ExprKind.ExprCompound (d, m) ->
        r d |> Result.bind (fun d' -> r m |> Result.map (fun m' -> re (ExprCompound (d', m'))))
    | ExprKind.ExprSparse (v, k) ->
        r v |> Result.bind (fun v' -> r k |> Result.map (fun k' -> re (ExprSparse (v', k'))))
    | ExprKind.ExprIntersect (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> re (ExprIntersect (a', b'))))
    | ExprKind.ExprUnion (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> re (ExprUnion (a', b'))))
    | ExprKind.ExprUnique a -> r a |> Result.map (fun a' -> re (ExprUnique a'))
    | ExprKind.ExprContains (a, v) ->
        r a |> Result.bind (fun a' -> r v |> Result.map (fun v' -> re (ExprContains (a', v'))))
    | ExprKind.ExprGroupBy (v, g) ->
        r v |> Result.bind (fun v' -> r g |> Result.map (fun g' -> re (ExprGroupBy (v', g'))))
    | ExprKind.ExprGroupKeys es -> rlist es |> Result.map (fun es' -> re (ExprGroupKeys es'))
    | ExprKind.ExprGroupBucket g -> r g |> Result.map (fun g' -> re (ExprGroupBucket g'))
    | ExprKind.ExprSort (a, k) ->
        r a |> Result.bind (fun a' -> r k |> Result.map (fun k' -> re (ExprSort (a', k'))))
    | ExprKind.ExprReduce (a, k, i, ax) ->
        r a |> Result.bind (fun a' -> r k |> Result.bind (fun k' ->
        ropt i |> Result.bind (fun i' -> ropt ax |> Result.map (fun ax' ->
        re (ExprReduce (a', k', i', ax'))))))
    | ExprKind.ExprTranspose (a, d1, d2) -> r a |> Result.map (fun a' -> re (ExprTranspose (a', d1, d2)))
    | ExprKind.ExprDecompact (a, d) -> r a |> Result.map (fun a' -> re (ExprDecompact (a', d)))
    | ExprKind.ExprGram (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> re (ExprGram (a', b'))))
    | ExprKind.ExprGramApply (a, b, x) ->
        r a |> Result.bind (fun a' -> r b |> Result.bind (fun b' -> r x |> Result.map (fun x' -> re (ExprGramApply (a', b', x')))))
    | ExprKind.ExprExtents a -> r a |> Result.map (fun a' -> re (ExprExtents a'))
    | ExprKind.ExprStruct (nm, fields, spread) ->
        fields
        |> traverseR (fun (fn, fe) -> r fe |> Result.map (fun fe' -> (fn, fe')))
        |> Result.bind (fun fields' ->
            ropt spread |> Result.map (fun sp' -> re (ExprStruct (nm, fields', sp'))))
    | ExprKind.ExprPartialApp (op, inner, isLeft) ->
        r inner |> Result.map (fun i -> re (ExprPartialApp (op, i, isLeft)))
    | ExprKind.ExprAssign (l, rhs) ->
        r l |> Result.bind (fun l' -> r rhs |> Result.map (fun r' -> re (ExprAssign (l', r'))))
    | ExprKind.ExprStatic inner -> r inner |> Result.map (fun i -> re (ExprStatic i))
    // Binders: shadow, check capture against the scope's actual contents,
    // then rename the bodies.
    | ExprKind.ExprLambda (ps, wc, body) ->
        let names = ps |> List.map _.Name
        let ren' = shadowNames names ren
        captureCheck ren' names (body :: (ps |> List.choose _.Default)) |> Result.bind (fun () ->
        ps
        |> traverseR (fun p ->
            match p.Default with
            | None -> Ok p
            | Some d -> renameExpr ren' d |> Result.map (fun d' -> { p with Default = Some d' }))
        |> Result.bind (fun ps' ->
        renameExpr ren' body |> Result.map (fun b' -> re (ExprLambda (ps', wc, b')))))
    | ExprKind.ExprLet (b, body) ->
        r b.Value |> Result.bind (fun v' ->
        let names = patternBoundNames b.Pattern
        let ren' = shadowNames names ren
        captureCheck ren' names (patGuardExprs b.Pattern @ [body]) |> Result.bind (fun () ->
        renamePatGuards ren' b.Pattern |> Result.bind (fun pat' ->
        renameExpr ren' body |> Result.map (fun body' ->
            re (ExprLet ({ b with Pattern = pat'; Value = v' }, body'))))))
    | ExprKind.ExprMatch (scrut, cases) ->
        // Per-case: pattern names shadow both the (pattern-embedded and
        // case-level) guards and the body. Pattern structure itself binds,
        // it does not reference -- only PatGuarded carries an expression.
        r scrut |> Result.bind (fun scrut' ->
        cases
        |> traverseR (fun (c: MatchCase) ->
            let names = patternBoundNames c.Pattern
            let ren' = shadowNames names ren
            let scope = patGuardExprs c.Pattern @ Option.toList c.Guard @ [c.Body]
            captureCheck ren' names scope |> Result.bind (fun () ->
            renamePatGuards ren' c.Pattern |> Result.bind (fun pat' ->
            (match c.Guard with
             | None -> Ok None
             | Some g -> renameExpr ren' g |> Result.map Some)
            |> Result.bind (fun g' ->
            renameExpr ren' c.Body |> Result.map (fun b' ->
                { c with Pattern = pat'; Guard = g'; Body = b' })))))
        |> Result.map (fun cases' -> re (ExprMatch (scrut', cases'))))
    | ExprKind.ExprBlock (stmts, lastOpt) ->
        // Statement binders shadow the REST of the block, sequentially.
        let rec goStmt renCur st : Result<Stmt * Map<string, string>, string> =
            match st with
            | StmtSpanned (inner, sp) ->
                goStmt renCur inner |> Result.map (fun (s', renNext) -> (StmtSpanned (s', sp), renNext))
            | StmtLet b ->
                // Conservative capture policy for block binders: enumerating
                // the binder's scope (the REST of the block) is not worth
                // the machinery for so rare a collision -- refuse on the
                // name clash alone.
                renameExpr renCur b.Value |> Result.bind (fun v' ->
                let names = patternBoundNames b.Pattern
                let renNext = shadowNames names renCur
                match renNext |> Map.tryPick (fun src tgt -> if List.contains tgt names then Some (src, tgt) else None) with
                | Some (src, tgt) -> Error (captureMsg src tgt)
                | None -> Ok (StmtLet { b with Value = v' }, renNext))
            | StmtAssign (l, op, rhs) ->
                renameExpr renCur l |> Result.bind (fun l' ->
                renameExpr renCur rhs |> Result.map (fun r' -> (StmtAssign (l', op, r'), renCur)))
            | StmtExpr inner ->
                renameExpr renCur inner |> Result.map (fun i -> (StmtExpr i, renCur))
            | StmtForIn (v, rng, body) ->
                renameExpr renCur rng |> Result.bind (fun rng' ->
                let renBody = shadowNames [v] renCur
                (match renBody |> Map.tryPick (fun src tgt -> if tgt = v then Some (src, tgt) else None) with
                 | Some (src, tgt) -> Error (captureMsg src tgt)
                 | None ->
                     body
                     |> List.fold (fun acc s2 ->
                         acc |> Result.bind (fun (ys, renB) ->
                             goStmt renB s2 |> Result.map (fun (s', renB') -> (s' :: ys, renB'))))
                         (Ok ([], renBody))
                     |> Result.map (fun (body', _) -> (StmtForIn (v, rng', List.rev body'), renCur))))
        stmts
        |> List.fold (fun acc st ->
            acc |> Result.bind (fun (ys, renCur) ->
                goStmt renCur st |> Result.map (fun (s', renNext) -> (s' :: ys, renNext))))
            (Ok ([], ren))
        |> Result.bind (fun (stmts', renEnd) ->
            (match lastOpt with
             | None -> Ok None
             | Some l -> renameExpr renEnd l |> Result.map Some)
            |> Result.map (fun l' -> re (ExprBlock (List.rev stmts', l'))))
    | ExprKind.ExprFor (src, constraints, kernOpt) ->
        // Constraint names are kernel-param-local (shadowed scope) -- carried
        // unchanged, same treatment as lambda where-clauses.
        (match src with
         | ForArrays (es, inOpt) ->
             rlist es |> Result.bind (fun es' -> ropt inOpt |> Result.map (fun i' -> ForArrays (es', i')))
         | ForKernel k -> r k |> Result.map ForKernel)
        |> Result.bind (fun src' ->
            ropt kernOpt |> Result.map (fun k' -> re (ExprFor (src', constraints, k'))))
    | ExprKind.ExprRecArray def ->
        // Name is the enclosing let's binder; the inliner renames that NLet
        // with the same map, so rename it here consistently. Prefix/step
        // vars are def-local binders and shadow (precise capture probe on
        // the slice/seed scopes).
        let sliceNames = [def.PrefixVar; def.StepVar]
        let renSlice = shadowNames sliceNames ren
        // The guard scopes exactly like the slice (reads prefix/step under
        // the same binders), so it renames under the same shadowed map.
        captureCheck renSlice sliceNames ([def.SliceExpr] @ Option.toList def.Guard) |> Result.bind (fun () ->
        renameExpr renSlice def.SliceExpr |> Result.bind (fun slice' ->
        (match def.Guard with
         | None -> Ok None
         | Some g -> renameExpr renSlice g |> Result.map Some)
        |> Result.bind (fun guard' ->
        (match def.SeedArm with
         | None -> Ok None
         | Some (sv, se) ->
             let renSeed = shadowNames [sv] ren
             captureCheck renSeed [sv] [se] |> Result.bind (fun () ->
             renameExpr renSeed se |> Result.map (fun se' -> Some (sv, se'))))
        |> Result.map (fun seed' ->
            re (ExprRecArray { def with Name = rn def.Name; SeedArm = seed'; SliceExpr = slice'; Guard = guard' })))))

/// A shadowed scope captures a renamed reference only when a SURVIVING
/// mapping's target equals a binder name AND its source occurs FREE in the
/// scope. Without the occurrence test the check would refuse every callee
/// whose lambda params reuse a caller's short name (`x`, `i`, `k`) -- far
/// too eager.
///
/// This ran on EVERY binder of every renamed body, and used to answer both
/// questions the expensive way: `Map.toList` to find the surviving mappings
/// (allocating the whole map as a list per binder, almost always to filter
/// it down to nothing), and, per collision, a probe RENAME of the entire
/// scope followed by a structural comparison of the two trees. `occursFree`
/// is the same question asked directly -- one early-exit walk, no allocation
/// -- and `Map.tryPick` never builds the list.
and internal captureCheck (ren': Map<string, string>) (names: string list) (scope: Expr list) : Result<unit, string> =
    // tryPick walks in key order, so the mapping reported on a collision is
    // the same one the filter-and-iterate version reported.
    match ren' |> Map.tryPick (fun src tgt ->
                     if List.contains tgt names && scope |> List.exists (occursFree src)
                     then Some (src, tgt) else None) with
    | Some (src, tgt) -> Error (captureMsg src tgt)
    | None -> Ok ()

/// Rename the guard EXPRESSIONS embedded in a pattern (PatGuarded); the
/// pattern's binder structure is untouched. `renInner` is the already-
/// shadowed map for the pattern's own scope.
and internal renamePatGuards (renInner: Map<string, string>) (p: Pattern) : Result<Pattern, string> =
    match p.Kind with
    | PatternKind.PatWildcard | PatternKind.PatVar _ | PatternKind.PatLit _ -> Ok p
    | PatternKind.PatTuple ps ->
        ps
        |> traverseR (renamePatGuards renInner)
        |> Result.map (fun ps' -> { p with Kind = PatternKind.PatTuple ps' })
    | PatternKind.PatCons (a, b) ->
        renamePatGuards renInner a |> Result.bind (fun a' ->
        renamePatGuards renInner b |> Result.map (fun b' -> { p with Kind = PatternKind.PatCons (a', b') }))
    | PatternKind.PatStruct (snm, sfields) ->
        sfields
        |> traverseR (fun (fn, sub) -> renamePatGuards renInner sub |> Result.map (fun s' -> (fn, s')))
        |> Result.map (fun sfields' -> { p with Kind = PatternKind.PatStruct (snm, sfields') })
    | PatternKind.PatVariant (vnm, sub) ->
        (match sub with
         | None -> Ok None
         | Some s -> renamePatGuards renInner s |> Result.map Some)
        |> Result.map (fun sub' -> { p with Kind = PatternKind.PatVariant (vnm, sub') })
    | PatternKind.PatGuarded (sub, g) ->
        renamePatGuards renInner sub |> Result.bind (fun sub' ->
        renameExpr renInner g |> Result.map (fun g' -> { p with Kind = PatternKind.PatGuarded (sub', g') }))
    | PatternKind.PatTyped (sub, t) ->
        renamePatGuards renInner sub |> Result.map (fun sub' -> { p with Kind = PatternKind.PatTyped (sub', t) })

let rec internal renameNStmts (ren: Map<string, string>) (stmts: NStmt list) : Result<NStmt list, string> =
    stmts
    |> traverseR (fun s ->
        match s with
        | NLet (n, m, e) ->
            let n' = Map.tryFind n ren |> Option.defaultValue n
            renameExpr ren e |> Result.map (fun e' -> NLet (n', m, e'))
        | NAssign (l, r) ->
            renameExpr ren l |> Result.bind (fun l' ->
            renameExpr ren r |> Result.map (fun r' -> NAssign (l', r')))
        | NFor (var, lo, hi, body) ->
            let var' = Map.tryFind var ren |> Option.defaultValue var
            renameExpr ren lo |> Result.bind (fun lo' ->
            renameExpr ren hi |> Result.bind (fun hi' ->
            renameNStmts ren body |> Result.map (fun b' -> NFor (var', lo', hi', b')))))

/// All names BOUND anywhere in a statement list (lets + loop vars).
let rec internal boundNames (stmts: NStmt list) : string list =
    stmts |> List.collect (fun s ->
        match s with
        | NLet (n, _, _) -> [n]
        | NAssign _ -> []
        | NFor (var, _, _, body) -> var :: boundNames body)

// ANF + inlining

/// Hoist user-function calls out of nested expression positions into
/// preceding lets, so calls only appear as direct `let x = f(args)` values.
/// Intrinsics and array reads stay in place.
let rec internal hoistCalls (fname: string) (ctx: Ctx) (e: Expr) : Result<NStmt list * Expr, string> =
    let recurse = hoistCalls fname ctx
    let re k = inheritSpan e k
    // Hoist a whole operand list: each operand's hoists in operand order,
    // then the rewritten operands.
    let hoistAll (xs: Expr list) : Result<NStmt list * Expr list, string> =
        traverseR recurse xs
        |> Result.map (fun pairs -> (pairs |> List.collect fst, pairs |> List.map snd))
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, args) when Map.containsKey name ctx.Decls ->
        // hoist arguments first (post-order), then this call
        hoistAll args |> Result.map (fun (stmts, args') ->
            // `__hc` = hoisted call. (Deliberately NOT `__t`: the `__t_` prefix
            // is reserved for forward-mode tangent names, one underscore away.)
            let tmp = fresh ctx "__hc"
            (stmts @ [NLet (tmp, false, re (ExprApp (re (ExprVar name), args')))], re (ExprVar tmp)))
    | ExprKind.ExprApp (f, args) ->
        hoistAll args |> Result.map (fun (stmts, args') -> (stmts, re (ExprApp (f, args'))))
    | ExprKind.ExprBinOp (m, op, l, r) ->
        recurse l |> Result.bind (fun (sl, l') ->
        recurse r |> Result.map (fun (sr, r') -> (sl @ sr, re (ExprBinOp (m, op, l', r')))))
    | ExprKind.ExprUnaryOp (op, inner) ->
        recurse inner |> Result.map (fun (s, i') -> (s, re (ExprUnaryOp (op, i'))))
    | ExprKind.ExprTyped (inner, t) ->
        recurse inner |> Result.map (fun (s, i') -> (s, re (ExprTyped (i', t))))
    | ExprKind.ExprArrayLit elems ->
        hoistAll elems |> Result.map (fun (stmts, es) -> (stmts, re (ExprArrayLit es)))
    | _ -> Ok ([], e)

// Pre-normalization of the imperative-free surface constructs. Recursive
// arrays and `reduce` reach grad as ExprRecArray / ExprReduce nodes the
// NFor pipeline never learned to read. This pass is a SOURCE-TO-SOURCE
// rewrite over each differentiated function body that lowers those forms
// into the ACCUMULATION and CONSTRUCTION statement shapes grad already
// differentiates -- additive-only, keeping the BL5500 discipline. It runs
// BEFORE convertStmts, so downstream sees only familiar for-in loops,
// element writes, and additive `+=`.

/// Float-ness of a type annotation (mirrors isFloatTy, needed earlier).
let internal preIsFloatTy (t: TypeExpr) : bool =
    match t with
    | TyFloat64 | TyFloat32 -> true
    | TyNamed (("Float" | "Float64" | "Float32"), []) -> true
    | _ -> false

/// Read the static per-axis extents of an `Array<Float like Idx<n>, ...>`
/// annotation. Returns (elem-is-Float, extents) when every axis is a literal
/// `Idx<n>`; None otherwise (non-literal extents / non-array type). v1 admits
/// literal extents only -- named/derived index extents are rejected upstream.
let internal arrayLiteralExtents (t: TypeExpr) : (bool * int list) option =
    match t with
    | TyArray (elem, idxs) ->
        let exts =
            idxs |> List.map (fun ix ->
                match ix with
                | TyIdx { Kind = ExprKind.ExprLit (LitInt n) } -> Some (int n)
                | _ -> None)
        if not idxs.IsEmpty && exts |> List.forall Option.isSome then
            Some (preIsFloatTy elem, exts |> List.map Option.get)
        else None
    | _ -> None

/// Does `e` reference the variable `name` anywhere?
///
/// `mentionsDeep`, specialized. It used to be its own walker over "the
/// arithmetic + array-read fragment recursive-array slices live in", with a
/// catch-all answering FALSE -- and the fragment was missing `ExprIf` and
/// `ExprTuple`. That made `expandRecArray`'s `hasPrefix` blind to a prefix read
/// under a branch: the slice was classified prefix-FREE, which produced a
/// wrong-shaped refusal in the ordinary case and, when the prefix pattern
/// variable shadowed an array in scope, emitted reads of that OTHER array --
/// a silently wrong value. A missed reference here is never safe, so the
/// answer comes from the exhaustive walker instead of a fragment.
let internal mentionsVar (name: string) (e: Expr) : bool =
    mentionsDeep (Set.singleton name) e

/// Expand a rank-1 recursive-array `let` into the buffer + element-write /
/// accumulation statements the NFor differentiation pipeline handles.
/// Returns the emitted statements and the buffer's leading extent (for the
/// ambient reduce-source extent env). The bound NAME is reused as the buffer
/// so downstream reads of it keep resolving.
///
///   * a recurrence reading its immediate predecessor `prefix(n-1)` (with a
///     `zero :: n` seed arm) -> the DIRECT loop `s(0) = seed; for n in 1..N
///     { s(n) = SLICE[prefix := s] }`, in BOTH modes. Forward mode
///     differentiates it in place (the tangent recurrence mirrors it).
///     Reverse mode admits any smooth first-order slice `g(prefix(n-1), ..)`:
///     its adjoint is the same loop run BACKWARDS -- the `CarryLoop`
///     recognizer (GradNormalize) exempts it from the loop discipline and
///     the `NFor` adjoint arm (GradSweeps) emits the descending sweep, O(n)
///     work (docs/plans/structural/01, milestones A and B). The trajectory
///     buffer is the tape: at descending step t the general-overwrite rule
///     evaluates `dg/ds` at the primal `s(t-1)`, which is final. The
///     triangular scatter-add this used to unroll into was O(n^2) in both
///     the replayed primal and the adjoint, and admitted the additive shape
///     only.
///   * prefix-free construction `prefix :: f(n)` -> direct element writes.
///   * anything else (deeper lags, rank >= 2, no seed) is rejected.
let internal expandRecArray (fname: string) (ctx: Ctx)
                           (name: string) (annot: TypeExpr) (def: RecArrayDef)
    : Result<Stmt list * int, string> =
    // A `while` guard makes the stopping ordinal DATA-DEPENDENT: the adjoint
    // loop's trip count would depend on primal values, which the fixed-extent
    // sweep below cannot express. Refused in v1. (v2: the guard declares a
    // fixed point -- exactly the structure an implicit-function-theorem
    // adjoint wants; see plan-fortran-killer.md arc 6.)
    if def.Guard.IsSome then
        err fname $"recursive array '{name}': a `while`-guarded recursive array is not differentiable (v1) -- the stopping ordinal is data-dependent"
    else
    match arrayLiteralExtents (resolveArrayTy ctx annot) with
    | None ->
        err fname $"recursive array '{name}': a differentiable recursive array needs an `Array<Float like Idx<n>>` annotation with a literal extent (v1)"
    | Some (false, _) ->
        err fname $"recursive array '{name}': only Float-element recursive arrays are differentiable (v1)"
    | Some (true, [n]) ->
        let bufName = name
        let bufVar = syn (ExprVar bufName)
        let sAt (idxE: Expr) = syn (ExprApp (bufVar, [idxE]))
        let zeros = syn (ExprArrayLit (List.replicate n (fLit 0.0)))
        let bufLet = StmtLet { Mutability = BindMut; Pattern = synPat (PatVar bufName); Type = None; Value = zeros }
        let stepVar = def.StepVar
        let prefixVar = def.PrefixVar
        // `name := repl` inside a slice or a seed arm -- binding a
        // recurrence's step ordinal or its prefix name to the buffer.
        //
        // `substKern` is the substitution. Its catch-all DECLINES instead of
        // silently returning an un-descended node, which is the whole reason
        // the fragment walker this replaced is gone: that one had arms for
        // arithmetic and array reads only, so a step ordinal used inside a
        // `reduce`, an `extents`, or a tuple-index was left dangling in the
        // emitted loop. A decline still leaves the expression exactly as
        // found -- the forms `substKern` refuses to cross are binders, and a
        // slice or seed containing one is refused downstream by the
        // AD-able-subset walk regardless.
        let subst (nm: string) (repl: Expr) (x: Expr) : Expr =
            substKern nm repl x |> Option.defaultValue x
        // `x` is exactly the immediate-predecessor read `prefix(stepVar - 1)`?
        let isPrevPrefixRead (x: Expr) =
            match x.Kind with
            | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar p }, [idx]) when p = prefixVar ->
                (match idx.Kind with
                 | ExprKind.ExprBinOp (_, OpSub, { Kind = ExprKind.ExprVar s }, { Kind = ExprKind.ExprLit (LitInt 1L) }) -> s = stepVar
                 | _ -> false)
            | _ -> false
        let hasPrefix (e: Expr) = mentionsVar prefixVar e
        if hasPrefix def.SliceExpr then
            // A genuine recurrence lowers to the DIRECT element-write loop in
            // both modes. Forward mode differentiates it in place (the
            // tangent recurrence mirrors it); reverse mode runs the same loop
            // backwards (the CarryLoop arm in GradSweeps), the trajectory
            // buffer serving as the tape. Both admit any smooth slice of the
            // immediate predecessor. Prefix reads become reads of the buffer
            // being built; v1 admits the immediate predecessor only -- deeper
            // lags rely on the implicit-zero reads a plain loop cannot supply.
            let rec onlyPrevReads (x: Expr) : bool =
                match x.Kind with
                | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar p }, [idx]) when p = prefixVar ->
                    (match idx.Kind with
                     | ExprKind.ExprBinOp (_, OpSub, { Kind = ExprKind.ExprVar s }, { Kind = ExprKind.ExprLit (LitInt 1L) }) -> s = stepVar
                     | _ -> false)
                | ExprKind.ExprVar p -> p <> prefixVar
                | ExprKind.ExprLit _ -> true
                | ExprKind.ExprTyped (inner, _) | ExprKind.ExprUnaryOp (_, inner) -> onlyPrevReads inner
                | ExprKind.ExprBinOp (_, _, l, r2) | ExprKind.ExprDotDot (l, r2) -> onlyPrevReads l && onlyPrevReads r2
                | ExprKind.ExprApp (fh, args) -> onlyPrevReads fh && List.forall onlyPrevReads args
                | ExprKind.ExprArrayLit es -> List.forall onlyPrevReads es
                | ExprKind.ExprIf (c, t, f2) -> onlyPrevReads c && onlyPrevReads t && onlyPrevReads f2
                | _ -> false
            match def.SeedArm with
            | None -> err fname $"recursive array '{name}': a recurrence needs a `zero :: n` seed arm to be differentiable (v1)"
            | Some (seedStep, seedExpr) ->
                if not (onlyPrevReads def.SliceExpr) then
                    err fname $"recursive array '{name}': a differentiable recurrence may read the immediate predecessor `prefix(n - 1)` only (deeper lags rely on implicit-zero reads a direct loop cannot supply, v1)"
                else
                    let sliceB = subst prefixVar bufVar def.SliceExpr
                    let seedWrite = [ StmtExpr (syn (ExprAssign (sAt (iLit 0L), subst seedStep (iLit 0L) seedExpr))) ]
                    let loop =
                        StmtForIn (stepVar,
                                   syn (ExprDotDot (iLit 1L, iLit (int64 n))),
                                   [ StmtExpr (syn (ExprAssign (sAt (v stepVar), sliceB))) ])
                    Ok (bufLet :: (seedWrite @ [loop]), n)
        else
            // Pure construction (no carried state): direct element writes.
            let loopStart, seedStmts =
                match def.SeedArm with
                | Some (seedStep, seedExpr) ->
                    1L, [ StmtExpr (syn (ExprAssign (sAt (iLit 0L), subst seedStep (iLit 0L) seedExpr))) ]
                | None -> 0L, []
            let loop =
                StmtForIn (stepVar,
                           syn (ExprDotDot (iLit loopStart, iLit (int64 n))),
                           [ StmtExpr (syn (ExprAssign (sAt (v stepVar), def.SliceExpr))) ])
            Ok (bufLet :: (seedStmts @ [loop]), n)
    | Some (true, exts) ->
        err fname $"recursive array '{name}': only rank-1 (scalar-slice) recursive arrays are differentiable (v1); a rank-{exts.Length} recursive array is not supported"

/// Rewrite additive `reduce(SRC, (+)[, init])` occurrences inside `e` into an
/// accumulator loop (returned as a prefix of statements) plus a reference to
/// the accumulator. Reject non-additive kernels and non-array-literal /
/// non-variable sources (deferred/former reductions), the additive subset
/// matching grad v1. `extents` maps in-scope array names to their extents.
let rec internal hoistReduces (fname: string) (ctx: Ctx) (extents: Map<string, int>) (e: Expr)
    : Result<Stmt list * Expr, string> =
    let re k = inheritSpan e k
    let recurse = hoistReduces fname ctx extents
    match e.Kind with
    | ExprKind.ExprReduce (src, kernel, initOpt, axesOpt) ->
        // A PARTIAL fold (`axes = n` with n < rank) produces an ARRAY, not a
        // scalar, so the accumulator-loop rewrite below does not model it.
        // Grad v1 differentiates the rank-1 fold only; an explicit axis count
        // is refused rather than silently rewritten as if it were one.
        (match axesOpt with
         | Some _ -> err fname "reduce with an explicit `axes = n` is not differentiable (v1): grad supports the rank-1 additive fold `reduce(A, (+)[, init])`"
         | None -> Ok ())
        |> Result.bind (fun () ->
        (match kernel.Kind with
         | ExprKind.ExprSection OpAdd -> Ok OpAdd
         // Forward mode folds the product section too: the tangent is a
         // paired fold updated in lockstep before the primal step -- the
         // product rule is local, no tape. Reverse keeps additive-only.
         | ExprKind.ExprSection OpMul when errMode.Value = "jvp" -> Ok OpMul
         | ExprKind.ExprSection _ ->
             err fname (if errMode.Value = "jvp"
                        then "reduce in differentiated code supports the `(+)` and `(*)` section kernels"
                        else "reduce in differentiated code supports only the additive kernel `(+)` (v1)")
         | _ -> err fname "reduce in differentiated code supports only section kernels; lambda kernels are not differentiable (v1)")
        |> Result.bind (fun secOp ->
        recurse src |> Result.bind (fun (srcPre, src') ->
        let initE =
            match initOpt with
            | Some ie -> ie
            | None -> if secOp = OpMul then fLit 1.0 else fLit 0.0
        let combine acc el = syn (ExprBinOp (Elementwise, secOp, acc, el))
        let accName = fresh ctx "__red"
        let accLet = StmtLet { Mutability = BindMut; Pattern = synPat (PatVar accName); Type = None; Value = initE }
        match src'.Kind with
        | ExprKind.ExprArrayLit elems ->
            // Inline literal: UNROLL to per-element scalar accumulations. A
            // read-loop over a bound literal would lose the cotangent --
            // grad treats array-literal lets as constant inits (nothing
            // flows back to the elements) -- so keep each active element in
            // scalar accumulation position, which grad differentiates exactly.
            let adds =
                elems |> List.map (fun el ->
                    StmtExpr (syn (ExprAssign (v accName, combine (v accName) el))))
            Ok (srcPre @ [accLet] @ adds, v accName)
        | ExprKind.ExprVar nm ->
            (match Map.tryFind nm extents with
             | Some cnt ->
                 let kVar = fresh ctx "__rik"
                 let readK = syn (ExprApp (syn (ExprVar nm), [v kVar]))
                 let loop =
                     StmtForIn (kVar,
                                syn (ExprDotDot (iLit 0L, iLit (int64 cnt))),
                                [ StmtExpr (syn (ExprAssign (v accName, combine (v accName) readK))) ])
                 Ok (srcPre @ [accLet; loop], v accName)
             | None -> err fname $"reduce source '{nm}' has no statically-known extent in differentiated code; reduce over a param/let array with an `Idx<n>` extent or over an inline array literal (v1)")
        | _ -> err fname "reduce in differentiated code requires an array-variable or inline-array-literal source; deferred/former reductions are not differentiable (v1)")))
    | ExprKind.ExprBinOp (m, op, l, r) ->
        recurse l |> Result.bind (fun (pl, l') ->
        recurse r |> Result.map (fun (pr, r') -> (pl @ pr, re (ExprBinOp (m, op, l', r')))))
    | ExprKind.ExprUnaryOp (op, inner) ->
        recurse inner |> Result.map (fun (p, i') -> (p, re (ExprUnaryOp (op, i'))))
    | ExprKind.ExprTyped (inner, t) ->
        recurse inner |> Result.map (fun (p, i') -> (p, re (ExprTyped (i', t))))
    | ExprKind.ExprAssign (l, r) ->
        recurse r |> Result.map (fun (pr, r') -> (pr, re (ExprAssign (l, r'))))
    | ExprKind.ExprApp (f, args) ->
        args |> List.fold (fun acc a ->
            acc |> Result.bind (fun (ps, args') ->
                recurse a |> Result.map (fun (p, a') -> (ps @ p, args' @ [a']))))
            (Ok ([], []))
        |> Result.map (fun (ps, args') -> (ps, re (ExprApp (f, args'))))
    | ExprKind.ExprArrayLit elems ->
        elems |> List.fold (fun acc a ->
            acc |> Result.bind (fun (ps, es) ->
                recurse a |> Result.map (fun (p, a') -> (ps @ p, es @ [a']))))
            (Ok ([], []))
        |> Result.map (fun (ps, es) -> (ps, re (ExprArrayLit es)))
    | _ -> Ok ([], e)

/// Leading-axis extent of an initializer, when it is statically knowable
/// from the expression's own STRUCTURE rather than from an annotation.
/// Feeds the reduce-lowering's extent env, so `reduce` over a local built
/// by one of these forms recovers its loop bound (previously only inline
/// array literals did, which is why a `replicate`d or `join`ed local could
/// not be reduced in differentiated code).
let rec internal staticExtentOf (ctx: Ctx) (env: Map<string, int>) (e: Expr) : int option =
    match e with
    // a LET-BOUND loop applied by name: the binding's extent was recorded
    // under the loop's NAME, which is more direct than resolving the loop
    // object, so it is tried before the decomposition below.
    | { Kind = ExprKind.ExprBinOp (_, OpApply, { Kind = ExprKind.ExprVar n }, _) } when Map.containsKey n env ->
        Map.tryFind n env
    // A map's leading extent is its FIRST operand's (C2): the loop iterates
    // the operand index spaces in order, so a reduce over the result knows
    // its bound whenever the operand does. Both spellings, one decomposition.
    | MapApply m -> m.Ops |> List.tryHead |> Option.bind (staticExtentOf ctx env)
    | _ ->
    match e.Kind with
    | ExprKind.ExprArrayLit elems -> Some elems.Length
    | ExprKind.ExprVar n -> Map.tryFind n env
    | ExprKind.ExprTyped (inner, _) | ExprKind.ExprCompute inner
    | ExprKind.ExprGuard (_, inner) -> staticExtentOf ctx env inner
    // replicate(n, _) and its `pure`-filled sibling (grad's ConstFill)
    | ExprKind.ExprReplicate ({ Kind = ExprKind.ExprLit (LitInt n) }, _) -> Some (int n)
    | ExprKind.ExprSequence es | ExprKind.ExprStack es -> Some es.Length
    // join concatenates along the leading axis only when d = 0
    | ExprKind.ExprJoin (parts, 0) ->
        parts |> List.fold (fun acc p ->
            acc |> Option.bind (fun tot -> staticExtentOf ctx env p |> Option.map (fun n -> tot + n)))
            (Some 0)
    // A halo traversal's extent is the SHRUNK interior, N - Shrink, read
    // off the same access record the checker and the runtime guards use
    // (docs/plans/structural/02, 2.1). This arm used to compute
    // `max - min` over the DECLARED offsets, which is only the shrink when
    // 0 is among them: for a one-sided lag set like `[-2, -4]` (loops/078)
    // it said 6 of 8 where the interior is 4 of 8 -- the centre is always
    // in the reach -- and both AD lanes' reduce loops over the map ran two
    // cells past its buffer (ASan-caught by tests/AccessTests.fs).
    | ExprKind.ExprHalo (inner, offs) ->
        haloAccessOfSurface ctx inner offs |> Option.map (fun (h, n) -> n - int h.Shrink)
    // a loop side the decomposition above could not read (an unrecorded name,
    // or the `>>@`-composed object of a DECLINED fusion): the right side is
    // then the operand list, and its leading operand is the extent
    | ExprKind.ExprBinOp (_, OpApply, { Kind = ExprKind.ExprVar _ | ExprKind.ExprBinOp (_, OpComposeObj, _, _) }, args) ->
        (match args.Kind with
         | ExprKind.ExprTuple (first :: _) -> staticExtentOf ctx env first
         | _ -> staticExtentOf ctx env args)
    // a bare loop binding's extent is its leading operand's
    | ExprKind.ExprMethodFor (first :: _) -> staticExtentOf ctx env first
    // C7: sorting permutes a rank-1 array, so the extent is the operand's
    | ExprKind.ExprSort (a, _) -> staticExtentOf ctx env a
    // Pipelines (C7) are FUSED away before anything reaches here, so these
    // arms only ever fire for a pipeline fusion DECLINED. They exist as
    // defense in depth: without them a declined pipeline dies with
    // "reduce source has no statically-known extent", which points at the
    // wrong thing entirely. A pipeline never changes the index space it
    // iterates, so the extent is the surviving stage's.
    | ExprKind.ExprBinOp (_, OpComposeMeth, c1, _) -> staticExtentOf ctx env c1
    | ExprKind.ExprBinOp (_, OpFunctor, _, c) -> staticExtentOf ctx env c
    | _ -> None

/// Full static DIMS of an initializer (C6): every axis extent, structurally.
/// The reverse sweep needs whole shapes -- a cotangent buffer for a
/// combinator-built local, and bounds for the reindexing accumulation
/// loops -- where the forward-only paths need just the leading extent.
///
/// This is NOT `staticExtentOf` with more axes, and the difference is not
/// laziness. Every caller here sizes a BUFFER or bounds an accumulation loop,
/// so a partial answer is worse than none: there is no sound "leading
/// dimension only" fallback onto `staticExtentOf` for the forms it knows and
/// this does not (halo interiors, map applications, declined pipelines).
/// Those forms stay silent on purpose, and the arms that do exist here are
/// exactly the ones `adjointOfInit` has a reverse flow for -- adding a shape
/// to this table without adding its flow only moves the refusal, it does not
/// remove one.
/// Rewrite the window reads of a halo kernel body: `w(k)` becomes
/// `idx + (Start + k)` -- the interior ordinal `idx` plus the static offset
/// into the inner space -- for a literal k (refused when outside the
/// declared reach, the checker's BL4019 rule), and `idx + (Start + e)` for a
/// computed offset e (never refused here: the declared reach is a runtime
/// property of e, as the checker also holds). The window itself may not be
/// passed on, and the walk covers the forms a stencil kernel is made of:
/// literals, names, arithmetic, calls, `if`, tuples, annotations. Anything
/// else that mentions the window is declined -- never silently kept.
let internal substWindowReads (fname: string) (wname: string) (h: Blade.Types.HaloAccess) (idx: Expr) (body: Expr) : Result<Expr, string> =
    let rec go (e: Expr) : Result<Expr, string> =
        let re (k: ExprKind) = inheritSpan e k
        match e.Kind with
        | ExprKind.ExprLit _ -> Ok e
        | ExprKind.ExprVar n when n = wname ->
            err fname $"the halo window '{wname}' may only be read through, as `{wname}(o)`; passing the window itself on is not differentiable in reverse mode (v1)"
        | ExprKind.ExprVar _ -> Ok e
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar n }, [ off ]) when n = wname ->
            (match literalOffsetOf off with
             | Some k when not (Blade.Types.haloOffsetInReach h k) ->
                 let reach = h.Offsets |> List.map string |> String.concat ", "
                 err fname $"the window read `{wname}({k})` is outside the declared halo reach [{reach}]"
             | Some k -> Ok (add idx (iLit (h.Start + int64 k)))
             | None -> go off |> Result.map (fun off' -> add idx (add (iLit h.Start) off')))
        | ExprKind.ExprApp (f, args) ->
            go f |> Result.bind (fun f' ->
            traverseR go args |> Result.map (fun args' -> re (ExprApp (f', args'))))
        | ExprKind.ExprBinOp (m, op, l, r) ->
            go l |> Result.bind (fun l' -> go r |> Result.map (fun r' -> re (ExprBinOp (m, op, l', r'))))
        | ExprKind.ExprUnaryOp (op, x) -> go x |> Result.map (fun x' -> re (ExprUnaryOp (op, x')))
        | ExprKind.ExprTyped (x, t) -> go x |> Result.map (fun x' -> re (ExprTyped (x', t)))
        | ExprKind.ExprIf (c, t, f) ->
            go c |> Result.bind (fun c' -> go t |> Result.bind (fun t' -> go f |> Result.map (fun f' -> re (ExprIf (c', t', f')))))
        | ExprKind.ExprTuple es -> traverseR go es |> Result.map (fun es' -> re (ExprTuple es'))
        | _ ->
            if mentionsVar wname e then
                err fname $"this kernel form reads the halo window '{wname}' in a way reverse mode does not support (v1): the window may be read as `{wname}(o)` inside arithmetic, intrinsic calls, array reads, `if` and tuples"
            else Ok e
    go body

let rec internal staticDimsOf (ctx: Ctx) (denv: Map<string, int list>) (e: Expr) : int list option =
    match e with
    | ConstFill ({ Kind = ExprKind.ExprLit (LitInt n) }, _) -> Some [int n]
    // A sole-halo stencil map kept as a local by the reverse gather route:
    // its shape is the shrunk interior N - Shrink, from the surface
    // literals. Its FLOW is GradSweeps.adjointOfInit's gather arm (the
    // shape is only offered where that flow exists: reverse mode).
    | _ when errMode.Value = "grad" && (soleHaloOf e).IsSome ->
        (match soleHaloOf e with
         | Some (inner, offs, _) ->
             haloAccessOfSurface ctx inner offs |> Option.map (fun (h, n) -> [ n - int h.Shrink ])
         | None -> None)
    | _ ->
    match e.Kind with
    | ExprKind.ExprVar n -> Map.tryFind n denv
    | ExprKind.ExprTyped (inner, _) | ExprKind.ExprCompute inner
    | ExprKind.ExprPure inner | ExprKind.ExprGuard (_, inner) -> staticDimsOf ctx denv inner
    | ExprKind.ExprArrayLit (first :: rest) ->
        (match staticDimsOf ctx denv first with
         | Some ds -> Some ((rest.Length + 1) :: ds)
         | None -> Some [rest.Length + 1])   // scalar elements
    | ExprKind.ExprTranspose (a, d1, d2) ->
        staticDimsOf ctx denv a |> Option.map (fun ds ->
            if d1 < ds.Length && d2 < ds.Length then
                ds |> List.mapi (fun i d -> if i = d1 then ds.[d2] elif i = d2 then ds.[d1] else d)
            else ds)
    // `sequence` and `stack` are the same shape -- a leading axis of N over
    // the member shape -- and lower identically; only the spelling differs.
    // (Both refuse SCALAR members, where the member has no dims of its own:
    // that stays a refusal because no reverse flow peels a scalar member.)
    | ExprKind.ExprStack es | ExprKind.ExprSequence es ->
        (match es with
         | first :: _ -> staticDimsOf ctx denv first |> Option.map (fun ds -> es.Length :: ds)
         | [] -> None)
    // `replicate(n, body)` is `sequence(body, ..., body)`: the same leading
    // axis over the body's own shape. The literal-fill case never reaches
    // here -- `ConstFill` above answers it, and answers it as rank 1.
    | ExprKind.ExprReplicate ({ Kind = ExprKind.ExprLit (LitInt n) }, body) ->
        staticDimsOf ctx denv body |> Option.map (fun ds -> int n :: ds)
    | ExprKind.ExprJoin (parts, 0) ->
        (match parts |> List.map (staticDimsOf ctx denv) with
         | (Some (h :: rest)) :: tail when tail |> List.forall (function Some (_ :: r) -> r = rest | _ -> false) ->
             let total = h + (tail |> List.sumBy (function Some (h2 :: _) -> h2 | _ -> 0))
             Some (total :: rest)
         | _ -> None)
    | ExprKind.ExprGram (a, b) ->
        (match staticDimsOf ctx denv a, staticDimsOf ctx denv b with
         | Some (i :: _), Some (j :: _) -> Some [i; j]
         | _ -> None)
    // gram_apply(A, B, x): a vector over A's leading axis
    | ExprKind.ExprGramApply (a, _, _) ->
        (match staticDimsOf ctx denv a with
         | Some (i :: _) -> Some [i]
         | _ -> None)
    // C7: a sort is a permutation -- same shape as its (rank-1) operand
    | ExprKind.ExprSort (a, _) -> staticDimsOf ctx denv a
    // a solve against LU factors: the right-hand side's shape
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar n }, [ _; _; b ]) when isLuSolveName n -> staticDimsOf ctx denv b
    | _ -> None

/// Zero array literal for a dims list (rank-general).
let rec internal zerosOfDims (dims: int list) : Expr =
    match dims with
    | [] -> fLit 0.0
    | n :: rest -> syn (ExprArrayLit (List.replicate n (zerosOfDims rest)))

/// Nested accumulation loops over `dims`: lhs(idx...) += rhs(idx...).
let internal accumLoop (ctx: Ctx) (dims: int list)
                      (mkLhs: Expr list -> Expr) (mkRhs: Expr list -> Expr) : NStmt list =
    let idxNames = dims |> List.map (fun _ -> fresh ctx "__ai")
    let idx = idxNames |> List.map v
    let body = [ NAssign (mkLhs idx, add (mkLhs idx) (mkRhs idx)) ]
    List.foldBack2 (fun nm n inner -> [ NFor (nm, iLit 0L, iLit (int64 n), inner) ]) idxNames dims body

