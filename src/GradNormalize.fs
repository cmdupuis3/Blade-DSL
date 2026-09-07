// Body normalization for the sweeps: the pre-pass (pipelines, rec-arrays,
// reduces), bounded call inlining, parameter classification, and the AD-able-
// subset analysis/rejection checks (write-after-read, scalar overwrite, loop
// discipline).
module Blade.GradNormalize

open Blade.Ast
open Blade.GradCommon
open Blade.GradExpand
open Blade.GradFusion
open Blade.GradPackUnroll

// Units

/// The unit argument of a (resolved) Float type as a SURFACE unit expression:
/// `Float<meters>` (a lone name), `Float<meters/second>` (the parser's
/// TyUnitExpr), `Float<meters^2>` (the rank-marked type-variable spelling
/// the parser gives `name^INT`). None for a bare Float or a non-Float.
let internal unitArgOf (ctx: Ctx) (t: TypeExpr) : UnitExpr option =
    match resolveTy ctx t with
    | TyNamed (("Float" | "Float64" | "Float32"), [arg]) ->
        (match arg with
         | TyNamed (n, []) -> Some (UnitNamed n)
         | TyUnitExpr ue -> Some ue
         | TyVar (n, Some k) -> Some (UnitPow (UnitNamed n, k))
         | _ -> None)
    | _ -> None

/// The ELEMENT unit of a scalar or array type: `Float<u>` and
/// `Array<Float<u> like ...>` both answer `u`.
let internal elemUnitOf (ctx: Ctx) (t: TypeExpr) : UnitExpr option =
    match resolveTy ctx t with
    | TyArray (elem, _) -> unitArgOf ctx elem
    | other -> unitArgOf ctx other


// The C2-reverse lowering: eager map pipelines, lowered pre-grad
//
// `plan-equivariant-nn-notebooks.md` 5.2, a scoped amendment to
// `plan-ad-combinators.md` 4's C6 verdict. C6's reasoning still bounds it:
// no general reverse-through-combinators pass is built here, and no new
// adjoint theory is introduced. An eager map is REWRITTEN, before the sweeps
// run, into the element-write construction loop grad's v1 subset already
// differentiates -- exactly what `expandRecArray` does for `let rec`, and
// what `fuseProgram` does for pipelines. The adjoint is then the existing
// loop replay (`adjointOfStmt`'s NFor arm), which is why this buys reverse
// mode a capability without buying it a rule.

/// One loop operand, decomposed: the axes it contributes to the map's
/// iteration space, and one substitution per kernel parameter it feeds
/// (a `zip` operand feeds one parameter per zipped array; every other form
/// feeds exactly one).
type private MapSlot = {
    Axes: int list
    Readers: (Expr list -> Expr) list
    /// A halo slot: the reader hands its parameter the INTERIOR ordinal, and
    /// the kernel body's window reads `w(k)` are rewritten against it rather
    /// than the parameter being substituted (GradExpand.substWindowReads).
    Window: Blade.Types.HaloAccess option
}

/// Statements that are pure loop-object / kernel PLUMBING: `let L =
/// method_for(...)`, `let O = object_for(k)`, `let P = L1 <*> L2`,
/// `let k = lambda(...)`. The eager-map lowering resolves these by name at
/// the application site, which leaves the binding itself dead -- and a dead
/// one still trips the combinator refusal the rewrite exists to avoid.
let private loopPlumbingName (s: Stmt) : string option =
    match unwrapStmt s with
    | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }
                Value = { Kind = ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _
                               | ExprKind.ExprLambda _
                               | ExprKind.ExprBinOp (_, OpArrayProd, _, _) } } -> Some nm
    | _ -> None

/// Every variable a surface statement mentions (binders ignored -- this is
/// liveness for a whole-name drop, where over-reporting only KEEPS a binding).
let rec private stmtVarsOf (s: Stmt) : Set<string> =
    match unwrapStmt s with
    | StmtLet b -> allVarsDeep b.Value
    | StmtExpr e -> allVarsDeep e
    | StmtAssign (l, _, r) -> Set.union (allVarsDeep l) (allVarsDeep r)
    | StmtForIn (_, rg, body) ->
        body |> List.fold (fun acc s2 -> Set.union acc (stmtVarsOf s2)) (allVarsDeep rg)
    | StmtSpanned _ -> Set.empty

/// Drop the loop-object and kernel bindings the eager-map lowering consumed.
/// A binding still mentioned anywhere AFTER it is KEPT, so a shape the
/// lowering declined keeps refusing exactly as it did before.
///
/// REVERSE MODE ONLY: forward mode resolves let-bound loop objects at their
/// APPLICATION sites (`noteLoopBinding`), so the bindings are live there.
let private dropDeadLoopBindings (stmts: Stmt list) (fin: Expr option) : Stmt list =
    if errMode.Value <> "grad" then stmts else
    let seed = match fin with Some e -> allVarsDeep e | None -> Set.empty
    List.foldBack (fun s (acc, used) ->
        match loopPlumbingName s with
        | Some nm when not (Set.contains nm used) -> (acc, used)
        | _ -> (s :: acc, Set.union used (stmtVarsOf s)))
        stmts ([], seed)
    |> fst

/// The extent of an index type, when it is a literal `Idx<n>` after alias
/// resolution. Deliberately narrow: `SymIdx`/`AntisymIdx` hand a `range`
/// kernel PREFIX OFFSETS rather than canonical indices
/// (`plan-ad-combinators.md` 1a), so a dense loop over them would compute the
/// wrong cells silently -- they must fail this test and be refused by name.
let private literalIdxExtent (ctx: Ctx) (t: TypeExpr) : int option =
    match resolveTy ctx t with
    | TyIdx { Kind = ExprKind.ExprLit (LitInt n) } -> Some (int n)
    | _ -> None

/// Buffer-size backstop. The lowering materializes its output as a zero
/// LITERAL (the only shape `zerosLikeLiteral` can build a cotangent for), so
/// the emitted AST is proportional to the iteration space. Past this the
/// refusal names the size rather than letting the compiler grind.
let private maxLoweredCells = 65536

/// ROUTE G admissibility (docs/plans/structural/02, 3.3): a sole-halo
/// stencil map whose reverse rule is the GATHER. Direct spelling with a
/// literal `Idx<n>` inner and literal offsets; a plain rank-0 lambda kernel
/// with no `where`; and a READ DISCIPLINE -- the window is read only as the
/// sole subscript of an array read `x(w(k))` with k literal, never passed to
/// a user function -- so every cotangent target the kernel's adjoint
/// produces is `__g_x(i + K)` with K static, and the gather's inverse is
/// exact. Anything else falls to the construction-loop lowering (route S),
/// which scatters and needs none of this.
let internal haloGatherPlan (ctx: Ctx) (resolve: string -> Expr option) (value: Expr)
    : (Blade.Types.HaloAccess * int * string * Expr) option =
    match soleHaloOfWith resolve value with
    | Some (inner, offs, kern) when (match kern.Kind with ExprKind.ExprLambda _ -> true | _ -> false) ->
        (match haloAccessOfSurface ctx inner offs, asKernelLambda ctx kern with
         | Some (h, n), Ok ([ p ], None, kbody, None)
                 when n - int h.Shrink >= 0
                      && (match p.Type |> Option.map (resolveTy ctx) with
                          | Some (TyVar (_, Some r)) -> r = 0
                          | Some (TyAbstractArray _) | Some (TyArray _) -> false
                          | _ -> true) ->
             let w = p.Name
             let isCombinatorOp op =
                 match op with
                 | OpApply | OpBind | OpParallel | OpFusion | OpArrayProd | OpFunctor | OpChoice
                 | OpComposeObj | OpComposeMeth | OpCompose | OpCons -> true
                 | _ -> false
             let rec ok (e: Expr) : bool =
                 match e.Kind with
                 | ExprKind.ExprLit _ -> true
                 | ExprKind.ExprVar n -> n <> w
                 // the one admitted window read: `x(w(k))`, k literal
                 | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f },
                                     [ { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar w' }, [ off ]) } ])
                         when w' = w && f <> w && not (Map.containsKey f ctx.Decls) ->
                     (literalOffsetOf off).IsSome
                 | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, args) when f <> w && not (Map.containsKey f ctx.Decls) ->
                     args |> List.forall ok
                 | ExprKind.ExprApp _ -> false
                 | ExprKind.ExprBinOp (_, op, l, r) -> not (isCombinatorOp op) && ok l && ok r
                 | ExprKind.ExprUnaryOp (_, x) | ExprKind.ExprTyped (x, _) -> ok x
                 | ExprKind.ExprIf (c, t, f) -> ok c && ok t && ok f
                 | _ -> false
             if ok kbody then Some (h, n, w, kbody) else None
         | _ -> None)
    | _ -> None

/// Lower an EAGER map pipeline into the element-write construction loop the
/// reverse sweep already differentiates:
///
///     let m = method_for(range<Idx<n>>) <@> lambda(i) -> BODY |> compute
///  becomes
///     let mut m = <zeros of the iteration shape>
///     for __mi0 in 0 .. n { m(__mi0) = BODY[i := __mi0] }
///
/// Both spellings (`method_for(ops) <@> k`, `object_for(k) <@> ops`) and both
/// indirections (a let-bound loop object, a let-bound kernel lambda) reach
/// the same decomposition, via `MapApplyWith` and `asKernelLambda`.
///
/// Operands: `range<Idx<n>>` (the parameter IS the loop index), a named array
/// with statically-known dims (the parameter is its cell), and `zip(...)` of
/// such arrays (co-iteration -- one shared axis group, one parameter each).
/// Several operands are the outer product, so their axes CONCATENATE.
///
/// REVERSE MODE ONLY. Forward mode has a Tier-2 map rule (`tangentOfMap`)
/// that keeps the loop object, its `range<SymIdx>` symmetric fast path, and
/// its parallelism licences; densifying a map for jvp would be a regression,
/// not a widening.
///
/// Returns `Ok None` when the initializer is not an eager map -- the caller
/// is then byte-identical to before. An `Error` is a map this cannot lower,
/// named: today EVERY map in reverse-differentiated code is refused by one
/// blanket message, so a specific refusal is a strict improvement.
let internal expandEagerMap (fname: string) (ctx: Ctx)
                           (extents: Map<string, int>) (denv: Map<string, int list>)
                           (loopEnv: Map<string, Expr>)
                           (name: string) (annot: TypeExpr option) (value: Expr)
    : Result<(Stmt list * int list) option, string> =
    if errMode.Value <> "grad" then Ok None else
    // `|> compute` is a materialization barrier with no value content; a
    // deferred binding read by index materializes anyway. Both strip.
    let rec strip (e: Expr) =
        match e.Kind with
        | ExprKind.ExprCompute inner -> strip inner
        | _ -> e
    // `<*>` concatenates two loops' operand lists and is symmetry-neutral --
    // "commutativity comes from the kernel later" (`formalism.md` 10.2), so
    // `method_for(A) <*> method_for(B)` IS `method_for(A, B)`. Flatten it
    // before the decomposition and the whole family below applies unchanged;
    // there is nothing else to say about `<*>` in either mode.
    let rec flattenProd (e: Expr) : Expr option =
        match e.Kind with
        | ExprKind.ExprMethodFor _ -> Some e
        | ExprKind.ExprVar n -> Map.tryFind n loopEnv |> Option.bind flattenProd
        | ExprKind.ExprBinOp (_, OpArrayProd, l, r) ->
            (match flattenProd l, flattenProd r with
             | Some { Kind = ExprKind.ExprMethodFor a }, Some { Kind = ExprKind.ExprMethodFor b } ->
                 Some (inheritSpan e (ExprMethodFor (a @ b)))
             | _ -> None)
        | _ -> None
    let core =
        match (strip value).Kind with
        | ExprKind.ExprBinOp (bm, OpApply, lo, rhs) when (match lo.Kind with
                                                          | ExprKind.ExprBinOp (_, OpArrayProd, _, _) -> true
                                                          | _ -> false) ->
            (match flattenProd lo with
             | Some flat -> inheritSpan (strip value) (ExprBinOp (bm, OpApply, flat, rhs))
             | None -> strip value)
        | _ -> strip value
    let resolveLoop (n: string) : Expr option =
        match Map.tryFind n loopEnv with
        | Some ({ Kind = ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _ } as b) -> Some b
        | Some ({ Kind = ExprKind.ExprBinOp (_, OpArrayProd, _, _) } as b) -> flattenProd b
        | _ -> None
    match core with
    // C7 sort plumbing (`let __sx_s = method_for(range<I>) <@> lambda(i) -> i
    // |> compute`) is an INDEX array admitted by both sweeps as constant
    // plumbing. Lowering it would replace an Int iota with a Float buffer and
    // strand `SortPermForm`, which recognizes the surface shape.
    | IndexIota -> Ok None
    // ROUTE G keeps the map: its reverse rule is the gather in
    // GradSweeps.adjointOfInit, and it replays verbatim in the forward half.
    | MapApplyWith resolveLoop _ when haloGatherEnabled () && (haloGatherPlan ctx resolveLoop core).IsSome -> Ok None
    | MapApplyWith resolveLoop mv ->
        let refuse (m: string) : Result<(Stmt list * int list) option, string> = err fname m
        let kernE =
            match mv.Kern.Kind with
            | ExprKind.ExprVar n ->
                (match Map.tryFind n loopEnv with
                 | Some ({ Kind = ExprKind.ExprLambda _ } as b) -> b
                 | _ -> mv.Kern)
            | _ -> mv.Kern
        // An arity-polymorphic kernel is refused BY NAME, ahead of
        // `asKernelLambda` -- which would report its `match arity(a)` body as
        // a generic "block body" and say nothing about the pack. Reverse mode
        // has no unroller: `tryUnrollPackKernel` (Route A) runs inside the
        // TANGENT map rule, which reverse mode does not reach.
        let packParams (ps: ParamDecl list) =
            ps |> List.exists (fun p ->
                match p.Type |> Option.map (resolveTy ctx) with
                | Some (TyPoly _) -> true
                | _ -> false)
        let isPackKernel =
            match kernE.Kind with
            | ExprKind.ExprVar f when Map.containsKey f ctx.Decls -> packParams ctx.Decls.[f].Params
            | ExprKind.ExprLambda (lps, _, _) ->
                lps |> List.exists (fun p ->
                    match p.Type |> Option.map (resolveTy ctx) with
                    | Some (TyPoly _) -> true
                    | _ -> false)
            | _ -> false
        if isPackKernel then
            refuse "reverse mode does not differentiate an arity-polymorphic `Poly<...>` pack kernel (v1): the surface unroller that expands one at its apply site (Route A) lives in the TANGENT map rule, so `ad.jvp` differentiates this kernel and `ad.grad` does not -- write the kernel at its fixed arity, or take the gradient through `ad.jvp` basis sweeps"
        else
        match asKernelLambda ctx kernE with
        | Error (KernBlockBody f) -> refuse (kernBlockBodyMsg f)
        | Error KernUnsupported -> refuse kernUnsupportedMsg
        | Ok (_, _, _, Some _) ->
            refuse "reverse mode differentiates a map with a plain lambda kernel (v1); a `reynolds(...)` kernel symmetrizes reads ACROSS loop slots, whose adjoint accumulation multiplicity this lowering does not model -- use `ad.jvp`, which has the Tier-2 reynolds rule"
        | Ok (_, Some _, _, None) ->
            refuse "reverse mode cannot lower a map whose kernel carries a `where` clause (v1): the lowering emits a DENSE construction loop, so a `comm`/`anticomm` license -- an iteration declaration, not a claim that the kernel IS symmetric -- would change which cells are computed, and an `omp`/`cuda` license has no loop object left to ride on. Drop the clause inside the differentiated function, or use `ad.jvp`, which keeps the loop object and its symmetric fast path"
        | Ok (ps, None, kbody, None) ->
        // A rank-raising kernel (an array-literal body) makes the result's
        // rank exceed the iteration space, which the zero buffer below cannot
        // size. Named rather than mis-shaped.
        match kbody.Kind with
        | ExprKind.ExprArrayLit _ ->
            refuse "reverse mode cannot lower a map whose kernel body is an array literal (a rank-raising row map, v1): the construction buffer is sized from the ITERATION space, which such a kernel exceeds"
        | _ ->
        if mv.Ops.IsEmpty then refuse "a differentiated map needs at least one loop operand" else
        // -- operand classification -------------------------------------------
        let arrayDims (n: string) =
            match Map.tryFind n denv with
            | Some ds when not ds.IsEmpty -> Some ds
            | _ -> None
        let classify (op: Expr) : Result<MapSlot, string> =
            match op.Kind with
            | ExprKind.ExprRange [t] ->
                (match literalIdxExtent ctx t with
                 | Some n -> Ok { Axes = [n]; Readers = [ fun ixs -> List.head ixs ]; Window = None }
                 | None ->
                     err fname "reverse mode lowers `range<I>` loops over a literal `Idx<n>` only (v1): a `SymIdx`/`AntisymIdx`/compound range hands the kernel PREFIX OFFSETS rather than canonical indices, so a dense construction loop would address the wrong cells")
            | ExprKind.ExprRange _ ->
                err fname "reverse mode lowers single-index `range<I>` loops only (v1); a multi-index `range<I, J>` is not supported -- spell it as `method_for(range<I>, range<J>)`"
            | ExprKind.ExprVar n ->
                (match arrayDims n with
                 | Some ds -> Ok { Axes = ds; Readers = [ fun ixs -> syn (ExprApp (v n, ixs)) ]; Window = None }
                 | None ->
                     err fname $"reverse mode needs each map operand to be a named array with statically-known extents (v1); '{n}' has none here -- annotate it `Array<Float like Idx<n>, ...>` or pass it as a parameter")
            | ExprKind.ExprZip zs ->
                let named =
                    zs |> List.map (fun z ->
                        match z.Kind with
                        | ExprKind.ExprVar n -> arrayDims n |> Option.map (fun ds -> (n, ds))
                        | _ -> None)
                if named |> List.exists Option.isNone then
                    err fname "reverse mode co-iterates `zip(...)` over named arrays with statically-known extents (v1); bind each zipped operand first"
                else
                    let named = named |> List.map Option.get
                    let ds0 = snd named.Head
                    if named |> List.exists (fun (_, ds) -> ds <> ds0) then
                        err fname "reverse mode co-iterates `zip(...)` over operands of IDENTICAL extents (v1); zip's shared min-rank prefix rule is not modelled by this lowering"
                    else
                        Ok { Axes = ds0
                             Readers = named |> List.map (fun (n, _) -> fun ixs -> syn (ExprApp (v n, ixs)))
                             Window = None }
            // ROUTE S (docs/plans/structural/02, 3.2): a halo slot iterates
            // the shrunk interior [0, N - Shrink); the kernel's window reads
            // become interior-ordinal-plus-offset reads of the operand, so
            // the construction loop's element write differentiates by the
            // ordinary rule and the adjoint SCATTERS -- no zero-Pad boundary
            // is needed, because a window that exists reads only inside.
            | ExprKind.ExprHalo (inner, offs) ->
                (match haloAccessOfSurface ctx inner offs with
                 | Some (h, n) when n - int h.Shrink >= 0 ->
                     Ok { Axes = [ n - int h.Shrink ]; Readers = [ fun ixs -> List.head ixs ]; Window = Some h }
                 | _ ->
                     err fname "reverse mode lowers a `halo<I, [offs]>` stencil map over a literal `Idx<n>` inner index with a literal offset set (v1); a compound inner or a computed offset set has no static interior here -- use `ad.jvp`, whose capture-read rule keeps the window")
            | ExprKind.ExprReverse _ ->
                err fname "reverse mode lowers `range<I>`, named-array and `zip(...)` map operands (v1); a `reverse<I>` traversal is not supported"
            | _ ->
                err fname "reverse mode needs each map operand to be `range<Idx<n>>`, a named array with statically-known extents, or `zip(...)` of such arrays (v1)"
        mv.Ops |> traverseR classify |> Result.bind (fun slots ->
        let expectedParams = slots |> List.sumBy (fun s -> s.Readers.Length)
        if ps.Length <> expectedParams then
            refuse $"kernel arity {ps.Length} does not match the {expectedParams} cell(s) its {mv.Ops.Length} loop operand(s) supply in differentiated code"
        else
        // A parameter bound to a rank-k FIBER is not a cell, and the reverse
        // rule for a fiber map is a partial fold, not a construction loop.
        let rankOfTy (t: TypeExpr option) =
            match t |> Option.map (resolveTy ctx) with
            | Some (TyVar (_, Some r)) -> r
            | Some (TyAbstractArray (_, { Kind = ExprKind.ExprLit (LitInt r) }, _)) -> int r
            | Some (TyArray (_, its)) -> its.Length
            | _ -> 0
        match ps |> List.tryFind (fun p -> rankOfTy p.Type > 0) with
        | Some p ->
            refuse $"kernel parameter '{p.Name}' is rank-carrying (`T^k` / `Array<...>`), so it is bound to a FIBER rather than a cell; reverse mode lowers rank-0 (scalar-cell) kernels (v1) -- the fiber adjoint is a contraction over the remaining axes, which is `ad.jvp` territory today"
        | None ->
        let dims = slots |> List.collect (fun s -> s.Axes)
        let cells = dims |> List.fold (*) 1
        if cells > maxLoweredCells then
            refuse $"this map's iteration space is {cells} cells; reverse mode materializes it as a zero-literal construction buffer, which is capped at {maxLoweredCells} (v1)"
        else
        // The annotation, if any, must agree with the iteration space -- the
        // buffer keeps it so downstream reads keep their index types.
        let annotOk =
            match annot with
            | None -> Ok None
            | Some t ->
                match arrayLiteralExtents (resolveArrayTy ctx t) with
                | Some (true, ds) when ds = dims -> Ok (Some t)
                | _ ->
                    err fname $"the annotation on '{name}' does not read as `Array<Float like Idx<n>, ...>` matching the map's iteration space {dims} (v1); drop it, or spell the extents literally"
        annotOk |> Result.bind (fun keptAnnot ->
        // -- substitution -------------------------------------------------------
        let idxNames = dims |> List.map (fun _ -> fresh ctx "__mi")
        let idxVars = idxNames |> List.map v
        // hand each operand the index variables for ITS OWN axes, in order
        let _, subs =
            slots |> List.fold (fun (rest: Expr list, acc) slot ->
                let mine, remaining = List.splitAt slot.Axes.Length rest
                (remaining, acc @ (slot.Readers |> List.map (fun r -> r mine))))
                (idxVars, [])
        // A halo slot's parameter is the window: its reads are rewritten in
        // the body against the interior ordinal; every other parameter is
        // substituted by its reader's expression.
        let windows = slots |> List.collect (fun s -> s.Readers |> List.map (fun _ -> s.Window))
        List.zip3 (ps |> List.map _.Name) subs windows
        |> List.fold (fun acc (pn, sub, win) ->
            acc |> Result.bind (fun (body, keep) ->
                match win with
                | Some h -> substWindowReads fname pn h sub body |> Result.map (fun b -> (b, keep))
                | None -> Ok (body, keep @ [ (pn, sub) ])))
            (Ok (kbody, []))
        |> Result.bind (fun (kbody', keep) ->
        substParamMany fname keep kbody'
        |> Result.bind (fun substituted ->
        // A `reduce` inside the kernel body is now in STATEMENT position, so
        // it lowers by the ordinary additive-fold rule -- into the innermost
        // loop, where its accumulator is loop-local and replays exactly.
        hoistReduces fname ctx extents substituted |> Result.map (fun (pre, body') ->
        let bufLet =
            StmtLet { Mutability = BindMut
                      Pattern = synPat (PatVar name)
                      Type = keptAnnot
                      Value = zerosOfDims dims }
        let write =
            StmtExpr (syn (ExprAssign (syn (ExprApp (v name, idxVars)), body')))
        let loops =
            List.foldBack2 (fun nm n inner ->
                [ StmtForIn (nm, syn (ExprDotDot (iLit 0L, iLit (int64 n))), inner) ])
                idxNames dims (pre @ [write])
        (Some (bufLet :: loops, dims)))))))
    | _ -> Ok None

/// The pre-pass proper: rewrite one function body's statements, expanding
/// recursive-array lets and hoisting reduces, threading an extent env so
/// reduce sources can recover their loop bound.
let internal preNormalizeBody (fname: string) (ctx: Ctx) (fd0: FunctionDecl) : Result<Expr, string> =
    // C7: pipelines first. Everything downstream -- the extent env, the
    // tangent walker, the reduce lowering -- then sees ordinary maps. A
    // pipeline the rewrite DECLINED is a refusal here rather than a
    // misleading BL5501 five seams later: the tangent walker has no other
    // rule to reach for.
    let fusedBody0, fuseDeclines = fuseFunctionBody ctx fd0
    // Then the grouped-peel lowering, which reads POST-fusion shapes and must
    // land before `hoistReduces` -- hoistReduces is what refuses the group
    // axis today, and it would refuse before ever seeing the peel.
    let fusedBody, gpDeclines = lowerGroupedPeels ctx fd0 fusedBody0
    let fd = { fd0 with Body = fusedBody }
    let declines = fuseDeclines @ gpDeclines
    if not (List.isEmpty declines) then err fname (List.head declines) else
    let paramExtents =
        fd.Params |> List.choose (fun p ->
            match p.Type with
            | Some t -> (match arrayLiteralExtents (resolveArrayTy ctx t) with Some (true, [n]) -> Some (p.Name, n) | _ -> None)
            | None -> None)
        |> Map.ofList
    let stmts0, finalOpt =
        match fd.Body.Kind with
        | ExprKind.ExprBlock (ss, fe) -> ss, fe
        | _ -> [], Some fd.Body
    // C7: rank-1 index types per named array, for the sort expansion's
    // `range<I>` and gather-lambda annotations. Seeded from the parameters
    // and extended by ANNOTATED locals -- the index type has to be declared
    // somewhere, since a sort's own result type does not carry it back.
    let paramIdxTys =
        fd.Params |> List.choose (fun p ->
            match p.Type |> Option.map (resolveTy ctx) with
            | Some (TyArray (_, [ity])) -> Some (p.Name, ity)
            | _ -> None)
        |> Map.ofList
    // Every name the SURFACE body binds: the sort expansion checks its
    // synthesized names against this before emitting them.
    let preBound = Set.union (surfaceBoundNames stmts0) (fd.Params |> List.map _.Name |> Set.ofList)
    // A sort ANYWHERE other than directly as a `let` initializer has no
    // expansion site, so it is refused here where the message can say why
    // (rather than reaching the sweeps' generic "unsupported form").
    let noNestedSort (what: string) (e: Expr) : Result<unit, string> =
        if containsSort e then
            err fname $"differentiating `sort` requires it to be the whole initializer of a `let` (v1); {what} contains a nested sort -- bind it with `let s = sort(...)` first"
        else Ok ()
    // ANNOTATED rank-1 array locals, collected in one pass over the surface
    // body rather than threaded through the fold: order does not matter here
    // (a sort can only name a binding already in scope), and over-collecting
    // only widens the key-closure refusal set, which is the safe direction.
    let idxTys =
        let rec collect (acc: Map<string, TypeExpr>) (ss: Stmt list) =
            ss |> List.fold (fun m s ->
                match unwrapStmt s with
                | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }; Type = Some t } ->
                    (match resolveTy ctx t with
                     | TyArray (_, [ity]) -> Map.add nm ity m
                     | _ -> m)
                | StmtForIn (_, _, body) -> collect m body
                | _ -> m) acc
        collect paramIdxTys stmts0
    // A unit-carrying annotation on a let is a CONVERSION site (`let y:
    // Float<meters> = x` with x in km multiplies by 1000 --
    // TypeCheck.convertScaleTo), and the NStmt form the sweeps consume has no
    // annotation slot: the conversion would vanish from the replayed primal
    // and the derivative alike, silently. Refused up front, scanning through
    // StmtSpanned like `idxTys` above (the fold below passes spanned
    // statements through untouched). A dimensionless annotation converts
    // nothing and passes.
    let unitLetCheck : Result<unit, string> =
        let rec scan (ss: Stmt list) : string option =
            ss |> List.tryPick (fun s ->
                match unwrapStmt s with
                | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }; Type = Some t }
                        when (elemUnitOf ctx t).IsSome -> Some nm
                | StmtForIn (_, _, body) -> scan body
                | _ -> None)
        match scan stmts0 with
        | Some nm ->
            err fname $"`let {nm}` is annotated with a unit-carrying type inside the differentiated body, which would be a unit conversion site whose factor the AD transform cannot see; convert outside the differentiated function, or drop the annotation (the value keeps the magnitude it was computed in)"
        | None -> Ok ()
    // Materialized index arrays already in the body -- see `SortPermForm`.
    let surfaceIotas =
        let rec collect (acc: Set<string>) (ss: Stmt list) =
            ss |> List.fold (fun m s ->
                match unwrapStmt s with
                | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }; Value = IndexIota } -> Set.add nm m
                | StmtForIn (_, _, body) -> collect m body
                | _ -> m) acc
        collect Set.empty stmts0
    // FULL static dims per named array, for the eager-map lowering: it sizes
    // a construction buffer and bounds its loops, where the extent env above
    // needs only the leading axis. Seeded from the module bindings (constant
    // data a kernel may read or iterate) and overlaid with the parameters,
    // which shadow them.
    let paramDims =
        fd.Params |> List.choose (fun p ->
            match p.Type with
            | Some t -> (match arrayLiteralExtents (resolveArrayTy ctx t) with
                         | Some (true, ds) -> Some (p.Name, ds)
                         | _ -> None)
            | None -> None)
        |> Map.ofList
    let initDims =
        let moduleDims =
            ctx.ModuleLets |> Map.toSeq
            |> Seq.choose (fun (n, ml) ->
                let byAnn =
                    ml.Ty |> Option.bind (fun t ->
                        match arrayLiteralExtents (resolveArrayTy ctx t) with
                        | Some (true, ds) -> Some ds
                        | _ -> None)
                match byAnn with
                | Some ds -> Some (n, ds)
                | None -> staticDimsOf ctx Map.empty ml.Value |> Option.map (fun ds -> (n, ds)))
            |> Map.ofSeq
        paramDims |> Map.fold (fun acc n ds -> Map.add n ds acc) moduleDims
    // (extent env, dims env, let-bound loop objects / kernel lambdas)
    let rec goStmts (st0: Map<string, int> * Map<string, int list> * Map<string, Expr>) (ss: Stmt list)
        : Result<(Map<string, int> * Map<string, int list> * Map<string, Expr>) * Stmt list, string> =
        ss |> List.fold (fun acc s ->
            acc |> Result.bind (fun ((env, denv, loopEnv), outp) ->
                /// A binding REBINDS its name: drop whatever the two
                /// name-keyed envs held for it rather than leaving them
                /// pointing at the previous value.
                let rebound nm = (Map.remove nm denv, Map.remove nm loopEnv)
                match unwrapStmt s with
                // C7: the plumbing this pass itself emitted (recognized by
                // shape) rides through untouched -- a composition round
                // re-runs the pre-pass over an already-expanded body.
                | StmtLet { Value = SortPermForm surfaceIotas _ } -> Ok ((env, denv, loopEnv), outp @ [s])
                // C7: `let s = sort(A, key)` -- materialize the permutation
                // (and, in reverse mode, its inverse) BEFORE the unchanged
                // primal sort.
                | StmtLet ({ Value = { Kind = ExprKind.ExprSort (operand, key) }
                             Pattern = { Kind = PatternKind.PatVar nm } } as b) ->
                    expandSort fname ctx idxTys preBound nm operand key
                    |> Result.map (fun (plumbing, plan) ->
                        let env' =
                            match Map.tryFind plan.Src env with
                            | Some cnt -> Map.add nm cnt env
                            | None -> env
                        let denv0, loopEnv' = rebound nm
                        let denv' =
                            match Map.tryFind plan.Src denv0 with
                            | Some ds -> Map.add nm ds denv0
                            | None -> denv0
                        ((env', denv', loopEnv'), outp @ plumbing @ [StmtLet b]))
                | StmtLet { Value = { Kind = ExprKind.ExprSort _ } } ->
                    err fname "differentiating `sort` requires it to bind a single name (v1)"
                | StmtLet { Value = { Kind = ExprKind.ExprRecArray def }; Type = Some annot; Pattern = { Kind = PatternKind.PatVar nm } } ->
                    expandRecArray fname ctx nm annot def
                    |> Result.map (fun (emitted, ext) ->
                        let denv0, loopEnv' = rebound nm
                        ((Map.add nm ext env, Map.add nm [ext] denv0, loopEnv'), outp @ emitted))
                | StmtLet { Value = { Kind = ExprKind.ExprRecArray _ } } ->
                    err fname "recursive array must bind a single annotated name to be differentiable (v1)"
                // A let-bound loop object or kernel lambda: RECORDED, so the
                // eager-map lowering can resolve `L <@> k` by name, and kept
                // -- `dropDeadLoopBindings` removes it only once every use has
                // been rewritten away, so a shape the lowering declined still
                // meets the combinator refusal it met before.
                | StmtLet ({ Pattern = { Kind = PatternKind.PatVar nm } } as b)
                        when errMode.Value = "grad" && (loopPlumbingName s).IsSome ->
                    Ok ((env, Map.remove nm denv, Map.add nm b.Value loopEnv), outp @ [s])
                | StmtLet ({ Pattern = { Kind = PatternKind.PatVar nm } } as b) ->
                    noNestedSort $"the initializer of '{nm}'" b.Value |> Result.bind (fun () ->
                    // C2-reverse: an eager map becomes the construction loop
                    // the sweeps already differentiate, BEFORE hoistReduces
                    // (which would otherwise meet the map as a reduce source
                    // and refuse it).
                    let plainLet () =
                        hoistReduces fname ctx env b.Value |> Result.map (fun (pre, value') ->
                            let byAnn =
                                match b.Type with
                                | Some t -> arrayLiteralExtents (resolveArrayTy ctx t)
                                | None -> None
                            let env' =
                                let byAnnLead = match byAnn with Some (true, [n]) -> Some n | _ -> None
                                let byLit = staticExtentOf ctx env value'
                                match (match byAnnLead with Some _ -> byAnnLead | None -> byLit) with
                                | Some cnt -> Map.add nm cnt env
                                | None -> env
                            let denv0, loopEnv' = rebound nm
                            let denv' =
                                match (match byAnn with
                                       | Some (true, ds) -> Some ds
                                       | _ -> staticDimsOf ctx denv0 value') with
                                | Some ds -> Map.add nm ds denv0
                                | None -> denv0
                            ((env', denv', loopEnv'), outp @ pre @ [StmtLet { b with Value = value' }]))
                    expandEagerMap fname ctx env denv loopEnv nm b.Type b.Value
                    |> Result.bind (fun mapped ->
                        match mapped with
                        | Some (emitted, dims) ->
                            let denv0, loopEnv' = rebound nm
                            let env' =
                                match dims with
                                | d :: _ -> Map.add nm d env
                                | [] -> env
                            Ok ((env', Map.add nm dims denv0, loopEnv'), outp @ emitted)
                        | None -> plainLet ()))
                | StmtLet b ->
                    noNestedSort "a let initializer" b.Value |> Result.bind (fun () ->
                    hoistReduces fname ctx env b.Value |> Result.map (fun (pre, value') ->
                        let names = patternBoundNames b.Pattern
                        let denv' = names |> List.fold (fun m n -> Map.remove n m) denv
                        let loopEnv' = names |> List.fold (fun m n -> Map.remove n m) loopEnv
                        ((env, denv', loopEnv'), outp @ pre @ [StmtLet { b with Value = value' }])))
                | StmtExpr ex ->
                    noNestedSort "this statement" ex |> Result.bind (fun () ->
                    hoistReduces fname ctx env ex |> Result.map (fun (pre, ex') ->
                        ((env, denv, loopEnv), outp @ pre @ [StmtExpr ex'])))
                | StmtAssign (lhs, op, rhs) ->
                    noNestedSort "this assignment" rhs |> Result.bind (fun () ->
                    hoistReduces fname ctx env rhs |> Result.map (fun (pre, rhs') ->
                        ((env, denv, loopEnv), outp @ pre @ [StmtAssign (lhs, op, rhs')])))
                | StmtForIn (var, range, body) ->
                    goStmts (env, denv, loopEnv) body |> Result.map (fun (_, body') ->
                        ((env, denv, loopEnv), outp @ [StmtForIn (var, range, body')]))
                | StmtSpanned _ -> Ok ((env, denv, loopEnv), outp @ [s])))
            (Ok (st0, []))
    unitLetCheck
    |> Result.bind (fun () -> goStmts (paramExtents, initDims, Map.empty) stmts0)
    |> Result.bind (fun ((env, _, _), stmts') ->
        match finalOpt with
        | Some fe ->
            noNestedSort "the returned expression" fe |> Result.bind (fun () ->
            hoistReduces fname ctx env fe |> Result.map (fun (pre, fe') ->
                inheritSpan fd.Body (ExprBlock (dropDeadLoopBindings (stmts' @ pre) (Some fe'), Some fe'))))
        | None ->
            Ok (inheritSpan fd.Body (ExprBlock (dropDeadLoopBindings stmts' None, None))))

/// How deep call substitution may nest before the transform gives up. One
/// constant for BOTH inliners -- `normalizeBody`'s statement-level one and
/// `kernelCallBody`'s expression-level one -- because they cap the same
/// thing: a self-recursive callee has no finite substitution, and the cap is
/// the backstop for the chains the by-name recursion check cannot see.
let internal maxInlineDepth = 32

/// May this callee be substituted into differentiated code at all? One gate
/// for BOTH inliners -- `inlineCall`'s statement-level splice and
/// `kernelCallBody`'s expression-level substitution -- because the three
/// conditions are properties of the DECLARATION, not of the position it is
/// met in: a static function has no runtime body to differentiate, a
/// mut-parameter callee writes through its arguments (which neither sweep
/// tracks), and a call at the wrong arity has no parameter-to-argument
/// pairing to substitute. Each inliner keeps its own depth and recursion
/// caps, which ARE position-specific.
///
/// The messages are pinned by corpus `ERROR-CONTAINS` tests; they were
/// byte-identical in the two copies this replaces, which is exactly the
/// invariant a shared gate makes structural.
let internal checkInlinable (fname: string) (fd: FunctionDecl) (argCount: int)
    : Result<unit, string> =
    if fd.IsStatic then err fname $"cannot differentiate through static function '{fd.Name}'"
    elif fd.Params |> List.exists (fun p -> p.Mutability = Mutable) then
        err fname $"cannot differentiate through '{fd.Name}': mut-parameter functions are not inlinable (v1)"
    elif argCount <> fd.Params.Length then
        err fname $"'{fd.Name}' called with {argCount} arguments, expects {fd.Params.Length}"
    else Ok ()

/// Normalize + inline a function body to the flat NStmt fragment:
/// all user calls inlined, all statements validated.
///
/// MEMOIZED per (callee, depth), which is what stops a helper CHAIN from
/// costing exponentially: `f` calling `g` twice, `g` calling `h` twice, is
/// four normalizations of `h` and 2^d at depth d, all of them producing the
/// same fragment. Every input that could make two normalizations of one
/// callee differ is fixed inside a single synthesis -- `Decls` (the memo
/// table is per-request, see `Ctx.NormMemo`), the top-level `fname` that
/// prefixes refusals, and `errMode` -- except `depth`, which is in the key:
/// it gates only the cap, so two calls at the SAME depth are interchangeable
/// while a deeper one must be allowed to hit the cap on its own.
///
/// Only successes are cached. Caching a refusal would be sound by the same
/// argument, but a refusal ends the synthesis anyway, so it would never be
/// read.
///
/// The arguments do not enter it: they bind AFTER normalization, through the
/// rename map `inlineCall` builds, which also gives the callee's binders a
/// call-site-unique `__in<N>_` prefix -- so one shared fragment cannot leak
/// a name from one site to another.
let rec internal normalizeBody (fname: string) (ctx: Ctx) (depth: int) (fd: FunctionDecl)
    : Result<NStmt list * Expr, string> =
    if depth > maxInlineDepth then
        err fname $"call inlining exceeded depth {maxInlineDepth} (recursive functions are not differentiable)"
    else
    match ctx.NormMemo.TryGetValue ((fd.Name, depth)) with
    | true, hit -> Ok hit
    | _ ->
    normalizeBodyUncached fname ctx depth fd
    |> Result.map (fun r -> ctx.NormMemo.[(fd.Name, depth)] <- r; r)

and internal normalizeBodyUncached (fname: string) (ctx: Ctx) (depth: int) (fd: FunctionDecl)
    : Result<NStmt list * Expr, string> =
    // Lower the imperative-free surface constructs (recursive arrays, reduce)
    // into accumulation/construction statements before the NFor pipeline runs.
    preNormalizeBody fname ctx fd |> Result.bind (fun body' ->
    convertBody fname body' |> Result.bind (fun (stmts, finalE) ->
    // hoist calls inside the final expression too
    hoistCalls fname ctx finalE |> Result.bind (fun (finalHoist, finalE') ->
    let rec normStmts (ss: NStmt list) : Result<NStmt list, string> =
        ss |> traverseR (fun s ->
            match s with
            // DIRECT user-call let: the call is already in inlinable
            // position -- hoist only inside its ARGUMENTS, then inline.
            // (Hoisting the call itself would create `let tmp = f(..)`
            // and re-normalizing that let would hoist again, forever.)
            | NLet (name, isMut, { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar callee }, args) }) when Map.containsKey callee ctx.Decls ->
                let argsFolded =
                    args |> traverseR (hoistCalls fname ctx)
                    |> Result.map (fun pairs -> (pairs |> List.collect fst, pairs |> List.map snd))
                argsFolded |> Result.bind (fun (argHoists, args') ->
                normStmts argHoists |> Result.bind (fun argHoists' ->
                inlineCall fname ctx depth callee args' name isMut
                |> Result.map (fun inlined -> argHoists' @ inlined)))
            | NLet (name, isMut, value) ->
                hoistCalls fname ctx value |> Result.bind (fun (hoisted, value') ->
                // `hoisted` contains only direct-call lets, which the
                // arm above inlines without further hoisting.
                normStmts hoisted |> Result.map (fun hoisted' ->
                    hoisted' @ [NLet (name, isMut, value')]))
            | NAssign (lhs, rhs) ->
                hoistCalls fname ctx rhs |> Result.bind (fun (hoisted, rhs') ->
                normStmts hoisted |> Result.map (fun hoisted' ->
                    hoisted' @ [NAssign (lhs, rhs')]))
            | NFor (var, lo, hi, body) ->
                normStmts body |> Result.map (fun body' ->
                    [NFor (var, lo, hi, body')]))
        |> Result.map List.concat
    normStmts (stmts @ finalHoist) |> Result.map (fun ns -> (ns, finalE')))))

/// Inline `let target = callee(args)`: bind arguments to fresh param names,
/// splice the callee's own normalized body with all its binders renamed,
/// and bind the callee's final expression to `target` (or rename in place
/// when the final expression is just a local variable -- avoiding an array
/// alias, which the adjoint bookkeeping cannot track).
and internal inlineCall (fname: string) (ctx: Ctx) (depth: int)
                       (callee: string) (args: Expr list)
                       (target: string) (targetMut: bool)
    : Result<NStmt list, string> =
    let fd = ctx.Decls.[callee]
    checkInlinable fname fd args.Length |> Result.bind (fun () ->
    normalizeBody fname ctx (depth + 1) fd |> Result.bind (fun (calleeStmts, calleeFinal) ->
        let tag = fresh ctx "__in"
        // Param binding: plain-var arguments bind by RENAMING the callee
        // param to the caller's variable (no let) -- this is what routes
        // array cotangents straight to the caller's d-buffers and avoids
        // array-alias lets, which the adjoint bookkeeping rejects.
        // Non-var arguments (scalar expressions) bind through a fresh let.
        let paramBinds =
            List.zip fd.Params args
            |> List.map (fun (p, a) ->
                match a.Kind with
                | ExprKind.ExprVar argName -> (p.Name, argName, None)
                | _ -> (p.Name, $"{tag}_{p.Name}", Some a))
        let paramRen =
            paramBinds |> List.map (fun (pn, target, _) -> (pn, target)) |> Map.ofList
        let localRen =
            boundNames calleeStmts
            |> List.distinct
            |> List.map (fun n -> (n, $"{tag}_{n}"))
            |> Map.ofList
        let ren = Map.fold (fun acc k v -> Map.add k v acc) paramRen localRen
        // Rename refusals (binder capture) carry the mode prefix via `err`
        // so the expand boundary codes them BL5500/BL5501 correctly.
        let viaErr res = match res with Ok v -> Ok v | Error m -> err fname m
        viaErr (renameNStmts ren calleeStmts) |> Result.bind (fun renStmts ->
        viaErr (renameExpr ren calleeFinal) |> Result.bind (fun renFinal ->
        let paramLets =
            paramBinds |> List.choose (fun (_, target, argOpt) ->
                argOpt |> Option.map (fun a -> NLet (target, false, a)))
        match renFinal.Kind with
        | ExprKind.ExprVar localName when (localRen |> Map.exists (fun _ v2 -> v2 = localName)) ->
            // Final expr is a callee-local: rename that local to the target
            // name instead of emitting `let target = local` (array-alias).
            let ren2 = Map.ofList [(localName, target)]
            viaErr (renameNStmts ren2 renStmts) |> Result.map (fun renStmts2 -> paramLets @ renStmts2)
        | _ ->
            Ok (paramLets @ renStmts @ [NLet (target, targetMut, renFinal)])))))

// Classification: differentiable variables, array-ness

let internal isFloatTy (t: TypeExpr) : bool =
    match t with
    | TyFloat64 | TyFloat32 -> true
    | TyNamed (("Float" | "Float64" | "Float32"), []) -> true
    | _ -> false

type internal ParamClass =
    | DiffArray
    | DiffScalar
    | NonDiff

/// Classify one parameter. Complex types get an EXPLICIT refusal rather than
/// the NonDiff fall-through: silently treating a parameter as
/// non-differentiable drops its partial from the gradient with no diagnostic
/// -- the same wrong-answer class as an unknown-derivative intrinsic, and
/// worse than refusing. Unit-carrying Floats are differentiable in BOTH
/// modes: forward gives a tangent the primal's type, reverse declares each
/// cotangent as <loss>/<parameter> (Grad.cotangentTy) and lets the checker
/// hold the synthesized arithmetic to it.
let internal classifyParam (fname: string) (ctx: Ctx) (p: ParamDecl) : Result<ParamClass, string> =
    match p.Type with
    | None -> err fname $"parameter '{p.Name}' must have a type annotation"
    | Some t0 ->
        let refuseComplex (what: string) =
            err fname $"parameter '{p.Name}' {what}: complex parameters are not differentiable (v1); complex derivatives need a holomorphic/Wirtinger convention the AD subset does not define"
        let t = resolveTy ctx t0
        match t with
        | _ when isFloatTy t -> Ok DiffScalar
        | TyNamed (("Float" | "Float64" | "Float32"), _ :: _) -> Ok DiffScalar
        | TyComplex64 | TyComplex128 | TyNamed (("Complex64" | "Complex128"), _) -> refuseComplex "is complex"
        | TyArray (elem, _) ->
            let el = resolveTy ctx elem
            if isFloatTy el then Ok DiffArray
            else
                (match el with
                 | TyNamed (("Float" | "Float64" | "Float32"), _ :: _) -> Ok DiffArray
                 | TyComplex64 | TyComplex128 | TyNamed (("Complex64" | "Complex128"), _) -> refuseComplex "is a complex array"
                 | _ -> Ok NonDiff)
        | _ -> Ok NonDiff

/// Zero value matching an array literal's (or constant fill's) shape.
let rec internal zerosLikeLiteral (e: Expr) : Expr option =
    match e with
    | { Kind = ExprKind.ExprArrayLit elems } ->
        let zs = elems |> List.map (fun el ->
            match zerosLikeLiteral el with
            | Some z -> z
            | None -> fLit 0.0)
        Some (inheritSpan e (ExprArrayLit zs))
    | ConstFill (cnt, _) -> Some (zeroFill cnt)
    | _ -> None

/// Zero value for a differentiable param's declared type (arrays need
/// literal extents in v1 -- callers allocate those buffers, so this is only
/// used for LOCAL d-vars, which come from literals instead).
let internal zerosOfType (fname: string) (t: TypeExpr) : Result<Expr, string> =
    let rec go t =
        match t with
        | _ when isFloatTy t -> Ok (fLit 0.0)
        | TyArray (elem, idxs) ->
            let folded =
                idxs |> traverseR (fun ix ->
                    match ix with
                    | TyIdx { Kind = ExprKind.ExprLit (LitInt n) } -> Ok (int n)
                    | _ -> err fname "differentiable arrays need literal Idx<n> extents (v1)")
            folded |> Result.bind (fun ns ->
                go elem |> Result.map (fun z ->
                    ns |> List.rev |> List.fold (fun inner n -> syn (ExprArrayLit (List.replicate n inner))) z))
        | _ -> err fname "cannot build a zero cotangent for this type"
    go t

/// Partials of the two-argument intrinsics, per operand -- the binary sibling
/// of `derivRule`, whose signature (name -> Expr -> Expr option) is
/// structurally unary and has nowhere to put a second operand. Returns
/// (d/dFirst, d/dSecond) as expressions of the FORWARD operands; both AD
/// modes consume it the same way (reverse multiplies by the cotangent,
/// forward by the operand tangents).
///   atan2(y, x): d/dy =  x/(x^2+y^2),  d/dx = -y/(x^2+y^2)
///   log_base(x, b) = log x / log b:
///                    d/dx = 1/(x log b),  d/db = -log x/(b (log b)^2)
let internal binaryDerivRule (name: string) (a: Expr) (b: Expr) : Expr * Expr =
    match name with
    | "atan2" ->
        let denom = add (mul a a) (mul b b)
        (div b denom, neg (div a denom))
    | _ ->  // log_base(x, b)
        let lb = call "log" [b]
        (div (fLit 1.0) (mul a lb),
         neg (div (call "log" [a]) (mul b (mul lb lb))))

// Taint analysis

/// Variables carrying differentiable (Float) dataflow, and which of those
/// are arrays. Two passes for loop-carried taint.
let internal analyze (fname: string) (ctx: Ctx)
                    (diffParams: Set<string>) (arrayParams: Set<string>)
                    (stmts: NStmt list) (finalE: Expr)
    : Result<Set<string> * Set<string>, string> =
    let mutable diff = diffParams
    let mutable arrays = arrayParams
    let luOf = luFactorsOf stmts
    let touches (e: Expr) : Result<bool, string> =
        let mutable hit = false
        walkExpr fname ctx (fun n -> if Set.contains n diff then hit <- true) false e
        |> Result.map (fun () -> hit)
    let rec pass (ss: NStmt list) : Result<unit, string> =
        ss |> iterR (fun s ->
            match s with
            // An LU factor (`let f = m.lu(A)`, elaborated `__math_lu(A)`) is
            // STRUCTURAL: it carries no taint of its own -- the solve arms
            // consult the matrix it was taken from (GradSweeps' lu arms) --
            // and a tuple is neither an array nor a scalar carrier.
            | NLet (_, _, { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "__math_lu" }, _) }) -> Ok ()
            // A solve against a factor is active when its right-hand side is
            // OR the factored matrix is (the factor itself never is).
            | NLet (name, _, { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar op }, [ luE; _; bE ]) }) when isLuSolveName op ->
                arrays <- Set.add name arrays
                let viaMatrix =
                    match luFactorMatrix luOf luE with
                    | Some a -> Set.contains a diff
                    | None -> false
                touches bE |> Result.map (fun t -> if t || viaMatrix then diff <- Set.add name diff)
            | NLet (name, _, value) ->
                (match value with
                 | { Kind = ExprKind.ExprArrayLit _ } | ConstFill _ -> arrays <- Set.add name arrays
                 | { Kind = ExprKind.ExprVar src } when Set.contains src arrays ->
                     arrays <- Set.add name arrays
                 // C1: a local bound to a linear combinator is an array
                 // if the form produces one -- without this its element
                 // reads would silently yield no tangent.
                 | _ when producesArray arrays value -> arrays <- Set.add name arrays
                 | _ -> ())
                // FLOAT array literals are differentiable carriers even
                // before any diff flow reaches them (their cotangents
                // must exist). Int-literal tables (index/offset data,
                // e.g. ML-elaboration path tables) are not -- their
                // reads only ever appear in index and bound positions.
                let rec isFloatLit (e: Expr) =
                    match e.Kind with
                    | ExprKind.ExprArrayLit es -> es |> List.forall isFloatLit
                    | ExprKind.ExprLit (LitFloat _) -> true
                    | _ -> false
                touches value |> Result.map (fun t ->
                    if t then diff <- Set.add name diff
                    match value with
                    | { Kind = ExprKind.ExprArrayLit _ } when isFloatLit value -> diff <- Set.add name diff
                    | ConstFill (_, LitFloat _) -> diff <- Set.add name diff
                    | _ -> ())
            | NAssign (lhs, rhs) ->
                touches rhs |> Result.map (fun t ->
                    if t then
                        match lhs.Kind with
                        | ExprKind.ExprVar n -> diff <- Set.add n diff
                        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, _) -> diff <- Set.add a diff
                        | _ -> ())
            | NFor (_, _, _, body) -> pass body)
    pass stmts
    |> Result.bind (fun () -> pass stmts)     // second pass: loop-carried
    |> Result.bind (fun () -> touches finalE)
    |> Result.bind (fun finalTouches ->
        if not finalTouches then
            err fname "the returned value does not depend on any differentiable parameter"
        else Ok (diff, arrays))

// Restriction checks (accumulator discipline)

/// Names assigned (either form) anywhere in a statement list.
let rec internal assignedNames (stmts: NStmt list) : Set<string> =
    stmts |> List.fold (fun acc s ->
        match s with
        | NLet _ -> acc
        | NAssign ({ Kind = ExprKind.ExprVar n }, _) -> Set.add n acc
        | NAssign ({ Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, _) }, _) -> Set.add a acc
        | NAssign _ -> acc
        | NFor (_, _, _, body) -> Set.union acc (assignedNames body)) Set.empty

/// `x = x + e` / `x = x - e` (and element forms), the additive-self
/// pattern, which the same-direction adjoint loop handles exactly.
/// Returns (sign, e) when matched: +1.0 for add; -1.0 for sub.
let internal additiveSelf (lhs: Expr) (rhs: Expr) : (float * Expr) option =
    let rec sameLhs (a: Expr) (b: Expr) =
        match a, b with
        | { Kind = ExprKind.ExprVar x }, { Kind = ExprKind.ExprVar y } -> x = y
        | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar x }, ix) }, { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar y }, iy) } ->
            x = y && ix.Length = iy.Length && List.forall2 sameLhs ix iy
        | { Kind = ExprKind.ExprLit l1 }, { Kind = ExprKind.ExprLit l2 } -> l1 = l2
        | { Kind = ExprKind.ExprBinOp (_, o1, a1, b1) }, { Kind = ExprKind.ExprBinOp (_, o2, a2, b2) } ->
            o1 = o2 && sameLhs a1 a2 && sameLhs b1 b2
        | { Kind = ExprKind.ExprTyped (i1, _) }, i2 | i1, { Kind = ExprKind.ExprTyped (i2, _) } -> sameLhs i1 i2
        | _ -> false
    match rhs.Kind with
    | ExprKind.ExprBinOp (_, OpAdd, l, e) when sameLhs l lhs -> Some (1.0, e)
    | ExprKind.ExprBinOp (_, OpAdd, e, l) when sameLhs l lhs -> Some (1.0, e)
    | ExprKind.ExprBinOp (_, OpSub, l, e) when sameLhs l lhs -> Some (-1.0, e)
    | _ -> None

/// Straight-line ordering discipline: the adjoint sweep re-evaluates
/// forward expressions, which see each variable's FINAL value. That is only
/// sound when no differentiable expression reads a mutable variable that is
/// written again LATER in forward order. Whole loops count as one position
/// (intra-loop ordering is checkLoopDiscipline's job). The additive-self
/// lhs occurrence is exempt (its derivative is 1 regardless of value).
let internal checkWriteAfterRead (fname: string) (ctx: Ctx) (stmts: NStmt list) : Result<unit, string> =
    // last write position per var, loops opaque
    let lastWrite = System.Collections.Generic.Dictionary<string, int>()
    stmts |> List.iteri (fun i s ->
        let record n = lastWrite.[n] <- i
        match s with
        | NLet _ -> ()
        | NAssign ({ Kind = ExprKind.ExprVar n }, _) -> record n
        | NAssign ({ Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, _) }, _) -> record a
        | NAssign _ -> ()
        | NFor (_, _, _, body) -> assignedNames body |> Set.iter record)
    let checkExprAt (i: int) (e: Expr) : Result<unit, string> =
        let mutable bad = None
        walkExpr fname ctx (fun n ->
            if bad.IsNone then
                match lastWrite.TryGetValue n with
                | true, j when j > i -> bad <- Some n
                | _ -> ()) false e
        |> Result.bind (fun () ->
            match bad with
            | Some n -> err fname $"'{n}' is read here but written again later; the reverse sweep re-evaluates forward expressions at FINAL values, so read-then-rewrite of a mutable is not differentiable (bind a fresh let instead)"
            | None -> Ok ())
    stmts |> List.mapi (fun i s -> (i, s)) |> iterR (fun (i, s) ->
        match s with
        | NLet (_, _, value) -> checkExprAt i value
        | NAssign (lhs, rhs) ->
            (match additiveSelf lhs rhs with
             | Some (_, e) -> checkExprAt i e
             | None -> checkExprAt i rhs)
            |> Result.bind (fun () ->
                // index expressions of an element write
                match lhs.Kind with
                | ExprKind.ExprApp (_, idxs) -> idxs |> iterR (checkExprAt i)
                | _ -> Ok ())
        | NFor (_, lo, hi, body) ->
            checkExprAt i lo
            |> Result.bind (fun () -> checkExprAt i hi)
            |> Result.bind (fun () ->
                // expressions INSIDE the loop must not read vars written
                // after the loop either
                let rec checkBody ss =
                    ss |> iterR (fun s2 ->
                        match s2 with
                        | NLet (_, _, value) -> checkExprAt i value
                        | NAssign (l2, r2) ->
                            (match additiveSelf l2 r2 with
                             | Some (_, e) -> checkExprAt i e
                             | None -> checkExprAt i r2)
                        | NFor (_, l2, h2, b2) ->
                            checkExprAt i l2
                            |> Result.bind (fun () -> checkExprAt i h2)
                            |> Result.bind (fun () -> checkBody b2))
                checkBody body))

/// `for t in lo..hi { lets*; s(t) = s(t - 1) + INC }` -- the additive carry
/// the recursive-array pre-pass emits (GradExpand.expandRecArray), with INC
/// and the lets free of `s`. Its adjoint is the same loop run BACKWARDS: the
/// general-overwrite rule saves and zeros `__g_s(t)`, the array-read rule
/// scatters the saved cotangent into `__g_s(t - 1)`, and INC's adjoint
/// follows -- every step is already right, only the ORDER was not (the
/// generic `NFor` adjoint replays ascending). GradSweeps' `NFor` arm uses
/// this to emit the descending sweep; checkLoopDiscipline uses it to exempt
/// the buffer from the "array recurrence" refusal. Structural, so it
/// survives the inliner's renaming (a name-keyed table would not).
/// Returns (buffer, step var, lo, hi, leading lets, increment).
let internal (|CarryLoop|_|) (s: NStmt) : (string * string * Expr * Expr * NStmt list * Expr) option =
    match s with
    | NFor (t, lo, hi, body) when not body.IsEmpty ->
        let lets, last = List.splitAt (body.Length - 1) body
        let allLets = lets |> List.forall (function NLet _ -> true | _ -> false)
        let isStepMinusOne (e: Expr) =
            match e.Kind with
            | ExprKind.ExprBinOp (_, OpSub, { Kind = ExprKind.ExprVar sv }, { Kind = ExprKind.ExprLit (LitInt 1L) }) -> sv = t
            | _ -> false
        let rec strip (e: Expr) = match e.Kind with ExprKind.ExprTyped (i, _) -> strip i | _ -> e
        let isPrevRead (buf: string) (e: Expr) =
            match (strip e).Kind with
            | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar b }, [idx]) -> b = buf && isStepMinusOne idx
            | _ -> false
        let isLitOne (e: Expr) = match e.Kind with ExprKind.ExprLit (LitInt 1L) -> true | _ -> false
        (match last with
         | [ NAssign ({ Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar buf }, [ { Kind = ExprKind.ExprVar tv } ]) }, rhs) ]
                when tv = t && allLets && isLitOne lo ->
             let inc =
                 match (strip rhs).Kind with
                 | ExprKind.ExprBinOp (_, OpAdd, a, b) when isPrevRead buf a && not (mentionsVar buf b) -> Some b
                 | ExprKind.ExprBinOp (_, OpAdd, a, b) when isPrevRead buf b && not (mentionsVar buf a) -> Some a
                 | _ -> None
             let letsFree =
                 lets |> List.forall (function NLet (_, _, value) -> not (mentionsVar buf value) | _ -> false)
             (match inc with
              | Some inc when letsFree -> Some (buf, t, lo, hi, lets, inc)
              | _ -> None)
         | _ -> None)
    | _ -> None

/// Non-additive reassignment of a differentiable SCALAR is rejected
/// everywhere: its adjoint needs the pre-statement value, which the
/// re-evaluating reverse sweep cannot see. (Array ELEMENT writes stay legal
/// as construction; their adjoints never read the overwritten value.)
let internal checkNoScalarOverwrite (fname: string) (diff: Set<string>) (stmts: NStmt list) : Result<unit, string> =
    let rec check ss =
        ss |> iterR (fun s ->
            match s with
            | NAssign (({ Kind = ExprKind.ExprVar x } as lhs), rhs) when Set.contains x diff ->
                (match additiveSelf lhs rhs with
                 | Some _ -> Ok ()
                 | None -> err fname $"non-additive reassignment of '{x}' is not differentiable (the reverse sweep sees final values); bind a fresh `let` instead")
            | NFor (_, _, _, body) -> check body
            | _ -> Ok ())
    check stmts

/// Inside a loop, expressions may not READ accumulators mutated in the same
/// loop UNLESS the accumulator is DECLARED in that same loop body: the
/// adjoint loop replays the whole body, reconstructing loop-local values
/// per iteration, so reads of loop-local accumulators are exact.
/// Accumulators that OUTLIVE the loop have unrecoverable mid-iteration
/// values and stay read-banned. The additive-self lhs occurrence is exempt.
let internal checkLoopDiscipline (fname: string) (ctx: Ctx) (loops: NStmt list) : Result<unit, string> =
    let rec check (ss: NStmt list) (inLoop: bool) (loopAccums: Set<string>) : Result<unit, string> =
        ss |> iterR (fun s ->
            match s with
            | NLet (_, _, value) when inLoop ->
                let mutable bad = None
                walkExpr fname ctx (fun n -> if Set.contains n loopAccums && bad.IsNone then bad <- Some n) false value
                |> Result.bind (fun () ->
                    match bad with
                    | Some n -> err fname $"loop-body let reads accumulator '{n}' mutated in the same loop (mid-iteration values are not recoverable; restructure)"
                    | None -> Ok ())
            | NLet _ -> Ok ()
            | NAssign (lhs, rhs) when inLoop ->
                (match additiveSelf lhs rhs with
                 | Some (_, e) ->
                     let mutable bad = None
                     walkExpr fname ctx (fun n -> if Set.contains n loopAccums && bad.IsNone then bad <- Some n) false e
                     |> Result.bind (fun () ->
                         match bad with
                         | Some n -> err fname $"accumulation reads accumulator '{n}' mutated in the same loop; restructure"
                         | None -> Ok ())
                 | None ->
                     match lhs.Kind with
                     | ExprKind.ExprVar x ->
                         err fname $"loop-carried reassignment of '{x}' is not additive (`{x} = {x} +/- e`); only additive accumulation is differentiable in loops (v1)"
                     | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, _) ->
                         // plain element write inside a loop: allowed as
                         // construction, but the rhs may not read the
                         // array being written (array recurrence)
                         let mutable bad = false
                         walkExpr fname ctx (fun n -> if n = a then bad <- true) false rhs
                         |> Result.bind (fun () ->
                             if bad then err fname $"array recurrence on '{a}' (element write whose rhs reads the same array) is not differentiable (v1)"
                             else Ok ())
                     | _ -> err fname "unsupported assignment target")
            | NAssign _ -> Ok ()
            | CarryLoop (buf, _, _, _, lets, inc) ->
                // The additive carry: its one element write reads the same
                // buffer at the previous ordinal BY DESIGN (that is the
                // recurrence), and its adjoint is exact in the descending
                // sweep -- so the "array recurrence" refusal below does not
                // apply. The increment and the leading lets are held to the
                // ordinary discipline against every OTHER accumulator.
                let declared = boundNames lets |> Set.ofList
                let accums = Set.difference (assignedNames lets) declared
                let outer = if inLoop then Set.union loopAccums accums else accums
                check lets true outer
                |> Result.bind (fun () ->
                    let mutable bad = None
                    walkExpr fname ctx (fun n -> if Set.contains n outer && bad.IsNone then bad <- Some n) false inc
                    |> Result.bind (fun () ->
                        match bad with
                        | Some n -> err fname $"accumulation reads accumulator '{n}' mutated in the same loop; restructure"
                        | None -> Ok ()))
            | NFor (_, _, _, body) ->
                // loop-local declarations are replay-reconstructed --
                // exclude them from the read ban
                let declared = boundNames body |> Set.ofList
                let accums = Set.difference (assignedNames body) declared
                check body true (if inLoop then Set.union loopAccums accums else accums))
    check loops false Set.empty

