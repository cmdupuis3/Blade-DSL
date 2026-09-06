/// sgs-module elaboration: the subgrid-closure field formers as compile-time source synthesis, mirroring the math elaborator.
///
/// Surface (reachable only through `import sgs [as <alias>]`; W is a `let static` name or literal). The ops read the
/// DECLARED shape (this pass runs before type inference and the generated former is specialized to the extents), so the
/// field argument must carry an annotation: a variable bound by an annotated `let` (module-level or block-local), an
/// annotated parameter, a call of a function with an annotated array return type, or an ascription `(expr : Array<...>)`.
///
///   sgs.grad(U, DX)        -- (3, n, n, n) -> (3, 3, n, n, n): G(c,d,i,j,k)
///                             = d_d u_c, 2nd-order central diff, periodic
///   sgs.box_filter(U, W)   -- (3, n, n, n) -> (3, m, m, m), m = n/W: tile means
///   sgs.stress(U, W)       -- (3, n, n, n) -> (6, m, m, m): exact subgrid
///                             stress tau_ij = mean(u_i u_j|tile) - mean_i mean_j,
///                             packed in CartesianBridge.packPairs order
///
/// For each distinct (op, resolved shape/config) the elaborator synthesizes ONE Blade function (`__sgs_1`, ...) via
/// Blade.Sgs.Decls; call sites rewrite to the generated names, deduped by fingerprint.
///
/// Pipeline position: AFTER ML elaboration (so `where ml.galilean` bodies can be judged with surface `sgs.*` calls still
/// visible at ML's seam) and before PPL/Math/Grad -- generated bodies remain plain differentiable Blade source.
module Blade.Sgs.Elaborate

open Blade.Ast
open Blade.StaticEval
open Blade.Sgs.Decls

// Module-level context (the MathElaborate contract)

/// A name -> declared array shape map (the module-level tables).
type private Shapes = Map<string, TypeExpr * TypeExpr list>

/// The LEXICAL scope threaded through the walker: parameters and block-local lets. A binder is recorded even when it is NOT
/// array-annotated (as `None`), because it still SHADOWS an outer name -- without that, an unannotated local `let f = ...`
/// over a module-level annotated `f` would fall through to the module-level shape and silently run at the wrong extents.
type private Scope = Map<string, (TypeExpr * TypeExpr list) option>

type private Ctx = {
    Arrays: Shapes                              // module-level annotated lets
    Funcs: Shapes                               // top-level fns with an annotated array return type
    Aliases: Map<string, TypeExpr>
    Statics: StaticEnv
}

let private collectAliases (decls: Located<Decl> list) : Map<string, TypeExpr> =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclType (TyDeclAlias (name, _, body)) -> Map.add name body acc
        | _ -> acc) Map.empty

/// Annotations may name a whole-array type alias (`type Field = Array<...>`) -- resolve top-level aliases (cycle-bounded)
/// before matching for TyArray. TyBounded is TRANSPARENT here, for diagnostic ORDER only (see MathElaborate.resolveTop): a
/// bound on an aggregate is the checker's rejection to make (BL4003); this pass must not pre-empt it with "no declared shape".
let rec private resolveTop (aliases: Map<string, TypeExpr>) (fuel: int) (ty: TypeExpr) =
    match ty with
    | TyNamed (n, []) when fuel > 0 ->
        match Map.tryFind n aliases with
        | Some body -> resolveTop aliases (fuel - 1) body
        | None -> ty
    | TyBounded (baseTy, _, _) when fuel > 0 -> resolveTop aliases (fuel - 1) baseTy
    | _ -> ty

/// An annotation, alias-resolved, if it denotes an array.
let private arrayAnnot (aliases: Map<string, TypeExpr>) (ty: TypeExpr option) : (TypeExpr * TypeExpr list) option =
    match ty |> Option.map (resolveTop aliases 8) with
    | Some (TyArray (elem, idxs)) -> Some (elem, idxs)
    | _ -> None

let private collectArrays (aliases: Map<string, TypeExpr>) (decls: Located<Decl> list) : Shapes =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclLet b | DeclStatic b ->
            match b.Pattern.Kind, arrayAnnot aliases b.Type with
            | PatternKind.PatVar name, Some shape -> Map.add name shape acc
            | _ -> acc
        | _ -> acc) Map.empty

/// Top-level functions whose annotated RETURN type is an array: a call of one is as good a shape witness as an annotated let.
let private collectFuncs (aliases: Map<string, TypeExpr>) (decls: Located<Decl> list) : Shapes =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclFunction fd ->
            match arrayAnnot aliases fd.ReturnType with
            | Some shape -> Map.add fd.Name shape acc
            | None -> acc
        | _ -> acc) Map.empty

/// Params of a function / lambda, as a lexical scope seed. Unannotated params are recorded as shadowing entries (see Scope).
let private paramShapes (aliases: Map<string, TypeExpr>) (ps: (string * TypeExpr option) list) : Scope =
    ps |> List.fold (fun acc (nm, ty) -> Map.add nm (arrayAnnot aliases ty) acc) Map.empty

let rec private resolveExtent (ctx: Ctx) (ty: TypeExpr) : int option =
    match ty with
    | TyIdx extent ->
        match evalExpr ctx.Statics maxSteps extent with
        | Ok (SVInt n) -> Some (int n)
        | _ -> None
    | TyNamed (name, []) ->
        match Map.tryFind name ctx.Aliases with
        | Some body -> resolveExtent ctx body
        // Not a source alias: a provider axis path, registered by TypeEnv during type checking, so the extent
        // comes from the store's metadata instead.
        | None -> providerIndexExtent ctx.Statics name
    | _ -> None

/// The steer appended to every "no declared shape here" rejection.
let private shapeSources =
    "sgs ops read the DECLARED shape at compile time (the generated former is specialized to the extents), so the argument must carry an annotation"

/// The declared shape of the field argument, with statically known extents. `scope` carries the lexical shapes in force
/// here (annotated params and block-local annotated lets). The returned label is what messages quote.
let private arrayShape (ctx: Ctx) (scope: Scope) (what: string) (e: Expr) : Result<string * int list, string> =
    let finish (label: string) ((_, idxs): TypeExpr * TypeExpr list) =
        let extents = idxs |> List.map (resolveExtent ctx)
        if extents |> List.forall Option.isSome then
            Ok (label, extents |> List.map Option.get)
        else
            Error $"{what}: every axis extent of {label} must be statically known (Idx<n> directly or through aliases)"
    let noShape name = Error $"{what}: '{name}' has no declared array shape -- {shapeSources}"
    match e.Kind with
    // A name: the innermost binder wins; a local binder with no array annotation shadows an outer one.
    | ExprKind.ExprVar name ->
        match Map.tryFind name scope with
        | Some (Some shape) -> finish $"'{name}'" shape
        | Some None -> noShape name
        | None ->
            match Map.tryFind name ctx.Arrays with
            | Some shape -> finish $"'{name}'" shape
            | None -> noShape name
    // An ascription: the universal escape hatch for a shape this pass cannot otherwise see.
    | ExprKind.ExprTyped (_, ty) ->
        match resolveTop ctx.Aliases 8 ty with
        | TyArray (elem, idxs) -> finish "the ascribed expression" (elem, idxs)
        | _ -> Error $"{what}: the ascription must name an array type (Array<Float64 like Idx<...>, ...>)"
    // A call whose function has an annotated array return type. GUARD: in Blade arrays ARE functions, so `A(i)` and `f(x)`
    // are the same node -- exclude known array names so an index read stays on the error path.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, _)
            when not (Map.containsKey f scope) && not (Map.containsKey f ctx.Arrays) ->
        match Map.tryFind f ctx.Funcs with
        | Some shape -> finish $"the result of '{f}'" shape
        | None -> Error $"{what}: '{f}' has no annotated array return type -- {shapeSources}"
    | _ ->
        Error $"{what}: the field argument must be a plain variable naming an annotated let, an annotated parameter, a call of a function with an annotated array return type, or an ascription `(expr : Array<Float64 like Idx<...>, ...>)` -- {shapeSources}"

let private staticInt (statics: StaticEnv) (what: string) (e: Expr) : Result<int, string> =
    match e.Kind with
    | ExprKind.ExprLit (LitInt n) -> Ok (int n)
    | ExprKind.ExprVar name ->
        match Map.tryFind name statics.Values with
        | Some (SVInt n) -> Ok (int n)
        | Some _ -> Error $"{what}: expected a static int"
        | None -> Error $"{what}: '{name}' is not a `let static` binding (sgs op configs must be static)"
    | _ -> Error $"{what}: config argument must be a `let static` binding name or literal"

// Elaboration state
type private ElabState = {
    mutable Counter: int
    mutable Made: Map<string, string>
    mutable Decls: FunctionDecl list
}

let private fingerprint (op: string) (parts: obj) : string =
    sprintf "%s|%A" op parts

let private ensure (st: ElabState) (key: string) (make: string -> FunctionDecl) : string =
    match Map.tryFind key st.Made with
    | Some n -> n
    | None ->
        st.Counter <- st.Counter + 1
        let n = $"__sgs_{st.Counter}"
        let decl = make n
        st.Made <- Map.add key n st.Made
        st.Decls <- st.Decls @ [ decl ]
        n

// Op elaboration

// Galilean stamping: the ML twin of this pass (MLElaborate's `equivStamp`) pins `__ml_equiv` on the kernels it
// synthesizes; the sgs formers carry the SAME kind of by-construction theorem via MLGalilean's discipline -- a constant
// boost u -> u + U0 is annihilated by every one of the two bodies stamped below, so the elaborator pins what it knows
// rather than leaving a loop nest for a later pass to re-derive.
//
// The conjunct is the NORMALIZED `__ml_galilean`, whose args NAME the boost-variant parameters (every other parameter is
// boost-invariant, and the certified result must be boost-invariant -- MLGalilean's rule); it is registered by
// `Blade.ML.Elaborate.expandStr`, which runs unconditionally first, so the name resolves even in a sgs-only program.
//
// ORDERING makes this safe: the galilean judgment runs INSIDE MLElaborate.expandModule over that pass's `decls1`, and sgs
// elaborates strictly AFTER ML, so the decls stamped here cannot exist when `buildCertTable` / `judgeFunction` /
// `inferGalileanCertificates` run -- keeping their bodies (flat work arrays, index arithmetic) out of a judgment they were
// never meant to face.
//
// box_filter is NOT stamped: its seam rule is STATUS-PRESERVING rather than invariant-producing (`sgs.box_filter(U, W)`
// maps a boost-variant field to a boost-variant field, since the weights sum to 1: filter(u + U0) = filter(u) + U0). A
// `__ml_galilean` certificate asserts a boost-INVARIANT result, so stamping it would be a false axiom, not a weaker one.
let private galileanStamp (boostParams: string list) (fd: FunctionDecl) : FunctionDecl =
    let conj = ("__ml_galilean", boostParams)
    let wc =
        match fd.WhereClause with
        | Some w -> { w with Custom = w.Custom @ [ conj ] }
        | None ->
            { Commutativity = []; Antisymmetry = []; Parallel = []
              Repro = false; TDims = []; Custom = [ conj ] }
    { fd with WhereClause = Some wc }

let private opList = "grad, box_filter, stress"

/// The (3, n, n, n) cubic-field shape every sgs former reads.
let private fieldShape (ctx: Ctx) (scope: Scope) (what: string) (uE: Expr) : Result<int, string> =
    arrayShape ctx scope what uE |> Result.bind (fun (_, dims) ->
        match dims with
        | [ 3; a; b; c ] when a = b && b = c -> Ok a
        | [ 3; _; _; _ ] -> Error $"{what}: the field must be cubic (Idx<3> then three equal spatial extents)"
        | _ -> Error $"{what}: the field must be Array<Float64 like Idx<3>, Idx<n>, Idx<n>, Idx<n>> (component-first, space-last)")

let private tileArg (ctx: Ctx) (what: string) (n: int) (wE: Expr) : Result<int, string> =
    staticInt ctx.Statics (what + " W") wE |> Result.bind (fun w ->
        if w < 1 then Error $"{what}: W must be >= 1"
        elif n % w <> 0 then Error $"{what}: the tile width W = {w} must divide the spatial extent n = {n}"
        else Ok w)

let private elabOp (st: ElabState) (ctx: Ctx) (scope: Scope) (op: string) (args: Expr list) : Result<Expr, string> =
    match op, args with
    | "grad", [ uE; dxE ] ->
        fieldShape ctx scope "grad" uE |> Result.map (fun n ->
            // Boost-variant in `u`: every emitted cell is (u(..+1..) - u(..-1..)) / (2 dx), a difference of two reads of
            // the SAME field, so the central-difference weights sum to zero and constant U0 cancels identically at every
            // grid point (`dx` is held-fixed). Exactly MLGalilean's axiom `sgs.grad(U, DX) : U any -> BInv`.
            let nm = ensure st (fingerprint "grad" (box n)) (fun nm -> galileanStamp [ "u" ] (gradDecl nm n))
            syn (ExprApp (syn (ExprVar nm), [ uE; dxE ])))
    | "grad", _ -> Error "grad: expected grad(U, DX) with DX the grid spacing"
    | "box_filter", [ uE; wE ] ->
        fieldShape ctx scope "box_filter" uE |> Result.bind (fun n ->
        tileArg ctx "box_filter" n wE |> Result.map (fun w ->
            let nm = ensure st (fingerprint "box_filter" (box (n, w))) (fun nm -> boxFilterDecl nm n w)
            syn (ExprApp (syn (ExprVar nm), [ uE ]))))
    | "box_filter", _ -> Error "box_filter: expected box_filter(U, W)"
    | "stress", [ uE; wE ] ->
        fieldShape ctx scope "stress" uE |> Result.bind (fun n ->
        tileArg ctx "stress" n wE |> Result.map (fun w ->
            // Boost-variant in `u`: each packed slot is the SECOND CENTRAL comoment over the filter cell,
            // prodsum(u_a, u_b)/T - mu_a*mu_b. Under u -> u + U0 the raw moment picks up
            // U0_a*mu_b + U0_b*mu_a + U0_a*U0_b and the product of means picks up precisely the same, so the difference is
            // unchanged: a central moment of any order >= 2 is boost-invariant, matching MLGalilean's axiom.
            let nm = ensure st (fingerprint "stress" (box (n, w))) (fun nm -> galileanStamp [ "u" ] (stressDecl nm n w))
            syn (ExprApp (syn (ExprVar nm), [ uE ]))))
    | "stress", _ -> Error "stress: expected stress(U, W)"
    | _ -> Error $"sgs: unknown op '{op}' (available: {opList})"

// Rewrite walker (same shape as MathElaborate.rewriteExpr)
let rec private rewriteExpr (st: ElabState) (ctx: Ctx) (aliases: Set<string>) (scope: Scope) (e: Expr)
    : Result<Expr, string> =
    let r = rewriteExpr st ctx aliases scope
    // Same walk under an EXTENDED lexical scope (a binder came into view).
    let rIn (sc: Scope) = rewriteExpr st ctx aliases sc
    // Every binder is recorded, annotated or not: an unannotated one must SHADOW an outer array of the same name.
    let bind (sc: Scope) (nm: string) (ty: TypeExpr option) =
        Map.add nm (arrayAnnot ctx.Aliases ty) sc
    let bindPat (sc: Scope) (b: Binding) =
        match b.Pattern.Kind with
        | PatternKind.PatVar nm -> bind sc nm b.Type
        | _ -> sc
    let rList es =
        es |> List.fold (fun acc x ->
            acc |> Result.bind (fun xs -> r x |> Result.map (fun x' -> xs @ [x'])))
            (Ok [])
    let rOpt (o: Expr option) =
        match o with
        | None -> Ok None
        | Some x -> r x |> Result.map Some
    match e.Kind with
    // Qualified sgs op: `alias.grad(...)` -> generated specialized function. Any alias-qualified call is claimed here so
    // an unknown op gets a steering error instead of an unbound-module type error downstream.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, op) }, args) when Set.contains alias aliases ->
        rList args |> Result.bind (fun args' -> elabOp st ctx scope op args')
    | ExprKind.ExprLit _ | ExprKind.ExprVar _ -> Ok e
    | ExprKind.ExprApp (f, args) ->
        r f |> Result.bind (fun f' -> rList args |> Result.map (fun args' -> inheritSpan e (ExprApp (f', args'))))
    | ExprKind.ExprBinOp (m, op, l, rr) ->
        r l |> Result.bind (fun l' -> r rr |> Result.map (fun r' -> inheritSpan e (ExprBinOp (m, op, l', r'))))
    | ExprKind.ExprUnaryOp (op, inner) -> r inner |> Result.map (fun i -> inheritSpan e (ExprUnaryOp (op, i)))
    | ExprKind.ExprTyped (inner, t) -> r inner |> Result.map (fun i -> inheritSpan e (ExprTyped (i, t)))
    | ExprKind.ExprAssign (l, rr) ->
        r l |> Result.bind (fun l' -> r rr |> Result.map (fun r' -> inheritSpan e (ExprAssign (l', r'))))
    | ExprKind.ExprTuple es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprTuple es'))
    | ExprKind.ExprArrayLit es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprArrayLit es'))
    | ExprKind.ExprDotDot (l, h) ->
        r l |> Result.bind (fun l' -> r h |> Result.map (fun h' -> inheritSpan e (ExprDotDot (l', h'))))
    | ExprKind.ExprIf (c, t, f) ->
        r c |> Result.bind (fun c' ->
        r t |> Result.bind (fun t' ->
        r f |> Result.map (fun f' -> inheritSpan e (ExprIf (c', t', f')))))
    | ExprKind.ExprLet (binding, body) ->
        // The value is checked in the OUTER scope; the body sees the binder.
        r binding.Value |> Result.bind (fun v' ->
        rIn (bindPat scope binding) body
        |> Result.map (fun b' -> inheritSpan e (ExprLet ({ binding with Value = v' }, b'))))
    | ExprKind.ExprBlock (stmts, finalE) ->
        // Statements thread the scope forward: an annotated `let` inside the block is a shape witness for what follows.
        let rec rStmt (sc: Scope) (s: Stmt) : Result<Stmt * Scope, string> =
            match s with
            | StmtSpanned (inner, sp) -> rStmt sc inner |> Result.map (fun (i, sc') -> (StmtSpanned (i, sp), sc'))
            | StmtLet binding ->
                rIn sc binding.Value |> Result.map (fun v' -> (StmtLet { binding with Value = v' }, bindPat sc binding))
            | StmtExpr e2 -> rIn sc e2 |> Result.map (fun x -> (StmtExpr x, sc))
            | StmtAssign (l, op, rr) ->
                rIn sc l |> Result.bind (fun l' -> rIn sc rr |> Result.map (fun r' -> (StmtAssign (l', op, r'), sc)))
            | StmtForIn (var, range, body) ->
                rIn sc range |> Result.bind (fun range' ->
                    body |> List.fold (fun acc bs ->
                        acc |> Result.bind (fun (ss, s0) -> rStmt s0 bs |> Result.map (fun (s', s1) -> (ss @ [s'], s1))))
                        (Ok ([], sc))
                    // Bindings made inside the loop body do not escape it.
                    |> Result.map (fun (body', _) -> (StmtForIn (var, range', body'), sc)))
        stmts |> List.fold (fun acc s ->
            acc |> Result.bind (fun (ss, sc) -> rStmt sc s |> Result.map (fun (s', sc') -> (ss @ [s'], sc'))))
            (Ok ([], scope))
        |> Result.bind (fun (stmts', sc) ->
            match finalE with
            | Some fe -> rIn sc fe |> Result.map (fun fe' -> inheritSpan e (ExprBlock (stmts', Some fe')))
            | None -> Ok (inheritSpan e (ExprBlock (stmts', None))))
    | ExprKind.ExprLambda (ps, w, body) ->
        let sc = ps |> List.fold (fun acc (p: LambdaParam) -> bind acc p.Name p.Type) scope
        rIn sc body |> Result.map (fun b -> inheritSpan e (ExprLambda (ps, w, b)))
    | ExprKind.ExprMatch (scrut, cases) ->
        r scrut |> Result.bind (fun s' ->
            cases |> List.fold (fun acc c ->
                acc |> Result.bind (fun cs ->
                    rOpt c.Guard |> Result.bind (fun g' ->
                    r c.Body |> Result.map (fun b -> cs @ [{ c with Guard = g'; Body = b }]))))
                (Ok [])
            |> Result.map (fun cs' -> inheritSpan e (ExprMatch (s', cs'))))
    // Recursive array (`let rec q: T = match q with ...`): the seed and inductive slices are ordinary expressions and may
    // contain qualified ops; without this arm they fell through unrewritten and reached the checker as an unbound variable.
    | ExprKind.ExprRecArray def ->
        rOpt (def.SeedArm |> Option.map snd) |> Result.bind (fun seedE ->
        rOpt def.Guard |> Result.bind (fun guardE ->
        r def.SliceExpr |> Result.map (fun slice' ->
            let seed' = Option.map2 (fun (sv, _) se -> (sv, se)) def.SeedArm seedE
            inheritSpan e (ExprRecArray { def with SeedArm = seed'; SliceExpr = slice'; Guard = guardE }))))
    // The rest of the expression algebra: every constructor holding a sub-expression is walked, and the catch-all wildcard
    // is deliberately GONE, so an unhandled case is an FS0025 build warning rather than a qualified call surviving unrewritten.
    | ExprKind.ExprCompute inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprCompute i))
    | ExprKind.ExprRead inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprRead i))
    | ExprKind.ExprPure inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprPure i))
    | ExprKind.ExprStatic inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprStatic i))
    | ExprKind.ExprRank inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprRank i))
    | ExprKind.ExprExtents inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprExtents i))
    | ExprKind.ExprUnique inner -> r inner |> Result.map (fun i -> inheritSpan e (ExprUnique i))
    | ExprKind.ExprObjectFor k -> r k |> Result.map (fun k' -> inheritSpan e (ExprObjectFor k'))
    | ExprKind.ExprReynolds (k, anti) -> r k |> Result.map (fun k' -> inheritSpan e (ExprReynolds (k', anti)))
    | ExprKind.ExprField (obj, fld) -> r obj |> Result.map (fun o -> inheritSpan e (ExprField (o, fld)))
    | ExprKind.ExprPartialApp (op, inner, isLeft) -> r inner |> Result.map (fun i -> inheritSpan e (ExprPartialApp (op, i, isLeft)))
    | ExprKind.ExprTranspose (a, d1, d2) -> r a |> Result.map (fun a' -> inheritSpan e (ExprTranspose (a', d1, d2)))
    | ExprKind.ExprDecompact (a, d) -> r a |> Result.map (fun a' -> inheritSpan e (ExprDecompact (a', d)))
    | ExprKind.ExprHalo (t, offs) -> r offs |> Result.map (fun o -> inheritSpan e (ExprHalo (t, o)))
    | ExprKind.ExprMethodFor es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprMethodFor es'))
    | ExprKind.ExprZip es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprZip es'))
    | ExprKind.ExprStack es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprStack es'))
    | ExprKind.ExprSequence es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprSequence es'))
    | ExprKind.ExprGroupKeys es -> rList es |> Result.map (fun es' -> inheritSpan e (ExprGroupKeys es'))
    | ExprKind.ExprGroupBucket g -> r g |> Result.map (fun g' -> inheritSpan e (ExprGroupBucket g'))
    | ExprKind.ExprAlign (es, spec) -> rList es |> Result.map (fun es' -> inheritSpan e (ExprAlign (es', spec)))
    | ExprKind.ExprJoin (es, d) -> rList es |> Result.map (fun es' -> inheritSpan e (ExprJoin (es', d)))
    | ExprKind.ExprTupleIndex (t, i) ->
        r t |> Result.bind (fun t' -> r i |> Result.map (fun i' -> inheritSpan e (ExprTupleIndex (t', i'))))
    | ExprKind.ExprGuard (c, b) ->
        r c |> Result.bind (fun c' -> r b |> Result.map (fun b' -> inheritSpan e (ExprGuard (c', b'))))
    | ExprKind.ExprReplicate (c, b) ->
        r c |> Result.bind (fun c' -> r b |> Result.map (fun b' -> inheritSpan e (ExprReplicate (c', b'))))
    | ExprKind.ExprMask (a, p) ->
        r a |> Result.bind (fun a' -> r p |> Result.map (fun p' -> inheritSpan e (ExprMask (a', p'))))
    | ExprKind.ExprCompound (d, m) ->
        r d |> Result.bind (fun d' -> r m |> Result.map (fun m' -> inheritSpan e (ExprCompound (d', m'))))
    | ExprKind.ExprSparse (v, k) ->
        r v |> Result.bind (fun v' -> r k |> Result.map (fun k' -> inheritSpan e (ExprSparse (v', k'))))
    | ExprKind.ExprIntersect (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> inheritSpan e (ExprIntersect (a', b'))))
    | ExprKind.ExprUnion (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> inheritSpan e (ExprUnion (a', b'))))
    | ExprKind.ExprContains (a, v) ->
        r a |> Result.bind (fun a' -> r v |> Result.map (fun v' -> inheritSpan e (ExprContains (a', v'))))
    | ExprKind.ExprGroupBy (v, g) ->
        r v |> Result.bind (fun v' -> r g |> Result.map (fun g' -> inheritSpan e (ExprGroupBy (v', g'))))
    | ExprKind.ExprSort (a, k) ->
        r a |> Result.bind (fun a' -> r k |> Result.map (fun k' -> inheritSpan e (ExprSort (a', k'))))
    | ExprKind.ExprGram (l, rr) ->
        r l |> Result.bind (fun l' -> r rr |> Result.map (fun r' -> inheritSpan e (ExprGram (l', r'))))
    | ExprKind.ExprReduce (a, k, init, ax) ->
        r a |> Result.bind (fun a' ->
        r k |> Result.bind (fun k' ->
        rOpt init |> Result.map (fun init' -> inheritSpan e (ExprReduce (a', k', init', ax)))))
    | ExprKind.ExprStruct (nm, fields, spread) ->
        fields |> List.fold (fun acc (fn, fe) ->
            acc |> Result.bind (fun fs -> r fe |> Result.map (fun fe' -> fs @ [(fn, fe')])))
            (Ok [])
        |> Result.bind (fun fields' ->
        rOpt spread |> Result.map (fun spread' -> inheritSpan e (ExprStruct (nm, fields', spread'))))
    | ExprKind.ExprFor (src, cs, kern) ->
        (match src with
         | ForArrays (arrs, inClause) ->
             rList arrs |> Result.bind (fun arrs' ->
             rOpt inClause |> Result.map (fun ic' -> ForArrays (arrs', ic')))
         | ForKernel k -> r k |> Result.map ForKernel)
        |> Result.bind (fun src' ->
        rOpt kern |> Result.map (fun kern' -> inheritSpan e (ExprFor (src', cs, kern'))))
    // Leaves: no sub-expressions. Index/type arguments (range<I>, reverse<I>) carry TypeExprs, not Exprs, never rewritten.
    | ExprKind.ExprWildcard | ExprKind.ExprQualified _ | ExprKind.ExprRange _
    | ExprKind.ExprReverse _ | ExprKind.ExprArity _ | ExprKind.ExprNth
    | ExprKind.ExprZero | ExprKind.ExprSection _ -> Ok e

// Gating + program expansion

/// `import sgs [as _]` -- the module this layer owns.
let private isSgsImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["sgs"], _) -> true
    | _ -> false

let private sgsAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["sgs"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "sgs") set)
            | DeclImport (["sgs"], ImportSelective _) ->
                Error "`sgs` supports only `import sgs [as <alias>]`; a selective `from sgs import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    sgsAliasesOf decls |> Result.bind (fun aliases ->
    // Import-gated: with no `import sgs`, this pass is a strict no-op.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isSgsImport)
        match resolveStatics declsNoImport with
        | Error e -> Error $"sgs elaboration: static resolution failed: {e}"
        | Ok (statics, _) ->
            let tyAliases = collectAliases declsNoImport
            let ctx = { Arrays = collectArrays tyAliases declsNoImport
                        Funcs = collectFuncs tyAliases declsNoImport
                        Aliases = tyAliases
                        Statics = statics }
            let st = { Counter = 0; Made = Map.empty; Decls = [] }
            let mapped =
                declsNoImport |> List.fold (fun acc d ->
                    acc |> Result.bind (fun out ->
                        Blade.Ast.synthSpan <- d.Span
                        let mapped =
                            match d.Value with
                            | DeclFunction fd ->
                                // Annotated array PARAMETERS are shape witnesses inside this body -- a galilean-certified
                                // function applies the formers to its own velocity parameter.
                                let pscope = paramShapes tyAliases (fd.Params |> List.map (fun p -> (p.Name, p.Type)))
                                rewriteExpr st ctx aliases pscope fd.Body
                                |> Result.map (fun b -> DeclFunction { fd with Body = b })
                            | DeclLet binding ->
                                rewriteExpr st ctx aliases Map.empty binding.Value
                                |> Result.map (fun v' -> DeclLet { binding with Value = v' })
                            | DeclStatic binding ->
                                rewriteExpr st ctx aliases Map.empty binding.Value
                                |> Result.map (fun v' -> DeclStatic { binding with Value = v' })
                            | other -> Ok other
                        mapped |> Result.map (fun value -> out @ [{ d with Value = value }])))
                    (Ok [])
            mapped |> Result.map (fun decls' ->
                if st.Decls.IsEmpty then decls'
                else
                    let span = { StartLine = 0; StartCol = 0; EndLine = 0; EndCol = 0; File = None }
                    let gen = st.Decls |> List.map (fun fd -> { Value = DeclFunction fd; Span = span })
                    gen @ decls'))

let private expandStr (program: Program) : Result<Program, string> =
    program.Modules
    |> List.fold (fun acc m ->
        acc |> Result.bind (fun ms ->
            expandModule m.Decls |> Result.map (fun ds -> ms @ [{ m with Decls = ds }])))
        (Ok [])
    |> Result.map (fun ms -> { program with Modules = ms })

/// Boundary: string-errored internals -> coded diagnostics (BL5600).
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun msg ->
        [ Blade.Diagnostics.mkError "BL5600" (Blade.Diagnostics.Codes.phaseOfCode "BL5600") Blade.Ast.synthSpan msg ])
