// Route A: unrolling arity-polymorphic pack kernels at the AD apply site,
// plus grouped-peel lowering (which consumes the unrolled shapes).
module Blade.GradPackUnroll

open Blade.Ast
open Blade.GradCommon
open Blade.GradExpand
open Blade.GradFusion

// ---------------------------------------------------------------------------
// Route A: unrolling an arity-polymorphic pack kernel at the AD apply site
// ---------------------------------------------------------------------------
//
// A `Poly<T^k>` kernel has ONE formal parameter standing for n operands, and
// a body that is a `match arity(a) with ...` block -- two things the map rule
// refuses outright (arity mismatch, block body). But the arity is already
// known where it matters: `object_for(pk) <@> (A, A)` says 2 before anything
// is typechecked. So the kernel is EXPANDED here, on the surface AST, into
// the fixed-arity inline lambda the user could have written by hand, and the
// ordinary map rules differentiate that.
//
// This is the surface twin of `specializeFunction` (src/IR.fs), which does
// the same job post-typecheck for the primal: pack views tracked as
// (slot, offset), `arity(...)` folded to a literal, `a[k]` resolved to the
// k-th expanded parameter, the recursive call re-entered with the tail view,
// and the `match arity` reduced to its one live arm. The two are deliberately
// separate: the primal's runs after typecheck on IR, this one runs before it
// on `Expr`, and neither can see the other's representation.
//
// Emitting an inline LAMBDA rather than minting a monomorphized declaration
// is what keeps this local: no name to collide, nothing added to the module,
// and the spelling (`lambda(x, y) where comm(x, y) -> ...`) is one the corpus
// already proves end to end.

/// What an arity-poly pack name currently VIEWS: the expanded parameter
/// names still in the pack, in order. `let head :: tail = a` gives `tail` a
/// view one shorter -- the same information `specializeFunction`'s
/// `aliasInfo` carries as an offset into one flat parameter run.
type internal PackEnv = Map<string, string list>

/// Total arms the unroller may expand across one kernel. Arity is the
/// natural bound (each step shortens the view by at least one), so this is
/// only a backstop against a kernel that recurses on an UNCHANGED view --
/// generous on purpose: a real pack kernel at a realistic arity expands a
/// handful of arms.
let internal packUnrollBudget = 256

/// The unroller's own refusal channel (BL5502). A pack kernel whose
/// recursion never reaches a base arm is not a grad or a jvp modelling limit
/// -- it is a property of the KERNEL, true in both modes and at that arity
/// only -- so it gets its own code rather than being folded into the generic
/// BL5500/BL5501 elaboration errors. The marker is a message prefix the
/// `expand` boundary strips: `err`'s mode prefix is what selects
/// BL5500/BL5501, and this one has to outrank it.
let internal packUnrollMarker = "[BL5502] "

let internal errPackUnroll (fname: string) (msg: string) : Result<'a, string> =
    Error $"{packUnrollMarker}{errMode.Value}({fname}): {msg}"

/// `Poly<inner>` after alias resolution -- the ELEMENT type each expanded
/// parameter carries. `Poly<T^1>` yields `T^1`, which is what makes the
/// unrolled lambda's parameters RANK-CARRYING (C8): the comoment kernels
/// take rank-1 fibers, and losing the annotation would silently turn a row
/// map into an element map.
let internal polyElemTy (ctx: Ctx) (t: TypeExpr option) : TypeExpr option =
    match t |> Option.map (resolveTy ctx) with
    | Some (TyPoly inner) -> Some inner
    | _ -> None

let internal isPackDecl (ctx: Ctx) (fd: FunctionDecl) : bool =
    fd.Params |> List.exists (fun p -> (polyElemTy ctx p.Type).IsSome)

/// `arity(p)` in an EXPRESSION inside a type argument (`Idx<arity(args)>`).
/// Folded to the literal wherever the shape is mechanical; `unresolved`
/// collects the pack names a shape this does not know about left behind, so
/// the caller refuses instead of emitting a lambda whose type mentions a
/// parameter that no longer exists.
let rec internal substArityInE (resolve: Ident -> int option) (unresolved: ResizeArray<string>) (e: Expr) : Expr =
    let go = substArityInE resolve unresolved
    let re k = inheritSpan e k
    match e.Kind with
    | ExprKind.ExprArity a ->
        (match resolve a with
         | Some n -> re (ExprLit (LitInt (int64 n)))
         | None -> e)
    | ExprKind.ExprBinOp (m, op, l, r) -> re (ExprBinOp (m, op, go l, go r))
    | ExprKind.ExprUnaryOp (op, i) -> re (ExprUnaryOp (op, go i))
    | ExprKind.ExprApp (f, args) -> re (ExprApp (go f, args |> List.map go))
    | ExprKind.ExprTuple es -> re (ExprTuple (es |> List.map go))
    | ExprKind.ExprArrayLit es -> re (ExprArrayLit (es |> List.map go))
    | ExprKind.ExprTupleIndex (t, i) -> re (ExprTupleIndex (go t, go i))
    | ExprKind.ExprStatic i -> re (ExprStatic (go i))
    | ExprKind.ExprTyped (i, t) -> re (ExprTyped (go i, t))
    | ExprKind.ExprLit _ | ExprKind.ExprVar _ | ExprKind.ExprWildcard
    | ExprKind.ExprQualified _ | ExprKind.ExprNth | ExprKind.ExprZero
    | ExprKind.ExprSection _ -> e
    | _ ->
        // A shape with no arm here cannot be walked, so any `arity` it hides
        // would survive silently. Record the fact and let the caller refuse.
        unresolved.Add "<unwalkable extent expression>"
        e

/// The same fold over a TYPE. EXHAUSTIVE over `TypeExpr` on purpose (the
/// `mentionsDeep` precedent): a case with no arm would drop an `arity` on
/// the floor, and a new grammar node should surface as an incomplete-match
/// warning rather than as a silently unresolved extent.
let rec internal substArityInTy (resolve: Ident -> int option) (unresolved: ResizeArray<string>) (t: TypeExpr) : TypeExpr =
    let ty = substArityInTy resolve unresolved
    let ex = substArityInE resolve unresolved
    let exO (o: Expr option) = o |> Option.map ex
    let sb (b: SymIdxBase) =
        match b with
        | SymBaseExtent e -> SymBaseExtent (ex e)
        | SymBaseIndex it -> SymBaseIndex (ty it)
    match t with
    | TyInt32 | TyInt64 | TyFloat32 | TyFloat64 | TyComplex64 | TyComplex128
    | TyBool | TyString | TyChar | TyUnit | TyTupleWidth _ | TyVar _
    | TyRaggedIdxOpaque | TyWildcard | TyUnitExpr _ -> t
    | TyNamed (n, args) -> TyNamed (n, args |> List.map ty)
    | TyBounded (b, lo, hi) -> TyBounded (ty b, exO lo, exO hi)
    | TyArray (el, its) -> TyArray (ty el, its |> List.map ty)
    | TyDist (o, el, axes) -> TyDist (ex o, ty el, axes |> List.map ty)
    | TyAbstractArray (el, r, sym) -> TyAbstractArray (ty el, ex r, sym)
    | TyFunc (args, ret) -> TyFunc (args |> List.map ty, ty ret)
    | TyTuple ts -> TyTuple (ts |> List.map ty)
    | TyIdx e -> TyIdx (ex e)
    | TySymIdx (r, b) -> TySymIdx (r, sb b)
    | TyAntisymIdx (r, b) -> TyAntisymIdx (r, sb b)
    | TyOrbIdx (levels, b) -> TyOrbIdx (levels, sb b)
    | TyBoundedIdx (lo, hi) -> TyBoundedIdx (ex lo, ex hi)
    | TyCompoundIdx m -> TyCompoundIdx (ex m)
    | TyChunked (t, s) -> TyChunked (ty t, ex s)
    | TySparseIdx k -> TySparseIdx (ex k)
    | TyEquivIdx (d, g, r) -> TyEquivIdx (ex d, ty g, ty r)
    | TyHermitianIdx e -> TyHermitianIdx (ex e)
    | TyEnumIdx e -> TyEnumIdx (ex e)
    | TyDepIdx (outer, p, body) -> TyDepIdx (ty outer, p, ty body)
    | TyRaggedIdx e -> TyRaggedIdx (ex e)
    | TyIrrepsIdx e -> TyIrrepsIdx (ex e)
    | TyPgIrrepsIdx (g, e) -> TyPgIrrepsIdx (g, ex e)
    | TyHalo (inner, offs) -> TyHalo (ty inner, ex offs)
    | TyConstrained (b, cs) -> TyConstrained (ty b, cs)
    | TyPoly inner -> TyPoly (ty inner)

/// Instance 2 of `mapWhereVars`: a `where` clause written over the PACK,
/// re-read over its expanded parameters -- `comm(a)` on `a: Poly<T^0>`
/// applied to two operands becomes `comm(a_0, a_1)`. One name in, n out.
///
/// Load-bearing, not cosmetic. The C5 symmetric-tangent gate accepts a comm
/// group only when it covers the FULL parameter-name set of the kernel it is
/// differentiating; a clause left naming the vanished pack covers nothing,
/// the gate silently declines, and the tangent falls to the dense path --
/// the r! storage saving lost with no diagnostic. Expanding the group is
/// what keeps `object_for(packprod) <@> (A, A)` triangular on BOTH legs.
let internal expandWhereForPack (packName: string) (names: string list) (w: WhereClause) : WhereClause =
    mapWhereVars (fun g -> if g = packName then names else [g]) w

/// Does an int pattern select arity `m`? Guarded arms never do -- a guard is
/// runtime data, and picking an arm on one would be a guess.
let internal packArmMatches (m: int) (p: Pattern) : bool =
    match p.Kind with
    | PatternKind.PatLit (LitInt k) -> int k = m
    | PatternKind.PatWildcard | PatternKind.PatVar _ -> true
    | _ -> false

/// Bind a `let <pat> = <pack view>` destructuring. Returns the element
/// substitutions (`head := <the k-th expanded parameter>`) and the new pack
/// aliases (`tail := the shorter view`).
let rec internal bindPackPattern (fname: string) (at: Expr) (view: string list) (p: Pattern)
    : Result<(string * Expr) list * (string * string list) list, string> =
    match p.Kind with
    | PatternKind.PatWildcard -> Ok ([], [])
    | PatternKind.PatVar x -> Ok ([], [(x, view)])
    | PatternKind.PatCons (h, t) ->
        (match view with
         | [] ->
             errPackUnroll fname "an arity-polymorphic kernel destructures a pack that is already EMPTY at this arity, so the recursion has run past its base case. Add a base arm (`| 0 -> ...` or `| 1 -> ...`) that stops before the pack is exhausted"
         | first :: rest ->
             let headSub =
                 match h.Kind with
                 | PatternKind.PatWildcard -> Ok []
                 | PatternKind.PatVar hn -> Ok [(hn, inheritSpan at (ExprVar first))]
                 | _ -> err fname "an arity-polymorphic kernel's `head :: tail` destructuring supports plain names for the peeled heads (v1)"
             headSub |> Result.bind (fun hs ->
                 bindPackPattern fname at rest t
                 |> Result.map (fun (ts, tls) -> (hs @ ts, tls))))
    | _ -> err fname "an arity-polymorphic kernel's pack destructuring supports `head :: tail` and a plain name (v1)"

/// Unroll one arity-poly function at a concrete pack VIEW, inlining every
/// call it makes to a pack kernel (including itself). `budget` bounds the
/// total arms expanded across the whole tree.
let rec internal unrollPackFn (ctx: Ctx) (fname: string) (budget: int ref)
                             (fd: FunctionDecl) (view: string list) : Result<Expr, string> =
    match fd.Params with
    | [p] when (polyElemTy ctx p.Type).IsSome ->
        if budget.Value <= 0 then
            errPackUnroll fname $"unrolling the arity-polymorphic kernel '{fd.Name}' exceeded the {packUnrollBudget}-arm budget: the recursion is not shrinking its pack, so it has no base case at this arity"
        else
            budget.Value <- budget.Value - 1
            unrollPackExpr ctx fname budget (Map.ofList [(p.Name, view)]) fd.Body
    | _ ->
        // Multi-pack (or a pack beside free parameters). A map apply site
        // presents ONE flat operand list, which says nothing about how the
        // operands divide between two `Poly<...>` slots (the primal reads
        // that split off a tuple-per-slot call shape, which `<@>` has no
        // spelling for), and a free parameter has no operand to fill it at
        // all. Refused by name rather than guessed.
        err fname $"differentiating an arity-polymorphic kernel supports exactly ONE `Poly<...>` pack parameter and nothing beside it (v1); '{fd.Name}' declares {fd.Params.Length} parameter(s), and a `<@>` operand list carries no way to say which operands fill which pack"

/// The body walk. Every form that can carry a pack occurrence has an arm;
/// the catch-all passes a pack-free subtree through untouched and REFUSES
/// one that still mentions a pack, so no pack reference is ever left behind
/// for typecheck to trip over as a dangling name.
and internal unrollPackExpr (ctx: Ctx) (fname: string) (budget: int ref)
                           (env: PackEnv) (e: Expr) : Result<Expr, string> =
    let go = unrollPackExpr ctx fname budget env
    let re k = inheritSpan e k
    let packNames = env |> Map.toList |> List.map fst |> Set.ofList
    let g1 mk a = go a |> Result.map (fun a' -> re (mk a'))
    let g2 mk a b = go a |> Result.bind (fun a' -> go b |> Result.map (fun b' -> re (mk a' b')))
    let gList (xs: Expr list) = traverseR go xs
    let gOpt (x: Expr option) =
        match x with
        | None -> Ok None
        | Some a -> go a |> Result.map Some
    // Fold `arity(...)` inside a type argument, refusing anything the fold
    // could not reach (design: substitute where mechanical, else refuse --
    // never leave an `arity` pointing at a parameter that is about to stop
    // existing).
    let subTy (t: TypeExpr) : Result<TypeExpr, string> =
        let unresolved = ResizeArray<string>()
        let resolve (a: Ident) = env |> Map.tryFind a |> Option.map List.length
        let t' = substArityInTy resolve unresolved t
        if unresolved.Count = 0 then Ok t'
        else err fname "an arity-polymorphic kernel uses `arity(...)` inside a type argument whose extent expression this pass cannot fold to a literal; write the extent as a literal, a `let static`, or plain arithmetic over `arity(...)`"
    let subTys (ts: TypeExpr list) : Result<TypeExpr list, string> = traverseR subTy ts
    // Unroll a block/let chain one binding at a time: the REST is unrolled
    // first (so the tail view is in scope for it) and the peeled heads are
    // substituted into the result afterwards, which is what turns a pack
    // destructuring into plain parameter reads with no binder left over.
    let rec goBlock (env: PackEnv) (stmts: Stmt list) (final: Expr option) : Result<Expr, string> =
        match stmts with
        | [] ->
            (match final with
             | Some f -> unrollPackExpr ctx fname budget env f
             | None -> err fname "an arity-polymorphic kernel arm has no result expression")
        | s :: rest ->
            (match unwrapStmt s with
             | StmtLet ({ Mutability = BindLet; Type = None } as b) ->
                 let viewOf =
                     match b.Value.Kind with
                     | ExprKind.ExprVar a -> Map.tryFind a env
                     | _ -> None
                 (match viewOf with
                  | Some view ->
                      bindPackPattern fname b.Value view b.Pattern
                      |> Result.bind (fun (subs, tails) ->
                          let env' = tails |> List.fold (fun m (nm, vw) -> Map.add nm vw m) env
                          goBlock env' rest final
                          |> Result.bind (substParamMany fname subs))
                  | None ->
                      // An ordinary local: inlined into the rest, because the
                      // differentiated form is an EXPRESSION and cannot keep a
                      // statement. `substParam` refuses rather than crossing a
                      // binder it cannot prove safe.
                      (match b.Pattern.Kind with
                       | PatternKind.PatVar x ->
                           unrollPackExpr ctx fname budget env b.Value |> Result.bind (fun v' ->
                               goBlock env rest final |> Result.bind (substParam fname x v'))
                       | _ -> err fname "an arity-polymorphic kernel's local bindings must bind a plain name (v1)"))
             | _ ->
                 err fname "an arity-polymorphic kernel body supports `let` bindings only; assignments, `for` statements and bare expression statements have no expression form to differentiate")
    match e.Kind with
    // -- the pack vocabulary -------------------------------------------------
    | ExprKind.ExprArity a when Map.containsKey a env ->
        Ok (re (ExprLit (LitInt (int64 (List.length env.[a])))))
    | ExprKind.ExprTupleIndex (t, ix) when
        (match t.Kind with ExprKind.ExprVar a -> Map.containsKey a env | _ -> false) ->
        let view = env.[(match t.Kind with ExprKind.ExprVar a -> a | _ -> "")]
        (match ix.Kind with
         | ExprKind.ExprLit (LitInt k) when int k >= 0 && int k < List.length view ->
             Ok (re (ExprVar view.[int k]))
         | ExprKind.ExprLit (LitInt k) ->
             errPackUnroll fname $"an arity-polymorphic kernel reads element {int k} of a pack that has only {List.length view} element(s) at this arity"
         | _ ->
             err fname "an arity-polymorphic kernel reads its pack at a NON-LITERAL index (`a[k]` under a former over `range<Idx<arity(a)>>`); the differentiated form has one parameter per element, so the subscript has to be a literal. Write the fold as a `head :: tail` recursion")
    | ExprKind.ExprMatch (scrut, cases) ->
        go scrut |> Result.bind (fun scrut' ->
            match scrut'.Kind with
            | ExprKind.ExprLit (LitInt m) ->
                (match cases |> List.tryFind (fun c -> c.Guard.IsNone && packArmMatches (int m) c.Pattern) with
                 | Some c ->
                     unrollPackExpr ctx fname budget env c.Body
                     |> Result.bind (fun b ->
                         match c.Pattern.Kind with
                         | PatternKind.PatVar x -> substParam fname x (re (ExprLit (LitInt m))) b
                         | _ -> Ok b)
                 | None ->
                     errPackUnroll fname $"an arity-polymorphic kernel has NO arm for arity {int m}: `match arity(...)` reached that arity through its own recursion and found no guard-free case. Add a base arm for it (`| {int m} -> ...`) or a catch-all")
            | _ ->
                err fname "`match` inside a differentiated kernel is supported only as `match arity(<pack>)`, whose arm is chosen at the apply site's arity")
    | ExprKind.ExprBlock (stmts, final) -> goBlock env stmts final
    | ExprKind.ExprLet (b, body) ->
        goBlock env [StmtLet b] (Some body)
    // -- a call: to another pack kernel (inline its unrolled body) or plain --
    | ExprKind.ExprApp (fn, args) ->
        (match fn.Kind with
         | ExprKind.ExprVar g when Map.containsKey g ctx.Decls && isPackDecl ctx ctx.Decls.[g] ->
             (match args with
              | [{ Kind = ExprKind.ExprVar a }] when Map.containsKey a env ->
                  unrollPackFn ctx fname budget ctx.Decls.[g] env.[a]
              | _ ->
                  err fname $"a call to the arity-polymorphic kernel '{g}' inside a differentiated kernel body must pass one whole pack (`{g}(tail)`); an element or expression argument list has no arity to unroll at")
         | _ -> gList args |> Result.map (fun args' -> re (ExprApp (fn, args'))))
    // -- type-carrying forms -------------------------------------------------
    | ExprKind.ExprRange tys -> subTys tys |> Result.map (fun ts -> re (ExprRange ts))
    | ExprKind.ExprTyped (i, t) ->
        go i |> Result.bind (fun i' -> subTy t |> Result.map (fun t' -> re (ExprTyped (i', t'))))
    | ExprKind.ExprBlocked (t, x) ->
        go x |> Result.bind (fun x' -> subTy t |> Result.map (fun t' -> re (ExprBlocked (t', x'))))
    | ExprKind.ExprHalo (t, offs) ->
        go offs |> Result.bind (fun o' -> subTy t |> Result.map (fun t' -> re (ExprHalo (t', o'))))
    // -- ordinary structure --------------------------------------------------
    | ExprKind.ExprBinOp (m, op, l, r) -> g2 (fun a b -> ExprBinOp (m, op, a, b)) l r
    | ExprKind.ExprUnaryOp (op, i) -> g1 (fun a -> ExprUnaryOp (op, a)) i
    | ExprKind.ExprIf (c, t, f) ->
        go c |> Result.bind (fun c' -> go t |> Result.bind (fun t' -> go f |> Result.map (fun f' -> re (ExprIf (c', t', f')))))
    | ExprKind.ExprTuple es -> gList es |> Result.map (fun es' -> re (ExprTuple es'))
    | ExprKind.ExprArrayLit es -> gList es |> Result.map (fun es' -> re (ExprArrayLit es'))
    | ExprKind.ExprStack es -> gList es |> Result.map (fun es' -> re (ExprStack es'))
    | ExprKind.ExprZip es -> gList es |> Result.map (fun es' -> re (ExprZip es'))
    | ExprKind.ExprSequence es -> gList es |> Result.map (fun es' -> re (ExprSequence es'))
    | ExprKind.ExprMethodFor es -> gList es |> Result.map (fun es' -> re (ExprMethodFor es'))
    | ExprKind.ExprJoin (es, d) -> gList es |> Result.map (fun es' -> re (ExprJoin (es', d)))
    | ExprKind.ExprAlign (es, sp) -> gList es |> Result.map (fun es' -> re (ExprAlign (es', sp)))
    | ExprKind.ExprObjectFor k -> g1 (fun a -> ExprObjectFor a) k
    // A nested lambda (the kernel of a pack FORMER, `method_for(range<Idx<
    // arity(a)>>) <@> lambda(k) -> a[k]`). Descending is safe: everything
    // this pass writes into a body is either a literal or one of the fresh
    // `__pk` parameter names, which no user binder can shadow. The point of
    // descending is the DIAGNOSTIC -- the former's `a[k]` then refuses at the
    // dynamic-index arm, which says what to write instead, rather than at the
    // catch-all, which only says "somewhere in here".
    | ExprKind.ExprLambda (lps, lwc, lb) when
        not (lps |> List.exists (fun p -> Map.containsKey p.Name env)) ->
        go lb |> Result.map (fun lb' -> re (ExprLambda (lps, lwc, lb')))
    | ExprKind.ExprTupleIndex (t, i) -> g2 (fun a b -> ExprTupleIndex (a, b)) t i
    | ExprKind.ExprField (x, f) -> g1 (fun a -> ExprField (a, f)) x
    | ExprKind.ExprPure i -> g1 (fun a -> ExprPure a) i
    | ExprKind.ExprCompute i -> g1 (fun a -> ExprCompute a) i
    | ExprKind.ExprRead i -> g1 (fun a -> ExprRead a) i
    | ExprKind.ExprStatic i -> g1 (fun a -> ExprStatic a) i
    | ExprKind.ExprRank i -> g1 (fun a -> ExprRank a) i
    | ExprKind.ExprExtents i -> g1 (fun a -> ExprExtents a) i
    | ExprKind.ExprUnique i -> g1 (fun a -> ExprUnique a) i
    | ExprKind.ExprTranspose (a, d1, d2) -> g1 (fun x -> ExprTranspose (x, d1, d2)) a
    | ExprKind.ExprDecompact (a, d) -> g1 (fun x -> ExprDecompact (x, d)) a
    | ExprKind.ExprPartialApp (op, x, l) -> g1 (fun a -> ExprPartialApp (op, a, l)) x
    | ExprKind.ExprReynolds (k, anti) -> g1 (fun a -> ExprReynolds (a, anti)) k
    | ExprKind.ExprGram (l, r) -> g2 (fun a b -> ExprGram (a, b)) l r
    | ExprKind.ExprGuard (c, b) -> g2 (fun a b2 -> ExprGuard (a, b2)) c b
    | ExprKind.ExprReplicate (c, b) -> g2 (fun a b2 -> ExprReplicate (a, b2)) c b
    | ExprKind.ExprMask (a, p) -> g2 (fun x y -> ExprMask (x, y)) a p
    | ExprKind.ExprCompound (d, m) -> g2 (fun x y -> ExprCompound (x, y)) d m
    | ExprKind.ExprDotDot (l, h) -> g2 (fun a b -> ExprDotDot (a, b)) l h
    | ExprKind.ExprReduce (a, k, init, axes) ->
        go a |> Result.bind (fun a' ->
        go k |> Result.bind (fun k' ->
        gOpt init |> Result.bind (fun i' ->
        gOpt axes |> Result.map (fun x' -> re (ExprReduce (a', k', i', x'))))))
    | ExprKind.ExprLit _ | ExprKind.ExprVar _ | ExprKind.ExprWildcard
    | ExprKind.ExprQualified _ | ExprKind.ExprReverse _ | ExprKind.ExprNth
    | ExprKind.ExprZero | ExprKind.ExprSection _ | ExprKind.ExprArity _
        when not (mentionsDeep packNames e) -> Ok e
    | _ ->
        if mentionsDeep packNames e then
            err fname "an arity-polymorphic pack may only be indexed (`a[k]`), destructured (`let h :: t = a`), measured (`arity(a)`) or passed whole to another pack kernel; this kernel body uses it in a position the unroller cannot expand"
        else Ok e

/// The canonical slot names a MEMOIZED unroll is built over. `#` is not a
/// Blade identifier character, so a slot can collide with nothing a user
/// wrote, and each apply site renames the slots to its own fresh parameters.
let internal packSlot (i: int) : string = $"__pk#{i}"

/// The unrolled body at one arity, over canonical slots -- memoized per
/// (kernel, arity), because a body applying the same pack kernel at the same
/// arity twice used to unroll it twice, and an unroll expands one arm per
/// element.
///
/// Caching the SHAPE rather than a finished lambda is what keeps the
/// synthesized names identical to the unmemoized ones: every apply site still
/// mints its own `__pk<N>` run off the shared fresh counter, in the same
/// order, and only the RENAME of the slots onto them is new. Nothing in the
/// unroll branches on a slot's text, so the renamed shape is exactly the body
/// an unroll over those names would have built.
///
/// Only successes are cached: a refusal ends the synthesis, and its message
/// carries the top-level function name and the mode, neither of which is in
/// the key. (`Decls` is not either -- hence a per-request table, as for
/// `NormMemo`.)
let internal unrollPackShape (ctx: Ctx) (fname: string) (fd: FunctionDecl) (n: int)
    : Result<Expr, string> =
    match ctx.PackMemo.TryGetValue ((fd.Name, n)) with
    | true, hit -> Ok hit
    | _ ->
        unrollPackFn ctx fname (ref packUnrollBudget) fd (List.init n packSlot)
        |> Result.map (fun b -> ctx.PackMemo.[(fd.Name, n)] <- b; b)

/// The shape at `names`. A rename refusal is not a real outcome here (a slot
/// name cannot be captured by anything a user could write), but rather than
/// invent a diagnostic for the impossible case, fall back to unrolling
/// directly at those names -- which is what this did before the memo.
let internal unrollPackAt (ctx: Ctx) (fname: string) (fd: FunctionDecl) (names: string list)
    : Result<Expr, string> =
    unrollPackShape ctx fname fd names.Length
    |> Result.bind (fun shape ->
        let ren = names |> List.mapi (fun i nm -> (packSlot i, nm)) |> Map.ofList
        match renameExpr ren shape with
        | Ok b -> Ok b
        | Error _ -> unrollPackFn ctx fname (ref packUnrollBudget) fd names)

/// Route A entry point: if `kern` names an arity-polymorphic pack function,
/// unroll it at the apply site's operand count into the fixed-arity inline
/// lambda the ordinary map rules already differentiate. `None` means "not a
/// pack kernel" -- every non-pack kernel takes exactly the path it did
/// before, which is what keeps the primal arity machinery untouched (this
/// runs on the AD side only, and only for a `Poly<...>` declaration).
let internal tryUnrollPackKernel (ctx: Ctx) (fname: string) (kern: Expr) (n: int)
    : Result<Expr, string> option =
    match kern.Kind with
    | ExprKind.ExprVar f when Map.containsKey f ctx.Decls && isPackDecl ctx ctx.Decls.[f] ->
        let fd = ctx.Decls.[f]
        let elemTy = fd.Params |> List.tryPick (fun p -> polyElemTy ctx p.Type)
        let names = List.init n (fun _ -> fresh ctx "__pk")
        let packName = fd.Params |> List.tryPick (fun p -> if (polyElemTy ctx p.Type).IsSome then Some p.Name else None)
        Some (
            unrollPackAt ctx fname fd names
            |> Result.map (fun body ->
                let ps =
                    names |> List.map (fun nm ->
                        { Name = nm; Type = elemTy; Default = None; NameSpan = noSpan })
                let wc =
                    match packName, fd.WhereClause with
                    | Some pn, Some w -> Some (expandWhereForPack pn names w)
                    | _, w -> w
                inheritSpan fd.Body (ExprLambda (ps, wc, body))))
    | _ -> None

/// The same expansion met as a CALL inside a kernel body
/// (`lambda(x, y) -> comoment(x, y)`, the docs' covariance spelling): unroll
/// the callee at the call's argument count, then substitute the arguments
/// for the unrolled parameters. The callee's `where` clause does not travel
/// -- the enclosing lambda's is the one the map rule reads -- which matches
/// what the primal does with the same spelling.
let internal tryUnrollPackCall (ctx: Ctx) (fname: string) (fd: FunctionDecl) (args: Expr list)
    : Result<Expr, string> option =
    if not (isPackDecl ctx fd) then None
    else
        let names = List.init args.Length (fun _ -> fresh ctx "__pc")
        Some (
            unrollPackAt ctx fname fd names
            |> Result.bind (substParamMany fname (List.zip names args)))

/// Rewrite a statement's VALUE position, keeping its span wrapper: the
/// statements this pass leaves alone still have to carry their locations into
/// everyone else's diagnostics.
let rec internal mapStmtValue (f: Expr -> Expr) (s: Stmt) : Stmt =
    match s with
    | StmtSpanned (inner, sp) -> StmtSpanned (mapStmtValue f inner, sp)
    | StmtLet b -> StmtLet { b with Value = f b.Value }
    | StmtExpr ex -> StmtExpr (f ex)
    | StmtAssign (l, op, r) -> StmtAssign (l, op, f r)
    | other -> other

/// The empty-group question, decided from the key array's ELEMENT type.
/// `Int64` (annotated or an unannotated int-literal table) is dynamic
/// discovery; an `Idx<N>` / `EnumIdx` element is a POSITIONAL key space with
/// N slots, some of which may take no rows. Anything unresolved is
/// `GRUnknown` and refuses, because guessing "dynamic" for a positional key
/// space is exactly the direction that would silently NaN an empty mean.
let internal groupRegimeOf (ctx: Ctx) (fd: FunctionDecl) (letTys: Map<string, TypeExpr>)
                          (keys: Expr list) : GroupRegime =
    // Several keys hash to a compound key: discovered, never empty.
    if List.length keys <> 1 then GRDynamic else
    match (stripTypedE (List.head keys)).Kind with
    | ExprKind.ExprVar kn ->
        let annot =
            match fd.Params |> List.tryPick (fun p -> if p.Name = kn then p.Type else None) with
            | Some t -> Some t
            | None -> Map.tryFind kn letTys
        match annot |> Option.map (resolveArrayTy ctx) with
        | Some (TyArray (elem, _)) ->
            (match resolveTy ctx elem with
             | TyInt64 | TyNamed (("Int" | "Int32" | "Int64"), []) -> GRDynamic
             | TyIdx { Kind = ExprKind.ExprLit (LitInt n) } -> GRStatic (int n)
             | TyEnumIdx { Kind = ExprKind.ExprArrayLit es } -> GRStatic es.Length
             | _ -> GRUnknown)
        | _ ->
            // No annotation anywhere: an int-LITERAL key table is the
            // unannotated-Int64 case, which is dynamic discovery.
            let litInts (x: Expr) =
                match x.Kind with
                | ExprKind.ExprArrayLit es ->
                    not es.IsEmpty
                    && es |> List.forall (fun el ->
                        match (stripTypedE el).Kind with
                        | ExprKind.ExprLit (LitInt _) -> true
                        | ExprKind.ExprUnaryOp (OpNeg, { Kind = ExprKind.ExprLit (LitInt _) }) -> true
                        | _ -> false)
                | _ -> false
            match ctx.ModuleLets |> Map.tryFind kn |> Option.map _.Value with
            | Some mv when litInts mv -> GRDynamic
            | _ -> GRUnknown
    | _ -> GRUnknown

/// The rewrite itself. Returns the (possibly unchanged) body plus the
/// refusals it wants the caller to raise -- routed through the caller's
/// `err`, so the expand boundary stamps BL5500 / BL5501 by MODE rather than
/// mislabelling a jvp refusal as a grad one.
let internal lowerGroupedPeels (ctx: Ctx) (fd: FunctionDecl) (body: Expr) : Expr * string list =
    let stmts0, finalOpt =
        match body.Kind with
        | ExprKind.ExprBlock (ss, fe) -> ss, fe
        | _ -> [], Some body
    // Annotated locals, for the source extent and the key-regime read.
    let letTys =
        stmts0 |> List.fold (fun m s ->
            match unwrapStmt s with
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }; Type = Some t } -> Map.add nm t m
            | _ -> m) (moduleLetTys ctx)
    // `let gk = group_keys(k...)`, body-local bindings shadowing module ones.
    let groupings =
        let mods =
            ctx.ModuleLets |> Map.toSeq
            |> Seq.choose (fun (nm, ml) ->
                match ml.Value.Kind with ExprKind.ExprGroupKeys ks -> Some (nm, ks) | _ -> None)
            |> Map.ofSeq
        stmts0 |> List.fold (fun m s ->
            match unwrapStmt s with
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }
                        Value = { Kind = ExprKind.ExprGroupKeys ks } } -> Map.add nm ks m
            | _ -> m) mods
    // `let g = group_by(V, gk)` over one of them, V a plain array name.
    let groupedOf =
        stmts0 |> List.fold (fun m s ->
            match unwrapStmt s with
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm }
                        Value = { Kind = ExprKind.ExprGroupBy (vals, gkE) } } ->
                (match (stripTypedE vals).Kind, (stripTypedE gkE).Kind with
                 | ExprKind.ExprVar vn, ExprKind.ExprVar gkn when Map.containsKey gkn groupings ->
                     Map.add nm (vn, gkn) m
                 | _ -> m)
            | _ -> m) Map.empty
    // The FIRST peel over one of those grouped values. v1 lowers one per body.
    let peelHit =
        stmts0 |> List.tryPick (fun s ->
            match unwrapStmt s with
            | StmtLet { Pattern = { Kind = PatternKind.PatVar mn }; Value = pv } ->
                (match peelOverNamed pv with
                 | Some (gn, kern) when Map.containsKey gn groupedOf -> Some (mn, gn, kern)
                 | _ -> None)
            | _ -> None)
    match peelHit with
    | None -> (body, [])
    | Some (mName, gName, kernE) ->
    let vName, gkName = Map.find gName groupedOf
    let refuse msg = (body, [msg])
    // -- the kernel ---------------------------------------------------------
    let kernRead =
        match (stripTypedE kernE).Kind with
        | ExprKind.ExprLambda ([rp], _, kbody) -> classifyPeelKernel rp.Name kbody
        | ExprKind.ExprLambda (ps, _, _) when List.length ps <> 1 -> Ok None
        | ExprKind.ExprVar fn when Map.containsKey fn ctx.Decls ->
            Error $"the peel kernel '{fn}' over grouped values must be spelled as a LAMBDA for the auto-lowering to read it -- a named function eta-expands WITHOUT its where-clause, so v1 does not accept one; inline it, e.g. `lambda(r) -> reduce(r, (+)) / extents(r)`"
        | _ -> Ok None
    match kernRead with
    | Error msg -> refuse msg
    | Ok None -> (body, [])
    | Ok (Some pk) ->
    // -- the source extent: one loop over V needs V's bound -----------------
    let srcExtent =
        let byParam =
            fd.Params |> List.tryPick (fun p ->
                if p.Name <> vName then None
                else p.Type |> Option.bind (fun t ->
                    match arrayLiteralExtents (resolveArrayTy ctx t) with
                    | Some (_, [n]) -> Some n
                    | _ -> None))
        match byParam with
        | Some n -> Some n
        | None ->
            Map.tryFind vName letTys
            |> Option.bind (fun t ->
                match arrayLiteralExtents (resolveArrayTy ctx t) with
                | Some (_, [n]) -> Some n
                | _ -> None)
    match srcExtent with
    | None -> (body, [])
    | Some n ->
    // -- the consumption ----------------------------------------------------
    let gbName = fresh ctx "__gb"
    let gnName = fresh ctx "__gn"
    let glName = fresh ctx "__gL"
    let giName = fresh ctx "__gi"
    let found = ResizeArray<Expr option>()
    let rw = rewriteGroupConsumption mName (v glName) found
    let stmts1 = stmts0 |> List.map (mapStmtValue rw)
    let final1 = finalOpt |> Option.map rw
    let notFullyReduced =
        $"the grouped peel '{mName}' is not reduced away: a differentiable per-group loss has to collapse the GROUP axis completely -- `reduce({mName}, (+))` or `reduce({mName} * <group-space weights>, (+))` -- because that axis has no compile-time extent for anything downstream to allocate over (v1)"
    if found.Count <> 1 then refuse notFullyReduced else
    // Every other mention of the peel or of the grouped value it came from
    // would be left dangling by dropping their lets, so it refuses instead.
    let leftovers = Set.ofList [mName; gName]
    let residual =
        (stmts1 |> List.exists (fun s ->
            match unwrapStmt s with
            // the two lets this rewrite DROPS are allowed to mention them
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm } } when nm = mName || nm = gName -> false
            | other -> stmtMentionsDeep leftovers other))
        || (match final1 with Some fe -> mentionsDeep leftovers fe | None -> false)
    if residual then refuse notFullyReduced else
    let weight = found.[0]
    // -- the empty-group policy (sql.md 10: the empty fold IS the init) -----
    let regime = groupRegimeOf ctx fd letTys (Map.find gkName groupings)
    let initOf = function PKSum i -> i | PKMean i -> i | PKCount -> None
    let nonzeroInit = match initOf pk with Some ie -> not (isZeroInit ie) | None -> false
    let policy =
        match regime, pk with
        | GRUnknown, _ ->
            Error $"cannot tell whether the key space behind '{gkName}' admits EMPTY groups, and the per-group fold of an empty group is only defined by an explicit init; annotate the key array -- `Array<Int64 like R>` for dynamic discovery (which never manufactures an empty group), `Array<Idx<N> like R>` / an `EnumIdx` element for a positional key space"
        | _, PKMean _ when nonzeroInit ->
            Error "a nonzero init in a per-group MEAN contributes `init * sum_g w_g / n_g`, which is a GROUP-space quantity and not recoverable from the source loop; v1 auto-lowers per-group means with an init of 0.0 (or none) only"
        | GRDynamic, PKSum _ when nonzeroInit ->
            Error "a nonzero init in a per-group sum contributes `init * <group count>`, and the group count of a dynamically-discovered key space is not known until run time; use `Idx<N>` / `EnumIdx` keys (whose group count is static) or an init of 0.0"
        | GRStatic _, PKSum None ->
            Error $"the key space behind '{gkName}' is positional, so it can have EMPTY groups -- and the fold of an empty group is undefined without an init (BL8003). Add one to define it: `reduce(r, (+), 0.0)`. Plain `Int64` keys need no init, because dynamic discovery never manufactures an empty group"
        | GRStatic _, PKMean _ ->
            Error $"the key space behind '{gkName}' is positional, so it can have EMPTY groups -- and the mean of an empty group is 0/0, which no init defines. Key the grouping by plain `Int64` (dynamic discovery never manufactures an empty group), or reformulate as a per-group sum with an explicit init"
        | _ -> Ok ()
    match policy with
    | Error msg -> refuse msg
    | Ok () ->
    // -- emission -----------------------------------------------------------
    // One loop over the SOURCE index space. `guard` is the drop mask: a
    // negative key means `group_bucket` reports -1, and the guard zeroes that
    // row's whole contribution. Zeroing is LINEAR, so the guard is already in
    // `LinearForm` and rides through both AD modes untouched.
    let bAt = syn (ExprApp (v gbName, [v giName]))
    let inRange = syn (ExprBinOp (Elementwise, OpGe, bAt, iLit 0L))
    // The bucket also has to be safe as a SUBSCRIPT, not just as a value. The
    // outer guard zeroes a dropped row's contribution, but neither AD lane
    // keeps the group-space reads inside it: reverse mode's quotient rule
    // emits `cot / __gn(b(i))` with the divisor OUTSIDE the condition, and the
    // weight leg's adjoint is a SCATTER `__g_W(b(i)) += ...`. At b(i) = -1
    // those are an out-of-range read and an out-of-range WRITE. Clamping the
    // subscript to 0 is exact precisely because the outer guard has already
    // zeroed everything that flows through it -- `0 / n_0` and `+= 0` are the
    // right answers -- and it costs a select, not a branch.
    let bIx = syn (ExprGuard (inRange, bAt))
    let nAt = syn (ExprApp (v gnName, [bIx]))
    let vAt = syn (ExprApp (v vName, [v giName]))
    let wAt = weight |> Option.map (fun we -> syn (ExprApp (we, [bIx])))
    let phi =
        match pk, wAt with
        | PKSum _, None -> vAt
        | PKSum _, Some wa -> mul wa vAt
        | PKMean _, None -> div vAt nAt
        | PKMean _, Some wa -> mul (div wa nAt) vAt
        // count is sum_g n_g (weighted: sum_g w_g * n_g), so each surviving
        // ROW contributes exactly its group's weight -- and no `v` at all,
        // which is why the gradient wrt the values is identically zero.
        | PKCount, None -> fLit 1.0
        | PKCount, Some wa -> wa
    let needsCounts = (match pk with PKMean _ -> true | _ -> false)
    // The init contribution, folded in EXACTLY rather than approximated:
    // sum_g w_g * init, with sum_g w_g = N (unweighted) or reduce(W, (+)).
    let seed =
        match pk, initOf pk, regime with
        | PKSum _, Some ie, GRStatic ng when not (isZeroInit ie) ->
            (match weight with
             | None -> mul ie (fLit (float ng))
             | Some we -> mul ie (syn (ExprReduce (we, syn (ExprSection OpAdd), None, None))))
        | _ -> fLit 0.0
    let emitted =
        [ yield StmtLet { Mutability = BindLet; Pattern = synPat (PatVar gbName); Type = None
                          Value = syn (ExprGroupBucket (v gkName)) }
          if needsCounts then
              yield StmtLet { Mutability = BindLet; Pattern = synPat (PatVar gnName); Type = None
                              Value = syn (ExprExtents (v gkName)) }
          yield StmtLet { Mutability = BindMut; Pattern = synPat (PatVar glName); Type = None
                          Value = seed }
          yield StmtForIn (giName,
                           syn (ExprDotDot (iLit 0L, iLit (int64 n))),
                           [ StmtExpr (syn (ExprAssign (v glName,
                                add (v glName)
                                    (syn (ExprGuard (inRange, phi)))))) ]) ]
    let stmts2 =
        stmts1 |> List.collect (fun s ->
            match unwrapStmt s with
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm } } when nm = gName -> []
            | StmtLet { Pattern = { Kind = PatternKind.PatVar nm } } when nm = mName -> emitted
            | _ -> [s])
    (inheritSpan body (ExprBlock (stmts2, final1)), [])

