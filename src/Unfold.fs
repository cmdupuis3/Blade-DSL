/// Unfold -- the staged-former elaboration pass.
///
/// `static method_for(...)` / `static object_for(...)` / `static for (...)`
/// mark a loop-object former whose ARGUMENT LIST elaborates at compile
/// time. This pass runs FIRST in the expansion pipeline (before
/// ML/PPL/math/grad elaboration and typechecking) and ELIMINATES every
/// ExprStatic node:
///   * Ground subexpressions fold through StaticEval (the existing
///     `let static` / static-function evaluator: recursion, fuel, builtins).
///   * Staged values -- array refs, lambdas, loop objects, any runtime
///     expression StaticEval's ground domain can't hold -- are carried as
///     opaque expression leaves (UOpaque). Tuples may mix both.
///   * A static FUNCTION applied to staged arguments is INLINED: binders in
///     the body are alpha-renamed with fresh `__unf_<fn>_<n>_` names per
///     unfold instance (capture safety), parameters substitute as
///     expressions, and unfolding recurses fuel-bounded.
///   * In static-former argument position, and ONLY there, tuple results
///     flatten recursively into the positional argument list (the
///     uncurrying rule: ((A,B),C) = (A,(B,C)) = (A,B,C)). Everywhere else
///     the pack convention stands (f((A,B)) = one tuple argument).
///   * `qs[k]` on a staged tuple with a compile-time k selects the element
///     (kernel selection); an index is always required.
///
/// `let static` ownership is shared with StaticEval.resolveStatics: a
/// DeclStatic whose RHS folds fully ground is LEFT ALONE; in a module that
/// USES static formers, one whose RHS unfolds to a STAGED value (a tuple
/// with opaque leaves, or a bare loop-object former) is consumed here --
/// its uses inside static formers substitute, the decl is DELETED before
/// resolveStatics could report it as a fold failure, and any surviving
/// reference outside a static former is an error. Modules with no static
/// former are untouched. Single-lambda statics keep their existing
/// function-declaration exemption. Staged statics resolve in DECLARATION
/// ORDER (no forward references among staged statics); imported
/// (cross-module) staged statics are out of scope -- ground qualified
/// statics keep working via the existing checkModule seed.
///
/// Poly interplay: none. Unfolding completes before typechecking, so
/// computePolyArity and the partial-application eta-expansion see ordinary
/// arity-n formers.
module Blade.Unfold

open Blade.Ast
open Blade.StaticEval

exception private UnfoldError of string

/// Fuel for one unfolding chain (static-function inlining depth x nodes).
let private unfoldFuel = 10_000

// Generic expression walk

/// Pre-order transform: `f` sees each node first; `Some r` replaces the
/// node (no further descent, the callback recurses itself if it needs to);
/// `None` rebuilds the node with transformed children. Exhaustive over
/// Expr so FS0025 audits future AST growth.
let rec mapExprPre (f: Expr -> Expr option) (e: Expr) : Expr =
    match f e with
    | Some r -> r
    | None ->
        let g = mapExprPre f
        let re k = inheritSpan e k
        match e.Kind with
        | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprVar _ | ExprKind.ExprQualified _ -> e
        | ExprKind.ExprBinOp (m, op, l, r) -> re (ExprBinOp (m, op, g l, g r))
        | ExprKind.ExprUnaryOp (op, x) -> re (ExprUnaryOp (op, g x))
        | ExprKind.ExprApp (fn, args) -> re (ExprApp (g fn, List.map g args))
        | ExprKind.ExprTupleIndex (t, i) -> re (ExprTupleIndex (g t, g i))
        | ExprKind.ExprField (x, id) -> re (ExprField (g x, id))
        | ExprKind.ExprLambda (ps, wc, body) -> re (ExprLambda (ps, wc, g body))
        | ExprKind.ExprLet (b, body) -> re (ExprLet ({ b with Value = g b.Value }, g body))
        | ExprKind.ExprMatch (scrut, cases) ->
            re (ExprMatch (g scrut, cases |> List.map (fun c ->
                { c with Guard = Option.map g c.Guard; Body = g c.Body })))
        | ExprKind.ExprIf (c, t, el) -> re (ExprIf (g c, g t, g el))
        | ExprKind.ExprTuple es -> re (ExprTuple (List.map g es))
        | ExprKind.ExprArrayLit es -> re (ExprArrayLit (List.map g es))
        | ExprKind.ExprBlock (stmts, fin) -> re (ExprBlock (stmts |> List.map (mapStmtPre f), Option.map g fin))
        | ExprKind.ExprMethodFor arrays -> re (ExprMethodFor (List.map g arrays))
        | ExprKind.ExprObjectFor k -> re (ExprObjectFor (g k))
        | ExprKind.ExprRange _ | ExprKind.ExprReverse _ | ExprKind.ExprHalo _ -> e
        | ExprKind.ExprDotDot (lo, hi) -> re (ExprDotDot (g lo, g hi))
        | ExprKind.ExprZip es -> re (ExprZip (List.map g es))
        | ExprKind.ExprAlign (es, spec) -> re (ExprAlign (List.map g es, spec))
        | ExprKind.ExprStack es -> re (ExprStack (List.map g es))
        | ExprKind.ExprJoin (es, d) -> re (ExprJoin (List.map g es, d))
        | ExprKind.ExprPure x -> re (ExprPure (g x))
        | ExprKind.ExprCompute x -> re (ExprCompute (g x))
        | ExprKind.ExprRead x -> re (ExprRead (g x))
        | ExprKind.ExprGuard (c, b) -> re (ExprGuard (g c, g b))
        | ExprKind.ExprSequence es -> re (ExprSequence (List.map g es))
        | ExprKind.ExprReplicate (c, b) -> re (ExprReplicate (g c, g b))
        | ExprKind.ExprReynolds (k, anti) -> re (ExprReynolds (g k, anti))
        | ExprKind.ExprTyped (x, ty) -> re (ExprTyped (g x, ty))
        | ExprKind.ExprArity _ | ExprKind.ExprNth | ExprKind.ExprZero -> e
        | ExprKind.ExprRank x -> re (ExprRank (g x))
        | ExprKind.ExprMask (a, p) -> re (ExprMask (g a, g p))
        | ExprKind.ExprCompound (d, m) -> re (ExprCompound (g d, g m))
        | ExprKind.ExprSparse (v, k) -> re (ExprSparse (g v, g k))
        | ExprKind.ExprIntersect (a, b) -> re (ExprIntersect (g a, g b))
        | ExprKind.ExprUnion (a, b) -> re (ExprUnion (g a, g b))
        | ExprKind.ExprUnique a -> re (ExprUnique (g a))
        | ExprKind.ExprContains (a, v) -> re (ExprContains (g a, g v))
        | ExprKind.ExprGroupBy (v, gk) -> re (ExprGroupBy (g v, g gk))
        | ExprKind.ExprGroupKeys ks -> re (ExprGroupKeys (List.map g ks))
        | ExprKind.ExprGroupBucket gk -> re (ExprGroupBucket (g gk))
        | ExprKind.ExprSort (a, k) -> re (ExprSort (g a, g k))
        | ExprKind.ExprReduce (a, k, init, ax) -> re (ExprReduce (g a, g k, Option.map g init, ax))
        | ExprKind.ExprTranspose (a, d1, d2) -> re (ExprTranspose (g a, d1, d2))
        | ExprKind.ExprDecompact (a, d) -> re (ExprDecompact (g a, d))
        | ExprKind.ExprGram (l, r) -> re (ExprGram (g l, g r))
        | ExprKind.ExprGramApply (l, r, x) -> re (ExprGramApply (g l, g r, g x))
        | ExprKind.ExprExtents a -> re (ExprExtents (g a))
        | ExprKind.ExprStruct (id, fields, spread) -> re (ExprStruct (id, fields |> List.map (fun (n, x) -> (n, g x)), spread |> Option.map g))
        | ExprKind.ExprSection _ -> e
        | ExprKind.ExprPartialApp (op, x, left) -> re (ExprPartialApp (op, g x, left))
        | ExprKind.ExprAssign (l, r) -> re (ExprAssign (g l, g r))
        | ExprKind.ExprFor (src, wcs, kern) ->
            let src' =
                match src with
                | ForArrays (arrays, inOpt) -> ForArrays (List.map g arrays, Option.map g inOpt)
                | ForKernel k -> ForKernel (g k)
            re (ExprFor (src', wcs, Option.map g kern))
        | ExprKind.ExprStatic x -> re (ExprStatic (g x))
        | ExprKind.ExprRecArray def ->
            re (ExprRecArray { def with
                                SeedArm = def.SeedArm |> Option.map (fun (v, s) -> (v, g s))
                                SliceExpr = g def.SliceExpr
                                Guard = def.Guard |> Option.map g })

and mapStmtPre (f: Expr -> Expr option) (s: Stmt) : Stmt =
    let g = mapExprPre f
    match s with
    | StmtLet b -> StmtLet { b with Value = g b.Value }
    | StmtAssign (l, op, r) -> StmtAssign (g l, op, g r)
    | StmtExpr x -> StmtExpr (g x)
    | StmtForIn (v, range, body) -> StmtForIn (v, g range, body |> List.map (mapStmtPre f))
    | StmtSpanned (inner, sp) -> StmtSpanned (mapStmtPre f inner, sp)

/// Every ExprVar name in the tree (FULL coverage, unlike the conservative
/// StaticEval.collectFreeNames -- shadowing is ignored, which for the
/// staged-escape check only risks a false positive on a shadowed name).
let private collectAllVars (e: Expr) : Set<string> =
    let mutable seen = Set.empty
    mapExprPre (fun x ->
        (match x.Kind with ExprKind.ExprVar n -> seen <- Set.add n seen | _ -> ())
        None) e |> ignore
    seen

/// Does the tree contain any ExprStatic node?
let private containsStatic (e: Expr) : bool =
    let mutable found = false
    mapExprPre (fun x ->
        (match x.Kind with ExprKind.ExprStatic _ -> found <- true | _ -> ())
        None) e |> ignore
    found

// Capture-avoiding substitution and per-instance binder freshening

/// Substitute free variables by expressions, stopping at shadowing binders.
// (public: also reused by TypeCheck.inferRecArray to substitute the prefix /
// step-ordinal names into the slice expression during the recursive-array
// desugar.)
let rec substFree (m: Map<string, Expr>) (e: Expr) : Expr =
    if Map.isEmpty m then e else
    let without (names: Set<string>) (mm: Map<string, Expr>) =
        names |> Set.fold (fun acc n -> Map.remove n acc) mm
    mapExprPre (fun x ->
        match x.Kind with
        | ExprKind.ExprVar n -> Map.tryFind n m
        | ExprKind.ExprLambda (ps, wc, body) ->
            let m' = without (ps |> List.map (_.Name) |> Set.ofList) m
            Some (inheritSpan x (ExprLambda (ps, wc, substFree m' body)))
        | ExprKind.ExprLet (b, body) ->
            let m' = without (collectPatternBindings b.Pattern) m
            Some (inheritSpan x (ExprLet ({ b with Value = substFree m b.Value }, substFree m' body)))
        | ExprKind.ExprMatch (scrut, cases) ->
            Some (inheritSpan x (ExprMatch (substFree m scrut, cases |> List.map (fun c ->
                let m' = without (collectPatternBindings c.Pattern) m
                { c with Guard = Option.map (substFree m') c.Guard; Body = substFree m' c.Body }))))
        | ExprKind.ExprBlock (stmts, fin) ->
            // statements bind sequentially -- thread the shrinking map
            let rec goStmt mm s =
                match s with
                | StmtSpanned (inner, sp) ->
                    let (s', mm') = goStmt mm inner
                    (StmtSpanned (s', sp), mm')
                | StmtLet b ->
                    let b' = { b with Value = substFree mm b.Value }
                    (StmtLet b', without (collectPatternBindings b.Pattern) mm)
                | StmtAssign (l, op, r) -> (StmtAssign (substFree mm l, op, substFree mm r), mm)
                | StmtExpr x -> (StmtExpr (substFree mm x), mm)
                | StmtForIn (v, range, body) ->
                    let mmBody = Map.remove v mm
                    let body' = body |> List.fold (fun (acc, mcur) st ->
                                    let (st', m') = goStmt mcur st in (st' :: acc, m')) ([], mmBody)
                                |> fst |> List.rev
                    (StmtForIn (v, substFree mm range, body'), mm)
            let (stmtsRev, mFin) =
                stmts |> List.fold (fun (acc, mcur) st ->
                    let (st', m') = goStmt mcur st in (st' :: acc, m')) ([], m)
            Some (inheritSpan x (ExprBlock (List.rev stmtsRev, Option.map (substFree mFin) fin)))
        | _ -> None) e

/// Alpha-rename every binder introduced inside an inlined static-function
/// body ("fresh names per unfold instance") so opaque argument expressions
/// cannot be captured. Parameters are free in the body and are untouched.
let rec private freshenBinders (prefix: string) (counter: int ref) (e: Expr) : Expr =
    let fresh (orig: string) =
        let n = counter.Value
        counter.Value <- n + 1
        $"{prefix}{orig}_{n}"
    let renamePat (pat: Pattern) : Pattern * Map<string, Expr> =
        let rec go (pat: Pattern) (acc: Map<string, Expr>) =
            match pat.Kind with
            | PatternKind.PatVar n -> let nn = fresh n in (inheritPatSpan pat (PatVar nn), Map.add n (mkExpr pat.Span (ExprVar nn)) acc)
            | PatternKind.PatTuple ps ->
                let (ps', acc') = ps |> List.fold (fun (rs, a) p -> let (p', a') = go p a in (p' :: rs, a')) ([], acc)
                (inheritPatSpan pat (PatTuple (List.rev ps')), acc')
            | PatternKind.PatTyped (p, ty) -> let (p', a') = go p acc in (inheritPatSpan pat (PatTyped (p', ty)), a')
            | _ -> (pat, acc)
        go pat Map.empty
    mapExprPre (fun x ->
        match x.Kind with
        | ExprKind.ExprLambda (ps, wc, body) ->
            let renames = ps |> List.map (fun p -> (p.Name, fresh p.Name))
            let ps' = (ps, renames) ||> List.zip |> List.map (fun (p, (_, nn)) -> { p with Name = nn })
            let m = renames |> List.map (fun (o, nn) -> (o, inheritSpan x (ExprVar nn))) |> Map.ofList
            Some (inheritSpan x (ExprLambda (ps', wc, freshenBinders prefix counter (substFree m body))))
        | ExprKind.ExprLet (b, body) ->
            let (pat', m) = renamePat b.Pattern
            let b' = { b with Pattern = pat'; Value = freshenBinders prefix counter b.Value }
            Some (inheritSpan x (ExprLet (b', freshenBinders prefix counter (substFree m body))))
        | _ -> None) e

// The staged value domain

/// A value during unfolding: ground (StaticEval's domain), an opaque runtime
/// expression leaf, or a staged tuple that may mix both.
type private UValue =
    | UGround of StaticValue
    | UOpaque of Expr
    | UTuple of UValue list

// StaticValue/UValue carry no span; folded ground literals spliced back into
// the AST take the ambient `synthSpan` (stamped to the enclosing decl's span
// in unfoldModule). Opaque leaves keep their own original spans.
let rec private staticValueToExpr (sv: StaticValue) : Expr =
    match sv with
    | SVInt n -> syn (ExprLit (LitInt n))
    | SVFloat f -> syn (ExprLit (LitFloat f))
    | SVBool b -> syn (ExprLit (LitBool b))
    | SVString s -> syn (ExprLit (LitString s))
    | SVUnit -> syn (ExprLit LitUnit)
    | SVTuple vs -> syn (ExprTuple (vs |> List.map staticValueToExpr))
    | SVStruct (n, fs) -> syn (ExprStruct (n, fs |> List.map (fun (fn, v) -> (fn, staticValueToExpr v)), None))

let rec private uvalueToExpr (v: UValue) : Expr =
    match v with
    | UGround sv -> staticValueToExpr sv
    | UOpaque e -> e
    | UTuple vs -> syn (ExprTuple (vs |> List.map uvalueToExpr))

/// Recursive tuple flatten -- the uncurrying rule, applied ONLY in
/// static-former argument position.
let rec private flattenU (v: UValue) : Expr list =
    match v with
    | UTuple vs -> vs |> List.collect flattenU
    | UGround (SVTuple vs) -> vs |> List.collect (fun sv -> flattenU (UGround sv))
    | other -> [uvalueToExpr other]

/// Is this U-value STAGED -- i.e. must it be consumed at compile time
/// (a tuple with opaque leaves, or a bare loop-object former)?
let rec private isStagedValue (v: UValue) : bool =
    match v with
    | UTuple vs ->
        vs |> List.exists (fun x ->
            match x with
            | UOpaque _ -> true
            | UTuple _ -> isStagedValue x
            | UGround _ -> false)
    | UOpaque { Kind = ExprKind.ExprMethodFor _ } | UOpaque { Kind = ExprKind.ExprObjectFor _ } | UOpaque { Kind = ExprKind.ExprFor _ } -> true
    | _ -> false

/// A lambda-valued `let static` declares a function -- same exemption as
/// StaticEval's fold assertion (its helper is private there).
let rec private isLambdaValued (e: Expr) : bool =
    match e.Kind with
    | ExprKind.ExprLambda _ -> true
    | ExprKind.ExprTyped (inner, _) -> isLambdaValued inner
    | _ -> false

// U-evaluation

/// Shared context for one module's unfolding: the ground static environment,
/// the fresh-name counter, and the set of static functions this pass
/// actually inlined (candidates for deletion -- a staged-only body like
/// rep's tuple recursion cannot lower as a runtime function).
type private UCtx = {
    Env: StaticEnv
    Counter: int ref
    Inlined: System.Collections.Generic.HashSet<string>
}

/// Evaluate an expression in the staged domain. `inInline` is true inside an
/// inlined static-function body, where control flow MUST fold (a residual
/// runtime `if` there means the unfolding cannot complete); at argument
/// depth a non-foldable expression is simply an opaque runtime leaf.
let rec private ueval (ctx: UCtx) (staged: Map<string, UValue>)
                      (inInline: bool) (fuel: int) (expr: Expr) : UValue =
    if fuel <= 0 then
        raise (UnfoldError "static unfolding: step limit exceeded (possible unbounded staged recursion)")
    match evalExpr ctx.Env maxSteps expr with
    | Ok sv -> UGround sv
    | Error _ ->
        let recur = ueval ctx staged inInline (fuel - 1)
        match expr.Kind with
        | ExprKind.ExprVar n when Map.containsKey n staged -> staged.[n]
        | ExprKind.ExprTuple es -> UTuple (es |> List.map recur)
        | ExprKind.ExprTupleIndex (head, idxE) ->
            (match recur head with
             | UTuple vs ->
                 (match evalExpr ctx.Env maxSteps idxE with
                  | Ok (SVInt i) when int i >= 0 && int i < vs.Length -> vs.[int i]
                  | Ok (SVInt i) ->
                      raise (UnfoldError $"static unfolding: staged tuple index {i} out of bounds (0..{vs.Length - 1})")
                  | _ ->
                      raise (UnfoldError "static unfolding: an index into a staged tuple must be a compile-time integer"))
             | _ -> UOpaque expr)  // Poly-tuple indexing on a runtime value -- not ours
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar fname }, args) when Map.containsKey fname ctx.Env.Functions ->
            // A static function whose ground evaluation failed: at least one
            // argument (or an intermediate) is staged -- inline the body.
            let fd = ctx.Env.Functions.[fname]
            if args.Length <> fd.Params.Length then
                raise (UnfoldError $"static unfolding: '{fname}' expects {fd.Params.Length} argument(s), got {args.Length}")
            let argVals = args |> List.map recur
            ctx.Inlined.Add fname |> ignore
            let inst = ctx.Counter.Value
            ctx.Counter.Value <- inst + 1
            let body = freshenBinders $"__unf_{fname}_{inst}_" ctx.Counter fd.Body
            let substMap = (fd.Params, argVals |> List.map uvalueToExpr) ||> List.zip |> Map.ofList
            ueval ctx staged true (fuel - 1) (substFree substMap body)
        | ExprKind.ExprIf (c, t, el) when inInline ->
            (match evalExpr ctx.Env maxSteps c with
             | Ok (SVBool true) -> recur t
             | Ok (SVBool false) -> recur el
             | Ok _ -> raise (UnfoldError "static unfolding: an `if` condition inside a static function must be a compile-time Bool")
             | Error why ->
                 raise (UnfoldError $"static unfolding: an `if` condition inside a static function does not evaluate at compile time ({why})"))
        | ExprKind.ExprLet (({ Pattern = { Kind = PatternKind.PatVar n } } as b), body) when inInline ->
            // let inside an inlined body: bind (possibly staged), continue
            let v = recur b.Value
            ueval ctx (Map.add n v staged) true (fuel - 1) body
        | _ -> UOpaque expr

// Static-former unfolding

let private unfoldFormer (ctx: UCtx) (staged: Map<string, UValue>) (inner: Expr) : Expr =
    let uevalArg = ueval ctx staged false unfoldFuel
    let single (what: string) (e: Expr) =
        match flattenU (uevalArg e) with
        | [k] -> k
        | many -> raise (UnfoldError $"static {what}: the kernel elaborated to {many.Length} expressions -- exactly one is required (select from a staged tuple with an index: qs[k])")
    match inner.Kind with
    | ExprKind.ExprMethodFor args ->
        inheritSpan inner (ExprMethodFor (args |> List.collect (uevalArg >> flattenU)))
    | ExprKind.ExprObjectFor kernel ->
        inheritSpan inner (ExprObjectFor (single "object_for" kernel))
    | ExprKind.ExprFor (ForArrays (arrays, inOpt), wcs, kern) ->
        inheritSpan inner (ExprFor (ForArrays (arrays |> List.collect (uevalArg >> flattenU), inOpt), wcs, kern))
    | ExprKind.ExprFor (ForKernel k, wcs, kern) ->
        inheritSpan inner (ExprFor (ForKernel (single "for" k), wcs, kern))
    | _ ->
        raise (UnfoldError "internal: `static` wraps a non-former expression (parser invariant violated)")

// Module pass

let private unfoldModule (m: ModuleDecl) : ModuleDecl =
    // Phase 1: ground statics + static functions via the existing resolver.
    // Fold failures are NOT reported here -- checkModule owns the fold-or-fail
    // assertion; we only need the ground environment.
    let senv =
        match resolveStatics m.Decls with
        | Ok (env, _failures) -> env
        | Error cycleMsg -> raise (UnfoldError cycleMsg)
    let ctx = { Env = senv; Counter = ref 0; Inlined = System.Collections.Generic.HashSet<string>() }

    // Phase 2: classify staged statics in declaration order. A `let static
    // qs = (lambda..., ...)` (tuple with opaque leaves, or a bare
    // loop-object former) is consumed here; ground and single-lambda
    // statics keep their existing paths untouched.
    let mutable staged : Map<string, UValue> = Map.empty
    for d in m.Decls do
        // Ground literals folded during staged-static classification take this
        // decl's span (staticValueToExpr / uvalueToExpr read the ambient).
        Blade.Ast.synthSpan <- d.Span
        match d.Value with
        | DeclStatic (({ Pattern = { Kind = PatternKind.PatVar name } } as b))
            when not (Map.containsKey name senv.Values) && not (isLambdaValued b.Value) ->
            let v =
                try Some (ueval ctx staged false unfoldFuel b.Value)
                with UnfoldError _ -> None
            (match v with
             | Some uv when isStagedValue uv -> staged <- Map.add name uv staged
             | _ -> ())  // not staged: leave for checkModule's fold-or-fail
        | _ -> ()

    // Phase 3: rewrite every static former, innermost-first (a static
    // former's arguments may themselves contain static formers). Errors
    // carry the enclosing declaration's line.
    let rec rewriteExpr (e: Expr) : Expr =
        mapExprPre (fun x ->
            match x.Kind with
            // Kernel selection `qs[k]` on a staged tuple resolves ANYWHERE in
            // the module (the kernel slot of `static method_for(A) <@> qs[1]`
            // sits outside the former's argument list) -- k must be a
            // compile-time integer; the selected element splices in place.
            | ExprKind.ExprTupleIndex ({ Kind = ExprKind.ExprVar n }, _) when Map.containsKey n staged ->
                Some (uvalueToExpr (ueval ctx staged false unfoldFuel x))
            | ExprKind.ExprStatic inner ->
                let inner' =
                    match inner.Kind with
                    | ExprKind.ExprMethodFor args -> inheritSpan inner (ExprMethodFor (List.map rewriteExpr args))
                    | ExprKind.ExprObjectFor k -> inheritSpan inner (ExprObjectFor (rewriteExpr k))
                    | ExprKind.ExprFor (ForArrays (arrays, inOpt), wcs, kern) ->
                        inheritSpan inner (ExprFor (ForArrays (List.map rewriteExpr arrays, Option.map rewriteExpr inOpt), wcs, Option.map rewriteExpr kern))
                    | ExprKind.ExprFor (ForKernel k, wcs, kern) ->
                        inheritSpan inner (ExprFor (ForKernel (rewriteExpr k), wcs, Option.map rewriteExpr kern))
                    | _ -> rewriteExpr inner
                Some (unfoldFormer ctx staged inner')
            | _ -> None) e
    let rewriteIn (spanLine: int) (e: Expr) : Expr =
        try rewriteExpr e
        with UnfoldError msg -> raise (UnfoldError $"{msg} (line {spanLine})")
    let rewritten =
        m.Decls |> List.map (fun d ->
            // Spliced ground literals in this decl take its span (ambient).
            Blade.Ast.synthSpan <- d.Span
            let line = d.Span.StartLine
            let value' =
                match d.Value with
                | DeclLet b -> DeclLet { b with Value = rewriteIn line b.Value }
                | DeclStatic b -> DeclStatic { b with Value = rewriteIn line b.Value }
                | DeclFunction fd -> DeclFunction { fd with Body = rewriteIn line fd.Body }
                | other -> other
            { d with Value = value' })

    // Phase 4: staged statics must not escape -- after substitution inside
    // static formers, no reference to a consumed name may survive.
    if not (Map.isEmpty staged) then
        for d in rewritten do
            let isStagedDecl =
                match d.Value with
                | DeclStatic { Pattern = { Kind = PatternKind.PatVar n } } -> Map.containsKey n staged
                | _ -> false
            if not isStagedDecl then
                let vars =
                    match d.Value with
                    | DeclLet b | DeclStatic b -> collectAllVars b.Value
                    | DeclFunction fd -> collectAllVars fd.Body
                    | _ -> Set.empty
                for KeyValue (n, _) in staged do
                    if Set.contains n vars then
                        raise (UnfoldError $"staged static '{n}' holds compile-time-only values (lambdas/arrays/loop objects) and is only usable inside a static former (line {d.Span.StartLine})")

    // Phase 5: delete consumed staged decls, then delete static FUNCTIONS
    // this pass inlined that have no surviving reference (a staged-only body
    // like rep's tuple recursion cannot lower as a runtime C++ function;
    // ones still referenced -- e.g. from a ground `let static` RHS -- stay).
    let declsAfterStaged =
        rewritten |> List.filter (fun d ->
            match d.Value with
            | DeclStatic { Pattern = { Kind = PatternKind.PatVar n } } -> not (Map.containsKey n staged)
            | _ -> true)
    let referencedElsewhere (fname: string) =
        declsAfterStaged |> List.exists (fun d ->
            match d.Value with
            | DeclFunction fd when fd.Name = fname -> false  // self-recursion doesn't count
            | DeclFunction fd -> Set.contains fname (collectAllVars fd.Body)
            | DeclLet b | DeclStatic b -> Set.contains fname (collectAllVars b.Value)
            | DeclType (TyDeclStruct (_, _, fields, constraints, _)) ->
                // Where-conjuncts, field-bound exprs, and field defaults are
                // real references: the checker synthesizes runtime guards
                // that CALL these functions at every assignment site.
                let fieldExprs =
                    fields |> List.collect (fun f ->
                        (match f.Bound with
                         | Some fb -> [fb.Lo; fb.Hi] |> List.choose id
                         | None -> [])
                        @ Option.toList f.Default)
                constraints @ fieldExprs
                |> List.exists (fun e -> Set.contains fname (collectAllVars e))
            | DeclType (TyDeclMutualGroup (_, constraints)) ->
                constraints |> List.exists (fun e -> Set.contains fname (collectAllVars e))
            | _ -> false)
    let decls' =
        declsAfterStaged |> List.filter (fun d ->
            match d.Value with
            | DeclFunction fd when fd.IsStatic && ctx.Inlined.Contains fd.Name && not (referencedElsewhere fd.Name) -> false
            | _ -> true)
    Blade.Ast.synthSpan <- noSpan
    { m with Decls = decls' }

/// The pass entry point -- same shape as the other elaborators. Modules
/// without any static former pass through untouched (so existing `let
/// static` semantics, including fold-or-fail rejects, are byte-identical).
let private expandStr (program: Program) : Result<Program, string> =
    try
        let usesStaticFormers (m: ModuleDecl) =
            m.Decls |> List.exists (fun d ->
                match d.Value with
                | DeclLet b | DeclStatic b -> containsStatic b.Value
                | DeclFunction fd -> containsStatic fd.Body
                | _ -> false)
        let modules' =
            program.Modules |> List.map (fun m -> if usesStaticFormers m then unfoldModule m else m)
        Ok { program with Modules = modules' }
    with UnfoldError msg -> Error msg

/// Boundary: string-errored internals -> coded diagnostics. The span is the
/// ambient synthSpan -- stamped per-decl by expandStr, so a mid-elaboration
/// failure points at the offending declaration.
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun msg ->
        [ Blade.Diagnostics.mkError "BL4002" (Blade.Diagnostics.Codes.phaseOfCode "BL4002") Blade.Ast.synthSpan msg ])
