/// `rand`-module elaboration: rewrites `alias.<fam>(key, params.., shape)` into the compiler-internal builtins
/// `__rand_<fam>`, which the type-checker self-types (dense Float64 array of the static shape) and codegen
/// materializes via the blade_rand runtime.
///
/// Surface (reachable only through `import rand [as <alias>]`):
///
///   rand.uniform(key, n)                 -- rank-1 Array<Float64 like Idx<n>> ~ U[0,1)
///   rand.uniform(key, [m, n])            -- rank-2, row-major
///   rand.normal(key, n)                  -- N(0,1) via Box-Muller
///   rand.exponential(key, rate, n)       -- Exp(rate), inverse CDF
///   rand.gamma(key, shape, rate, n)      -- Gamma(shape, rate), Marsaglia-Tsang
///   rand.poisson(key, lam, n)            -- Poisson(lam), Knuth
///   rand.bernoulli(key, p, n)            -- Bernoulli(p), as 0.0/1.0
///   rand.beta(key, a, b, n)              -- Beta(a, b), from two gammas
///   rand.categorical(key, W, n)          -- index in [0,|W|), P(i) ~ W_i; Int64 elements
///
/// `key` is an Int64 stream key (same key => same draws). The distribution parameters are ordinary RUNTIME Float64
/// expressions -- they need not be static, and are evaluated once per fill. `shape` is a static int or a list of static
/// ints (`let static` names or literals) and is always the LAST argument. Every family yields Float64 elements,
/// including the integer-valued poisson/bernoulli (see cpp/rand_runtime.hpp for why) -- except `categorical`, which
/// yields Int64 because its draws are subscripts. `categorical`'s `W` is a rank-1 Float64 array (unnormalized weights
/// are fine) with a STATIC extent, not a scalar; it is the one array-valued parameter in the surface.
///
/// Unlike the math module this pass synthesizes no Blade source -- a counter-free RNG is not expressible in Blade
/// (no unsigned/bitwise ops), so the RNG lives in the C++ runtime and this pass only rewrites the call.
///
/// Pipeline position: after Math elaboration, BEFORE Grad expansion -- rand output is not differentiable, so Grad sees
/// only the settled opaque builtin.
module Blade.Rand.Elaborate

open Blade.Ast
open Blade.StaticEval

let private v (name: string) : Expr = syn (ExprVar name)

/// Resolve a static-int argument: an int literal or a `let static` name.
let private staticInt (statics: StaticEnv) (what: string) (e: Expr) : Result<int, string> =
    match e.Kind with
    | ExprKind.ExprLit (LitInt n) -> Ok (int n)
    | ExprKind.ExprVar name ->
        match Map.tryFind name statics.Values with
        | Some (SVInt n) -> Ok (int n)
        | Some _ -> Error $"{what}: '{name}' is not a static int"
        | None -> Error $"{what}: '{name}' is not a `let static` binding (rand shapes must be static)"
    | _ -> Error $"{what}: shape must be a static int or list of static ints"

/// Resolve a shape argument to its list of positive extents.
let private resolveShape (statics: StaticEnv) (what: string) (shapeE: Expr) : Result<int list, string> =
    let dims =
        match shapeE.Kind with
        | ExprKind.ExprArrayLit elems -> elems
        | _ -> [ shapeE ]
    dims
    |> List.fold (fun acc d ->
        acc |> Result.bind (fun xs ->
            staticInt statics what d |> Result.bind (fun n ->
                if n > 0 then Ok (xs @ [n])
                else Error $"{what}: shape extents must be positive (got {n})")))
        (Ok [])

/// The `rand` surface: (op, internal builtin, count of non-shape parameter arguments).
/// Every op has the shape `rand.<op>(key, p1, .., pk, shape)` -- key first, shape LAST, the family's
/// parameters in between. This pass does not type the parameters at all; it counts them, passes them
/// through verbatim, and lets the checker arm decide what each one must be. The checker fixes the same
/// arity on the intrinsic, so this table is the surface spelling, not the authority.
///
/// Those parameters are ordinary runtime SCALAR Float64 expressions (only the shape is static) for
/// every family EXCEPT `categorical`, whose single parameter is an ARRAY: a rank-1 Float64 weights
/// array whose extent the checker pins statically. Adding a scalar-parameter family = one row here,
/// one row in the checker arm, a C++ fill, and a mirror.
///
/// CATEGORICAL, formerly deferred here, resolved the two design questions that kept it out of wave 1:
///
///  1. ARRAY PARAMETER CHANNEL. Every other parameter is a Float64 scalar -- a `TypedExpr` in
///     TExprRandGen, an `IRExpr` in RandGen, a `(double)`-cast argument in codegen, a `float` in
///     RandMirror.draws. Weights get a SECOND, differently-shaped channel carried alongside rather
///     than inside `pars`: an explicit `weights` field paired with the checker-pinned static extent,
///     emitted by codegen as `pool_base(W.data), (size_t)k` and unwrapped by the interpreter from a
///     VArray's SFloat store. The extent must be a literal because the length travels with the
///     pointer; a symbolic extent is refused in the checker arm.
///  2. OUTPUT TYPE. Categorical yields INDICES, so it returns Int64 rather than joining
///     poisson/bernoulli in the all-Float64 convention -- Float64 indices would need a coercion the
///     rand surface does not have, defeating the purpose. This is the first non-`double` fill, and it
///     cost less than expected: codegen's allocation was already generic in `elemTypeToCpp`, so the
///     checker choosing ETInt64 selects an `int64_t` pool by itself. Only the fill signature
///     (`int64_t* out`) and the mirror's return type (`int64[]` into an SInt store) are new.
///
/// The drawn indices DO subscript arrays directly -- `method_for(idx) <@> lambda(i) -> w[i]` gathers,
/// pinned by corpus test 017 -- so no index-tag seam blocks the motivating SMC-resampling use.
let private ops : (string * string * int) list =
    [ "uniform",     "__rand_uniform",     0
      "normal",      "__rand_normal",      0
      "exponential", "__rand_exponential", 1   // rate
      "gamma",       "__rand_gamma",       2   // shape, rate
      "poisson",     "__rand_poisson",     1   // lam
      "bernoulli",   "__rand_bernoulli",   1   // p
      "beta",        "__rand_beta",        2   // a, b
      "categorical", "__rand_categorical", 1 ] // weights (ARRAY, not a scalar)

/// Per-op parameter names, for the arity error message only.
let private paramNames (op: string) : string list =
    match op with
    | "exponential" -> ["rate"]
    | "gamma"       -> ["shape"; "rate"]
    | "poisson"     -> ["lam"]
    | "bernoulli"   -> ["p"]
    | "beta"        -> ["a"; "b"]
    | "categorical" -> ["weights"]
    | _             -> []

/// Elaborate one qualified rand op. `keyE` and the distribution parameters are passed through verbatim
/// (they are runtime Float64 expressions); the shape becomes trailing int-literal args.
let private elabOp (statics: StaticEnv) (op: string) (args: Expr list) : Result<Expr, string> =
    match ops |> List.tryFind (fun (o, _, _) -> o = op) with
    | None ->
        Error ($"""rand: unknown op '{op}' (available: {(ops |> List.map (fun (o, _, _) -> o) |> String.concat ", ")})""")
    | Some (_, fn, nPars) ->
        // key + nPars distribution params + exactly one shape argument.
        if List.length args <> nPars + 2 then
            Error (sprintf "rand.%s: expected rand.%s(key%s, shape) where shape is a static int or list of static ints"
                       op op (paramNames op |> List.map (sprintf ", %s") |> String.concat ""))
        else
            let keyE = List.head args
            let parEs = args |> List.skip 1 |> List.take nPars
            let shapeE = List.last args
            resolveShape statics $"rand.{op}" shapeE
            |> Result.map (fun dims ->
                let dimEs = dims |> List.map (fun n -> syn (ExprLit (LitInt (int64 n))))
                syn (ExprApp (v fn, (keyE :: parEs) @ dimEs)))

// Rewrite walker (same shape as MathElaborate.rewriteExpr)
let rec private rewriteExpr (statics: StaticEnv) (aliases: Set<string>) (e: Expr) : Result<Expr, string> =
    let r = rewriteExpr statics aliases
    let rList es =
        es |> List.fold (fun acc x ->
            acc |> Result.bind (fun xs -> r x |> Result.map (fun x' -> xs @ [x'])))
            (Ok [])
    let rOpt (o: Expr option) =
        match o with
        | None -> Ok None
        | Some x -> r x |> Result.map Some
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, op) }, args) when Set.contains alias aliases ->
        rList args |> Result.bind (fun args' -> elabOp statics op args')
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
        r binding.Value |> Result.bind (fun v' ->
        r body |> Result.map (fun b' -> inheritSpan e (ExprLet ({ binding with Value = v' }, b'))))
    | ExprKind.ExprBlock (stmts, finalE) ->
        let rec rStmt (s: Stmt) : Result<Stmt, string> =
            match s with
            | StmtSpanned (inner, sp) -> rStmt inner |> Result.map (fun i -> StmtSpanned (i, sp))
            | StmtLet binding -> r binding.Value |> Result.map (fun v' -> StmtLet { binding with Value = v' })
            | StmtExpr e2 -> r e2 |> Result.map StmtExpr
            | StmtAssign (l, op, rr) ->
                r l |> Result.bind (fun l' -> r rr |> Result.map (fun r' -> StmtAssign (l', op, r')))
            | StmtForIn (var, range, body) ->
                r range |> Result.bind (fun range' ->
                    body |> List.fold (fun acc bs ->
                        acc |> Result.bind (fun ss -> rStmt bs |> Result.map (fun s' -> ss @ [s'])))
                        (Ok [])
                    |> Result.map (fun body' -> StmtForIn (var, range', body')))
        stmts |> List.fold (fun acc s ->
            acc |> Result.bind (fun ss -> rStmt s |> Result.map (fun s' -> ss @ [s'])))
            (Ok [])
        |> Result.bind (fun stmts' ->
            match finalE with
            | Some fe -> r fe |> Result.map (fun fe' -> inheritSpan e (ExprBlock (stmts', Some fe')))
            | None -> Ok (inheritSpan e (ExprBlock (stmts', None))))
    | ExprKind.ExprLambda (ps, w, body) -> r body |> Result.map (fun b -> inheritSpan e (ExprLambda (ps, w, b)))
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

let private isRandImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["rand"], _) -> true
    | _ -> false

let private randAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["rand"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "rand") set)
            | DeclImport (["rand"], ImportSelective _) ->
                Error "`rand` supports only `import rand [as <alias>]`; a selective `from rand import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    randAliasesOf decls |> Result.bind (fun aliases ->
    // Import-gated: with no `import rand`, this pass is a strict no-op.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isRandImport)
        match resolveStatics declsNoImport with
        | Error e -> Error $"rand elaboration: static resolution failed: {e}"
        | Ok (statics, _) ->
            declsNoImport |> List.fold (fun acc d ->
                acc |> Result.bind (fun out ->
                    // Stamp the user decl's span so every syn-built node attributes to this declaration's source line.
                    Blade.Ast.synthSpan <- d.Span
                    let mapped =
                        match d.Value with
                        | DeclFunction fd ->
                            rewriteExpr statics aliases fd.Body
                            |> Result.map (fun b -> DeclFunction { fd with Body = b })
                        | DeclLet binding ->
                            rewriteExpr statics aliases binding.Value
                            |> Result.map (fun v' -> DeclLet { binding with Value = v' })
                        | DeclStatic binding ->
                            rewriteExpr statics aliases binding.Value
                            |> Result.map (fun v' -> DeclStatic { binding with Value = v' })
                        | other -> Ok other
                    mapped |> Result.map (fun value -> out @ [{ d with Value = value }])))
                (Ok []))

/// Entry point: elaborate rand ops across a program (after Math elaboration,
/// before Grad expansion).
let private expandStr (program: Program) : Result<Program, string> =
    program.Modules
    |> List.fold (fun acc m ->
        acc |> Result.bind (fun ms ->
            expandModule m.Decls |> Result.map (fun ds -> ms @ [{ m with Decls = ds }])))
        (Ok [])
    |> Result.map (fun ms -> { program with Modules = ms })

/// Boundary: string-errored internals -> coded diagnostics. The span is the
/// ambient synthSpan -- stamped per-decl by expandStr, so a mid-elaboration
/// failure points at the offending declaration.
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun msg ->
        [ Blade.Diagnostics.mkError "BL5300" (Blade.Diagnostics.Codes.phaseOfCode "BL5300") Blade.Ast.synthSpan msg ])
