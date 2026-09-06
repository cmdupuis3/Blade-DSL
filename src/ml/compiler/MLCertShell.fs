/// The abstract-interpretation walker shell shared by the three certification
/// disciplines -- MLEquiv (`ml.equiv(G)`, O(3)/SO(3)), MLGalilean
/// (`ml.galilean(u, ...)`, constant boosts) and MLPerm (`ml.perm_equiv(N)`,
/// Sn node relabelling).
///
/// WHAT LIVES HERE: the syntactic walk the three disciplines cannot disagree
/// about -- which names a pattern binds, which names an expression reads
/// freely, how a list of subexpressions is judged left to right with the
/// first error winning, and how a normalized where-conjunct is read off a
/// function declaration. These are properties of the AST, not of any group
/// action.
///
/// WHAT DOES NOT LIVE HERE: the RULES -- status lattices, signature
/// classifiers, judgment arms, op tables, diagnostics. The three lattices
/// have OPPOSITE POLARITY at several arms (MLPerm's header tabulates it), so
/// `judge` / `judgeStmts` / `judgeAssign` stay per-discipline; sharing them
/// would take six callback parameters to save twenty-odd lines, a worse
/// trade than the copy. Shell = the walk; module = the rules.
module Blade.ML.CertShell

open Blade.Ast

/// Judge subexpressions left to right; the FIRST error wins and nothing
/// after it is judged (`Result.bind` short-circuits once the accumulator is
/// an `Error`). Replaces the identical `judgeAll` local each walker defined.
let judgeEach (j: 'a -> Result<'b, 'e>) (xs: 'a list) : Result<'b list, 'e> =
    xs
    |> List.fold (fun acc a ->
        acc |> Result.bind (fun sts -> j a |> Result.map (fun s -> sts @ [ s ])))
        (Ok [])

/// All pattern variables of a pattern. Each discipline binds them at its own
/// invariant status when it destructures (see `bindPatternVars`).
let rec patternVars (p: Pattern) : string list =
    match p.Kind with
    | PatternKind.PatVar n -> [ n ]
    | PatternKind.PatTuple ps -> ps |> List.collect patternVars
    | _ -> []

/// Bind every variable of a pattern at one status -- each discipline passes
/// its own invariant (`Inv` / `BInv` / `Pow 0`).
let bindPatternVars (st: 'st) (env: Map<string, 'st>) (p: Pattern) : Map<string, 'st> =
    List.fold (fun m v -> Map.add v st m) env (patternVars p)

/// Free variables of an expression that are NOT locally bound (the lambda
/// capture rule of every discipline, and MLPerm's former-scope scan).
/// EXHAUSTIVE BY CONSTRUCTION: a name this walk misses is invisible to the
/// soundness guards built on it, so a missing arm is a false ACCEPT, not a
/// missed diagnostic. A three-way diff of the pre-shell copies found two such
/// gaps, both fixed here: a `| _ -> Set.empty` catch-all that dropped
/// `ExprMethodFor` / `ExprFor` / `ExprCompute` / `ExprReduce` (so an array
/// named only in a former's source list was invisible, and MLPerm's
/// former-scope scan cleared a node-covariant `x` as invariant); and a
/// `for`-body walk that only recursed one level (see `freeVarsStmts`). Every
/// `ExprKind` case is now listed explicitly, so a new AST node is a compile
/// error here instead of a silent hole.
let rec freeVars (bound: Set<string>) (e: Expr) : Set<string> =
    let fv = freeVars bound
    let fvs (es: Expr list) = es |> List.map fv |> Set.unionMany
    let fvOpt (o: Expr option) = match o with Some x -> fv x | None -> Set.empty
    match e.Kind with
    | ExprKind.ExprVar n -> if Set.contains n bound then Set.empty else Set.singleton n
    | ExprKind.ExprLet (b, body) ->
        Set.union (fv b.Value) (freeVars (Set.union bound (Set.ofList (patternVars b.Pattern))) body)
    | ExprKind.ExprLambda (ps, _, body) ->
        freeVars (Set.union bound (Set.ofList (ps |> List.map _.Name))) body
    | ExprKind.ExprMatch (s, cases) ->
        // A case pattern binds over its own arm -- the same names `judge`
        // binds via `bindPatternVars`, not the outer scope.
        Set.unionMany
            (fv s :: (cases |> List.map (fun c ->
                freeVars (Set.union bound (Set.ofList (patternVars c.Pattern))) c.Body)))
    | ExprKind.ExprBlock (stmts, fin) ->
        let acc, scope = freeVarsStmts bound stmts
        Set.union acc (match fin with Some fe -> freeVars scope fe | None -> Set.empty)
    | ExprKind.ExprRecArray d ->
        // `let rec q = match q with | zero :: n -> zero :: SEED | p :: n -> ...`:
        // the recursive family, prefix and step ordinal are bound in the
        // arms that read them.
        let seed =
            match d.SeedArm with
            | Some (stepVar, se) -> freeVars (Set.union bound (Set.ofList [ d.Name; stepVar ])) se
            | None -> Set.empty
        let inductiveBound = Set.union bound (Set.ofList [ d.Name; d.PrefixVar; d.StepVar ])
        Set.unionMany
            [ seed
              freeVars inductiveBound d.SliceExpr
              (match d.Guard with Some g -> freeVars inductiveBound g | None -> Set.empty) ]
    // co-iteration formers: a former hands its kernel the ELEMENTS of these
    // expressions, so every name here is live even though the kernel never
    // spells it.
    | ExprKind.ExprMethodFor arrays -> fvs arrays
    | ExprKind.ExprObjectFor kernel -> fv kernel
    | ExprKind.ExprFor (src, _, kernel) ->
        // where-clause conjuncts carry Idents and TypeExprs, never values.
        let srcVars =
            match src with
            | ForArrays (arrays, inClause) -> Set.union (fvs arrays) (fvOpt inClause)
            | ForKernel k -> fv k
        Set.union srcVars (fvOpt kernel)
    // leaves: nothing to read. `ExprArity` names a Poly parameter but queries
    // its ARITY, a static property -- no cell of it is read.
    | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprQualified _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _
    | ExprKind.ExprArity _ -> Set.empty
    // type-level only: `range<I>` / `reverse<I>` enumerate an INDEX type,
    // resolved against the static environment, not any value binding.
    | ExprKind.ExprRange _ | ExprKind.ExprReverse _ -> Set.empty
    | ExprKind.ExprUnaryOp (_, i) | ExprKind.ExprTyped (i, _) | ExprKind.ExprField (i, _)
    | ExprKind.ExprPure i | ExprKind.ExprCompute i | ExprKind.ExprRead i
    | ExprKind.ExprRank i | ExprKind.ExprUnique i | ExprKind.ExprExtents i
    | ExprKind.ExprDecompact (i, _) | ExprKind.ExprTranspose (i, _, _)
    | ExprKind.ExprReynolds (i, _) | ExprKind.ExprStatic i
    | ExprKind.ExprPartialApp (_, i, _)
    | ExprKind.ExprHalo (_, i) -> fv i
    | ExprKind.ExprBinOp (_, _, l, r) | ExprKind.ExprDotDot (l, r)
    | ExprKind.ExprTupleIndex (l, r) | ExprKind.ExprGuard (l, r)
    | ExprKind.ExprReplicate (l, r) | ExprKind.ExprMask (l, r)
    | ExprKind.ExprCompound (l, r) | ExprKind.ExprSparse (l, r) | ExprKind.ExprIntersect (l, r)
    | ExprKind.ExprUnion (l, r) | ExprKind.ExprContains (l, r)
    | ExprKind.ExprGroupBy (l, r) | ExprKind.ExprSort (l, r)
    | ExprKind.ExprGram (l, r) | ExprKind.ExprAssign (l, r) -> Set.union (fv l) (fv r)
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprZip es
    | ExprKind.ExprStack es | ExprKind.ExprSequence es | ExprKind.ExprGroupKeys es
    | ExprKind.ExprJoin (es, _) -> fvs es
    | ExprKind.ExprGroupBucket g -> fv g
    | ExprKind.ExprApp (f, args) -> Set.union (fv f) (fvs args)
    | ExprKind.ExprIf (c, t, f) -> Set.unionMany [ fv c; fv t; fv f ]
    | ExprKind.ExprReduce (src, kern, init, _) -> Set.unionMany [ fv src; fv kern; fvOpt init ]
    | ExprKind.ExprAlign (es, spec) ->
        let pad =
            match spec with
            | Some sp -> (match sp.Boundary with BndPad p -> fv p | _ -> Set.empty)
            | None -> Set.empty
        Set.union (fvs es) pad
    | ExprKind.ExprStruct (_, fields, spread) ->
        Set.union (fields |> List.map (snd >> fv) |> Set.unionMany) (fvOpt spread)

/// Free variables of a statement list, threading the scope each `let` opens
/// along the sequence. Returns the names read freely, together with the
/// scope in force after the last statement -- a block's final expression is
/// judged under that scope.
///
/// NESTED `for` STATEMENTS RECURSE: a shallow one-level walk would silently
/// drop whatever a loop inside a loop reads, the same false-accept shape as a
/// missing expression arm. The surface language no longer has the imperative
/// `for` (BL1003 rejects it), so no source program reaches this today; the
/// arm stays because synthesized bodies (Grad.fs, MathDecls.fs) still use it.
and freeVarsStmts (bound: Set<string>) (stmts: Stmt list) : Set<string> * Set<string> =
    stmts
    |> List.fold (fun (acc, scope) s ->
        match unwrapStmt s with
        | StmtLet binding ->
            (Set.union acc (freeVars scope binding.Value),
             Set.union scope (Set.ofList (patternVars binding.Pattern)))
        | StmtExpr e -> (Set.union acc (freeVars scope e), scope)
        | StmtAssign (l, _, r) ->
            (Set.unionMany [ acc; freeVars scope l; freeVars scope r ], scope)
        | StmtForIn (v, range, body) ->
            // the loop variable, and anything the body binds, are scoped to
            // the body: neither escapes into the rest of the sequence.
            let inner, _ = freeVarsStmts (Set.add v scope) body
            (Set.unionMany [ acc; freeVars scope range; inner ], scope)
        // `unwrapStmt` has already peeled the span wrapper.
        | StmtSpanned _ -> (acc, scope))
        (Set.empty, bound)

/// The normalized where-conjuncts of one name carried by a function
/// declaration (`__ml_equiv` / `__ml_galilean` / `__ml_perm_equiv`) -- the
/// pre-scan every `buildCertTable` opens with. Zero, one, or (the duplicate
/// case each discipline rejects with its own message) more.
let conjunctsOf (name: string) (fd: FunctionDecl) : (Ident * Ident list) list =
    fd.WhereClause
    |> Option.map _.Custom
    |> Option.defaultValue []
    |> List.filter (fun (n, _) -> n = name)
