/// PPL moment formers: moment/comoment tensors and declared independence,
/// elaborated to ordinary Blade source before type checking (between ML-op
/// elaboration and grad expansion; see TypeCheck.typeCheck).
///
/// Surface (call-shaped, recognized when the name is not user-bound; each
/// former must be the ENTIRE right-hand side of a top-level let):
///
///   moments(A, k)       raw comoment tensor of order k (static k >= 1):
///                         method_for(A, ..., A) <@> lambda(x1..xk)
///                           where comm(x1..xk) -> prodsum(x1..xk)/N
///                         |> compute
///                       Output: SymIdx<k, D> packed over A's fused leading
///                       axes; the LAST declared index of A is the sample
///                       (fiber) axis, its static extent is N.
///   comoments(A, 2)     central pair comoment (covariance), same shape with
///                       E[ab] - ma*mb as the kernel. Orders > 2 deferred
///                       (subset-lattice expansion over prodsums).
///   comoments(X, Y)     central cross-covariance block between two arrays --
///                       rectangular (method_for(X, Y), no comm clause).
///   independent(X, Y)   declaration `let _ = independent(X, Y)`: a declared-
///                       independent pair's comoments(X, Y) elaborates to a
///                       literal zero block, never the cross computation
///                       (exact for central pair comoments).
///
/// A moment-formed array must be a module-level `let`/`let static` with a
/// compile-time shape: either an Array annotation (`Array<Elem like I1, ...,
/// Ik, SampleIdx>`) with statically resolving index extents, or -- for
/// COMPUTED arrays -- an un-annotated `method_for(range<...>) <@> kernel
/// [|> compute]` RHS whose iteration space is the shape (one axis per dense
/// range slot, halo slots shrunk to their interior; see computedShapeOf).
module Blade.Ppl.Elaborate

open Blade.Ast
open Blade.StaticEval

// AST construction helpers (mirroring MLElaborate.fs / Grad.fs style)

let private v (n: string) = syn (ExprVar n)
let private fLit (x: float) = syn (ExprLit (LitFloat x))
let private addE a b = syn (ExprBinOp (Elementwise, OpAdd, a, b))
let private divE a b = syn (ExprBinOp (Elementwise, OpDiv, a, b))
let private mulE a b = syn (ExprBinOp (Elementwise, OpMul, a, b))
let private subE a b = syn (ExprBinOp (Elementwise, OpSub, a, b))
let private powE a b = syn (ExprBinOp (Elementwise, OpCaret, a, b))
let private sLet n value = StmtLet { Pattern = synPat (PatVar n); Type = None; Value = value; Mutability = BindLet }
let private sMutStmt nm vl = StmtLet { Pattern = synPat (PatVar nm); Type = None; Value = vl; Mutability = BindMut }
let private assignStmt (lhs: Expr) (rhs: Expr) = StmtExpr (syn (ExprAssign (lhs, rhs)))
let private forIn (var: string) (lo: Expr) (hi: Expr) (body: Stmt list) =
    StmtForIn (var, syn (ExprDotDot (lo, hi)), body)
let private meanE arr n = divE (syn (ExprReduce (arr, syn (ExprSection OpAdd), None, None))) (fLit n)
let private prodsumE args = syn (ExprApp (v "prodsum", args))
let private commWhere (names: string list) =
    Some { Commutativity = [names]; Antisymmetry = []; Parallel = []; Repro = false; TDims = []; Custom = [] }
// Full-span construction wrappers (stamp the ambient synthSpan); structural combinators only, scalar helpers above already wrap.
let private appE f args = syn (ExprApp (f, args))
let private arrLitE (cells: Expr list) = syn (ExprArrayLit cells)
let private methodForE (arrs: Expr list) = syn (ExprMethodFor arrs)
let private lambdaE ps w body = syn (ExprLambda (ps, w, body))
let private applyE l k = syn (ExprBinOp (Elementwise, OpApply, l, k))
let private computeE e = syn (ExprCompute e)
let private reduceAddE e = syn (ExprReduce (e, syn (ExprSection OpAdd), None, None))
let private pvar n = synPat (PatVar n)
let private ptuple (ps: Pattern list) = synPat (PatTuple ps)
/// Inline co-iteration pipeline over same-shape (packed included) arrays:
/// method_for(zip(a, b)) <@> lambda(u, w) -> body |> compute -- the corpus-blessed one-binding form (sql-set-ops/004).
let private zipMap2 (a: Expr) (b: Expr) (body: Expr) =
    computeE (applyE
        (methodForE [syn (ExprZip [a; b])])
        (lambdaE [{ Name = "__u"; Type = None; Default = None; NameSpan = noSpan }; { Name = "__w"; Type = None; Default = None; NameSpan = noSpan }] None body))
let private map1 (a: Expr) (body: Expr) =
    computeE (applyE
        (methodForE [a])
        (lambdaE [{ Name = "__u"; Type = None; Default = None; NameSpan = noSpan }] None body))

// "cumulant" is NOT a former name: it is a checker-level projection on
// Dist-typed values (TypeCheck.inferCumulantProj), so elaboration lets it flow through untouched.
// The named-family constructors (gaussian..beta), the log-density formers
// (logpdf/loglik), exact family sampling (sample), the approximate tower
// bridge (dist_pdf_approx/dist_quantile_approx/dist_sample_approx), and the
// P4 sampling-inference surface (mh/hmc + chain_mean/chain_var/autocorr/ess/
// rhat; see the section comment at elabMh) are formers too: the family list
// here mirrors familyParams below (kept literal so this set stays the one
// place the qualified-surface/misplaced-use machinery reads).
let private formerNames = set [ "moments"; "comoments"; "cumulants"; "independent"; "dist"; "dist_add"; "dist_scale"; "comoments_merge"; "mstate"; "mstate_merge"; "mstate_cumulants"; "mixed_cumulants"; "dist_affine"; "dist_jet"; "dist_jet_closed"; "dist_map"; "dist_map_closed"; "free_cumulants"; "dist_expect"; "dist_reweight"; "dist_mix"; "dist_atoms"; "dist_negativity"; "gaussian"; "exponential"; "gamma"; "poisson"; "uniform"; "lognormal"; "bernoulli"; "beta"; "logpdf"; "loglik"; "sample"; "dist_pdf_approx"; "dist_quantile_approx"; "dist_sample_approx"; "mh"; "hmc"; "chain_mean"; "chain_var"; "autocorr"; "ess"; "rhat"; "bayes"; "gaussian_lik"; "bernoulli_lik"; "poisson_lik"; "dist_condition" ]

// Partition lattice: cumulants are Moebius-weighted sums over set partitions;
// Bell(r) partitions, 2^r - 1 distinct blocks, each block's moment bound once and shared.
let rec private factorial (n: int) : float =
    if n <= 1 then 1.0 else float n * factorial (n - 1)

/// All set partitions of [0 .. k-1] (size Bell(k)); blocks kept sorted.
let rec private setPartitions (k: int) : int list list list =
    if k = 0 then [ [] ]
    else
        setPartitions (k - 1)
        |> List.collect (fun p ->
            let el = k - 1
            let asSingleton = p @ [[el]]
            let inserted =
                p |> List.mapi (fun i _ ->
                    p |> List.mapi (fun j b -> if i = j then b @ [el] else b))
            asSingleton :: inserted)

/// Nonempty subsets of [0 .. k-1], each sorted.
let private nonemptySubsets (k: int) : int list list =
    [ 1 .. (1 <<< k) - 1 ]
    |> List.map (fun mask -> [ for i in 0 .. k - 1 do if mask &&& (1 <<< i) <> 0 then yield i ])

// The sufficient-statistic pool: every block moment is the raw prodsum of a row-multiset, so one sweep filling
// P_S = Sum_t Prod_{l in S} row_l(t) replaces per-cell-per-order prodsum loops. Combinator algebra: rows -> zip -> shared
// method_for -> one product kernel per multiset -> chain -> reduce((+)); single-leading-axis sources with static extents only.
let private iLit (n: int) = syn (ExprLit (LitInt (int64 n)))

/// Canonical (non-decreasing) label tuples of rank p over dim d, lex order.
let private canonicalTuples (d: int) (p: int) : int list list =
    let rec go lo p =
        if p = 0 then [ [] ]
        else [ for x in lo .. d - 1 do for rest in go x (p - 1) -> x :: rest ]
    go 0 p

type private PoolInfo = {
    /// Canonical multiset (sorted row-position list) -> scalar binding name.
    Names: Map<int list, string>
    /// Static sample count (the raw-moment normalizer).
    N: float
}

/// The raw prodsum P_S.
let private poolRead (pool: PoolInfo) (s: int list) : Expr =
    v pool.Names.[List.sort s]

/// The raw moment E[Prod_{l in S} x_l] = P_S / N.
let private poolMoment (pool: PoolInfo) (s: int list) : Expr =
    divE (poolRead pool s) (fLit pool.N)

/// Emit the single-pass pool over a shared row list. `uniq` seeds binding names; `rows` = one slice expr per row position; `needed` =
/// the multisets the caller's cells read (deduped/canonicalized). Returns decls + reader.
let private poolDecls (span: Span) (uniq: string) (rows: Expr list)
    (needed: int list list) (n: float) : Located<Decl> list * PoolInfo =
    let mkDecl name value =
        { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
    let rowName i = $"__ppl_row_{uniq}_{i}"
    let rowDecls = rows |> List.mapi (fun i e -> mkDecl (rowName i) e)
    let lName = $"__ppl_poolL_{uniq}"
    let lValue =
        match rows with
        | [_] -> methodForE [v (rowName 0)]
        | _ -> methodForE [syn (ExprZip [ for i in 0 .. rows.Length - 1 -> v (rowName i) ])]
    let sets = needed |> List.map List.sort |> List.distinct |> List.sortBy (fun s -> (s.Length, s))
    let tag (s: int list) = s |> List.map string |> String.concat "_"
    let pName s = $"__ppl_P_{uniq}_{tag s}"
    let kName s = $"__ppl_poolk_{uniq}_{tag s}"
    let xName i = $"__x{i}"
    let ps = [ for i in 0 .. rows.Length - 1 -> { Name = xName i; Type = None; Default = None; NameSpan = noSpan } ]
    let kDecls =
        sets |> List.map (fun s ->
            let body = s |> List.map (fun i -> v (xName i)) |> List.reduce mulE
            mkDecl (kName s) (lambdaE ps None body))
    let applied = sets |> List.map (fun s -> applyE (v lName) (v (kName s)))
    let chain =
        match applied with
        | first :: rest -> rest |> List.fold (fun acc e -> syn (ExprBinOp (Elementwise, OpFusion, acc, e))) first
        | [] -> failwith "poolDecls: empty multiset list"
    let outPat =
        match sets with
        | [one] -> pvar (pName one)
        | _ -> ptuple (sets |> List.map (fun s -> pvar (pName s)))
    let outDecl = { Value = DeclLet { Pattern = outPat; Type = None
                                      Value = reduceAddE chain
                                      Mutability = BindLet }; Span = span }
    let names = sets |> List.map (fun s -> (s, pName s)) |> Map.ofList
    (rowDecls @ [mkDecl lName lValue] @ kDecls @ [outDecl], { Names = names; N = n })

/// Row slices of a single-leading-axis array: A(0) .. A(d-1).
let private rowSlices (aName: string) (d: int) : Expr list =
    [ for i in 0 .. d - 1 -> appE (v aName) [iLit i] ]

/// The order-r cumulant cell at `labels`: Sum over set partitions pi of [r]: (-1)^(|pi|-1)(|pi|-1)! * Prod_B E[Prod x_B].
let private cumulantCellExpr (pool: PoolInfo) (labels: int[]) (r: int) : Expr =
    let terms =
        setPartitions r |> List.map (fun p ->
            let b = p.Length
            let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
            p |> List.fold (fun acc blk ->
                mulE acc (poolMoment pool (blk |> List.map (fun pos -> labels.[pos])))) (fLit w))
    terms |> List.reduce addE

// Module context: array annotations, alias resolution, static extents

/// Array annotations in scope: name -> (element TypeExpr, index TypeExprs); only annotated bindings participate, formers never infer a shape.
let private collectArrays (decls: Located<Decl> list) : Map<string, TypeExpr * TypeExpr list> =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclLet b | DeclStatic b ->
            match b.Pattern, b.Type with
            | { Kind = PatternKind.PatVar name }, Some (TyArray (elem, idxs)) -> Map.add name (elem, idxs) acc
            | _ -> acc
        | _ -> acc) Map.empty

let private collectAliases (decls: Located<Decl> list) : Map<string, TypeExpr> =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclType (TyDeclAlias (name, _, body)) -> Map.add name body acc
        | _ -> acc) Map.empty

/// Resolve an index TypeExpr to its static extent, following alias chains.
let rec private resolveExtent (aliases: Map<string, TypeExpr>) (statics: StaticEnv) (ty: TypeExpr) : int option =
    match ty with
    | TyIdx extent ->
        match evalExpr statics maxSteps extent with
        | Ok (SVInt n) -> Some (int n)
        | _ -> None
    | TyNamed (name, []) ->
        match Map.tryFind name aliases with
        | Some body -> resolveExtent aliases statics body
        // Not a source alias: a provider axis path, registered by TypeEnv during checking, so the extent comes from store metadata.
        | None -> providerIndexExtent statics name
    | _ -> None

/// Shape inference for COMPUTED source arrays: an un-annotated module-level `let A = method_for(range<...>) <@> kernel
/// [|> compute]` has no declared TyArray, but its iteration space IS its shape -- one axis per range slot, halo slots shrunk
/// to their interior (interior = n - (hi - lo)). Unresolvable slots return None; element type Float64.
let private computedShapeOf (aliases: Map<string, TypeExpr>) (statics: StaticEnv) (value: Expr) : (TypeExpr * TypeExpr list) option =
    // Interior extents of one halo slot: flat offsets -> [n - reach]; nested per-axis form -> one shrunk extent per axis.
    let haloExtents (innerTy: TypeExpr) (offsetsExpr: Expr) : int list option =
        resolveExtent aliases statics innerTy |> Option.bind (fun n ->
            let asInt = function SVInt v -> Some (int v) | _ -> None
            let shrunk (offs: int list) =
                if List.isEmpty offs then None
                else
                    let lo = min 0 (List.min offs)
                    let hi = max 0 (List.max offs)
                    Some (n - (hi - lo))
            match evalExpr statics maxSteps offsetsExpr with
            | Ok (SVInt v) -> shrunk [int v] |> Option.map List.singleton
            | Ok (SVTuple vs) when not vs.IsEmpty ->
                let flat = vs |> List.map asInt
                if List.forall Option.isSome flat then
                    // Flat form: one axis.
                    shrunk (flat |> List.map Option.get) |> Option.map List.singleton
                else
                    // Nested per-axis form: every entry a non-empty int array.
                    let perAxis =
                        vs |> List.map (function
                            | SVTuple xs ->
                                let os = xs |> List.map asInt
                                if List.forall Option.isSome os && not os.IsEmpty
                                then shrunk (os |> List.map Option.get) else None
                            | _ -> None)
                    if List.forall Option.isSome perAxis
                    then Some (perAxis |> List.map Option.get) else None
            | _ -> None)
    let slotExtents (ty: TypeExpr) : int list option =
        match ty with
        | TyHalo (inner, offs) -> haloExtents inner offs
        | _ -> resolveExtent aliases statics ty |> Option.map List.singleton
    // Unwrap |> compute and walk down the <@> application to the loop source.
    let rec sourceOf (e: Expr) : Expr option =
        match e.Kind with
        | ExprKind.ExprCompute inner -> sourceOf inner
        | ExprKind.ExprBinOp (_, OpApply, l, _) -> sourceOf l
        | ExprKind.ExprMethodFor [src] -> Some src
        | _ -> None
    sourceOf value |> Option.bind (fun src ->
        let slots =
            match src.Kind with
            | ExprKind.ExprRange idxTys -> Some idxTys
            | ExprKind.ExprHalo (inner, offs) -> Some [TyHalo (inner, offs)]
            | _ -> None
        slots |> Option.bind (fun tys ->
            if List.isEmpty tys then None else
            let exts = tys |> List.map slotExtents
            if List.forall Option.isSome exts then
                let idxs = exts |> List.collect Option.get |> List.map (fun n -> TyIdx (iLit n))
                Some (TyNamed ("Float64", []), idxs)
            else None))

// Misplaced-use detection: formers are decl-RHS only.
let rec private anyExpr (p: Expr -> bool) (e: Expr) : bool =
    if p e then true else
    let any = anyExpr p
    match e.Kind with
    | ExprKind.ExprBinOp (_, _, l, r) -> any l || any r
    | ExprKind.ExprUnaryOp (_, x) -> any x
    | ExprKind.ExprApp (f, args) -> any f || List.exists any args
    | ExprKind.ExprTupleIndex (t, i) -> any t || any i
    | ExprKind.ExprField (o, _) -> any o
    | ExprKind.ExprLambda (_, _, b) -> any b
    | ExprKind.ExprLet (bind, body) -> any bind.Value || any body
    | ExprKind.ExprMatch (s, cases) -> any s || (cases |> List.exists (fun c -> any c.Body || (c.Guard |> Option.map any |> Option.defaultValue false)))
    | ExprKind.ExprIf (c, t, f) -> any c || any t || any f
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprZip es | ExprKind.ExprStack es | ExprKind.ExprSequence es -> List.exists any es
    | ExprKind.ExprJoin (es, _) -> List.exists any es
    | ExprKind.ExprBlock (stmts, fin) ->
        let stmtAny s =
            let rec go s =
                match s with
                | StmtSpanned (inner, _) -> go inner
                | StmtLet b -> any b.Value
                | StmtAssign (l, _, r) -> any l || any r
                | StmtExpr x -> any x
                | StmtForIn (_, r, body) -> any r || List.exists go body
            go s
        List.exists stmtAny stmts || (fin |> Option.map any |> Option.defaultValue false)
    | ExprKind.ExprStruct (_, fields, spread) ->
        fields |> List.exists (snd >> any) || (spread |> Option.map any |> Option.defaultValue false)
    | ExprKind.ExprTyped (x, _) -> any x
    | ExprKind.ExprMethodFor arrays -> List.exists any arrays
    | ExprKind.ExprObjectFor k -> any k
    | ExprKind.ExprAlign (es, _) -> List.exists any es
    | ExprKind.ExprPure x | ExprKind.ExprCompute x | ExprKind.ExprRead x | ExprKind.ExprUnique x | ExprKind.ExprRank x | ExprKind.ExprExtents x -> any x
    | ExprKind.ExprGuard (c, b) -> any c || any b
    | ExprKind.ExprReplicate (c, b) -> any c || any b
    | ExprKind.ExprMask (a, pr) | ExprKind.ExprCompound (a, pr) | ExprKind.ExprSparse (a, pr) | ExprKind.ExprGroupBy (a, pr)
    | ExprKind.ExprIntersect (a, pr) | ExprKind.ExprUnion (a, pr) | ExprKind.ExprContains (a, pr)
    | ExprKind.ExprSort (a, pr) | ExprKind.ExprGram (a, pr) -> any a || any pr
    | ExprKind.ExprReduce (a, k, i, _) -> any a || any k || (i |> Option.map any |> Option.defaultValue false)
    | ExprKind.ExprAssign (l, r) -> any l || any r
    | _ -> false

let private isFormerCallOf (activeNames: Set<string>) (e: Expr) =
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar n }, _) -> Set.contains n activeNames
    | _ -> false

// Struct-declared independence (formalism Sec 17.13.2): `struct S { X: Array<...>, Y: Array<...> } where indep(X, Y)` --
// indep is a static license, comm's sibling, not a runtime proposition. Conjuncts are stripped from construction-time
// validate() into the independence relation; residual invariants stay runtime-checked.

/// Split a struct where-invariant into indep(...) conjuncts and the residual expression (None when indep was the whole invariant).
let rec private splitInvariant (e: Expr) : (string * string) list * Expr option =
    match e.Kind with
    | ExprKind.ExprBinOp (m, OpAnd, l, r) ->
        let (il, le) = splitInvariant l
        let (ir, re) = splitInvariant r
        let residual =
            match le, re with
            | Some a, Some b -> Some (inheritSpan e (ExprBinOp (m, OpAnd, a, b)))
            | Some a, None | None, Some a -> Some a
            | None, None -> None
        (il @ ir, residual)
    // Normalized from `where <alias>.indep(X, Y)` by stripQualified.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "__ppl_indep" }, [{ Kind = ExprKind.ExprVar x }; { Kind = ExprKind.ExprVar y }]) -> ([(x, y)], None)
    | _ -> ([], Some e)

/// Deterministic alias binding name for a struct-field array path m.f -- formers iterate named bindings (method_for over a raw
/// field access doesn't reach codegen's binding-keyed loop machinery), so field paths normalize to `let __ppl_arr_m_f = m.f`.
let private aliasOf (m: string) (f: string) = $"__ppl_arr_{m}_{f}"

// Former elaboration
type private Ctx = {
    Arrays: Map<string, TypeExpr * TypeExpr list>
    Aliases: Map<string, TypeExpr>
    Statics: StaticEnv
    /// Unordered independence relation over array names.
    Indep: Set<string * string>
    /// Single-array pools already emitted this module (source array name -> handle); later formers over the same array reuse the sweep.
    Pools: Map<string, PoolInfo> ref
    /// Pre-scanned maximal multiset size each source array needs across all its formers, so the first former emits one maximal pool.
    PoolMax: Map<string, int>
    /// Pool-path former outputs that are FLAT lex SymIdx<2, d>-shaped tensors (binding name -> variable-axis extent d); consumers
    /// like comoments_merge switch to cell-wise flat reads for these.
    FlatDims: Map<string, int> ref
}

/// The module's pool for a single-axis source array: reuse if already emitted, else emit ONE maximal sweep (sizes 1..max over
/// every former that reads this array) at the first former's position and cache it.
let private acquirePool (ctx: Ctx) (span: Span) (aName: string) (d: int) (n: float) (selfMax: int)
    : Located<Decl> list * PoolInfo =
    match Map.tryFind aName ctx.Pools.Value with
    | Some pool -> ([], pool)
    | None ->
        let maxR = max selfMax (Map.tryFind aName ctx.PoolMax |> Option.defaultValue selfMax)
        let needed = [ for p in 1 .. maxR do yield! canonicalTuples d p ]
        let (pd, pool) = poolDecls span aName (rowSlices aName d) needed n
        ctx.Pools.Value <- Map.add aName pool ctx.Pools.Value
        (pd, pool)

let private indepKey (a: string) (b: string) = if a <= b then (a, b) else (b, a)

/// Shape info the formers need: leading axes, fiber axis, static N.
let private arrayShape (ctx: Ctx) (what: string) (name: string) : Result<TypeExpr * TypeExpr list * TypeExpr * int, string> =
    match Map.tryFind name ctx.Arrays with
    | None ->
        Error $"{what}: '{name}' must be a module-level let with an Array<Elem like ..., SampleIdx> annotation (the formers read the declared shape)"
    | Some (_, idxs) when idxs.Length < 2 ->
        Error $"{what}: '{name}' needs at least one variable axis plus the sample axis (Array<Elem like VarIdx, SampleIdx>); a lone sample axis has no comoment structure"
    | Some (elem, idxs) ->
        let fiber = List.last idxs
        let leading = idxs |> List.take (idxs.Length - 1)
        match resolveExtent ctx.Aliases ctx.Statics fiber with
        | None -> Error $"{what}: '{name}' sample axis extent must be statically known (Idx<n> directly or through aliases)"
        | Some n -> Ok (elem, leading, fiber, n)

/// moments(A, k): raw order-k comoment former.
let private elabMoments (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar aName }; kExpr] ->
        let k =
            match evalExpr ctx.Statics maxSteps kExpr with
            | Ok (SVInt n) when n >= 1L -> Ok (int n)
            | Ok (SVInt n) -> Error $"moments: order must be >= 1, got {n}"
            | _ -> Error "moments: the order must be a compile-time integer (a literal, `let static`, or static-function call)"
        k |> Result.bind (fun k ->
        arrayShape ctx "moments" aName |> Result.map (fun (elem, leading, fiber, n) ->
            match leading with
            | [ix] when (resolveExtent ctx.Aliases ctx.Statics ix).IsSome ->
                // Single-pass path: mu_S = P_S / N from the shared pool sweep.
                let d = (resolveExtent ctx.Aliases ctx.Statics ix).Value
                let (pd, pool) = acquirePool ctx span aName d (float n) k
                let cells = [ for labels in canonicalTuples d k -> poolMoment pool labels ]
                pd @ [ { Value = DeclLet { binding with Value = arrLitE cells }; Span = span } ]
            | _ ->
                // Multiaxis / unresolvable leading extent: per-cell pipeline.
                let paramNames = [ for i in 1 .. k -> $"__x{i}" ]
                let ps = paramNames |> List.map (fun p -> { Name = p; Type = Some (TyArray (elem, [fiber])); Default = None; NameSpan = noSpan })
                let whereC = if k >= 2 then commWhere paramNames else None
                let body = divE (prodsumE (paramNames |> List.map v)) (fLit (float n))
                let lName = $"__ppl_L_{outName}"
                let kName = $"__ppl_k_{outName}"
                let mk value = { Pattern = pvar ""; Type = None; Value = value; Mutability = BindLet }
                [ { Value = DeclLet { mk (methodForE (List.replicate k (v aName))) with Pattern = pvar lName }; Span = span }
                  { Value = DeclLet { mk (lambdaE ps whereC body) with Pattern = pvar kName }; Span = span }
                  { Value = DeclLet { binding with Value = computeE (applyE (v lName) (v kName)) }; Span = span } ]))
    | _ ->
        Error "moments expects moments(A, k): an annotated module-level array and a static order"

/// Central pair kernel body: E[ab] - ma*mb, spelled over reduce/prodsum (both proven kernel-position primitives).
let private centralPairBody (n: float) =
    syn (ExprBlock (
        [ sLet "__ma" (meanE (v "__x1") n)
          sLet "__mb" (meanE (v "__x2") n) ],
        Some (subE (divE (prodsumE [v "__x1"; v "__x2"]) (fLit n)) (mulE (v "__ma") (v "__mb")))))

/// comoments(A, 2) same-array | comoments(X, Y) cross-block.
let private elabComoments (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    let lName = $"__ppl_L_{outName}"
    let kName = $"__ppl_k_{outName}"
    let mkDecl pat value = { Value = DeclLet { Pattern = pvar pat; Type = None; Value = value; Mutability = BindLet }; Span = span }
    match args with
    // Same-array central comoment of static order (only 2 for now)
    | [{ Kind = ExprKind.ExprVar aName }; kExpr] when (match evalExpr ctx.Statics maxSteps kExpr with Ok (SVInt _) -> true | _ -> false) ->
        match evalExpr ctx.Statics maxSteps kExpr with
        | Ok (SVInt 2L) ->
            arrayShape ctx "comoments" aName |> Result.map (fun (elem, leading, fiber, n) ->
                match leading with
                | [ix] when (resolveExtent ctx.Aliases ctx.Statics ix).IsSome ->
                    // Single-pass path: C_ij = P_ij/N - (P_i/N)(P_j/N) off the shared pool.
                    let d = (resolveExtent ctx.Aliases ctx.Statics ix).Value
                    let (pd, pool) = acquirePool ctx span aName d (float n) 2
                    ctx.FlatDims.Value <- Map.add outName d ctx.FlatDims.Value
                    let cells =
                        [ for labels in canonicalTuples d 2 ->
                            match labels with
                            | [i; j] -> subE (poolMoment pool labels) (mulE (poolMoment pool [i]) (poolMoment pool [j]))
                            | _ -> fLit 0.0 ]
                    pd @ [ { Value = DeclLet { binding with Value = arrLitE cells }; Span = span } ]
                | _ ->
                    let ps = ["__x1"; "__x2"] |> List.map (fun p -> { Name = p; Type = Some (TyArray (elem, [fiber])); Default = None; NameSpan = noSpan })
                    [ mkDecl lName (methodForE [v aName; v aName])
                      mkDecl kName (lambdaE ps (commWhere ["__x1"; "__x2"]) (centralPairBody (float n)))
                      { Value = DeclLet { binding with Value = computeE (applyE (v lName) (v kName)) }; Span = span } ])
        | _ ->
            Error "comoments: only order 2 (covariance) is supported so far; higher central orders await the subset-lattice expansion over prodsums"
    // Cross block between two distinct arrays
    | [{ Kind = ExprKind.ExprVar xName }; { Kind = ExprKind.ExprVar yName }] ->
        if xName = yName then
            Error "comoments(X, X): use comoments(X, 2) for the same-array (packed) form"
        else
        arrayShape ctx "comoments" xName |> Result.bind (fun (elemX, leadX, fibX, nX) ->
        arrayShape ctx "comoments" yName |> Result.bind (fun (elemY, leadY, fibY, nY) ->
            if nX <> nY then
                Error $"comoments: '{xName}' and '{yName}' sample axes disagree ({nX} vs {nY})"
            elif Set.contains (indepKey xName yName) ctx.Indep then
                // Declared independent: the central cross block is structurally zero -- emit the literal (needs both leading
                // extents statically, single leading axis each).
                match leadX, leadY with
                | [ix], [iy] ->
                    match resolveExtent ctx.Aliases ctx.Statics ix, resolveExtent ctx.Aliases ctx.Statics iy with
                    | Some dx, Some dy ->
                        let zeros = arrLitE (List.replicate dx (arrLitE (List.replicate dy (fLit 0.0))))
                        Ok [ { Value = DeclLet { binding with Value = zeros }; Span = span } ]
                    | _ -> Error $"comoments: independent zero block needs static variable-axis extents for '{xName}' and '{yName}'"
                | _ -> Error "comoments: independent zero blocks support one variable axis per array so far (multi-axis blocks deferred)"
            else
                match leadX, leadY with
                | [ixx], [ixy] when (resolveExtent ctx.Aliases ctx.Statics ixx).IsSome
                                     && (resolveExtent ctx.Aliases ctx.Statics ixy).IsSome ->
                    // Single-pass path: a joint pool over both arrays' rows (X at 0..dx-1, Y at dx..dx+dy-1; one sweep of the
                    // sample axis). Rank-2 output via nested cells. Joint pools are per-former (keyed by output name), not cached.
                    let dx = (resolveExtent ctx.Aliases ctx.Statics ixx).Value
                    let dy = (resolveExtent ctx.Aliases ctx.Statics ixy).Value
                    let rows = rowSlices xName dx @ rowSlices yName dy
                    let needed =
                        [ for i in 0 .. dx - 1 -> [i] ]
                        @ [ for j in 0 .. dy - 1 -> [dx + j] ]
                        @ [ for i in 0 .. dx - 1 do for j in 0 .. dy - 1 -> [i; dx + j] ]
                    let (pd, pool) = poolDecls span outName rows needed (float nX)
                    let cells =
                        arrLitE
                            [ for i in 0 .. dx - 1 ->
                                arrLitE
                                    [ for j in 0 .. dy - 1 ->
                                        subE (poolMoment pool [i; dx + j])
                                             (mulE (poolMoment pool [i]) (poolMoment pool [dx + j])) ] ]
                    Ok (pd @ [ { Value = DeclLet { binding with Value = cells }; Span = span } ])
                | _ ->
                    let px = { Name = "__x1"; Type = Some (TyArray (elemX, [fibX])); Default = None; NameSpan = noSpan }
                    let py = { Name = "__x2"; Type = Some (TyArray (elemY, [fibY])); Default = None; NameSpan = noSpan }
                    Ok [ mkDecl lName (methodForE [v xName; v yName])
                         mkDecl kName (lambdaE [px; py] None (centralPairBody (float nX)))
                         { Value = DeclLet { binding with Value = computeE (applyE (v lName) (v kName)) }; Span = span } ]))
    | _ ->
        Error "comoments expects comoments(A, 2) (same-array covariance) or comoments(X, Y) (cross block)"

// Cumulants: the partition-lattice expander.

/// Order-r cumulant kernel over fiber params __x1..__xr:
///   kappa_r = Sum over set partitions pi of [r]: (-1)^(|pi|-1) (|pi|-1)! * Prod over blocks B: E[Prod_{i in B} x_i]
/// Each distinct block's raw moment E[Prod x_B] = prodsum(x_B)/N is bound once (2^r - 1 lets), shared across Bell(r) terms.
let private cumulantKernelBody (r: int) (n: float) : Expr =
    let blockName (s: int list) = "__m" + (s |> List.map (fun i -> string (i + 1)) |> String.concat "")
    let lets =
        nonemptySubsets r |> List.map (fun s ->
            sLet (blockName s) (divE (prodsumE (s |> List.map (fun i -> v $"__x{i + 1}"))) (fLit n)))
    let terms =
        setPartitions r |> List.map (fun p ->
            let b = p.Length
            let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
            p |> List.fold (fun acc blk -> mulE acc (v (blockName (List.sort blk)))) (fLit w))
    syn (ExprBlock (lets, Some (terms |> List.reduce addE)))

/// The proven three-decl former pipeline over ONE array: L = method_for(A xk); kernel = lambda over annotated fiber params
/// (comm for k >= 2); out = L <@> kernel |> compute.
let private formerPipeline (span: Span) (outName: string) (outBinding: Binding option)
    (aName: string) (elem: TypeExpr) (fiber: TypeExpr) (k: int) (body: Expr) : Located<Decl> list =
    let lName = $"__ppl_L_{outName}"
    let kName = $"__ppl_k_{outName}"
    let paramNames = [ for i in 1 .. k -> $"__x{i}" ]
    let ps = paramNames |> List.map (fun p -> { Name = p; Type = Some (TyArray (elem, [fiber])); Default = None; NameSpan = noSpan })
    let whereC = if k >= 2 then commWhere paramNames else None
    let outValue = computeE (applyE (v lName) (v kName))
    let outDecl =
        match outBinding with
        | Some b -> DeclLet { b with Value = outValue }
        | None -> DeclLet { Pattern = pvar outName; Type = None; Value = outValue; Mutability = BindLet }
    [ { Value = DeclLet { Pattern = pvar lName; Type = None; Value = methodForE (List.replicate k (v aName)); Mutability = BindLet }; Span = span }
      { Value = DeclLet { Pattern = pvar kName; Type = None; Value = lambdaE ps whereC body; Mutability = BindLet }; Span = span }
      { Value = outDecl; Span = span } ]

/// cumulants(A, r): the order-r joint cumulant tensor, SymIdx<r, D> packed.
let private elabCumulants (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar aName }; rExpr] ->
        let r =
            match evalExpr ctx.Statics maxSteps rExpr with
            | Ok (SVInt n) when n >= 1L && n <= 6L -> Ok (int n)
            | Ok (SVInt n) -> Error $"cumulants: order must be in 1..6 (got {n}) -- Bell-number kernel growth beyond that needs the shared-subexpression pass"
            | _ -> Error "cumulants: the order must be a compile-time integer (a literal, `let static`, or static-function call)"
        r |> Result.bind (fun r ->
        arrayShape ctx "cumulants" aName |> Result.map (fun (elem, leading, fiber, n) ->
            match leading with
            | [ix] when (resolveExtent ctx.Aliases ctx.Statics ix).IsSome ->
                // Single-pass path: the shared pool sweep (one sample-axis traversal instead of one prodsum loop per block per cell);
                // kappa_r cells as straight-line partition sums over pool reads.
                let d = (resolveExtent ctx.Aliases ctx.Statics ix).Value
                let (pd, pool) = acquirePool ctx span aName d (float n) r
                let cells =
                    [ for labels in canonicalTuples d r ->
                        cumulantCellExpr pool (List.toArray labels) r ]
                pd @ [ { Value = DeclLet { binding with Value = arrLitE cells }; Span = span } ]
            | _ ->
                // Multiaxis / unresolvable leading extent: per-cell pipeline (fused leading axes, kernel prodsum lets).
                formerPipeline span outName (Some binding) aName elem fiber r (cumulantKernelBody r (float n))))
    | _ ->
        Error "cumulants expects cumulants(A, r): an annotated module-level array and a static order"

// The Dist tower: a dist is a compile-time object -- the binding is consumed and its cumulant components materialize as
// packed arrays.
//   dist(A, r)        kappa_1..kappa_r pipelines from data
//   dist_add(d1, d2)  per-order tensor addition -- requires declared independence of every source-array pair (cumulants of a
//                     sum add exactly iff independent)
//   dist_scale(c, d)  kappa_k scaled by c^k (multilinearity)
//   cumulant(d, k)    the order-k component, bound as an ordinary array

type private DistInfo = {
    Order: int
    /// Component binding name per order (index k-1 -> kappa_k array).
    Components: string list
    /// Underlying data arrays, for the independence requirement.
    Sources: Set<string>
    /// Variable-space dimension override: pushforward results carry their output dimension here (scalar jet results = Some 1);
    /// None = derive it from the single source array's annotation (distDim).
    Dim: int option
    /// Components stored FLAT (lex-canonical ArrayLits read by offset) instead of method_for-packed (logical multi-index reads).
    Flat: bool
}

let private distComponentName (dName: string) (k: int) = $"__dist_{dName}_k{k}"

/// The binding that makes a dist a VALUE: `let d = __dist_pack(k1, ..., kr)`. The checker types the intrinsic as
/// Dist<r, tau like axes> (nominal), so `d` is first-class: it crosses function boundaries and cumulant(d, k) projects anywhere.
let private distPackDecl (span: Span) (dName: string) (info: DistInfo) : Located<Decl> =
    { Value = DeclLet { Pattern = pvar dName; Type = None
                        Value = appE (v "__dist_pack") (info.Components |> List.map v)
                        Mutability = BindLet }
      Span = span }

let private elabDist (ctx: Ctx) (span: Span) (dName: string) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar aName }; rExpr] ->
        let r =
            match evalExpr ctx.Statics maxSteps rExpr with
            | Ok (SVInt n) when n >= 1L && n <= 6L -> Ok (int n)
            | _ -> Error "dist: the order must be a compile-time integer in 1..6"
        r |> Result.bind (fun r ->
        arrayShape ctx "dist" aName |> Result.map (fun (elem, _leading, fiber, n) ->
            // The tower, fused: per order a loop object and a cumulant kernel, then ONE compute over the <&>-chain, destructured
            // into the per-order components -- a single deferred computation owns the whole tower.
            let comps = [ for k in 1 .. r -> distComponentName dName k ]
            let lName k = $"__ppl_L_{dName}_k{k}"
            let kName k = $"__ppl_k_{dName}_k{k}"
            let stageDecls =
                [ for k in 1 .. r do
                    let paramNames = [ for i in 1 .. k -> $"__x{i}" ]
                    let ps = paramNames |> List.map (fun p -> { Name = p; Type = Some (TyArray (elem, [fiber])); Default = None; NameSpan = noSpan })
                    let whereC = if k >= 2 then commWhere paramNames else None
                    yield { Value = DeclLet { Pattern = pvar (lName k); Type = None; Value = methodForE (List.replicate k (v aName)); Mutability = BindLet }; Span = span }
                    yield { Value = DeclLet { Pattern = pvar (kName k); Type = None; Value = lambdaE ps whereC (cumulantKernelBody k (float n)); Mutability = BindLet }; Span = span } ]
            let applied = [ for k in 1 .. r -> applyE (v (lName k)) (v (kName k)) ]
            let fusedVal =
                match applied with
                | [one] -> computeE one
                | first :: restA -> computeE (restA |> List.fold (fun acc e -> syn (ExprBinOp (Elementwise, OpParallel, acc, e))) first)
                | [] -> computeE (v aName)  // unreachable: r >= 1
            let outPat =
                match comps with
                | [one] -> pvar one
                | _ -> ptuple (comps |> List.map pvar)
            let fusedDecl = { Value = DeclLet { Pattern = outPat; Type = None; Value = fusedVal; Mutability = BindLet }; Span = span }
            (stageDecls @ [fusedDecl], { Order = r; Components = comps; Sources = Set.singleton aName; Dim = None; Flat = false })))
    | _ ->
        Error "dist expects dist(A, r): an annotated module-level array and a static order"

/// Shared body of dist addition/subtraction: c_k = a_k + weight(k)*b_k. Addition is weight = 1; subtraction is weight k = (-1)^k
/// (kappa_k(-Y) = (-1)^k kappa_k(Y)). Exact only for independent operands.
let private elabDistCombine (opName: string) (weight: int -> float) (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar n1 }; { Kind = ExprKind.ExprVar n2 }] ->
        match Map.tryFind n1 dists, Map.tryFind n2 dists with
        | Some d1, Some d2 when d1.Order = d2.Order ->
            let missing =
                [ for s1 in d1.Sources do
                    for s2 in d2.Sources do
                      if not (Set.contains (indepKey s1 s2) ctx.Indep) then yield (s1, s2) ]
            match missing with
            | (s1, s2) :: _ ->
                Error $"dist {opName}: cumulants combine only for independent distributions -- declare independence of {s1} and {s2} (loose `let _ = ppl.independent(...)` or a struct `where ppl.indep(...)`)"
            | [] ->
                let decls =
                    [ for k in 1 .. d1.Order ->
                        let outN = distComponentName dName k
                        let contrib =
                            if weight k = 1.0 then v "__w"
                            else mulE (fLit (weight k)) (v "__w")
                        { Value = DeclLet { Pattern = pvar outN; Type = None
                                            Value = zipMap2 (v d1.Components.[k - 1]) (v d2.Components.[k - 1]) (addE (v "__u") contrib)
                                            Mutability = BindLet }
                          Span = span } ]
                let info = { Order = d1.Order
                             Components = [ for k in 1 .. d1.Order -> distComponentName dName k ]
                             Sources = Set.union d1.Sources d2.Sources
                             Dim = (if d1.Dim.IsSome then d1.Dim else d2.Dim)
                             Flat = d1.Flat || d2.Flat }
                Ok (decls, info)
        | Some d1, Some d2 ->
            Error $"dist {opName}: orders disagree ({d1.Order} vs {d2.Order}) -- carry the same stochastic order on both sides"
        | _ ->
            Error $"dist {opName} expects two previously declared dist(...) bindings"
    | _ ->
        Error $"dist {opName} expects two dist operands"

let private elabDistScale (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [cExpr; { Kind = ExprKind.ExprVar dn }] ->
        match Map.tryFind dn dists with
        | Some d ->
            // kappa_k(c*X) = c^k kappa_k(X): multilinearity, spelled as k repeated multiplications so c may be any pure scalar expr.
            let decls =
                [ for k in 1 .. d.Order ->
                    let outN = distComponentName dName k
                    let scaled = List.replicate k cExpr |> List.fold mulE (v "__u")
                    { Value = DeclLet { Pattern = pvar outN; Type = None
                                        Value = map1 (v d.Components.[k - 1]) scaled
                                        Mutability = BindLet }
                      Span = span } ]
            let info = { d with Components = [ for k in 1 .. d.Order -> distComponentName dName k ] }
            Ok (decls, info)
        | None ->
            Error "dist_scale expects dist_scale(c, d) with a previously declared dist binding d"
    | _ ->
        Error "dist_scale expects dist_scale(c, d)"

// Streaming merge: a growing input is a stream, and the merge monoid IS the semantics of "the file got longer".

/// comoments_merge(cA, mA, nA, cB, mB, nB): combine two chunks' pair comoments and means into the whole's -- pooled-covariance
/// identity (k=2 Pebay/Chan): C = (nA*CA + nB*CB)/n + (nA*nB/n^2)*delta*delta^T, delta = mB-mA, n = nA+nB. Chunk sizes are
/// static; the correction is a packed symmetric outer square, giving the same SymIdx<2, D> storage as inputs.
let private elabComomentsMerge (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar cA }; { Kind = ExprKind.ExprVar mA }; nAExpr; { Kind = ExprKind.ExprVar cB }; { Kind = ExprKind.ExprVar mB }; nBExpr] ->
        let staticN what e =
            match evalExpr ctx.Statics maxSteps e with
            | Ok (SVInt n) when n >= 1L -> Ok (float n)
            | _ -> Error $"comoments_merge: {what} must be a compile-time chunk size >= 1"
        staticN "nA" nAExpr |> Result.bind (fun nA ->
        staticN "nB" nBExpr |> Result.map (fun nB ->
            let n = nA + nB
            let deltaN = $"__ppl_delta_{outName}"
            let ddLN = $"__ppl_ddL_{outName}"
            let ddKN = $"__ppl_ddk_{outName}"
            let ddN = $"__ppl_dd_{outName}"
            let mkDecl pat value = { Value = DeclLet { Pattern = pvar pat; Type = None; Value = value; Mutability = BindLet }; Span = span }
            // delta = mB - mA (lockstep over the mean vectors)
            let deltaDecl = mkDecl deltaN (zipMap2 (v mA) (v mB) (subE (v "__w") (v "__u")))
            match Map.tryFind cA ctx.FlatDims.Value, Map.tryFind cB ctx.FlatDims.Value with
            | Some dA, Some dB when dA = dB ->
                // Flat inputs (pool-path comoments / earlier flat merges): fully cell-wise merge, delta*delta^T inlined per cell.
                let d = dA
                let dRead i = appE (v deltaN) [iLit i]
                let cells = canonicalTuples d 2
                let merged =
                    arrLitE
                        [ for k in 0 .. cells.Length - 1 ->
                            let (i, j) = (match cells.[k] with [i; j] -> (i, j) | _ -> (0, 0))
                            addE
                                (divE (addE (mulE (fLit nA) (appE (v cA) [iLit k]))
                                            (mulE (fLit nB) (appE (v cB) [iLit k]))) (fLit n))
                                (mulE (fLit (nA * nB / (n * n))) (mulE (dRead i) (dRead j))) ]
                ctx.FlatDims.Value <- Map.add outName d ctx.FlatDims.Value
                [ deltaDecl
                  { Value = DeclLet { binding with Value = merged }; Span = span } ]
            | _ ->
                // delta*delta^T as a packed symmetric outer square (scalar comm kernel)
                let ddL = mkDecl ddLN (methodForE [v deltaN; v deltaN])
                let ddK = mkDecl ddKN (lambdaE [{ Name = "__a"; Type = None; Default = None; NameSpan = noSpan }; { Name = "__b"; Type = None; Default = None; NameSpan = noSpan }]
                                               (commWhere ["__a"; "__b"])
                                               (mulE (v "__a") (v "__b")))
                let dd = mkDecl ddN (computeE (applyE (v ddLN) (v ddKN)))
                // merged = (nA*CA + nB*CB)/n + (nA*nB/n^2)*delta*delta^T, three-way lockstep
                let body =
                    addE
                        (divE (addE (mulE (fLit nA) (v "__ca")) (mulE (fLit nB) (v "__cb"))) (fLit n))
                        (mulE (fLit (nA * nB / (n * n))) (v "__dd"))
                let merged =
                    computeE (applyE
                        (methodForE [syn (ExprZip [v cA; v cB; v ddN])])
                        (lambdaE [{ Name = "__ca"; Type = None; Default = None; NameSpan = noSpan }; { Name = "__cb"; Type = None; Default = None; NameSpan = noSpan }; { Name = "__dd"; Type = None; Default = None; NameSpan = noSpan }] None body))
                [ deltaDecl; ddL; ddK; dd
                  { Value = DeclLet { binding with Value = merged }; Span = span } ]))
    | _ ->
        Error "comoments_merge expects comoments_merge(cA, mA, nA, cB, mB, nB): two chunks' pair comoments, means, and static sizes"

// Arbitrary-order streaming state: the Pebay generalization of the k = 2 merge. State = (n static, mean vector, central
// comoment SUMS M_2..M_r). Merge, for every canonical entry S with delta = meanB - meanA, cA = -nB/n, cB = nA/n:
//   M'_S = Sum_{K subset S, |S\K|<>1} M_{S\K}(A)*Prod_{k in K}(cA*delta_k)
//                                    + M_{S\K}(B)*Prod_{k in K}(cB*delta_k)
// with M_empty = n_side and M_single = 0 (pruned). Everything is static, so merge and finalize generate cell-wise
// straight-line code: packed method_for tensors read logically, merge-emitted flat arrays read at the lex offset.
let private lexOffsetOf (d: int) (p: int) (labels: int list) : int =
    canonicalTuples d p |> List.findIndex (fun t -> t = List.sort labels)

type private MStateInfo = {
    Order: int
    Dim: int
    /// Static observation count, carried through merges.
    N: float
    Mean: string
    /// M tensor binding name per rank p (index p-2), p = 2..Order.
    Ms: string list
    /// True: method_for-packed tensors (logical reads). False: merge-emitted flat arrays in lex cell order (offset reads).
    Packed: bool
}

let private mstateComponent (sName: string) (what: string) = $"__mst_{sName}_{what}"

/// Read M_S for |S| >= 2 from a state, representation-aware.
let private mReadExpr (info: MStateInfo) (labels: int list) : Expr =
    let p = labels.Length
    let name = info.Ms.[p - 2]
    if info.Packed then appE (v name) (labels |> List.map iLit)
    else appE (v name) [iLit (lexOffsetOf info.Dim p labels)]

let private elabMState (ctx: Ctx) (span: Span) (sName: string) (args: Expr list)
    : Result<Located<Decl> list * MStateInfo, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar aName }; rExpr] ->
        let r =
            match evalExpr ctx.Statics maxSteps rExpr with
            | Ok (SVInt x) when x >= 2L && x <= 6L -> Ok (int x)
            | _ -> Error "mstate: the order must be a compile-time integer in 2..6"
        r |> Result.bind (fun r ->
        arrayShape ctx "mstate" aName |> Result.bind (fun (elem, leading, fiber, n) ->
            match leading with
            | [ix] ->
                match resolveExtent ctx.Aliases ctx.Statics ix with
                | Some d ->
                    // Single-pass path: the shared pool sweep, then mean and every central comoment SUM as straight-line cells:
                    // M_S = Sum_{K subset S} (-1)^|K| Prod_{i in K} mu_i * P_{S\K}, P_empty = n. State components are FLAT lex
                    // ArrayLits (Packed = false), same representation merge outputs carry.
                    let (pd, pool) = acquirePool ctx span aName d (float n) r
                    let meanN = mstateComponent sName "mean"
                    let mN p = mstateComponent sName $"m{p}"
                    let mkDecl name value = { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
                    let meanDecl = mkDecl meanN (arrLitE [ for i in 0 .. d - 1 -> poolMoment pool [i] ])
                    let mDecls =
                        [ for p in 2 .. r ->
                            let cells =
                                [ for labels in canonicalTuples d p ->
                                    let labArr = List.toArray labels
                                    let terms =
                                        [ for mask in 0 .. (1 <<< p) - 1 ->
                                            let inK = [ for i in 0 .. p - 1 do if (mask >>> i) &&& 1 = 1 then yield labArr.[i] ]
                                            let rest = [ for i in 0 .. p - 1 do if (mask >>> i) &&& 1 = 0 then yield labArr.[i] ]
                                            let sign = if inK.Length % 2 = 0 then 1.0 else -1.0
                                            let ps = if rest.IsEmpty then fLit (float n) else poolRead pool rest
                                            let muProd = inK |> List.fold (fun acc i -> mulE acc (poolMoment pool [i])) (fLit sign)
                                            mulE muProd ps ]
                                    terms |> List.reduce addE ]
                            mkDecl (mN p) (arrLitE cells) ]
                    Ok (pd @ [meanDecl] @ mDecls, { Order = r; Dim = d; N = float n; Mean = meanN; Ms = [ for p in 2 .. r -> mN p ]; Packed = false })
                | None -> Error "mstate: the variable-axis extent must be statically known"
            | _ -> Error "mstate: one variable axis per array so far (multi-axis states deferred)"))
    | _ ->
        Error "mstate expects mstate(A, r): an annotated module-level array and a static order in 2..6"

let private elabMStateMerge (ctx: Ctx) (span: Span) (outName: string)
    (mstates: Map<string, MStateInfo>) (args: Expr list)
    : Result<Located<Decl> list * MStateInfo, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar sa }; { Kind = ExprKind.ExprVar sb }] ->
        match Map.tryFind sa mstates, Map.tryFind sb mstates with
        | Some a, Some b when a.Order = b.Order && a.Dim = b.Dim ->
            let n = a.N + b.N
            let cA = -b.N / n
            let cB = a.N / n
            let deltaN = mstateComponent outName "delta"
            let meanN = mstateComponent outName "mean"
            let mN p = mstateComponent outName $"m{p}"
            let dRead lbl = appE (v deltaN) [iLit lbl]
            let mSide (info: MStateInfo) (labels: int list) =
                if labels.IsEmpty then fLit info.N else mReadExpr info labels
            let mkDecl name value = { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
            let deltaDecl = mkDecl deltaN (zipMap2 (v a.Mean) (v b.Mean) (subE (v "__w") (v "__u")))
            let meanDecl = mkDecl meanN (zipMap2 (v a.Mean) (v b.Mean) (addE (v "__u") (mulE (fLit (b.N / n)) (subE (v "__w") (v "__u")))))
            let mDecls =
                [ for p in 2 .. a.Order ->
                    let cells =
                        [ for labels in canonicalTuples a.Dim p ->
                            let labArr = List.toArray labels
                            let terms =
                                [ for mask in 0 .. (1 <<< p) - 1 do
                                    let inK = [ for i in 0 .. p - 1 do if (mask >>> i) &&& 1 = 1 then yield labArr.[i] ]
                                    let rest = [ for i in 0 .. p - 1 do if (mask >>> i) &&& 1 = 0 then yield labArr.[i] ]
                                    if rest.Length <> 1 then  // M_single = 0: pruned at elaboration
                                        let deltaProd (c: float) =
                                            inK |> List.fold (fun acc lbl -> mulE acc (dRead lbl)) (fLit (c ** float inK.Length))
                                        yield addE (mulE (mSide a rest) (deltaProd cA))
                                                   (mulE (mSide b rest) (deltaProd cB)) ]
                            terms |> List.reduce addE ]
                    mkDecl (mN p) (arrLitE cells) ]
            let info = { Order = a.Order; Dim = a.Dim; N = n; Mean = meanN; Ms = [ for p in 2 .. a.Order -> mN p ]; Packed = false }
            Ok ([deltaDecl; meanDecl] @ mDecls, info)
        | Some a, Some b ->
            Error $"mstate_merge: shapes disagree (order {a.Order} vs {b.Order}, dim {a.Dim} vs {b.Dim})"
        | _ ->
            Error "mstate_merge expects two previously declared mstate(...) bindings"
    | _ ->
        Error "mstate_merge expects mstate_merge(sA, sB)"

/// Freeze a state into cumulant tensors: central mu_p = M_p / n (mu_1 = 0), then the partition formula restricted to
/// partitions with no singleton blocks; kappa_1 is the mean. Destructuring surface: `let (k1, ..., kr) = mstate_cumulants(s)`.
let private elabMStateCumulants (ctx: Ctx) (span: Span) (binding: Binding)
    (mstates: Map<string, MStateInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar sn }] ->
        match Map.tryFind sn mstates with
        | Some s ->
            let compNames =
                match binding.Pattern.Kind with
                | PatternKind.PatTuple pats when pats.Length = s.Order ->
                    let names = pats |> List.map (fun p -> match p.Kind with PatternKind.PatVar nm -> Some nm | _ -> None)
                    if names |> List.forall Option.isSome then Ok (names |> List.map Option.get)
                    else Error "mstate_cumulants: destructure into plain names"
                | _ ->
                    Error $"mstate_cumulants: destructure the result -- `let (k1, ..., k{s.Order}) = mstate_cumulants({sn})`"
            compNames |> Result.map (fun names ->
                let mkDecl name value = { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
                let muE (labels: int list) = divE (mReadExpr s labels) (fLit s.N)
                let kDecl p name =
                    if p = 1 then mkDecl name (v s.Mean)
                    else
                        let cells =
                            [ for labels in canonicalTuples s.Dim p ->
                                let labArr = List.toArray labels
                                let parts =
                                    setPartitions p
                                    |> List.filter (fun pt -> pt |> List.forall (fun blk -> blk.Length >= 2))
                                let terms =
                                    [ for pt in parts ->
                                        let b = pt.Length
                                        let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
                                        pt |> List.fold (fun acc blk ->
                                            mulE acc (muE (blk |> List.map (fun pos -> labArr.[pos])))) (fLit w) ]
                                terms |> List.reduce addE ]
                        mkDecl name (arrLitE cells)
                names |> List.mapi (fun i nm -> kDecl (i + 1) nm))
        | None ->
            Error "mstate_cumulants expects a previously declared mstate(...) binding"
    | _ ->
        Error "mstate_cumulants expects mstate_cumulants(s)"

// The closing formers: moment reconstruction (Wick under closure), mixed cumulant blocks, affine pushforward, and the
// non-crossing (free) lattice. All cell-wise straight-line generation over static shapes, reading dist components with
// logical multi-index subscripts (packed, order >= 2) or plain subscripts (order 1 / flat outputs).

/// Read the order-q cumulant of a registry dist at labels: kappa_1 is a plain rank-1 array; kappa_{q>=2} are method_for-packed
/// (logical reads) unless the dist carries FLAT components (pushforward results), lex-canonical ArrayLits read by offset.
let private distKappaRead (info: DistInfo) (labels: int list) : Expr =
    let q = labels.Length
    if info.Flat && q >= 2 then
        let d = defaultArg info.Dim 1
        appE (v info.Components.[q - 1]) [iLit (lexOffsetOf d q labels)]
    else
        appE (v info.Components.[q - 1]) (labels |> List.map iLit)

/// moments(d, k) on a dist binding: reconstruct the order-k raw moment tensor from carried cumulants -- mu_S = Sum over set
/// partitions of S: Prod_blocks kappa_|B|(labels at B), kappa beyond the carried order = 0 (order-2 closure = Gaussian =
/// Wick's theorem). Exact when k <= carried order; a documented truncation beyond.
let private elabMomentsOfDist (ctx: Ctx) (span: Span) (binding: Binding)
    (info: DistInfo) (dim: int) (kExpr: Expr)
    : Result<Located<Decl> list, string> =
    match evalExpr ctx.Statics maxSteps kExpr with
    | Ok (SVInt kk) when kk >= 1L && kk <= 8L ->
        let k = int kk
        let cells =
            [ for labels in canonicalTuples dim k ->
                let labArr = List.toArray labels
                let parts =
                    setPartitions k
                    |> List.filter (fun pt -> pt |> List.forall (fun blk -> blk.Length <= info.Order))
                let terms =
                    [ for pt in parts ->
                        pt |> List.fold (fun acc blk ->
                            mulE acc (distKappaRead info (blk |> List.map (fun pos -> labArr.[pos])))) (fLit 1.0) ]
                match terms with
                | [] -> fLit 0.0   // every partition needs a block > carried order
                | _ -> terms |> List.reduce addE ]
        Ok [ { Value = DeclLet { binding with Value = arrLitE cells }; Span = span } ]
    | _ -> Error "moments: on a dist, the order must be a compile-time integer in 1..8"

/// The dist's variable dimension: an explicit override (pushforward results) wins; otherwise derived off the order-1
/// component's source array, which needs a dist(A, r) over a single-leading-axis array (same constraint the streaming state has).
let private distDim (ctx: Ctx) (info: DistInfo) : Result<int, string> =
    match info.Dim with
    | Some d -> Ok d
    | None ->
    match Set.toList info.Sources with
    | [one] ->
        match Map.tryFind one ctx.Arrays with
        | Some (_, idxs) when idxs.Length = 2 ->
            match resolveExtent ctx.Aliases ctx.Statics idxs.Head with
            | Some d -> Ok d
            | None -> Error "dist reconstruction: the source array's variable-axis extent must be statically known"
        | _ -> Error "dist reconstruction: the source array needs one variable axis plus the sample axis"
    | _ -> Error "dist reconstruction: supported for single-source dists so far (sums/scales of dists carry derived components; project with cumulant(d, k) instead)"

/// mixed_cumulants(X, Y, p, q): the (p, q) mixed joint-cumulant block -- method_for(X *p, Y *q) with per-array comm groups;
/// the kernel is the same partition sum over all p+q positions. Output is slot-major: packed SymIdx<p, dX> outer, packed
/// SymIdx<q, dY> inner. A declared independent(X, Y) makes every mixed cumulant exactly zero -- a literal zero array.
let private elabMixedCumulants (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar xName }; { Kind = ExprKind.ExprVar yName }; pExpr; qExpr] ->
        let staticOrd what e =
            match evalExpr ctx.Statics maxSteps e with
            | Ok (SVInt x) when x >= 1L && x <= 5L -> Ok (int x)
            | _ -> Error $"mixed_cumulants: {what} must be a compile-time integer in 1..5"
        staticOrd "p" pExpr |> Result.bind (fun p ->
        staticOrd "q" qExpr |> Result.bind (fun q ->
        arrayShape ctx "mixed_cumulants" xName |> Result.bind (fun (elemX, leadX, fibX, nX) ->
        arrayShape ctx "mixed_cumulants" yName |> Result.bind (fun (_elemY, leadY, fibY, nY) ->
            if nX <> nY then
                Error $"mixed_cumulants: '{xName}' and '{yName}' sample axes disagree ({nX} vs {nY})"
            elif Set.contains (indepKey xName yName) ctx.Indep then
                // Structural sparsity at all orders: cumulants factor over independent subalgebras, so any block touching both is 0.
                match leadX, leadY with
                | [ix], [iy] ->
                    match resolveExtent ctx.Aliases ctx.Statics ix, resolveExtent ctx.Aliases ctx.Statics iy with
                    | Some dx, Some dy ->
                        let cellCount =
                            (canonicalTuples dx p |> List.length) * (canonicalTuples dy q |> List.length)
                        let zeros = arrLitE (List.replicate cellCount (fLit 0.0))
                        Ok [ { Value = DeclLet { binding with Value = zeros }; Span = span } ]
                    | _ -> Error "mixed_cumulants: independent zero block needs static variable-axis extents"
                | _ -> Error "mixed_cumulants: independent zero blocks support one variable axis per array so far"
            else
                let r = p + q
                match leadX, leadY with
                | [ixx], [ixy] when (resolveExtent ctx.Aliases ctx.Statics ixx).IsSome
                                     && (resolveExtent ctx.Aliases ctx.Statics ixy).IsSome ->
                    // Single-pass path: one joint pool over both arrays' rows (X at 0..dx-1, Y at dx..dx+dy-1); needed multisets
                    // collected from the cells' actual partition blocks. Output stays slot-major flat (X canonical outer, Y
                    // canonical inner, lex -- matching packed emission order).
                    let dx = (resolveExtent ctx.Aliases ctx.Statics ixx).Value
                    let dy = (resolveExtent ctx.Aliases ctx.Statics ixy).Value
                    let rows = rowSlices xName dx @ rowSlices yName dy
                    let cellLabels =
                        [ for xl in canonicalTuples dx p do
                            for yl in canonicalTuples dy q ->
                              List.toArray (xl @ (yl |> List.map (fun j -> dx + j))) ]
                    let needed =
                        cellLabels |> List.collect (fun labArr ->
                            setPartitions r |> List.collect (fun pt ->
                                pt |> List.map (fun blk -> blk |> List.map (fun pos -> labArr.[pos]) |> List.sort)))
                    let (pd, pool) = poolDecls span outName rows needed (float nX)
                    let cells = [ for labArr in cellLabels -> cumulantCellExpr pool labArr r ]
                    Ok (pd @ [ { Value = DeclLet { binding with Value = arrLitE cells }; Span = span } ])
                | _ ->
                    let lName = $"__ppl_L_{outName}"
                    let kName = $"__ppl_k_{outName}"
                    let xParams = [ for i in 1 .. p -> $"__x{i}" ]
                    let yParams = [ for i in p + 1 .. r -> $"__x{i}" ]
                    let ps =
                        (xParams |> List.map (fun nm -> { Name = nm; Type = Some (TyArray (elemX, [fibX])); Default = None; NameSpan = noSpan }))
                        @ (yParams |> List.map (fun nm -> { Name = nm; Type = Some (TyArray (elemX, [fibY])); Default = None; NameSpan = noSpan }))
                    let commGroups = [ xParams; yParams ] |> List.filter (fun g -> g.Length >= 2)
                    let whereC =
                        if commGroups.IsEmpty then None
                        else Some { Commutativity = commGroups; Antisymmetry = []; Parallel = []; Repro = false; TDims = []; Custom = [] }
                    let mkDecl name value = { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
                    Ok [ mkDecl lName (methodForE ((List.replicate p (v xName)) @ (List.replicate q (v yName))))
                         mkDecl kName (lambdaE ps whereC (cumulantKernelBody r (float nX)))
                         { Value = DeclLet { binding with Value = computeE (applyE (v lName) (v kName)) }; Span = span } ]))))
    | _ ->
        Error "mixed_cumulants expects mixed_cumulants(X, Y, p, q): two annotated arrays and static per-array orders"

/// dist_affine(W, d): multilinearity under a linear map -- kappa'_k = W^{ox k} kappa_k, the exact-linear case of the Faa di
/// Bruno pushforward. W is an annotated m x n module array read at runtime (W(i, j)); the contraction unrolls cell-wise over
/// the static shape. Destructuring surface: `let (p1, ..., pr) = dist_affine(W, d)`.
let private elabDistAffine (ctx: Ctx) (span: Span) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar wName }; { Kind = ExprKind.ExprVar dn }] ->
        match Map.tryFind dn dists with
        | None -> Error "dist_affine expects dist_affine(W, d) with a previously declared dist binding d"
        | Some info ->
            distDim ctx info |> Result.bind (fun n ->
            match Map.tryFind wName ctx.Arrays with
            | Some (_, [im; inn]) ->
                match resolveExtent ctx.Aliases ctx.Statics im, resolveExtent ctx.Aliases ctx.Statics inn with
                | Some m, Some nCols when nCols = n ->
                    let compNames =
                        match binding.Pattern.Kind with
                        | PatternKind.PatTuple pats when pats.Length = info.Order ->
                            let names = pats |> List.map (fun p -> match p.Kind with PatternKind.PatVar nm -> Some nm | _ -> None)
                            if names |> List.forall Option.isSome then Ok (names |> List.map Option.get)
                            else Error "dist_affine: destructure into plain names"
                        | _ -> Error $"dist_affine: destructure the result -- `let (p1, ..., p{info.Order}) = dist_affine({wName}, {dn})`"
                    compNames |> Result.map (fun names ->
                        let wRead i j = appE (v wName) [iLit i; iLit j]
                        // All index tuples over [0, n)^k (order matters for the W factors; kappa reads canonicalize the j-tuple).
                        let rec jTuples k = if k = 0 then [ [] ] else [ for j in 0 .. n - 1 do for rest in jTuples (k - 1) -> j :: rest ]
                        names |> List.mapi (fun ki nm ->
                            let k = ki + 1
                            let cells =
                                [ for iLabels in canonicalTuples m k ->
                                    let iArr = List.toArray iLabels
                                    let terms =
                                        [ for js in jTuples k ->
                                            let jArr = List.toArray js
                                            let wProd =
                                                [ 0 .. k - 1 ]
                                                |> List.fold (fun acc l -> mulE acc (wRead iArr.[l] jArr.[l])) (fLit 1.0)
                                            mulE wProd (distKappaRead info (List.sort js)) ]
                                    terms |> List.reduce addE ]
                            { Value = DeclLet { Pattern = pvar nm; Type = None; Value = arrLitE cells; Mutability = BindLet }; Span = span }))
                | Some _, Some nCols ->
                    Error $"dist_affine: W's column count ({nCols}) must match the dist's dimension ({n})"
                | _ -> Error "dist_affine: W's extents must be statically known"
            | _ -> Error "dist_affine: W must be an annotated module-level mxn array (Array<Elem like Idx<m>, Idx<n>>)")
    | _ ->
        Error "dist_affine expects dist_affine(W, d)"

/// dist_jet(d, q, g0, D1, ..., Ds): the Faa di Bruno pushforward, scalar
/// output -- Y = g(X) for g supplied as its degree-s jet at the dist's
/// mean: g0 = g(mu) (scalar expr), D_k = g^(k)(mu), the rank-k symmetric
/// derivative tensor (scalar when the dist is univariate; else a named
/// C(d+k-1,k)-cell rank-1 array or inline literal, canonical lex order).
/// Y - g0 is the Taylor polynomial in Z = X - mu:
///   central moments of X  = partition sums over kappa, block size >= 2
///   raw moments of Y - g0 = multinomial over jet-degree compositions,
///                           derivative reads * central-moment reads
///   kappa(Y)               = univariate Moebius inversion; kappa_1 shifts by g0
/// Emitted as straight-line scalar lets over the input dist's component
/// reads (derivative values are runtime, the contraction structure is
/// static -- the dist_affine split, one degree higher). Exact for
/// polynomial g of degree <= s when q*s <= the carried order;
/// dist_jet_closed zero-fills cumulants beyond it instead (the moments(d,k)
/// closure convention). Result: a univariate order-q dist, registered with
/// inherited sources and FLAT 1-cell components.
/// VECTOR OUTPUT (mixed-block Faa di Bruno): g : R^dim -> R^m, supplied as
/// g0 = the m-cell vector of g_a(mu) and per-order coordinate-major flat
/// derivative slots (cell (a, tuple) of D_k at offset a*C(dim+k-1,k) +
/// lexOffsetOf). Joint raw moments of Y' = Y - g0 expand over ordered
/// per-factor degree assignments (k_1..k_p) in [1..s]^p with weight
/// Prod_j 1/k_j!; factor j reads D_{k_j} of coordinate a_j against the
/// central moments of all t = Sum k_j input labels; the multivariate
/// Moebius inversion over output positions returns the joint output
/// cumulant tensors, flat in canonical lex order over m (kappa_1 shifts
/// back by g0). Registered Dim = m, Flat = true; m multiplies only the
/// my/component decls by C(m+k-1,k), same q*s <= 8 generation bound.
let private elabDistJetVec (closed: bool) (former: string) (ctx: Ctx) (span: Span) (dName: string)
    (info: DistInfo) (dim: int) (m: int) (qExpr: Expr) (g0Expr: Expr) (dArgs: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
            let qRes =
                match evalExpr ctx.Statics maxSteps qExpr with
                | Ok (SVInt x) when x >= 1L && x <= 6L -> Ok (int x)
                | _ -> Error $"{former}: the output order q must be a compile-time integer in 1..6"
            qRes |> Result.bind (fun q ->
            let s = dArgs.Length
            let tMax = q * s
            if not closed && tMax > info.Order then
                Error $"{former}: computing {q} output cumulants through a degree-{s} jet needs input order {tMax} but the dist carries {info.Order} -- insufficient stochastic order. Carry more or accept the truncation explicitly with {former}_closed(...)"
            elif closed && tMax > 8 then
                Error $"{former}: q*s = {tMax} exceeds the generation bound (8) -- lower the output order or the jet degree"
            else
                let cellsOf k = canonicalTuples dim k |> List.length
                let g0Read (a: int) : Expr =
                    match g0Expr.Kind with
                    | ExprKind.ExprArrayLit cells -> cells.[a]
                    | _ -> appE g0Expr [iLit a]   // named rank-1 array of extent m
                let dReadOf (k: int) (dArg: Expr) : Result<(int -> int list -> Expr option), string> =
                    let want = m * cellsOf k
                    match dArg.Kind with
                    | ExprKind.ExprArrayLit cells when cells.Length = want ->
                        let cellArr = List.toArray cells
                        Ok (fun a labels ->
                            let cell = cellArr.[a * cellsOf k + lexOffsetOf dim k labels]
                            match cell.Kind with
                            | ExprKind.ExprLit (LitFloat 0.0) -> None
                            | _ -> Some cell)
                    | ExprKind.ExprArrayLit cells ->
                        Error $"{former}: vector D{k} needs {want} cells (coordinate-major: m = {m} outer x {cellsOf k} canonical cells over dim {dim}), got {cells.Length}"
                    | ExprKind.ExprVar w ->
                        (match Map.tryFind w ctx.Arrays with
                         | Some (_, [ix]) when resolveExtent ctx.Aliases ctx.Statics ix = Some want ->
                             Ok (fun a labels -> Some (appE (v w) [iLit (a * cellsOf k + lexOffsetOf dim k labels)]))
                         | Some _ ->
                             Error $"{former}: vector D{k} ('{w}') must be a rank-1 array of {want} coordinate-major cells"
                         | None ->
                             Error $"{former}: vector D{k} ('{w}') must be an annotated module-level array or an inline array literal")
                    | _ ->
                        Error $"{former}: with a vector g0 (m = {m}), D{k} must be a named array or an array literal of {m * cellsOf k} coordinate-major cells"
                let dReadsRes =
                    dArgs
                    |> List.mapi (fun i a -> dReadOf (i + 1) a)
                    |> List.fold (fun acc r -> acc |> Result.bind (fun rs -> r |> Result.map (fun f -> rs @ [f])))
                                 (Ok [])
                dReadsRes |> Result.map (fun dReadFns ->
                    let dRead (k: int) (a: int) (labels: int list) = dReadFns.[k - 1] a labels
                    let mkDecl name value =
                        { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
                    let kappaName kk ci = $"__ppl_jetk_{dName}_o{kk}_c{ci}"
                    let cmName t ci = $"__ppl_jetcm_{dName}_t{t}_c{ci}"
                    let myName k ci = $"__ppl_jetmy_{dName}_o{k}_c{ci}"
                    let partsOf t =
                        setPartitions t
                        |> List.filter (fun pt ->
                            pt |> List.forall (fun blk -> blk.Length >= 2 && blk.Length <= info.Order))
                    // kappa pool + central moments: identical to the scalar path (input-side, output-agnostic; every cell bound once).
                    let neededKappa =
                        [ for t in 2 .. tMax do
                            for labels in canonicalTuples dim t do
                                let labArr = List.toArray labels
                                for pt in partsOf t do
                                    for blk in pt do
                                        yield (blk.Length, blk |> List.map (fun pos -> labArr.[pos]) |> List.sort) ]
                        |> List.distinct
                    let kappaDecls =
                        [ for (kk, sub) in neededKappa ->
                            mkDecl (kappaName kk (lexOffsetOf dim kk sub)) (distKappaRead info sub) ]
                    let kappaRead (sub: int list) =
                        v (kappaName sub.Length (lexOffsetOf dim sub.Length sub))
                    let cmDecls =
                        [ for t in 2 .. tMax do
                            yield! canonicalTuples dim t |> List.mapi (fun ci labels ->
                                let labArr = List.toArray labels
                                let terms =
                                    [ for pt in partsOf t ->
                                        pt |> List.fold (fun acc blk ->
                                            mulE acc (kappaRead (blk |> List.map (fun pos -> labArr.[pos]) |> List.sort))) (fLit 1.0) ]
                                let value = match terms with [] -> fLit 0.0 | _ -> terms |> List.reduce addE
                                mkDecl (cmName t ci) value) ]
                    let cmRead (labels: int list) =
                        v (cmName labels.Length (lexOffsetOf dim labels.Length labels))
                    let rec tuples (k: int) : int list list =
                        if k = 0 then [ [] ]
                        else [ for lab in 0 .. dim - 1 do for rest in tuples (k - 1) -> lab :: rest ]
                    // Ordered per-factor degree assignments (k_1..k_p) in [1..s]^p.
                    let rec degAssigns (p: int) : int list list =
                        if p = 0 then [ [] ]
                        else [ for kj in 1 .. s do for rest in degAssigns (p - 1) -> kj :: rest ]
                    // Joint raw moments of Y' over canonical output tuples.
                    let myDecls =
                        [ for k in 1 .. q do
                            yield! canonicalTuples m k |> List.mapi (fun ci oLabels ->
                                let oArr = List.toArray oLabels
                                let terms =
                                    [ for degs in degAssigns k do
                                        let t = List.sum degs
                                        if t >= 2 then   // t = 1 => D_1*E[Z] = 0
                                            let w = degs |> List.fold (fun acc kj -> acc / factorial kj) 1.0
                                            // factor fi reads D_{k_fi} of coordinate oArr.[fi]; literal-zero cells prune
                                            let rec go (fi: int) (ds: int list) : (Expr list * int list) list =
                                                match ds with
                                                | [] -> [ ([], []) ]
                                                | kj :: rest ->
                                                    let restA = go (fi + 1) rest
                                                    [ for tup in tuples kj do
                                                        match dRead kj oArr.[fi] tup with
                                                        | Some de ->
                                                            for (es, ls) in restA do
                                                                yield (de :: es, tup @ ls)
                                                        | None -> () ]
                                            let assigns = go 0 degs
                                            if not assigns.IsEmpty then
                                                let sum =
                                                    assigns
                                                    |> List.map (fun (des, ls) -> des |> List.fold mulE (cmRead ls))
                                                    |> List.reduce addE
                                                yield mulE (fLit w) sum ]
                                mkDecl (myName k ci) (match terms with [] -> fLit 0.0 | _ -> terms |> List.reduce addE)) ]
                    let myRead (sub: int list) =
                        v (myName sub.Length (lexOffsetOf m sub.Length sub))
                    // Joint kappa(Y) by multivariate Moebius over output positions; order-1 cells shift by g0.
                    let compDecls =
                        [ for k in 1 .. q ->
                            let cells =
                                [ for oLabels in canonicalTuples m k ->
                                    let oArr = List.toArray oLabels
                                    let mobius =
                                        setPartitions k
                                        |> List.map (fun pt ->
                                            let b = pt.Length
                                            let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
                                            pt |> List.fold (fun acc blk ->
                                                mulE acc (myRead (blk |> List.map (fun pos -> oArr.[pos]) |> List.sort))) (fLit w))
                                        |> List.reduce addE
                                    if k = 1 then addE mobius (g0Read oArr.[0]) else mobius ]
                            mkDecl (distComponentName dName k) (arrLitE cells) ]
                    let outInfo = { Order = q
                                    Components = [ for k in 1 .. q -> distComponentName dName k ]
                                    Sources = info.Sources
                                    Dim = Some m
                                    Flat = true }
                    (kappaDecls @ cmDecls @ myDecls @ compDecls, outInfo)))

let private elabDistJet (closed: bool) (former: string) (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | { Kind = ExprKind.ExprVar dn } :: qExpr :: g0Expr :: dArgs when not dArgs.IsEmpty ->
        match Map.tryFind dn dists with
        | None -> Error $"{former} expects {former}(d, q, g0, D1, ..., Ds) with a previously declared dist binding d"
        | Some info ->
            distDim ctx info |> Result.bind (fun dim ->
            // Vector mode is keyed off g0's shape: an m-cell (m >= 2) array literal or named rank-1 array of static extent m
            // means g : R^dim -> R^m; a scalar g0 keeps the univariate path.
            let vecM =
                match g0Expr.Kind with
                | ExprKind.ExprArrayLit cells -> Some cells.Length
                | ExprKind.ExprVar w ->
                    (match Map.tryFind w ctx.Arrays with
                     | Some (_, [ix]) -> resolveExtent ctx.Aliases ctx.Statics ix
                     | _ -> None)
                | _ -> None
            match vecM with
            | Some m when m >= 2 ->
                elabDistJetVec closed former ctx span dName info dim m qExpr g0Expr dArgs
            | _ ->
            let qRes =
                match evalExpr ctx.Statics maxSteps qExpr with
                | Ok (SVInt x) when x >= 1L && x <= 6L -> Ok (int x)
                | _ -> Error $"{former}: the output order q must be a compile-time integer in 1..6"
            qRes |> Result.bind (fun q ->
            let s = dArgs.Length
            let tMax = q * s
            if not closed && tMax > info.Order then
                Error $"{former}: computing {q} output cumulants through a degree-{s} jet needs input order {tMax} but '{dn}' carries {info.Order} -- insufficient stochastic order. Carry more (dist(A, {min tMax 6})) or accept the truncation explicitly with {former}_closed(...)"
            elif closed && tMax > 8 then
                Error $"{former}: q*s = {tMax} exceeds the generation bound (8) -- lower the output order or the jet degree"
            else
                // Per-degree derivative read at a label tuple: inline literals/scalar exprs splice (zero cells prune terms);
                // named arrays read at the flat canonical offset.
                let cellsOf k = canonicalTuples dim k |> List.length
                let dReadOf (k: int) (dArg: Expr) : Result<(int list -> Expr option), string> =
                    if dim = 1 then
                        match dArg.Kind with
                        | ExprKind.ExprLit (LitFloat 0.0) -> Ok (fun _ -> None)
                        | _ -> Ok (fun _ -> Some dArg)
                    else
                        match dArg.Kind with
                        | ExprKind.ExprArrayLit cells when cells.Length = cellsOf k ->
                            let cellArr = List.toArray cells
                            Ok (fun labels ->
                                let cell = cellArr.[lexOffsetOf dim k labels]
                                match cell.Kind with
                                | ExprKind.ExprLit (LitFloat 0.0) -> None
                                | _ -> Some cell)
                        | ExprKind.ExprArrayLit cells ->
                            Error $"{former}: D{k} needs {cellsOf k} cells in canonical lex order over dim {dim}, got {cells.Length}"
                        | ExprKind.ExprVar w ->
                            (match Map.tryFind w ctx.Arrays with
                             | Some (_, [ix]) when resolveExtent ctx.Aliases ctx.Statics ix = Some (cellsOf k) ->
                                 Ok (fun labels -> Some (appE (v w) [iLit (lexOffsetOf dim k labels)]))
                             | Some _ ->
                                 Error $"{former}: D{k} ('{w}') must be a rank-1 array of {cellsOf k} cells (canonical lex order over dim {dim})"
                             | None ->
                                 Error $"{former}: D{k} ('{w}') must be an annotated module-level array or an inline array literal")
                        | _ ->
                            Error $"{former}: with a {dim}-dimensional dist, D{k} must be a named array or an array literal (scalar-expr jets need a univariate dist)"
                let dReadsRes =
                    dArgs
                    |> List.mapi (fun i a -> dReadOf (i + 1) a)
                    |> List.fold (fun acc r -> acc |> Result.bind (fun rs -> r |> Result.map (fun f -> rs @ [f])))
                                 (Ok [])
                dReadsRes |> Result.map (fun dReadFns ->
                    let dRead (k: int) (labels: int list) = dReadFns.[k - 1] labels
                    let mkDecl name value =
                        { Value = DeclLet { Pattern = pvar name; Type = None; Value = value; Mutability = BindLet }; Span = span }
                    let kappaName kk ci = $"__ppl_jetk_{dName}_o{kk}_c{ci}"
                    let cmName t ci = $"__ppl_jetcm_{dName}_t{t}_c{ci}"
                    let myName m = $"__ppl_jetmy_{dName}_m{m}"
                    let partsOf t =
                        setPartitions t
                        |> List.filter (fun pt ->
                            pt |> List.forall (fun blk -> blk.Length >= 2 && blk.Length <= info.Order))
                    // Every kappa cell the partition sums touch, bound ONCE as a scalar let (packed logical reads carry heavy
                    // codegen -- repeating per partition term blows up the generated C++; ppl's pool discipline).
                    let neededKappa =
                        [ for t in 2 .. tMax do
                            for labels in canonicalTuples dim t do
                                let labArr = List.toArray labels
                                for pt in partsOf t do
                                    for blk in pt do
                                        yield (blk.Length, blk |> List.map (fun pos -> labArr.[pos]) |> List.sort) ]
                        |> List.distinct
                    let kappaDecls =
                        [ for (kk, sub) in neededKappa ->
                            mkDecl (kappaName kk (lexOffsetOf dim kk sub)) (distKappaRead info sub) ]
                    let kappaRead (sub: int list) =
                        v (kappaName sub.Length (lexOffsetOf dim sub.Length sub))
                    // Central moments of X, orders 2..q*s: partition sums over the bound kappa cells, singleton blocks excluded
                    // (kappa_1(Z) = 0), overlarge blocks dropped only under explicit closure.
                    let cmDecls =
                        [ for t in 2 .. tMax do
                            yield! canonicalTuples dim t |> List.mapi (fun ci labels ->
                                let labArr = List.toArray labels
                                let terms =
                                    [ for pt in partsOf t ->
                                        pt |> List.fold (fun acc blk ->
                                            mulE acc (kappaRead (blk |> List.map (fun pos -> labArr.[pos]) |> List.sort))) (fLit 1.0) ]
                                let value = match terms with [] -> fLit 0.0 | _ -> terms |> List.reduce addE
                                mkDecl (cmName t ci) value) ]
                    let cmRead (labels: int list) =
                        v (cmName labels.Length (lexOffsetOf dim labels.Length labels))
                    // Raw moments of Y' = Y - g0: multinomial over the ordered ways m factors distribute over jet degrees.
                    let rec compositions (m: int) (t: int) : int list list =
                        if t = 1 then [ [ m ] ]
                        else [ for first in 0 .. m do for rest in compositions (m - first) (t - 1) -> first :: rest ]
                    let rec tuples (k: int) : int list list =
                        if k = 0 then [ [] ]
                        else [ for lab in 0 .. dim - 1 do for rest in tuples (k - 1) -> lab :: rest ]
                    let myDecls =
                        [ for m in 1 .. q ->
                            let terms =
                                [ for comp in compositions m s do
                                    let t = comp |> List.mapi (fun i c -> (i + 1) * c) |> List.sum
                                    if t >= 2 then   // t = 1 => D_1*E[Z] = 0
                                        let w =
                                            (factorial m, List.indexed comp)
                                            ||> List.fold (fun acc (i, c) ->
                                                acc / factorial c / (factorial (i + 1) ** float c))
                                        let degs = [ for (i, c) in List.indexed comp do for _ in 1 .. c -> i + 1 ]
                                        // All label assignments, factor by factor; literal-zero cells prune.
                                        let rec go (ds: int list) : (Expr list * int list) list =
                                            match ds with
                                            | [] -> [ ([], []) ]
                                            | k :: rest ->
                                                let restA = go rest
                                                [ for tup in tuples k do
                                                    match dRead k tup with
                                                    | Some de ->
                                                        for (es, ls) in restA do
                                                            yield (de :: es, tup @ ls)
                                                    | None -> () ]
                                        let assigns = go degs
                                        if not assigns.IsEmpty then
                                            let sum =
                                                assigns
                                                |> List.map (fun (des, ls) -> des |> List.fold mulE (cmRead ls))
                                                |> List.reduce addE
                                            yield mulE (fLit w) sum ]
                            mkDecl (myName m) (match terms with [] -> fLit 0.0 | _ -> terms |> List.reduce addE) ]
                    // kappa_m(Y') by univariate Moebius inversion; kappa_1 shifts by g0.
                    let compDecls =
                        [ for m in 1 .. q ->
                            let mobius =
                                setPartitions m
                                |> List.map (fun pt ->
                                    let b = pt.Length
                                    let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
                                    pt |> List.fold (fun acc blk -> mulE acc (v (myName blk.Length))) (fLit w))
                                |> List.reduce addE
                            let value = if m = 1 then addE mobius g0Expr else mobius
                            mkDecl (distComponentName dName m) (arrLitE [ value ]) ]
                    let outInfo = { Order = q
                                    Components = [ for k in 1 .. q -> distComponentName dName k ]
                                    Sources = info.Sources
                                    Dim = Some 1
                                    Flat = true }
                    (kappaDecls @ cmDecls @ myDecls @ compDecls, outInfo))))
    | _ ->
        Error $"{former} expects {former}(d, q, g0, D1, ..., Ds): a dist binding, a static output order, g(mu), and the derivative tensors at the mean"

// Tower Bayes: three low-level conditioning primitives, univariate.
// Straight-line scalar generation over the dist's component reads,
// mirroring the jet pushforward's conventions (flat 1-cell output
// components, registry-level composition, cumulant(d, k) projection).
//
//   dist_expect(d, c0, ..., cq)    E[c0 + c1 X + ... + cq X^q]: a scalar.
//                                  Exact for q <= carried order; beyond it,
//                                  the moments(d, k) closure convention
//                                  (overlarge blocks drop). Model-evidence
//                                  / normalizer primitive.
//   dist_reweight(d, c0, ..., cq)  the tower of X under the reweighted law
//                                  dm' = (c0 + ... + cq x^q) dm / Z --
//                                  Bayes with a polynomial likelihood.
//                                  Exact and order-accounted: each
//                                  posterior moment consumes q extra input
//                                  moments (order-r prior -> order-(r-q)
//                                  posterior).
//   dist_mix(w1, d1, w2, d2)       the normalized mixture (w1 m1 + w2 m2)
//                                  / (w1 + w2): raw moments mix linearly;
//                                  cumulants do not. No independence
//                                  demanded: mixing is always lawful.
//
// Conditioning on a finite-support variable = dist_reweight by its Lagrange
// indicator polynomial; disintegrate-then-dist_mix is the law of total
// probability; sequential Bayes = chained dist_reweight.

/// Bind the raw moments m_1..m_top of a univariate registered dist as scalar lets (partition sums over the carried cumulants,
/// the moments(d,k) reconstruction at dim 1). Exact for j <= carried order. Returns decls and a reader (m_0 = literal 1).
let private rawMomentDecls (span: Span) (tag: string) (info: DistInfo) (top: int)
    : Located<Decl> list * (int -> Expr) =
    let mName j = $"__ppl_tb_{tag}_m{j}"
    let decls =
        [ for j in 1 .. top ->
            let parts =
                setPartitions j
                |> List.filter (fun pt -> pt |> List.forall (fun blk -> blk.Length <= info.Order))
            let terms =
                [ for pt in parts ->
                    pt |> List.fold (fun acc blk ->
                        mulE acc (distKappaRead info (List.replicate blk.Length 0))) (fLit 1.0) ]
            let value = match terms with [] -> fLit 0.0 | _ -> terms |> List.reduce addE
            { Value = DeclLet { Pattern = pvar (mName j); Type = None; Value = value; Mutability = BindLet }
              Span = span } ]
    let read j = if j = 0 then fLit 1.0 else v (mName j)
    (decls, read)

/// kappa_1..kappa_r FLAT 1-cell component decls for dName from a raw-moment reader, by univariate Moebius inversion over set partitions.
let private mobiusComponentDecls (span: Span) (dName: string) (r: int) (mRead: int -> Expr)
    : Located<Decl> list =
    [ for m in 1 .. r ->
        let value =
            setPartitions m
            |> List.map (fun pt ->
                let b = pt.Length
                let w = (if b % 2 = 1 then 1.0 else -1.0) * factorial (b - 1)
                pt |> List.fold (fun acc blk -> mulE acc (mRead blk.Length)) (fLit w))
            |> List.reduce addE
        { Value = DeclLet { Pattern = pvar (distComponentName dName m); Type = None
                            Value = arrLitE [ value ]; Mutability = BindLet }
          Span = span } ]

let private univariateOnly (former: string) (ctx: Ctx) (info: DistInfo) : Result<unit, string> =
    distDim ctx info |> Result.bind (fun dim ->
        if dim = 1 then Ok ()
        else Error $"{former}: univariate dists only so far -- marginalize or push forward (dist_map/dist_affine) first, then condition")

let private elabDistExpect (ctx: Ctx) (span: Span) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args, binding.Pattern with
    | { Kind = ExprKind.ExprVar dn } :: coeffs, { Kind = PatternKind.PatVar outName } when not coeffs.IsEmpty ->
        match Map.tryFind dn dists with
        | None -> Error "dist_expect expects dist_expect(d, c0, ..., cq) with a previously declared dist binding d"
        | Some info ->
            univariateOnly "dist_expect" ctx info |> Result.bind (fun () ->
            let q = coeffs.Length - 1
            if q > 8 then Error "dist_expect: polynomial degree exceeds the generation bound (8)"
            else
                let mDecls, mRead = rawMomentDecls span ("ex_" + outName) info q
                let value =
                    coeffs
                    |> List.mapi (fun j c -> if j = 0 then c else mulE c (mRead j))
                    |> List.reduce addE
                Ok (mDecls @ [ { Value = DeclLet { binding with Value = value }; Span = span } ]))
    | _ ->
        Error "dist_expect expects dist_expect(d, c0, ..., cq): a dist binding and the polynomial's coefficients (constant first)"

let private elabDistReweight (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | { Kind = ExprKind.ExprVar dn } :: coeffs when not coeffs.IsEmpty ->
        match Map.tryFind dn dists with
        | None -> Error "dist_reweight expects dist_reweight(d, c0, ..., cq) with a previously declared dist binding d"
        | Some info ->
            univariateOnly "dist_reweight" ctx info |> Result.bind (fun () ->
            let q = coeffs.Length - 1
            let rOut = info.Order - q
            if rOut < 1 then
                Error $"dist_reweight: a degree-{q} weight consumes {q} orders of the tower and '{dn}' carries only {info.Order} -- insufficient stochastic order. Carry more (dist(A, {(min (q + 1) 6)})) or lower the weight degree."
            else
                let mDecls, mRead = rawMomentDecls span ("rw_" + dName) info info.Order
                let zName = $"__ppl_tb_rw_{dName}_Z"
                let zVal =
                    coeffs
                    |> List.mapi (fun c cf -> if c = 0 then cf else mulE cf (mRead c))
                    |> List.reduce addE
                let zDecl = { Value = DeclLet { Pattern = pvar zName; Type = None; Value = zVal; Mutability = BindLet }; Span = span }
                let wName j = $"__ppl_tb_rw_{dName}_w{j}"
                let wDecls =
                    [ for j in 1 .. rOut ->
                        let num =
                            coeffs
                            |> List.mapi (fun c cf -> mulE cf (mRead (j + c)))
                            |> List.reduce addE
                        { Value = DeclLet { Pattern = pvar (wName j); Type = None
                                            Value = divE num (v zName); Mutability = BindLet }
                          Span = span } ]
                let wRead j = if j = 0 then fLit 1.0 else v (wName j)
                let compDecls = mobiusComponentDecls span dName rOut wRead
                let outInfo = { Order = rOut
                                Components = [ for k in 1 .. rOut -> distComponentName dName k ]
                                Sources = info.Sources
                                Dim = Some 1
                                Flat = true }
                Ok (mDecls @ (zDecl :: wDecls) @ compDecls, outInfo))
    | _ ->
        Error "dist_reweight expects dist_reweight(d, c0, ..., cq): a dist binding and the weight polynomial's coefficients (constant first)"

let private elabDistMix (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [w1; { Kind = ExprKind.ExprVar n1 }; w2; { Kind = ExprKind.ExprVar n2 }] ->
        match Map.tryFind n1 dists, Map.tryFind n2 dists with
        | Some d1, Some d2 ->
            univariateOnly "dist_mix" ctx d1 |> Result.bind (fun () ->
            univariateOnly "dist_mix" ctx d2 |> Result.bind (fun () ->
            let rOut = min d1.Order d2.Order
            let w1Name = $"__ppl_tb_mx_{dName}_w1"
            let w2Name = $"__ppl_tb_mx_{dName}_w2"
            let wsName = $"__ppl_tb_mx_{dName}_ws"
            let bind n vl = { Value = DeclLet { Pattern = pvar n; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let m1Decls, m1Read = rawMomentDecls span ("mxa_" + dName) d1 rOut
            let m2Decls, m2Read = rawMomentDecls span ("mxb_" + dName) d2 rOut
            let mixName j = $"__ppl_tb_mx_{dName}_m{j}"
            let mixDecls =
                [ for j in 1 .. rOut ->
                    bind (mixName j)
                        (divE (addE (mulE (v w1Name) (m1Read j)) (mulE (v w2Name) (m2Read j)))
                              (v wsName)) ]
            let mixRead j = if j = 0 then fLit 1.0 else v (mixName j)
            let compDecls = mobiusComponentDecls span dName rOut mixRead
            let outInfo = { Order = rOut
                            Components = [ for k in 1 .. rOut -> distComponentName dName k ]
                            Sources = Set.union d1.Sources d2.Sources
                            Dim = Some 1
                            Flat = true }
            Ok ([ bind w1Name w1; bind w2Name w2; bind wsName (addE (v w1Name) (v w2Name)) ]
               @ m1Decls @ m2Decls @ mixDecls @ compDecls, outInfo)))
        | _ ->
            Error "dist_mix expects dist_mix(w1, d1, w2, d2) with two previously declared dist bindings"
    | _ ->
        Error "dist_mix expects dist_mix(w1, d1, w2, d2): two weights (any pure scalar expressions) and two dist bindings"

/// dist_condition(d, i, x) (plan section 6, P5): condition a MULTIVARIATE
/// order-2 tower on coordinate i taking value x -- the Schur complement on
/// the kappa_2 block, over the remaining coordinates in ascending order:
///   mean'_j = mu_j + k2[j,i]/k2[i,i] * (x - mu_i)
///   cov'_ab = k2[a,b] - k2[a,i] k2[i,b] / k2[i,i]
/// EXACT ONLY AT ORDER 2: an order-2 tower is the Gaussian truncation, and
/// Gaussian conditionals are again Gaussian with exactly these two blocks --
/// so conditioning is CLOSED on order-2 towers. At order r > 2 the
/// conditional cumulants are not a function of the carried tower at all
/// (they need the full conditional density, not its truncation), so higher
/// orders are refused rather than silently truncated, the module
/// convention. The result registers as a FLAT (D-1)-dimensional order-2
/// dist (lex cell order, the pushforward representation), so cumulant()/
/// moments()/dist_affine compose downstream, and a D=2 input conditions to
/// an ordinary univariate tower (dist_expect, the approx bridge). k2[i,i]
/// is a runtime value: conditioning on a zero-variance coordinate is the
/// usual runtime division hazard, not a compile-time refusal.
let private elabDistCondition (ctx: Ctx) (span: Span) (dName: string)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar dn }; iE; xE] ->
        match Map.tryFind dn dists with
        | None -> Error "dist_condition expects dist_condition(d, i, x) with a previously declared dist binding d"
        | Some info ->
            distDim ctx info |> Result.bind (fun dim ->
            if dim = 1 then
                Error $"dist_condition: '{dn}' is univariate -- conditioning fixes one coordinate of a JOINT tower and returns the rest, and a 1-dimensional tower has no rest. Construct a joint dist(A, 2) over a multi-variable array first; for updating a univariate prior on data, use ppl.bayes."
            elif info.Order = 1 then
                Error $"dist_condition: '{dn}' carries only the mean (order 1) -- conditioning is the Schur complement on the kappa_2 block, so construct with dist(A, 2)."
            elif info.Order > 2 then
                Error $"dist_condition: '{dn}' carries order {info.Order}, and conditioning is exact ONLY at order 2 (the Gaussian truncation, whose conditionals stay order-2 with the Schur-complement blocks); an order-{info.Order} conditional is not a function of the carried tower, and the module refuses rather than truncates. Construct with dist(A, 2)."
            else
            match evalExpr ctx.Statics maxSteps iE with
            | Ok (SVInt ii) when ii >= 0L && ii < int64 dim ->
                let i = int ii
                let rest = [ 0 .. dim - 1 ] |> List.filter (fun j -> j <> i)
                let k1 j = distKappaRead info [j]
                let k2 a b = distKappaRead info [min a b; max a b]
                let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
                let xN = $"__ppl_cond_{dName}_x"
                let sN = $"__ppl_cond_{dName}_s"
                let devN = $"__ppl_cond_{dName}_dev"
                let meanCells =
                    [ for j in rest ->
                        addE (k1 j) (mulE (divE (k2 j i) (v sN)) (v devN)) ]
                let covCells =
                    [ for t in canonicalTuples (dim - 1) 2 ->
                        let a = rest.[t.[0]]
                        let b = rest.[t.[1]]
                        subE (k2 a b) (divE (mulE (k2 a i) (k2 b i)) (v sN)) ]
                let comps = [ distComponentName dName 1; distComponentName dName 2 ]
                let outInfo = { Order = 2
                                Components = comps
                                Sources = info.Sources
                                Dim = Some (dim - 1)
                                Flat = true }
                Ok ([ bind xN xE
                      bind sN (k2 i i)
                      bind devN (subE (v xN) (k1 i))
                      bind comps.[0] (arrLitE meanCells)
                      bind comps.[1] (arrLitE covCells) ], outInfo)
            | Ok (SVInt ii) ->
                Error $"dist_condition: coordinate {ii} is out of range for the {dim}-dimensional dist '{dn}' (coordinates are 0..{dim - 1})"
            | _ ->
                Error "dist_condition: the coordinate must be a compile-time integer (a literal, `let static`, or static-function call)")
    | _ ->
        Error "dist_condition expects dist_condition(d, i, x): a previously declared dist binding, a static coordinate, and the observed value (any pure scalar expression)"

// Signed atomic towers: quasi-distributions as first-class values.
//   dist_atoms(r, x1, w1, ..., xk, wk)  the order-r tower of the atomic
//                                       measure sum_i w_i delta(x_i),
//                                       normalized by sum w_i. Weights may
//                                       be negative (non-classical towers,
//                                       negative variance included, are
//                                       carryable values). Subsumes the
//                                       point-tower renewal (all-positive).
//   dist_negativity(d, x1, ..., xs)     the L1 negativity of d read as a
//                                       quasi-distribution on the claimed
//                                       support {x_1..x_s}: cells by
//                                       Lagrange indicators (exact when
//                                       s - 1 <= carried order), N = sum
//                                       max(0, -cell). Zero iff the tower
//                                       is a genuine probability on that
//                                       support.

let private elabDistAtoms (ctx: Ctx) (span: Span) (dName: string) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | rExpr :: rest when rest.Length >= 2 && rest.Length % 2 = 0 ->
        let rRes =
            match evalExpr ctx.Statics maxSteps rExpr with
            | Ok (SVInt n) when n >= 1L && n <= 6L -> Ok (int n)
            | _ -> Error "dist_atoms: the order must be a compile-time integer in 1..6"
        rRes |> Result.map (fun r ->
            let k = rest.Length / 2
            let xName i = $"__ppl_tb_at_{dName}_x{i}"
            let wName i = $"__ppl_tb_at_{dName}_w{i}"
            let bind n vl = { Value = DeclLet { Pattern = pvar n; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let bindDecls =
                [ for i in 0 .. k - 1 do
                    yield bind (xName i) rest.[2 * i]
                    yield bind (wName i) rest.[2 * i + 1] ]
            let wsName = $"__ppl_tb_at_{dName}_ws"
            let wsDecl = bind wsName ([ for i in 0 .. k - 1 -> v (wName i) ] |> List.reduce addE)
            let mName j = $"__ppl_tb_at_{dName}_m{j}"
            let mDecls =
                [ for j in 1 .. r ->
                    let terms =
                        [ for i in 0 .. k - 1 ->
                            List.replicate j (v (xName i)) |> List.fold mulE (v (wName i)) ]
                    bind (mName j) (divE (terms |> List.reduce addE) (v wsName)) ]
            let mRead j = if j = 0 then fLit 1.0 else v (mName j)
            let compDecls = mobiusComponentDecls span dName r mRead
            let info = { Order = r
                         Components = [ for kk in 1 .. r -> distComponentName dName kk ]
                         Sources = Set.empty
                         Dim = Some 1
                         Flat = true }
            (bindDecls @ (wsDecl :: mDecls) @ compDecls, info))
    | _ ->
        Error "dist_atoms expects dist_atoms(r, x1, w1, ..., xk, wk): a static order and support/weight pairs (weights may be negative -- quasi-distributions are values here)"

let private elabDistNegativity (ctx: Ctx) (span: Span) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args, binding.Pattern with
    | { Kind = ExprKind.ExprVar dn } :: xs, { Kind = PatternKind.PatVar outName } when xs.Length >= 2 ->
        match Map.tryFind dn dists with
        | None -> Error "dist_negativity expects dist_negativity(d, x1, ..., xs) with a previously declared dist binding d"
        | Some info ->
            univariateOnly "dist_negativity" ctx info |> Result.bind (fun () ->
            let sPts = xs.Length
            if sPts - 1 > info.Order then
                Error $"dist_negativity: reading {sPts} cells needs the degree-{sPts - 1} Lagrange indicators but '{dn}' carries order {info.Order} -- insufficient stochastic order. Carry more, or claim fewer support points."
            else
                let xName i = $"__ppl_tb_ng_{outName}_x{i}"
                let bind n vl = { Value = DeclLet { Pattern = pvar n; Type = None; Value = vl; Mutability = BindLet }; Span = span }
                let xDecls = [ for i in 0 .. sPts - 1 -> bind (xName i) xs.[i] ]
                let mDecls, mRead = rawMomentDecls span ("ng_" + outName) info (sPts - 1)
                let cellName j = $"__ppl_tb_ng_{outName}_c{j}"
                let cellDecls =
                    [ for j in 0 .. sPts - 1 ->
                        let others = [ for kk in 0 .. sPts - 1 do if kk <> j then yield v (xName kk) ]
                        // coefficients of prod (m - x_k), degree ascending
                        let coeffs =
                            others |> List.fold (fun (acc: Expr list) xk ->
                                let n = acc.Length
                                [ for i in 0 .. n ->
                                    let lower = if i >= 1 then Some acc.[i - 1] else None
                                    let upper = if i <= n - 1 then Some (mulE xk acc.[i]) else None
                                    match lower, upper with
                                    | Some l, Some u -> subE l u
                                    | Some l, None -> l
                                    | None, Some u -> subE (fLit 0.0) u
                                    | None, None -> fLit 0.0 ]) [ fLit 1.0 ]
                        let den = others |> List.fold (fun acc xk -> mulE acc (subE (v (xName j)) xk)) (fLit 1.0)
                        let num = coeffs |> List.mapi (fun c ce -> mulE ce (mRead c)) |> List.reduce addE
                        bind (cellName j) (divE num den) ]
                let negTerm j =
                    let c = v (cellName j)
                    divE (subE (appE (v "sqrt") [ mulE c c ]) c) (fLit 2.0)
                let total = [ for j in 0 .. sPts - 1 -> negTerm j ] |> List.reduce addE
                Ok (xDecls @ mDecls @ cellDecls
                    @ [ { Value = DeclLet { binding with Value = total }; Span = span } ]))
    | _ ->
        Error "dist_negativity expects dist_negativity(d, x1, ..., xs): a dist binding and at least two support points"

// Named distribution families (docs/plan-ppl-proper.md P1): closed-form
// cumulant towers in VALUE position and log-densities through logpdf/loglik.
// A family application is a SYNTACTIC form with two readings:
//   value position     `let d = gaussian(mu, s2, r)` -- the order-r
//                      univariate tower, FLAT 1-cell components (the
//                      dist_atoms representation: register-only, no
//                      __dist_pack -- its erasure type declares SymIdx-packed
//                      components and flat ArrayLits aren't). The registry
//                      algebra composes: cumulant() projects at elaboration,
//                      dist_add/dist_scale/dist_jet/dist_map/tower Bayes read
//                      through distKappaRead. Parameters are arbitrary
//                      runtime scalars (cumulant formulas are plain
//                      arithmetic); the ORDER is static, capped at 6.
//                      Sources = empty: a closed-form family carries no data
//                      provenance, so dist_add needs no independence
//                      declaration (the dist_atoms convention).
//   argument position  `logpdf(gaussian(mu, s2), x)` (and the same slot in
//                      loglik/sample) -- a (tag, params) pair the family-aware
//                      formers read symbolically (the dist_map-lambda
//                      precedent); no tower materializes and no order
//                      argument is taken.
// Log-densities are the ON-SUPPORT closed forms -- no branching, because an
// if/match would leave the AD-able subset (Grad.fs:27-50); x outside the
// support is the caller's contract (uniform's logpdf is the in-support
// constant -log(b-a)). loglik emits a scalar accumulation loop
// (`let mut` + for + `+=`), never a combinator pipeline, so a later phase
// can hand the body to ad.grad unchanged.

/// Constructor-capable families: parameter names in signature order.
/// (Mirrored literally in formerNames above -- keep the two in sync.)
let private familyParams : Map<string, string list> =
    Map.ofList [
        "gaussian",    ["mu"; "s2"]
        "exponential", ["rate"]
        "gamma",       ["shape"; "rate"]
        "poisson",     ["lam"]
        "uniform",     ["a"; "b"]
        "lognormal",   ["mu"; "s2"]
        "bernoulli",   ["p"]
        "beta",        ["a"; "b"] ]

let private familyNames : Set<string> = familyParams |> Map.toList |> List.map fst |> Set.ofList

let private familySig (fam: string) : string =
    $"""{fam}({(String.concat ", " familyParams.[fam])})"""

/// x * x * ... * x, k >= 1 copies: repeated multiplication keeps arbitrary
/// scalar parameter exprs inside plain arithmetic (the dist_scale convention).
let private powN (e: Expr) (k: int) : Expr =
    List.replicate (k - 1) e |> List.fold mulE e

/// ppl.<family>(params..., r) in VALUE position: the order-r univariate
/// cumulant tower. Closed cumulant ladders where they exist; lognormal,
/// bernoulli, and beta go through raw moments + Moebius inversion
/// (mobiusComponentDecls).
let private elabFamilyDist (ctx: Ctx) (span: Span) (fam: string) (dName: string) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    let pNames = familyParams.[fam]
    let arity = pNames.Length
    if args.Length <> arity + 1 then
        Error (sprintf "%s: the value-position constructor is %s(%s, r) -- %d parameter(s) then a static order in 1..6, got %d argument(s). (In logpdf/loglik argument position the family takes no order: %s.)"
                       fam fam (String.concat ", " pNames) arity args.Length (familySig fam))
    else
        match evalExpr ctx.Statics maxSteps (List.last args) with
        | Ok (SVInt x) when x >= 1L && x <= 6L ->
            let r = int x
            let bind n vl = { Value = DeclLet { Pattern = pvar n; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let pName i = $"__ppl_fam_{dName}_p{i}"
            let pDecls = args |> List.take arity |> List.mapi (fun i e -> bind (pName i) e)
            let p i = v (pName i)
            let direct (kappa: int -> Expr) =
                [ for k in 1 .. r -> bind (distComponentName dName k) (arrLitE [ kappa k ]) ]
            let compDecls =
                match fam with
                | "gaussian" ->
                    // kappa_1 = mu, kappa_2 = s2, higher cumulants vanish.
                    direct (fun k -> if k = 1 then p 0 elif k = 2 then p 1 else fLit 0.0)
                | "exponential" ->
                    // kappa_k = (k-1)! / rate^k
                    direct (fun k -> divE (fLit (factorial (k - 1))) (powN (p 0) k))
                | "gamma" ->
                    // kappa_k = shape * (k-1)! / rate^k
                    direct (fun k -> mulE (p 0) (divE (fLit (factorial (k - 1))) (powN (p 1) k)))
                | "poisson" ->
                    // every cumulant equals lambda
                    direct (fun _ -> p 0)
                | "uniform" ->
                    // kappa_1 = (a+b)/2; even kappa_n = B_n (b-a)^n / n over the
                    // Bernoulli numbers (B_2, B_4, B_6 = 1/6, -1/30, 1/42 ->
                    // divisors 12, -120, 252); odd orders >= 3 vanish.
                    let w = subE (p 1) (p 0)
                    direct (fun k ->
                        match k with
                        | 1 -> divE (addE (p 0) (p 1)) (fLit 2.0)
                        | 2 -> divE (powN w 2) (fLit 12.0)
                        | 4 -> divE (powN w 4) (fLit (-120.0))
                        | 6 -> divE (powN w 6) (fLit 252.0)
                        | _ -> fLit 0.0)
                | "lognormal" ->
                    // raw moments m_j = exp(j*mu + j^2*s2/2), then Moebius inversion.
                    let mName j = $"__ppl_fam_{dName}_m{j}"
                    let mDecls =
                        [ for j in 1 .. r ->
                            bind (mName j)
                                 (appE (v "exp") [ addE (mulE (fLit (float j)) (p 0))
                                                        (mulE (fLit (float (j * j) / 2.0)) (p 1)) ]) ]
                    let mRead j = if j = 0 then fLit 1.0 else v (mName j)
                    mDecls @ mobiusComponentDecls span dName r mRead
                | "bernoulli" ->
                    // every raw moment is p; Moebius inversion gives the tower.
                    let mRead j = if j = 0 then fLit 1.0 else p 0
                    mobiusComponentDecls span dName r mRead
                | "beta" ->
                    // raw moments climb one factor at a time -- m_j = m_{j-1} *
                    // (a+j-1)/(a+b+j-1), m_0 = 1 -- then Moebius inversion.
                    // Plain arithmetic all the way down: no lgamma is needed for
                    // the TOWER (only the log-density touches it), so the
                    // constructor rides the same moment route as lognormal.
                    let mName j = $"__ppl_fam_{dName}_m{j}"
                    let mDecls =
                        [ for j in 1 .. r ->
                            let prev = if j = 1 then fLit 1.0 else v (mName (j - 1))
                            bind (mName j)
                                 (mulE prev (divE (addE (p 0) (fLit (float (j - 1))))
                                                  (addE (addE (p 0) (p 1)) (fLit (float (j - 1)))))) ]
                    let mRead j = if j = 0 then fLit 1.0 else v (mName j)
                    mDecls @ mobiusComponentDecls span dName r mRead
                | _ -> []   // unreachable: fam ranges over familyParams keys
            let info = { Order = r
                         Components = [ for k in 1 .. r -> distComponentName dName k ]
                         Sources = Set.empty
                         Dim = Some 1
                         Flat = true }
            Ok (pDecls @ compDecls, info)
        | Ok (SVInt x) ->
            Error $"{fam}: the order must be in 1..6 (got {x}) -- Bell-number kernel growth beyond that needs the shared-subexpression pass"
        | _ ->
            Error $"{fam}: the order must be a compile-time integer (a literal, `let static`, or static-function call)"

// Log-densities. The lgamma-free closed forms (gaussian/exponential/uniform/
// lognormal) shipped first; gamma/poisson/beta/bernoulli landed once lgamma
// became a real intrinsic (hand-rolled Lanczos in blade_runtime.hpp with the
// bit-exact interp mirror; domain x > 0, BL8008 panic otherwise -- every
// lgamma argument below is positive whenever the family's parameters are).
//
// AD-ability: all eight families are inside the AD-able subset. lgamma has a
// derivative rule in Grad (digamma; corpus ad/015, ppl/117 exercise HMC over
// gamma/poisson models). The frontier is digamma itself -- no trigamma
// exists, so second derivatives of lgamma-gated densities are refused by
// Grad's intrinsic gate. loglik emits the scalar `let mut` + for + `+=`
// accumulation loop for every family.

/// log(2 pi s2), the Gaussian/lognormal normalizer.
let private log2piE (s2: Expr) : Expr = appE (v "log") [ mulE (fLit (2.0 * System.Math.PI)) s2 ]

// Scalar intrinsic/branching construction helpers for the density and
// approximate-bridge formers (the loop-object helpers live at the top).
let private logE (x: Expr) : Expr = appE (v "log") [x]
let private expE (x: Expr) : Expr = appE (v "exp") [x]
let private sqrtE (x: Expr) : Expr = appE (v "sqrt") [x]
let private absE (x: Expr) : Expr = appE (v "abs") [x]
let private lgammaE (x: Expr) : Expr = appE (v "lgamma") [x]
let private negE (x: Expr) : Expr = subE (fLit 0.0) x
let private ltE a b = syn (ExprBinOp (Elementwise, OpLt, a, b))
let private leE a b = syn (ExprBinOp (Elementwise, OpLe, a, b))
let private gtE a b = syn (ExprBinOp (Elementwise, OpGt, a, b))
let private ifE c t f = syn (ExprIf (c, t, f))

/// Argument-position family recognition: `gaussian(mu, s2)` as a syntactic
/// (tag, param exprs) -- the dist_map-lambda precedent. A user definition of
/// the family's name shadows it (same rule as the formers), which makes the
/// argument opaque here.
let private familyArg (former: string) (active: string -> bool) (e: Expr) : Result<string * Expr list, string> =
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, ps) when Set.contains f familyNames && active f -> Ok (f, ps)
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, _) when Set.contains f familyNames ->
        Error $"{former}: '{f}' is shadowed by a user definition in this module, so the argument is not a family constructor here"
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, _) ->
        Error $"{former}: unknown family '{f}' -- available: gaussian(mu, s2), exponential(rate), gamma(shape, rate), poisson(lam), uniform(a, b), lognormal(mu, s2), bernoulli(p), beta(a, b)"
    | _ ->
        Error $"{former}: the first argument must be a family constructor application written syntactically, e.g. {former}(gaussian(mu, s2), ...)"

/// Family + parameter validation shared by logpdf/loglik/sample: arity and
/// the stray-order-argument steer. `position` names the symbolic slot in the
/// steering text ("log-density position" / "sample position").
let private checkSymbolicFamily (ctx: Ctx) (former: string) (position: string) (fam: string) (ps: Expr list) : Result<unit, string> =
    let arity = familyParams.[fam].Length
    if ps.Length = arity then Ok ()
    elif ps.Length = arity + 1 && (match evalExpr ctx.Statics maxSteps (List.last ps) with Ok (SVInt _) -> true | _ -> false) then
        Error $"{former}: {fam} takes no order argument in {position} -- the family is symbolic here ({familySig fam}); the order-r tower is the value-position constructor `let d = {fam}(..., r)`"
    else
        Error ($"""{former}: {fam} expects {arity} parameter(s) ({(String.concat ", " familyParams.[fam])}), got {ps.Length}""")

let private checkDensityFamily (ctx: Ctx) (former: string) (fam: string) (ps: Expr list) : Result<unit, string> =
    checkSymbolicFamily ctx former "log-density position" fam ps

/// Shared logpdf synthesis: the ordered scalar bindings and the final value
/// expression, names keyed by `tok`. Decl position (elabLogPdf) wraps the
/// bindings as module lets under the user's decl; expression position
/// (rewriteBodyFormers, plan section 4's density-form model bodies) wraps
/// them as statement lets in a block, so the same closed forms serve both.
let private logPdfParts (tok: string) (fam: string) (ps: Expr list) (xExpr: Expr) : (string * Expr) list * Expr =
    let pName i = $"__ppl_lp_{tok}_p{i}"
    let xName = $"__ppl_lp_{tok}_x"
    let pBinds = ps |> List.mapi (fun i e -> (pName i, e))
    let p i = v (pName i)
    let x = v xName
    let extra, value =
        match fam with
        | "gaussian" ->
            // -log(2 pi s2)/2 - (x - mu)^2 / (2 s2)
            let dN = $"__ppl_lp_{tok}_d"
            ([ (dN, subE x (p 0)) ],
             subE (mulE (fLit (-0.5)) (log2piE (p 1)))
                  (divE (mulE (v dN) (v dN)) (mulE (fLit 2.0) (p 1))))
        | "exponential" ->
            // log(rate) - rate x
            ([], subE (appE (v "log") [p 0]) (mulE (p 0) x))
        | "uniform" ->
            // the in-support constant -log(b - a)
            ([], subE (fLit 0.0) (appE (v "log") [subE (p 1) (p 0)]))
        | "lognormal" ->
            // -log(x) - log(2 pi s2)/2 - (log(x) - mu)^2 / (2 s2)
            let lxN = $"__ppl_lp_{tok}_lx"
            let dN = $"__ppl_lp_{tok}_d"
            ([ (lxN, appE (v "log") [x]); (dN, subE (v lxN) (p 0)) ],
             subE (subE (mulE (fLit (-0.5)) (log2piE (p 1))) (v lxN))
                  (divE (mulE (v dN) (v dN)) (mulE (fLit 2.0) (p 1))))
        | "gamma" ->
            // (shape-1) log(x) - rate x + shape log(rate) - lgamma(shape)
            ([], subE (addE (subE (mulE (subE (p 0) (fLit 1.0)) (logE x))
                                 (mulE (p 1) x))
                           (mulE (p 0) (logE (p 1))))
                      (lgammaE (p 0)))
        | "poisson" ->
            // k log(lam) - lam - lgamma(k + 1)
            ([], subE (subE (mulE x (logE (p 0))) (p 0))
                      (lgammaE (addE x (fLit 1.0))))
        | "beta" ->
            // (a-1) log(x) + (b-1) log(1-x) - (lgamma a + lgamma b - lgamma(a+b))
            ([], subE (addE (mulE (subE (p 0) (fLit 1.0)) (logE x))
                            (mulE (subE (p 1) (fLit 1.0)) (logE (subE (fLit 1.0) x))))
                      (subE (addE (lgammaE (p 0)) (lgammaE (p 1)))
                            (lgammaE (addE (p 0) (p 1)))))
        | "bernoulli" ->
            // x log(p) + (1-x) log(1-p)
            ([], addE (mulE x (logE (p 0)))
                      (mulE (subE (fLit 1.0) x) (logE (subE (fLit 1.0) (p 0)))))
        | _ -> ([], fLit 0.0)   // unreachable: checkDensityFamily gates
    (pBinds @ [ (xName, xExpr) ] @ extra, value)

/// logpdf(family(params), x): the scalar log-density at x -- closed-form
/// arithmetic over once-bound parameters, ON-SUPPORT by design (no branching;
/// see the section comment).
let private elabLogPdf (active: string -> bool) (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [famE; xExpr] ->
        familyArg "logpdf" active famE |> Result.bind (fun (fam, ps) ->
        checkDensityFamily ctx "logpdf" fam ps |> Result.map (fun () ->
            let bind n vl = { Value = DeclLet { Pattern = pvar n; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let (binds, value) = logPdfParts outName fam ps xExpr
            (binds |> List.map (fun (n, e) -> bind n e))
                @ [ { Value = DeclLet { binding with Value = value }; Span = span } ]))
    | _ ->
        Error "logpdf expects logpdf(family(params), x): a symbolic family argument and the evaluation point"

/// loglik(family(params), A): the summed log-density over A's sample axis
/// (its last -- and only -- declared index; the shape comes from the
/// declared annotation or the computed method_for shape, never from a
/// literal). Emitted as an AD-able scalar accumulation loop with the
/// per-family constants hoisted out of the loop; uniform needs no loop at
/// all (the on-support sum is -n log(b-a)). Leading variable axes are
/// refused: a univariate family has no per-coordinate loglik.
let private logLikParts (ctx: Ctx) (tok: string) (fam: string) (ps: Expr list) (aName: string)
    : Result<(string * Expr) list * Expr, string> =
        match Map.tryFind aName ctx.Arrays with
        | None ->
            Error $"loglik: '{aName}' must be a module-level let with an Array<Float like SampleIdx> annotation (or a computed method_for shape) -- the former reads the declared shape"
        | Some (_, idxs) when idxs.Length <> 1 ->
            Error $"loglik: '{aName}' carries {idxs.Length} declared axes -- a univariate family sums a rank-1 sample vector (Array<Float like SampleIdx>); slice or push forward (dist_map/dist_affine) first"
        | Some (_, idxs) ->
            let fiber = idxs.Head
            match resolveExtent ctx.Aliases ctx.Statics fiber with
            | None -> Error $"loglik: '{aName}' sample axis extent must be statically known (Idx<n> directly or through aliases)"
            | Some n ->
                let nF = float n
                let pName i = $"__ppl_ll_{tok}_p{i}"
                let pBinds = ps |> List.mapi (fun i e -> (pName i, e))
                let p i = v (pName i)
                let accN i = $"__ppl_ll_{tok}_acc{i}"
                let iN = $"__ppl_ll_{tok}_i"
                // The loop var is a plain Int64 read against a tagged sample
                // axis; route the read through a synthesized `__` alias so
                // BL4003's synthesized-buffer gate applies (the read is
                // compiler-generated -- the user has no cast to write).
                let srcN = $"__ppl_ll_{tok}_src"
                let aRead = appE (v srcN) [v iN]
                let sMut nm vl = StmtLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindMut }
                let accAdd i term = StmtExpr (syn (ExprAssign (v (accN i), addE (v (accN i)) term)))
                let loop (accs: int) (body: Stmt list) (final: Expr) : Expr =
                    syn (ExprBlock (
                            [ for i in 0 .. accs - 1 -> sMut (accN i) (fLit 0.0) ]
                            @ [ StmtForIn (iN, syn (ExprDotDot (iLit 0, iLit n)), body) ],
                            Some final))
                let value =
                    match fam with
                    | "gaussian" ->
                        // -n/2 log(2 pi s2) - sum (x_i - mu)^2 / (2 s2)
                        let dN = $"__ppl_ll_{tok}_d"
                        loop 1
                             [ sLet dN (subE aRead (p 0)); accAdd 0 (mulE (v dN) (v dN)) ]
                             (subE (mulE (fLit (-nF / 2.0)) (log2piE (p 1)))
                                   (divE (v (accN 0)) (mulE (fLit 2.0) (p 1))))
                    | "exponential" ->
                        // n log(rate) - rate sum x_i
                        loop 1 [ accAdd 0 aRead ]
                             (subE (mulE (fLit nF) (appE (v "log") [p 0])) (mulE (p 0) (v (accN 0))))
                    | "uniform" ->
                        // the in-support constant: -n log(b - a); the data drops out
                        mulE (fLit (-nF)) (appE (v "log") [subE (p 1) (p 0)])
                    | "lognormal" ->
                        // -n/2 log(2 pi s2) - sum log x_i - sum (log x_i - mu)^2 / (2 s2)
                        let lxN = $"__ppl_ll_{tok}_lx"
                        let dN = $"__ppl_ll_{tok}_d"
                        loop 2
                             [ sLet lxN (appE (v "log") [aRead])
                               accAdd 0 (v lxN)
                               sLet dN (subE (v lxN) (p 0))
                               accAdd 1 (mulE (v dN) (v dN)) ]
                             (subE (subE (mulE (fLit (-nF / 2.0)) (log2piE (p 1))) (v (accN 0)))
                                   (divE (v (accN 1)) (mulE (fLit 2.0) (p 1))))
                    | "gamma" ->
                        // (shape-1) sum log x_i - rate sum x_i + n (shape log rate - lgamma shape)
                        loop 2 [ accAdd 0 (logE aRead); accAdd 1 aRead ]
                             (addE (subE (mulE (subE (p 0) (fLit 1.0)) (v (accN 0)))
                                         (mulE (p 1) (v (accN 1))))
                                   (mulE (fLit nF) (subE (mulE (p 0) (logE (p 1))) (lgammaE (p 0)))))
                    | "poisson" ->
                        // log(lam) sum k_i - n lam - sum lgamma(k_i + 1); the
                        // lgamma sum stays in the loop (k_i-dependent).
                        loop 2 [ accAdd 0 aRead; accAdd 1 (lgammaE (addE aRead (fLit 1.0))) ]
                             (subE (subE (mulE (logE (p 0)) (v (accN 0)))
                                         (mulE (fLit nF) (p 0)))
                                   (v (accN 1)))
                    | "beta" ->
                        // (a-1) sum log x_i + (b-1) sum log(1-x_i) - n log B(a, b)
                        loop 2 [ accAdd 0 (logE aRead); accAdd 1 (logE (subE (fLit 1.0) aRead)) ]
                             (subE (addE (mulE (subE (p 0) (fLit 1.0)) (v (accN 0)))
                                         (mulE (subE (p 1) (fLit 1.0)) (v (accN 1))))
                                   (mulE (fLit nF) (subE (addE (lgammaE (p 0)) (lgammaE (p 1)))
                                                         (lgammaE (addE (p 0) (p 1))))))
                    | "bernoulli" ->
                        // log(p) sum x_i + log(1-p) (n - sum x_i)
                        loop 1 [ accAdd 0 aRead ]
                             (addE (mulE (logE (p 0)) (v (accN 0)))
                                   (mulE (logE (subE (fLit 1.0) (p 0))) (subE (fLit nF) (v (accN 0)))))
                    | _ -> fLit 0.0   // unreachable: checkDensityFamily gates
                // uniform reads no data: no alias, or it would print as an unused copy.
                let srcBinds = if fam = "uniform" then [] else [ (srcN, v aName) ]
                Ok (pBinds @ srcBinds, value)

let private elabLogLik (active: string -> bool) (ctx: Ctx) (span: Span) (outName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [famE; { Kind = ExprKind.ExprVar aName }] ->
        familyArg "loglik" active famE |> Result.bind (fun (fam, ps) ->
        checkDensityFamily ctx "loglik" fam ps |> Result.bind (fun () ->
        logLikParts ctx outName fam ps aName |> Result.map (fun (binds, value) ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            (binds |> List.map (fun (nm, e) -> bind nm e))
                @ [ { Value = DeclLet { binding with Value = value }; Span = span } ])))
    | [_; _] ->
        Error "loglik: the second argument must be a named module-level array (the sample vector)"
    | _ ->
        Error "loglik expects loglik(family(params), A): a symbolic family argument and a rank-1 sample array"

// Conjugate posterior updates (plan section 6, P5): bayes(prior(hyper),
// <family>_lik(params), A, r) -- the closed-form posterior AS AN ORDINARY
// FAMILY TOWER. Pure source synthesis mirroring oracles/ppl/Density.fs
// Conjugate (the oracle's dump-conjugate verb): the sufficient statistic
// comes off the data by loglik's accumulation idiom (the data's only use),
// the posterior hyperparameters are once-bound scalar arithmetic, and the
// tower itself is re-emitted through the ordinary family constructor
// (elabFamilyDist), so the result is a registered flat univariate dist that
// every tower consumer (cumulant/dist_expect/dist_map/the approx bridge)
// composes with. Supported pairs (prior + likelihood -> posterior family):
//   gaussian(m0, v0)   + gaussian_lik(s2) -> gaussian   (Normal-Normal, s2 KNOWN)
//   beta(a, b)         + bernoulli_lik()  -> beta       (Beta-Bernoulli)
//   gamma(shape, rate) + poisson_lik()    -> gamma      (Gamma-Poisson)
// Normal-InverseGamma (unknown mean AND variance) is deliberately deferred:
// its prior is not a family tower (no nig constructor exists -- the
// mu-margin is Student-t, whose cumulants past order 2*alpha do not exist),
// and its posterior is a 4-hyperparameter object of which only the
// PRECISION margin is a Gamma tower -- a tuple-former surface, not this
// prior-in/posterior-out one.

let private likParams : Map<string, string list> =
    Map.ofList [
        "gaussian_lik",  ["s2"]
        "bernoulli_lik", []
        "poisson_lik",   [] ]

let private likNames : Set<string> = likParams |> Map.toList |> List.map fst |> Set.ofList

let private bayesPairsText =
    "gaussian(m0, v0) + gaussian_lik(s2) (Normal-Normal, known likelihood variance), beta(a, b) + bernoulli_lik() (Beta-Bernoulli), gamma(shape, rate) + poisson_lik() (Gamma-Poisson)"

/// Likelihood-position recognition for bayes: `<family>_lik(params...)`,
/// syntactic like familyArg, with the same shadowing rule.
let private likArg (active: string -> bool) (e: Expr) : Result<string * Expr list, string> =
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, ps) when Set.contains f likNames && active f ->
        let arity = likParams.[f].Length
        if ps.Length = arity then Ok (f, ps)
        else Error ($"""bayes: {f} expects {arity} parameter(s) ({(String.concat ", " likParams.[f])}), got {ps.Length}""")
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, _) when Set.contains f likNames ->
        Error $"bayes: '{f}' is shadowed by a user definition in this module, so the argument is not a likelihood former here"
    | _ ->
        Error "bayes: the second argument must be a likelihood former written syntactically -- gaussian_lik(s2), bernoulli_lik(), or poisson_lik()"

/// The bayes data contract IS loglik's sample-axis contract: a named
/// module-level rank-1 array with a statically known extent.
let private bayesSampleVector (ctx: Ctx) (aName: string) : Result<int, string> =
    match Map.tryFind aName ctx.Arrays with
    | None ->
        Error $"bayes: '{aName}' must be a module-level let with an Array<Float like SampleIdx> annotation (or a computed method_for shape) -- the former reads the declared shape"
    | Some (_, idxs) when idxs.Length <> 1 ->
        Error $"bayes: '{aName}' carries {idxs.Length} declared axes -- a conjugate update consumes a rank-1 sample vector (Array<Float like SampleIdx>); slice or push forward first"
    | Some (_, idxs) ->
        match resolveExtent ctx.Aliases ctx.Statics idxs.Head with
        | None -> Error $"bayes: '{aName}' sample axis extent must be statically known (Idx<n> directly or through aliases)"
        | Some n -> Ok n

let private elabBayes (active: string -> bool) (ctx: Ctx) (span: Span) (dName: string) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    match args with
    | [priorE; likE; { Kind = ExprKind.ExprVar aName }; rExpr] ->
        familyArg "bayes" active priorE |> Result.bind (fun (priorFam, priorPs) ->
        checkSymbolicFamily ctx "bayes" "prior position" priorFam priorPs |> Result.bind (fun () ->
        likArg active likE |> Result.bind (fun (likFam, likPs) ->
        (match priorFam, likFam with
         | "gaussian", "gaussian_lik" | "beta", "bernoulli_lik" | "gamma", "poisson_lik" -> Ok ()
         | _ ->
             Error $"bayes: no conjugate update for a {priorFam} prior with a {likFam} likelihood -- supported pairs: {bayesPairsText}. (Normal-InverseGamma, unknown mean AND variance, is deferred: its posterior is a 4-hyperparameter object whose mu-margin is Student-t, not a family tower.)")
        |> Result.bind (fun () ->
        (match evalExpr ctx.Statics maxSteps rExpr with
         | Ok (SVInt x) when x >= 1L && x <= 6L -> Ok (int x)
         | Ok (SVInt x) -> Error $"bayes: the posterior order must be in 1..6 (got {x}) -- Bell-number kernel growth beyond that needs the shared-subexpression pass"
         | _ -> Error "bayes: the posterior order must be a compile-time integer (a literal, `let static`, or static-function call)")
        |> Result.bind (fun r ->
        bayesSampleVector ctx aName |> Result.bind (fun n ->
            let nF = float n
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let nm s = $"__ppl_by_{dName}_{s}"
            // Prior/likelihood parameters bound once (arbitrary pure scalar
            // exprs; the Normal-Normal update reads v0 and s2 twice).
            let prN i = nm $"pr{i}"
            let lkN i = nm $"lk{i}"
            let paramDecls =
                (priorPs |> List.mapi (fun i e -> bind (prN i) e))
                @ (likPs |> List.mapi (fun i e -> bind (lkN i) e))
            let pr i = v (prN i)
            let lk i = v (lkN i)
            // The one sufficient statistic every pair needs beyond the static
            // n: the sample-axis sum (= #successes for Bernoulli data) --
            // loglik's accumulation idiom, read through a `__` alias so the
            // synthesized-buffer gate applies.
            let srcN, sumN, iN, accN = nm "src", nm "sum", nm "i", nm "acc"
            let sumVal =
                syn (ExprBlock (
                        [ sMutStmt accN (fLit 0.0)
                          forIn iN (iLit 0) (iLit n)
                              [ assignStmt (v accN) (addE (v accN) (appE (v srcN) [v iN])) ] ],
                        Some (v accN)))
            let statDecls = [ bind srcN (v aName); bind sumN sumVal ]
            let S = v sumN
            // Posterior hyperparameters (Density.fs Conjugate's formulas,
            // same expression shapes as the oracle), then the ordinary
            // family-constructor emission of the posterior tower.
            let hyperDecls, postFam, postArgs =
                match priorFam with
                | "gaussian" ->
                    // vn = 1/(1/v0 + n/s2); mn = vn * (m0/v0 + sum/s2)
                    let vnN = nm "vn"
                    ([ bind vnN (divE (fLit 1.0) (addE (divE (fLit 1.0) (pr 1)) (divE (fLit nF) (lk 0)))) ],
                     "gaussian",
                     [ mulE (v vnN) (addE (divE (pr 0) (pr 1)) (divE S (lk 0))); v vnN ])
                | "beta" ->
                    // a_n = a + k; b_n = b + (n - k), k = #successes
                    ([], "beta", [ addE (pr 0) S; addE (pr 1) (subE (fLit nF) S) ])
                | _ ->
                    // gamma: shape_n = shape + sum k_i; rate_n = rate + n
                    ([], "gamma", [ addE (pr 0) S; addE (pr 1) (fLit nF) ])
            elabFamilyDist ctx span postFam dName (postArgs @ [iLit r])
            |> Result.map (fun (famDecls, info) ->
                // The posterior is a deterministic function of A: carry it as
                // the source so combining two same-data posteriors demands
                // the (unsatisfiable) independence license instead of
                // silently adding dependent cumulants.
                (paramDecls @ statDecls @ hyperDecls @ famDecls,
                 { info with Sources = Set.singleton aName }))))))))
    | [_; _; _; _] ->
        Error "bayes: the third argument must be a named module-level array (the sample vector)"
    | _ ->
        Error "bayes expects bayes(prior(hyper), <family>_lik(params), A, r): a conjugate prior family, its likelihood former, a rank-1 data array, and a static posterior order in 1..6"

/// Expression-position density formers, for top-level FUNCTION BODIES only
/// (docs/plan-ppl-proper.md section 4: a model is an ordinary named function
/// from latents to a Float log-density, assembled from log-prob terms). Each
/// call site rewrites to the same closed forms the decl-position formers
/// emit, with the site's bindings HOISTED as statement lets into the
/// enclosing statement list (the function-body block, or the innermost loop
/// body) and the site expression replaced by the closed-form value -- logpdf
/// stays straight-line scalar arithmetic and loglik keeps its AD-able
/// accumulation loop AT STATEMENT LEVEL, so a model body remains inside the
/// Grad subset for the HMC wave (grad refuses nested expression blocks, so
/// the pre-hoist `{lets; value}`-in-expression shape would poison every
/// model containing a density term). Positions that cannot host hoisted
/// statements (if/match arms, lambda and expression-let bodies -- none
/// differentiable anyway) fall back to wrapping the statements in a local
/// block. Every OTHER former stays decl-RHS only (pass 3's misplaced check
/// still walks these bodies and refuses what this pass did not rewrite).
let private rewriteBodyFormers (active: string -> bool) (ctx: Ctx) (fnName: string) (body: Expr)
    : Result<Expr, string> =
    let mutable err : string option = None
    let mutable site = 0
    let fail msg e = (if err.IsNone then err <- Some msg); ([], e)
    // A rewritten site: parameter/x/extra bindings become statements; a
    // loglik value is itself a block (mut accumulators + loop), so its
    // statements join the hoist and its final expression is the site value.
    let siteParts (binds: (string * Expr) list) (value: Expr) : Stmt list * Expr =
        let bindStmts = binds |> List.map (fun (nm, e) -> sLet nm e)
        match value.Kind with
        | ExprKind.ExprBlock (vstmts, Some vfinal) -> (bindStmts @ vstmts, vfinal)
        | _ -> (bindStmts, value)
    let rec go (e: Expr) : Stmt list * Expr =
        let r = go
        // Rewrite in a position with no statement list of its own: wrap any
        // hoisted statements back into a local expression block.
        let wrapped (e2: Expr) : Expr =
            match go e2 with
            | ([], e') -> e'
            | (ss, e') -> syn (ExprBlock (ss, Some e'))
        let rList es =
            es |> List.fold (fun (accS, accE) x ->
                let (ss, x') = r x
                (accS @ ss, accE @ [x'])) ([], [])
        let rOpt = Option.map wrapped
        let rec goStmt (s: Stmt) : Stmt list =
            match s with
            | StmtLet b -> let (ss, v') = r b.Value in ss @ [StmtLet { b with Value = v' }]
            | StmtAssign (l, op, rr) ->
                let (sl, l') = r l
                let (sr, r') = r rr
                sl @ sr @ [StmtAssign (l', op, r')]
            | StmtExpr x -> let (ss, x') = r x in ss @ [StmtExpr x']
            | StmtForIn (var, range, sbody) ->
                let (sr, range') = r range
                sr @ [StmtForIn (var, range', List.collect goStmt sbody)]
            | StmtSpanned (inner, sp) ->
                match List.rev (goStmt inner) with
                | last :: pre -> List.rev pre @ [StmtSpanned (last, sp)]
                | [] -> []
        match e.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "logpdf" }, args) when active "logpdf" ->
            let tok = $"{fnName}_{site}"
            site <- site + 1
            (match args with
             | [famE; xExpr] ->
                 (familyArg "logpdf" active famE |> Result.bind (fun (fam, ps) ->
                  checkDensityFamily ctx "logpdf" fam ps |> Result.map (fun () ->
                     let (binds, value) = logPdfParts tok fam (List.map wrapped ps) (wrapped xExpr)
                     siteParts binds value)))
                 |> function Ok parts -> parts | Error m -> fail m e
             | _ -> fail "logpdf expects logpdf(family(params), x): a symbolic family argument and the evaluation point" e)
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "loglik" }, args) when active "loglik" ->
            let tok = $"{fnName}_{site}"
            site <- site + 1
            (match args with
             | [famE; { Kind = ExprKind.ExprVar aName }] ->
                 (familyArg "loglik" active famE |> Result.bind (fun (fam, ps) ->
                  checkDensityFamily ctx "loglik" fam ps |> Result.bind (fun () ->
                  logLikParts ctx tok fam (List.map wrapped ps) aName |> Result.map (fun (binds, value) ->
                     siteParts binds value))))
                 |> function Ok parts -> parts | Error m -> fail m e
             | [_; _] -> fail "loglik: the second argument must be a named module-level array (the sample vector)" e
             | _ -> fail "loglik expects loglik(family(params), A): a symbolic family argument and a rank-1 sample array" e)
        | ExprKind.ExprApp (f, args) ->
            let (sf, f') = r f
            let (sa, args') = rList args
            (sf @ sa, inheritSpan e (ExprApp (f', args')))
        | ExprKind.ExprBinOp (m, op, a, b) ->
            let (sa, a') = r a
            let (sb, b') = r b
            (sa @ sb, inheritSpan e (ExprBinOp (m, op, a', b')))
        | ExprKind.ExprUnaryOp (op, a) -> let (ss, a') = r a in (ss, inheritSpan e (ExprUnaryOp (op, a')))
        | ExprKind.ExprTyped (a, t) -> let (ss, a') = r a in (ss, inheritSpan e (ExprTyped (a', t)))
        | ExprKind.ExprAssign (l, rr) ->
            let (sl, l') = r l
            let (sr, r') = r rr
            (sl @ sr, inheritSpan e (ExprAssign (l', r')))
        | ExprKind.ExprTuple es -> let (ss, es') = rList es in (ss, inheritSpan e (ExprTuple es'))
        | ExprKind.ExprArrayLit es -> let (ss, es') = rList es in (ss, inheritSpan e (ExprArrayLit es'))
        | ExprKind.ExprDotDot (a, b) ->
            let (sa, a') = r a
            let (sb, b') = r b
            (sa @ sb, inheritSpan e (ExprDotDot (a', b')))
        // Branch arms evaluate conditionally: hoisting a site out of one
        // would evaluate it unconditionally, so arms wrap locally instead.
        | ExprKind.ExprIf (c, t, f) ->
            let (sc, c') = r c
            (sc, inheritSpan e (ExprIf (c', wrapped t, wrapped f)))
        | ExprKind.ExprLet (b, bodyE) ->
            let (sv, v') = r b.Value
            (sv, inheritSpan e (ExprLet ({ b with Value = v' }, wrapped bodyE)))
        | ExprKind.ExprLambda (ps, w, bodyE) -> ([], inheritSpan e (ExprLambda (ps, w, wrapped bodyE)))
        | ExprKind.ExprMatch (s, cases) ->
            let (ss, s') = r s
            (ss, inheritSpan e (ExprMatch (s', cases |> List.map (fun c -> { c with Guard = rOpt c.Guard; Body = wrapped c.Body }))))
        // A block hosts its own statement list: everything hoisted inside
        // stays inside (statements splice in place, final-expression hoists
        // append after the last statement).
        | ExprKind.ExprBlock (stmts, fin) ->
            let stmts' = List.collect goStmt stmts
            (match fin with
             | Some fe ->
                 let (sf, fe') = r fe
                 ([], inheritSpan e (ExprBlock (stmts' @ sf, Some fe')))
             | None -> ([], inheritSpan e (ExprBlock (stmts', None))))
        | ExprKind.ExprCompute a -> ([], inheritSpan e (ExprCompute (wrapped a)))
        | ExprKind.ExprPure a -> ([], inheritSpan e (ExprPure (wrapped a)))
        | ExprKind.ExprReduce (a, k, init, ax) ->
            let (ss, a') = r a
            // `ax` (the axes selector) is passed through unrewritten, matching
            // every other module walker (rand/math/spectra) and this file's own.
            (ss, inheritSpan e (ExprReduce (a', wrapped k, rOpt init, ax)))
        | ExprKind.ExprMethodFor es -> let (ss, es') = rList es in (ss, inheritSpan e (ExprMethodFor es'))
        | ExprKind.ExprZip es -> let (ss, es') = rList es in (ss, inheritSpan e (ExprZip es'))
        | ExprKind.ExprField (obj, fld) -> let (ss, o') = r obj in (ss, inheritSpan e (ExprField (o', fld)))
        | ExprKind.ExprTupleIndex (t, i) ->
            let (st, t') = r t
            let (si, i') = r i
            (st @ si, inheritSpan e (ExprTupleIndex (t', i')))
        | ExprKind.ExprReplicate (c, b) ->
            let (sc, c') = r c
            (sc, inheritSpan e (ExprReplicate (c', wrapped b)))
        | ExprKind.ExprGuard (c, b) ->
            let (sc, c') = r c
            (sc, inheritSpan e (ExprGuard (c', wrapped b)))
        // Everything else is left untouched: a density former buried in a
        // node this walker does not descend is still caught by pass 3.
        | _ -> ([], e)
    let body' =
        match body.Kind with
        // A block body absorbs its hoists itself; a bare-expression body
        // grows a block only when a site actually rewrote.
        | ExprKind.ExprBlock _ -> snd (go body)
        | _ ->
            match go body with
            | ([], e') -> e'
            | (ss, e') -> syn (ExprBlock (ss, Some e'))
    match err with
    | Some m -> Error m
    | None -> Ok body'

// =========================================================================
// The approximate tower bridge (docs/plan-ppl-proper.md section 2) -- an
// honest approximate density/quantile for ANY carried univariate tower,
// family-constructed or data-estimated alike:
//
//   dist_pdf_approx(d, x)       Edgeworth/Gram-Charlier density from
//                               kappa_1..kappa_r. NOT a probability density
//                               in general: the expansion can go negative in
//                               the tails -- dist_negativity is the honesty
//                               meter for exactly that (pair them).
//   dist_quantile_approx(d, p)  Cornish-Fisher quantile: the formal series
//                               inverse of the Edgeworth CDF, on top of the
//                               standard-normal quantile (Wichura AS241
//                               PPND16, emitted as plain arithmetic).
//   dist_sample_approx(d, k, n) a keyed __rand_uniform fill mapped through
//                               the Cornish-Fisher transform -- approximate
//                               tower sampling, deterministic given the key,
//                               landable before any family-specific runtime
//                               fill exists.
//
// Both expansions are generated from the same formal-series construction the
// reference oracle uses (oracles/ppl/Density.fs, module Expansion) -- the
// eps-truncation bookkeeping below mirrors it line for line, only over a
// symbolic coefficient ring (sparse polynomials in z, phi(z), and the
// standardized cumulants lambda_3..lambda_6) instead of floats, so the
// collapsed series can be emitted as straight-line arithmetic over
// runtime-read kappas. r = 2 carries no correction terms: the Edgeworth
// density of a Gaussian tower is the exact Gaussian pdf and Cornish-Fisher
// degenerates to mu + sd * PPND(p) exactly. An order-r tower supports terms
// through eps^(r-2); r > 6 (unreachable through today's constructors, all of
// which cap at 6) and r < 2 refuse rather than truncate silently.
//
// kappa_2 <= 0 (lawful for quasi-distribution towers -- dist_atoms admits
// negative variance) is NOT refused at elaboration: sd = sqrt(kappa_2)
// evaluates to NaN at runtime and the NaN propagates through every read,
// which is the honest answer for a tower with no Gaussian anchor.
// =========================================================================

// ---- sparse polynomials over (z, phi(z), lambda_3..lambda_6) ----

/// Monomial key: z power, phi(z) power, lambda_3..lambda_6 powers.
type private AxMono = int * int * int list

let private axMonoOne : AxMono = (0, 0, [0; 0; 0; 0])
let private axZeroP : Map<AxMono, float> = Map.empty
let private axConst (c: float) : Map<AxMono, float> =
    if c = 0.0 then Map.empty else Map.ofList [ (axMonoOne, c) ]
let private axZ : Map<AxMono, float> = Map.ofList [ ((1, 0, [0; 0; 0; 0]), 1.0) ]
let private axPhi : Map<AxMono, float> = Map.ofList [ ((0, 1, [0; 0; 0; 0]), 1.0) ]
let private axLam (k: int) : Map<AxMono, float> =
    Map.ofList [ ((0, 0, [ for i in 0 .. 3 -> if i = k - 3 then 1 else 0 ]), 1.0) ]

let private axAdd (a: Map<AxMono, float>) (b: Map<AxMono, float>) : Map<AxMono, float> =
    b |> Map.fold (fun acc m c ->
        let c2 = (defaultArg (Map.tryFind m acc) 0.0) + c
        if c2 = 0.0 then Map.remove m acc else Map.add m c2 acc) a
let private axScale (c: float) (a: Map<AxMono, float>) : Map<AxMono, float> =
    if c = 0.0 then axZeroP else a |> Map.map (fun _ x -> c * x)
let private axSub (a: Map<AxMono, float>) (b: Map<AxMono, float>) = axAdd a (axScale -1.0 b)
let private axMul (a: Map<AxMono, float>) (b: Map<AxMono, float>) : Map<AxMono, float> =
    a |> Map.fold (fun acc (za, pa, la) ca ->
        b |> Map.fold (fun acc2 (zb, pb, lb) cb ->
            let m = (za + zb, pa + pb, List.map2 (+) la lb)
            let c2 = (defaultArg (Map.tryFind m acc2) 0.0) + ca * cb
            if c2 = 0.0 then Map.remove m acc2 else Map.add m c2 acc2) acc) axZeroP

// ---- truncated power series in the bookkeeping parameter eps (index =
// eps power), coefficients in the polynomial ring above; the float twin
// lives in the oracle's Density.fs Eps module ----

let private aeZero (n: int) : Map<AxMono, float>[] = Array.create (n + 1) axZeroP
let private aeKonst (n: int) (p: Map<AxMono, float>) : Map<AxMono, float>[] =
    let a = aeZero n
    a.[0] <- p
    a
let private aeAdd a b : Map<AxMono, float>[] = Array.map2 axAdd a b
let private aeSub a b : Map<AxMono, float>[] = Array.map2 axSub a b
let private aeScaleF (c: float) (a: Map<AxMono, float>[]) = Array.map (axScale c) a
let private aeScaleP (p: Map<AxMono, float>) (a: Map<AxMono, float>[]) = Array.map (axMul p) a
let private aeMul (a: Map<AxMono, float>[]) (b: Map<AxMono, float>[]) : Map<AxMono, float>[] =
    let n = a.Length - 1
    let out = aeZero n
    for i in 0 .. n do
        if not (Map.isEmpty a.[i]) then
            for j in 0 .. n - i do
                out.[i + j] <- axAdd out.[i + j] (axMul a.[i] b.[j])
    out
/// a / b by forward substitution. b's head must be a single monomial (it is
/// phi(z) in the Cornish-Fisher solve) and every division must be exact --
/// anything else is a compiler bug, not a user error.
let private aeDiv (a: Map<AxMono, float>[]) (b: Map<AxMono, float>[]) : Map<AxMono, float>[] =
    let n = a.Length - 1
    let q = aeZero n
    let (m0, c0) =
        match Map.toList b.[0] with
        | [ one ] -> one
        | _ -> failwith "ppl approx bridge: series division by a non-monomial head (compiler bug)"
    let divMono (p: Map<AxMono, float>) : Map<AxMono, float> =
        p |> Map.toList |> List.map (fun ((z, ph, l), c) ->
            let (z0, p0, l0) = m0
            let z2 = z - z0
            let p2 = ph - p0
            let l2 = List.map2 (-) l l0
            if z2 < 0 || p2 < 0 || List.exists (fun x -> x < 0) l2 then
                failwith "ppl approx bridge: inexact monomial division in the Cornish-Fisher solve (compiler bug)"
            ((z2, p2, l2), c / c0))
        |> Map.ofList
    for i in 0 .. n do
        let mutable s = a.[i]
        for j in 1 .. i do
            s <- axSub s (axMul b.[j] q.[i - j])
        q.[i] <- divMono s
    q
/// Substitute eps = 1.
let private aeCollapse (a: Map<AxMono, float>[]) : Map<AxMono, float> = Array.fold axAdd axZeroP a

/// Probabilists' Hermite polynomials He_0(z)..He_n(z) in the z symbol.
let private hermiteZ (n: int) : Map<AxMono, float>[] =
    let h = Array.create (max 1 (n + 1)) axZeroP
    h.[0] <- axConst 1.0
    if n >= 1 then h.[1] <- axZ
    for m in 1 .. n - 1 do
        h.[m + 1] <- axSub (axMul axZ h.[m]) (axScale (float m) h.[m - 1])
    h

/// Coefficients c_j of phi_Z(t) = e^(-t^2/2) sum_j c_j (it)^j as eps-series
/// over the lambda ring, truncated at eps^(r-2) -- the symbolic twin of the
/// oracle's charCoeffs (Density.fs), same maxJ = 3n bound, same loop shape.
let private axCharCoeffs (r: int) : Map<AxMono, float>[][] =
    let n = r - 2
    let maxJ = 3 * n
    let s = Array.init (maxJ + 1) (fun _ -> aeZero n)
    for k in 3 .. r do
        if k - 2 <= n && k <= maxJ then
            s.[k].[k - 2] <- axScale (1.0 / factorial k) (axLam k)
    let res = Array.init (maxJ + 1) (fun _ -> aeZero n)
    res.[0].[0] <- axConst 1.0
    let mutable pow = Array.init (maxJ + 1) (fun _ -> aeZero n)
    pow.[0].[0] <- axConst 1.0
    for m in 1 .. n do
        let next = Array.init (maxJ + 1) (fun _ -> aeZero n)
        for i in 0 .. maxJ do
            for k in 3 .. maxJ do
                if i + k <= maxJ then
                    next.[i + k] <- aeAdd next.[i + k] (aeMul pow.[i] s.[k])
        pow <- next
        let w = 1.0 / factorial m
        for j in 0 .. maxJ do
            res.[j] <- aeAdd res.[j] (aeScaleF w pow.[j])
    res

/// Collapsed (eps = 1) Edgeworth bracket coefficients: index j -> the
/// lambda-only polynomial multiplying He_j(z), as (coefficient, lambda
/// exponents) term lists (deterministic Map order).
let private axEdgeworthCoeffs (r: int) : (float * int list) list [] =
    axCharCoeffs r
    |> Array.map (fun series ->
        aeCollapse series
        |> Map.toList
        |> List.map (fun ((z, p, lams), c) ->
            if z <> 0 || p <> 0 then failwith "ppl approx bridge: Edgeworth coefficient escaped the lambda ring (compiler bug)"
            (c, lams)))

/// The collapsed Cornish-Fisher correction U with x_p = mu + sd*(z + U),
/// grouped by z power: z degree -> lambda term list. Mirrors the oracle's
/// series-Newton (Density.fs cornishFisher) iteration for iteration; the
/// phi(z) factors cancel exactly in the solve, which the fold asserts.
/// Caller guarantees r >= 3 (r = 2 has no correction at all).
let private axCornishFisherU (r: int) : Map<int, (float * int list) list> =
    let n = r - 2
    let c = axCharCoeffs r
    let maxJ = 3 * n
    let heZ = hermiteZ (maxJ + 1)
    let mutable u = aeZero n
    for _ in 1 .. n + 4 do
        let w = aeAdd (aeKonst n axZ) u
        let heW = Array.init (maxJ + 1) (fun _ -> aeZero n)
        heW.[0] <- aeKonst n (axConst 1.0)
        if maxJ >= 1 then heW.[1] <- w
        for m in 1 .. maxJ - 1 do
            heW.[m + 1] <- aeSub (aeMul w heW.[m]) (aeScaleF (float m) heW.[m - 1])
        // Phi(z+u) - Phi(z) = sum_{i>=1} (-1)^(i-1) He_(i-1)(z) phi(z) u^i / i!
        let mutable dPhi = aeZero n
        let mutable up = aeKonst n (axConst 1.0)
        for i in 1 .. n do
            up <- aeMul up u
            let sgn = if (i - 1) % 2 = 0 then 1.0 else -1.0
            dPhi <- aeAdd dPhi (aeScaleP (axScale (sgn / factorial i) (axMul heZ.[i - 1] axPhi)) up)
        // phi(z+u) = sum_{i>=0} (-1)^i He_i(z) phi(z) u^i / i!
        let mutable phiW = aeZero n
        up <- aeKonst n (axConst 1.0)
        for i in 0 .. n do
            if i > 0 then up <- aeMul up u
            let sgn = if i % 2 = 0 then 1.0 else -1.0
            phiW <- aeAdd phiW (aeScaleP (axScale (sgn / factorial i) (axMul heZ.[i] axPhi)) up)
        let mutable tail = aeZero n
        for j in 1 .. maxJ do
            tail <- aeAdd tail (aeMul c.[j] heW.[j - 1])
        let g = aeSub dPhi (aeMul phiW tail)
        let mutable dens = aeZero n
        for j in 0 .. maxJ do
            dens <- aeAdd dens (aeMul c.[j] heW.[j])
        u <- aeSub u (aeDiv g (aeMul phiW dens))
    aeCollapse u
    |> Map.toList
    |> List.fold (fun acc ((z, p, lams), coeff) ->
        if p <> 0 then failwith "ppl approx bridge: phi(z) failed to cancel in the Cornish-Fisher series (compiler bug)"
        let cur = defaultArg (Map.tryFind z acc) []
        Map.add z (cur @ [ (coeff, lams) ]) acc) Map.empty

/// A lambda-monomial term list as an Expr over the bound lambda decls.
let private axTermsExpr (lam: int -> Expr) (terms: (float * int list) list) : Expr =
    match terms with
    | [] -> fLit 0.0
    | _ ->
        terms
        |> List.map (fun (c, pows) ->
            pows
            |> List.mapi (fun i pw -> (i + 3, pw))
            |> List.filter (fun (_, pw) -> pw > 0)
            |> List.fold (fun acc (k, pw) -> mulE acc (powN (lam k) pw)) (fLit c))
        |> List.reduce addE

// ---- standard normal quantile: Wichura AS241 PPND16, coefficient tables
// copied verbatim from the oracle (Density.fs normalQuantile) so the two
// implementations agree to reordering-of-arithmetic ----

let private ppndCentralNum = [ 2509.0809287301226727; 33430.575583588128105; 67265.770927008700853; 45921.953931549871457; 13731.693765509461125; 1971.5909503065514427; 133.14166789178437745; 3.387132872796366608 ]
let private ppndCentralDen = [ 5226.495278852854561; 28729.085735721942674; 39307.89580009271061; 21213.794301586595867; 5394.1960214247511077; 687.1870074920579083; 42.313330701600911252; 1.0 ]
let private ppndMidNum = [ 7.7454501427834140764e-4; 0.0227238449892691845833; 0.24178072517745061177; 1.27045825245236838258; 3.64784832476320460504; 5.7694972214606914055; 4.6303378461565452959; 1.42343711074968357734 ]
let private ppndMidDen = [ 1.05075007164441684324e-9; 5.475938084995344946e-4; 0.0151986665636164571966; 0.14810397642748007459; 0.68976733498510000455; 1.6763848301838038494; 2.05319162663775882187; 1.0 ]
let private ppndFarNum = [ 2.01033439929228813265e-7; 2.71155556874348757815e-5; 0.00124266094738807843860; 0.026532189526576123093; 0.29656057182850489123; 1.7848265399172913358; 5.4637849111641143699; 6.6579046435011037772 ]
let private ppndFarDen = [ 2.04426310338993978564e-15; 1.4215117583164458887e-7; 1.8463183175100546818e-5; 7.868691311456132591e-4; 0.0148753612908506148525; 0.13692988092273580531; 0.59983220655588793769; 1.0 ]

/// ((c_hi * r + c_next) * r + ...) -- the exact nesting the oracle writes out.
let private hornerE (cs: float list) (r: Expr) : Expr =
    match cs with
    | [] -> fLit 0.0
    | c :: rest -> rest |> List.fold (fun acc k -> addE (mulE acc r) (fLit k)) (fLit c)

/// The central branch q * A(rc)/B(rc), rc = 0.180625 - q^2 (|q| <= 0.425).
let private ppndCentral (q: Expr) : Expr =
    let rc = subE (fLit 0.180625) (mulE q q)
    mulE q (divE (hornerE ppndCentralNum rc) (hornerE ppndCentralDen rc))

/// The tail value from r1 = sqrt(-log(min(p, 1-p))): moderate branch at
/// r1 <= 5, far branch beyond (sign applied by the caller).
let private ppndTail (r1: Expr) : Expr =
    let rm = subE r1 (fLit 1.6)
    let rf = subE r1 (fLit 5.0)
    ifE (leE r1 (fLit 5.0))
        (divE (hornerE ppndMidNum rm) (hornerE ppndMidDen rm))
        (divE (hornerE ppndFarNum rf) (hornerE ppndFarDen rf))

/// PPND16 as ONE expression over p -- kernel position (no local bindings in a
/// kernel body, so shared sub-terms are duplicated structurally; they are
/// pure arithmetic and the duplication is per-source-site, not per-element
/// state). Scalar position binds the pieces as decls instead (quantile former).
let private ppndExprOf (p: Expr) : Expr =
    let q = subE p (fLit 0.5)
    let r0 = ifE (ltE q (fLit 0.0)) p (subE (fLit 1.0) p)
    let r1 = sqrtE (negE (logE r0))
    let vt = ppndTail r1
    ifE (leE (absE q) (fLit 0.425)) (ppndCentral q) (ifE (ltE q (fLit 0.0)) (negE vt) vt)

// ---- the three approximate-bridge formers ----

/// Shared setup: registry lookup, univariate + order gates, and the
/// standardization decls mu = kappa_1, sd = sqrt(kappa_2) (NaN when
/// kappa_2 < 0 -- see the section comment), lambda_k = kappa_k / sd^k.
/// Reads go through distKappaRead, so packed (dist(A, r)) and flat
/// (family constructors, jet/Bayes results) towers both work.
let private approxSetupDecls (former: string) (ctx: Ctx) (span: Span) (tag: string)
    (dists: Map<string, DistInfo>) (dn: string)
    : Result<Located<Decl> list * int * Expr * Expr * (int -> Expr), string> =
    match Map.tryFind dn dists with
    | None -> Error $"{former} expects {former}(d, ...) with a previously declared dist binding d"
    | Some info ->
        univariateOnly former ctx info |> Result.bind (fun () ->
        if info.Order < 2 then
            Error $"{former}: the expansion is anchored on kappa_2 and '{dn}' carries only order {info.Order} -- insufficient stochastic order. Construct with order >= 2 (dist(A, 2..6) or a family constructor)."
        elif info.Order > 6 then
            Error $"{former}: the Edgeworth/Cornish-Fisher series are generated up to order 6 and '{dn}' carries {info.Order} -- rebuild or project the tower at order <= 6"
        else
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let r = info.Order
            let muN = $"__ppl_ax_{tag}_mu"
            let sdN = $"__ppl_ax_{tag}_sd"
            let lamN k = $"__ppl_ax_{tag}_l{k}"
            let decls =
                [ bind muN (distKappaRead info [0])
                  bind sdN (sqrtE (distKappaRead info [0; 0])) ]
                @ [ for k in 3 .. r ->
                      bind (lamN k) (divE (distKappaRead info (List.replicate k 0)) (powN (v sdN) k)) ]
            Ok (decls, r, v muN, v sdN, (fun k -> v (lamN k))))

/// Emit the z-grouped Cornish-Fisher correction coefficients as decls and a
/// Horner builder for U(z) over them. Caller guarantees r >= 3.
let private cfCorrectionDecls (span: Span) (tag: string) (lam: int -> Expr) (r: int)
    : Located<Decl> list * (Expr -> Expr) =
    let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
    let grouped = axCornishFisherU r
    let maxDeg = grouped |> Map.toList |> List.map fst |> List.max
    let cName i = $"__ppl_cf_{tag}_c{i}"
    let decls =
        [ for i in 0 .. maxDeg do
            match Map.tryFind i grouped with
            | Some terms -> yield bind (cName i) (axTermsExpr lam terms)
            | None -> () ]
    let horner (z: Expr) =
        let cRef i = if Map.containsKey i grouped then v (cName i) else fLit 0.0
        let rec go i acc = if i < 0 then acc else go (i - 1) (addE (cRef i) (mulE z acc))
        go (maxDeg - 1) (cRef maxDeg)
    (decls, horner)

/// dist_pdf_approx(d, x): phi(z) * [sum_j c_j(lambda) He_j(z)] / sd at
/// z = (x - mu)/sd -- He_j by the recurrence as decls, the c_j as collapsed
/// lambda polynomials.
let private elabDistPdfApprox (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar dn }; xExpr] ->
        approxSetupDecls "dist_pdf_approx" ctx span ("ea_" + outName) dists dn
        |> Result.map (fun (setup, r, mu, sd, lam) ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let maxJ = 3 * (r - 2)
            let cs = axEdgeworthCoeffs r
            let xN = $"__ppl_ea_{outName}_x"
            let zN = $"__ppl_ea_{outName}_z"
            let fN = $"__ppl_ea_{outName}_f"
            let heName j = $"__ppl_ea_{outName}_he{j}"
            let z = v zN
            let heDecls =
                [ for j in 0 .. maxJ ->
                    let value =
                        if j = 0 then fLit 1.0
                        elif j = 1 then z
                        else subE (mulE z (v (heName (j - 1)))) (mulE (fLit (float (j - 1))) (v (heName (j - 2))))
                    bind (heName j) value ]
            let factor =
                [ for j in 0 .. maxJ do
                    match cs.[j] with
                    | [] -> ()
                    | terms -> yield mulE (axTermsExpr lam terms) (v (heName j)) ]
                |> List.reduce addE   // c_0 = 1 always survives
            let sqrt2pi = sqrt (2.0 * System.Math.PI)
            let value = divE (mulE (divE (expE (mulE (mulE (fLit -0.5) z) z)) (fLit sqrt2pi)) (v fN)) sd
            setup
            @ [ bind xN xExpr
                bind zN (divE (subE (v xN) mu) sd) ]
            @ heDecls
            @ [ bind fN factor
                { Value = DeclLet { binding with Value = value }; Span = span } ])
    | _ ->
        Error "dist_pdf_approx expects dist_pdf_approx(d, x): a previously declared univariate dist binding and the evaluation point"

/// dist_quantile_approx(d, p): mu + sd*(z + U(z, lambda)) at z = PPND16(p),
/// AS241 pieces bound as scalar decls (branching stays in ExprIf decl RHSs).
let private elabDistQuantileApprox (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar dn }; pExpr] ->
        approxSetupDecls "dist_quantile_approx" ctx span ("cf_" + outName) dists dn
        |> Result.map (fun (setup, r, mu, sd, lam) ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let nm suffix = $"__ppl_cf_{outName}_{suffix}"
            let pN, qN, r0N, r1N, ceN, vtN, zN = nm "p", nm "q", nm "r0", nm "r1", nm "ce", nm "vt", nm "z"
            let z = v zN
            let ppndDecls =
                [ bind pN pExpr
                  bind qN (subE (v pN) (fLit 0.5))
                  bind r0N (ifE (ltE (v qN) (fLit 0.0)) (v pN) (subE (fLit 1.0) (v pN)))
                  bind r1N (sqrtE (negE (logE (v r0N))))
                  bind ceN (ppndCentral (v qN))
                  bind vtN (ppndTail (v r1N))
                  bind zN (ifE (leE (absE (v qN)) (fLit 0.425)) (v ceN)
                               (ifE (ltE (v qN) (fLit 0.0)) (negE (v vtN)) (v vtN))) ]
            let tailDecls, value =
                if r = 2 then [], addE mu (mulE sd z)
                else
                    let (cDecls, horner) = cfCorrectionDecls span outName lam r
                    cDecls, addE mu (mulE sd (addE z (horner z)))
            setup @ ppndDecls @ tailDecls
            @ [ { Value = DeclLet { binding with Value = value }; Span = span } ])
    | _ ->
        Error "dist_quantile_approx expects dist_quantile_approx(d, p): a previously declared univariate dist binding and a probability"

/// Static sample-count resolution shared by sample/dist_sample_approx.
let private staticSampleCount (ctx: Ctx) (former: string) (nExpr: Expr) : Result<int, string> =
    match evalExpr ctx.Statics maxSteps nExpr with
    | Ok (SVInt x) when x >= 1L -> Ok (int x)
    | Ok (SVInt x) -> Error $"{former}: the sample count must be >= 1 (got {x})"
    | _ -> Error $"{former}: the sample count must be a compile-time integer (a literal, `let static`, or static-function call) -- shapes are static everywhere in Blade; only the key and distribution parameters may be runtime values"

/// dist_sample_approx(d, key, n): a keyed uniform fill mapped through PPND16
/// and then the Cornish-Fisher transform -- two elementwise passes, both
/// backends free, deterministic given the key.
let private elabDistSampleApprox (ctx: Ctx) (span: Span) (sName: string) (binding: Binding)
    (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar dn }; keyE; nExpr] ->
        staticSampleCount ctx "dist_sample_approx" nExpr |> Result.bind (fun m ->
        approxSetupDecls "dist_sample_approx" ctx span ("sa_" + sName) dists dn
        |> Result.map (fun (setup, r, mu, sd, lam) ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let uN = $"__ppl_sa_{sName}_u"
            let zN = $"__ppl_sa_{sName}_z"
            let cDecls, body =
                if r = 2 then [], addE mu (mulE sd (v "__u"))
                else
                    let (cds, horner) = cfCorrectionDecls span ("sa_" + sName) lam r
                    cds, addE mu (mulE sd (addE (v "__u") (horner (v "__u"))))
            setup
            @ [ bind uN (appE (v "__rand_uniform") [keyE; iLit m])
                bind zN (map1 (v uN) (ppndExprOf (v "__u"))) ]
            @ cDecls
            @ [ { Value = DeclLet { binding with Value = map1 (v zN) body }; Span = span } ]))
    | _ ->
        Error "dist_sample_approx expects dist_sample_approx(d, key, n): a previously declared univariate dist binding, an Int64 stream key, and a static sample count"

// ---- ppl.sample: exact family sampling over the __rand_* intrinsics ----
// sample(family(params), key, n) elaborates to the matching keyed batch fill
// (docs/plan-ppl-proper.md section 3): legal without `import rand` because
// the __rand_* intrinsics are checker-level builtins, and the checker's own
// arity table re-validates the parameter count on the intrinsic itself.
// exponential/gamma/poisson/bernoulli/beta lower to their direct fills
// (parameters are runtime Float64 scalars, passed through verbatim -- the
// rand-elaborator convention); gaussian/lognormal/uniform are synthesized as
// elementwise transforms over the normal/uniform fills:
//   gaussian(mu, s2)  -> mu + sqrt(s2) * __rand_normal(key, n)
//   lognormal(mu, s2) -> exp(gaussian construction)
//   uniform(a, b)     -> a + (b - a) * __rand_uniform(key, n)
// The result is a rank-1 Array<Float64 like Idx<n>>; __rand_* output stays
// non-differentiable (the rand/grad boundary is unchanged).
let private elabPplSample (active: string -> bool) (ctx: Ctx) (span: Span) (sName: string) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [famE; keyE; nExpr] ->
        familyArg "sample" active famE |> Result.bind (fun (fam, ps) ->
        checkSymbolicFamily ctx "sample" "sample position" fam ps |> Result.bind (fun () ->
        staticSampleCount ctx "sample" nExpr |> Result.map (fun m ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let direct fill = [ { Value = DeclLet { binding with Value = appE (v fill) (keyE :: ps @ [iLit m]) }; Span = span } ]
            match fam with
            | "exponential" -> direct "__rand_exponential"
            | "gamma"       -> direct "__rand_gamma"
            | "poisson"     -> direct "__rand_poisson"
            | "bernoulli"   -> direct "__rand_bernoulli"
            | "beta"        -> direct "__rand_beta"
            | "gaussian" | "lognormal" ->
                let muN = $"__ppl_sm_{sName}_mu"
                let sdN = $"__ppl_sm_{sName}_sd"
                let fN = $"__ppl_sm_{sName}_fill"
                let affine = addE (v muN) (mulE (v sdN) (v "__u"))
                let body = if fam = "gaussian" then affine else expE affine
                [ bind muN ps.[0]
                  bind sdN (sqrtE ps.[1])
                  bind fN (appE (v "__rand_normal") [keyE; iLit m])
                  { Value = DeclLet { binding with Value = map1 (v fN) body }; Span = span } ]
            | "uniform" ->
                let aN = $"__ppl_sm_{sName}_a"
                let wN = $"__ppl_sm_{sName}_w"
                let fN = $"__ppl_sm_{sName}_fill"
                [ bind aN ps.[0]
                  bind wN (subE ps.[1] (v aN))
                  bind fN (appE (v "__rand_uniform") [keyE; iLit m])
                  { Value = DeclLet { binding with Value = map1 (v fN) (addE (v aN) (mulE (v wN) (v "__u"))) }; Span = span } ]
            | _ -> [])))   // unreachable: familyArg gates on familyNames
    | _ ->
        Error "sample expects sample(family(params), key, n): a symbolic family argument, an Int64 stream key, and a static sample count"

// =========================================================================
// P4 -- sampling inference (docs/plan-ppl-proper.md section 5): the chain
// machinery and its diagnostics.
//
//   mh(logpost, x0, n, scale, key)   random-walk Metropolis. The chain is the
//       language's own sequential construct -- a RECURSIVE ARRAY whose slice
//       calls a synthesized step function (the corpus-blessed block-wrapped
//       `let rec` + per-step function shape, sgs/015 / ml-e2e/001):
//         let <name> = {
//             let rec __c: Array<Float64 like Idx<n>> =
//                 match __c with
//                 | zero -> zero
//                 | zero :: s -> zero :: x0
//                 | prefix :: t -> prefix :: __step(prefix(t - 1), t)
//             __c }
//       All randomness is pregenerated OUTSIDE the chain as two keyed batch
//       fills of length n -- proposals __rand_normal(key + 1000003, n) and
//       accept draws __rand_uniform(key + 2000003, n) -- so the sweep itself
//       is deterministic arithmetic. Distinct additive site constants are
//       enough key discipline here: the rand runtime reseeds mt19937_64
//       through the SplitMix64/mix64 finalizer per call, which decorrelates
//       even ADJACENT keys (the rand/004 pin), so any two distinct offsets
//       give independent streams; the constants are >= 1e6 apart so two
//       user keys for two chains cannot collide with each other's offsets
//       unless they differ by exactly 1000000. Element 0 of each fill is
//       unused (the seed slice consumes no randomness); step t reads fill
//       slot t. The step function holds the proposal and the accept/reject
//       CONDITIONAL EXPRESSION -- legal here, nothing in the sampler loop is
//       ever differentiated -- and is the seam the HMC step function fills
//       with a leapfrog sweep; `logpost` is called BY NAME (recursive-array
//       slices call ordinary same-module functions; no inlining needed,
//       unlike dist_map's symbolic differentiation which must see one closed
//       expression). Convention: chain(0) = x0; chain(t), t >= 1, are the
//       MH states, so a length-n chain carries n-1 transitions.
//
//   hmc(logpost, x0, n, eps, L, key)   fixed-leapfrog Hamiltonian Monte
//       Carlo over the same recursive-array chain shape (momenta
//       __rand_normal(key + 3000017, n), accept draws
//       __rand_uniform(key + 4000037, n) -- new site constants, so an mh and
//       an hmc chain under the SAME user key stay decorrelated). Energy
//       convention: H(q, p) = -logpost(q) + p^2/2 (unit mass), so the MH
//       correction accepts when log u < dH = (logpost(q_L) - logpost(q_0)) +
//       (p_0^2 - p_L^2)/2. The step function runs the standard L-step
//       velocity-Verlet sweep with L+1 gradient evaluations -- initial half
//       momentum step, L-1 interleaved full steps, final position update and
//       half momentum step -- with each gradient obtained by calling the
//       `<ad>.grad(logpost)` SURFACE FORM (gate (a) of the plan: the Grad
//       pass runs after ppl elaboration and expands each site to the
//       synthesized `logpost__grad`, whose single-scalar-param return is the
//       (primal, gradient) tuple -- both leapfrog endpoint log-posteriors
//       therefore come for free from the same calls). `import ad` is
//       REQUIRED in the module: without it the Grad pass is a no-op and the
//       emitted surface form would be unbound, so hmc refuses with steering
//       instead. The model must sit inside the AD-able subset
//       (src/Grad.fs:27-50); models outside it get grad's own BL5500
//       refusal, which is the honest boundary. The sampler loop itself is
//       never differentiated -- the accept/reject if-expr and the leapfrog
//       for-loop live in the step function, not the model.
//
//   chain_mean(c, burn) / chain_var(c, burn)   moments of the post-burn
//       suffix c(burn..n-1); population normalization (/m, matching the
//       module's moment estimators). Emitted as the elabLogLik scalar
//       accumulation-loop idiom over a `__` read alias.
//   autocorr(c, maxlag)   lag-0..maxlag autocorrelation array: rho_k =
//       sum_{t<n-k} (x_t - m)(x_{t+k} - m) / sum_t (x_t - m)^2 (the biased
//       estimator, standard for ESS); rho_0 = 1 by construction. A constant
//       chain has zero denominator and honestly reports NaN.
//   ess(c)   effective sample size via Geyer's INITIAL POSITIVE SEQUENCE:
//       pair sums G_k = rho_{2k} + rho_{2k+1} for k = 0..P-1 with
//       P = min((n-2)/2, 128) pairs; truncate at the FIRST non-positive
//       pair (it and everything after contribute nothing); tau = -1 +
//       2 * sum of surviving G_k; ess = n / tau. No initial-convex
//       monotonicity pass, and tau is not clamped (an antithetic chain may
//       lawfully report ess > n).
//   rhat(c1, c2)   split-Rhat over two equal-length chains: each splits in
//       half -> J = 4 segments of m = n/2; within W = mean of the 4 sample
//       variances (/(m-1)); between B = m * variance of the 4 segment means
//       (/(J-1)); Rhat = sqrt(((m-1)/m * W + B/m) / W).
//
// The diagnostics accept an mh/hmc chain from this module (length registered
// at elaboration) or any module-level rank-1 Float array with a static
// extent (annotated or computed) -- hand pins ride literal arrays. autocorr/
// ess/rhat lower to synthesized top-level FUNCTIONS whose bodies are the
// mut-array + element-write + for-loop shape grad() already generates and
// the whole pipeline supports; chain_mean/chain_var are module-block
// accumulation loops. `dist(chain, r)` -- the round-trip that unifies the
// module -- composes too: a chain carries no declared annotation, so the
// dist dispatch arm consults the chain registry and wraps the rank-1 chain
// to Idx<1> x Idx<n> (the corpus-096 range-kernel idiom) before the
// ordinary dist elaboration.
// =========================================================================

let private tyFloat64 = TyNamed ("Float64", [])
let private tyIntP = TyNamed ("Int", [])
let private tyFloatArr (n: int) = TyArray (tyFloat64, [TyIdx (iLit n)])
let private mkParamD (nm: string) (ty: TypeExpr) : ParamDecl =
    { Name = nm; Type = Some ty; Mutability = Immutable; Default = None; NameSpan = noSpan }
let private mkFnDecl (span: Span) (name: string) (ps: ParamDecl list) (ret: TypeExpr) (body: Expr) : Located<Decl> =
    { Value = DeclFunction { Name = name; TypeParams = []; Params = ps; WhereClause = None
                             ReturnType = Some ret; Body = body; IsStatic = false; NameSpan = noSpan }
      Span = span }
/// Shared logpost-name guards for mh/hmc: the first argument must NAME a
/// top-level same-module Float -> Float function. `example` seeds the
/// steering text with the former's own call shape.
let private samplerLogpost (former: string) (example: string) (funcs: Map<string, FunctionDecl>) (lpE: Expr)
    : Result<string, string> =
    (match lpE.Kind with
     | ExprKind.ExprVar f -> Ok f
     | _ -> Error $"{former}: the first argument must be the NAME of a top-level same-module function (the log-posterior, Float -> Float), e.g. {example}")
    |> Result.bind (fun f ->
    match Map.tryFind f funcs with
    | None ->
        Error $"{former}: unknown log-posterior '{f}' -- it must be a top-level function in this module (Float -> Float); assemble it from ppl.logpdf/ppl.loglik terms"
    | Some fd ->
        let isFloatTy = function
            | TyFloat64 | TyFloat32 -> true
            | TyNamed (("Float" | "Float64" | "Float32" | "Double"), []) -> true
            | _ -> false
        if fd.Params.Length <> 1 then
            Error $"{former}: '{f}' takes {fd.Params.Length} parameter(s) -- this wave samples a SCALAR latent, so the log-posterior must be Float -> Float; fold extra structure into module-level data arrays"
        elif not (fd.Params.Head.Type |> Option.forall isFloatTy) then
            Error $"{former}: '{f}' must take a scalar Float latent (its parameter is annotated otherwise)"
        elif not (fd.ReturnType |> Option.forall isFloatTy) then
            Error $"{former}: '{f}' must return a Float log-density (its return type is annotated otherwise)"
        else Ok f)

/// Static chain length for mh/hmc: n >= 2, element 0 is the seed state.
let private samplerChainLen (ctx: Ctx) (former: string) (runtimeOnes: string) (nE: Expr) : Result<int, string> =
    match evalExpr ctx.Statics maxSteps nE with
    | Ok (SVInt x) when x >= 2L -> Ok (int x)
    | Ok (SVInt x) -> Error $"{former}: the chain length must be >= 2 (got {x}) -- element 0 is the initial state"
    | _ -> Error $"{former}: the chain length must be a compile-time integer (a literal, `let static`, or static-function call) -- shapes are static everywhere in Blade; {runtimeOnes} may be runtime values"

/// mh(logpost, x0, n, scale, key) -- see the section comment for the emitted
/// shape. Returns the decls and the static chain length for the registry.
let private elabMh (ctx: Ctx) (span: Span) (chainName: string) (binding: Binding)
    (funcs: Map<string, FunctionDecl>) (args: Expr list)
    : Result<Located<Decl> list * int, string> =
    match args with
    | [lpE; x0E; nE; scaleE; keyE] ->
        samplerLogpost "mh" "mh(logpost, 0.0, 4096, 1.0, 7)" funcs lpE
        |> Result.bind (fun f ->
            samplerChainLen ctx "mh" "x0, scale, and the key" nE
            |> Result.map (fun n ->
                let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
                let nm s = $"__ppl_mh_{chainName}_{s}"
                let x0N, scN, keyN = nm "x0", nm "scale", nm "key"
                let epsN, uN, stepN, recN = nm "eps", nm "u", nm "step", nm "c"
                let prevN, tN, propN, dN = "__prev", "__t", "__prop", "__lpd"
                let stepBody =
                    syn (ExprBlock (
                            [ sLet propN (addE (v prevN) (mulE (v scN) (appE (v epsN) [v tN])))
                              sLet dN (subE (appE (v f) [v propN]) (appE (v f) [v prevN])) ],
                            Some (ifE (ltE (logE (appE (v uN) [v tN])) (v dN)) (v propN) (v prevN))))
                let recBinding =
                    { Mutability = BindLet
                      Pattern = pvar recN
                      Type = Some (tyFloatArr n)
                      Value = syn (ExprRecArray {
                                    Name = recN
                                    SeedArm = Some ("__s", v x0N)
                                    PrefixVar = "__p"
                                    StepVar = tN
                                    SliceExpr = appE (v stepN) [appE (v "__p") [subE (v tN) (iLit 1)]; v tN]
                                    Guard = None }) }
                [ bind x0N x0E
                  bind scN scaleE
                  bind keyN keyE
                  bind epsN (appE (v "__rand_normal") [addE (v keyN) (iLit 1000003); iLit n])
                  bind uN (appE (v "__rand_uniform") [addE (v keyN) (iLit 2000003); iLit n])
                  mkFnDecl span stepN [mkParamD prevN tyFloat64; mkParamD tN tyIntP] tyFloat64 stepBody
                  { Value = DeclLet { binding with Value = syn (ExprBlock ([ StmtLet recBinding ], Some (v recN))) }
                    Span = span } ], n))
    | _ ->
        Error "mh expects mh(logpost, x0, n, scale, key): a top-level log-posterior function name, an initial state, a static chain length, the proposal standard deviation, and an Int64 stream key"

/// hmc(logpost, x0, n, eps, L, key) -- fixed-leapfrog HMC; see the section
/// comment for the emitted shape and the H = -logpost + p^2/2 energy
/// convention. `adAlias` is the module's `import ad` binding: the leapfrog
/// gradient is emitted as the `<alias>.grad(logpost)` surface form for the
/// Grad pass (which runs after ppl elaboration) to expand, so the import is
/// REQUIRED. Returns the decls and the static chain length for the registry.
let private elabHmc (ctx: Ctx) (span: Span) (chainName: string) (binding: Binding)
    (funcs: Map<string, FunctionDecl>) (adAlias: string option) (args: Expr list)
    : Result<Located<Decl> list * int, string> =
    match args with
    | [lpE; x0E; nE; epsE; lE; keyE] ->
        samplerLogpost "hmc" "hmc(logpost, 0.0, 2048, 0.1, 8, 7)" funcs lpE
        |> Result.bind (fun f ->
        samplerChainLen ctx "hmc" "x0, eps, and the key" nE
        |> Result.bind (fun n ->
        (match evalExpr ctx.Statics maxSteps lE with
         | Ok (SVInt x) when x >= 1L -> Ok (int x)
         | Ok (SVInt x) -> Error $"hmc: the leapfrog step count L must be >= 1 (got {x})"
         | _ -> Error "hmc: the leapfrog step count L must be a compile-time integer (a literal, `let static`, or static-function call) -- the sweep is a static loop; x0, eps, and the key may be runtime values")
        |> Result.bind (fun lSteps ->
        match adAlias with
        | None ->
            Error "hmc: the leapfrog gradient rides ad.grad, so the module needs `import ad as ad` -- add the import; the Grad pass expands the emitted ad.grad(logpost) sites after ppl elaboration"
        | Some ad ->
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let nm s = $"__ppl_hmc_{chainName}_{s}"
            let x0N, epsN, eps2N, keyN = nm "x0", nm "eps", nm "eps2", nm "key"
            let pFillN, uN, stepN, recN = nm "p", nm "u", nm "step", nm "c"
            let prevN, tN = "__prev", "__t"
            let p0N, qN, pmN, lN = "__p0", "__q", "__pm", "__l"
            let lpaN, gaN, lpbN, gbN, lpnN, gnN, dHN = "__lpa", "__ga", "__lpb", "__gb", "__lpn", "__gn", "__dH"
            // (primal, gradient) at x: the ad.grad surface form, expanded by
            // the Grad pass into a `logpost__grad(x)` call.
            let gradCall (x: Expr) = appE (appE (syn (ExprField (v ad, "grad"))) [v f]) [x]
            let gradLet (pPat: Pattern) (gName: string) (x: Expr) =
                StmtLet { Pattern = synPat (PatTuple [pPat; pvar gName]); Type = None; Value = gradCall x; Mutability = BindLet }
            let stepBody =
                syn (ExprBlock (
                        [ sLet p0N (appE (v pFillN) [v tN])
                          sMutStmt qN (v prevN)
                          // half step in: p <- p0 + (eps/2) grad(q0); __lpa
                          // is logpost(q0), free from the same call.
                          gradLet (pvar lpaN) gaN (v qN)
                          sMutStmt pmN (addE (v p0N) (mulE (v eps2N) (v gaN)))
                          // L-1 interleaved full steps (loop is empty at
                          // L = 1). Only the gradient is wanted here, so the
                          // interior primal is discarded with a wildcard.
                          forIn lN (iLit 1) (iLit lSteps)
                              [ assignStmt (v qN) (addE (v qN) (mulE (v epsN) (v pmN)))
                                gradLet (synPat PatWildcard) gbN (v qN)
                                assignStmt (v pmN) (addE (v pmN) (mulE (v epsN) (v gbN))) ]
                          // final position update + half step out; __lpn is
                          // logpost(q_L), again free from the gradient call.
                          assignStmt (v qN) (addE (v qN) (mulE (v epsN) (v pmN)))
                          gradLet (pvar lpnN) gnN (v qN)
                          assignStmt (v pmN) (addE (v pmN) (mulE (v eps2N) (v gnN)))
                          // dH = H(q0, p0) - H(qL, pL) under H = -logpost + p^2/2
                          sLet dHN (addE (subE (v lpnN) (v lpaN))
                                         (divE (subE (mulE (v p0N) (v p0N)) (mulE (v pmN) (v pmN))) (fLit 2.0))) ],
                        Some (ifE (ltE (logE (appE (v uN) [v tN])) (v dHN)) (v qN) (v prevN))))
            let recBinding =
                { Mutability = BindLet
                  Pattern = pvar recN
                  Type = Some (tyFloatArr n)
                  Value = syn (ExprRecArray {
                                Name = recN
                                SeedArm = Some ("__s", v x0N)
                                PrefixVar = "__p"
                                StepVar = tN
                                SliceExpr = appE (v stepN) [appE (v "__p") [subE (v tN) (iLit 1)]; v tN]
                                Guard = None }) }
            Ok ([ bind x0N x0E
                  bind epsN epsE
                  bind eps2N (divE (v epsN) (fLit 2.0))
                  bind keyN keyE
                  bind pFillN (appE (v "__rand_normal") [addE (v keyN) (iLit 3000017); iLit n])
                  bind uN (appE (v "__rand_uniform") [addE (v keyN) (iLit 4000037); iLit n])
                  mkFnDecl span stepN [mkParamD prevN tyFloat64; mkParamD tN tyIntP] tyFloat64 stepBody
                  { Value = DeclLet { binding with Value = syn (ExprBlock ([ StmtLet recBinding ], Some (v recN))) }
                    Span = span } ], n))))
    | _ ->
        Error "hmc expects hmc(logpost, x0, n, eps, L, key): a top-level log-posterior function name, an initial state, a static chain length, the leapfrog step size, a static leapfrog step count, and an Int64 stream key"

/// A chain argument: an mh/hmc chain elaborated earlier in this module (its
/// length is in the registry) or a module-level rank-1 array with a static
/// extent (declared annotation or computed method_for shape).
let private chainSource (ctx: Ctx) (chains: Map<string, int>) (former: string) (e: Expr)
    : Result<string * int, string> =
    match e.Kind with
    | ExprKind.ExprVar nmv ->
        (match Map.tryFind nmv chains with
         | Some n -> Ok (nmv, n)
         | None ->
             match Map.tryFind nmv ctx.Arrays with
             | Some (_, [ix]) ->
                 (match resolveExtent ctx.Aliases ctx.Statics ix with
                  | Some n -> Ok (nmv, n)
                  | None -> Error $"{former}: '{nmv}' must have a statically known extent (Idx<n> directly or through aliases)")
             | Some (_, idxs) ->
                 Error $"{former}: '{nmv}' carries {idxs.Length} declared axes -- chain diagnostics run on rank-1 chains"
             | None ->
                 Error $"{former}: '{nmv}' must be a ppl.mh or ppl.hmc chain declared earlier in this module, or a module-level rank-1 array with an Array<Float like Idx<n>> annotation")
    | _ -> Error $"{former}: the chain argument must be a named module-level binding"

let private staticNat (ctx: Ctx) (former: string) (what: string) (e: Expr) : Result<int, string> =
    match evalExpr ctx.Statics maxSteps e with
    | Ok (SVInt x) when x >= 0L -> Ok (int x)
    | Ok (SVInt x) -> Error $"{former}: {what} must be >= 0 (got {x})"
    | _ -> Error $"{former}: {what} must be a compile-time integer (a literal, `let static`, or static-function call)"

/// chain_mean(c, burn) / chain_var(c, burn): post-burn moments as the
/// elabLogLik module-block accumulation idiom, reads through a `__` alias.
let private elabChainMoment (isVar: bool) (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (chains: Map<string, int>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    let former = if isVar then "chain_var" else "chain_mean"
    match args with
    | [chainE; burnE] ->
        chainSource ctx chains former chainE |> Result.bind (fun (src, len) ->
        staticNat ctx former "the burn-in count" burnE |> Result.bind (fun burn ->
        if burn >= len then
            Error $"{former}: burn = {burn} discards the whole length-{len} chain -- burn must be < the chain length"
        else
            let m = float (len - burn)
            let bind nm vl = { Value = DeclLet { Pattern = pvar nm; Type = None; Value = vl; Mutability = BindLet }; Span = span }
            let nmOf s = $"""__ppl_{(if isVar then "cv" else "cm")}_{outName}_{s}"""
            let srcN, iN, aN, bN = nmOf "src", nmOf "i", nmOf "acc", nmOf "acc2"
            let read = appE (v srcN) [v iN]
            let value =
                if isVar then
                    // population variance of the suffix: E[x^2] - mean^2
                    syn (ExprBlock (
                            [ sMutStmt aN (fLit 0.0)
                              sMutStmt bN (fLit 0.0)
                              forIn iN (iLit burn) (iLit len)
                                  [ assignStmt (v aN) (addE (v aN) read)
                                    assignStmt (v bN) (addE (v bN) (mulE read read)) ] ],
                            Some (subE (divE (v bN) (fLit m))
                                       (mulE (divE (v aN) (fLit m)) (divE (v aN) (fLit m))))))
                else
                    syn (ExprBlock (
                            [ sMutStmt aN (fLit 0.0)
                              forIn iN (iLit burn) (iLit len) [ assignStmt (v aN) (addE (v aN) read) ] ],
                            Some (divE (v aN) (fLit m))))
            Ok [ bind srcN (v src)
                 { Value = DeclLet { binding with Value = value }; Span = span } ]))
    | _ ->
        Error $"{former} expects {former}(chain, burn): a chain binding and a static burn-in count"

/// Shared prologue statements: mean and centered-sum-of-squares of a length-n
/// source read through `srcE` -- `let mut mu; for; let m; let mut v; for`.
let private meanVarStmts (tok: string) (srcRead: Expr -> Expr) (n: int) : Stmt list * string * string =
    let muN, mN, vN, tN, dN = $"__mu_{tok}", $"__m_{tok}", $"__v_{tok}", $"__t_{tok}", $"__d_{tok}"
    let stmts =
        [ sMutStmt muN (fLit 0.0)
          forIn tN (iLit 0) (iLit n) [ assignStmt (v muN) (addE (v muN) (srcRead (v tN))) ]
          sLet mN (divE (v muN) (fLit (float n)))
          sMutStmt vN (fLit 0.0)
          forIn tN (iLit 0) (iLit n)
              [ sLet dN (subE (srcRead (v tN)) (v mN))
                assignStmt (v vN) (addE (v vN) (mulE (v dN) (v dN))) ] ]
    (stmts, mN, vN)

/// autocorr(c, maxlag): a synthesized function (mut array + element writes +
/// nested for loops, the grad-emission shape) applied to the chain.
let private elabAutocorr (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (chains: Map<string, int>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [chainE; maxlagE] ->
        chainSource ctx chains "autocorr" chainE |> Result.bind (fun (src, len) ->
        staticNat ctx "autocorr" "maxlag" maxlagE |> Result.bind (fun maxlag ->
        if maxlag < 1 || maxlag >= len then
            Error $"autocorr: maxlag must be in 1..{len - 1} for a length-{len} chain (got {maxlag})"
        else
            let fnN = $"__ppl_ac_{outName}_fn"
            let srcP = "__src"
            let read (i: Expr) = appE (v srcP) [i]
            let (pre, mN, vN) = meanVarStmts "0" read len
            let rN, kN, aN, tN = "__r", "__k", "__a", "__t2"
            let body =
                syn (ExprBlock (
                        pre
                        @ [ sMutStmt rN (computeE (syn (ExprReplicate (iLit (maxlag + 1), syn (ExprPure (fLit 0.0))))))
                            assignStmt (appE (v rN) [iLit 0]) (fLit 1.0)
                            forIn kN (iLit 1) (iLit (maxlag + 1))
                                [ sMutStmt aN (fLit 0.0)
                                  forIn tN (iLit 0) (subE (iLit len) (v kN))
                                      [ assignStmt (v aN)
                                            (addE (v aN)
                                                  (mulE (subE (read (v tN)) (v mN))
                                                        (subE (read (addE (v tN) (v kN))) (v mN)))) ]
                                  assignStmt (appE (v rN) [v kN]) (divE (v aN) (v vN)) ] ],
                        Some (v rN)))
            Ok [ mkFnDecl span fnN [mkParamD srcP (tyFloatArr len)] (tyFloatArr (maxlag + 1)) body
                 { Value = DeclLet { binding with Value = appE (v fnN) [v src] }; Span = span } ]))
    | _ ->
        Error "autocorr expects autocorr(chain, maxlag): a chain binding and a static maximum lag"

/// ess(c): Geyer initial-positive-sequence truncation -- see the section
/// comment for the exact rule.
let private elabEss (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (chains: Map<string, int>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [chainE] ->
        chainSource ctx chains "ess" chainE |> Result.bind (fun (src, len) ->
        if len < 4 then Error $"ess: a length-{len} chain is too short -- ess needs at least 4 elements"
        else
            let pairs = min ((len - 2) / 2) 128
            let fnN = $"__ppl_ess_{outName}_fn"
            let srcP = "__src"
            let read (i: Expr) = appE (v srcP) [i]
            let (pre, mN, vN) = meanVarStmts "0" read len
            let tauN, stopN, kN, aN, bN, tN, gN = "__tau", "__stop", "__k", "__a", "__b", "__t2", "__g"
            let lagSum (accN: string) (lag: Expr) =
                [ sMutStmt accN (fLit 0.0)
                  forIn tN (iLit 0) (subE (iLit len) lag)
                      [ assignStmt (v accN)
                            (addE (v accN)
                                  (mulE (subE (read (v tN)) (v mN))
                                        (subE (read (addE (v tN) lag)) (v mN)))) ] ]
            let evenLag = mulE (iLit 2) (v kN)
            let oddLag = addE (mulE (iLit 2) (v kN)) (iLit 1)
            let body =
                syn (ExprBlock (
                        pre
                        @ [ sMutStmt tauN (fLit -1.0)
                            sMutStmt stopN (fLit 0.0)
                            forIn kN (iLit 0) (iLit pairs)
                                (lagSum aN evenLag
                                 @ lagSum bN oddLag
                                 @ [ sLet gN (divE (addE (v aN) (v bN)) (v vN))
                                     assignStmt (v stopN) (ifE (leE (v gN) (fLit 0.0)) (fLit 1.0) (v stopN))
                                     assignStmt (v tauN) (addE (v tauN) (ifE (gtE (v stopN) (fLit 0.5)) (fLit 0.0) (mulE (fLit 2.0) (v gN)))) ]) ],
                        Some (divE (fLit (float len)) (v tauN))))
            Ok [ mkFnDecl span fnN [mkParamD srcP (tyFloatArr len)] tyFloat64 body
                 { Value = DeclLet { binding with Value = appE (v fnN) [v src] }; Span = span } ])
    | _ ->
        Error "ess expects ess(chain): one chain binding"

/// rhat(c1, c2): split-Rhat over two equal-length chains -- see the section
/// comment for the exact formula.
let private elabRhat (ctx: Ctx) (span: Span) (outName: string) (binding: Binding)
    (chains: Map<string, int>) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [c1E; c2E] ->
        chainSource ctx chains "rhat" c1E |> Result.bind (fun (s1, n1) ->
        chainSource ctx chains "rhat" c2E |> Result.bind (fun (s2, n2) ->
        if n1 <> n2 then Error $"rhat: the two chains must have the same static length (got {n1} and {n2})"
        elif n1 < 4 || n1 % 2 <> 0 then Error $"rhat: the chain length must be even and >= 4 (got {n1}) -- each chain splits into two halves"
        else
            let len = n1
            let m = len / 2
            let mF = float m
            let fnN = $"__ppl_rhat_{outName}_fn"
            let aP, bP = "__ca", "__cb"
            // segment j: (source param, start offset)
            let segs = [ (aP, 0); (aP, m); (bP, 0); (bP, m) ]
            let muN j = $"__mu{j}"
            let mN j = $"__m{j}"
            let vvN j = $"__vv{j}"
            let sN j = $"__s{j}"
            let tN, dN = "__t", "__d"
            let segStmts =
                segs |> List.mapi (fun j (srcP, lo) ->
                    let read = appE (v srcP) [v tN]
                    [ sMutStmt (muN j) (fLit 0.0)
                      forIn tN (iLit lo) (iLit (lo + m)) [ assignStmt (v (muN j)) (addE (v (muN j)) read) ]
                      sLet (mN j) (divE (v (muN j)) (fLit mF))
                      sMutStmt (vvN j) (fLit 0.0)
                      forIn tN (iLit lo) (iLit (lo + m))
                          [ sLet dN (subE read (v (mN j)))
                            assignStmt (v (vvN j)) (addE (v (vvN j)) (mulE (v dN) (v dN))) ]
                      sLet (sN j) (divE (v (vvN j)) (fLit (mF - 1.0))) ])
                |> List.concat
            let wN, mbN, bigBN, vpN = "__W", "__mb", "__B", "__vp"
            let sumOver f = [0 .. 3] |> List.map f |> List.reduce addE
            let finish =
                [ sLet wN (divE (sumOver (fun j -> v (sN j))) (fLit 4.0))
                  sLet mbN (divE (sumOver (fun j -> v (mN j))) (fLit 4.0))
                  sLet bigBN (divE (mulE (fLit mF)
                                         (sumOver (fun j ->
                                             mulE (subE (v (mN j)) (v mbN)) (subE (v (mN j)) (v mbN)))))
                                   (fLit 3.0))
                  sLet vpN (divE (addE (mulE (fLit (mF - 1.0)) (v wN)) (v bigBN)) (fLit mF)) ]
            let body = syn (ExprBlock (segStmts @ finish, Some (sqrtE (divE (v vpN) (v wN)))))
            Ok [ mkFnDecl span fnN [mkParamD aP (tyFloatArr len); mkParamD bP (tyFloatArr len)] tyFloat64 body
                 { Value = DeclLet { binding with Value = appE (v fnN) [v s1; v s2] }; Span = span } ]))
    | _ ->
        Error "rhat expects rhat(c1, c2): two chain bindings of the same static length"

// dist_map: the symbolic front-end over dist_jet -- differentiate a lambda at elaboration time, evaluate the derivatives at
// the runtime mean, and delegate to the jet pushforward. A polynomial's derivative chain terminates in structural zeros
// (finite jet = exact pushforward); any other map needs an explicit truncation degree the program must own.

/// Structural constant folding -- enough for polynomial derivative chains to terminate in literal zeros (0*e, e*0, 0+-e drop; literals fold).
let rec private simplifyExpr (e: Expr) : Expr =
    match e.Kind with
    | ExprKind.ExprBinOp (m, op, a0, b0) ->
        let a = simplifyExpr a0
        let b = simplifyExpr b0
        (match op, a.Kind, b.Kind with
         | OpAdd, ExprKind.ExprLit (LitFloat 0.0), _ -> b
         | OpAdd, _, ExprKind.ExprLit (LitFloat 0.0) -> a
         | OpSub, _, ExprKind.ExprLit (LitFloat 0.0) -> a
         | OpMul, ExprKind.ExprLit (LitFloat 0.0), _ -> fLit 0.0
         | OpMul, _, ExprKind.ExprLit (LitFloat 0.0) -> fLit 0.0
         | OpMul, ExprKind.ExprLit (LitFloat 1.0), _ -> b
         | OpMul, _, ExprKind.ExprLit (LitFloat 1.0) -> a
         | OpDiv, ExprKind.ExprLit (LitFloat 0.0), _ -> fLit 0.0
         | OpDiv, _, ExprKind.ExprLit (LitFloat 1.0) -> a
         | _, ExprKind.ExprLit (LitFloat x), ExprKind.ExprLit (LitFloat y) ->
             (match op with
              | OpAdd -> fLit (x + y)
              | OpSub -> fLit (x - y)
              | OpMul -> fLit (x * y)
              | OpDiv when y <> 0.0 -> fLit (x / y)
              | _ -> inheritSpan e (ExprBinOp (m, op, a, b)))
         | _ -> inheritSpan e (ExprBinOp (m, op, a, b)))
    | ExprKind.ExprApp (f, args) -> inheritSpan e (ExprApp (f, args |> List.map simplifyExpr))
    | _ -> e

let private isZeroE (e: Expr) = match e.Kind with ExprKind.ExprLit (LitFloat 0.0) -> true | _ -> false

let rec private containsVar (n: string) (e: Expr) : bool =
    match e.Kind with
    | ExprKind.ExprVar m -> m = n
    | ExprKind.ExprBinOp (_, _, a, b) -> containsVar n a || containsVar n b
    | ExprKind.ExprApp (f, args) -> containsVar n f || args |> List.exists (containsVar n)
    | _ -> false

/// de/dparam over the supported grammar: arithmetic and exp/log/sqrt/sin/cos of the coordinates. Any subtree not mentioning the
/// parameter is a constant (array reads and other opaque calls included).
let rec private diffExpr (param: string) (e: Expr) : Result<Expr, string> =
    if not (containsVar param e) then Ok (fLit 0.0)
    else
        match e.Kind with
        | ExprKind.ExprVar _ -> Ok (fLit 1.0)   // containsVar => it IS the param
        | ExprKind.ExprBinOp (_, OpAdd, a, b) ->
            diffExpr param a |> Result.bind (fun da ->
            diffExpr param b |> Result.map (fun db -> addE da db))
        | ExprKind.ExprBinOp (_, OpSub, a, b) ->
            diffExpr param a |> Result.bind (fun da ->
            diffExpr param b |> Result.map (fun db -> subE da db))
        | ExprKind.ExprBinOp (_, OpMul, a, b) ->
            diffExpr param a |> Result.bind (fun da ->
            diffExpr param b |> Result.map (fun db -> addE (mulE da b) (mulE a db)))
        | ExprKind.ExprBinOp (_, OpDiv, a, b) ->
            diffExpr param a |> Result.bind (fun da ->
            diffExpr param b |> Result.map (fun db ->
                divE (subE (mulE da b) (mulE a db)) (mulE b b)))
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "exp" }, [a]) ->
            diffExpr param a |> Result.map (fun da -> mulE da (appE (v "exp") [a]))
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "log" }, [a]) ->
            diffExpr param a |> Result.map (fun da -> divE da a)
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "sqrt" }, [a]) ->
            diffExpr param a |> Result.map (fun da -> divE da (mulE (fLit 2.0) (appE (v "sqrt") [a])))
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "sin" }, [a]) ->
            diffExpr param a |> Result.map (fun da -> mulE da (appE (v "cos") [a]))
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "cos" }, [a]) ->
            diffExpr param a |> Result.map (fun da -> subE (fLit 0.0) (mulE da (appE (v "sin") [a])))
        | ExprKind.ExprBinOp (_, OpCaret, b, e) ->
            // d(b^e) = e*b^(e-1)*b' + b^e*log(b)*e'. The base term keeps the power form (not e*b^e/b) so b=0 stays finite; the
            // log term is pruned by simplifyExpr whenever e is constant (e'=0), keeping a negative base differentiable.
            diffExpr param b |> Result.bind (fun db ->
            diffExpr param e |> Result.map (fun de ->
                let baseTerm = mulE (mulE e (powE b (subE e (fLit 1.0)))) db
                let expTerm  = mulE (mulE (powE b e) (appE (v "log") [b])) de
                addE baseTerm expTerm))
        | _ ->
            Error "dist_map: cannot differentiate the map -- supported: +, -, *, /, ^ and exp/log/sqrt/sin/cos of the coordinates (opaque subterms are fine when they don't mention a coordinate)"

let rec private substVars (map: Map<string, Expr>) (e: Expr) : Expr =
    match e.Kind with
    | ExprKind.ExprVar n -> (match Map.tryFind n map with Some r -> r | None -> e)
    | ExprKind.ExprBinOp (m, op, a, b) -> inheritSpan e (ExprBinOp (m, op, substVars map a, substVars map b))
    | ExprKind.ExprApp (f, args) -> inheritSpan e (ExprApp (substVars map f, args |> List.map (substVars map)))
    | _ -> e

/// dist_map body normalization, applied before differentiation: unwrap trivial `{ expr }` function bodies and inline
/// full-arity calls to same-module top-level functions -- transitively, so helper reuse presents diffExpr with one closed
/// expression in the coordinates. The budget counts function expansions and breaks recursive helper cycles; helper bodies are
/// expected closed over their own parameters (a free variable colliding with a map coordinate would be captured).
let private normalizeMapBody (former: string) (funcs: Map<string, FunctionDecl>) (body: Expr) : Result<Expr, string> =
    let rec unwrap (e: Expr) : Expr =
        match e.Kind with
        | ExprKind.ExprBlock ([], Some r) -> unwrap r
        | _ -> e
    let rec goList (n: int) (acc: Expr list) (es: Expr list) : Result<int * Expr list, string> =
        match es with
        | [] -> Ok (n, List.rev acc)
        | x :: rest -> go n x |> Result.bind (fun (n2, x') -> goList n2 (x' :: acc) rest)
    and go (n: int) (e: Expr) : Result<int * Expr, string> =
        let e = unwrap e
        match e.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, args) when Map.containsKey f funcs ->
            let fd = funcs.[f]
            if args.Length <> fd.Params.Length then
                Error $"{former}: helper '{f}' is called with {args.Length} argument(s) but takes {fd.Params.Length}"
            elif n <= 0 then
                Error $"{former}: the map's helper-call chain is too deep -- is a helper function recursive?"
            else
                goList (n - 1) [] args |> Result.bind (fun (n2, args') ->
                    let bindMap = List.zip (fd.Params |> List.map _.Name) args' |> Map.ofList
                    go n2 (substVars bindMap (unwrap fd.Body)))
        | ExprKind.ExprBinOp (m, op, a, b) ->
            go n a |> Result.bind (fun (n2, a') ->
                go n2 b |> Result.map (fun (n3, b') -> (n3, inheritSpan e (ExprBinOp (m, op, a', b')))))
        | ExprKind.ExprApp (f, args) ->
            goList n [] args |> Result.map (fun (n2, args') -> (n2, inheritSpan e (ExprApp (f, args'))))
        | ExprKind.ExprTuple es ->
            goList n [] es |> Result.map (fun (n2, es') -> (n2, inheritSpan e (ExprTuple es')))
        | _ -> Ok (n, e)
    go 256 body |> Result.map snd

/// dist_map(d, q, lambda(x...) -> e) / dist_map(d, q, s, lambda(x...) -> e): derive the jet symbolically -- the lambda takes
/// one coordinate per dist dimension; derivative tensors come from repeated symbolic differentiation, evaluated at the
/// runtime mean (kappa_1, bound once); pushforward is dist_jet's. Without s the tower must terminate (polynomial, exact).
let private elabDistMap (closed: bool) (ctx: Ctx) (span: Span) (dName: string)
    (funcs: Map<string, FunctionDecl>) (dists: Map<string, DistInfo>) (args: Expr list)
    : Result<Located<Decl> list * DistInfo, string> =
    let former = if closed then "dist_map_closed" else "dist_map"
    // The map slot: an inline lambda, a same-module top-level function name, or a prefix partial application of one.
    // Named/partial forms inline the function body here at elaboration time (before typechecking); residual params become
    // the jet coordinates, and bound prefix args are constants w.r.t. diffExpr since differentiation is by name only.
    let asCoordLambda (e: Expr) : Result<(LambdaParam list * Expr) option, string> =
        match e.Kind with
        | ExprKind.ExprLambda (ps, None, body) -> Ok (Some (ps, body))
        | ExprKind.ExprVar f ->
            match Map.tryFind f funcs with
            | Some fd ->
                let ps = fd.Params |> List.map (fun p -> { Name = p.Name; Type = None; Default = None; NameSpan = noSpan } : LambdaParam)
                Ok (Some (ps, fd.Body))
            | None -> Ok None
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, prefixArgs) when Map.containsKey f funcs ->
            let fd = funcs.[f]
            if prefixArgs.Length >= fd.Params.Length then
                Error ($"{former}: partial application of '{f}' supplies {prefixArgs.Length} of its {fd.Params.Length} parameter(s) -- leave at least one free to map over")
            else
                // Capture avoidance in two steps: rename residual params to reserved coordinate names FIRST, so no variable inside
                // the bound-arg expressions can collide with a coordinate after.
                let k = prefixArgs.Length
                let bound = fd.Params |> List.truncate k
                let residual = fd.Params |> List.skip k
                let freshNames = residual |> List.mapi (fun i _ -> $"__ppl_pa_{dName}_{i}")
                let renameMap =
                    List.zip (residual |> List.map _.Name) (freshNames |> List.map v)
                    |> Map.ofList
                let bindMap =
                    List.zip (bound |> List.map _.Name) prefixArgs
                    |> Map.ofList
                let body = fd.Body |> substVars renameMap |> substVars bindMap
                let ps = freshNames |> List.map (fun n -> { Name = n; Type = None; Default = None; NameSpan = noSpan } : LambdaParam)
                Ok (Some (ps, body))
        | _ -> Ok None
    let parseErr =
        $"{former} expects {former}(d, q, f) or {former}(d, q, s, f) where f is lambda(x...) -> expr, a same-module top-level function name, or a prefix partial application of one"
    let parsed =
        match args with
        | [{ Kind = ExprKind.ExprVar dn }; qExpr; fArg] ->
            asCoordLambda fArg |> Result.bind (function
                | Some (ps, body) ->
                    normalizeMapBody former funcs body
                    |> Result.map (fun body' -> (dn, qExpr, None, ps, body'))
                | None -> Error parseErr)
        | [{ Kind = ExprKind.ExprVar dn }; qExpr; sExpr; fArg] ->
            asCoordLambda fArg |> Result.bind (function
                | Some (ps, body) ->
                    normalizeMapBody former funcs body
                    |> Result.map (fun body' -> (dn, qExpr, Some sExpr, ps, body'))
                | None -> Error parseErr)
        | _ -> Error parseErr
    parsed |> Result.bind (fun (dn, qExpr, sOpt, ps, body) ->
    match Map.tryFind dn dists with
    | None -> Error $"{former}: '{dn}' must be a previously declared dist binding"
    | Some info ->
        distDim ctx info |> Result.bind (fun dim ->
        if ps.Length <> dim then
            Error $"{former}: the lambda takes {ps.Length} coordinate(s) but '{dn}' is {dim}-dimensional"
        else
        let sRes =
            match sOpt with
            | None -> Ok None
            | Some e ->
                match evalExpr ctx.Statics maxSteps e with
                | Ok (SVInt x) when x >= 1L && x <= 8L -> Ok (Some (int x))
                | _ -> Error $"{former}: the truncation degree s must be a compile-time integer in 1..8"
        sRes |> Result.bind (fun sOpt ->
        let paramNames = ps |> List.map (fun (p: LambdaParam) -> p.Name)
        // Vector mode: a tuple-valued body lambda(x, y) -> (e1, ..., em)
        // (m >= 2) means g : R^dim -> R^m, one symbolic jet per output
        // component, delegated to the vector dist_jet. A single-expression
        // body keeps the scalar path.
        let comps =
            match body.Kind with
            | ExprKind.ExprTuple es when es.Length >= 2 -> es
            | _ -> [body]
        // Level k for one component: canonical tuple (i1 <= ... <= ik) ->
        // d^k f, symbolic in the coordinates; each cell differentiates the
        // (k-1)-level cell of its tail by x_{i1} (Schwarz symmetry makes
        // canonical tuples enough).
        let levelFrom (fbody: Expr) (prev: Map<int list, Expr>) (k: int) : Result<Map<int list, Expr>, string> =
            canonicalTuples dim k
            |> List.fold (fun acc t ->
                acc |> Result.bind (fun m ->
                    let parent = if k = 1 then fbody else prev.[List.tail t]
                    diffExpr paramNames.[List.head t] parent
                    |> Result.map (fun de -> Map.add t (simplifyExpr de) m)))
                (Ok Map.empty)
        let allZero (m: Map<int list, Expr>) = m |> Map.forall (fun _ e -> isZeroE e)
        let rec grow (fbody: Expr) (acc: Map<int list, Expr> list) (k: int) : Result<Map<int list, Expr> list, string> =
            let prev = match acc with [] -> Map.empty | h :: _ -> h
            levelFrom fbody prev k |> Result.bind (fun lv ->
                match sOpt with
                | Some s -> if k >= s then Ok (List.rev (lv :: acc)) else grow fbody (lv :: acc) (k + 1)
                | None ->
                    if allZero lv then Ok (List.rev acc)   // the polynomial terminated at degree k-1
                    elif k >= 8 then Error $"{former}: the map is not polynomial (its derivatives never vanish) -- own the truncation with an explicit degree: {former}(d, q, s, lambda(x...) -> expr)"
                    else grow fbody (lv :: acc) (k + 1))
        let towersRes =
            comps |> List.fold (fun acc cb ->
                acc |> Result.bind (fun ts -> grow cb [] 1 |> Result.map (fun lv -> ts @ [lv]))) (Ok [])
        towersRes |> Result.bind (fun towers ->
        let sPer = towers |> List.map List.length
        let s = List.max sPer
        if s = 0 then
            Error $"{former}: the map is constant in the coordinates -- there is no jet to push"
        else
        // the mean components, bound once; coordinates substitute to them
        let muName i = $"__ppl_jetmu_{dName}_{i}"
        let muDecls =
            [ for i in 0 .. dim - 1 ->
                { Value = DeclLet { Pattern = pvar (muName i); Type = None
                                    Value = appE (v info.Components.[0]) [iLit i]
                                    Mutability = BindLet }
                  Span = span } ]
        let subst = Map.ofList [ for i in 0 .. dim - 1 -> (paramNames.[i], v (muName i)) ]
        let atMean e = simplifyExpr (substVars subst e)
        let g0, dArgs =
            match comps with
            | [single] ->
                let levels = towers.Head
                let dArgs =
                    [ for k in 1 .. s ->
                        let lv = levels.[k - 1]
                        if dim = 1 then atMean lv.[List.replicate k 0]
                        else arrLitE [ for t in canonicalTuples dim k -> atMean lv.[t] ] ]
                (atMean single, dArgs)
            | _ ->
                // vector: g0 = the m atMean cells; D_k = coordinate-major flat cells, zero-filled where a shorter component
                // tower has no level k (its polynomial terminated earlier)
                let g0 = arrLitE (comps |> List.map atMean)
                let dArgs =
                    [ for k in 1 .. s ->
                        arrLitE
                            [ for (a, tower) in List.indexed towers do
                                for t in canonicalTuples dim k do
                                    yield (if k <= sPer.[a] then atMean tower.[k - 1].[t] else fLit 0.0) ] ]
                (g0, dArgs)
        elabDistJet closed former ctx span dName dists (v dn :: qExpr :: g0 :: dArgs)
        |> Result.map (fun (nds, outInfo) -> (muDecls @ nds, outInfo))))))

// Free cumulants: the same moment<->cumulant machinery summed over the non-crossing partition lattice (Catalan combinatorics)
// instead of all set partitions -- the transform underlying free probability. Triangular recursion
// fk_p = mu_p - Sum over non-crossing pi <> full-block of Prod fk_|B|, reading raw-moment tensors and lower-rank fk tensors
// (flat, emitted earlier). fk_1..fk_3 coincide with classical cumulants; rank 4 is where the lattices first diverge.

let private isNonCrossing (partition: int list list) : bool =
    let blockOf = partition |> List.mapi (fun i blk -> blk |> List.map (fun x -> (x, i))) |> List.concat |> Map.ofList
    let n = partition |> List.sumBy List.length
    let crossing =
        Seq.exists (fun (a, b, c, d) ->
            blockOf.[a] = blockOf.[c] && blockOf.[b] = blockOf.[d] && blockOf.[a] <> blockOf.[b])
            (seq { for a in 0 .. n - 4 do
                     for b in a + 1 .. n - 3 do
                       for c in b + 1 .. n - 2 do
                         for d in c + 1 .. n - 1 -> (a, b, c, d) })
    not crossing

let private elabFreeCumulants (ctx: Ctx) (span: Span) (binding: Binding) (args: Expr list)
    : Result<Located<Decl> list, string> =
    match args with
    | [{ Kind = ExprKind.ExprVar aName }; rExpr] ->
        let r =
            match evalExpr ctx.Statics maxSteps rExpr with
            | Ok (SVInt x) when x >= 1L && x <= 6L -> Ok (int x)
            | _ -> Error "free_cumulants: the order must be a compile-time integer in 1..6"
        r |> Result.bind (fun r ->
        arrayShape ctx "free_cumulants" aName |> Result.bind (fun (elem, leading, fiber, n) ->
            match leading with
            | [ix] ->
                match resolveExtent ctx.Aliases ctx.Statics ix with
                | None -> Error "free_cumulants: the variable-axis extent must be statically known"
                | Some d ->
                    let compNames =
                        match binding.Pattern.Kind with
                        | PatternKind.PatTuple pats when pats.Length = r ->
                            let names = pats |> List.map (fun p -> match p.Kind with PatternKind.PatVar nm -> Some nm | _ -> None)
                            if names |> List.forall Option.isSome then Ok (names |> List.map Option.get)
                            else Error "free_cumulants: destructure into plain names"
                        | _ -> Error $"free_cumulants: destructure the result -- `let (f1, ..., f{r}) = free_cumulants({aName}, {r})`"
                    compNames |> Result.map (fun fkNames ->
                        // Raw moments mu_S = P_S / N from the shared pool sweep.
                        let (pd, pool) = acquirePool ctx span aName d (float n) r
                        let muDecls = pd
                        let muRead (labels: int list) = poolMoment pool labels
                        // fk tensors ascending; fk_1 = mu_1; flat lex reads on earlier fk outputs.
                        let fkRead (kIdx: int) (labels: int list) =
                            if labels.Length = 1 then appE (v fkNames.[0]) (labels |> List.map iLit)
                            else appE (v fkNames.[labels.Length - 1]) [iLit (lexOffsetOf d labels.Length labels)]
                        let fkDecl p nm =
                            if p = 1 then
                                { Value = DeclLet { Pattern = pvar nm; Type = None
                                                    Value = arrLitE [ for i in 0 .. d - 1 -> poolMoment pool [i] ]
                                                    Mutability = BindLet }; Span = span }
                            else
                                let cells =
                                    [ for labels in canonicalTuples d p ->
                                        let labArr = List.toArray labels
                                        let ncParts =
                                            setPartitions p
                                            |> List.filter (fun pt -> pt.Length > 1 && isNonCrossing pt)
                                        let subtracted =
                                            [ for pt in ncParts ->
                                                pt |> List.fold (fun acc blk ->
                                                    mulE acc (fkRead p (blk |> List.map (fun pos -> labArr.[pos]) |> List.sort))) (fLit 1.0) ]
                                        match subtracted with
                                        | [] -> muRead labels
                                        | _ -> subE (muRead labels) (subtracted |> List.reduce addE) ]
                                { Value = DeclLet { Pattern = pvar nm; Type = None; Value = arrLitE cells; Mutability = BindLet }; Span = span }
                        muDecls @ (fkNames |> List.mapi (fun i nm -> fkDecl (i + 1) nm)))
            | _ -> Error "free_cumulants: one variable axis per array so far"))
    | _ ->
        Error "free_cumulants expects free_cumulants(A, r)"

// Independence: per-compilation state + the `indep` constraint handler.

/// PPL-owned independence state, consumed by the checker's Dist machinery: the declared relation (loose
/// `let _ = independent(X, Y)` + struct `where indep` licenses), the license stack (pairs opened around function-body
/// checking by the registered `indep` where-clause handler), and module-dist source sets (dist binding name -> underlying
/// arrays). All state is AsyncLocal -- the test suite checks programs in parallel, each compilation in one async context.
module Independence =
    open System.Threading

    let private declaredStore = new AsyncLocal<Set<string * string>>()
    let private licenseStore = new AsyncLocal<(string * string) list>()
    let private sourcesStore = new AsyncLocal<Map<string, Set<string>>>()

    let private declared () = match box declaredStore.Value with null -> Set.empty | _ -> declaredStore.Value
    let private licenses () = match box licenseStore.Value with null -> [] | _ -> licenseStore.Value
    let private sources () = match box sourcesStore.Value with null -> Map.empty | _ -> sourcesStore.Value

    let key (a: string) (b: string) = if a <= b then (a, b) else (b, a)

    /// Fresh compilation: expand() calls this once per program.
    let reset () =
        declaredStore.Value <- Set.empty
        licenseStore.Value <- []
        sourcesStore.Value <- Map.empty

    let addDeclared (pairs: Set<string * string>) =
        declaredStore.Value <- Set.union (declared ()) pairs

    let addSources (m: Map<string, Set<string>>) =
        sourcesStore.Value <- m |> Map.fold (fun acc k v -> Map.add k v acc) (sources ())

    /// Checker-facing: source arrays of a module-level dist binding.
    let distSources (name: string) : Set<string> option = Map.tryFind name (sources ())

    /// Checker-facing: is the pair independent under declared or licenses?
    let isRelated (a: string) (b: string) : bool =
        let k = key a b
        Set.contains k (declared ()) || List.contains k (licenses ())

    let pushLicense (a: string) (b: string) =
        licenseStore.Value <- key a b :: licenses ()

    let popLicense (a: string) (b: string) =
        let k = key a b
        let rec removeFirst = function
            | [] -> []
            | x :: rest -> if x = k then rest else x :: removeFirst rest
        licenseStore.Value <- removeFirst (licenses ())

    /// The `indep(a, b)` where-clause handler: declaring it on a function
    /// promotes it to a PPL function -- the body checks under the license
    /// (a, b treated as independent), and every call site must prove
    /// independence of the actual arguments' sources.
    let private indepHandler : Blade.Constraints.ConstraintHandler = {
        Describe = "indep(a, b) -- declares two Dist-valued parameters independent for the function body; call sites must prove independence of the actuals' sources"
        Validate = fun funcName paramNames args ->
            match args with
            | [a; b] when a = b ->
                Error $"function '{funcName}': indep({a}, {b}) -- a value is not independent of itself"
            | [a; b] ->
                let missing = [a; b] |> List.filter (fun n -> not (List.contains n paramNames))
                if missing.IsEmpty then Ok ()
                else Error ($"""function '{funcName}': where indep({a}, {b}) must name function parameters (unknown: {(String.concat ", " missing)})""")
            | _ ->
                Error $"function '{funcName}': indep expects exactly two parameter names -- indep(a, b)"
        EnterBody = fun funcName args ->
            match args with
            | [a; b] -> pushLicense (Blade.Constraints.paramProvenanceToken funcName a)
                                    (Blade.Constraints.paramProvenanceToken funcName b)
            | _ -> ()
        ExitBody = fun funcName args ->
            match args with
            | [a; b] -> popLicense (Blade.Constraints.paramProvenanceToken funcName a)
                                   (Blade.Constraints.paramProvenanceToken funcName b)
            | _ -> ()
        Discharge = fun funcName args provOf ->
            match args with
            | [a; b] ->
                let pa = provOf a
                let pb = provOf b
                if Set.isEmpty pa || Set.isEmpty pb then
                    Error $"call to '{funcName}': cannot establish provenance for the dist argument bound to '{if Set.isEmpty pa then a else b}' -- pass a dist binding (or an expression built from dists) so independence of its sources can be verified"
                else
                    let missing =
                        [ for s1 in pa do
                            for s2 in pb do
                              if not (isRelated s1 s2) then yield (s1, s2) ]
                    match missing with
                    | [] -> Ok ()
                    | (s1, s2) :: _ when s1 = s2 ->
                        Error $"call to '{funcName}' requires indep({a}, {b}): both arguments carry source '{s1}' -- a value is not independent of itself; pass dists built from disjoint sources"
                    | (s1, s2) :: _ ->
                        Error $"call to '{funcName}' requires indep({a}, {b}): sources '{s1}' and '{s2}' are not declared independent -- add `let _ = ppl.independent({s1}, {s2})` (or a struct/function `where ppl.indep(...)` license)"
            | _ -> Error "indep expects exactly two arguments"
    }

    // Registered under the internal (normalized) name: the surface spelling
    // `where <alias>.indep(...)` (with `import ppl`) is normalized to
    // "__ppl_indep" before checking; a bare `where indep(...)` no longer
    // resolves.
    let register () = Blade.Constraints.registerConstraint "__ppl_indep" indepHandler

/// IDE-facing dist registry: every dist binding this module elaborates,
/// mapped to its carried order and component bindings. The packed formers
/// leave a `__dist_pack` decl under the user's name, but the flat ones
/// (dist_jet/dist_map/dist_reweight/dist_mix/dist_atoms) are register-only
/// -- the name is erased from the program, so `ide check` sees no binding
/// for it. The components always survive, so recording (order, components)
/// is enough for the IDE to rebuild `Dist<order, elem like axes>` from
/// kappa_1's inferred type. Consumed by Ide.fs; AsyncLocal for the same
/// reason as Independence.
module IdeDists =
    open System.Threading

    let private store = new AsyncLocal<Map<string, int * string list>>()
    let private dists () = match box store.Value with null -> Map.empty | _ -> store.Value

    /// Fresh compilation: expand() calls this once per program.
    let reset () = store.Value <- Map.empty

    let add (m: Map<string, int * string list>) =
        store.Value <- m |> Map.fold (fun acc k v -> Map.add k v acc) (dists ())

    /// IDE-facing: carried order and component bindings of a dist name.
    let tryFind (name: string) : (int * string list) option = Map.tryFind name (dists ())

    /// IDE-facing: every registered dist, as (name, order, components).
    let entries () : (string * int * string list) list =
        dists () |> Map.toList |> List.map (fun (n, (o, cs)) -> (n, o, cs))

/// Checker-facing synthesis (typed-Dist operator dispatch): surface
/// expansions for TypeCheck's Dist operator dispatch (inferDistBinOp) --
/// the checker synthesizes these block expressions and re-infers them, so
/// dist operators work in any expression position, notably on Dist-typed
/// function parameters, which the elaboration-level registry rewrites above
/// can never see. The synthesized code calls `cumulant` and `__dist_pack`,
/// both checker intrinsics; a user shadowing `cumulant` disables dist
/// operators in checker positions (documented edge).
module DistSynth =
    /// c * d (either side): kappa_k(c*X) = c^k kappa_k(X) -- multilinearity,
    /// exact with no independence requirement, dispatchable anywhere.
    ///   { let __dsd = d
    ///     let __dsk<k> = cumulant(__dsd, k)          per order
    ///     let __dss<k> = map1(__dsk<k>, c^k * __u)   per order
    ///     __dist_pack(__dss1, ..., __dssr) }
    /// `uniq` disambiguates nested synthesized expansions. The scalar expr
    /// `c` is spliced into each kernel body verbatim (k copies), not bound
    /// to a synthesized block-local: inlined kernels render captured
    /// block-locals by name while block emission names them by id
    /// (`__v<id>`), so a `let __dsc = c` capture would dangle in the
    /// generated C++.
    let scaleExpr (uniq: int) (c: Expr) (d: Expr) (order: int) : Expr =
        let dN = $"__dsd_{uniq}"
        let kN k = $"__dsk_{uniq}_{k}"
        let sN k = $"__dss_{uniq}_{k}"
        let stmts =
            [ sLet dN d ]
            @ [ for k in 1 .. order ->
                  sLet (kN k) (appE (v "__ppl_cumulant") [v dN; iLit k]) ]
            @ [ for k in 1 .. order ->
                  let scaled = List.replicate k c |> List.fold mulE (v "__u")
                  sLet (sN k) (map1 (v (kN k)) scaled) ]
        syn (ExprBlock (stmts, Some (appE (v "__dist_pack") [ for k in 1 .. order -> v (sN k) ])))

    /// l +- r for independent dists: per-order c_k = a_k + weight(k)*b_k --
    /// addition is weight = 1; subtraction is weight k = (-1)^k (kappa_k(-Y)
    /// = (-1)^k kappa_k(Y), odd orders subtract, even orders add). The
    /// caller verifies independence before synthesizing; weights are
    /// literals, so the kernels capture nothing.
    ///   { let __dcl = l; let __dcr = r
    ///     let __dka<k> = cumulant(__dcl, k); __dkb<k> = cumulant(__dcr, k)
    ///     let __dks<k> = zipMap2(__dka<k>, __dkb<k>, __u + w_k*__w)
    ///     __dist_pack(__dks1, ..., __dksr) }
    let combineExpr (uniq: int) (weight: int -> float) (l: Expr) (r: Expr) (order: int) : Expr =
        let lN = $"__dcl_{uniq}"
        let rN = $"__dcr_{uniq}"
        let aN k = $"__dka_{uniq}_{k}"
        let bN k = $"__dkb_{uniq}_{k}"
        let sN k = $"__dks_{uniq}_{k}"
        let stmts =
            [ sLet lN l; sLet rN r ]
            @ [ for k in 1 .. order do
                  yield sLet (aN k) (appE (v "__ppl_cumulant") [v lN; iLit k])
                  yield sLet (bN k) (appE (v "__ppl_cumulant") [v rN; iLit k]) ]
            @ [ for k in 1 .. order ->
                  let contrib =
                      if weight k = 1.0 then v "__w"
                      else mulE (fLit (weight k)) (v "__w")
                  sLet (sN k) (zipMap2 (v (aN k)) (v (bN k)) (addE (v "__u") contrib)) ]
        syn (ExprBlock (stmts, Some (appE (v "__dist_pack") [ for k in 1 .. order -> v (sN k) ])))

// Module expansion

/// `import ppl [as _]` -- the module this layer owns.
let private isPplImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["ppl"], _) -> true
    | _ -> false

/// Aliases bound to `ppl` in this decl list. Errors on a selective `from ppl import ...`, which would reintroduce the global
/// names the module system is meant to remove.
let private pplAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["ppl"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "ppl") set)
            | DeclImport (["ppl"], ImportSelective _) ->
                Error "`ppl` supports only `import ppl [as <alias>]`; a selective `from ppl import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

/// Normalize the qualified ppl surface to the internal forms the passes below (and the checker) recognize: `alias.<former>(...)`
/// -> bare `<former>(...)`, and `alias.cumulant(...)` -> `__ppl_cumulant(...)`. A missed position leaves an `ExprField` that
/// fails to type-check, so this need not be exhaustive.
let rec private stripQualified (aliases: Set<string>) (e: Expr) : Expr =
    let r = stripQualified aliases
    let rStmt s =
        let rec go s =
            match s with
            | StmtLet b -> StmtLet { b with Value = r b.Value }
            | StmtAssign (l, op, rr) -> StmtAssign (r l, op, r rr)
            | StmtExpr e2 -> StmtExpr (r e2)
            | StmtForIn (var, range, body) -> StmtForIn (var, r range, List.map go body)
            | StmtSpanned (inner, sp) -> StmtSpanned (go inner, sp)
        go s
    match e.Kind with
    | ExprKind.ExprField ({ Kind = ExprKind.ExprVar a }, name)
        when Set.contains a aliases
             && (name = "cumulant" || name = "indep" || Set.contains name formerNames) ->
        match name with
        | "cumulant" -> inheritSpan e (ExprVar "__ppl_cumulant")
        // `indep` appears qualified in struct where-invariants (`where p.indep(X, Y)`); normalize to the registered internal
        // constraint name (splitInvariant and the checker match it).
        | "indep" -> inheritSpan e (ExprVar "__ppl_indep")
        | _ -> inheritSpan e (ExprVar name)
    | ExprKind.ExprApp (f, args) -> inheritSpan e (ExprApp (r f, List.map r args))
    | ExprKind.ExprBinOp (m, op, a, b) -> inheritSpan e (ExprBinOp (m, op, r a, r b))
    | ExprKind.ExprUnaryOp (op, a) -> inheritSpan e (ExprUnaryOp (op, r a))
    | ExprKind.ExprTyped (a, t) -> inheritSpan e (ExprTyped (r a, t))
    | ExprKind.ExprAssign (l, rr) -> inheritSpan e (ExprAssign (r l, r rr))
    | ExprKind.ExprTuple es -> inheritSpan e (ExprTuple (List.map r es))
    | ExprKind.ExprArrayLit es -> inheritSpan e (ExprArrayLit (List.map r es))
    | ExprKind.ExprDotDot (a, b) -> inheritSpan e (ExprDotDot (r a, r b))
    | ExprKind.ExprIf (c, t, f) -> inheritSpan e (ExprIf (r c, r t, r f))
    | ExprKind.ExprLet (b, body) -> inheritSpan e (ExprLet ({ b with Value = r b.Value }, r body))
    | ExprKind.ExprLambda (ps, w, body) -> inheritSpan e (ExprLambda (ps, w, r body))
    | ExprKind.ExprMatch (s, cases) ->
        inheritSpan e (ExprMatch (r s, cases |> List.map (fun c -> { c with Guard = Option.map r c.Guard; Body = r c.Body })))
    | ExprKind.ExprBlock (stmts, fin) -> inheritSpan e (ExprBlock (List.map rStmt stmts, Option.map r fin))
    // Recursive array (`let rec q: T = match q with ...`): seed and inductive slices are ordinary expressions and may carry qualified names.
    | ExprKind.ExprRecArray def ->
        inheritSpan e (ExprRecArray { def with
                                        SeedArm = def.SeedArm |> Option.map (fun (sv, se) -> (sv, r se))
                                        SliceExpr = r def.SliceExpr
                                        Guard = def.Guard |> Option.map r })
    // The rest of the expression algebra. The wildcard is deliberately gone so FS0025 flags AST growth here rather than leaving
    // a qualified name to fail downstream as an unbound variable.
    | ExprKind.ExprCompute a -> inheritSpan e (ExprCompute (r a))
    | ExprKind.ExprRead a -> inheritSpan e (ExprRead (r a))
    | ExprKind.ExprPure a -> inheritSpan e (ExprPure (r a))
    | ExprKind.ExprStatic a -> inheritSpan e (ExprStatic (r a))
    | ExprKind.ExprRank a -> inheritSpan e (ExprRank (r a))
    | ExprKind.ExprExtents a -> inheritSpan e (ExprExtents (r a))
    | ExprKind.ExprUnique a -> inheritSpan e (ExprUnique (r a))
    | ExprKind.ExprObjectFor k -> inheritSpan e (ExprObjectFor (r k))
    | ExprKind.ExprReynolds (k, anti) -> inheritSpan e (ExprReynolds (r k, anti))
    | ExprKind.ExprField (obj, fld) -> inheritSpan e (ExprField (r obj, fld))
    | ExprKind.ExprPartialApp (op, a, isLeft) -> inheritSpan e (ExprPartialApp (op, r a, isLeft))
    | ExprKind.ExprTranspose (a, d1, d2) -> inheritSpan e (ExprTranspose (r a, d1, d2))
    | ExprKind.ExprDecompact (a, d) -> inheritSpan e (ExprDecompact (r a, d))
    | ExprKind.ExprHalo (t, offs) -> inheritSpan e (ExprHalo (t, r offs))
    | ExprKind.ExprMethodFor es -> inheritSpan e (ExprMethodFor (List.map r es))
    | ExprKind.ExprZip es -> inheritSpan e (ExprZip (List.map r es))
    | ExprKind.ExprStack es -> inheritSpan e (ExprStack (List.map r es))
    | ExprKind.ExprSequence es -> inheritSpan e (ExprSequence (List.map r es))
    | ExprKind.ExprGroupKeys es -> inheritSpan e (ExprGroupKeys (List.map r es))
    | ExprKind.ExprGroupBucket g -> inheritSpan e (ExprGroupBucket (r g))
    | ExprKind.ExprAlign (es, spec) -> inheritSpan e (ExprAlign (List.map r es, spec))
    | ExprKind.ExprJoin (es, d) -> inheritSpan e (ExprJoin (List.map r es, d))
    | ExprKind.ExprTupleIndex (t, i) -> inheritSpan e (ExprTupleIndex (r t, r i))
    | ExprKind.ExprGuard (c, b) -> inheritSpan e (ExprGuard (r c, r b))
    | ExprKind.ExprReplicate (c, b) -> inheritSpan e (ExprReplicate (r c, r b))
    | ExprKind.ExprMask (a, p) -> inheritSpan e (ExprMask (r a, r p))
    | ExprKind.ExprCompound (d, m) -> inheritSpan e (ExprCompound (r d, r m))
    | ExprKind.ExprSparse (v, k) -> inheritSpan e (ExprSparse (r v, r k))
    | ExprKind.ExprIntersect (a, b) -> inheritSpan e (ExprIntersect (r a, r b))
    | ExprKind.ExprUnion (a, b) -> inheritSpan e (ExprUnion (r a, r b))
    | ExprKind.ExprContains (a, v) -> inheritSpan e (ExprContains (r a, r v))
    | ExprKind.ExprGroupBy (v, g) -> inheritSpan e (ExprGroupBy (r v, r g))
    | ExprKind.ExprSort (a, k) -> inheritSpan e (ExprSort (r a, r k))
    | ExprKind.ExprGram (l, rr) -> inheritSpan e (ExprGram (r l, r rr))
    | ExprKind.ExprReduce (a, k, init, ax) -> inheritSpan e (ExprReduce (r a, r k, Option.map r init, ax))
    | ExprKind.ExprStruct (nm, fields, spread) ->
        inheritSpan e (ExprStruct (nm, fields |> List.map (fun (fn, fe) -> (fn, r fe)), Option.map r spread))
    | ExprKind.ExprFor (src, cs, kern) ->
        let src' =
            match src with
            | ForArrays (arrs, inClause) -> ForArrays (List.map r arrs, Option.map r inClause)
            | ForKernel k -> ForKernel (r k)
        inheritSpan e (ExprFor (src', cs, Option.map r kern))
    // Leaves: no sub-expressions. Index/type args (range<I>, reverse<I>) carry TypeExprs, not Exprs, and are never rewritten.
    | ExprKind.ExprLit _ | ExprKind.ExprVar _ | ExprKind.ExprWildcard
    | ExprKind.ExprQualified _ | ExprKind.ExprRange _ | ExprKind.ExprReverse _
    | ExprKind.ExprArity _ | ExprKind.ExprNth | ExprKind.ExprZero
    | ExprKind.ExprSection _ -> e

/// Normalize a qualified constraint-conjunct name (`"<alias>.indep"` from the parser's dotted where-clause arm) to the internal name.
let private stripConjunctName (aliases: Set<string>) (cname: string) : string =
    match cname.Split('.') with
    | [| a; "indep" |] when Set.contains a aliases -> "__ppl_indep"
    | _ -> cname

/// Apply stripQualified to every expression-bearing decl (function where-clause conjunct names and struct where-invariants included).
let private stripDecl (aliases: Set<string>) (d: Located<Decl>) : Located<Decl> =
    let s = stripQualified aliases
    let value =
        match d.Value with
        | DeclFunction fd ->
            let w' =
                fd.WhereClause
                |> Option.map (fun w ->
                    { w with Custom = w.Custom |> List.map (fun (n, args) -> (stripConjunctName aliases n, args)) })
            DeclFunction { fd with Body = s fd.Body; WhereClause = w' }
        | DeclLet b -> DeclLet { b with Value = s b.Value }
        | DeclStatic b -> DeclStatic { b with Value = s b.Value }
        | DeclType (TyDeclStruct (sname, tps, fields, conjuncts, isStatic)) ->
            DeclType (TyDeclStruct (sname, tps, fields, conjuncts |> List.map s, isStatic))
        | other -> other
    { d with Value = value }

let private expandModuleCore (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    // User definitions shadow the formers entirely (same rule as ML ops and the math intrinsics).
    let declNames =
        decls |> List.choose (fun d ->
            match d.Value with
            | DeclFunction fd -> Some fd.Name
            | _ -> None)
        |> Set.ofList
    let active n = not (Set.contains n declNames)
    // Same-module top-level functions by name: dist_map accepts a named function or a prefix partial application of one in
    // its map slot and inlines the body at elaboration time.
    let funcDecls : Map<string, FunctionDecl> =
        decls |> List.choose (fun d ->
            match d.Value with
            | DeclFunction fd -> Some (fd.Name, fd)
            | _ -> None)
        |> Map.ofList
    // The module's `import ad` alias, if any (the ad import survives ppl
    // elaboration; the Grad pass consumes it later). hmc requires it: the
    // leapfrog gradient is emitted as the `<alias>.grad(logpost)` surface
    // form for Grad to expand -- gate (a) of the plan's import-ad options,
    // chosen because it needs no Grad-side marker machinery at all.
    let adAlias : string option =
        decls |> List.tryPick (fun d ->
            match d.Value with
            | DeclImport (["ad"], ImportQualified aliasOpt) -> Some (defaultArg aliasOpt "ad")
            | _ -> None)
    match resolveStatics decls with
    | Error e -> Error $"PPL elaboration: static resolution failed: {e}"
    // Fold failures are the type-checker's to report (assertion semantics).
    | Ok (statics, _) ->
        // Pass 0.5: strip indep(...) conjuncts out of struct where-invariants (static licenses, not runtime propositions);
        // residual invariants keep their construction-time validate().
        let mutable structIndep : Map<string, (string * string) list> = Map.empty
        let decls =
            decls |> List.map (fun d ->
                match d.Value with
                | DeclType (TyDeclStruct (sname, tps, fields, conjuncts, isStatic)) when not conjuncts.IsEmpty ->
                    // Per-conjunct split: an indep(...) conjunct is consumed as a static license (`&&`-joined forms split
                    // recursively); residual conjuncts stay runtime-checked.
                    let (pairs, residuals) =
                        conjuncts |> List.fold (fun (ps, rs) c ->
                            let (cp, cr) = splitInvariant c
                            (ps @ cp, rs @ Option.toList cr)) ([], [])
                    if pairs.IsEmpty then d
                    else
                        structIndep <- Map.add sname pairs structIndep
                        { d with Value = DeclType (TyDeclStruct (sname, tps, fields, residuals, isStatic)) }
                | _ -> d)
        // Array-typed struct fields and struct-typed instances: each instance contributes alias-named array shapes and, per
        // the struct's declared indep pairs, instance-scoped independence.
        let structFields =
            decls |> List.fold (fun acc d ->
                match d.Value with
                | DeclType (TyDeclStruct (sname, _, fields, _, _)) ->
                    let arrFields =
                        fields |> List.choose (fun f ->
                            match f.Type with
                            | TyArray (e, ix) -> Some (f.Name, (e, ix))
                            | _ -> None)
                        |> Map.ofList
                    Map.add sname arrFields acc
                | _ -> acc) Map.empty
        let instances =
            decls |> List.fold (fun acc d ->
                match d.Value with
                | DeclLet { Pattern = { Kind = PatternKind.PatVar iname }; Value = { Kind = ExprKind.ExprStruct (sname, _, _) } } when Map.containsKey sname structFields ->
                    Map.add iname sname acc
                | _ -> acc) Map.empty
        let aliasArrays =
            instances |> Map.fold (fun acc iname sname ->
                match Map.tryFind sname structFields with
                | Some fs -> fs |> Map.fold (fun a fName shape -> Map.add (aliasOf iname fName) shape a) acc
                | None -> acc) Map.empty
        let structPairIndep =
            instances |> Map.fold (fun acc iname sname ->
                match Map.tryFind sname structIndep with
                | Some pairs -> pairs |> List.fold (fun s (fa, fb) -> Set.add (indepKey (aliasOf iname fa) (aliasOf iname fb)) s) acc
                | None -> acc) Set.empty
        // Pass 0.8: normalize struct-field arguments of former calls to alias bindings (`let __ppl_arr_m_f = m.f`), inserted
        // before first use -- codegen's loop machinery iterates named bindings, not raw field accesses.
        let mutable emittedAliases = Set.empty
        let decls =
            decls |> List.collect (fun d ->
                let normArgs (args: Expr list) : Expr list * Located<Decl> list =
                    args |> List.fold (fun (acc, ads) a ->
                        match a.Kind with
                        | ExprKind.ExprField ({ Kind = ExprKind.ExprVar m }, f) when Map.containsKey m instances ->
                            let al = aliasOf m f
                            let newDecls =
                                if Set.contains al emittedAliases then []
                                else
                                    emittedAliases <- Set.add al emittedAliases
                                    [ { Value = DeclLet { Pattern = mkPat d.Span (PatVar al); Type = None; Value = a; Mutability = BindLet }; Span = d.Span } ]
                            (acc @ [mkExpr d.Span (ExprVar al)], ads @ newDecls)
                        | _ -> (acc @ [a], ads)) ([], [])
                match d.Value with
                | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar n }, args) } } as b) when Set.contains n formerNames && active n ->
                    let (args', aliasDecls) = normArgs args
                    aliasDecls @ [ { d with Value = DeclLet { b with Value = mkExpr d.Span (ExprApp (mkExpr d.Span (ExprVar n), args')) } } ]
                | _ -> [d])
        // Pass 1: consume `let _ = independent(X, Y)` declarations.
        let mutable indep = structPairIndep
        let mutable rest = []
        let mutable err = None
        for d in decls do
            match d.Value with
            | DeclLet { Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "independent" }, args) } } when active "independent" ->
                match args with
                | [{ Kind = ExprKind.ExprVar x }; { Kind = ExprKind.ExprVar y }] when x <> y ->
                    indep <- Set.add (indepKey x y) indep
                | [{ Kind = ExprKind.ExprVar x }; { Kind = ExprKind.ExprVar y }] when x = y ->
                    if err.IsNone then err <- Some $"independent({x}, {y}): an array is not independent of itself"
                | _ ->
                    if err.IsNone then err <- Some "independent expects two array names (or struct fields): `let _ = ppl.independent(X, Y)`"
            | _ -> rest <- rest @ [d]
        match err with
        | Some e -> Error e
        | None ->
        let aliases = collectAliases rest
        // Inferred shapes for UN-annotated computed arrays (method_for over range/halo slots) seed the map; explicit
        // annotations and struct-field aliases override them.
        let inferredArrays =
            rest |> List.fold (fun acc d ->
                match d.Value with
                | DeclLet b | DeclStatic b ->
                    match b.Pattern, b.Type with
                    | { Kind = PatternKind.PatVar name }, None ->
                        match computedShapeOf aliases statics b.Value with
                        | Some shape -> Map.add name shape acc
                        | None -> acc
                    | _ -> acc
                | _ -> acc) Map.empty
        // Pre-scan: dist-over-chain wrappers. Pass 2 synthesizes an
        // Idx<1> x Idx<n> wrapper for `dist(chain, r)` over an mh/hmc chain
        // (see the dist dispatch arm); the wrapper's shape must be in the
        // SHARED ctx.Arrays built here, because downstream tower consumers
        // holding that ctx (distDim under dist_expect/dist_reweight, the
        // approx bridge) resolve the dist's source shape through it. Names
        // that never elaborate (a chain decl that later refuses) are
        // harmless, the poolMax convention.
        let chainLenScan =
            rest |> List.choose (fun d ->
                match d.Value with
                | DeclLet { Pattern = { Kind = PatternKind.PatVar cn }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, cargs) } }
                    when (f = "mh" || f = "hmc") && active f ->
                    (match cargs with
                     | [_; _; nE; _; _] when f = "mh" ->
                         (match evalExpr statics maxSteps nE with
                          | Ok (SVInt x) when x >= 2L -> Some (cn, int x)
                          | _ -> None)
                     | [_; _; nE; _; _; _] when f = "hmc" ->
                         (match evalExpr statics maxSteps nE with
                          | Ok (SVInt x) when x >= 2L -> Some (cn, int x)
                          | _ -> None)
                     | _ -> None)
                | _ -> None)
            |> Map.ofList
        let distChainWraps =
            rest |> List.choose (fun d ->
                match d.Value with
                | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist" }, [{ Kind = ExprKind.ExprVar cn }; _]) } }
                    when active "dist" ->
                    chainLenScan
                    |> Map.tryFind cn
                    |> Option.map (fun n -> ($"__ppl_dist_{dName}_chain", (tyFloat64, [TyIdx (iLit 1); TyIdx (iLit n)])))
                | _ -> None)
        let arrays =
            collectArrays rest
            |> Map.fold (fun acc k s -> Map.add k s acc) inferredArrays
            |> fun annotated -> aliasArrays |> Map.fold (fun acc k s -> Map.add k s acc) annotated
            |> fun withAliases -> distChainWraps |> List.fold (fun acc (k, s) -> Map.add k s acc) withAliases
        // Pre-scan: the maximal multiset size each source array needs across
        // all its single-array formers in this module, so the first former
        // emits ONE maximal pool the rest reuse. Names that turn out to be
        // dist bindings or ineligible arrays are harmless here.
        let poolMax =
            rest |> List.choose (fun d ->
                match d.Value with
                | DeclLet { Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, [{ Kind = ExprKind.ExprVar a }; kExpr]) } }
                    when (List.contains f ["moments"; "cumulants"; "free_cumulants"; "mstate"; "comoments"]) && active f ->
                    (match evalExpr statics maxSteps kExpr with
                     | Ok (SVInt k) when k >= 1L && k <= 8L -> Some (a, int k)
                     | _ -> None)
                | _ -> None)
            |> List.fold (fun m (a, k) -> Map.add a (max k (defaultArg (Map.tryFind a m) 0)) m) Map.empty
        let ctx = { Arrays = arrays; Aliases = aliases; Statics = statics; Indep = indep
                    Pools = ref Map.empty; PoolMax = poolMax; FlatDims = ref Map.empty }
        // Pass 1.5: expression-position logpdf/loglik inside top-level
        // function bodies (the density-form model layer, plan section 4) --
        // each site becomes a block of statement lets, so `function
        // logpost(theta) -> Float = logpdf(...) + loglik(...)` is ordinary
        // straight-line/loop code by the time the checker sees it. Other
        // formers in bodies still hit pass 3's misplaced-use refusal.
        let rewrittenRes =
            rest |> List.fold (fun acc d ->
                acc |> Result.bind (fun ds ->
                    match d.Value with
                    | DeclFunction fd ->
                        Blade.Ast.synthSpan <- d.Span
                        rewriteBodyFormers active ctx fd.Name fd.Body
                        |> Result.map (fun b -> ds @ [ { d with Value = DeclFunction { fd with Body = b } } ])
                    | _ -> Ok (ds @ [d]))) (Ok [])
        match rewrittenRes with
        | Error e -> Error e
        | Ok rest ->
        // ppl.mh chains elaborated in this module: chain name -> static
        // length, consumed by the chain diagnostics formers below.
        let mutable chainLens : Map<string, int> = Map.empty
        // Pass 2: rewrite decl-RHS former calls, threading the dist registry (dist bindings are compile-time objects: consumed
        // here, their cumulant components materialize as ordinary array decls).
        let expanded =
            rest |> List.fold (fun acc d ->
                acc |> Result.bind (fun (ds, dists, mstates) ->
                    // Stamp the user decl's span so every syn-built (former-generated) node attributes to this decl's line.
                    Blade.Ast.synthSpan <- d.Span
                    match d.Value with
                    // moments(d, k) on a DIST binding: reconstruction (kappa->mu, Wick under closure), dispatched by registry
                    // membership, ahead of the data-array form.
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar _ }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "moments" }, [{ Kind = ExprKind.ExprVar dn }; kExpr]) } } as b) when active "moments" && Map.containsKey dn dists ->
                        let info = dists.[dn]
                        distDim ctx info
                        |> Result.bind (fun dim -> elabMomentsOfDist ctx d.Span b info dim kExpr)
                        |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "moments" }, args) } } as b) when active "moments" ->
                        elabMoments ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "mixed_cumulants" }, args) } } as b) when active "mixed_cumulants" ->
                        elabMixedCumulants ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_affine" }, args) } } as b) when active "dist_affine" ->
                        elabDistAffine ctx d.Span b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "free_cumulants" }, args) } } as b) when active "free_cumulants" ->
                        elabFreeCumulants ctx d.Span b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "comoments" }, args) } } as b) when active "comoments" ->
                        elabComoments ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "cumulants" }, args) } } as b) when active "cumulants" ->
                        elabCumulants ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "comoments_merge" }, args) } } as b) when active "comoments_merge" ->
                        elabComomentsMerge ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist" }, args) } } when active "dist" ->
                        // The P4 round-trip: dist over an mh/hmc chain. A
                        // chain binding is rank-1 with no declared annotation
                        // (neither of arrayShape's routes sees it), but its
                        // length is in the chain registry -- wrap it to
                        // Idx<1> x Idx<n> through the same range-kernel form
                        // corpus 096 writes by hand, register the wrapper's
                        // shape, and elaborate dist over the wrapper.
                        (match args with
                         | [{ Kind = ExprKind.ExprVar cn }; rExpr] when Map.containsKey cn chainLens ->
                             let n = chainLens.[cn]
                             let wrapN = $"__ppl_dist_{dName}_chain"
                             let wrapVal =
                                 computeE (applyE
                                     (methodForE [syn (ExprRange [TyIdx (iLit 1); TyIdx (iLit n)])])
                                     (lambdaE [ { Name = "__i"; Type = None; Default = None; NameSpan = noSpan }
                                                { Name = "__j"; Type = None; Default = None; NameSpan = noSpan } ] None
                                              (appE (v cn) [v "__j"])))
                             let wrapDecl = { Value = DeclLet { Pattern = pvar wrapN; Type = None; Value = wrapVal; Mutability = BindLet }; Span = d.Span }
                             let ctx2 = { ctx with Arrays = Map.add wrapN (tyFloat64, [TyIdx (iLit 1); TyIdx (iLit n)]) ctx.Arrays }
                             elabDist ctx2 d.Span dName [mkExpr d.Span (ExprVar wrapN); rExpr]
                             |> Result.map (fun (nds, info) -> (ds @ [wrapDecl] @ nds @ [distPackDecl d.Span dName info], Map.add dName info dists, mstates))
                         | _ ->
                             elabDist ctx d.Span dName args |> Result.map (fun (nds, info) -> (ds @ nds @ [distPackDecl d.Span dName info], Map.add dName info dists, mstates)))
                    // A FLAT operand (constructor/pushforward towers) makes the
                    // result flat: register-only, like dist_jet -- __dist_pack's
                    // erasure type declares rank-k SymIdx-packed components and
                    // the flat combine outputs are rank-1, so packing one is a
                    // codegen type clash. cumulant() projects at elaboration.
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_add" }, args) } } when active "dist_add" ->
                        elabDistCombine "+" (fun _ -> 1.0) ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds @ (if info.Flat then [] else [distPackDecl d.Span dName info]), Map.add dName info dists, mstates))
                    // Dist operators (+ / - / scalar *) flow through untouched: dists are values (distPackDecl), and the
                    // checker's inferDistBinOp dispatches operators in any expression position, gated on the independence
                    // state this module exports (Independence.addDeclared/addSources below).
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_scale" }, args) } } when active "dist_scale" ->
                        elabDistScale ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds @ (if info.Flat then [] else [distPackDecl d.Span dName info]), Map.add dName info dists, mstates))
                    // The Faa di Bruno pushforward: a univariate order-q dist with FLAT 1-cell components. Registered but NOT
                    // packed: __dist_pack's erasure type declares SymIdx-packed components, and flat ArrayLits aren't.
                    // cumulant(d, k) on flat dists projects at elaboration (arm below).
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_jet" }, args) } } when active "dist_jet" ->
                        elabDistJet false "dist_jet" ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_jet_closed" }, args) } } when active "dist_jet_closed" ->
                        elabDistJet true "dist_jet_closed" ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // dist_map: the symbolic front-end, same registration and flat-component representation as dist_jet.
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_map" }, args) } } when active "dist_map" ->
                        elabDistMap false ctx d.Span dName funcDecls dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_map_closed" }, args) } } when active "dist_map_closed" ->
                        elabDistMap true ctx d.Span dName funcDecls dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // Tower Bayes: dist_expect is a scalar projection; dist_reweight / dist_mix register flat univariate
                    // dists exactly like the jet results above.
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_expect" }, args) } } as b) when active "dist_expect" ->
                        elabDistExpect ctx d.Span b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_reweight" }, args) } } when active "dist_reweight" ->
                        elabDistReweight ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_mix" }, args) } } when active "dist_mix" ->
                        elabDistMix ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // Multivariate Gaussian conditioning (plan P5): Schur
                    // complement on the kappa_2 block; registers a flat
                    // (D-1)-dimensional order-2 dist, register-only like the
                    // pushforwards (flat components are not __dist_pack-able).
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_condition" }, args) } } when active "dist_condition" ->
                        elabDistCondition ctx d.Span dName dists args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // Signed atomic towers: quasi-dists as registered values; negativity as a scalar meter.
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_atoms" }, args) } } when active "dist_atoms" ->
                        elabDistAtoms ctx d.Span dName args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_negativity" }, args) } } as b) when active "dist_negativity" ->
                        elabDistNegativity ctx d.Span b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // Named-family constructors (plan P1): closed-form cumulant
                    // towers, registered flat-univariate exactly like dist_atoms
                    // (value erased, components live; cumulant() projects at
                    // elaboration through the __ppl_cumulant arm below).
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar fam }, args) } } when Set.contains fam familyNames && active fam ->
                        elabFamilyDist ctx d.Span fam dName args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // Log-densities (plan P1): scalar logpdf; loglik as the AD-able accumulation loop.
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "logpdf" }, args) } } as b) when active "logpdf" ->
                        elabLogPdf active ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "loglik" }, args) } } as b) when active "loglik" ->
                        elabLogLik active ctx d.Span outName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // Conjugate posterior updates (plan P5): the closed-form
                    // posterior as a registered family tower, flat univariate
                    // like the constructors it reuses.
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar dName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "bayes" }, args) } } when active "bayes" ->
                        elabBayes active ctx d.Span dName args |> Result.map (fun (nds, info) -> (ds @ nds, Map.add dName info dists, mstates))
                    // The approximate tower bridge (plan section 2): Edgeworth
                    // density, Cornish-Fisher quantile, and the uniform-fill
                    // Cornish-Fisher sampler -- scalar/array projections off a
                    // registered univariate tower, packed or flat alike.
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_pdf_approx" }, args) } } as b) when active "dist_pdf_approx" ->
                        elabDistPdfApprox ctx d.Span outName b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_quantile_approx" }, args) } } as b) when active "dist_quantile_approx" ->
                        elabDistQuantileApprox ctx d.Span outName b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar sName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "dist_sample_approx" }, args) } } as b) when active "dist_sample_approx" ->
                        elabDistSampleApprox ctx d.Span sName b dists args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // Exact family sampling (plan P2 surface): sample(family(params), key, n)
                    // lowers to the matching __rand_* intrinsic fill.
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar sName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "sample" }, args) } } as b) when active "sample" ->
                        elabPplSample active ctx d.Span sName b args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // Sampling inference (plan P4): the random-walk Metropolis
                    // and leapfrog-HMC chains and their diagnostics (see the
                    // section comment at elabMh for shapes and conventions).
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar chainName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "mh" }, args) } } as b) when active "mh" ->
                        elabMh ctx d.Span chainName b funcDecls args |> Result.map (fun (nds, n) ->
                            chainLens <- Map.add chainName n chainLens
                            (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar chainName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "hmc" }, args) } } as b) when active "hmc" ->
                        elabHmc ctx d.Span chainName b funcDecls adAlias args |> Result.map (fun (nds, n) ->
                            chainLens <- Map.add chainName n chainLens
                            (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "chain_mean" }, args) } } as b) when active "chain_mean" ->
                        elabChainMoment false ctx d.Span outName b chainLens args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "chain_var" }, args) } } as b) when active "chain_var" ->
                        elabChainMoment true ctx d.Span outName b chainLens args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "autocorr" }, args) } } as b) when active "autocorr" ->
                        elabAutocorr ctx d.Span outName b chainLens args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "ess" }, args) } } as b) when active "ess" ->
                        elabEss ctx d.Span outName b chainLens args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    | DeclLet ({ Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "rhat" }, args) } } as b) when active "rhat" ->
                        elabRhat ctx d.Span outName b chainLens args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // cumulant(d, k) on a FLAT registry dist (a pushforward
                    // result): no packed value exists for the checker's
                    // Dist-typed projection to see, so project here; the
                    // order guard is an elaboration error instead.
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "__ppl_cumulant" }, [{ Kind = ExprKind.ExprVar dn }; kExpr]) } } as b)
                        when Map.containsKey dn dists && dists.[dn].Flat ->
                        let info = dists.[dn]
                        (match evalExpr ctx.Statics maxSteps kExpr with
                         | Ok (SVInt k) when k >= 1L && int k <= info.Order ->
                             Ok (ds @ [ { d with Value = DeclLet { b with Value = mkExpr d.Span (ExprVar info.Components.[int k - 1]) } } ], dists, mstates)
                         | Ok (SVInt k) ->
                             Error $"cumulant: order {k} exceeds the dist's carried order {info.Order} -- insufficient stochastic order. Construct with a higher order or project a carried component."
                         | _ ->
                             Error "cumulant: the order must be a compile-time integer (a literal, `let static`, or static-function call)")
                    // Streaming state formers: mstate/mstate_merge bind compile-time state objects; mstate_cumulants freezes
                    // one into destructured cumulant tensors.
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar sName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "mstate" }, args) } } when active "mstate" ->
                        elabMState ctx d.Span sName args |> Result.map (fun (nds, info) -> (ds @ nds, dists, Map.add sName info mstates))
                    | DeclLet { Pattern = { Kind = PatternKind.PatVar outName }; Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "mstate_merge" }, args) } } when active "mstate_merge" ->
                        elabMStateMerge ctx d.Span outName mstates args |> Result.map (fun (nds, info) -> (ds @ nds, dists, Map.add outName info mstates))
                    | DeclLet ({ Value = { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "mstate_cumulants" }, args) } } as b) when active "mstate_cumulants" ->
                        elabMStateCumulants ctx d.Span b mstates args |> Result.map (fun nds -> (ds @ nds, dists, mstates))
                    // cumulant(d, k) flows through untouched: it is a checker-level projection on the Dist-typed value that
                    // distPackDecl binds (TypeCheck.inferCumulantProj).
                    | _ -> Ok (ds @ [d], dists, mstates)))
                (Ok ([], Map.empty, Map.empty))
        // Pass 3: any surviving former reference is misplaced -- formers are decl-RHS only, `independent` only as a consumed declaration.
        expanded |> Result.bind (fun (ds, dists, _mstates) ->
            let activeFormers = formerNames |> Set.filter active
            let misplaced =
                ds |> List.exists (fun d ->
                    let check e = anyExpr (isFormerCallOf activeFormers) e
                    match d.Value with
                    | DeclFunction fd -> check fd.Body
                    | DeclLet b | DeclStatic b -> check b.Value
                    | _ -> false)
            if misplaced then
                Error "moments/comoments must be the entire right-hand side of a top-level let (moments(A, k) as a nested expression is deferred); independent(X, Y) must be a top-level `let _ = ppl.independent(X, Y)` declaration"
            else
                // Export independence state for the checker: the declared relation gates Dist +- dispatch and call-site
                // discharge; dist sources seed value provenance.
                Independence.addDeclared indep
                Independence.addSources (dists |> Map.map (fun _ info -> info.Sources))
                // Every dist, packed or not -- Ide.fs only consults this for names with no binding, so packed ones' real types win.
                IdeDists.add (dists |> Map.map (fun _ info -> (info.Order, info.Components)))
                Ok ds)

/// Import-gated wrapper. With no `import ppl` in the module, PPL elaboration is a no-op -- bare former names are left unbound.
/// With an alias in scope, the qualified surface (`ppl.moments(...)`, `ppl.cumulant(...)`) is normalized to the internal
/// forms the core passes and the checker recognize. The `import ppl` decl itself is consumed here.
let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    pplAliasesOf decls |> Result.bind (fun aliases ->
        if Set.isEmpty aliases then Ok decls
        else
            decls
            |> List.filter (not << isPplImport)
            |> List.map (stripDecl aliases)
            |> expandModuleCore)

/// Entry point: elaborate PPL formers across a program. Runs after ML-op elaboration and before grad expansion, so grad()
/// differentiates the generated pipelines as plain Blade source.
let private expandStr (program: Program) : Result<Program, string> =
    // Register the `indep` where-clause handler (idempotent) and start a
    // fresh independence state for this compilation -- expand always runs
    // before checkProgram in the same async flow, so the checker sees this
    // program's declared relation and dist sources.
    Independence.register ()
    Independence.reset ()
    IdeDists.reset ()
    program.Modules
    |> List.fold (fun acc m ->
        acc |> Result.bind (fun ms ->
            expandModule m.Decls |> Result.map (fun ds -> ms @ [{ m with Decls = ds }])))
        (Ok [])
    |> Result.map (fun ms -> { program with Modules = ms })

/// Boundary: string-errored internals -> coded diagnostics. The span is the ambient synthSpan -- stamped per-decl by expandStr,
/// so a mid-elaboration failure points at the offending declaration.
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun msg ->
        [ Blade.Diagnostics.mkError "BL5100" (Blade.Diagnostics.Codes.phaseOfCode "BL5100") Blade.Ast.synthSpan msg ])
