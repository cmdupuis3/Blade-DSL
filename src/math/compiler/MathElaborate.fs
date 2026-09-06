/// Math-module elaboration: dense linear algebra and tensor decompositions as compile-time source synthesis, mirroring the
/// ml/ppl elaborators.
///
/// Surface (reachable only through `import math [as <alias>]`; config ints are `let static` names or literals). The ops
/// read the DECLARED shape (this pass runs before type inference and the generated routine is specialized to the extents),
/// so an array argument must carry an annotation: a variable bound by an annotated `let` (module-level or block-local), an
/// annotated parameter, a call of a function with an annotated array return type, or an ascription `(expr : Array<...>)`.
///
///   m.matmul(A, B)                 -- m x k . k x n -> m x n
///   m.solve(A, b)                  -- A.x = b by partial-pivoted LU, A n x n, b n -> x n
///   m.svd(A) | m.svd(A, SWEEPS)    -- thin SVD, m >= n -> (U, S, V), S descending
///   m.eigh(S) | m.eigh(S, SWEEPS)  -- symmetric -> (Q, LAM), LAM descending
///   m.unfold(X, MODE)              -- Kolda-Bader mode-n matricization
///   m.mode_product(X, U, MODE)     -- tensor x matrix along MODE
///   m.hosvd(X) | m.hosvd(X, R1..RN)-- Tucker/HOSVD -> (G, U1, ..., UN)
///
/// For each distinct (op, resolved shape/config) the elaborator synthesizes ONE Blade function (`__math_1`, ...) via
/// Blade.Math.Decls; call sites rewrite to the generated names, deduped by fingerprint.
///
/// Pipeline position: after ML/PPL elaboration, before Grad expansion, so generated bodies remain plain differentiable
/// Blade source.
module Blade.Math.Elaborate

open Blade.Ast
open Blade.StaticEval
open Blade.Math.Decls

// Module-level context: annotated array shapes, aliases, statics

/// A name -> declared array shape map (the module-level tables).
type private Shapes = Map<string, TypeExpr * TypeExpr list>

/// The LEXICAL scope threaded through the walker: parameters and block-local lets. A binder is recorded even when it is NOT
/// array-annotated (as `None`), because it still SHADOWS an outer name -- without that, an unannotated local `let f = ...`
/// over a module-level annotated `f` would fall through to the module-level shape and silently run at the wrong extents.
type private Scope = Map<string, (TypeExpr * TypeExpr list) option>

/// Shape/static context the op elaborations read. The ops read a DECLARED shape, never an inferred one (this pass runs
/// before type inference), but the declaration may live on a let, a parameter, a function return type, or an ascription.
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
/// before matching for TyArray.
///
/// TyBounded is TRANSPARENT here, for diagnostic ORDER only. A bound on an aggregate is rejected by the checker (BL4003,
/// TypeCheck.boundedAggregateError), but this pass runs BEFORE the checker: without this arm `A: MA<min=0.0, max=99.0>`
/// fails to match TyArray and the user is told "'A' has no declared array shape" about an annotation that plainly declares
/// one. Seeing through the bound lets the elaborator do its job and the checker deliver the diagnostic that actually
/// explains the mistake. A bound on a SCALAR still resolves to a non-array and is still not a shape.
let rec private resolveTop (aliases: Map<string, TypeExpr>) (fuel: int) (ty: TypeExpr) =
    match ty with
    | TyNamed (n, []) when fuel > 0 ->
        match Map.tryFind n aliases with
        | Some body -> resolveTop aliases (fuel - 1) body
        | None -> ty
    // TyBounded resolves through to its base: bounds on AGGREGATE types are rejected upstream (boundedAggregateError).
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

/// Top-level functions whose annotated RETURN type is an array: a call of one
/// is as good a shape witness as an annotated let.
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

/// Resolve an index TypeExpr to its static extent, following alias chains.
let rec private resolveExtent (ctx: Ctx) (ty: TypeExpr) : int option =
    match ty with
    | TyIdx extent ->
        match evalExpr ctx.Statics maxSteps extent with
        | Ok (SVInt n) -> Some (int n)
        | _ -> None
    | TyNamed (name, []) ->
        match Map.tryFind name ctx.Aliases with
        | Some body -> resolveExtent ctx body
        // Not a source alias: a provider axis path, registered by TypeEnv during type checking, so the extent comes
        // from the store's metadata instead.
        | None -> providerIndexExtent ctx.Statics name
    // A bound in an alias body does not change the extent (twin arm in SpectraElaborate.resolveExtent).
    | TyBounded (inner, _, _) -> resolveExtent ctx inner
    | _ -> None

/// The steer appended to every "no declared shape here" rejection.
let private shapeSources =
    "math ops read the DECLARED shape at compile time (the generated routine is specialized to the extents), so the argument must carry an annotation"

/// The declared shape of an op's array argument: every axis extent must be statically known. `scope` carries the lexical
/// shapes in force here (annotated params and block-local annotated lets). The returned label is what op-level messages quote.
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
        Error $"{what}: the array argument must be a plain variable naming an annotated let, an annotated parameter, a call of a function with an annotated array return type, or an ascription `(expr : Array<Float64 like Idx<...>, ...>)` -- {shapeSources}"

/// Resolve a static-argument expression: a plain variable naming a `let static` binding, or an inline int literal.
let private staticArg (statics: StaticEnv) (what: string) (e: Expr) : Result<StaticValue, string> =
    match e.Kind with
    | ExprKind.ExprLit (LitInt n) -> Ok (SVInt n)
    | ExprKind.ExprVar name ->
        match Map.tryFind name statics.Values with
        | Some sv -> Ok sv
        | None -> Error $"{what}: '{name}' is not a `let static` binding (math op configs must be static)"
    | _ -> Error $"{what}: config argument must be a `let static` binding name or literal"

let private staticInt (statics: StaticEnv) (what: string) (e: Expr) : Result<int, string> =
    staticArg statics what e |> Result.bind (fun sv ->
        match sv with
        | SVInt n -> Ok (int n)
        | _ -> Error $"{what}: expected a static int")

// Elaboration state (fingerprint-deduped generated decls)
type private ElabState = {
    mutable Counter: int
    /// (op, config fingerprint) -> generated function name
    mutable Made: Map<string, string>
    /// generated decls in creation order
    mutable Decls: FunctionDecl list
}

let private fingerprint (op: string) (parts: obj) : string =
    sprintf "%s|%A" op parts

let private ensure (st: ElabState) (key: string) (make: string -> Result<FunctionDecl, string>)
    : Result<string, string> =
    match Map.tryFind key st.Made with
    | Some n -> Ok n
    | None ->
        st.Counter <- st.Counter + 1
        let n = $"__math_{st.Counter}"
        make n |> Result.map (fun decl ->
            st.Made <- Map.add key n st.Made
            st.Decls <- st.Decls @ [ decl ]
            n)

/// ensure for a TOTAL decl builder (no failure path) -- used by the hosvd orchestration, which ensures a batch per mode.
let private ensureT (st: ElabState) (key: string) (make: string -> FunctionDecl) : string =
    match ensure st key (fun n -> Ok (make n)) with
    | Ok n -> n
    | Error e -> failwith e // unreachable: make is total

// Op elaboration

let private opNames =
    Set.ofList [ "matmul"; "solve"; "svd"; "eigh"; "eig"; "unfold"; "mode_product"; "hosvd" ]

let private opList = "matmul, solve, svd, eigh, eig, unfold, mode_product, hosvd"

/// Optional trailing SWEEPS argument shared by svd/eigh.
let private sweepsArg (statics: StaticEnv) (what: string) (rest: Expr list) : Result<int, string> =
    match rest with
    | [] -> Ok defaultSweeps
    | [swE] ->
        staticInt statics (what + " SWEEPS") swE |> Result.bind (fun s ->
            if s >= 1 then Ok s else Error $"{what}: SWEEPS must be >= 1")
    | _ -> Error $"{what}: at most one SWEEPS argument"

/// Elaborate one qualified op call. Arguments arrive already rewritten.
let private elabOp (st: ElabState) (ctx: Ctx) (scope: Scope) (op: string) (args: Expr list) : Result<Expr, string> =
    match op, args with
    | "matmul", [aE; bE] ->
        arrayShape ctx scope "matmul" aE |> Result.bind (fun (_, aDims) ->
        arrayShape ctx scope "matmul" bE |> Result.bind (fun (_, bDims) ->
            match aDims, bDims with
            | [_m; k], [k2; _n] when k = k2 ->
                // matmul is not synthesized as a per-(m,k,n) Blade triple loop: it rewrites to the `__math_matmul` intrinsic
                // marker, which TypeCheck.inferMatmul types and codegen emits as one `blade_linalg::blade_matmul` call (the
                // shim resolves to cblas_dgemm or a native fallback depending on the build). svd/eigh/eig/unfold/
                // mode_product/hosvd stay synthesized. Shape validation above is unchanged: matmul still reads the
                // DECLARED shapes and rejects the same programs with the same messages -- only what it lowers to changed.
                Ok (syn (ExprApp (v "__math_matmul", [aE; bE])))
            | [_; k], [k2; _] ->
                Error $"matmul: inner extents disagree (A is ..x{k}, B is {k2}x..)"
            | _ -> Error "matmul: both arguments must be rank-2 (mxk * kxn)"))
    | "matmul", _ -> Error "matmul: expected matmul(A, B)"
    | "solve", [aE; bE] ->
        arrayShape ctx scope "solve" aE |> Result.bind (fun (_, aDims) ->
        arrayShape ctx scope "solve" bE |> Result.bind (fun (_, bDims) ->
            match aDims, bDims with
            | [n; n2], [m] when n = n2 && m = n ->
                // Like matmul and UNLIKE svd/eigh/eig, solve is not synthesized as a per-n Blade routine: it rewrites to
                // the `__math_solve` intrinsic marker, which TypeCheck.inferSolve types and codegen emits as either its own
                // LU loop nest or one `blade_lapack::blade_solve` call. NOTE THE ABSENCE OF A GATE CHECK HERE, which is the
                // one place this arm differs from eigh's: eigh's marker exists only when `lapackAvailable ()` held, because
                // its non-dispatched arm is synthesized SOURCE that must be generated instead. Solve's non-dispatched arm
                // is emitted by codegen from the same node, so the node is unconditional and the gate is consulted one
                // level down, at emission. That keeps the surface's meaning independent of the build.
                Ok (syn (ExprApp (v "__math_solve", [aE; bE])))
            | [n; n2], _ when n <> n2 ->
                Error $"solve: A must be square (got {n}x{n2})"
            | [n; _], [m] ->
                Error $"solve: b's extent must match A's dimension (A is {n}x{n}, b is length {m})"
            | [_; _], _ ->
                Error "solve: b must be rank-1 (Array<Float64 like Idx<n>>)"
            | _ -> Error "solve: A must be rank-2 square (Array<Float64 like Idx<n>, Idx<n>>)"))
    | "solve", _ -> Error "solve: expected solve(A, b) -- A an n x n matrix, b a length-n vector, returning x with A.x = b"
    | "svd", (aE :: rest) ->
        sweepsArg ctx.Statics "svd" rest |> Result.bind (fun sweeps ->
        arrayShape ctx scope "svd" aE |> Result.bind (fun (_, dims) ->
            match dims with
            | [m; n] when m >= n ->
                ensure st (fingerprint "svd" (box (m, n, sweeps))) (fun nm -> Ok (svdDecl nm m n sweeps))
                |> Result.map (fun nm -> syn (ExprApp (v nm, [aE])))
            | [m; n] ->
                Error $"svd: m < n unsupported ({m}x{n}); svd the transpose (transpose(A, [0, 1])) and swap U/V"
            | _ -> Error "svd: the argument must be rank-2 (Array<Float64 like Idx<m>, Idx<n>>)"))
    | "svd", _ -> Error "svd: expected svd(A) or svd(A, SWEEPS)"
    | "eigh", (aE :: rest) ->
        sweepsArg ctx.Statics "eigh" rest |> Result.bind (fun sweeps ->
        arrayShape ctx scope "eigh" aE |> Result.bind (fun (_, dims) ->
            match dims with
            | [n; n2] when n = n2 ->
                // `eigh` routes to the `__math_eigh` intrinsic marker (TypeCheck.inferEigh types it, codegen emits one
                // `blade_lapack::blade_eigh_*` call) only when BOTH hold: LAPACK is available at build time
                // (`LinAlgPatterns.lapackAvailable ()`, the same predicate Build.fs uses for `-DBLADE_HAS_LAPACK` -- two
                // copies disagreeing would emit calls into a header that won't compile); and NO explicit SWEEPS argument
                // was given (a stated sweep budget requests the cyclic-Jacobi ALGORITHM, which LAPACK's blocked tridiagonal
                // reduction has no analogue of, so `m.eigh(S, 30)` keeps the synthesized path on any machine).
                //
                // With the gate off (the default) the synthesized path is untouched, byte for byte: same
                // `ensure`/fingerprint key, same `eighDecl`, same call expression. That is load-bearing -- the Jacobi
                // source is the single copy of this math, the one the interpreter and pinned-oracle differentials cover,
                // and an eigensolver's output is not unique (eigenvector signs, degenerate-subspace bases), so it stays
                // the verification truth. `interp` / `diff-oracle` must never run with the LAPACK gate set.
                if List.isEmpty rest && Blade.LinAlgPatterns.lapackAvailable () then
                    Ok (syn (ExprApp (v "__math_eigh", [aE])))
                else
                    ensure st (fingerprint "eigh" (box (n, sweeps))) (fun nm -> Ok (eighDecl nm n sweeps))
                    |> Result.map (fun nm -> syn (ExprApp (v nm, [aE])))
            | [n; n2] -> Error $"eigh: the argument must be square (got {n}x{n2}); symmetry is assumed, not checked"
            | _ -> Error "eigh: the argument must be rank-2 square (Array<Float64 like Idx<n>, Idx<n>>, symmetric)"))
    | "eigh", _ -> Error "eigh: expected eigh(S) or eigh(S, SWEEPS)"
    | "eig", (aE :: rest) ->
        arrayShape ctx scope "eig" aE |> Result.bind (fun (_, dims) ->
            match dims with
            | [n; n2] when n = n2 ->
                let maxIterRes =
                    match rest with
                    | [] -> Ok (30 * n)
                    | [mE] ->
                        staticInt ctx.Statics "eig MAXITER" mE |> Result.bind (fun mi ->
                            if mi >= 1 then Ok mi else Error "eig: MAXITER must be >= 1")
                    | _ -> Error "eig: at most one MAXITER argument"
                maxIterRes |> Result.bind (fun maxIter ->
                    ensure st (fingerprint "eig" (box (n, maxIter))) (fun nm -> Ok (eigDecl nm n maxIter))
                    |> Result.map (fun nm -> syn (ExprApp (v nm, [aE]))))
            | [n; n2] -> Error $"eig: the argument must be square (got {n}x{n2})"
            | _ -> Error "eig: the argument must be rank-2 square (Array<Float64 like Idx<n>, Idx<n>>)")
    | "eig", _ -> Error "eig: expected eig(A) or eig(A, MAXITER) -- returns (LRE, LIM) by descending modulus"
    | "unfold", [xE; modeE] ->
        staticInt ctx.Statics "unfold MODE" modeE |> Result.bind (fun mode ->
        arrayShape ctx scope "unfold" xE |> Result.bind (fun (_, dims) ->
            let r = dims.Length
            if r < 2 || r > 4 then
                Error $"unfold: tensor rank must be 2..4 (got rank {r}); the generator is rank-generic -- raise the cap when needed"
            elif mode < 0 || mode >= r then
                Error $"unfold: MODE must be in 0..{r - 1} for a rank-{r} tensor (got {mode})"
            else
                ensure st (fingerprint "unfold" (box (dims, mode))) (fun nm -> Ok (unfoldDecl nm dims mode))
                |> Result.map (fun nm -> syn (ExprApp (v nm, [xE])))))
    | "unfold", _ -> Error "unfold: expected unfold(X, MODE) with a static MODE"
    | "mode_product", [xE; uE; modeE] ->
        staticInt ctx.Statics "mode_product MODE" modeE |> Result.bind (fun mode ->
        arrayShape ctx scope "mode_product" xE |> Result.bind (fun (_, dims) ->
        arrayShape ctx scope "mode_product" uE |> Result.bind (fun (_, uDims) ->
            let r = dims.Length
            if r < 2 || r > 4 then
                Error $"mode_product: tensor rank must be 2..4 (got rank {r})"
            elif mode < 0 || mode >= r then
                Error $"mode_product: MODE must be in 0..{r - 1} for a rank-{r} tensor (got {mode})"
            else
                match uDims with
                | [jOut; im] when im = dims.[mode] ->
                    ensure st (fingerprint "mode_product" (box (dims, mode, jOut)))
                        (fun nm -> Ok (modeProductDecl nm dims mode jOut))
                    |> Result.map (fun nm -> syn (ExprApp (v nm, [xE; uE])))
                | [_; im] ->
                    Error $"mode_product: U's second extent must match the mode-{mode} extent (U is ..x{im}, mode extent is {dims.[mode]})"
                | _ -> Error "mode_product: U must be rank-2 (Array<Float64 like Idx<j>, Idx<i_mode>>)")))
    | "mode_product", _ -> Error "mode_product: expected mode_product(X, U, MODE) with a static MODE"
    | "hosvd", (xE :: rankArgs) ->
        arrayShape ctx scope "hosvd" xE |> Result.bind (fun (_, dims) ->
            let r = dims.Length
            if r < 2 || r > 4 then
                Error $"hosvd: tensor rank must be 2..4 (got rank {r})"
            else
                let ranksRes =
                    if List.isEmpty rankArgs then Ok dims
                    elif rankArgs.Length <> r then
                        Error $"hosvd: expected {r} truncation ranks for a rank-{r} tensor (got {rankArgs.Length}); use hosvd(X) for the full decomposition"
                    else
                        rankArgs |> List.fold (fun acc rE ->
                            acc |> Result.bind (fun rs ->
                                staticInt ctx.Statics "hosvd rank" rE |> Result.map (fun rk -> rs @ [rk])))
                            (Ok [])
                ranksRes |> Result.bind (fun ranks ->
                    if List.exists2 (fun rk ik -> rk < 1 || rk > ik) ranks dims then
                        Error (sprintf "hosvd: each truncation rank must be in 1..I_k (dims %A, ranks %A)" dims ranks)
                    else
                        // Per-mode helpers. The eigh fingerprint matches the m.eigh arm exactly, so hosvd and user eigh
                        // calls of the same shape share one generated function.
                        let gramNames =
                            [ for mode in 0 .. r - 1 ->
                                ensureT st (fingerprint "gram" (box (dims, mode))) (fun nm -> gramDecl nm dims mode) ]
                        let eighNames =
                            [ for mode in 0 .. r - 1 ->
                                ensureT st (fingerprint "eigh" (box (dims.[mode], defaultSweeps))) (fun nm -> eighDecl nm dims.[mode] defaultSweeps) ]
                        // Successive core shapes: mode n contracts against dims-with-earlier-modes-already-truncated.
                        let mutable curDims = dims
                        let mptNames =
                            [ for mode in 0 .. r - 1 ->
                                let dcur = curDims
                                let nm = ensureT st (fingerprint "mpt" (box (dcur, mode, ranks.[mode])))
                                            (fun nm -> modeProdTDecl nm dcur mode ranks.[mode])
                                curDims <- dcur |> List.mapi (fun k d -> if k = mode then ranks.[mode] else d)
                                nm ]
                        ensure st (fingerprint "hosvd" (box (dims, ranks)))
                            (fun nm -> Ok (hosvdDecl nm dims ranks gramNames eighNames mptNames))
                        |> Result.map (fun nm -> syn (ExprApp (v nm, [xE])))))
    | "hosvd", _ -> Error "hosvd: expected hosvd(X) or hosvd(X, R1, ..., RN) with static ranks"
    | _ -> Error $"math: unknown op '{op}' (available: {opList})"

// Rewrite walker (same shape as MLElaborate.rewriteExpr)
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
    // Qualified math op: `alias.svd(...)` -> generated specialized function. Any alias-qualified call is claimed here so
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

/// `import math [as _]` -- the module this layer owns.
let private isMathImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["math"], _) -> true
    | _ -> false

/// Aliases bound to `math` in this decl list. Errors on a selective
/// `from math import ...`, which would reintroduce the global names the
/// module system is meant to remove.
let private mathAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["math"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "math") set)
            | DeclImport (["math"], ImportSelective _) ->
                Error "`math` supports only `import math [as <alias>]`; a selective `from math import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    mathAliasesOf decls |> Result.bind (fun aliases ->
    // Import-gated: with no `import math`, this pass is a strict no-op -- a user's own `svd`/`matmul` functions are never touched.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isMathImport)
        // Fold failures are the type-checker's to report; elaboration only
        // needs the successfully folded environment.
        match resolveStatics declsNoImport with
        | Error e -> Error $"math elaboration: static resolution failed: {e}"
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
                        // Stamp the user decl's span so every syn-built node
                        // (generated function bodies + synthesized call sites)
                        // attributes to this declaration's source line.
                        Blade.Ast.synthSpan <- d.Span
                        let mapped =
                            match d.Value with
                            | DeclFunction fd ->
                                // Annotated array PARAMS are shape witnesses
                                // inside the body.
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
                    // Generated functions are self-contained (no captures):
                    // splice them at the FRONT so every use site (top-level
                    // lets included) sees them defined.
                    let span = { StartLine = 0; StartCol = 0; EndLine = 0; EndCol = 0; File = None }
                    let gen = st.Decls |> List.map (fun fd -> { Value = DeclFunction fd; Span = span })
                    gen @ decls'))

/// Entry point: elaborate math ops across a program (after ML/PPL
/// elaboration, before Grad expansion).
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
        [ Blade.Diagnostics.mkError "BL5200" (Blade.Diagnostics.Codes.phaseOfCode "BL5200") Blade.Ast.synthSpan msg ])
