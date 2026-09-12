/// Spectra-module elaboration: FFT and arity-polymorphic polyspectra as compile-time source synthesis (the MathElaborate mold).
///
/// Surface (reachable only through `import spectra [as <alias>]`). The ops read the DECLARED shape (this pass runs before
/// type inference, and the generated transform bakes in the per-axis radix choice plus the twiddle and bit-reversal
/// tables), so an array argument must carry an annotation: a variable bound by an annotated `let` (module-level or
/// block-local), an annotated parameter, a call of a function with an annotated array return type, or an ascription.
///
///   sp.fft(x)             -- real x -> Array<Complex128 like Idx<n>> (unnormalized)
///   sp.ifft(X)            -- complex spectrum -> real signal (carries the 1/n)
///   sp.power(x)           -- |FFT(x)|^2 per bin (real)
///   sp.polyspec(x1,..,xk) -- order-k cross-polyspectrum, k = the CALL-SITE
///                            ARITY (2 cross-power, 3 bispectrum, 4 trispectrum):
///                            P(f_0..f_{k-2}) = X1(f_0)***X_{k-1}(f_{k-2}) * conj(Xk((Sum f) mod n))
///
/// For each distinct (op, resolved shape/order) the elaborator synthesizes ONE Blade function (`__spectra_1`, ...) via
/// Blade.Spectra.Decls; call sites rewrite to the generated names, deduped by fingerprint -- every op at one n shares a
/// single generated FFT.
///
/// Pipeline position: after ML/PPL/Math/Rand elaboration, BEFORE Grad expansion.
module Blade.Spectra.Elaborate

open Blade.Ast
open Blade.StaticEval
open Blade.Spectra.Decls

// Module-level context (MathElaborate's contract)

/// A name -> declared array shape map (the module-level tables).
type private Shapes = Map<string, TypeExpr * TypeExpr list>

/// The LEXICAL scope threaded through the walker: parameters and block-local lets. A binder is recorded even when it is NOT
/// array-annotated (as `None`), because it still SHADOWS an outer name -- without that, an unannotated local `let f = ...`
/// over a module-level annotated `f` would fall through to the module-level shape and silently transform at wrong extents.
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

/// Annotations may name a whole-array type alias (`type Field = Array<...>`;
/// `let mut q: Field = ...`) -- resolve top-level aliases (cycle-bounded) before matching for TyArray. TyBounded is
/// TRANSPARENT here, for diagnostic ORDER only (see MathElaborate.resolveTop): a bound on an aggregate is the checker's
/// rejection to make (BL4003); this pass must not pre-empt it with "no declared array shape".
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
            match b.Pattern, arrayAnnot aliases b.Type with
            | { Kind = PatternKind.PatVar name }, Some shape -> Map.add name shape acc
            | _ -> acc
        | _ -> acc) Map.empty

/// Top-level functions whose annotated RETURN type is an array of static extents: `flux(...) -> RF` is as good a shape
/// witness as an annotated let, so `sp.fft2(flux(u, 0.0, qr))` needs no staging binding.
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
        // Not a source alias: a provider axis path, registered by TypeEnv during type checking, so the extent comes
        // from the store's metadata instead.
        | None -> providerIndexExtent ctx.Statics name
    | _ -> None

/// Element-type classes the ops care about.
type private ElemClass = ElemFloat | ElemComplex | ElemOther

let private classifyElem (ty: TypeExpr) : ElemClass =
    match ty with
    | TyFloat64 | TyNamed (("Float" | "Float64" | "Double"), []) -> ElemFloat
    | TyComplex128 | TyNamed ("Complex128", []) -> ElemComplex
    | _ -> ElemOther

/// The steer appended to every "no declared shape here" rejection: the ops need (elem, extents) at compile time.
let private shapeSources =
    "spectra ops read the DECLARED shape at compile time (the generated transform bakes in the per-axis radix choice and the twiddle tables), so the argument must carry an annotation"

/// The declared shape AND element class of an op's array argument. The shape must be statically known (the generated
/// transform picks radix-2 vs table DFT per axis and bakes twiddle/bit-reversal tables into the source), but it need not
/// come from a module-level let. `scope` carries the lexical shapes in force here. The returned label is what messages quote.
let private arrayShape (ctx: Ctx) (scope: Scope) (what: string) (e: Expr) : Result<string * ElemClass * int list, string> =
    let finish (label: string) ((elem, idxs): TypeExpr * TypeExpr list) =
        let extents = idxs |> List.map (resolveExtent ctx)
        if extents |> List.forall Option.isSome then
            Ok (label, classifyElem elem, extents |> List.map Option.get)
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
    // An ascription: the universal escape hatch for a shape the elaborator cannot otherwise see.
    | ExprKind.ExprTyped (_, ty) ->
        match resolveTop ctx.Aliases 8 ty with
        | TyArray (elem, idxs) -> finish "the ascribed expression" (elem, idxs)
        | _ -> Error $"{what}: the ascription must name an array type (Array<... like Idx<...>, ...>)"
    // A call whose function has an annotated array return type. GUARD: in Blade arrays ARE functions, so `A(i)` and `f(x)`
    // are the same node -- exclude known array names so an index read stays on the error path.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar f }, _)
            when not (Map.containsKey f scope) && not (Map.containsKey f ctx.Arrays) ->
        match Map.tryFind f ctx.Funcs with
        | Some shape -> finish $"the result of '{f}'" shape
        | None -> Error $"{what}: '{f}' has no annotated array return type -- {shapeSources}"
    | _ ->
        Error $"{what}: the array argument must be a plain variable naming an annotated let, an annotated parameter, a call of a function with an annotated array return type, or an ascription `(expr : Array<... like Idx<...>, ...>)` -- {shapeSources}"

/// A real rank-1 signal of static length.
let private realSignal (ctx: Ctx) (scope: Scope) (what: string) (e: Expr) : Result<int, string> =
    arrayShape ctx scope what e |> Result.bind (fun (label, elem, dims) ->
        match elem, dims with
        | ElemFloat, [n] -> Ok n
        | ElemFloat, _ -> Error $"{what}: {label} must be rank-1 (Array<Float64 like Idx<n>>)"
        | _, _ -> Error $"{what}: {label} must have Float64 elements (a real signal)")

// Elaboration state (fingerprint-deduped generated decls)
type private ElabState = {
    mutable Counter: int
    mutable Made: Map<string, string>
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
        let n = $"__spectra_{st.Counter}"
        make n |> Result.map (fun decl ->
            st.Made <- Map.add key n st.Made
            st.Decls <- st.Decls @ [ decl ]
            n)

/// ensure for a TOTAL decl builder -- used to mint the shared per-n FFT before the ops that orchestrate it.
let private ensureT (st: ElabState) (key: string) (make: string -> FunctionDecl) : string =
    match ensure st key (fun n -> Ok (make n)) with
    | Ok n -> n
    | Error e -> failwith e // unreachable: make is total

let private ensureFft (st: ElabState) (n: int) : string =
    ensureT st (fingerprint "fft" (box n)) (fun nm -> fftDecl nm n)

// Op elaboration

let private opList = "fft, ifft, fft2, ifft2, power, polyspec"

/// Cap on the polyspectrum output (the zeros literal and the C++ initializer both scale with n^(k-1); steer before g++
/// meets a million-element initializer).
let private maxOutCells = 65536

let private elabOp (st: ElabState) (ctx: Ctx) (scope: Scope) (op: string) (args: Expr list) : Result<Expr, string> =
    match op, args with
    | "fft", [xE] ->
        realSignal ctx scope "fft" xE |> Result.bind (fun n ->
            Ok (ensureFft st n) |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), [xE]))))
    | "fft", _ -> Error "fft: expected fft(X) -- the unnormalized forward DFT of a real signal"
    | "ifft", [xE] ->
        arrayShape ctx scope "ifft" xE |> Result.bind (fun (label, elem, dims) ->
            match elem, dims with
            | ElemComplex, [n] ->
                ensure st (fingerprint "ifft" (box n)) (fun nm -> Ok (ifftDecl nm n))
                |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), [xE])))
            | ElemComplex, _ -> Error $"ifft: {label} must be rank-1 (Array<Complex128 like Idx<n>>)"
            | _, _ -> Error $"ifft: {label} must have Complex128 elements (ifft takes the complex spectrum; rebind the fft result with a full Array<Complex128 like Idx<n>> annotation first)")
    | "ifft", _ -> Error "ifft: expected ifft(X) -- real inverse synthesis of a complex spectrum (carries the 1/n)"
    | "fft2", [xE] ->
        arrayShape ctx scope "fft2" xE |> Result.bind (fun (label, elem, dims) ->
            match elem, dims with
            | ElemFloat, [r; c] ->
                if r * c > maxOutCells then
                    Error $"fft2: the {r}x{c} field has {r * c} cells; spectra outputs are capped at {maxOutCells} (the generated initializers scale with it) -- reduce the grid"
                else
                    ensure st (fingerprint "fft2" (box (r, c))) (fun nm -> Ok (fft2Decl nm r c))
                    |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), [xE])))
            | ElemFloat, _ -> Error $"fft2: {label} must be rank-2 (Array<Float64 like Idx<r>, Idx<c>>)"
            | _, _ -> Error $"fft2: {label} must have Float64 elements (a real 2-D field)")
    | "fft2", _ -> Error "fft2: expected fft2(X) -- the unnormalized forward 2-D DFT of a real rank-2 field"
    | "ifft2", [xE] ->
        arrayShape ctx scope "ifft2" xE |> Result.bind (fun (label, elem, dims) ->
            match elem, dims with
            | ElemComplex, [r; c] ->
                if r * c > maxOutCells then
                    Error $"ifft2: the {r}x{c} spectrum has {r * c} cells; spectra outputs are capped at {maxOutCells} (the generated initializers scale with it) -- reduce the grid"
                else
                    ensure st (fingerprint "ifft2" (box (r, c))) (fun nm -> Ok (ifft2Decl nm r c))
                    |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), [xE])))
            | ElemComplex, _ -> Error $"ifft2: {label} must be rank-2 (Array<Complex128 like Idx<r>, Idx<c>>)"
            | _, _ -> Error $"ifft2: {label} must have Complex128 elements (ifft2 takes the complex spectrum; rebind the fft2 result with a full Array<Complex128 like Idx<r>, Idx<c>> annotation first)")
    | "ifft2", _ -> Error "ifft2: expected ifft2(X) -- real inverse synthesis of a rank-2 complex spectrum (carries the 1/(r*c))"
    | "power", [xE] ->
        realSignal ctx scope "power" xE |> Result.bind (fun n ->
            let fftName = ensureFft st n
            ensure st (fingerprint "power" (box n)) (fun nm -> Ok (powerDecl nm n fftName))
            |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), [xE]))))
    | "power", _ -> Error "power: expected power(X) -- |FFT(X)|^2 per bin"
    | "polyspec", args when args.Length >= 2 && args.Length <= 4 ->
        let k = args.Length
        args
        |> List.fold (fun acc a ->
            acc |> Result.bind (fun ns ->
                realSignal ctx scope "polyspec" a |> Result.map (fun n -> ns @ [n]))) (Ok [])
        |> Result.bind (fun ns ->
            match List.distinct ns with
            | [n] ->
                let cells = pown n (k - 1)
                if cells > maxOutCells then
                    Error $"polyspec: the order-{k} output at n={n} has {cells} cells; the output is capped at {maxOutCells} (the generated initializers scale with it) -- reduce n or the order"
                else
                    let fftName = ensureFft st n
                    ensure st (fingerprint "polyspec" (box (n, k))) (fun nm -> Ok (polyspecDecl nm n k fftName))
                    |> Result.map (fun nm -> syn (ExprApp (syn (ExprVar nm), args)))
            | _ ->
                Error ($"""polyspec: all signals must share one static length (got {(ns |> List.map string |> String.concat ", ")})"""))
    | "polyspec", args ->
        Error $"polyspec: the argument count IS the polyspectrum order and must be 2..4 (got {args.Length}) -- k=2 cross-power, k=3 bispectrum, k=4 trispectrum; the generator is order-generic, raise the cap when needed"
    | _ -> Error $"spectra: unknown op '{op}' (available: {opList})"

// Rewrite walker (MathElaborate's shape)
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
    // Qualified spectra op: `alias.fft(...)` -> generated specialized function. Any alias-qualified call is claimed here
    // so an unknown op gets a steering error instead of an unbound-module type error.
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
    | ExprKind.ExprGramApply (l, rr, x) ->
        r l |> Result.bind (fun l' -> r rr |> Result.bind (fun r' -> r x |> Result.map (fun x' -> inheritSpan e (ExprGramApply (l', r', x')))))
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

let private isSpectraImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["spectra"], _) -> true
    | _ -> false

let private spectraAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["spectra"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "spectra") set)
            | DeclImport (["spectra"], ImportSelective _) ->
                Error "`spectra` supports only `import spectra [as <alias>]`; a selective `from spectra import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    spectraAliasesOf decls |> Result.bind (fun aliases ->
    // Import-gated: with no `import spectra`, this pass is a strict no-op -- a user's own `fft`/`power` functions untouched.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isSpectraImport)
        match resolveStatics declsNoImport with
        | Error e -> Error $"spectra elaboration: static resolution failed: {e}"
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
                        // Stamp the user decl's span so every syn-built node attributes to this declaration's source line.
                        Blade.Ast.synthSpan <- d.Span
                        let mapped =
                            match d.Value with
                            | DeclFunction fd ->
                                // Annotated array PARAMS are shape witnesses inside the body -- `sp.ifft2(qs)` on a
                                // `qs: CF` param needs no staging binding.
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
                    // Generated functions are self-contained: splice at the
                    // FRONT so every use site sees them defined (the shared
                    // fft precedes its power/polyspec callers by ensure order).
                    let span = { StartLine = 0; StartCol = 0; EndLine = 0; EndCol = 0; File = None }
                    let gen = st.Decls |> List.map (fun fd -> { Value = DeclFunction fd; Span = span })
                    gen @ decls'))

/// Entry point: elaborate spectra ops across a program (after ML/PPL/Math/
/// Rand elaboration, before Grad expansion).
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
        [ Blade.Diagnostics.mkError "BL5400" (Blade.Diagnostics.Codes.phaseOfCode "BL5400") Blade.Ast.synthSpan msg ])
