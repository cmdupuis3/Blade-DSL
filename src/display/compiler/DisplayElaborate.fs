/// `display`-module elaboration: rewrites `alias.emit(mime, data[, meta])` into the compiler-internal builtin
/// `__display_emit`, which the type-checker self-types (Bool) and both back ends turn into one display-frame line on
/// stdout (Blade.Display.Frame -- the wire format in Blade-REPL/docs/display-frames.md).
///
/// Surface (reachable only through `import display [as <alias>]`):
///
///   display.emit(mime, data)          -- one frame; `encoding` inferred from `mime`
///   display.emit(mime, data, meta)    -- same, with a user `meta` object merged after the generated `"id"`
///   display.emit_id(mime, id, data[, meta])
///                                     -- same frame, with `meta.id` taken from the RUNTIME `id` string instead
///                                        of the run's `<SessionTag><ordinal>`. A chart whose identity the
///                                        program chooses is what the live plot stream merges on.
///
/// `mime` and `meta` must be STRING LITERALS, the same discipline `rand` puts on its shapes: everything in a frame
/// except `data` is fixed at elaboration time, so the encoding rule, the JSON head and the meta tail are computed here
/// ONCE and the two runtimes only concatenate. `data` is an ordinary runtime `String` -- raw JSON text for a `+json`
/// mime, base64 for anything binary, and the checker rejects any other type.
///
/// This is the LOW-LEVEL emitter the plot package will wrap; it deliberately knows nothing about plots. A future
/// `plot.contourf(...)` elaborates to Blade source that serializes its figure to a String and calls this.
///
/// Pipeline position: last of the module elaborations, before Grad expansion -- a frame is a side effect on an already
/// elaborated payload, so nothing downstream needs to see the surface call.
module Blade.Display.Elaborate

open Blade.Ast

let private v (name: string) : Expr = syn (ExprVar name)
let private strLit (s: string) : Expr = syn (ExprLit (LitString s))
let private boolLit (b: bool) : Expr = syn (ExprLit (LitBool b))

/// A string literal argument, or an error naming what was wrong. Frames are
/// fixed-shape by design (see the module header), so this is a hard
/// requirement rather than a fallback to a runtime read.
let private literalString (what: string) (e: Expr) : Result<string, string> =
    match e.Kind with
    | ExprKind.ExprLit (LitString s) -> Ok s
    | _ -> Error $"{what} must be a string literal"

/// Elaborate one `display.<op>(...)` call.
let private elabOp (op: string) (args: Expr list) : Result<Expr, string> =
    /// Shared by `emit` and `emit_id`: validate the two elaboration-time
    /// literals (mime, meta) and hand the head / quoting flag / meta tail to
    /// `k`, which builds the internal call. `what` names the surface op so a
    /// bad `emit_id` does not report itself as `display.emit`.
    let withLiterals (what: string) (mimeE: Expr) (metaE: Expr option)
                     (k: string -> bool -> string -> Expr) =
        literalString $"{what}: the mime type" mimeE |> Result.bind (fun mime ->
        if not (Blade.Display.Frame.isMimeType mime) then
            Error ($"{what}: '{mime}' is not a mime type (expected type/subtype, e.g. \"application/vnd.plotly.v1+json\")")
        else
            let metaText =
                match metaE with
                | None -> Ok "{}"
                | Some e -> literalString $"{what}: the meta argument" e
            metaText |> Result.bind (fun metaJson ->
                match Blade.Display.Frame.metaTailOf metaJson with
                | None ->
                    Error ($"{what}: meta must be a JSON object literal like \"{{\\\"title\\\": \\\"my plot\\\"}}\" (got {metaJson})")
                | Some metaTail ->
                    Ok (k (Blade.Display.Frame.headFor mime)
                          (Blade.Display.Frame.quotedFor mime)
                          metaTail)))
    let build (mimeE: Expr) (dataE: Expr) (metaE: Expr option) =
        withLiterals "display.emit" mimeE metaE (fun head quoted metaTail ->
            syn (ExprApp (v "__display_emit",
                          [ strLit head; boolLit quoted; dataE; strLit metaTail ])))
    // `emit_id`'s ONLY difference from `emit`: the frame's `meta.id` is the
    // runtime `id` expression instead of the run's `<SessionTag><ordinal>`.
    // That is what gives a chart an identity the program chooses -- stable
    // across calls, across a session replay, and independent of how many other
    // plots ran first -- which is what the live plot stream merges on.
    let buildId (mimeE: Expr) (idE: Expr) (dataE: Expr) (metaE: Expr option) =
        withLiterals "display.emit_id" mimeE metaE (fun head quoted metaTail ->
            syn (ExprApp (v "__display_emit_id",
                          [ strLit head; boolLit quoted; idE; dataE; strLit metaTail ])))
    match op, args with
    | "emit", [mimeE; dataE] -> build mimeE dataE None
    | "emit", [mimeE; dataE; metaE] -> build mimeE dataE (Some metaE)
    | "emit", _ ->
        Error "display.emit: expected display.emit(mime, data) or display.emit(mime, data, meta)"
    | "emit_id", [mimeE; idE; dataE] -> buildId mimeE idE dataE None
    | "emit_id", [mimeE; idE; dataE; metaE] -> buildId mimeE idE dataE (Some metaE)
    | "emit_id", _ ->
        Error "display.emit_id: expected display.emit_id(mime, id, data) or display.emit_id(mime, id, data, meta)"
    // JSON serialization helpers (the plot package's substrate): a rank-1 or
    // rank-2 numeric array (`json_array`) or a numeric scalar (`json_num`)
    // rendered as JSON text. Formatting is the byte-parity 15-significant-
    // digit rule both lanes already share for prints (Interp.CppFormat
    // mirrors `cout << setprecision(15)`), so the differential gate covers
    // these exactly like emit itself.
    | "json_array", [arrE] ->
        Ok (syn (ExprApp (v "__display_json_array", [arrE])))
    | "json_array", _ ->
        Error "display.json_array: expected display.json_array(array) with a rank-1 or rank-2 numeric array"
    | "json_num", [numE] ->
        Ok (syn (ExprApp (v "__display_json_num", [numE])))
    | "json_num", _ ->
        Error "display.json_num: expected display.json_num(x) with a numeric scalar"
    // `json_string(s)`: a runtime String as a QUOTED, escaped JSON string --
    // the one safe way to put a user-supplied title or axis label inside a
    // figure object. Concatenating the quotes by hand is what plot.blade used
    // to do, and a title containing `"` or `\` made the whole frame
    // unparseable. Same escape table as a quoted frame payload
    // (Blade.Display.Frame.escape), so the differential gate covers it.
    | "json_string", [strE] ->
        Ok (syn (ExprApp (v "__display_json_string", [strE])))
    | "json_string", _ ->
        Error "display.json_string: expected display.json_string(s) with a String"
    // The unit-label probe: `display.unit_label(x)` becomes a STRING LITERAL
    // at typecheck time naming x's unit or quantity ("meter / second^2",
    // "speed", "" when bare) -- the typechecker is the one place the type is
    // known, and collapsing to a literal there means no runtime
    // representation exists in either lane. Feeds axis labels:
    //     plot.line(t, v, d.unit_label(v): ylabel)
    | "unit_label", [e] ->
        Ok (syn (ExprApp (v "__display_unit_label", [e])))
    | "unit_label", _ ->
        Error "display.unit_label: expected display.unit_label(x)"
    | _ -> Error $"display: unknown op '{op}' (available: emit, emit_id, json_array, json_num, json_string, unit_label)"

// Rewrite walker (same shape as RandElaborate.rewriteExpr; the catch-all wildcard is deliberately absent so an
// unhandled constructor is an FS0025 build warning rather than a qualified call surviving unrewritten).
let rec private rewriteExpr (aliases: Set<string>) (e: Expr) : Result<Expr, string> =
    let r = rewriteExpr aliases
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
        rList args |> Result.bind (fun args' -> elabOp op args')
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
    | ExprKind.ExprRecArray def ->
        rOpt (def.SeedArm |> Option.map snd) |> Result.bind (fun seedE ->
        rOpt def.Guard |> Result.bind (fun guardE ->
        r def.SliceExpr |> Result.map (fun slice' ->
            let seed' = Option.map2 (fun (sv, _) se -> (sv, se)) def.SeedArm seedE
            inheritSpan e (ExprRecArray { def with SeedArm = seed'; SliceExpr = slice'; Guard = guardE }))))
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
    | ExprKind.ExprSparse (vv, k) ->
        r vv |> Result.bind (fun v' -> r k |> Result.map (fun k' -> inheritSpan e (ExprSparse (v', k'))))
    | ExprKind.ExprIntersect (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> inheritSpan e (ExprIntersect (a', b'))))
    | ExprKind.ExprUnion (a, b) ->
        r a |> Result.bind (fun a' -> r b |> Result.map (fun b' -> inheritSpan e (ExprUnion (a', b'))))
    | ExprKind.ExprContains (a, vx) ->
        r a |> Result.bind (fun a' -> r vx |> Result.map (fun v' -> inheritSpan e (ExprContains (a', v'))))
    | ExprKind.ExprGroupBy (vv, g) ->
        r vv |> Result.bind (fun v' -> r g |> Result.map (fun g' -> inheritSpan e (ExprGroupBy (v', g'))))
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

let private isDisplayImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["display"], _) -> true
    | _ -> false

let private displayAliasesOf (decls: Located<Decl> list) : Result<Set<string>, string> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["display"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "display") set)
            | DeclImport (["display"], ImportSelective _) ->
                Error "`display` supports only `import display [as <alias>]`; a selective `from display import ...` would reintroduce global names"
            | _ -> Ok set))
        (Ok Set.empty)

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, string> =
    displayAliasesOf decls |> Result.bind (fun aliases ->
    // Import-gated: with no `import display`, this pass is a strict no-op.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isDisplayImport)
        declsNoImport |> List.fold (fun acc d ->
            acc |> Result.bind (fun out ->
                // Stamp the user decl's span so every syn-built node attributes to this declaration's source line.
                Blade.Ast.synthSpan <- d.Span
                let mapped =
                    match d.Value with
                    | DeclFunction fd ->
                        rewriteExpr aliases fd.Body
                        |> Result.map (fun b -> DeclFunction { fd with Body = b })
                    | DeclLet binding ->
                        rewriteExpr aliases binding.Value
                        |> Result.map (fun v' -> DeclLet { binding with Value = v' })
                    | DeclStatic binding ->
                        rewriteExpr aliases binding.Value
                        |> Result.map (fun v' -> DeclStatic { binding with Value = v' })
                    | other -> Ok other
                mapped |> Result.map (fun value -> out @ [{ d with Value = value }])))
            (Ok []))

let private expandStr (program: Program) : Result<Program, string> =
    program.Modules
    |> List.fold (fun acc m ->
        acc |> Result.bind (fun ms ->
            expandModule m.Decls |> Result.map (fun ds -> ms @ [{ m with Decls = ds }])))
        (Ok [])
    |> Result.map (fun ms -> { program with Modules = ms })

/// Boundary: string-errored internals -> coded diagnostics, on the ambient
/// synthSpan (stamped per-decl by expandStr, so a failure points at the
/// offending declaration). BL5700 is display's own slot in the elaborator band
/// -- reusing a sibling's code would render a display failure under that
/// sibling's phase name (Diagnostics.Codes.phaseOfCode).
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun msg ->
        [ Blade.Diagnostics.mkError "BL5700" (Blade.Diagnostics.Codes.phaseOfCode "BL5700") Blade.Ast.synthSpan msg ])
