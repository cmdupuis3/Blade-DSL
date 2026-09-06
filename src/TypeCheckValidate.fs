// Post-zonk structural validators: inline mixed-EnumIdx pre-validation,
// cross-module static-import rewriting, and the whole-module sweeps for
// application rank errors, misplaced provider writes, and group_keys
// escapes.
module Blade.TypeCheckValidate

open Blade.Ast
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.TypedAst
open Blade.Unify
open Blade.TypeEnv
open Blade.Zonk
open Blade.TypeCheckIde
open Blade.TypeLower
open Blade.TypeCheckSupport
open Blade.TypeCheckInfer

// After type checking, some IRTInfer nodes may remain in the typed AST.
// These are either (a) solved but unresolved (the substitution knows the answer
// but nobody called Resolve on that particular node), or (b) genuinely ambiguous
// (no constraints were generated). Zonking walks the entire typed AST, resolves
// every type through the substitution, and defaults any remaining unknowns to
// Float64. After zonking, no IRTInfer should remain in the program.

// 11c. Type expression pre-validation
// Walks every TypeExpr in a module looking for inline `EnumIdx<[...]>` with
// mixed value kinds (ints and strings in the same list). The aliased case
// `type X = EnumIdx<[1, "two"]>` is caught by registerTypeDecl's per-decl
// validation; this pre-pass covers the inline cases that the aliased check
// can't see, e.g. `let x: Array<EnumIdx<[1, "two"]> like ...> = ...`.

let internal isMixedEnumIdxValues (valuesExpr: Expr) : bool =
    match valuesExpr.Kind with
    | ExprKind.ExprArrayLit elems ->
        let isInt (e: Expr) =
            match e.Kind with
            | ExprKind.ExprLit (LitInt _) | ExprKind.ExprUnaryOp (OpNeg, { Kind = ExprKind.ExprLit (LitInt _) }) -> true
            | _ -> false
        let isString (e: Expr) = match e.Kind with ExprKind.ExprLit (LitString _) -> true | _ -> false
        let hasInt = elems |> List.exists isInt
        let hasString = elems |> List.exists isString
        hasInt && hasString
    | _ -> false

let rec internal walkTypeExprForMixedEnumIdx (ty: TypeExpr) : Expr list =
    let here =
        match ty with
        | TyEnumIdx v when isMixedEnumIdxValues v -> [v]
        | _ -> []
    let children =
        match ty with
        | TyArray (elem, idxs) ->
            walkTypeExprForMixedEnumIdx elem @ (idxs |> List.collect walkTypeExprForMixedEnumIdx)
        | TyAbstractArray (elem, _, _) -> walkTypeExprForMixedEnumIdx elem
        | TyFunc (args, ret) ->
            (args |> List.collect walkTypeExprForMixedEnumIdx) @ walkTypeExprForMixedEnumIdx ret
        | TyTuple ts -> ts |> List.collect walkTypeExprForMixedEnumIdx
        | TyDepIdx (outer, _, body) ->
            walkTypeExprForMixedEnumIdx outer @ walkTypeExprForMixedEnumIdx body
        | TyConstrained (inner, _) -> walkTypeExprForMixedEnumIdx inner
        | TyBounded (inner, _, _) -> walkTypeExprForMixedEnumIdx inner
        | TyPoly inner -> walkTypeExprForMixedEnumIdx inner
        | TyNamed (_, args) -> args |> List.collect walkTypeExprForMixedEnumIdx
        | TyEquivIdx (_, g, r) ->
            walkTypeExprForMixedEnumIdx g @ walkTypeExprForMixedEnumIdx r
        | _ -> []
    here @ children

/// Find all mixed-value TyEnumIdx inside a single declaration. Returns the
/// list of offending valuesExpr nodes (one per occurrence). The caller
/// converts each into a TypeError with the decl's span.
let collectMixedEnumIdxInDecl (decl: Decl) : Expr list =
    let walkOpt = function Some t -> walkTypeExprForMixedEnumIdx t | None -> []
    match decl with
    | DeclLet binding | DeclStatic binding ->
        walkOpt binding.Type
    | DeclFunction f ->
        (f.Params |> List.collect (fun p -> walkOpt p.Type))
        @ walkOpt f.ReturnType
    | DeclType (TyDeclAlias (_, _, body)) ->
        // The TyDeclAlias body when itself a TyEnumIdx is caught by
        // registerTypeDecl. We still walk deeper into the body for nested
        // inline forms (e.g., `type X = Array<EnumIdx<[1, "x"]> like ...>`).
        match body with
        | TyEnumIdx _ -> []  // already handled at the alias site
        | _ -> walkTypeExprForMixedEnumIdx body
    | DeclType (TyDeclStruct (_, _, fields, _, _)) ->
        fields |> List.collect (fun f -> walkTypeExprForMixedEnumIdx f.Type)
    | DeclType (TyDeclSum (_, _, variants)) ->
        variants |> List.collect (fun v -> walkOpt v.Data)
    | DeclType (TyDeclMutualGroup (members, _)) ->
        members |> List.collect (fun (_, mty) -> walkTypeExprForMixedEnumIdx mty)
    | DeclImpl impl ->
        impl.Methods |> List.collect (fun m ->
            (m.Params |> List.collect (fun p -> walkOpt p.Type))
            @ walkOpt m.ReturnType)
    | DeclInterface _ | DeclImport _ | DeclUnit _ -> []

// Cross-module static value visibility (checkModule's import-seeding
// pre-pass). KNOWN GAP being closed: `let static k = 5` in module M wasn't
// visible to module Main's own static resolution -- `let static x = M.k +
// 1` failed the fold assertion even though M.k is compile-time-known.
// Root cause: StaticEval.resolveStatics is a pure function of a module's
// OWN decls with no seed parameter, so it can't learn what a DIFFERENT
// module folded its statics to -- and even a seed wouldn't help, since
// `M.k` parses as ExprField(ExprVar "M", "k") and StaticEval.evalExpr's
// ExprField arm unconditionally errors on qualified access.
//
// The fix: substitute resolved cross-module static references with their
// literal values in a COPY of the decls handed to resolveStatics, before
// it runs. `M.k` and a bare `k` (selective import) both become e.g.
// `ExprLit (LitInt 3L)`. Only that copy is rewritten; the decls used for
// ordinary type-checking are untouched, so `let v = M.k` still goes
// through the ordinary qualified-value-access path.

/// Render a folded StaticValue back into surface `Expr` literal form, for
/// splicing into another module's decls ahead of static resolution. The
/// TypedExpr analog of this conversion (checkDecl's DeclStatic
/// "RESOLVED-VALUE SHORTCUT") runs one stage later, on already-typed trees;
/// this one runs pre-typing, directly on the surface AST.
let rec internal staticValueToImportExpr sp (v: StaticEval.StaticValue) : Expr =
    match v with
    | StaticEval.SVInt i -> mkExpr sp (ExprLit (LitInt i))
    | StaticEval.SVFloat f -> mkExpr sp (ExprLit (LitFloat f))
    | StaticEval.SVBool b -> mkExpr sp (ExprLit (LitBool b))
    | StaticEval.SVString s -> mkExpr sp (ExprLit (LitString s))
    | StaticEval.SVUnit -> mkExpr sp (ExprLit LitUnit)
    | StaticEval.SVTuple vs -> mkExpr sp (ExprTuple (vs |> List.map (staticValueToImportExpr sp)))
    | StaticEval.SVStruct (n, fs) ->
        mkExpr sp (ExprStruct (n, fs |> List.map (fun (fn, v) -> (fn, staticValueToImportExpr sp v)), None))

/// Substitute references to cross-module static values (keyed in `seed` as
/// "alias.name" for a qualified import, "name" for a selective one) with
/// their literal form. Shadow-aware for local binding forms (let/match/
/// lambda) that could plausibly appear inside a `let static` RHS or a
/// static function body, so a same-named local doesn't get clobbered.
/// Structured after StaticEval.collectFreeNames's case coverage (the set of
/// expression forms StaticEval.evalExpr actually supports; substitution
/// beyond that set is moot since resolveStatics would reject the form
/// regardless).
let rec internal rewriteImportedStaticRefs (seed: Map<string, StaticEval.StaticValue>) (expr: Expr) : Expr =
    if Map.isEmpty seed then expr else
    let go = rewriteImportedStaticRefs seed
    let goWithout (boundNames: string list) (e: Expr) =
        let seed' = boundNames |> List.fold (fun (s: Map<string, StaticEval.StaticValue>) n -> Map.remove n s) seed
        rewriteImportedStaticRefs seed' e
    match expr.Kind with
    | ExprKind.ExprVar name ->
        match Map.tryFind name seed with
        | Some sv -> staticValueToImportExpr expr.Span sv
        | None -> expr
    | ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, field) ->
        match Map.tryFind $"{alias}.{field}" seed with
        | Some sv -> staticValueToImportExpr expr.Span sv
        | None -> expr
    | ExprKind.ExprField (obj, field) -> inheritSpan expr (ExprField (go obj, field))
    | ExprKind.ExprBinOp (mode, op, l, r) -> inheritSpan expr (ExprBinOp (mode, op, go l, go r))
    | ExprKind.ExprUnaryOp (op, e) -> inheritSpan expr (ExprUnaryOp (op, go e))
    | ExprKind.ExprApp (f, args) -> inheritSpan expr (ExprApp (go f, args |> List.map go))
    | ExprKind.ExprIf (c, t, e) -> inheritSpan expr (ExprIf (go c, go t, go e))
    | ExprKind.ExprTuple es -> inheritSpan expr (ExprTuple (es |> List.map go))
    | ExprKind.ExprArrayLit es -> inheritSpan expr (ExprArrayLit (es |> List.map go))
    | ExprKind.ExprLet (binding, body) ->
        let bound = StaticEval.collectPatternBindings binding.Pattern |> Set.toList
        inheritSpan expr (ExprLet ({ binding with Value = go binding.Value }, goWithout bound body))
    | ExprKind.ExprMatch (scrut, cases) ->
        inheritSpan expr (ExprMatch (go scrut, cases |> List.map (fun c ->
            let bound = StaticEval.collectPatternBindings c.Pattern |> Set.toList
            { c with Guard = c.Guard |> Option.map (goWithout bound); Body = goWithout bound c.Body })))
    | ExprKind.ExprBlock (stmts, finalExpr) ->
        inheritSpan expr (ExprBlock (stmts |> List.map (rewriteImportedStaticRefsStmt seed), finalExpr |> Option.map go))
    | ExprKind.ExprStruct (name, fields, spread) -> inheritSpan expr (ExprStruct (name, fields |> List.map (fun (n, e) -> (n, go e)), spread |> Option.map go))
    | ExprKind.ExprTyped (e, t) -> inheritSpan expr (ExprTyped (go e, t))
    | ExprKind.ExprLambda (parms, whereClause, body) ->
        let bound = parms |> List.map (_.Name)
        inheritSpan expr (ExprLambda (parms, whereClause, goWithout bound body))
    | _ -> expr
and internal rewriteImportedStaticRefsStmt (seed: Map<string, StaticEval.StaticValue>) (stmt: Stmt) : Stmt =
    match stmt with
    | StmtSpanned (inner, span) -> StmtSpanned (rewriteImportedStaticRefsStmt seed inner, span)
    | StmtLet binding -> StmtLet { binding with Value = rewriteImportedStaticRefs seed binding.Value }
    | StmtAssign (lhs, op, rhs) -> StmtAssign (rewriteImportedStaticRefs seed lhs, op, rewriteImportedStaticRefs seed rhs)
    | StmtExpr e -> StmtExpr (rewriteImportedStaticRefs seed e)
    | StmtForIn (n, range, body) ->
        StmtForIn (n, rewriteImportedStaticRefs seed range, body |> List.map (rewriteImportedStaticRefsStmt seed))

/// Rewrite only the two decl shapes StaticEval.resolveStatics actually
/// consults (its Phase 1: `DeclStatic` and `DeclFunction ... IsStatic`) --
/// everything else (including plain `let`s and the DeclImport decls
/// themselves) passes through unchanged. A static function's own parameters
/// are excluded from the substitution seed (they're local, not the
/// cross-module reference).
let internal seedImportedStaticsIntoDecls (seed: Map<string, StaticEval.StaticValue>) (decls: Located<Decl> list) : Located<Decl> list =
    if Map.isEmpty seed then decls else
    decls |> List.map (fun locDecl ->
        match locDecl.Value with
        | DeclStatic binding ->
            { locDecl with Value = DeclStatic { binding with Value = rewriteImportedStaticRefs seed binding.Value } }
        | DeclFunction fd when fd.IsStatic ->
            let paramNames = fd.Params |> List.map (_.Name)
            let seed' = paramNames |> List.fold (fun (s: Map<string, StaticEval.StaticValue>) n -> Map.remove n s) seed
            { locDecl with Value = DeclFunction { fd with Body = rewriteImportedStaticRefs seed' fd.Body } }
        | _ -> locDecl)

/// Collect the StaticValues exported by this module's imports, keyed the
/// same way references to them actually parse: "alias.name" for a
/// qualified/aliased import (`M.k` parses as ExprField(ExprVar "M", "k")),
/// "name" for a selective import (`from M import k` brings in a bare
/// ExprVar "k"). Only modules already present in env.ModuleExports
/// (checked earlier in program order) contribute -- providers and
/// not-yet-checked modules are silently skipped, same as the existing
/// DeclImport handling in checkDecl.
let internal importedStaticSeed (env: TypeEnv) (decls: Located<Decl> list) : Map<string, StaticEval.StaticValue> =
    decls
    |> List.fold (fun acc locDecl ->
        match locDecl.Value with
        | DeclImport (qname, style) ->
            let fullName = String.concat "." qname
            match Map.tryFind fullName env.ModuleExports with
            | Some exports ->
                match style with
                | ImportQualified aliasOpt ->
                    let alias = aliasOpt |> Option.defaultValue (List.last qname)
                    exports.StaticValues
                    |> Map.fold (fun acc2 k v -> Map.add $"{alias}.{k}" v acc2) acc
                | ImportSelective names ->
                    names |> List.fold (fun acc2 n ->
                        match Map.tryFind n exports.StaticValues with
                        | Some v -> Map.add n v acc2
                        | None -> acc2) acc
            | None -> acc
        | _ -> acc) Map.empty

/// Post-unification sweep for direct-application RANK disagreements -- the
/// late half of the check whose eager half lives in `dispatchAppOrIndex`'s
/// FuncElem arm (both call `firstArgRankClash`, so the rule exists once).
/// The eager half only compares CLOSED types; an unannotated parameter is
/// not closed at its call site (Blade arithmetic is rank-polymorphic, so
/// `x * s` leaves `x` open and zonking defaults it to a SCALAR). That is how
///
///     function f(x, s: Float) -> Array<Float like IrrepsIdx<S>> = x * s
///     let r = f(xv, 2.0)          // xv : Array<Float64 like Idx<4>>
///
/// passed `blade check` and was refused by g++, which saw `double
/// f(double, double)` handed an `Array<double,1>` -- filling an array return
/// from a scalar body is a real, separately-tested feature, so the CALL
/// SITE is what is wrong. Running on the ZONKED module makes this total:
/// every parameter/argument carries the exact type codegen will emit.
let rec internal collectAppRankErrors (subst: Subst) (expr: TypedExpr) : CompileError list =
    let here =
        match expr.Kind with
        | TExprApp (tFunc, tArgs) ->
            match subst.Resolve tFunc.Type with
            | FuncElem (paramTys, _) ->
                match firstArgRankClash subst paramTys (tArgs |> List.map (_.Type)) with
                | Some (i, pr, ar, pTy, aTy) ->
                    let arg = List.item i tArgs
                    [ { Error = ArgRankMismatch (i + 1, pr, ar,
                                                 ppIRType (subst.Resolve pTy),
                                                 ppIRType (subst.Resolve aTy))
                        Span = arg.Span
                        Context = []
                        Code = None } ]
                | None ->
                // The ABSTRACT-PARAMETER twin, and the reason this sweep has
                // to carry it: a `T^k` parameter is STILL an open variable
                // after zonking -- polymorphic ids are deliberately preserved
                // for IR-phase monomorphization -- so the rank clash above
                // cannot see these, and neither could the eager seam when the
                // arguments were not yet determined there. What IS determined
                // by now is every ARGUMENT type, which is all this predicate
                // reads. See firstAbstractVarConflict.
                match firstAbstractVarConflict subst paramTys (tArgs |> List.map (_.Type)) with
                | Some (firstPos, conflictPos, firstTy, conflictTy) ->
                    let arg = List.item conflictPos tArgs
                    let calleeDesc =
                        match tFunc.Kind with
                        | TExprVar (name, _, _) -> $"'{name}'"
                        | _ -> "this function"
                    [ { Error = Other (abstractVarConflictMessage subst calleeDesc
                                                                  firstPos conflictPos firstTy conflictTy)
                        Span = arg.Span
                        Context = []
                        Code = None } ]
                | None -> []
            | _ -> []
        | _ -> []
    here @ (typedExprChildren expr |> List.collect (collectAppRankErrors subst))

/// Post-check sweep for MISPLACED provider writes.
///
/// `alias.write("path", A)` is a module-level DECLARATION form, not an
/// expression: `Lowering`'s decl loop intercepts the whole `let _ = c.write(..)`
/// binding into `IRModule.ProviderWrites` (a spec keyed by the binding's IRId)
/// and `genProviderWriteBinding` emits the flatten + the provider's writer for
/// it. Nothing intercepts a write written anywhere ELSE -- inside a block,
/// a function or lambda body, a loop body, an if/match branch -- so it used to
/// lower as an ordinary method call on the alias value and reach g++ as
/// `c.write(std::string("out.csv"), __v5)`, which fails with `'c' was not
/// declared in this scope`. Refuse it here instead, where the write's own span
/// is still in hand.
///
/// `nested` is false only for the top node of a module-level `let` RHS -- the
/// one position the decl loop actually intercepts. Every descent sets it, so
/// the rule needs no per-construct bookkeeping: anything that is not literally
/// that node is refused, `let static` included (the intercept arm matches
/// `TDeclLet` alone).
let rec internal collectMisplacedProviderWrites (subst: Subst) (nested: bool) (expr: TypedExpr) : CompileError list =
    let here =
        match expr.Kind with
        | TExprApp ({ Kind = TExprField (recv, "write", _) }, _) when nested ->
            (match recv.Kind, subst.Resolve recv.Type with
             | TExprVar (alias, _, _), IRTNamed pn when (Blade.ProviderRegistry.tryFind pn).IsSome ->
                 [ { Error = ProviderWriteModuleScope alias
                     Span = expr.Span
                     Context = []
                     Code = None } ]
             | _ -> [])
        | _ -> []
    here @ (typedExprChildren expr |> List.collect (collectMisplacedProviderWrites subst true))

/// Declaration entry points for the sweep above, tagged with whether the
/// expression sits in the one blessed position (a plain `let`'s RHS).
let internal declWriteRoots (decl: TypedDecl) : (bool * TypedExpr) list =
    let ofFunc (f: TypedFunctionDecl) = [(true, f.Body)]
    match decl with
    | TDeclLet b -> [(false, b.Value)]
    | TDeclStatic b -> [(true, b.Value)]
    | TDeclFunction f -> ofFunc f
    | TDeclImpl impl -> impl.Methods |> List.collect ofFunc
    | TDeclType _ | TDeclInterface _ | TDeclUnit _ | TDeclImport _ -> []

/// Post-check sweep for GROUP-KEYS ESCAPES (BL3017).
///
/// A `group_keys` result is NAME-KEYED, not a value. `genGroupKeysBinding`
/// (CodeGen.fs) puts the entire CSR structure into C++ locals suffixed off the
/// BINDING name -- `<name>__ngroups`, `<name>__offsets`, `<name>__perm` -- and
/// gives the binding itself a `void*` sentinel; `genGroupByBinding` recovers
/// the state by re-deriving those suffixed symbols from whatever cpp name the
/// grouping EXPRESSION resolves to. So the two ops are joined by a NAME, and
/// every indirection breaks the joint silently: `let gk2 = gk` emitted
/// `gk2__offsets`, a tuple round-trip emitted the same, and an untyped
/// function parameter zonked to a scalar and emitted `double g` alongside
/// `g__offsets`. All three died in g++, not in Blade.
///
/// The invariant is already ASSUMED elsewhere -- `sameGroupKeysBinding` in
/// inferMethodFor decides grouped co-iteration by comparing gk operands'
/// binding NAMES, which is only meaningful if a gk always IS its binding name.
/// This sweep is what makes it enforced.
///
/// `pos` is `None` in the two blessed positions (the direct RHS of a `let`
/// binding the call, and `group_by`'s grouping slot when it holds a plain
/// variable) and `Some phrase` everywhere else, where `phrase` completes
/// "... cannot be used <phrase>". Running on the ZONKED module makes this
/// total: IRTGroupKeys is minted only by `inferGroupKeys` and survives
/// zonking intact, so a resolved IRTGroupKeys anywhere else IS the escape.
/// A node that fires does not descend -- a gk-typed block or function body is
/// one mistake, not one per enclosing layer.
///
/// THE blessing for a `let` RHS, shared by the block walk and the declaration
/// roots so module-level and function-local `let`s cannot drift apart. Blessed
/// only when the RHS IS the call and the pattern is a bare name: `let gk2 = gk`
/// is an alias, and the alias is exactly the bug.
let internal groupKeysLetRhs (b: TypedBinding) : string option * TypedExpr =
    match b.Value.Kind with
    // `segments(A)` is a grouping on the same name-keyed terms as group_keys
    // (docs/plans/structural/07 §3.2): its locals are suffixed off the binding.
    | TExprGroupKeys _ | TExprSegments _ when List.isEmpty b.SubBindings -> (None, b.Value)
    | _ -> (Some "as another binding's value", b.Value)

let rec internal collectGroupKeysEscapes (subst: Subst) (pos: string option) (expr: TypedExpr) : CompileError list =
    let isGk (e: TypedExpr) = (subst.Resolve e.Type).IsIRTGroupKeys
    let describe (e: TypedExpr) =
        match e.Kind with
        | TExprGroupKeys _ -> "a `group_keys(...)` call"
        | TExprSegments _ -> "a `segments(...)` call"
        | TExprVar (n, _, _) -> $"the group_keys binding '{n}'"
        | _ -> "a group_keys result"
    // A block is TRANSPARENT here: its type is its final expression's, and its
    // span covers the whole body, so firing on the block would point the
    // caret at the innocent first statement. Descend and let the final
    // expression carry the enclosing position instead.
    let isBlock = expr.Kind.IsTExprBlock
    match pos with
    | Some phrase when isGk expr && not isBlock ->
        [ { Error = GroupKeysEscapes (describe expr, phrase); Span = expr.Span; Context = []; Code = None } ]
    | _ ->
    let elsewhere = "in this position"
    let kids : (string option * TypedExpr) list =
        match expr.Kind with
        | TExprGroupBy (values, gk) ->
            // The grouping slot takes the BINDING NAME only -- an inline
            // `group_by(v, group_keys(k))` has no locals to suffix off.
            let gkPos =
                match gk.Kind with
                | TExprVar _ -> None
                | _ -> Some "inline as `group_by`'s grouping argument"
            [ (Some elsewhere, values); (gkPos, gk) ]
        // The grouping ACCESSORS are blessed slots too, on the same terms as
        // group_by's: they read the CSR tables through the binding name, so a
        // bare name is fine and anything else has no locals to suffix off.
        // Without these arms the default below would fire BL3017 on the very
        // spellings these accessors exist to provide.
        | TExprGroupBucket gk ->
            let gkPos =
                match gk.Kind with
                | TExprVar _ -> None
                | _ -> Some "inline as `group_bucket`'s argument"
            [ (gkPos, gk) ]
        // `extents` is only a grouping accessor when its operand IS a grouping;
        // over an array it is the ordinary extent query, which must keep
        // descending normally.
        | TExprExtents a when isGk a ->
            let gkPos =
                match a.Kind with
                | TExprVar _ -> None
                | _ -> Some "inline as `extents`' argument"
            [ (gkPos, a) ]
        | TExprTuple es -> es |> List.map (fun e -> (Some "as a tuple element", e))
        | TExprArrayLit (elems, _) -> elems |> List.map (fun e -> (Some "as an array element", e))
        | TExprStruct (_, fields) -> fields |> List.map (fun (_, e) -> (Some "as a struct field", e))
        | TExprApp (f, args) ->
            (Some elsewhere, f) :: (args |> List.map (fun a -> (Some "as a function argument", a)))
        | TExprBlock (stmts, final) ->
            let rec ofStmt (s: TypedStmt) : (string option * TypedExpr) list =
                match s with
                | TStmtLet b -> [groupKeysLetRhs b]
                | TStmtAssign (l, r) -> [(Some elsewhere, l); (Some elsewhere, r)]
                | TStmtExpr e -> [(Some elsewhere, e)]
                | TStmtForIn (_, _, lo, hi, body) ->
                    (Some elsewhere, lo) :: (Some elsewhere, hi) :: (body |> List.collect ofStmt)
            // The final expression inherits the block's own position, so a
            // gk returned out of a function body reads "as a function's
            // return value" rather than the generic block phrasing.
            let finalPos = pos |> Option.defaultValue "as a block's result value"
            (stmts |> List.collect ofStmt)
            @ (final |> Option.toList |> List.map (fun e -> (Some finalPos, e)))
        | _ -> typedExprChildren expr |> List.map (fun e -> (Some elsewhere, e))
    kids |> List.collect (fun (p, e) -> collectGroupKeysEscapes subst p e)

/// Declaration entry points for the sweep above. Same blessing as the block
/// case: a module-level `let` may hold the call itself and nothing else, and a
/// function body is a returning position (BL7001's "no rule for IRGroupKeys in
/// expression position" was the old, misleadingly backend-flavoured verdict).
let internal declGroupKeysRoots (decl: TypedDecl) : (string option * TypedExpr) list =
    let ofFunc (f: TypedFunctionDecl) = [(Some "as a function's return value", f.Body)]
    match decl with
    | TDeclLet b | TDeclStatic b -> [groupKeysLetRhs b]
    | TDeclFunction f -> ofFunc f
    | TDeclImpl impl -> impl.Methods |> List.collect ofFunc
    | TDeclType _ | TDeclInterface _ | TDeclUnit _ | TDeclImport _ -> []

/// Every expression a zonked declaration carries, for the sweep above.
let internal declExprs (decl: TypedDecl) : TypedExpr list =
    let ofFunc (f: TypedFunctionDecl) = [f.Body]
    match decl with
    | TDeclLet b | TDeclStatic b -> [b.Value]
    | TDeclFunction f -> ofFunc f
    | TDeclImpl impl -> impl.Methods |> List.collect ofFunc
    | TDeclType _ | TDeclInterface _ | TDeclUnit _ | TDeclImport _ -> []

