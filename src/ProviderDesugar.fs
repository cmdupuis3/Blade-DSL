/// ProviderDesugar -- the icechunk `checkout` desugar (raw AST -> raw AST).
///
/// Icechunk is the first provider whose module-creating verb has a DERIVED
/// binding as its receiver rather than the imported alias:
///
///     import icechunk as ic
///     let repo = ic.load("data/weather.icechunk")   // repo handle
///     let ck1  = repo.checkout("main")              // bare
///     let ck2  = repo.checkout("v1.0", ic.tag)      // ref-unit marker
///
/// Teaching that shape to every phase individually would plant provider-
/// specific knowledge in four base-compiler passes (typecheck's load
/// recognition, Lowering's `tryInvokeProvider`, StaticEval's raw-AST
/// `providerRoots` scan, and two Ide walkers), because the binding -> path
/// association is carried by three independently built maps. Instead this
/// single pass rewrites checkout into the LOAD SHAPE all three already
/// understand, keyed on the canonical key of
/// docs/plans/plan-icechunk-provider.md section 3.1:
///
///     let ck2 = repo.checkout("v1.0", ic.tag)
///         =>  let ck2 = ic.load("data/weather.icechunk@tag:v1.0")
///
/// `<kind>` is read off the marker (`branch` / `tag` / `snapshot`), or `?`
/// for the bare form, which the provider resolves by cross-namespace
/// uniqueness at `LoadAsModule` time. After the rewrite every downstream
/// phase sees a shape it already handles -- ZERO new arms anywhere.
///
/// Three properties this pass is responsible for:
///
///   * SPAN FIDELITY. The synthesized `ExprApp`/`ExprField`/`ExprVar`/
///     `ExprLit` nodes wear the ORIGINAL checkout call's spans, so every
///     later diagnostic underlines the text the user actually wrote and not
///     a synthesized key that appears in no source file.
///   * IDEMPOTENCE. A rewritten binding loads a path ending in
///     `@<kind>:<name>`, which the repo-binding scan declines (see
///     `isCheckoutKey`), so a second application is a no-op. (It is applied at
///     more than one funnel -- see the wiring note below.) The test is that
///     suffix and not a bare `@`, because repo PATHS contain '@' too.
///   * A NO-OP FAST PATH. A module with no `import icechunk` is returned
///     unchanged and reference-equal, so the pass costs one decl scan on
///     every program that does not use the provider.
///
/// Deliberately CONCRETE to icechunk, per the plan: no generic
/// "derived-binding verb" registry slot until a second provider wants one.
/// It therefore depends on nothing but `Blade.Ast` and `Blade.Diagnostics`
/// and can compile very early -- which it must, since its consumers are
/// TypeCheck.fs, Lowering.fs and Ide.fs.
///
/// WIRING (all three must stay wired; see the plan's section 13 risk note --
/// a consumer that grabs the AST upstream of this pass sees undesugared
/// checkouts and fails obliquely, and StaticEval's miss is SILENT):
///
///   * `Blade.TypeCheck.typeCheck` runs `expand` FIRST, ahead of Unfold --
///     which is itself the first `resolveStatics` consumer -- so typecheck,
///     every domain elaborator's own static fold, and `checkModule`'s
///     `providerRoots` scan all see the load shape.
///   * `Blade.Lowering.lowerTypedProgram` runs `desugarOrIdentity` over its
///     `rawProgram`, because callers hand lowering the program they parsed,
///     not the one typeCheck rewrote internally; Phase 0's `resolveStatics`
///     reads that raw decl list.
///   * `Blade.Ide.ideCheckSourceWith` runs it on the parsed entry buffer,
///     which is what the editor's provider/provenance walkers read.
module Blade.ProviderDesugar

open Blade.Ast

/// The one provider module name this pass knows. Matched as a literal rather
/// than through `ProviderRegistry`: the pass has to run before typecheck
/// installs the registry, and its knowledge is icechunk-specific anyway.
let private icechunkModule = "icechunk"

/// The ref-namespace markers. `ic.branch` / `ic.tag` / `ic.snapshot` are
/// unit-carrying marker constants at the surface (plan section 3), but the
/// desugar reads them SYNTACTICALLY -- it runs before there is a unit
/// environment to consult, and the field name is the whole content.
let private refMarkers = set [ "branch"; "tag"; "snapshot" ]

/// The kind slot of a bare (unmarked) checkout: the provider resolves it by
/// demanding uniqueness across branches, tags and plausible snapshot ids.
let private bareKind = "?"

/// The canonical key: `"<repoPath>@<kind>:<name>"`. The one string that
/// enters every path-keyed carrier (`ProviderPaths`, `ProviderRoots`, the
/// fold/axis caches, `ProviderReadSpec.FilePath`); `IcechunkProvider` parses
/// it back internally. Public so the provider and its tests spell it once.
let canonicalKey (repoPath: string) (kind: string) (refName: string) : string =
    repoPath + "@" + kind + ":" + refName

/// The kind tokens a canonical key's suffix may name -- exactly the tokens
/// this pass itself writes, since `rewriteValue` passes a marker name through
/// as the kind. Stated from the two sets above rather than re-listed, so a
/// namespace added there cannot be forgotten here.
let private refKindTokens = Set.add bareKind refMarkers

/// Does this path already carry a ref suffix? A load whose path names a
/// checkout is not a repo handle, so `ck.checkout(...)` on it is left alone
/// (and this is what makes the pass idempotent).
///
/// The test is the LAST '@' followed by `<known-kind>:`, not merely "contains
/// an '@'" -- a repo path may legitimately contain one (a Windows profile
/// directory, `C:/Users/o@corp/data/w.icechunk`), and treating that as a
/// checkout key left the user's `repo.checkout("main")` unrewritten, dying
/// downstream as a missing-member error about a `checkout` field.
///
/// MIRRORS `IcechunkProvider.hasRefSuffix` (src/providers/IcechunkProvider.fs):
/// this decides whether a load is a repo handle, that decides whether the key
/// the rewrite produced splits back into path + refspec. Change both together.
let private isCheckoutKey (path: string) =
    let at = path.LastIndexOf '@'
    if at < 0 then false
    else
        let refspec = path.Substring(at + 1)
        match refspec.IndexOf ':' with
        | colon when colon > 0 -> Set.contains (refspec.Substring(0, colon)) refKindTokens
        | _ -> false

// Diagnostics

/// Wrong-shaped `checkout` on a receiver we DID recognize as a repo handle.
/// Reported as BL3007 ("invalid builtin argument"), whose registry entry
/// already covers "the provider read/write forms" -- a new code would need
/// its protocol/surface.json + protocol/data/diagnostics.json companions and
/// buys nothing here.
///
/// A checkout on a receiver we did NOT record is deliberately NOT an error:
/// it is left unrewritten and fails as the ordinary missing-member error it
/// is, which is the right message for `someTuple.checkout(...)`.
let private checkoutShapeError (span: Span) (repo: string) (alias: string)
                               (args: Expr list) : Blade.Diagnostics.Diagnostic =
    let isStringLit (e: Expr) =
        match e.Kind with
        | ExprKind.ExprLit (LitString _) -> true
        | _ -> false
    let detail =
        match args with
        | [] -> "no arguments"
        | [ _ ] -> "one argument that is not a string literal"
        | [ a; _ ] when not (isStringLit a) -> "a first argument that is not a string literal"
        | [ _; _ ] ->
            $"a second argument that is not one of the ref markers `{alias}.branch`, `{alias}.tag`, `{alias}.snapshot`"
        | many -> $"{many.Length} arguments"
    let msg =
        $"`{repo}.checkout(...)` got {detail}. A checkout takes a string LITERAL ref name, "
        + $"optionally followed by one ref marker: `{repo}.checkout(\"main\")`, "
        + $"`{repo}.checkout(\"v1.0\", {alias}.tag)`, `{repo}.checkout(\"main\", {alias}.branch)` "
        + $"or `{repo}.checkout(\"<id>\", {alias}.snapshot)`. The store's metadata is resolved at "
        + "compile time, so a computed ref name cannot be served."
    Blade.Diagnostics.mkError "BL3007" (Blade.Diagnostics.Codes.phaseOfCode "BL3007") span msg

/// A well-shaped `checkout` on a recognized repo handle sitting somewhere the
/// rewrite cannot reach -- a function body, a block, an argument of another
/// call. Same code as the shape refusal (BL3007 already covers "the provider
/// read/write forms"), different complaint: the CALL is fine, the POSITION is
/// not.
///
/// Detection only. `rewriteValue` deliberately serves exactly one position --
/// the only one the three binding -> path carriers look at (see its own
/// comment) -- so everywhere else the checkout used to be left standing and
/// die downstream as a missing-member error about a `checkout` field the
/// provider never had.
let private checkoutPositionError (span: Span) (repo: string) : Blade.Diagnostics.Diagnostic =
    let msg =
        $"`{repo}.checkout(...)` is not in a position a checkout can be resolved from. "
        + "A checkout must be a module-level binding: the store's metadata is resolved at "
        + "compile time, so the compiler has to see it at the top level of the module -- not "
        + "inside a function body, a block, or another expression. Bind it once at module "
        + $"level (`let ck = {repo}.checkout(\"main\")`) and use that binding here."
    Blade.Diagnostics.mkError "BL3007" (Blade.Diagnostics.Codes.phaseOfCode "BL3007") span msg

// Node construction

/// Build `alias.load("<key>")` wearing the checkout call's own spans:
/// the whole application keeps the checkout call's span, `alias.load` keeps
/// `repo.checkout`'s, the alias keeps `repo`'s, and the key literal keeps the
/// ref-name literal's -- so a bad-ref diagnostic underlines the string the
/// user wrote rather than a key that exists in no source file.
let private loadNode (appSpan: Span) (fieldSpan: Span) (recvSpan: Span) (litSpan: Span)
                     (alias: string) (key: string) : Expr =
    mkExpr appSpan
        (ExprKind.ExprApp (
            mkExpr fieldSpan
                (ExprKind.ExprField (mkExpr recvSpan (ExprKind.ExprVar alias), "load")),
            [ mkExpr litSpan (ExprKind.ExprLit (LitString key)) ]))

// The module pass

/// Every name a pattern binds. Local (and tiny) on purpose: the shared
/// helpers live in StaticEval / TypeCheckSupport, both far later in compile
/// order than this pass may sit.
let rec private patternBoundNames (p: Pattern) : string list =
    match p.Kind with
    | PatternKind.PatVar n -> [ n ]
    | PatternKind.PatTuple ps -> ps |> List.collect patternBoundNames
    | PatternKind.PatCons (h, t) -> patternBoundNames h @ patternBoundNames t
    | PatternKind.PatStruct (_, flds) -> flds |> List.collect (snd >> patternBoundNames)
    | PatternKind.PatVariant (_, Some inner)
    | PatternKind.PatGuarded (inner, _)
    | PatternKind.PatTyped (inner, _) -> patternBoundNames inner
    | _ -> []

/// Drop from a repo-handle name set every name a pattern rebinds. The
/// position scan's shadowing rule, and the same claim `recordRepo` makes at
/// module level: a name that has been rebound is not the repo handle any more.
let private shadow (handles: Set<string>) (p: Pattern) : Set<string> =
    patternBoundNames p |> List.fold (fun (s: Set<string>) n -> Set.remove n s) handles

/// Every sub-expression a node holds, for the detection walk below. NOT
/// exhaustive by type -- the trailing arm covers the leaves and anything a
/// later grammar addition introduces -- and the binder-carrying nodes (lambda,
/// let, match, block, recursive array) are deliberately absent, because
/// `scanExpr` has to remove the names they bind before descending.
///
/// A catch-all is admissible here and would not be in `rewriteValue`: this
/// walk only ever ADDS a diagnostic, so a node it fails to reach costs a
/// message the user would not have got before this pass existed -- never a
/// rewrite that should not have happened.
let private childrenOf (e: Expr) : Expr list =
    match e.Kind with
    | ExprKind.ExprBinOp (_, _, a, b) -> [ a; b ]
    | ExprKind.ExprUnaryOp (_, a) -> [ a ]
    | ExprKind.ExprApp (f, args) -> f :: args
    | ExprKind.ExprTupleIndex (t, i) -> [ t; i ]
    | ExprKind.ExprField (r, _) -> [ r ]
    | ExprKind.ExprIf (c, t, f) -> [ c; t; f ]
    | ExprKind.ExprTuple es
    | ExprKind.ExprArrayLit es
    | ExprKind.ExprMethodFor es
    | ExprKind.ExprZip es
    | ExprKind.ExprStack es
    | ExprKind.ExprSequence es
    | ExprKind.ExprGroupKeys es -> es
    | ExprKind.ExprAlign (es, _) -> es
    | ExprKind.ExprJoin (es, _) -> es
    | ExprKind.ExprDotDot (a, b) -> [ a; b ]
    | ExprKind.ExprHalo (_, a) -> [ a ]
    | ExprKind.ExprObjectFor a
    | ExprKind.ExprPure a
    | ExprKind.ExprCompute a
    | ExprKind.ExprRead a
    | ExprKind.ExprRank a
    | ExprKind.ExprUnique a
    | ExprKind.ExprGroupBucket a
    | ExprKind.ExprExtents a
    | ExprKind.ExprStatic a -> [ a ]
    | ExprKind.ExprReynolds (a, _) -> [ a ]
    | ExprKind.ExprTyped (a, _) -> [ a ]
    | ExprKind.ExprPartialApp (_, a, _) -> [ a ]
    | ExprKind.ExprTranspose (a, _, _) -> [ a ]
    | ExprKind.ExprDecompact (a, _) -> [ a ]
    | ExprKind.ExprGuard (a, b)
    | ExprKind.ExprReplicate (a, b)
    | ExprKind.ExprMask (a, b)
    | ExprKind.ExprCompound (a, b)
    | ExprKind.ExprSparse (a, b)
    | ExprKind.ExprIntersect (a, b)
    | ExprKind.ExprUnion (a, b)
    | ExprKind.ExprContains (a, b)
    | ExprKind.ExprGroupBy (a, b)
    | ExprKind.ExprSort (a, b)
    | ExprKind.ExprGram (a, b)
    | ExprKind.ExprAssign (a, b) -> [ a; b ]
    | ExprKind.ExprGramApply (a, b, x) -> [ a; b; x ]
    | ExprKind.ExprReduce (a, k, init, axes) ->
        [ a; k ] @ Option.toList init @ Option.toList axes
    | ExprKind.ExprStruct (_, flds, spread) ->
        (flds |> List.map snd) @ Option.toList spread
    | ExprKind.ExprFor (src, _, kernel) ->
        let sourceKids =
            match src with
            | ForArrays (arrays, inClause) -> arrays @ Option.toList inClause
            | ForKernel k -> [ k ]
        sourceKids @ Option.toList kernel
    | _ -> []

/// `import icechunk` / `import icechunk as ic` aliases declared in a module.
/// The parser leaves `ModuleDecl.Imports` empty and emits every import as a
/// `DeclImport` (Parser.fs, `Imports = []`), so scanning the decls is
/// complete -- same idiom as StaticEval's `providerAliases`.
let private icechunkAliases (decls: Located<Decl> list) : Set<string> =
    decls |> List.fold (fun acc d ->
        match d.Value with
        | DeclImport ([ pname ], ImportQualified aliasOpt) when pname = icechunkModule ->
            Set.add (aliasOpt |> Option.defaultValue pname) acc
        | _ -> acc) Set.empty

let private desugarModule (diags: ResizeArray<Blade.Diagnostics.Diagnostic>)
                          (m: ModuleDecl) : ModuleDecl =
    let aliases = icechunkAliases m.Decls
    // Fast path: no icechunk import, nothing to look at -- return the SAME
    // object so `expand` can hand a whole untouched program straight back.
    if Set.isEmpty aliases then m else
    // binding name -> (icechunk alias, repo path). Built LEFT TO RIGHT as the
    // walk proceeds, so a checkout only ever resolves against a repo bound
    // ABOVE it -- which is also the only ordering Blade's scoping admits.
    // Reference cells rather than `let mutable`: both are read and written
    // from the local functions below, which are closures.
    let repos : Map<string, string * string> ref = ref Map.empty
    let changed = ref false

    /// The rewrite itself, at a top-level binding's right-hand side. That is
    /// the ONLY position worth rewriting: all three binding -> path carriers
    /// (TypeCheckInfer's load-recognition arm in `checkDecl`, StaticEval's
    /// `providerRoots`, Ide's `collectProviderStores`) match a module-level
    /// binding's VALUE, so a load synthesized anywhere else would be
    /// recognized by nobody.
    let rewriteValue (b: Binding) : Binding =
        match b.Value.Kind with
        | ExprKind.ExprApp (({ Kind = ExprKind.ExprField (({ Kind = ExprKind.ExprVar recv } as recvE), "checkout") } as headE), args)
            when Map.containsKey recv repos.Value ->
            let (alias, repoPath) = repos.Value.[recv]
            let rewritten (nameE: Expr) (kind: string) (refName: string) =
                changed.Value <- true
                { b with Value =
                            loadNode b.Value.Span headE.Span recvE.Span nameE.Span
                                     alias (canonicalKey repoPath kind refName) }
            match args with
            // `repo.checkout("main")` -- bare, resolved across namespaces.
            | [ ({ Kind = ExprKind.ExprLit (LitString refName) } as nameE) ] ->
                rewritten nameE bareKind refName
            // `repo.checkout("v1.0", ic.tag)` -- the marker names the namespace.
            //
            // The marker may come off ANY icechunk alias in scope, not only the
            // one whose `load` produced this handle: `import icechunk as ic`
            // and `import icechunk as ice` name one module twice, so `ice.tag`
            // is literally the same marker constant as `ic.tag`. Testing
            // `markerAlias = alias` made the second spelling fall through to
            // the not-a-marker refusal below. A marker off a NON-icechunk
            // alias (`z.tag`) is still no marker, which the set test still
            // catches.
            | [ ({ Kind = ExprKind.ExprLit (LitString refName) } as nameE)
                { Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar markerAlias }, marker) } ]
                when Set.contains markerAlias aliases && Set.contains marker refMarkers ->
                rewritten nameE marker refName
            // Anything else on a RECOGNIZED repo handle is loud: silently
            // leaving it would surface as a baffling missing-member error
            // about a `checkout` field that the provider never had.
            | _ ->
                diags.Add(checkoutShapeError b.Value.Span recv alias args)
                b
        | _ -> b

    /// Record (or drop) a repo handle for the names this binding binds.
    /// Dropping matters: a later `let repo = 5` must stop `repo.checkout(...)`
    /// from rewriting, and so must a destructuring that rebinds the name.
    let recordRepo (b: Binding) =
        for n in patternBoundNames b.Pattern do
            repos.Value <- Map.remove n repos.Value
        match b.Pattern.Kind, b.Value.Kind with
        | PatternKind.PatVar root,
          ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, "load") },
                            [ { Kind = ExprKind.ExprLit (LitString path) } ])
            when Set.contains alias aliases && not (isCheckoutKey path) ->
            repos.Value <- Map.add root (alias, path) repos.Value
        | _ -> ()

    let decls' =
        m.Decls |> List.map (fun (d: Located<Decl>) ->
            match d.Value with
            | DeclLet b ->
                let b' = rewriteValue b
                recordRepo b'
                if LanguagePrimitives.PhysicalEquality b' b then d
                else { d with Value = DeclLet b' }
            | DeclStatic b ->
                let b' = rewriteValue b
                recordRepo b'
                if LanguagePrimitives.PhysicalEquality b' b then d
                else { d with Value = DeclStatic b' }
            | DeclFunction fd ->
                // A function shadows the value namespace for its name.
                repos.Value <- Map.remove fd.Name repos.Value
                d
            | _ -> d)

    // -----------------------------------------------------------------
    // Detection-only second walk: a checkout OUTSIDE the blessed position.
    // -----------------------------------------------------------------
    // Finds a syntactically perfect `repo.checkout("main")` sitting somewhere
    // the rewrite cannot reach, and names the problem at the checkout's own
    // span. It NEVER rewrites -- a load synthesized in a function body would
    // be recognized by none of the three binding -> path carriers.
    let rec scanExpr (handles: Set<string>) (e: Expr) : unit =
        match e.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar recv }, "checkout") }, args)
            when Set.contains recv handles ->
            diags.Add(checkoutPositionError e.Span recv)
            // Still descend: `f(a.checkout("x"), b.checkout("y"))` is two
            // misplaced checkouts and one run should name both.
            args |> List.iter (scanExpr handles)
        | ExprKind.ExprLambda (parms, _, body) ->
            for p in parms do p.Default |> Option.iter (scanExpr handles)
            let inner =
                parms |> List.fold (fun (s: Set<string>) (p: LambdaParam) -> Set.remove p.Name s) handles
            scanExpr inner body
        | ExprKind.ExprLet (b, body) ->
            scanExpr handles b.Value
            scanExpr (shadow handles b.Pattern) body
        | ExprKind.ExprMatch (scrutinee, cases) ->
            scanExpr handles scrutinee
            for c in cases do
                let inner = shadow handles c.Pattern
                c.Guard |> Option.iter (scanExpr inner)
                scanExpr inner c.Body
        | ExprKind.ExprBlock (stmts, tail) ->
            let final = stmts |> List.fold scanStmt handles
            tail |> Option.iter (scanExpr final)
        | ExprKind.ExprRecArray def ->
            let inner =
                handles |> Set.remove def.Name |> Set.remove def.PrefixVar |> Set.remove def.StepVar
            def.SeedArm |> Option.iter (fun (seedVar, seedE) -> scanExpr (Set.remove seedVar inner) seedE)
            def.Guard |> Option.iter (scanExpr inner)
            scanExpr inner def.SliceExpr
        | _ -> childrenOf e |> List.iter (scanExpr handles)

    and scanStmt (handles: Set<string>) (s: Stmt) : Set<string> =
        match s with
        | StmtSpanned (inner, _) -> scanStmt handles inner
        | StmtLet b ->
            scanExpr handles b.Value
            shadow handles b.Pattern
        | StmtAssign (lhs, _, rhs) ->
            scanExpr handles lhs
            scanExpr handles rhs
            handles
        | StmtExpr e ->
            scanExpr handles e
            handles
        | StmtForIn (v, rangeE, body) ->
            scanExpr handles rangeE
            body |> List.fold scanStmt (Set.remove v handles) |> ignore
            handles

    /// A binding's RHS at module level. A checkout AT the root here is the
    /// blessed position: `rewriteValue` has already dealt with it (rewrote it,
    /// or refused its shape), so flagging it again would double-report. Its
    /// arguments are ordinary expressions and still get walked.
    let scanBindingValue (handles: Set<string>) (v: Expr) : unit =
        match v.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar recv }, "checkout") }, args)
            when Set.contains recv handles ->
            args |> List.iter (scanExpr handles)
        | _ -> scanExpr handles v

    // Replay the handle map over the REWRITTEN decls, left to right, so each
    // position sees exactly the handles bound above it. `recordRepo` records
    // the same set it did the first time: a blessed checkout is an `ic.load`
    // by now, and the key it carries declines `isCheckoutKey`.
    let scanFuncBody (outer: Set<string>) (fd: FunctionDecl) =
        // Defaults are evaluated in the ENCLOSING scope, the body inside the
        // parameter scope -- so a parameter named after a repo handle shadows it.
        for p in fd.Params do p.Default |> Option.iter (scanExpr outer)
        let inner =
            fd.Params |> List.fold (fun (s: Set<string>) (p: ParamDecl) -> Set.remove p.Name s) outer
        scanExpr inner fd.Body
    repos.Value <- Map.empty
    let handleNames () = repos.Value |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    for d in decls' do
        match d.Value with
        | DeclLet b
        | DeclStatic b ->
            scanBindingValue (handleNames ()) b.Value
            recordRepo b
        | DeclFunction fd ->
            scanFuncBody (handleNames ()) fd
            // Same shadowing rule the rewrite walk applies.
            repos.Value <- Map.remove fd.Name repos.Value
        | DeclImpl impl ->
            let outer = handleNames ()
            for fd in impl.Methods do scanFuncBody outer fd
        | _ -> ()

    if changed.Value then { m with Decls = decls' } else m

// Entry points

/// The pass entry point -- same shape as the other AST->AST expansions
/// (`Unfold.expand`, `Grad.expand`). Programs with no icechunk import come
/// back reference-equal. Every wrong-shaped checkout on a recognized repo, and
/// every well-shaped one in an unrewritable position, is collected, so one run
/// reports them all.
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    let diags = ResizeArray<Blade.Diagnostics.Diagnostic>()
    let changed = ref false
    let modules' =
        program.Modules |> List.map (fun m ->
            let m' = desugarModule diags m
            if not (LanguagePrimitives.PhysicalEquality m' m) then changed.Value <- true
            m')
    if diags.Count > 0 then Error (List.ofSeq diags)
    elif changed.Value then Ok { program with Modules = modules' }
    else Ok program

/// Total variant for the funnels that run AFTER typecheck has already
/// accepted the program (lowering's raw decl list, the IDE's payload
/// collectors). A refusal there is not actionable -- `expand` already
/// reported it from `typeCheck`, and re-reporting from lowering would
/// double every diagnostic -- so a failed desugar simply leaves the program
/// alone and the shape fails downstream exactly as it did before.
let desugarOrIdentity (program: Program) : Program =
    match expand program with
    | Ok p -> p
    | Error _ -> program
