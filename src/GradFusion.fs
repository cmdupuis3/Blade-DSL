// C7 pipeline fusion (`>>@` / `@>>` / `<$>` -- the Compose-Apply duality,
// mechanized as map fusion) and the grouped-peel classification helpers.
module Blade.GradFusion

open Blade.Ast
open Blade.GradCommon
open Blade.GradExpand

// ===========================================================================
// C7: pipeline fusion -- `>>@`, `@>>`, `<$>`
// ===========================================================================
// `>>@` / `@>>` / `<$>` are the Compose-Apply duality of formalism.md 10.3,
// whose mechanized proof "is literally map fusion". So a pipeline of stage
// kernels IS a single map over the composed kernel, and the whole of C7 is
// that rewrite: fuse first, differentiate the fusion. The tangent walker
// never learns a pipeline rule -- one normalization upstream closes all
// three seams (staticExtentOf, walkExpr, tangentOfExpr) at once.
//
// The rewrite is total in the sense that matters: when it cannot prove a
// fusion sound it DECLINES, leaving the node exactly as it found it and
// recording a reason. Differentiated code turns a decline into a refusal
// (the tangent walker has no other rule); primal code lets the decline fall
// through to the existing IRComposeApply path.

/// Why a kernel expression could not be normalized to a lambda. The two
/// reasons are distinguished because they get different diagnostics: a
/// block body is a v1 restriction with a fix, anything else is "that is not
/// a kernel".
type internal KernelShape =
    | KernBlockBody of string
    | KernUnsupported

/// Normalize a kernel expression to (params, where-clause, body, reynolds
/// sign). The four spellings the map rule accepts: a lambda, a
/// `reynolds(lambda[, Antisymmetric])`, an expression-bodied same-module
/// named function, and a scalar math intrinsic (eta-expanded).
///
/// Factored out of `tangentOfMap`'s `normKern`, which now calls it: the map
/// rule and pipeline fusion must not drift apart on what counts as a kernel.
let internal asKernelLambda (ctx: Ctx) (k: Expr)
    : Result<LambdaParam list * WhereClause option * Expr * bool option, KernelShape> =
    match k.Kind with
    | ExprKind.ExprLambda (ps, wc, body) -> Ok (ps, wc, body, None)
    | ExprKind.ExprReynolds ({ Kind = ExprKind.ExprLambda (ps, wc, body) }, isAnti) ->
        Ok (ps, wc, body, Some isAnti)
    | ExprKind.ExprVar f when Map.containsKey f ctx.Decls ->
        let fd = ctx.Decls.[f]
        (match fd.Body.Kind with
         | ExprKind.ExprBlock _ -> Error (KernBlockBody f)
         | _ ->
             Ok (fd.Params |> List.map (fun p ->
                     { Name = p.Name; Type = None; Default = None; NameSpan = noSpan }),
                 fd.WhereClause, fd.Body, None))
    | ExprKind.ExprVar name when isMathIntrinsic name ->
        let p = fresh ctx "__ck"
        Ok ([ { Name = p; Type = None; Default = None; NameSpan = noSpan } ],
            None,
            inheritSpan k (ExprApp (inheritSpan k (ExprVar name), [inheritSpan k (ExprVar p)])),
            None)
    | _ -> Error KernUnsupported

/// Rewrite every PARAMETER reference a where-clause carries, one name to a
/// LIST of names: `comm`/`anticomm` groups, `omp(x: n)` variable lists, and
/// the open `Custom` conjuncts. `TDims` names dimensions, not parameters
/// (and the parser never populates it), so it rides through untouched.
///
/// One walker, two instances, because there are two ways a kernel's
/// parameters get rewritten under the AD transforms and both must reach
/// every clause form. Rewriting rather than dropping is deliberate in both:
/// a parallelism license is part of what the user declared, and the omp
/// census invariant says a dropped `omp` is never silent.
let internal mapWhereVars (f: Ident -> Ident list) (w: WhereClause) : WhereClause =
    let ex (group: Ident list) = group |> List.collect f
    { w with
        Commutativity = w.Commutativity |> List.map ex
        Antisymmetry = w.Antisymmetry |> List.map ex
        Parallel =
            w.Parallel |> List.map (function
                | Omp s ->
                    Omp { s with
                            Vars = s.Vars |> List.collect (fun (n, d) ->
                                f n |> List.map (fun nm -> (nm, d))) }
                | other -> other)
        Custom = w.Custom |> List.map (fun (n, args) -> (n, ex args)) }

/// Instance 1: pipeline fusion alpha-renames stage 1's parameters, so every
/// clause naming them must follow. One name in, one name out.
let internal renameWhereVars (ren: string -> string) (w: WhereClause) : WhereClause =
    mapWhereVars (fun n -> [ren n]) w

/// Does this where-clause say anything? A second-stage kernel's clause
/// cannot survive fusion (its parameter does not), so a non-inert one is a
/// refusal rather than a silent drop.
let internal whereIsInert (w: WhereClause option) : bool =
    match w with
    | None -> true
    | Some w ->
        List.isEmpty w.Commutativity && List.isEmpty w.Antisymmetry
        && List.isEmpty w.Parallel && List.isEmpty w.TDims && List.isEmpty w.Custom

/// How a kernel names itself in a diagnostic.
let internal kernName (k: Expr) : string =
    match k.Kind with
    | ExprKind.ExprVar n -> $"'{n}'"
    | _ -> "<lambda>"

/// Fuse two stage kernels into one. `k1` runs FIRST and may have any arity
/// n (it is the one that meets the loop operands); `k2` runs SECOND on k1's
/// single result, so its arity must be exactly 1. The fused kernel is a
/// lambda over FRESH copies of k1's parameters -- alpha-renaming is not
/// cosmetic, it is what stops k2's free captures from being captured by k1's
/// parameter names.
///
/// `at` supplies the span of the pipeline node the fusion replaces.
let internal fuseKernels (ctx: Ctx) (at: Expr) (k1: Expr) (k2: Expr) : Result<Expr, string> =
    let norm (ordinal: string) (k: Expr) =
        match asKernelLambda ctx k with
        | Ok (_, _, _, Some _) ->
            Error ($"fusing a pipeline does not support a `reynolds(...)` stage kernel (the {ordinal} stage kernel {(kernName k)} is one)")
        | Ok (ps, wc, body, None) -> Ok (ps, wc, body)
        | Error (KernBlockBody f) ->
            Error (kernBlockBodyMsg f)
        | Error KernUnsupported ->
            Error "differentiating a pipeline supports lambda, named-function and intrinsic stage kernels (v1)"
    norm "first" k1 |> Result.bind (fun (ps1, wc1, b1) ->
    norm "second" k2 |> Result.bind (fun (ps2, wc2, b2) ->
    match ps2 with
    | [p2] when whereIsInert wc2 ->
        let renames = ps1 |> List.map (fun p -> (p, fresh ctx "__fs"))
        let renMap = renames |> List.map (fun (p, n) -> (p.Name, n)) |> Map.ofList
        let ren n = match Map.tryFind n renMap with Some x -> x | None -> n
        let b1' =
            renames |> List.fold (fun acc (p, newN) ->
                acc |> Option.bind (substKern p.Name (inheritSpan k1 (ExprVar newN)))) (Some b1)
        // The SECOND stage's parameter annotation is a constraint on what
        // flows into it, exactly like the first stage's (carried below onto
        // the fused lambda's params). Its parameter is substituted away, so
        // the constraint rides the substituted value as an ascription --
        // `(b1 : T)` is a bidirectional CHECK, not a cast (inferExpr's
        // ExprTyped arm) -- and `lambda(y: Bool)` after an Int64 stage is
        // refused with the same BL3001 the unfused application gives.
        // Without this the annotation simply vanished: the fused pipeline
        // accepted what the direct application refused.
        let stage1Value =
            b1' |> Option.map (fun b ->
                match p2.Type with
                | Some ty -> inheritSpan k2 (ExprTyped (b, ty))
                | None -> b)
        match stage1Value |> Option.bind (fun b -> substKern p2.Name b b2) with
        | None ->
            Error ($"fusing a pipeline cannot substitute through the stage kernels {(kernName k1)} and {(kernName k2)} (a binder or an unsupported form stands between the stages)")
        | Some body ->
            // Carry the first stage's DECLARED parameter types onto the fused
            // lambda. An annotation is a constraint as much as a hint --
            // `lambda(x: Float<mps>)` refuses things `lambda(x)` accepts --
            // and a rename is no reason to lose one.
            let ps =
                renames |> List.map (fun (p, newN) ->
                    { Name = newN; Type = p.Type; Default = None; NameSpan = noSpan })
            Ok (inheritSpan at (ExprLambda (ps, wc1 |> Option.map (renameWhereVars ren), body)))
    | [_] ->
        Error ($"fusing a pipeline cannot carry the second stage's `where` clause: stage kernel {(kernName k2)} declares one, but its parameter does not survive the fusion")
    | _ ->
        Error ($"differentiating a pipeline requires each stage after the first to take exactly one argument; stage kernel {(kernName k2)} takes {ps2.Length}")))

/// A span-insensitive key for a loop's operand list, so `@>>` can insist
/// that both computations iterate the SAME loop before it merges them.
/// `None` means "cannot tell" -- which is a refusal, not a match.
let rec internal loopKey (ops: Expr list) : string option =
    let one (x: Expr) =
        match x.Kind with
        | ExprKind.ExprVar n -> Some ("v " + n)
        | ExprKind.ExprRange tys -> Some (sprintf "r %d %A" tys.Length tys)
        | ExprKind.ExprReverse t -> Some (sprintf "rev %A" t)
        | ExprKind.ExprMethodFor inner -> loopKey inner |> Option.map (fun k -> "m " + k)
        | _ -> None
    ops |> traverseO one |> Option.map (String.concat " | ")

/// Structural array-ness, for the one `<$>` case that needs to tell an
/// already-materialized array from a deferred computation: `f <$> A` over an
/// array is the trivial map `method_for(A) <@> f`, while `f <$> c` over a
/// computation is a post-map on c's kernel. Getting this wrong in the safe
/// direction costs a fusion; getting it wrong in the other direction builds
/// a loop over a loop, so anything unrecognized declines.
let internal isArrayish (known: Set<string>) (resolved: Expr) (original: Expr) : bool =
    let rec go (x: Expr) =
        match x.Kind with
        | ExprKind.ExprArrayLit _ | ExprKind.ExprStack _ | ExprKind.ExprSequence _
        | ExprKind.ExprJoin _ | ExprKind.ExprZip _ | ExprKind.ExprReplicate _
        | ExprKind.ExprRange _ | ExprKind.ExprCompute _ | ExprKind.ExprRead _ -> true
        | ExprKind.ExprTyped (i, _) -> go i
        | _ -> false
    (match original.Kind with
     | ExprKind.ExprVar n -> Set.contains n known
     | _ -> false)
    || go resolved || go original

/// Which bound values are worth remembering for pipeline resolution: loop
/// objects, map applications, unfused pipeline nodes, and kernel lambdas.
let rec internal bindsPipelineValue (v: Expr) : bool =
    match v.Kind with
    | ExprKind.ExprObjectFor _ | ExprKind.ExprMethodFor _ | ExprKind.ExprLambda _
    | ExprKind.ExprReynolds _ -> true
    | ExprKind.ExprBinOp (_, (OpApply | OpComposeObj | OpComposeMeth | OpFunctor), _, _) -> true
    | ExprKind.ExprTyped (i, _) -> bindsPipelineValue i
    | _ -> false

/// Is there a pipeline operator anywhere in `e`? The rewrite below rebuilds
/// every node it walks, so a body with no `>>@` / `@>>` / `<$>` in it comes back
/// structurally equal and freshly allocated -- for EVERY declaration of EVERY
/// program, since fusion runs universally, and again in the AD lane over bodies
/// the universal pass already fused. This pre-scan makes those cases return the
/// original term, reference-identical.
///
/// Skipping is sound because a DECLINED pipeline keeps its operator: a body the
/// rewrite would have left alone still answers true here and still gets walked.
/// Coverage matches `mentionsDeep`'s (a superset of what the rewrite descends
/// into), leaves listed by name so a new grammar node is an incomplete-match
/// warning rather than a silent "nothing here".
let rec internal containsPipelineOp (e: Expr) : bool =
    let any = List.exists containsPipelineOp
    let opt o = match o with Some x -> containsPipelineOp x | None -> false
    match e.Kind with
    | ExprKind.ExprBinOp (_, (OpComposeObj | OpComposeMeth | OpFunctor), _, _) -> true
    | ExprKind.ExprBinOp (_, _, l, r) -> containsPipelineOp l || containsPipelineOp r
    | ExprKind.ExprUnaryOp (_, i) -> containsPipelineOp i
    | ExprKind.ExprApp (f, args) -> containsPipelineOp f || any args
    | ExprKind.ExprTupleIndex (t, i) -> containsPipelineOp t || containsPipelineOp i
    | ExprKind.ExprField (t, _) -> containsPipelineOp t
    | ExprKind.ExprLambda (ps, _, b) ->
        (ps |> List.exists (fun p -> opt p.Default)) || containsPipelineOp b
    | ExprKind.ExprLet (bnd, b) -> containsPipelineOp bnd.Value || containsPipelineOp b
    | ExprKind.ExprMatch (s, cases) ->
        containsPipelineOp s || (cases |> List.exists (fun c -> opt c.Guard || containsPipelineOp c.Body))
    | ExprKind.ExprIf (c, t, f) ->
        containsPipelineOp c || containsPipelineOp t || containsPipelineOp f
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprStack es
    | ExprKind.ExprSequence es | ExprKind.ExprZip es | ExprKind.ExprMethodFor es
    | ExprKind.ExprGroupKeys es -> any es
    | ExprKind.ExprAlign (es, _) | ExprKind.ExprJoin (es, _) -> any es
    | ExprKind.ExprBlock (ss, fe) -> (ss |> List.exists stmtContainsPipelineOp) || opt fe
    | ExprKind.ExprObjectFor k -> containsPipelineOp k
    | ExprKind.ExprDotDot (l, h) -> containsPipelineOp l || containsPipelineOp h
    | ExprKind.ExprHalo (_, o) -> containsPipelineOp o
    | ExprKind.ExprPure i | ExprKind.ExprCompute i | ExprKind.ExprRead i
    | ExprKind.ExprRank i | ExprKind.ExprUnique i | ExprKind.ExprGroupBucket i
    | ExprKind.ExprExtents i | ExprKind.ExprStatic i | ExprKind.ExprTyped (i, _)
    | ExprKind.ExprTranspose (i, _, _) | ExprKind.ExprDecompact (i, _)
    | ExprKind.ExprPartialApp (_, i, _) | ExprKind.ExprReynolds (i, _) -> containsPipelineOp i
    | ExprKind.ExprGuard (l, r) | ExprKind.ExprReplicate (l, r) | ExprKind.ExprMask (l, r)
    | ExprKind.ExprCompound (l, r) | ExprKind.ExprSparse (l, r)
    | ExprKind.ExprIntersect (l, r) | ExprKind.ExprUnion (l, r)
    | ExprKind.ExprContains (l, r) | ExprKind.ExprGroupBy (l, r)
    | ExprKind.ExprSort (l, r) | ExprKind.ExprGram (l, r)
    | ExprKind.ExprAssign (l, r) -> containsPipelineOp l || containsPipelineOp r
    | ExprKind.ExprReduce (a, k, i, ax) ->
        containsPipelineOp a || containsPipelineOp k || opt i || opt ax
    | ExprKind.ExprStruct (_, fields, spread) ->
        (fields |> List.exists (fun (_, fe) -> containsPipelineOp fe)) || opt spread
    | ExprKind.ExprFor (src, _, k) ->
        (match src with
         | ForArrays (es, inc) -> any es || opt inc
         | ForKernel k2 -> containsPipelineOp k2)
        || opt k
    | ExprKind.ExprRecArray d ->
        containsPipelineOp d.SliceExpr
        || (match d.SeedArm with Some (_, se) -> containsPipelineOp se | None -> false)
        || (match d.Guard with Some g -> containsPipelineOp g | None -> false)
    | ExprKind.ExprVar _ | ExprKind.ExprLit _ | ExprKind.ExprWildcard
    | ExprKind.ExprQualified _ | ExprKind.ExprRange _ | ExprKind.ExprReverse _
    | ExprKind.ExprArity _ | ExprKind.ExprNth | ExprKind.ExprZero
    | ExprKind.ExprSection _ -> false

and internal stmtContainsPipelineOp (s: Stmt) : bool =
    match s with
    | StmtSpanned (inner, _) -> stmtContainsPipelineOp inner
    | StmtLet b -> containsPipelineOp b.Value
    | StmtExpr ex -> containsPipelineOp ex
    | StmtAssign (l, _, r) -> containsPipelineOp l || containsPipelineOp r
    | StmtForIn (_, r, body) ->
        containsPipelineOp r || (body |> List.exists stmtContainsPipelineOp)

/// Binding-site bookkeeping for the fusion HYGIENE check. `Locals` stamps
/// every name bound inside the body being rewritten -- the function's
/// parameters, lambda parameters, block lets -- with a fresh number per
/// binding, so two bindings of one spelling are told apart. `DefLocals`
/// remembers, for each env entry bound inside this body, the `Locals` in
/// force where it was bound. A module-level entry has no `DefLocals` entry
/// and reads as the empty map: module scope binds none of this body's locals.
type internal FuseScope = { Locals: Map<string, int>; DefLocals: Map<string, Map<string, int>> }

/// The rewrite. Bottom-up over one body, threading `env` (names bound to
/// loop objects, computations, compose values and kernel lambdas -- whatever
/// a pipeline operand might hide behind), `arrays` (names known to hold
/// materialized arrays) and the hygiene scope. `params0` are the enclosing
/// function's parameter names (empty for a module-level binding). Returns
/// the rewritten expression and the DECLINE reasons for pipeline nodes it
/// left alone.
let internal fusePipelinesEnvIn (ctx: Ctx) (env0: Map<string, Expr>) (arrays0: Set<string>)
                               (params0: string list) (body: Expr) : Expr * string list =
    let declines = ResizeArray<string>()
    let decline (m: string) = if not (declines.Contains m) then declines.Add m
    let stampCounter = ref 0
    let stamp () =
        stampCounter.Value <- stampCounter.Value + 1
        stampCounter.Value
    let bindLocals (names: string list) (sc: FuseScope) : FuseScope =
        { sc with Locals = names |> List.fold (fun m n -> Map.add n (stamp ()) m) sc.Locals }
    let scope0 = bindLocals params0 { Locals = Map.empty; DefLocals = Map.empty }
    /// Chase a name to the value it was bound to (depth-capped: `let x = x`
    /// is someone else's error, not a hang).
    let rec resolve (env: Map<string, Expr>) (d: int) (x: Expr) : Expr =
        if d > 8 then x
        else
        match x.Kind with
        | ExprKind.ExprVar n ->
            (match Map.tryFind n env with
             | Some b -> resolve env (d + 1) b
             | None -> x)
        | ExprKind.ExprTyped (inner, _) -> resolve env (d + 1) inner
        // a let-bound LOOP applied by name: `L <@> k` where `let L = method_for(A)`
        | ExprKind.ExprBinOp (bm, OpApply, { Kind = ExprKind.ExprVar n }, rhs) ->
            (match Map.tryFind n env with
             | Some ({ Kind = ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _ } as b) ->
                 inheritSpan x (ExprBinOp (bm, OpApply, b, rhs))
             | _ -> x)
        | _ -> x
    /// A kernel operand may itself be a let-bound lambda (`let calibrate =
    /// lambda(x) -> ...` then `object_for(calibrate)`), which is neither a
    /// same-module `function` nor an intrinsic, so `asKernelLambda` needs it
    /// resolved first.
    let resolveKern (env: Map<string, Expr>) (k: Expr) : Expr =
        match k.Kind with
        | ExprKind.ExprVar n when not (Map.containsKey n ctx.Decls) ->
            (match Map.tryFind n env with
             | Some ({ Kind = ExprKind.ExprLambda _ | ExprKind.ExprReynolds _ } as b) -> b
             | _ -> k)
        | _ -> k
    /// The `Locals` in force where an operand was WRITTEN: a name resolved
    /// through the env was bound earlier in this body (its DefLocals entry)
    /// or at module level (none of this body's locals); a same-module
    /// `function` is module-level too; anything else is inline, i.e. here.
    let originLocals (env: Map<string, Expr>) (sc: FuseScope) (x: Expr) : Map<string, int> =
        match x.Kind with
        | ExprKind.ExprVar n when Map.containsKey n env ->
            Map.tryFind n sc.DefLocals |> Option.defaultValue Map.empty
        | ExprKind.ExprVar n when Map.containsKey n ctx.Decls -> Map.empty
        | _ -> sc.Locals
    /// Where a KERNEL was written, given the operand it was found in: a
    /// kernel named by a variable has its own origin (the let that bound the
    /// lambda, or module scope for a `function`); an inline lambda shares
    /// its operand's.
    let kernOrigin (env: Map<string, Expr>) (sc: FuseScope) (operand: Expr) (k: Expr) : Map<string, int> =
        match k.Kind with
        | ExprKind.ExprVar n when Map.containsKey n ctx.Decls -> Map.empty
        | ExprKind.ExprVar n when Map.containsKey n env ->
            Map.tryFind n sc.DefLocals |> Option.defaultValue Map.empty
        | ExprKind.ExprVar _ -> sc.Locals   // an intrinsic: no free names
        | _ -> originLocals env sc operand
    /// HYGIENE. A kernel that reaches the fusion site by NAME was written
    /// somewhere else, and a free name in its body means the binding
    /// visible THERE. Substituting that body into a lambda at this site makes
    /// every free name resolve HERE instead, and when the two scopes bind
    /// the name differently the fusion changes what the program computes:
    /// `function add_global(x) = x + scale` fused inside `function
    /// piped(scale) = ...` read the PARAMETER `scale` (101 where 11 was
    /// meant), on both backends, because the rewrite runs ahead of both.
    /// The check compares each free name's binding stamp at the site with
    /// its stamp at the kernel's origin; a difference is a decline (the
    /// pipeline then stays an ordinary staged application, which calls the
    /// kernel by name and keeps its scope). `allVarsDeep` over-reports inner
    /// binders, which can only add declines -- the safe direction.
    let captureClash (sc: FuseScope) (origin: Map<string, int>) (kOrig: Expr) (k: Expr) : string option =
        if origin = sc.Locals then None else
        let kernelNameRebound =
            match kOrig.Kind with
            | ExprKind.ExprVar n when Map.tryFind n sc.Locals <> Map.tryFind n origin -> Some n
            | _ -> None
        match kernelNameRebound with
        | Some n -> Some n
        | None ->
        match asKernelLambda ctx k with
        | Ok (ps, _, kb, _) ->
            let own = ps |> List.map _.Name |> Set.ofList
            allVarsDeep kb
            |> Set.toList
            |> List.tryFind (fun v ->
                not (Set.contains v own) && Map.tryFind v sc.Locals <> Map.tryFind v origin)
        | Error _ -> None
    /// `fuseKernels` behind the hygiene check. `o1`/`o2` are the origins of
    /// the kernels as found (`kernOrigin`), `k1o`/`k2o` the kernel exprs
    /// before env resolution.
    let fuseChecked (sc: FuseScope) (at: Expr)
                    (o1: Map<string, int>) (k1o: Expr) (k1: Expr)
                    (o2: Map<string, int>) (k2o: Expr) (k2: Expr) : Result<Expr, string> =
        match captureClash sc o1 k1o k1, captureClash sc o2 k2o k2 with
        | Some v, _ ->
            Error ($"fusing a pipeline would change what '{v}' means: stage kernel {(kernName k1o)} reads '{v}' from the scope it was written in, and this site binds a different '{v}' (a parameter or local); the stages are left as separate applications")
        | _, Some v ->
            Error ($"fusing a pipeline would change what '{v}' means: stage kernel {(kernName k2o)} reads '{v}' from the scope it was written in, and this site binds a different '{v}' (a parameter or local); the stages are left as separate applications")
        | None, None -> fuseKernels ctx at k1 k2
    let rec go (env: Map<string, Expr>) (arrays: Set<string>) (sc: FuseScope) (e: Expr) : Expr =
        let re k = inheritSpan e k
        let g x = go env arrays sc x
        let gl xs = xs |> List.map g
        match e.Kind with
        // ---- `>>@`: compose two kernel OBJECTS; the LEFT stage runs first
        | ExprKind.ExprBinOp (m0, OpComposeObj, l, r) ->
            let l' = g l
            let r' = g r
            (match (resolve env 0 l').Kind, (resolve env 0 r').Kind with
             | ExprKind.ExprObjectFor k1, ExprKind.ExprObjectFor k2 ->
                 let o1 = kernOrigin env sc l' k1
                 let o2 = kernOrigin env sc r' k2
                 (match fuseChecked sc e o1 k1 (resolveKern env k1) o2 k2 (resolveKern env k2) with
                  | Ok fk -> re (ExprObjectFor fk)
                  | Error msg -> decline msg; re (ExprBinOp (m0, OpComposeObj, l', r')))
             | _ ->
                 decline "differentiating `>>@` needs both operands to resolve to `object_for(<kernel>)` (directly, or through a let- or module-level binding)"
                 re (ExprBinOp (m0, OpComposeObj, l', r')))
        // ---- `@>>`: compose two COMPUTATIONS over one loop; LEFT runs first
        | ExprKind.ExprBinOp (m0, OpComposeMeth, c1, c2) ->
            let c1' = g c1
            let c2' = g c2
            (match resolve env 0 c1', resolve env 0 c2' with
             | MapApply m1, MapApply m2 ->
                 (match loopKey m1.Ops, loopKey m2.Ops with
                  | Some a, Some b when a = b ->
                      let o1 = kernOrigin env sc c1' m1.Kern
                      let o2 = kernOrigin env sc c2' m2.Kern
                      (match fuseChecked sc e o1 m1.Kern (resolveKern env m1.Kern) o2 m2.Kern (resolveKern env m2.Kern) with
                       | Ok fk -> m1.Rebuild fk
                       | Error msg -> decline msg; re (ExprBinOp (m0, OpComposeMeth, c1', c2')))
                  | _ ->
                      decline "differentiating `@>>` requires both computations to iterate the same loop object"
                      re (ExprBinOp (m0, OpComposeMeth, c1', c2')))
             | _ ->
                 decline "differentiating `@>>` requires both operands to resolve to a map application over one loop object"
                 re (ExprBinOp (m0, OpComposeMeth, c1', c2')))
        // ---- `<$>`: post-map. The LEFT operand is the SECOND stage.
        | ExprKind.ExprBinOp (m0, OpFunctor, kf, c) ->
            let kf' = g kf
            let c' = g c
            let rc = resolve env 0 c'
            (match rc with
             | MapApply m1 ->
                 let o1 = kernOrigin env sc c' m1.Kern
                 let o2 = kernOrigin env sc kf' kf'
                 (match fuseChecked sc e o1 m1.Kern (resolveKern env m1.Kern) o2 kf' (resolveKern env kf') with
                  | Ok fk -> m1.Rebuild fk
                  | Error msg -> decline msg; re (ExprBinOp (m0, OpFunctor, kf', c')))
             | _ when isArrayish arrays rc c' ->
                 // over an already-materialized array `<$>` IS the trivial map.
                 // The kernel is resolved through the env for the same reason
                 // the compose arms resolve theirs: a module-level
                 // `let k = lambda(...)` is not a `function`, so nothing
                 // downstream can see it as a kernel unless fusion inlines it.
                 // Same hygiene rule: an inlined let-bound lambda keeps its
                 // own scope only when this site binds its free names the
                 // same way; otherwise the name is left for the checker.
                 let kfR = resolveKern env kf'
                 (match captureClash sc (kernOrigin env sc kf' kf') kf' kfR with
                  | Some v ->
                      decline ($"inlining the `<$>` kernel {(kernName kf')} would change what '{v}' means at this site")
                      re (ExprBinOp (Elementwise, OpApply, re (ExprMethodFor [c']), kf'))
                  | None ->
                      re (ExprBinOp (Elementwise, OpApply, re (ExprMethodFor [c']), kfR)))
             | _ ->
                 decline "differentiating `<$>` requires its right operand to resolve to a map application or to a named array"
                 re (ExprBinOp (m0, OpFunctor, kf', c')))
        // ---- structural recursion ------------------------------------------
        | ExprKind.ExprBinOp (m, op, l, r) -> re (ExprBinOp (m, op, g l, g r))
        | ExprKind.ExprUnaryOp (op, i) -> re (ExprUnaryOp (op, g i))
        | ExprKind.ExprTyped (i, t) -> re (ExprTyped (g i, t))
        | ExprKind.ExprCompute i -> re (ExprCompute (g i))
        | ExprKind.ExprPure i -> re (ExprPure (g i))
        | ExprKind.ExprRead i -> re (ExprRead (g i))
        | ExprKind.ExprGuard (c, b) -> re (ExprGuard (g c, g b))
        | ExprKind.ExprApp (f, args) -> re (ExprApp (f, gl args))
        | ExprKind.ExprIf (c, t, f) -> re (ExprIf (g c, g t, g f))
        | ExprKind.ExprTuple es -> re (ExprTuple (gl es))
        | ExprKind.ExprArrayLit es -> re (ExprArrayLit (gl es))
        | ExprKind.ExprStack es -> re (ExprStack (gl es))
        | ExprKind.ExprSequence es -> re (ExprSequence (gl es))
        | ExprKind.ExprZip es -> re (ExprZip (gl es))
        | ExprKind.ExprJoin (es, d) -> re (ExprJoin (gl es, d))
        | ExprKind.ExprMethodFor ops -> re (ExprMethodFor (gl ops))
        | ExprKind.ExprObjectFor k -> re (ExprObjectFor (g k))
        | ExprKind.ExprReplicate (c, b) -> re (ExprReplicate (g c, g b))
        | ExprKind.ExprReduce (a, k, i, ax) -> re (ExprReduce (g a, g k, Option.map g i, ax))
        | ExprKind.ExprTupleIndex (t, i) -> re (ExprTupleIndex (g t, i))
        | ExprKind.ExprLambda (ps, wc, b) ->
            // the lambda's own parameters shadow anything the env knows
            let names = ps |> List.map _.Name
            let env2 = names |> List.fold (fun (m: Map<string, Expr>) n -> Map.remove n m) env
            let arr2 = names |> List.fold (fun s n -> Set.remove n s) arrays
            re (ExprLambda (ps, wc, go env2 arr2 (bindLocals names sc) b))
        // NOTE for anyone adding arms below: every remaining BINDER form
        // (`ExprLet`, `ExprMatch`, `ExprFor`, `ExprRecArray`) falls to the
        // catch-all and is returned UNCHANGED. That forgoes fusion inside them,
        // which is only a missed rewrite; descending without first dropping the
        // form's binders from `env`/`arrays` would resolve a shadowed name to an
        // outer binding, which is a wrong answer. Shadow first, then descend.
        | ExprKind.ExprBlock (ss, fe) ->
            let env2, arr2, sc2, ss' =
                ss |> List.fold (fun (en, ar, s, acc) st ->
                    match unwrapStmt st with
                    | StmtLet ({ Pattern = { Kind = PatternKind.PatVar nm } } as b) ->
                        let v2 = go en ar s b.Value
                        let en2 = if bindsPipelineValue v2 then Map.add nm v2 en else Map.remove nm en
                        let ar2 = if isArrayish ar (resolve en 0 v2) v2 then Set.add nm ar else Set.remove nm ar
                        // The value was written under `s.Locals` (a let is
                        // not recursive: `nm` itself is not in scope there).
                        let s2 =
                            { bindLocals [nm] s with
                                DefLocals =
                                    if bindsPipelineValue v2 then Map.add nm s.Locals s.DefLocals
                                    else Map.remove nm s.DefLocals }
                        (en2, ar2, s2, StmtLet { b with Value = v2 } :: acc)
                    // A NON-PatVar pattern still BINDS: `let (inc, dec) = ...`
                    // shadows a module-level `inc` for the rest of the block.
                    // Threading the env untouched left the stale binding
                    // visible, so `inc <$> a` fused the MODULE kernel into a
                    // pipeline the local one owns -- a wrong answer, silently.
                    // Nothing here can say what a destructured component is
                    // bound to, so the names are simply dropped from both maps.
                    | StmtLet b ->
                        let bound = patternBoundNames b.Pattern
                        let v2 = go en ar s b.Value
                        let en2 = bound |> List.fold (fun (m: Map<string, Expr>) n -> Map.remove n m) en
                        let ar2 = bound |> List.fold (fun st n -> Set.remove n st) ar
                        let s2 =
                            { bindLocals bound s with
                                DefLocals = bound |> List.fold (fun (m: Map<string, Map<string, int>>) n -> Map.remove n m) s.DefLocals }
                        (en2, ar2, s2, StmtLet { b with Value = v2 } :: acc)
                    | StmtExpr ex -> (en, ar, s, StmtExpr (go en ar s ex) :: acc)
                    | StmtAssign (l, o, r) -> (en, ar, s, StmtAssign (l, o, go en ar s r) :: acc)
                    | other -> (en, ar, s, other :: acc)) (env, arrays, sc, [])
            re (ExprBlock (List.rev ss', fe |> Option.map (go env2 arr2 sc2)))
        | _ -> e
    let out = go env0 arrays0 scope0 body
    (out, List.ofSeq declines)

/// `fusePipelinesEnvIn` for a MODULE-LEVEL body: no enclosing parameters.
let internal fusePipelinesEnv (ctx: Ctx) (env0: Map<string, Expr>) (arrays0: Set<string>)
                             (body: Expr) : Expr * string list =
    fusePipelinesEnvIn ctx env0 arrays0 [] body

/// Fuse the pipelines in one function's body, seeded from the module scope:
/// module-level bindings are visible inside every body, except where a
/// parameter shadows one. Array-typed parameters seed the `<$>`
/// array-vs-computation decision.
let internal fuseFunctionBody (ctx: Ctx) (fd: FunctionDecl) : Expr * string list =
    if not (containsPipelineOp fd.Body) then (fd.Body, []) else
    let paramNames = fd.Params |> List.map _.Name |> Set.ofList
    let env =
        moduleLetValues ctx
        |> Map.filter (fun n _ -> not (Set.contains n paramNames))
    let moduleArrays =
        ctx.ModuleLets
        |> Map.toSeq
        |> Seq.filter (fun (_, ml) -> isArrayish Set.empty ml.Value ml.Value)
        |> Seq.map fst
        |> Set.ofSeq
    let paramArrays =
        fd.Params
        |> List.filter (fun p ->
            match p.Type with
            | Some t -> (match resolveArrayTy ctx t with TyArray _ -> true | _ -> false)
            | None -> false)
        |> List.map _.Name
        |> Set.ofList
    fusePipelinesEnvIn ctx env (Set.union (Set.difference moduleArrays paramNames) paramArrays)
                       (fd.Params |> List.map _.Name) fd.Body

// ---------------------------------------------------------------------------
// Auto-lowered grouped peels
//
// A grouped peel -- `group_by(V, gk)` fed to `method_for(g) <@> lambda(r) -> K`
// -- produces a value over the GROUP axis, and a group axis has no
// compile-time extent. Nothing downstream can allocate over it, so the
// natural spelling of a per-group loss dies in `hoistReduces` with the
// generic "no statically-known extent" message.
//
// It does not have to be allocated. For the GROUP-LINEAR kernels the whole
// loss factors back through the SOURCE index space:
//
//     L = sum_g w_g * A_g,  A_g = init + sum_{i in g} phi(v_i)
//       = init * sum_g w_g  +  sum_i w_{b(i)} * phi(v_i)
//
// and the right-hand side is one loop over `V`, with the group axis appearing
// only as the subscript `b(i) = group_bucket(gk)(i)` into arrays the user
// already has. That form differentiates today in both modes (it is what
// `ad-jvp-comb/018` hand-writes), so this rewrite only has to EMIT it.
//
// The rule is purely additive: it fires on the shapes below and leaves every
// other body byte-identical, so its failure mode is "does not fire", never
// "fires wrong". Deliberately NOT done: teaching `staticExtentOf` about
// `ExprGroupBy`. The group axis must stay extent-unknown so a peel this
// rewrite declined keeps refusing loudly instead of silently allocating.
// ---------------------------------------------------------------------------

/// The per-group kernel of a peel, restricted to the group-linear subset:
/// the member partial depends on the group only through its SIZE, which is
/// key-derived data and so carries no derivative.
type internal PeelKernel =
    | PKSum of init: Expr option
    | PKMean of init: Expr option
    | PKCount

/// Whether the key space has a static group count -- equivalently, and
/// exactly invertedly, whether EMPTY groups are possible. Dynamic discovery
/// only ever manufactures a group it saw a row for; a positional key space
/// has slots nothing lands in.
type internal GroupRegime =
    | GRDynamic
    | GRStatic of ngroups: int
    | GRUnknown

let rec internal stripTypedE (e: Expr) : Expr =
    match e.Kind with
    | ExprKind.ExprTyped (i, _) -> stripTypedE i
    | _ -> e

/// Strip the wrappers a peel initializer may carry: `|> compute` and any
/// ascription. Both are transparent to the shape below them.
let rec internal stripPeelWrap (e: Expr) : Expr =
    match e.Kind with
    | ExprKind.ExprTyped (i, _) | ExprKind.ExprCompute i -> stripPeelWrap i
    | _ -> e

let internal isVarNamed (nm: string) (e: Expr) : bool =
    match (stripTypedE e).Kind with
    | ExprKind.ExprVar n -> n = nm
    | _ -> false

/// `<peel> over a NAMED grouped value` -- both spellings, the method-side
/// `method_for(g) <@> k` and the object-side `object_for(k) <@> g`.
let internal peelOverNamed (e: Expr) : (string * Expr) option =
    match stripPeelWrap e with
    | MapApply { Ops = [gv]; Kern = kern } ->
        (match (stripTypedE gv).Kind with ExprKind.ExprVar g -> Some (g, kern) | _ -> None)
    | _ -> None

/// `reduce(<r>, (+)[, init])` on the peel's own parameter -> its init slot.
/// `Some None` is "matched, no init"; `None` is "not this shape".
let internal peelSumOf (rp: string) (e: Expr) : Expr option option =
    match (stripTypedE e).Kind with
    | ExprKind.ExprReduce (src, { Kind = ExprKind.ExprSection OpAdd }, initOpt, None)
        when isVarNamed rp src -> Some initOpt
    | _ -> None

/// `extents(<r>)` on the peel's own parameter -- the per-group count. Fully
/// supported inside a peel kernel (sql.md 7b gather elision).
let internal peelCountOf (rp: string) (e: Expr) : bool =
    match (stripTypedE e).Kind with
    | ExprKind.ExprExtents src -> isVarNamed rp src
    | _ -> false

/// Read a peel kernel body. `Ok None` means "not a shape this rewrite knows"
/// -- the body is left alone and whatever refused it before refuses it still.
/// `Error` is reserved for a shape that IS a per-group aggregate but is not
/// group-linear, where the generic extent message would misdescribe the wall.
let internal classifyPeelKernel (rp: string) (body: Expr) : Result<PeelKernel option, string> =
    let b = stripTypedE body
    // The mean's denominator may be the count itself or its explicit float
    // cast -- `reduce(r, (+)) / Float64(extents(r))`, the BL3020-clean
    // spelling -- the VALUE is identical (int->float division promotes), so
    // the classification is unchanged. Bare PKCount is deliberately NOT
    // cast-transparent: there the wrapper changes the peel's element type.
    let castWrappedCount (e: Expr) =
        match (stripTypedE e).Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar cn }, [inner]) ->
            (match Blade.Types.castTargetOf cn with
             | Some (Blade.Types.ETFloat32 | Blade.Types.ETFloat64) -> peelCountOf rp inner
             | _ -> false)
        | _ -> false
    match b.Kind with
    | ExprKind.ExprBinOp (_, OpDiv, num, den) when peelCountOf rp den || castWrappedCount den ->
        (match peelSumOf rp num with
         | Some initOpt -> Ok (Some (PKMean initOpt))
         | None -> Ok None)
    | _ ->
        match peelSumOf rp b with
        | Some initOpt -> Ok (Some (PKSum initOpt))
        | None ->
            if peelCountOf rp b then Ok (Some PKCount)
            else
                match b.Kind with
                | ExprKind.ExprReduce (src, { Kind = ExprKind.ExprSection op }, _, _)
                    when isVarNamed rp src && op <> OpAdd ->
                    let opName = (match op with OpMul -> "(*)" | OpSub -> "(-)" | OpDiv -> "(/)" | _ -> "this")
                    Error $"the per-group {opName} aggregate is not sum-decomposable, so differentiating it needs the group axis MATERIALIZED -- and a group-space accumulator needs a group count known at COMPILE time, which a grouping does not have (v1). The auto-lowered subset is the group-linear one: `reduce(r, (+))`, `reduce(r, (+)) / extents(r)`, `extents(r)`"
                | _ -> Ok None

/// Is this init the additive identity, i.e. does it contribute nothing?
let internal isZeroInit (e: Expr) : bool =
    match (stripTypedE e).Kind with
    | ExprKind.ExprLit (LitFloat 0.0) | ExprKind.ExprLit (LitInt 0L) -> true
    | _ -> false

/// Rewrite the FULLY-REDUCED consumption of a grouped peel result `mName` to
/// `repl`, recording the group-space weight each hit carried:
///   C1  `reduce(m, (+))`      -> no weight
///   C2  `reduce(m * W, (+))`  -> weight `W` (either operand order)
/// The recursion mirrors `hoistReduces`: the arithmetic fragment a scalar
/// loss is built out of. A use this walker does not reach stays put, and the
/// caller's residual check then turns it into a named refusal.
let rec internal rewriteGroupConsumption (mName: string) (repl: Expr)
                                        (found: ResizeArray<Expr option>) (e: Expr) : Expr =
    let re k = inheritSpan e k
    let rc = rewriteGroupConsumption mName repl found
    match e.Kind with
    | ExprKind.ExprReduce (src, ({ Kind = ExprKind.ExprSection OpAdd } as sec), None, None) ->
        let hit =
            if isVarNamed mName src then Some None
            else
                match (stripTypedE src).Kind with
                | ExprKind.ExprBinOp (_, OpMul, a, b) when isVarNamed mName a ->
                    (match (stripTypedE b).Kind with ExprKind.ExprVar _ -> Some (Some (stripTypedE b)) | _ -> None)
                | ExprKind.ExprBinOp (_, OpMul, a, b) when isVarNamed mName b ->
                    (match (stripTypedE a).Kind with ExprKind.ExprVar _ -> Some (Some (stripTypedE a)) | _ -> None)
                | _ -> None
        (match hit with
         | Some w -> found.Add w; repl
         | None -> re (ExprReduce (rc src, sec, None, None)))
    | ExprKind.ExprBinOp (m, op, l, r) -> re (ExprBinOp (m, op, rc l, rc r))
    | ExprKind.ExprUnaryOp (op, i) -> re (ExprUnaryOp (op, rc i))
    | ExprKind.ExprTyped (i, t) -> re (ExprTyped (rc i, t))
    | ExprKind.ExprApp (f, args) -> re (ExprApp (f, args |> List.map rc))
    | ExprKind.ExprArrayLit es -> re (ExprArrayLit (es |> List.map rc))
    | ExprKind.ExprReduce (src, sec, initOpt, ax) -> re (ExprReduce (rc src, sec, initOpt, ax))
    | _ -> e

