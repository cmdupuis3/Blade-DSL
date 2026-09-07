// The two AD sweeps over normalized bodies: the reverse (adjoint) sweep
// behind ad.grad and the forward (tangent) sweep behind ad.jvp, plus the
// shared preparation that feeds them.
module Blade.GradSweeps

open Blade.Ast
open Blade.GradCommon
open Blade.GradExpand
open Blade.GradFusion
open Blade.GradPackUnroll
open Blade.GradNormalize

// The reverse sweep

let internal dName (n: string) = "__g_" + n

type internal RevCtx = {
    Fname: string
    Ctx: Ctx
    Diff: Set<string>
    Arrays: Set<string>
    /// Every name a bare `name(args)` may legitimately refer to WITHOUT a
    /// derivative rule: params (incl. int arrays), function-local binders,
    /// and module-level value bindings. A named application outside this set
    /// (and outside the intrinsic/decl arms above) is an unknown call and is
    /// refused rather than silently zero-differentiated.
    Known: Set<string>
    /// Declared index types per array PARAMETER, so the C2 map rule can
    /// spell `range<I>` over an operand's own index space (which also keeps
    /// the emitted reads properly tagged, unlike a raw extent).
    ArrayIdxTys: Map<string, TypeExpr list>
    /// LET-BOUND LOOP OBJECTS (`let Lp = method_for(a, b)`): the idiomatic
    /// style stores the loop as a value and applies it later. The tangent
    /// walker resolves the binding at the application site; the binding
    /// itself gets no tangent (a loop object is a deferred iteration, not
    /// data).
    LoopBindings: Map<string, Expr>
    /// Full static dims per named array (params + locals), for the C6
    /// reverse rules: cotangent buffers for combinator-built locals and
    /// bounds for the reindexing accumulation loops.
    Dims: Map<string, int list>
    /// C7: per sorted local, the permutation plumbing the pre-pass emitted
    /// beside it. Both sweeps read the SAME entry, which is what keeps the
    /// primal, the tangent gather, and the adjoint gather on one permutation.
    SortPlans: Map<string, SortPlan>
    /// The chain of same-module functions currently being substituted INTO a
    /// kernel body, innermost first (see `kernelCallBody`). Statement-level
    /// calls are inlined before either sweep runs and are capped by
    /// `normalizeBody`'s depth counter; expression-level ones are capped by
    /// this list's LENGTH. Membership is deliberately NOT a recursion test --
    /// a helper nested inside itself through an argument (`mean(x - mean(x))`)
    /// revisits the name and still terminates; self-recursion is read off the
    /// declaration instead.
    Inlining: string list
}

/// A call to a same-module user function from inside a KERNEL BODY, resolved
/// to the callee's body with its parameters substituted by the arguments.
///
/// Statement-position calls never get here: `hoistCalls` lifts them into
/// `let x = f(args)` and `inlineCall` splices them before either sweep runs.
/// But `hoistCalls` stops at a lambda (its catch-all), so a helper called
/// from a KERNEL -- `lambda(x) -> center(x) * w` -- arrives at the sweeps
/// intact, and both sweeps' generic named-application arms then refuse it as
/// an unknown call. Substitution is the expression-level twin of
/// `inlineCall`: the same admissibility gates (same-module, non-static, no
/// mut parameters, matching arity) and the same depth cap, with
/// expression-bodied callees only -- a block body has statements, which is
/// exactly what an expression position cannot hold.
///
/// Alpha-safety comes from `substParam`/`substKern`, which decline to cross
/// any binder they cannot prove safe, so no argument can be captured by a
/// binder in the callee's body.
///
/// Only the DERIVATIVE side substitutes. The primal keeps the call it was
/// written with -- it type-checks and code-generates as the ordinary
/// function it is -- so this rewrite cannot change what the primal computes.
let internal kernelCallBody (rc: RevCtx) (f: string) (args: Expr list)
    : Result<Expr * RevCtx, string> =
    let fd = rc.Ctx.Decls.[f]
    // An arity-polymorphic callee is expanded rather than substituted: its
    // ONE pack parameter stands for all `args`, so the parameter-count and
    // self-recursion gates below both read it wrong. Route A owns it.
    match tryUnrollPackCall rc.Ctx rc.Fname fd args with
    | Some r -> r |> Result.map (fun b -> (b, rc))
    | None ->
    // Self-recursion is read off the DECLARATION, not off the substitution
    // path. A path can revisit a name innocently -- `mean((x - mean(x)) * ...)`
    // substitutes mean's body and then meets a mean call that came in through
    // the ARGUMENT, which terminates -- so a path-membership test refuses the
    // comoment shapes it exists to support. A body that names itself is the
    // real non-terminating case. Mutual recursion cannot be seen this way, but
    // it is rejected by the language (BL2001) and, since this pass runs BEFORE
    // typecheck, by the depth cap below.
    if mentionsDeep (Set.singleton f) fd.Body then
        err rc.Fname $"cannot differentiate the call to '{f}' inside a kernel body: '{f}' is recursive (its own body names '{f}'), and a kernel body is substituted rather than taped, so the substitution would not terminate. Restructure the helper without recursion, or move the recursion out of the kernel"
    elif List.length rc.Inlining >= maxInlineDepth then
        err rc.Fname $"kernel-body call substitution exceeded depth {maxInlineDepth} (recursive functions are not differentiable)"
    else
        checkInlinable rc.Fname fd args.Length |> Result.bind (fun () ->
        match fd.Body.Kind with
        | ExprKind.ExprBlock _ ->
            err rc.Fname $"cannot differentiate the call to '{f}' inside a kernel body: only EXPRESSION-bodied same-module functions can be substituted into a kernel (v1), and '{f}' has a block body. Rewrite it as a single expression, or call it outside the kernel"
        | _ ->
            List.zip fd.Params args
            |> List.fold (fun acc (p, a) ->
                acc |> Result.bind (fun b -> substParam rc.Fname p.Name a b))
                (Ok fd.Body)
            |> Result.map (fun b -> (b, { rc with Inlining = f :: rc.Inlining })))

/// Recover the sort plumbing by SHAPE rather than by name: inlining renames
/// callee locals, so name arithmetic would not survive a differentiated call.
/// For each primal `let s = sort(A, key)` the permutation is the nearest
/// PRECEDING `sort(iota, lambda(i: I) -> A(i))`, and the inverse permutation
/// the nearest preceding sort keyed on that permutation.
let internal collectSortPlans (ss: NStmt list) : Map<string, SortPlan> =
    // (bound name, keyed array, index type), most recent first
    let rec go (iotas: Set<string>) (perms: (string * string * TypeExpr) list)
               (acc: Map<string, SortPlan>) (ss: NStmt list) =
        ss |> List.fold (fun (iotas, perms, acc) st ->
            match st with
            | NLet (n, _, IndexIota) -> (Set.add n iotas, perms, acc)
            | NLet (p, _, SortPermForm iotas (keyed, Some ity)) -> (iotas, (p, keyed, ity) :: perms, acc)
            | NLet (s, _, { Kind = ExprKind.ExprSort ({ Kind = ExprKind.ExprVar src }, _) }) ->
                (match perms |> List.tryFind (fun (_, keyed, _) -> keyed = src) with
                 | Some (pn, _, ity) when not (Map.containsKey s acc) ->
                     let inv = perms |> List.tryPick (fun (q, k2, _) -> if k2 = pn then Some q else None)
                     (iotas, perms, Map.add s { Perm = pn; InvPerm = (defaultArg inv ""); IdxTy = ity; Src = src } acc)
                 | _ -> (iotas, perms, acc))
            | NFor (_, _, _, body) -> go iotas perms acc body
            | _ -> (iotas, perms, acc)) (iotas, perms, acc)
    let (_, _, plans) = go Set.empty [] Map.empty ss
    plans

/// Emit `d += cot`-style accumulation onto a cotangent target.
let internal accum (target: Expr) (cot: Expr) : NStmt =
    NAssign (target, add target cot)

/// Bind a nontrivial cotangent expression to a temp so the adjoints of the
/// operands reference a variable, not a duplicated tree. Returns the
/// prefix statement(s) and the expression to use as the cotangent.
let internal bindCot (rc: RevCtx) (cot: Expr) : NStmt list * Expr =
    match cot.Kind with
    | ExprKind.ExprLit _ | ExprKind.ExprVar _ -> [], cot
    | _ ->
        let c = fresh rc.Ctx "__c"
        [NLet (c, false, cot)], inheritSpan cot (ExprVar c)

/// Adjoint statements for expression `e` with cotangent `cot`.
/// Every returned statement accumulates into a `__g_*` target.
let rec internal adjointOf (rc: RevCtx) (e: Expr) (cot: Expr) : Result<NStmt list, string> =
    match e.Kind with
    | ExprKind.ExprLit _ -> Ok []
    | ExprKind.ExprTyped (inner, _) -> adjointOf rc inner cot
    | ExprKind.ExprVar x ->
        if Set.contains x rc.Diff then Ok [accum (v (dName x)) cot] else Ok []
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, idxs) when Set.contains a rc.Arrays ->
        if Set.contains a rc.Diff then
            Ok [accum (inheritSpan e (ExprApp (v (dName a), idxs))) cot]
        else Ok []
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [u]) when isMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        (match derivRule name u with
         | None when Set.contains name zeroDerivIntrinsics -> Ok []   // floor/ceil
         | None ->
             // An intrinsic with no derivative rule. REFUSING is the point:
             // falling through to `Ok []` here would hand back a gradient that
             // silently drops this term (`digamma` would differentiate to zero).
             Error $"'{name}' has no derivative rule, so it cannot appear in a differentiated function (its derivative is not expressible in the AD-able subset). Compute it outside the function passed to ad.grad, or pass the value in as a parameter"
         | Some d ->
             let pre, c = bindCot rc cot
             adjointOf rc u (mul c d) |> Result.map (fun ss -> pre @ ss))
    // BINARY intrinsics: chain rule per operand via the shared partial table.
    // Without this arm both would fall to the unknown-call refusal below.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [a; b]) when isBinaryMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        let pre, c = bindCot rc cot
        let (dA, dB) = binaryDerivRule name a b
        adjointOf rc a (mul c dA) |> Result.bind (fun sa ->
        adjointOf rc b (mul c dB) |> Result.map (fun sb -> pre @ sa @ sb))
    // fma(a, b, c) = a*b + c: partials b, a, 1. The adjoint arithmetic is
    // unfused on purpose (see GradCommon.ternaryMathIntrinsics).
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [a; b; cc]) when isTernaryMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        let pre, c = bindCot rc cot
        adjointOf rc a (mul c b) |> Result.bind (fun sa ->
        adjointOf rc b (mul c a) |> Result.bind (fun sb ->
        adjointOf rc cc c |> Result.map (fun sc -> pre @ sa @ sb @ sc)))
    // A same-module user call the statement-level inliner did not reach.
    // `hoistCalls` walks only the arithmetic fragment, so a call wrapped in
    // `pure`/`compute`/`guard` (all of which the adjoint DOES walk through)
    // arrives here whole; substituting the callee's body and taking the
    // adjoint of THAT accumulates into the caller's own cotangent buffers by
    // the chain rule, exactly as the statement-level inline would have.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, args) when Map.containsKey name rc.Ctx.Decls ->
        (match kernelCallBody rc name args with
         | Error m -> Error m
         | Ok (body, rc') -> adjointOf rc' body cot)
    // Explicit numeric cast. An int-target cast (`Int64(floor(x))`) is
    // int-valued -- no gradient flows, same as extents. A float/complex
    // target is the identity linear map at AD's Float64 working width, so
    // the cotangent passes straight through to the operand (whose own
    // classification decides: `Float64(extents(xs))` bottoms out in the
    // int-valued extents and contributes nothing).
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [inner])
            when (Blade.Types.castTargetOf name).IsSome ->
        (match Blade.Types.castTargetOf name with
         | Some (Blade.Types.ETInt32 | Blade.Types.ETInt64) -> Ok []
         | _ -> adjointOf rc inner cot)
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, _) ->
        // Array read of non-diff data (a param, local, or module binding):
        // genuinely no adjoint. Anything ELSE named here is a call the
        // transform has no rule for -- `abs`, `min`, an imported function --
        // and REFUSING is the point: falling through to `Ok []` would hand
        // back a gradient that silently drops this term (the same
        // wrong-answer class as the digamma refusal above).
        if Set.contains name rc.Known then Ok []
        else
            Error $"cannot differentiate through '{name}': it is not a same-module function, a math intrinsic with a derivative rule, or an array in scope, so its contribution would silently vanish from the gradient. Compute it outside the function passed to ad.grad, or pass the value in as a parameter"
    | ExprKind.ExprUnaryOp (OpNeg, inner) -> adjointOf rc inner (neg cot)
    // C6: scalar pure/compute are materialization barriers -- the adjoint
    // passes straight through; a scalar guard gates its cotangent by the
    // SAME condition (emitted code may use if/else freely).
    | ExprKind.ExprPure inner | ExprKind.ExprCompute inner -> adjointOf rc inner cot
    | ExprKind.ExprGuard (c, inner) ->
        adjointOf rc inner (inheritSpan e (ExprIf (c, cot, fLit 0.0)))
    | ExprKind.ExprBinOp (_, OpAdd, l, r) ->
        adjointOf rc l cot |> Result.bind (fun sl ->
        adjointOf rc r cot |> Result.map (fun sr -> sl @ sr))
    | ExprKind.ExprBinOp (_, OpSub, l, r) ->
        adjointOf rc l cot |> Result.bind (fun sl ->
        adjointOf rc r (neg cot) |> Result.map (fun sr -> sl @ sr))
    | ExprKind.ExprBinOp (_, OpMul, l, r) ->
        let pre, c = bindCot rc cot
        adjointOf rc l (mul c r) |> Result.bind (fun sl ->
        adjointOf rc r (mul c l) |> Result.map (fun sr -> pre @ sl @ sr))
    | ExprKind.ExprBinOp (_, OpDiv, l, r) ->
        let pre, c = bindCot rc cot
        adjointOf rc l (div c r) |> Result.bind (fun sl ->
        adjointOf rc r (neg (div (mul c l) (mul r r))) |> Result.map (fun sr -> pre @ sl @ sr))
    | ExprKind.ExprBinOp (_, OpCaret, b, { Kind = ExprKind.ExprLit (LitInt n) }) when int n >= 0 ->
        // Constant natural exponent: emit the closed form directly rather
        // than routing through the general rule, which would leave a
        // pow(b, 2.0 - 1.0) in the output.
        let n' = int n
        if n' = 0 then Ok []
        else
            let dterm =
                if n' = 1 then fLit 1.0
                elif n' = 2 then mul (fLit 2.0) b
                else mul (fLit (float n')) (pow b (iLit (int64 (n' - 1))))
            adjointOf rc b (mul cot dterm)
    | ExprKind.ExprBinOp (_, OpCaret, b, e) ->
        // d(b^e) = e*b^(e-1) db  +  b^e*log(b) de.
        // The base term keeps the power form instead of the equivalent
        // e*b^e/b so that b = 0 stays finite. The log term is reachable
        // only when `e` is itself active -- a constant exponent takes the
        // ExprLit/inactive-var path to Ok [] and never evaluates log(b),
        // so a negative base still differentiates.
        let pre, c = bindCot rc cot
        adjointOf rc b (mul c (mul e (pow b (sub e (fLit 1.0))))) |> Result.bind (fun sb ->
        adjointOf rc e (mul c (mul (pow b e) (call "log" [b]))) |> Result.map (fun se ->
            pre @ sb @ se))
    | ExprKind.ExprBinOp (_, (OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe | OpAnd | OpOr), _, _) ->
        Ok []   // boolean-valued: no adjoint
    | ExprKind.ExprBinOp (_, OpMod, _, _) -> Ok []  // int-valued
    | ExprKind.ExprExtents _ -> Ok []  // int-valued size: no adjoint (the
                                       // tangent sweep's twin arm answers 0)
    | ExprKind.ExprArrayLit _ ->
        err rc.Fname "array literals may only appear as let initializers in differentiated code"
    | _ -> err rc.Fname "unsupported expression form in differentiated code (adjoint)"

/// C6: the adjoint of a COMBINATOR-built array local. `flow cotAt dims init`
/// accumulates the cotangent (read per-index through `cotAt`) back into the
/// operands' buffers by the form's TRANSPOSED reindexing:
///   alias      : straight copy loop
///   transpose  : the same swap (self-inverse)
///   stack      : rank-peel at the member's position
///   join d=0   : offset-shifted slices
///   pure/compute/typed : pass through
///   guard      : the cotangent gated by the SAME condition (emitted code
///                may use if/else freely; the input-side refusal is about
///                bodies the REVERSE sweep must re-evaluate, not output)
///   gram       : both adjoints are grams of transposes -- Tier-2, so the
///                BLAS route covers the backward pass too (probe-verified
///                to emit inside function bodies)
/// Operand positions must be named diff arrays (or nested supported forms);
/// anything else refuses with a named message.
///
/// Every arm but `gram` reads the cotangent THROUGH `cotAt`, the reindexer the
/// enclosing forms have composed. `gram`'s adjoints are whole-array `gram`
/// expressions, not per-index reads, so they cannot express an arbitrary
/// reindexing -- they read the cotangent BUFFER directly. Under a non-identity
/// `cotAt` that silently ignores the wrapper (a guard's gate, a transpose's
/// swap), which is why `cotIdent` is threaded explicitly rather than guessed
/// from the shape of `value`: only the identity reader may reach the gram arm.
let internal adjointOfInit (rc: RevCtx) (denv: Map<string, int list>) (xname: string) (value: Expr) : Result<NStmt list, string> option =
    let ctx = rc.Ctx
    // ROUTE G (docs/plans/structural/02, 3.3): the reverse rule of a
    // sole-halo stencil map is a GATHER. The kernel body is differentiated
    // ONCE against a symbolic interior ordinal i and a cotangent cell c;
    // every accumulation it produces targets either a windowed cell
    // `__g_x(i + K)` (K static, by the admissibility test) or a captured
    // scalar. Per (x, K), one loop over the INPUT ordinal j collects the
    // cotangent of output ordinal i = j - K WHEN that window exists: the
    // boundary is ZeroOutside -- outside [0, M) there is no window and the
    // cell is left as it is (the `else` re-assigns the same value; a `+ 0.0`
    // would not be the same, it flips a signed zero). The loops run in
    // DESCENDING K, the order the scatter would have written each cell in,
    // so the two routes accumulate in the same order and agree bitwise.
    // Scalar accumulators collect in one loop over the output ordinals.
    let haloGather (h: Blade.Types.HaloAccess) (n: int) (wname: string) (kbody: Expr) : Result<NStmt list, string> =
        let m = n - int h.Shrink
        let i = fresh ctx "__hi"
        let j = fresh ctx "__hj"
        let c = fresh ctx "__hc"
        substWindowReads rc.Fname wname h (v i) kbody |> Result.bind (fun body' ->
        adjointOf rc body' (v c) |> Result.bind (fun adj ->
        let temps = adj |> List.choose (function NLet (nm, mt, e) -> Some (nm, mt, e) | _ -> None)
        let assigns = adj |> List.choose (function NAssign (t, rhs) -> Some (t, rhs) | _ -> None)
        let windowTarget (t: Expr) =
            match t.Kind with
            | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar g },
                                [ { Kind = ExprKind.ExprBinOp (_, OpAdd, { Kind = ExprKind.ExprVar i' }, { Kind = ExprKind.ExprLit (LitInt k) }) } ])
                    when i' = i -> Some (g, int k)
            | _ -> None
        let addend (rhs: Expr) =
            match rhs.Kind with
            | ExprKind.ExprBinOp (_, OpAdd, _, t) -> Some t
            | _ -> None
        let isScalarTarget (t: Expr) = match t.Kind with ExprKind.ExprVar _ -> true | _ -> false
        let malformed =
            adj |> List.exists (function NFor _ -> true | _ -> false)
            || assigns |> List.exists (fun (t, rhs) ->
                   match windowTarget t with
                   | Some _ -> (addend rhs).IsNone
                   | None -> not (isScalarTarget t))
        if malformed then
            err rc.Fname "internal: the halo gather rule met an adjoint shape its admissibility test should have declined -- please report it"
        else
        let cotRead = syn (ExprApp (v (dName xname), [ v i ]))
        let inRange =
            syn (ExprBinOp (Elementwise, OpAnd,
                            syn (ExprBinOp (Elementwise, OpLe, iLit 0L, v i)),
                            syn (ExprBinOp (Elementwise, OpLt, v i, iLit (int64 m)))))
        let letOf (nm: string) (e: Expr) =
            StmtLet { Mutability = BindLet; Pattern = synPat (PatVar nm); Type = None; Value = e }
        let tempLets = temps |> List.map (fun (nm, _, e) -> letOf nm e)
        let gathers =
            assigns
            |> List.choose (fun (t, rhs) ->
                windowTarget t |> Option.bind (fun (g, k) -> addend rhs |> Option.map (fun a -> (g, k, a))))
            |> List.sortByDescending (fun (_, k, _) -> k)
            |> List.map (fun (g, k, a) ->
                let cell = syn (ExprApp (v g, [ v j ]))
                let collected = syn (ExprBlock (letOf c cotRead :: tempLets, Some (add cell a)))
                NFor (j, iLit 0L, iLit (int64 n),
                      [ NLet (i, false, sub (v j) (iLit (int64 k)))
                        NAssign (cell, syn (ExprIf (inRange, collected, cell))) ]))
        let scalars = assigns |> List.filter (fun (t, _) -> isScalarTarget t)
        let scalarLoop =
            if scalars.IsEmpty then []
            else
                [ NFor (i, iLit 0L, iLit (int64 m),
                        NLet (c, false, cotRead)
                        :: (temps |> List.map (fun (nm, mt, e) -> NLet (nm, mt, e)))
                        @ (scalars |> List.map NAssign)) ]
        Ok (gathers @ scalarLoop)))
    let accumInto (aname: string) (mkIdx: Expr list -> Expr list) (dims: int list) (cotAt: Expr list -> Expr) =
        if Set.contains aname rc.Diff then
            accumLoop ctx dims (fun idx -> syn (ExprApp (v (dName aname), mkIdx idx))) cotAt
        else []
    /// `cotIdent`: `cotAt` is still the plain `__g_<xname>(idx)` reader the
    /// dispatch below started from -- no enclosing form has wrapped it.
    let rec flow (cotIdent: bool) (cotAt: Expr list -> Expr) (dims: int list) (init: Expr) : Result<NStmt list, string> =
        match init.Kind with
        | ExprKind.ExprVar a -> Ok (accumInto a id dims cotAt)
        | ExprKind.ExprTyped (inner, _) | ExprKind.ExprCompute inner | ExprKind.ExprPure inner ->
            flow cotIdent cotAt dims inner
        | ExprKind.ExprGuard (c, inner) ->
            flow false (fun idx -> syn (ExprIf (c, cotAt idx, fLit 0.0))) dims inner
        | ExprKind.ExprTranspose (inner, d1, d2) ->
            let swap (xs: 'a list) =
                xs |> List.mapi (fun i x -> if i = d1 then xs.[d2] elif i = d2 then xs.[d1] else x)
            flow false (fun idx -> cotAt (swap idx)) (swap dims) inner
        // `sequence` is `stack` under another name -- same leading axis, same
        // lowering -- so the member peel is the same rule.
        | ExprKind.ExprStack es | ExprKind.ExprSequence es ->
            (match dims with
             | _ :: rest ->
                 es |> List.mapi (fun k e2 -> (k, e2))
                    |> traverseR (fun (k, e2) ->
                        flow false (fun idx -> cotAt (iLit (int64 k) :: idx)) rest e2)
                    |> Result.map List.concat
             | [] -> err rc.Fname "internal: stack initializer with no dims")
        // `replicate(n, body)` copies ONE body into every leading slot, so
        // every slot's cotangent flows back into that same body: n peels
        // accumulating into one operand, which is what `+=` already does.
        | ExprKind.ExprReplicate ({ Kind = ExprKind.ExprLit (LitInt n) }, body) ->
            (match dims with
             | _ :: rest ->
                 [ 0 .. int n - 1 ]
                 |> traverseR (fun k ->
                     flow false (fun idx -> cotAt (iLit (int64 k) :: idx)) rest body)
                 |> Result.map List.concat
             | [] -> err rc.Fname "internal: replicate initializer with no dims")
        | ExprKind.ExprJoin (parts, 0) ->
            (match dims with
             | _ :: rest ->
                 parts |> List.fold (fun acc part ->
                     acc |> Result.bind (fun (off, ss) ->
                         match staticDimsOf ctx denv part with
                         | Some (h :: _) ->
                             flow false
                                  (fun idx ->
                                      match idx with
                                      | lead :: tail -> cotAt (add lead (iLit (int64 off)) :: tail)
                                      | [] -> cotAt idx)
                                  (h :: rest) part
                             |> Result.map (fun s2 -> (off + h, ss @ s2))
                         | _ -> err rc.Fname "join parts need statically-known leading extents to differentiate (v1)"))
                     (Ok (0, []))
                 |> Result.map snd
             | [] -> err rc.Fname "internal: join initializer with no dims")
        | ExprKind.ExprGram _ when not cotIdent ->
            // The arm below reads `__g_<xname>` whole. Reaching it through a
            // wrapper would drop that wrapper's reindexing on the floor --
            // guard(FALSE, gram(a, b)) came back with the UNGATED adjoint.
            err rc.Fname "the adjoint of gram nested under transpose/guard/stack/join is not supported (v1); bind the gram to its own let first (`let g = gram(a, b)` then wrap `g`), which gives it an identity cotangent and differentiates today"
        | ExprKind.ExprGram (ga, gb) ->
            // direct operands only (v1): the adjoints read the PRIMAL
            // operands by name, and the cotangent buffer by name
            (match ga.Kind, gb.Kind with
             | ExprKind.ExprVar a, ExprKind.ExprVar b ->
                 (match Map.tryFind a denv, Map.tryFind b denv with
                  | Some dimsA, Some dimsB ->
                      let tr (x: string) = syn (ExprTranspose (v x, 0, 1))
                      let flows = ResizeArray<NStmt>()
                      if Set.contains a rc.Diff then
                          let tA = fresh ctx "__ga"
                          flows.Add (NLet (tA, false, syn (ExprGram (v (dName xname), tr b))))
                          for st in accumLoop ctx dimsA (fun idx -> syn (ExprApp (v (dName a), idx))) (fun idx -> syn (ExprApp (v tA, idx))) do flows.Add st
                      if Set.contains b rc.Diff then
                          let tB = fresh ctx "__gb"
                          flows.Add (NLet (tB, false, syn (ExprGram (tr (dName xname), tr a))))
                          for st in accumLoop ctx dimsB (fun idx -> syn (ExprApp (v (dName b), idx))) (fun idx -> syn (ExprApp (v tB, idx))) do flows.Add st
                      Ok (List.ofSeq flows)
                  | _ -> err rc.Fname "gram operands need statically-known dims to differentiate (v1)")
             | _ -> err rc.Fname "differentiating gram needs named array operands (v1); bind the operands first")
        | ExprKind.ExprGramApply _ when not cotIdent ->
            err rc.Fname "the adjoint of gram_apply nested under transpose/guard/stack/join is not supported (v1); bind it to its own let first (`let y = gram_apply(a, b, x)` then wrap `y`), which gives it an identity cotangent and differentiates today"
        | ExprKind.ExprGramApply (ga, gb, gx) ->
            // y = a (b^T x) over REAL named operands (v1, as gram's arm). With
            // the cotangent ybar read whole by name and t = b^T x (n cells):
            //   xbar += b (a^T ybar) = gram_apply(b, a, ybar)  -- the adjoint
            //                          ACTION, itself a gram_apply: no m x p
            //   abar(i, k) += ybar(i) * t(k)
            //   bbar(j, k) += x(j) * s(k),   s = a^T ybar (n cells)
            // The two n-cell vectors are per-column prodsums over the
            // factor's transpose (docs/plans/structural/05, 3.2); the
            // m x p Gram matrix is never formed, in the primal or here.
            (match ga.Kind, gb.Kind, gx.Kind with
             | ExprKind.ExprVar a, ExprKind.ExprVar b, ExprKind.ExprVar x ->
                 (match Map.tryFind a denv, Map.tryFind b denv, Map.tryFind x denv with
                  | Some dimsA, Some dimsB, Some dimsX ->
                      // t(k) = sum_j mat(j, k) * vec(j): a row map over mat's transpose.
                      let colDots (mat: string) (vec: string) =
                          let rname = fresh ctx "__gr"
                          syn (ExprCompute (syn (ExprBinOp (Elementwise, OpApply,
                                                             syn (ExprMethodFor [ syn (ExprTranspose (v mat, 0, 1)) ]),
                                                             syn (ExprLambda ([ { Name = rname; Type = None; Default = None; NameSpan = noSpan } ], None,
                                                                              syn (ExprApp (v "prodsum", [ v rname; v vec ]))))))))
                      let flows = ResizeArray<NStmt>()
                      if Set.contains x rc.Diff then
                          let tX = fresh ctx "__gx"
                          flows.Add (NLet (tX, false, syn (ExprGramApply (v b, v a, v (dName xname)))))
                          for st in accumLoop ctx dimsX (fun idx -> syn (ExprApp (v (dName x), idx))) (fun idx -> syn (ExprApp (v tX, idx))) do flows.Add st
                      if Set.contains a rc.Diff then
                          let tT = fresh ctx "__gt"
                          flows.Add (NLet (tT, false, colDots b x))
                          for st in accumLoop ctx dimsA
                                        (fun idx -> syn (ExprApp (v (dName a), idx)))
                                        (fun idx ->
                                            match idx with
                                            | [ i; k ] -> mul (syn (ExprApp (v (dName xname), [ i ]))) (syn (ExprApp (v tT, [ k ])))
                                            | _ -> fLit 0.0) do flows.Add st
                      if Set.contains b rc.Diff then
                          let tS = fresh ctx "__gs"
                          flows.Add (NLet (tS, false, colDots a (dName xname)))
                          for st in accumLoop ctx dimsB
                                        (fun idx -> syn (ExprApp (v (dName b), idx)))
                                        (fun idx ->
                                            match idx with
                                            | [ j; k ] -> mul (syn (ExprApp (v x, [ j ]))) (syn (ExprApp (v tS, [ k ])))
                                            | _ -> fLit 0.0) do flows.Add st
                      Ok (List.ofSeq flows)
                  | _ -> err rc.Fname "gram_apply operands need statically-known dims to differentiate (v1)")
             | _ -> err rc.Fname "differentiating gram_apply needs named array operands (v1); bind the operands first")
        // C7: the adjoint of a sort is the cotangent GATHERED through the
        // INVERSE permutation -- dA(j) += ds(invperm(j)). No scatter
        // primitive is needed: the inverse is a second sort the pre-pass
        // already materialized, so this is an ordinary data-dependent read,
        // and it reuses the SAME permutation the primal took (ties included).
        | ExprKind.ExprSort _ ->
            (match Map.tryFind xname rc.SortPlans with
             | Some plan when plan.InvPerm <> "" ->
                 if Set.contains plan.Src rc.Diff then
                     Ok (accumLoop ctx dims
                            (fun idx -> syn (ExprApp (v (dName plan.Src), idx)))
                            (fun idx -> cotAt [ syn (ExprApp (v plan.InvPerm, idx)) ]))
                 else Ok []
             | _ ->
                 err rc.Fname "internal: a differentiated `sort` reached the reverse sweep without its permutation plumbing -- this is a gap in the transform's sort expansion, please report it")
        | _ -> err rc.Fname "this combinator initializer has no reverse rule (v1); forward mode (ad.jvp) supports the wider set"
    // dispatch: only takes over for the combinator forms; literals and
    // scalar expressions keep their existing arms
    match value.Kind with
    | _ when haloGatherEnabled () && (haloGatherPlan ctx (fun n -> Map.tryFind n rc.LoopBindings) value).IsSome ->
        (match haloGatherPlan ctx (fun n -> Map.tryFind n rc.LoopBindings) value, Map.tryFind xname denv with
         | Some (h, n, w, kbody), Some [ _ ] -> Some (haloGather h n w kbody)
         | _ -> Some (err rc.Fname "internal: a halo stencil local reached the reverse sweep without its rank-1 shape -- please report it"))
    | ExprKind.ExprVar _ when Set.contains xname rc.Arrays ->
        // array ALIAS: cotangent flows whole-buffer (grad refused this
        // before C6 because no adjoint existed; now one does)
        (match Map.tryFind xname denv with
         | Some dims -> Some (flow true (fun idx -> syn (ExprApp (v (dName xname), idx))) dims value)
         | None -> None)
    | ExprKind.ExprTranspose _ | ExprKind.ExprStack _ | ExprKind.ExprJoin _
    | ExprKind.ExprGram _ | ExprKind.ExprGramApply _ | ExprKind.ExprSort _
    | ExprKind.ExprSequence _ | ExprKind.ExprReplicate _ ->
        (match Map.tryFind xname denv with
         | Some dims -> Some (flow true (fun idx -> syn (ExprApp (v (dName xname), idx))) dims value)
         | None -> Some (err rc.Fname "this combinator initializer needs statically-known dims to differentiate (v1)"))
    // pure/compute/guard over an ARRAY use the reindexing flow; the scalar
    // case falls through to adjointOf, which has pass-through arms
    | ExprKind.ExprGuard _ | ExprKind.ExprPure _ | ExprKind.ExprCompute _
            when Set.contains xname rc.Arrays ->
        (match Map.tryFind xname denv with
         | Some dims -> Some (flow true (fun idx -> syn (ExprApp (v (dName xname), idx))) dims value)
         | None -> Some (err rc.Fname "this combinator initializer needs statically-known dims to differentiate (v1)"))
    | _ -> None

/// Adjoint of one forward statement (statements arrive in REVERSE order).
let rec internal adjointOfStmt (rc: RevCtx) (s: NStmt) : Result<NStmt list, string> =
    match s with
    | NLet (x, _, value) ->
        if not (Set.contains x rc.Diff) then Ok []
        else
            // Array-literal init: flow each ELEMENT's adjoint from the
            // matching cotangent cell. Pure-literal elements contribute
            // nothing; nested literals recurse through curried reads.
            // (Treating the whole literal as a constant init -- the old
            // behavior -- silently dropped the gradient terms of any
            // ACTIVE element: `let a = [x, 2.0*x]` gave dx = 0.)
            let rec flowLit (cotCell: Expr) (elem: Expr) : Result<NStmt list, string> =
                let rec allLit (e: Expr) =
                    match e.Kind with
                    | ExprKind.ExprLit _ -> true
                    | ExprKind.ExprArrayLit es -> es |> List.forall allLit
                    | _ -> false
                if allLit elem then Ok []
                else
                    match elem.Kind with
                    | ExprKind.ExprArrayLit es ->
                        es |> List.mapi (fun j ej -> (j, ej))
                           |> traverseR (fun (j, ej) ->
                                flowLit (inheritSpan elem (ExprApp (cotCell, [iLit (int64 j)]))) ej)
                           |> Result.map List.concat
                    | _ -> adjointOf rc elem cotCell
            (match value with
             | { Kind = ExprKind.ExprArrayLit elems } ->
                 elems |> List.mapi (fun i el -> (i, el))
                       |> traverseR (fun (i, el) ->
                            flowLit (inheritSpan value (ExprApp (v (dName x), [iLit (int64 i)]))) el)
                       |> Result.map List.concat
             | ConstFill _ -> Ok []   // fill of a literal: nothing flows back
             | _ ->
                 // C6: combinator-built array locals flow through their
                 // form's transposed reindexing; everything else keeps the
                 // scalar-expression adjoint.
                 match adjointOfInit rc rc.Dims x value with
                 | Some r -> r
                 | None -> adjointOf rc value (v (dName x)))
    | NAssign (lhs, rhs) ->
        (match additiveSelf lhs rhs with
         | Some (sign, e) ->
             let cotBase =
                 match lhs.Kind with
                 | ExprKind.ExprVar x when Set.contains x rc.Diff -> Some (v (dName x))
                 | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, idxs) when Set.contains a rc.Diff ->
                     Some (inheritSpan lhs (ExprApp (v (dName a), idxs)))
                 | _ -> None
             match cotBase with
             | None -> Ok []
             | Some c ->
                 let cot = if sign < 0.0 then neg c else c
                 adjointOf rc e cot
         | None ->
             // general overwrite: save cotangent, zero it, then flow into rhs
             let target =
                 match lhs.Kind with
                 | ExprKind.ExprVar x when Set.contains x rc.Diff -> Some (v (dName x))
                 | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, idxs) when Set.contains a rc.Diff ->
                     Some (inheritSpan lhs (ExprApp (v (dName a), idxs)))
                 | _ -> None
             match target with
             | None -> Ok []
             | Some t ->
                 let c = fresh rc.Ctx "__c"
                 adjointOf rc rhs (inheritSpan lhs (ExprVar c)) |> Result.map (fun flow ->
                     [NLet (c, false, t); NAssign (t, fLit 0.0)] @ flow))
    | CarryLoop (_, t, lo, hi, _, _) & NFor (_, _, _, body) ->
        // The additive carry `s(t) = s(t-1) + INC` (GradNormalize.CarryLoop):
        // a scan, whose adjoint is the SAME loop run backwards. Every
        // per-step adjoint is the generic one -- general-overwrite saves and
        // zeros `__g_s(t)`, the array-read rule scatters the saved value into
        // `__g_s(t-1)`, INC's adjoint follows -- and descending order is what
        // makes `__g_s(t)` complete when step t reads it: the loss's direct
        // cotangent (deposited by later statements' adjoints, which run
        // first) plus the carry from step t+1 (deposited one iteration
        // earlier in this sweep). O(n), where the triangular unroll this
        // replaces was O(n^2) (docs/plans/structural/01, section 2.1).
        //
        // The body is replayed exactly as the generic arm replays it, and
        // the replay is a no-op here: at descending step t, `s(t-1)` is
        // final (written at forward step t-1, not yet touched by this sweep)
        // and INC reads only non-mutated inputs, so `s(t)` is rewritten with
        // its own value.
        let j = fresh rc.Ctx "__rj"
        let tLet = NLet (t, false, sub (sub hi (iLit 1L)) (v j))
        let localLets = body |> List.choose (fun s ->
            match s with
            | NLet (n, _, _) when Set.contains n rc.Diff -> Some n
            | _ -> None)
        let localCots =
            localLets |> List.map (fun n ->
                match body |> List.tryPick (fun s ->
                        match s with
                        | NLet (m, _, init) when m = n -> zerosLikeLiteral init
                        | _ -> None) with
                | Some z -> NLet (dName n, true, z)
                | None -> NLet (dName n, true, fLit 0.0))
        let folded =
            List.rev body
            |> traverseR (adjointOfStmt rc)
            |> Result.map List.concat
        folded |> Result.map (fun bodyAdjoints ->
            [NFor (j, iLit 0L, sub hi lo, tLet :: body @ localCots @ bodyAdjoints)])
    | NFor (var, lo, hi, body) ->
        // Same-direction adjoint loop: REPLAY THE WHOLE BODY (fresh
        // per-iteration values, including loop-local arrays filled by
        // interior construction loops), declare loop-local cotangents for
        // loop-local diff lets, then run the body's adjoints reversed.
        //
        // Replaying accumulations into variables that OUTLIVE the loop
        // corrupts them -- soundly: by reverse-sweep order every adjoint
        // that reads such a variable's forward value has already run
        // (post-loop statements' adjoints precede this loop's), and the
        // discipline checks ban pre-loop reads. Lets-only replay is NOT
        // enough: a loop-local array built by an interior loop would read
        // back as its zero literal.
        let localLets = body |> List.choose (fun s ->
            match s with
            | NLet (n, _, _) when Set.contains n rc.Diff -> Some n
            | _ -> None)
        let replay = body
        let localCots =
            localLets |> List.map (fun n ->
                match body |> List.tryPick (fun s ->
                        match s with
                        // literal or constant-fill initializer: zerosLike
                        // yields the matching zero shape (None for other
                        // forms, so tryPick keeps them scalar-defaulted)
                        | NLet (m, _, init) when m = n -> zerosLikeLiteral init
                        | _ -> None) with
                | Some z -> NLet (dName n, true, z)
                | None -> NLet (dName n, true, fLit 0.0))
        let folded =
            List.rev body
            |> traverseR (adjointOfStmt rc)
            |> Result.map List.concat
        folded |> Result.map (fun bodyAdjoints ->
            [NFor (var, lo, hi, replay @ localCots @ bodyAdjoints)])

// The forward (tangent) sweep -- ad.jvp
//
// Forward mode propagates tangents IN PROGRAM ORDER beside the primal:
// every differentiable binding/assignment gets a `__t_<name>` twin computed
// from the operands' tangents via the same derivative tables the adjoint
// uses. No reverse sweep, no cotangent buffers, no replay. Tangent
// assignments are emitted BEFORE their primal assignment so they read the
// pre-assignment primal values (d(x*x) needs the old x).

/// Tangent-name prefix for the CURRENT jvp synthesis. Defaults to `__t_`;
/// composition bumps it (`__t1_`, `__t2_`, ...) because a trusted source's
/// params already include the previous round's `__t_*` names -- `tName`
/// must stay injective against them or the second round's tangent of `x`
/// would collide with the first round's tangent parameter `__t_x`.
let internal tangentPrefix = ref "__t_"
let internal tName (n: string) = tangentPrefix.Value + n

/// Zero-folding expression builders. Folding `0.0` operands away matters
/// twice over: the emitted tangent stays readable, and -- decisively for the
/// general power rule -- a syntactically-zero tangent SUPPRESSES the term
/// entirely, so `log(b)` is never emitted for an inactive exponent (the same
/// reachability guarantee the adjoint's inactive-path routing provides).
let internal isZeroLit (e: Expr) =
    match e.Kind with
    | ExprKind.ExprLit (LitFloat 0.0) -> true
    | _ -> false
let internal addZ a b = if isZeroLit a then b elif isZeroLit b then a else add a b
let internal subZ a b = if isZeroLit b then a elif isZeroLit a then neg b else sub a b
let internal mulZ a b = if isZeroLit a || isZeroLit b then fLit 0.0 else mul a b
let internal divZ a b = if isZeroLit a then fLit 0.0 else div a b

/// Tangent of an expression, as an expression over primal names and
/// `__t_*` tangent names. Mirrors `adjointOf` case-for-case; refusals
/// match the adjoint's so both modes accept identical fragments (v1).
let rec internal tangentOfExpr (rc: RevCtx) (e: Expr) : Result<Expr, string> =
    // A bare-name loop side resolves through the let-bound loop objects.
    let resolveLoop (n: string) = Map.tryFind n rc.LoopBindings
    match e with
    | { Kind = ExprKind.ExprLit _ } -> Ok (fLit 0.0)
    | { Kind = ExprKind.ExprVar x } ->
        if Set.contains x rc.Diff then Ok (v (tName x)) else Ok (fLit 0.0)
    | { Kind = ExprKind.ExprTyped (inner, _) } -> tangentOfExpr rc inner
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, idxs) } when Set.contains a rc.Arrays ->
        if Set.contains a rc.Diff then Ok (inheritSpan e (ExprApp (v (tName a), idxs)))
        else Ok (fLit 0.0)
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [u]) } when isMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        (match derivRule name u with
         | None when Set.contains name zeroDerivIntrinsics -> Ok (fLit 0.0)   // floor/ceil
         | None ->
             // Through `err` (unlike the adjoint's raw Error) so the message
             // carries the jvp prefix the diagnostic boundary keys on.
             err rc.Fname $"'{name}' has no derivative rule, so it cannot appear in a differentiated function (its derivative is not expressible in the AD-able subset). Compute it outside the function passed to ad.{errMode.Value}, or pass the value in as a parameter"
         | Some d -> tangentOfExpr rc u |> Result.map (fun tu -> mulZ d tu))
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [a; b]) } when isBinaryMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        let (dA, dB) = binaryDerivRule name a b
        tangentOfExpr rc a |> Result.bind (fun ta ->
        tangentOfExpr rc b |> Result.map (fun tb -> addZ (mulZ dA ta) (mulZ dB tb)))
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [a; b; cc]) } when isTernaryMathIntrinsic name
                                       && not (Map.containsKey name rc.Ctx.Decls) ->
        // fma: tangent = b*ta + a*tb + tc (unfused, see GradCommon).
        tangentOfExpr rc a |> Result.bind (fun ta ->
        tangentOfExpr rc b |> Result.bind (fun tb ->
        tangentOfExpr rc cc |> Result.map (fun tc -> addZ (addZ (mulZ b ta) (mulZ a tb)) tc)))
    // A same-module user call the statement-level inliner did not reach --
    // the shape a KERNEL BODY produces, since `hoistCalls` stops at a lambda.
    // See `kernelCallBody`: substitute, then differentiate the result.
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, args) } when Map.containsKey name rc.Ctx.Decls ->
        kernelCallBody rc name args
        |> Result.bind (fun (body, rc') -> tangentOfExpr rc' body)
    // Explicit numeric cast: int targets are int-valued (literal-zero
    // tangent, same as extents); float/complex targets are the identity
    // linear map, so the tangent is the SAME cast applied to the operand's
    // tangent (keeping the tangent's width coherent with the primal's).
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, [inner]) }
            when (Blade.Types.castTargetOf name).IsSome ->
        (match Blade.Types.castTargetOf name with
         | Some (Blade.Types.ETInt32 | Blade.Types.ETInt64) -> Ok (fLit 0.0)
         | _ ->
             tangentOfExpr rc inner |> Result.map (fun t ->
                 if isZeroLit t then t
                 else mkExpr e.Span (ExprKind.ExprApp (mkExpr e.Span (ExprKind.ExprVar name), [t]))))
    | { Kind = ExprKind.ExprApp ({ Kind = ExprKind.ExprVar name }, _) } ->
        // A name that carries differentiable data but was never registered
        // as an ARRAY cannot be read as a constant: that combination means
        // the taint pass and the array tracker disagree, and returning zero
        // here is the silent-wrong-gradient failure mode. Refuse instead --
        // this fires on internal inconsistency, so it names the cause.
        if Set.contains name rc.Diff && not (Set.contains name rc.Arrays) then
            err rc.Fname $"internal: '{name}' carries differentiable data but is not tracked as an array, so its element read has no tangent rule -- this is a gap in the transform's array tracking, please report it"
        elif Set.contains name rc.Known then Ok (fLit 0.0)   // non-diff data read
        else
            err rc.Fname $"cannot differentiate through '{name}': it is not a same-module function, a math intrinsic with a derivative rule, or an array in scope, so its contribution would silently vanish from the gradient. Compute it outside the function passed to ad.{errMode.Value}, or pass the value in as a parameter"
    | { Kind = ExprKind.ExprUnaryOp (OpNeg, inner) } ->
        tangentOfExpr rc inner |> Result.map (fun t -> if isZeroLit t then t else neg t)
    | { Kind = ExprKind.ExprUnaryOp (OpNot, _) } -> Ok (fLit 0.0)
    | { Kind = ExprKind.ExprBinOp (_, OpAdd, l, r) } ->
        tangentOfExpr rc l |> Result.bind (fun tl ->
        tangentOfExpr rc r |> Result.map (fun tr -> addZ tl tr))
    | { Kind = ExprKind.ExprBinOp (_, OpSub, l, r) } ->
        tangentOfExpr rc l |> Result.bind (fun tl ->
        tangentOfExpr rc r |> Result.map (fun tr -> subZ tl tr))
    | { Kind = ExprKind.ExprBinOp (_, OpMul, l, r) } ->
        tangentOfExpr rc l |> Result.bind (fun tl ->
        tangentOfExpr rc r |> Result.map (fun tr -> addZ (mulZ tl r) (mulZ l tr)))
    | { Kind = ExprKind.ExprBinOp (_, OpDiv, l, r) } ->
        tangentOfExpr rc l |> Result.bind (fun tl ->
        tangentOfExpr rc r |> Result.map (fun tr ->
            subZ (divZ tl r) (divZ (mulZ l tr) (mul r r))))
    | { Kind = ExprKind.ExprBinOp (_, OpCaret, b, { Kind = ExprKind.ExprLit (LitInt n) }) } when int n >= 0 ->
        // Constant natural exponent: closed form (mirrors the adjoint arm).
        let n' = int n
        if n' = 0 then Ok (fLit 0.0)
        else
            tangentOfExpr rc b |> Result.map (fun tb ->
                let dterm =
                    if n' = 1 then fLit 1.0
                    elif n' = 2 then mul (fLit 2.0) b
                    else mul (fLit (float n')) (pow b (iLit (int64 (n' - 1))))
                mulZ dterm tb)
    | { Kind = ExprKind.ExprBinOp (_, OpCaret, bb, ee) } ->
        // d(b^e) = e*b^(e-1) db + b^e*log(b) de; mulZ suppresses the log
        // term when the exponent is inactive (see the builder note above).
        tangentOfExpr rc bb |> Result.bind (fun tb ->
        tangentOfExpr rc ee |> Result.map (fun te ->
            addZ (mulZ (mul ee (pow bb (sub ee (fLit 1.0)))) tb)
                 (mulZ (mul (pow bb ee) (call "log" [bb])) te)))
    | { Kind = ExprKind.ExprBinOp (_, (OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe | OpAnd | OpOr), _, _) } ->
        Ok (fLit 0.0)   // boolean-valued: no tangent
    | { Kind = ExprKind.ExprBinOp (_, OpMod, _, _) } -> Ok (fLit 0.0)  // int-valued
    | { Kind = ExprKind.ExprExtents _ } -> Ok (fLit 0.0)  // int-valued size
    // C8: an additive fold INSIDE a kernel body. The statement-level route
    // (`hoistReduces`) rewrites a reduce into an accumulator loop, but a
    // kernel body is an expression the pre-pass never descends into, so a
    // `reduce` over a rank-carrying kernel parameter arrives here whole.
    // Summation is linear, so the tangent is the fold of the tangents:
    // d reduce(S, (+), c) = reduce(dS, (+)) + dc. A syntactically zero
    // source tangent collapses the whole fold (the sum of an all-zero row is
    // zero WHATEVER the kernel, so this stays true if the kernel set widens).
    | { Kind = ExprKind.ExprReduce (src, kern, initOpt, axesOpt) } ->
        (match axesOpt with
         | Some _ ->
             err rc.Fname "reduce with an explicit `axes = n` is not differentiable inside a kernel body (v1): only the default fold `reduce(<operand>, (+)[, init])` is"
         | None ->
             match kern.Kind with
             | ExprKind.ExprSection OpAdd -> Ok ()
             | ExprKind.ExprSection _ ->
                 err rc.Fname "reduce inside a kernel body is differentiable with the additive section kernel `(+)` only (v1); the paired-fold rule the statement-level route uses for `(*)` needs a statement to carry the running product"
             | _ ->
                 err rc.Fname "reduce inside a kernel body is differentiable with section kernels only (v1); a lambda fold kernel is not")
        |> Result.bind (fun () ->
        tangentOfExpr rc src |> Result.bind (fun tsrc ->
        (match initOpt with
         | None -> Ok (fLit 0.0)
         | Some ie -> tangentOfExpr rc ie)
        |> Result.map (fun tinit ->
            if isZeroLit tsrc then tinit
            else addZ (inheritSpan e (ExprReduce (tsrc, kern, None, None))) tinit)))
    // gram is bilinear: d gram(A, B) = gram(dA, B) + gram(A, dB), with
    // inactive-operand terms folded away (an inactive ARRAY operand's
    // tangent is the scalar zero placeholder, which must not reach gram).
    | { Kind = ExprKind.ExprGram (ga, gb) } ->
        tangentOfExpr rc ga |> Result.bind (fun ta ->
        tangentOfExpr rc gb |> Result.map (fun tb ->
            let t1 = if isZeroLit ta then fLit 0.0 else syn (ExprGram (ta, gb))
            let t2 = if isZeroLit tb then fLit 0.0 else syn (ExprGram (ga, tb))
            addZ t1 t2))
    // gram_apply is trilinear: d[a (b^H x)] = gram_apply(da, b, x) +
    // gram_apply(a, db, x) + gram_apply(a, b, dx), inactive terms folded
    // away (a scalar-zero placeholder must not reach the node).
    | { Kind = ExprKind.ExprGramApply (ga, gb, gx) } ->
        tangentOfExpr rc ga |> Result.bind (fun ta ->
        tangentOfExpr rc gb |> Result.bind (fun tb ->
        tangentOfExpr rc gx |> Result.map (fun tx ->
            let t1 = if isZeroLit ta then fLit 0.0 else syn (ExprGramApply (ta, gb, gx))
            let t2 = if isZeroLit tb then fLit 0.0 else syn (ExprGramApply (ga, tb, gx))
            let t3 = if isZeroLit tx then fLit 0.0 else syn (ExprGramApply (ga, gb, tx))
            addZ (addZ t1 t2) t3)))
    | { Kind = ExprKind.ExprIf (c, t, f) } ->
        // Branch of tangents under the same condition (see walkExpr's arm).
        tangentOfExpr rc t |> Result.bind (fun tt ->
        tangentOfExpr rc f |> Result.map (fun tf ->
            inheritSpan e (ExprIf (c, tt, tf))))
    | ConstFill (cnt, _) -> Ok (zeroFill cnt)
    // C2-C5: the rank-0 MAP. See `tangentOfMap`. A bare-name loop side
    // resolves through the let-bound loop objects first.
    | MapApplyWith resolveLoop m ->
        tangentOfMap rc e m.Mode m.Ops m.Kern
    | { Kind = ExprKind.ExprBinOp (_, OpApply, _, _) } ->
        err rc.Fname "differentiating `<@>` supports `method_for(<operands>) <@> <kernel>` and `object_for(<kernel>) <@> <operands>` (v1, directly or through a let-bound loop object); pipelines and section kernels are not yet differentiable"
    // C1: same form, tangent operands (see the LinearForm doc comment).
    | LinearForm (ops, rebuild) ->
        ops |> traverseR (tangentOfExpr rc) |> Result.map rebuild
    | { Kind = ExprKind.ExprArrayLit _ } ->
        err rc.Fname "array literals may only appear as let initializers in differentiated code"
    | _ -> err rc.Fname "unsupported expression form in differentiated code (tangent)"

/// The C2-C5 MAP rule, shared by both spellings
/// (`method_for(ops) <@> kern` and `object_for(kern) <@> ops`).
///
/// The tangent is the SAME iteration over VIRTUAL operands, with values and
/// tangents read by index from the enclosing scope (capture-read; zip is
/// refused as one operand of a multi-array loop, so pairing is not
/// available for n >= 2):
///
///   * a named array operand becomes `range<its index types>`, its kernel
///     param becomes indexed reads `A(i...)` substituted into the body;
///   * a `halo<...>` or `range<...>` operand is already virtual -- it stays,
///     and its kernel param (window / index) is non-differentiable data;
///   * `reynolds(g[, Antisymmetric])` kernels are EXPANDED first --
///     sum_sigma sign * body[param_k := read_{sigma(k)}] -- so the ordinary
///     expression rule differentiates the symmetrized sum (correct under
///     the JOINT permutation: seeds travel with their values because the
///     substitution moves reads and, through them, tangent reads together);
///   * named-function and intrinsic kernels normalize to lambdas.
///
/// C8, rank-carrying kernel parameters: a parameter annotated `T^k` is bound
/// to a rank-k FIBER, so the loop iterates the operand's LEADING axes only
/// (its index types minus the trailing k) and the parameter's read is the
/// partial application `A(i...)`. This is what lets the comoment shapes --
/// `lambda(a: T^1, b: T^1) -> mean((a - mean(a)) * (b - mean(b)))` over the
/// rows of a 2-D table -- differentiate: the fiber's tangent is the same
/// partial application of `__t_A`, and the reductions inside the body
/// differentiate by the (linear) fold rule in `tangentOfExpr`.
///
/// C5, the symmetric fast path: when the kernel carries a STRUCTURAL
/// `where comm(...)` covering all params and every operand is the SAME
/// rank-1 array, the tangent loop runs over `range<SymIdx<r, N>>` --
/// canonical cells only, triangular storage, the full r! saving on the
/// tangent leg. tangent_joint_swap (proofs/BladeJacobian.v) licenses
/// exactly this: the tangent of a structurally symmetric primal is
/// invariant under the joint pair swap. The declared-comm gate is
/// load-bearing (semantic_hypothesis_insufficient refutes relaxing it),
/// and `range<SymIdx>` hands the kernel PREFIX OFFSETS, so canonical
/// indices are the prefix sums of the params.
///
/// Route A, arity-polymorphic kernels: a `Poly<T^k>` pack kernel is UNROLLED
/// against the operand count before anything else runs, so what the rules
/// below see is an ordinary fixed-arity lambda. The unrolled kernel keeps the
/// pack's `where comm(...)` -- expanded over the new parameter names -- which
/// is what lets `object_for(packprod) <@> (A, A)` keep the C5 triangular
/// tangent instead of quietly falling to the dense path.
and internal tangentOfMap (rc: RevCtx) (e: Expr) (bm: BinOpMode) (arrays: Expr list) (kern: Expr) : Result<Expr, string> =
    match tryUnrollPackKernel rc.Ctx rc.Fname kern (List.length arrays) with
    | Some (Error m) -> Error m
    | Some (Ok kern') -> tangentOfMapCore rc e bm arrays kern'
    | None -> tangentOfMapCore rc e bm arrays kern

/// The map rule proper, over a kernel that is already fixed-arity.
and internal tangentOfMapCore (rc: RevCtx) (e: Expr) (bm: BinOpMode) (arrays: Expr list) (kern: Expr) : Result<Expr, string> =
    let reMap k = inheritSpan e k
    // -- kernel normalization ------------------------------------------------
    // The four spellings live in `asKernelLambda` (shared with pipeline
    // fusion, so the two cannot drift on what counts as a kernel); only the
    // wording of the refusals is the map rule's own.
    let normKern =
        match asKernelLambda rc.Ctx kern with
        | Ok r -> Ok r
        | Error (KernBlockBody f) ->
            err rc.Fname (kernBlockBodyMsg f)
        | Error KernUnsupported ->
            err rc.Fname kernUnsupportedMsg
    normKern |> Result.bind (fun (ps, wc, body, reynoldsSign) ->
    if ps.Length <> arrays.Length || arrays.IsEmpty then
        err rc.Fname $"kernel arity {ps.Length} does not match {arrays.Length} loop operand(s) in differentiated code"
    else
    // -- C8: kernel parameter RANK -------------------------------------------
    // A kernel parameter annotated `T^k` (or `Array<T like I, ...>`) is bound
    // to a rank-k FIBER of its operand, not to an element: `lambda(row: T^1)`
    // over an `Array<Float like R, C>` sees one row per iteration. The map
    // rule then iterates only the LEADING axes -- the operand's index types
    // minus the trailing k the parameter absorbs -- and the parameter's read
    // is the partial application `A(i)`, whose tangent is the SAME partial
    // application of `__t_A` (the existing element-read arm never counted
    // indices, so a row read tangents for free).
    //
    // Unannotated is rank 0, which is what every pre-existing kernel is, so
    // the dense path below is byte-identical for them.
    //
    // `asKernelLambda` ERASES a named function's parameter types (pipeline
    // fusion rebuilds lambdas from those params and must not acquire
    // annotations the source never wrote), so a named kernel's ranks are read
    // back from its declaration rather than from `ps`.
    // `T^1` with a LITERAL rank parses to `TyVar (name, Some r)` (Parser.fs's
    // caret arm); `T<u>^1` and variable ranks (`T^r`) keep the
    // `TyAbstractArray` spelling, whose rank is an expression -- only a
    // literal one is readable here. A rank this cannot read stays 0, which is
    // the pre-existing behaviour and fails as a shape mismatch downstream
    // rather than as a wrong derivative.
    let rankOfTy (t: TypeExpr option) =
        match t |> Option.map (resolveTy rc.Ctx) with
        | Some (TyVar (_, Some r)) -> r
        | Some (TyAbstractArray (_, { Kind = ExprKind.ExprLit (LitInt r) }, _)) -> int r
        | Some (TyArray (_, its)) -> its.Length
        | _ -> 0
    let paramRanks =
        match kern.Kind with
        | ExprKind.ExprVar f when Map.containsKey f rc.Ctx.Decls
                                  && (rc.Ctx.Decls.[f].Params.Length = ps.Length) ->
            rc.Ctx.Decls.[f].Params |> List.map (fun p -> rankOfTy p.Type)
        | _ -> ps |> List.map (fun p -> rankOfTy p.Type)
    let allRank0 = paramRanks |> List.forall (fun k -> k = 0)
    // -- operand classification ---------------------------------------------
    // Named n -> (name, index types); Passthrough -> virtual operand kept as-is
    let classify (a: Expr) =
        // The same refusal for both ways an operand can fail to be one: not a
        // name at all, or a name with no declared index type.
        let notAnOperand () =
            err rc.Fname "differentiating a map needs each loop operand to be a named array PARAMETER with a declared index type (v1); bind the operand to a parameter, or compute it outside the differentiated function"
        match a.Kind with
        | ExprKind.ExprVar n ->
            (match Map.tryFind n rc.ArrayIdxTys with
             | Some idxTys when Set.contains n rc.Arrays -> Ok (Choice1Of2 (n, idxTys))
             | _ -> notAnOperand ())
        | ExprKind.ExprHalo _ | ExprKind.ExprRange _ -> Ok (Choice2Of2 a)
        | _ -> notAnOperand ()
    arrays |> traverseR classify
    |> Result.bind (fun classes ->
    // -- C5: the symmetric fast path -----------------------------------------
    let symCase =
        if reynoldsSign.IsSome then None
        else
            let names = classes |> List.map (function Choice1Of2 (n, its) -> Some (n, its) | _ -> None)
            if names |> List.exists Option.isNone then None
            else
                let names = names |> List.map Option.get
                let r = names.Length
                let allSame = r >= 2 && (names |> List.forall (fun (n, _) -> n = fst names.Head))
                let rank1 =
                    names |> List.forall (fun (_, its) ->
                        match its with
                        | [one] -> (match resolveTy rc.Ctx one with TyIdx { Kind = ExprKind.ExprLit (LitInt _) } -> true | _ -> false)
                        | _ -> false)
                let commAll =
                    match wc with
                    | Some w ->
                        w.Commutativity |> List.exists (fun group ->
                            Set.ofList group = Set.ofList (ps |> List.map _.Name))
                    | None -> false
                // `rank1` already excludes every multi-axis operand, so a
                // rank-carrying parameter cannot reach here anyway; the gate
                // is written out so the exclusion is a statement rather than
                // a consequence -- SymIdx prefix offsets index CELLS, and a
                // fiber read is not a cell.
                if allSame && rank1 && commAll && allRank0 then
                    let n =
                        match resolveTy rc.Ctx (snd names.Head |> List.head) with
                        | TyIdx { Kind = ExprKind.ExprLit (LitInt n) } -> int n
                        | _ -> 0
                    Some (fst names.Head, r, n)
                else None
    match symCase with
    | Some (aname, r, n) ->
        // canonical cells only: params are PREFIX OFFSETS, indices are their
        // prefix sums; no `where comm` on the emitted kernel (silently
        // discarded on range operands) and SymIdx spelled inline.
        let offNames = List.init r (fun _ -> fresh rc.Ctx "__cp")
        let canonical =
            offNames
            |> List.fold (fun (acc, prev) nm ->
                let ix = match prev with None -> v nm | Some p -> add p (v nm)
                (acc @ [ix], Some ix)) ([], None)
            |> fst
        List.zip ps canonical
        |> List.fold (fun accE (p, ix) ->
            accE |> Result.bind (substParam rc.Fname p.Name (syn (ExprApp (v aname, [ix])))))
            (Ok body)
        |> Result.bind (fun substituted ->
        tangentOfExpr rc substituted |> Result.map (fun tBody ->
            let symTy = TySymIdx (r, SymBaseExtent (iLit (int64 n)))
            let lamParams =
                offNames |> List.map (fun nm ->
                    { Name = nm; Type = None; Default = None; NameSpan = noSpan })
            reMap (ExprBinOp (bm, OpApply,
                              syn (ExprMethodFor [syn (ExprRange [symTy])]),
                              syn (ExprLambda (lamParams, None, tBody))))))
    | None ->
    // -- the uniform dense path ----------------------------------------------
    // reads per slot (Named) / kept params (Passthrough), then reynolds
    // expansion if requested, then the ordinary expression rule.
    // Each Named slot keeps the axes the LOOP iterates, which for a
    // rank-k parameter is the operand's index types minus the trailing k it
    // absorbs. `range<those>` is then the virtual operand and `A(i...)` the
    // partial application the parameter stands for.
    let slotsR =
        List.zip3 ps classes paramRanks
        |> traverseR (fun (p, c, k) ->
            match c with
            | Choice1Of2 (nm, idxTys) ->
                if k >= idxTys.Length then
                    err rc.Fname $"kernel parameter '{p.Name}' is declared rank {k} but its operand '{nm}' has {idxTys.Length} axis(es), so the map has no axis left to iterate; a kernel parameter's rank must be strictly less than its operand's"
                else
                    let loopTys = idxTys |> List.truncate (idxTys.Length - k)
                    let ixs = loopTys |> List.map (fun _ -> fresh rc.Ctx "__ci")
                    Ok (Choice1Of2 (p, nm, loopTys, ixs))
            | Choice2Of2 a ->
                if k > 0 then
                    err rc.Fname $"kernel parameter '{p.Name}' is declared rank {k}, but its loop operand is a `halo`/`range` traversal, which hands the kernel a window or an index rather than an array fiber; rank-carrying kernel parameters are differentiable over NAMED array operands only (v1)"
                else Ok (Choice2Of2 (p, a)))
    slotsR |> Result.bind (fun slots ->
    let readOf slot =
        match slot with
        | Choice1Of2 (_, nm, _, ixs) -> Some (syn (ExprApp (v nm, ixs |> List.map v)))
        | Choice2Of2 _ -> None
    let expandedBody =
        match reynoldsSign with
        | None ->
            slots |> List.fold (fun accE slot ->
                match slot with
                | Choice1Of2 (p, _, _, _) ->
                    accE |> Result.bind (substParam rc.Fname p.Name (readOf slot |> Option.get))
                | Choice2Of2 _ -> accE) (Ok body)
        | Some isAnti ->
            // reynolds needs every slot readable (a window has no permuted read)
            if slots |> List.exists (function Choice2Of2 _ -> true | _ -> false) then
                err rc.Fname "reynolds kernels over halo/range operands are not differentiable (v1)"
            // Symmetrization permutes READS across slots. With rank-carrying
            // parameters the slots' reads are fibers, and nothing here proves
            // the permuted fiber has the shape the receiving parameter
            // declares -- so it is refused rather than guessed.
            elif not allRank0 then
                err rc.Fname "reynolds kernels whose parameters carry a rank (`T^k`) are not differentiable (v1): symmetrization permutes reads between slots, which is only shape-safe for rank-0 element reads"
            else
                let reads = slots |> List.map (readOf >> Option.get)
                let rec perms xs =
                    match xs with
                    | [] -> [[]]
                    | _ -> xs |> List.collect (fun x ->
                            perms (List.filter ((<>) x) xs) |> List.map (fun p -> x :: p))
                let parity (p: int list) =
                    let arr = List.toArray p
                    let mutable inv = 0
                    for i in 0 .. arr.Length - 2 do
                        for j in i + 1 .. arr.Length - 1 do
                            if arr.[i] > arr.[j] then inv <- inv + 1
                    inv % 2 = 0
                let termsR =
                    perms [0 .. reads.Length - 1]
                    |> traverseR (fun perm ->
                        List.zip ps perm
                        |> List.fold (fun accE (p, srcSlot) ->
                            accE |> Result.bind (substParam rc.Fname p.Name reads.[srcSlot]))
                            (Ok body)
                        |> Result.map (fun t -> (parity perm, t)))
                termsR |> Result.map (fun terms ->
                    match terms with
                    | [] -> fLit 0.0
                    | (firstEven, firstT) :: rest ->
                        let init = if not isAnti || firstEven then firstT else neg firstT
                        rest |> List.fold (fun accE (even, t) ->
                            if not isAnti || even then add accE t else sub accE t) init)
    expandedBody |> Result.bind (fun bodyExpanded ->
    let passthroughNames =
        slots |> List.choose (function Choice2Of2 (p, _) -> Some p.Name | _ -> None) |> Set.ofList
    let rc' = { rc with Known = Set.union rc.Known passthroughNames }
    tangentOfExpr rc' bodyExpanded |> Result.map (fun tBody ->
        let operandsOut =
            slots |> List.map (function
                | Choice1Of2 (_, _, idxTys, _) -> syn (ExprRange idxTys)
                | Choice2Of2 (_, a) -> a)
        let lamParams =
            slots |> List.collect (function
                | Choice1Of2 (_, _, _, ixs) ->
                    ixs |> List.map (fun nm -> { Name = nm; Type = None; Default = None; NameSpan = noSpan })
                | Choice2Of2 (p, _) -> [p])
        reMap (ExprBinOp (bm, OpApply,
                          syn (ExprMethodFor operandsOut),
                          syn (ExprLambda (lamParams, None, tBody)))))))))

/// Elementwise tangent of a (possibly nested) array-literal initializer.
let rec internal tangentOfLit (rc: RevCtx) (e: Expr) : Result<Expr, string> =
    match e.Kind with
    | ExprKind.ExprArrayLit elems ->
        elems |> traverseR (tangentOfLit rc)
        |> Result.map (fun ts -> inheritSpan e (ExprArrayLit ts))
    | _ -> tangentOfExpr rc e

/// The loop-object environment AFTER `s`. Loop-object bindings are resolved at
/// their APPLICATION sites, so the environment has to be position-aware: a body
/// may re-`let` the same name over a different loop, and the sweep is what
/// decides which one an apply site sees.
///
/// Both wrong answers are reachable from a whole-body map. First-wins (the
/// predecessor) resolved the SECOND `let L = method_for(b)` to the first loop,
/// so `r2`'s tangent iterated `a`. Last-wins would break the first apply the
/// same way. Only "the latest binding preceding this statement" is right, and
/// that is what folding this over the statement list gives.
///
/// A `let` that rebinds the name to something that is NOT a loop object must
/// REMOVE it, or the stale loop would keep answering.
let internal noteLoopBinding (rc: RevCtx) (s: NStmt) : RevCtx =
    match s with
    | NLet (n, _, ({ Kind = ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _ } as value)) ->
        { rc with LoopBindings = Map.add n value rc.LoopBindings }
    | NLet (n, _, _) when Map.containsKey n rc.LoopBindings ->
        { rc with LoopBindings = Map.remove n rc.LoopBindings }
    | _ -> rc

/// Sweep a statement list in order, threading the loop-object environment.
/// Returns the context AFTER the list (the final expression's tangent is built
/// against it) and the interleaved statements.
let rec internal tangentOfStmts (rc: RevCtx) (ss: NStmt list) : Result<RevCtx * NStmt list, string> =
    ss |> List.fold (fun acc s ->
        acc |> Result.bind (fun (rcCur, out) ->
            tangentOfStmt rcCur s |> Result.map (fun s2 -> (noteLoopBinding rcCur s, s2 :: out))))
        (Ok (rc, []))
    |> Result.map (fun (rcEnd, out) -> (rcEnd, out |> List.rev |> List.concat))

/// Tangent-interleaved form of one forward statement. The tangent
/// ASSIGNMENT precedes its primal so it reads pre-assignment values;
/// tangent LETS follow their primal (the tangent reads operands bound
/// earlier, never the freshly-bound name).
and internal tangentOfStmt (rc: RevCtx) (s: NStmt) : Result<NStmt list, string> =
    match s with
    // A let-bound loop object gets no tangent binding: it is a deferred
    // iteration, not data. Its tangent materializes at the application
    // site, where the OpApply rule resolves the binding.
    | NLet (_, _, { Kind = ExprKind.ExprMethodFor _ | ExprKind.ExprObjectFor _ }) -> Ok [s]
    // C7: the tangent of a sort is the CO-GATHER of the source's tangent
    // through the SAME permutation -- ds(i) = dA(perm(i)). Sharing `perm` by
    // name with the primal is what makes the two legs agree at ties (both
    // inherit std::stable_sort's input-order convention). Built here rather
    // than in `tangentOfExpr` because the permutation is per-BINDING data:
    // the plan is keyed on the bound name.
    | NLet (x, isMut, { Kind = ExprKind.ExprSort _ }) when Set.contains x rc.Diff ->
        (match Map.tryFind x rc.SortPlans with
         | Some plan when Set.contains plan.Src rc.Diff ->
             Ok [s; NLet (tName x, isMut, permGather plan.Perm plan.IdxTy (tName plan.Src))]
         | Some _ -> Ok [s]   // sorted array carries no tangent
         | None ->
             err rc.Fname "internal: a differentiated `sort` reached the tangent sweep without its permutation plumbing -- this is a gap in the transform's sort expansion, please report it")
    | NLet (x, isMut, value) ->
        if not (Set.contains x rc.Diff) then Ok [s]
        else
            (match value with
             | { Kind = ExprKind.ExprArrayLit _ } ->
                 tangentOfLit rc value |> Result.map (fun t -> [s; NLet (tName x, isMut, t)])
             // gram: bind each bilinear term to its own temp -- an
             // array-add whose operands are gram NODES (not vars) hits the
             // flat-elementwise emitter's unnamed-operand hazard
             // (IR.fs's `arr0` note), and named temps keep both terms on
             // the BLAS route besides.
             | { Kind = ExprKind.ExprGram (ga, gb) } ->
                 tangentOfExpr rc ga |> Result.bind (fun ta ->
                 tangentOfExpr rc gb |> Result.map (fun tb ->
                     match isZeroLit ta, isZeroLit tb with
                     | true, true -> [s]
                     | false, true -> [s; NLet (tName x, isMut, syn (ExprGram (ta, gb)))]
                     | true, false -> [s; NLet (tName x, isMut, syn (ExprGram (ga, tb)))]
                     | false, false ->
                         let t1 = fresh rc.Ctx "__gt"
                         let t2 = fresh rc.Ctx "__gt"
                         [ s
                           NLet (t1, false, syn (ExprGram (ta, gb)))
                           NLet (t2, false, syn (ExprGram (ga, tb)))
                           NLet (tName x, isMut, add (v t1) (v t2)) ]))
             // gram_apply: the same per-term temps, three terms.
             | { Kind = ExprKind.ExprGramApply (ga, gb, gx) } ->
                 tangentOfExpr rc ga |> Result.bind (fun ta ->
                 tangentOfExpr rc gb |> Result.bind (fun tb ->
                 tangentOfExpr rc gx |> Result.map (fun tx ->
                     let terms =
                         [ (ta, (fun () -> syn (ExprGramApply (ta, gb, gx))))
                           (tb, (fun () -> syn (ExprGramApply (ga, tb, gx))))
                           (tx, (fun () -> syn (ExprGramApply (ga, gb, tx)))) ]
                         |> List.filter (fun (t, _) -> not (isZeroLit t))
                         |> List.map (fun (_, mk) -> mk ())
                     match terms with
                     | [] -> [s]
                     | [ one ] -> [s; NLet (tName x, isMut, one)]
                     | many ->
                         let temps = many |> List.map (fun e -> (fresh rc.Ctx "__gt", e))
                         let total = temps |> List.map (fun (nm, _) -> v nm) |> List.reduce add
                         [s] @ (temps |> List.map (fun (nm, e) -> NLet (nm, false, e))) @ [ NLet (tName x, isMut, total) ])))
             | ConstFill (cnt, _) -> Ok [s; NLet (tName x, isMut, zeroFill cnt)]
             | _ -> tangentOfExpr rc value |> Result.map (fun t -> [s; NLet (tName x, isMut, t)]))
    | NAssign (lhs, rhs) ->
        let tangentLhs =
            match lhs.Kind with
            | ExprKind.ExprVar x when Set.contains x rc.Diff -> Some (v (tName x))
            | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar a }, idxs) when Set.contains a rc.Diff ->
                Some (inheritSpan lhs (ExprApp (v (tName a), idxs)))
            | _ -> None
        (match tangentLhs with
         | None -> Ok [s]   // non-diff target: rhs is inactive by taint
         | Some tl ->
             tangentOfExpr rc rhs |> Result.map (fun tr -> [NAssign (tl, tr); s]))
    | NFor (var, lo, hi, body) ->
        // the body's own bindings are scoped to it, so the threaded context is
        // discarded at the closing brace
        tangentOfStmts rc body
        |> Result.map (fun (_, body') -> [NFor (var, lo, hi, body')])

// NStmt -> Stmt conversion

let rec internal toStmts (ns: NStmt list) : Stmt list =
    ns |> List.map (fun s ->
        match s with
        | NLet (n, isMut, value) ->
            StmtLet { Pattern = synPat (PatVar n)
                      Type = None
                      Value = value
                      Mutability = if isMut then BindMut else BindLet }
        | NAssign (lhs, rhs) -> StmtExpr (mkExpr (mergeSpan lhs.Span rhs.Span) (ExprAssign (lhs, rhs)))
        | NFor (var, lo, hi, body) -> StmtForIn (var, mkExpr (mergeSpan lo.Span hi.Span) (ExprDotDot (lo, hi)), toStmts body))

// The shared prelude
//
// Both sweeps need the same five things before they can differentiate
// anything, in the same order, with the same refusals: the parameters
// classified, the body normalized and inlined, the reserved synthesized
// names proved free, every expression in the fragment validated, and the
// name/type environments the sweep context is built from. The two drivers
// used to carry a verbatim copy of all of it -- including the refusal
// STRINGS, which is the kind of duplication that makes a message fix land
// in one mode only.

/// Everything `prepareForSweeps` recovers. `FinalE` is the fragment's final
/// expression as `normalizeBody` produced it; `SweepFinal` is what the mode
/// actually walks and differentiates, which for a tuple-returning jvp source
/// is a SURROGATE (the sum of the components) rather than the tuple node.
type internal Prepared = {
    Classes: (ParamDecl * ParamClass) list
    DiffParams: Set<string>
    ArrayParams: Set<string>
    Stmts: NStmt list
    FinalE: Expr
    SweepFinal: Expr
    Known: Set<string>
    ArrayIdxTys: Map<string, TypeExpr list>
    SortPlans: Map<string, SortPlan>
}

/// The prelude. `sweepFinalOf` is the mode's choice of what to validate and
/// differentiate (grad: the final expression itself); `onNames` is the one
/// hook that has to run BETWEEN collecting the fragment's binder names and
/// the reserved-name gate (jvp picks its depth-indexed tangent prefix off
/// those names, and picks it whether or not the gate then refuses);
/// `skipReservedGate` is jvp's trusted-source exemption.
let internal prepareForSweeps (ctx: Ctx) (fd: FunctionDecl)
                             (sweepFinalOf: Expr -> Expr)
                             (onNames: string list -> unit)
                             (skipReservedGate: bool)
    : Result<Prepared, string> =
    let fname = fd.Name
    fd.Params
    |> traverseR (fun p -> classifyParam fname ctx p |> Result.map (fun c -> (p, c)))
    |> Result.bind (fun classes ->
    let diffParams =
        classes |> List.choose (fun (p, c) ->
            match c with DiffArray | DiffScalar -> Some p.Name | NonDiff -> None)
        |> Set.ofList
    let arrayParams =
        classes |> List.choose (fun (p, c) ->
            match c with DiffArray -> Some p.Name | _ -> None)
        |> Set.ofList
    if Set.isEmpty diffParams then
        err fname "no differentiable (Float or Float-array) parameters"
    else
    normalizeBody fname ctx 0 fd |> Result.bind (fun (stmts, finalE) ->
    // Reserved-name gate. The transform's deterministic synthesized names
    // (`__g_<x>` cotangents, `__t_<x>` forward-mode tangents, `__primal`)
    // would silently SHADOW a same-named user binding: the synthesized
    // zero-initialized declaration wins, and every derivative expression that
    // should read the user's value reads the zeroed cotangent instead --
    // a wrong gradient with no diagnostic. (Inlined callee locals are
    // exempt by construction: renaming gives them an `__in<N>_` prefix.)
    // A TRUSTED source -- one this pass itself synthesized, in a composition
    // chain -- binds reserved names by construction and skips the gate.
    let reservedName (n: string) =
        n = "__primal" || n.StartsWith "__g_" || n.StartsWith "__t_"
    let allNames = (fd.Params |> List.map _.Name) @ boundNames stmts
    onNames allNames
    match (if skipReservedGate then None else allNames |> List.tryFind reservedName) with
    | Some n ->
        err fname $"binding or parameter '{n}' collides with a reserved AD name (`__g_*`, `__t_*`, and `__primal` are synthesized by the transform and would shadow it); rename it"
    | None ->
    // validate every expression in the fragment
    let sweepFinal = sweepFinalOf finalE
    let validateAll =
        let rec valStmts ss =
            ss |> iterR (fun s ->
                match s with
                | NLet (_, _, e) -> walkExpr fname ctx ignore false e
                | NAssign (l, r) ->
                    walkExpr fname ctx ignore false l
                    |> Result.bind (fun () -> walkExpr fname ctx ignore false r)
                | NFor (_, lo, hi, body) ->
                    walkExpr fname ctx ignore false lo
                    |> Result.bind (fun () -> walkExpr fname ctx ignore false hi)
                    |> Result.bind (fun () -> valStmts body))
        valStmts stmts |> Result.bind (fun () -> walkExpr fname ctx ignore false sweepFinal)
    validateAll |> Result.map (fun () ->
    { Classes = classes
      DiffParams = diffParams
      ArrayParams = arrayParams
      Stmts = stmts
      FinalE = finalE
      SweepFinal = sweepFinal
      Known =
        Set.unionMany [
            fd.Params |> List.map _.Name |> Set.ofList
            boundNames stmts |> Set.ofList
            ctx.ModuleVals ]
      ArrayIdxTys =
        fd.Params |> List.choose (fun p ->
            match p.Type |> Option.map (resolveTy ctx) with
            | Some (TyArray (_, idxTys)) when not idxTys.IsEmpty -> Some (p.Name, idxTys)
            | _ -> None)
        |> Map.ofList
      SortPlans = collectSortPlans stmts })))

