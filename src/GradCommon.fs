// The AD transform's shared substrate: traversal plumbing, the math-
// intrinsics whitelists, tiny expression builders, derivative rules, the
// NStmt/Ctx normalization types, error reporting (errMode: grad vs jvp),
// kernel substitution, and statement conversion. Everything later in the
// Grad* chain builds on this file.
module Blade.GradCommon

open Blade.Ast

let internal traverseR (f: 'a -> Result<'b, string>) (xs: 'a list) : Result<'b list, string> =
    let rec go acc rest =
        match rest with
        | [] -> Ok (List.rev acc)
        | x :: tl ->
            match f x with
            | Ok y -> go (y :: acc) tl
            | Error e -> Error e
    go [] xs

/// `traverseR` for effects only: run `f` over every element in order,
/// stopping at the first `Error`.
let internal iterR (f: 'a -> Result<unit, string>) (xs: 'a list) : Result<unit, string> =
    let rec go rest =
        match rest with
        | [] -> Ok ()
        | x :: tl ->
            match f x with
            | Ok _ -> go tl
            | Error e -> Error e
    go xs

/// The Option twin: any element's `None` DECLINES the whole list (the
/// decline discipline `substKern` and the loop-key readers are built on).
let internal traverseO (f: 'a -> 'b option) (xs: 'a list) : 'b list option =
    let rec go acc rest =
        match rest with
        | [] -> Some (List.rev acc)
        | x :: tl ->
            match f x with
            | Some y -> go (y :: acc) tl
            | None -> None
    go [] xs

// Math intrinsics -- the single source of truth (TypeCheck reads these too)

/// Scalar math intrinsics recognized as plain calls (`exp(x)`) when the name
/// is not user-bound. Unary, real-valued, rendered as std::<name> in C++ --
/// except `lgamma` and `digamma`, which render as `blade_rt::<name>`
/// (CodeGen.unaryOpToCpp) because they have no bit-exact interpreter twin in
/// any shared library; see src/cpp/blade_runtime.hpp.
/// Keep in sync with StaticEval.evalBuiltin and derivRule below.
let mathIntrinsics : Set<string> =
    Set.ofList [
        "exp"; "log"; "log10"; "sqrt"
        "sin"; "cos"; "tan"
        "sinh"; "cosh"; "tanh"
        "asin"; "acos"; "atan"
        "floor"; "ceil"
        "lgamma"; "digamma"
    ]

let isMathIntrinsic (name: string) : bool = Set.contains name mathIntrinsics

/// BINARY math intrinsics -- plain two-argument calls (`atan2(y, x)`) that
/// TypeCheck rewrites to `TExprBinOp (Elementwise, OpMath2 name, ...)` when the
/// name is not user-bound. Deliberately a SEPARATE set from `mathIntrinsics`:
/// that one is documented unary and `TypeCheck.isUnaryIntrinsic` /
/// `etaExpandFunctionKernel` read it for an arity they cannot otherwise
/// recover, so widening it would eta-expand `atan2` to one parameter.
/// Real-only (neither has a std::complex overload), and their result is
/// always dimensionless -- see TypeCheck.unitRulesForOp's OpMath2 arms.
/// Keep in sync with StaticEval.evalBuiltin, `adjointOf`'s binary-intrinsic
/// arm below, and CodeGen's IRMath2 rendering.
let binaryMathIntrinsics : Set<string> =
    Set.ofList [ "atan2"; "log_base" ]

let isBinaryMathIntrinsic (name: string) : bool = Set.contains name binaryMathIntrinsics

/// TERNARY math intrinsics: `fma(a, b, c)` = a*b + c rounded once. TypeCheck
/// types it as its own TExprFma node (never a binop chain, so no pass can
/// split the fusion). For AD it is exactly a*b + c: d/da = b, d/db = a,
/// d/dc = 1 -- the sweeps below differentiate it through those partials,
/// and the derivative code is ordinary (unfused) arithmetic, which is the
/// right call: a gradient does not need the single rounding, the forward
/// value does. Keep in sync with StaticEval.evalBuiltin's fma arm and the
/// ide `surface` census.
let ternaryMathIntrinsics : Set<string> =
    Set.ofList [ "fma" ]

let isTernaryMathIntrinsic (name: string) : bool = Set.contains name ternaryMathIntrinsics

/// Subset of the intrinsics that have std::complex overloads in <complex>
/// and so are permitted on complex operands (result is complex, same
/// width). exp/log/sqrt and the trig/hyperbolic families qualify; floor/
/// ceil do not (no complex overload, stay real-only and reject complex).
/// Differentiation of complex intrinsics is out of scope.
let complexMathIntrinsics : Set<string> =
    Set.ofList [
        "exp"; "log"; "sqrt"
        "sin"; "cos"; "tan"
        "sinh"; "cosh"; "tanh"
        "asin"; "acos"; "atan"
    ]

let isComplexMathIntrinsic (name: string) : bool = Set.contains name complexMathIntrinsics

// Expression construction helpers. Span-free derivative-synthesis builders:
// `syn` stamps each node with the ambient `synthSpan` (set to the
// differentiated decl's span in expand).
let internal fLit (v: float) = syn (ExprLit (LitFloat v))
let internal iLit (n: int64) = syn (ExprLit (LitInt n))
let internal v (name: string) = syn (ExprVar name)
let internal add a b = syn (ExprBinOp (Elementwise, OpAdd, a, b))
let internal sub a b = syn (ExprBinOp (Elementwise, OpSub, a, b))
let internal mul a b = syn (ExprBinOp (Elementwise, OpMul, a, b))
let internal div a b = syn (ExprBinOp (Elementwise, OpDiv, a, b))
let internal pow a b = syn (ExprBinOp (Elementwise, OpCaret, a, b))
let internal neg a = syn (ExprUnaryOp (OpNeg, a))
let internal call name args = syn (ExprApp (v name, args))

/// Intrinsics whose derivative is IDENTICALLY ZERO (a.e.), so `derivRule`
/// returning None for them means "contributes nothing", not "unknown". Split
/// out from derivRule's None because those two readings must not be confused:
/// an intrinsic with no rule that is silently treated as zero-derivative
/// yields a WRONG gradient with no diagnostic, which is worse than a refusal.
/// adjointOf refuses anything outside this set that derivRule cannot handle.
let internal zeroDerivIntrinsics : Set<string> = Set.ofList [ "floor"; "ceil" ]

/// d/du of intrinsic(u), as a function of the FORWARD expression u.
/// Returns None for the zero-derivative intrinsics above AND for any
/// intrinsic with no rule yet (`digamma`: its derivative is the trigamma
/// function, which the language does not have) -- adjointOf tells the two
/// apart via zeroDerivIntrinsics and refuses the latter.
let internal derivRule (name: string) (u: Expr) : Expr option =
    match name with
    | "exp" -> Some (call "exp" [u])
    | "log" -> Some (div (fLit 1.0) u)
    // d/du log10(u) = 1/(u ln 10). `log(10.0)` is left symbolic rather than
    // spelled as a decimal so the emitted derivative carries the same rounding
    // as the forward pass's std::log10 base; both back ends constant-fold it.
    | "log10" -> Some (div (fLit 1.0) (mul u (call "log" [fLit 10.0])))
    | "sqrt" -> Some (div (fLit 1.0) (mul (fLit 2.0) (call "sqrt" [u])))
    | "sin" -> Some (call "cos" [u])
    | "cos" -> Some (neg (call "sin" [u]))
    | "tan" -> Some (div (fLit 1.0) (mul (call "cos" [u]) (call "cos" [u])))
    | "sinh" -> Some (call "cosh" [u])
    | "cosh" -> Some (call "sinh" [u])
    | "tanh" -> Some (sub (fLit 1.0) (mul (call "tanh" [u]) (call "tanh" [u])))
    | "asin" -> Some (div (fLit 1.0) (call "sqrt" [sub (fLit 1.0) (mul u u)]))
    | "acos" -> Some (neg (div (fLit 1.0) (call "sqrt" [sub (fLit 1.0) (mul u u)])))
    | "atan" -> Some (div (fLit 1.0) (add (fLit 1.0) (mul u u)))
    // d/dx log Gamma(x) = psi(x). The chain rule closes here because digamma
    // is itself an intrinsic (blade_rt::digamma), so the emitted adjoint is
    // an ordinary Blade expression the rest of the pipeline already handles.
    // This is what makes gamma / beta / poisson log-likelihoods -- whose
    // normalizers are all lgamma -- differentiable, and so HMC-able.
    | "lgamma" -> Some (call "digamma" [u])
    | "floor" | "ceil" -> None
    // digamma is now the frontier: d/dx psi(x) = psi'(x) is trigamma, which
    // the language does not have, so it is REFUSED below rather than silently
    // differentiated to zero. Adding trigamma would move this line, not
    // change the shape of the rule.
    | _ -> None

// Normalized statement model

/// The statement fragment the transform reasons over, after unwrapping
/// spans and desugaring. Assignments keep their surface Expr lhs (a plain
/// var or an element application).
type internal NStmt =
    | NLet of name: string * isMut: bool * value: Expr
    | NAssign of lhs: Expr * rhs: Expr
    | NFor of var: string * lo: Expr * hi: Expr * body: NStmt list

/// One module-level single-name binding, as the differentiator reads it.
///
/// The VALUE is what pipeline fusion resolves through: `let dbl =
/// object_for(...)` at module level has to be visible when a function body
/// writes `(dbl >>@ inc)`, and a module-level `let calibrate = lambda(x) ->
/// ...` has to be visible as a stage KERNEL (it is neither a `function` nor
/// an intrinsic, so `asKernelLambda` cannot see it any other way).
///
/// The ANNOTATION, where the binding carries one, is what the grouped-peel
/// lowering reads to decide whether a key space is positional (empty groups
/// possible) or dynamically discovered (never empty) -- guessing from an int
/// literal would misread an annotated `Array<Idx<N> like R>` table as
/// dynamic, the one direction that silently NaNs an empty mean.
///
/// One record rather than two parallel maps: they are two projections of the
/// same binding, and building them by two folds is how the position-aware
/// snapshot below and the whole-module map drifted apart on what a rebind
/// does to a stale annotation.
type internal ModuleLet = {
    Value: Expr
    Ty: TypeExpr option
}

type internal Ctx = {
    /// Same-module user function declarations by name.
    Decls: Map<string, FunctionDecl>
    /// Same-module transparent type aliases (unparameterized `type X = T` and
    /// mutual-group members), so `type Speed = Float<mps>` classifies as the
    /// unit-carrying Float it is instead of silently falling to NonDiff.
    TypeAliases: Map<string, TypeExpr>
    /// Module-level value binding names (DeclLet / DeclStatic): reads of these
    /// inside a differentiated body are constant data, not unknown calls.
    ModuleVals: Set<string>
    /// Module-level single-name bindings visible to the body being
    /// differentiated -- a PREFIX snapshot taken at that body's declaration,
    /// so nothing resolves through a `let` declared below it.
    ModuleLets: Map<string, ModuleLet>
    /// Fresh-suffix counter, shared across one expand run. A `ref` CELL, not a
    /// `mutable` field: the synthesis loop derives per-request contexts with
    /// `{ ctx with Decls = available }`, and a record copy would duplicate a
    /// mutable field by VALUE -- restarting the numbering each round, so two
    /// synthesis rounds over one module could mint the same `__ck1`/`__gt2`.
    /// The cell is shared by every copy, which is what "shared" has to mean.
    Fresh: int ref
    /// Per-synthesis memo of `normalizeBody`, keyed (callee name, depth).
    /// UNLIKE `Fresh` this is deliberately NOT shared across requests:
    /// `ctxFor` gives each one an empty table, because the normalization of a
    /// callee depends on `Decls` -- and two requests see different prefix
    /// snapshots. Within one request `Decls`, the top-level name, and the
    /// mode are all fixed, which is what makes the key sufficient.
    NormMemo: System.Collections.Generic.Dictionary<string * int, NStmt list * Expr>
    /// Per-synthesis memo of the arity-poly pack unroller, keyed
    /// (kernel name, arity), holding the SHAPE over canonical slot names.
    /// Same request-scoping rationale as `NormMemo`.
    PackMemo: System.Collections.Generic.Dictionary<string * int, Expr>
}

let internal fresh (ctx: Ctx) (prefix: string) : string =
    ctx.Fresh.Value <- ctx.Fresh.Value + 1
    $"{prefix}{ctx.Fresh.Value}"

/// The VALUES of the visible module bindings (pipeline fusion's environment).
let internal moduleLetValues (ctx: Ctx) : Map<string, Expr> =
    ctx.ModuleLets |> Map.map (fun _ ml -> ml.Value)

/// The ANNOTATIONS of the visible module bindings that carry one.
let internal moduleLetTys (ctx: Ctx) : Map<string, TypeExpr> =
    ctx.ModuleLets
    |> Map.toSeq
    |> Seq.choose (fun (n, ml) -> ml.Ty |> Option.map (fun t -> (n, t)))
    |> Map.ofSeq

/// Which transform is currently synthesizing ("grad" | "jvp") -- prefixes
/// every internal error and selects the diagnostic code at the `expand`
/// boundary. A module-level ref (like `synthSpan`): the pass is sequential.
let internal errMode = ref "grad"

let internal err (fname: string) (msg: string) : Result<'a, string> =
    Error $"{errMode.Value}({fname}): {msg}"

/// Route selector for the reverse-mode halo stencil rule (docs/plans/
/// structural/02, section 3.3). ON (the default): the map is KEPT and its
/// adjoint is the gather -- one guarded loop per (array, offset) the kernel
/// reads. OFF: the map lowers into the construction loop like any other
/// eager map and its adjoint scatters. The two are the same function; the
/// harness (tests/AccessTests.fs) runs both and compares the outputs
/// byte-for-byte. Read per call, never cached, so that toggle works.
let internal haloGatherEnabled () : bool =
    match System.Environment.GetEnvironmentVariable "BLADE_AD_HALO_GATHER" with
    | null | "" -> true
    | s ->
        match s.Trim().ToLowerInvariant() with
        | "0" | "off" | "false" | "no" -> false
        | _ -> true

// Kernel-shape refusal wording, spoken by more than one site.
//
// `asKernelLambda` (further down) decides what counts as a kernel and
// returns a `KernelShape` for what it rejected; the WORDING lives here
// because the validation walk answers the same two rejections before that
// function is even in scope. Both strings are pinned by corpus
// `ERROR-CONTAINS` tests, so they are one definition rather than two copies
// that a reworded message could quietly split.

/// A named kernel whose body is a block -- a v1 restriction with a fix.
let internal kernBlockBodyMsg (f: string) : string =
    $"kernel '{f}' has a block body; only expression-bodied named functions are differentiable as kernels (v1)"

/// Anything else in the kernel position of a `<@>`.
let internal kernUnsupportedMsg =
    "differentiating `<@>` supports lambda, reynolds(lambda), named-function, and intrinsic kernels (v1)"

/// Resolve same-module transparent type aliases, depth-capped (an alias
/// cycle is a type error elsewhere; the cap just keeps this total). Defined
/// this early because extent reading, parameter classification, and the map
/// rule all need to see through `type I = Idx<3>`.
let internal resolveTy (ctx: Ctx) (t: TypeExpr) : TypeExpr =
    let rec go d t =
        if d > 8 then t
        else
            match t with
            | TyNamed (n, []) ->
                (match Map.tryFind n ctx.TypeAliases with
                 | Some body -> go (d + 1) body
                 | None -> t)
            | _ -> t
    go 0 t

/// `arrayLiteralExtents` after alias resolution on the element and index
/// slots, so `Array<Float like I>` with `type I = Idx<3>` reads as extent 3.
let internal resolveArrayTy (ctx: Ctx) (t: TypeExpr) : TypeExpr =
    match resolveTy ctx t with
    | TyArray (elem, idxs) -> TyArray (resolveTy ctx elem, idxs |> List.map (resolveTy ctx))
    | other -> other

/// Does any of `names` occur anywhere in `e`? EXHAUSTIVE over the grammar,
/// on purpose: `mentionsAnyOf` answers TRUE for every node it has no arm for,
/// which would make the residual check below refuse any body that so much as
/// contains a `group_keys` call. Shadowing is ignored -- the names asked
/// about are the peel's own bindings, so a false positive costs a refusal
/// while a missed use would leave a name dangling, which typecheck says
/// loudly. Anything genuinely leaf-shaped answers false, listed by name so a
/// new grammar node shows up as an incomplete-match warning instead of
/// silently defaulting either way.
let rec internal mentionsDeep (names: Set<string>) (e: Expr) : bool =
    let m = mentionsDeep names
    let any = List.exists m
    let opt o = match o with Some x -> m x | None -> false
    match e.Kind with
    | ExprKind.ExprVar n -> Set.contains n names
    | ExprKind.ExprBinOp (_, _, l, r) -> m l || m r
    | ExprKind.ExprUnaryOp (_, i) -> m i
    | ExprKind.ExprApp (f, args) -> m f || any args
    | ExprKind.ExprTupleIndex (t, i) -> m t || m i
    | ExprKind.ExprField (t, _) -> m t
    | ExprKind.ExprLambda (ps, _, b) -> (ps |> List.exists (fun p -> opt p.Default)) || m b
    | ExprKind.ExprLet (bnd, b) -> m bnd.Value || m b
    | ExprKind.ExprMatch (s, cases) ->
        m s || (cases |> List.exists (fun c -> opt c.Guard || m c.Body))
    | ExprKind.ExprIf (c, t, f) -> m c || m t || m f
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprStack es
    | ExprKind.ExprSequence es | ExprKind.ExprZip es | ExprKind.ExprMethodFor es
    | ExprKind.ExprGroupKeys es -> any es
    | ExprKind.ExprAlign (es, _) | ExprKind.ExprJoin (es, _) -> any es
    | ExprKind.ExprBlock (ss, fe) -> (ss |> List.exists (stmtMentionsDeep names)) || opt fe
    | ExprKind.ExprObjectFor k -> m k
    | ExprKind.ExprDotDot (l, h) -> m l || m h
    | ExprKind.ExprHalo (_, o) -> m o
    | ExprKind.ExprPure i | ExprKind.ExprCompute i | ExprKind.ExprRead i
    | ExprKind.ExprRank i | ExprKind.ExprUnique i | ExprKind.ExprGroupBucket i
    | ExprKind.ExprExtents i | ExprKind.ExprStatic i | ExprKind.ExprTyped (i, _)
    | ExprKind.ExprTranspose (i, _, _) | ExprKind.ExprDecompact (i, _)
    | ExprKind.ExprPartialApp (_, i, _) | ExprKind.ExprReynolds (i, _) -> m i
    | ExprKind.ExprGuard (l, r) | ExprKind.ExprReplicate (l, r) | ExprKind.ExprMask (l, r)
    | ExprKind.ExprCompound (l, r) | ExprKind.ExprSparse (l, r)
    | ExprKind.ExprIntersect (l, r) | ExprKind.ExprUnion (l, r)
    | ExprKind.ExprContains (l, r) | ExprKind.ExprGroupBy (l, r)
    | ExprKind.ExprSort (l, r) | ExprKind.ExprGram (l, r)
    | ExprKind.ExprAssign (l, r) -> m l || m r
    | ExprKind.ExprReduce (a, k, i, ax) -> m a || m k || opt i || opt ax
    | ExprKind.ExprStruct (_, fields, spread) ->
        (fields |> List.exists (fun (_, fe) -> m fe)) || opt spread
    | ExprKind.ExprFor (src, _, k) ->
        (match src with
         | ForArrays (es, inc) -> any es || opt inc
         | ForKernel k2 -> m k2)
        || opt k
    | ExprKind.ExprRecArray d ->
        m d.SliceExpr || (match d.SeedArm with Some (_, se) -> m se | None -> false)
        || (match d.Guard with Some g -> m g | None -> false)
    | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprQualified _
    | ExprKind.ExprRange _ | ExprKind.ExprReverse _ | ExprKind.ExprArity _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _ -> false

and internal stmtMentionsDeep (names: Set<string>) (s: Stmt) : bool =
    match s with
    | StmtSpanned (inner, _) -> stmtMentionsDeep names inner
    | StmtLet b -> mentionsDeep names b.Value
    | StmtExpr ex -> mentionsDeep names ex
    | StmtAssign (l, _, r) -> mentionsDeep names l || mentionsDeep names r
    | StmtForIn (_, r, body) ->
        mentionsDeep names r || (body |> List.exists (stmtMentionsDeep names))

/// EVERY variable name occurring anywhere in `e`. `mentionsDeep`'s dual: same
/// exhaustive coverage, same deliberate blindness to inner binders. Callers
/// that need the FREE names subtract their own binders; a name an inner lambda
/// rebinds is still reported, which over-reports and therefore over-taints --
/// the direction that costs a refusal rather than a silent zero.
let rec internal allVarsDeep (e: Expr) : Set<string> =
    let any (es: Expr list) = es |> List.fold (fun acc x -> Set.union acc (allVarsDeep x)) Set.empty
    let opt o = match o with Some x -> allVarsDeep x | None -> Set.empty
    match e.Kind with
    | ExprKind.ExprVar n -> Set.singleton n
    | ExprKind.ExprBinOp (_, _, l, r) -> any [l; r]
    | ExprKind.ExprUnaryOp (_, i) -> allVarsDeep i
    | ExprKind.ExprApp (f, args) -> Set.union (allVarsDeep f) (any args)
    | ExprKind.ExprTupleIndex (t, i) -> any [t; i]
    | ExprKind.ExprField (t, _) -> allVarsDeep t
    | ExprKind.ExprLambda (ps, _, b) ->
        Set.union (ps |> List.fold (fun acc p -> Set.union acc (opt p.Default)) Set.empty) (allVarsDeep b)
    | ExprKind.ExprLet (bnd, b) -> any [bnd.Value; b]
    | ExprKind.ExprMatch (s, cases) ->
        cases |> List.fold (fun acc c -> Set.union acc (Set.union (opt c.Guard) (allVarsDeep c.Body)))
                           (allVarsDeep s)
    | ExprKind.ExprIf (c, t, f) -> any [c; t; f]
    | ExprKind.ExprTuple es | ExprKind.ExprArrayLit es | ExprKind.ExprStack es
    | ExprKind.ExprSequence es | ExprKind.ExprZip es | ExprKind.ExprMethodFor es
    | ExprKind.ExprGroupKeys es -> any es
    | ExprKind.ExprAlign (es, _) | ExprKind.ExprJoin (es, _) -> any es
    | ExprKind.ExprBlock (ss, fe) ->
        ss |> List.fold (fun acc s -> Set.union acc (stmtAllVarsDeep s)) (opt fe)
    | ExprKind.ExprObjectFor k -> allVarsDeep k
    | ExprKind.ExprDotDot (l, h) -> any [l; h]
    | ExprKind.ExprHalo (_, o) -> allVarsDeep o
    | ExprKind.ExprPure i | ExprKind.ExprCompute i | ExprKind.ExprRead i
    | ExprKind.ExprRank i | ExprKind.ExprUnique i | ExprKind.ExprGroupBucket i
    | ExprKind.ExprExtents i | ExprKind.ExprStatic i | ExprKind.ExprTyped (i, _)
    | ExprKind.ExprTranspose (i, _, _) | ExprKind.ExprDecompact (i, _)
    | ExprKind.ExprPartialApp (_, i, _) | ExprKind.ExprReynolds (i, _) -> allVarsDeep i
    | ExprKind.ExprGuard (l, r) | ExprKind.ExprReplicate (l, r) | ExprKind.ExprMask (l, r)
    | ExprKind.ExprCompound (l, r) | ExprKind.ExprSparse (l, r)
    | ExprKind.ExprIntersect (l, r) | ExprKind.ExprUnion (l, r)
    | ExprKind.ExprContains (l, r) | ExprKind.ExprGroupBy (l, r)
    | ExprKind.ExprSort (l, r) | ExprKind.ExprGram (l, r)
    | ExprKind.ExprAssign (l, r) -> any [l; r]
    | ExprKind.ExprReduce (a, k, i, ax) -> Set.union (any [a; k]) (Set.union (opt i) (opt ax))
    | ExprKind.ExprStruct (_, fields, spread) ->
        fields |> List.fold (fun acc (_, fe) -> Set.union acc (allVarsDeep fe)) (opt spread)
    | ExprKind.ExprFor (src, _, k) ->
        Set.union
            (match src with
             | ForArrays (es, inc) -> Set.union (any es) (opt inc)
             | ForKernel k2 -> allVarsDeep k2)
            (opt k)
    | ExprKind.ExprRecArray d ->
        Set.unionMany
            [ allVarsDeep d.SliceExpr
              (match d.SeedArm with Some (_, se) -> allVarsDeep se | None -> Set.empty)
              (match d.Guard with Some g -> allVarsDeep g | None -> Set.empty) ]
    | ExprKind.ExprLit _ | ExprKind.ExprWildcard | ExprKind.ExprQualified _
    | ExprKind.ExprRange _ | ExprKind.ExprReverse _ | ExprKind.ExprArity _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _ -> Set.empty

and internal stmtAllVarsDeep (s: Stmt) : Set<string> =
    match s with
    | StmtSpanned (inner, _) -> stmtAllVarsDeep inner
    | StmtLet b -> allVarsDeep b.Value
    | StmtExpr ex -> allVarsDeep ex
    | StmtAssign (l, _, r) -> Set.union (allVarsDeep l) (allVarsDeep r)
    | StmtForIn (_, r, body) ->
        body |> List.fold (fun acc s2 -> Set.union acc (stmtAllVarsDeep s2)) (allVarsDeep r)

/// Capture-avoiding substitution: `name := repl`, over kernel bodies
/// (fusion, the map rule, kernel-body calls) and over the recursive-array
/// lowering's slices and seed arms.
///
/// This is the file's ONLY substitution walker, and its catch-all is
/// `None` -- DECLINE, never "return the node unchanged". A form it does not
/// might contain a live occurrence of `name`, and quietly leaving that
/// occurrence behind is how a fusion turns into a wrong answer (or, more
/// often, a baffling BL2001 far from here). Forms that BIND names
/// (`ExprLambda` that does not shadow, `let`, blocks, matches, `for`,
/// recursive arrays) also decline: `repl` is a whole kernel body carrying
/// free captures, and proving those survive a binder is not worth it for
/// shapes no kernel actually uses.
let rec internal substKernMany (subs: Map<string, Expr>) (e: Expr) : Expr option =
    let s = substKernMany subs
    let re k = Some (inheritSpan e k)
    let s1 mk a = s a |> Option.bind (fun a' -> re (mk a'))
    let s2 mk a b =
        match s a, s b with
        | Some a', Some b' -> re (mk a' b')
        | _ -> None
    let sList (xs: Expr list) = traverseO s xs
    let sOpt (x: Expr option) =
        match x with
        | None -> Some None
        | Some a -> s a |> Option.map Some
    match e.Kind with
    | ExprKind.ExprVar n when Map.containsKey n subs -> Some subs.[n]
    // leaves: nothing to rewrite, nothing to miss
    | ExprKind.ExprVar _ | ExprKind.ExprLit _ | ExprKind.ExprWildcard
    | ExprKind.ExprQualified _ | ExprKind.ExprRange _ | ExprKind.ExprReverse _
    | ExprKind.ExprNth | ExprKind.ExprZero | ExprKind.ExprSection _
    | ExprKind.ExprArity _ -> Some e
    | ExprKind.ExprBinOp (m, op, l, r) -> s2 (fun a b -> ExprBinOp (m, op, a, b)) l r
    | ExprKind.ExprUnaryOp (op, i) -> s1 (fun a -> ExprUnaryOp (op, a)) i
    | ExprKind.ExprApp (f, args) ->
        match s f, sList args with
        | Some f', Some args' -> re (ExprApp (f', args'))
        | _ -> None
    | ExprKind.ExprTupleIndex (t, i) -> s2 (fun a b -> ExprTupleIndex (a, b)) t i
    | ExprKind.ExprField (x, f) -> s1 (fun a -> ExprField (a, f)) x
    | ExprKind.ExprIf (c, t, f) ->
        match s c, s t, s f with
        | Some c', Some t', Some f' -> re (ExprIf (c', t', f'))
        | _ -> None
    | ExprKind.ExprTuple es -> sList es |> Option.bind (fun es' -> re (ExprTuple es'))
    | ExprKind.ExprArrayLit es -> sList es |> Option.bind (fun es' -> re (ExprArrayLit es'))
    | ExprKind.ExprMethodFor ops -> sList ops |> Option.bind (fun o -> re (ExprMethodFor o))
    | ExprKind.ExprObjectFor k -> s1 (fun a -> ExprObjectFor a) k
    | ExprKind.ExprDotDot (l, h) -> s2 (fun a b -> ExprDotDot (a, b)) l h
    | ExprKind.ExprHalo (t, offs) -> s1 (fun a -> ExprHalo (t, a)) offs
    | ExprKind.ExprZip es -> sList es |> Option.bind (fun es' -> re (ExprZip es'))
    | ExprKind.ExprAlign (es, spec) -> sList es |> Option.bind (fun es' -> re (ExprAlign (es', spec)))
    | ExprKind.ExprStack es -> sList es |> Option.bind (fun es' -> re (ExprStack es'))
    | ExprKind.ExprJoin (es, d) -> sList es |> Option.bind (fun es' -> re (ExprJoin (es', d)))
    | ExprKind.ExprSequence es -> sList es |> Option.bind (fun es' -> re (ExprSequence es'))
    | ExprKind.ExprGroupKeys es -> sList es |> Option.bind (fun es' -> re (ExprGroupKeys es'))
    | ExprKind.ExprPure i -> s1 (fun a -> ExprPure a) i
    | ExprKind.ExprCompute i -> s1 (fun a -> ExprCompute a) i
    | ExprKind.ExprRead i -> s1 (fun a -> ExprRead a) i
    | ExprKind.ExprGuard (c, b) -> s2 (fun a b2 -> ExprGuard (a, b2)) c b
    | ExprKind.ExprReplicate (c, b) -> s2 (fun a b2 -> ExprReplicate (a, b2)) c b
    | ExprKind.ExprReynolds (k, anti) -> s1 (fun a -> ExprReynolds (a, anti)) k
    | ExprKind.ExprTyped (i, t) -> s1 (fun a -> ExprTyped (a, t)) i
    | ExprKind.ExprRank i -> s1 (fun a -> ExprRank a) i
    | ExprKind.ExprMask (a, p) -> s2 (fun x y -> ExprMask (x, y)) a p
    | ExprKind.ExprCompound (d, m) -> s2 (fun x y -> ExprCompound (x, y)) d m
    | ExprKind.ExprSparse (vs, ks) -> s2 (fun x y -> ExprSparse (x, y)) vs ks
    | ExprKind.ExprIntersect (a, b) -> s2 (fun x y -> ExprIntersect (x, y)) a b
    | ExprKind.ExprUnion (a, b) -> s2 (fun x y -> ExprUnion (x, y)) a b
    | ExprKind.ExprUnique a -> s1 (fun x -> ExprUnique x) a
    | ExprKind.ExprContains (a, x) -> s2 (fun p q -> ExprContains (p, q)) a x
    | ExprKind.ExprGroupBy (vs, g) -> s2 (fun x y -> ExprGroupBy (x, y)) vs g
    | ExprKind.ExprGroupBucket g -> s1 (fun x -> ExprGroupBucket x) g
    | ExprKind.ExprSort (a, k) -> s2 (fun x y -> ExprSort (x, y)) a k
    | ExprKind.ExprReduce (a, k, init, axes) ->
        match s a, s k, sOpt init, sOpt axes with
        | Some a', Some k', Some i', Some x' -> re (ExprReduce (a', k', i', x'))
        | _ -> None
    | ExprKind.ExprTranspose (a, d1, d2) -> s1 (fun x -> ExprTranspose (x, d1, d2)) a
    | ExprKind.ExprDecompact (a, d) -> s1 (fun x -> ExprDecompact (x, d)) a
    | ExprKind.ExprGram (l, r) -> s2 (fun x y -> ExprGram (x, y)) l r
    | ExprKind.ExprExtents a -> s1 (fun x -> ExprExtents x) a
    | ExprKind.ExprPartialApp (op, x, isLeft) -> s1 (fun a -> ExprPartialApp (op, a, isLeft)) x
    | ExprKind.ExprAssign (l, r) -> s2 (fun x y -> ExprAssign (x, y)) l r
    | ExprKind.ExprStatic i -> s1 (fun a -> ExprStatic a) i
    | ExprKind.ExprStruct (n, fields, spread) ->
        let fs = fields |> traverseO (fun (fn, fv) -> s fv |> Option.map (fun y -> (fn, y)))
        match fs, sOpt spread with
        | Some fs', Some sp' -> re (ExprStruct (n, fs', sp'))
        | _ -> None
    // A lambda that SHADOWS the names is already correct as written. It must
    // shadow ALL of them: a lambda hiding one substitution but not another is
    // exactly what one-at-a-time substitution declines at, on the pass for
    // the name it does not hide, so the multi-name walk has to decline too.
    | ExprKind.ExprLambda (ps, _, _) when
        subs |> Map.forall (fun n _ -> ps |> List.exists (fun p -> p.Name = n)) -> Some e
    // everything else -- binders, blocks, matches, loop forms, and anything
    // added to the grammar after this was written -- declines.
    | _ -> None

/// The one-name spelling, which is what most callers want.
let internal substKern (name: string) (repl: Expr) (e: Expr) : Expr option =
    substKernMany (Map.ofList [(name, repl)]) e

/// Substitute `pname := repl` into a KERNEL-SHAPED body -- either a kernel
/// parameter meeting its indexed read (the map rule) or a callee parameter
/// meeting its argument (the kernel-body call rule) -- refusing rather than
/// half-substituting.
///
/// `substKern` is the substitution: its catch-all DECLINES, so it never
/// leaves a live occurrence behind and never crosses a binder it cannot
/// prove safe, which is exactly the alpha-safety both callers need. A
/// decline is only interesting when the name actually occurs, though, and
/// `mentionsDeep` (exhaustive over the grammar, shadowing ignored) decides
/// that: a body that never mentions the parameter passes through untouched,
/// so a kernel whose body merely CONTAINS an unrelated inner lambda is not
/// refused for it.
///
/// The predecessor here was a fragment walker whose catch-all silently
/// returned the node unchanged -- which meant a parameter used inside a
/// `reduce`, an `extents`, or any other form it lacked an arm for was left
/// dangling in the emitted tangent lambda, surfacing as an unbound-name type
/// error far from the cause. Refusing by name is the whole improvement.
let internal substParam (fname: string) (pname: string) (repl: Expr) (body: Expr) : Result<Expr, string> =
    if not (mentionsDeep (Set.singleton pname) body) then Ok body
    else
        match substKern pname repl body with
        | Some b -> Ok b
        | None ->
            err fname $"cannot substitute '{pname}' into the kernel body: a binder or an unsupported form stands between the parameter and its use, so the substitution cannot be proved capture-free"

/// The same substitution for a whole batch, in ONE walk instead of one walk
/// per name. The pack unroller peels a `head :: tail` one element at a time
/// and used to substitute each peeled head separately, which is a walk per
/// element over a body that grows with the arity.
///
/// Simultaneous and sequential agree here because no replacement mentions
/// another batch member's name: the unroller's replacements are the pack's
/// own expanded parameters (compiler-fresh) and the apply site's arguments
/// (written before those names existed).
///
/// The `occurring` filter is `substParam`'s "not mentioned, nothing to do"
/// skip, computed once for the batch rather than per name -- and it is what
/// makes the two agree on DECLINES too, since both then act on exactly the
/// mentioned names. On a decline the one-at-a-time fold runs after all, for
/// its message alone: `substKern`'s refusal is structural and cannot say
/// which name it gave up on.
let internal substParamMany (fname: string) (subs: (string * Expr) list) (body: Expr)
    : Result<Expr, string> =
    let occurring = allVarsDeep body
    let live = subs |> List.filter (fun (n, _) -> Set.contains n occurring)
    if List.isEmpty live then Ok body
    else
        match substKernMany (Map.ofList live) body with
        | Some b -> Ok b
        | None ->
            subs |> List.fold (fun acc (nm, repl) -> acc |> Result.bind (substParam fname nm repl))
                              (Ok body)

// Body -> NStmt conversion

let rec internal convertStmts (fname: string) (stmts: Stmt list) : Result<NStmt list, string> =
    let one stmt =
        match unwrapStmt stmt with
        | StmtSpanned _ -> err fname "internal: unwrapStmt left a span"
        | StmtLet binding ->
            match binding.Pattern.Kind with
            | PatternKind.PatVar name ->
                let isMut = (binding.Mutability = BindMut)
                Ok (NLet (name, isMut, binding.Value))
            | _ -> err fname "tuple/struct patterns in let are not differentiable (v1); bind names individually"
        | StmtExpr { Kind = ExprKind.ExprAssign (lhs, rhs) } ->
            Ok (NAssign (lhs, rhs))
        | StmtAssign (lhs, op, rhs) ->
            // Defensive: the parser emits ExprAssign, but normalize
            // StmtAssign if one arrives.
            let rhs' =
                match op with
                | AssignEq -> rhs
                | AssignAdd -> add lhs rhs
                | AssignSub -> sub lhs rhs
                | AssignMul -> mul lhs rhs
                | AssignDiv -> div lhs rhs
            Ok (NAssign (lhs, rhs'))
        | StmtExpr _ ->
            err fname "bare expression statements are not supported in differentiated code"
        | StmtForIn (var, { Kind = ExprKind.ExprDotDot (lo, hi) }, body) ->
            convertStmts fname body |> Result.map (fun nbody -> NFor (var, lo, hi, nbody))
        | StmtForIn _ ->
            err fname "for-in ranges must use the a..b form in differentiated code"
    stmts |> traverseR one

/// A function body is either a block or a bare expression.
let internal convertBody (fname: string) (body: Expr) : Result<NStmt list * Expr, string> =
    match body.Kind with
    | ExprKind.ExprBlock (stmts, Some finalE) ->
        convertStmts fname stmts |> Result.map (fun ns -> (ns, finalE))
    | ExprKind.ExprBlock (_, None) ->
        err fname "function body has no final expression (must return a Float)"
    | _ -> Ok ([], body)

// Expression validation + variable collection over the AD-able fragment

/// The constant-fill array constructor `replicate(N, pure(lit)) |> compute`,
/// the idiomatic replacement for hand-written N-element zero literals.
/// Combinators are otherwise rejected in differentiated code (v1), but a
/// literal fill computes nothing and reads nothing, so it is admitted as
/// an array-literal equivalent wherever ExprArrayLit initializers are.
/// Captures (count expr, fill literal).
let internal (|ConstFill|_|) (e: Expr) =
    match e.Kind with
    | ExprKind.ExprCompute { Kind = ExprKind.ExprReplicate (cnt, { Kind = ExprKind.ExprPure { Kind = ExprKind.ExprLit lit } }) } -> Some (cnt, lit)
    | _ -> None

/// A ConstFill of the same count with the fill value zeroed.
let internal zeroFill (cnt: Expr) : Expr =
    let re k = inheritSpan cnt k
    re (ExprCompute (re (ExprReplicate (cnt, re (ExprPure (re (ExprLit (LitFloat 0.0))))))))

/// The C1 LINEAR CLOSURE: combinator forms that only REINDEX or WRAP, so
/// their tangent is the same form applied to the tangents of their
/// differentiable operands, with non-differentiable operands (masks,
/// grouping keys, counts, conditions, axis indices) reused verbatim. No
/// derivative rules and no kernel synthesis are involved -- this is
/// plumbing, and it is what lets a tangent travel through a pipeline of
/// structural operations. Returns (differentiable operands, rebuild).
///
/// `guard` belongs here because zeroing is linear: d(c ? e : 0) = c ? ė : 0.
/// `<|:>` belongs here because its selector is ALLOCATION, not value --
/// storage branching is linear in both legs (unlike `<|>`, which branches
/// on a VALUE test and is discontinuous, so it stays refused).
let internal (|LinearForm|_|) (e: Expr) : (Expr list * (Expr list -> Expr)) option =
    let re k = inheritSpan e k
    match e.Kind with
    | ExprKind.ExprPure inner -> Some ([inner], fun ts -> re (ExprPure (List.head ts)))
    | ExprKind.ExprCompute inner -> Some ([inner], fun ts -> re (ExprCompute (List.head ts)))
    | ExprKind.ExprGuard (c, body) -> Some ([body], fun ts -> re (ExprGuard (c, List.head ts)))
    | ExprKind.ExprStack es -> Some (es, fun ts -> re (ExprStack ts))
    | ExprKind.ExprSequence es -> Some (es, fun ts -> re (ExprSequence ts))
    | ExprKind.ExprJoin (es, d) -> Some (es, fun ts -> re (ExprJoin (ts, d)))
    | ExprKind.ExprReplicate (cnt, body) -> Some ([body], fun ts -> re (ExprReplicate (cnt, List.head ts)))
    | ExprKind.ExprTranspose (a, d1, d2) -> Some ([a], fun ts -> re (ExprTranspose (List.head ts, d1, d2)))
    | ExprKind.ExprDecompact (a, d) -> Some ([a], fun ts -> re (ExprDecompact (List.head ts, d)))
    | ExprKind.ExprCompound (dense, mask) -> Some ([dense], fun ts -> re (ExprCompound (List.head ts, mask)))
    | ExprKind.ExprGroupBy (vals, gk) -> Some ([vals], fun ts -> re (ExprGroupBy (List.head ts, gk)))
    | ExprKind.ExprBinOp (m, OpFallback, l, r) ->
        Some ([l; r], fun ts -> re (ExprBinOp (m, OpFallback, ts.[0], ts.[1])))
    | _ -> None

/// Structural array-ness, used by the taint pass so a local bound to a
/// linear combinator is tracked as an ARRAY. Under-reporting here is a
/// silent-zero bug (an element read of an untracked array yields no
/// tangent), so the forms are listed exhaustively rather than inferred.
let rec internal producesArray (arrays: Set<string>) (e: Expr) : bool =
    match e.Kind with
    | ExprKind.ExprArrayLit _ -> true
    | ExprKind.ExprVar n -> Set.contains n arrays
    | ExprKind.ExprStack _ | ExprKind.ExprSequence _ | ExprKind.ExprJoin _
    | ExprKind.ExprReplicate _ | ExprKind.ExprTranspose _ | ExprKind.ExprDecompact _
    | ExprKind.ExprCompound _ | ExprKind.ExprGroupBy _ | ExprKind.ExprMask _
    | ExprKind.ExprSort _ | ExprKind.ExprUnique _
    | ExprKind.ExprIntersect _ | ExprKind.ExprUnion _ -> true
    | ExprKind.ExprPure inner | ExprKind.ExprCompute inner
    | ExprKind.ExprGuard (_, inner) | ExprKind.ExprTyped (inner, _) -> producesArray arrays inner
    | ExprKind.ExprBinOp (_, OpFallback, l, r) -> producesArray arrays l || producesArray arrays r
    // a map application (C2) always yields an array-shaped result
    | ExprKind.ExprBinOp (_, OpApply, _, _) -> true
    // grouping-derived data arrays (2.17a): the row->bucket map and
    // per-group sizes are Int arrays read by index
    | ExprKind.ExprGroupBucket _ | ExprKind.ExprExtents _ -> true
    | ExprKind.ExprGram _ -> true
    | _ -> false

