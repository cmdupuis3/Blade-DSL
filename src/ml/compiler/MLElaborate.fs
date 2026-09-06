/// ML-module elaboration: equivariant ops synthesized as compile-time Blade
/// source rather than opaque builtins. Surface (call-shaped, recognized only
/// when the name is not user-bound; configs are `let static` bindings):
///
///   y_to(LMAX, x, y, z)                 -> Array<Float like Idx<(LMAX+1)^2>>
///   tensor_product(CFG, x, y, w)        -> Array<Float like Idx<total_dim(specOut)>>
///   linear(SPEC_IN, SPEC_OUT, w, x)     -> Array<Float like Idx<total_dim(SPEC_OUT)>>
///   gated(SPEC, x)                      -> Array<Float like Idx<total_dim(SPEC)>>
///   scalars(SPEC, x)                    -> Array<Float like Idx<#l=0 entries>>
///   norms(SPEC, x)                      -> Array<Float like Idx<#(block, mu) slots>>
///
/// Functions carrying `where <alias>.equiv(O3|SO3)` are JUDGED (Blade.ML.Equiv)
/// at the pass-1/pass-2 seam: the body must compose only equivariance-preserving
/// operations, else BL4008. Two SIBLING judgments run at the same seam over
/// their own status sets and never interact: `where <alias>.galilean(u, ...)`
/// (Blade.ML.Galilean, BL4009) and `where <alias>.perm_equiv(N)` (Blade.ML.Perm,
/// BL4012 -- the S_n node-axis lattice, whose polarity is the OPPOSITE of
/// equiv's at the pointwise arms).
///
/// A SPEC is a static array of (l, parity, mult) int triples (0/1 = even/odd
/// parity); a CFG is a static (spec1, spec2, specOut) triple. `sh_spec(lmax)`
/// builds the Y-expansion spec statically; `total_dim`/`tp_weight_dim`/
/// `linear_weight_dim` size weight buffers, all registered by MLStatics.fs
/// through StaticEval's builtin registry.
///
/// For each distinct (op, resolved config) the elaborator synthesizes ONE
/// Blade function (`__ml_tp_1`, ...) whose body is exactly the ml/ reference
/// implementation's loop structure (same iteration order, ulp agreement),
/// with path metadata and real-basis CG coefficients (WignerTables) baked as
/// literal tables. Call sites rewrite to the generated names.
///
/// Runs BEFORE Grad expansion (TypeCheck.typeCheck), so grad() differentiates
/// elaborated ops through its normal inliner -- no VJP registry, no new IR
/// nodes, and the generated functions type-check like user code.
module Blade.ML.Elaborate

open Blade.Ast
open Blade.StaticEval
open Blade.ML.Spec

// Elaboration errors

/// Elaboration failure: a message plus the BLxxxx code it surfaces under.
/// BL5000 = generic ML-elaboration failure; BL4007 = "no equivariant map
/// exists" (Schur selection-rule violations: unreachable tensor_product
/// output blocks, linear over specs sharing no (l, parity)).
type private ElabError = { Code: string; Msg: string }
let private err5000 (msg: string) : ElabError = { Code = "BL5000"; Msg = msg }
let private err4007 (msg: string) : ElabError = { Code = "BL4007"; Msg = msg }

// Spec model lives in ml/compiler/MLSpec.fs; StaticValue conversions and
// the sizing builtins in ml/compiler/MLStatics.fs (shared seam). Local
// aliases wrap their string errors into coded ElabErrors:
let private specOfStatic what v = Blade.ML.Statics.specOfStatic what v |> Result.mapError err5000
let private cfgOfStatic what v = Blade.ML.Statics.cfgOfStatic what v |> Result.mapError err5000

// AST construction helpers (mirroring Grad.fs's style)

let private v (n: string) = syn (ExprVar n)
let private fLit (x: float) = syn (ExprLit (LitFloat x))
let private iLit (n: int) = syn (ExprLit (LitInt (int64 n)))
let private add a b = syn (ExprBinOp (Elementwise, OpAdd, a, b))
let private sub a b = syn (ExprBinOp (Elementwise, OpSub, a, b))
let private mul a b = syn (ExprBinOp (Elementwise, OpMul, a, b))
let private divE a b = syn (ExprBinOp (Elementwise, OpDiv, a, b))
let private idx (arr: string) (i: Expr) = syn (ExprApp (v arr, [i]))
let private sLet n value = StmtLet { Pattern = synPat (PatVar n); Type = None; Value = value; Mutability = BindLet }
let private sLetMut n value = StmtLet { Pattern = synPat (PatVar n); Type = None; Value = value; Mutability = BindMut }
let private sAccum lhs e = StmtExpr (syn (ExprAssign (lhs, add lhs e)))
let private sAssign lhs e = StmtExpr (syn (ExprAssign (lhs, e)))
let private sFor var lo hi body = StmtForIn (var, syn (ExprDotDot (iLit lo, iLit hi)), body)
let private zerosLit (n: int) = syn (ExprArrayLit (List.replicate n (fLit 0.0)))
let private intArrLit (xs: int list) = syn (ExprArrayLit (xs |> List.map iLit))
let private floatArrLit (xs: float list) = syn (ExprArrayLit (xs |> List.map fLit))
let private tyFloatArr (n: int) = TyArray (TyNamed ("Float", []), [ TyIdx (iLit n) ])

/// Array<Float like IrrepsIdx<[(l, p, m), ...]>> -- irreps-typed signature
/// slot, the inline spec literal rebuilt from the RESOLVED Spec. Used for the
/// ops' feature params/results that genuinely ARE the irreps space
/// (single-vector forms); row-stacked `_rows` buffers and path-major weight
/// buffers are NOT irreps spaces and stay plain Idx. Anonymous, so a user's
/// alias of the same spec unifies by the name-permissive rule while a
/// wrong-spec annotation or argument is a type error.
let private tyIrrepsArr (s: Spec) : TypeExpr =
    let specLit =
        syn (ExprArrayLit (s |> List.map (fun e ->
            syn (ExprTuple [ iLit e.L; iLit e.Parity; iLit e.Mult ]))))
    TyArray (TyNamed ("Float", []), [ TyIrrepsIdx specLit ])

/// Array<Float like PgIrrepsIdx<GROUP, [(LABEL, mult), ...]>> -- the pg twin
/// of `tyIrrepsArr`, the inline spec literal rebuilt from the RESOLVED
/// PgSpec. Anonymous, so a user's alias of the same (group, spec) unifies by
/// the name-permissive rule while a wrong-spec annotation, a wrong-GROUP
/// annotation, or an O(3) irreps argument of the same extent is a type error.
let private tyPgIrrepsArr (group: string) (s: Blade.ML.PointSpec.PgSpec) : TypeExpr =
    let specLit =
        syn (ExprArrayLit (s |> List.map (fun (label, m) ->
            syn (ExprTuple [ syn (ExprLit (LitString label)); iLit m ]))))
    TyArray (TyNamed ("Float", []), [ TyPgIrrepsIdx (group, specLit) ])

let private mkFunc name (ps: (string * TypeExpr) list) retTy body : FunctionDecl =
    { Name = name
      TypeParams = []
      Params = ps |> List.map (fun (n, t) -> { Name = n; Type = Some t; Mutability = Immutable; Default = None; NameSpan = noSpan })
      WhereClause = None
      ReturnType = Some retTy
      Body = body
      IsStatic = false
      NameSpan = noSpan }

// Equivariance stamping
//
// The functions this elaborator SYNTHESIZES are equivariant BY CONSTRUCTION:
// each is a Schur/CG basis expansion whose admissible-map count IS the
// theorem. `homBlocks`/`linearBlocks` connect only (l, parity)-matched
// blocks; `tpPaths` guards `eo.Parity = parityMul e1.Parity e2.Parity`;
// `polyLabels` carries the degree-K monomial parity (-1)^(sum j*p), and
// `derivePolyDecl.matched` mixes a label only into output blocks of its own
// (L, parity); the point-group member is the same construction over a
// frozen character table with the Frobenius-Schur correction. After pass 2
// the `ml.*` vocabulary is gone and the body is a loop nest over baked
// tables, so nothing downstream can re-derive any of this -- which is why
// the seam judgment refuses composition on these bodies, and why the
// elaborator PINS what it knows (in the normalized spelling `__ml_equiv`,
// after the `<alias>.equiv -> __ml_equiv` rewrite, which only touches
// SOURCE decls) and later consumers read the pin as an axiom.
//
// The stamp is inert in every pass but three: TypeCheck dispatches it
// through the Blade.Constraints registry to MLEquiv's handler (an
// unparseable group name is a check error, not a silent pin), `ide check
// --json` renders it in bindings[].where, and the typecheck-resident
// certified-callee walker consumes it. It never reaches the seam's own
// `buildCertTable` / `judgeFunction` / `inferCertificates`, which all run
// over `decls1` BEFORE pass 2 exists -- so a stamped body never has to
// survive a composition judgment it would refuse.
//
// SOUNDNESS: the stamp claims, of the declared SIGNATURE (read as
// MLEquiv.statusOfType reads it -- an `Array<Float like IrrepsIdx<S>>` /
// `PgIrrepsIdx<G, S>` slot is a Rep, everything else an invariant), that
// for all arguments meeting their declared status, the result meets its
// own -- a theorem OF THE EMITTED BODY. Three families this elaborator
// also synthesizes fail that test and are deliberately left UNSTAMPED
// rather than stamped weakly:
//   * rep-INTRODUCTION (y_to, tensor_to_irreps, sym_to_irreps): invariant
//     scalars in, Rep out, sound only under an unstatable premise ("the
//     coordinates really are the components of the standard vector") --
//     an axiom of that shape would let any three invariants manufacture a
//     representation.
//   * rep-ESCAPE (irreps_to_sym, sym_lift): Rep in, invariant out, false
//     for basis-dependent Cartesian components and unnameable for monomial
//     coordinates. MLEquiv rejects both by name.
//   * non-representation buffers: the row-stacked `linear_rows` /
//     `gated_rows` kernels (seam refuses by name) and the S_n index-action
//     layers, whose discipline is `__ml_perm_equiv`, not this one.
//
// Under-stamping costs the stage-B walker recall; a wrong stamp would be a
// false axiom, so the balance is struck on that side every time.

/// Attach the normalized `__ml_equiv(<group>)` conjunct to a synthesized
/// declaration. Additive: an existing where-clause keeps everything it had.
let private equivStamp (group: string) (fd: FunctionDecl) : FunctionDecl =
    let conj = ("__ml_equiv", [ group ])
    let wc =
        match fd.WhereClause with
        | Some w -> { w with Custom = w.Custom @ [ conj ] }
        | None ->
            { Commutativity = []; Antisymmetry = []; Parallel = []
              Repro = false; TDims = []; Custom = [ conj ] }
    { fd with WhereClause = Some wc }

/// The strongest group admitted by a spec's l = 0 content, for the two
/// emitters that treat l = 0 entries as ordinary numbers: `gated` applies a
/// nonlinear scalar map (silu) to every one, and `scalars` hands them out as
/// declared invariants.
///
/// O(3)'s improper elements act on an l = 0 block as (-1)^parity, so a
/// parity-ODD entry is a PSEUDOSCALAR: not an invariant (`scalars` may not
/// export it as one), and silu does not commute with the sign flip either
/// (silu(-s) = -s + s*sigmoid(s), while equivariance needs
/// -silu(s) = -s*sigmoid(s); equal only at s = 0). SO(3) has no improper
/// elements, so every l = 0 entry is a genuine invariant and the claim
/// WEAKENS to SO3 rather than disappearing.
///
/// DRIFT (reported upstream, not patchable here): MLEquiv's `gated` arm
/// tests only `spec.Head.Parity`, so a spec with an even gate block and a
/// parity-odd l = 0 block ELSEWHERE is wrongly accepted under equiv(O3) at
/// the seam. The predicate here is over the WHOLE spec -- sound, and
/// strictly more conservative than the seam.
let private o3UnlessPseudoscalar (s: Spec) : string =
    if s |> List.exists (fun e -> e.L = 0 && e.Parity <> 0) then "SO3" else "O3"

// Op synthesis

/// __ml_sigmoid: shared scalar helper for gated activations.
let private sigmoidDecl (name: string) : FunctionDecl =
    mkFunc name [ ("z", TyNamed ("Float", [])) ] (TyNamed ("Float", []))
        (divE (fLit 1.0) (add (fLit 1.0) (syn (ExprApp (v "exp", [ syn (ExprUnaryOp (OpNeg, v "z")) ])))))

/// NOT equiv-stamped: a rep-INTRODUCTION form. Three invariant scalars in, a
/// Rep out -- sound only under the premise that (x, y, z) really are the
/// components of the standard vector, which the signature cannot state. See
/// the stamping header above.
///
/// y_to (closed forms, lmax <= 2): mirrors ml/SphericalHarmonics component
/// order (m ascending per l) and the orthonormalized real solid harmonics
/// constants pinned by ml/Tests_SphericalHarmonics.
let private yToDecl (name: string) (lmax: int) : Result<FunctionDecl, ElabError> =
    if lmax < 0 || lmax > 2 then
        Error (err5000 "y_to: lmax must be 0..2 (closed forms only)")
    else
    let dimTot = (lmax + 1) * (lmax + 1)
    let f = TyNamed ("Float", [])
    let stmts =
        [ yield sLetMut "sh" (zerosLit dimTot)
          yield sAssign (idx "sh" (iLit 0)) (fLit 0.28209479177387814)
          if lmax >= 1 then
              yield sAssign (idx "sh" (iLit 1)) (mul (fLit 0.4886025119029199) (v "y"))
              yield sAssign (idx "sh" (iLit 2)) (mul (fLit 0.4886025119029199) (v "z"))
              yield sAssign (idx "sh" (iLit 3)) (mul (fLit 0.4886025119029199) (v "x"))
          if lmax >= 2 then
              yield sLet "r2" (add (add (mul (v "x") (v "x")) (mul (v "y") (v "y"))) (mul (v "z") (v "z")))
              yield sAssign (idx "sh" (iLit 4)) (mul (fLit 1.0925484305920792) (mul (v "x") (v "y")))
              yield sAssign (idx "sh" (iLit 5)) (mul (fLit 1.0925484305920792) (mul (v "y") (v "z")))
              yield sAssign (idx "sh" (iLit 6)) (mul (fLit 0.31539156525252005) (sub (mul (fLit 3.0) (mul (v "z") (v "z"))) (v "r2")))
              yield sAssign (idx "sh" (iLit 7)) (mul (fLit 1.0925484305920792) (mul (v "x") (v "z")))
              yield sAssign (idx "sh" (iLit 8)) (mul (fLit 0.5462742152960396) (sub (mul (v "x") (v "x")) (mul (v "y") (v "y")))) ]
    Ok (mkFunc name [ ("x", f); ("y", f); ("z", f) ] (tyIrrepsArr (shSpec lmax))
            (syn (ExprBlock (stmts, Some (v "sh")))))

/// The tensor_product kernel's statement list, parameterized by the name of
/// the DENSE weight buffer it reads: returns (statements, result expression).
/// The loop order and the left-associated product `(((coef*w)*x)*y)` mirror
/// the ml/TensorProduct reference exactly, which is what makes
/// tensor_product / derive_tp agree with it to the ulp. The S2-compacted
/// kernels do NOT go through here -- they emit only the kept paths, with the
/// dropped contributions fused in (deriveS2TpDecl).
let private tpBodyStmts (cfg: TPConfig) (wName: string) : Stmt list * Expr =
    let dO = totalDim cfg.SpecOut
    let paths = tpPaths cfg
    let s1 = blockStarts cfg.Spec1
    let s2 = blockStarts cfg.Spec2
    let so = blockStarts cfg.SpecOut
    // per-path metadata
    let pMult1 = paths |> List.map (fun (b1, _, _) -> cfg.Spec1.[b1].Mult)
    let pMult2 = paths |> List.map (fun (_, b2, _) -> cfg.Spec2.[b2].Mult)
    let pMultO = paths |> List.map (fun (_, _, bo) -> cfg.SpecOut.[bo].Mult)
    let pD1 = paths |> List.map (fun (b1, _, _) -> dim cfg.Spec1.[b1])
    let pD2 = paths |> List.map (fun (_, b2, _) -> dim cfg.Spec2.[b2])
    let pDO = paths |> List.map (fun (_, _, bo) -> dim cfg.SpecOut.[bo])
    let pS1 = paths |> List.map (fun (b1, _, _) -> s1.[b1])
    let pS2 = paths |> List.map (fun (_, b2, _) -> s2.[b2])
    let pSO = paths |> List.map (fun (_, _, bo) -> so.[bo])
    let pWOff =
        (0, paths) ||> List.scan (fun acc (b1, b2, bo) ->
            acc + cfg.SpecOut.[bo].Mult * cfg.Spec1.[b1].Mult * cfg.Spec2.[b2].Mult)
    let cgPerPath =
        paths |> List.map (fun (b1, b2, bo) ->
            Blade.ML.WignerTables.realCGSparse cfg.Spec1.[b1].L cfg.Spec2.[b2].L cfg.SpecOut.[bo].L)
    let cgOff = (0, cgPerPath) ||> List.scan (fun acc es -> acc + es.Length)
    let cgC1 = cgPerPath |> List.collect (fun es -> es |> Array.toList |> List.map _.C1)
    let cgC2 = cgPerPath |> List.collect (fun es -> es |> Array.toList |> List.map _.C2)
    let cgC3 = cgPerPath |> List.collect (fun es -> es |> Array.toList |> List.map _.C3)
    let cgCo = cgPerPath |> List.collect (fun es -> es |> Array.toList |> List.map _.Coef)
    let nPaths = paths.Length
    // out(pSO(p) + mo*pDO(p) + c3(t)) += coef(t) * w(woff(p) + (mo*m1 + u1)*m2 + u2)
    //                                     * x(pS1(p) + u1*pD1(p) + c1(t))
    //                                     * y(pS2(p) + u2*pD2(p) + c2(t))
    let stmts =
        [ sLetMut "out" (zerosLit dO)
          sLet "__t_m1" (intArrLit pMult1)
          sLet "__t_m2" (intArrLit pMult2)
          sLet "__t_mo" (intArrLit pMultO)
          sLet "__t_d1" (intArrLit pD1)
          sLet "__t_d2" (intArrLit pD2)
          sLet "__t_do" (intArrLit pDO)
          sLet "__t_s1" (intArrLit pS1)
          sLet "__t_s2" (intArrLit pS2)
          sLet "__t_so" (intArrLit pSO)
          sLet "__t_wo" (intArrLit pWOff)
          sLet "__t_co" (intArrLit cgOff)
          sLet "__cg_c1" (intArrLit cgC1)
          sLet "__cg_c2" (intArrLit cgC2)
          sLet "__cg_c3" (intArrLit cgC3)
          sLet "__cg_v" (floatArrLit cgCo)
          sFor "p" 0 nPaths
            [ StmtForIn ("mo", syn (ExprDotDot (iLit 0, idx "__t_mo" (v "p"))),
                [ StmtForIn ("u1", syn (ExprDotDot (iLit 0, idx "__t_m1" (v "p"))),
                    [ StmtForIn ("u2", syn (ExprDotDot (iLit 0, idx "__t_m2" (v "p"))),
                        [ sLet "wv" (idx wName (add (idx "__t_wo" (v "p"))
                                                  (add (mul (add (mul (v "mo") (idx "__t_m1" (v "p"))) (v "u1"))
                                                            (idx "__t_m2" (v "p")))
                                                       (v "u2"))))
                          StmtForIn ("t", syn (ExprDotDot (idx "__t_co" (v "p"), idx "__t_co" (add (v "p") (iLit 1)))),
                            // LEFT-associated product (((coef*w)*x)*y):
                            // exactly the ml/ reference's evaluation
                            // order, so values agree to the ulp.
                            [ sAccum (idx "out" (add (idx "__t_so" (v "p"))
                                                     (add (mul (v "mo") (idx "__t_do" (v "p")))
                                                          (idx "__cg_c3" (v "t")))))
                                     (mul (mul (mul (idx "__cg_v" (v "t")) (v "wv"))
                                               (idx "x" (add (idx "__t_s1" (v "p"))
                                                             (add (mul (v "u1") (idx "__t_d1" (v "p")))
                                                                  (idx "__cg_c1" (v "t"))))))
                                          (idx "y" (add (idx "__t_s2" (v "p"))
                                                        (add (mul (v "u2") (idx "__t_d2" (v "p")))
                                                             (idx "__cg_c2" (v "t")))))) ]) ]) ]) ]) ] ]
    (stmts, v "out")

/// tensor_product for a fixed config: path/mult loops over baked tables,
/// real-basis CG entries flattened path-major. Mirrors ml/TensorProduct
/// loop order (paths -> muO -> mu1 -> mu2 -> entries); the forward w<>0
/// skip is omitted (adding exact zeros in the same order is the identity).
let private tpDecl (name: string) (cfg: TPConfig) : FunctionDecl =
    let stmts, ret = tpBodyStmts cfg "w"
    // O3: every term is a real-basis Clebsch-Gordan contraction over a path
    // `tpPaths` admitted, and that filter carries the full O(3) selection
    // rule (triangle inequality on l AND
    // `eo.Parity = parityMul e1.Parity e2.Parity`). An unreachable output
    // block is never written, and exact zero transforms as anything, so a
    // SpecOut wider than the decomposition is still covered.
    mkFunc name
        [ ("x", tyIrrepsArr cfg.Spec1); ("y", tyIrrepsArr cfg.Spec2); ("w", tyFloatArr (tpWeightDim cfg)) ]
        (tyIrrepsArr cfg.SpecOut) (syn (ExprBlock (stmts, Some ret)))
    |> equivStamp "O3"

/// derive_sym_tp / derive_alt_tp for a fixed spec: the S2-compacted
/// self-tensor-product derive_tp(S, S, x, y, w), compacted in ARITHMETIC as
/// well as in parameters. Only the KEPT paths (b1 <= b2, minus the tau = -1
/// multiplicity-1 diagonals) are emitted; each kept path's dropped
/// counterpart (the b1 > b2 mirror path, or the (u2, u1) half of a diagonal
/// path's tau-symmetric weight block) folds into the kept term as a SECOND
/// product against the same baked CG entry, per MLSpec.S2TpCell:
///
///   out[oo + mo*do + c3] += (coef * w[wb + mo*ws])
///                         * ( x[oA + c1]*y[oB + c2]
///                           + pairSign * (y[oA + e2]*x[oB + e1]) )
///
/// so the dense path table, the dense weight buffer and the expansion loop
/// all disappear. Collapsing the mirror path onto the kept path's CG table
/// is licensed by the cross-block exchange identity
/// realCG(l2,l1,l3)[m2,m1,m3] = sigma*realCG(l1,l2,l3)[m1,m2,m3], pinned
/// bit-exact in ml/Tests_Wigner.
///
/// Association is `(coef*w) * (x*y)` per term rather than tpDecl's
/// left-associated `((coef*w)*x)*y`, so in the F(x, x) case the two products
/// of a mirror cell are bit-identical and Lambda^2's `F(x, x) = 0` stays
/// EXACT (diagonal cells cancel across the CG entry pairs). Values differ in
/// the last ulps from the unfused kernel -- tolerance pin: relative 1e-13
/// against derive_tp on the embedded dense weights.
let private deriveS2TpDecl (name: string) (s: Spec) (comp: S2Component) : Result<FunctionDecl, ElabError> =
    let cfg = selfTpConfig s
    let packedDim = match comp with S2Sym -> symTpWeightDim s | S2Alt -> altTpWeightDim s
    let cells = s2TpCells comp s
    // The S2 split is a partition of the dense parameter space -- cheap to
    // check here, and a violation would mis-size a user's weight buffer.
    if not (s2TpSplitIsPartition s) then
        Error (err5000 ($"internal: the S2 split of the self-TP weight space is not a partition (sym {(symTpWeightDim s)} + alt {(altTpWeightDim s)} <> dense {(tpWeightDim cfg)})"))
    elif packedDim = 0 || cells.IsEmpty then
        Error (err5000 "internal: empty S2 component reached kernel synthesis (the call site must reject it as BL4007)")
    // Every packed slot must be read by exactly one (cell, mo): the cells
    // cover the buffer the user is asked to supply, or parameters are dead.
    elif cells |> List.sumBy _.MultO <> packedDim then
        Error (err5000 ($"internal: the fused S2 cell table reads {(cells |> List.sumBy _.MultO)} of the {packedDim} packed weight slots"))
    else
    let dO = totalDim cfg.SpecOut
    let paths = tpPaths cfg |> List.toArray
    // CG tables for the KEPT paths only, flattened in first-use order. `e1`/
    // `e2` are the partner term's component reads: the CG transpose on a
    // mirror path, the identity on a diagonal one (S2TpCell).
    let used = cells |> List.map (fun c -> (c.Path, c.IsMirror)) |> List.distinct
    let cgOf (p: int, isMirror: bool) =
        let (b1, b2, bo) = paths.[p]
        Blade.ML.WignerTables.realCGSparse s.[b1].L s.[b2].L cfg.SpecOut.[bo].L
        |> Array.toList
        |> List.map (fun e ->
            if isMirror then (e.C1, e.C2, e.C3, e.Coef, e.C2, e.C1)
            else (e.C1, e.C2, e.C3, e.Coef, e.C1, e.C2))
    let cgPerPath = used |> List.map cgOf
    let cgOff = (0, cgPerPath) ||> List.scan (fun acc es -> acc + es.Length)
    let cgRange =
        used |> List.mapi (fun i (p, _) -> (p, (cgOff.[i], cgOff.[i + 1]))) |> Map.ofList
    let flat = List.concat cgPerPath
    let pick f = flat |> List.map f
    let kI f = intArrLit (cells |> List.map f)
    let kIdx (t: string) = idx t (v "k")
    let tIdx (t: string) = idx t (v "t")
    let stmts =
        [ sLetMut "out" (zerosLit dO)
          sLet "__k_oa" (kI _.OffA)
          sLet "__k_ob" (kI _.OffB)
          sLet "__k_oo" (kI _.OutOff)
          sLet "__k_do" (kI _.OutDim)
          sLet "__k_nm" (kI _.MultO)
          sLet "__k_wb" (kI _.WBase)
          sLet "__k_ws" (kI _.WStride)
          sLet "__k_ps" (floatArrLit (cells |> List.map _.PairSign))
          sLet "__k_cl" (kI (fun c -> fst (Map.find c.Path cgRange)))
          sLet "__k_ch" (kI (fun c -> snd (Map.find c.Path cgRange)))
          sLet "__cg_c1" (intArrLit (pick (fun (a, _, _, _, _, _) -> a)))
          sLet "__cg_c2" (intArrLit (pick (fun (_, a, _, _, _, _) -> a)))
          sLet "__cg_c3" (intArrLit (pick (fun (_, _, a, _, _, _) -> a)))
          sLet "__cg_v" (floatArrLit (pick (fun (_, _, _, a, _, _) -> a)))
          sLet "__cg_e1" (intArrLit (pick (fun (_, _, _, _, a, _) -> a)))
          sLet "__cg_e2" (intArrLit (pick (fun (_, _, _, _, _, a) -> a)))
          sFor "k" 0 cells.Length
            [ StmtForIn ("mo", syn (ExprDotDot (iLit 0, kIdx "__k_nm")),
                [ sLet "wv" (idx "w" (add (kIdx "__k_wb") (mul (v "mo") (kIdx "__k_ws"))))
                  StmtForIn ("t", syn (ExprDotDot (kIdx "__k_cl", kIdx "__k_ch")),
                    [ sAccum (idx "out" (add (kIdx "__k_oo")
                                             (add (mul (v "mo") (kIdx "__k_do")) (tIdx "__cg_c3"))))
                             (mul (mul (tIdx "__cg_v") (v "wv"))
                                  (add (mul (idx "x" (add (kIdx "__k_oa") (tIdx "__cg_c1")))
                                            (idx "y" (add (kIdx "__k_ob") (tIdx "__cg_c2"))))
                                       (mul (kIdx "__k_ps")
                                            (mul (idx "y" (add (kIdx "__k_oa") (tIdx "__cg_e2")))
                                                 (idx "x" (add (kIdx "__k_ob") (tIdx "__cg_e1"))))))) ]) ]) ] ]
    // O3, for tpDecl's reason exactly. The S2 compaction is a
    // reparameterization of a SUBSPACE of the same hom-space -- it drops
    // weights, never relaxes a selection rule -- and the exchange symmetry
    // is a property of the weights, not of the equivariance claim.
    Ok (mkFunc name
            [ ("x", tyIrrepsArr s); ("y", tyIrrepsArr s); ("w", tyFloatArr packedDim) ]
            (tyIrrepsArr cfg.SpecOut) (syn (ExprBlock (stmts, Some (v "out"))))
        |> equivStamp "O3")

/// NOT equiv-stamped: a rep the lattice cannot name. The monomial
/// coordinates co-rotate POLYNOMIALLY (as Sym^K(V)), so the declared
/// plain-Idx result is neither a Rep of any spec nor an invariant.
///
/// The monomial lift x |-> its symmetric K-th power: a flat Idx<C(n+K-1, K)>
/// vector of the UNWEIGHTED monomials prod_j x(i_j) over the canonical
/// multisets i1 <= ... <= iK of 0..n-1 (n = total_dim(SPEC)), in LEX order
/// (the SymIdx<K, .> cell order). NO multiplicity weights: the cell IS the
/// coefficient of the monomial e_{i1}...e_{iK}, not the symmetrized-tensor
/// component (which differs by multiplicity(idx) per cell).
///
/// The K tuple-position tables are baked flat, one per position, so the
/// loop is a single pass over cells with a left-associated K-fold product.
/// The input is the irreps space; the OUTPUT is a plain Idx<cells> -- the
/// monomial space is not an irreps space (its O(3) action is Sym^K(V) =
/// ml.sym_spec(SPEC, K)).
///
/// `SymIdx<K, IrrepsIdx<SPEC>>` is writable and its storage agrees exactly
/// with this result, but the result is deliberately NOT declared as that
/// type: it would be rank-K vs the emitted rank-1 flat pass, different C++
/// shapes (`Array<double, 1>` vs the rank-K compact `Array<double, K>`).
/// The retype compiles in Blade (Unify's SymNone wildcard accepts one slot
/// against one slot) but produces C++ that does not build; fixing it means
/// REWRITING the body to K-ary canonical accesses, tracked as a follow-up.
let private symLiftDecl (name: string) (s: Spec) (k: int) : Result<FunctionDecl, ElabError> =
    let n = totalDim s
    let cells = binomial (n + k - 1) k
    if cells > 100000L then
        Error (err5000 ($"sym_lift: the degree-{k} monomial space of a dim-{n} input has C({(n + k - 1)}, {k}) = {cells} cells, over the 100000-cell limit -- lift a smaller spec (ml.scalars/ml.linear first) or lower K"))
    else
    let tuples = symMultisets n k
    let nCells = tuples.Length
    let stmts =
        [ yield sLetMut "out" (zerosLit nCells)
          for j in 0 .. k - 1 do
            yield sLet $"__m{j}" (intArrLit (tuples |> List.map (fun t -> t.[j])))
          yield StmtForIn ("c", syn (ExprDotDot (iLit 0, iLit nCells)),
                    [ sAssign (idx "out" (v "c"))
                              ([ 0 .. k - 1 ]
                               |> List.map (fun j -> idx "x" (idx $"__m{j}" (v "c")))
                               |> List.reduce mul) ]) ]
    Ok (mkFunc name [ ("x", tyIrrepsArr s) ] (tyFloatArr nCells)
            (syn (ExprBlock (stmts, Some (v "out")))))

/// derive_poly(SPEC, K, SOUT, x, w) for a fixed (spec, K, specOut): the
/// COMPLETE basis of the degree-K homogeneous equivariant maps V -> W, one
/// weight per basis map, `polyWeightDim SPEC K SOUT` of them. K = 1 is
/// derive_linear; K = 2 is derive_sym_tp's hom-space in the UNIFORM label
/// convention rather than the kept-path layout.
///
/// THE EMITTED KERNEL (ordinary Blade -- lets, loops, baked tables,
/// accumulate-only, so grad() differentiates it through its normal
/// inliner), in emission order:
///
///  1. MONOMIALS. Per (copy, degree) used by some label, the degree-j
///     monomials of that copy's own 2l+1 components over the canonical
///     multisets (lex `symMultisets`, no multiplicity weights), baked as j
///     position tables of absolute x indices.
///  2. OCCURRENCE FEATURES. The T_{j,l} matvec (SymPowerTables.symPowerTable,
///     sparse (row, cell, coef)): f[row] = sum_I T[occ][row, I]*mono_I, no
///     /N_I factor (lives in the Gram identity). At j = 1 there is no
///     table: Sym^1(V_l) = V_l and the feature IS the copy's slice of x --
///     what makes K = 1 reduce to derive_linear's arithmetic bit for bit.
///  3. CHAINS, SHARED BY PREFIX. Copies' features couple left-comb through
///     baked `realCGSparse` tables at unit weight; each distinct prefix
///     node (occurrence choices, intermediate L's) is emitted once, on
///     first appearance, giving code size ~ O(#labels) rather than
///     O(#labels*K). Each node is a `let mut` buffer written once, read
///     later (Grad.checkWriteAfterRead forbids read-then-rewrite).
///  4. SECTOR CONSTANT. sqrt(k!/prod_c j_c!), the cross-copy multinomial,
///     baked as an explicit scalar in the weight product (1.0 for every
///     single-copy sector, hence exactly 1.0 at K = 1).
///  5. WEIGHT MIXING. Per-label features land in one flat `__lf` buffer,
///     label-major; accumulation into SOUT is one table-driven loop.
///
/// WEIGHT LAYOUT (surface contract): LABEL-MAJOR -- `MLSpec.polyLabels`
/// order, then matching W copies (SOUT blocks of the label's (L, parity),
/// multiplicity inner). Agrees with `polyWeightDim` on COUNT but not
/// necessarily ORDER: homBlocks (derive_linear's) is OUTPUT-block-major
/// with input multiplicity innermost, coinciding only when no (l, parity)
/// has both mults > 1 (the K = 1 corpus pin). Label indices name
/// multiplicity SLOTS -- the basis is NOT GL(m)-channel-covariant.
let private derivePolyDecl (name: string) (s: Spec) (k: int) (sOut: Spec) : Result<FunctionDecl, ElabError> =
    let labels = polyLabels s k |> List.toArray
    let copies = polyCopies s |> List.toArray
    let inStarts = blockStarts s |> List.toArray
    let outStarts = blockStarts sOut |> List.toArray
    let copyOff (c: int) = inStarts.[copies.[c].Block] + copies.[c].MultIdx * (2 * copies.[c].L + 1)
    // The W copies a label can be mixed into: SOUT blocks of the label's
    // (L, parity), block order, multiplicity index inner.
    let matched (lb: PolyLabel) =
        [ for bo in 0 .. sOut.Length - 1 do
            if sOut.[bo].L = lb.L && sOut.[bo].Parity = lb.Parity then
              for mo in 0 .. sOut.[bo].Mult - 1 -> outStarts.[bo] + mo * (2 * lb.L + 1) ]
    // Labels with no matching W copy carry no parameter and are not emitted
    // (Schur: their contribution to every admissible map is zero).
    let used = labels |> Array.filter (fun lb -> not (matched lb).IsEmpty)
    let mutable lfAcc = 0
    let lfOff =
        used
        |> Array.map (fun lb ->
            let o = lfAcc
            lfAcc <- lfAcc + 2 * lb.L + 1
            (lb.Index, o))
        |> Map.ofArray
    let lfDim = lfAcc
    let slots =
        [ for lb in used do
            for oo in matched lb -> (lfOff.[lb.Index], oo, 2 * lb.L + 1, sqrt (float lb.Multinomial)) ]
    let wDim = slots.Length
    // The convention pin the two files cannot check against each other
    // (MLSpec stays dependency-free): a label's flat occurrence index must be
    // a direct index into symPowerTable's `Occurrences`.
    let occProblem =
        used |> Array.tryPick (fun lb ->
            lb.Uses |> List.tryPick (fun u ->
                if u.Degree < 2 then None
                else
                    let occs = (Blade.ML.SymPowerTables.symPowerTable u.Degree u.CopyL).Occurrences |> List.toArray
                    if u.Occ < 0 || u.Occ >= occs.Length then
                        Some ($"T_{{{u.Degree},{u.CopyL}}} has {occs.Length} occurrences but a label selects index {u.Occ}")
                    elif occs.[u.Occ].L <> u.OccL || occs.[u.Occ].Copy <> u.OccCopy then
                        Some (sprintf "T_{%d,%d} occurrence %d is (L=%d, copy=%d) but the label basis says (L=%d, copy=%d)"
                                  u.Degree u.CopyL u.Occ occs.[u.Occ].L occs.[u.Occ].Copy u.OccL u.OccCopy)
                    else None))
    if occProblem.IsSome then
        Error (err5000 $"internal: derive_poly occurrence order drift -- {occProblem.Value}")
    elif wDim <> polyWeightDim s k sOut then
        Error (err5000 (sprintf "internal: derive_poly enumerated %d weight slots label by label but poly_weight_dim says %d (spec %A, K %d, out %A)"
                            wDim (polyWeightDim s k sOut) s k sOut))
    else
    let stmts = ResizeArray<Stmt> ()
    let mutable ctr = 0
    let fresh (p: string) = ctr <- ctr + 1; $"__{p}{ctr}"
    stmts.Add (sLetMut "out" (zerosLit (totalDim sOut)))
    stmts.Add (sLetMut "__lf" (zerosLit (max lfDim 1)))
    // 1. monomial buffers, one per used (copy, degree), degree >= 2
    let monKeys =
        used
        |> Array.collect (fun lb -> lb.Uses |> List.filter (fun u -> u.Degree >= 2) |> List.map (fun u -> (u.Copy, u.Degree)) |> List.toArray)
        |> Array.toList
        |> List.distinct
    let mutable monNames : Map<int * int, string> = Map.empty
    for (c, j) in monKeys do
        let d = 2 * copies.[c].L + 1
        let off = copyOff c
        let tuples = symMultisets d j
        let nm = fresh "mn"
        monNames <- Map.add (c, j) nm monNames
        stmts.Add (sLetMut nm (zerosLit tuples.Length))
        for a in 0 .. j - 1 do
            stmts.Add (sLet $"{nm}_i{a}" (intArrLit (tuples |> List.map (fun t -> off + t.[a]))))
        stmts.Add (StmtForIn ("c", syn (ExprDotDot (iLit 0, iLit tuples.Length)),
                     [ sAccum (idx nm (v "c"))
                              ([ 0 .. j - 1 ]
                               |> List.map (fun a -> idx "x" (idx $"{nm}_i{a}" (v "c")))
                               |> List.reduce mul) ]))
    // 2. the T_{j,l} matvec, sparse (row, cell, coef)
    let emitMatvec (dstName: string) (dstBase: int) (u: PolyCopyUse) =
        let tbl = Blade.ML.SymPowerTables.symPowerTable u.Degree u.CopyL
        let occ = tbl.Occurrences |> List.item u.Occ
        let monNm = Map.find (u.Copy, u.Degree) monNames
        let entries =
            [ for r in 0 .. occ.Rows.Length - 1 do
                for i in 0 .. tbl.Cells.Length - 1 do
                  if abs occ.Rows.[r].[i] > 1e-12 then yield (dstBase + r, i, occ.Rows.[r].[i]) ]
        let nm = fresh "tt"
        stmts.Add (sLet (nm + "_d") (intArrLit (entries |> List.map (fun (a, _, _) -> a))))
        stmts.Add (sLet (nm + "_s") (intArrLit (entries |> List.map (fun (_, b, _) -> b))))
        stmts.Add (sLet (nm + "_v") (floatArrLit (entries |> List.map (fun (_, _, cc) -> cc))))
        stmts.Add (StmtForIn ("t", syn (ExprDotDot (iLit 0, iLit entries.Length)),
                     [ sAccum (idx dstName (idx (nm + "_d") (v "t")))
                              (mul (idx (nm + "_v") (v "t")) (idx monNm (idx (nm + "_s") (v "t")))) ]))
    // j = 1: the occurrence feature IS the copy's slice, copied verbatim into
    // the label buffer (0.0 + x is exact, so the K = 1 kernel reads exactly
    // the components derive_linear reads).
    let emitCopySlice (dstBase: int) (srcOff: int) (d: int) =
        stmts.Add (StmtForIn ("c", syn (ExprDotDot (iLit 0, iLit d)),
                     [ sAccum (idx "__lf" (add (iLit dstBase) (v "c")))
                              (idx "x" (add (iLit srcOff) (v "c"))) ]))
    // 3. one pairwise CG contraction, unit weight
    let emitCouple (dstName: string) (dstBase: int)
                   (aName: string, aBase: int, la: int) (bName: string, bBase: int, lb: int) (lMid: int) =
        let cg = Blade.ML.WignerTables.realCGSparse la lb lMid |> Array.toList
        let nm = fresh "cg"
        stmts.Add (sLet (nm + "_1") (intArrLit (cg |> List.map (fun e -> aBase + e.C1))))
        stmts.Add (sLet (nm + "_2") (intArrLit (cg |> List.map (fun e -> bBase + e.C2))))
        stmts.Add (sLet (nm + "_3") (intArrLit (cg |> List.map (fun e -> dstBase + e.C3))))
        stmts.Add (sLet (nm + "_v") (floatArrLit (cg |> List.map _.Coef)))
        stmts.Add (StmtForIn ("t", syn (ExprDotDot (iLit 0, iLit cg.Length)),
                     [ sAccum (idx dstName (idx (nm + "_3") (v "t")))
                              (mul (mul (idx (nm + "_v") (v "t")) (idx aName (idx (nm + "_1") (v "t"))))
                                   (idx bName (idx (nm + "_2") (v "t")))) ]))
    // Shared nodes: occurrence features keyed by (copy, degree, occ), chain
    // prefixes keyed by the whole (uses, chain) prefix. First appearance IS a
    // topological order -- a child key extends its parent's.
    let mutable nodes : Map<string, string> = Map.empty
    let useKey (u: PolyCopyUse) = $"{u.Copy}.{u.Degree}.{u.Occ}"
    let prefixKey (lb: PolyLabel) (m: int) =
        let us = lb.Uses |> List.truncate m |> List.map useKey |> String.concat ","
        let ch = lb.Chain |> List.truncate (m - 1) |> List.map string |> String.concat ","
        us + "|" + ch
    // Occurrence feature of one use: an x slice at j = 1, else its own buffer.
    let occFeature (u: PolyCopyUse) : string * int =
        if u.Degree = 1 then ("x", copyOff u.Copy)
        else
            let key = "o" + useKey u
            match Map.tryFind key nodes with
            | Some nm -> (nm, 0)
            | None ->
                let nm = fresh "of"
                stmts.Add (sLetMut nm (zerosLit (2 * u.OccL + 1)))
                emitMatvec nm 0 u
                nodes <- Map.add key nm nodes
                (nm, 0)
    for lb in used do
        let uses = lb.Uses |> List.toArray
        let dstBase = lfOff.[lb.Index]
        if uses.Length = 1 then
            // A single-copy sector has degree K, so its occurrence feature is
            // final and unique to this label: emit it straight into __lf.
            let u = uses.[0]
            if u.Degree = 1 then emitCopySlice dstBase (copyOff u.Copy) (2 * u.OccL + 1)
            else emitMatvec "__lf" dstBase u
        else
            let mutable accName = ""
            let mutable accBase = 0
            let mutable accL = uses.[0].OccL
            let a0, b0 = occFeature uses.[0]
            accName <- a0
            accBase <- b0
            for i in 1 .. uses.Length - 1 do
                let bName, bBase = occFeature uses.[i]
                let lMid = lb.Chain.[i - 1]
                if i = uses.Length - 1 then
                    // The last coupling has total degree K, so it is final and
                    // never a prefix of anything: it writes into __lf.
                    emitCouple "__lf" dstBase (accName, accBase, accL) (bName, bBase, uses.[i].OccL) lMid
                else
                    let key = prefixKey lb (i + 1)
                    match Map.tryFind key nodes with
                    | Some nm ->
                        accName <- nm
                        accBase <- 0
                    | None ->
                        let nm = fresh "ch"
                        stmts.Add (sLetMut nm (zerosLit (2 * lMid + 1)))
                        emitCouple nm 0 (accName, accBase, accL) (bName, bBase, uses.[i].OccL) lMid
                        nodes <- Map.add key nm nodes
                        accName <- nm
                        accBase <- 0
                accL <- lMid
    // 5. weight mixing, one table-driven loop
    stmts.Add (sLet "__w_fo" (intArrLit (slots |> List.map (fun (a, _, _, _) -> a))))
    stmts.Add (sLet "__w_oo" (intArrLit (slots |> List.map (fun (_, b, _, _) -> b))))
    stmts.Add (sLet "__w_d" (intArrLit (slots |> List.map (fun (_, _, c, _) -> c))))
    stmts.Add (sLet "__w_sc" (floatArrLit (slots |> List.map (fun (_, _, _, d) -> d))))
    stmts.Add (sFor "kk" 0 wDim
                 [ sLet "wv" (mul (idx "__w_sc" (v "kk")) (idx "w" (v "kk")))
                   StmtForIn ("c", syn (ExprDotDot (iLit 0, idx "__w_d" (v "kk"))),
                     [ sAccum (idx "out" (add (idx "__w_oo" (v "kk")) (v "c")))
                              (mul (v "wv") (idx "__lf" (add (idx "__w_fo" (v "kk")) (v "c")))) ]) ])
    // O3: the label basis is built by CG coupling chains off the input's
    // copies, so each label transforms as its own (L, parity) -- computed as
    // (-1)^(sum_c j_c * p_c), the honest O(3) parity of a degree-K monomial --
    // and `matched` above mixes a label ONLY into output blocks carrying that
    // same (L, parity). Unmatched labels carry no weight and are not emitted.
    Ok (mkFunc name [ ("x", tyIrrepsArr s); ("w", tyFloatArr wDim) ]
            (tyIrrepsArr sOut) (syn (ExprBlock (List.ofSeq stmts, Some (v "out"))))
        |> equivStamp "O3")

/// linear for fixed (specIn, specOut), nRows row vectors stored flat
/// (row-major; nRows = 1 is the single-vector `linear`): block-diagonal
/// multiplicity mixing, first-match input block, ml/Linear loop order
/// (blocks -> muO -> muI -> c), inside an outer row loop with x/out indices
/// offset by the row base. `rows` is MLSpec.linearBlocks output: one
/// (inputBlockIdx, eo, ei) per OUTPUT block, in output-block order. The
/// batched `linear_rows` form exists so callers do not hand-write
/// row-extract/write-back copy loops around the single-vector op.
let private linearDecl (name: string) (specIn: Spec) (specOut: Spec)
                       (rows: (int * SpecEntry * SpecEntry) list) (nRows: int) : FunctionDecl =
    let dIn = totalDim specIn
    let dOut = totalDim specOut
    let sIn = blockStarts specIn
    let sOut = blockStarts specOut
    let wDim = rows |> List.sumBy (fun (_, eo, ei) -> eo.Mult * ei.Mult)
    let baseIn = mul (v "rr") (iLit dIn)
    let baseOut = mul (v "rr") (iLit dOut)
    let mutable wOff = 0
    let blockStmts =
        rows |> List.mapi (fun b (bi, eo, ei) ->
            let d = dim eo
            let thisOff = wOff
            wOff <- wOff + eo.Mult * ei.Mult
            sFor "mo" 0 eo.Mult
                [ sFor "mi" 0 ei.Mult
                    [ sLet "wv" (idx "w" (add (iLit thisOff) (add (mul (v "mo") (iLit ei.Mult)) (v "mi"))))
                      sFor "c" 0 d
                        [ sAccum (idx "out" (add baseOut (add (iLit sOut.[b]) (add (mul (v "mo") (iLit d)) (v "c")))))
                                 (mul (v "wv")
                                      (idx "x" (add baseIn (add (iLit sIn.[bi]) (add (mul (v "mi") (iLit d)) (v "c")))))) ] ] ])
    let body =
        syn (ExprBlock (
            [ sLetMut "out" (zerosLit (nRows * dOut))
              sFor "rr" 0 nRows blockStmts ],
            Some (v "out")))
    // nRows = 1: x/out ARE the irreps spaces -- stamp them. nRows > 1: the
    // row-stacked buffers (extent nRows * total_dim) are not irreps spaces.
    let tyIn = if nRows = 1 then tyIrrepsArr specIn else tyFloatArr (nRows * dIn)
    let tyOut = if nRows = 1 then tyIrrepsArr specOut else tyFloatArr (nRows * dOut)
    let fd = mkFunc name [ ("w", tyFloatArr wDim); ("x", tyIn) ] tyOut body
    // O3 at nRows = 1: `linearBlocks` selects the input block by (l, parity)
    // equality, a SUB-basis of derive_linear's complete Schur basis (one
    // input block per output block instead of all matches), and a subspace
    // of the equivariant hom-space is equivariant.
    //
    // nRows > 1 is left UNSTAMPED: the row-stacked buffers are declared plain
    // `Idx<nRows * total_dim>`, so the signature carries no representation to
    // claim anything about (the seam refuses `linear_rows` by name for the
    // same reason) -- a stamp there would read "invariants in, invariants
    // out", vacuously true and misleading.
    if nRows = 1 then equivStamp "O3" fd else fd

/// gated for a fixed spec (nRows row vectors stored flat; nRows = 1 is the
/// single-vector `gated`): block-0 scalars silu'd AND reused as gates for
/// higher-L blocks (gate for multiplicity mu is
/// sigmoid(x[row_base + mu % numGates]) -- the F2 double-duty rule, per row,
/// mirroring ml/Activations.gated), inside an outer row loop.
let private gatedDecl (name: string) (sigmoidName: string) (spec: Spec) (nRows: int) : Result<FunctionDecl, ElabError> =
    if spec.IsEmpty then Error (err5000 "gated: empty spec")
    elif spec.Head.L <> 0 then Error (err5000 "gated: the first block must be scalars (L=0)")
    else
    let dTot = totalDim spec
    let starts = blockStarts spec
    let numGates = spec.Head.Mult
    let sigCall e = syn (ExprApp (v sigmoidName, [e]))
    let baseE = mul (v "rr") (iLit dTot)
    let rowStmts =
        [ for b in 0 .. spec.Length - 1 do
            let e = spec.[b]
            let d = dim e
            if e.L = 0 then
                yield sFor "mu" 0 e.Mult
                    [ sAssign (idx "out" (add baseE (add (iLit starts.[b]) (v "mu"))))
                              (mul (idx "x" (add baseE (add (iLit starts.[b]) (v "mu"))))
                                   (sigCall (idx "x" (add baseE (add (iLit starts.[b]) (v "mu")))))) ]
            else
                yield sFor "mu" 0 e.Mult
                    [ sLet "g" (sigCall (idx "x" (add baseE (syn (ExprBinOp (Elementwise, OpMod, v "mu", iLit numGates))))))
                      sFor "c" 0 d
                        [ sAssign (idx "out" (add baseE (add (iLit starts.[b]) (add (mul (v "mu") (iLit d)) (v "c")))))
                                  (mul (v "g")
                                       (idx "x" (add baseE (add (iLit starts.[b]) (add (mul (v "mu") (iLit d)) (v "c")))))) ] ] ]
    // nRows = 1: x/out ARE the irreps space (same spec in and out) -- stamp.
    let tyVec = if nRows = 1 then tyIrrepsArr spec else tyFloatArr (nRows * dTot)
    let fd =
        mkFunc name [ ("x", tyVec) ] tyVec
            (syn (ExprBlock (
                [ sLetMut "out" (zerosLit (nRows * dTot))
                  sFor "rr" 0 nRows rowStmts ],
                Some (v "out"))))
    // GROUP-CONDITIONAL at nRows = 1. Two things happen to l = 0 entries here
    // and both are parity-sensitive: the gate factor is
    // sigmoid(x[head + mu mod numGates]) -- an invariant scalar only if the
    // head block is parity-even -- and EVERY l = 0 block (not just the head)
    // is silu'd in place, which is not sign-equivariant. So
    // `o3UnlessPseudoscalar` weakens the whole spec to SO3 when any l = 0
    // block is odd. Blocks with l > 0 are only scaled by that one factor,
    // which commutes with D^l, so the output spec is the input spec.
    //
    // nRows > 1 is left UNSTAMPED for linearDecl's reason: row-stacked
    // buffers are not representation spaces.
    Ok (if nRows = 1 then equivStamp (o3UnlessPseudoscalar spec) fd else fd)

/// derive_linear for fixed (specIn, specOut): the COMPLETE Schur basis of
/// Hom_G(V_in, V_out) -- every (l, parity)-matched (input, output) block
/// pair mixes multiplicities, weight layout pair-major (MLSpec.homBlocks
/// order) mult_out x mult_in per pair, ACCUMULATING (+=) so duplicate
/// matches add; output blocks with no matching input stay exactly zero, the
/// unique equivariant completion. Mirrors ml/Linear.homLinear loop order
/// (pairs -> mo -> mi -> c) for ulp agreement.
let private deriveLinearDecl (name: string) (specIn: Spec) (specOut: Spec) : FunctionDecl =
    let dOut = totalDim specOut
    let sIn = blockStarts specIn
    let sOut = blockStarts specOut
    let pairs = homBlocks specIn specOut
    let wDim = pairs |> List.sumBy (fun (_, _, eo, ei) -> eo.Mult * ei.Mult)
    let mutable wOff = 0
    let pairStmts =
        pairs |> List.map (fun (bi, bo, eo, ei) ->
            let d = dim eo
            let thisOff = wOff
            wOff <- wOff + eo.Mult * ei.Mult
            sFor "mo" 0 eo.Mult
                [ sFor "mi" 0 ei.Mult
                    [ sLet "wv" (idx "w" (add (iLit thisOff) (add (mul (v "mo") (iLit ei.Mult)) (v "mi"))))
                      sFor "c" 0 d
                        [ sAccum (idx "out" (add (iLit sOut.[bo]) (add (mul (v "mo") (iLit d)) (v "c"))))
                                 (mul (v "wv")
                                      (idx "x" (add (iLit sIn.[bi]) (add (mul (v "mi") (iLit d)) (v "c"))))) ] ] ])
    let body =
        syn (ExprBlock (
            [ yield sLetMut "out" (zerosLit dOut)
              yield! pairStmts ],
            Some (v "out")))
    // O3: `homBlocks` admits an (input, output) block pair only when BOTH l
    // and parity agree, so the emitted basis is exactly the complete
    // Hom_{O(3)}, which is why zero-filled output blocks (no matching input)
    // are the unique equivariant completion rather than a gap in the claim.
    mkFunc name [ ("w", tyFloatArr wDim); ("x", tyIrrepsArr specIn) ] (tyIrrepsArr specOut) body
    |> equivStamp "O3"

// The POINT-GROUP block-spec surface -- `derive_linear`'s SECOND member, and
// the first place the Frobenius-Schur correction is visible in emitted code.
//
// Over R, Schur's lemma does NOT give one free scalar per (input copy,
// output copy) pair: it gives one free element of the division algebra
// End_G(U) in {R, C, H}, so a cell carries e = dim_R End_G(U) scalars:
//
//     dim_R Hom_G(+i mi*Ui, +i ni*Ui) = sum_i mi*ni*ei     (the FS formula)
//
// Every O(3) irrep is of real type, which is why `deriveLinearDecl` above
// can be `sum mi*ni` and still be right. Point groups break that: C4's E is
// of COMPLEX type (e = 2) while D4's E -- same dimension, same R90
// generator -- is of REAL type (e = 1); the difference lives in this file's
// `basis`.
//
// THE EMITTED BASIS OF A CELL is `PointSpec.endBasis`: [Id] at e = 1, [Id,
// J] at e = 2 (J the BAKED complex structure from the frozen table -- no
// call site "derives" it; it depends on the chosen real form):
//
//     e = 1:  out_block += w * x_block                (Id only)
//     e = 2:  out_block += w_Id * x_block + w_J * (J * x_block)
//
// WEIGHT LAYOUT: homBlocks order (pair-major, output-major within), mult_out
// x mult_in cells per pair, e scalars of a cell CONSECUTIVE ([Id, J]
// adjacent), so the e = 1 layout degenerates to `deriveLinearDecl`'s
// exactly. THE e = 1 PATH IS ITS LOOP NEST VERBATIM (same statement order,
// index association, `wv` binding), because an all-real-type point group
// must agree with a hand-written O(3)-shaped layer to the ULP, and that pin
// is only meaningful if the arithmetic is literally the same shape.

/// derive_pg_linear for a fixed (group, specIn, specOut): the COMPLETE
/// R-Schur basis of Hom_G(V_in, V_out) for a point group. See the block
/// comment above for the FS formula, the emitted cell basis and the layout.
let private derivePgLinearDecl (name: string) (grp: Blade.ML.PointSpec.PointGroup)
                               (specIn: Blade.ML.PointSpec.PgSpec)
                               (specOut: Blade.ML.PointSpec.PgSpec)
    : Result<FunctionDecl, ElabError> =
    let dOut = Blade.ML.PointSpec.pgTotalDim grp specOut
    let sIn = Blade.ML.PointSpec.pgBlockStarts grp specIn |> List.toArray
    let sOut = Blade.ML.PointSpec.pgBlockStarts grp specOut |> List.toArray
    let pairs = Blade.ML.PointSpec.pgHomBlocks grp specIn specOut
    let mutable wOff = 0
    let pairStmts =
        pairs |> List.collect (fun (bi, bo, (label, mOut), (_, mIn)) ->
            let ir = Blade.ML.PointSpec.pgIrrep grp label
            let d = ir.DimR
            // THE FsQuat GUARD: `endBasis` is the emission-adjacent path
            // MLPointSpec reserves the quaternionic value against -- counting
            // reads endDim FsQuat = 4 happily, this raises a loud internal
            // error. Calling it on EVERY pair (not just the e = 2 ones) is
            // what makes the guard reachable rather than decorative.
            let basis = Blade.ML.PointSpec.endBasis ir
            let e = List.length basis
            if not (Blade.ML.PointSpec.matEq (List.head basis) (Blade.ML.PointSpec.matId d)) then
                failwith $"internal: the End-basis of {grp.Name}::{label} does not lead with the identity -- the e = 1 emission path assumes it"
            let thisOff = wOff
            wOff <- wOff + mOut * mIn * e
            // The weight index of scalar `k` of cell (mo, mi): cells in
            // (mo, mi) row-major order, e scalars consecutive within a cell.
            let wAt (k: int) =
                let cell = add (mul (v "mo") (iLit mIn)) (v "mi")
                let flat = if e = 1 then cell else mul (iLit e) cell
                idx "w" (add (iLit thisOff) (if k = 0 then flat else add flat (iLit k)))
            if e = 1 then
                // VERBATIM `deriveLinearDecl`'s nest (mo -> mi -> c, one `wv`
                // let, one accumulate) -- the ulp pin depends on this shape.
                [ sFor "mo" 0 mOut
                    [ sFor "mi" 0 mIn
                        [ sLet "wv" (wAt 0)
                          sFor "c" 0 d
                            [ sAccum (idx "out" (add (iLit sOut.[bo]) (add (mul (v "mo") (iLit d)) (v "c"))))
                                     (mul (v "wv")
                                          (idx "x" (add (iLit sIn.[bi]) (add (mul (v "mi") (iLit d)) (v "c"))))) ] ] ] ]
            else
                // e = 2: the two-term form, with J's entries BAKED SPARSE. J
                // is +/-1-sparse over the shipped roster (matrix rationality
                // is what put C4/D4 there), so each output component gets
                // the Id term plus exactly the nonzero J terms written out --
                // no inner contraction loop, no zero multiplies. `c` is
                // unrolled because J's support varies per row; d <= 2 here.
                let j = List.item 1 basis
                let anyNeg = j |> Array.exists (Array.exists (fun v -> v < 0))
                [ sFor "mo" 0 mOut
                    [ sFor "mi" 0 mIn
                        ([ yield sLet "wid" (wAt 0)
                           yield sLet "wj" (wAt 1)
                           // -w bound once per cell (exact), so every
                           // negative J entry reuses one binding.
                           if anyNeg then yield sLet "wjn" (sub (fLit 0.0) (v "wj"))
                           for c in 0 .. d - 1 do
                             let outAt = idx "out" (add (iLit sOut.[bo]) (add (mul (v "mo") (iLit d)) (iLit c)))
                             let xAt (k: int) =
                                 idx "x" (add (iLit sIn.[bi]) (add (mul (v "mi") (iLit d)) (iLit k)))
                             yield sAccum outAt (mul (v "wid") (xAt c))
                             for k in 0 .. d - 1 do
                               let jv = j.[c].[k]
                               if jv = 1 then yield sAccum outAt (mul (v "wj") (xAt k))
                               elif jv = -1 then yield sAccum outAt (mul (v "wjn") (xAt k))
                               elif jv <> 0 then
                                   failwith $"internal: the baked J of {grp.Name}::{label} has entry {jv} at ({c}, {k}) -- the emitted two-term form bakes J SPARSE and handles only {{0, +-1}} (the shipped roster's matrix-rationality boundary)" ]) ] ])
    let wDim = wOff
    // The count is the theorem because the basis is emitted: the number of
    // weight slots the kernel actually reads, checked against the number the
    // user sized their buffer by (`ml.pg_hom_dim`).
    let declared = Blade.ML.PointSpec.pgHomDim grp specIn specOut
    if wDim <> declared then
        Error (err5000 ($"internal: derive_pg_linear emitted {wDim} weight slots but pg_hom_dim({grp.Name}, ...) says {declared}"))
    else
        let body =
            syn (ExprBlock (
                [ yield sLetMut "out" (zerosLit dOut)
                  yield! pairStmts ],
                Some (v "out")))
        // The POINT GROUP NAMED IN THE CALL, and nothing weaker or stronger.
        // This is derive_linear's construction over a finite group's frozen
        // character table -- cells connect equal LABELS only, with the
        // Frobenius-Schur correction supplying e = dim_R End_G(U) scalars per
        // cell -- so the emitted basis is exactly Hom_{grp}. It says nothing
        // about any other group: `grp.Name` is a registered point-group name
        // (MLPointSpec.pointGroupNames), which is what MLEquiv's parseGroup
        // accepts, and certificates do not transfer between groups.
        Ok (mkFunc name [ ("x", tyPgIrrepsArr grp.Name specIn); ("w", tyFloatArr wDim) ]
                (tyPgIrrepsArr grp.Name specOut) body
            |> equivStamp grp.Name)

// NOT equiv-stamped: a DIFFERENT DISCIPLINE, not a weaker claim. These
// kernels are equivariant for the node-relabelling action of Sn over flat
// N^K buffers (claim vocabulary `__ml_perm_equiv`, lattice MLPerm's);
// `__ml_equiv` names O(3)/SO(3)/point-group representation spaces, and no
// `IrrepsIdx` slot appears in any signature below, so stamping them here
// would be a category error, not conservatism.
//
// The Sn INDEX-ACTION surface
//
// `ml.derive_perm_linear(K, L, N, x, w)` is deriveLinearDecl's sibling for a
// FINITE group acting on INDICES rather than on irrep blocks: the complete
// basis of Hom_{Sn}(R^{N^K}, R^{N^L}), one weight per basis map. There is
// no spec, no character table and no Clebsch-Gordan anything, because for
// PERMUTATION modules the layer algebra is orbit combinatorics:
//
//     dim Hom_{Sn}(R^{N^K}, R^{N^L}) = #orbits of Sn on [N]^{K+L}
//                                    = #partitions of [K+L] into <= N blocks
//
// and the basis is the set of ORBIT (coarsening) INDICATORS B_gamma, one
// per partition gamma of the m = K + L axis positions (inputs 0..K-1,
// outputs K..K+L-1, per MLPermSpec's header). The count is the theorem
// because the basis is emitted; MLPermSpec.permPartitions supplies the
// partitions in canonical weight order and certifies the emitted order is
// a linear extension of refinement.
//
// BUFFERS ARE FLAT ROW-MAJOR: `x` is one `Idx<N^K>` axis, the result one
// `Idx<N^L>` axis, the `_rows` house precedent (linearDecl). L = 0 is the
// INVARIANT READOUT, `Array<Float like Idx<1>>` (N^0 = 1), matching the
// shape every other ml op gives a scalar result.

/// The flat-buffer cell cap of the Sn ops. ORTHOGONAL to
/// `PermSpec.checkPermSizing`, which decides which BASIS the surface admits;
/// this one is about emitted code size -- `out` is materialized as a literal
/// array of N^L zeros, so the extent appears verbatim in the generated
/// source. Same number and reason as symLiftDecl's monomial cap.
let private permCellCap = 100000

/// N^e, saturating just past `permCellCap`. N is bounded only by the caller's
/// sanity range, so the honest product can overflow int64 -- the only
/// question this answers is "is the buffer over the cap?".
let private permPow (n: int) (e: int) : int64 =
    let mutable acc = 1L
    let mutable i = 0
    while i < e && acc <= int64 permCellCap do
        acc <- acc * int64 n
        i <- i + 1
    acc

/// THE EMITTED KERNEL, shared by derive_perm_linear and derive_perm_bias:
/// ONE LOOP NEST PER PARTITION, in `permPartitions` order (so weight slot g
/// is partition g). For partition gamma, one `let`-free block variable per
/// gamma-BLOCK, each ranging over 0..N-1 (b(gamma) loops deep), with the two
/// flat indices read off the position convention directly:
///
///     inIdx  = sum_{i=0..K-1}   v_{gamma(i)} * N^{K-1-i}    (input positions)
///     outIdx = sum_{i=K..K+L-1} v_{gamma(i)} * N^{K+L-1-i}  (output positions)
///     out(outIdx) += w(g) * x(inIdx)         [bias: += b(g), no x factor]
///
/// That IS the orbit indicator B_gamma contracted with x: the nest visits
/// exactly the tuples of [N]^m constant on every block of gamma, adding
/// each one's x-cell into its out-cell.
///
/// THE SUM / GATHER / BROADCAST CLASSIFICATION NEEDS NO CODE -- it falls
/// out of which index a block variable appears in: INPUT-only (in inIdx,
/// not outIdx) is a SUMMATION (K=L=1's `sum(x)`, K=2/L=0's trace); MIXED
/// (both) is a GATHER (identity, transpose, diagonal read); OUTPUT-only is
/// a BROADCAST. Nothing below branches on this: one uniform nest, whatever
/// semantics the index expressions give it.
///
/// Accumulate-only (`+=` into a zero-initialized `out`, never a
/// read-then-rewrite), so grad() differentiates it through its normal
/// inliner. Code size is sum_gamma b(gamma) loops -- 37 at the Maron point
/// (K=L=2), bounded by the K+L <= 6 cap at Bell(6) = 203 partitions.
let private permNestStmts (k: int) (l: int) (n: int) (coefName: string) (readsX: bool)
                          (parts: int[] list) : Stmt list =
    parts |> List.mapi (fun g rgs ->
        let nBlocks = Blade.ML.PermSpec.blockCount rgs
        let bv (j: int) = $"__pv{g}_{j}"
        // sum_{i=lo..hi-1} v_{gamma(i)} * N^{hi-1-i}: the flat row-major index
        // of one axis run. The EMPTY run (K = 0 bias inputs, or L = 0
        // outputs) is the single cell 0 -- N^0 = 1.
        let flat (lo: int) (hi: int) =
            if lo >= hi then iLit 0
            else
                [ for i in lo .. hi - 1 ->
                    let coef = pown n (hi - 1 - i)
                    if coef = 1 then v (bv rgs.[i]) else mul (iLit coef) (v (bv rgs.[i])) ]
                |> List.reduce add
        let term =
            if readsX then mul (idx coefName (iLit g)) (idx "x" (flat 0 k))
            else idx coefName (iLit g)
        let body = [ sAccum (idx "out" (flat k (k + l))) term ]
        // b(gamma) block loops, block 0 outermost. b(gamma) = 0 only at
        // m = 0, where the "nest" is the bare body: out(0) += b(0).
        List.foldBack (fun j inner -> [ sFor (bv j) 0 n inner ]) [ 0 .. nBlocks - 1 ] body
        |> List.exactlyOne)

/// derive_perm_linear for a fixed (K, L, N): the complete Sn-equivariant
/// linear layer R^{N^K} -> R^{N^L}, `ml.perm_weight_dim(K, L, N)` weights,
/// weight slot g = the g-th partition in `permPartitions (K+L) N` order. See
/// the block comment above for the kernel and the position convention.
let private derivePermLinearDecl (name: string) (k: int) (l: int) (n: int)
    : Result<FunctionDecl, ElabError> =
    let inCells = permPow n k
    let outCells = permPow n l
    if inCells > int64 permCellCap || outCells > int64 permCellCap then
        Error (err5000 ($"derive_perm_linear: the flat node-power buffers of K = {k}, L = {l}, N = {n} are N^K and N^L cells, and at least one is over the {permCellCap}-cell limit -- the emitted kernel materializes the output as a literal zero array of that extent. Lower N (the node-axis extent), or lower K / L"))
    else
    let parts = Blade.ML.PermSpec.permPartitions (k + l) n
    let wDim = List.length parts
    // The count is the theorem because the basis is emitted: one nest per
    // weight slot, checked against the number the user sized their buffer by.
    if wDim <> Blade.ML.PermSpec.permWeightDim k l n then
        Error (err5000 ($"internal: derive_perm_linear emitted {wDim} loop nests but perm_weight_dim({k}, {l}, {n}) says {(Blade.ML.PermSpec.permWeightDim k l n)}"))
    else
        let stmts = sLetMut "out" (zerosLit (int outCells)) :: permNestStmts k l n "w" true parts
        Ok (mkFunc name [ ("x", tyFloatArr (int inCells)); ("w", tyFloatArr wDim) ]
                (tyFloatArr (int outCells)) (syn (ExprBlock (stmts, Some (v "out")))))

/// derive_perm_bias for a fixed (L, N): the REP-INTRODUCTION form -- the
/// complete space of Sn-invariant constants in R^{N^L},
/// `ml.perm_bias_dim(L, N)` of them. It is derive_perm_linear at K = 0
/// (partitions of the L output positions alone, every block output-only,
/// hence every nest a pure broadcast), which is exactly why K = 0 is
/// REFUSED by the linear op with a pointer here rather than silently
/// accepted.
let private derivePermBiasDecl (name: string) (l: int) (n: int)
    : Result<FunctionDecl, ElabError> =
    let outCells = permPow n l
    if outCells > int64 permCellCap then
        Error (err5000 ($"derive_perm_bias: the flat node-power buffer of L = {l}, N = {n} is N^L cells, over the {permCellCap}-cell limit -- the emitted kernel materializes it as a literal zero array of that extent. Lower N (the node-axis extent), or lower L"))
    else
    let parts = Blade.ML.PermSpec.permPartitions l n
    let bDim = List.length parts
    if bDim <> Blade.ML.PermSpec.permBiasDim l n then
        Error (err5000 ($"internal: derive_perm_bias emitted {bDim} loop nests but perm_bias_dim({l}, {n}) says {(Blade.ML.PermSpec.permBiasDim l n)}"))
    else
        let stmts = sLetMut "out" (zerosLit (int outCells)) :: permNestStmts 0 l n "b" false parts
        Ok (mkFunc name [ ("b", tyFloatArr bDim) ] (tyFloatArr (int outCells))
                (syn (ExprBlock (stmts, Some (v "out")))))

/// perm_matmul for a fixed N: the flat N^2-buffer matrix product
///
///     out(i*N + j) += a(i*N + t) * b(t*N + j)
///
/// -- PPGN's engine (Maron et al.'s provably powerful graph network), and the
/// ONE BILINEAR SHIPPED BY NAME rather than by synthesis. Naming it is the
/// point: the S_n-equivariant bilinear maps R^{N^2} x R^{N^2} -> R^{N^2} are
/// a large space with no analogue of the orbit-indicator basis at this
/// arity, so this ships the one map the literature actually uses and defers
/// the synthesis (`derive_perm_tp`, the Burnside/orbit-quotient
/// construction) as a named item rather than pretending to a complete basis.
///
/// The equivariance is one line: conjugating both factors by a permutation
/// matrix P conjugates the product, (P A P^T)(P B P^T) = P (A B) P^T, which
/// is exactly "both arguments are Pow 2 and so is the result" in the MLPerm
/// lattice.
///
/// Buffers are the same FLAT ROW-MAJOR N^2 convention as derive_perm_linear
/// at K = L = 2, so a matmul composes with the derived layers with no
/// reshape. Accumulate-only into a zero-initialized `out`, so grad()
/// differentiates it through its normal inliner.
let private permMatmulDecl (name: string) (n: int) : FunctionDecl =
    let cells = n * n
    let flat (r: string) (c: string) = add (mul (iLit n) (v r)) (v c)
    let body =
        syn (ExprBlock (
            [ sLetMut "out" (zerosLit cells)
              sFor "i" 0 n
                [ sFor "j" 0 n
                    [ sFor "t" 0 n
                        [ sAccum (idx "out" (flat "i" "j"))
                                 (mul (idx "a" (flat "i" "t")) (idx "b" (flat "t" "j"))) ] ] ] ],
            Some (v "out")))
    mkFunc name [ ("a", tyFloatArr cells); ("b", tyFloatArr cells) ] (tyFloatArr cells) body

/// scalars for a fixed spec: the l=0 blocks' entries copied into a plain
/// Idx array (block order, multiplicity order) -- an invariant-exit op, the
/// compile-time twin of ml/Activations.scalars (pure copies, ulp-trivial).
/// Emits ALL l=0 entries regardless of parity; the equiv judgment governs
/// which callers may treat them as invariants (O3 rejects (0, odd) specs).
let private scalarsDecl (name: string) (spec: Spec) : Result<FunctionDecl, ElabError> =
    let starts = blockStarts spec
    let offs =
        [ for b in 0 .. spec.Length - 1 do
            if spec.[b].L = 0 then
                yield! [ starts.[b] .. starts.[b] + spec.[b].Mult - 1 ] ]
    if offs.IsEmpty then Error (err5000 "scalars: the spec has no l=0 blocks")
    else
        let stmts =
            [ yield sLetMut "out" (zerosLit offs.Length)
              for k in 0 .. offs.Length - 1 do
                yield sAssign (idx "out" (iLit k)) (idx "x" (iLit offs.[k])) ]
        // GROUP-CONDITIONAL. The declared return type is a plain Idx array,
        // i.e. the claim is "these entries are INVARIANTS". True of every
        // l = 0 entry under SO(3); true under O(3) only when no l = 0 block
        // is parity-odd, since a pseudoscalar flips under improper
        // rotations. Same predicate the seam's `scalars` arm applies.
        Ok (mkFunc name [ ("x", tyIrrepsArr spec) ] (tyFloatArr offs.Length)
                (syn (ExprBlock (stmts, Some (v "out"))))
            |> equivStamp (o3UnlessPseudoscalar spec))

/// norms for a fixed spec: per-(block, multiplicity) 2-norms in (block, mu)
/// order -- mirrors ml/Activations.norms exactly (sum of squares in
/// ascending component order, then sqrt). O(3)-invariant for every parity.
/// Squares accumulate into a scratch buffer and `out` is written ONCE per
/// slot: grad() differentiates `x = x + e` accumulation but rejects the
/// general read-then-rewrite `out(k) = sqrt(out(k))`.
let private normsDecl (name: string) (spec: Spec) : FunctionDecl =
    let starts = blockStarts spec
    let slots =
        [ for b in 0 .. spec.Length - 1 do
            for mu in 0 .. spec.[b].Mult - 1 ->
              (starts.[b] + mu * dim spec.[b], dim spec.[b]) ]
    let stmts =
        [ yield sLetMut "sq" (zerosLit slots.Length)
          yield sLetMut "out" (zerosLit slots.Length)
          // ALL square-accumulation first, THEN all sqrt reads: grad's
          // read-then-rewrite analysis is per-variable, so no write to sq
          // may follow any read of sq.
          yield! slots
                 |> List.mapi (fun k (off, d) ->
                     sFor "c" 0 d
                         [ sAccum (idx "sq" (iLit k))
                                  (mul (idx "x" (add (iLit off) (v "c")))
                                       (idx "x" (add (iLit off) (v "c")))) ])
          for k in 0 .. slots.Length - 1 do
            yield sAssign (idx "out" (iLit k)) (syn (ExprApp (v "sqrt", [ idx "sq" (iLit k) ]))) ]
    // O3, unconditionally: the one l = 0 exporter that needs no parity
    // side-condition. Each slot is the Euclidean norm of one (block,
    // multiplicity) component vector, and every O(3) irrep in the real basis
    // acts by an ORTHOGONAL matrix (parity contributes only an overall sign,
    // which the sum of squares annihilates), so a norm is invariant under the
    // full group including improper elements.
    mkFunc name [ ("x", tyIrrepsArr spec) ] (tyFloatArr slots.Length)
        (syn (ExprBlock (stmts, Some (v "out"))))
    |> equivStamp "O3"

/// NOT equiv-stamped, in either direction. `tensor_to_irreps` /
/// `sym_to_irreps` are rep-INTRODUCTION forms carrying y_to's unstatable
/// premise; `irreps_to_sym` is a rep ESCAPE whose declared invariant result
/// is a vector of basis-dependent Cartesian components, and MLEquiv rejects
/// it by name inside a certified body. See the stamping header above.
///
/// Cartesian<->irreps bridge ops (rank-2, 3-D): a dense matvec over the
/// baked orthonormal closed-form table (Blade.ML.CartesianBridge -- the
/// single source of truth, fit-certified against SphericalHarmonics by the
/// ml/ `dump-cartesian` oracle). Loop order mirrors the oracle's matvec
/// (i ascending, j ascending) for ulp agreement with the sgs corpus pins.
let private bridgeDecl (name: string) (table: float list) (n: int)
                       (pName: string) (tyIn: TypeExpr) (tyOut: TypeExpr) : FunctionDecl =
    let body =
        syn (ExprBlock (
            [ sLetMut "out" (zerosLit n)
              sLet "__b" (floatArrLit table)
              sFor "i" 0 n
                [ sFor "j" 0 n
                    [ sAccum (idx "out" (v "i"))
                             (mul (idx "__b" (add (mul (v "i") (iLit n)) (v "j")))
                                  (idx pName (v "j"))) ] ] ],
            Some (v "out")))
    mkFunc name [ (pName, tyIn) ] tyOut body

// Call-site recognition + program expansion

let private opNames =
    Set.ofList [ "y_to"; "tensor_product"; "linear"; "gated"; "linear_rows"; "gated_rows"
                 "scalars"; "norms"; "derive_linear"; "derive_tp"
                 "derive_sym_tp"; "derive_alt_tp"; "sym_lift"; "derive_poly"
                 "derive_perm_linear"; "derive_perm_bias"; "perm_matmul"
                 "derive_pg_linear"
                 "tensor_to_irreps"; "sym_to_irreps"; "irreps_to_sym" ]

/// Static sizing builtins that make up the rest of the ML surface (used in
/// `let static` positions). Registered in the static evaluator under mangled
/// internal names (Blade.ML.Statics.statName); a qualified `ml.total_dim(...)`
/// is normalized to that internal name here, so bare `total_dim(...)` no
/// longer resolves. Keep in sync with the registrations in MLStatics.install.
let private sizingNames =
    Set.ofList [ "sh_spec"; "total_dim"; "tp_weight_dim"; "linear_weight_dim"
                 "tp_spec"; "hom_dim"; "tp_full_weight_dim"
                 "sym_tp_weight_dim"; "alt_tp_weight_dim"
                 "sym_spec"; "alt_spec"; "poly_weight_dim"
                 "perm_weight_dim"; "perm_bias_dim"
                 "irreps_len"; "irreps_l"; "irreps_parity"; "irreps_mult"
                 "irreps_dim"; "irreps_offset"
                 // The point-group sizing surface. All ints except
                 // `pg_restrict`, the restriction table, which returns a pg
                 // SPEC.
                 "pg_total_dim"; "pg_hom_dim"; "pg_irreps_len"
                 "pg_irreps_dim"; "pg_irreps_mult"; "pg_irreps_fs"
                 "pg_irreps_offset"; "pg_restrict" ]

type private ElabState = {
    mutable Counter: int
    /// (op, config fingerprint) -> generated function name
    mutable Made: Map<string, string>
    /// generated decls in creation order
    mutable Decls: FunctionDecl list
    mutable SigmoidName: string option
}

let private fingerprint (op: string) (parts: obj) : string =
    sprintf "%s|%A" op parts

/// Resolve a static-argument expression: must be a plain variable naming a
/// `let static` binding (or an inline int literal for lmax).
let private staticArg (statics: StaticEnv) (what: string) (e: Expr) : Result<StaticValue, ElabError> =
    match e.Kind with
    | ExprKind.ExprLit (LitInt n) -> Ok (SVInt n)
    | ExprKind.ExprVar name ->
        match Map.tryFind name statics.Values with
        | Some sv -> Ok sv
        | None -> Error (err5000 $"{what}: '{name}' is not a `let static` binding (ML op configs must be static)")
    | _ -> Error (err5000 $"{what}: config argument must be a `let static` binding name or literal")

/// Resolve the GROUP argument of a point-group op. Two accepted spellings,
/// tried in order: (1) a `let static` binding holding a STRING -- the
/// ordinary static-argument contract every other op config obeys, and the
/// spelling the pg SIZING builtins require, since those go through
/// StaticEval; (2) a BARE IDENTIFIER naming a registered group --
/// `ml.derive_pg_linear(C4, ...)`, exactly how the group reads in
/// `PgIrrepsIdx<C4, SPEC>` and in the mathematics. Legal here and nowhere
/// else because this pass REWRITES the call before the checker sees it: the
/// group argument is consumed at elaboration and never reaches name
/// resolution. Statics win, so an explicit `let static G = "D4"` is never
/// shadowed by the registry; a string literal also works, via arm 1.
let private pgGroupArg (statics: StaticEnv) (what: string) (e: Expr)
    : Result<Blade.ML.PointSpec.PointGroup, ElabError> =
    let byName (n: string) =
        Blade.ML.Statics.pgGroupByName what n |> Result.mapError err5000
    match e.Kind with
    | ExprKind.ExprLit (LitString s) -> byName s
    | ExprKind.ExprVar name ->
        match Map.tryFind name statics.Values with
        | Some (SVString s) -> byName s
        | Some _ ->
            Error (err5000 ($"{what}: '{name}' is a `let static` binding but not a STRING -- GROUP names a registered point group, e.g. \"C4\""))
        | None -> byName name
    | _ ->
        Error (err5000 $"{what}: GROUP must be a point-group name (a bare C4 / D4, a string literal, or a `let static` string binding)")

let private ensureSigmoid (st: ElabState) : string =
    match st.SigmoidName with
    | Some n -> n
    | None ->
        let n = "__ml_sigmoid"
        st.SigmoidName <- Some n
        st.Decls <- st.Decls @ [ sigmoidDecl n ]
        n

let private ensure (st: ElabState) (key: string) (make: string -> Result<FunctionDecl, ElabError>)
    : Result<string, ElabError> =
    match Map.tryFind key st.Made with
    | Some n -> Ok n
    | None ->
        st.Counter <- st.Counter + 1
        let n = $"__ml_{st.Counter}"
        make n |> Result.map (fun decl ->
            st.Made <- Map.add key n st.Made
            st.Decls <- st.Decls @ [ decl ]
            n)

/// Shared elaboration for linear / linear_rows (nRows = 1 is the
/// single-vector form; the fingerprint includes nRows so each batch size
/// gets its own generated function).
/// `site` is the surface call expression: the generated application inherits
/// its span (as y_to/tensor_product/scalars already do) so diagnostics and
/// editor tooling reading back the checked call land on what the user wrote
/// rather than on the ambient per-declaration synthetic span.
let private elabLinear (st: ElabState) (statics: StaticEnv) (what: string) (site: Expr)
                       (sInE: Expr) (sOutE: Expr) (nRows: int) (wE: Expr) (xE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics (what + " specIn") sInE |> Result.bind (fun svi ->
    staticArg statics (what + " specOut") sOutE |> Result.bind (fun svo ->
    specOfStatic (what + " specIn") svi |> Result.bind (fun si ->
    specOfStatic (what + " specOut") svo |> Result.bind (fun so ->
    (match linearBlocks si so with
     | Ok rows -> Ok rows
     | Error detail ->
        // Two Schur failure grades: no shared (l, parity) at all means the
        // whole hom-space is zero; a partial miss keeps the classic
        // all_irreps_present framing from linearBlocks.
        if homDim si so = 0 then
            Error (err4007 $"{what}: no equivariant linear map exists from the input spec to the output spec -- by Schur's lemma an equivariant linear map can only connect irreps of identical (l, parity), and these specs share none: every admissible map is zero")
        else
            Error (err4007 (detail + " -- the only equivariant map into that block is zero (Schur's lemma); ml.derive_linear gives the zero-completed complete basis")))
    |> Result.bind (fun rows ->
        ensure st (fingerprint "linear" (box (si, so, nRows))) (fun n -> Ok (linearDecl n si so rows nRows))
        |> Result.map (fun n -> inheritSpan site (ExprApp (v n, [ wE; xE ]))))))))

/// Shared elaboration for gated / gated_rows. `site`: see elabLinear.
let private elabGated (st: ElabState) (statics: StaticEnv) (what: string) (site: Expr)
                      (specE: Expr) (nRows: int) (xE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics (what + " spec") specE |> Result.bind (fun sv ->
    specOfStatic what sv |> Result.bind (fun spec ->
        let sig_ = ensureSigmoid st
        ensure st (fingerprint "gated" (box (spec, nRows))) (fun n -> gatedDecl n sig_ spec nRows)
        |> Result.map (fun n -> inheritSpan site (ExprApp (v n, [ xE ])))))

/// Shared elaboration for derive_linear's call and binding forms: resolve
/// specs, refuse the Schur-zero case (BL4007), synthesize (or reuse) the
/// complete-basis layer, return the generated name.
let private elabDeriveLinear (st: ElabState) (statics: StaticEnv) (sInE: Expr) (sOutE: Expr)
    : Result<string, ElabError> =
    staticArg statics "derive_linear specIn" sInE |> Result.bind (fun svi ->
    staticArg statics "derive_linear specOut" sOutE |> Result.bind (fun svo ->
    specOfStatic "derive_linear specIn" svi |> Result.bind (fun si ->
    specOfStatic "derive_linear specOut" svo |> Result.bind (fun so ->
        if homDim si so = 0 then
            Error (err4007 "derive_linear: no equivariant linear map exists from the input spec to the output spec -- by Schur's lemma an equivariant linear map can only connect irreps of identical (l, parity), and these specs share none: every admissible map is zero")
        else
            ensure st (fingerprint "derive_linear" (box (si, so))) (fun n -> Ok (deriveLinearDecl n si so))))))

/// Shared elaboration for derive_tp: the output spec is DERIVED as the full
/// CG decomposition (tpSpec), so allValidOutputs holds by construction.
/// Shares the "tp" fingerprint: an explicit full-config tensor_product
/// dedups to the same generated function.
let private elabDeriveTp (st: ElabState) (statics: StaticEnv) (s1E: Expr) (s2E: Expr)
    : Result<string, ElabError> =
    staticArg statics "derive_tp spec1" s1E |> Result.bind (fun sv1 ->
    staticArg statics "derive_tp spec2" s2E |> Result.bind (fun sv2 ->
    specOfStatic "derive_tp spec1" sv1 |> Result.bind (fun s1 ->
    specOfStatic "derive_tp spec2" sv2 |> Result.bind (fun s2 ->
        let cfg = { Spec1 = s1; Spec2 = s2; SpecOut = tpSpec s1 s2 }
        ensure st (fingerprint "tp" (box cfg)) (fun n -> Ok (tpDecl n cfg))))))

/// Shared elaboration for derive_sym_tp / derive_alt_tp: the S2-compacted
/// self-TP, one spec argument (both inputs and the derived output follow
/// from it). BL4007 when the requested component is EMPTY -- then every map
/// of that exchange symmetry is zero, the exact analogue of derive_linear's
/// Schur-zero refusal. Fingerprints are distinct from "tp" (different weight
/// arity), so the dense and compacted kernels for the same spec coexist.
let private elabDeriveS2Tp (st: ElabState) (statics: StaticEnv) (comp: S2Component) (site: Expr)
                           (specE: Expr) (xE: Expr) (yE: Expr) (wE: Expr)
    : Result<Expr, ElabError> =
    let what, key, thisName, otherName =
        match comp with
        | S2Sym -> "derive_sym_tp", "sym_tp", "symmetric", "antisymmetric"
        | S2Alt -> "derive_alt_tp", "alt_tp", "antisymmetric", "symmetric"
    staticArg statics (what + " spec") specE |> Result.bind (fun sv ->
    specOfStatic (what + " spec") sv |> Result.bind (fun s ->
        let packedDim = match comp with S2Sym -> symTpWeightDim s | S2Alt -> altTpWeightDim s
        if packedDim = 0 then
            Error (err4007 (sprintf "%s: no nonzero exchange-%s equivariant bilinear map exists on this spec -- every map in the %s component is zero for this spec, so the whole hom-space sits in the %s component (use ml.derive_%s_tp, or ml.derive_tp for the uncompacted parameterization)"
                                what thisName thisName otherName
                                (match comp with S2Sym -> "alt" | S2Alt -> "sym")))
        else
            ensure st (fingerprint key (box s)) (fun n -> deriveS2TpDecl n s comp)
            |> Result.map (fun n -> inheritSpan site (ExprApp (v n, [ xE; yE; wE ])))))

/// Shared elaboration for derive_poly: resolve (SPEC, K, SOUT), gate K to
/// 1..4 and the label count to the 100000-cell cap (symLiftDecl's precedent
/// -- the label basis is indexed by the same Sym^K cells), refuse the
/// Schur-zero case with BL4007 in derive_linear's framing, then synthesize
/// the complete degree-K basis.
let private elabDerivePoly (st: ElabState) (statics: StaticEnv) (site: Expr)
                           (specE: Expr) (kE: Expr) (sOutE: Expr) (xE: Expr) (wE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics "derive_poly SPEC" specE |> Result.bind (fun sv ->
    specOfStatic "derive_poly SPEC" sv |> Result.bind (fun s ->
    staticArg statics "derive_poly K" kE |> Result.bind (fun kv ->
    staticArg statics "derive_poly SOUT" sOutE |> Result.bind (fun sov ->
    specOfStatic "derive_poly SOUT" sov |> Result.bind (fun sOut ->
        match kv with
        | SVInt kk when kk >= 1L && kk <= 4L ->
            let k = int kk
            let n = totalDim s
            let cells = binomial (n + k - 1) k
            if cells > 100000L then
                Error (err5000 ($"derive_poly: the degree-{k} symmetric power of a dim-{n} input has C({(n + k - 1)}, {k}) = {cells} cells, over the 100000-cell limit -- the label basis is one vector per cell, so the emitted kernel would be unusable; lower K, or reduce the input spec first (ml.scalars / ml.linear). The channel-shared degree-K op that amortizes one basis over many multiplicity slots is future work"))
            elif polyWeightDim s k sOut = 0 then
                Error (err4007 ($"derive_poly: no equivariant degree-{k} polynomial map exists from the input spec to the output spec -- by Schur's lemma a degree-{k} homogeneous equivariant map is a linear map out of Sym^{k} of the input (ml.sym_spec(SPEC, {k})), which can only connect irreps of identical (l, parity), and those specs share none: every admissible map is zero"))
            else
                ensure st (fingerprint "derive_poly" (box (s, k, sOut))) (fun n2 -> derivePolyDecl n2 s k sOut)
                |> Result.map (fun n2 -> inheritSpan site (ExprApp (v n2, [ xE; wE ])))
        | SVInt kk ->
            Error (err5000 $"derive_poly: K must be a static int in 1..4 (got {kk}) -- the symmetric-power surface is capped at degree 4 (retired transforms-as-types plan section 6.5)")
        | _ -> Error (err5000 "derive_poly: K must be a static int"))))))

/// Shared elaboration for derive_pg_linear: resolve the group, decode the
/// two label-named specs against ITS table, refuse the Schur-zero case, then
/// synthesize (or reuse) the complete R-Schur basis.
///
/// The zero case is BL4007 in derive_linear's framing -- the same code, the
/// same theorem, one block-spec member over. Its finite-group reading is
/// sharper: `pg_hom_dim = 0` says the two specs share no LABEL, and by Schur
/// over R every equivariant map between modules with no common irreducible
/// constituent is zero. BL4007 "no equivariant map exists" is a TITLE, and
/// this is another instance of exactly that title -- unlike the BL4011
/// double-booking, which was two different meanings under one code.
let private elabDerivePgLinear (st: ElabState) (statics: StaticEnv) (site: Expr)
                               (groupE: Expr) (sInE: Expr) (sOutE: Expr) (xE: Expr) (wE: Expr)
    : Result<Expr, ElabError> =
    pgGroupArg statics "derive_pg_linear GROUP" groupE |> Result.bind (fun grp ->
    staticArg statics "derive_pg_linear SIN" sInE |> Result.bind (fun svi ->
    staticArg statics "derive_pg_linear SOUT" sOutE |> Result.bind (fun svo ->
    (Blade.ML.Statics.pgSpecOfStatic "derive_pg_linear SIN" grp svi |> Result.mapError err5000)
    |> Result.bind (fun si ->
    (Blade.ML.Statics.pgSpecOfStatic "derive_pg_linear SOUT" grp svo |> Result.mapError err5000)
    |> Result.bind (fun so ->
        if Blade.ML.PointSpec.pgHomDim grp si so = 0 then
            Error (err4007 ($"derive_pg_linear: no {grp.Name}-equivariant linear map exists from the input spec to the output spec -- by Schur's lemma over R an equivariant linear map can only connect irreducible blocks carrying the SAME label, and these specs share none: every admissible map is zero"))
        else
            ensure st (fingerprint "pg_linear" (box (grp.Name, si, so)))
                (fun n -> derivePgLinearDecl n grp si so)
            |> Result.map (fun n -> inheritSpan site (ExprApp (v n, [ xE; wE ]))))))))

/// The sanity range of the Sn ops' static arguments, mirroring the sizing
/// builtins' guard (MLStatics) so a wild literal is a clean message rather
/// than an int overflow inside `permPartitions`. The REAL gates -- the K+L
/// cap and N >= K+L -- are `PermSpec.checkPermSizing`'s, shared verbatim
/// with `perm_weight_dim` / `perm_bias_dim`.
let private permRangeOk (k: int64) (l: int64) (n: int64) = k <= 64L && l <= 64L && n <= 1000000L

/// Shared elaboration for derive_perm_linear: three static ints, then the
/// shared precondition, then the complete Sn layer. K = 0 is refused BY NAME
/// (it is derive_perm_bias, whose weight buffer is sized by a different
/// builtin), and that check runs BEFORE the sizing gate so the diagnostic
/// names the op the user wants rather than an N that is beside the point.
let private elabDerivePermLinear (st: ElabState) (statics: StaticEnv) (site: Expr)
                                 (kE: Expr) (lE: Expr) (nE: Expr) (xE: Expr) (wE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics "derive_perm_linear K" kE |> Result.bind (fun kv ->
    staticArg statics "derive_perm_linear L" lE |> Result.bind (fun lv ->
    staticArg statics "derive_perm_linear N" nE |> Result.bind (fun nv ->
        match kv, lv, nv with
        | SVInt kk, SVInt ll, SVInt nn ->
            if kk < 1L then
                Error (err5000 $"derive_perm_linear: K must be a static int >= 1 (got {kk}) -- K = 0 has no input axes, so the map is a CONSTANT and its complete basis is the rep-introduction form ml.derive_perm_bias(L, N, b), whose buffer is sized by ml.perm_bias_dim(L, N) rather than ml.perm_weight_dim(0, L, N)")
            elif ll < 0L then
                Error (err5000 $"derive_perm_linear: L must be a static int >= 0 (got {ll}) -- L = 0 is the invariant readout, a one-cell Idx<1> result")
            elif not (permRangeOk kk ll nn) then
                Error (err5000 $"derive_perm_linear: K, L and N are static ints out of any sane range (got {kk}, {ll}, {nn})")
            else
                let k, l, n = int kk, int ll, int nn
                Blade.ML.PermSpec.checkPermSizing "derive_perm_linear" "K + L" (k + l) n
                |> Result.mapError err5000
                |> Result.bind (fun () ->
                    ensure st (fingerprint "perm_linear" (box (k, l, n))) (fun nm ->
                        derivePermLinearDecl nm k l n)
                    |> Result.map (fun nm -> inheritSpan site (ExprApp (v nm, [ xE; wE ]))))
        | _ -> Error (err5000 "derive_perm_linear: K, L and N must be static ints"))))

/// Shared elaboration for derive_perm_bias -- derive_perm_linear at K = 0,
/// and the same shared precondition with `L` spelling m.
let private elabDerivePermBias (st: ElabState) (statics: StaticEnv) (site: Expr)
                               (lE: Expr) (nE: Expr) (bE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics "derive_perm_bias L" lE |> Result.bind (fun lv ->
    staticArg statics "derive_perm_bias N" nE |> Result.bind (fun nv ->
        match lv, nv with
        | SVInt ll, SVInt nn ->
            if ll < 0L then
                Error (err5000 $"derive_perm_bias: L must be a static int >= 0 (got {ll})")
            elif not (permRangeOk 0L ll nn) then
                Error (err5000 $"derive_perm_bias: L and N are static ints out of any sane range (got {ll}, {nn})")
            else
                let l, n = int ll, int nn
                Blade.ML.PermSpec.checkPermSizing "derive_perm_bias" "L" l n
                |> Result.mapError err5000
                |> Result.bind (fun () ->
                    ensure st (fingerprint "perm_bias" (box (l, n))) (fun nm ->
                        derivePermBiasDecl nm l n)
                    |> Result.map (fun nm -> inheritSpan site (ExprApp (v nm, [ bE ]))))
        | _ -> Error (err5000 "derive_perm_bias: L and N must be static ints")))

/// Shared elaboration for perm_matmul. Its gate is its OWN -- a matrix
/// product has no K + L basis behind it, so `checkPermSizing`'s N >= K + L
/// rule does not apply (perm_matmul at N = 2 is a perfectly good 2x2
/// product). What it does share is the flat-buffer cell cap: `out` is
/// materialized as a literal N^2 zero array.
let private elabPermMatmul (st: ElabState) (statics: StaticEnv) (site: Expr)
                           (nE: Expr) (aE: Expr) (bE: Expr)
    : Result<Expr, ElabError> =
    staticArg statics "perm_matmul N" nE |> Result.bind (fun nv ->
        match nv with
        | SVInt nn ->
            if nn < 1L then
                Error (err5000 $"perm_matmul: N must be a static int >= 1 (got {nn}) -- it is the node-axis extent, and the buffers are the flat row-major N^2 matrices")
            elif not (permRangeOk 0L 0L nn) then
                Error (err5000 $"perm_matmul: N is a static int out of any sane range (got {nn})")
            else
                let n = int nn
                if permPow n 2 > int64 permCellCap then
                    Error (err5000 ($"perm_matmul: the flat node-power buffers of N = {n} are N^2 = {(n * n)} cells, over the {permCellCap}-cell limit -- the emitted kernel materializes the result as a literal zero array of that extent. Lower N (the node-axis extent)"))
                else
                    ensure st (fingerprint "perm_matmul" (box n)) (fun nm -> Ok (permMatmulDecl nm n))
                    |> Result.map (fun nm -> inheritSpan site (ExprApp (v nm, [ aE; bE ])))
        | _ -> Error (err5000 "perm_matmul: N must be a static int"))

/// Rewrite ML-op calls in an expression. Same walker shape as
/// Grad.rewriteExpr; the two passes stay separate because this one carries
/// elaboration state and runs first.
let rec private rewriteExpr (st: ElabState) (statics: StaticEnv) (aliases: Set<string>) (opsEnabled: bool) (e: Expr)
    : Result<Expr, ElabError> =
    let r = rewriteExpr st statics aliases opsEnabled
    let rList es =
        es |> List.fold (fun acc x ->
            acc |> Result.bind (fun xs -> r x |> Result.map (fun x' -> xs @ [x'])))
            (Ok [])
    let rOpt (o: Expr option) =
        match o with
        | None -> Ok None
        | Some x -> r x |> Result.map Some
    match e.Kind with
    // Qualified ML sizing builtin: `alias.total_dim(...)` -> the mangled
    // internal registry name so the static evaluator folds it (and a bare
    // `total_dim(...)` no longer resolves anywhere). Normalized in every
    // pass -- sizing must resolve before op configs fold.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, name) }, args)
        when Set.contains alias aliases && Set.contains name sizingNames ->
        rList args |> Result.map (fun args' -> inheritSpan e (ExprApp (v (Blade.ML.Statics.statName name), args')))
    // Qualified ML op: `alias.y_to(...)` -> generated specialized function.
    // Bare `y_to(...)` is no longer recognized: the ML surface is reachable
    // only through an `import ml` alias.
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, op) }, args)
        when opsEnabled && Set.contains alias aliases && Set.contains op opNames ->
        rList args |> Result.bind (fun args' ->
            match op, args' with
            | "y_to", (lmaxE :: rest) when rest.Length = 3 ->
                staticArg statics "y_to lmax" lmaxE |> Result.bind (fun sv ->
                    match sv with
                    | SVInt lmax ->
                        ensure st (fingerprint "y_to" (box lmax)) (fun n -> yToDecl n (int lmax))
                        |> Result.map (fun n -> inheritSpan e (ExprApp (v n, rest)))
                    | _ -> Error (err5000 "y_to: lmax must be a static int"))
            | "y_to", _ -> Error (err5000 "y_to: expected y_to(LMAX, x, y, z)")
            | "tensor_product", [ cfgE; xE; yE; wE ] ->
                staticArg statics "tensor_product cfg" cfgE |> Result.bind (fun sv ->
                cfgOfStatic "tensor_product" sv |> Result.bind (fun cfg ->
                    if not (allValidOutputs cfg) then
                        let reachable = tpPaths cfg |> List.map (fun (_, _, bo) -> bo) |> Set.ofList
                        let missing =
                            cfg.SpecOut
                            |> List.mapi (fun i entry -> (i, entry))
                            |> List.filter (fun (i, _) -> not (Set.contains i reachable))
                        let names =
                            missing
                            |> List.map (fun (_, entry) ->
                                $"""(l={entry.L}, {(if entry.Parity = 0 then "even" else "odd")})""")
                            |> String.concat ", "
                        let plural = missing.Length > 1
                        Error (err4007 (sprintf "tensor_product: output irrep%s %s %s unreachable from the inputs -- no Clebsch-Gordan path satisfies the triangle inequality |l1-l2| <= l <= l1+l2 with parity p1*p2, so by Schur's lemma the only equivariant map into %s is zero"
                                            (if plural then "s" else "") names
                                            (if plural then "are" else "is")
                                            (if plural then "those blocks" else "that block")))
                    else
                        ensure st (fingerprint "tp" (box cfg)) (fun n -> Ok (tpDecl n cfg))
                        |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ xE; yE; wE ])))))
            | "tensor_product", _ -> Error (err5000 "tensor_product: expected tensor_product(CFG, x, y, w)")
            | "linear", [ sInE; sOutE; wE; xE ] ->
                elabLinear st statics "linear" e sInE sOutE 1 wE xE
            | "linear", _ -> Error (err5000 "linear: expected linear(SPEC_IN, SPEC_OUT, w, x)")
            | "linear_rows", [ sInE; sOutE; nE; wE; xE ] ->
                staticArg statics "linear_rows nrows" nE |> Result.bind (fun sv ->
                    match sv with
                    | SVInt n when n >= 1L ->
                        elabLinear st statics "linear_rows" e sInE sOutE (int n) wE xE
                    | _ -> Error (err5000 "linear_rows: NROWS must be a static int >= 1"))
            | "linear_rows", _ -> Error (err5000 "linear_rows: expected linear_rows(SPEC_IN, SPEC_OUT, NROWS, w, x)")
            | "gated", [ specE; xE ] ->
                elabGated st statics "gated" e specE 1 xE
            | "gated", _ -> Error (err5000 "gated: expected gated(SPEC, x)")
            | "gated_rows", [ specE; nE; xE ] ->
                staticArg statics "gated_rows nrows" nE |> Result.bind (fun sv ->
                    match sv with
                    | SVInt n when n >= 1L ->
                        elabGated st statics "gated_rows" e specE (int n) xE
                    | _ -> Error (err5000 "gated_rows: NROWS must be a static int >= 1"))
            | "gated_rows", _ -> Error (err5000 "gated_rows: expected gated_rows(SPEC, NROWS, x)")
            | "scalars", [ specE; xE ] ->
                staticArg statics "scalars spec" specE |> Result.bind (fun sv ->
                specOfStatic "scalars" sv |> Result.bind (fun spec ->
                    ensure st (fingerprint "scalars" (box spec)) (fun n -> scalarsDecl n spec)
                    |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ xE ])))))
            | "scalars", _ -> Error (err5000 "scalars: expected scalars(SPEC, x)")
            | "norms", [ specE; xE ] ->
                staticArg statics "norms spec" specE |> Result.bind (fun sv ->
                specOfStatic "norms" sv |> Result.bind (fun spec ->
                    ensure st (fingerprint "norms" (box spec)) (fun n -> Ok (normsDecl n spec))
                    |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ xE ])))))
            | "norms", _ -> Error (err5000 "norms: expected norms(SPEC, x)")
            | "derive_linear", [ sInE; sOutE; wE; xE ] ->
                elabDeriveLinear st statics sInE sOutE
                |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ wE; xE ])))
            | "derive_linear", [ sInE; sOutE ] ->
                // Binding form: the derived layer as a function VALUE --
                // `let layer = ml.derive_linear(SIN, SOUT)` then
                // `layer(w, x)` through the normal FuncElem path (wrong-spec
                // calls hit the IrrepsIdx strictness seam, BL4003).
                elabDeriveLinear st statics sInE sOutE
                |> Result.map (fun n -> inheritSpan e (ExprVar n))
            | "derive_linear", _ -> Error (err5000 "derive_linear: expected derive_linear(SPEC_IN, SPEC_OUT[, w, x])")
            | "derive_tp", [ s1E; s2E; xE; yE; wE ] ->
                elabDeriveTp st statics s1E s2E
                |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ xE; yE; wE ])))
            | "derive_tp", [ s1E; s2E ] ->
                elabDeriveTp st statics s1E s2E
                |> Result.map (fun n -> inheritSpan e (ExprVar n))
            | "derive_tp", _ -> Error (err5000 "derive_tp: expected derive_tp(SPEC1, SPEC2[, x, y, w])")
            // S2-compacted self-TP: same arithmetic as derive_tp(SPEC, SPEC,
            // x, y, .) with a smaller weight buffer (sym/alt_tp_weight_dim).
            | "derive_sym_tp", [ specE; xE; yE; wE ] ->
                elabDeriveS2Tp st statics S2Sym e specE xE yE wE
            | "derive_sym_tp", _ -> Error (err5000 "derive_sym_tp: expected derive_sym_tp(SPEC, x, y, w) with w of extent ml.sym_tp_weight_dim(SPEC)")
            | "derive_alt_tp", [ specE; xE; yE; wE ] ->
                elabDeriveS2Tp st statics S2Alt e specE xE yE wE
            | "derive_alt_tp", _ -> Error (err5000 "derive_alt_tp: expected derive_alt_tp(SPEC, x, y, w) with w of extent ml.alt_tp_weight_dim(SPEC)")
            // The degree-K equivariant polynomial layer: the uniform Sym^K
            // label basis, one weight per basis map. K = 1 is derive_linear;
            // K = 2 is derive_sym_tp's hom-space in the uniform convention
            // rather than in the kept-path one.
            | "derive_poly", [ specE; kE; sOutE; xE; wE ] ->
                elabDerivePoly st statics e specE kE sOutE xE wE
            | "derive_poly", _ -> Error (err5000 "derive_poly: expected derive_poly(SPEC, K, SOUT, x, w) with x of type Array<Float like IrrepsIdx<SPEC>> and w of extent ml.poly_weight_dim(SPEC, K, SOUT)")
            // The Sn index-action layer: the complete Hom_{Sn}(R^{N^K}, R^{N^L})
            // basis over FLAT ROW-MAJOR node-power buffers, one loop nest per
            // partition of the K+L axis positions.
            | "derive_perm_linear", [ kE; lE; nE; xE; wE ] ->
                elabDerivePermLinear st statics e kE lE nE xE wE
            | "derive_perm_linear", _ -> Error (err5000 "derive_perm_linear: expected derive_perm_linear(K, L, N, x, w) with K, L, N static ints, x of extent N^K (flat row-major over the K node axes) and w of extent ml.perm_weight_dim(K, L, N); the result has extent N^L (Idx<1> at L = 0, the invariant readout)")
            | "derive_perm_bias", [ lE; nE; bE ] ->
                elabDerivePermBias st statics e lE nE bE
            | "derive_perm_bias", _ -> Error (err5000 "derive_perm_bias: expected derive_perm_bias(L, N, b) with L, N static ints and b of extent ml.perm_bias_dim(L, N); the result has extent N^L")
            // The one bilinear shipped BY NAME: PPGN's flat N^2 matrix
            // product, S_n-equivariant because conjugation distributes over it.
            | "perm_matmul", [ nE; aE; bE ] ->
                elabPermMatmul st statics e nE aE bE
            | "perm_matmul", _ -> Error (err5000 "perm_matmul: expected perm_matmul(N, a, b) with N a static int and a, b of extent N^2 (flat row-major N x N matrices); the result has extent N^2")
            // The point-group layer: the complete R-Schur basis of
            // Hom_G(V_in, V_out) over a FINITE group's labelled blocks, with
            // the Frobenius-Schur correction (e scalars per cell, [Id, J] at
            // complex type) that the O(3) member never needed.
            | "derive_pg_linear", [ gE; sInE; sOutE; xE; wE ] ->
                elabDerivePgLinear st statics e gE sInE sOutE xE wE
            | "derive_pg_linear", _ -> Error (err5000 "derive_pg_linear: expected derive_pg_linear(GROUP, SIN, SOUT, x, w) with GROUP a registered point group (C4, D4), SIN/SOUT static arrays of (LABEL_NAME, mult) tuples, x of type Array<Float like PgIrrepsIdx<GROUP, SIN>> and w of extent ml.pg_hom_dim(GROUP, SIN, SOUT)")
            // The monomial lift: the value-side half of the symmetric-power
            // bridge. Its type-side twin is ml.sym_spec(SPEC, K), and
            // ml.derive_linear(ml.sym_spec(SPEC, K), SPEC_OUT) composed with
            // it is degree-K equivariant synthesis.
            | "sym_lift", [ specE; kE; xE ] ->
                staticArg statics "sym_lift spec" specE |> Result.bind (fun sv ->
                specOfStatic "sym_lift spec" sv |> Result.bind (fun s ->
                staticArg statics "sym_lift K" kE |> Result.bind (fun kv ->
                    match kv with
                    | SVInt k when k >= 1L && k <= 4L ->
                        ensure st (fingerprint "sym_lift" (box (s, int k))) (fun n -> symLiftDecl n s (int k))
                        |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ xE ])))
                    | SVInt k ->
                        Error (err5000 $"sym_lift: K must be a static int in 1..4 (got {k}) -- the symmetric-power surface is capped at degree 4 (retired transforms-as-types plan section 6.5)")
                    | _ -> Error (err5000 "sym_lift: K must be a static int"))))
            | "sym_lift", _ -> Error (err5000 "sym_lift: expected sym_lift(SPEC, K, x) with x of type Array<Float like IrrepsIdx<SPEC>>; the result is a plain Idx<C(total_dim(SPEC)+K-1, K)> monomial vector")
            | "tensor_to_irreps", [ gE ] ->
                ensure st (fingerprint "tensor_to_irreps" (box ())) (fun n ->
                    Ok (bridgeDecl n Blade.ML.CartesianBridge.bridge9Flat 9 "g"
                            (tyFloatArr 9) (tyIrrepsArr Blade.ML.CartesianBridge.gradSpec)))
                |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ gE ])))
            | "tensor_to_irreps", _ -> Error (err5000 "tensor_to_irreps: expected tensor_to_irreps(g) with g the flat row-major 3x3 Cartesian tensor (Idx<9>)")
            | "sym_to_irreps", [ sE ] ->
                ensure st (fingerprint "sym_to_irreps" (box ())) (fun n ->
                    Ok (bridgeDecl n Blade.ML.CartesianBridge.symToIrrFlat 6 "s"
                            (tyFloatArr 6) (tyIrrepsArr Blade.ML.CartesianBridge.tauSpec)))
                |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ sE ])))
            | "sym_to_irreps", _ -> Error (err5000 "sym_to_irreps: expected sym_to_irreps(s) with s the packed symmetric tensor [s00, s01, s02, s11, s12, s22] (Idx<6>)")
            | "irreps_to_sym", [ tE ] ->
                ensure st (fingerprint "irreps_to_sym" (box ())) (fun n ->
                    Ok (bridgeDecl n Blade.ML.CartesianBridge.irrToSymFlat 6 "t"
                            (tyIrrepsArr Blade.ML.CartesianBridge.tauSpec) (tyFloatArr 6)))
                |> Result.map (fun n -> inheritSpan e (ExprApp (v n, [ tE ])))
            | "irreps_to_sym", _ -> Error (err5000 "irreps_to_sym: expected irreps_to_sym(t) with t transforming as IrrepsIdx<[(0,0,1), (2,0,1)]>")
            | _ -> Error (err5000 $"{op}: unrecognized ML-op call shape"))
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
        let rec rStmt (s: Stmt) : Result<Stmt, ElabError> =
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
    // Recursive array (`let rec q: T = match q with ...`): the seed and
    // inductive slices are ordinary expressions and may contain qualified
    // ops. Without this arm they fell through untouched, and since this pass
    // DELETES the import that would bind the alias, the call reached the
    // checker as an unbound variable.
    | ExprKind.ExprRecArray def ->
        rOpt (def.SeedArm |> Option.map snd) |> Result.bind (fun seedE ->
        rOpt def.Guard |> Result.bind (fun guardE ->
        r def.SliceExpr |> Result.map (fun slice' ->
            let seed' = Option.map2 (fun (sv, _) se -> (sv, se)) def.SeedArm seedE
            inheritSpan e (ExprRecArray { def with SeedArm = seed'; SliceExpr = slice'; Guard = guardE }))))
    // The rest of the expression algebra. Every constructor holding a
    // sub-expression is walked, and the catch-all wildcard is deliberately
    // GONE: an unhandled case is an FS0025 incomplete-match warning at build
    // time rather than a qualified call silently surviving unrewritten.
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
    // Leaves: no sub-expressions. Index/type arguments (range<I>, reverse<I>)
    // carry TypeExprs, not Exprs, and are never rewritten.
    | ExprKind.ExprWildcard | ExprKind.ExprQualified _ | ExprKind.ExprRange _
    | ExprKind.ExprReverse _ | ExprKind.ExprArity _ | ExprKind.ExprNth
    | ExprKind.ExprZero | ExprKind.ExprSection _ -> Ok e

/// `import ml [as _]` -- the module this layer owns.
let private isMlImport (d: Located<Decl>) =
    match d.Value with
    | DeclImport (["ml"], _) -> true
    | _ -> false

/// Aliases bound to `ml` in this decl list. Errors on a selective
/// `from ml import ...`, which would reintroduce the global names the module
/// system is meant to remove.
let private mlAliasesOf (decls: Located<Decl> list) : Result<Set<string>, ElabError> =
    decls |> List.fold (fun acc d ->
        acc |> Result.bind (fun set ->
            match d.Value with
            | DeclImport (["ml"], ImportQualified aliasOpt) ->
                Ok (Set.add (aliasOpt |> Option.defaultValue "ml") set)
            | DeclImport (["ml"], ImportSelective _) ->
                Error (err5000 "`ml` supports only `import ml [as <alias>]`; a selective `from ml import ...` would reintroduce global names")
            | _ -> Ok set))
        (Ok Set.empty)

/// Module-expansion failure: either a decl-span coded message (the ambient
/// synthSpan boundary, existing behavior) or pre-spanned diagnostics from
/// the equiv judgment (expression-precise).
type private ExpandFailure = Choice<ElabError, Blade.Diagnostics.Diagnostic list>

let private expandModule (decls: Located<Decl> list) : Result<Located<Decl> list, ExpandFailure> =
    (mlAliasesOf decls |> Result.mapError Choice1Of2) |> Result.bind (fun aliases ->
    // Import-gated: with no `import ml`, this pass is a no-op -- bare op
    // names are left unbound (a normal type error) and never rewritten.
    if Set.isEmpty aliases then Ok decls
    else
        let declsNoImport = decls |> List.filter (not << isMlImport)
        // Normalize `<alias>.equiv` where-conjuncts to their registered
        // internal name, so the judgment and the checker's registry dispatch
        // see one spelling.
        let normalizeConjunct (cname: string) =
            match cname.Split('.') with
            | [| a; "equiv" |] when Set.contains a aliases -> "__ml_equiv"
            | [| a; "galilean" |] when Set.contains a aliases -> "__ml_galilean"
            | [| a; "perm_equiv" |] when Set.contains a aliases -> "__ml_perm_equiv"
            | _ -> cname
        let declsNoImport =
            declsNoImport |> List.map (fun d ->
                match d.Value with
                | DeclFunction fd ->
                    let w' =
                        fd.WhereClause
                        |> Option.map (fun w ->
                            { w with Custom = w.Custom |> List.map (fun (n, args) -> (normalizeConjunct n, args)) })
                    { d with Value = DeclFunction { fd with WhereClause = w' } }
                | _ -> d)
        let st = { Counter = 0; Made = Map.empty; Decls = []; SigmoidName = None }
        let emptyStatics : StaticEnv =
            { Values = Map.empty; Globals = Map.empty; Functions = Map.empty
              CalledFunctions = ref Set.empty; ProviderRoots = Map.empty
              Structs = Map.empty; Segments = Map.empty }
        // Run rewriteExpr over every expression-bearing decl.
        let mapDecls (statics: StaticEnv) (opsEnabled: bool) (ds: Located<Decl> list) =
            ds |> List.fold (fun acc d ->
                acc |> Result.bind (fun out ->
                    // Stamp the user decl's span so every syn-built node
                    // attributes to this declaration's source line.
                    Blade.Ast.synthSpan <- d.Span
                    let mapped =
                        match d.Value with
                        | DeclFunction fd ->
                            rewriteExpr st statics aliases opsEnabled fd.Body
                            |> Result.map (fun b -> DeclFunction { fd with Body = b })
                        | DeclLet binding ->
                            rewriteExpr st statics aliases opsEnabled binding.Value
                            |> Result.map (fun v' -> DeclLet { binding with Value = v' })
                        | DeclStatic binding ->
                            rewriteExpr st statics aliases opsEnabled binding.Value
                            |> Result.map (fun v' -> DeclStatic { binding with Value = v' })
                        | other -> Ok other
                    mapped |> Result.map (fun value -> out @ [{ d with Value = value }])))
                (Ok [])
        // Pass 1: normalize qualified sizing builtins (`ml.total_dim(...)`) to
        // their internal names so the static evaluator can fold them. Ops are
        // left untouched (opsEnabled = false); statics are unused here.
        (mapDecls emptyStatics false declsNoImport |> Result.mapError Choice1Of2) |> Result.bind (fun decls1 ->
        // Fold failures are the type-checker's to report; elaboration only
        // needs the successfully folded environment.
        match Blade.StaticEval.resolveStatics decls1 with
        | Error e -> Error (Choice1Of2 (err5000 $"ML elaboration: static resolution failed: {e}"))
        | Ok (statics, _) ->
            // The equiv judgment runs HERE, at the pass-1/pass-2 seam: `ml.*`
            // op calls are still surface-visible, and specs resolve through
            // the identical static machinery pass 2 uses, so judgment and
            // synthesis cannot disagree about a spec.
            let judged =
                match Blade.ML.Equiv.buildCertTable statics decls1 with
                | Error d -> Error [ d ]
                | Ok certs ->
                    let diags =
                        if Map.isEmpty certs then []
                        else
                            decls1
                            |> List.collect (fun d ->
                                match d.Value with
                                | DeclFunction fd ->
                                    match Map.tryFind fd.Name certs with
                                    | Some cert ->
                                        let globalShapes = Blade.ML.Equiv.buildGlobalShapes cert.Group statics decls1
                                        Blade.ML.Equiv.judgeFunction cert.Group certs statics globalShapes aliases fd
                                    | None -> []
                                | _ -> [])
                    if not diags.IsEmpty then Error diags
                    else
                        // The CERTIFICATE-INFERENCE channel, run at the same
                        // seam and off the same tables. It only ever ADDS
                        // warnings (BL4011): the certified functions have
                        // just been checked and none of them reached here, so
                        // an uncertified neighbour that happens to judge
                        // equivariant costs nothing but a suggestion -- which
                        // is why it runs even when `certs` is empty.
                        for (msg, span) in Blade.ML.Equiv.inferCertificates statics aliases certs decls1 do
                            Blade.ML.Equiv.CertSuggestions.add msg span
                        Ok ()
            match judged with
            | Error ds -> Error (Choice2Of2 ds)
            | Ok () ->
            // The galilean judgment runs at the SAME seam (surface `sgs.*`
            // calls are still visible -- sgs elaborates after ml). It is
            // independent of the equiv judgment: a function may carry both
            // conjuncts, each judged in its own domain.
            let judgedGal =
                match Blade.ML.Galilean.buildCertTable decls1 with
                | Error d -> Error [ d ]
                | Ok gcerts ->
                    // `sgsAliases` is computed OUTSIDE the empty-table
                    // short-circuit: the inference channel below needs the
                    // sgs axioms precisely on files where no function yet
                    // carries a conjunct -- exactly the empty-table case.
                    let sgsAliases = Blade.ML.Galilean.sgsAliasesOf decls1
                    let diags =
                        if Map.isEmpty gcerts then []
                        else
                            decls1
                            |> List.collect (fun d ->
                                match d.Value with
                                | DeclFunction fd ->
                                    Blade.ML.Galilean.judgeFunction gcerts aliases sgsAliases fd
                                | _ -> [])
                    if not diags.IsEmpty then Error diags
                    else
                        // The GALILEAN twin of the certificate-inference
                        // channel above, adding BL4014 warnings only, for the
                        // same reason (including running when `gcerts` is
                        // empty). A file whose DECLARED certificate fails
                        // never gets here -- deliberate: a module the checker
                        // is already rejecting gets one story, not two.
                        for (msg, span) in
                            Blade.ML.Galilean.inferGalileanCertificates aliases sgsAliases gcerts decls1 do
                            Blade.ML.Galilean.GalCertSuggestions.add msg span
                        Ok ()
            match judgedGal with
            | Error ds -> Error (Choice2Of2 ds)
            | Ok () ->
            // The S_n index-action judgment: the THIRD member, at the same
            // seam and for the same reason. The three lattices do NOT
            // interact: a function may carry perm_equiv + galilean, or
            // perm_equiv + equiv, each judged in its own domain over its own
            // status set -- node relabelling, frame velocity and the Wigner
            // action are orthogonal hypotheses about the same arguments.
            let judgedPerm =
                match Blade.ML.Perm.buildCertTable statics decls1 with
                | Error d -> Error [ d ]
                | Ok pcerts when Map.isEmpty pcerts -> Ok ()
                | Ok pcerts ->
                    let diags =
                        decls1
                        |> List.collect (fun d ->
                            match d.Value with
                            | DeclFunction fd ->
                                Blade.ML.Perm.judgeFunction pcerts statics aliases decls1 fd
                            | _ -> [])
                    if diags.IsEmpty then Ok () else Error diags
            match judgedPerm with
            | Error ds -> Error (Choice2Of2 ds)
            | Ok () ->
            // Pass 2: rewrite qualified ops into generated specialized functions.
            (mapDecls statics true decls1 |> Result.mapError Choice1Of2) |> Result.map (fun decls2 ->
                if st.Decls.IsEmpty then decls2
                else
                    // Generated functions are self-contained (literal tables,
                    // no captures): splice them at the FRONT so every use site
                    // (top-level lets included) sees them defined.
                    let span = { StartLine = 0; StartCol = 0; EndLine = 0; EndCol = 0; File = None }
                    let gen = st.Decls |> List.map (fun fd -> { Value = DeclFunction fd; Span = span })
                    gen @ decls2)))

/// Entry point: elaborate ML ops across a program (before Grad expansion).
/// Also installs the ML sizing builtins into the static evaluator -- expand
/// runs unconditionally as the first pipeline stage, so this makes sh_spec /
/// total_dim / tp_weight_dim / linear_weight_dim visible to every
/// resolveStatics pass (the elaborator's own, checkModule's, and Lowering's
/// Phase 0) without the core evaluator knowing about ML.
let private expandStr (program: Program) : Result<Program, ExpandFailure> =
    Blade.ML.Statics.install ()
    // The suggestion side-channel accumulates across the program's modules,
    // so it is cleared once here -- the elaborator is its only producer.
    // The structured twin and the galilean channel follow the same lifecycle.
    Blade.ML.Equiv.CertSuggestions.reset ()
    Blade.ML.Equiv.CertFacts.reset ()
    Blade.ML.Galilean.GalCertSuggestions.reset ()
    Blade.ML.Equiv.register ()
    Blade.ML.Galilean.register ()
    Blade.ML.Perm.register ()
    program.Modules
    |> List.fold (fun acc m ->
        acc |> Result.bind (fun ms ->
            expandModule m.Decls |> Result.map (fun ds -> ms @ [{ m with Decls = ds }])))
        (Ok [])
    |> Result.map (fun ms -> { program with Modules = ms })

/// Boundary: coded internals -> diagnostics. For ElabError failures the
/// span is the ambient synthSpan -- stamped per-decl by mapDecls, so a
/// mid-elaboration failure points at the offending declaration; the Code
/// (BL5000 generic / BL4007 Schur) is rendered faithfully. Equiv-judgment
/// failures carry their own expression-precise diagnostics (BL4008).
let expand (program: Program) : Result<Program, Blade.Diagnostics.Diagnostic list> =
    Blade.Ast.synthSpan <- Blade.Ast.noSpan
    expandStr program
    |> Result.mapError (fun failure ->
        match failure with
        | Choice1Of2 err ->
            [ Blade.Diagnostics.mkError err.Code (Blade.Diagnostics.Codes.phaseOfCode err.Code) Blade.Ast.synthSpan err.Msg ]
        | Choice2Of2 ds -> ds)
