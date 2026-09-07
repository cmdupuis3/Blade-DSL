/// The constrained-record INDEX FENCE: the semantic half of the
/// constrained-index-types feature. This module answers three questions
/// and nothing else: (1) is this struct STATIC -- are all its fields
/// StaticValue-representable; (2) is this struct INDEX-ELIGIBLE --
/// additionally, is every field an Int with statically-foldable bounds,
/// so its solutions can be found by enumerating a rectangular box; (3) at
/// one cell of that box, do the struct's conjuncts hold.
///
/// It does NOT enumerate, order, count, or cap. That is the counting
/// layer's job (StructIdxSpec), deliberately: the certificate discipline
/// wants two independent enumeration routes over ONE shared cell
/// predicate, and a shared enumerator here would make them agree for the
/// wrong reason.
///
/// THE TWO READINGS, the semantic law of this feature: one conjunct list,
/// two readings. At CONSTRUCTION a false conjunct is an ERROR
/// (assert-not-solve, unchanged in both worlds -- the runtime guard in
/// TypeCheck.synthesizeStructChecks and the fold-time check in
/// StaticEval's ExprStruct arm); at ENUMERATION a false conjunct is
/// EXCLUSION (membership). `evalConjunctsAtCell` below implements the
/// SECOND reading only, returning `Ok false` rather than an error for a
/// violated conjunct. Construction must never be routed through it.
module Blade.StructIdxFence

open Blade.Ast
open Blade.StaticEval

// The box

/// One field's box range. INCLUSIVE ON BOTH ENDS -- the fence normalizes
/// `in lo .. hi` (half-open) and `<min=a, max=b>` (inclusive) alike.
type FieldBox = {
    Field: string
    Lo: int64
    Hi: int64
}

/// A struct that passed the index fence: the box, plus the conjuncts to
/// filter it by.
type StructBoxSpec = {
    Name: string
    /// DECLARATION order = lex nesting order, first field outermost.
    Fields: FieldBox list
    /// The DECLARED where-conjuncts only: desugared field-bound conjuncts
    /// are absent since the box already enforces them exactly. Numbering
    /// agrees with the construction reading's (both count 1-based over
    /// `Ast.structConjuncts`' order, declared conjuncts FIRST).
    Conjuncts: Expr list
}

/// Number of values a field's box admits. Zero when the range is inverted:
/// an EMPTY box is a legitimate (warned-about) outcome, not an error.
let extent (b: FieldBox) : int64 =
    if b.Hi < b.Lo then 0L else b.Hi - b.Lo + 1L

// Type classification

/// Short human-readable label for a field's declared type, for
/// diagnostics. Deliberately lossy: names a REJECTION REASON, not source.
let rec typeExprLabel (ty: TypeExpr) : string =
    match ty with
    | TyInt32 -> "Int32"
    | TyInt64 -> "Int64"
    | TyFloat32 -> "Float32"
    | TyFloat64 -> "Float64"
    | TyComplex64 -> "Complex64"
    | TyComplex128 -> "Complex128"
    | TyBool -> "Bool"
    | TyString -> "String"
    | TyChar -> "Char"
    | TyUnit -> "Unit"
    | TyNamed (n, []) -> n
    | TyNamed (n, _) -> n + "<...>"
    | TyBounded (b, _, _) -> typeExprLabel b
    | TyConstrained (inner, _) -> typeExprLabel inner
    | TyArray _ -> "an array type"
    | TyAbstractArray _ -> "an abstract array type"
    | TyFunc _ -> "a function type"
    | TyTuple ts -> $"a {List.length ts}-tuple type"
    | TyVar (v, _) -> $"type variable {v}"
    | _ -> "a non-enumerable type"

/// Is this field type an Int? Unit/tag arguments are transparent -- a
/// tagged `Int<angular_momentum>` enumerates like a bare `Int`. `Nat` is
/// deliberately EXCLUDED: motivating boxes are shifted and carry negative
/// values (m in [-l, l]), and a non-negative type whose box straddles
/// zero would be a contradiction the fence cannot see.
let rec private isIntFieldType (ty: TypeExpr) : bool =
    match ty with
    | TyInt32 | TyInt64 -> true
    // `Nat` is an integer field whose box must not go negative (the fence
    // checks the folded lower bound); the enumeration reading is the same.
    | TyNamed (("Int" | "Int32" | "Int64" | "Nat"), _) -> true
    | TyBounded (b, _, _) -> isIntFieldType b
    | TyConstrained (inner, _) -> isIntFieldType inner
    | _ -> false

// THE WEAK FENCE (`static struct` eligibility) is implemented once, at its
// only call site, in TypeCheck's TyDeclStruct registration arm; the INDEX
// fence below consumes that decision through `StructStaticInfo.IsStatic`.

// The strong (index) fence

let private seqResult (rs: Result<'a, string> list) : Result<'a list, string> =
    rs |> List.fold (fun acc r ->
        acc |> Result.bind (fun xs -> r |> Result.map (fun x -> xs @ [x]))) (Ok [])

/// Fold a bound expression to an int64 in the static environment. Bounds
/// may name statics and call static functions (TypeCheck's
/// `StructBoundScope`); a bound naming an EARLIER FIELD fails here with
/// "undefined variable" -- same-record dependent bounds are out of the
/// BOX grammar.
let private foldBound (env: StaticEnv) (sname: string) (fname: string) (side: string) (e: Expr) : Result<int64, string> =
    match evalExpr env maxSteps e with
    | Ok (SVInt n) -> Ok n
    | Ok v -> Error $"struct {sname}, field '{fname}': {side} bound is not static -- it folded to {ppStaticValue v}, not an integer"
    | Error why -> Error $"struct {sname}, field '{fname}': {side} bound is not static -- {why}"

/// One field's normalized inclusive box.
let private fieldBox (env: StaticEnv) (sname: string) (f: FieldDecl) : Result<FieldBox, string> =
    if not (isIntFieldType f.Type) then
        Error ($"struct {sname}, field '{f.Name}': non-enumerable field type {(typeExprLabel f.Type)} -- an index-eligible struct needs every field Int with static bounds")
    else
        // `Ast.fieldBoxBounds` hands back a HALF-OPEN pair whichever
        // spelling was written (`max=b` becomes `b + 1`); subtracting one
        // from the folded exclusive endpoint is the inclusive normalization.
        let isNat = (match f.Type with TyNamed ("Nat", _) -> true | _ -> false)
        match fieldBoxBounds f with
        | Some loE, Some hiExclE ->
            foldBound env sname f.Name "min" loE |> Result.bind (fun lo ->
            foldBound env sname f.Name "max" hiExclE |> Result.bind (fun hiExcl ->
                if isNat && lo < 0L then
                    Error $"struct {sname}, field '{f.Name}': a Nat field's box cannot start below 0 (min folded to {lo})"
                else Ok { Field = f.Name; Lo = lo; Hi = hiExcl - 1L }))
        | _ ->
            Error ($"struct {sname}, field '{f.Name}': unbounded field -- an index-eligible struct needs a static min and max")

/// STRONG FENCE, over an explicit declaration. `isStatic` is the DECLARED
/// `static struct` marker: index-eligibility is an OPT-IN, not a property
/// a struct can acquire by accident.
let structStaticFenceOf
        (env: StaticEnv)
        (name: string)
        (isStatic: bool)
        (fields: FieldDecl list)
        (declared: Expr list)
        : Result<StructBoxSpec, string> =
    if not isStatic then
        Error $"struct {name} is not declared static -- write `static struct {name}` to make it index-eligible"
    elif List.isEmpty fields then
        Error $"struct {name} has no fields -- an index-eligible struct needs at least one"
    else
        fields
        |> List.map (fieldBox env name)
        |> seqResult
        |> Result.map (fun boxes -> { Name = name; Fields = boxes; Conjuncts = declared })

/// STRONG FENCE, by name, resolved against the static evaluator's struct
/// registry (declaration order is irrelevant).
let structStaticFence (env: StaticEnv) (name: string) : Result<StructBoxSpec, string> =
    match Map.tryFind name env.Structs with
    | None -> Error $"'{name}' is not a declared struct"
    | Some info -> structStaticFenceOf env name info.IsStatic info.FieldDecls info.Declared

// The enumeration reading

/// Fold the struct's conjuncts at ONE cell of the box. `Ok true`: the cell
/// satisfies every conjunct, a MEMBER of the solution set. `Ok false`:
/// some conjunct is false, EXCLUDED -- the enumeration reading, NOT an
/// error (construction-false is an error and lives elsewhere). `Error _`:
/// a conjunct did not fold to a boolean within its budget; the reason is
/// raw, since the caller owns the witness-cell suffix.
///
/// The budget is `StaticEval.cellBudget`, spent afresh PER CELL and far
/// smaller than the `let static` folding budget, keeping the worst case
/// at the box cap from being 100,000 steps x 100,000 cells.
/// `StructIdxSpec.routeFlat` stops at the FIRST erroring cell, so a
/// conjunct that cannot fold is paid once, not once per cell.
let evalConjunctsAtCell
        (env: StaticEnv)
        (spec: StructBoxSpec)
        (cell: (string * int64) list)
        : Result<bool, string> =
    // A cell must bind every field: a partial cell would silently read a
    // same-named static from the ambient environment instead of failing.
    let cellNames = cell |> List.map fst
    let fieldNames = spec.Fields |> List.map (_.Field)
    if List.length cellNames <> List.length fieldNames
       || not (fieldNames |> List.forall (fun f -> List.contains f cellNames)) then
        Error ($"""struct {spec.Name}: cell binds {{{(String.concat ", " cellNames)}}} but the box has fields {{{(String.concat ", " fieldNames)}}}""")
    else
        let cellEnv =
            { env with Values = cell |> List.fold (fun m (n, v) -> Map.add n (SVInt v) m) env.Values }
        let rec go i (cs: Expr list) =
            match cs with
            | [] -> Ok true
            | c :: rest ->
                // Skipped in BOTH readings via the one shared predicate.
                if isPplLicenseConjunct c then go (i + 1) rest
                else
                    match evalExprWith cellEnv cellBudget c with
                    | Ok (SVBool true) -> go (i + 1) rest
                    | Ok (SVBool false) -> Ok false
                    | Ok _ -> Error $"conjunct {i} of {spec.Name} is not a boolean at compile time"
                    | Error why -> Error $"conjunct {i} of {spec.Name} did not fold: {why}"
        go 1 spec.Conjuncts

// ---------------------------------------------------------------------------
// The enumeration recogniser (docs/plans/structural/06, 3.1): from the
// struct's DECLARED conjuncts to a DomainPlan, or the reason it is not
// closed-form (class B).
//
// Each conjunct normalizes to `Σ a_i x_i + c ⋈ 0` over the fields; `<`
// tightens to `<= -1`, `>=`/`>` swap sides, `==` becomes two inequalities,
// `abs(e) <= c` becomes `e <= c, -e <= c`, a `static` name or a
// field-free subexpression folds to a constant. A conjunct bounds its
// HIGHEST-INDEXED field, whose coefficient must be +-1. Difference
// constraints (at most two fields, unit coefficients) are projected
// (Fourier-Motzkin, innermost first) so every level's interval is exactly the
// set with a completion; a bound with several earlier fields is attached
// unprojected (an empty interval at a dead prefix costs one empty loop).
// ---------------------------------------------------------------------------

open Blade.Types

type private LinForm = { Vars: Map<int, int64>; K: int64 }

let private linAdd (a: LinForm) (b: LinForm) =
    let coefs = b.Vars |> Map.fold (fun m k v -> Map.add k ((Map.tryFind k m |> Option.defaultValue 0L) + v) m) a.Vars
    { Vars = coefs |> Map.filter (fun _ v -> v <> 0L); K = a.K + b.K }
let private linScale (s: int64) (a: LinForm) =
    { Vars = a.Vars |> Map.map (fun _ v -> v * s) |> Map.filter (fun _ v -> v <> 0L); K = a.K * s }
let private linNeg = linScale -1L

/// `e` as a linear form over the fields (by index), constants folded in the
/// static environment. Error names the first non-linear shape met.
let rec private linOf (env: StaticEnv) (fieldIx: Map<string, int>) (e: Expr) : Result<LinForm, string> =
    let constOf (x: Expr) =
        match evalExpr env maxSteps x with
        | Ok (SVInt n) -> Ok { Vars = Map.empty; K = n }
        | Ok v -> Error $"`{ppStaticValue v}` is not an integer"
        | Error why -> Error why
    match e.Kind with
    | ExprVar f when Map.containsKey f fieldIx -> Ok { Vars = Map.ofList [ (fieldIx.[f], 1L) ]; K = 0L }
    | ExprLit (LitInt n) -> Ok { Vars = Map.empty; K = n }
    | ExprVar _ -> constOf e
    | ExprUnaryOp (OpNeg, inner) -> linOf env fieldIx inner |> Result.map linNeg
    | ExprBinOp (_, OpAdd, a, b) ->
        linOf env fieldIx a |> Result.bind (fun la -> linOf env fieldIx b |> Result.map (linAdd la))
    | ExprBinOp (_, OpSub, a, b) ->
        linOf env fieldIx a |> Result.bind (fun la -> linOf env fieldIx b |> Result.map (fun lb -> linAdd la (linNeg lb)))
    | ExprBinOp (_, OpMul, a, b) ->
        (match linOf env fieldIx a, linOf env fieldIx b with
         | Ok la, Ok lb when la.Vars.IsEmpty -> Ok (linScale la.K lb)
         | Ok la, Ok lb when lb.Vars.IsEmpty -> Ok (linScale lb.K la)
         | Ok _, Ok _ -> Error "a product of two field terms is not linear"
         | Error w, _ | _, Error w -> Error w)
    | ExprTyped (inner, _) -> linOf env fieldIx inner
    | _ ->
        let names = fieldIx |> Map.toList |> List.map fst |> String.concat ", "
        constOf e |> Result.mapError (fun why -> $"not a linear expression in the fields {names} ({why}; node {e.Kind.GetType().Name})")

/// `Σ a_i x_i + c <= 0` constraints from one conjunct.
let rec private constraintsOf (env: StaticEnv) (fieldIx: Map<string, int>) (c: Expr) : Result<LinForm list, string> =
    let le (l: Expr) (r: Expr) =   // l <= r  ->  l - r <= 0
        linOf env fieldIx l |> Result.bind (fun ll -> linOf env fieldIx r |> Result.map (fun lr -> [ linAdd ll (linNeg lr) ]))
    match c.Kind with
    | ExprBinOp (_, OpLe, { Kind = ExprApp ({ Kind = ExprVar "abs" }, [ inner ]) }, r) ->
        // abs(e) <= r  ->  e <= r, -e <= r
        le inner r |> Result.bind (fun a -> le (inheritSpan inner (ExprUnaryOp (OpNeg, inner))) r |> Result.map (fun b -> a @ b))
    | ExprBinOp (_, OpGe, r, { Kind = ExprApp ({ Kind = ExprVar "abs" }, [ inner ]) }) ->
        le inner r |> Result.bind (fun a -> le (inheritSpan inner (ExprUnaryOp (OpNeg, inner))) r |> Result.map (fun b -> a @ b))
    | ExprBinOp (_, OpLe, l, r) -> le l r
    | ExprBinOp (_, OpGe, l, r) -> le r l
    | ExprBinOp (_, OpLt, l, r) -> le l r |> Result.map (List.map (fun f -> { f with K = f.K + 1L }))
    | ExprBinOp (_, OpGt, l, r) -> le r l |> Result.map (List.map (fun f -> { f with K = f.K + 1L }))
    | ExprBinOp (_, OpEq, l, r) -> le l r |> Result.bind (fun a -> le r l |> Result.map (fun b -> a @ b))
    | ExprBinOp (_, OpAnd, a, b) ->
        constraintsOf env fieldIx a |> Result.bind (fun ca -> constraintsOf env fieldIx b |> Result.map (fun cb -> ca @ cb))
    | ExprApp ({ Kind = ExprVar "abs" }, [ inner ]) ->
        Error "a bare `abs(...)` is not a boolean conjunct"
    | ExprBinOp (_, (OpNeq | OpOr), _, _) -> Error "`!=` and `||` are not closed-form (the solution set is not an interval per prefix)"
    | ExprBinOp (_, OpMod, _, _) -> Error "a congruence is not closed-form in v1"
    | _ -> Error "not a linear inequality (`<=`, `<`, `>=`, `>`, `==`, `abs(..) <= ..`) on the fields"

/// The recogniser. `spec.Conjuncts` are the DECLARED conjuncts; the box is
/// `spec.Fields`.
let domainPlanOf (env: StaticEnv) (spec: StructBoxSpec) : Result<DomainPlan, string> =
    let fields = Array.ofList spec.Fields
    let r = fields.Length
    let fieldIx = fields |> Array.mapi (fun i f -> f.Field, i) |> Map.ofArray
    let numbered = spec.Conjuncts |> List.mapi (fun i c -> (i + 1, c))
    let emptyPlan () =
        { Name = spec.Name
          Levels = [ for k in 0 .. r - 1 -> { Field = fields.[k].Field; Lo = [ { Const = 1L; Coefs = [] } ]; Hi = [ { Const = 0L; Coefs = [] } ] } ]
          Card = 0L }
    numbered
    |> List.fold (fun acc (n, c) ->
        acc |> Result.bind (fun cs ->
            match constraintsOf env fieldIx c with
            | Ok more -> Ok (cs @ more)
            | Error why -> Error $"conjunct {n} of {spec.Name}: {why}")) (Ok [])
    |> Result.bind (fun forms ->
        // Difference constraints `x_a - x_b <= c` (a or b = -1 for none),
        // keyed by (a, b) with the tightest c: projected below. Every other
        // constraint bounds its highest field by an affine form over the
        // earlier ones and is attached to that level as it stands.
        let diff = System.Collections.Generic.Dictionary<int * int, int64>()
        let tighten (a: int) (b: int) (c: int64) =
            match diff.TryGetValue((a, b)) with
            | true, c0 when c0 <= c -> ()
            | _ -> diff.[(a, b)] <- c
        let generalLo = Array.init r (fun _ -> ResizeArray<DomainAffine>())
        let generalHi = Array.init r (fun _ -> ResizeArray<DomainAffine>())
        for k in 0 .. r - 1 do
            tighten k -1 fields.[k].Hi       // x_k <= Hi
            tighten -1 k (-fields.[k].Lo)    // -x_k <= -Lo
        let mutable bad : string option = None
        let mutable infeasible = false
        for f in forms do
            if bad.IsNone then
                match f.Vars |> Map.toList with
                | [] -> if f.K > 0L then infeasible <- true
                | [ (k, a) ] when abs a = 1L ->
                    if a = 1L then tighten k -1 (-f.K) else tighten -1 k f.K
                | [ (i, ai); (j, aj) ] when abs ai = 1L && abs aj = 1L && ai = -aj ->
                    if ai = 1L then tighten i j (-f.K) else tighten j i (-f.K)
                | coefs ->
                    let k = coefs |> List.map fst |> List.max
                    let ak = coefs |> List.find (fun (v, _) -> v = k) |> snd
                    if abs ak <> 1L then
                        bad <- Some $"in {spec.Name}, a constraint gives field '{fields.[k].Field}' the coefficient {ak}; a level's bound must have coefficient +-1 on its own field"
                    else
                        // ak*x_k + Σ_j a_j x_j + c <= 0:
                        //   ak = +1 -> x_k <= -c - Σ a_j x_j   (an upper form)
                        //   ak = -1 -> x_k >= c + Σ a_j x_j    (a lower form)
                        let others = coefs |> List.filter (fun (v, _) -> v <> k)
                        if ak = 1L then generalHi.[k].Add { Const = -f.K; Coefs = others |> List.map (fun (j, a) -> (j, -a)) }
                        else generalLo.[k].Add { Const = f.K; Coefs = others }
        match bad with
        | Some why -> Error why
        | None ->
        if infeasible then Ok (emptyPlan ())
        else
        // Fourier-Motzkin over the difference constraints, innermost first:
        // level v's ends are the constraints on x_v over smaller fields;
        // eliminating v combines each lower/upper pair into a constraint
        // among the smaller fields (so no prefix is dead for these).
        let levels = Array.zeroCreate<DomainLevel> r
        let mutable empty = false
        for v in r - 1 .. -1 .. 0 do
            let uppers = diff |> Seq.filter (fun kv -> fst kv.Key = v && snd kv.Key < v) |> Seq.map (fun kv -> snd kv.Key, kv.Value) |> List.ofSeq   // x_v <= x_b + c
            let lowers = diff |> Seq.filter (fun kv -> snd kv.Key = v && fst kv.Key < v) |> Seq.map (fun kv -> fst kv.Key, kv.Value) |> List.ofSeq   // x_a - x_v <= c  ->  x_v >= x_a - c
            let hiForms =
                (uppers |> List.map (fun (b, c) -> if b < 0 then { Const = c; Coefs = [] } else { Const = c; Coefs = [ (b, 1L) ] }))
                @ List.ofSeq generalHi.[v]
            let loForms =
                (lowers |> List.map (fun (a, c) -> if a < 0 then { Const = -c; Coefs = [] } else { Const = -c; Coefs = [ (a, 1L) ] }))
                @ List.ofSeq generalLo.[v]
            levels.[v] <- { Field = fields.[v].Field; Lo = loForms; Hi = hiForms }
            for (a, c') in lowers do
                for (b, c) in uppers do
                    if a = b then (if c + c' < 0L then empty <- true)
                    else tighten a b (c + c')
        if empty then Ok (emptyPlan ())
        else
            let levelList = List.ofArray levels
            Ok { Name = spec.Name; Levels = levelList; Card = domainCardOf levelList })
