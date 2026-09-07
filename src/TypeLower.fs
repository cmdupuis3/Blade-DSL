// Surface-type lowering: the static-eval bridge (evalConstExpr and
// friends), extent lowering, the builtin scalar-name registry, the
// lowerTypeExpr rec-chain (surface TypeExpr -> IRType, index types
// included), and the index-shape/co-iteration helpers.
module Blade.TypeLower

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

let rec evalConstExpr (env: TypeEnv) (expr: Expr) : int64 option =
    match expr.Kind with
    | ExprKind.ExprLit (LitInt n) -> Some n
    | ExprKind.ExprLit (LitFloat f) -> Some (int64 f)
    | ExprKind.ExprVar name ->
        match lookupVar name env with
        | Some info ->
            match info.Type with
            | IRTNat (Some n) -> Some (int64 n)
            | _ -> None
        | None -> None
    | ExprKind.ExprBinOp (_, OpAdd, l, r) ->
        match evalConstExpr env l, evalConstExpr env r with
        | Some a, Some b -> Some (a + b) | _ -> None
    | ExprKind.ExprBinOp (_, OpSub, l, r) ->
        match evalConstExpr env l, evalConstExpr env r with
        | Some a, Some b -> Some (a - b) | _ -> None
    | ExprKind.ExprBinOp (_, OpMul, l, r) ->
        match evalConstExpr env l, evalConstExpr env r with
        | Some a, Some b -> Some (a * b) | _ -> None
    | ExprKind.ExprBinOp (_, OpDiv, l, r) ->
        match evalConstExpr env l, evalConstExpr env r with
        | Some a, Some b when b <> 0L -> Some (a / b) | _ -> None
    | _ -> None

/// Single-entry memo for staticEnvOf's `StaticFunctions -> StaticFuncDef`
/// projection, which was rebuilt on every call. Keyed on the REFERENCE identity
/// of the source map, so it is correct however calls interleave: an F# Map is
/// immutable, and any change to `env.StaticFunctions` yields a different object,
/// which misses the cache. The per-call `CalledFunctions` ref is deliberately
/// NOT cached -- it accumulates per evaluation and must stay fresh.
let mutable internal staticFuncProjCache
    : (obj * Map<string, StaticEval.StaticFuncDef>) option = None

/// Evaluate an expression to a compile-time int under the FULL static
/// contract (the replicate-count rule): a literal, a Nat-typed var, a
/// `let static` value, or a static-function call. Two tiers: the cheap
/// evalConstExpr first, then StaticEval against the StaticValues/
/// StaticFunctions maps populated by checkModule's pre-pass. Shared by the
/// Dist annotation order (lowerTypeExpr's TyDist arm) and the cumulant
/// projection order (inferCumulantProj).
let staticEnvOf (env: TypeEnv) : StaticEval.StaticEnv =
    let src = env.StaticFunctions
    let projected =
        match staticFuncProjCache with
        | Some (key, cached) when System.Object.ReferenceEquals (key, box src) -> cached
        | _ ->
            let p =
                src
                |> Map.map (fun _ (fd: FunctionDecl) ->
                    { StaticEval.Name = fd.Name
                      StaticEval.Params = fd.Params |> List.map (_.Name)
                      StaticEval.Body = fd.Body })
            staticFuncProjCache <- Some (box src, p)
            p
    { Values = env.StaticValues
      Globals = env.StaticValues
      Functions = projected
      CalledFunctions = ref Set.empty
      ProviderRoots = Map.empty
      Segments = Map.empty
      Structs = env.StructStatics }

let evalStaticIntExpr (env: TypeEnv) (expr: Expr) : int option =
    match evalConstExpr env expr with
    | Some n -> Some (int n)
    | None ->
        match StaticEval.evalExpr (staticEnvOf env) StaticEval.maxSteps expr with
        | Ok (StaticEval.SVInt v) -> Some (int v)
        | _ -> None

/// Evaluate an expression to its raw StaticValue under the same full static
/// contract as evalStaticIntExpr. For type arguments whose payload is
/// structured rather than an int (the IrrepsIdx spec: an array of triples,
/// which StaticEval folds to nested SVTuples).
let evalStaticValueExpr (env: TypeEnv) (expr: Expr) : Result<StaticEval.StaticValue, string> =
    StaticEval.evalExpr (staticEnvOf env) StaticEval.maxSteps expr

/// Dist provenance of a surface expression: the union of the provenance
/// sets of every variable reachable in it (conservative -- an
/// over-approximated source set can only make independence HARDER to
/// prove, never easier, so union is sound). Empty means "unknown", which
/// consumers treat as un-provable rather than vacuously independent.
/// Sources of ground truth: module-level dist bindings (seeded from the
/// PPL elaboration state at checkDecl) and Dist-typed function parameters
/// (seeded with their license token at checkFunctionDecl).
let rec provenanceOfSurface (env: TypeEnv) (e: Expr) : Set<string> =
    let prov = provenanceOfSurface env
    let unionMany es = es |> List.map prov |> List.fold Set.union Set.empty
    match e.Kind with
    | ExprKind.ExprVar n ->
        (match lookupVar n env with
         | Some vi ->
             match env.Provenance.TryGetValue vi.VarId with
             | true, s -> s
             | _ -> Set.empty
         | None -> Set.empty)
    | ExprKind.ExprBinOp (_, _, l, r) -> Set.union (prov l) (prov r)
    | ExprKind.ExprUnaryOp (_, x) -> prov x
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprVar "__ppl_cumulant" }, _) -> Set.empty   // a projected component is an ARRAY, not a dist
    | ExprKind.ExprApp (_, args) -> unionMany args            // call result: union of Dist-relevant args (conservative)
    | ExprKind.ExprTyped (x, _) -> prov x
    | ExprKind.ExprTuple es -> unionMany es
    | ExprKind.ExprIf (_, t, f) -> Set.union (prov t) (prov f)
    | ExprKind.ExprLet (b, body) -> Set.union (prov b.Value) (prov body)
    | ExprKind.ExprBlock (stmts, fin) ->
        let stmtProv s =
            let rec go s =
                match s with
                | StmtSpanned (inner, _) -> go inner
                | StmtLet b -> prov b.Value
                | StmtAssign (_, _, r) -> prov r
                | StmtExpr x -> prov x
                | StmtForIn (_, _, _) -> Set.empty
            go s
        Set.union (stmts |> List.map stmtProv |> List.fold Set.union Set.empty)
                  (fin |> Option.map prov |> Option.defaultValue Set.empty)
    | _ -> Set.empty

/// Lower an extent expression to IRExpr, preserving as much info as possible.
let lowerExtentExpr (env: TypeEnv) (expr: Expr) : IRExpr =
    match evalConstExpr env expr with
    | Some n -> IRLit (IRLitInt n)
    | None ->
        // Full static contract (`let static`, static functions) BEFORE the
        // symbolic fallbacks: `Idx<n_seg + 1>` with `let static n_seg = 10`
        // must lower to the literal 11, not to a symbolic IRParam -- a
        // symbolic extent survives to codegen sites (virtual-range extents
        // fills, `extents(range<T>)`) that have no runtime object to read
        // it from, and emits undeclared `__range<i>` references.
        match evalStaticIntExpr env expr with
        | Some n -> IRLit (IRLitInt (int64 n))
        | None ->
            match expr.Kind with
            | ExprKind.ExprVar name -> IRParam (name, 0, IRTNat None)
            | ExprKind.ExprLit (LitInt n) -> IRLit (IRLitInt n)
            | _ -> IRParam ("?", 0, IRTNat None)

/// Lower an extent expression with one bound parameter substituted for an IR
/// expression. Used by DepIdx to substitute the lambda parameter (the outer
/// index variable) into the inner extent expression. Walks the AST recursively
/// so binary-op extents like `n - i` lower correctly.
///
/// This is more general than `lowerExtentExpr`, which falls through to a `?`
/// placeholder for anything beyond ExprLit and ExprVar. For DepIdx the inner
/// extent expression is consumed directly at the iteration-bound emission
/// site, so the expression structure must survive.
let rec substituteAndLowerExtent (env: TypeEnv) (paramName: Ident) (subst: IRExpr) (expr: Expr) : IRExpr =
    match expr.Kind with
    | ExprKind.ExprVar n when n = paramName -> subst
    | _ ->
        match evalConstExpr env expr with
        | Some k -> IRLit (IRLitInt k)
        | None ->
            match expr.Kind with
            | ExprKind.ExprVar name -> IRParam (name, 0, IRTNat None)
            | ExprKind.ExprLit (LitInt n) -> IRLit (IRLitInt n)
            | ExprKind.ExprBinOp (_mode, op, l, r) ->
                let l' = substituteAndLowerExtent env paramName subst l
                let r' = substituteAndLowerExtent env paramName subst r
                let irOpOpt =
                    match op with
                    | OpAdd -> Some IRAdd
                    | OpSub -> Some IRSub
                    | OpMul -> Some IRMul
                    | OpDiv -> Some IRDiv
                    | OpMod -> Some IRMod
                    | _ -> None
                match irOpOpt with
                | Some irOp -> IRBinOp (IRElementwise, irOp, l', r')
                | None -> IRParam ("?", 0, IRTNat None)
            | ExprKind.ExprUnaryOp (OpNeg, e) ->
                IRUnaryOp (IRNeg, substituteAndLowerExtent env paramName subst e)
            | _ ->
                IRParam ("?", 0, IRTNat None)

// 4. AST TypeExpr -> IRType (with extent preservation)

/// Built-in scalar bases and constructors: the names a bare `T` can never be
/// an implicit type VARIABLE for. Module-level and public because the
/// language-surface dump (`blade ide surface`, Ide.fs) reports them as data;
/// this list used to be a `match` local to `prescanTypeVarNames`, which is
/// still the only consumer of the predicate below.
let builtinScalarNames : string list =
    [ "Int"; "Int32"; "Int64"
      "Float"; "Float32"; "Float64"; "Double"
      "Complex64"; "Complex128"
      "Bool"; "Void"; "Nat"; "String"; "Char"
      "Poly"; "Array" ]

let internal builtinScalarSet : Set<string> = Set.ofList builtinScalarNames

/// Membership in `builtinScalarNames`, derived rather than restated: a second
/// copy of the list is exactly the drift the hoist exists to prevent.
let isBuiltinScalar (name: string) : bool = Set.contains name builtinScalarSet

/// Names that can never be a type VARIABLE: `builtinScalarNames` above PLUS
/// `Dist`. Nearly the list `prescanTypeVarNames` tests, and used for the same
/// decision from the other side -- the `T<u>^k` head (array-expression plan
/// bug #8) is a variable exactly when the name is neither one of these nor a
/// declared type. Kept next to `lowerTypeExpr` because that is where the head
/// is classified; the `unitSlotBases` set further down answers a different
/// question (which bases OWN a unit slot) and is not interchangeable.
let isConcreteTypeBaseName (name: string) : bool =
    match name with
    | "Int" | "Int32" | "Int64"
    | "Float" | "Float32" | "Float64" | "Double"
    | "Complex64" | "Complex128"
    | "Bool" | "Void" | "Nat" | "String" | "Char"
    | "Poly" | "Array" | "Dist" -> true
    | _ -> false

/// The UNIT named by a type-variable head's argument list, if any -- the three
/// spellings `tryResolveTagArg` admits in a `Float<u>` slot: a bare unit name,
/// a compound unit expression, and the `u^n` power spelling that parses as a
/// rank-marked type var.
///
/// Hoisted out of `lowerTypeExpr`'s `T<u>^k` arm so that the CARET-FREE
/// spelling `T<u>` decides the same question with the same code. The owner's
/// ruling (2026-08-08) is that `T<u>` and `T<u>^0` are the SAME type, so the
/// only safe implementation is one where the two spellings cannot disagree
/// about whether the head is a variable at all.
let unitOfTypeVarArgs (env: TypeEnv) (args: TypeExpr list) : UnitSig option =
    match args with
    | [TyNamed (argName, [])] -> Map.tryFind argName env.Units
    | [TyUnitExpr ue] ->
        (match resolveUnitExpr env.Units ue with Ok s -> Some s | Error _ -> None)
    | [TyVar (argName, Some n)] when Map.containsKey argName env.Units ->
        (match resolveUnitExpr env.Units (UnitPow (UnitNamed argName, n)) with
         | Ok s -> Some s
         | Error _ -> None)
    | _ -> None

/// Is `TyNamed (name, args)` the CARET-FREE spelling of a unit-carrying type
/// VARIABLE (`T<time>`), rather than an ordinary named-type application?
///
/// Exactly the three conditions the caret arm applies to its head -- not a
/// built-in base, not a declared type, and an argument list that resolves to a
/// unit -- so `T<u>` and `T<u>^0` classify identically by construction.
///
/// Deliberately NOT gated on `Subst.IsTypeVar`. A defaulted parameter's
/// annotation is re-lowered at the CALL SITE (`tryFillDefaultArgs`' `mkFill`
/// ascription), where the callee's prescanned type-var scope is gone -- which
/// is precisely why bare `T` and `T^0` are NOT equivalent today (measured:
/// `function f(t: T = 0.0)` called as `f()` is BL3001 "expected T, got
/// Float64", while `t: T^0` is accepted; `TyVar` mints a variable
/// unconditionally, `TyNamed` consults the scope). The motivating program
/// writes `t_zero: T<time> = 0.0`, i.e. exactly that shape, so a scope-gated
/// rule would have broken on its first use.
let isUnitCarryingTypeVarHead (env: TypeEnv) (name: string) (args: TypeExpr list) : bool =
    not args.IsEmpty
    && not (isConcreteTypeBaseName name)
    && (lookupTypeDef name env).IsNone
    && (unitOfTypeVarArgs env args).IsSome

let rec lowerTypeExpr (env: TypeEnv) (ty: TypeExpr) : IRType =
    match ty with
    | TyInt32 -> IRTScalar ETInt32
    | TyInt64 -> IRTScalar ETInt64
    | TyFloat32 -> IRTScalar ETFloat32
    | TyFloat64 -> IRTScalar ETFloat64
    | TyComplex64 -> IRTScalar ETComplex64
    | TyComplex128 -> IRTScalar ETComplex128
    | TyBool -> IRTScalar ETBool
    | TyUnit -> IRTUnit
    | TyString -> IRTScalar ETString
    | TyChar -> IRTScalar ETInt32

    // A BARE wildcard is unreachable from the surface grammar -- the parser
    // only builds TyWildcard as the sole argument of `Base<_>`, handled just
    // below. Kept total (and equal to `Nat<_>`) so the match stays exhaustive
    // and FS0025 keeps auditing future TypeExpr growth.
    | TyWildcard -> IRTIdxTagged (IRTScalar ETInt64, IRefAny)

    // Tag wildcard `Base<_>`: lower the base bare and wrap it in the
    // any-tag marker, ahead of the name dispatch so no base silently drops
    // the wildcard. `Nat<_>` and `Float64<_>` are the useful spellings;
    // `MyStruct<_>` lowering to "any MyStruct" is harmless. Position
    // legality is enforced separately by irTypeHasTagWildcard.
    | TyNamed (name, [TyWildcard]) ->
        // Nat's bare form is IRTNat (a type-level natural, not a value type),
        // so the wildcard's inner follows elemTypeForIterationIndex instead --
        // int64, exactly what a tagged index VALUE carries.
        let inner =
            if name = "Nat" then IRTScalar ETInt64
            else lowerTypeExpr env (TyNamed (name, []))
        IRTIdxTagged (inner, IRefAny)

    // Bounded primitive (section 2.4): the bounds are a REFINEMENT of the base type,
    // not a distinct runtime representation, so they erase here -- `Float<min=0,
    // max=1>` lowers exactly as `Float`, and `Float<velocity, min=0, max=1>`
    // exactly as `Float<velocity>` (the unit lives on the base node). Nothing
    // downstream -- unification, promotion, codegen -- needs to know. The bounds
    // are enforced where the annotation is WRITTEN: struct fields normalize
    // into the conjunct list at parse time, and other annotation sites carry
    // the surface TypeExpr (Ast.boundedConjuncts).
    | TyBounded (baseTy, _, _) -> lowerTypeExpr env baseTy

    // CARET-FREE `T<u>`: the SAME TYPE as `T<u>^0` (owner ruling, 2026-08-09 --
    // "`^0` should be optional, they're semantically equivalent"). The head is
    // a unit-carrying type VARIABLE under exactly the conditions the caret arm
    // uses (isUnitCarryingTypeVarHead); a real named type keeps its ordinary
    // reading and falls through to the arm below.
    //
    // Implemented as a DESUGAR onto the caret node rather than as a second
    // construction, so the two spellings cannot drift: unification, the unit
    // checks, HM monomorphization, printing and every diagnostic see one
    // representation, and a future change to `T<u>^0` reaches `T<u>` for free.
    //
    // What it replaces: `T<time>` used to fall to the named-type fallback and
    // lower to `IRTNamed "T"` -- an opaque nominal type with the unit SILENTLY
    // DROPPED, so every argument was BL3001 "the parameter is declared T but
    // the argument is Float64<second>". The annotation was not an error and not
    // meaningful either, which is the worst of the three options.
    | TyNamed (name, args) when isUnitCarryingTypeVarHead env name args ->
        lowerTypeExpr env
            (TyAbstractArray (TyNamed (name, args),
                              { Kind = ExprKind.ExprLit (LitInt 0L); Span = noSpan },
                              None))

    | TyNamed (name, args) ->
        // Helper: try to resolve a type arg as a unit annotation, then -- for
        // integer bases -- as a nominal index-type alias. `taggedInner` is the
        // type that sits UNDER an index tag, which differs from `baseType`
        // for Nat (whose bare form is the type-level IRTNat).
        //
        // Units are tried first so `Nat<angular_momentum>` keeps its existing
        // meaning; an index alias only wins when the name is not a unit.
        let tryResolveTagArg baseType (taggedInner: IRType) args =
            let isIntBase =
                match taggedInner with
                | IRTScalar (ETInt32 | ETInt64) -> true
                | _ -> false
            match args with
            | [TyNamed (argName, [])] ->
                match Map.tryFind argName env.Units with
                | Some unitSig -> IRTUnitAnnotated (baseType, unitSig)
                | None when isIntBase ->
                    // `Nat<LatIdx>` -- the explicit spelling of what
                    // elemTypeForIterationIndex produces for `range<LatIdx>`.
                    match lookupTypeDef argName env with
                    | Some (TDIIndexType _) ->
                        IRTIdxTagged (taggedInner, IRefNamed argName)
                    | Some (TDIEnumIdx (_, _, values, _)) ->
                        IRTIdxTagged (IRTScalar (EnumValue.underlyingElemType values),
                                      IRefNamed argName)
                    | _ -> baseType
                | None -> baseType  // not a unit, ignore
            | [TyUnitExpr ue] ->
                // COMPOUND unit annotation (`Float<meter/second^2>`,
                // `Float<second^-1>`, `Float<1>`): structural composition
                // through the same resolver Unit-declaration RHSs use.
                // Lowering stays total: a terminal-quantity misuse (BL3011,
                // surfaced by unitAnnoError at the annotation
                // consumers) or an unknown name degrades to the bare base,
                // exactly like an unknown unit name in the arm above.
                (match resolveUnitExpr env.Units ue with
                 | Ok sig' -> IRTUnitAnnotated (baseType, sig')
                 | Error _ -> baseType)
            | [TyVar (argName, Some n)] when Map.containsKey argName env.Units ->
                // `Float<meter^2>`: the positive-exponent power spelling
                // parses as a rank-marked type VARIABLE (grammar collision
                // with `T^2`); the name being a registered unit disambiguates
                // -- the same units-first policy as the bare-name arm.
                // Terminal quantities reject via the consumer check; resolver
                // failure degrades to the bare base.
                (match resolveUnitExpr env.Units (UnitPow (UnitNamed argName, n)) with
                 | Ok sig' -> IRTUnitAnnotated (baseType, sig')
                 | Error _ -> baseType)
            | _ -> baseType
        let tryResolveUnitArg baseType args = tryResolveTagArg baseType baseType args
        match name with
        | "Int" | "Int32" -> tryResolveUnitArg (IRTScalar ETInt32) args
        | "Int64" -> tryResolveUnitArg (IRTScalar ETInt64) args
        | "Float" | "Float64" | "Double" -> tryResolveUnitArg (IRTScalar ETFloat64) args
        | "Float32" -> tryResolveUnitArg (IRTScalar ETFloat32) args
        | "Complex64" -> tryResolveUnitArg (IRTScalar ETComplex64) args
        | "Complex128" -> tryResolveUnitArg (IRTScalar ETComplex128) args
        // Bool/String route through tryResolveUnitArg like the numeric bases
        // (previously they silently DROPPED type args), so `Bool<flag>` and
        // `String<title>` carry their (typically dimensionless-quantity) tag.
        | "Bool" -> tryResolveUnitArg (IRTScalar ETBool) args
        | "Void" -> IRTUnit
        // Nat resolves a unit arg like the other numeric bases so
        // `Nat<angular_momentum>` carries its tag instead of silently
        // dropping it (non-unit args keep returning bare Nat, as before).
        | "Nat" -> tryResolveTagArg (IRTNat None) (IRTScalar ETInt64) args
        | "String" -> tryResolveUnitArg (IRTScalar ETString) args
        | "Char" -> IRTScalar ETInt32
        | "Poly" ->
            // Each Poly occurrence gets its own fresh arity variable name --
            // packs are independent at the call site, so the rep shouldn't
            // claim they share one variable. Used by ppIRType for
            // diagnostics; doesn't drive per-slot specialization (that's
            // keyed by parameter position in the IR phase).
            let arityName = $"r{env.Builder.FreshId()}"
            match args with
            | [inner] -> IRTPoly (lowerTypeExpr env inner, arityName)
            | _ -> IRTPoly (IRTScalar ETFloat64, arityName)
        | _ when args.IsEmpty && Map.containsKey name env.Units ->
            // BARE unit/quantity name in type position (`10 : levels`,
            // `4.0 : speed`, param `x: speed`). Checked BEFORE the named-type
            // fallback (previously this fell through to IRTNamed and failed as
            // an unknown type). The inner scalar is a fresh inference var: the
            // checked expression's bare type flows in bidirectionally (the
            // permissive asymmetric unify arm binds it), so the ascription
            // adopts the value's scalar while stamping the signature.
            IRTUnitAnnotated (env.Subst.Fresh(), Map.find name env.Units)
        | _ ->
            match lookupTypeDef name env with
            | Some (TDIAlias resolvedTy) -> resolvedTy
            | Some (TDIStruct (n, _, _, _)) -> IRTNamed n
            | Some (TDIVariant (n, _, _)) -> IRTNamed n
            | Some (TDIIndexType _) ->
                // Aliased index type in value position (function param,
                // struct field, let-binding annotation, etc.). Under Option C
                // this lowers to IRTIdxTagged (int64, IRefNamed name) -- an
                // int64 tagged by the index type's nominal name. The codegen
                // emits `using <name> = int64_t;` so the C++ type carries
                // the alias for documentation; runtime backing is still int.
                IRTIdxTagged (IRTScalar ETInt64, IRefNamed name)
            | Some (TDIEnumIdx (_, _, values, _)) ->
                // Same shape, but the underlying type follows the values
                // list (all-string -> ETString, else ETInt64). The C++
                // typedef emitted alongside resolves the alias to the
                // matching primitive.
                let underlying = EnumValue.underlyingElemType values
                IRTIdxTagged (IRTScalar underlying, IRefNamed name)
            | None ->
                // If this name is in the type variable scope (introduced by T^k
                // elsewhere in this declaration), bare T means T^0 (scalar).
                if args.IsEmpty && env.Subst.IsTypeVar(name) then
                    env.Subst.LookupOrCreateTypeVar(name, 0, env.Builder)
                else
                    IRTNamed name  // Forward reference or external type

    | TyArray (elemTy, indexTys) ->
        let elem = lowerElemType env elemTy
        // RaggedIdx requires at least one prior index in the index list to
        // iterate over. A 1-D Array<T like RaggedIdx<lens>> is malformed:
        // there's no prior position to provide the iteration that drives the
        // lengths-array lookup. The check is structural -- first index can't
        // be a closed RaggedIdx.
        //
        // The opaque variant `RaggedIdx<_>` is exempted: it's specifically
        // designed for kernel-parameter types (`g: Array<T like RaggedIdx<_>>`)
        // representing a sub-array peeled from a parent ragged. There is no
        // lengths array to look up; the extent is supplied by the loop
        // binding's ExtentArrayRef at the peel point.
        let firstIsRagged =
            match indexTys with
            | TyRaggedIdx _ :: _ -> true
            | _ -> false
        if firstIsRagged then
            // Produce a degenerate IRTArray; the actual error reporting site
            // is the typechecker proper, not lowering. The placeholder lets
            // downstream lowering proceed to surface a clearer diagnostic
            // when the type appears in a function signature or let binding.
            // Emit a Tag that downstream phases can detect for error reporting.
            let placeholderIdx = {
                Id = env.Builder.FreshId(); Rank = 1
                Extent = IRParam ("__error_ragged_no_prior", 0, IRTNat None)
                Symmetry = SymNone
                Tag = Some "__error_ragged_no_prior"; IxKind = IxKErrorRaggedNoPrior
                Kind = SDimension; Dependencies = []
            }
            mkArrayArrow [placeholderIdx] elem None
        else
            // Index types are normally one IRIndexType per surface index, but
            // dependent forms like DepIdx produce TWO records (outer + inner with
            // Dependencies linking them). lowerIndexTypeList handles the expansion.
            let indices = indexTys |> List.mapi (fun i ity -> lowerIndexTypeList env i ity) |> List.concat
            // Nested-array normalization: `Array<Array<T like Idx<n>> like
            // Idx<m>>` flattens to `Array<T like Idx<m>, Idx<n>>` so all
            // downstream rank-N machinery keys off IndexTypes count, not
            // nesting depth. Without it, arrayRank=1 (outer only) but a
            // literal's computeArrayDims recurses to depth 2, malforming
            // extents[1]={n,m} and breaking allocate<>. Limited to explicit
            // `TyArray (TyArray, _)` syntax; other IRTArray producers don't
            // compose nested arrays. Inner Identity/IsVirtual reset on flatten.
            match elem with
            | ArrayElem inner ->
                mkArrayArrow (indices @ inner.IndexTypes) inner.ElemType None
            | _ ->
                mkArrayArrow indices elem None

    | TyDist (orderExpr, elemTy, axesTys) ->
        // Typed dist tower: Dist<order, Elem like I1, ..., Ik>.
        // The order must be a compile-time integer >= 1 -- a literal, a
        // `let static`, or a static-function call (the replicate-count
        // contract; same two-tier resolution as inferReplicate: cheap
        // evalConstExpr first, then the full StaticEval against the
        // checkModule pre-pass's StaticValues/StaticFunctions). Failure
        // lowers to the -1 SENTINEL, reported at the annotation-consumption
        // sites (inferLetBindingValue / checkFunctionDecl) alongside the
        // ragged no-prior check -- lowerTypeExpr itself has no error channel.
        let order = evalStaticIntExpr env orderExpr
        let elem = lowerElemType env elemTy
        let axes = axesTys |> List.mapi (fun i ity -> lowerIndexTypeList env i ity) |> List.concat
        match order with
        | Some n when n >= 1 -> IRTDist (n, elem, axes)
        | _ -> IRTDist (-1, elem, axes)

    | TyAbstractArray (elemTy, rankExpr, _symmOpt) ->
        // `T<u>^k` -- a UNIT-CARRYING type variable (array-expression plan
        // bug #8). The trailing caret is what marks the head as a variable:
        // without it `T<x>` keeps its ordinary named-type reading, and a head
        // that names a real type (`Float<day>^1`, or any declared struct /
        // alias) is concrete and lowers through the ordinary element path
        // below. The element becomes IRTUnitAnnotated over the SCALAR type
        // variable -- exactly the shape a concrete `Float<u>` element
        // produces -- so every unit walk (IR.getUnits and the arithmetic /
        // kernel rules built on it) reads it identically, and a caller's
        // `Array<Float<u'> like I>` meets it element-to-element in `unify`,
        // where the unit-compatibility check already lives. Nothing here is
        // a new unit mechanism; it is the existing one, reached from an
        // abstract signature.
        //
        // Name resolution follows the `Float<u>` slot exactly (the three
        // spellings tryResolveTagArg admits): a bare unit name, a compound
        // unit expression, and the `u^n` power spelling that parses as a
        // rank-marked type var. An unresolvable name yields None here and is
        // REPORTED by unitAnnoError's TyAbstractArray arm (BL3015), which
        // knows this slot cannot hold anything but a unit. The resolver itself
        // lives at `unitOfTypeVarArgs` above, shared with the CARET-FREE
        // spelling so the two cannot classify a head differently.
        let unitOfArgs (args: TypeExpr list) : UnitSig option = unitOfTypeVarArgs env args
        // (variable name, element type) when the head is a unit-carrying type
        // variable. A bare `T` (TyVar) is NOT one: it keeps the existing
        // whole-array-is-the-variable reading, arity constraint and all.
        let unitVarElem =
            match elemTy with
            | TyNamed (vname, args) when not args.IsEmpty
                                         && not (isConcreteTypeBaseName vname)
                                         && (lookupTypeDef vname env).IsNone ->
                unitOfArgs args
                |> Option.map (fun u ->
                    (vname,
                     IRTUnitAnnotated (env.Subst.LookupOrCreateTypeVar (vname, 0, env.Builder), u)))
            | _ -> None
        match evalConstExpr env rankExpr with
        | Some rank ->
            let r = int rank
            let elemVarName =
                match elemTy with
                | TyVar (n, _) -> Some n
                | _ -> None
            match elemVarName, unitVarElem with
            | Some name, _ ->
                // Type variable with arity: route through type var scope
                env.Subst.LookupOrCreateTypeVar(name, r, env.Builder)
            | None, Some (_, unitElem) when r = 0 ->
                // `T<u>^0`: the scalar type variable, unit stamped on.
                unitElem
            | _ ->
                if r = 0 then
                    lowerTypeExpr env elemTy  // Rank-0: just the scalar
                else
                    let elem =
                        match unitVarElem with
                        | Some (_, unitElem) -> unitElem
                        | None -> lowerElemType env elemTy
                    // Every abstract axis is genuinely UNKNOWN, so it takes
                    // `lowerExtentExpr`'s own unknown-extent spelling rather
                    // than a made-up name like `n`: two abstract parameters
                    // of the same rank are independently shaped (`t:
                    // T<time>^1, w: T<freq>^1` are different lengths), and a
                    // real name would read as the `Idx<n>, Idx<n>` tie and
                    // print as a variable the user never wrote. Nothing
                    // consumes it: extents are never compared in `unify`, and
                    // codegen bakes only LITERAL extents, falling back to the
                    // runtime `.extents[dim]` read for everything else.
                    let indices = [0 .. r - 1] |> List.map (fun _ ->
                        { Id = env.Builder.FreshId(); Rank = 1
                          Extent = IRParam ("?", 0, IRTNat None)
                          Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] })
                    mkArrayArrow indices elem None
        | None ->
            // Non-constant rank (e.g., T^r where r is a variable)
            match elemTy, unitVarElem with
            | TyVar (name, _), _ ->
                // Can't resolve arity statically -- create unconstrained type var
                env.Subst.LookupOrCreateTypeVar(name)
            | _, Some (vname, IRTUnitAnnotated (_, u)) ->
                // `T<u>^r`: rank unknown, so no array shape can be built.
                // The unit still rides the unconstrained variable, the same
                // shape a bare quantity annotation produces.
                IRTUnitAnnotated (env.Subst.LookupOrCreateTypeVar(vname), u)
            | _ ->
                lowerElemType env elemTy  // Arity-polymorphic fallback

    | TyFunc (args, ret) ->
        mkFuncArrow (args |> List.map (lowerTypeExpr env)) (lowerTypeExpr env ret)

    | TyTuple tys -> IRTTuple (tys |> List.map (lowerTypeExpr env))

    // `Tuple<N>` (docs/plan-tuples-vs-arg-packs.md 6c): width written,
    // element types inferred. Lowers to the SAME `IRTTuple` a written
    // `(T1, ..., TN)` produces, with N fresh inference variables in the
    // element slots -- so unify's equal-length `IRTTuple` rule
    // (Unify.fs:710) supplies both the width check (a width mismatch is an
    // ordinary TypeMismatch, no new diagnostic) and the element inference.
    // The parser guarantees n >= 2.
    | TyTupleWidth n -> IRTTuple (List.init n (fun _ -> env.Subst.Fresh()))

    | TyVar (name, arityOpt) ->
        // Type variable with optional arity annotation.
        // T or T^0 = scalar type variable. T^k (k>0) = rank-k array type variable.
        let arity = arityOpt |> Option.defaultValue 0
        env.Subst.LookupOrCreateTypeVar(name, arity, env.Builder)

    // Index types as standalone type expressions denote VALUE TYPES, NOT
    // array types: an anonymous Idx<n> in value position is an int in [0,n)
    // (a loop bound at codegen), not a Float-array. Lowers to IRTIdxTagged
    // (int64, IRefAnon) -- parallel to Float<meters> -> IRTUnitAnnotated --
    // so inferArithType rejects arithmetic on it. Named aliases route
    // through TyNamed above to the same shape with IRefNamed.
    //
    // KNOWN GAP: higher-arity/dependent cases below (TySymIdx, TyAntisymIdx,
    // TyHermitianIdx, TyCompoundIdx, TyDepIdx/TyRaggedIdx/TyRaggedIdxOpaque,
    // TyEquivIdx) preserve the IRTArray-with-Float64 shape -- what a
    // SymIdx<r, n> value-level type should mean isn't decided. No test
    // exercises these paths; the wrong shape is a dead-code latent bug.
    | TyIdx extent ->
        // Value-position TyIdx lowers to IRTIdxTagged wrapping int64.
        // Per Option C: index values are int64 tagged with a nominal
        // IdxRef. The fresh nominalId is the identity; the extent is
        // preserved on the IdxRef solely for diagnostics / pretty-print.
        let nominalId = env.Builder.FreshId()
        IRTIdxTagged (IRTScalar ETInt64,
                      IRefAnon (nominalId, lowerExtentExpr env extent))

    // Both arms keep the documented KNOWN-GAP shape above (IRTArray-with-
    // Float64), but build the index record through the SHARED
    // `symPowerIndexRecord` so the value-position and index-position twins
    // cannot drift. For a `SymIdx<r, IrrepsIdx<s>>` base it carries the spec
    // identity (see the helper's doc comment).
    | TySymIdx (rank, baseIdx) ->
        let idx = symPowerIndexRecord env (env.Builder.FreshId()) rank SymSymmetric baseIdx
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    | TyAntisymIdx (rank, baseIdx) ->
        let idx = symPowerIndexRecord env (env.Builder.FreshId()) rank SymAntisymmetric baseIdx
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    // Same KNOWN-GAP shape as the two arms above, through the same shared
    // record builder -- so the value-position and index-position twins of an
    // OrbIdx cannot drift, exactly as symPowerIndexRecord guarantees for its
    // own pair.
    | TyOrbIdx (levels, baseIdx) ->
        let idx = orbitIndexRecord env (env.Builder.FreshId()) levels baseIdx
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    | TyHermitianIdx extent ->
        let ext = lowerExtentExpr env extent
        let idx = { Id = env.Builder.FreshId(); Rank = 2; Extent = ext
                    Symmetry = SymHermitian; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    | TyBoundedIdx _ -> IRTScalar ETInt64

    // Chunked<I, _> IS I (docs/plans/structural/07 §2.1): every position
    // lowers the inner axis; the segmentation is registered beside the
    // alias by registerTypeDecl and read only by `segments(Alias)`.
    | TyChunked (inner, _) -> lowerTypeExpr env inner
    | TyCompoundIdx _mask ->
        let idx = { Id = env.Builder.FreshId(); Rank = 1; Extent = IRParam ("compound", 0, IRTNat None)
                    Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    | TyEnumIdx valuesExpr ->
        // Determine underlying element type from values. All-string lowers to
        // ETString; otherwise (all-int, empty, or mixed-but-recoverable) ETInt64.
        // Mixed lists won't reach here in practice -- the same extraction is
        // run by registerTypeDecl and surfaces a clean error when aliased; an
        // unaliased mixed-list slips through silently with ETInt64.
        let isAllString =
            match valuesExpr.Kind with
            | ExprKind.ExprArrayLit elems when not elems.IsEmpty ->
                elems |> List.forall (fun el -> match el.Kind with ExprKind.ExprLit (LitString _) -> true | _ -> false)
            | _ -> false
        if isAllString then IRTScalar ETString else IRTScalar ETInt64

    | TyDepIdx _ | TyRaggedIdx _ | TyRaggedIdxOpaque | TyIrrepsIdx _ | TyPgIrrepsIdx _ | TySparseIdx _ ->
        // DepIdx/RaggedIdx/IrrepsIdx/PgIrrepsIdx/SparseIdx in non-index position --
        // the pg member takes PARITY with the O(3) one here: same known gap,
        // same treatment. Defensive fallback matching TyCompoundIdx/TyEquivIdx:
        // wrap in a single-index Array so the IR shape is consistent. Real
        // iteration happens via lowerIndexType, which produces the correctly
        // tagged IRIndexType.
        let idx = lowerIndexType env 0 ty
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

    | TyHalo _ ->
        // halo<Inner, [offs]> is legal only as a range<> slot (handled in the
        // ExprRange arm via haloSlotOf). In any other type position there is
        // no value meaning; degrade to the iteration value (int64) -- the slot
        // path never routes here.
        IRTScalar ETInt64

    | TyPoly inner ->
        // Fresh arity-variable name per Poly occurrence. See the TyNamed "Poly"
        // case above for rationale: packs are independent, so each Poly param
        // gets its own identifier in the type rep.
        let arityName = $"r{env.Builder.FreshId()}"
        IRTPoly (lowerTypeExpr env inner, arityName)
    | TyUnitExpr ue ->
        // A compound unit expression standing ALONE in type position. The
        // parser only produces TyUnitExpr inside a type-argument list, where
        // the enclosing TyNamed arm consumes it via tryResolveTagArg -- but
        // the node is a TypeExpr, so lower it totally: annotate a fresh
        // inferred scalar, exactly like a bare unit name in type position
        // (the checked expression's bare type flows in bidirectionally).
        (match resolveUnitExpr env.Units ue with
         | Ok sig' -> IRTUnitAnnotated (env.Subst.Fresh(), sig')
         | Error _ -> env.Subst.Fresh())
    | TyConstrained (inner, _) -> lowerTypeExpr env inner
    | TyEquivIdx (_dim, _group, _rep) ->
        let idx = { Id = env.Builder.FreshId(); Rank = 1; Extent = IRParam ("equiv", 0, IRTNat None)
                    Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
        mkArrayArrow [idx] (IRTScalar ETFloat64) None

and lowerElemType env ty : IRType =
    // Primitives become IRTScalar et; named index types in element position
    // (foreign-key syntax) become IRTIdxTagged + IRefNamed; user-defined
    // types pass through as IRTNamed; nested arrays pass through as
    // IRTArray; etc.
    //
    // Struct-element and nested-array types propagate as their actual
    // IRType rather than collapsing to ETFloat64 (codegen support for
    // non-primitive element types is still incomplete). Named-index cases
    // produce IRTIdxTagged wrapping the underlying primitive, unifying
    // element-position and value-position encodings.
    match ty with
    | TyNamed (name, []) ->
        match lookupTypeDef name env with
        | Some (TDIIndexType _) ->
            IRTIdxTagged (IRTScalar ETInt64, IRefNamed name)
        | Some (TDIEnumIdx (_, _, values, _)) ->
            let underlying = EnumValue.underlyingElemType values
            IRTIdxTagged (IRTScalar underlying, IRefNamed name)
        | _ -> lowerTypeExpr env ty   // struct, sum, alias, type variable, etc.
    | TyIdx _ | TySymIdx _ | TyAntisymIdx _ | TyOrbIdx _ | TyHermitianIdx _ ->
        // Raw index type syntax in element position (e.g., Array<Idx<3> like ...>).
        // Anonymous-tag preservation here is deferred: collapses to bare
        // int64, losing index identity (named tags are preserved elsewhere).
        IRTScalar ETInt64
    | TyEnumIdx valuesExpr ->
        // Mirror of the value-position handling: all-string values -> ETString,
        // otherwise -> ETInt64. Same anonymous-tag-loss caveat as above.
        let isAllString =
            match valuesExpr.Kind with
            | ExprKind.ExprArrayLit elems when not elems.IsEmpty ->
                elems |> List.forall (fun el -> match el.Kind with ExprKind.ExprLit (LitString _) -> true | _ -> false)
            | _ -> false
        if isAllString then IRTScalar ETString else IRTScalar ETInt64
    | _ ->
        lowerTypeExpr env ty

/// A `SymIdx`/`AntisymIdx` base written as a BARE NAME that turns out to name
/// an index type: the base record to inherit, or None to keep reading the
/// slot as an extent expression. See symPowerIndexRecord's SymBaseExtent arm
/// for why this fallback exists and why it cannot change an existing
/// program's meaning.
///
/// Admits exactly what the inline grammar admits -- a rank-1 dense
/// (`Idx<n>`) or irreps (`IrrepsIdx<spec>`) record. Ragged/dep/compound/
/// sparse/wreath aliases have no symmetric power this builder constructs, so
/// they return None and fall through unchanged rather than inherit a record
/// that would misdescribe their storage.
and symPowerAliasBase (env: TypeEnv) (extent: Expr) : IRIndexType option =
    match extent.Kind with
    | ExprKind.ExprVar name
            when (evalConstExpr env extent).IsNone
                 && (evalStaticIntExpr env extent).IsNone ->
        match lookupTypeDef name env with
        | Some (TDIIndexType _) | Some (TDIEnumIdx _) ->
            let rec' = lowerIndexType env 0 (TyNamed (name, []))
            let admissible =
                rec'.Rank = 1 && rec'.Symmetry = SymNone
                && List.isEmpty rec'.Dependencies
                && (match rec'.IxKind with
                    | IxKPlain | IxKIrreps -> true
                    | _ -> false)
            if admissible then Some rec' else None
        | _ -> None
    | _ -> None

/// The index record for `SymIdx<k, base>` / `AntisymIdx<k, base>`, shared by
/// index-position and value-position lowering.
///
///   - `SymBaseExtent e`: an anonymous rank-k compact record over extent `e`.
///   - `SymBaseIndex ty` (e.g. `SymIdx<k, IrrepsIdx<s>>`): the base index
///     type is lowered first, then re-stamped with Rank/Symmetry; Extent,
///     Tag (mkIrrepsTag), IxKind, Kind, Dependencies (incl. bad-spec ERROR
///     marker) ride through verbatim, so a malformed spec still surfaces at
///     irTypeBadIrrepsDetail.
///
/// Re-stamping (not rebuilding) makes this field-for-field what
/// `deduceOutputType` produces for an INFERRED symmetric group over
/// irreps-typed inputs -- a written annotation and an inferred type are the
/// same type by construction. `IxSymmetryLike` dispatches on Symmetry before
/// IxKind, so this is compact-simplex over total_dim(spec) cells like
/// `SymIdx<k, total_dim>`.
and symPowerIndexRecord env (id: IRId) (rank: int) (symmetry: SymmetryClass)
                          (baseIdx: SymIdxBase) : IRIndexType =
    match baseIdx with
    // A BARE NAME reaches here as SymBaseExtent, never SymBaseIndex:
    // `Parser.parseSymIdxBase` admits only the `Idx`/`IrrepsIdx` KEYWORDS as
    // an index-type base, so `SymIdx<2, n>` reads as "extent n" and stays
    // readable that way forever (a `let static n = 3` base must not change
    // meaning). But when the name resolves to NO value and DOES name a
    // registered index type, the extent reading is not merely unintended --
    // it is unrepresentable: `lowerExtentExpr` falls through to its symbolic
    // `IRParam name` placeholder, and a symbolic extent on a VIRTUAL range
    // operand reaches codegen with no runtime object to read it from, which
    // emitted an undeclared `__range<i>.extents[0]` -- a g++ error naming a
    // compiler-internal, for a type the user spelled correctly.
    //
    // So resolve it to the index type here, as a FALLBACK. Value and static
    // resolution are still attempted first (and `lookupTypeDef` searches a
    // different namespace than `evalConstExpr`), so this arm is reachable
    // only where the old code was heading for that broken placeholder: no
    // existing program can change meaning. Inheriting the base record also
    // carries its nominal Tag onto the symmetric-power record, which is what
    // makes `range<SymIdx<2, N3>>` flow `Nat<N3>` into the kernel instead of
    // an untagged integer that then trips BL4003 against its own base.
    //
    // Admitted bases are exactly what the inline grammar admits -- a rank-1
    // dense (`Idx<n>`) or irreps (`IrrepsIdx<spec>`) record. A ragged, dep,
    // compound, sparse or wreath alias has no symmetric power this builder
    // could construct, so it falls through to the extent path unchanged
    // rather than inheriting a record that would misdescribe its storage.
    | SymBaseExtent extent ->
        match symPowerAliasBase env extent with
        | Some baseRec -> { baseRec with Id = id; Rank = rank; Symmetry = symmetry }
        | None ->
            { Id = id; Rank = rank; Extent = lowerExtentExpr env extent
              Symmetry = symmetry; Tag = None; IxKind = IxKPlain
              Kind = SDimension; Dependencies = [] }
    | SymBaseIndex baseTy ->
        let baseRec = lowerIndexType env 0 baseTy
        { baseRec with Id = id; Rank = rank; Symmetry = symmetry }

/// The index record for `OrbIdx<[(r1,s1), ..., (rd,sd)], base>`, the twin of
/// `symPowerIndexRecord`, shared by index-position and value-position
/// lowering. Normalized first via `IR.orbitNormalForm` (shared with the
/// deduction producer `IRLoopStructure.deduceWreathTie`, so written and deduced classes
/// never disagree): a rank-1 level is the trivial group S_1, so `(1,-)`
/// zeroes nothing and `(1,+)` ties nothing, keeping depth logarithmic.
///
///   []        trivial -- the PLAIN `Idx<n>` record, field for field.
///   [(r,+)]   the `SymIdx<r, base>` record via symPowerIndexRecord (not
///             rebuilt): depth-1 rides the existing compact machinery.
///   [(r,-)]   likewise `AntisymIdx<r, base>`.
///   depth >=2 SymWreath: Rank = product of level ranks, levels carried in
///             Extent as IROrbitClass, IxKOrbit + "__orbidx" sentinel Tag.
///
/// DEFERRED: a BLOCK-SPEC base under depth >= 2
/// (`OrbIdx<[(2,+),(2,+)], IrrepsIdx<s>>`) parses, but the wreath Tag is
/// "__orbidx", so spec identity is lost -- only the base EXTENT survives.
/// Depth <= 1 keeps the irreps tag (goes through symPowerIndexRecord).
and orbitIndexRecord env (id: IRId) (levels: (int * bool) list)
                     (baseIdx: SymIdxBase) : IRIndexType =
    match orbitNormalForm levels with
    | OrbNfTrivial ->
        (match baseIdx with
         | SymBaseExtent extent ->
             { Id = id; Rank = 1; Extent = lowerExtentExpr env extent
               Symmetry = SymNone; Tag = None; IxKind = IxKPlain
               Kind = SDimension; Dependencies = [] }
         | SymBaseIndex baseTy ->
             // The base index type verbatim (rank 1, no symmetry) -- its own
             // identity, which is what `OrbIdx<[], Idx<n>>` should mean.
             { lowerIndexType env 0 baseTy with Id = id; Rank = 1; Symmetry = SymNone })
    | OrbNfDepth1 (r, isPlus) ->
        symPowerIndexRecord env id r (if isPlus then SymSymmetric else SymAntisymmetric) baseIdx
    | OrbNfWreath normalized ->
        // Rank is the RAW AXIS COUNT (the product of the level ranks), bounded
        // before it lands in the record's int field -- see mkWreathIndexRecord,
        // which both this producer and DEDUCTION share so the two cannot build
        // differently-shaped records for the same class.
        let baseExtent =
            match baseIdx with
            | SymBaseExtent extent -> lowerExtentExpr env extent
            | SymBaseIndex baseTy -> (lowerIndexType env 0 baseTy).Extent
        mkWreathIndexRecord id normalized baseExtent

/// Resolve a SparseIdx keys expression to its (source, rank). Shared by the
/// SparseIdx<keys> TYPE form (lowerIndexType, which failwiths on Error) and
/// the sparse(values, keys) BUILDER (which surfaces Error as a type error).
///
///   STATIC:  the keys expression folds under the static contract (a `let
///            static` tuple list). Entries are validated (uniform arity,
///            non-negative Nat components, no duplicates) and BAKED
///            (SkStatic) -- codegen emits the table as literals and no
///            runtime array is consulted, so a mutated source cannot desync
///            the index.
///   RUNTIME: a named variable of rank-1 tuple-element array type
///            (SkRuntime), mirroring the compound mask's deferred build.
and resolveSparseKeysSource (env: TypeEnv) (keysExpr: Expr) : Result<SparseKeysSource * int, string> =
    match evalStaticValueExpr env keysExpr with
    | Ok sv ->
        let decodeEntry (e: StaticEval.StaticValue) : Result<int64 list, string> =
            match e with
            | StaticEval.SVTuple comps ->
                comps |> List.fold (fun acc c ->
                    acc |> Result.bind (fun vs ->
                        match c with
                        | StaticEval.SVInt v when v >= 0L -> Ok (vs @ [v])
                        | StaticEval.SVInt v -> Error $"SparseIdx: key coordinates must be non-negative; got {v}"
                        | other -> Error (sprintf "SparseIdx: key tuple components must be Nat literals; got %A" other))) (Ok [])
            | StaticEval.SVInt v when v >= 0L -> Ok [v]   // rank-1: bare Nat keys
            | StaticEval.SVInt v -> Error $"SparseIdx: key coordinates must be non-negative; got {v}"
            | other -> Error (sprintf "SparseIdx: keys must be a static list of Nat tuples; got element %A" other)
        (match sv with
         | StaticEval.SVTuple elems when not elems.IsEmpty ->
             elems |> List.fold (fun acc e ->
                 acc |> Result.bind (fun es -> decodeEntry e |> Result.map (fun d -> es @ [d]))) (Ok [])
         | other -> Error (sprintf "SparseIdx: keys must be a non-empty static list of Nat tuples; got %A" other))
        |> Result.bind (fun entries ->
            let arity = entries.Head.Length
            if entries |> List.exists (fun e -> e.Length <> arity) then
                Error $"SparseIdx: all key tuples must have the same arity; first is {arity}-ary"
            elif (entries |> List.distinct |> List.length) <> entries.Length then
                Error "SparseIdx: duplicate key tuple in static key list"
            else Ok (SkStatic entries, arity))
    | Error _ ->
        // Runtime branch: named keys variable, rank-1 array of Nat tuples
        // (or of bare Nats for a rank-1 sparse index).
        (match keysExpr.Kind with
         | ExprKind.ExprVar name ->
             (match lookupVar name env with
              | Some vi ->
                  (match vi.Type with
                   | ArrayElem arr when (arr.IndexTypes |> List.sumBy (_.Rank)) = 1 ->
                       (match arr.ElemType with
                        | IRTTuple ts when ts.Length >= 2 ->
                            let natLike t =
                                match t with
                                | IRTNat _ | IRTScalar ETInt64 | IRTScalar ETInt32 -> true
                                | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> true
                                | _ -> false
                            if ts |> List.forall natLike then Ok (SkRuntime (IRVar (vi.VarId, vi.Type)), ts.Length)
                            else Error (sprintf "SparseIdx<%s>: key tuple components must be Nat-valued; '%s' has element type %A" name name arr.ElemType)
                        | IRTNat _ | IRTScalar ETInt64 | IRTScalar ETInt32 ->
                            Ok (SkRuntime (IRVar (vi.VarId, vi.Type)), 1)
                        | other ->
                            Error (sprintf "SparseIdx<%s>: keys must be a rank-1 array of Nat tuples (Array<(Nat, ...) like ...>); '%s' has element type %A" name name other))
                   | ArrayElem _ ->
                       Error $"SparseIdx<{name}>: keys array must be rank 1 (one key tuple per entry)"
                   | other ->
                       Error (sprintf "SparseIdx<%s>: keys must be an array (Array<(Nat, ...) like ...>); '%s' has type %A" name name other))
              | None -> Ok (SkRuntime (lowerExtentExpr env keysExpr), 1))
         | _ -> Ok (SkRuntime (lowerExtentExpr env keysExpr), 1))

and lowerIndexType env (_position: int) (ty: TypeExpr) : IRIndexType =
    let id = env.Builder.FreshId()
    match ty with
    | TyIdx extent ->
        { Id = id; Rank = 1; Extent = lowerExtentExpr env extent
          Symmetry = SymNone; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    | TySymIdx (rank, baseIdx) -> symPowerIndexRecord env id rank SymSymmetric baseIdx
    | TyAntisymIdx (rank, baseIdx) -> symPowerIndexRecord env id rank SymAntisymmetric baseIdx
    | TyOrbIdx (levels, baseIdx) -> orbitIndexRecord env id levels baseIdx
    | TyHermitianIdx extent ->
        { Id = id; Rank = 2; Extent = lowerExtentExpr env extent
          Symmetry = SymHermitian; Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    | TyEnumIdx valuesExpr ->
        let nValues =
            match valuesExpr.Kind with
            | ExprKind.ExprArrayLit elems -> int64 elems.Length
            | _ -> 0L
        { Id = id; Rank = 1; Extent = IRLit (IRLitInt nValues); Symmetry = SymNone
          Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }
    | TyDepIdx (outerTy, _paramName, _bodyTy) ->
        // Single-slot context (e.g., type alias, range): return a placeholder
        // inner-only record. Two-record expansion happens at the array-index-
        // list construction site (lowerIndexTypeList). Single-slot use of
        // DepIdx is suspect -- code paths that need the full DepIdx structure
        // (iteration, etc.) should route through lowerIndexTypeList instead.
        let outerIdx = lowerIndexType env _position outerTy
        { Id = id; Rank = 2; Extent = IRParam ("__depidx_inner", 0, IRTNat None)
          Symmetry = SymNone; Tag = Some "__depidx"; IxKind = IxKDep
          Kind = SDimension; Dependencies = [outerIdx.Id] }
    | TyRaggedIdx lengthsExpr ->
        // Single-record shape matching lowerIndexTypeList's unaliased TyRaggedIdx
        // case, so aliasing (`type R = RaggedIdx<lens>` then `Array<... R>`)
        // gets the correct rank. The structural tag `__raggedidx` is preserved
        // so codegen predicates (isRaggedArrayType etc.) keep detecting the
        // ragged form through alias indirection.
        let lengthsIR = lowerExtentExpr env lengthsExpr
        { Id = id; Rank = 1; Extent = IRRaggedLookup lengthsIR
          Symmetry = SymNone; Tag = Some "__raggedidx"; IxKind = IxKRagged
          Kind = SDimension; Dependencies = [] }
    | TyRaggedIdxOpaque ->
        // Opaque-extent variant: rank-1, no lengths array, no outer position.
        // Used in kernel-parameter types where the extent is supplied by the
        // peel context. The Extent is a sentinel (IROpaqueExtent) rather than
        // a placeholder IRParam, so codegen can distinguish "extent unknown
        // because we haven't computed it yet" (IRParam) from "extent supplied
        // by surrounding loop binding" (IROpaqueExtent).
        { Id = id; Rank = 1; Extent = IROpaqueExtent
          Symmetry = SymNone; Tag = Some "__raggedidx_opaque"; IxKind = IxKRaggedOpaque
          Kind = SDimension; Dependencies = [] }
    | TyIrrepsIdx specExpr ->
        // IrrepsIdx<spec>: block-structured dense index over an irreps spec.
        // The spec resolves under the full static contract (like Dist's
        // order); extent = total_dim(spec) and EVERY cell is stored -- flat
        // dense, no compression -- so the record rides the ordinary dense
        // paths (SymNone). The block structure matters for IDENTITY, carried
        // in the Tag (mkIrrepsTag: spec equality = index-space identity;
        // Unify adds the spec-mismatch strictness arm). lowerIndexType has
        // no error channel, so a non-static/malformed spec lowers to the
        // marker record consumed by irTypeHasBadIrrepsSpec at let-binding /
        // function-signature sites (ragged-no-prior pattern), the failure
        // detail smuggled in the IRParam name.
        (match evalStaticValueExpr env specExpr
               |> Result.bind (Blade.ML.Statics.specOfStatic "IrrepsIdx") with
         | Ok spec ->
             let triples = spec |> List.map (fun e -> (e.L, e.Parity, e.Mult))
             { Id = id; Rank = 1
               Extent = IRLit (IRLitInt (int64 (Blade.ML.Spec.totalDim spec)))
               Symmetry = SymNone; Tag = Some (mkIrrepsTag None triples)
               IxKind = IxKIrreps; Kind = SDimension; Dependencies = [] }
         | Error detail ->
             // specOfStatic prefixes its own "IrrepsIdx: " (its `what`
             // label); the consumption-site diagnostic adds the same prefix,
             // so strip it here to avoid "IrrepsIdx: IrrepsIdx: ...".
             let detail =
                 if detail.StartsWith "IrrepsIdx: " then detail.Substring "IrrepsIdx: ".Length
                 else detail
             { Id = id; Rank = 1
               Extent = IRParam (detail, 0, IRTNat None)
               Symmetry = SymNone; Tag = Some "__error_irreps_bad_spec"
               IxKind = IxKErrorIrrepsBadSpec; Kind = SDimension; Dependencies = [] })
    | TyPgIrrepsIdx (groupName, specExpr) ->
        // PgIrrepsIdx<GROUP, spec>: the point-group block-spec member. Field
        // for field the shape of the TyIrrepsIdx arm above -- rank 1, extent
        // a folded literal, dense (SymNone) with the block structure carried
        // as IDENTITY in the Tag, and the same error-marker channel for a
        // non-static/malformed spec -- over a DIFFERENT frozen tag prefix and
        // a DIFFERENT decoder.
        //
        // TWO resolutions, in order, because the diagnostics differ: the GROUP
        // name against the frozen registry, then the spec's LABELS against
        // THAT group's character table. Getting the group wrong and getting a
        // label wrong are different mistakes and each names its own roster.
        (match Blade.ML.Statics.pgGroupByName "PgIrrepsIdx" groupName
               |> Result.bind (fun grp ->
                    evalStaticValueExpr env specExpr
                    |> Result.bind (Blade.ML.Statics.pgSpecOfStatic "PgIrrepsIdx" grp)
                    |> Result.map (fun spec -> (grp, spec))) with
         | Ok (grp, spec) ->
             { Id = id; Rank = 1
               Extent = IRLit (IRLitInt (int64 (Blade.ML.PointSpec.pgTotalDim grp spec)))
               Symmetry = SymNone; Tag = Some (mkPgIrrepsTag grp.Name None spec)
               IxKind = IxKPgIrreps; Kind = SDimension; Dependencies = [] }
         | Error detail ->
             // The decoders prefix their own "PgIrrepsIdx: " (their `what`
             // label); the consumption-site diagnostic adds the same prefix,
             // so strip it here to avoid "PgIrrepsIdx: PgIrrepsIdx: ...".
             let detail =
                 if detail.StartsWith "PgIrrepsIdx: " then detail.Substring "PgIrrepsIdx: ".Length
                 else detail
             { Id = id; Rank = 1
               Extent = IRParam (detail, 0, IRTNat None)
               Symmetry = SymNone; Tag = Some "__error_pgirreps_bad_spec"
               IxKind = IxKErrorPgIrrepsBadSpec; Kind = SDimension; Dependencies = [] })
    | TyNamed (name, _) ->
        match lookupTypeDef name env with
        | Some (TDIIndexType (_, idx, _)) -> { idx with Id = id }
        | Some (TDIEnumIdx (_, idx, _, _)) -> { idx with Id = id }
        // A `static struct` name stands where an index type stands
        // (docs/plans/structural/06): its solution set is the key
        // enumeration of a SparseIdx-shaped slot -- closed-form (SkDomain)
        // when the constraints are linear, a certified baked table otherwise.
        // Refusals surface at the range<> seam (TypeCheckInfer) with their
        // own code; this arm's failwith is the annotation-only backstop.
        | Some (TDIStruct _) when (match Map.tryFind name (staticEnvOf env).Structs with
                                  | Some si -> si.IsStatic
                                  | None -> false) ->
            (match Blade.StructIdxSpec.domainRoute (staticEnvOf env) name with
             | Ok (Blade.StructIdxSpec.DomainClosedForm plan) ->
                 { Id = id; Rank = plan.Levels.Length; Extent = IRSparseKeys (SkDomain plan)
                   Symmetry = SymNone; Tag = Some "__sparseidx"; IxKind = IxKSparse
                   Kind = SDimension; Dependencies = [] }
             | Ok (Blade.StructIdxSpec.DomainTable (entries, _, _)) ->
                 let rank =
                     match entries with
                     | e :: _ -> e.Length
                     | [] -> (match Map.tryFind name (staticEnvOf env).Structs with Some si -> si.FieldDecls.Length | None -> 1)
                 { Id = id; Rank = rank; Extent = IRSparseKeys (SkStatic entries)
                   Symmetry = SymNone; Tag = Some "__sparseidx"; IxKind = IxKSparse
                   Kind = SDimension; Dependencies = [] }
             | Error msg -> failwith msg)
        | _ ->
            { Id = id; Rank = 1; Extent = IRParam (name, 0, IRTNat None); Symmetry = SymNone
              Tag = Some name; IxKind = ixKindOfTag (Some name); Kind = SDimension; Dependencies = [] }
    | TyChunked (inner, _) -> lowerIndexType env _position inner
    | TyCompoundIdx maskExpr ->
        // CompoundIdx<mask> -- masked product space (formalism 4.5). Rank = the
        // RANK of the mask array (its number of dimensions). The mask is a runtime
        // array value carried in IRCompoundMask for codegen; cardinality (popcount)
        // is computed at runtime by the emitted compound_index_t. Canonical surface
        // form is a named mask whose declared type yields the rank; other forms
        // fall back to a rank-1 degraded placeholder for now (no producer relies on
        // them yet). Nested matches are parenthesized to avoid outer-arm absorption.
        let maskIR, rank =
            match maskExpr.Kind with
            | ExprKind.ExprVar name ->
                (match lookupVar name env with
                 | Some vi ->
                     let rank =
                         (match vi.Type with
                          | ArrayElem arr ->
                              // Enforce: a CompoundIdx mask must be Array<bool like ...>.
                              // Construction (popcount + flatten to std::vector<bool>) is
                              // cheap only for a boolean mask, so a non-bool (or non-array)
                              // mask is a hard type error here rather than a silent
                              // downstream miscompile. (A span-attributed diagnostic would
                              // be nicer, but lowerIndexType has no error channel today.)
                              (match arr.ElemType with
                               | IRTScalar ETBool -> ()
                               | other ->
                                   failwithf "CompoundIdx<%s>: mask must have bool element type (Array<bool like ...>); '%s' has element type %A" name name other)
                              arr.IndexTypes |> List.sumBy (_.Rank)
                          | other ->
                              failwithf "CompoundIdx<%s>: mask must be an array (Array<bool like ...>); '%s' has type %A" name name other)
                     IRVar (vi.VarId, vi.Type), rank
                 | None -> lowerExtentExpr env maskExpr, 1)
            | _ -> lowerExtentExpr env maskExpr, 1
        { Id = id; Rank = rank; Extent = IRCompoundMask maskIR
          Symmetry = SymNone; Tag = Some "__compoundidx"; IxKind = IxKCompound
          Kind = SDimension; Dependencies = [] }
    | TySparseIdx keysExpr ->
        // SparseIdx<keys> -- explicit valid-tuple enumeration (formalism 3.5).
        // Rank is IMPLICIT: the key tuple arity. Keys keep their given order;
        // lookup is by tuple hash (no grid, no per-axis extents). The
        // static/runtime branch split lives in resolveSparseKeysSource.
        // Validation failures are hard errors like the compound-mask arm's
        // (lowerIndexType has no error channel today).
        let source, rank =
            match resolveSparseKeysSource env keysExpr with
            | Ok sr -> sr
            | Error msg -> failwith msg
        { Id = id; Rank = rank; Extent = IRSparseKeys source
          Symmetry = SymNone; Tag = Some "__sparseidx"; IxKind = IxKSparse
          Kind = SDimension; Dependencies = [] }
    | _ ->
        { Id = id; Rank = 1; Extent = IRParam ("?", 0, IRTNat None); Symmetry = SymNone
          Tag = None; IxKind = IxKPlain; Kind = SDimension; Dependencies = [] }

/// Lower an index type to a list of IRIndexType records. Most types produce a
/// single-element list; dependent forms (DepIdx, RaggedIdx) produce two records
/// -- outer + inner with Dependencies linking them.
///
/// Used at array-index-list construction sites where a multi-record expansion
/// is meaningful. Single-slot contexts (range, type alias) use lowerIndexType
/// directly and get a placeholder for dependent forms.
and lowerIndexTypeList (env: TypeEnv) (position: int) (ty: TypeExpr) : IRIndexType list =
    match ty with
    | TyDepIdx (outerTy, paramName, bodyTy) ->
        // Lower outer first to get its Id; that Id is the dependency target
        // for the inner extent's reference to the lambda parameter.
        let outerIdx = lowerIndexType env position outerTy
        let outerWithTag = { outerIdx with Tag = Some "__depidx_outer"; IxKind = IxKDepOuter }
        let outerVarRef = IRVar (outerWithTag.Id, IRTScalar ETInt64)
        // Extract the inner extent expression. Two body shapes are recognized:
        //   - Lambda form: `lambda(i) -> Idx<expr>` parses to `TyIdx expr`.
        //     Substitute paramName with outerVarRef in expr.
        //   - Eta-reduced form: `DepIdx<O, f>` parses to a synthesized
        //     TyNamed(funcName, [TyNamed(paramName, [])]) body. Inline by
        //     looking up f in StaticFunctions, taking its body Expr, and
        //     substituting f's param name (not paramName) with outerVarRef.
        // Anything else falls back to a runtime placeholder.
        let innerExtent =
            match bodyTy with
            | TyIdx e ->
                substituteAndLowerExtent env paramName outerVarRef e
            | TyNamed (funcName, [TyNamed (innerParam, [])]) when innerParam = paramName ->
                match Map.tryFind funcName env.StaticFunctions with
                | Some funcDecl when funcDecl.Params.Length = 1 ->
                    // Substitute the function's formal param with the outer
                    // iteration var. The function's body becomes the inner
                    // extent expression -- its structure is fixed at compile
                    // time, but it evaluates per-iteration as outer walks.
                    //
                    // Peel a trivial block wrapper so `function f(x) = { e }`
                    // (parses to ExprBlock([], Some e)) reduces the same as
                    // the inline form `function f(x) = e`.
                    let funcParamName = funcDecl.Params.[0].Name
                    let bodyExpr =
                        match funcDecl.Body.Kind with
                        | ExprKind.ExprBlock ([], Some e) -> e
                        | _ -> funcDecl.Body
                    substituteAndLowerExtent env funcParamName outerVarRef bodyExpr
                | _ ->
                    IRParam ("__depidx_inner", 0, IRTNat None)
            | _ ->
                IRParam ("__depidx_inner", 0, IRTNat None)
        let innerIdx = {
            Id = env.Builder.FreshId()
            Rank = 1
            Extent = innerExtent
            Symmetry = SymNone
            Tag = Some "__depidx_inner"; IxKind = IxKDepInner
            Kind = SDimension
            Dependencies = [outerWithTag.Id]
        }
        [outerWithTag; innerIdx]
    | TyRaggedIdx lengthsExpr ->
        // RaggedIdx contributes a SINGLE record. Its inner extent is a
        // per-iteration lookup into the lengths array, indexed by the
        // current outer iteration's flat position. The lengths array's
        // shape conceptually mirrors the prior index dimensions of the
        // enclosing array (e.g., for Idx<M>, Idx<N>, RaggedIdx<lens>,
        // lens is internally M*N elements); the codegen handles the
        // flat-position computation so the user-facing type stays clean.
        //
        // RaggedIdx is "open" -- it does NOT declare its own outer position;
        // it references the iteration over the prior index types in the
        // enclosing Array's index list. A 1-D `Array<T like RaggedIdx<lens>>`
        // is malformed (no prior index to iterate); RaggedIdx requires at
        // least one prior index. The malformedness check happens at the
        // TyArray level (see lowerTypeExpr), not here, since this function
        // doesn't see the surrounding context.
        let lengthsIR = lowerExtentExpr env lengthsExpr
        [{
            Id = env.Builder.FreshId()
            Rank = 1
            Extent = IRRaggedLookup lengthsIR
            Symmetry = SymNone
            Tag = Some "__raggedidx"; IxKind = IxKRagged
            Kind = SDimension
            Dependencies = []  // populated by the codegen iteration as needed
        }]
    | TyRaggedIdxOpaque ->
        // Opaque-extent variant -- used in kernel-parameter types where the
        // extent is supplied by the surrounding peel context, not declared
        // up front. Single-record like the closed form, but the Extent is
        // IROpaqueExtent (a marker, no payload) and the Tag distinguishes it
        // from the closed form for downstream codegen routing.
        [{
            Id = env.Builder.FreshId()
            Rank = 1
            Extent = IROpaqueExtent
            Symmetry = SymNone
            Tag = Some "__raggedidx_opaque"; IxKind = IxKRaggedOpaque
            Kind = SDimension
            Dependencies = []
        }]
    | TyNamed (n, _) ->
        // DepIdx aliases: recurse on the stored body so the multi-record
        // expansion (outer + inner, linked by Dependencies) runs at the use
        // site -- the catch-all below would return the single-record
        // placeholder registered at declaration time, structurally wrong for
        // DepIdx. Other aliases are structurally one record and skip
        // recursion deliberately: the alias name is baked into the stored
        // idx's Tag (nominative identity for ETIndexRef foreign keys), and
        // re-walking would lose it. Chained aliases (`type B = A` where
        // `type A = DepIdx<...>`) are not handled: registerTypeDecl routes
        // that to TDIAlias, not TDIIndexType.
        match lookupTypeDef n env with
        | Some (TDIIndexType (_, _, (TyDepIdx _ as body))) ->
            lowerIndexTypeList env position body
        | _ ->
            [lowerIndexType env position ty]
    | _ -> [lowerIndexType env position ty]

/// Decide the element type for a virtual array iterating over a given
/// index: the element values produced ARE positions in the indexed space,
/// so they carry that space's tag when it has a user-named identity
/// (anonymous/synthetic-tagged indices fall back to plain int64). This is
/// the iteration-tagging hook (section 4.18 indirect): `range<LatIdx> <@>
/// lambda(i) -> A(i)` typechecks under step 5's tag rule because i inherits
/// `Nat<LatIdx>` here, with no manual annotation needed.
let elemTypeForIterationIndex (idx: IRIndexType) : IRType =
    match idx.Tag with
    | Some name when name.StartsWith("__halowin|") ->
        // Halo window param: the kernel receives this as `w`, and w(o) neighbor
        // reads dispatch on the "__halowin|" tag (offsets + inner name encoded
        // in it). Carried as a tagged int index so it erases to int64 in C++.
        IRTIdxTagged (IRTScalar ETInt64, IRefNamed name)
    | Some name when not (name.StartsWith("__")) ->
        IRTIdxTagged (IRTScalar ETInt64, IRefNamed name)
    | _ ->
        IRTScalar ETInt64

// Co-iteration shape agreement (multi-axis co-iteration)

/// A plain dense rank-1 index record -- the kind that can share a co-iteration
/// product axis. Mirrors the isPlainDense predicate used by the fallback
/// operator's operand checks.
let isPlainDenseIx (ix: IRIndexType) : bool =
    ix.IxKind = IxKPlain && ix.Symmetry = SymNone && ix.Rank <= 1

/// Structural agreement of two index records for co-iteration purposes.
/// Compares shape-bearing fields only -- the nominal Id is EXCLUDED because
/// every occurrence of an index type gets a fresh Id; two Array<F64 like
/// Lat, Lon> annotations must agree.
let indexRecordsAgree (a: IRIndexType) (b: IRIndexType) : bool =
    a.Rank = b.Rank && a.Symmetry = b.Symmetry && a.IxKind = b.IxKind
    && a.Kind = b.Kind && a.Tag = b.Tag && a.Extent = b.Extent

/// Whole-shape agreement: same record count, records pairwise agree.
let indexShapesAgree (xs: IRIndexType list) (ys: IRIndexType list) : bool =
    xs.Length = ys.Length && List.forall2 indexRecordsAgree xs ys

/// A record list co-iteration can span: exactly one record (dense rank-1 OR
/// packed symmetric of any logical rank -- walked as flat canonical cells), or
/// several records ALL plain dense (the product space). Mixed dense+packed
/// multi-record shapes are rejected -- the packed record's triangular walk
/// cannot interleave a foreign dense axis.
let coIterableRecords (recs: IRIndexType list) : bool =
    recs.Length = 1 || recs |> List.forall isPlainDenseIx

/// The record shape of a `group_by` result: the group axis, then the ragged
/// member axis. Several of these CAN co-iterate -- the rows line up one-to-one
/// -- but only when they were grouped by the SAME keys, which is a fact about
/// the operand EXPRESSIONS, not their types (two `group_keys` calls produce
/// structurally identical records). So this recognises the shape only; the
/// same-keys obligation is discharged at the call site, which can see the
/// expressions (`sameGroupKeysBinding` in inferMethodFor).
let isGroupedRaggedShape (recs: IRIndexType list) : bool =
    match recs with
    | [outer; inner] -> outer.IxKind = IxKGroupOuter && inner.IxKind = IxKGroupMember
    | _ -> false

/// PACKING agreement for ONE shared co-iteration axis: same logical rank and
/// same symmetry. That pair is exactly how cells are addressed -- a compact
/// axis (`SymIdx<2, 4>`: rank 2, one cell per canonical tuple, laid out as a
/// simplex) and a plain dense axis (`Idx<4>`: rank 1, one cell per subscript)
/// enumerate different cell counts with different subscript arithmetic, so no
/// single walk serves both whatever their extents say. That mix used to reach
/// codegen and die in g++ on `double[size_t]` -- the invalid-emission class.
///
/// Deliberately does NOT compare `IxKind`/`Kind`. Most index kinds vary the
/// PROVENANCE of a flat dense walk, not the walk, and they co-iterate with a
/// plain `Idx<n>` today by design: an irreps block index (`IrrepsIdx<..>`,
/// `PgIrrepsIdx<..>`) is block-structured DENSE, and a `group_by` outer slot
/// (`Idx<__ngroups>`) is a dense group axis. The equivariant, ml-ops and
/// grouped-peel corpora all zip those against plain axes; requiring IxKind
/// equality here cost 11 of them.
let indexPackingCoIterable (a: IRIndexType) (b: IRIndexType) : bool =
    a.Rank = b.Rank && a.Symmetry = b.Symmetry

/// NOMINAL agreement for a shared co-iteration axis: two index types that both
/// carry a user-facing NAME must carry the SAME name. `type LatIdx = Idx<180>`
/// registers its record with `Tag = Some "LatIdx"` (the plain-index arm of
/// `registerTypeDecl`), so the provenance really is in the IR and this
/// really is checkable -- which is what CLAUDE.md's invariant asks for
/// ("Nat<LatIdx> and Nat<LonIdx> don't unify even at equal extent ... index
/// provenance is part of the type"), and what the rank->=2 product rule
/// already enforces through `indexRecordsAgree`'s Tag comparison. Rank 1 was
/// the hole.
///
/// PERMISSIVE on the anonymous side, on purpose and on precedent. An untagged
/// record is "some axis of this extent", not "an axis distinct from every
/// named one": `let mut dv = [0.0, 0.0, 0.0]` beside an
/// `Array<Float64 like R>` is the ordinary way to write a seed vector, and
/// `dv * seed` must keep working (ad-jvp-comb/043-045 are exactly that). This
/// is the same name-permissive rule the irreps aliases already state --
/// "anonymous `IrrepsIdx<spec>` unifies with either" -- one layer down.
///
/// Restricted to `IxKPlain` because Tag doubles as the KIND sentinel
/// (`__group_outer`, `__raggedidx`, the irreps payloads). For anything but a
/// plain record the tag is not a name and comparing it would mean the
/// packing/provenance conflation `indexPackingCoIterable` deliberately avoids.
///
/// THE ONE EXCEPTION is a PROVIDER PROVENANCE tag (`isProviderAxisTag`: the
/// dense `__icaxis|` spelling and both pool spellings), which is provenance,
/// not a kind sentinel -- a pool's kind is recovered from the tag's spelling
/// instead (`ixKindOfTag`). Two provider-tagged records must therefore agree
/// whatever their `IxKind` says, which is what makes a cross-repo
/// `ca.vars.cov - cb.vars.cov` refuse for a WREATH pool (IxKOrbit) as well as a
/// simplex one -- a dense `__icaxis|` record is `IxKPlain` and would reach the
/// same verdict through the fallthrough, but naming the whole family here
/// keeps the rule one rule. A provider tag against anything else stays
/// permissive, the same anonymous-side rule as above: a checkout's packed
/// variable beside a locally-declared `Array<Float64 like SymIdx<2, 4>>` is
/// not a provenance clash, nor is one beside a plain Zarr store's untagged
/// pool.
///
/// A provider axis reaching this predicate under an ALIAS still carries its
/// own tag, not the alias name (`registerTypeDecl`'s provider carve-out), so
/// `type L = ck.index.lat; let w: Array<Float64 like L> = lats; w * lats`
/// co-iterates (same axis, same tag), while the same shape across two
/// diverged checkouts does not.
let indexNamesCoIterable (a: IRIndexType) (b: IRIndexType) : bool =
    match a.Tag, b.Tag with
    | Some x, Some y when isProviderAxisTag x && isProviderAxisTag y -> x = y
    | _ ->
        a.IxKind <> IxKPlain || b.IxKind <> IxKPlain
        || (match a.Tag, b.Tag with
            | Some x, Some y -> x = y
            | _ -> true)

/// The provider-split clause for a pair of records, ready to append (a leading
/// space, or nothing). See `Types.providerSplitClause` for what it says and
/// when: the short version is that two identities of one provider axis print
/// as 'lat' and 'lat#2' -- or, across two repos, identically -- and neither
/// spelling explains itself.
let private axisSplitSuffix (a: IRIndexType) (b: IRIndexType) : string =
    match a.Tag, b.Tag with
    | Some ta, Some tb ->
        (match providerSplitClause ta tb with
         | Some c -> " " + c
         | None -> "")
    | _ -> ""

/// WHICH record of two multi-axis shapes disagrees, ready to append to a
/// shape-clash refusal (a leading ": ", or "" when nothing structural is to
/// blame -- e.g. the operands are shape-equal and were refused for PACKING).
///
/// Every multi-axis co-iteration refusal in the front end used to end at
/// "... must have identical index shapes (same records: tags, extents,
/// symmetry)", naming no operand, no axis and no disagreement. On a provider
/// program -- two checkouts of one store, exactly one of five axes moved --
/// that is the difference between a one-line fix and re-reading the store.
/// Shared by the two sites that say it (`zipSharedRecords` here and the
/// elementwise-operator arm in TypeCheckInfer) so the two cannot drift; the
/// labels are the caller's because the two word their operands differently.
let indexShapeClashDetail (labelA: string) (labelB: string)
                          (a: IRIndexType list) (b: IRIndexType list) : string =
    let nameOf (ix: IRIndexType) =
        match ix.Tag with
        | Some t -> displayTagName t
        | None -> "<unnamed>"
    if a.Length <> b.Length then
        $": {labelA} has {a.Length} index records and {labelB} has {b.Length}"
    else
        List.zip a b
        |> List.mapi (fun d (x, y) -> (d + 1, x, y))
        |> List.tryPick (fun (axis, x, y) ->
            if indexRecordsAgree x y then None
            else
                let extents =
                    match tryEvalIntIR x.Extent, tryEvalIntIR y.Extent with
                    | Some p, Some q when p <> q -> $", extents {p} vs {q}"
                    | _ -> ""
                Some (sprintf ": they first differ at index record %d of %d -- %s has '%s' and %s has '%s'%s.%s"
                          axis a.Length labelA (nameOf x) labelB (nameOf y) extents (axisSplitSuffix x y)))
        |> Option.defaultValue ""

/// The rank->=2 co-iteration refusal, with the FIRST disagreeing record of the
/// FIRST disagreeing operand named -- the same "first offender, 1-based"
/// contract `zipHeadClash` follows.
let private multiAxisShapeClash (shape0: IRIndexType list) (rest: IRArrayType list) : string =
    let head =
        "co-iteration over multi-axis arrays requires all operands to have identical index shapes (same records: tags, extents, symmetry)"
    let detail =
        rest
        |> List.mapi (fun i at -> (i + 2, at.IndexTypes))
        |> List.tryPick (fun (pos, ixs) ->
            match indexShapeClashDetail "operand 1" $"operand {pos}" shape0 ixs with
            | "" -> None
            | d -> Some d)
    head + defaultArg detail ""

/// The shared-axis disagreement in a single-record co-iteration, if any --
/// every operand's HEAD record against the FIRST operand's, which is the
/// record the walk will actually use. Reports the first offender (1-based).
///
/// Three obligations, in this order, because that is decreasing severity:
///   1. Packing (`indexPackingCoIterable`) -> BL3999, the band this function's
///      other structural refusals already use. No walk exists at all.
///   2. Extent, LITERAL vs LITERAL only -> BL3016 (`ZipExtentMismatch`). The
///      walk exists and is out of bounds. Checked BEFORE names so that
///      `I8 + I2` reports the memory-safety fact rather than the naming one.
///      A symbolic extent reads `.extents[d]` at runtime and keeps the
///      historical looseness -- the same rule `kernelExtentClash` follows.
///   3. Nominal identity (`indexNamesCoIterable`) -> BL3999. The walk is
///      in-bounds; the operands just are not over the same index space.
///
/// Only the HEAD is compared. A rank-2 operand zipped with a rank-1 one takes
/// this arm too (`minRank <= 1`), and there the shared record is the leading
/// one; the trailing records are the kernel's business, trimmed by its slice
/// rank in buildApplyInfo.
let private zipHeadClash (arrayTypes: IRArrayType list) : TypeError option =
    match arrayTypes with
    | [] | [_] -> None
    | first :: rest ->
        match first.IndexTypes with
        | [] -> None
        | h0 :: _ ->
            rest
            |> List.mapi (fun i at -> (i + 2, at))
            |> List.tryPick (fun (pos, at) ->
                match at.IndexTypes with
                | [] -> None
                | h :: _ ->
                    if not (indexPackingCoIterable h0 h) then
                        Some (Other ($"co-iteration operands must share ONE index space on the co-iterated axis, but operand 1 is over {(ppIndexType h0)} and operand {pos} is over {(ppIndexType h)}. These walk different cell counts with different subscript arithmetic (a packed/compact axis stores one cell per canonical tuple; a plain dense axis stores one per subscript), so there is no shared iteration for the kernel to run over. Decompact the packed operand, or write the traversal over each explicitly."))
                    else
                        match tryEvalIntIR h0.Extent, tryEvalIntIR h.Extent with
                        | Some e0, Some e when e0 <> e -> Some (ZipExtentMismatch (pos, e0, e))
                        | _ ->
                            if not (indexNamesCoIterable h0 h) then
                                // Names, not raw Tags. A provider axis tag is a
                                // provenance IDENTITY ("__icaxis|lat@wx:9f3a1c
                                // 2b4d5e6f70#2"), and printing it verbatim
                                // handed the user a 40-character internal token
                                // where the store says `lat`. `displayTagName`
                                // decodes it through the hook the provider
                                // registers; every other tag (user names,
                                // sentinels) comes back unchanged, so this is
                                // the old text for everything that is not a
                                // provider axis.
                                Some (Other (sprintf
                                        "co-iteration operands are over DIFFERENT index types: operand 1 is indexed by '%s' and operand %d by '%s'. A named index type carries provenance, so two of them do not interoperate merely by having the same extent (%s) -- that is what makes a subscript bounds-safe by construction. Index one of them through the other's space, or drop the annotation on one operand if they really are the same axis (an UNNAMED index co-iterates with either).%s"
                                        (displayTagName (defaultArg h0.Tag "?")) pos
                                        (displayTagName (defaultArg h.Tag "?")) (ppExtentOf h0.Extent)
                                        (axisSplitSuffix h0 h)))
                            else None)

/// Shared iteration records for a zip co-iteration, from the operands' array
/// types. Single-record operands (dense rank-1, packed symmetric) co-iterate
/// the FIRST operand's record, and every other operand's head record must
/// agree with it (`zipHeadClash`) -- shape first, then literal extents.
/// Multi-record operands (dense rank >= 2) span the FULL product of records
/// and require structural agreement + all-plain-dense records (mixed
/// dense/packed multi-axis rejects), with the grouped-ragged shape as the one
/// non-dense exception.
let zipSharedRecords (arrayTypes: IRArrayType list) : Result<IRIndexType list, TypeError> =
    match arrayTypes with
    | [] -> Ok []
    | first :: rest ->
        let shape0 = first.IndexTypes
        let minRank = arrayTypes |> List.map (_.IndexTypes.Length) |> List.min
        if shape0.Length <= 1 || minRank <= 1 then
            // Single-record rule (first array's first record). The head
            // records must still AGREE: this arm used to take shape0.Head
            // unchecked, so `[1.0; x8] + [10.0, 20.0]` walked 8 cells over a
            // 2-cell pool in both lanes, and a SymIdx<2,4> + Idx<4> zip
            // reached g++ as invalid C++.
            match zipHeadClash arrayTypes with
            | Some e when minRank > 0 -> Error e
            | _ -> Ok (if minRank > 0 then [shape0.Head] else [])
        elif not (rest |> List.forall (fun at -> indexShapesAgree at.IndexTypes shape0)) then
            Error (Other (multiAxisShapeClash shape0 rest))
        elif isGroupedRaggedShape shape0 then
            // All operands grouped: the ragged member axis is NOT a product
            // axis, so this is not the plain-dense product rule -- the rows
            // co-iterate positionally, group g of each operand together, and
            // the kernel receives one row per operand. Legal only for
            // same-keys operands (checked by the caller).
            Ok shape0
        elif not (coIterableRecords shape0) then
            Error (Other "co-iteration spans one index record per operand (dense rank-1 or packed symmetric), or a product of plain-dense records; mixed dense/packed multi-axis shapes are not supported")
        else
            Ok shape0

/// halo<Inner, offsets> slot construction -- shared by the expression form
/// (`method_for(halo<..>)`) and the range<> slot form (`range<halo<..>, ..>`).
/// The offsets payload is either
///   - a FLAT int array `[-1, 0, 1]`  -> ONE slot (a 1-D halo), or
///   - an array of per-axis int arrays `[[-1,0,1],[0],[-1,0,1]]` -> k slots
///     over the SAME inner index (arity = sub-array count), the n-D product
///     window written as one halo.
/// Each slot is the inner index SHRUNK to its interior (BndShrink: every
/// declared neighbor of every iterated center is in-bounds, so window reads
/// need no guards) and tagged "__halowin|<d|c>:<innerName>|<o1,o2,..>". The
/// center's start offset (max(0, -min offsets union {0})) is re-derived from the
/// tag at loop-building time (IR mkElement via Types.haloStartOffsetOfTag) --
/// per-slot, which the single shared IRRange offset cannot express for
/// multi-slot ranges.
let haloSlotsOf (env: TypeEnv) (innerTy: TypeExpr) (offsetsExpr: Expr) : TypeResult<IRIndexType list> =
    let inner = lowerIndexType env 0 innerTy
    // One slot from one flat per-axis offset set.
    let slotOfInts (offsets: int list) : TypeResult<IRIndexType> =
        if List.isEmpty offsets then Error (Other "halo<...>: offsets array must be non-empty")
        elif inner.Rank <> 1 then
            Error (Other "halo<...>: the inner index must be rank-1 (n-D = per-axis offset arrays [[..],[..],..] or separate range<halo<..>, ..> slots)")
        else
        let offCsv = offsets |> List.map string |> String.concat ","
        match inner.Extent with
        | IRCompoundMask _ ->
            // Compound inner: ordinals walk the PRESENT cells, so "next"
            // is the next present cell (the hashed-index generalization).
            // The mask cardinality is runtime -- the interior shrink can't
            // fold into the extent here; it rides the tag and is applied
            // at the loop bound (buildLoopNestCodeGen StrictOffset).
            // IxKCompound is KEPT so the cidx materialization and
            // cardinality-bound machinery still engage.
            Ok { inner with
                    Id = env.Builder.FreshId()
                    Extent = inner.Extent
                    Tag = Some $"__halowin|c:|{offCsv}"
                    IxKind = IxKCompound }
        | _ ->
            // Dense inner. Reach includes the implicit center 0: w(0) is
            // always readable even when 0 is not in the declared set
            // (e.g. lag sets [-12,-24]).
            let lo = min 0 (List.min offsets)
            let hi = max 0 (List.max offsets)
            let shrink = int64 (-lo + hi)
            let shrunkExtent =
                match inner.Extent with
                | IRLit (IRLitInt n) -> IRLit (IRLitInt (n - shrink))
                | e -> IRBinOp (IRElementwise, IRSub, e, IRLit (IRLitInt shrink))
            // A `Chunked` alias adopts its inner axis's record (so it IS the
            // axis), which for a store axis carries no name: the halo was
            // declared over the ALIAS, and that is the name the segment-run
            // emitter looks the runs up by (docs/plans/structural/07 §2.3).
            let innerName =
                match innerTy with
                | TyNamed (n, []) when Map.containsKey n env.Segmentations -> n
                | _ ->
                    match inner.Tag with
                    | Some n when not (n.StartsWith("__")) -> n
                    | _ -> ""
            Ok { inner with
                    Id = env.Builder.FreshId()
                    Extent = shrunkExtent
                    Tag = Some $"__halowin|d:{innerName}|{offCsv}"
                    IxKind = IxKPlain }
    match evalStaticValueExpr env offsetsExpr with
    | Error msg -> Error (Other $"halo<...>: offsets must be a compile-time int array ({msg})")
    | Ok sv ->
        let asInt = function StaticEval.SVInt n -> Some (int n) | _ -> None
        match sv with
        | StaticEval.SVInt n -> slotOfInts [int n] |> Result.map List.singleton
        | StaticEval.SVTuple vs when not vs.IsEmpty ->
            let flat = vs |> List.map asInt
            if List.forall Option.isSome flat then
                // Flat form: one axis.
                slotOfInts (flat |> List.map Option.get) |> Result.map List.singleton
            else
                // Nested form: every entry must be a non-empty int array;
                // each becomes one slot over the same inner index.
                let perAxis =
                    vs |> List.map (function
                        | StaticEval.SVTuple xs ->
                            let os = xs |> List.map asInt
                            if List.forall Option.isSome os && not os.IsEmpty
                            then Some (os |> List.map Option.get) else None
                        | _ -> None)
                if List.forall Option.isSome perAxis then
                    // (local sequencer: TypeCheck's sequenceResults is defined
                    // further down the file, out of scope here)
                    perAxis
                    |> List.map (Option.get >> slotOfInts)
                    |> List.fold (fun acc r ->
                        match acc, r with
                        | Ok xs, Ok x -> Ok (xs @ [x])
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                else
                    Error (Other "halo<...>: offsets must be a flat int array [-1,0,1] or an array of per-axis int arrays [[-1,0,1],[0],[-1,0,1]] (no mixing, no empty axes)")
        | _ -> Error (Other "halo<...>: offsets must be a compile-time array of integer literals, e.g. [-1, 0, 1]")


// ---------------------------------------------------------------------------
// Closed-RaggedIdx lens resolution
// ---------------------------------------------------------------------------

/// The two ways a closed `RaggedIdx<lens>` can name its per-row lengths,
/// split the way `resolveSparseKeysSource` splits SparseIdx's keys: a lens
/// whose values the compiler can hold (`RlStatic`), and one that exists only
/// once the program runs (`RlRuntime`).
///
/// Unlike SparseIdx the verdict is NOT stored in the IR. `IRRaggedLookup`'s
/// payload has no consumer in CodeGen*/Interp -- ragged construction bakes
/// its `_lens`/`_offsets` companions from the LITERAL's own nesting -- and
/// putting the resolved values into the index record's `Extent` would change
/// index-type IDENTITY: two separate lens bindings holding equal values would
/// start unifying. So the resolution stays on demand, at the one seam that
/// has something to check a lens against.
type RaggedLensSource =
    | RlStatic of int64 list
    | RlRuntime

/// Resolve the lengths reference of a closed `RaggedIdx<lens>` -- the IRExpr
/// inside its `IRRaggedLookup` extent. `lowerExtentExpr` leaves that as the
/// bare NAME (`IRParam (name, ...)`): an array-valued lens folds to no
/// integer, so both of its constant-folding tiers are skipped.
///
/// Two static sources, and the second is the load-bearing one. A `let static`
/// entry in `StaticValues` is the branch resolveSparseKeysSource's Ok side
/// takes, but a lens is conventionally a plain `let` (every closed-lens site
/// in the corpus spells it that way) and a plain `let` never reaches
/// StaticValues -- so the checked binding's own value is consulted too.
let resolveRaggedLensSource (env: TypeEnv) (lengths: IRExpr) : RaggedLensSource =
    let allInts (xs: int64 option list) =
        if not xs.IsEmpty && List.forall Option.isSome xs
        then Some (xs |> List.map Option.get) else None
    let ofStaticValue (sv: StaticEval.StaticValue) =
        match sv with
        | StaticEval.SVTuple comps ->
            comps |> List.map (function StaticEval.SVInt v -> Some v | _ -> None) |> allInts
        | _ -> None
    // The checked value of a plain `let`. A negative entry resolves rather
    // than falling through to RlRuntime: a negative row length is nonsense,
    // and reporting it as "the lens disagrees with the literal" beats "the
    // lens is not compile-time", which sends the reader after the wrong fix.
    let ofTypedValue (te: TypedExpr) =
        match te.Kind with
        | TExprArrayLit (rows, _) ->
            rows
            |> List.map (fun e ->
                match e.Kind with
                | TExprLit (LitInt v) -> Some v
                | TExprUnaryOp (OpNeg, { Kind = TExprLit (LitInt v) }) -> Some (-v)
                | _ -> None)
            |> allInts
        | _ -> None
    match lengths with
    // A synthesized (`__`-prefixed) or unresolved (`?`) name is not a lens
    // anyone wrote: the inline and opaque ragged forms both land there.
    | IRParam (name, _, _) when name <> "?" && not (name.StartsWith "__") ->
        match Map.tryFind name env.StaticValues |> Option.bind ofStaticValue with
        | Some vs -> RlStatic vs
        | None ->
            match lookupVar name env |> Option.bind (_.TypedValue) |> Option.bind ofTypedValue with
            | Some vs -> RlStatic vs
            | None -> RlRuntime
    | _ -> RlRuntime
