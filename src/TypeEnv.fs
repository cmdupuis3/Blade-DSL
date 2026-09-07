// Type-checking environment: schemes, instantiate/generalize, TypeEnv's
// variable/type-def registries (audit sec 4: Check/TypeEnv.fs).
module Blade.TypeEnv

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

/// Instantiate a type scheme: replace each quantified variable with a fresh
/// inference variable, so each use site gets independent type constraints.
let instantiate (subst: Subst) (scheme: TypeScheme) : IRType =
    if scheme.QuantifiedVars.IsEmpty then
        scheme.Body
    else
        let mapping =
            scheme.QuantifiedVars
            |> List.map (fun v ->
                let fresh = subst.Fresh()
                // Copy arity constraints and rank lower bounds too: without the
                // rank copy, a generalized (static) value's deduced bound
                // resets to unbounded/scalar at each use site. Only the
                // ReadOnly/generalized case reaches here (plain `let` shares one var, never instantiates).
                match fresh with
                | IRTInfer freshId ->
                    subst.CopyArityConstraint(v, freshId)
                    subst.CopyRankLowerBound(v, freshId)
                | _ -> ()
                (v, fresh))
            |> Map.ofList
        let rec replace ty =
            match ty with
            | IRTInfer id ->
                Map.tryFind id mapping |> Option.defaultValue ty
            | IRTTuple ts -> IRTTuple (ts |> List.map replace)
            | IRTArrow (slots, ret, identity) ->
                let replaceSlot = function
                    | SIdx idx -> SIdx idx
                    | SIdxVirt idx -> SIdxVirt idx
                    | SVal t -> SVal (replace t)
                IRTArrow (slots |> List.map replaceSlot, replace ret, identity)
            | IRTComputation t -> IRTComputation (replace t)
            | IRTPoly (t, v) -> IRTPoly (replace t, v)
            | IRTDist (order, elem, axes) -> IRTDist (order, replace elem, axes)
            | IRTLoop lt ->
                IRTLoop { lt with
                            ArrayTypes = lt.ArrayTypes |> List.map replace
                            KernelType = lt.KernelType |> Option.map replace }
            | _ -> ty  // IRTScalar, IRTUnit, IRTNamed, IRTNat (no inference vars to replace)
        replace scheme.Body

// 2. Type Environment

/// Variable assignability levels tracked during type checking.
/// Maps to binding forms: static -> ReadOnly, let -> Assignable, let mut -> MutPassable
type Assignability =
    | ReadOnly      // static: not assignable, generalizable
    | Assignable    // let: assignable in scope, not passable to mut params
    | MutPassable   // let mut: assignable + passable to mut params

/// Variable binding information tracked during type checking.
type VarInfo = {
    VarId: IRId
    Type: IRType
    Identity: ArrayIdentity option
    Assign: Assignability
    /// The TypedExpr this variable was bound to, for <@> resolution.
    TypedValue: TypedExpr option
    /// If Some, this binding is polymorphic (let-generalized).
    /// Variable lookup instantiates fresh type variables from the scheme.
    Scheme: TypeScheme option
}

/// Registered type definition
type TypeDefInfo =
    | TDIAlias of IRType
    | TDIStruct of name: string * typeParams: string list * fields: (string * IRType) list * constraints: Expr list
    | TDIVariant of name: string * typeParams: string list * variants: (string * IRType option) list
    | TDIIndexType of name: string * idx: IRIndexType * body: TypeExpr
    | TDIEnumIdx of name: string * idx: IRIndexType * values: EnumValue list * body: TypeExpr

/// What a mutual-group member aliases: a registered struct (constraints
/// reference its fields as `P.f`) or a scalar type (referenced bare).
type MutualMemberKind =
    | MMStruct of structName: string
    | MMScalar of IRType

/// A `type P1 = T1 and P2 = T2 where ...` group. Members stay transparent
/// aliases; this record carries the joint constraint for binding-site checks.
type MutualGroupInfo = {
    /// First member's name -- doubles as the group's display id.
    GroupId: string
    /// Members in declaration order.
    Members: (string * MutualMemberKind) list
    /// Untyped where-conjuncts, validated at declaration time.
    Constraints: Expr list
}

/// Exported bindings from a type-checked module, for cross-module imports
/// One level of a segmentation of one axis (docs/plans/structural/07 §2.1):
/// a regular chunk grid, or stores tiling the axis in declaration order.
type SegLevel =
    | SegRegular of edge: int64
    /// Stores tiling the axis in declaration order: (label, extent, that
    /// file's own chunk edge if it is chunked). The per-file edge is what
    /// makes the chunk level DEPENDENT on the file level (§2.7): two files
    /// may be chunked differently.
    | SegFiles of (string * int64 * int64 option) list

/// The segmentation registered beside a `type A = Chunked<I, spec>`: the
/// axis it partitions (I's own record -- the alias IS I), I's static extent,
/// and the levels, outermost first. Read by `segments(A)`; nothing else in
/// the type system sees it, which is what keeps `Chunked<I, K>` unifying as
/// `I` (storage is unchanged in this arc).
type Segmentation = {
    Alias: string
    Source: IRIndexType
    Extent: int64
    Levels: SegLevel list
}

/// The run boundaries of the INNERMOST level, flattened over the whole axis:
/// `[b_0 = 0; ...; b_G = N]`. A regular grid over a file level is applied
/// within each file's run, so a file's last chunk may be short.
let segmentationOffsets (s: Segmentation) : int64 list =
    let regular (lo: int64) (hi: int64) (edge: int64) : int64 list =
        // interior boundaries of [lo, hi) at multiples of edge from lo
        [ for b in lo + edge .. edge .. hi - 1L -> b ]
    let rec go (runs: (int64 * int64) list) (levels: SegLevel list) : (int64 * int64) list =
        match levels with
        | [] -> runs
        | SegFiles files :: rest ->
            let runs' =
                runs |> List.collect (fun (lo, hi) ->
                    let mutable at = lo
                    [ for (_, ext, edge) in files do
                        let flo, fhi = at, min hi (at + ext)
                        at <- at + ext
                        match edge with
                        | Some k -> yield! List.pairwise (flo :: regular flo fhi k @ [ fhi ])
                        | None -> yield (flo, fhi) ])
            go runs' rest
        | SegRegular edge :: rest ->
            let runs' =
                runs |> List.collect (fun (lo, hi) ->
                    let cuts = lo :: regular lo hi edge @ [ hi ]
                    List.pairwise cuts)
            go runs' rest
    let runs = go [ (0L, s.Extent) ] s.Levels
    match runs with
    | [] -> [ 0L; s.Extent ]
    | _ -> (runs |> List.map fst) @ [ s.Extent ]

/// The labels of the OUTERMOST level when it is a file level: the
/// `EnumIdx` states of a file-segmented axis (decision D3).
let segmentationLabels (s: Segmentation) : string list option =
    match s.Levels with
    | SegFiles files :: _ -> Some (files |> List.map (fun (l, _, _) -> l))
    | _ -> None

/// The FILE-level run boundaries `[0; ..; N]` of a file-segmented axis (the
/// outer grouping of §2.7), or None when the axis has no file level.
let segmentationFileOffsets (s: Segmentation) : int64 list option =
    match s.Levels with
    | SegFiles files :: _ ->
        let cuts = files |> List.scan (fun at (_, ext, _) -> at + ext) 0L
        Some cuts
    | _ -> None

type TypeModuleExport = {
    Variables: Map<string, VarInfo>
    TypeDefs: Map<string, TypeDefInfo>
    VariantTags: Map<string, string * IRType option>
    Units: Map<string, UnitSig>
    /// Static function ASTs from this module, imported alongside Variables so
    /// an importing module's eta-reduced DepIdx can inline a static function
    /// defined here (needs the body; TypeEnv-side StaticFunctions is the consumer).
    StaticFunctions: Map<string, FunctionDecl>
    /// Folded `let static` values, bare names only (see checkProgram's export
    /// builder). Mirrors StaticFunctions: importedStaticSeed /
    /// rewriteImportedStaticRefs seed these under "alias.name" (qualified) or
    /// "name" (selective) ahead of StaticEval.resolveStatics.
    StaticValues: Map<string, StaticEval.StaticValue>
    /// This module's defaults-carrying callables (bare names), snapshotted
    /// from the shared `FuncDefaults` table when the module's check ends --
    /// BEFORE a later module can overwrite the bare-name entry with its own
    /// `f`. A qualified import re-registers them as `alias.name` and a
    /// selective one as `name`, so a call resolves the defaults of the
    /// module it actually named.
    Defaults: Map<string, (string * TypeExpr option * Expr option) list>
    /// The matching FuncDefaultCaptures entries (see TypeEnv.FuncDefaultCaptures).
    DefaultCaptures: Map<string, Map<string, IRId>>
}

/// Type checking environment
type TypeEnv = {
    Variables: Map<string, VarInfo>
    TypeDefs: Map<string, TypeDefInfo>
    /// Variant tag -> (parentTypeName, payloadType option)
    VariantTags: Map<string, string * IRType option>
    Subst: Subst
    Builder: IRBuilder
    OuterScope: Map<string, VarInfo>
    InPolyContext: bool
    /// True while a LAMBDA body is being type-inferred. Unit checks that fire
    /// at scalar position read it to DEFER premature rejections: an unresolved
    /// kernel param contributes "no units" to the first-pass walk, so an
    /// annotation computed against it is provisional until buildApplyInfo
    /// unifies the params and reruns kernelBodyUnits (the authoritative pass).
    /// NOT set for named-function declaration bodies: their unannotated params
    /// are dimensionless by contract, so decl-time strictness is correct there.
    InLambdaBody: bool
    /// True while ANY callable body is being type-inferred -- a lambda body, a
    /// named-function declaration body, or an impl-method body. Those three are
    /// exactly the `enterCallableBody` sites, and exactly the bodies Lowering
    /// runs `forceCallableBody` (S2 + S4) over, so this flag is the
    /// checker-side name for "a `let` bound here WILL be materialized".
    /// Distinct from `InLambdaBody`, which is deliberately unset for
    /// named-function bodies (their unannotated params are dimensionless by
    /// contract).
    ///
    /// Read together with `OuterScope`: since `enterCallableBody` snapshots
    /// everything visible at the body boundary, a name in `Variables` but NOT
    /// in `OuterScope` is bound INSIDE the innermost body -- which is what
    /// `bodyLocalBinding` tests.
    InCallableBody: bool
    /// Names of the `mut` parameters of the function whose body is being
    /// checked (all array-typed -- MutParamNotArray rejects any other kind
    /// first). Rebinding one WHOLE (`a = <array expr>`) cannot reach the
    /// caller, because the C++ ABI passes the `Array<>` wrapper by value; only
    /// element writes travel, through the shared data pointer. A `let mut`
    /// BINDING, by contrast, may be rebound whole -- that is real rebind
    /// semantics with its own corpus (memfree/015, 016) -- and both bind
    /// `MutPassable`, so assignability alone cannot tell them apart. This set
    /// is how `assignTargetError` does.
    MutArrayParams: Set<string>
    CurrentCommGroups: int list list
    /// Interface name -> InterfaceDecl
    Interfaces: Map<string, InterfaceDecl>
    /// (typeName, methodName) -> (mangledFuncVarId, funcType)
    ImplMethods: Map<string * string, IRId * IRType>
    /// Unit name -> canonical UnitSig
    Units: Map<string, UnitSig>
    /// The MAGNITUDE an enclosing annotation says the value being checked is
    /// supposed to have, threaded down from the annotated-let seam. Read by
    /// the scalar +/- conversion seam so each operand converts straight into
    /// the unit that was asked for -- ONE factor per operand -- instead of
    /// joining at the left operand and correcting the sum afterwards, which
    /// rounds twice and computes in a magnitude nobody chose. Applied only
    /// when it agrees dimensionally with what the operands actually produce,
    /// so it can never turn a real unit error into a conversion.
    UnitTarget: UnitSig option
    /// Context stack for error reporting, e.g. ["in function 'foo'"]
    Context: string list
    /// Exports from modules type-checked earlier in this compilation
    ModuleExports: Map<string, TypeModuleExport>
    /// Static function ASTs, populated in checkModule's pre-pass. Lets
    /// lowerIndexTypeList inline eta-reduced DepIdx bodies (`DepIdx<O, f>`
    /// desugars to `lambda(i) -> Idx<f(i)>`; substitution needs f's body).
    StaticFunctions: Map<string, FunctionDecl>
    /// `let static x = ...` bindings, resolved once via StaticEval in
    /// checkModule's pre-pass, so compile-time-known scalars (e.g. a
    /// `replicate` count) resolve ahead of lowering's own resolveStatics.
    /// Best-effort: non-evaluable entries are simply absent.
    StaticValues: Map<string, StaticEval.StaticValue>
    /// Type aliases' SURFACE bodies, by name. `TypeDefs` stores an alias as a
    /// LOWERED `TDIAlias`, which loses `min=`/`max=`; the element-bound guard
    /// synthesis needs the bound EXPRESSIONS, so it resolves alias chains
    /// through this map instead. Populated by registerTypeDecl per `type X = ...`.
    SurfaceAliases: Map<string, TypeExpr>
    /// Segmentations registered by `type A = Chunked<I, spec>`, by alias
    /// (docs/plans/structural/07). Read by `segments(A)`.
    Segmentations: Map<string, Segmentation>
    /// The OUTER slot ids of `group_by(_, segments(A))` results, by the
    /// alias they came from: how `ungroup(G)` finds the axis to restore
    /// when G's outer slot carries one of these ids (inferGroupBy mints the
    /// slot fresh, so the id is the only handle that survives).
    SegmentedOuters: System.Collections.Generic.Dictionary<IRId, string>
    /// The `Chunked` aliases named by a value binding's ANNOTATION, per slot
    /// (None for a slot that is not a Chunked alias), by binding name: what
    /// `segments(a)` over a VALUE reads. An array whose slots are all
    /// `Chunked` already says its tiling; the aliases need not be repeated.
    SlotAliases: System.Collections.Generic.Dictionary<string, string option list>
    /// Names declared `static struct` (the static-eligibility fence).
    /// Registration validates fields against StaticValue shapes and records
    /// the name on success, so later static structs can nest earlier ones.
    /// A name set, not a TDIStruct flag: only decl-time checks and the
    /// constrained-index layers consult it.
    StaticStructs: Set<string>
    /// Every declared struct's static-evaluator record (fields, raw field
    /// decls with their bounds, declared and full conjunct lists, the static
    /// marker), registered beside TDIStruct so the index fence and the
    /// enumeration route (StructIdxFence / StructIdxSpec) can be asked from
    /// the checker and the lowerer through `staticEnvOf`.
    StructStatics: Map<string, StaticEval.StructStaticInfo>
    /// Non-fatal diagnostics accumulated during type-checking. A mutable
    /// ResizeArray so `{ env with ... }` updates share one collector across
    /// scopes. Surfaced only via `typeCheck`'s Ok return; skipped on the error path.
    Warnings: ResizeArray<string>
    /// Dist value provenance: varId -> source set (underlying array names for
    /// module-level dists, `func.param` tokens for Dist params). Consumed by
    /// Dist +/- dispatch and where-clause discharge. Shared by reference, like Warnings.
    Provenance: System.Collections.Generic.Dictionary<IRId, Set<string>>
    /// Custom where-clause conjuncts per function: funcName -> (paramNames,
    /// conjuncts). Populated by checkFunctionDecl; consulted at call sites for discharge.
    FuncConstraints: System.Collections.Generic.Dictionary<string, string list * (string * string list) list>
    /// Parameter metadata for callables with DEFAULT parameter values:
    /// callee name -> (paramName, surface type annotation, surface default)
    /// per param, in declaration order. Populated by checkFunctionDecl and by
    /// let bindings whose value is a defaults-carrying lambda; consulted by
    /// the surface call-site desugar (omitted trailing args re-type the
    /// default at the call site). Name-keyed like FuncConstraints, and shares
    /// its known shadowing weakness. Shared by reference.
    FuncDefaults: System.Collections.Generic.Dictionary<string, (string * TypeExpr option * Expr option) list>
    /// The BINDING IDENTITY of every free name a default expression reads
    /// from its declaration scope: callee name -> (free name -> VarId at the
    /// declaration). A default is spliced into the CALL SITE as surface
    /// syntax and re-inferred there, so a name it reads resolves in the
    /// caller's scope -- `function f(x = k)` called from `function g(k) =
    /// f()` used to read g's parameter. The splice compares each free name's
    /// call-site binding against the identity recorded here and refuses on
    /// disagreement (BL3012). Keyed exactly like FuncDefaults, including the
    /// `alias.name` entries a qualified import registers. Shared by reference.
    FuncDefaultCaptures: System.Collections.Generic.Dictionary<string, Map<string, IRId>>
    /// Mutually constrained alias groups: groupId -> group info.
    MutualGroups: Map<string, MutualGroupInfo>
    /// Member alias name -> owning groupId, for annotation scanning.
    MutualMembers: Map<string, string>
    /// Functions whose return type introduces a mutual group (`-> (P1, P2)`):
    /// funcName -> groupId. Joint check emitted at return; callers don't re-check. Shared by reference.
    MutualReturnFuncs: System.Collections.Generic.Dictionary<string, string>
    /// Callee name -> the 0-based positions of its `mut` parameters. A `mut`
    /// parameter grants the callee WRITE access to the caller's array, so the
    /// caller has to have write access to grant: formalism 2.7 lists only
    /// `let mut x = e` as passable to a `mut` param. The check needs the
    /// callee's DECLARATION at the call site, and the function type carries
    /// only param types, so the positions ride here. Name-keyed like
    /// FuncConstraints/FuncDefaults, and shares their known shadowing
    /// weakness. Shared by reference.
    MutParamPositions: System.Collections.Generic.Dictionary<string, int list>
    /// Callee name -> the co-iterations its body performs over its own
    /// PARAMETERS: each entry is (parameter positions walked, literal leading
    /// extents of that co-iteration's other operands), all of which must agree.
    ///
    /// The agreement obligation a zip carries is discharged AT the zip
    /// (`TypeLower.zipHeadClash`, BL3016) only when both extents are literals
    /// there. Through abstract `T^1` parameters they are not -- the body sees
    /// two rank-1 arrays with nothing to compare -- so the obligation has to
    /// travel to the call, which is where the extents become concrete. This
    /// records it; `CoIterArgExtentMismatch` on the call-site ladder discharges
    /// it. Without it, a body that zips its two parameters accepted a 6-cell
    /// and a 3-cell argument and read three doubles past the shorter one, in
    /// both lanes, with no diagnostic.
    ///
    /// Two populating sources, both in checkFunctionDecl: a zip DIRECTLY over
    /// parameters, and a call to an already-obligated callee passing this
    /// body's own parameters into its obligated positions (so the obligation
    /// travels up a forwarding chain). Declaration order makes one forward pass
    /// enough -- a body sees only names bound before it, and mutual recursion
    /// is rejected (BL2001).
    ///
    /// Name-keyed like MutParamPositions, and shares its shadowing weakness.
    /// Shared by reference.
    FuncCoIterObligations: System.Collections.Generic.Dictionary<string, (int list * int64 list) list>
    /// Callee name -> how its return's UNIT is built from its arguments':
    /// `(exponents, residual)` means the result measures
    /// `residual * PROD_i (unit of argument i) ^ exponents[i]`.
    ///
    /// A generic signature's return is DEDUCED, and a `T^1 -> T^0` pair shares
    /// ONE inference variable between the parameter's element and the return --
    /// but direct application never unifies parameters against arguments (that
    /// variable has to stay open for per-call-site monomorphization), so the
    /// substitution never learns the caller's unit and every unit rule read a
    /// bare variable. Propagating the ARGUMENT's unit unchanged is not the fix:
    /// `mean` preserves it, `variance` SQUARES it. What transfers is the
    /// exponent the body derives, which is what this records.
    ///
    /// Computed once per declaration by probing the typed body with a synthetic
    /// base dimension per generic parameter (checkFunctionDecl), consumed by
    /// `unitStampedReturn` at the call site. Absent = no claim, which is the
    /// pre-existing silence; a body the unit walk cannot read is never entered.
    /// Name-keyed like MutParamPositions, and shares its shadowing weakness.
    /// Shared by reference.
    FuncUnitTransform: System.Collections.Generic.Dictionary<string, int list * UnitSig>
    /// Named functions' `where comm(...)` groups (by param index): funcName ->
    /// int list list. Populated by checkFunctionDecl; must survive
    /// eta-expansion (etaExpandFunctionKernel) onto the loop-kernel wrapper, or
    /// `object_for(f)` on a `where comm` function silently produces DENSE output. Shared by reference.
    FuncCommGroups: System.Collections.Generic.Dictionary<string, int list list>
    /// Named functions' `where anticomm(...)` groups (by param index):
    /// funcName -> int list list. Signed twin of FuncCommGroups, same seam: if
    /// not re-attached to the synthesized kernel, `object_for(f) <@> (A, A)`
    /// silently falls back to dense. Shared by reference.
    FuncAntisymGroups: System.Collections.Generic.Dictionary<string, int list list>
    /// Stage-3 deduction, early tier: per-function parameter NAMES plus
    /// ADJACENT-pair swap parities (n params -> n-1 entries), from
    /// checkFunctionDecl for fixed-arity functions. buildApplyInfo consults it
    /// so an eta-expanded `object_for(f)` gets f's deduced symmetry and real
    /// param names, without reanalyzing. Shared by reference.
    FuncDeducedPairs: System.Collections.Generic.Dictionary<string, string list * Blade.Deduce.Parity list>
    /// Stage-3 interprocedural sign-linearity: per-function, per-param sign
    /// parities in decl order (checkFunctionDecl, fixed-arity only). SOdd
    /// means `f(.., -x, ..) = -f(..)`; the call rule uses it to propagate
    /// PNeg on e.g. `mymean(x - y)`, where callee-and-args-invariant alone
    /// gives only PBottom. Keyed by BINDER ID (`FuncId`), not name (a
    /// shadowing local can't borrow the law); DECL ORDER resolution (self/forward -> SUnknown) needs no fixpoint. Shared by reference.
    FuncSignParities: System.Collections.Generic.Dictionary<IRId, Blade.Deduce.SignParity list>
    /// Binder IDs of every NAMED function declaration (`function f`, static or
    /// not, including the static pre-pass registration and any imported
    /// module's functions -- the set is shared by reference across the whole
    /// program, like Warnings).
    ///
    /// The one consumer is `buildCaptures`: a named function lowers to an
    /// IRCallable emitted at C++ GLOBAL scope, so a lambda body that calls one
    /// never needs it on the capture list -- the body already emits the
    /// callable's own (possibly monomorphized) name. Left on the list it
    /// becomes a dead parameter that the call site still forwards by SOURCE
    /// name, which is not a C++ declaration whenever the callee was
    /// monomorphized: `'scale' was not declared in this scope`.
    ///
    /// Keyed by BINDER ID, not name, so a local that SHADOWS a function name
    /// still captures (its VarId is a different binder).
    DeclaredFuncIds: System.Collections.Generic.HashSet<IRId>
    /// Function BINDER ID -> the conservative effect summary of its typed
    /// body (Blade.Effects.EffectSummary; TypeCheckSupport.effectsOfBody).
    /// Populated by checkFunctionDecl after the body is checked, so a body
    /// sees summaries for every function declared before it (declaration
    /// order; mutual recursion is rejected, BL2001) and its own recursive
    /// calls are assumed pure while its summary is being computed. Read by
    /// inferRecArray's freeze-recognition callee test. Keyed by id like
    /// DeclaredFuncIds: a shadowing local never borrows a summary. Shared
    /// by reference.
    FuncEffects: System.Collections.Generic.Dictionary<IRId, Blade.Effects.EffectSummary>
    /// CERTIFIED half of the typed equivariance lattice (FuncRepSpec below is
    /// the speculative half): per-function rep signatures for functions
    /// carrying an `__ml_equiv` conjunct (a source `where ml.equiv(G)` pin, or
    /// an elaborator stamp -- read uniformly). Recorded at checkFunctionDecl
    /// from ZONKED types; keyed by BINDER ID (shadowing reason, as
    /// FuncSignParities). DeduceRep trusts this as an AXIOM -- pins aren't validated at typecheck. Shared by reference.
    FuncRepSigs: System.Collections.Generic.Dictionary<IRId, Blade.DeduceRep.RepSigT>
    /// SPECULATIVE half: summaries DEDUCED this pass, their dependency
    /// closures, and decl order, per candidate group. Analysis only -- never
    /// exported; only source-written pins license checking. Single-pass,
    /// no fixpoint: a forward/self call resolves to nothing (declines silently, correctly). Shared by reference.
    FuncRepSpec: Blade.DeduceRep.RepSpecTable
    /// Stage-3 late tier: PACK symmetry for arity-polymorphic (Poly) kernels
    /// -- funcName -> (packParamName, parity). PInv means invariant under
    /// every permutation at every arity, established by the AC-fold template
    /// (Deduce.deducePackFold) or compositionally (Deduce.packParityOf,
    /// resolving callees via this table in decl order). Packs never claim
    /// PNeg (no signed exchange law) -- fuels suggestions only, never errors. Shared by reference.
    PackDeducedComm: System.Collections.Generic.Dictionary<string, string * Blade.Deduce.Parity>
    /// Named functions' `where` parallel strategies (`omp`/`cuda`/`mpi`) with
    /// param NAMES: funcName -> (paramNames, strategies). Consulted so the
    /// clause survives onto a synthesized loop-kernel wrapper -- without it,
    /// `object_for(f)` on a `where omp(...)` function silently emitted a
    /// SERIAL nest (`Parallel = []` reached lowering). Names matter because
    /// `extractParallelism` resolves `omp(a: n)` by NAME, but wrapper params are renamed (`__k<uid>_<i>`); surfacing remaps by position. Shared by reference.
    FuncParallel: System.Collections.Generic.Dictionary<string, string list * ParallelStrategy list>
    /// Named 2-param functions whose BODY is a bare builtin commutative +
    /// associative op over the two params (`a + b`, `a * b`, `a && b`,
    /// `a || b`, either order): funcName -> true. Populated by
    /// checkFunctionDecl; consulted only by the fold-kernel omp licence check
    /// (`reduce(xs, f)` where `f` carries `where omp`) -- a side channel since
    /// BODY lives nowhere else in TypeEnv. Codegen re-derives the same predicate from IRCallable (CodeGen.foldKernelBuiltinOp); the two must agree.
    FuncFoldBuiltin: System.Collections.Generic.Dictionary<string, bool>
    /// WIDTH SCHEMA side channel (docs/plan-tuples-vs-arg-packs.md 6c, Design
    /// C): parameters DECLARED `Tuple<N>`, keyed by the parameter's binder
    /// VarId -> N. A parameter list is a width schema over the pack's flat leaf
    /// sequence -- unannotated = 1, `Tuple<k>` = k -- and the matcher must read
    /// the WRITTEN annotation, never the inferred type (ruling 1: tuple-ness is
    /// always written). The lowered type `IRTTuple [v1..vk]` cannot be trusted
    /// for this: an unannotated param unifies INTO a tuple as soon as the pack
    /// binds it, so reading widths off the resolved type would make pack widths
    /// inference-dependent -- the exact cliff 5.1 rules out. Populated at
    /// inferLambda / checkFunctionDecl from `TyTupleWidth`; read by
    /// buildApplyInfo's schema matcher and by the direct-call arg pairing.
    /// Shared by reference.
    DeclaredTupleWidths: System.Collections.Generic.Dictionary<IRId, int>
    /// REDUCTION-JOIN leg lists (docs/plan-reduction-joins.md, Form 2): the
    /// SURFACE elements of an array literal bound to a name,
    /// `let ps = [prodsum(a, b), reduce(c, (+))]`. `reduce(ps, (<&!>))` reads
    /// them back and joins the legs into one traversal; the elements never
    /// escape as values, so the surface list is what the join needs and the
    /// TYPED literal (four independent scalars) is not.
    ///
    /// Name-keyed, with FuncDefaults' known shadowing weakness and the same
    /// justification: it is a SURFACE side channel, and the join re-validates
    /// what it finds against the resolved binding (an array literal of the
    /// same width) before using it. Shared by reference.
    JoinLegLists: System.Collections.Generic.Dictionary<string, Expr list>
}

let emptyEnv () = {
    Variables = Map.empty
    TypeDefs = Map.empty
    Segmentations = Map.empty
    SegmentedOuters = System.Collections.Generic.Dictionary<IRId, string>()
    SlotAliases = System.Collections.Generic.Dictionary<string, string option list>()
    VariantTags = Map.empty
    Subst = Subst()
    Builder = IRBuilder()
    OuterScope = Map.empty
    InPolyContext = false
    InLambdaBody = false
    InCallableBody = false
    MutArrayParams = Set.empty
    CurrentCommGroups = []
    Interfaces = Map.empty
    ImplMethods = Map.empty
    Units = Map.empty
    UnitTarget = None
    Context = []
    ModuleExports = Map.empty
    StaticFunctions = Map.empty
    StaticValues = Map.empty
    SurfaceAliases = Map.empty
    StaticStructs = Set.empty
    StructStatics = Map.empty
    Warnings = ResizeArray<string>()
    Provenance = System.Collections.Generic.Dictionary<IRId, Set<string>>()
    FuncConstraints = System.Collections.Generic.Dictionary<string, string list * (string * string list) list>()
    FuncDefaults = System.Collections.Generic.Dictionary<string, (string * TypeExpr option * Expr option) list>()
    FuncDefaultCaptures = System.Collections.Generic.Dictionary<string, Map<string, IRId>>()
    MutualGroups = Map.empty
    MutualMembers = Map.empty
    MutualReturnFuncs = System.Collections.Generic.Dictionary<string, string>()
    MutParamPositions = System.Collections.Generic.Dictionary<string, int list>()
    FuncCoIterObligations = System.Collections.Generic.Dictionary<string, (int list * int64 list) list>()
    FuncUnitTransform = System.Collections.Generic.Dictionary<string, int list * UnitSig>()
    FuncCommGroups = System.Collections.Generic.Dictionary<string, int list list>()
    FuncAntisymGroups = System.Collections.Generic.Dictionary<string, int list list>()
    FuncDeducedPairs = System.Collections.Generic.Dictionary<string, string list * Blade.Deduce.Parity list>()
    FuncSignParities = System.Collections.Generic.Dictionary<IRId, Blade.Deduce.SignParity list>()
    DeclaredFuncIds = System.Collections.Generic.HashSet<IRId>()
    FuncEffects = System.Collections.Generic.Dictionary<IRId, Blade.Effects.EffectSummary>()
    FuncRepSigs = System.Collections.Generic.Dictionary<IRId, Blade.DeduceRep.RepSigT>()
    FuncRepSpec = Blade.DeduceRep.RepSpecTable()
    PackDeducedComm = System.Collections.Generic.Dictionary<string, string * Blade.Deduce.Parity>()
    FuncParallel = System.Collections.Generic.Dictionary<string, string list * ParallelStrategy list>()
    FuncFoldBuiltin = System.Collections.Generic.Dictionary<string, bool>()
    DeclaredTupleWidths = System.Collections.Generic.Dictionary<IRId, int>()
    JoinLegLists = System.Collections.Generic.Dictionary<string, Expr list>()
}

/// Structured twin of `TypeEnv.Warnings`: every warning as a coded, spanned
/// `Diagnostic`. One of an AsyncLocal side-channel family (also
/// TypeCheck.PinSuggestions / IdePartial / ML.Equiv.CertSuggestions) that
/// surfaces a fact without widening `typeCheck`'s Ok-only `string list`
/// return (Repl.fs and three test files consume it by shape); this channel
/// drains on both the Ok and error arms, unlike that return type. Lives here
/// (not TypeCheck) because its writer `emitWarning` is TypeEnv-owned and
/// TypeEnv (compile index 156) cannot reference TypeCheck (158); re-exported as `TypeCheck.WarningLog`.
module WarningLog =
    let private slot = new System.Threading.AsyncLocal<Blade.Diagnostics.Diagnostic list>()
    let reset () = slot.Value <- []
    let add (d: Blade.Diagnostics.Diagnostic) = slot.Value <- d :: slot.Value
    let get () : Blade.Diagnostics.Diagnostic list =
        match box slot.Value with
        | null -> []
        | _ -> List.rev slot.Value

/// What the checker DEDUCED vs. what the source ANNOTATED -- an editor
/// otherwise can't tell a rank the user WROTE from one the checker PROVED
/// (both render as the same type). Recording only, at deduction sites;
/// drained by `ide check --json` on both arms into a top-level `deduced[]`
/// array. Hosted in TypeEnv because Zonk (compile index 157) also closes
/// deduced ranks and cannot reference TypeCheck (158). IDE-only:
/// ppType/abstractRenderer has no provenance hook into the REPL renderer.
type DeducedFact =
    /// A parameter whose array RANK came from the body-only rank lower bound
    /// rather than an annotation. `index` is the parameter position.
    | DeducedRank of owner: string * param: string * index: int * rank: int
    /// An ADJACENT-PAIR swap parity the deduction proved, with nothing declared
    /// for that pair. `isAnti` distinguishes antisymm from comm.
    | DeducedPairSym of owner: string * left: string * right: string * index: int * isAnti: bool
    /// The late tier: an arity-polymorphic pack proved invariant under every
    /// permutation at every arity.
    | DeducedPackComm of owner: string * pack: string

module DeducedFacts =
    let private slot = new System.Threading.AsyncLocal<(DeducedFact * Span) list>()
    let reset () = slot.Value <- []
    let add (f: DeducedFact) (span: Span) = slot.Value <- (f, span) :: slot.Value
    let get () : (DeducedFact * Span) list =
        match box slot.Value with
        | null -> []
        | _ -> List.rev slot.Value |> List.distinct

    /// THE ZONK STITCH: hook for the zonk rank auto-close in
    /// `Zonk.zonkType`'s `IRTInfer n` arm (where it builds a rank-k array
    /// from a lower bound instead of defaulting to Float64):
    ///
    ///     Blade.TypeEnv.DeducedFacts.recordZonkClosedRank n k
    ///
    /// Own function because `zonkType` has no owner/param name, index, or
    /// span -- only a type variable. Sentinel placeholders live here once:
    /// owner `"<zonk>"`, param renders as the variable itself, index -1.
    /// Safe because `deduced[]` consumers key on `kind`, not owner/name.
    let recordZonkClosedRank (varId: IRId) (rank: int) =
        add (DeducedRank ("<zonk>", $"?{varId}", -1, rank)) noSpan

/// Append a non-fatal diagnostic to BOTH warning channels: the plain
/// string list (`typeCheck`'s Ok payload, kept exact for Repl.fs and the
/// provider tests) and the structured WarningLog (BLxxxx code + span,
/// survives the ERROR path). The collector is shared by reference across all
/// functional env updates, so call sites thread nothing through. Pass the
/// tightest span in scope; `noSpan` renders header-only.
let emitWarning (env: TypeEnv) (code: string) (span: Span) (msg: string) : unit =
    env.Warnings.Add(msg)
    WarningLog.add
        (Blade.Diagnostics.mkWarning code (Blade.Diagnostics.Codes.phaseOfCode code) span msg)

/// Push a context frame onto the environment
let pushContext (ctx: string) (env: TypeEnv) : TypeEnv =
    { env with Context = ctx :: env.Context }

/// Span of the statement currently being type-checked, per async flow (audit
/// sec 3.4; expression granularity is rewrite work). inferBlock stamps it per
/// StmtSpanned; locateError prefers it over the caller's declaration span, so
/// an error points at the failing STATEMENT (inferBlock stops at the first
/// error, so the last stamp is always the failing one). Reset at every
/// checkDecl/typeCheck/checkModule entry: AsyncLocal storage outlives one
/// compilation in a long-lived process (`blade ide check`), so a stale span would leak.
let private currentStmtSpanStorage = System.Threading.AsyncLocal<Span>()

/// Expression-level span, stamped by inferExpr on entry to every node.
/// Finer than the statement span; since inference short-circuits on first
/// error, the last stamp lies on the path to the failing expression. Cleared
/// on each new statement so a previous leaf can't win.
let private currentExprSpanStorage = System.Threading.AsyncLocal<Span>()

let setCurrentExprSpan (s: Span) = currentExprSpanStorage.Value <- s

let currentExprSpan () : Span =
    match box currentExprSpanStorage.Value with
    | null -> noSpan
    | _ -> currentExprSpanStorage.Value

let setCurrentStmtSpan (s: Span) =
    currentStmtSpanStorage.Value <- s
    currentExprSpanStorage.Value <- noSpan
let resetCurrentStmtSpan () =
    currentStmtSpanStorage.Value <- noSpan
    currentExprSpanStorage.Value <- noSpan

let currentStmtSpan () : Span =
    match box currentStmtSpanStorage.Value with
    | null -> noSpan
    | _ -> currentStmtSpanStorage.Value

/// Wrap a TypeError with span and context into a CompileError. Precision
/// order: active expression span > active statement span > the caller's span.
let locateError (span: Span) (env: TypeEnv) (err: TypeError) : CompileError =
    let exprSpan = currentExprSpan ()
    let stmtSpan = currentStmtSpan ()
    let span =
        if exprSpan.StartLine > 0 then exprSpan
        elif stmtSpan.StartLine > 0 then stmtSpan
        else span
    { Error = err; Span = span; Context = env.Context; Code = None }

/// Stands in for a declaration name when a unit-expression error comes from a
/// TYPE ANNOTATION rather than a `Unit` declaration. The annotation consumers
/// in TypeCheck.fs pass this; formatTypeError words the message around it.
let unitAnnoContext = "<type annotation>"

/// The extra line a TYPE MISMATCH earns when its two sides RENDER IDENTICALLY.
///
/// `ppIndexType` prints an index record from its extent and symmetry and reads
/// no Tag at all, so `expected Array<Float64 like Idx<5>>, got Array<Float64
/// like Idx<5>>` is exactly what a user sees when two DIFFERENT provider axis
/// identities meet -- two checkouts whose `lat` diverged, or two repos' `lat`.
/// The message is then not merely unhelpful, it reads as a compiler bug.
///
/// Fixed HERE and not in the printer on purpose: many corpus categories pin
/// `Idx<n>` in error text, and teaching the global type printer about tags
/// would rewrite all of them. This is one appended line, so every
/// ERROR-CONTAINS pin on the sentence above it keeps matching.
///
/// Scoped to PROVIDER tags (`isProviderAxisTag`) for the same containment
/// reason: `__`-prefixed KIND sentinels also vanish from the render, and a note
/// reading "'__raggedidx' vs '__group_outer'" would be noise in categories that
/// have nothing to do with providers.
let private indexIdentityNote (exp: IRType) (act: IRType) : string =
    let recsOf (t: IRType) =
        match t with
        | ArrayElem at -> at.IndexTypes
        | _ -> []
    let a, b = recsOf exp, recsOf act
    if a.Length <> b.Length || a.IsEmpty then ""
    else
        List.zip a b
        |> List.tryPick (fun (x, y) ->
            match x.Tag, y.Tag with
            | Some tx, Some ty when tx <> ty && (isProviderAxisTag tx || isProviderAxisTag ty) ->
                let clause =
                    match providerSplitClause tx ty with
                    | Some c -> " " + c
                    | None -> ""
                Some $"\nnote: the index types differ by identity: '{(displayTagName tx)}' vs '{(displayTagName ty)}'{clause}"
            | _ -> None)
        |> Option.defaultValue ""

/// Format a TypeError as a human-readable string
let formatTypeError (err: TypeError) : string =
    match err with
    | UnboundVariable name ->
        // The steer for imperative-loop refugees. `while`/`do` are not
        // keywords (deliberately: programs may bind them), so a Fortran/C
        // programmer's first `while cond { ... }` dies HERE as a bare
        // unbound-variable error -- the language's whole thesis, with no
        // pointer to it. Same rationale as the removed-`for` BL1003 steer,
        // fired at the one place we know the name is genuinely unbound, so
        // a real variable named `while` never trips it.
        match name with
        | "while" | "do" ->
            $"Unbound variable: {name}. Blade has no imperative loops -- iteration is declarative. A converge/accumulate loop is a recursive array (`let rec q: Array<T like Step> = match q with | zero -> zero | prefix :: n -> prefix :: <step>`), and iterate-until-converged is that array's inductive arm carrying a `while` guard over a BUDGET extent (`| prefix :: n while <cond> -> prefix :: <step>` -- frozen once the guard goes false, BL8010 if it never does). A fold is `reduce(...)`, and a parallel map is `method_for(range<...>) <@> lambda(...)` or plain array arithmetic. See formalism 7.5."
        | _ -> $"Unbound variable: {name}"
    | DuplicateFunctionDecl (name, firstSite) ->
        $"duplicate declaration of function '{name}': this scope already declares it at {firstSite}. A function name may be declared only once per scope -- without this refusal the later declaration silently shadows the earlier one, and calls matching the first signature fail blaming the call site. Rename one of the declarations. (Dispatching one name across several signatures -- function clauses -- is a planned feature, not yet supported.)"
    | TypeMismatch (exp, act) ->
        let rendered = $"Type mismatch: expected {ppIRType exp}, got {ppIRType act}"
        if ppIRType exp = ppIRType act then rendered + indexIdentityNote exp act else rendered
    | ArityMismatch (exp, act) -> $"Arity mismatch: expected {exp} args, got {act}"
    | KernelPackArity msg -> msg
    | ArgRankMismatch (pos, expRank, actRank, expTy, actTy) ->
        let describe rank ty =
            if rank = 0 then $"a scalar ({ty})"
            else $"a rank-{rank} array ({ty})"
        $"argument {pos}: rank mismatch: the parameter expects {(describe expRank expTy)} but the argument is {(describe actRank actTy)}. A call site neither broadcasts nor reduces rank -- pass a value of the declared rank, or change the parameter's declared type."
    | ArgTypeMismatch (pos, func, expTy, actTy) ->
        $"argument {pos} of {func}: type mismatch: the parameter is declared {expTy} but the argument is {actTy}. A call site performs no conversion between these -- pass a value of the declared type, or change the parameter's declared type."
    | InvalidArrayCapture name -> $"Lambda cannot capture array '{name}'"
    | InvalidApplication funcTy -> sprintf "Cannot apply non-function type: %A" funcTy
    | PatternTypeMismatch (pat, ty) -> sprintf "Pattern '%s' incompatible with type %A" pat ty
    | ProviderNativeLoadFailure (provider, path, detail) ->
        $"provider '{provider}' cannot load its native library, so the store '{path}' cannot be read at compile time: {detail}. Every type this store binds is unresolvable until the library loads -- install the provider's runtime, or point its install-root variable at it (NETCDF_DIR for netcdf: the compiler and generated programs then use that install's own libraries)."
    | ProviderStoreUnresolvable (provider, path, detail) ->
        $"provider '{provider}' cannot resolve the store '{path}' at compile time: {detail}"
    // Promoted variants (Stage 5): text reproduced verbatim.
    | IndexTagMismatchNamed (expected, actual) -> $"Array index tag mismatch: slot expects '{expected}' but argument has type '{actual}'."
    | IndexTagMismatchAnon expected -> $"Array index tag mismatch: slot expects named tag '{expected}' but argument is an anonymous index value."
    | CrossNominalIndexArith (left, right) -> $"Cross-nominal index-type arithmetic: cannot combine values of distinct index domains '{left}' and '{right}'."
    | CrossAnonIndexArith (left, right) -> $"Cross-nominal index-type arithmetic: cannot combine values of distinct anonymous index domains (#{left} vs #{right})."
    | CompoundTupleForm rank -> $"Compound arrays take FLAT positional subscripts like SymIdx: write B(c0, ..., c{rank - 1}), not the tuple form B((c0, ..., c{rank - 1})) -- and wildcards (`_`) are not accepted on a compound axis. Partial/wildcard reads (pinning some coordinates, gathering the matches) are a SparseIdx feature: build the valid tuples as a SparseIdx<keys> and index S((c0, _, ...)) there (formalism 3.5)."
    | CompoundUnderSupplied (rank, got) -> $"Compound index under-supplied: this array's compound axis has rank {rank} (mask is {rank}-dimensional), so it needs {rank} flat subscripts B(c0, ..., c{rank - 1}); got {got}. Partial reads are a SparseIdx feature (formalism 3.5)."
    | CompoundOverSupplied (rank, got) -> $"Compound index over-supplied: this array's compound axis has rank {rank} (mask is {rank}-dimensional) and consumes {rank} flat subscripts (plus one per trailing dim); got {got} total."
    | SparseBareWildcard rank -> $"A bare wildcard `_` cannot index a sparse axis: it pins no coordinate (the result would just be the array itself). Index with a full {rank}-tuple, pinning at least one coordinate."
    | SparseWildcardArity (rank, tupleLen) -> $"Wildcard sparse indexing must use a FULL-arity tuple: this sparse axis has rank {rank}, so write all {rank} coordinates with `_` marking each free axis (got a {tupleLen}-tuple). Short tuples (without wildcards) pin a leading prefix instead: S((c0, ..., cj))."
    | SparseAllFree rank -> $"Sparse index with all {rank} coordinates free (`_`) pins nothing -- the result is the array itself. Drop the index, or pin at least one coordinate."
    | SparseOverSupplied (rank, got) -> $"Sparse index over-supplied: this array's sparse axis has rank {rank} (keys are {rank}-tuples), so it takes at most a {rank}-tuple like S((c0, ..., c{rank - 1})); got a {got}-tuple."
    | SparseNeedsTuple rank -> $"Sparse index must be a single tuple: write S((c0, ..., cj)) with inner parentheses, not the flat form S(c0, ..., cj). A SparseIdx<keys> axis of rank {rank} is indexed as one joint tuple, full or partial (formalism 3.5)."
    // OrbIdx storage-refusal text, front-end half. Not routed through
    // IR.orbitStorageUnsupported: a TypeError carries strings, not IR
    // structures, so this renderer can't recover depth as a number -- the
    // ONLY difference between the two spellings ("depth >= 2" here, "depth d"
    // there); everything else is identical and corpus-pinned. KEEP THEM IN
    // STEP or a half-updated pair tells the user two different stories.
    | ConstrainedDomainRefused (name, why) ->
        $"range<{name}>: {why}"
    | OrbitStorageUnsupported (levels, where_) ->
        sprintf "%s: OrbIdx<%s, n> is a declarable index class of depth >= 2, and a DEDUCED one can now be \
allocated, written, printed, READ at an arbitrary tuple (the per-level canon fold, the accumulated \
character, the zero set), FULLY DECOMPACTED to its dense tensor, and round-tripped through a Zarr \
store (the spec_version 2 'orbit' head -- providers/ZarrTriangularSpec.md). What is still missing is every \
path that would put a wreath pool anywhere but under its own traversal nest: an OrbIdx ANNOTATION \
(a store is now a producer, but the annotation also admits classes nothing produces), reduce/prodsum \
over the pool, transpose, PARTIAL (per-level) decompaction, a WINDOWED or distributed store read, and \
provider I/O outside Zarr (CSV and NetCDF have no pool axis to carry the class on). So the \
compiler refuses here rather than compute an address it cannot compute. \
The depth-1 spellings work through the existing compact machinery instead: OrbIdx<[(r,+)], n> is \
exactly SymIdx<r, n>, OrbIdx<[(r,-)], n> is exactly AntisymIdx<r, n>, and OrbIdx<[], n> is exactly \
Idx<n>." where_ levels
    | OrbitSubscriptArity (levels, axes, got) ->
        sprintf "OrbIdx<%s, n> acts on %d raw axes, so a subscript of it takes exactly %d flat \
coordinates (W(i0, ..., i%d)) -- the same flat spelling a rank-k SymIdx group takes; got %d. \
%sA wreath record is ONE index slot spanning all %d axes, not %d slots." levels axes axes (axes - 1) got
                (if got < axes then
                    "A PARTIAL subscript has no answer for a wreath class: its rows shrink per LEVEL, \
so no residual index type describes a fibre of the pool -- decompact(W, 0) first and slice the \
dense result. "
                 else "")
                axes axes
    | OrbitDecompactPartial (levels, dim) ->
        sprintf "decompact(W, %d): only FULL decompaction of an OrbIdx<%s, n> class is implemented -- \
write decompact(W, 0), which produces the complete dense tensor (one plain Idx<n> axis per raw axis, \
each cell the class's own read: the per-level canon fold, the accumulated character, and 0 on the \
zero set). For a wreath class the second argument is the number of LEVELS TO KEEP \
(docs/plan-orbidx-decompaction.md section 4.3), not a dimension to free, and the partial/peel lattice of \
that plan's section 3 is not built: peeling level d unties its r_d sub-blocks into a juxtaposition of \
depth-(d-1) classes, which needs a typed residual nothing produces yet." dim levels
    | OrbitFoldUnsupported (levels, op) ->
        sprintf "%s() over OrbIdx<%s, n> compact storage is not supported: folding the canonical POOL \
cells and folding the logical (mirrored) cells differ. The pool holds one cell per ORBIT, so a fold \
over it answers for an array of that many elements instead of the n^rank tensor it stands for -- and \
each cell would need its orbit multiplicity (a Burnside count), with a '-' level's character \
cancelling terms outright. decompact(W, 0) first for the logical fold: full decompaction of a wreath \
class IS implemented, and the dense result folds like any other array." op levels
    | RaggedIdxNeedsPrior func -> $"function '{func}': RaggedIdx requires at least one prior index in the array's index list -- the ragged extent is a per-row function of the OUTER iteration position (formalism 4.4). Add an outer index, e.g. Array<T like Idx<n>, RaggedIdx<lens>>."
    | TagWildcardNotParam where_ -> $"{where_}: the tag wildcard `_` is legal in PARAMETER position only. A parameter may decline to constrain its argument's index tag or unit, but this position has to PRODUCE one -- a wildcard here would erase the tag rather than relax it. Write the concrete index type (e.g. Nat<LatIdx>) or the bare base type."
    | IndexRankMismatch (where_, left, leftRank, right, rightRank) ->
        let components n = if n = 1 then "1 index component" else $"{n} index components"
        $"{where_}: {left} spans {components leftRank} but {right} spans {components rightRank}. A rank-k compact group (SymIdx<k, n> / AntisymIdx<k, n>) is ONE index slot covering k dimensions -- indexed A(i0, ..., i(k-1)), not A(j) -- so it is a different type from a flat axis holding the same cells (SymIdx<2, 3> packs 6 cells, exactly Idx<6>). An equal cell count does NOT make the two interchangeable. Convert with decompact (compact group -> dense axes); an annotation cannot reinterpret one form as the other."
    | DecompactDimRange (dim, totalDims) -> $"decompact: dimension {dim} is out of range for a rank-{totalDims} array (valid dims 0..{totalDims - 1})"
    | DecompactPlainAxis dim -> $"decompact: dimension {dim} is a plain (rank-1, non-symmetric) axis; there is nothing to decompact. decompact pulls a component out of a compact group (SymIdx/AntisymIdx/HermitianIdx)."
    | DecompactLastSlotOnly (slots, slot) -> $"decompact: only a compact group in the LAST index slot, optionally preceded by plain free Idx dimensions, is supported by codegen (the chained to-the-right peel shape). The array here has {slots} index slots with the compact group at slot {slot}."
    | TransposeAxisRange (axis, totalDims) -> $"transpose: axis {axis} is out of range for a rank-{totalDims} array (valid axes 0..{totalDims - 1})"
    | TransposeAxesEqual (axisA, axisB) -> $"transpose: the two axes must differ (got [{axisA}, {axisB}]); swapping an axis with itself is the identity"
    | TransposeWithinGroup rank -> $"transpose: swapping two dimensions within a single rectangular index group (rank {rank}) is not yet supported."
    | StackNeedsArrays (pos, got) -> $"stack: argument {pos} has type {got}, not an array. stack(A1, ..., An) adds a fresh LEADING axis over n arrays of the SAME shape; to build a rank-1 array from scalars write the array literal [a, b, c] instead."
    | StackShapeMismatch (pos, detail) -> $"stack: argument {pos} does not match argument 1 ({detail}). stack(A1, ..., An) requires every operand to have the same rank, extents, and element type -- the fresh leading axis selects among them."
    | JoinNeedsArrays (pos, got) -> $"join: argument {pos} has type {got}, not an array. join(A, B, d) concatenates arrays along dimension d."
    | JoinDimRange (dim, totalDims) -> $"join: dimension {dim} is out of range for a rank-{totalDims} array (valid dims 0..{totalDims - 1})"
    | JoinShapeMismatch (pos, detail) -> $"join: argument {pos} does not match argument 1 ({detail}). join(A, B, d) requires equal rank, equal element type, and equal extents on EVERY axis except the joined dimension d."
    | StackJoinCompactSlot (op, slot) -> $"{op}: index slot {slot} is a compact, ragged, or compound group. {op} materializes a dense rectangular result, so its operands must be dense (plain Idx) on every axis -- decompact the axis first."
    | UnitMismatch (context, left, right) -> $"Unit mismatch in {context}: {left} vs {right}"
    | QuantityArgMismatch (pos, quantity, got) ->
        $"argument {pos}: the parameter's declared type carries the quantity '{quantity}', and a quantity-typed slot only accepts values ASSERTED to be that quantity -- this argument is {got}. Ascribe it at the call site (e.g. `x : {quantity}`); matching dimensions alone do not imply the quantity."
    | ExtentArgMismatch (pos, dim, expected, actual) ->
        $"argument {pos}: extent mismatch on index slot {dim} -- the parameter declares Idx<{expected}> but the argument has Idx<{actual}>. A LITERAL parameter extent is baked into the emitted loop bounds and result allocations (a symbolic extent like Idx<n> reads the argument's extent at runtime instead), so this reads past the argument's allocation rather than merely disagreeing. Make the extents match, or declare the parameter over a symbolic extent."
    | ZipExtentMismatch (pos, expected, actual) ->
        $"elementwise co-iteration: operand {pos} has extent {actual} on the shared axis, but operand 1 has extent {expected}. A zip walks ONE index space, taken from the first operand, so the longer walk reads past the shorter operand's allocation -- silent out-of-bounds, not a broadcast (Blade does not broadcast mismatched extents). Bring the operands to a common extent, or index/slice the longer one first."
    | CoIterArgExtentMismatch (callee, posA, posB, extA, extB) ->
        $"arguments {posA} and {posB} of '{callee}': the body of '{callee}' CO-ITERATES these two parameters (an elementwise zip walks them as one index space), but argument {posA} has extent {extA} on the shared axis and argument {posB} has extent {extB}. The walk takes its bound from the first operand, so the longer one runs off the end of the shorter one's allocation -- silent out-of-bounds, not a broadcast (Blade does not broadcast mismatched extents). Because '{callee}' declares those parameters abstractly (`T^1`), the body has no extents to compare and this call is the first place the disagreement is visible. Pass arrays of equal extent, or slice the longer one to the shorter one's index space first."
    | CoIterBodyExtentMismatch (callee, pos, argExt, bodyExt) ->
        $"argument {pos} of '{callee}': the body of '{callee}' CO-ITERATES this parameter with an array of extent {bodyExt} (an elementwise zip walks them as one index space), but this argument has extent {argExt} on the shared axis. The walk takes its bound from the first operand, so whichever is longer runs off the end of the shorter one's allocation -- silent out-of-bounds, not a broadcast (Blade does not broadcast mismatched extents). The zip has one concrete side and one abstract (`T^1`) side, so the body could not compare them and this call is the first place both are known. Pass an array of extent {bodyExt}, or slice this one to that index space first."
    | ProviderReadExtentMismatch (provider, dim, annotated, actual) ->
        $"provider read: the annotation declares extent {annotated} on index slot {dim}, but the read's own type has extent {actual}. A provider read is typed BY THE STORE -- the annotation cannot reshape it -- while codegen allocates the store's true shape and compiles every later subscript against the ANNOTATED one, so a disagreement here is an out-of-bounds read with no runtime symptom, not a naming quarrel. Correct the annotation to the {provider} store's shape, or drop it and let the read supply the type (slice or reshape the value afterwards if a different shape is what you want)."
    | HaloExtentMismatch (declared, dim, targetName, actual) ->
        $"halo extent mismatch: the halo declares an inner extent of {declared}, but '{targetName}' (read through the window at index slot {dim}) has extent {actual}. The window walk is bounded by the DECLARED extent, so an oversized halo reads past '{targetName}''s allocation and an undersized one silently emits fewer windows. Make the halo's inner index match the array it windows over."
    | HaloOffsetOutsideSet (offset, declared, targetName) ->
        let set = declared |> List.map string |> String.concat ", "
        $"halo offset outside the declared set: '{targetName}' is read at window offset {offset}, but the halo declares the offsets [{set}], whose reach is {min 0 (List.min declared)}..{max 0 (List.max declared)}. The interior is shrunk for that reach only, so this read lands past the array's allocation at the boundary. Add {offset} to the halo's offset list (which widens the reach), or read within it."
    | QuantityTerminal (quantity, declName) ->
        $"unit '{declName}': the quantity '{quantity}' cannot be used inside a unit expression. Quantities are TERMINAL -- the nominal layer is exactly one level deep -- so a quantity name can neither be composed (`Unit x = {quantity} * m`) nor re-derived from (`Unit q: {quantity}`). Compose from the structural units the quantity was declared over instead."
    | UnknownUnitName (name, declName, candidates) ->
        let where =
            if declName = unitAnnoContext then "unit annotation"
            else $"unit '{declName}'"
        sprintf "%s: '%s' is not a declared unit or a known scale constant. A unit expression composes names already in scope -- only a numeric LITERAL may appear without being declared -- so declare '%s' first (`Unit %s`), import the module that exports it, or fix the spelling.%s" where name name name
            (if List.isEmpty candidates then "" else $""" Did you mean: {(String.concat ", " candidates)}?""")
    | DefaultParamOrder (func, requiredParam, defaultedParam) ->
        $"{func}: parameter '{requiredParam}' has no default but follows the defaulted parameter '{defaultedParam}'. Defaults are TRAILING: once a parameter has a default, every later parameter needs one too (otherwise an omitted-argument call is ambiguous). Reorder the parameters or give '{requiredParam}' a default."
    | DefaultParamScope (func, param, referenced) ->
        $"{func}: the default for parameter '{param}' references '{referenced}', which is itself a defaulted parameter. A default may reference the REQUIRED parameters only -- defaults evaluate left-to-right at call entry with just the required arguments bound, so another default's value is not available."
    | DefaultParamShadowed (func, param, name) ->
        $"{func}: the default for parameter '{param}' reads '{name}' from the scope where the function was declared, but at this call site '{name}' is a different binding (a parameter or local that shadows it). A default keeps its declaration-site meaning, so it cannot be filled in here: pass the argument explicitly, or rename the local '{name}'."
    | FactoryDupQuantityDecl (func, quantity, param1, param2) ->
        $"{func}: defaulted parameters '{param1}' and '{param2}' both carry the quantity '{quantity}'. By-nominal argument routing (`f(x, 3 : {quantity})`) needs each quantity to name exactly ONE defaulted slot -- give the second slot a distinct quantity, or make it a plain (non-quantity) parameter."
    | FactoryDupFill (callee, quantity, slot) ->
        $"call to '{callee}': the quantity slot '{slot}' (quantity '{quantity}') is supplied twice -- a second argument tagged '{quantity}' (or a positional argument already claiming that slot) conflicts with an earlier one. Each slot takes at most one argument."
    | FactoryUnknownTag (callee, quantity, candidates) ->
        $"""call to '{callee}': an argument is tagged with the quantity '{quantity}', but '{callee}' has no defaulted slot of that quantity. Its quantity slots are: {(if List.isEmpty candidates then "none" else String.concat ", " candidates)}."""
    | FactoryAmbiguousMix (callee, pos) ->
        $"call to '{callee}': argument {pos} has no quantity tag but appears AFTER a quantity-tagged argument, so its slot would be a guess. Positional (untagged) arguments must come first, in declared order; tag the stragglers (`v : quantity`) or reorder the call."
    | IntrinsicBindArrayFailed op -> $"{op}(): failed to bind array type after unification"
    | IntrinsicNeedsArray op -> $"{op}() requires an array as argument"
    | IntrinsicNotComplex name -> $"{name} is not defined for complex operands."
    | IntrinsicNeedsNumeric name -> $"{name} expects a numeric operand."
    | InvalidCast msg -> msg
    | SpecIndexMatchNotStatic (scrutinee, offender) ->
        $"match on {scrutinee}: arm selection happens at specialization (each arity/rank clone keeps exactly one arm), but {offender}. Arms of a specialization-index match must be unguarded integer literals plus at most one catch-all (`_` or a name); move the runtime condition inside the chosen arm's body instead: `| 1 -> if threshold > 0.0 then ... else ...`."
    | AbsNeedsNumericScalar got -> $"abs expects a numeric scalar operand, got {got}"
    | IntrinsicComplexScalarOnly name -> $"{name} applies to complex scalars; map it over the array elementwise (e.g. method_for(A) <@> lambda(z) -> {name}(z) |> compute)."
    | IntrinsicNeedsComplex (name, got) -> $"{name} expects a complex operand, got {got}"
    | ReduceEmptyArray extent -> $"reduce() rejects statically empty arrays (extent = {extent}). Empty input has no defined reduction without an identity; supply one with the 3-arg form `reduce(arr, op, init)`."
    | ProdsumExtentMismatch (a, b) -> $"prodsum() operands must share one extent: got {a} and {b}"
    | GramNeedsRank2 (leftRank, rightRank) -> $"gram(A, B): both operands must be rank-2 (matrix) arrays; got rank-{leftRank} and rank-{rightRank}. gram contracts the trailing axis: A (m x n), B (p x n) -> m x p."
    | GramCompactOperand side -> $"gram(A, B): operands must be rank-2 with two PLAIN index axes; {side} compact rank-2 group storage (SymIdx / AntisymIdx / HermitianIdx, e.g. a gram result), whose single packed axis cannot supply the outer and contracted dimensions separately. Expand to a dense matrix first: decompact(X, 0)."
    | ArrayLitLength (got, expected, axisTag) ->
        let axis = match axisTag with Some t -> $" for axis '{t}'" | None -> ""
        $"Array literal{axis} has {got} elements, but the annotation's extent is {expected}"
    | CompactLitShape (idx, shape, where_, detail) ->
        sprintf "Array literal over the compact group %s: %s %s. A compact group is ONE axis storing \
one cell per canonical tuple, laid out as a left-justified simplex, so its literal is written in that \
same shape -- %s. (A flat list or a rectangular nest names cells the storage does not have.)"
            idx where_ detail shape
    | HermitianLitDiagComplex where_ ->
        sprintf "Hermitian literal: %s is a DIAGONAL cell with a non-zero imaginary part. \
A(i, i) = conj(A(i, i)) forces the diagonal real, and the stored diagonal cell is read unconjugated, \
so this value is not one the class can hold. Write a real diagonal (the off-diagonal cells carry the \
complex half)." where_
    | RaggedLensMismatch (lensName, declared, actual) ->
        $"Ragged literal: the annotation names `RaggedIdx<{lensName}>`, and {lensName} holds {declared}, but the literal's rows are {actual}. A ragged array takes its row lengths from its LITERAL -- the baked lens and offsets are computed from this nesting, and nothing reads `{lensName}` back -- so the annotation would describe a shape the array does not have. Fix whichever one is wrong; if the lengths are meant to come from the data, drop the annotation and let the literal infer the ragged type."
    | RaggedLensNotStatic lensName ->
        $"Ragged literal: the annotation names `RaggedIdx<{lensName}>`, but `{lensName}` is not a compile-time value. A ragged array's row lengths are baked from the LITERAL's own nesting, so a lens known only at run time can neither be honoured nor checked -- it would be accepted and then ignored. Make `{lensName}` a compile-time array of integer literals (so the two can be compared), or drop the annotation and let the literal infer the ragged type. Allocating to lengths only the running program knows is separate, planned work."
    | ObjectForKernel got -> sprintf "object_for kernel must be a lambda, reynolds, or zero, but got %A" got
    | ChainOpNeedsMethodFor leftDesc -> $"<@> requires method_for or object_for on the left side, but got {leftDesc}"
    | ChainOpBadKernel rightDesc -> $"<@> kernel must be a lambda, operator section, named function, reynolds(...), or zero, but got {rightDesc}"
    | ChainOpUndecidable (leftDesc, rightDesc) -> $"cannot infer the roles of the <@> operands: the left side is {leftDesc} and the right side is {rightDesc}, so the arrays/kernel roles are ambiguous. A former is implicit only when one side is decisive: a kernel (lambda, operator section, named function, reynolds(...), zero) or a former. Write it explicitly: method_for(arrays) <@> kernel, or object_for(kernel) <@> (arrays)."
    | CommContradictsBody (p1, p2) -> $"`where comm({p1}, {p2})` contradicts the kernel body, which is provably ANTIcommutative under that swap (f({p2}, {p1}) = -f({p1}, {p2})): triangular storage would silently corrupt half the output. Remove the comm clause, or wrap the kernel in reynolds(...) if a signed iteration license over the permutation sum is what you intend."
    | AntisymmContradictsBody (p1, p2) -> $"`where anticomm({p1}, {p2})` contradicts the kernel body, which is provably COMMUTATIVE under that swap (f({p2}, {p1}) = f({p1}, {p2})): strict-triangular anticommutative storage would drop the diagonal and negate half the output. Remove the anticomm clause (use `where comm({p1}, {p2})` for the symmetric triangle), or wrap the kernel in reynolds(..., Antisymmetric) if a signed antisymmetrization is what you intend."
    | CommContradictsConjBody (p1, p2) -> $"`where comm({p1}, {p2})` contradicts the kernel body, which provably CONJUGATES under that swap (f({p2}, {p1}) = conj(f({p1}, {p2}))): symmetric storage recovers mirrored cells by IDENTITY, so every read across the diagonal would return the stored value un-conjugated -- the imaginary part of half the output silently flips sign. Remove the comm clause (dense storage computes both triangles); if the symmetric real part is what you intend, real(...) of this kernel IS commutative; if a Hermitian Gram matrix is the goal, `gram(A, A)` routes to Hermitian storage, whose mirrored reads conjugate."
    | AntisymmContradictsConjBody (p1, p2) -> $"`where anticomm({p1}, {p2})` contradicts the kernel body, which provably CONJUGATES under that swap (f({p2}, {p1}) = conj(f({p1}, {p2}))): strict-triangular storage recovers mirrored cells by NEGATION, but the true mirror is the conjugate -- the real part of half the output silently flips sign (only the imaginary half happens to agree). Remove the anticomm clause (dense storage computes both triangles); if the antisymmetric imaginary part is what you intend, imag(...) of this kernel IS anticommutative."
    | AntisymMapNotOdd (param, proved) -> $"mapping this kernel over an ANTISYMMETRIC (AntisymIdx) array would keep the input's strict-triangular storage, and that is only correct for a SIGN-ODD kernel (f(-x) = -f(x)); the deduction says this one is {proved} in '{param}'. An even or unknown-parity map of an antisymmetric array is SYMMETRIC -- it has a diagonal, and the strict iteration the input forces cannot produce one -- so the compact result would negate every mirrored read. Map over a dense copy instead (`decompact(A, 0)` materializes the full tensor, and the kernel over THAT is symmetric with the right diagonal), or use a sign-odd kernel."
    | WreathTieKernelNotOdd (param, proved, levels) -> $"the declared clause ties every argument over a compact class with a '-' inner level ({levels}), and that tie is only sound for a kernel SIGN-ODD in each argument separately (h(-p, q) = -h(p, q)): a '-' level claims that mirroring ONE argument's sub-block negates the value, so an even or unknown-parity kernel would store a class whose mirrored reads and decompaction answer with signs the values do not satisfy. The deduction says this kernel is {proved} in '{param}'. Use a per-argument sign-odd kernel (e.g. p * q; note p + q is NOT odd in each argument), or map over dense copies instead: decompact(_, 0) materializes the full tensor, and the kernel over THAT carries no wreath claim."
    | HermitianMapNotReal param -> $"mapping this kernel over a HERMITIAN (HermitianIdx) array would keep the input's Hermitian storage, whose mirrored reads recover H(j,i) as conj(H(i,j)); that is only correct when the kernel commutes with conjugation (f(conj z) = conj(f z)), which is not deducible for '{param}'. A kernel built from the parameter, real constants, + - * /, and neg/conj/real qualifies; a complex constant, imag(z), arg(z), `^` and the math intrinsics (exp/log/sqrt/...) do not. Map over a dense copy instead: `decompact(A, 0)` materializes the full conjugate-mirrored matrix, and the kernel over THAT carries no storage claim."
    | FoldOmpNeedsLicense kernelDesc -> $"parallel reduction needs comm(...) or a builtin op: {kernelDesc} carries `omp` but nothing licenses the reorder. A parallel fold splits the axis into per-thread chunks and combines the partials, so the kernel must be COMMUTATIVE and ASSOCIATIVE -- write `where comm(a, b), omp` to declare it (the same word `<@>` uses for symmetric storage, cross-checked against the body's parity), or use a builtin fold body (a + b, a * b, a && b, a || b), which carries both properties outright. Drop `omp` to keep the serial fold."
    | PlaceholderNeedsAllBound (got, total) -> $"the `_` placeholder needs every other parameter bound: this call supplies {got} of {total} args. Combine with prefix partial application in two steps, or use a lambda."
    | GroupKeysRank1 -> "group_keys: all key arrays must be rank-1 and share the same outer index (same length). Compound grouping requires each i-th element of every key array to refer to the same record."
    | GroupKeysEscapes (what, pos) -> $"{what} cannot be used {pos}: a `group_keys` result is NAME-KEYED, not a value. It compiles to three locals named after the binding (`<name>__ngroups`, `<name>__offsets`, `<name>__perm`) and the binding itself carries only an opaque sentinel, so `group_by` can find the grouping only under the exact name the keys were bound to. Bind the keys once (`let gk = group_keys(...)`) and pass that same `gk` directly to each `group_by` -- a group_keys result cannot be aliased to a second name, put in a tuple or struct, passed as a function argument, or returned. (Grouping two arrays the same way is what one shared `gk` is FOR: `group_by(a, gk)` and `group_by(b, gk)` co-iterate; two separate `group_keys` calls do not.)"
    | GroupingNeedsName (intrinsic, got) -> $"{intrinsic}(gk) requires the BARE NAME of a `group_keys(...)` binding; got {got}. A grouping is not a value -- its state lives in locals named after the binding (`<gk>__ngroups`, `<gk>__offsets`, `<gk>__perm`), so it cannot be aliased, passed, returned, or built inline. Bind it once (`let gk = group_keys(...)`) and write `{intrinsic}(gk)`."
    | GroupBucketNotGrouping got -> $"group_bucket expects a `group_keys(...)` binding; '{got}' is not one. Bind the keys first: `let gk = group_keys(k)`, then `group_bucket(gk)`."
    | FallbackNeedsArrays (leftDesc, rightDesc) -> $"<|:> (allocated-fallback) reads the LEFT array where its storage holds a cell and the right array elsewhere, so both operands must be arrays; got {leftDesc} and {rightDesc}. For value-level choice (first nonzero wins) over scalars or computations, use <|>."
    | FallbackSymmetricLeft -> "<|:> over a symmetric/antisymmetric/Hermitian left operand is not yet supported: symmetric A requires symmetric allocation (formalism 2.6), which the compiler cannot yet verify. decompact(A, d) to dense first."
    | FallbackRightNotDense what -> $"<|:> right operand must be a plain dense array (it supplies the value for every cell the left side lacks); got {what}."
    | FallbackRankMismatch (leftRank, rightRank) -> $"<|:> operands must cover the same index space: the left side spans {leftRank} dimension(s) but the right side has rank {rightRank}."
    | CumulantOrderPositive order -> $"cumulant: order must be >= 1, got {order}"
    | CumulantNeedsDist got -> $"cumulant expects cumulant(d, k) where d is a Dist value (a dist(...) binding or Dist-typed parameter); got {got}"
    | DistOpUndefined (left, right) -> $"this operator is not defined on Dist values (left: {left}, right: {right}): dists support scalar * (multilinearity), + and - of independent dists, and component projection via cumulant(d, k)"
    | EnumIdxMixedKinds name -> $"EnumIdx '{name}' has mixed value kinds: integer and string literals in the same EnumIdx<[...]> aren't allowed. The runtime backing must be one or the other (int64_t or std::string)."
    | EnumIdxUnknownLabel (enumName, label, available) ->
        $"""'{label}' is not a value of EnumIdx '{enumName}'. Available: {(available |> String.concat ", ")}."""
    | ImplMissingMethods (iface, typeName, methods) -> $"impl {iface} for {typeName} is missing required methods: {methods}"
    | StructFieldDuplicate (structName, field) -> $"struct {structName}: field '{field}' assigned more than once"
    | StructNoField (structName, field) -> $"struct {structName} has no field '{field}'"
    | StructFieldUnknown (structName, field, available, steering) ->
        let avail =
            match available with
            | [] -> "; it declares no fields"
            | fs -> $"""; available fields: {(fs |> String.concat ", ")}"""
        $"struct {structName} has no field '{field}'{steering}{avail}"
    | StructSpreadBase structName -> $"struct {structName}: a spread base must be a variable or field path -- bind it with let first"
    | StructSpreadNotStruct (structName, got) -> $"struct {structName}: spread base must be a {structName} value, got {got}"
    | StructSpreadRedundant structName -> $"struct {structName}: every field is provided explicitly -- the '..' spread is redundant"
    | StructMissingField (structName, field) -> $"struct {structName}: missing field '{field}' in constructor"
    | StructFieldType (structName, field, expected, actual) -> $"struct {structName}, field '{field}': expected {expected}, got {actual}"
    | UnknownStructType name -> $"unknown struct type '{name}' in constructor"
    | StructWhereNotBool (structName, got) -> $"struct {structName}: where-constraint must be a boolean expression, got {got}"
    | StructWhereError (structName, inner) -> $"struct {structName} where-constraint: {inner}"
    | WherePredicateUnannotated (owner, func) -> $"static function '{func}' is called from a where-clause of '{owner}': annotate all its parameter types and its return type"
    | UnknownWhereConstraint (func, name, vocab) -> $"function '{func}': unknown where-clause constraint '{name}' (registered constraints: {vocab})"
    | DistOrderCompileTime func -> $"function '{func}': Dist order must be a compile-time integer >= 1 (a literal, `let static`, or static-function call): Dist<order, Elem like I1, ..., Ik>"
    | ImmutableStaticAssign name -> $"Cannot assign to '{name}': static bindings are immutable"
    | MutAssignRefused (target, reason) -> $"Cannot assign to '{target}': {reason}"
    | MutArgNotPassable (func, argIndex, got) ->
        $"function '{func}': argument {argIndex} is passed to a `mut` parameter, which writes back into the caller's array, but {got}. Only a `let mut` binding (or another `mut` parameter being forwarded) may be passed there -- declare it `let mut`, or drop `mut` from the parameter if the callee does not write to it."
    | MutParamNotArray (func, param) -> $"function '{func}': parameter '{param}' is `mut` but not array-typed. Only array parameters can be mutated in place (scalars pass by value); return the new scalar instead."
    | MutualBindJointly (typeName, describe, lowerNames) -> $"type '{typeName}' belongs to mutual group ({describe}); bind the group jointly: let ({lowerNames}): ({describe}) = ..."
    | MutualDirectElementsOnly describe -> $"mutual member types (group {describe}) may appear only as direct elements of a joint tuple annotation"
    | MutualMixedGroups -> "annotation mixes members of different mutual groups"
    | MutualDuplicateMember describe -> $"duplicate mutual member in annotation (group {describe})"
    | MutualIncompleteAnnotation describe -> $"mutual group ({describe}) is incomplete in this annotation; all group members must appear together"
    | MutualJointAnnotationOnly describe -> $"mutual member types (group {describe}) may appear only in a joint let annotation or a function's declared return type"
    | MutualParamMemberType (func, param, memberName) -> $"function '{func}': parameter '{param}' uses mutual member type '{memberName}'; mutual member types may appear only in a joint let annotation or a function's declared return type"
    | MutualBindTuple names -> $"a mutual group ({names}) must be bound jointly with a tuple of variables: let (x, y): ({names}) = ..."
    | MutualReturnTupleElements describe -> $"mutual group ({describe}): declared return type must list every member as a direct tuple element"
    | StructFieldMutualType (structName, field, memberName) -> $"struct {structName}, field '{field}': mutual member type '{memberName}' may not be used as a field type"
    | MutualMemberDupGroup memberName -> $"mutual-group member '{memberName}' is already part of another group"
    | MutualMemberNotStruct (memberName, name) -> $"mutual-group member '{memberName}': '{name}' is not a declared struct"
    | MutualMemberBadAlias (memberName, got) -> $"mutual-group member '{memberName}' must alias a struct or scalar type, got {got}"
    | MutualUnknownField (memberName, field, structName) -> $"mutual constraint references unknown field '{memberName}.{field}' (struct {structName})"
    | MutualScalarBare (memberName, field) -> $"'{memberName}' aliases a scalar; reference it bare, not '{memberName}.{field}'"
    | MutualStructNeedsField memberName -> $"'{memberName}' aliases a struct; reference one of its fields as '{memberName}.<field>'"
    | MutualUnknownIdent name -> $"identifier '{name}' in a mutual-group constraint must be a group member, a member field path, or a static"
    | MutualUnsupportedExpr -> "unsupported expression form in a mutual-group constraint"
    | MutualConstraintNotBool (groupId, got) -> $"mutual-group constraint (group {groupId}) must be a boolean expression, got {got}"
    | MutualConstraintError (groupId, inner) -> $"mutual-group constraint (group {groupId}): {inner}"
    | ProviderStreamNeedsVar alias -> $"{alias}.stream expects a provider array variable"
    | ProviderReadWindowBounds (alias, lo, hi, n) -> $"{alias}.read_window bounds [{lo}, {hi}) must satisfy 0 <= lo < hi <= {n} (the packed extent)"
    | ProviderReadWindowLiteralExtent alias -> $"{alias}.read_window needs a literal packed extent"
    | ProviderReadWindowPacked alias -> $"{alias}.read_window applies to PACKED variables (leading SymIdx/AntisymIdx); use {alias}.read for dense variables"
    | ProviderReadWindowNeedsVar alias -> $"{alias}.read_window expects a provider array variable as its first argument"
    | ProviderReadWindowArgs alias -> $"{alias}.read_window expects (variable, lo, hi) with integer-literal bounds"
    | ProviderWriteNeedsArray alias -> $"{alias}.write expects an array as its second argument (the variable to store)"
    | ProviderWriteNamedBinding alias -> $"{alias}.write stores a NAMED array binding (its name becomes the store variable's name): bind the value first (let A = ...; {alias}.write(\"path\", A))"
    | ProviderWriteArgs alias -> $"{alias}.write expects (\"path\", array): a string-literal store path and the array to write"
    | ProviderWriteModuleScope alias -> $"{alias}.write is a MODULE-LEVEL declaration form: it is only allowed as the whole right-hand side of a top-level `let` (let _ = {alias}.write(\"path\", A)). A write nested inside a block, a function or lambda body, a loop, or a branch is not lowered -- hoist it to module scope, or return the array from the block and write it there."
    | IrrepsIdxArgMismatch (pos, expected, actual) -> $"argument {pos}: IrrepsIdx mismatch: the parameter expects {expected} but the argument carries {actual}. IrrepsIdx identity is the spec (plus nominative alias name) -- equal total_dim does not make two irreps spaces interchangeable."
    | BlockSpecArgMismatch (pos, expected, actual) -> $"argument {pos}: block-spec index mismatch: the parameter expects {expected} but the argument carries {actual}. A block-structured index's identity is its GROUP FAMILY plus its spec (plus nominative alias name) -- equal total_dim does not make two block spaces interchangeable, and an O(3) irreps space is never a point-group one."
    | IrrepsIdxSpec detail -> $"IrrepsIdx: {detail}. The spec must be a static array of (l, parity, mult) int triples -- a `let static` binding or an inline literal like IrrepsIdx<[(0, 0, 2), (1, 1, 2)]>."
    | IrrepsIdxSpecFn (func, detail) -> $"function '{func}': IrrepsIdx: {detail}. The spec must be a static array of (l, parity, mult) int triples -- a `let static` binding or an inline literal like IrrepsIdx<[(0, 0, 2), (1, 1, 2)]>."
    | PgIrrepsIdxSpec detail -> $"PgIrrepsIdx: {detail}. The form is PgIrrepsIdx<GROUP, SPEC> with GROUP a registered point group and SPEC a static array of (LABEL_NAME, mult) tuples -- a `let static` binding or an inline literal like PgIrrepsIdx<C4, [(\"A\", 1), (\"E\", 2)]>."
    | PgIrrepsIdxSpecFn (func, detail) -> $"function '{func}': PgIrrepsIdx: {detail}. The form is PgIrrepsIdx<GROUP, SPEC> with GROUP a registered point group and SPEC a static array of (LABEL_NAME, mult) tuples -- a `let static` binding or an inline literal like PgIrrepsIdx<C4, [(\"A\", 1), (\"E\", 2)]>."
    | ComplexArity got -> $"complex expects exactly two float components -- complex(re, im) -- got {got} argument(s)"
    | CumulantOrderExceeds (order, carried) -> $"cumulant: order {order} exceeds the dist's carried order {carried} -- insufficient stochastic order. Construct with a higher order (dist(A, {order})) or project a carried component."
    | DistOrderDisagree (op, leftOrder, rightOrder) -> $"dist {op}: orders disagree ({leftOrder} vs {rightOrder}) -- carry the same stochastic order on both sides"
    | DistNotIndependent (op, source1, source2, steering) -> $"dist {op}: cumulants combine only for independent distributions -- sources '{source1}' and '{source2}' are not declared independent; {steering}"
    | PplConstraintNeedsImport (func, bare) -> $"function '{func}': constraint '{bare}' belongs to the ppl module -- add `import ppl as <alias>` and write `where <alias>.{bare}(...)`"
    | StructBoundScope (structName, field, bad) -> $"struct {structName}, field '{field}': bound references '{bad}' -- bounds may reference only earlier fields and statics"
    | StaticStructField (structName, field, why) -> $"static struct {structName}, field '{field}': {why} -- every field of a `static struct` must have a statically evaluable type (Int, Float, Bool, String, Char, a tuple of those, or another static struct)"
    | BoundsInverted (where_, lo, hi) -> $"{where_}: bounds cross -- min={lo} is greater than max={hi} (bounds are inclusive on both ends, so this type has no values)"
    | BoundsOnAggregate (where_, noun, subject) -> $"{where_}: bounds apply to primitive types, not aggregates -- the bound is applied to {noun}. A bound is a runtime comparison against {subject} itself (formalism 2.4: bounded PRIMITIVES carry runtime-checked bounds), and an aggregate has no such comparison. Write the bound on the ELEMENT type instead -- `Array<Float64<min=.., max=..> like I, J>` is checked cell by cell."
    // Wording must match Unify.fs's rank-bound block: `got` carries the
    // whole "a scalar" / "a rank-N array" tail, so the sentence stays exact.
    | RankBoundViolation (needed, got) -> $"this value flows into a position that requires a rank-{needed} (or higher) array, but it resolved to {got}"
    | ProviderImportByModule (suggestion, providers) -> $"provider modules are imported by module name -- write `import {suggestion} as <alias>` (the Providers.* spelling was removed; registered providers: {providers})"
    | ProviderNoSelectiveImport pname -> $"provider module '{pname}' does not support selective import -- use `import {pname} as <alias>` and call <alias>.load/read/write"
    | IndexTypeArithForbidden name -> $"Arithmetic on index type '{name}' is not permitted. Index types are nominal labels -- for value-level arithmetic on positions, use virtual array iteration (which produces plain ints); for new index types derived from arithmetic, type-level construction is a separate workstream not yet implemented."
    | Other msg -> msg

/// Format a CompileError with location and context
let formatCompileError (err: CompileError) : string =
    let loc =
        if err.Span.StartLine > 0 then
            match err.Span.File with
            | Some f -> $"{f}:{err.Span.StartLine}:{err.Span.StartCol}"
            | None -> $"{err.Span.StartLine}:{err.Span.StartCol}"
        else ""
    let msg = formatTypeError err.Error
    let context =
        err.Context
        |> List.rev
        |> List.map (sprintf "  %s")
        |> String.concat "\n"
    if loc = "" && context = "" then msg
    elif context = "" then $"{loc}: {msg}"
    else $"{loc}: {msg}\n{context}"

/// CompileError -> unified Diagnostic. The code comes from the raiser when
/// present (CompileError.Code), else from the TypeError variant.
let diagnosticOfCompileError (e: CompileError) : Blade.Diagnostics.Diagnostic =
    let code =
        match e.Code with
        | Some c -> c
        | None ->
            match e.Error with
            | UnboundVariable _ -> "BL2001"
            // Same-scope duplicate `function` name: a name-binding refusal,
            // so it lives in the BL2xxx resolution band, not BL3xxx.
            | DuplicateFunctionDecl _ -> "BL2009"
            // Environment condition, not a type judgment: the provider's
            // native library is unloadable, so the store's names cannot
            // resolve -- same band as BL2004's "module not found".
            | ProviderNativeLoadFailure _ -> "BL2007"
            // Same band, same reason: the store named at the load site does not
            // resolve, so no name it binds can. BL2007's sibling, one condition
            // over (library vs store).
            | ProviderStoreUnresolvable _ -> "BL2008"
            | TypeMismatch _ | ArgRankMismatch _ | ArgTypeMismatch _ -> "BL3001"
            | ArityMismatch _ | KernelPackArity _ -> "BL3002"
            | InvalidApplication _ -> "BL3003"
            | PatternTypeMismatch _ -> "BL3004"
            | InvalidArrayCapture _ -> "BL3005"
            // Promoted variants (Stage 5)
            | UnitMismatch _ -> "BL3006"
            | QuantityArgMismatch _ -> "BL3010"
            | ExtentArgMismatch _ | HaloExtentMismatch _ | ZipExtentMismatch _
            | CoIterArgExtentMismatch _ | CoIterBodyExtentMismatch _ -> "BL3016"
            | ProviderReadExtentMismatch _ -> "BL3016"
            | QuantityTerminal _ -> "BL3011"
            | DefaultParamOrder _ | DefaultParamScope _ | DefaultParamShadowed _ -> "BL3012"
            | FactoryDupQuantityDecl _ -> "BL3013"
            | FactoryDupFill _ | FactoryUnknownTag _ | FactoryAmbiguousMix _ -> "BL3014"
            | UnknownUnitName _ -> "BL3015"
            | GroupKeysEscapes _ -> "BL3017"
            | IntrinsicBindArrayFailed _ | IntrinsicNeedsArray _
            | IntrinsicNotComplex _ | IntrinsicNeedsNumeric _ | AbsNeedsNumericScalar _
            | IntrinsicComplexScalarOnly _ | IntrinsicNeedsComplex _ | ComplexArity _
            | ReduceEmptyArray _ | ProdsumExtentMismatch _ | GramNeedsRank2 _
            | GramCompactOperand _
            | ArrayLitLength _ | CompactLitShape _ | HermitianLitDiagComplex _
            | ObjectForKernel _ | ChainOpNeedsMethodFor _ | ChainOpBadKernel _
            | ChainOpUndecidable _
            | PlaceholderNeedsAllBound _ | GroupKeysRank1
            | GroupingNeedsName _ | GroupBucketNotGrouping _
            | CumulantOrderPositive _
            | CumulantOrderExceeds _ | CumulantNeedsDist _ | DistOrderDisagree _
            | DistNotIndependent _ | DistOpUndefined _ | EnumIdxMixedKinds _
            | EnumIdxUnknownLabel _
            | ImplMissingMethods _
            | FallbackNeedsArrays _ | FallbackSymmetricLeft
            | FallbackRightNotDense _ | FallbackRankMismatch _ -> "BL3007"
            // Field ACCESS gets its own code: BL3008 reads "struct
            // construction error", and this one is raised nowhere near a
            // constructor.
            | StructFieldUnknown _ -> "BL3018"
            // Explicit numeric cast refusals get their own code: BL3007's
            // "invalid builtin argument" bucket would bury the one message
            // users need (how to license the conversion they meant).
            | InvalidCast _ -> "BL3019"
            // A guard (or non-int pattern) on a `match arity(p)` /
            // `match rank(p)` scrutinee: arm selection happens at
            // specialization, where a runtime guard has no answer. Its own
            // code, because the fix (move the condition into the arm's
            // body) is nothing like BL3004's pattern-type story.
            | SpecIndexMatchNotStatic _ -> "BL3021"
            | StructFieldDuplicate _ | StructNoField _ | StructMissingField _
            | StructFieldType _ | UnknownStructType _ | StructBoundScope _
            | StaticStructField _
            | StructSpreadBase _ | StructSpreadNotStruct _ | StructSpreadRedundant _ -> "BL3008"
            | RankBoundViolation _ -> "BL3009"
            // A `comm`/`antisymm` clause the deduction PROVED wrong is not
            // BL3007's generic "invalid builtin argument" bucket: it's an
            // annotation contradicting its own body -- drop the clause, or
            // wrap in `reynolds` for the signed iteration license.
            | CommContradictsBody _ | AntisymmContradictsBody _
            | CommContradictsConjBody _ | AntisymmContradictsConjBody _ -> "BL4013"
            // Same family, other direction: nothing is DECLARED here -- the
            // output would inherit the input's compact class, and the
            // deduction can't certify the kernel commutes with its mirror
            // involution. WreathTieKernelNotOdd joins deliberately: it DOES
            // declare a clause (the SWAP h(p,q) = h(q,p)), but the failing
            // certificate -- per-argument oddness against the inner class's
            // mirror -- is exactly this family's undeclared gap.
            | AntisymMapNotOdd _ | HermitianMapNotReal _
            | WreathTieKernelNotOdd _ -> "BL4015"
            // Own code, not BL4013's: nothing is CONTRADICTED here -- `omp`
            // asks for a reorder the checker has no licence for. Fix is to
            // add `comm(...)` (or use a builtin body), not remove a disproved annotation.
            | FoldOmpNeedsLicense _ -> "BL4016"
            // Own code, not BL4003's index-type bucket: the index type is
            // well formed and the USE is servable -- it is the LENS, the one
            // part of the annotation construction does not derive for itself,
            // that the literal contradicts. The fix is to reconcile two
            // spellings of one shape, not to stop using RaggedIdx.
            | RaggedLensMismatch _ | RaggedLensNotStatic _ -> "BL4018"
            | HaloOffsetOutsideSet _ -> "BL4019"
            // BL4020: a constrained domain that no route can enumerate --
            // not closed-form (class B) and over the box cap for a table.
            | ConstrainedDomainRefused _ -> "BL4020"
            | StructWhereNotBool _ | StructWhereError _ | WherePredicateUnannotated _
            | PplConstraintNeedsImport _
            | UnknownWhereConstraint _ -> "BL4001"
            | DistOrderCompileTime _ -> "BL4002"
            | IndexTagMismatchNamed _ | IndexTagMismatchAnon _ | CrossNominalIndexArith _
            | CrossAnonIndexArith _ | IndexTypeArithForbidden _ | IrrepsIdxArgMismatch _
            | BlockSpecArgMismatch _
            | CompoundTupleForm _ | CompoundUnderSupplied _ | CompoundOverSupplied _
            | SparseBareWildcard _ | SparseWildcardArity _ | SparseAllFree _
            | SparseOverSupplied _ | SparseNeedsTuple _ | RaggedIdxNeedsPrior _
            // BL4003: the index TYPE is well formed but this USE can't be
            // served. Not BL7001 ("not yet supported by THIS BACKEND") --
            // both backends refuse, in the front end.
            | OrbitStorageUnsupported _ | OrbitSubscriptArity _
            | OrbitDecompactPartial _ | OrbitFoldUnsupported _
            | IrrepsIdxSpec _ | IrrepsIdxSpecFn _
            | PgIrrepsIdxSpec _ | PgIrrepsIdxSpecFn _ | TagWildcardNotParam _
            | BoundsInverted _ | BoundsOnAggregate _ -> "BL4003"
            | IndexRankMismatch _
            | DecompactDimRange _ | DecompactPlainAxis _ | DecompactLastSlotOnly _
            | TransposeAxisRange _ | TransposeAxesEqual _ | TransposeWithinGroup _
            | StackNeedsArrays _ | StackShapeMismatch _ | JoinNeedsArrays _
            | JoinDimRange _ | JoinShapeMismatch _ | StackJoinCompactSlot _ -> "BL4004"
            | ImmutableStaticAssign _ | MutParamNotArray _ | MutAssignRefused _
            | MutArgNotPassable _ -> "BL4005"
            | MutualBindJointly _ | MutualDirectElementsOnly _ | MutualMixedGroups
            | MutualDuplicateMember _ | MutualIncompleteAnnotation _ | MutualJointAnnotationOnly _
            | MutualParamMemberType _ | MutualBindTuple _ | MutualReturnTupleElements _
            | StructFieldMutualType _ | MutualMemberDupGroup _ | MutualMemberNotStruct _
            | MutualMemberBadAlias _ | MutualUnknownField _ | MutualScalarBare _
            | MutualStructNeedsField _ | MutualUnknownIdent _
            | MutualUnsupportedExpr | MutualConstraintNotBool _ | MutualConstraintError _ -> "BL4006"
            | ProviderStreamNeedsVar _ | ProviderReadWindowBounds _ | ProviderReadWindowLiteralExtent _
            | ProviderReadWindowPacked _ | ProviderReadWindowNeedsVar _ | ProviderReadWindowArgs _
            | ProviderWriteNeedsArray _ | ProviderWriteNamedBinding _ | ProviderWriteArgs _
            | ProviderWriteModuleScope _ -> "BL3007"
            | ProviderImportByModule _ | ProviderNoSelectiveImport _ -> "BL2003"
            | Other _ -> "BL3999"
    Blade.Diagnostics.mkError code (Blade.Diagnostics.Codes.phaseOfCode code) e.Span (formatTypeError e.Error)
    |> Blade.Diagnostics.withContext e.Context

/// Unified Diagnostic -> CompileError, for pipeline stages (elaborators) that
/// produce Diagnostics inside typeCheck's CompileError channel. Code and span survive; extra context appends outermost.
let compileErrorOfDiagnostic (extraContext: string list) (d: Blade.Diagnostics.Diagnostic) : CompileError =
    { Error = Other d.Message
      Span = d.Span
      Context = d.Context @ extraContext
      Code = Some d.Code }

let bindVar name info (env: TypeEnv) =
    { env with Variables = Map.add name info env.Variables }

let bindVarSimple name varId ty env =
    bindVar name { VarId = varId; Type = ty; Identity = None
                   Assign = ReadOnly; TypedValue = None; Scheme = None } env


let bindVarFull name varId ty identity assign typedValue env =
    bindVar name { VarId = varId; Type = ty; Identity = identity
                   Assign = assign; TypedValue = typedValue; Scheme = None } env

/// Bind a polymorphic (let-generalized) variable.
let bindVarPoly name varId ty identity assign typedValue scheme env =
    bindVar name { VarId = varId; Type = ty; Identity = identity
                   Assign = assign; TypedValue = typedValue
                   Scheme = Some scheme } env

let lookupVar name env = Map.tryFind name env.Variables
let lookupTypeDef name env = Map.tryFind name env.TypeDefs

/// Convert AST BindingMut to Assignability.
let assignOfBindingMut = function
    | BindConst -> ReadOnly    // static / let const -> immutable
    | BindLet -> Assignable    // let -> assignable in scope
    | BindMut -> MutPassable   // let mut -> assignable + mut-passable

/// Enter a callable body: snapshot every currently-visible binding into
/// `OuterScope`, and mark the environment as being inside a body.
///
/// The two halves are one operation because the only scope boundary that
/// exists in the checker IS a callable body -- a LAMBDA body, a named-FUNCTION
/// declaration body, or an IMPL-METHOD body. (A `{ ... }` block does not enter
/// a scope; its lets go straight into `Variables`.) Keeping them together is
/// what makes `bodyLocalBinding` below sound.
let enterCallableBody env =
    { env with
        OuterScope = Map.foldBack Map.add env.Variables env.OuterScope
        InCallableBody = true }

/// Was `name` bound INSIDE the callable body currently being inferred?
///
/// `enterCallableBody` snapshots every visible binding into `OuterScope` at the
/// body boundary, and only a NESTED body extends it again -- so `OuterScope` is
/// always the snapshot taken at the INNERMOST enclosing body, and a name absent
/// from it was bound after that boundary. False at module level (no body, so
/// nothing is body-local) and false for a captured outer binding.
///
/// Shadowing reads conservatively: a body-local `let e` that shadows an outer
/// `e` answers false, because the outer name is in the snapshot. Callers use
/// this to opt OUT of an optimization, so a false negative costs efficiency,
/// never correctness.
let bodyLocalBinding (name: string) (env: TypeEnv) =
    env.InCallableBody && not (Map.containsKey name env.OuterScope)

/// Is `name` a CAPTURE of the lambda currently being inferred -- visible here,
/// but bound outside the innermost enclosing body, so `buildCaptures` will put
/// it in the lambda's `Captures` and codegen will forward it by name at every
/// call site?
///
/// Gated on `InLambdaBody`, NOT `InCallableBody`, because only a lambda has a
/// capture list: a NAMED function's `Captures` is always empty (IRCallable),
/// and the module bindings its body spells are served by main-local emission /
/// the S0 declaration hoist instead. Widening this to every callable body
/// silently breaks that path -- a named function reading a deferred binding by
/// name has nothing to materialize it (`function g(w) = w * reduce(c, (+))`
/// over a deferred `c` emits `c[0]` with no definition of `c`).
///
/// False at module level, like `bodyLocalBinding` -- there is no body, so a
/// name is neither body-local nor captured.
let capturedOuterBinding (name: string) (env: TypeEnv) =
    env.InLambdaBody && Map.containsKey name env.OuterScope

let registerTypeDef name info (env: TypeEnv) =
    { env with TypeDefs = Map.add name info env.TypeDefs }

/// Register a loaded provider module's struct types and return the binding's
/// module-struct type. The provider's dims/vars structs register as-is; a
/// synthetic top-level struct `name` is added so the binding carries
/// `.dims`/`.vars` fields (e.g. `sample.vars.temp` resolves to the real Array
/// type). Pure (no file IO) -- unit-testable with a mock module; reading `pm`'s file metadata is the caller's concern.
let registerProviderModule (env: TypeEnv) (name: string) (pm: IRModule) : TypeEnv * IRType =
    let envS =
        pm.Types |> List.fold (fun e td ->
            match td with
            | IRTDStruct (n, fields) -> registerTypeDef n (TDIStruct (n, [], fields, [])) e
            | IRTDEnumIdx (n, idx, values) ->
                // Provider-synthesized column enums (CSV headered mode):
                // registration folds string-literal column subscripts to
                // ordinals at dispatchAppOrIndex. Body is a synthesized
                // surface TypeExpr -- no source declaration exists.
                let bodyExpr =
                    mkExpr noSpan (ExprKind.ExprArrayLit (
                        values |> List.map (fun v ->
                            match v with
                            | EVString s -> mkExpr noSpan (ExprKind.ExprLit (LitString s))
                            | EVInt n -> mkExpr noSpan (ExprKind.ExprLit (LitInt n)))))
                registerTypeDef n (TDIEnumIdx (n, idx, values, TyEnumIdx bodyExpr)) e
            | IRTDIndexType (n, idx) ->
                // Axis types the provider derived from the file, exposed as
                // `<binding>.index.<dim>` (e.g. `Array<Float64 like
                // store.index.y>`) instead of hand-copying an extent that goes
                // stale on regeneration. QUALIFIED ONLY, deliberately: bare
                // names like `time` recur across stores and would clobber
                // last-write-wins in the shared TypeDefs map (alias down with
                // `type Y = store.index.y` if needed). Registered EXACTLY as
                // the provider built it (Tag = None, asserted by
                // NetcdfTests); lowerIndexType stamps a fresh Id per use, unifying on extent match like a hand-written Idx<64>.
                let extentExpr =
                    match idx.Extent with
                    | IRLit (IRLitInt v) -> mkExpr noSpan (ExprKind.ExprLit (LitInt v))
                    | _ -> mkExpr noSpan (ExprKind.ExprLit (LitInt 0L))
                let qualified = $"{name}.index.{n}"
                registerTypeDef qualified (TDIIndexType (qualified, idx, TyIdx extentExpr)) e
            | _ -> e) env
    // Module-struct fields point at the dims/vars struct decls the provider
    // emitted, namespaced "<binding>__dims"/"<binding>__vars" so multiple
    // loads don't clobber each other in this flat TypeDefs map (a clobbered
    // entry silently retypes earlier uses). Bare `n = label` covers providers that emit unsuffixed structs.
    let structNames = pm.Types |> List.choose (function IRTDStruct (n, _) -> Some n | _ -> None)
    let fieldFor (label: string) =
        structNames
        |> List.tryFind (fun n -> n = label || n = $"{name}__{label}")
        |> Option.map (fun n -> (label, IRTNamed n))
    let moduleFields = [fieldFor "dims"; fieldFor "vars"] |> List.choose id
    let moduleStruct = TDIStruct (name, [], moduleFields, [])
    (registerTypeDef name moduleStruct envS, IRTNamed name)


/// Check if a variant type is a pure enum (all constructors have no data)
let isEnumType (env: TypeEnv) (parentName: string) : bool =
    match Map.tryFind parentName env.TypeDefs with
    | Some (TDIVariant (_, _, variants)) -> variants |> List.forall (fun (_, d) -> d.IsNone)
    | _ -> false

let registerVariantTag tag parentName payload (env: TypeEnv) =
    { env with VariantTags = Map.add tag (parentName, payload) env.VariantTags }

/// Why a UnitExpr failed to resolve. Both cases are hard declaration-site
/// errors at a `Unit` RHS -- an unknown name is BL3015, a terminal-quantity
/// misuse BL3011. The split survives because the ANNOTATION consumers
/// (compound unit annotations) still degrade rather than reject, and they
/// distinguish the two: see unitAnnoTerminalError in TypeCheck.fs.
type UnitResolveErr =
    | UResolveUnknown of name: string
    /// A quantity (nominal) name referenced inside unit algebra. Quantities
    /// are terminal: the nominal layer is exactly one level deep.
    | UResolveTerminal of quantity: string

/// Render a UnitResolveErr for the channels that still degrade rather than
/// reject (the defensive lowering fallback; annotation resolution).
let ppUnitResolveErr (e: UnitResolveErr) : string =
    match e with
    | UResolveUnknown name -> $"Unknown unit '{name}'"
    | UResolveTerminal q -> $"Quantity '{q}' cannot appear in a unit expression (quantities are terminal)"

/// Resolve a UnitExpr AST node to a canonical UnitSig. Quantity names
/// (Nominal = Some) are REJECTED in every position — a quantity is an
/// identity, not a factor, so it can neither be composed (`speed * m`) nor
/// re-derived from (`Unit q: speed`).
let rec resolveUnitExpr (units: Map<string, UnitSig>) (expr: UnitExpr) : Result<UnitSig, UnitResolveErr> =
    match expr with
    | UnitNamed name ->
        match Map.tryFind name units with
        | Some sig' when sig'.Nominal.IsSome -> Error (UResolveTerminal name)
        | Some sig' -> Ok sig'
        // Irrational scale constants (`pi`) resolve only AFTER the unit
        // table, so a user's own `Unit pi` still shadows the built-in and no
        // existing program changes meaning. Dimensionless: a constant
        // contributes a magnitude, never a dim.
        | None when unitScaleConstants.ContainsKey name ->
            Ok (unitOfDimsScaled Map.empty (scaleOfConst name))
        | None -> Error (UResolveUnknown name)
    | UnitOne -> Ok unitDimensionless
    | UnitScaleLit (num, den) -> Ok (unitOfDimsScaled Map.empty (scaleOfRational num den))
    | UnitMul (a, b) ->
        resolveUnitExpr units a |> Result.bind (fun sa ->
        resolveUnitExpr units b |> Result.map (fun sb ->
            unitMul sa sb))
    | UnitDiv (a, b) ->
        resolveUnitExpr units a |> Result.bind (fun sa ->
        resolveUnitExpr units b |> Result.map (fun sb ->
            unitDiv sa sb))
    | UnitPow (a, n) ->
        resolveUnitExpr units a |> Result.map (fun sa ->
            unitPow sa n)

/// Names already in scope that a misspelling plausibly meant. Quantities are
/// excluded: suggesting one would only trade BL3015 for BL3011. Shared with
/// the ANNOTATION check in TypeCheck.fs, which raises the same BL3015.
let unitSpellingCandidates (units: Map<string, UnitSig>) (name: string) : string list =
    let close (a: string) (b: string) =
        // One transposition, or a one-character insert/delete/substitute --
        // enough for `pii`/`pi` and `metre`/`meter`, tight enough that an
        // unrelated unit never shows up as a suggestion.
        if abs (a.Length - b.Length) > 1 then false
        elif a.Length = b.Length then
            let diffs = Seq.zip a b |> Seq.filter (fun (x, y) -> x <> y) |> Seq.toList
            match diffs with
            | [] | [_] -> true
            | [(x1, y1); (x2, y2)] -> x1 = y2 && x2 = y1  // transposition
            | _ -> false
        else
            let short, long = if a.Length < b.Length then a, b else b, a
            // One deletion turns `long` into `short`.
            [0 .. long.Length - 1]
            |> List.exists (fun i -> long.Remove(i, 1) = short)
    let lowered = name.ToLowerInvariant()
    Map.toList units
    |> List.filter (fun (n, s) -> s.Nominal.IsNone)
    |> List.map fst
    |> List.append (Map.toList unitScaleConstants |> List.map fst)
    |> List.filter (fun n -> n <> name && (n.ToLowerInvariant() = lowered || close n name))
    |> List.distinct
    |> List.sort

/// Register a unit declaration in the environment. A `Unit` right-hand side
/// composes names already in scope, so a name that is neither a declared unit
/// nor a scale constant is a hard error (BL3015) -- the old warn-and-fallback
/// minted the declared name as a fresh BASE unit, which typechecks a
/// misspelling into a silently wrong dimension. A terminal-quantity misuse
/// stays BL3011.
let registerUnit (env: TypeEnv) (decl: UnitDecl) : Result<TypeEnv, TypeError> =
    // Base-unit signature: canonical form is {name: 1}
    let baseSig () = unitOfDims (Map.ofList [(decl.Name, 1)])
    let resolveErr (err: UnitResolveErr) =
        match err with
        | UResolveTerminal q -> QuantityTerminal (q, decl.Name)
        | UResolveUnknown n -> UnknownUnitName (n, decl.Name, unitSpellingCandidates env.Units n)
    let sigResult =
        match decl.Definition with
        | None | Some UnitBase -> Ok (baseSig ())
        | Some (UnitDerived expr) ->
            resolveUnitExpr env.Units expr
            |> Result.mapError resolveErr
        | Some (UnitQuantity expr) ->
            // Quantity: nominal identity entailing the RHS dims. The RHS is
            // resolved through the same terminal-checking path, so a quantity
            // on the RHS of another quantity rejects here too.
            resolveUnitExpr env.Units expr
            |> Result.map (fun resolved -> { resolved with Nominal = Some decl.Name })
            |> Result.mapError resolveErr
    sigResult |> Result.map (fun sig' ->
        { env with Units = Map.add decl.Name sig' env.Units })

// 2b. Generalization (needs VarInfo defined above)

/// Collect free inference vars across all variable types in scope.
let freeInferVarsInEnv (subst: Subst) (vars: Map<string, VarInfo>) : Set<int> =
    vars |> Map.fold (fun acc _ info ->
        Set.union acc (freeInferVars subst info.Type)) Set.empty

/// Generalize a type: quantify over inference vars free in the type
/// but NOT free in the environment.
let generalize (subst: Subst) (envVars: Map<string, VarInfo>) (ty: IRType) : TypeScheme =
    let envFree = freeInferVarsInEnv subst envVars
    let tyFree = freeInferVars subst (subst.Resolve ty)
    let quantified = Set.difference tyFree envFree |> Set.toList
    { QuantifiedVars = quantified; Body = subst.Resolve ty }

// 3. Constant Expression Evaluation (for index extents)

/// Try to evaluate an AST expression to a compile-time int64.
